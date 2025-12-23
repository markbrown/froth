%-----------------------------------------------------------------------%
% eval.m
% Evaluator for the Froth programming language.
%-----------------------------------------------------------------------%

:- module eval.
:- interface.

:- import_module io.
:- import_module list.
:- import_module types.

%-----------------------------------------------------------------------%

    % eval_terms(OpTable, BaseDir, Terms, !Env, !Stack, !ST, !IO):
    % Evaluate a list of terms, updating the environment and stack.
    % OpTable is read-only (fixed at engine startup).
    % BaseDir is used to resolve relative paths in imports.
    % StringTable is threaded through (may grow with dynamic imports).
    % Throws eval_error on failure.
    %
:- pred eval_terms(operator_table::in, string::in, list(term)::in,
    env::in, env::out, stack::in, stack::out,
    string_table::in, string_table::out, io::di, io::uo) is det.

    % Environment operations.
    %
:- pred get_env(string_id::in, value::out, env::in) is semidet.
:- pred set_env(string_id::in, value::in, env::in, env::out) is det.

%-----------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module dir.
:- import_module exception.
:- import_module lexer.
:- import_module map.
:- import_module operators.
:- import_module parser.
:- import_module string.

%-----------------------------------------------------------------------%
% Environment operations
%-----------------------------------------------------------------------%

get_env(Name, Value, Env) :-
    map.search(Env, Name, Value).

set_env(Name, Value, !Env) :-
    map.set(Name, Value, !Env).

%-----------------------------------------------------------------------%
% Main evaluation
%-----------------------------------------------------------------------%

eval_terms(_, _, [], !Env, !Stack, !ST, !IO).
eval_terms(OpTable, BaseDir, [Term | Terms], !Env, !Stack, !ST, !IO) :-
    eval_term(OpTable, BaseDir, Term, !Env, !Stack, !ST, !IO),
    eval_terms(OpTable, BaseDir, Terms, !Env, !Stack, !ST, !IO).

:- pred eval_term(operator_table::in, string::in, term::in, env::in, env::out,
    stack::in, stack::out, string_table::in, string_table::out,
    io::di, io::uo) is det.

eval_term(OpTable, BaseDir, Term, !Env, !Stack, !ST, !IO) :-
    (
        Term = identifier(NameId),
        eval_identifier(OpTable, BaseDir, NameId, !Env, !Stack, !ST, !IO)
    ;
        Term = binder(NameId),
        eval_binder(NameId, !Env, !Stack)
    ;
        Term = function(Terms),
        eval_function(Terms, !.Env, !Stack)
    ;
        Term = generator(Terms),
        eval_generator(OpTable, BaseDir, Terms, !Env, !Stack, !ST, !IO)
    ;
        Term = quoted(T),
        push(termval(T), !Stack)
    ;
        Term = value(V),
        push(V, !Stack)
    ;
        Term = apply_term,
        eval_apply(OpTable, BaseDir, !Env, !Stack, !ST, !IO)
    ).

%-----------------------------------------------------------------------%
% Identifier evaluation
%-----------------------------------------------------------------------%

:- pred eval_identifier(operator_table::in, string::in, string_id::in,
    env::in, env::out, stack::in, stack::out,
    string_table::in, string_table::out, io::di, io::uo) is det.

eval_identifier(OpTable, BaseDir, NameId, !Env, !Stack, !ST, !IO) :-
    ( if get_env(NameId, V, !.Env) then
        push(V, !Stack)
    else if map.search(OpTable, NameId, Info) then
        ( if Info ^ oi_operator = op_import then
            % import is special: it can modify Env and ST
            eval_import(OpTable, BaseDir, !Env, !Stack, !ST, !IO)
        else if Info ^ oi_operator = op_restore then
            % restore is special: it replaces the current Env
            eval_restore(!Env, !Stack)
        else
            operators.eval_operator(OpTable, !.ST, Info ^ oi_operator, !.Env,
                !Stack, !IO)
        )
    else
        throw(undefined_name(NameId))
    ).

%-----------------------------------------------------------------------%
% Binder evaluation
%-----------------------------------------------------------------------%

:- pred eval_binder(string_id::in, env::in, env::out,
    stack::in, stack::out) is det.

eval_binder(NameId, !Env, !Stack) :-
    pop("binder", V, !Stack),
    set_env(NameId, V, !Env).

%-----------------------------------------------------------------------%
% Function evaluation
%-----------------------------------------------------------------------%

:- pred eval_function(list(term)::in, env::in,
    stack::in, stack::out) is det.

% A closure is represented as: consval(mapval(Env), termval(function(Terms)))
eval_function(Terms, Env, !Stack) :-
    Closure = consval(mapval(Env), termval(function(Terms))),
    push(Closure, !Stack).

%-----------------------------------------------------------------------%
% Generator evaluation
%-----------------------------------------------------------------------%

:- pred eval_generator(operator_table::in, string::in, list(term)::in,
    env::in, env::out, stack::in, stack::out,
    string_table::in, string_table::out, io::di, io::uo) is det.

eval_generator(OpTable, BaseDir, Terms, !Env, !Stack, !ST, !IO) :-
    % Save current stack, evaluate with empty stack
    SavedStack = !.Stack,
    !:Stack = [],
    eval_terms(OpTable, BaseDir, Terms, !Env, !Stack, !ST, !IO),
    % Convert stack to array (reverse since stack is LIFO)
    Array = array.from_reverse_list(!.Stack),
    % Restore stack and push array
    !:Stack = SavedStack,
    push(arrayval(Array), !Stack).

%-----------------------------------------------------------------------%
% apply (!)
%-----------------------------------------------------------------------%

:- pred eval_apply(operator_table::in, string::in, env::in, env::out,
    stack::in, stack::out, string_table::in, string_table::out,
    io::di, io::uo) is det.

eval_apply(OpTable, BaseDir, Env, Env, !Stack, !ST, !IO) :-
    pop("!", V, !Stack),
    ( if V = consval(mapval(ClosureEnv), termval(function(Terms))) then
        % Evaluate with closure's env, then discard env changes (lexical scoping)
        eval_terms(OpTable, BaseDir, Terms, ClosureEnv, _, !Stack, !ST, !IO)
    else if V = termval(identifier(Id)), map.search(OpTable, Id, Info) then
        ( if Info ^ oi_operator = op_import then
            % import is special: it can modify Env (but we discard here like closures)
            eval_import(OpTable, BaseDir, Env, _, !Stack, !ST, !IO)
        else
            % Apply a quoted operator
            operators.eval_operator(OpTable, !.ST, Info ^ oi_operator, Env,
                !Stack, !IO)
        )
    else
        throw(type_error("closure or operator", V))
    ).

%-----------------------------------------------------------------------%
% import: ( filename -- ) Load and evaluate a file
%-----------------------------------------------------------------------%

:- pred eval_import(operator_table::in, string::in, env::in, env::out,
    stack::in, stack::out, string_table::in, string_table::out,
    io::di, io::uo) is det.

eval_import(OpTable, BaseDir, !Env, !Stack, !ST, !IO) :-
    pop("import", V, !Stack),
    RelFilename = operators.value_to_string(!.ST, V),
    % Resolve relative paths using BaseDir
    ( if dir.path_name_is_absolute(RelFilename) then
        Filename = RelFilename
    else
        Filename = dir.make_path_name(BaseDir, RelFilename)
    ),
    NewBaseDir = dir.dirname(Filename),
    io.read_named_file_as_string(Filename, ReadResult, !IO),
    (
        ReadResult = ok(Content),
        lexer.tokenize(Content, !.ST, LexResult),
        (
            LexResult = ok(Tokens, !:ST),
            parser.parse(Tokens, ParseResult),
            (
                ParseResult = ok(Terms),
                eval_terms(OpTable, NewBaseDir, Terms, !Env, !Stack, !ST, !IO)
            ;
                ParseResult = error(ParseError),
                throw_parse_error(Filename, ParseError)
            )
        ;
            LexResult = error(LexError),
            throw_lex_error(Filename, LexError)
        )
    ;
        ReadResult = error(Error),
        throw(io_error("import", Filename, io.error_message(Error)))
    ).

:- pred throw_lex_error(string::in, lex_error::in) is erroneous.

throw_lex_error(Filename, unterminated_string(Pos)) :-
    throw(io_error("import", Filename,
        string.format("%d:%d: unterminated string",
            [i(Pos ^ line), i(Pos ^ column)]))).
throw_lex_error(Filename, unterminated_block_comment(Pos)) :-
    throw(io_error("import", Filename,
        string.format("%d:%d: unterminated block comment",
            [i(Pos ^ line), i(Pos ^ column)]))).
throw_lex_error(Filename, invalid_escape_sequence(Pos, Char)) :-
    throw(io_error("import", Filename,
        string.format("%d:%d: invalid escape sequence: \\%c",
            [i(Pos ^ line), i(Pos ^ column), c(Char)]))).

:- pred throw_parse_error(string::in, parse_error::in) is erroneous.

throw_parse_error(Filename, unexpected_token(Pos, _)) :-
    throw(io_error("import", Filename,
        string.format("%d:%d: unexpected token",
            [i(Pos ^ line), i(Pos ^ column)]))).
throw_parse_error(Filename, unexpected_close_curly(Pos)) :-
    throw(io_error("import", Filename,
        string.format("%d:%d: unexpected '}'",
            [i(Pos ^ line), i(Pos ^ column)]))).
throw_parse_error(Filename, unexpected_close_square(Pos)) :-
    throw(io_error("import", Filename,
        string.format("%d:%d: unexpected ']'",
            [i(Pos ^ line), i(Pos ^ column)]))).
throw_parse_error(Filename, unclosed_curly(Pos)) :-
    throw(io_error("import", Filename,
        string.format("%d:%d: unclosed '{'",
            [i(Pos ^ line), i(Pos ^ column)]))).
throw_parse_error(Filename, unclosed_square(Pos)) :-
    throw(io_error("import", Filename,
        string.format("%d:%d: unclosed '['",
            [i(Pos ^ line), i(Pos ^ column)]))).
throw_parse_error(Filename, quote_at_end(Pos)) :-
    throw(io_error("import", Filename,
        string.format("%d:%d: unexpected end after quote",
            [i(Pos ^ line), i(Pos ^ column)]))).
throw_parse_error(Filename, junk_token(Pos, S)) :-
    throw(io_error("import", Filename,
        string.format("%d:%d: invalid token: '%s'",
            [i(Pos ^ line), i(Pos ^ column), s(S)]))).

%-----------------------------------------------------------------------%
% restore: ( map -- ) Replace current environment with the given map
%-----------------------------------------------------------------------%

:- pred eval_restore(env::in, env::out, stack::in, stack::out) is det.

eval_restore(!Env, !Stack) :-
    pop("restore", V, !Stack),
    ( if V = mapval(NewEnv) then
        !:Env = NewEnv
    else
        throw(type_error("map", V))
    ).

%-----------------------------------------------------------------------%
:- end_module eval.
%-----------------------------------------------------------------------%
