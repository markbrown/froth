%-----------------------------------------------------------------------%
% eval.m
% Evaluator for the Froth programming language.
%-----------------------------------------------------------------------%

:- module eval.
:- interface.

:- import_module array.
:- import_module io.
:- import_module list.
:- import_module types.

%-----------------------------------------------------------------------%

    % eval_terms(OpTable, BaseDir, Terms, !Env, !Stack, !StackPtr,
    %            !ST, !Bytecode, !BytecodeSize, !IO):
    % Evaluate a list of terms, updating the environment and stack.
    % OpTable is read-only (fixed at engine startup).
    % BaseDir is used to resolve relative paths in imports.
    % StringTable is threaded through (may grow with dynamic imports).
    % Bytecode array and size are threaded through (may grow with compile).
    % Stack is represented as an array and pointer (for efficient operations).
    % Throws eval_error on failure.
    %
:- pred eval_terms(operator_table::in, string::in, list(term)::in,
    env::in, env::out,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out,
    string_table::in, string_table::out,
    array(int)::array_di, array(int)::array_uo,
    int::in, int::out,
    io::di, io::uo) is det.

    % Environment operations.
    %
:- pred get_env(string_id::in, value::out, env::in) is semidet.
:- pred set_env(string_id::in, value::in, env::in, env::out) is det.

%-----------------------------------------------------------------------%

:- implementation.

:- import_module bytecode.
:- import_module datastack.
:- import_module dir.
:- import_module exception.
:- import_module int.
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

eval_terms(_, _, [], !Env, !Array, !Ptr, !ST, !BC, !BCSz, !IO).
eval_terms(OpTable, BaseDir, [Term | Terms], !Env, !Array, !Ptr, !ST, !BC, !BCSz, !IO) :-
    eval_term(OpTable, BaseDir, Term, !Env, !Array, !Ptr, !ST, !BC, !BCSz, !IO),
    eval_terms(OpTable, BaseDir, Terms, !Env, !Array, !Ptr, !ST, !BC, !BCSz, !IO).

:- pred eval_term(operator_table::in, string::in, term::in, env::in, env::out,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out, string_table::in, string_table::out,
    array(int)::array_di, array(int)::array_uo,
    int::in, int::out,
    io::di, io::uo) is det.

eval_term(OpTable, BaseDir, Term, !Env, !Array, !Ptr, !ST, !BC, !BCSz, !IO) :-
    (
        Term = identifier(NameId),
        eval_identifier(OpTable, BaseDir, NameId, !Env, !Array, !Ptr, !ST, !BC, !BCSz, !IO)
    ;
        Term = binder(NameId),
        eval_binder(NameId, !Env, !Array, !Ptr)
    ;
        Term = function(Terms),
        eval_function(Terms, !.Env, !Array, !Ptr)
    ;
        Term = generator(Terms),
        eval_generator(OpTable, BaseDir, Terms, !Env, !Array, !Ptr, !ST, !BC, !BCSz, !IO)
    ;
        Term = quoted(T),
        datastack.push(termval(T), !Array, !Ptr)
    ;
        Term = value(V),
        datastack.push(V, !Array, !Ptr)
    ;
        Term = apply_term,
        eval_apply(OpTable, BaseDir, !Env, !Array, !Ptr, !ST, !BC, !BCSz, !IO)
    ).

%-----------------------------------------------------------------------%
% Identifier evaluation
%-----------------------------------------------------------------------%

:- pred eval_identifier(operator_table::in, string::in, string_id::in,
    env::in, env::out,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out,
    string_table::in, string_table::out,
    array(int)::array_di, array(int)::array_uo,
    int::in, int::out,
    io::di, io::uo) is det.

eval_identifier(OpTable, BaseDir, NameId, !Env, !Array, !Ptr, !ST, !BC, !BCSz, !IO) :-
    ( if get_env(NameId, V, !.Env) then
        datastack.push(V, !Array, !Ptr)
    else if map.search(OpTable, NameId, Info) then
        ( if Info ^ oi_operator = op_import then
            % import is special: it can modify Env and ST
            eval_import(OpTable, BaseDir, !Env, !Array, !Ptr, !ST, !BC, !BCSz, !IO)
        else if Info ^ oi_operator = op_restore then
            % restore is special: it replaces the current Env
            eval_restore(!Env, !Array, !Ptr)
        else if Info ^ oi_operator = op_emit then
            % emit is special: it modifies the bytecode store
            eval_emit(!Array, !Ptr, !BC, !BCSz)
        else
            operators.eval_operator(OpTable, !.ST, Info ^ oi_operator, !.Env,
                !Array, !Ptr, !IO)
        )
    else
        throw(undefined_name(NameId))
    ).

%-----------------------------------------------------------------------%
% Binder evaluation
%-----------------------------------------------------------------------%

:- pred eval_binder(string_id::in, env::in, env::out,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.

eval_binder(NameId, !Env, !Array, !Ptr) :-
    datastack.pop("binder", V, !Array, !Ptr),
    set_env(NameId, V, !Env).

%-----------------------------------------------------------------------%
% Function evaluation
%-----------------------------------------------------------------------%

:- pred eval_function(list(term)::in, env::in,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.

% A closure is represented as: closureval(Env, Terms)
eval_function(Terms, Env, !Array, !Ptr) :-
    Closure = closureval(Env, Terms),
    datastack.push(Closure, !Array, !Ptr).

%-----------------------------------------------------------------------%
% Generator evaluation
%-----------------------------------------------------------------------%

:- pred eval_generator(operator_table::in, string::in, list(term)::in,
    env::in, env::out,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out,
    string_table::in, string_table::out,
    array(int)::array_di, array(int)::array_uo,
    int::in, int::out,
    io::di, io::uo) is det.

eval_generator(OpTable, BaseDir, Terms, !Env, !Array, !Ptr, !ST, !BC, !BCSz, !IO) :-
    % Save current stack pointer
    SavedPtr = !.Ptr,
    % Evaluate generator terms (they push values onto the stack)
    eval_terms(OpTable, BaseDir, Terms, !Env, !Array, !Ptr, !ST, !BC, !BCSz, !IO),
    % Extract values pushed by the generator as an array
    datastack.extract_range(!.Array, SavedPtr, !.Ptr, ResultArray),
    % Restore stack pointer and push the result array
    !:Ptr = SavedPtr,
    datastack.push(arrayval(ResultArray), !Array, !Ptr).

%-----------------------------------------------------------------------%
% apply (!)
%-----------------------------------------------------------------------%

:- pred eval_apply(operator_table::in, string::in, env::in, env::out,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out, string_table::in, string_table::out,
    array(int)::array_di, array(int)::array_uo,
    int::in, int::out,
    io::di, io::uo) is det.

eval_apply(OpTable, BaseDir, Env, Env, !Array, !Ptr, !ST, !BC, !BCSz, !IO) :-
    datastack.pop("!", V, !Array, !Ptr),
    ( if V = closureval(ClosureEnv, Terms) then
        % Evaluate with closure's env, then discard env changes (lexical scoping)
        eval_terms(OpTable, BaseDir, Terms, ClosureEnv, _, !Array, !Ptr, !ST, !BC, !BCSz, !IO)
    else if V = termval(identifier(Id)), map.search(OpTable, Id, Info) then
        ( if Info ^ oi_operator = op_import then
            % import is special: it can modify Env (but we discard here like closures)
            eval_import(OpTable, BaseDir, Env, _, !Array, !Ptr, !ST, !BC, !BCSz, !IO)
        else
            % Apply a quoted operator
            operators.eval_operator(OpTable, !.ST, Info ^ oi_operator, Env,
                !Array, !Ptr, !IO)
        )
    else
        throw(type_error("closure or operator", V))
    ).

%-----------------------------------------------------------------------%
% import: ( filename -- ) Load and evaluate a file
%-----------------------------------------------------------------------%

:- pred eval_import(operator_table::in, string::in, env::in, env::out,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out,
    string_table::in, string_table::out,
    array(int)::array_di, array(int)::array_uo,
    int::in, int::out,
    io::di, io::uo) is det.

eval_import(OpTable, BaseDir, !Env, !Array, !Ptr, !ST, !BC, !BCSz, !IO) :-
    datastack.pop("import", V, !Array, !Ptr),
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
                eval_terms(OpTable, NewBaseDir, Terms, !Env, !Array, !Ptr, !ST, !BC, !BCSz, !IO)
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

:- pred eval_restore(env::in, env::out,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.

eval_restore(!Env, !Array, !Ptr) :-
    datastack.pop("restore", V, !Array, !Ptr),
    ( if V = mapval(NewEnv) then
        !:Env = NewEnv
    else
        throw(type_error("map", V))
    ).

%-----------------------------------------------------------------------%
% emit: ( int -- ) Append an integer to the bytecode store
%-----------------------------------------------------------------------%

:- pred eval_emit(
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out,
    array(int)::array_di, array(int)::array_uo,
    int::in, int::out) is det.

eval_emit(!Array, !Ptr, !BC, !BCSz) :-
    datastack.pop("emit", V, !Array, !Ptr),
    ( if V = intval(I) then
        bytecode.emit(I, !BC, !BCSz)
    else
        throw(type_error("int", V))
    ).

%-----------------------------------------------------------------------%
:- end_module eval.
%-----------------------------------------------------------------------%
