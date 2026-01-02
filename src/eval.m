%-----------------------------------------------------------------------%
% eval.m
% Evaluator for the Froth programming language.
%-----------------------------------------------------------------------%

:- module eval.
:- interface.

:- import_module array.
:- import_module hash_table.
:- import_module io.
:- import_module list.
:- import_module types.

%-----------------------------------------------------------------------%

    % eval_terms(OpTable, BaseDir, Terms, !Env, !Stack, !StackPtr,
    %            !ST, !Bytecode, !Pool, !HT, !IO):
    % Evaluate a list of terms, updating the environment and stack.
    % OpTable is read-only (fixed at engine startup).
    % BaseDir is used to resolve relative paths in imports.
    % StringTable is threaded through (may grow with dynamic imports).
    % Bytecode array is threaded through (may grow with poke).
    % Pool and HT are the constant pool and its deduplication hash table.
    % Stack is represented as an array and pointer (for efficient operations).
    % Throws eval_error on failure.
    %
:- pred eval_terms(operator_table::in, string::in, list(term)::in,
    env::in, env::out,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out,
    string_table::in, string_table::out,
    array(int)::array_di, array(int)::array_uo,
    array(value)::array_di, array(value)::array_uo,
    hash_table(value, int)::hash_table_di, hash_table(value, int)::hash_table_uo,
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
:- import_module values.
:- import_module vm.

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

eval_terms(_, _, [], !Env, !Array, !Ptr, !ST, !BC, !Pool, !HT, !IO).
eval_terms(OpTable, BaseDir, [Term | Terms], !Env, !Array, !Ptr, !ST, !BC,
        !Pool, !HT, !IO) :-
    eval_term(OpTable, BaseDir, Term, !Env, !Array, !Ptr, !ST, !BC, !Pool, !HT, !IO),
    eval_terms(OpTable, BaseDir, Terms, !Env, !Array, !Ptr, !ST, !BC, !Pool, !HT, !IO).

:- pred eval_term(operator_table::in, string::in, term::in, env::in, env::out,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out, string_table::in, string_table::out,
    array(int)::array_di, array(int)::array_uo,
    array(value)::array_di, array(value)::array_uo,
    hash_table(value, int)::hash_table_di, hash_table(value, int)::hash_table_uo,
    io::di, io::uo) is det.

eval_term(OpTable, BaseDir, Term, !Env, !Array, !Ptr, !ST, !BC, !Pool, !HT, !IO) :-
    (
        Term = identifier(NameId),
        eval_identifier(OpTable, BaseDir, NameId, !Env, !Array, !Ptr, !ST, !BC,
            !Pool, !HT, !IO)
    ;
        Term = binder(NameId),
        eval_binder(NameId, !Env, !Array, !Ptr)
    ;
        Term = function(Terms),
        eval_function(Terms, !.Env, !Array, !Ptr)
    ;
        Term = generator(Terms),
        eval_generator(OpTable, BaseDir, Terms, !Env, !Array, !Ptr, !ST, !BC,
            !Pool, !HT, !IO)
    ;
        Term = quoted(T),
        datastack.push(termval(T), !Array, !Ptr)
    ;
        Term = value(V),
        datastack.push(V, !Array, !Ptr)
    ;
        Term = apply_term,
        eval_apply(OpTable, BaseDir, !Env, !Array, !Ptr, !ST, !BC, !Pool, !HT, !IO)
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
    array(value)::array_di, array(value)::array_uo,
    hash_table(value, int)::hash_table_di, hash_table(value, int)::hash_table_uo,
    io::di, io::uo) is det.

eval_identifier(OpTable, BaseDir, NameId, !Env, !Array, !Ptr, !ST, !BC,
        !Pool, !HT, !IO) :-
    ( if get_env(NameId, V, !.Env) then
        datastack.push(V, !Array, !Ptr)
    else if map.search(OpTable, NameId, Info) then
        ( if Info ^ oi_operator = op_import then
            % import is special: it can modify Env and ST
            eval_import(OpTable, BaseDir, !Env, !Array, !Ptr, !ST, !BC,
                !Pool, !HT, !IO)
        else if Info ^ oi_operator = op_restore then
            % restore is special: it replaces the current Env
            eval_restore(!Env, !Array, !Ptr)
        else if Info ^ oi_operator = op_peek then
            % peek is special: it reads the bytecode store
            eval_peek(!Array, !Ptr, !.BC)
        else if Info ^ oi_operator = op_poke then
            % poke is special: it modifies the bytecode store
            eval_poke(!Array, !Ptr, !BC)
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
    array(value)::array_di, array(value)::array_uo,
    hash_table(value, int)::hash_table_di, hash_table(value, int)::hash_table_uo,
    io::di, io::uo) is det.

eval_generator(OpTable, BaseDir, Terms, !Env, !Array, !Ptr, !ST, !BC,
        !Pool, !HT, !IO) :-
    % Save current stack pointer
    SavedPtr = !.Ptr,
    % Evaluate generator terms (they push values onto the stack)
    eval_terms(OpTable, BaseDir, Terms, !Env, !Array, !Ptr, !ST, !BC,
        !Pool, !HT, !IO),
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
    array(value)::array_di, array(value)::array_uo,
    hash_table(value, int)::hash_table_di, hash_table(value, int)::hash_table_uo,
    io::di, io::uo) is det.

eval_apply(OpTable, BaseDir, Env, Env, !Array, !Ptr, !ST, !BC, !Pool, !HT, !IO) :-
    datastack.pop("!", V, !Array, !Ptr),
    ( if V = closureval(ClosureEnv, Terms) then
        % Evaluate with closure's env, then discard env changes (lexical scoping)
        eval_terms(OpTable, BaseDir, Terms, ClosureEnv, _, !Array, !Ptr, !ST, !BC,
            !Pool, !HT, !IO)
    else if V = bytecodeval(Context, CodeAddr) then
        % Execute bytecode closure via VM
        % RP=-1 means return to eval_apply (sentinel value)
        % FP starts at top of stack array (frame grows downward)
        % GenStack starts empty (no active generators)
        FP = array.size(!.Array),
        PP0 = array.size(!.Pool),
        Store0 = vm.vm_store(PP0, !.ST),
        vm.run(CodeAddr, -1, FP, Context, [], !Ptr, Store0, Store,
            OpTable, Env, !BC, !Array, !Pool, !HT, !IO),
        !:ST = Store ^ vm.vs_string_table
    else
        throw(type_error("closure", V))
    ).

%-----------------------------------------------------------------------%
% import: ( filename -- ) Load and evaluate a file
%-----------------------------------------------------------------------%

:- pred eval_import(operator_table::in, string::in, env::in, env::out,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out,
    string_table::in, string_table::out,
    array(int)::array_di, array(int)::array_uo,
    array(value)::array_di, array(value)::array_uo,
    hash_table(value, int)::hash_table_di, hash_table(value, int)::hash_table_uo,
    io::di, io::uo) is det.

eval_import(OpTable, BaseDir, !Env, !Array, !Ptr, !ST, !BC, !Pool, !HT, !IO) :-
    datastack.pop("import", V, !Array, !Ptr),
    RelFilename = values.value_to_string(!.ST, V),
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
                eval_terms(OpTable, NewBaseDir, Terms, !Env, !Array, !Ptr, !ST, !BC,
                    !Pool, !HT, !IO)
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
% peek: ( addr -- int ) Read bytecode at address
%-----------------------------------------------------------------------%

:- pred eval_peek(
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out,
    array(int)::in) is det.

eval_peek(!Array, !Ptr, BC) :-
    datastack.pop("peek", V, !Array, !Ptr),
    ( if V = intval(Addr) then
        Value = bytecode.peek(Addr, BC),
        datastack.push(intval(Value), !Array, !Ptr)
    else
        throw(type_error("int", V))
    ).

%-----------------------------------------------------------------------%
% poke: ( value addr -- ) Write value to bytecode address
%-----------------------------------------------------------------------%

:- pred eval_poke(
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out,
    array(int)::array_di, array(int)::array_uo) is det.

eval_poke(!Array, !Ptr, !BC) :-
    datastack.pop("poke", AddrVal, !Array, !Ptr),
    datastack.pop("poke", ValueVal, !Array, !Ptr),
    ( if AddrVal = intval(Addr), ValueVal = intval(Value) then
        bytecode.poke(Addr, Value, !BC)
    else if AddrVal = intval(_) then
        throw(type_error("int", ValueVal))
    else
        throw(type_error("int", AddrVal))
    ).

%-----------------------------------------------------------------------%
:- end_module eval.
%-----------------------------------------------------------------------%
