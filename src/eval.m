%-----------------------------------------------------------------------%
% eval.m
% Evaluator for the Froth! programming language.
%-----------------------------------------------------------------------%

:- module eval.
:- interface.

:- import_module array.
:- import_module hash_table.
:- import_module io.
:- import_module list.
:- import_module types.

%-----------------------------------------------------------------------%

    % eval_terms(Ctx, Terms, !Env, !Store, FP,
    %            !SP, !Stack, !Pool, !Bytecode, !HashTable, !IO):
    %
    % Evaluate a list of terms, updating the environment and stack.
    %
    % Read-only context:
    %   Ctx - execution context (operator table, base directory)
    %   Terms - list of terms to evaluate
    %
    % Threaded state:
    %   Env - environment map
    %   Store - bundled PP (pool count) and ST (string table)
    %
    % Machine registers:
    %   FP - frame pointer (read-only, for nested VM calls)
    %   SP - stack pointer
    %
    % Threaded memory:
    %   Stack - data stack array
    %   Pool - constant pool
    %   Bytecode - bytecode array (may grow with poke)
    %   HashTable - value to pool index mapping
    %   IO - I/O state
    %
    % Throws eval_error on failure.
    %
:- pred eval_terms(exec_context::in, list(term)::in,
    env::in, env::out,
    eval_store::in, eval_store::out,
    int::in,
    int::in, int::out,
    array(value)::array_di, array(value)::array_uo,
    array(value)::array_di, array(value)::array_uo,
    array(int)::array_di, array(int)::array_uo,
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
:- import_module pool.
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

eval_terms(_, [], !Env, !Store, _, !SP, !Stack, !Pool, !Bytecode, !HashTable, !IO).
eval_terms(Ctx, [Term | Terms], !Env, !Store, FP, !SP, !Stack, !Pool,
        !Bytecode, !HashTable, !IO) :-
    eval_term(Ctx, Term, !Env, !Store, FP, !SP, !Stack, !Pool,
        !Bytecode, !HashTable, !IO),
    eval_terms(Ctx, Terms, !Env, !Store, FP, !SP, !Stack, !Pool,
        !Bytecode, !HashTable, !IO).

:- pred eval_term(exec_context::in, term::in, env::in, env::out,
    eval_store::in, eval_store::out,
    int::in,
    int::in, int::out,
    array(value)::array_di, array(value)::array_uo,
    array(value)::array_di, array(value)::array_uo,
    array(int)::array_di, array(int)::array_uo,
    hash_table(value, int)::hash_table_di, hash_table(value, int)::hash_table_uo,
    io::di, io::uo) is det.

eval_term(Ctx, Term, !Env, !Store, FP, !SP, !Stack, !Pool,
        !Bytecode, !HashTable, !IO) :-
    (
        Term = identifier(NameId),
        eval_identifier(Ctx, NameId, !Env, !Store, FP, !SP, !Stack, !Pool,
            !Bytecode, !HashTable, !IO)
    ;
        Term = binder(NameId),
        eval_binder(NameId, !Env, !Stack, !SP)
    ;
        Term = function(Terms),
        eval_function(Terms, !.Env, !Stack, !SP)
    ;
        Term = generator(Terms),
        eval_generator(Ctx, Terms, !Env, !Store, FP, !SP, !Stack, !Pool,
            !Bytecode, !HashTable, !IO)
    ;
        Term = quoted(T),
        datastack.push(termval(T), !Stack, !SP)
    ;
        Term = value(V),
        datastack.push(V, !Stack, !SP)
    ;
        Term = apply_term,
        eval_apply(Ctx, !Env, !Store, FP, !SP, !Stack, !Pool,
            !Bytecode, !HashTable, !IO)
    ).

%-----------------------------------------------------------------------%
% Identifier evaluation
%-----------------------------------------------------------------------%

:- pred eval_identifier(exec_context::in, string_id::in,
    env::in, env::out,
    eval_store::in, eval_store::out,
    int::in,
    int::in, int::out,
    array(value)::array_di, array(value)::array_uo,
    array(value)::array_di, array(value)::array_uo,
    array(int)::array_di, array(int)::array_uo,
    hash_table(value, int)::hash_table_di, hash_table(value, int)::hash_table_uo,
    io::di, io::uo) is det.

eval_identifier(Ctx, NameId, !Env, !Store, FP, !SP, !Stack, !Pool,
        !Bytecode, !HashTable, !IO) :-
    OpTable = Ctx ^ ec_op_table,
    ( if get_env(NameId, V, !.Env) then
        datastack.push(V, !Stack, !SP)
    else if map.search(OpTable, NameId, Info) then
        ( if Info ^ oi_operator = op_import then
            % import is special: it can modify Env and Store
            eval_import(Ctx, !Env, !Store, FP, !SP, !Stack, !Pool,
                !Bytecode, !HashTable, !IO)
        else if Info ^ oi_operator = op_restore then
            % restore is special: it replaces the current Env
            eval_restore(!Env, !Stack, !SP)
        else if Info ^ oi_operator = op_peek then
            % peek is special: it reads the bytecode store
            eval_peek(!Stack, !SP, !.Bytecode)
        else if Info ^ oi_operator = op_poke then
            % poke is special: it modifies the bytecode store
            eval_poke(!Stack, !SP, !Bytecode)
        else if Info ^ oi_operator = op_ref then
            % ref is special: it modifies the constant pool
            eval_ref(!Store, !Stack, !SP, !Pool, !HashTable)
        else if Info ^ oi_operator = op_deref then
            % deref is special: it reads the constant pool
            eval_deref(!.Store, !Stack, !SP, !.Pool)
        else
            operators.eval_operator(OpTable, !.Store ^ es_string_table,
                Info ^ oi_operator, !.Env, !Stack, !SP, !IO)
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

eval_binder(NameId, !Env, !Stack, !SP) :-
    datastack.pop("binder", V, !Stack, !SP),
    set_env(NameId, V, !Env).

%-----------------------------------------------------------------------%
% Function evaluation
%-----------------------------------------------------------------------%

:- pred eval_function(list(term)::in, env::in,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.

% A closure is represented as: closureval(Env, Terms)
eval_function(Terms, Env, !Stack, !SP) :-
    Closure = closureval(Env, Terms),
    datastack.push(Closure, !Stack, !SP).

%-----------------------------------------------------------------------%
% Generator evaluation
%-----------------------------------------------------------------------%

:- pred eval_generator(exec_context::in, list(term)::in,
    env::in, env::out,
    eval_store::in, eval_store::out,
    int::in,
    int::in, int::out,
    array(value)::array_di, array(value)::array_uo,
    array(value)::array_di, array(value)::array_uo,
    array(int)::array_di, array(int)::array_uo,
    hash_table(value, int)::hash_table_di, hash_table(value, int)::hash_table_uo,
    io::di, io::uo) is det.

eval_generator(Ctx, Terms, !Env, !Store, FP, !SP, !Stack, !Pool,
        !Bytecode, !HashTable, !IO) :-
    % Save current stack pointer
    SavedSP = !.SP,
    % Evaluate generator terms (they push values onto the stack)
    eval_terms(Ctx, Terms, !Env, !Store, FP, !SP, !Stack, !Pool,
        !Bytecode, !HashTable, !IO),
    % Extract values pushed by the generator as an array
    datastack.extract_range(!.Stack, SavedSP, !.SP, ResultArray),
    % Restore stack pointer and push the result array
    !:SP = SavedSP,
    datastack.push(arrayval(ResultArray), !Stack, !SP).

%-----------------------------------------------------------------------%
% apply (!)
%-----------------------------------------------------------------------%

:- pred eval_apply(exec_context::in, env::in, env::out,
    eval_store::in, eval_store::out,
    int::in,
    int::in, int::out,
    array(value)::array_di, array(value)::array_uo,
    array(value)::array_di, array(value)::array_uo,
    array(int)::array_di, array(int)::array_uo,
    hash_table(value, int)::hash_table_di, hash_table(value, int)::hash_table_uo,
    io::di, io::uo) is det.

eval_apply(Ctx, Env, Env, !Store, FP, !SP, !Stack, !Pool,
        !Bytecode, !HashTable, !IO) :-
    datastack.pop("!", V, !Stack, !SP),
    ( if V = closureval(ClosureEnv, Terms) then
        % Evaluate with closure's env, then discard env changes (lexical scoping)
        eval_terms(Ctx, Terms, ClosureEnv, _, !Store, FP, !SP, !Stack, !Pool,
            !Bytecode, !HashTable, !IO)
    else if V = bytecodeval(Context, CodeAddr) then
        % Execute bytecode closure via VM
        % RP=-1 means return to eval_apply (sentinel value)
        % FP is passed through from caller (set at top level or by outer VM)
        % GenStack starts empty (no active generators)
        vm.run(Ctx, Env, Context, [], !Store, CodeAddr, -1, FP, !SP,
            !Stack, !Pool, !Bytecode, !HashTable, !IO)
    else
        throw(type_error("closure", V))
    ).

%-----------------------------------------------------------------------%
% import: ( filename -- ) Load and evaluate a file
%-----------------------------------------------------------------------%

:- pred eval_import(exec_context::in, env::in, env::out,
    eval_store::in, eval_store::out,
    int::in,
    int::in, int::out,
    array(value)::array_di, array(value)::array_uo,
    array(value)::array_di, array(value)::array_uo,
    array(int)::array_di, array(int)::array_uo,
    hash_table(value, int)::hash_table_di, hash_table(value, int)::hash_table_uo,
    io::di, io::uo) is det.

eval_import(Ctx, !Env, !Store, FP, !SP, !Stack, !Pool,
        !Bytecode, !HashTable, !IO) :-
    BaseDir = Ctx ^ ec_base_dir,
    OpTable = Ctx ^ ec_op_table,
    datastack.pop("import", V, !Stack, !SP),
    RelFilename = values.value_to_string(!.Store ^ es_string_table, V),
    % Resolve relative paths using BaseDir
    ( if dir.path_name_is_absolute(RelFilename) then
        Filename = RelFilename
    else
        Filename = dir.make_path_name(BaseDir, RelFilename)
    ),
    NewBaseDir = dir.dirname(Filename),
    NewCtx = exec_context(OpTable, NewBaseDir),
    io.read_named_file_as_string(Filename, ReadResult, !IO),
    (
        ReadResult = ok(Content),
        ST0 = !.Store ^ es_string_table,
        lexer.tokenize(Content, ST0, LexResult),
        (
            LexResult = ok(Tokens, ST1),
            !:Store = !.Store ^ es_string_table := ST1,
            parser.parse(Tokens, ParseResult),
            (
                ParseResult = ok(Terms),
                eval_terms(NewCtx, Terms, !Env, !Store, FP, !SP, !Stack, !Pool,
                    !Bytecode, !HashTable, !IO)
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

eval_restore(!Env, !Stack, !SP) :-
    datastack.pop("restore", V, !Stack, !SP),
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

eval_peek(!Stack, !SP, Bytecode) :-
    datastack.pop("peek", V, !Stack, !SP),
    ( if V = intval(Addr) then
        Value = bytecode.peek(Addr, Bytecode),
        datastack.push(intval(Value), !Stack, !SP)
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

eval_poke(!Stack, !SP, !Bytecode) :-
    datastack.pop("poke", AddrVal, !Stack, !SP),
    datastack.pop("poke", ValueVal, !Stack, !SP),
    ( if AddrVal = intval(Addr), ValueVal = intval(Value) then
        bytecode.poke(Addr, Value, !Bytecode)
    else if AddrVal = intval(_) then
        throw(type_error("int", ValueVal))
    else
        throw(type_error("int", AddrVal))
    ).

%-----------------------------------------------------------------------%
% ref: ( value -- int ) Store value in constant pool, return index
%-----------------------------------------------------------------------%

:- pred eval_ref(eval_store::in, eval_store::out,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out,
    array(value)::array_di, array(value)::array_uo,
    hash_table(value, int)::hash_table_di,
    hash_table(value, int)::hash_table_uo) is det.

eval_ref(!Store, !Stack, !SP, !Pool, !HashTable) :-
    datastack.pop("ref", V, !Stack, !SP),
    pool.deep_ref(V, Idx, !Pool, !HashTable),
    % Update pool count to match pool size (deep_ref may have added multiple entries)
    !:Store = !.Store ^ es_pool_count := array.size(!.Pool),
    datastack.push(intval(Idx), !Stack, !SP).

%-----------------------------------------------------------------------%
% deref: ( int -- value ) Retrieve value from constant pool
%-----------------------------------------------------------------------%

:- pred eval_deref(eval_store::in,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out,
    array(value)::in) is det.

eval_deref(Store, !Stack, !SP, Pool) :-
    datastack.pop("deref", V, !Stack, !SP),
    PP = Store ^ es_pool_count,
    ( if V = intval(Idx) then
        ( if Idx >= 0, Idx < PP then
            array.lookup(Pool, Idx, Value),
            datastack.push(Value, !Stack, !SP)
        else
            throw(index_out_of_bounds(Idx, PP))
        )
    else
        throw(type_error("int", V))
    ).

%-----------------------------------------------------------------------%
:- end_module eval.
%-----------------------------------------------------------------------%
