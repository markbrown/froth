%-----------------------------------------------------------------------%
% operators.m
% Operator definitions for the Froth programming language.
%-----------------------------------------------------------------------%

:- module operators.
:- interface.

:- import_module array.
:- import_module io.
:- import_module types.

%-----------------------------------------------------------------------%

    % init_operators(!ST, OpTable):
    % Initialize the operator table by interning all operator names.
    % Returns the operator table and updated string table.
    %
:- pred init_operators(string_table::in, string_table::out,
    operator_table::out) is det.

    % operator(Name, Op):
    % Map a name to an operator.
    %
:- pred operator(string::in, operator::out) is semidet.

    % eval_operator(OpTable, ST, Op, Env, !StackArray, !StackPtr, !IO):
    % Execute an operator. OpTable and ST are read-only.
    % Env is read-only (for the env operator).
    %
:- pred eval_operator(operator_table::in, string_table::in, operator::in,
    env::in, array(value)::array_di, array(value)::array_uo,
    int::in, int::out, io::di, io::uo) is det.

    % Individual operator implementations.
    % All operators take the stack as array + pointer pair.
    %
:- pred operator_print(string_table::in,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out, io::di, io::uo) is det.
:- pred operator_env(env::in,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_add(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_sub(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_mul(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_gt(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_lt(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_gte(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_lte(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_get(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_length(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_eq(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_ite(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_nil(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_cons(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_fst(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_snd(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_write(string_table::in,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out, io::di, io::uo) is det.
:- pred operator_fwrite(string_table::in,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out, io::di, io::uo) is det.
:- pred operator_empty(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_keys(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_store(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_in(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_delete(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_is_int(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_is_string(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_is_array(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_is_map(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_is_nil(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_is_cons(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_is_ident(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_is_binder(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_is_func(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_is_gen(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_is_quote(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_is_apply(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_is_value(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_unwrap(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_intern(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_id_to_string(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_id_to_ident(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_id_to_binder(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_is_operator(operator_table::in,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_arity_op(operator_table::in,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_stack(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_time(array(value)::array_di, array(value)::array_uo,
    int::in, int::out, io::di, io::uo) is det.
:- pred operator_close(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_closure_env(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_closure_body(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_is_closure(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.

    % value_to_string(ST, Value) = String:
    % Convert a value to its string representation (for print/import).
    %
:- func value_to_string(string_table, value) = string.

    % operator_to_int(Op) = Int:
    % Convert an operator to its integer representation for bytecode.
    %
:- func operator_to_int(operator) = int.

    % int_to_operator(Int) = Op:
    % Convert an integer to an operator. Fails if out of range.
    %
:- pred int_to_operator(int::in, operator::out) is semidet.

    % num_operators:
    % The total number of operators.
    %
:- func num_operators = int.

%-----------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module datastack.
:- import_module bool.
:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module time.

%-----------------------------------------------------------------------%

operator("print", op_print).
operator("env", op_env).
operator("+", op_add).
operator("-", op_sub).
operator("*", op_mul).
operator(">", op_gt).
operator("<", op_lt).
operator(">=", op_gte).
operator("<=", op_lte).
operator("@", op_get).
operator("#", op_length).
operator("=", op_eq).
operator("?", op_ite).
operator(".", op_nil).
operator(",", op_cons).
operator("fst", op_fst).
operator("snd", op_snd).
operator("write", op_write).
operator("fwrite", op_fwrite).
operator("$", op_empty).
operator("keys", op_keys).
operator(":", op_store).
operator("in", op_in).
operator("delete", op_delete).
operator("isInt", op_is_int).
operator("isString", op_is_string).
operator("isArray", op_is_array).
operator("isMap", op_is_map).
operator("isNil", op_is_nil).
operator("isCons", op_is_cons).
operator("isIdent", op_is_ident).
operator("isBinder", op_is_binder).
operator("isFunc", op_is_func).
operator("isGen", op_is_gen).
operator("isQuote", op_is_quote).
operator("isApply", op_is_apply).
operator("isValue", op_is_value).
operator("unwrap", op_unwrap).
operator("intern", op_intern).
operator("idToString", op_id_to_string).
operator("idToIdent", op_id_to_ident).
operator("idToBinder", op_id_to_binder).
operator("isOperator", op_is_operator).
operator("arity", op_arity).
operator("stack", op_stack).
operator("import", op_import).
operator("time", op_time).
operator("restore", op_restore).
operator("close", op_close).
operator("closureEnv", op_closure_env).
operator("closureBody", op_closure_body).
operator("isClosure", op_is_closure).
operator("emit", op_emit).

%-----------------------------------------------------------------------%
% Operator arity (number of values popped from stack)
%-----------------------------------------------------------------------%

:- func operator_arity(operator) = int.

operator_arity(op_print) = 1.
operator_arity(op_env) = 0.
operator_arity(op_add) = 2.
operator_arity(op_sub) = 2.
operator_arity(op_mul) = 2.
operator_arity(op_gt) = 2.
operator_arity(op_lt) = 2.
operator_arity(op_gte) = 2.
operator_arity(op_lte) = 2.
operator_arity(op_get) = 2.
operator_arity(op_length) = 1.
operator_arity(op_eq) = 2.
operator_arity(op_ite) = 3.
operator_arity(op_nil) = 0.
operator_arity(op_cons) = 2.
operator_arity(op_fst) = 1.
operator_arity(op_snd) = 1.
operator_arity(op_write) = 1.
operator_arity(op_fwrite) = 2.
operator_arity(op_empty) = 0.
operator_arity(op_keys) = 1.
operator_arity(op_store) = 3.
operator_arity(op_in) = 2.
operator_arity(op_delete) = 2.
operator_arity(op_is_int) = 1.
operator_arity(op_is_string) = 1.
operator_arity(op_is_array) = 1.
operator_arity(op_is_map) = 1.
operator_arity(op_is_nil) = 1.
operator_arity(op_is_cons) = 1.
operator_arity(op_is_ident) = 1.
operator_arity(op_is_binder) = 1.
operator_arity(op_is_func) = 1.
operator_arity(op_is_gen) = 1.
operator_arity(op_is_quote) = 1.
operator_arity(op_is_apply) = 1.
operator_arity(op_is_value) = 1.
operator_arity(op_unwrap) = 1.
operator_arity(op_intern) = 1.
operator_arity(op_id_to_string) = 1.
operator_arity(op_id_to_ident) = 1.
operator_arity(op_id_to_binder) = 1.
operator_arity(op_is_operator) = 1.
operator_arity(op_arity) = 1.
operator_arity(op_stack) = 0.
operator_arity(op_import) = 1.
operator_arity(op_time) = 0.
operator_arity(op_restore) = 1.
operator_arity(op_close) = 2.
operator_arity(op_closure_env) = 1.
operator_arity(op_closure_body) = 1.
operator_arity(op_is_closure) = 1.
operator_arity(op_emit) = 1.

%-----------------------------------------------------------------------%
% Operator to/from integer conversion for bytecode
% NOTE: These numbers must be kept in sync with lib/bytecode.froth
%-----------------------------------------------------------------------%

num_operators = 53.

operator_to_int(op_print) = 0.
operator_to_int(op_env) = 1.
operator_to_int(op_add) = 2.
operator_to_int(op_sub) = 3.
operator_to_int(op_mul) = 4.
operator_to_int(op_gt) = 5.
operator_to_int(op_lt) = 6.
operator_to_int(op_gte) = 7.
operator_to_int(op_lte) = 8.
operator_to_int(op_get) = 9.
operator_to_int(op_length) = 10.
operator_to_int(op_eq) = 11.
operator_to_int(op_ite) = 12.
operator_to_int(op_nil) = 13.
operator_to_int(op_cons) = 14.
operator_to_int(op_fst) = 15.
operator_to_int(op_snd) = 16.
operator_to_int(op_write) = 17.
operator_to_int(op_fwrite) = 18.
operator_to_int(op_empty) = 19.
operator_to_int(op_keys) = 20.
operator_to_int(op_store) = 21.
operator_to_int(op_in) = 22.
operator_to_int(op_delete) = 23.
operator_to_int(op_is_int) = 24.
operator_to_int(op_is_string) = 25.
operator_to_int(op_is_array) = 26.
operator_to_int(op_is_map) = 27.
operator_to_int(op_is_nil) = 28.
operator_to_int(op_is_cons) = 29.
operator_to_int(op_is_ident) = 30.
operator_to_int(op_is_binder) = 31.
operator_to_int(op_is_func) = 32.
operator_to_int(op_is_gen) = 33.
operator_to_int(op_is_quote) = 34.
operator_to_int(op_is_apply) = 35.
operator_to_int(op_is_value) = 36.
operator_to_int(op_unwrap) = 37.
operator_to_int(op_intern) = 38.
operator_to_int(op_id_to_string) = 39.
operator_to_int(op_id_to_ident) = 40.
operator_to_int(op_id_to_binder) = 41.
operator_to_int(op_is_operator) = 42.
operator_to_int(op_arity) = 43.
operator_to_int(op_stack) = 44.
operator_to_int(op_import) = 45.
operator_to_int(op_time) = 46.
operator_to_int(op_restore) = 47.
operator_to_int(op_close) = 48.
operator_to_int(op_closure_env) = 49.
operator_to_int(op_closure_body) = 50.
operator_to_int(op_is_closure) = 51.
operator_to_int(op_emit) = 52.

int_to_operator(0, op_print).
int_to_operator(1, op_env).
int_to_operator(2, op_add).
int_to_operator(3, op_sub).
int_to_operator(4, op_mul).
int_to_operator(5, op_gt).
int_to_operator(6, op_lt).
int_to_operator(7, op_gte).
int_to_operator(8, op_lte).
int_to_operator(9, op_get).
int_to_operator(10, op_length).
int_to_operator(11, op_eq).
int_to_operator(12, op_ite).
int_to_operator(13, op_nil).
int_to_operator(14, op_cons).
int_to_operator(15, op_fst).
int_to_operator(16, op_snd).
int_to_operator(17, op_write).
int_to_operator(18, op_fwrite).
int_to_operator(19, op_empty).
int_to_operator(20, op_keys).
int_to_operator(21, op_store).
int_to_operator(22, op_in).
int_to_operator(23, op_delete).
int_to_operator(24, op_is_int).
int_to_operator(25, op_is_string).
int_to_operator(26, op_is_array).
int_to_operator(27, op_is_map).
int_to_operator(28, op_is_nil).
int_to_operator(29, op_is_cons).
int_to_operator(30, op_is_ident).
int_to_operator(31, op_is_binder).
int_to_operator(32, op_is_func).
int_to_operator(33, op_is_gen).
int_to_operator(34, op_is_quote).
int_to_operator(35, op_is_apply).
int_to_operator(36, op_is_value).
int_to_operator(37, op_unwrap).
int_to_operator(38, op_intern).
int_to_operator(39, op_id_to_string).
int_to_operator(40, op_id_to_ident).
int_to_operator(41, op_id_to_binder).
int_to_operator(42, op_is_operator).
int_to_operator(43, op_arity).
int_to_operator(44, op_stack).
int_to_operator(45, op_import).
int_to_operator(46, op_time).
int_to_operator(47, op_restore).
int_to_operator(48, op_close).
int_to_operator(49, op_closure_env).
int_to_operator(50, op_closure_body).
int_to_operator(51, op_is_closure).
int_to_operator(52, op_emit).

%-----------------------------------------------------------------------%
% init_operators: intern all operator names and build the operator table
%-----------------------------------------------------------------------%

init_operators(!ST, OpTable) :-
    OpNames = [
        "print", "env",
        "+", "-", "*",
        ">", "<", ">=", "<=",
        "@", "#", "=", "?",
        ".", ",", "fst", "snd",
        "write", "fwrite",
        "$", "keys", ":", "in", "delete",
        "isInt", "isString", "isArray", "isMap", "isNil", "isCons",
        "isIdent", "isBinder", "isFunc", "isGen", "isQuote", "isApply",
        "isValue", "unwrap", "intern",
        "idToString", "idToIdent", "idToBinder", "isOperator", "arity",
        "stack", "import", "time", "restore",
        "close", "closureEnv", "closureBody", "isClosure",
        "emit"
    ],
    list.foldl2(intern_operator, OpNames, map.init, OpTable, !ST).

:- pred intern_operator(string::in, operator_table::in, operator_table::out,
    string_table::in, string_table::out) is det.

intern_operator(Name, !OpTable, !ST) :-
    ( if operator(Name, Op) then
        intern_string(Name, Id, !ST),
        Arity = operator_arity(Op),
        map.det_insert(Id, operator_info(Op, Arity), !OpTable)
    else
        unexpected($pred, "unknown operator: " ++ Name)
    ).

%-----------------------------------------------------------------------%
% eval_operator: dispatch to operator implementation
%-----------------------------------------------------------------------%

eval_operator(OpTable, ST, Op, Env, !Array, !Ptr, !IO) :-
    (
        Op = op_print,
        operator_print(ST, !Array, !Ptr, !IO)
    ;
        Op = op_env,
        operator_env(Env, !Array, !Ptr)
    ;
        Op = op_add,
        operator_add(!Array, !Ptr)
    ;
        Op = op_sub,
        operator_sub(!Array, !Ptr)
    ;
        Op = op_mul,
        operator_mul(!Array, !Ptr)
    ;
        Op = op_gt,
        operator_gt(!Array, !Ptr)
    ;
        Op = op_lt,
        operator_lt(!Array, !Ptr)
    ;
        Op = op_gte,
        operator_gte(!Array, !Ptr)
    ;
        Op = op_lte,
        operator_lte(!Array, !Ptr)
    ;
        Op = op_get,
        operator_get(!Array, !Ptr)
    ;
        Op = op_length,
        operator_length(!Array, !Ptr)
    ;
        Op = op_eq,
        operator_eq(!Array, !Ptr)
    ;
        Op = op_ite,
        operator_ite(!Array, !Ptr)
    ;
        Op = op_nil,
        operator_nil(!Array, !Ptr)
    ;
        Op = op_cons,
        operator_cons(!Array, !Ptr)
    ;
        Op = op_fst,
        operator_fst(!Array, !Ptr)
    ;
        Op = op_snd,
        operator_snd(!Array, !Ptr)
    ;
        Op = op_write,
        operator_write(ST, !Array, !Ptr, !IO)
    ;
        Op = op_fwrite,
        operator_fwrite(ST, !Array, !Ptr, !IO)
    ;
        Op = op_empty,
        operator_empty(!Array, !Ptr)
    ;
        Op = op_keys,
        operator_keys(!Array, !Ptr)
    ;
        Op = op_store,
        operator_store(!Array, !Ptr)
    ;
        Op = op_in,
        operator_in(!Array, !Ptr)
    ;
        Op = op_delete,
        operator_delete(!Array, !Ptr)
    ;
        Op = op_is_int,
        operator_is_int(!Array, !Ptr)
    ;
        Op = op_is_string,
        operator_is_string(!Array, !Ptr)
    ;
        Op = op_is_array,
        operator_is_array(!Array, !Ptr)
    ;
        Op = op_is_map,
        operator_is_map(!Array, !Ptr)
    ;
        Op = op_is_nil,
        operator_is_nil(!Array, !Ptr)
    ;
        Op = op_is_cons,
        operator_is_cons(!Array, !Ptr)
    ;
        Op = op_is_ident,
        operator_is_ident(!Array, !Ptr)
    ;
        Op = op_is_binder,
        operator_is_binder(!Array, !Ptr)
    ;
        Op = op_is_func,
        operator_is_func(!Array, !Ptr)
    ;
        Op = op_is_gen,
        operator_is_gen(!Array, !Ptr)
    ;
        Op = op_is_quote,
        operator_is_quote(!Array, !Ptr)
    ;
        Op = op_is_apply,
        operator_is_apply(!Array, !Ptr)
    ;
        Op = op_is_value,
        operator_is_value(!Array, !Ptr)
    ;
        Op = op_unwrap,
        operator_unwrap(!Array, !Ptr)
    ;
        Op = op_intern,
        operator_intern(!Array, !Ptr)
    ;
        Op = op_id_to_string,
        operator_id_to_string(!Array, !Ptr)
    ;
        Op = op_id_to_ident,
        operator_id_to_ident(!Array, !Ptr)
    ;
        Op = op_id_to_binder,
        operator_id_to_binder(!Array, !Ptr)
    ;
        Op = op_is_operator,
        operator_is_operator(OpTable, !Array, !Ptr)
    ;
        Op = op_arity,
        operator_arity_op(OpTable, !Array, !Ptr)
    ;
        Op = op_stack,
        operator_stack(!Array, !Ptr)
    ;
        Op = op_import,
        % import is handled specially in eval.m, should not reach here
        unexpected($pred, "import should be handled in eval.m")
    ;
        Op = op_time,
        operator_time(!Array, !Ptr, !IO)
    ;
        Op = op_restore,
        % restore is handled specially in eval.m, should not reach here
        unexpected($pred, "restore should be handled in eval.m")
    ;
        Op = op_close,
        operator_close(!Array, !Ptr)
    ;
        Op = op_closure_env,
        operator_closure_env(!Array, !Ptr)
    ;
        Op = op_closure_body,
        operator_closure_body(!Array, !Ptr)
    ;
        Op = op_is_closure,
        operator_is_closure(!Array, !Ptr)
    ;
        Op = op_emit,
        % emit is handled specially in eval.m, should not reach here
        unexpected($pred, "emit should be handled in eval.m")
    ).

%-----------------------------------------------------------------------%
% print: ( a -- ) Pop and print a value
%-----------------------------------------------------------------------%

operator_print(ST, !Array, !Ptr, !IO) :-
    datastack.pop("print", V, !Array, !Ptr),
    io.write_string(value_to_string(ST, V), !IO).

%-----------------------------------------------------------------------%
% value_to_string: convert a value to its string representation
%-----------------------------------------------------------------------%

value_to_string(_, intval(I)) = int_to_string(I).
value_to_string(ST, stringval(StrId)) = lookup_string(ST, StrId).
value_to_string(ST, arrayval(A)) = String :-
    array.to_list(A, List),
    Strings = list.map(value_to_string(ST), List),
    String = string.append_list(Strings).
value_to_string(_, mapval(M)) = string.format("<map:%d>", [i(map.count(M))]).
value_to_string(ST, termval(T)) = term_to_string(ST, T).
value_to_string(_, nilval) = ".".
value_to_string(ST, consval(H, T)) =
    "(" ++ value_to_string(ST, H) ++ "," ++ value_to_string(ST, T) ++ ")".
value_to_string(ST, closureval(_, Body)) =
    "<closure:" ++ terms_to_string(ST, Body) ++ ">".
value_to_string(_, bytecodeval(_, Addr)) =
    "<bytecode:" ++ int_to_string(Addr) ++ ">".

:- func term_to_string(string_table, term) = string.

term_to_string(ST, identifier(NameId)) = lookup_string(ST, NameId).
term_to_string(ST, binder(NameId)) = "/" ++ lookup_string(ST, NameId).
term_to_string(ST, function(Terms)) = "{ " ++ terms_to_string(ST, Terms) ++ "}".
term_to_string(ST, generator(Terms)) = "[ " ++ terms_to_string(ST, Terms) ++ "]".
term_to_string(ST, quoted(T)) = "'" ++ term_to_string(ST, T).
term_to_string(ST, value(V)) = value_to_string(ST, V).
term_to_string(_, apply_term) = "!".

:- func terms_to_string(string_table, list(term)) = string.

terms_to_string(_, []) = "".
terms_to_string(ST, [T | Ts]) =
    term_to_string(ST, T) ++ " " ++ terms_to_string(ST, Ts).

%-----------------------------------------------------------------------%
% env: ( -- map ) Push the current environment as a map
%-----------------------------------------------------------------------%

operator_env(Env, !Array, !Ptr) :-
    datastack.push(mapval(Env), !Array, !Ptr).

%-----------------------------------------------------------------------%
% +: ( int int -- int ) Add two integers
%-----------------------------------------------------------------------%

operator_add(!Array, !Ptr) :-
    datastack.pop("+", V1, !Array, !Ptr),
    datastack.pop("+", V2, !Array, !Ptr),
    ( if V1 = intval(I1), V2 = intval(I2) then
        datastack.push(intval(I1 + I2), !Array, !Ptr)
    else if V1 = intval(_) then
        throw(type_error("int", V2))
    else
        throw(type_error("int", V1))
    ).

%-----------------------------------------------------------------------%
% -: ( int int -- int ) Subtract: a b - computes a - b
%-----------------------------------------------------------------------%

operator_sub(!Array, !Ptr) :-
    datastack.pop("-", V1, !Array, !Ptr),
    datastack.pop("-", V2, !Array, !Ptr),
    ( if V1 = intval(I1), V2 = intval(I2) then
        datastack.push(intval(I2 - I1), !Array, !Ptr)
    else if V1 = intval(_) then
        throw(type_error("int", V2))
    else
        throw(type_error("int", V1))
    ).

%-----------------------------------------------------------------------%
% *: ( int int -- int ) Multiply two integers
%-----------------------------------------------------------------------%

operator_mul(!Array, !Ptr) :-
    datastack.pop("*", V1, !Array, !Ptr),
    datastack.pop("*", V2, !Array, !Ptr),
    ( if V1 = intval(I1), V2 = intval(I2) then
        datastack.push(intval(I1 * I2), !Array, !Ptr)
    else if V1 = intval(_) then
        throw(type_error("int", V2))
    else
        throw(type_error("int", V1))
    ).

%-----------------------------------------------------------------------%
% >: ( int int -- int ) Greater than: a b > is 0 if a > b, else 1
%-----------------------------------------------------------------------%

operator_gt(!Array, !Ptr) :-
    datastack.pop(">", V1, !Array, !Ptr),
    datastack.pop(">", V2, !Array, !Ptr),
    ( if V1 = intval(I1), V2 = intval(I2) then
        ( if I2 > I1 then datastack.push(intval(0), !Array, !Ptr)
        else datastack.push(intval(1), !Array, !Ptr)
        )
    else if V1 = intval(_) then
        throw(type_error("int", V2))
    else
        throw(type_error("int", V1))
    ).

%-----------------------------------------------------------------------%
% <: ( int int -- int ) Less than: a b < is 0 if a < b, else 1
%-----------------------------------------------------------------------%

operator_lt(!Array, !Ptr) :-
    datastack.pop("<", V1, !Array, !Ptr),
    datastack.pop("<", V2, !Array, !Ptr),
    ( if V1 = intval(I1), V2 = intval(I2) then
        ( if I2 < I1 then datastack.push(intval(0), !Array, !Ptr)
        else datastack.push(intval(1), !Array, !Ptr)
        )
    else if V1 = intval(_) then
        throw(type_error("int", V2))
    else
        throw(type_error("int", V1))
    ).

%-----------------------------------------------------------------------%
% >=: ( int int -- int ) Greater or equal: a b >= is 0 if a >= b, else 1
%-----------------------------------------------------------------------%

operator_gte(!Array, !Ptr) :-
    datastack.pop(">=", V1, !Array, !Ptr),
    datastack.pop(">=", V2, !Array, !Ptr),
    ( if V1 = intval(I1), V2 = intval(I2) then
        ( if I2 >= I1 then datastack.push(intval(0), !Array, !Ptr)
        else datastack.push(intval(1), !Array, !Ptr)
        )
    else if V1 = intval(_) then
        throw(type_error("int", V2))
    else
        throw(type_error("int", V1))
    ).

%-----------------------------------------------------------------------%
% <=: ( int int -- int ) Less or equal: a b <= is 0 if a <= b, else 1
%-----------------------------------------------------------------------%

operator_lte(!Array, !Ptr) :-
    datastack.pop("<=", V1, !Array, !Ptr),
    datastack.pop("<=", V2, !Array, !Ptr),
    ( if V1 = intval(I1), V2 = intval(I2) then
        ( if I2 =< I1 then datastack.push(intval(0), !Array, !Ptr)
        else datastack.push(intval(1), !Array, !Ptr)
        )
    else if V1 = intval(_) then
        throw(type_error("int", V2))
    else
        throw(type_error("int", V1))
    ).

%-----------------------------------------------------------------------%
% @: ( container key -- val ) Get element from array by index or from map by key
% For arrays: key must be int. For maps: key must be quoted identifier ('name).
%-----------------------------------------------------------------------%

operator_get(!Array, !Ptr) :-
    datastack.pop("@", KeyVal, !Array, !Ptr),
    datastack.pop("@", ContainerVal, !Array, !Ptr),
    ( if ContainerVal = arrayval(Arr), KeyVal = intval(Index) then
        ( if array.semidet_lookup(Arr, Index, Elem) then
            datastack.push(Elem, !Array, !Ptr)
        else
            throw(index_out_of_bounds(Index, array.size(Arr)))
        )
    else if ContainerVal = mapval(Map), KeyVal = termval(identifier(NameId)) then
        ( if map.search(Map, NameId, Value) then
            datastack.push(Value, !Array, !Ptr)
        else
            throw(undefined_name(NameId))
        )
    else if ContainerVal = termval(function(Terms)), KeyVal = intval(Index) then
        ( if list.index0(Terms, Index, Term) then
            datastack.push(termval(Term), !Array, !Ptr)
        else
            throw(index_out_of_bounds(Index, list.length(Terms)))
        )
    else if ContainerVal = termval(generator(Terms)), KeyVal = intval(Index) then
        ( if list.index0(Terms, Index, Term) then
            datastack.push(termval(Term), !Array, !Ptr)
        else
            throw(index_out_of_bounds(Index, list.length(Terms)))
        )
    else if ContainerVal = arrayval(_) then
        throw(type_error("int", KeyVal))
    else if ContainerVal = mapval(_) then
        throw(type_error("term", KeyVal))
    else
        throw(type_error("array, map, or quoted function/generator", ContainerVal))
    ).

%-----------------------------------------------------------------------%
% #: ( container -- int ) Get length of array or size of map
%-----------------------------------------------------------------------%

operator_length(!Array, !Ptr) :-
    datastack.pop("#", V, !Array, !Ptr),
    ( if V = arrayval(Arr) then
        datastack.push(intval(array.size(Arr)), !Array, !Ptr)
    else if V = mapval(Map) then
        datastack.push(intval(map.count(Map)), !Array, !Ptr)
    else if V = termval(function(Terms)) then
        datastack.push(intval(list.length(Terms)), !Array, !Ptr)
    else if V = termval(generator(Terms)) then
        datastack.push(intval(list.length(Terms)), !Array, !Ptr)
    else
        throw(type_error("array, map, or quoted function/generator", V))
    ).

%-----------------------------------------------------------------------%
% =: ( a b -- int ) Test equality: push 0 if equal, 1 if not equal
%-----------------------------------------------------------------------%

operator_eq(!Array, !Ptr) :-
    datastack.pop("=", V1, !Array, !Ptr),
    datastack.pop("=", V2, !Array, !Ptr),
    ( if values_equal(V1, V2, Equal) then
        ( Equal = yes, datastack.push(intval(0), !Array, !Ptr)
        ; Equal = no, datastack.push(intval(1), !Array, !Ptr)
        )
    else
        throw(type_error("comparable values", V1))
    ).

:- pred values_equal(value::in, value::in, bool::out) is semidet.

values_equal(intval(I1), intval(I2), Equal) :-
    ( if I1 = I2 then Equal = yes else Equal = no ).
values_equal(stringval(Id1), stringval(Id2), Equal) :-
    ( if Id1 = Id2 then Equal = yes else Equal = no ).
values_equal(arrayval(A1), arrayval(A2), Equal) :-
    arrays_equal(A1, A2, Equal).
values_equal(mapval(M1), mapval(M2), Equal) :-
    maps_equal(M1, M2, Equal).
values_equal(termval(T1), termval(T2), Equal) :-
    ( if T1 = T2 then Equal = yes else Equal = no ).
values_equal(nilval, nilval, yes).
values_equal(consval(H1, T1), consval(H2, T2), Equal) :-
    values_equal(H1, H2, HeadEq),
    ( HeadEq = no, Equal = no
    ; HeadEq = yes, values_equal(T1, T2, Equal)
    ).
values_equal(closureval(Env1, Body1), closureval(Env2, Body2), Equal) :-
    ( if Body1 = Body2 then
        maps_equal(Env1, Env2, Equal)
    else
        Equal = no
    ).

:- pred arrays_equal(array(value)::in, array(value)::in, bool::out) is semidet.

arrays_equal(A1, A2, Equal) :-
    Size1 = array.size(A1),
    Size2 = array.size(A2),
    ( if Size1 \= Size2 then
        Equal = no
    else
        arrays_equal_loop(A1, A2, 0, Size1, Equal)
    ).

:- pred arrays_equal_loop(array(value)::in, array(value)::in, int::in, int::in,
    bool::out) is semidet.

arrays_equal_loop(A1, A2, I, Size, Equal) :-
    ( if I >= Size then
        Equal = yes
    else
        array.lookup(A1, I, V1),
        array.lookup(A2, I, V2),
        values_equal(V1, V2, ElemEqual),
        ( ElemEqual = no, Equal = no
        ; ElemEqual = yes, arrays_equal_loop(A1, A2, I + 1, Size, Equal)
        )
    ).

% Convert map to assoc_list to perform bulk operations.
:- pred maps_equal(map(string_id, value)::in, map(string_id, value)::in,
    bool::out) is semidet.

maps_equal(M1, M2, Equal) :-
    map.to_assoc_list(M1, AL1),
    map.to_assoc_list(M2, AL2),
    assoc_lists_equal(AL1, AL2, Equal).

:- pred assoc_lists_equal(assoc_list(string_id, value)::in,
    assoc_list(string_id, value)::in, bool::out) is semidet.

assoc_lists_equal([], [], yes).
assoc_lists_equal([K1 - V1 | Rest1], [K2 - V2 | Rest2], Equal) :-
    ( if K1 = K2 then
        values_equal(V1, V2, ValEqual),
        ( ValEqual = no, Equal = no
        ; ValEqual = yes, assoc_lists_equal(Rest1, Rest2, Equal)
        )
    else
        Equal = no
    ).

%-----------------------------------------------------------------------%
% ?: ( cond then else -- result ) If cond is 0, push then; otherwise push else
%-----------------------------------------------------------------------%

operator_ite(!Array, !Ptr) :-
    datastack.pop("?", C, !Array, !Ptr),
    datastack.pop("?", B, !Array, !Ptr),
    datastack.pop("?", A, !Array, !Ptr),
    ( if A = intval(0) then
        datastack.push(B, !Array, !Ptr)
    else if A = intval(_) then
        datastack.push(C, !Array, !Ptr)
    else
        throw(type_error("int", A))
    ).

%-----------------------------------------------------------------------%
% .: ( -- nil ) Push nil onto the stack
%-----------------------------------------------------------------------%

operator_nil(!Array, !Ptr) :-
    datastack.push(nilval, !Array, !Ptr).

%-----------------------------------------------------------------------%
% ,: ( tail head -- cons ) Create a cons cell with head and tail
%-----------------------------------------------------------------------%

operator_cons(!Array, !Ptr) :-
    datastack.pop(",", Head, !Array, !Ptr),
    datastack.pop(",", Tail, !Array, !Ptr),
    datastack.push(consval(Head, Tail), !Array, !Ptr).

%-----------------------------------------------------------------------%
% fst: ( cons -- head ) Get the first element (head) of a cons cell
%-----------------------------------------------------------------------%

operator_fst(!Array, !Ptr) :-
    datastack.pop("fst", V, !Array, !Ptr),
    ( if V = consval(H, _) then
        datastack.push(H, !Array, !Ptr)
    else
        throw(type_error("cons", V))
    ).

%-----------------------------------------------------------------------%
% snd: ( cons -- tail ) Get the second element (tail) of a cons cell
%-----------------------------------------------------------------------%

operator_snd(!Array, !Ptr) :-
    datastack.pop("snd", V, !Array, !Ptr),
    ( if V = consval(_, T) then
        datastack.push(T, !Array, !Ptr)
    else
        throw(type_error("cons", V))
    ).

%-----------------------------------------------------------------------%
% write: ( a -- ) Pop and print value in executable (round-trippable) form
%-----------------------------------------------------------------------%

operator_write(ST, !Array, !Ptr, !IO) :-
    datastack.pop("write", V, !Array, !Ptr),
    io.write_string(value_to_write_string(ST, V), !IO).

%-----------------------------------------------------------------------%
% value_to_write_string: convert a value to executable string form
%-----------------------------------------------------------------------%

:- func value_to_write_string(string_table, value) = string.

value_to_write_string(_, intval(I)) = int_to_string(I).
value_to_write_string(ST, stringval(StrId)) =
    "\"" ++ escape_string(lookup_string(ST, StrId)) ++ "\"".
value_to_write_string(ST, arrayval(A)) = "[ " ++ ArrayElems ++ "]" :-
    array.to_list(A, List),
    ElemStrings = list.map(
        (func(V) = value_to_write_string(ST, V) ++ " "),
        List),
    ArrayElems = string.append_list(ElemStrings).
value_to_write_string(ST, mapval(M)) = Result :-
    map.foldl(map_entry_to_string(ST), M, "$", Result).
value_to_write_string(ST, termval(T)) = "'" ++ term_to_write_string(ST, T).
value_to_write_string(_, nilval) = ".".
value_to_write_string(ST, consval(H, T)) =
    value_to_write_string(ST, T) ++ " " ++ value_to_write_string(ST, H) ++ " ,".
value_to_write_string(ST, closureval(_, Body)) =
    "<closure:{ " ++ terms_to_write_string(ST, Body) ++ "}>".
value_to_write_string(_, bytecodeval(_, Addr)) =
    "<bytecode:" ++ int_to_string(Addr) ++ ">".

:- pred map_entry_to_string(string_table::in, string_id::in, value::in,
    string::in, string::out) is det.

map_entry_to_string(ST, NameId, V, !Acc) :-
    !:Acc = !.Acc ++ " " ++ value_to_write_string(ST, V) ++ " '" ++
        lookup_string(ST, NameId) ++ " :".

:- func term_to_write_string(string_table, term) = string.

term_to_write_string(ST, identifier(NameId)) = lookup_string(ST, NameId).
term_to_write_string(ST, binder(NameId)) = "/" ++ lookup_string(ST, NameId).
term_to_write_string(ST, function(Terms)) = "{ " ++ terms_to_write_string(ST, Terms) ++ "}".
term_to_write_string(ST, generator(Terms)) = "[ " ++ terms_to_write_string(ST, Terms) ++ "]".
term_to_write_string(ST, quoted(T)) = "'" ++ term_to_write_string(ST, T).
term_to_write_string(ST, value(V)) = value_to_write_string(ST, V).
term_to_write_string(_, apply_term) = "!".

:- func terms_to_write_string(string_table, list(term)) = string.

terms_to_write_string(_, []) = "".
terms_to_write_string(ST, [T | Ts]) =
    term_to_write_string(ST, T) ++ " " ++ terms_to_write_string(ST, Ts).

%-----------------------------------------------------------------------%
% fwrite: ( value file -- ) Write value to file in executable form
%-----------------------------------------------------------------------%

operator_fwrite(ST, !Array, !Ptr, !IO) :-
    datastack.pop("fwrite", FileVal, !Array, !Ptr),
    datastack.pop("fwrite", V, !Array, !Ptr),
    Filename = value_to_string(ST, FileVal),
    Content = value_to_write_string(ST, V),
    io.open_output(Filename, Result, !IO),
    (
        Result = ok(Stream),
        io.write_string(Stream, Content, !IO),
        io.write_char(Stream, '\n', !IO),
        io.close_output(Stream, !IO)
    ;
        Result = error(Error),
        throw(io_error("fwrite", Filename, io.error_message(Error)))
    ).

%-----------------------------------------------------------------------%
% $: ( -- map ) Push an empty map onto the stack
%-----------------------------------------------------------------------%

operator_empty(!Array, !Ptr) :-
    datastack.push(mapval(map.init), !Array, !Ptr).

%-----------------------------------------------------------------------%
% keys: ( map -- array ) Get map keys as an array of quoted identifiers
%-----------------------------------------------------------------------%

operator_keys(!Array, !Ptr) :-
    datastack.pop("keys", V, !Array, !Ptr),
    ( if V = mapval(Map) then
        Keys = map.keys(Map),
        KeyTerms = list.map(
            (func(NameId) = termval(identifier(NameId))),
            Keys),
        datastack.push(arrayval(array.from_list(KeyTerms)), !Array, !Ptr)
    else
        throw(type_error("map", V))
    ).

%-----------------------------------------------------------------------%
% :: ( map val 'key -- map ) Store value in map under key, return new map
%-----------------------------------------------------------------------%

operator_store(!Array, !Ptr) :-
    datastack.pop(":", KeyVal, !Array, !Ptr),
    datastack.pop(":", Val, !Array, !Ptr),
    datastack.pop(":", MapVal, !Array, !Ptr),
    ( if MapVal = mapval(Map), KeyVal = termval(identifier(NameId)) then
        map.set(NameId, Val, Map, NewMap),
        datastack.push(mapval(NewMap), !Array, !Ptr)
    else if MapVal = mapval(_) then
        throw(type_error("term", KeyVal))
    else
        throw(type_error("map", MapVal))
    ).

%-----------------------------------------------------------------------%
% in: ( map 'key -- int ) Test if key exists in map: 0 if yes, 1 if no
%-----------------------------------------------------------------------%

operator_in(!Array, !Ptr) :-
    datastack.pop("in", KeyVal, !Array, !Ptr),
    datastack.pop("in", MapVal, !Array, !Ptr),
    ( if MapVal = mapval(Map), KeyVal = termval(identifier(NameId)) then
        ( if map.contains(Map, NameId) then
            datastack.push(intval(0), !Array, !Ptr)
        else
            datastack.push(intval(1), !Array, !Ptr)
        )
    else if MapVal = mapval(_) then
        throw(type_error("term", KeyVal))
    else
        throw(type_error("map", MapVal))
    ).

%-----------------------------------------------------------------------%
% delete: ( map 'key -- map ) Remove key from map, return new map
%-----------------------------------------------------------------------%

operator_delete(!Array, !Ptr) :-
    datastack.pop("delete", KeyVal, !Array, !Ptr),
    datastack.pop("delete", MapVal, !Array, !Ptr),
    ( if MapVal = mapval(Map), KeyVal = termval(identifier(NameId)) then
        map.delete(NameId, Map, NewMap),
        datastack.push(mapval(NewMap), !Array, !Ptr)
    else if MapVal = mapval(_) then
        throw(type_error("term", KeyVal))
    else
        throw(type_error("map", MapVal))
    ).

%-----------------------------------------------------------------------%
% Type testing predicates
% Each returns 0 if true, 1 if false (following Froth's boolean convention)
%-----------------------------------------------------------------------%

operator_is_int(!Array, !Ptr) :-
    datastack.pop("isInt", V, !Array, !Ptr),
    ( if V = intval(_) then datastack.push(intval(0), !Array, !Ptr)
    else datastack.push(intval(1), !Array, !Ptr)
    ).

operator_is_string(!Array, !Ptr) :-
    datastack.pop("isString", V, !Array, !Ptr),
    ( if V = stringval(_) then datastack.push(intval(0), !Array, !Ptr)
    else datastack.push(intval(1), !Array, !Ptr)
    ).

operator_is_array(!Array, !Ptr) :-
    datastack.pop("isArray", V, !Array, !Ptr),
    ( if V = arrayval(_) then datastack.push(intval(0), !Array, !Ptr)
    else datastack.push(intval(1), !Array, !Ptr)
    ).

operator_is_map(!Array, !Ptr) :-
    datastack.pop("isMap", V, !Array, !Ptr),
    ( if V = mapval(_) then datastack.push(intval(0), !Array, !Ptr)
    else datastack.push(intval(1), !Array, !Ptr)
    ).

operator_is_nil(!Array, !Ptr) :-
    datastack.pop("isNil", V, !Array, !Ptr),
    ( if V = nilval then datastack.push(intval(0), !Array, !Ptr)
    else datastack.push(intval(1), !Array, !Ptr)
    ).

operator_is_cons(!Array, !Ptr) :-
    datastack.pop("isCons", V, !Array, !Ptr),
    ( if V = consval(_, _) then datastack.push(intval(0), !Array, !Ptr)
    else datastack.push(intval(1), !Array, !Ptr)
    ).

operator_is_ident(!Array, !Ptr) :-
    datastack.pop("isIdent", V, !Array, !Ptr),
    ( if V = termval(identifier(_)) then datastack.push(intval(0), !Array, !Ptr)
    else datastack.push(intval(1), !Array, !Ptr)
    ).

operator_is_binder(!Array, !Ptr) :-
    datastack.pop("isBinder", V, !Array, !Ptr),
    ( if V = termval(binder(_)) then datastack.push(intval(0), !Array, !Ptr)
    else datastack.push(intval(1), !Array, !Ptr)
    ).

operator_is_func(!Array, !Ptr) :-
    datastack.pop("isFunc", V, !Array, !Ptr),
    ( if V = termval(function(_)) then datastack.push(intval(0), !Array, !Ptr)
    else datastack.push(intval(1), !Array, !Ptr)
    ).

operator_is_gen(!Array, !Ptr) :-
    datastack.pop("isGen", V, !Array, !Ptr),
    ( if V = termval(generator(_)) then datastack.push(intval(0), !Array, !Ptr)
    else datastack.push(intval(1), !Array, !Ptr)
    ).

operator_is_quote(!Array, !Ptr) :-
    datastack.pop("isQuote", V, !Array, !Ptr),
    ( if V = termval(quoted(_)) then datastack.push(intval(0), !Array, !Ptr)
    else datastack.push(intval(1), !Array, !Ptr)
    ).

operator_is_apply(!Array, !Ptr) :-
    datastack.pop("isApply", V, !Array, !Ptr),
    ( if V = termval(apply_term) then datastack.push(intval(0), !Array, !Ptr)
    else datastack.push(intval(1), !Array, !Ptr)
    ).

%-----------------------------------------------------------------------%
% isValue: ( a -- int ) Test if value is a quoted value term
%-----------------------------------------------------------------------%

operator_is_value(!Array, !Ptr) :-
    datastack.pop("isValue", V, !Array, !Ptr),
    ( if V = termval(value(_)) then datastack.push(intval(0), !Array, !Ptr)
    else datastack.push(intval(1), !Array, !Ptr)
    ).

%-----------------------------------------------------------------------%
% unwrap: ( 'value -- value ) Extract value from quoted value term
%-----------------------------------------------------------------------%

operator_unwrap(!Array, !Ptr) :-
    datastack.pop("unwrap", V, !Array, !Ptr),
    ( if V = termval(value(Inner)) then
        datastack.push(Inner, !Array, !Ptr)
    else if V = termval(quoted(InnerTerm)) then
        datastack.push(termval(InnerTerm), !Array, !Ptr)
    else
        throw(type_error("quoted value or quoted term", V))
    ).

%-----------------------------------------------------------------------%
% intern: ( string|'ident|'binder -- int ) Get the intern id
%-----------------------------------------------------------------------%

operator_intern(!Array, !Ptr) :-
    datastack.pop("intern", V, !Array, !Ptr),
    ( if V = stringval(Id) then
        datastack.push(intval(Id), !Array, !Ptr)
    else if V = termval(identifier(Id)) then
        datastack.push(intval(Id), !Array, !Ptr)
    else if V = termval(binder(Id)) then
        datastack.push(intval(Id), !Array, !Ptr)
    else
        throw(type_error("string, identifier, or binder", V))
    ).

%-----------------------------------------------------------------------%
% idToString: ( int -- string ) Create string from intern id
%-----------------------------------------------------------------------%

operator_id_to_string(!Array, !Ptr) :-
    datastack.pop("idToString", V, !Array, !Ptr),
    ( if V = intval(Id) then
        datastack.push(stringval(Id), !Array, !Ptr)
    else
        throw(type_error("int", V))
    ).

%-----------------------------------------------------------------------%
% idToIdent: ( int -- 'ident ) Create quoted identifier from intern id
%-----------------------------------------------------------------------%

operator_id_to_ident(!Array, !Ptr) :-
    datastack.pop("idToIdent", V, !Array, !Ptr),
    ( if V = intval(Id) then
        datastack.push(termval(identifier(Id)), !Array, !Ptr)
    else
        throw(type_error("int", V))
    ).

%-----------------------------------------------------------------------%
% idToBinder: ( int -- 'binder ) Create quoted binder from intern id
%-----------------------------------------------------------------------%

operator_id_to_binder(!Array, !Ptr) :-
    datastack.pop("idToBinder", V, !Array, !Ptr),
    ( if V = intval(Id) then
        datastack.push(termval(binder(Id)), !Array, !Ptr)
    else
        throw(type_error("int", V))
    ).

%-----------------------------------------------------------------------%
% isOperator: ( 'ident -- int ) Test if identifier is an operator name
%-----------------------------------------------------------------------%

operator_is_operator(OpTable, !Array, !Ptr) :-
    datastack.pop("isOperator", V, !Array, !Ptr),
    ( if V = termval(identifier(Id)) then
        ( if map.contains(OpTable, Id) then
            datastack.push(intval(0), !Array, !Ptr)
        else
            datastack.push(intval(1), !Array, !Ptr)
        )
    else
        throw(type_error("identifier", V))
    ).

%-----------------------------------------------------------------------%
% arity: ( 'ident -- int ) Get the arity of an operator
%-----------------------------------------------------------------------%

operator_arity_op(OpTable, !Array, !Ptr) :-
    datastack.pop("arity", V, !Array, !Ptr),
    ( if V = termval(identifier(Id)) then
        ( if map.search(OpTable, Id, Info) then
            datastack.push(intval(Info ^ oi_arity), !Array, !Ptr)
        else
            throw(type_error("operator", V))
        )
    else
        throw(type_error("identifier", V))
    ).

%-----------------------------------------------------------------------%
% stack: ( ... -- array ) Convert entire stack to an array
%-----------------------------------------------------------------------%

operator_stack(!Array, !Ptr) :-
    % Extract all values from the stack (bottom to top order)
    List = datastack.to_list(!.Array, !.Ptr),
    ResultArray = array.from_list(List),
    % Clear the stack and push the result
    !:Ptr = 0,
    datastack.push(arrayval(ResultArray), !Array, !Ptr).

%-----------------------------------------------------------------------%
% time: ( -- int ) Push current clock ticks
%-----------------------------------------------------------------------%

operator_time(!Array, !Ptr, !IO) :-
    time.clock(Ticks, !IO),
    datastack.push(intval(Ticks), !Array, !Ptr).

%-----------------------------------------------------------------------%
% close: Create a closure
%   ( env body -- closure )     map + function -> term closure
%   ( context addr -- bytecode ) array + int -> bytecode closure
%-----------------------------------------------------------------------%

operator_close(!Array, !Ptr) :-
    datastack.pop("close", Second, !Array, !Ptr),
    datastack.pop("close", First, !Array, !Ptr),
    ( if First = mapval(Env), Second = termval(function(Body)) then
        % Term closure: map + function -> closureval
        datastack.push(closureval(Env, Body), !Array, !Ptr)
    else if First = arrayval(Context), Second = intval(CodeAddr) then
        % Bytecode closure: array + int -> bytecodeval
        datastack.push(bytecodeval(Context, CodeAddr), !Array, !Ptr)
    else if First = mapval(_) then
        throw(type_error("function", Second))
    else if First = arrayval(_) then
        throw(type_error("int", Second))
    else
        throw(type_error("map or array", First))
    ).

%-----------------------------------------------------------------------%
% closureEnv: ( closure -- env ) Get the environment from a closure
%-----------------------------------------------------------------------%

operator_closure_env(!Array, !Ptr) :-
    datastack.pop("closureEnv", V, !Array, !Ptr),
    ( if V = closureval(Env, _) then
        datastack.push(mapval(Env), !Array, !Ptr)
    else
        throw(type_error("closure", V))
    ).

%-----------------------------------------------------------------------%
% closureBody: ( closure -- body ) Get the body from a closure
%-----------------------------------------------------------------------%

operator_closure_body(!Array, !Ptr) :-
    datastack.pop("closureBody", V, !Array, !Ptr),
    ( if V = closureval(_, Body) then
        datastack.push(termval(function(Body)), !Array, !Ptr)
    else
        throw(type_error("closure", V))
    ).

%-----------------------------------------------------------------------%
% isClosure: ( a -- int ) Test if value is a closure
%-----------------------------------------------------------------------%

operator_is_closure(!Array, !Ptr) :-
    datastack.pop("isClosure", V, !Array, !Ptr),
    ( if V = closureval(_, _) then datastack.push(intval(0), !Array, !Ptr)
    else datastack.push(intval(1), !Array, !Ptr)
    ).

%-----------------------------------------------------------------------%
:- end_module operators.
%-----------------------------------------------------------------------%
