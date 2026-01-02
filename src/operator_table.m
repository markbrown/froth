%-----------------------------------------------------------------------%
% operator_table.m
% Operator metadata: name mappings, arity, and bytecode encoding.
% NOTE: Operator numbers must be kept in sync with lib/bytecode.froth
%-----------------------------------------------------------------------%

:- module operator_table.
:- interface.

:- import_module types.

%-----------------------------------------------------------------------%

    % operator(Name, Op):
    % Map an operator name to its enum value.
    %
:- pred operator(string::in, operator::out) is semidet.

    % operator_arity(Op):
    % Get the number of values an operator pops from the stack.
    %
:- func operator_arity(operator) = int.

    % operator_to_int(Op):
    % Convert an operator to its bytecode integer encoding.
    %
:- func operator_to_int(operator) = int.

    % int_to_operator(Int, Op):
    % Convert a bytecode integer to an operator. Fails if out of range.
    %
:- pred int_to_operator(int::in, operator::out) is semidet.

    % num_operators:
    % The total number of operators.
    %
:- func num_operators = int.

    % init_operators(!ST, OpTable):
    % Initialize the operator table by interning all operator names.
    %
:- pred init_operators(string_table::in, string_table::out,
    operator_table::out) is det.

%-----------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module map.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------%
% Operator name to enum mapping
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
operator("open", op_open).
operator("isClosure", op_is_closure).
operator("peek", op_peek).
operator("poke", op_poke).
operator("ref", op_ref).
operator("deref", op_deref).
operator("applyOperator", op_apply_operator).
operator("wrap", op_wrap).

%-----------------------------------------------------------------------%
% Operator arity (number of values popped from stack)
%-----------------------------------------------------------------------%

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
operator_arity(op_open) = 1.
operator_arity(op_is_closure) = 1.
operator_arity(op_peek) = 1.
operator_arity(op_poke) = 2.
operator_arity(op_ref) = 1.
operator_arity(op_deref) = 1.
operator_arity(op_apply_operator) = 1.
operator_arity(op_wrap) = 1.

%-----------------------------------------------------------------------%
% Operator to/from integer conversion for bytecode
%-----------------------------------------------------------------------%

num_operators = 57.

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
operator_to_int(op_open) = 49.
operator_to_int(op_is_closure) = 50.
operator_to_int(op_peek) = 51.
operator_to_int(op_poke) = 52.
operator_to_int(op_ref) = 53.
operator_to_int(op_deref) = 54.
operator_to_int(op_apply_operator) = 55.
operator_to_int(op_wrap) = 56.

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
int_to_operator(49, op_open).
int_to_operator(50, op_is_closure).
int_to_operator(51, op_peek).
int_to_operator(52, op_poke).
int_to_operator(53, op_ref).
int_to_operator(54, op_deref).
int_to_operator(55, op_apply_operator).
int_to_operator(56, op_wrap).

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
        "close", "open", "isClosure",
        "peek", "poke", "ref", "deref", "applyOperator", "wrap"
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
:- end_module operator_table.
%-----------------------------------------------------------------------%
