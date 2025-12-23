%-----------------------------------------------------------------------%
% operators.m
% Operator definitions for the Froth programming language.
%-----------------------------------------------------------------------%

:- module operators.
:- interface.

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

    % eval_operator(OpTable, ST, Op, Env, !Stack, !IO):
    % Execute an operator. OpTable and ST are read-only.
    % Env is read-only (for the env operator).
    %
:- pred eval_operator(operator_table::in, string_table::in, operator::in,
    env::in, stack::in, stack::out, io::di, io::uo) is det.

    % Individual operator implementations.
    %
:- pred operator_print(string_table::in, stack::in, stack::out,
    io::di, io::uo) is det.
:- pred operator_env(env::in, stack::in, stack::out) is det.
:- pred operator_add(stack::in, stack::out) is det.
:- pred operator_sub(stack::in, stack::out) is det.
:- pred operator_mul(stack::in, stack::out) is det.
:- pred operator_gt(stack::in, stack::out) is det.
:- pred operator_lt(stack::in, stack::out) is det.
:- pred operator_gte(stack::in, stack::out) is det.
:- pred operator_lte(stack::in, stack::out) is det.
:- pred operator_get(stack::in, stack::out) is det.
:- pred operator_length(stack::in, stack::out) is det.
:- pred operator_eq(stack::in, stack::out) is det.
:- pred operator_ite(stack::in, stack::out) is det.
:- pred operator_nil(stack::in, stack::out) is det.
:- pred operator_cons(stack::in, stack::out) is det.
:- pred operator_fst(stack::in, stack::out) is det.
:- pred operator_snd(stack::in, stack::out) is det.
:- pred operator_write(string_table::in, stack::in, stack::out,
    io::di, io::uo) is det.
:- pred operator_fwrite(string_table::in, stack::in, stack::out,
    io::di, io::uo) is det.
:- pred operator_empty(stack::in, stack::out) is det.
:- pred operator_keys(stack::in, stack::out) is det.
:- pred operator_store(stack::in, stack::out) is det.
:- pred operator_in(stack::in, stack::out) is det.
:- pred operator_delete(stack::in, stack::out) is det.
:- pred operator_is_int(stack::in, stack::out) is det.
:- pred operator_is_string(stack::in, stack::out) is det.
:- pred operator_is_array(stack::in, stack::out) is det.
:- pred operator_is_map(stack::in, stack::out) is det.
:- pred operator_is_nil(stack::in, stack::out) is det.
:- pred operator_is_cons(stack::in, stack::out) is det.
:- pred operator_is_ident(stack::in, stack::out) is det.
:- pred operator_is_binder(stack::in, stack::out) is det.
:- pred operator_is_func(stack::in, stack::out) is det.
:- pred operator_is_gen(stack::in, stack::out) is det.
:- pred operator_is_quote(stack::in, stack::out) is det.
:- pred operator_is_apply(stack::in, stack::out) is det.
:- pred operator_is_value(stack::in, stack::out) is det.
:- pred operator_unwrap(stack::in, stack::out) is det.
:- pred operator_intern(stack::in, stack::out) is det.
:- pred operator_id_to_string(stack::in, stack::out) is det.
:- pred operator_id_to_ident(stack::in, stack::out) is det.
:- pred operator_id_to_binder(stack::in, stack::out) is det.
:- pred operator_is_operator(operator_table::in, stack::in, stack::out) is det.
:- pred operator_arity_op(operator_table::in, stack::in, stack::out) is det.
:- pred operator_stack(stack::in, stack::out) is det.
:- pred operator_time(stack::in, stack::out, io::di, io::uo) is det.

    % value_to_string(ST, Value) = String:
    % Convert a value to its string representation (for print/import).
    %
:- func value_to_string(string_table, value) = string.

%-----------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module assoc_list.
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
        "stack", "import", "time"
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

eval_operator(OpTable, ST, Op, Env, !Stack, !IO) :-
    (
        Op = op_print,
        operator_print(ST, !Stack, !IO)
    ;
        Op = op_env,
        operator_env(Env, !Stack)
    ;
        Op = op_add,
        operator_add(!Stack)
    ;
        Op = op_sub,
        operator_sub(!Stack)
    ;
        Op = op_mul,
        operator_mul(!Stack)
    ;
        Op = op_gt,
        operator_gt(!Stack)
    ;
        Op = op_lt,
        operator_lt(!Stack)
    ;
        Op = op_gte,
        operator_gte(!Stack)
    ;
        Op = op_lte,
        operator_lte(!Stack)
    ;
        Op = op_get,
        operator_get(!Stack)
    ;
        Op = op_length,
        operator_length(!Stack)
    ;
        Op = op_eq,
        operator_eq(!Stack)
    ;
        Op = op_ite,
        operator_ite(!Stack)
    ;
        Op = op_nil,
        operator_nil(!Stack)
    ;
        Op = op_cons,
        operator_cons(!Stack)
    ;
        Op = op_fst,
        operator_fst(!Stack)
    ;
        Op = op_snd,
        operator_snd(!Stack)
    ;
        Op = op_write,
        operator_write(ST, !Stack, !IO)
    ;
        Op = op_fwrite,
        operator_fwrite(ST, !Stack, !IO)
    ;
        Op = op_empty,
        operator_empty(!Stack)
    ;
        Op = op_keys,
        operator_keys(!Stack)
    ;
        Op = op_store,
        operator_store(!Stack)
    ;
        Op = op_in,
        operator_in(!Stack)
    ;
        Op = op_delete,
        operator_delete(!Stack)
    ;
        Op = op_is_int,
        operator_is_int(!Stack)
    ;
        Op = op_is_string,
        operator_is_string(!Stack)
    ;
        Op = op_is_array,
        operator_is_array(!Stack)
    ;
        Op = op_is_map,
        operator_is_map(!Stack)
    ;
        Op = op_is_nil,
        operator_is_nil(!Stack)
    ;
        Op = op_is_cons,
        operator_is_cons(!Stack)
    ;
        Op = op_is_ident,
        operator_is_ident(!Stack)
    ;
        Op = op_is_binder,
        operator_is_binder(!Stack)
    ;
        Op = op_is_func,
        operator_is_func(!Stack)
    ;
        Op = op_is_gen,
        operator_is_gen(!Stack)
    ;
        Op = op_is_quote,
        operator_is_quote(!Stack)
    ;
        Op = op_is_apply,
        operator_is_apply(!Stack)
    ;
        Op = op_is_value,
        operator_is_value(!Stack)
    ;
        Op = op_unwrap,
        operator_unwrap(!Stack)
    ;
        Op = op_intern,
        operator_intern(!Stack)
    ;
        Op = op_id_to_string,
        operator_id_to_string(!Stack)
    ;
        Op = op_id_to_ident,
        operator_id_to_ident(!Stack)
    ;
        Op = op_id_to_binder,
        operator_id_to_binder(!Stack)
    ;
        Op = op_is_operator,
        operator_is_operator(OpTable, !Stack)
    ;
        Op = op_arity,
        operator_arity_op(OpTable, !Stack)
    ;
        Op = op_stack,
        operator_stack(!Stack)
    ;
        Op = op_import,
        % import is handled specially in eval.m, should not reach here
        unexpected($pred, "import should be handled in eval.m")
    ;
        Op = op_time,
        operator_time(!Stack, !IO)
    ).

%-----------------------------------------------------------------------%
% print: ( a -- ) Pop and print a value
%-----------------------------------------------------------------------%

operator_print(ST, !Stack, !IO) :-
    pop("print", V, !Stack),
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

operator_env(Env, !Stack) :-
    push(mapval(Env), !Stack).

%-----------------------------------------------------------------------%
% +: ( int int -- int ) Add two integers
%-----------------------------------------------------------------------%

operator_add(!Stack) :-
    pop("+", V1, !Stack),
    pop("+", V2, !Stack),
    ( if V1 = intval(I1), V2 = intval(I2) then
        push(intval(I1 + I2), !Stack)
    else if V1 = intval(_) then
        throw(type_error("int", V2))
    else
        throw(type_error("int", V1))
    ).

%-----------------------------------------------------------------------%
% -: ( int int -- int ) Subtract: a b - computes a - b
%-----------------------------------------------------------------------%

operator_sub(!Stack) :-
    pop("-", V1, !Stack),
    pop("-", V2, !Stack),
    ( if V1 = intval(I1), V2 = intval(I2) then
        push(intval(I2 - I1), !Stack)
    else if V1 = intval(_) then
        throw(type_error("int", V2))
    else
        throw(type_error("int", V1))
    ).

%-----------------------------------------------------------------------%
% *: ( int int -- int ) Multiply two integers
%-----------------------------------------------------------------------%

operator_mul(!Stack) :-
    pop("*", V1, !Stack),
    pop("*", V2, !Stack),
    ( if V1 = intval(I1), V2 = intval(I2) then
        push(intval(I1 * I2), !Stack)
    else if V1 = intval(_) then
        throw(type_error("int", V2))
    else
        throw(type_error("int", V1))
    ).

%-----------------------------------------------------------------------%
% >: ( int int -- int ) Greater than: a b > is 0 if a > b, else 1
%-----------------------------------------------------------------------%

operator_gt(!Stack) :-
    pop(">", V1, !Stack),
    pop(">", V2, !Stack),
    ( if V1 = intval(I1), V2 = intval(I2) then
        ( if I2 > I1 then push(intval(0), !Stack)
        else push(intval(1), !Stack)
        )
    else if V1 = intval(_) then
        throw(type_error("int", V2))
    else
        throw(type_error("int", V1))
    ).

%-----------------------------------------------------------------------%
% <: ( int int -- int ) Less than: a b < is 0 if a < b, else 1
%-----------------------------------------------------------------------%

operator_lt(!Stack) :-
    pop("<", V1, !Stack),
    pop("<", V2, !Stack),
    ( if V1 = intval(I1), V2 = intval(I2) then
        ( if I2 < I1 then push(intval(0), !Stack)
        else push(intval(1), !Stack)
        )
    else if V1 = intval(_) then
        throw(type_error("int", V2))
    else
        throw(type_error("int", V1))
    ).

%-----------------------------------------------------------------------%
% >=: ( int int -- int ) Greater or equal: a b >= is 0 if a >= b, else 1
%-----------------------------------------------------------------------%

operator_gte(!Stack) :-
    pop(">=", V1, !Stack),
    pop(">=", V2, !Stack),
    ( if V1 = intval(I1), V2 = intval(I2) then
        ( if I2 >= I1 then push(intval(0), !Stack)
        else push(intval(1), !Stack)
        )
    else if V1 = intval(_) then
        throw(type_error("int", V2))
    else
        throw(type_error("int", V1))
    ).

%-----------------------------------------------------------------------%
% <=: ( int int -- int ) Less or equal: a b <= is 0 if a <= b, else 1
%-----------------------------------------------------------------------%

operator_lte(!Stack) :-
    pop("<=", V1, !Stack),
    pop("<=", V2, !Stack),
    ( if V1 = intval(I1), V2 = intval(I2) then
        ( if I2 =< I1 then push(intval(0), !Stack)
        else push(intval(1), !Stack)
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

operator_get(!Stack) :-
    pop("@", KeyVal, !Stack),
    pop("@", ContainerVal, !Stack),
    ( if ContainerVal = arrayval(Array), KeyVal = intval(Index) then
        ( if array.semidet_lookup(Array, Index, Elem) then
            push(Elem, !Stack)
        else
            throw(index_out_of_bounds(Index, array.size(Array)))
        )
    else if ContainerVal = mapval(Map), KeyVal = termval(identifier(NameId)) then
        ( if map.search(Map, NameId, Value) then
            push(Value, !Stack)
        else
            throw(undefined_name(NameId))
        )
    else if ContainerVal = termval(function(Terms)), KeyVal = intval(Index) then
        ( if list.index0(Terms, Index, Term) then
            push(termval(Term), !Stack)
        else
            throw(index_out_of_bounds(Index, list.length(Terms)))
        )
    else if ContainerVal = termval(generator(Terms)), KeyVal = intval(Index) then
        ( if list.index0(Terms, Index, Term) then
            push(termval(Term), !Stack)
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

operator_length(!Stack) :-
    pop("#", V, !Stack),
    ( if V = arrayval(Array) then
        push(intval(array.size(Array)), !Stack)
    else if V = mapval(Map) then
        push(intval(map.count(Map)), !Stack)
    else if V = termval(function(Terms)) then
        push(intval(list.length(Terms)), !Stack)
    else if V = termval(generator(Terms)) then
        push(intval(list.length(Terms)), !Stack)
    else
        throw(type_error("array, map, or quoted function/generator", V))
    ).

%-----------------------------------------------------------------------%
% =: ( a b -- int ) Test equality: push 0 if equal, 1 if not equal
%-----------------------------------------------------------------------%

operator_eq(!Stack) :-
    pop("=", V1, !Stack),
    pop("=", V2, !Stack),
    ( if values_equal(V1, V2, Equal) then
        ( Equal = yes, push(intval(0), !Stack)
        ; Equal = no, push(intval(1), !Stack)
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

operator_ite(!Stack) :-
    pop("?", C, !Stack),
    pop("?", B, !Stack),
    pop("?", A, !Stack),
    ( if A = intval(0) then
        push(B, !Stack)
    else if A = intval(_) then
        push(C, !Stack)
    else
        throw(type_error("int", A))
    ).

%-----------------------------------------------------------------------%
% .: ( -- nil ) Push nil onto the stack
%-----------------------------------------------------------------------%

operator_nil(!Stack) :-
    push(nilval, !Stack).

%-----------------------------------------------------------------------%
% ,: ( tail head -- cons ) Create a cons cell with head and tail
%-----------------------------------------------------------------------%

operator_cons(!Stack) :-
    pop(",", Head, !Stack),
    pop(",", Tail, !Stack),
    push(consval(Head, Tail), !Stack).

%-----------------------------------------------------------------------%
% fst: ( cons -- head ) Get the first element (head) of a cons cell
%-----------------------------------------------------------------------%

operator_fst(!Stack) :-
    pop("fst", V, !Stack),
    ( if V = consval(H, _) then
        push(H, !Stack)
    else
        throw(type_error("cons", V))
    ).

%-----------------------------------------------------------------------%
% snd: ( cons -- tail ) Get the second element (tail) of a cons cell
%-----------------------------------------------------------------------%

operator_snd(!Stack) :-
    pop("snd", V, !Stack),
    ( if V = consval(_, T) then
        push(T, !Stack)
    else
        throw(type_error("cons", V))
    ).

%-----------------------------------------------------------------------%
% write: ( a -- ) Pop and print value in executable (round-trippable) form
%-----------------------------------------------------------------------%

operator_write(ST, !Stack, !IO) :-
    pop("write", V, !Stack),
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

operator_fwrite(ST, !Stack, !IO) :-
    pop("fwrite", FileVal, !Stack),
    pop("fwrite", V, !Stack),
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

operator_empty(!Stack) :-
    push(mapval(map.init), !Stack).

%-----------------------------------------------------------------------%
% keys: ( map -- array ) Get map keys as an array of quoted identifiers
%-----------------------------------------------------------------------%

operator_keys(!Stack) :-
    pop("keys", V, !Stack),
    ( if V = mapval(Map) then
        Keys = map.keys(Map),
        KeyTerms = list.map(
            (func(NameId) = termval(identifier(NameId))),
            Keys),
        push(arrayval(array.from_list(KeyTerms)), !Stack)
    else
        throw(type_error("map", V))
    ).

%-----------------------------------------------------------------------%
% :: ( map val 'key -- map ) Store value in map under key, return new map
%-----------------------------------------------------------------------%

operator_store(!Stack) :-
    pop(":", KeyVal, !Stack),
    pop(":", Val, !Stack),
    pop(":", MapVal, !Stack),
    ( if MapVal = mapval(Map), KeyVal = termval(identifier(NameId)) then
        map.set(NameId, Val, Map, NewMap),
        push(mapval(NewMap), !Stack)
    else if MapVal = mapval(_) then
        throw(type_error("term", KeyVal))
    else
        throw(type_error("map", MapVal))
    ).

%-----------------------------------------------------------------------%
% in: ( map 'key -- int ) Test if key exists in map: 0 if yes, 1 if no
%-----------------------------------------------------------------------%

operator_in(!Stack) :-
    pop("in", KeyVal, !Stack),
    pop("in", MapVal, !Stack),
    ( if MapVal = mapval(Map), KeyVal = termval(identifier(NameId)) then
        ( if map.contains(Map, NameId) then
            push(intval(0), !Stack)
        else
            push(intval(1), !Stack)
        )
    else if MapVal = mapval(_) then
        throw(type_error("term", KeyVal))
    else
        throw(type_error("map", MapVal))
    ).

%-----------------------------------------------------------------------%
% delete: ( map 'key -- map ) Remove key from map, return new map
%-----------------------------------------------------------------------%

operator_delete(!Stack) :-
    pop("delete", KeyVal, !Stack),
    pop("delete", MapVal, !Stack),
    ( if MapVal = mapval(Map), KeyVal = termval(identifier(NameId)) then
        map.delete(NameId, Map, NewMap),
        push(mapval(NewMap), !Stack)
    else if MapVal = mapval(_) then
        throw(type_error("term", KeyVal))
    else
        throw(type_error("map", MapVal))
    ).

%-----------------------------------------------------------------------%
% Type testing predicates
% Each returns 0 if true, 1 if false (following Froth's boolean convention)
%-----------------------------------------------------------------------%

operator_is_int(!Stack) :-
    pop("isInt", V, !Stack),
    ( if V = intval(_) then push(intval(0), !Stack)
    else push(intval(1), !Stack)
    ).

operator_is_string(!Stack) :-
    pop("isString", V, !Stack),
    ( if V = stringval(_) then push(intval(0), !Stack)
    else push(intval(1), !Stack)
    ).

operator_is_array(!Stack) :-
    pop("isArray", V, !Stack),
    ( if V = arrayval(_) then push(intval(0), !Stack)
    else push(intval(1), !Stack)
    ).

operator_is_map(!Stack) :-
    pop("isMap", V, !Stack),
    ( if V = mapval(_) then push(intval(0), !Stack)
    else push(intval(1), !Stack)
    ).

operator_is_nil(!Stack) :-
    pop("isNil", V, !Stack),
    ( if V = nilval then push(intval(0), !Stack)
    else push(intval(1), !Stack)
    ).

operator_is_cons(!Stack) :-
    pop("isCons", V, !Stack),
    ( if V = consval(_, _) then push(intval(0), !Stack)
    else push(intval(1), !Stack)
    ).

operator_is_ident(!Stack) :-
    pop("isIdent", V, !Stack),
    ( if V = termval(identifier(_)) then push(intval(0), !Stack)
    else push(intval(1), !Stack)
    ).

operator_is_binder(!Stack) :-
    pop("isBinder", V, !Stack),
    ( if V = termval(binder(_)) then push(intval(0), !Stack)
    else push(intval(1), !Stack)
    ).

operator_is_func(!Stack) :-
    pop("isFunc", V, !Stack),
    ( if V = termval(function(_)) then push(intval(0), !Stack)
    else push(intval(1), !Stack)
    ).

operator_is_gen(!Stack) :-
    pop("isGen", V, !Stack),
    ( if V = termval(generator(_)) then push(intval(0), !Stack)
    else push(intval(1), !Stack)
    ).

operator_is_quote(!Stack) :-
    pop("isQuote", V, !Stack),
    ( if V = termval(quoted(_)) then push(intval(0), !Stack)
    else push(intval(1), !Stack)
    ).

operator_is_apply(!Stack) :-
    pop("isApply", V, !Stack),
    ( if V = termval(apply_term) then push(intval(0), !Stack)
    else push(intval(1), !Stack)
    ).

%-----------------------------------------------------------------------%
% isValue: ( a -- int ) Test if value is a quoted value term
%-----------------------------------------------------------------------%

operator_is_value(!Stack) :-
    pop("isValue", V, !Stack),
    ( if V = termval(value(_)) then push(intval(0), !Stack)
    else push(intval(1), !Stack)
    ).

%-----------------------------------------------------------------------%
% unwrap: ( 'value -- value ) Extract value from quoted value term
%-----------------------------------------------------------------------%

operator_unwrap(!Stack) :-
    pop("unwrap", V, !Stack),
    ( if V = termval(value(Inner)) then
        push(Inner, !Stack)
    else if V = termval(quoted(InnerTerm)) then
        push(termval(InnerTerm), !Stack)
    else
        throw(type_error("quoted value or quoted term", V))
    ).

%-----------------------------------------------------------------------%
% intern: ( string|'ident|'binder -- int ) Get the intern id
%-----------------------------------------------------------------------%

operator_intern(!Stack) :-
    pop("intern", V, !Stack),
    ( if V = stringval(Id) then
        push(intval(Id), !Stack)
    else if V = termval(identifier(Id)) then
        push(intval(Id), !Stack)
    else if V = termval(binder(Id)) then
        push(intval(Id), !Stack)
    else
        throw(type_error("string, identifier, or binder", V))
    ).

%-----------------------------------------------------------------------%
% idToString: ( int -- string ) Create string from intern id
%-----------------------------------------------------------------------%

operator_id_to_string(!Stack) :-
    pop("idToString", V, !Stack),
    ( if V = intval(Id) then
        push(stringval(Id), !Stack)
    else
        throw(type_error("int", V))
    ).

%-----------------------------------------------------------------------%
% idToIdent: ( int -- 'ident ) Create quoted identifier from intern id
%-----------------------------------------------------------------------%

operator_id_to_ident(!Stack) :-
    pop("idToIdent", V, !Stack),
    ( if V = intval(Id) then
        push(termval(identifier(Id)), !Stack)
    else
        throw(type_error("int", V))
    ).

%-----------------------------------------------------------------------%
% idToBinder: ( int -- 'binder ) Create quoted binder from intern id
%-----------------------------------------------------------------------%

operator_id_to_binder(!Stack) :-
    pop("idToBinder", V, !Stack),
    ( if V = intval(Id) then
        push(termval(binder(Id)), !Stack)
    else
        throw(type_error("int", V))
    ).

%-----------------------------------------------------------------------%
% isOperator: ( 'ident -- int ) Test if identifier is an operator name
%-----------------------------------------------------------------------%

operator_is_operator(OpTable, !Stack) :-
    pop("isOperator", V, !Stack),
    ( if V = termval(identifier(Id)) then
        ( if map.contains(OpTable, Id) then
            push(intval(0), !Stack)
        else
            push(intval(1), !Stack)
        )
    else
        throw(type_error("identifier", V))
    ).

%-----------------------------------------------------------------------%
% arity: ( 'ident -- int ) Get the arity of an operator
%-----------------------------------------------------------------------%

operator_arity_op(OpTable, !Stack) :-
    pop("arity", V, !Stack),
    ( if V = termval(identifier(Id)) then
        ( if map.search(OpTable, Id, Info) then
            push(intval(Info ^ oi_arity), !Stack)
        else
            throw(type_error("operator", V))
        )
    else
        throw(type_error("identifier", V))
    ).

%-----------------------------------------------------------------------%
% stack: ( ... -- array ) Convert entire stack to an array
%-----------------------------------------------------------------------%

operator_stack(!Stack) :-
    % Stack is LIFO (top first), reverse to get push order (bottom first)
    Array = array.from_reverse_list(!.Stack),
    !:Stack = [arrayval(Array)].

%-----------------------------------------------------------------------%
% time: ( -- int ) Push current clock ticks
%-----------------------------------------------------------------------%

operator_time(!Stack, !IO) :-
    time.clock(Ticks, !IO),
    push(intval(Ticks), !Stack).

%-----------------------------------------------------------------------%
:- end_module operators.
%-----------------------------------------------------------------------%
