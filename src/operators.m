%-----------------------------------------------------------------------%
% operators.m
% Operator implementations for the Froth! programming language.
%-----------------------------------------------------------------------%

:- module operators.
:- interface.

:- import_module array.
:- import_module io.
:- import_module types.

%-----------------------------------------------------------------------%

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
:- pred operator_open(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_is_closure(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_apply_operator(operator_table::in, string_table::in, env::in,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out, io::di, io::uo) is det.
:- pred operator_wrap(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_mk_func(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.
:- pred operator_mk_gen(array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.

%-----------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module datastack.
:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module operator_table.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module time.
:- import_module values.

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
        Op = op_open,
        operator_open(!Array, !Ptr)
    ;
        Op = op_is_closure,
        operator_is_closure(!Array, !Ptr)
    ;
        Op = op_peek,
        % peek is handled specially in eval.m, should not reach here
        unexpected($pred, "peek should be handled in eval.m")
    ;
        Op = op_poke,
        % poke is handled specially in eval.m, should not reach here
        unexpected($pred, "poke should be handled in eval.m")
    ;
        Op = op_ref,
        % ref is handled specially in eval.m/vm.m, should not reach here
        unexpected($pred, "ref should be handled in eval.m/vm.m")
    ;
        Op = op_deref,
        % deref is handled specially in eval.m/vm.m, should not reach here
        unexpected($pred, "deref should be handled in eval.m/vm.m")
    ;
        Op = op_apply_operator,
        operator_apply_operator(OpTable, ST, Env, !Array, !Ptr, !IO)
    ;
        Op = op_wrap,
        operator_wrap(!Array, !Ptr)
    ;
        Op = op_mk_func,
        operator_mk_func(!Array, !Ptr)
    ;
        Op = op_mk_gen,
        operator_mk_gen(!Array, !Ptr)
    ).

%-----------------------------------------------------------------------%
% print: ( a -- ) Pop and print a value
%-----------------------------------------------------------------------%

operator_print(ST, !Array, !Ptr, !IO) :-
    datastack.pop("print", V, !Array, !Ptr),
    io.write_string(values.value_to_string(ST, V), !IO).

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
    ( if values_equal(V1, V2) then
        datastack.push(intval(0), !Array, !Ptr)
    else
        datastack.push(intval(1), !Array, !Ptr)
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
    io.write_string(values.value_to_write_string(ST, V), !IO).

%-----------------------------------------------------------------------%
% fwrite: ( value file -- ) Write value to file in executable form
%-----------------------------------------------------------------------%

operator_fwrite(ST, !Array, !Ptr, !IO) :-
    datastack.pop("fwrite", FileVal, !Array, !Ptr),
    datastack.pop("fwrite", V, !Array, !Ptr),
    Filename = values.value_to_string(ST, FileVal),
    Content = values.value_to_write_string(ST, V),
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
% Each returns 0 if true, 1 if false (following Froth!'s boolean convention)
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
% open: Decompose a closure into its components (inverse of close)
%   ( closure -- env body )      term closure -> map + function
%   ( bytecode -- context addr ) bytecode closure -> array + int
%-----------------------------------------------------------------------%

operator_open(!Array, !Ptr) :-
    datastack.pop("open", V, !Array, !Ptr),
    ( if V = closureval(Env, Body) then
        datastack.push(mapval(Env), !Array, !Ptr),
        datastack.push(termval(function(Body)), !Array, !Ptr)
    else if V = bytecodeval(Context, Addr) then
        datastack.push(arrayval(Context), !Array, !Ptr),
        datastack.push(intval(Addr), !Array, !Ptr)
    else
        throw(type_error("closure or bytecode", V))
    ).

%-----------------------------------------------------------------------%
% isClosure: ( a -- int ) Test if value is a closure
%-----------------------------------------------------------------------%

operator_is_closure(!Array, !Ptr) :-
    datastack.pop("isClosure", V, !Array, !Ptr),
    ( if ( V = closureval(_, _) ; V = bytecodeval(_, _) ) then
        datastack.push(intval(0), !Array, !Ptr)
    else
        datastack.push(intval(1), !Array, !Ptr)
    ).

%-----------------------------------------------------------------------%
% applyOperator: ( 'op -- ... ) Apply a quoted operator
%-----------------------------------------------------------------------%

operator_apply_operator(OpTable, ST, Env, !Array, !Ptr, !IO) :-
    datastack.pop("applyOperator", V, !Array, !Ptr),
    ( if V = termval(identifier(Id)) then
        ( if map.search(OpTable, Id, Info) then
            eval_operator(OpTable, ST, Info ^ oi_operator, Env,
                !Array, !Ptr, !IO)
        else
            throw(type_error("operator", V))
        )
    else
        throw(type_error("quoted operator", V))
    ).

%-----------------------------------------------------------------------%
% wrap: ( value -- 'value ) Wrap a value as a quoted term (inverse of unwrap)
%   int -> quoted int value
%   string -> quoted string value
%   quoted term -> quoted quote
%-----------------------------------------------------------------------%

operator_wrap(!Array, !Ptr) :-
    datastack.pop("wrap", V, !Array, !Ptr),
    ( if V = intval(_) then
        datastack.push(termval(value(V)), !Array, !Ptr)
    else if V = stringval(_) then
        datastack.push(termval(value(V)), !Array, !Ptr)
    else if V = termval(Term) then
        datastack.push(termval(quoted(Term)), !Array, !Ptr)
    else
        throw(type_error("int, string, or quoted term", V))
    ).

%-----------------------------------------------------------------------%
% mkFunc: ( array -- 'function ) Create a function term from array of terms
%-----------------------------------------------------------------------%

operator_mk_func(!Array, !Ptr) :-
    datastack.pop("mkFunc", V, !Array, !Ptr),
    ( if V = arrayval(Arr) then
        array_to_terms(Arr, Terms),
        datastack.push(termval(function(Terms)), !Array, !Ptr)
    else
        throw(type_error("array", V))
    ).

%-----------------------------------------------------------------------%
% mkGen: ( array -- 'generator ) Create a generator term from array of terms
%-----------------------------------------------------------------------%

operator_mk_gen(!Array, !Ptr) :-
    datastack.pop("mkGen", V, !Array, !Ptr),
    ( if V = arrayval(Arr) then
        array_to_terms(Arr, Terms),
        datastack.push(termval(generator(Terms)), !Array, !Ptr)
    else
        throw(type_error("array", V))
    ).

    % array_to_terms(Array, Terms):
    % Convert an array of termvals to a list of terms.
    % Throws a type error if any element is not a termval.
    %
:- pred array_to_terms(array(value)::in, list(term)::out) is det.

array_to_terms(Arr, Terms) :-
    array.to_list(Arr, Values),
    list.map(value_to_term, Values, Terms).

:- pred value_to_term(value::in, term::out) is det.

value_to_term(V, Term) :-
    ( if V = termval(T) then
        Term = T
    else
        throw(type_error("term", V))
    ).

%-----------------------------------------------------------------------%
:- end_module operators.
%-----------------------------------------------------------------------%
