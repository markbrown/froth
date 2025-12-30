%-----------------------------------------------------------------------%
% stack_test.m
% Test module for the array-based stack implementation.
%-----------------------------------------------------------------------%

:- module stack_test.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module string.
:- import_module types.
:- import_module datastack.

%-----------------------------------------------------------------------%

main(!IO) :-
    io.write_string("=== Stack Tests ===\n", !IO),

    % Test 1: Basic push and pop
    io.write_string("\nTest 1: Basic push and pop\n", !IO),
    datastack.init(A0, P0),
    datastack.push(intval(1), A0, A1, P0, P1),
    datastack.push(intval(2), A1, A2, P1, P2),
    datastack.push(intval(3), A2, A3, P2, P3),
    io.format("Depth after 3 pushes: %d (expected 3)\n", [i(P3)], !IO),
    datastack.pop("test", V1, A3, A4, P3, P4),
    datastack.pop("test", V2, A4, A5, P4, P5),
    datastack.pop("test", V3, A5, _, P5, P6),
    io.format("Popped: %s, %s, %s (expected 3, 2, 1)\n",
        [s(value_to_string(V1)), s(value_to_string(V2)), s(value_to_string(V3))], !IO),
    io.format("Depth after 3 pops: %d (expected 0)\n", [i(P6)], !IO),
    ( if datastack.is_empty(P6) then
        io.write_string("Stack is empty: PASS\n", !IO)
    else
        io.write_string("Stack is empty: FAIL\n", !IO)
    ),

    % Test 2: to_list and from_list
    io.write_string("\nTest 2: to_list and from_list\n", !IO),
    datastack.init(A10, P10),
    datastack.push(intval(10), A10, A11, P10, P11),
    datastack.push(intval(20), A11, A12, P11, P12),
    datastack.push(intval(30), A12, A13, P12, P13),
    List1 = datastack.to_list(A13, P13),
    io.format("to_list: %s (expected [10, 20, 30])\n",
        [s(list_to_string(List1))], !IO),
    datastack.from_list([intval(100), intval(200)], A14, P14),
    io.format("from_list depth: %d (expected 2)\n", [i(P14)], !IO),
    datastack.pop("test", V10, A14, A15, P14, P15),
    datastack.pop("test", V11, A15, _, P15, _),
    io.format("Popped from from_list: %s, %s (expected 200, 100)\n",
        [s(value_to_string(V10)), s(value_to_string(V11))], !IO),

    % Test 3: Stack overflow (doubling)
    io.write_string("\nTest 3: Stack growth (pushing 2000 elements)\n", !IO),
    datastack.init(A20, P20),
    push_n(2000, A20, A21, P20, P21),
    io.format("Depth after 2000 pushes: %d (expected 2000)\n", [i(P21)], !IO),
    pop_n(2000, A21, A22, P21, P22),
    io.format("Depth after 2000 pops: %d (expected 0)\n", [i(P22)], !IO),
    _ = A22,  % Suppress unused warning

    % Test 4: Stack underflow
    io.write_string("\nTest 4: Stack underflow\n", !IO),
    datastack.init(A30, P30),
    ( try []
        datastack.pop("underflow_test", _, A30, _, P30, _)
    then
        io.write_string("Underflow: FAIL (no exception)\n", !IO)
    catch stack_underflow(Op) ->
        io.format("Underflow caught for '%s': PASS\n", [s(Op)], !IO)
    ),

    % Test 5: Mixed value types
    io.write_string("\nTest 5: Mixed value types\n", !IO),
    datastack.init(A40, P40),
    datastack.push(intval(42), A40, A41, P40, P41),
    datastack.push(nilval, A41, A42, P41, P42),
    datastack.push(consval(intval(1), intval(2)), A42, A43, P42, P43),
    io.format("Depth: %d (expected 3)\n", [i(P43)], !IO),
    datastack.pop("test", V40, A43, A44, P43, P44),
    datastack.pop("test", V41, A44, A45, P44, P45),
    datastack.pop("test", V42, A45, _, P45, _),
    io.format("Types: %s, %s, %s (expected cons, nil, int)\n",
        [s(value_type(V40)), s(value_type(V41)), s(value_type(V42))], !IO),

    io.write_string("\n=== All tests completed ===\n", !IO).

%-----------------------------------------------------------------------%

:- pred push_n(int::in,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.

push_n(N, !Array, !Ptr) :-
    ( if N =< 0 then
        true
    else
        datastack.push(intval(N), !Array, !Ptr),
        push_n(N - 1, !Array, !Ptr)
    ).

:- pred pop_n(int::in,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.

pop_n(N, !Array, !Ptr) :-
    ( if N =< 0 then
        true
    else
        datastack.pop("pop_n", _, !Array, !Ptr),
        pop_n(N - 1, !Array, !Ptr)
    ).

:- func value_to_string(value) = string.

value_to_string(intval(I)) = string.int_to_string(I).
value_to_string(stringval(Id)) = string.format("string(%d)", [i(Id)]).
value_to_string(arrayval(_)) = "array".
value_to_string(mapval(_)) = "map".
value_to_string(termval(_)) = "term".
value_to_string(nilval) = "nil".
value_to_string(consval(_, _)) = "cons".
value_to_string(closureval(_, _)) = "closure".
value_to_string(bytecodeval(_, _)) = "bytecode".

:- func value_type(value) = string.

value_type(intval(_)) = "int".
value_type(stringval(_)) = "string".
value_type(arrayval(_)) = "array".
value_type(mapval(_)) = "map".
value_type(termval(_)) = "term".
value_type(nilval) = "nil".
value_type(consval(_, _)) = "cons".
value_type(closureval(_, _)) = "closure".
value_type(bytecodeval(_, _)) = "bytecode".

:- func list_to_string(list(value)) = string.

list_to_string(List) = Result :-
    Strs = list.map(value_to_string, List),
    Result = "[" ++ string.join_list(", ", Strs) ++ "]".

%-----------------------------------------------------------------------%
:- end_module stack_test.
%-----------------------------------------------------------------------%
