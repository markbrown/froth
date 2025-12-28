%-----------------------------------------------------------------------%
% datastack.m
% Array-based stack implementation for the Froth programming language.
%-----------------------------------------------------------------------%

:- module datastack.
:- interface.

:- import_module array.
:- import_module list.
:- import_module types.

%-----------------------------------------------------------------------%

    % init(Array, Ptr):
    % Create an empty stack with initial capacity of 1024 elements.
    %
:- pred init(array(value)::array_uo, int::out) is det.

    % push(Value, !Array, !Ptr):
    % Push a value onto the stack. Doubles capacity if needed.
    %
:- pred push(value::in,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.

    % pop(Op, Value, !Array, !Ptr):
    % Pop a value from the stack. Throws stack_underflow(Op) if empty.
    %
:- pred pop(string::in, value::out,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out) is det.

    % is_empty(Ptr):
    % Succeeds if the stack is empty.
    %
:- pred is_empty(int::in) is semidet.

    % to_list(Array, Ptr) = List:
    % Convert stack to a list (top of stack is head of list).
    %
:- func to_list(array(value), int) = list(value).

    % from_list(List, Array, Ptr):
    % Create a stack from a list (head of list becomes top of stack).
    %
:- pred from_list(list(value)::in,
    array(value)::array_uo, int::out) is det.

%-----------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int.

%-----------------------------------------------------------------------%

:- func initial_capacity = int.

initial_capacity = 1024.

%-----------------------------------------------------------------------%

init(Array, 0) :-
    Array = array.init(initial_capacity, nilval).

%-----------------------------------------------------------------------%

push(Value, !Array, !Ptr) :-
    Size = array.size(!.Array),
    ( if !.Ptr >= Size then
        % Double the array size
        NewSize = Size * 2,
        Array1 = array.init(NewSize, nilval),
        copy_elements(0, Size, !.Array, Array1, Array2),
        array.set(!.Ptr, Value, Array2, !:Array),
        !:Ptr = !.Ptr + 1
    else
        array.set(!.Ptr, Value, !Array),
        !:Ptr = !.Ptr + 1
    ).

:- pred copy_elements(int::in, int::in, array(value)::in,
    array(value)::array_di, array(value)::array_uo) is det.

copy_elements(I, Size, Src, !Dst) :-
    ( if I >= Size then
        true
    else
        array.lookup(Src, I, Value),
        array.set(I, Value, !Dst),
        copy_elements(I + 1, Size, Src, !Dst)
    ).

%-----------------------------------------------------------------------%

pop(Op, Value, !Array, !Ptr) :-
    ( if !.Ptr > 0 then
        !:Ptr = !.Ptr - 1,
        array.lookup(!.Array, !.Ptr, Value),
        % Clear the slot to avoid holding references
        array.set(!.Ptr, nilval, !Array)
    else
        throw(stack_underflow(Op))
    ).

%-----------------------------------------------------------------------%

is_empty(0).

%-----------------------------------------------------------------------%

to_list(Array, Ptr) = List :-
    to_list_loop(Array, Ptr - 1, [], List).

:- pred to_list_loop(array(value)::in, int::in,
    list(value)::in, list(value)::out) is det.

to_list_loop(Array, I, !List) :-
    ( if I < 0 then
        true
    else
        array.lookup(Array, I, Value),
        !:List = [Value | !.List],
        to_list_loop(Array, I - 1, !List)
    ).

%-----------------------------------------------------------------------%

from_list(List, Array, Length) :-
    Length = list.length(List),
    Capacity = max(initial_capacity, next_power_of_2(Length)),
    Array0 = array.init(Capacity, nilval),
    from_list_loop(List, 0, Array0, Array).

:- pred from_list_loop(list(value)::in, int::in,
    array(value)::array_di, array(value)::array_uo) is det.

from_list_loop([], _, !Array).
from_list_loop([V | Vs], I, !Array) :-
    array.set(I, V, !Array),
    from_list_loop(Vs, I + 1, !Array).

:- func next_power_of_2(int) = int.

next_power_of_2(N) = Result :-
    next_power_of_2_loop(1, N, Result).

:- pred next_power_of_2_loop(int::in, int::in, int::out) is det.

next_power_of_2_loop(P, N, Result) :-
    ( if P >= N then
        Result = P
    else
        next_power_of_2_loop(P * 2, N, Result)
    ).

%-----------------------------------------------------------------------%
:- end_module datastack.
%-----------------------------------------------------------------------%
