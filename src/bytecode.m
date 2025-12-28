%-----------------------------------------------------------------------%
% bytecode.m
% Bytecode representation for the Froth programming language.
%-----------------------------------------------------------------------%

:- module bytecode.
:- interface.

:- import_module array.

%-----------------------------------------------------------------------%

    % init(Array, Size):
    % Create an empty bytecode store with initial capacity.
    % Size is the number of bytecodes emitted (initially 0).
    %
:- pred init(array(int)::array_uo, int::out) is det.

    % emit(Value, !Array, !Size):
    % Append an integer to the bytecode store. Grows the array if needed.
    %
:- pred emit(int::in,
    array(int)::array_di, array(int)::array_uo,
    int::in, int::out) is det.

%-----------------------------------------------------------------------%

:- implementation.

:- import_module int.

%-----------------------------------------------------------------------%

:- func initial_capacity = int.

initial_capacity = 1024.

init(Array, 0) :-
    Array = array.init(initial_capacity, 0).

%-----------------------------------------------------------------------%

emit(Value, !Array, !Size) :-
    Capacity = array.size(!.Array),
    ( if !.Size >= Capacity then
        % Double the array size
        NewCapacity = Capacity * 2,
        Array1 = array.init(NewCapacity, 0),
        copy_elements(0, Capacity, !.Array, Array1, Array2),
        array.set(!.Size, Value, Array2, !:Array),
        !:Size = !.Size + 1
    else
        array.set(!.Size, Value, !Array),
        !:Size = !.Size + 1
    ).

:- pred copy_elements(int::in, int::in, array(int)::in,
    array(int)::array_di, array(int)::array_uo) is det.

copy_elements(I, Size, Src, !Dst) :-
    ( if I >= Size then
        true
    else
        array.lookup(Src, I, Value),
        array.set(I, Value, !Dst),
        copy_elements(I + 1, Size, Src, !Dst)
    ).

%-----------------------------------------------------------------------%
:- end_module bytecode.
%-----------------------------------------------------------------------%
