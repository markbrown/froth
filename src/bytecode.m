%-----------------------------------------------------------------------%
% bytecode.m
% Bytecode store for the Froth programming language.
%-----------------------------------------------------------------------%

:- module bytecode.
:- interface.

:- import_module array.

%-----------------------------------------------------------------------%

    % init(Array):
    % Create a bytecode store with initial capacity, filled with zeros.
    %
:- pred init(array(int)::array_uo) is det.

    % peek(Addr, Array) = Value:
    % Read value at address. Returns 0 for addresses >= array size.
    % Throws index_out_of_bounds for negative addresses.
    %
:- func peek(int, array(int)) = int.

    % poke(Addr, Value, !Array):
    % Write value at address. Extends array if needed (doubling capacity).
    % Throws index_out_of_bounds for negative addresses.
    %
:- pred poke(int::in, int::in,
    array(int)::array_di, array(int)::array_uo) is det.

%-----------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module types.

%-----------------------------------------------------------------------%

:- func initial_capacity = int.

initial_capacity = 1024.

init(Array) :-
    Array = array.init(initial_capacity, 0).

%-----------------------------------------------------------------------%

peek(Addr, Array) = Value :-
    ( if Addr < 0 then
        throw(index_out_of_bounds(Addr, array.size(Array)))
    else if Addr >= array.size(Array) then
        Value = 0
    else
        array.lookup(Array, Addr, Value)
    ).

%-----------------------------------------------------------------------%

poke(Addr, Value, !Array) :-
    ( if Addr < 0 then
        throw(index_out_of_bounds(Addr, array.size(!.Array)))
    else
        ensure_capacity(Addr + 1, !Array),
        array.set(Addr, Value, !Array)
    ).

:- pred ensure_capacity(int::in,
    array(int)::array_di, array(int)::array_uo) is det.

ensure_capacity(Required, !Array) :-
    Size = array.size(!.Array),
    ( if Required =< Size then
        true
    else
        % Double until big enough
        NewSize = find_new_size(Size, Required),
        grow_array(NewSize, !Array)
    ).

:- func find_new_size(int, int) = int.

find_new_size(Current, Required) =
    ( if Current >= Required then
        Current
    else
        find_new_size(Current * 2, Required)
    ).

:- pred grow_array(int::in,
    array(int)::array_di, array(int)::array_uo) is det.

grow_array(NewSize, !Array) :-
    OldSize = array.size(!.Array),
    NewArray = array.init(NewSize, 0),
    copy_elements(0, OldSize, !.Array, NewArray, !:Array).

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
