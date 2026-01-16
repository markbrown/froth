%-----------------------------------------------------------------------%
% pool.m
% Constant pool operations with deep referencing for value serialization.
%-----------------------------------------------------------------------%

:- module pool.
:- interface.

:- import_module array.
:- import_module hash_table.

:- import_module types.

%-----------------------------------------------------------------------%

    % deep_ref(Value, Index, !Pool, !HashTable):
    % Store Value in the constant pool, recursively pooling structured
    % subterms first. Returns the pool index, or -1 for primitive values
    % (int, string, nil) and closurevals (which must be compiled first).
    %
    % Deduplication: if Value is already in the pool, returns existing index.
    %
    % For structured values (array, map, cons, bytecodeval), subterms are
    % recursively pooled before the value itself, ensuring bottom-up ordering.
    % This enables reconstruction code to use deref for shared subterms.
    %
    % termval is not recursed into (terms are syntax, not data structures).
    %
:- pred deep_ref(value::in, int::out,
    array(value)::array_di, array(value)::array_uo,
    hash_table(value, int)::hash_table_di,
    hash_table(value, int)::hash_table_uo) is det.

%-----------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module map.

%-----------------------------------------------------------------------%

deep_ref(V, Idx, !Pool, !HashTable) :-
    ( if is_primitive_or_closure(V) then
        Idx = -1
    else if hash_table.search(!.HashTable, V, ExistingIdx) then
        Idx = ExistingIdx
    else
        % Recurse into subterms first (ignore results, just populates pool)
        deep_ref_subterms(V, !Pool, !HashTable),
        % Add this value to pool
        NewIdx = array.size(!.Pool),
        array.resize(NewIdx + 1, V, !Pool),
        hash_table.det_insert(V, NewIdx, !HashTable),
        Idx = NewIdx
    ).

    % is_primitive_or_closure(V):
    % True if V should return -1 from deep_ref (not pooled).
    %
:- pred is_primitive_or_closure(value::in) is semidet.

is_primitive_or_closure(intval(_)).
is_primitive_or_closure(stringval(_)).
is_primitive_or_closure(nilval).
is_primitive_or_closure(closureval(_, _)).

    % deep_ref_subterms(V, !Pool, !HashTable):
    % Recursively ref all subterms of V. Results are ignored; this just
    % ensures subterms are in the pool before V is added.
    %
:- pred deep_ref_subterms(value::in,
    array(value)::array_di, array(value)::array_uo,
    hash_table(value, int)::hash_table_di,
    hash_table(value, int)::hash_table_uo) is det.

deep_ref_subterms(V, !Pool, !HashTable) :-
    (
        V = arrayval(Arr),
        deep_ref_array(Arr, 0, !Pool, !HashTable)
    ;
        V = mapval(Map),
        map.values(Map, Values),
        deep_ref_list(Values, !Pool, !HashTable)
    ;
        V = consval(Head, Tail),
        deep_ref(Head, _, !Pool, !HashTable),
        deep_ref(Tail, _, !Pool, !HashTable)
    ;
        V = bytecodeval(Context, _),
        deep_ref(arrayval(Context), _, !Pool, !HashTable)
    ;
        V = termval(_)
        % Don't recurse into terms
    ;
        % Primitives and closures don't reach here (filtered by is_primitive_or_closure)
        % but Mercury requires exhaustive cases
        V = intval(_)
    ;
        V = stringval(_)
    ;
        V = nilval
    ;
        V = closureval(_, _)
    ).

    % deep_ref_array(Array, Index, !Pool, !HashTable):
    % Recursively ref each element of Array starting at Index.
    %
:- pred deep_ref_array(array(value)::in, int::in,
    array(value)::array_di, array(value)::array_uo,
    hash_table(value, int)::hash_table_di,
    hash_table(value, int)::hash_table_uo) is det.

deep_ref_array(Arr, I, !Pool, !HashTable) :-
    ( if I < array.size(Arr) then
        array.lookup(Arr, I, Elem),
        deep_ref(Elem, _, !Pool, !HashTable),
        deep_ref_array(Arr, I + 1, !Pool, !HashTable)
    else
        true
    ).

    % deep_ref_list(Values, !Pool, !HashTable):
    % Recursively ref each value in the list.
    %
:- pred deep_ref_list(list(value)::in,
    array(value)::array_di, array(value)::array_uo,
    hash_table(value, int)::hash_table_di,
    hash_table(value, int)::hash_table_uo) is det.

deep_ref_list([], !Pool, !HashTable).
deep_ref_list([V | Vs], !Pool, !HashTable) :-
    deep_ref(V, _, !Pool, !HashTable),
    deep_ref_list(Vs, !Pool, !HashTable).

%-----------------------------------------------------------------------%
:- end_module pool.
%-----------------------------------------------------------------------%
