%-----------------------------------------------------------------------%
% values.m
% Predicates and functions for working with Froth! values.
%-----------------------------------------------------------------------%

:- module values.
:- interface.

:- import_module list.

:- import_module types.

%-----------------------------------------------------------------------%

    % values_equal(V1, V2):
    % Test structural equality of two values.
    % Succeeds if equal, fails if not equal.
    %
:- pred values_equal(value::in, value::in) is semidet.

    % term_equal(T1, T2):
    % Test structural equality of two terms.
    % Uses values_equal for value terms.
    %
:- pred term_equal(term::in, term::in) is semidet.

    % terms_equal(Ts1, Ts2):
    % Test structural equality of two term lists.
    %
:- pred terms_equal(list(term)::in, list(term)::in) is semidet.

    % value_to_string(ST, Value) = String:
    % Convert a value to its display string representation (for print).
    %
:- func value_to_string(string_table, value) = string.

    % value_to_write_string(ST, Value) = String:
    % Convert a value to its executable (round-trippable) string form.
    %
:- func value_to_write_string(string_table, value) = string.

    % term_to_string(ST, Term) = String:
    % Convert a term to its display string representation.
    %
:- func term_to_string(string_table, term) = string.

    % value_hash_pred(Value, Hash):
    % Compute a hash value for use in hash tables.
    % Currently a placeholder that only hashes simple types properly.
    %
:- pred value_hash_pred(value::in, int::out) is det.

%-----------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module assoc_list.
:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module string.

%-----------------------------------------------------------------------%
% Value equality
%-----------------------------------------------------------------------%

values_equal(intval(I), intval(I)).
values_equal(stringval(Id), stringval(Id)).
values_equal(arrayval(A1), arrayval(A2)) :-
    arrays_equal(A1, A2).
values_equal(mapval(M1), mapval(M2)) :-
    maps_equal(M1, M2).
values_equal(termval(T1), termval(T2)) :-
    term_equal(T1, T2).
values_equal(nilval, nilval).
values_equal(consval(H1, T1), consval(H2, T2)) :-
    values_equal(H1, H2),
    values_equal(T1, T2).
values_equal(closureval(Env1, Body1), closureval(Env2, Body2)) :-
    terms_equal(Body1, Body2),
    maps_equal(Env1, Env2).
values_equal(bytecodeval(Ctx1, Addr), bytecodeval(Ctx2, Addr)) :-
    arrays_equal(Ctx1, Ctx2).

:- pred arrays_equal(array(value)::in, array(value)::in) is semidet.

arrays_equal(A1, A2) :-
    Size = array.size(A1),
    Size = array.size(A2),
    arrays_equal_loop(A1, A2, 0, Size).

:- pred arrays_equal_loop(array(value)::in, array(value)::in,
    int::in, int::in) is semidet.

arrays_equal_loop(A1, A2, I, Size) :-
    ( if I >= Size then
        true
    else
        array.lookup(A1, I, V1),
        array.lookup(A2, I, V2),
        values_equal(V1, V2),
        arrays_equal_loop(A1, A2, I + 1, Size)
    ).

:- pred maps_equal(map(string_id, value)::in, map(string_id, value)::in)
    is semidet.

maps_equal(M1, M2) :-
    map.to_assoc_list(M1, AL1),
    map.to_assoc_list(M2, AL2),
    assoc_lists_equal(AL1, AL2).

:- pred assoc_lists_equal(assoc_list(string_id, value)::in,
    assoc_list(string_id, value)::in) is semidet.

assoc_lists_equal([], []).
assoc_lists_equal([K - V1 | Rest1], [K - V2 | Rest2]) :-
    values_equal(V1, V2),
    assoc_lists_equal(Rest1, Rest2).

%-----------------------------------------------------------------------%
% Term equality
%-----------------------------------------------------------------------%

term_equal(identifier(Id), identifier(Id)).
term_equal(binder(Id), binder(Id)).
term_equal(function(Ts1), function(Ts2)) :-
    terms_equal(Ts1, Ts2).
term_equal(generator(Ts1), generator(Ts2)) :-
    terms_equal(Ts1, Ts2).
term_equal(quoted(T1), quoted(T2)) :-
    term_equal(T1, T2).
term_equal(value(V1), value(V2)) :-
    values_equal(V1, V2).
term_equal(apply_term, apply_term).

terms_equal([], []).
terms_equal([T1 | Ts1], [T2 | Ts2]) :-
    term_equal(T1, T2),
    terms_equal(Ts1, Ts2).

%-----------------------------------------------------------------------%
% value_to_string: convert a value to its display representation
%-----------------------------------------------------------------------%

value_to_string(_, intval(I)) = int_to_string(I).
value_to_string(ST, stringval(StrId)) = lookup_string(ST, StrId).
value_to_string(ST, arrayval(A)) = String :-
    array.to_list(A, List),
    Strings = list.map(value_to_string(ST), List),
    String = string.append_list(Strings).
value_to_string(_, mapval(M)) = string.format("<map:%d>", [i(map.count(M))]).
value_to_string(ST, termval(T)) = term_to_string(ST, T).
value_to_string(_, nilval) = "".
value_to_string(ST, consval(H, T)) =
    value_to_string(ST, H) ++ value_to_string(ST, T).
value_to_string(ST, closureval(_, Body)) =
    "<closure:" ++ terms_to_string(ST, Body) ++ ">".
value_to_string(_, bytecodeval(_, Addr)) =
    "<bytecode:" ++ int_to_string(Addr) ++ ">".

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
% value_to_write_string: convert a value to executable string form
%-----------------------------------------------------------------------%

value_to_write_string(_, intval(I)) = int_to_string(I).
value_to_write_string(ST, stringval(StrId)) =
    "\"" ++ types.escape_string(lookup_string(ST, StrId)) ++ "\"".
value_to_write_string(ST, arrayval(A)) = "[ " ++ ArrayElems ++ "]" :-
    array.to_list(A, List),
    ElemStrings = list.map(
        (func(V) = value_to_write_string(ST, V) ++ " "),
        List),
    ArrayElems = string.append_list(ElemStrings).
value_to_write_string(ST, mapval(M)) = Result :-
    map.to_assoc_list(M, Pairs),
    % Sort by key name (not string_id) for deterministic output
    list.sort(compare_by_key_name(ST), Pairs, SortedPairs),
    list.foldl(map_entry_to_string(ST), SortedPairs, "$", Result).
value_to_write_string(ST, termval(T)) = "'" ++ term_to_write_string(ST, T).
value_to_write_string(_, nilval) = ".".
value_to_write_string(ST, consval(H, T)) =
    value_to_write_string(ST, T) ++ " " ++ value_to_write_string(ST, H) ++ " ,".
value_to_write_string(ST, closureval(_, Body)) =
    "<closure:{ " ++ terms_to_write_string(ST, Body) ++ "}>".
value_to_write_string(_, bytecodeval(_, Addr)) =
    "<bytecode:" ++ int_to_string(Addr) ++ ">".

:- pred compare_by_key_name(string_table::in,
    pair(string_id, value)::in, pair(string_id, value)::in,
    comparison_result::out) is det.

compare_by_key_name(ST, K1 - _, K2 - _, Result) :-
    compare(Result, lookup_string(ST, K1), lookup_string(ST, K2)).

:- pred map_entry_to_string(string_table::in, pair(string_id, value)::in,
    string::in, string::out) is det.

map_entry_to_string(ST, NameId - V, !Acc) :-
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
% value_hash_pred: compute hash for hash table deduplication
%-----------------------------------------------------------------------%

value_hash_pred(intval(I), I).
value_hash_pred(stringval(Id), Id).
value_hash_pred(arrayval(_), 0).
value_hash_pred(mapval(_), 0).
value_hash_pred(termval(_), 0).
value_hash_pred(nilval, 0).
value_hash_pred(consval(_, _), 0).
value_hash_pred(closureval(_, _), 0).
value_hash_pred(bytecodeval(_, Addr), Addr).

%-----------------------------------------------------------------------%
:- end_module values.
%-----------------------------------------------------------------------%
