%-----------------------------------------------------------------------%
% value_format.m
% String conversion utilities for Froth values and terms.
%-----------------------------------------------------------------------%

:- module value_format.
:- interface.

:- import_module types.

%-----------------------------------------------------------------------%

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

%-----------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module string.

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
:- end_module value_format.
%-----------------------------------------------------------------------%
