%-----------------------------------------------------------------------%
% builtins.m
% Builtin operations for the Froth programming language.
%-----------------------------------------------------------------------%

:- module builtins.
:- interface.

:- import_module io.
:- import_module types.

%-----------------------------------------------------------------------%

    % Builtin operations.
    % Each constructor corresponds to a language operator.
    %
:- type builtin
    --->    bi_print        % print  ( a -- )
    ;       bi_dump         % dump   ( -- )
    ;       bi_env          % env    ( -- map )
    ;       bi_add          % +      ( a b -- a+b )
    ;       bi_get          % @      ( container key -- val )
    ;       bi_length       % #      ( container -- int )
    ;       bi_eq           % =      ( a b -- int )
    ;       bi_ite          % ?      ( cond then else -- result )
    ;       bi_nil          % .      ( -- nil )
    ;       bi_cons         % ,      ( tail head -- cons )
    ;       bi_fst          % fst    ( cons -- head )
    ;       bi_snd          % snd    ( cons -- tail )
    ;       bi_write        % write  ( a -- )
    ;       bi_fwrite       % fwrite ( value file -- )
    ;       bi_empty        % $      ( -- map )
    ;       bi_keys         % keys   ( map -- array )
    ;       bi_store.       % :      ( map val 'key -- map )

    % builtin(Name, Builtin):
    % Map a name to a builtin operation.
    %
:- pred builtin(string::in, builtin::out) is semidet.

    % Individual builtin operations.
    %
:- pred builtin_print(intern_table::in, stack::in, stack::out,
    io::di, io::uo) is det.
:- pred builtin_dump(intern_table::in, stack::in, io::di, io::uo) is det.
:- pred builtin_env(env::in, stack::in, stack::out) is det.
:- pred builtin_add(stack::in, stack::out) is det.
:- pred builtin_get(stack::in, stack::out) is det.
:- pred builtin_length(stack::in, stack::out) is det.
:- pred builtin_eq(stack::in, stack::out) is det.
:- pred builtin_ite(stack::in, stack::out) is det.
:- pred builtin_nil(stack::in, stack::out) is det.
:- pred builtin_cons(stack::in, stack::out) is det.
:- pred builtin_fst(stack::in, stack::out) is det.
:- pred builtin_snd(stack::in, stack::out) is det.
:- pred builtin_write(intern_table::in, stack::in, stack::out,
    io::di, io::uo) is det.
:- pred builtin_fwrite(intern_table::in, stack::in, stack::out,
    io::di, io::uo) is det.
:- pred builtin_empty(stack::in, stack::out) is det.
:- pred builtin_keys(stack::in, stack::out) is det.
:- pred builtin_store(stack::in, stack::out) is det.

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
:- import_module string.

%-----------------------------------------------------------------------%

builtin("print", bi_print).
builtin("dump", bi_dump).
builtin("env", bi_env).
builtin("+", bi_add).
builtin("@", bi_get).
builtin("#", bi_length).
builtin("=", bi_eq).
builtin("?", bi_ite).
builtin(".", bi_nil).
builtin(",", bi_cons).
builtin("fst", bi_fst).
builtin("snd", bi_snd).
builtin("write", bi_write).
builtin("fwrite", bi_fwrite).
builtin("$", bi_empty).
builtin("keys", bi_keys).
builtin(":", bi_store).

%-----------------------------------------------------------------------%
% print: ( a -- ) Pop and print a value
%-----------------------------------------------------------------------%

builtin_print(IT, !Stack, !IO) :-
    pop("print", V, !Stack),
    io.write_string(value_to_string(IT, V), !IO),
    io.nl(!IO).

%-----------------------------------------------------------------------%
% value_to_string: convert a value to its string representation
%-----------------------------------------------------------------------%

:- func value_to_string(intern_table, value) = string.

value_to_string(_, intval(I)) = int_to_string(I).
value_to_string(IT, stringval(StrId)) = lookup_string(IT ^ it_strings, StrId).
value_to_string(IT, arrayval(A)) = String :-
    array.to_list(A, List),
    Strings = list.map(value_to_string(IT), List),
    String = string.append_list(Strings).
value_to_string(_, mapval(M)) = string.format("<map:%d>", [i(map.count(M))]).
value_to_string(IT, termval(T)) = term_to_string(IT, T).
value_to_string(_, nilval) = ".".
value_to_string(IT, consval(H, T)) =
    "(" ++ value_to_string(IT, H) ++ "," ++ value_to_string(IT, T) ++ ")".

:- func term_to_string(intern_table, term) = string.

term_to_string(IT, identifier(NameId)) = lookup_name(IT ^ it_names, NameId).
term_to_string(IT, binder(NameId)) = "/" ++ lookup_name(IT ^ it_names, NameId).
term_to_string(IT, function(Terms)) = "{ " ++ terms_to_string(IT, Terms) ++ "}".
term_to_string(IT, generator(Terms)) = "[ " ++ terms_to_string(IT, Terms) ++ "]".
term_to_string(IT, quoted(T)) = "'" ++ term_to_string(IT, T).
term_to_string(IT, value(V)) = value_to_string(IT, V).
term_to_string(_, apply_term) = "!".

:- func terms_to_string(intern_table, list(term)) = string.

terms_to_string(_, []) = "".
terms_to_string(IT, [T | Ts]) =
    term_to_string(IT, T) ++ " " ++ terms_to_string(IT, Ts).

%-----------------------------------------------------------------------%
% dump: ( -- ) Print the entire stack (for debugging)
%-----------------------------------------------------------------------%

builtin_dump(IT, Stack, !IO) :-
    io.write_string("--- stack ---\n", !IO),
    list.foldl(print_stack_entry(IT), Stack, !IO),
    io.write_string("-------------\n", !IO).

:- pred print_stack_entry(intern_table::in, value::in, io::di, io::uo) is det.

print_stack_entry(IT, V, !IO) :-
    io.write_string("  ", !IO),
    print_value_debug(IT, V, !IO),
    io.nl(!IO).

:- pred print_value_debug(intern_table::in, value::in, io::di, io::uo) is det.

print_value_debug(_, intval(I), !IO) :-
    io.format("int(%d)", [i(I)], !IO).
print_value_debug(IT, stringval(Id), !IO) :-
    io.format("string(\"%s\")", [s(lookup_string(IT ^ it_strings, Id))], !IO).
print_value_debug(IT, arrayval(A), !IO) :-
    io.write_string("array(", !IO),
    array.foldl(print_array_elem_debug(IT), A, !IO),
    io.write_string(")", !IO).
print_value_debug(_, mapval(M), !IO) :-
    io.format("map(%d)", [i(map.count(M))], !IO).
print_value_debug(IT, termval(T), !IO) :-
    io.write_string("term(", !IO),
    io.write_string(term_to_string(IT, T), !IO),
    io.write_string(")", !IO).
print_value_debug(_, nilval, !IO) :-
    io.write_string("nil", !IO).
print_value_debug(IT, consval(H, T), !IO) :-
    io.write_string("cons(", !IO),
    print_value_debug(IT, H, !IO),
    io.write_string(", ", !IO),
    print_value_debug(IT, T, !IO),
    io.write_string(")", !IO).

:- pred print_array_elem_debug(intern_table::in, value::in,
    io::di, io::uo) is det.

print_array_elem_debug(IT, V, !IO) :-
    print_value_debug(IT, V, !IO),
    io.write_string(" ", !IO).

%-----------------------------------------------------------------------%
% env: ( -- map ) Push the current environment as a map
%-----------------------------------------------------------------------%

builtin_env(Env, !Stack) :-
    push(mapval(Env), !Stack).

%-----------------------------------------------------------------------%
% +: ( int int -- int ) Add two integers
%-----------------------------------------------------------------------%

builtin_add(!Stack) :-
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
% @: ( container key -- val ) Get element from array by index or from map by key
% For arrays: key must be int. For maps: key must be quoted identifier ('name).
%-----------------------------------------------------------------------%

builtin_get(!Stack) :-
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
    else if ContainerVal = arrayval(_) then
        throw(type_error("int", KeyVal))
    else if ContainerVal = mapval(_) then
        throw(type_error("term", KeyVal))
    else
        throw(type_error("array or map", ContainerVal))
    ).

%-----------------------------------------------------------------------%
% #: ( container -- int ) Get length of array or size of map
%-----------------------------------------------------------------------%

builtin_length(!Stack) :-
    pop("#", V, !Stack),
    ( if V = arrayval(Array) then
        push(intval(array.size(Array)), !Stack)
    else if V = mapval(Map) then
        push(intval(map.count(Map)), !Stack)
    else
        throw(type_error("array or map", V))
    ).

%-----------------------------------------------------------------------%
% =: ( a b -- int ) Test equality: push 0 if equal, 1 if not equal
%-----------------------------------------------------------------------%

builtin_eq(!Stack) :-
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
:- pred maps_equal(map(name_id, value)::in, map(name_id, value)::in,
    bool::out) is semidet.

maps_equal(M1, M2, Equal) :-
    map.to_assoc_list(M1, AL1),
    map.to_assoc_list(M2, AL2),
    assoc_lists_equal(AL1, AL2, Equal).

:- pred assoc_lists_equal(assoc_list(name_id, value)::in,
    assoc_list(name_id, value)::in, bool::out) is semidet.

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

builtin_ite(!Stack) :-
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

builtin_nil(!Stack) :-
    push(nilval, !Stack).

%-----------------------------------------------------------------------%
% ,: ( tail head -- cons ) Create a cons cell with head and tail
%-----------------------------------------------------------------------%

builtin_cons(!Stack) :-
    pop(",", Head, !Stack),
    pop(",", Tail, !Stack),
    push(consval(Head, Tail), !Stack).

%-----------------------------------------------------------------------%
% fst: ( cons -- head ) Get the first element (head) of a cons cell
%-----------------------------------------------------------------------%

builtin_fst(!Stack) :-
    pop("fst", V, !Stack),
    ( if V = consval(H, _) then
        push(H, !Stack)
    else
        throw(type_error("cons", V))
    ).

%-----------------------------------------------------------------------%
% snd: ( cons -- tail ) Get the second element (tail) of a cons cell
%-----------------------------------------------------------------------%

builtin_snd(!Stack) :-
    pop("snd", V, !Stack),
    ( if V = consval(_, T) then
        push(T, !Stack)
    else
        throw(type_error("cons", V))
    ).

%-----------------------------------------------------------------------%
% write: ( a -- ) Pop and print value in executable (round-trippable) form
%-----------------------------------------------------------------------%

builtin_write(IT, !Stack, !IO) :-
    pop("write", V, !Stack),
    io.write_string(value_to_write_string(IT, V), !IO),
    io.nl(!IO).

%-----------------------------------------------------------------------%
% value_to_write_string: convert a value to executable string form
%-----------------------------------------------------------------------%

:- func value_to_write_string(intern_table, value) = string.

value_to_write_string(_, intval(I)) = int_to_string(I).
value_to_write_string(IT, stringval(StrId)) =
    "\"" ++ escape_string(lookup_string(IT ^ it_strings, StrId)) ++ "\"".
value_to_write_string(IT, arrayval(A)) = "[ " ++ ArrayElems ++ "]" :-
    array.to_list(A, List),
    ElemStrings = list.map(
        (func(V) = value_to_write_string(IT, V) ++ " "),
        List),
    ArrayElems = string.append_list(ElemStrings).
value_to_write_string(IT, mapval(M)) = Result :-
    map.foldl(map_entry_to_string(IT), M, "$", Result).
value_to_write_string(IT, termval(T)) = "'" ++ term_to_write_string(IT, T).
value_to_write_string(_, nilval) = ".".
value_to_write_string(IT, consval(H, T)) =
    value_to_write_string(IT, T) ++ " " ++ value_to_write_string(IT, H) ++ " ,".

:- pred map_entry_to_string(intern_table::in, name_id::in, value::in,
    string::in, string::out) is det.

map_entry_to_string(IT, NameId, V, !Acc) :-
    !:Acc = !.Acc ++ " " ++ value_to_write_string(IT, V) ++ " '" ++
        lookup_name(IT ^ it_names, NameId) ++ " :".

:- func term_to_write_string(intern_table, term) = string.

term_to_write_string(IT, identifier(NameId)) = lookup_name(IT ^ it_names, NameId).
term_to_write_string(IT, binder(NameId)) = "/" ++ lookup_name(IT ^ it_names, NameId).
term_to_write_string(IT, function(Terms)) = "{ " ++ terms_to_write_string(IT, Terms) ++ "}".
term_to_write_string(IT, generator(Terms)) = "[ " ++ terms_to_write_string(IT, Terms) ++ "]".
term_to_write_string(IT, quoted(T)) = "'" ++ term_to_write_string(IT, T).
term_to_write_string(IT, value(V)) = value_to_write_string(IT, V).
term_to_write_string(_, apply_term) = "!".

:- func terms_to_write_string(intern_table, list(term)) = string.

terms_to_write_string(_, []) = "".
terms_to_write_string(IT, [T | Ts]) =
    term_to_write_string(IT, T) ++ " " ++ terms_to_write_string(IT, Ts).

%-----------------------------------------------------------------------%
% fwrite: ( value file -- ) Write value to file in executable form
%-----------------------------------------------------------------------%

builtin_fwrite(IT, !Stack, !IO) :-
    pop("fwrite", FileVal, !Stack),
    pop("fwrite", V, !Stack),
    Filename = value_to_string(IT, FileVal),
    Content = value_to_write_string(IT, V),
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

builtin_empty(!Stack) :-
    push(mapval(map.init), !Stack).

%-----------------------------------------------------------------------%
% keys: ( map -- array ) Get map keys as an array of quoted identifiers
%-----------------------------------------------------------------------%

builtin_keys(!Stack) :-
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

builtin_store(!Stack) :-
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
:- end_module builtins.
%-----------------------------------------------------------------------%
