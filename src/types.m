%-----------------------------------------------------------------------%
% types.m
% Core type definitions for the Froth programming language.
%-----------------------------------------------------------------------%

:- module types.
:- interface.

:- import_module array.
:- import_module list.
:- import_module map.

%-----------------------------------------------------------------------%

:- type string_id == int.

    % Intern table: bidirectional mapping between strings and IDs.
    % All strings (including names) are interned during lexing.
    %
:- type string_table.
:- type intern_table
    --->    intern_table(
                it_strings  :: string_table
            ).

:- func empty_string_table = string_table.
:- pred intern_string(string::in, string_id::out,
    string_table::in, string_table::out) is det.
:- func lookup_string(string_table, string_id) = string.

:- func empty_intern_table = intern_table.

    % escape_string(String) = EscapedString:
    % Escape special characters for output as a quoted string literal.
    % Escapes: \ -> \\, " -> \", newline -> \n, tab -> \t, cr -> \r
    %
:- func escape_string(string) = string.

    % Values in the Froth language.
    %
    % Note: Closures are represented as cons pairs:
    %   consval(mapval(Env), termval(function(Terms)))
    % This allows closures to be inspected using fst/snd and compared with =.
    % Evaluating { terms } creates this cons pair with the current environment.
    % The ! operator matches this pattern and evaluates the function body.
    %
:- type value
    --->    intval(int)
    ;       stringval(string_id)
    ;       arrayval(array(value))
    ;       mapval(map(string_id, value))
    ;       termval(term)
    ;       nilval
    ;       consval(value, value).  % cons(head, tail)

:- type term
    --->    identifier(string_id)
    ;       binder(string_id)
    ;       function(list(term))
    ;       generator(list(term))
    ;       quoted(term)
    ;       value(value)
    ;       apply_term.

:- type env == map(string_id, value).

:- type stack == list(value).

%-----------------------------------------------------------------------%
% Runtime errors
%-----------------------------------------------------------------------%

:- type eval_error
    --->    stack_underflow(string)         % Operation that caused it
    ;       type_error(string, value)       % Expected type, actual value
    ;       undefined_name(string_id)
    ;       index_out_of_bounds(int, int)   % Index, array size
    ;       io_error(string, string, string).  % Operation, filename, message

    % format_error(InternTable, Error) = Message:
    % Convert an eval_error to a human-readable message.
    %
:- func format_error(intern_table, eval_error) = string.

%-----------------------------------------------------------------------%
% Stack operations
%-----------------------------------------------------------------------%

:- pred push(value::in, stack::in, stack::out) is det.
:- pred pop(string::in, value::out, stack::in, stack::out) is det.

%-----------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module exception.
:- import_module int.
:- import_module string.

%-----------------------------------------------------------------------%
% String table implementation
%-----------------------------------------------------------------------%

:- type string_table
    --->    string_table(
                st_next_id      :: int,
                st_to_id        :: map(string, string_id),
                st_to_string    :: map(string_id, string)
            ).

empty_string_table = string_table(0, map.init, map.init).

intern_string(String, Id, !Table) :-
    ToId0 = !.Table ^ st_to_id,
    ( if map.search(ToId0, String, ExistingId) then
        Id = ExistingId
    else
        Id = !.Table ^ st_next_id,
        map.det_insert(String, Id, ToId0, ToId),
        map.det_insert(Id, String, !.Table ^ st_to_string, ToStr),
        !:Table = string_table(Id + 1, ToId, ToStr)
    ).

lookup_string(Table, Id) = String :-
    map.lookup(Table ^ st_to_string, Id, String).

empty_intern_table = intern_table(empty_string_table).

%-----------------------------------------------------------------------%
% String escaping
%-----------------------------------------------------------------------%

escape_string(String) = EscapedString :-
    string.to_char_list(String, Chars),
    list.map(escape_char, Chars, EscapedChars),
    list.condense(EscapedChars, FlatChars),
    string.from_char_list(FlatChars, EscapedString).

:- pred escape_char(char::in, list(char)::out) is det.

escape_char(Char, Escaped) :-
    ( if Char = ('\\') then
        Escaped = ['\\', '\\']
    else if Char = ('"') then
        Escaped = ['\\', '"']
    else if Char = ('\n') then
        Escaped = ['\\', 'n']
    else if Char = ('\t') then
        Escaped = ['\\', 't']
    else if Char = ('\r') then
        Escaped = ['\\', 'r']
    else
        Escaped = [Char]
    ).

%-----------------------------------------------------------------------%

format_error(_, stack_underflow(Op)) =
    string.format("stack underflow in '%s'", [s(Op)]).
format_error(_, type_error(Expected, Actual)) =
    string.format("type error: expected %s, got %s",
        [s(Expected), s(value_type_name(Actual))]).
format_error(IT, undefined_name(NameId)) =
    string.format("undefined name: %s", [s(lookup_string(IT ^ it_strings, NameId))]).
format_error(_, index_out_of_bounds(Index, Size)) =
    string.format("index out of bounds: %d (array size: %d)", [i(Index), i(Size)]).
format_error(_, io_error(Op, Filename, Msg)) =
    string.format("%s: %s: %s", [s(Op), s(Filename), s(Msg)]).

:- func value_type_name(value) = string.

value_type_name(intval(_)) = "int".
value_type_name(stringval(_)) = "string".
value_type_name(arrayval(_)) = "array".
value_type_name(mapval(_)) = "map".
value_type_name(termval(_)) = "term".
value_type_name(nilval) = "nil".
value_type_name(consval(_, _)) = "cons".

%-----------------------------------------------------------------------%

push(V, Stack, [V | Stack]).

pop(Op, V, Stack0, Stack) :-
    (
        Stack0 = [V | Stack]
    ;
        Stack0 = [],
        throw(stack_underflow(Op))
    ).

%-----------------------------------------------------------------------%
:- end_module types.
%-----------------------------------------------------------------------%
