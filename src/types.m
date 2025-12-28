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

    % Operators.
    % Each constructor corresponds to a language operator.
    %
:- type operator
    --->    op_print        % print  ( a -- )
    ;       op_env          % env    ( -- map )
    ;       op_add          % +      ( a b -- a+b )
    ;       op_sub          % -      ( a b -- a-b )
    ;       op_mul          % *      ( a b -- a*b )
    ;       op_gt           % >      ( a b -- int )
    ;       op_lt           % <      ( a b -- int )
    ;       op_gte          % >=     ( a b -- int )
    ;       op_lte          % <=     ( a b -- int )
    ;       op_get          % @      ( container key -- val )
    ;       op_length       % #      ( container -- int )
    ;       op_eq           % =      ( a b -- int )
    ;       op_ite          % ?      ( cond then else -- result )
    ;       op_nil          % .      ( -- nil )
    ;       op_cons         % ,      ( tail head -- cons )
    ;       op_fst          % fst    ( cons -- head )
    ;       op_snd          % snd    ( cons -- tail )
    ;       op_write        % write  ( a -- )
    ;       op_fwrite       % fwrite ( value file -- )
    ;       op_empty        % $      ( -- map )
    ;       op_keys         % keys   ( map -- array )
    ;       op_store        % :      ( map val 'key -- map )
    ;       op_in           % in     ( map 'key -- int )
    ;       op_delete       % delete ( map 'key -- map )
    ;       op_is_int       % isInt    ( a -- int )
    ;       op_is_string    % isString ( a -- int )
    ;       op_is_array     % isArray  ( a -- int )
    ;       op_is_map       % isMap    ( a -- int )
    ;       op_is_nil       % isNil    ( a -- int )
    ;       op_is_cons      % isCons   ( a -- int )
    ;       op_is_ident     % isIdent  ( a -- int )
    ;       op_is_binder    % isBinder ( a -- int )
    ;       op_is_func      % isFunc   ( a -- int )
    ;       op_is_gen       % isGen    ( a -- int )
    ;       op_is_quote     % isQuote  ( a -- int )
    ;       op_is_apply     % isApply  ( a -- int )
    ;       op_is_value     % isValue  ( a -- int )
    ;       op_unwrap       % unwrap   ( 'value -- value )
    ;       op_intern       % intern   ( string|'ident|'binder -- int )
    ;       op_id_to_string % idToString ( int -- string )
    ;       op_id_to_ident  % idToIdent  ( int -- 'ident )
    ;       op_id_to_binder % idToBinder ( int -- 'binder )
    ;       op_is_operator  % isOperator ( 'ident -- int )
    ;       op_arity        % arity      ( 'ident -- int )
    ;       op_stack        % stack      ( ... -- array )
    ;       op_import       % import     ( filename -- )
    ;       op_time         % time       ( -- int )
    ;       op_restore      % restore    ( map -- )
    ;       op_close        % close       ( env body -- closure )
    ;       op_closure_env  % closureEnv  ( closure -- env )
    ;       op_closure_body % closureBody ( closure -- body )
    ;       op_is_closure   % isClosure   ( a -- int )
    ;       op_emit.        % emit        ( int -- )

:- type operator_info
    --->    operator_info(
                oi_operator :: operator,
                oi_arity    :: int
            ).

:- type operator_table == map(string_id, operator_info).

    % Intern table: bidirectional mapping between strings and IDs,
    % plus operator lookup table.
    %
:- type string_table.
:- type intern_table
    --->    intern_table(
                it_strings   :: string_table,
                it_operators :: operator_table
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
    % Closures are represented as closureval(Env, Body) where Env is a map
    % and Body is a list of terms. Evaluating { terms } creates a closure
    % with the current environment. The ! operator evaluates the body
    % in the closure's environment.
    %
:- type value
    --->    intval(int)
    ;       stringval(string_id)
    ;       arrayval(array(value))
    ;       mapval(map(string_id, value))
    ;       termval(term)
    ;       nilval
    ;       consval(value, value)   % cons(head, tail)
    ;       closureval(env, list(term))   % closure(env, body)
    ;       bytecodeval(array(value), int).  % bytecode closure(context, code_addr)

:- type term
    --->    identifier(string_id)
    ;       binder(string_id)
    ;       function(list(term))
    ;       generator(list(term))
    ;       quoted(term)
    ;       value(value)
    ;       apply_term.

:- type env == map(string_id, value).

%-----------------------------------------------------------------------%
% Runtime errors
%-----------------------------------------------------------------------%

:- type eval_error
    --->    stack_underflow(string)         % Operation that caused it
    ;       type_error(string, value)       % Expected type, actual value
    ;       undefined_name(string_id)
    ;       index_out_of_bounds(int, int)   % Index, array size
    ;       io_error(string, string, string).  % Operation, filename, message

    % format_error(StringTable, Error) = Message:
    % Convert an eval_error to a human-readable message.
    %
:- func format_error(string_table, eval_error) = string.

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

empty_intern_table = intern_table(empty_string_table, map.init).

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
format_error(ST, undefined_name(NameId)) =
    string.format("undefined name: %s", [s(lookup_string(ST, NameId))]).
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
value_type_name(closureval(_, _)) = "closure".
value_type_name(bytecodeval(_, _)) = "bytecode".

%-----------------------------------------------------------------------%
:- end_module types.
%-----------------------------------------------------------------------%
