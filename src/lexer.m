%-----------------------------------------------------------------------%
% lexer.m
% Lexical analyzer for the Froth programming language.
%-----------------------------------------------------------------------%

:- module lexer.
:- interface.

:- import_module char.
:- import_module list.
:- import_module types.

%-----------------------------------------------------------------------%

:- type token
    --->    name(string_id)
    ;       slash_name(string_id)   % The string_id excludes the leading /
    ;       number(int)
    ;       string(string_id)       % Interned string ID
    ;       quote                   % The ' character
    ;       bang                    % The ! character (apply)
    ;       open_curly
    ;       close_curly
    ;       open_square
    ;       close_square
    ;       junk(string).

:- type position
    --->    position(
                line    :: int,
                column  :: int
            ).

:- type located(T)
    --->    located(
                loc_position    :: position,
                loc_value       :: T
            ).

:- type lex_error
    --->    unterminated_string(position)
    ;       unterminated_block_comment(position)
    ;       invalid_escape_sequence(position, char).

:- type lex_result
    --->    ok(list(located(token)), intern_table)
    ;       error(lex_error).

    % tokenize(Input, InternTable, Result):
    % Tokenize the input string into a list of located tokens.
    % Takes initial intern table (use empty_intern_table for fresh start).
    %
:- pred tokenize(string::in, intern_table::in, lex_result::out) is det.

%-----------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module string.

%-----------------------------------------------------------------------%

:- type lex_state
    --->    lex_state(
                ls_input    :: string,
                ls_pos      :: int,         % Current position in string
                ls_line     :: int,
                ls_column   :: int,
                ls_intern   :: intern_table
            ).

    % Internal result type to avoid ambiguity with other ok/error types.
:- type maybe_result(T, E)
    --->    ok(T)
    ;       error(E).

%-----------------------------------------------------------------------%

tokenize(Input, InternTable, Result) :-
    State0 = lex_state(Input, 0, 1, 1, InternTable),
    tokenize_loop(State0, [], Result).

:- pred tokenize_loop(lex_state::in, list(located(token))::in,
    lex_result::out) is det.

tokenize_loop(State0, RevTokens, Result) :-
    skip_whitespace_and_comments(State0, SkipResult),
    (
        SkipResult = ok(State1),
        ( if at_end(State1) then
            Result = ok(list.reverse(RevTokens), State1 ^ ls_intern)
        else
            Pos = position(State1 ^ ls_line, State1 ^ ls_column),
            read_token(State1, Pos, TokenResult),
            (
                TokenResult = ok({Token, State2}),
                LocToken = located(Pos, Token),
                tokenize_loop(State2, [LocToken | RevTokens], Result)
            ;
                TokenResult = error(Error),
                Result = error(Error)
            )
        )
    ;
        SkipResult = error(Error),
        Result = error(Error)
    ).

%-----------------------------------------------------------------------%
% Whitespace and comment handling
%-----------------------------------------------------------------------%

:- pred skip_whitespace_and_comments(lex_state::in,
    maybe_result(lex_state, lex_error)::out) is det.

skip_whitespace_and_comments(State0, Result) :-
    ( if at_end(State0) then
        Result = ok(State0)
    else if peek_char(State0, Char) then
        ( if char.is_whitespace(Char) then
            advance(State0, State1),
            skip_whitespace_and_comments(State1, Result)
        else if Char = (';') then
            skip_line_comment(State0, State1),
            skip_whitespace_and_comments(State1, Result)
        else if Char = '(' then
            Pos = position(State0 ^ ls_line, State0 ^ ls_column),
            advance(State0, State1),
            skip_block_comment(State1, 1, Pos, CommentResult),
            (
                CommentResult = ok(State2),
                skip_whitespace_and_comments(State2, Result)
            ;
                CommentResult = error(Error),
                Result = error(Error)
            )
        else
            Result = ok(State0)
        )
    else
        Result = ok(State0)
    ).

:- pred skip_line_comment(lex_state::in, lex_state::out) is det.

skip_line_comment(State0, State) :-
    ( if at_end(State0) then
        State = State0
    else if peek_char(State0, Char) then
        ( if Char = '\n' then
            advance(State0, State)
        else
            advance(State0, State1),
            skip_line_comment(State1, State)
        )
    else
        State = State0
    ).

:- pred skip_block_comment(lex_state::in, int::in, position::in,
    maybe_result(lex_state, lex_error)::out) is det.

skip_block_comment(State0, Depth, StartPos, Result) :-
    ( if at_end(State0) then
        Result = error(unterminated_block_comment(StartPos))
    else if peek_char(State0, Char) then
        ( if Char = ')' then
            advance(State0, State1),
            ( if Depth = 1 then
                Result = ok(State1)
            else
                skip_block_comment(State1, Depth - 1, StartPos, Result)
            )
        else if Char = '(' then
            advance(State0, State1),
            skip_block_comment(State1, Depth + 1, StartPos, Result)
        else
            advance(State0, State1),
            skip_block_comment(State1, Depth, StartPos, Result)
        )
    else
        Result = error(unterminated_block_comment(StartPos))
    ).

%-----------------------------------------------------------------------%
% Token reading
%-----------------------------------------------------------------------%

:- pred read_token(lex_state::in, position::in,
    maybe_result({token, lex_state}, lex_error)::out) is det.

read_token(State0, Pos, Result) :-
    ( if peek_char(State0, Char) then
        ( if Char = '{' then
            advance(State0, State1),
            Result = ok({open_curly, State1})
        else if Char = '}' then
            advance(State0, State1),
            Result = ok({close_curly, State1})
        else if Char = '[' then
            advance(State0, State1),
            Result = ok({open_square, State1})
        else if Char = ']' then
            advance(State0, State1),
            Result = ok({close_square, State1})
        else if Char = '"' then
            advance(State0, State1),
            read_string(State1, Pos, Result)
        else if Char = '''' then
            advance(State0, State1),
            Result = ok({quote, State1})
        else if Char = ('!') then
            advance(State0, State1),
            Result = ok({bang, State1})
        else if Char = ('/') then
            advance(State0, State1),
            read_slash_name(State1, Result)
        else if is_number_start(State0) then
            read_number(State0, Result)
        else if is_text_name_start(Char) then
            read_text_name(State0, Result)
        else if is_graphical_char(Char) then
            read_graphical_name(State0, Result)
        else
            read_junk(State0, Result)
        )
    else
        % Should not happen if at_end was checked
        Result = ok({junk(""), State0})
    ).

%-----------------------------------------------------------------------%
% Name tokens
%
% Names are either text or graphical:
%   - Text names: [a-zA-Z][a-zA-Z0-9-]*
%   - Graphical names: [+=.,<>?@#:$-]+
%-----------------------------------------------------------------------%

:- pred is_text_name_start(char::in) is semidet.

is_text_name_start(Char) :-
    char.is_alpha(Char).

:- pred is_text_name_char(char::in) is semidet.

is_text_name_char(Char) :-
    ( char.is_alnum(Char)
    ; Char = ('-')
    ).

:- pred is_graphical_char(char::in) is semidet.

is_graphical_char(Char) :-
    ( Char = ('+')
    ; Char = ('-')
    ; Char = ('*')
    ; Char = ('=')
    ; Char = (',')
    ; Char = ('.')
    ; Char = ('<')
    ; Char = ('>')
    ; Char = ('?')
    ; Char = ('@')
    ; Char = ('#')
    ; Char = (':')
    ; Char = ('$')
    ).

:- pred read_text_name(lex_state::in,
    maybe_result({token, lex_state}, lex_error)::out) is det.

read_text_name(State0, Result) :-
    read_text_name_chars(State0, Chars, State1),
    NameStr = string.from_char_list(Chars),
    IT0 = State1 ^ ls_intern,
    intern_string(NameStr, NameId, IT0 ^ it_strings, NewStrings),
    State2 = State1 ^ ls_intern := (IT0 ^ it_strings := NewStrings),
    Result = ok({name(NameId), State2}).

:- pred read_text_name_chars(lex_state::in, list(char)::out,
    lex_state::out) is det.

read_text_name_chars(State0, Chars, State) :-
    ( if
        not at_end(State0),
        peek_char(State0, Char),
        is_text_name_char(Char)
    then
        advance(State0, State1),
        read_text_name_chars(State1, RestChars, State),
        Chars = [Char | RestChars]
    else
        Chars = [],
        State = State0
    ).

:- pred read_graphical_name(lex_state::in,
    maybe_result({token, lex_state}, lex_error)::out) is det.

read_graphical_name(State0, Result) :-
    read_graphical_chars(State0, Chars, State1),
    NameStr = string.from_char_list(Chars),
    IT0 = State1 ^ ls_intern,
    intern_string(NameStr, NameId, IT0 ^ it_strings, NewStrings),
    State2 = State1 ^ ls_intern := (IT0 ^ it_strings := NewStrings),
    Result = ok({name(NameId), State2}).

:- pred read_graphical_chars(lex_state::in, list(char)::out,
    lex_state::out) is det.

read_graphical_chars(State0, Chars, State) :-
    ( if
        not at_end(State0),
        peek_char(State0, Char),
        is_graphical_char(Char),
        % Stop if this would start a number: +/- followed by digit
        not (
            ( Char = ('+') ; Char = ('-') ),
            peek_char_at(State0, 1, NextChar),
            char.is_digit(NextChar)
        )
    then
        advance(State0, State1),
        read_graphical_chars(State1, RestChars, State),
        Chars = [Char | RestChars]
    else
        Chars = [],
        State = State0
    ).

%-----------------------------------------------------------------------%
% Slash name tokens
%-----------------------------------------------------------------------%

:- pred read_slash_name(lex_state::in,
    maybe_result({token, lex_state}, lex_error)::out) is det.

read_slash_name(State0, Result) :-
    ( if peek_char(State0, Char), is_text_name_start(Char) then
        read_text_name_chars(State0, Chars, State1),
        NameStr = string.from_char_list(Chars),
        IT0 = State1 ^ ls_intern,
        intern_string(NameStr, NameId, IT0 ^ it_strings, NewStrings),
        State2 = State1 ^ ls_intern := (IT0 ^ it_strings := NewStrings),
        Result = ok({slash_name(NameId), State2})
    else if peek_char(State0, Char), is_graphical_char(Char) then
        read_graphical_chars(State0, Chars, State1),
        NameStr = string.from_char_list(Chars),
        IT0 = State1 ^ ls_intern,
        intern_string(NameStr, NameId, IT0 ^ it_strings, NewStrings),
        State2 = State1 ^ ls_intern := (IT0 ^ it_strings := NewStrings),
        Result = ok({slash_name(NameId), State2})
    else
        % Just a slash followed by non-name, treat as junk
        % The slash was already consumed, so we return junk("/")
        Result = ok({junk("/"), State0})
    ).

%-----------------------------------------------------------------------%
% Number tokens
%-----------------------------------------------------------------------%

:- pred is_number_start(lex_state::in) is semidet.

is_number_start(State) :-
    peek_char(State, Char),
    ( char.is_digit(Char)
    ; ( Char = ('-') ; Char = ('+') ),
      peek_char_at(State, 1, NextChar),
      char.is_digit(NextChar)
    ).

:- pred read_number(lex_state::in,
    maybe_result({token, lex_state}, lex_error)::out) is det.

read_number(State0, Result) :-
    read_number_chars(State0, Chars, State1),
    String = string.from_char_list(Chars),
    ( if string.to_int(String, Int) then
        Result = ok({number(Int), State1})
    else
        % Should not happen given our checks, but handle gracefully
        Result = ok({junk(String), State1})
    ).

:- pred read_number_chars(lex_state::in, list(char)::out, lex_state::out) is det.

read_number_chars(State0, Chars, State) :-
    % This is only called after is_number_start validated the input,
    % so the first char is either a digit or +/- followed by digit.
    ( if
        not at_end(State0),
        peek_char(State0, Char),
        ( char.is_digit(Char)
        ; Char = ('-')
        ; Char = ('+')
        )
    then
        advance(State0, State1),
        read_number_chars_rest(State1, RestChars, State),
        Chars = [Char | RestChars]
    else
        Chars = [],
        State = State0
    ).

:- pred read_number_chars_rest(lex_state::in, list(char)::out,
    lex_state::out) is det.

read_number_chars_rest(State0, Chars, State) :-
    ( if
        not at_end(State0),
        peek_char(State0, Char),
        char.is_digit(Char)
    then
        advance(State0, State1),
        read_number_chars_rest(State1, RestChars, State),
        Chars = [Char | RestChars]
    else
        Chars = [],
        State = State0
    ).

%-----------------------------------------------------------------------%
% String tokens
%-----------------------------------------------------------------------%

:- pred read_string(lex_state::in, position::in,
    maybe_result({token, lex_state}, lex_error)::out) is det.

read_string(State0, StartPos, Result) :-
    read_string_chars(State0, StartPos, Chars, StringResult),
    (
        StringResult = ok(State1),
        String = string.from_char_list(Chars),
        IT0 = State1 ^ ls_intern,
        intern_string(String, Id, IT0 ^ it_strings, NewStrings),
        State2 = State1 ^ ls_intern := (IT0 ^ it_strings := NewStrings),
        Result = ok({string(Id), State2})
    ;
        StringResult = error(Error),
        Result = error(Error)
    ).

:- pred read_string_chars(lex_state::in, position::in,
    list(char)::out, maybe_result(lex_state, lex_error)::out) is det.

read_string_chars(State0, StartPos, Chars, Result) :-
    ( if at_end(State0) then
        Chars = [],
        Result = error(unterminated_string(StartPos))
    else if peek_char(State0, Char) then
        ( if Char = '"' then
            advance(State0, State1),
            Chars = [],
            Result = ok(State1)
        else if Char = '\n' then
            Chars = [],
            Result = error(unterminated_string(StartPos))
        else if Char = ('\\') then
            advance(State0, State1),
            read_escape_sequence(State1, StartPos, EscapeResult),
            (
                EscapeResult = ok({EscapedChar, State2}),
                read_string_chars(State2, StartPos, RestChars, Result),
                Chars = [EscapedChar | RestChars]
            ;
                EscapeResult = error(Error),
                Chars = [],
                Result = error(Error)
            )
        else
            advance(State0, State1),
            read_string_chars(State1, StartPos, RestChars, Result),
            Chars = [Char | RestChars]
        )
    else
        Chars = [],
        Result = error(unterminated_string(StartPos))
    ).

:- pred read_escape_sequence(lex_state::in, position::in,
    maybe_result({char, lex_state}, lex_error)::out) is det.

read_escape_sequence(State0, StartPos, Result) :-
    ( if at_end(State0) then
        Result = error(unterminated_string(StartPos))
    else if peek_char(State0, Char) then
        ( if Char = ('\\') then
            advance(State0, State1),
            Result = ok({('\\'), State1})
        else if Char = ('"') then
            advance(State0, State1),
            Result = ok({('"'), State1})
        else if Char = ('n') then
            advance(State0, State1),
            Result = ok({('\n'), State1})
        else if Char = ('t') then
            advance(State0, State1),
            Result = ok({('\t'), State1})
        else if Char = ('r') then
            advance(State0, State1),
            Result = ok({('\r'), State1})
        else
            EscapePos = position(State0 ^ ls_line, State0 ^ ls_column),
            Result = error(invalid_escape_sequence(EscapePos, Char))
        )
    else
        Result = error(unterminated_string(StartPos))
    ).

%-----------------------------------------------------------------------%
% Junk tokens
%-----------------------------------------------------------------------%

:- pred read_junk(lex_state::in,
    maybe_result({token, lex_state}, lex_error)::out) is det.

read_junk(State0, Result) :-
    read_junk_chars(State0, Chars, State1),
    Junk = string.from_char_list(Chars),
    Result = ok({junk(Junk), State1}).

:- pred read_junk_chars(lex_state::in, list(char)::out, lex_state::out) is det.

read_junk_chars(State0, Chars, State) :-
    ( if
        not at_end(State0),
        peek_char(State0, Char),
        not char.is_whitespace(Char),
        not is_token_starter(Char)
    then
        advance(State0, State1),
        read_junk_chars(State1, RestChars, State),
        Chars = [Char | RestChars]
    else
        Chars = [],
        State = State0
    ).

:- pred is_token_starter(char::in) is semidet.

is_token_starter(Char) :-
    ( Char = '{'
    ; Char = '}'
    ; Char = '['
    ; Char = ']'
    ; Char = '"'
    ; Char = ''''
    ; Char = ('!')
    ; Char = (';')
    ; Char = ('(')
    ; Char = ('/')
    ; char.is_digit(Char)
    ; is_text_name_start(Char)
    ; is_graphical_char(Char)
    ).

%-----------------------------------------------------------------------%
% Low-level state operations
%-----------------------------------------------------------------------%

:- pred at_end(lex_state::in) is semidet.

at_end(State) :-
    State ^ ls_pos >= string.length(State ^ ls_input).

:- pred peek_char(lex_state::in, char::out) is semidet.

peek_char(State, Char) :-
    string.index(State ^ ls_input, State ^ ls_pos, Char).

:- pred peek_char_at(lex_state::in, int::in, char::out) is semidet.

peek_char_at(State, Offset, Char) :-
    string.index(State ^ ls_input, State ^ ls_pos + Offset, Char).

:- pred advance(lex_state::in, lex_state::out) is det.

advance(State0, State) :-
    ( if peek_char(State0, Char) then
        ( if Char = '\n' then
            State = (((State0
                ^ ls_pos := State0 ^ ls_pos + 1)
                ^ ls_line := State0 ^ ls_line + 1)
                ^ ls_column := 1)
        else
            State = ((State0
                ^ ls_pos := State0 ^ ls_pos + 1)
                ^ ls_column := State0 ^ ls_column + 1)
        )
    else
        State = State0
    ).

%-----------------------------------------------------------------------%
:- end_module lexer.
%-----------------------------------------------------------------------%
