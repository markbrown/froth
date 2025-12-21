%-----------------------------------------------------------------------%
% lexer_test.m
% Test driver for the Froth lexer.
%-----------------------------------------------------------------------%

:- module lexer_test.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------%

:- implementation.

:- import_module lexer.
:- import_module list.
:- import_module string.
:- import_module types.

%-----------------------------------------------------------------------%

main(!IO) :-
    io.write_string("=== Froth Lexer Tests ===\n\n", !IO),
    run_tests(!IO).

:- pred run_tests(io::di, io::uo) is det.

run_tests(!IO) :-
    % Text names: [a-zA-Z][a-zA-Z0-9-]*
    test("Simple text name", "foo", !IO),
    test("Text name with hyphen", "my-func", !IO),
    test("Text name with digits", "var1", !IO),
    test("Multiple text names", "foo bar baz", !IO),

    % Graphical names: [+=.,<>?@#:$-]+
    test("Single graphical", "+", !IO),
    test("Multi-char graphical", "->", !IO),
    test("Adjacent graphicals", "++ --", !IO),
    test("All graphical chars", "+-*=.,<>?@#:$", !IO),

    % Text and graphical separation
    test("Text then graphical", "foo+bar", !IO),
    test("Graphical then text", "+foo", !IO),

    % Numbers
    test("Positive number", "42", !IO),
    test("Negative number", "-17", !IO),
    test("Number with plus", "+5", !IO),
    test("Adjacent numbers", "1+2", !IO),
    test("Graphical before number", "--3", !IO),

    % Slash names (text or graphical)
    test("Slash text name", "/define", !IO),
    test("Slash graphical name", "/+", !IO),
    test("Slash multi-graphical", "/->", !IO),
    test("Multiple slash names", "/foo /bar", !IO),
    test("Slash alone (junk)", "/ foo", !IO),

    % Quoted strings
    test("Double quoted", "\"hello world\"", !IO),
    test("Empty string", "\"\"", !IO),

    % Quote and bang
    test("Quote token", "'foo", !IO),
    test("Bang token", "foo !", !IO),

    % Brackets
    test("Curly braces", "{ foo }", !IO),
    test("Square brackets", "[ 1 2 3 ]", !IO),
    test("Nested brackets", "{ [ foo ] }", !IO),

    % Comments
    test("Line comment", "foo ; this is a comment\nbar", !IO),
    test("Block comment", "foo ( comment ) bar", !IO),
    test("Nested block comment", "foo ( outer ( inner ) outer ) bar", !IO),

    % Complex examples
    test("Stack addition", "1 2 +", !IO),
    test("Function definition", "{ 2 + } /add-two", !IO),

    % Escape sequences
    test("String with escapes", "\"hello\\nworld\"", !IO),
    test("String with quotes", "\"say \\\"hi\\\"\"", !IO),

    % Error cases
    test("Unterminated string", "\"hello", !IO),
    test("Unterminated block comment", "foo ( comment", !IO),
    test("Invalid escape sequence", "\"hello\\x\"", !IO),

    io.write_string("\n=== Tests Complete ===\n", !IO).

:- pred test(string::in, string::in, io::di, io::uo) is det.

test(Name, Input, !IO) :-
    io.format("Test: %s\n", [s(Name)], !IO),
    io.format("  Input: %s\n", [s(escape_for_display(Input))], !IO),
    lexer.tokenize(Input, types.empty_intern_table, Result),
    (
        Result = ok(Tokens, IT),
        io.write_string("  Tokens:\n", !IO),
        list.foldl(print_token(IT), Tokens, !IO)
    ;
        Result = error(Error),
        io.format("  Error: %s\n", [s(error_to_string(Error))], !IO)
    ),
    io.write_string("\n", !IO).

:- pred print_token(intern_table::in, located(token)::in, io::di, io::uo) is det.

print_token(IT, located(Pos, Token), !IO) :-
    Pos = position(Line, Col),
    io.format("    [%d:%d] %s\n",
        [i(Line), i(Col), s(token_to_string(IT, Token))], !IO).

:- func token_to_string(intern_table, token) = string.

token_to_string(IT, name(NameId)) =
    string.format("name(%s)", [s(lookup_name(IT ^ it_names, NameId))]).
token_to_string(IT, slash_name(NameId)) =
    string.format("slash_name(%s)", [s(lookup_name(IT ^ it_names, NameId))]).
token_to_string(_, number(N)) = string.format("number(%d)", [i(N)]).
token_to_string(IT, string(StrId)) =
    string.format("string(\"%s\")", [s(lookup_string(IT ^ it_strings, StrId))]).
token_to_string(_, quote) = "quote".
token_to_string(_, bang) = "bang".
token_to_string(_, open_curly) = "open_curly".
token_to_string(_, close_curly) = "close_curly".
token_to_string(_, open_square) = "open_square".
token_to_string(_, close_square) = "close_square".
token_to_string(_, junk(S)) = string.format("junk(%s)", [s(S)]).

:- func error_to_string(lex_error) = string.

error_to_string(unterminated_string(position(L, C))) =
    string.format("unterminated string at %d:%d", [i(L), i(C)]).
error_to_string(unterminated_block_comment(position(L, C))) =
    string.format("unterminated block comment at %d:%d", [i(L), i(C)]).
error_to_string(invalid_escape_sequence(position(L, C), Char)) =
    string.format("invalid escape sequence '\\%c' at %d:%d", [c(Char), i(L), i(C)]).

:- func escape_for_display(string) = string.

escape_for_display(S) =
    string.replace_all(string.replace_all(S, "\n", "\\n"), "\t", "\\t").

%-----------------------------------------------------------------------%
:- end_module lexer_test.
%-----------------------------------------------------------------------%
