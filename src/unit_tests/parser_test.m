%-----------------------------------------------------------------------%
% parser_test.m
% Test driver for the Froth! parser.
%-----------------------------------------------------------------------%

:- module parser_test.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------%

:- implementation.

:- import_module lexer.
:- import_module list.
:- import_module parser.
:- import_module string.
:- import_module types.

%-----------------------------------------------------------------------%

main(!IO) :-
    io.write_string("=== Froth! Parser Tests ===\n\n", !IO),
    run_tests(!IO).

:- pred run_tests(io::di, io::uo) is det.

run_tests(!IO) :-
    % Basic terms
    test("Single name", "foo", !IO),
    test("Multiple names", "foo bar baz", !IO),
    test("Binder", "/x", !IO),
    test("Number", "42", !IO),
    test("Negative number", "-17", !IO),
    test("String", "\"hello\"", !IO),

    % Functions
    test("Empty function", "{ }", !IO),
    test("Simple function", "{ 1 2 + }", !IO),
    test("Nested function", "{ { 1 } }", !IO),

    % Generators
    test("Empty generator", "[ ]", !IO),
    test("Simple generator", "[ 1 2 3 ]", !IO),
    test("Nested generator", "[ [ 1 ] ]", !IO),

    % Mixed brackets
    test("Mixed brackets", "{ [ 1 2 ] }", !IO),
    test("Function in generator", "[ { + } ]", !IO),

    % Quoting
    test("Quoted name", "'foo", !IO),
    test("Quoted function", "'{ 1 2 + }", !IO),
    test("Double quote", "''foo", !IO),
    test("Quoted in function", "{ 'x }", !IO),

    % Apply
    test("Apply", "foo !", !IO),
    test("Quoted apply", "'!", !IO),

    % Operators
    test("Graphical operators", "+ - = . , ? @ # : $", !IO),

    % Complete program
    test("Complete program", "1 /x { x x + } /double x double !", !IO),

    % Error cases
    test("Unclosed curly", "{ foo", !IO),
    test("Unclosed square", "[ foo", !IO),
    test("Unexpected close curly", "foo }", !IO),
    test("Unexpected close square", "foo ]", !IO),
    test("Mismatched brackets", "{ foo ]", !IO),
    test("Quote at end", "foo '", !IO),
    test("Junk token", "foo * bar", !IO),

    io.write_string("\n=== Tests Complete ===\n", !IO).

:- pred test(string::in, string::in, io::di, io::uo) is det.

test(Name, Input, !IO) :-
    io.format("Test: %s\n", [s(Name)], !IO),
    io.format("  Input: %s\n", [s(Input)], !IO),
    lexer.tokenize(Input, types.empty_string_table, LexResult),
    (
        LexResult = lexer.ok(Tokens, ST),
        parser.parse(Tokens, ParseResult),
        (
            ParseResult = parser.ok(Terms),
            io.write_string("  Terms:\n", !IO),
            list.foldl(print_term(ST), Terms, !IO)
        ;
            ParseResult = parser.error(Error),
            io.format("  Parse Error: %s\n", [s(parse_error_to_string(Error))], !IO)
        )
    ;
        LexResult = lexer.error(Error),
        io.format("  Lex Error: %s\n", [s(lex_error_to_string(Error))], !IO)
    ),
    io.write_string("\n", !IO).

:- pred print_term(string_table::in, term::in, io::di, io::uo) is det.

print_term(ST, Term, !IO) :-
    io.format("    %s\n", [s(term_to_string(ST, Term))], !IO).

:- func term_to_string(string_table, term) = string.

term_to_string(ST, identifier(NameId)) =
    string.format("identifier(%s)", [s(lookup_string(ST, NameId))]).
term_to_string(ST, binder(NameId)) =
    string.format("binder(%s)", [s(lookup_string(ST, NameId))]).
term_to_string(ST, function(Terms)) =
    "function(" ++ terms_to_string(ST, Terms) ++ ")".
term_to_string(ST, generator(Terms)) =
    "generator(" ++ terms_to_string(ST, Terms) ++ ")".
term_to_string(ST, quoted(T)) =
    "quoted(" ++ term_to_string(ST, T) ++ ")".
term_to_string(ST, value(V)) =
    "value(" ++ value_to_string(ST, V) ++ ")".
term_to_string(_, apply_term) = "apply".

:- func terms_to_string(string_table, list(term)) = string.

terms_to_string(_, []) = "".
terms_to_string(ST, [T]) = term_to_string(ST, T).
terms_to_string(ST, [T1, T2 | Ts]) =
    term_to_string(ST, T1) ++ ", " ++ terms_to_string(ST, [T2 | Ts]).

:- func value_to_string(string_table, value) = string.

value_to_string(_, intval(I)) = string.format("int(%d)", [i(I)]).
value_to_string(ST, stringval(StrId)) =
    string.format("string(\"%s\")", [s(lookup_string(ST, StrId))]).
value_to_string(_, arrayval(_)) = "array(...)".
value_to_string(_, mapval(_)) = "map(...)".
value_to_string(ST, termval(T)) = "term(" ++ term_to_string(ST, T) ++ ")".
value_to_string(_, nilval) = "nil".
value_to_string(_, consval(_, _)) = "cons(...)".
value_to_string(_, closureval(_, _)) = "closure(...)".
value_to_string(_, bytecodeval(_, _)) = "bytecode(...)".

:- func parse_error_to_string(parse_error) = string.

parse_error_to_string(unexpected_token(position(L, C), _)) =
    string.format("unexpected token at %d:%d", [i(L), i(C)]).
parse_error_to_string(unexpected_close_curly(position(L, C))) =
    string.format("unexpected '}' at %d:%d", [i(L), i(C)]).
parse_error_to_string(unexpected_close_square(position(L, C))) =
    string.format("unexpected ']' at %d:%d", [i(L), i(C)]).
parse_error_to_string(unclosed_curly(position(L, C))) =
    string.format("unclosed '{' at %d:%d", [i(L), i(C)]).
parse_error_to_string(unclosed_square(position(L, C))) =
    string.format("unclosed '[' at %d:%d", [i(L), i(C)]).
parse_error_to_string(quote_at_end(position(L, C))) =
    string.format("quote at end of input at %d:%d", [i(L), i(C)]).
parse_error_to_string(junk_token(position(L, C), S)) =
    string.format("junk token '%s' at %d:%d", [s(S), i(L), i(C)]).

:- func lex_error_to_string(lex_error) = string.

lex_error_to_string(unterminated_string(position(L, C))) =
    string.format("unterminated string at %d:%d", [i(L), i(C)]).
lex_error_to_string(unterminated_block_comment(position(L, C))) =
    string.format("unterminated block comment at %d:%d", [i(L), i(C)]).
lex_error_to_string(invalid_escape_sequence(position(L, C), Char)) =
    string.format("invalid escape sequence '\\%c' at %d:%d", [c(Char), i(L), i(C)]).

%-----------------------------------------------------------------------%
:- end_module parser_test.
%-----------------------------------------------------------------------%
