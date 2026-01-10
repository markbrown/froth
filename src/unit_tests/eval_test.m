%-----------------------------------------------------------------------%
% eval_test.m
% Test driver for the Froth! evaluator.
%-----------------------------------------------------------------------%

:- module eval_test.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module eval.
:- import_module exception.
:- import_module lexer.
:- import_module list.
:- import_module map.
:- import_module parser.
:- import_module string.
:- import_module types.
:- import_module univ.

%-----------------------------------------------------------------------%

main(!IO) :-
    io.write_string("=== Froth! Evaluator Tests ===\n\n", !IO),
    run_tests(!IO).

:- pred run_tests(io::di, io::uo) is cc_multi.

run_tests(!IO) :-
    % Basic values
    test("Push integer", "42", !IO),
    test("Push negative", "-5", !IO),
    test("Push string", "\"hello\"", !IO),
    test("Push multiple", "1 2 3", !IO),

    % Arithmetic
    test("Addition", "1 2 +", !IO),
    test("Nested addition", "1 2 + 3 +", !IO),

    % Binders
    test("Simple binder", "42 /x x", !IO),
    test("Multiple binders", "1 /a 2 /b a b +", !IO),
    test("Rebind", "1 /x 2 /x x", !IO),

    % Functions and closures
    test("Simple function", "{ 1 }", !IO),
    test("Apply function", "{ 1 } !", !IO),
    test("Function with stack", "5 { 1 + } !", !IO),
    test("Closure capture", "1 /x { x } /f 2 /x f !", !IO),

    % Generators (arrays)
    test("Empty generator", "[ ]", !IO),
    test("Simple generator", "[ 1 2 3 ]", !IO),
    test("Generator with ops", "[ 1 2 + 3 4 + ]", !IO),

    % List operations
    test("Nil", ".", !IO),
    test("Cons", ". 1 ,", !IO),
    test("Multiple cons", ". 3 , 2 , 1 ,", !IO),
    test("Fst", ". 2 , 1 , fst", !IO),
    test("Snd", ". 2 , 1 , snd fst", !IO),

    % Map operations
    test("Empty map", "$", !IO),
    test("Store in map", "$ 42 'x :", !IO),
    test("Lookup in map", "$ 42 'x : 'x @", !IO),
    test("Map keys", "$ 1 'a : 2 'b : keys", !IO),
    test("Map length", "$ 1 'a : 2 'b : #", !IO),

    % Equality
    test("Equal ints", "1 1 =", !IO),
    test("Unequal ints", "1 2 =", !IO),
    test("Equal strings", "\"a\" \"a\" =", !IO),

    % Conditional
    test("If-then-else true", "0 'yes 'no ?", !IO),
    test("If-then-else false", "1 'yes 'no ?", !IO),

    % Quoting
    test("Quoted name", "'foo", !IO),
    test("Quoted function", "'{ 1 2 + }", !IO),

    % Environment
    test("Get env", "42 /x env", !IO),

    % Array operations
    test("Array length", "[ 1 2 3 ] #", !IO),
    test("Array index", "[ 10 20 30 ] 1 @", !IO),

    % Error cases
    test("Stack underflow", "+", !IO),
    test("Type error add", "1 \"x\" +", !IO),
    test("Undefined name", "undefined", !IO),
    test("Index out of bounds", "[ 1 2 ] 5 @", !IO),

    io.write_string("\n=== Tests Complete ===\n", !IO).

:- pred test(string::in, string::in, io::di, io::uo) is cc_multi.

test(Name, Input, !IO) :-
    io.format("Test: %s\n", [s(Name)], !IO),
    io.format("  Input: %s\n", [s(Input)], !IO),
    lexer.tokenize(Input, types.empty_intern_table, LexResult),
    (
        LexResult = lexer.ok(Tokens, IT),
        parser.parse(Tokens, ParseResult),
        (
            ParseResult = parser.ok(Terms),
            Env0 = map.init,
            Stack0 = [],
            try_io(eval_wrapper(IT, Terms, Env0, Stack0), Result, !IO),
            (
                Result = succeeded(FinalStack),
                io.write_string("  Stack: [", !IO),
                print_stack(IT, FinalStack, !IO),
                io.write_string("]\n", !IO)
            ;
                Result = exception(Exn),
                ( if univ_to_type(Exn, EvalError) then
                    io.format("  Error: %s\n",
                        [s(types.format_error(IT, EvalError))], !IO)
                else if univ_to_type(Exn, FrothError) then
                    io.format("  Error: %s\n",
                        [s(types.format_froth_error(IT, FrothError))], !IO)
                else
                    io.format("  Exception: %s\n",
                        [s(string.string(univ_value(Exn)))], !IO)
                )
            )
        ;
            ParseResult = parser.error(Error),
            io.format("  Parse Error: %s\n", [s(parse_error_to_string(Error))], !IO)
        )
    ;
        LexResult = lexer.error(Error),
        io.format("  Lex Error: %s\n", [s(lex_error_to_string(Error))], !IO)
    ),
    io.write_string("\n", !IO).

:- pred eval_wrapper(intern_table::in, list(term)::in, env::in, stack::in,
    stack::out, io::di, io::uo) is det.

eval_wrapper(IT, Terms, Env0, Stack0, Stack, !IO) :-
    eval.eval_terms(IT, Terms, Env0, _, Stack0, Stack, !IO).

:- pred print_stack(intern_table::in, stack::in, io::di, io::uo) is det.

print_stack(_, [], !IO).
print_stack(IT, [V], !IO) :-
    io.write_string(value_to_string(IT, V), !IO).
print_stack(IT, [V1, V2 | Vs], !IO) :-
    io.write_string(value_to_string(IT, V1), !IO),
    io.write_string(", ", !IO),
    print_stack(IT, [V2 | Vs], !IO).

:- func value_to_string(intern_table, value) = string.

value_to_string(_, intval(I)) = string.int_to_string(I).
value_to_string(IT, stringval(StrId)) =
    "\"" ++ lookup_string(IT ^ it_strings, StrId) ++ "\"".
value_to_string(_, arrayval(A)) =
    string.format("[array:%d]", [i(array.size(A))]).
value_to_string(_, mapval(M)) =
    string.format("[map:%d]", [i(map.count(M))]).
value_to_string(IT, termval(T)) = term_to_string(IT, T).
value_to_string(_, nilval) = ".".
value_to_string(IT, consval(H, T)) =
    "(" ++ value_to_string(IT, H) ++ "," ++ value_to_string(IT, T) ++ ")".

:- func term_to_string(intern_table, term) = string.

term_to_string(IT, identifier(NameId)) = lookup_string(IT ^ it_strings, NameId).
term_to_string(IT, binder(NameId)) = "/" ++ lookup_string(IT ^ it_strings, NameId).
term_to_string(IT, function(Terms)) = "{ " ++ terms_to_string(IT, Terms) ++ "}".
term_to_string(IT, generator(Terms)) = "[ " ++ terms_to_string(IT, Terms) ++ "]".
term_to_string(IT, quoted(T)) = "'" ++ term_to_string(IT, T).
term_to_string(IT, value(V)) = value_to_string(IT, V).
term_to_string(_, apply_term) = "!".

:- func terms_to_string(intern_table, list(term)) = string.

terms_to_string(_, []) = "".
terms_to_string(IT, [T | Ts]) =
    term_to_string(IT, T) ++ " " ++ terms_to_string(IT, Ts).

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
:- end_module eval_test.
%-----------------------------------------------------------------------%
