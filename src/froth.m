%-----------------------------------------------------------------------%
% froth.m
% Main module for the Froth programming language interpreter.
%-----------------------------------------------------------------------%

:- module froth.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------%

:- implementation.

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
    io.command_line_arguments(Args, !IO),
    (
        Args = [],
        % No arguments: start REPL
        repl(!IO)
    ;
        Args = [Arg],
        ( if ( Arg = "-h" ; Arg = "--help" ) then
            print_usage(!IO)
        else
            % Single argument: treat as filename
            run_file(Arg, !IO)
        )
    ;
        Args = [First, Second | Rest],
        ( if First = "-f", Rest = [] then
            run_file(Second, !IO)
        else if First = "-e", Rest = [] then
            run_file_then_repl(Second, !IO)
        else
            print_usage(!IO),
            io.set_exit_status(1, !IO)
        )
    ).

:- pred print_usage(io::di, io::uo) is det.

print_usage(!IO) :-
    io.write_string("Usage: froth [options] [filename]\n", !IO),
    io.write_string("\n", !IO),
    io.write_string("Options:\n", !IO),
    io.write_string("  -e FILE    Execute FILE then start REPL\n", !IO),
    io.write_string("  -f FILE    Run FILE\n", !IO),
    io.write_string("  -h, --help Show this help\n", !IO),
    io.write_string("\n", !IO),
    io.write_string("With no arguments, starts an interactive REPL.\n", !IO).

%-----------------------------------------------------------------------%
% REPL
%-----------------------------------------------------------------------%

:- pred repl(io::di, io::uo) is cc_multi.

repl(!IO) :-
    io.write_string("Froth REPL. Press Ctrl-D to exit.\n", !IO),
    Env0 = map.init,
    Stack0 = [],
    IT0 = empty_intern_table,
    repl_loop(IT0, Env0, Stack0, !IO).

:- pred repl_loop(intern_table::in, env::in, stack::in,
    io::di, io::uo) is cc_multi.

repl_loop(IT0, Env0, Stack0, !IO) :-
    io.write_string("> ", !IO),
    io.flush_output(!IO),
    io.read_line_as_string(ReadResult, !IO),
    (
        ReadResult = ok(Line),
        repl_eval(Line, IT0, IT, Env0, Env, Stack0, Stack, !IO),
        repl_loop(IT, Env, Stack, !IO)
    ;
        ReadResult = eof,
        io.nl(!IO)
    ;
        ReadResult = error(Error),
        io.format("Error reading input: %s\n",
            [s(io.error_message(Error))], !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred repl_eval(string::in, intern_table::in, intern_table::out,
    env::in, env::out, stack::in, stack::out, io::di, io::uo) is cc_multi.

repl_eval(Input, IT0, IT, Env0, Env, Stack0, Stack, !IO) :-
    lexer.tokenize(Input, IT0, LexResult),
    (
        LexResult = ok(Tokens, IT1),
        parser.parse(Tokens, ParseResult),
        (
            ParseResult = ok(Terms),
            execute_repl(IT1, Terms, Env0, Env, Stack0, Stack, !IO),
            IT = IT1
        ;
            ParseResult = error(ParseError),
            report_parse_error(ParseError, !IO),
            IT = IT1,
            Env = Env0,
            Stack = Stack0
        )
    ;
        LexResult = error(LexError),
        report_lex_error(LexError, !IO),
        IT = IT0,
        Env = Env0,
        Stack = Stack0
    ).

:- pred execute_repl(intern_table::in, list(term)::in,
    env::in, env::out, stack::in, stack::out, io::di, io::uo) is cc_multi.

execute_repl(IT, Terms, Env0, Env, Stack0, Stack, !IO) :-
    try_io(eval_terms_wrapper(IT, Terms, Env0, Stack0), Result, !IO),
    (
        Result = succeeded({Env, Stack})
    ;
        Result = exception(Exn),
        ( if univ_to_type(Exn, EvalError) then
            io.format("Runtime error: %s\n",
                [s(types.format_error(IT, EvalError))], !IO),
            Env = Env0,
            Stack = Stack0
        else
            rethrow(Result)
        )
    ).

%-----------------------------------------------------------------------%
% File execution
%-----------------------------------------------------------------------%

:- pred run_file(string::in, io::di, io::uo) is cc_multi.

run_file(Filename, !IO) :-
    io.read_named_file_as_string(Filename, ReadResult, !IO),
    (
        ReadResult = ok(Input),
        run(Input, !IO)
    ;
        ReadResult = error(Error),
        io.format("Error reading '%s': %s\n",
            [s(Filename), s(io.error_message(Error))], !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred run_file_then_repl(string::in, io::di, io::uo) is cc_multi.

run_file_then_repl(Filename, !IO) :-
    io.read_named_file_as_string(Filename, ReadResult, !IO),
    (
        ReadResult = ok(Input),
        run_then_repl(Input, !IO)
    ;
        ReadResult = error(Error),
        io.format("Error reading '%s': %s\n",
            [s(Filename), s(io.error_message(Error))], !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred run_then_repl(string::in, io::di, io::uo) is cc_multi.

run_then_repl(Input, !IO) :-
    lexer.tokenize(Input, empty_intern_table, LexResult),
    (
        LexResult = ok(Tokens, IT),
        parser.parse(Tokens, ParseResult),
        (
            ParseResult = ok(Terms),
            execute_then_repl(IT, Terms, !IO)
        ;
            ParseResult = error(ParseError),
            report_parse_error(ParseError, !IO),
            io.set_exit_status(1, !IO)
        )
    ;
        LexResult = error(LexError),
        report_lex_error(LexError, !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred execute_then_repl(intern_table::in, list(term)::in,
    io::di, io::uo) is cc_multi.

execute_then_repl(IT, Terms, !IO) :-
    Env0 = map.init,
    Stack0 = [],
    try_io(eval_terms_wrapper(IT, Terms, Env0, Stack0), Result, !IO),
    (
        Result = succeeded({Env, Stack}),
        io.write_string("Froth REPL. Press Ctrl-D to exit.\n", !IO),
        repl_loop(IT, Env, Stack, !IO)
    ;
        Result = exception(Exn),
        ( if univ_to_type(Exn, EvalError) then
            io.format("Runtime error: %s\n",
                [s(types.format_error(IT, EvalError))], !IO),
            io.set_exit_status(1, !IO)
        else
            rethrow(Result)
        )
    ).

:- pred run(string::in, io::di, io::uo) is cc_multi.

run(Input, !IO) :-
    lexer.tokenize(Input, empty_intern_table, LexResult),
    (
        LexResult = ok(Tokens, IT),
        parser.parse(Tokens, ParseResult),
        (
            ParseResult = ok(Terms),
            execute(IT, Terms, !IO)
        ;
            ParseResult = error(ParseError),
            report_parse_error(ParseError, !IO),
            io.set_exit_status(1, !IO)
        )
    ;
        LexResult = error(LexError),
        report_lex_error(LexError, !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred execute(intern_table::in, list(term)::in, io::di, io::uo) is cc_multi.

execute(IT, Terms, !IO) :-
    Env0 = map.init,
    Stack0 = [],
    try_io(eval_terms_wrapper(IT, Terms, Env0, Stack0), Result, !IO),
    (
        Result = succeeded({_, _})
    ;
        Result = exception(Exn),
        ( if univ_to_type(Exn, EvalError) then
            io.format("Runtime error: %s\n",
                [s(types.format_error(IT, EvalError))], !IO),
            io.set_exit_status(1, !IO)
        else
            rethrow(Result)
        )
    ).

:- pred eval_terms_wrapper(intern_table::in, list(term)::in,
    env::in, stack::in, {env, stack}::out, io::di, io::uo) is det.

eval_terms_wrapper(IT, Terms, Env0, Stack0, {Env, Stack}, !IO) :-
    eval.eval_terms(IT, Terms, Env0, Env, Stack0, Stack, !IO).

%-----------------------------------------------------------------------%
% Error reporting
%-----------------------------------------------------------------------%

:- pred report_lex_error(lex_error::in, io::di, io::uo) is det.

report_lex_error(unterminated_string(Pos), !IO) :-
    report_at_position(Pos, "unterminated string", !IO).
report_lex_error(unterminated_block_comment(Pos), !IO) :-
    report_at_position(Pos, "unterminated block comment", !IO).
report_lex_error(invalid_escape_sequence(Pos, Char), !IO) :-
    report_at_position(Pos,
        string.format("invalid escape sequence: \\%c", [c(Char)]), !IO).

:- pred report_parse_error(parse_error::in, io::di, io::uo) is det.

report_parse_error(unexpected_token(Pos, Token), !IO) :-
    report_at_position(Pos,
        string.format("unexpected token: %s", [s(token_to_string(Token))]),
        !IO).
report_parse_error(unexpected_close_curly(Pos), !IO) :-
    report_at_position(Pos, "unexpected '}'", !IO).
report_parse_error(unexpected_close_square(Pos), !IO) :-
    report_at_position(Pos, "unexpected ']'", !IO).
report_parse_error(unclosed_curly(Pos), !IO) :-
    report_at_position(Pos, "unclosed '{'", !IO).
report_parse_error(unclosed_square(Pos), !IO) :-
    report_at_position(Pos, "unclosed '['", !IO).
report_parse_error(quote_at_end(Pos), !IO) :-
    report_at_position(Pos, "unexpected end after quote", !IO).
report_parse_error(junk_token(Pos, S), !IO) :-
    report_at_position(Pos,
        string.format("invalid token: '%s'", [s(S)]), !IO).

:- pred report_at_position(position::in, string::in, io::di, io::uo) is det.

report_at_position(position(Line, Col), Msg, !IO) :-
    io.format("%d:%d: %s\n", [i(Line), i(Col), s(Msg)], !IO).

:- func token_to_string(token) = string.

token_to_string(name(Id)) = string.format("name#%d", [i(Id)]).
token_to_string(slash_name(Id)) = string.format("/name#%d", [i(Id)]).
token_to_string(number(N)) = string.format("number %d", [i(N)]).
token_to_string(string(Id)) = string.format("string#%d", [i(Id)]).
token_to_string(quote) = "'''".
token_to_string(bang) = "'!'".
token_to_string(open_curly) = "'{'".
token_to_string(close_curly) = "'}'".
token_to_string(open_square) = "'['".
token_to_string(close_square) = "']'".
token_to_string(junk(S)) = string.format("'%s'", [s(S)]).

%-----------------------------------------------------------------------%
:- end_module froth.
%-----------------------------------------------------------------------%
