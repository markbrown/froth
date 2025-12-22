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

:- import_module dir.
:- import_module eval.
:- import_module exception.
:- import_module io.file.
:- import_module lexer.
:- import_module list.
:- import_module map.
:- import_module operators.
:- import_module parser.
:- import_module string.
:- import_module types.
:- import_module univ.

%-----------------------------------------------------------------------%

    % Initialize tables: string table with operator names pre-interned,
    % and operator table for dispatch.
    %
:- pred init_tables(string_table::out, operator_table::out) is det.

init_tables(ST, OpTable) :-
    operators.init_operators(empty_string_table, ST, OpTable).

%-----------------------------------------------------------------------%
% Standard library loading
%-----------------------------------------------------------------------%

    % Get the path to the standard library relative to the executable.
    %
:- pred get_stdlib_path(string::out, io::di, io::uo) is det.

get_stdlib_path(StdlibPath, !IO) :-
    io.progname("froth", ProgName, !IO),
    ExeDir = dir.dirname(ProgName),
    % Go up one level from bin/ to find lib/
    BaseDir = dir.dirname(ExeDir),
    StdlibPath = dir.make_path_name(BaseDir,
                     dir.make_path_name("lib", "stdlib.froth")).

%-----------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    process_args(Args, !IO).

:- pred process_args(list(string)::in, io::di, io::uo) is cc_multi.

process_args(Args, !IO) :-
    ( if Args = ["-n" | RestArgs] then
        % -n: no stdlib
        process_args_no_stdlib(RestArgs, !IO)
    else
        % Default: load stdlib first
        get_stdlib_path(StdlibPath, !IO),
        load_stdlib_and_run(StdlibPath, Args, !IO)
    ).

:- pred process_args_no_stdlib(list(string)::in, io::di, io::uo) is cc_multi.

process_args_no_stdlib(Args, !IO) :-
    (
        Args = [],
        repl_no_stdlib(!IO)
    ;
        Args = [Arg],
        ( if ( Arg = "-h" ; Arg = "--help" ) then
            print_usage(!IO)
        else
            run_file_no_stdlib(Arg, !IO)
        )
    ;
        Args = [First, Second | Rest],
        ( if First = "-f", Rest = [] then
            run_file_no_stdlib(Second, !IO)
        else if First = "-e", Rest = [] then
            run_file_then_repl_no_stdlib(Second, !IO)
        else
            print_usage(!IO),
            io.set_exit_status(1, !IO)
        )
    ).

:- pred load_stdlib_and_run(string::in, list(string)::in,
    io::di, io::uo) is cc_multi.

load_stdlib_and_run(StdlibPath, Args, !IO) :-
    io.file.check_file_accessibility(StdlibPath, [read], AccessResult, !IO),
    (
        AccessResult = ok,
        % Load stdlib by evaluating it as normal Froth code
        eval_stdlib(StdlibPath, EvalResult, !IO),
        (
            EvalResult = ok({OpTable, ST, Env}),
            % Then process remaining args with stdlib state
            process_args_with_state(OpTable, ST, Env, Args, !IO)
        ;
            EvalResult = error,
            io.set_exit_status(1, !IO)
        )
    ;
        AccessResult = error(_),
        % Stdlib not found, run without it
        process_args_no_stdlib(Args, !IO)
    ).

:- type eval_result
    --->    ok({operator_table, string_table, env})
    ;       error.

    % Evaluate stdlib.froth as normal Froth code.
    % The BaseDir is set to the directory containing stdlib.froth
    % so that relative imports work correctly.
    %
:- pred eval_stdlib(string::in, eval_result::out, io::di, io::uo) is cc_multi.

eval_stdlib(StdlibPath, Result, !IO) :-
    LibDir = dir.dirname(StdlibPath),
    io.read_named_file_as_string(StdlibPath, ReadResult, !IO),
    (
        ReadResult = ok(Content),
        init_tables(ST0, OpTable),
        lexer.tokenize(Content, ST0, LexResult),
        (
            LexResult = ok(Tokens, ST1),
            parser.parse(Tokens, ParseResult),
            (
                ParseResult = ok(Terms),
                try_io(eval_terms_wrapper(OpTable, LibDir, ST1, Terms,
                    map.init, []), EvalResult, !IO),
                (
                    EvalResult = succeeded({ST, Env, _Stack}),
                    Result = ok({OpTable, ST, Env})
                ;
                    EvalResult = exception(Exn),
                    ( if univ_to_type(Exn, EvalError) then
                        io.format("Runtime error in stdlib: %s\n",
                            [s(types.format_error(ST1, EvalError))], !IO),
                        Result = error
                    else
                        rethrow(EvalResult)
                    )
                )
            ;
                ParseResult = error(ParseError),
                io.format("In stdlib: ", [], !IO),
                report_parse_error(ParseError, !IO),
                Result = error
            )
        ;
            LexResult = error(LexError),
            io.format("In stdlib: ", [], !IO),
            report_lex_error(LexError, !IO),
            Result = error
        )
    ;
        ReadResult = error(Error),
        io.format("Error reading stdlib '%s': %s\n",
            [s(StdlibPath), s(io.error_message(Error))], !IO),
        Result = error
    ).

:- pred process_args_with_state(operator_table::in, string_table::in,
    env::in, list(string)::in, io::di, io::uo) is cc_multi.

process_args_with_state(OpTable, ST, Env, Args, !IO) :-
    (
        Args = [],
        % No arguments: start REPL with stdlib loaded
        io.write_string("Froth REPL. Press Ctrl-D to exit.\n", !IO),
        repl_loop(OpTable, ST, Env, [], !IO)
    ;
        Args = [Arg],
        ( if ( Arg = "-h" ; Arg = "--help" ) then
            print_usage(!IO)
        else
            run_file_with_state(OpTable, ST, Env, Arg, !IO)
        )
    ;
        Args = [First, Second | Rest],
        ( if First = "-f", Rest = [] then
            run_file_with_state(OpTable, ST, Env, Second, !IO)
        else if First = "-e", Rest = [] then
            run_file_then_repl_with_state(OpTable, ST, Env, Second, !IO)
        else
            print_usage(!IO),
            io.set_exit_status(1, !IO)
        )
    ).

:- pred repl_no_stdlib(io::di, io::uo) is cc_multi.

repl_no_stdlib(!IO) :-
    io.write_string("Froth REPL. Press Ctrl-D to exit.\n", !IO),
    Env0 = map.init,
    Stack0 = [],
    init_tables(ST0, OpTable),
    repl_loop(OpTable, ST0, Env0, Stack0, !IO).

:- pred run_file_no_stdlib(string::in, io::di, io::uo) is cc_multi.

run_file_no_stdlib(Filename, !IO) :-
    BaseDir = dir.dirname(Filename),
    io.read_named_file_as_string(Filename, ReadResult, !IO),
    (
        ReadResult = ok(Input),
        run_no_stdlib(BaseDir, Input, !IO)
    ;
        ReadResult = error(Error),
        io.format("Error reading '%s': %s\n",
            [s(Filename), s(io.error_message(Error))], !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred run_no_stdlib(string::in, string::in, io::di, io::uo) is cc_multi.

run_no_stdlib(BaseDir, Input, !IO) :-
    init_tables(ST0, OpTable),
    lexer.tokenize(Input, ST0, LexResult),
    (
        LexResult = ok(Tokens, ST),
        parser.parse(Tokens, ParseResult),
        (
            ParseResult = ok(Terms),
            execute(OpTable, BaseDir, ST, Terms, !IO)
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

:- pred run_file_then_repl_no_stdlib(string::in, io::di, io::uo) is cc_multi.

run_file_then_repl_no_stdlib(Filename, !IO) :-
    BaseDir = dir.dirname(Filename),
    io.read_named_file_as_string(Filename, ReadResult, !IO),
    (
        ReadResult = ok(Input),
        run_then_repl_no_stdlib(BaseDir, Input, !IO)
    ;
        ReadResult = error(Error),
        io.format("Error reading '%s': %s\n",
            [s(Filename), s(io.error_message(Error))], !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred run_then_repl_no_stdlib(string::in, string::in, io::di, io::uo) is cc_multi.

run_then_repl_no_stdlib(BaseDir, Input, !IO) :-
    init_tables(ST0, OpTable),
    lexer.tokenize(Input, ST0, LexResult),
    (
        LexResult = ok(Tokens, ST),
        parser.parse(Tokens, ParseResult),
        (
            ParseResult = ok(Terms),
            execute_then_repl(OpTable, BaseDir, ST, Terms, !IO)
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

:- pred print_usage(io::di, io::uo) is det.

print_usage(!IO) :-
    io.write_string("Usage: froth [options] [filename]\n", !IO),
    io.write_string("\n", !IO),
    io.write_string("Options:\n", !IO),
    io.write_string("  -n         No stdlib (don't auto-load standard library)\n", !IO),
    io.write_string("  -e FILE    Execute FILE then start REPL\n", !IO),
    io.write_string("  -f FILE    Run FILE\n", !IO),
    io.write_string("  -h, --help Show this help\n", !IO),
    io.write_string("\n", !IO),
    io.write_string("With no arguments, starts an interactive REPL.\n", !IO).

:- pred repl_loop(operator_table::in, string_table::in, env::in, stack::in,
    io::di, io::uo) is cc_multi.

repl_loop(OpTable, ST0, Env0, Stack0, !IO) :-
    io.write_string("> ", !IO),
    io.flush_output(!IO),
    io.read_line_as_string(ReadResult, !IO),
    (
        ReadResult = ok(Line),
        repl_eval(OpTable, Line, ST0, ST, Env0, Env, Stack0, Stack, !IO),
        repl_loop(OpTable, ST, Env, Stack, !IO)
    ;
        ReadResult = eof,
        io.nl(!IO)
    ;
        ReadResult = error(Error),
        io.format("Error reading input: %s\n",
            [s(io.error_message(Error))], !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred repl_eval(operator_table::in, string::in,
    string_table::in, string_table::out,
    env::in, env::out, stack::in, stack::out, io::di, io::uo) is cc_multi.

repl_eval(OpTable, Input, ST0, ST, Env0, Env, Stack0, Stack, !IO) :-
    lexer.tokenize(Input, ST0, LexResult),
    (
        LexResult = ok(Tokens, ST1),
        parser.parse(Tokens, ParseResult),
        (
            ParseResult = ok(Terms),
            execute_repl(OpTable, ST1, Terms, Env0, Env, Stack0, Stack, !IO),
            ST = ST1
        ;
            ParseResult = error(ParseError),
            report_parse_error(ParseError, !IO),
            ST = ST1,
            Env = Env0,
            Stack = Stack0
        )
    ;
        LexResult = error(LexError),
        report_lex_error(LexError, !IO),
        ST = ST0,
        Env = Env0,
        Stack = Stack0
    ).

:- pred execute_repl(operator_table::in, string_table::in, list(term)::in,
    env::in, env::out, stack::in, stack::out, io::di, io::uo) is cc_multi.

execute_repl(OpTable, ST, Terms, Env0, Env, Stack0, Stack, !IO) :-
    try_io(eval_terms_wrapper(OpTable, ".", ST, Terms, Env0, Stack0), Result, !IO),
    (
        Result = succeeded({_, Env, Stack})
    ;
        Result = exception(Exn),
        ( if univ_to_type(Exn, EvalError) then
            io.format("Runtime error: %s\n",
                [s(types.format_error(ST, EvalError))], !IO),
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
    BaseDir = dir.dirname(Filename),
    io.read_named_file_as_string(Filename, ReadResult, !IO),
    (
        ReadResult = ok(Input),
        run(BaseDir, Input, !IO)
    ;
        ReadResult = error(Error),
        io.format("Error reading '%s': %s\n",
            [s(Filename), s(io.error_message(Error))], !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred run_file_then_repl(string::in, io::di, io::uo) is cc_multi.

run_file_then_repl(Filename, !IO) :-
    BaseDir = dir.dirname(Filename),
    io.read_named_file_as_string(Filename, ReadResult, !IO),
    (
        ReadResult = ok(Input),
        run_then_repl(BaseDir, Input, !IO)
    ;
        ReadResult = error(Error),
        io.format("Error reading '%s': %s\n",
            [s(Filename), s(io.error_message(Error))], !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred run_then_repl(string::in, string::in, io::di, io::uo) is cc_multi.

run_then_repl(BaseDir, Input, !IO) :-
    init_tables(ST0, OpTable),
    lexer.tokenize(Input, ST0, LexResult),
    (
        LexResult = ok(Tokens, ST),
        parser.parse(Tokens, ParseResult),
        (
            ParseResult = ok(Terms),
            execute_then_repl(OpTable, BaseDir, ST, Terms, !IO)
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

:- pred execute_then_repl(operator_table::in, string::in, string_table::in,
    list(term)::in, io::di, io::uo) is cc_multi.

execute_then_repl(OpTable, BaseDir, ST0, Terms, !IO) :-
    Env0 = map.init,
    Stack0 = [],
    try_io(eval_terms_wrapper(OpTable, BaseDir, ST0, Terms, Env0, Stack0), Result, !IO),
    (
        Result = succeeded({ST, Env, Stack}),
        io.write_string("Froth REPL. Press Ctrl-D to exit.\n", !IO),
        repl_loop(OpTable, ST, Env, Stack, !IO)
    ;
        Result = exception(Exn),
        ( if univ_to_type(Exn, EvalError) then
            io.format("Runtime error: %s\n",
                [s(types.format_error(ST0, EvalError))], !IO),
            io.set_exit_status(1, !IO)
        else
            rethrow(Result)
        )
    ).

:- pred run(string::in, string::in, io::di, io::uo) is cc_multi.

run(BaseDir, Input, !IO) :-
    init_tables(ST0, OpTable),
    lexer.tokenize(Input, ST0, LexResult),
    (
        LexResult = ok(Tokens, ST),
        parser.parse(Tokens, ParseResult),
        (
            ParseResult = ok(Terms),
            execute(OpTable, BaseDir, ST, Terms, !IO)
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

:- pred execute(operator_table::in, string::in, string_table::in, list(term)::in,
    io::di, io::uo) is cc_multi.

execute(OpTable, BaseDir, ST, Terms, !IO) :-
    Env0 = map.init,
    Stack0 = [],
    try_io(eval_terms_wrapper(OpTable, BaseDir, ST, Terms, Env0, Stack0), Result, !IO),
    (
        Result = succeeded({_, _, _})
    ;
        Result = exception(Exn),
        ( if univ_to_type(Exn, EvalError) then
            io.format("Runtime error: %s\n",
                [s(types.format_error(ST, EvalError))], !IO),
            io.set_exit_status(1, !IO)
        else
            rethrow(Result)
        )
    ).

:- pred run_file_with_state(operator_table::in, string_table::in, env::in,
    string::in, io::di, io::uo) is cc_multi.

run_file_with_state(OpTable, ST0, Env0, Filename, !IO) :-
    BaseDir = dir.dirname(Filename),
    io.read_named_file_as_string(Filename, ReadResult, !IO),
    (
        ReadResult = ok(Input),
        lexer.tokenize(Input, ST0, LexResult),
        (
            LexResult = ok(Tokens, ST),
            parser.parse(Tokens, ParseResult),
            (
                ParseResult = ok(Terms),
                try_io(eval_terms_wrapper(OpTable, BaseDir, ST, Terms, Env0, []),
                    Result, !IO),
                (
                    Result = succeeded({_, _, _})
                ;
                    Result = exception(Exn),
                    ( if univ_to_type(Exn, EvalError) then
                        io.format("Runtime error: %s\n",
                            [s(types.format_error(ST, EvalError))], !IO),
                        io.set_exit_status(1, !IO)
                    else
                        rethrow(Result)
                    )
                )
            ;
                ParseResult = error(ParseError),
                report_parse_error(ParseError, !IO),
                io.set_exit_status(1, !IO)
            )
        ;
            LexResult = error(LexError),
            report_lex_error(LexError, !IO),
            io.set_exit_status(1, !IO)
        )
    ;
        ReadResult = error(Error),
        io.format("Error reading '%s': %s\n",
            [s(Filename), s(io.error_message(Error))], !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred run_file_then_repl_with_state(operator_table::in, string_table::in,
    env::in, string::in, io::di, io::uo) is cc_multi.

run_file_then_repl_with_state(OpTable, ST0, Env0, Filename, !IO) :-
    BaseDir = dir.dirname(Filename),
    io.read_named_file_as_string(Filename, ReadResult, !IO),
    (
        ReadResult = ok(Input),
        lexer.tokenize(Input, ST0, LexResult),
        (
            LexResult = ok(Tokens, ST),
            parser.parse(Tokens, ParseResult),
            (
                ParseResult = ok(Terms),
                try_io(eval_terms_wrapper(OpTable, BaseDir, ST, Terms, Env0, []),
                    Result, !IO),
                (
                    Result = succeeded({ST1, Env, Stack}),
                    io.write_string("Froth REPL. Press Ctrl-D to exit.\n", !IO),
                    repl_loop(OpTable, ST1, Env, Stack, !IO)
                ;
                    Result = exception(Exn),
                    ( if univ_to_type(Exn, EvalError) then
                        io.format("Runtime error: %s\n",
                            [s(types.format_error(ST, EvalError))], !IO),
                        io.set_exit_status(1, !IO)
                    else
                        rethrow(Result)
                    )
                )
            ;
                ParseResult = error(ParseError),
                report_parse_error(ParseError, !IO),
                io.set_exit_status(1, !IO)
            )
        ;
            LexResult = error(LexError),
            report_lex_error(LexError, !IO),
            io.set_exit_status(1, !IO)
        )
    ;
        ReadResult = error(Error),
        io.format("Error reading '%s': %s\n",
            [s(Filename), s(io.error_message(Error))], !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred eval_terms_wrapper(operator_table::in, string::in, string_table::in,
    list(term)::in, env::in, stack::in, {string_table, env, stack}::out,
    io::di, io::uo) is det.

eval_terms_wrapper(OpTable, BaseDir, ST0, Terms, Env0, Stack0, {ST, Env, Stack}, !IO) :-
    eval.eval_terms(OpTable, BaseDir, Terms, Env0, Env, Stack0, Stack, ST0, ST, !IO).

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
