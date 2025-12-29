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

:- import_module array.
:- import_module bool.
:- import_module bytecode.
:- import_module datastack.
:- import_module dir.
:- import_module eval.
:- import_module exception.
:- import_module io.file.
:- import_module lexer.
:- import_module list.
:- import_module map.
:- import_module operator_table.
:- import_module operators.
:- import_module parser.
:- import_module string.
:- import_module types.
:- import_module univ.

%-----------------------------------------------------------------------%
% Command line options
%-----------------------------------------------------------------------%

:- type action
    --->    exec_code(string)       % -e CODE: execute code string
    ;       exec_file(string).      % -f FILE: execute file

:- type options
    --->    options(
                opt_no_stdlib    :: bool,       % -n: don't load stdlib
                opt_quiet        :: bool,       % -q: suppress REPL banner
                opt_interactive  :: bool,       % -i: force REPL after actions
                opt_actions      :: list(action)
            ).

:- type args_result
    --->    args_parsed(options)
    ;       args_help
    ;       args_error(string).

:- func default_options = options.

default_options = options(no, no, no, []).

% Unescape \! to ! for -e arguments (shell escaping workaround)
:- func unescape_bang(string) = string.
unescape_bang(S) = string.replace_all(S, "\\!", "!").

%-----------------------------------------------------------------------%
% Argument parsing
%-----------------------------------------------------------------------%

:- pred parse_args(list(string)::in, args_result::out) is det.

parse_args(Args, Result) :-
    parse_args_loop(Args, default_options, Result).

:- pred parse_args_loop(list(string)::in, options::in, args_result::out) is det.

parse_args_loop([], Opts, args_parsed(FinalOpts)) :-
    % Reverse actions to get them in command-line order
    FinalOpts = Opts ^ opt_actions := list.reverse(Opts ^ opt_actions).
parse_args_loop([Arg | Rest], Opts0, Result) :-
    ( if Arg = "-h" ; Arg = "--help" then
        Result = args_help
    else if Arg = "-n" ; Arg = "--no-stdlib" then
        Opts = Opts0 ^ opt_no_stdlib := yes,
        parse_args_loop(Rest, Opts, Result)
    else if Arg = "-q" ; Arg = "--quiet" then
        Opts = Opts0 ^ opt_quiet := yes,
        parse_args_loop(Rest, Opts, Result)
    else if Arg = "-i" ; Arg = "--interactive" then
        Opts = Opts0 ^ opt_interactive := yes,
        parse_args_loop(Rest, Opts, Result)
    else if Arg = "-e" ; Arg = "--exec" then
        ( if Rest = [Code | Rest2] then
            Actions = [exec_code(unescape_bang(Code)) | Opts0 ^ opt_actions],
            Opts = Opts0 ^ opt_actions := Actions,
            parse_args_loop(Rest2, Opts, Result)
        else
            Result = args_error("option " ++ Arg ++ " requires an argument")
        )
    else if Arg = "-f" ; Arg = "--file" then
        ( if Rest = [File | Rest2] then
            Actions = [exec_file(File) | Opts0 ^ opt_actions],
            Opts = Opts0 ^ opt_actions := Actions,
            parse_args_loop(Rest2, Opts, Result)
        else
            Result = args_error("option " ++ Arg ++ " requires an argument")
        )
    else if string.prefix(Arg, "-") then
        Result = args_error("unknown option: " ++ Arg)
    else
        % Positional argument treated as -f FILE
        Actions = [exec_file(Arg) | Opts0 ^ opt_actions],
        Opts = Opts0 ^ opt_actions := Actions,
        parse_args_loop(Rest, Opts, Result)
    ).

%-----------------------------------------------------------------------%
% Initialize tables
%-----------------------------------------------------------------------%

:- pred init_tables(string_table::out, operator_table::out) is det.

init_tables(ST, OpTable) :-
    operator_table.init_operators(empty_string_table, ST, OpTable).

%-----------------------------------------------------------------------%
% Standard library loading
%-----------------------------------------------------------------------%

:- pred get_stdlib_path(string::out, io::di, io::uo) is det.

get_stdlib_path(StdlibPath, !IO) :-
    io.progname("froth", ProgName, !IO),
    ExeDir = dir.dirname(ProgName),
    BaseDir = dir.dirname(ExeDir),
    StdlibPath = dir.make_path_name(BaseDir,
                     dir.make_path_name("lib", "stdlib.froth")).

:- type init_result
    --->    init_ok(operator_table, string_table, array(int), int, env)
    ;       init_error.

:- pred init_with_stdlib(init_result::out, io::di, io::uo) is cc_multi.

init_with_stdlib(Result, !IO) :-
    get_stdlib_path(StdlibPath, !IO),
    io.file.check_file_accessibility(StdlibPath, [read], AccessResult, !IO),
    (
        AccessResult = ok,
        eval_stdlib(StdlibPath, EvalResult, !IO),
        (
            EvalResult = stdlib_ok(OpTable, ST, BC, BCSz, Env),
            Result = init_ok(OpTable, ST, BC, BCSz, Env)
        ;
            EvalResult = stdlib_error,
            Result = init_error
        )
    ;
        AccessResult = error(_),
        % Stdlib not found, initialize without it
        init_tables(ST, OpTable),
        bytecode.init(BC, BCSz),
        Result = init_ok(OpTable, ST, BC, BCSz, map.init)
    ).

:- pred init_without_stdlib(operator_table::out, string_table::out,
    array(int)::array_uo, int::out, env::out) is det.

init_without_stdlib(OpTable, ST, BC, BCSz, Env) :-
    init_tables(ST, OpTable),
    bytecode.init(BC, BCSz),
    Env = map.init.

:- type stdlib_result
    --->    stdlib_ok(operator_table, string_table, array(int), int, env)
    ;       stdlib_error.

:- pred eval_stdlib(string::in, stdlib_result::out, io::di, io::uo) is cc_multi.

eval_stdlib(StdlibPath, Result, !IO) :-
    LibDir = dir.dirname(StdlibPath),
    io.read_named_file_as_string(StdlibPath, ReadResult, !IO),
    (
        ReadResult = ok(Content),
        init_tables(ST0, OpTable),
        bytecode.init(BC0, BCSz0),
        lexer.tokenize(Content, ST0, LexResult),
        (
            LexResult = ok(Tokens, ST1),
            parser.parse(Tokens, ParseResult),
            (
                ParseResult = ok(Terms),
                datastack.init(Array0, Ptr0),
                try_io(eval_terms_wrapper(OpTable, LibDir, ST1, BC0, BCSz0,
                    Terms, map.init, Array0, Ptr0), EvalResult, !IO),
                (
                    EvalResult = succeeded({ST, BC, BCSz, Env, _Array, _Ptr}),
                    Result = stdlib_ok(OpTable, ST, BC, BCSz, Env)
                ;
                    EvalResult = exception(Exn),
                    ( if univ_to_type(Exn, EvalError) then
                        io.format("Runtime error in stdlib: %s\n",
                            [s(types.format_error(ST1, EvalError))], !IO),
                        Result = stdlib_error
                    else
                        rethrow(EvalResult)
                    )
                )
            ;
                ParseResult = error(ParseError),
                io.format("In stdlib: ", [], !IO),
                report_parse_error(ParseError, !IO),
                Result = stdlib_error
            )
        ;
            LexResult = error(LexError),
            io.format("In stdlib: ", [], !IO),
            report_lex_error(LexError, !IO),
            Result = stdlib_error
        )
    ;
        ReadResult = error(Error),
        io.format("Error reading stdlib '%s': %s\n",
            [s(StdlibPath), s(io.error_message(Error))], !IO),
        Result = stdlib_error
    ).

%-----------------------------------------------------------------------%
% Main entry point
%-----------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    parse_args(Args, ArgsResult),
    (
        ArgsResult = args_help,
        print_usage(!IO)
    ;
        ArgsResult = args_error(Msg),
        io.format("Error: %s\n", [s(Msg)], !IO),
        io.write_string("Try 'froth --help' for usage.\n", !IO),
        io.set_exit_status(1, !IO)
    ;
        ArgsResult = args_parsed(Opts),
        run_with_options(Opts, !IO)
    ).

:- pred run_with_options(options::in, io::di, io::uo) is cc_multi.

run_with_options(Opts, !IO) :-
    % Initialize state
    ( if Opts ^ opt_no_stdlib = yes then
        init_without_stdlib(OpTable, ST0, BC0, BCSz0, Env0),
        datastack.init(Array0, Ptr0),
        run_actions(Opts, OpTable, ST0, BC0, BCSz0, Env0, Array0, Ptr0, !IO)
    else
        init_with_stdlib(InitResult, !IO),
        (
            InitResult = init_ok(OpTable, ST0, BC0, BCSz0, Env0),
            datastack.init(Array0, Ptr0),
            run_actions(Opts, OpTable, ST0, BC0, BCSz0, Env0, Array0, Ptr0, !IO)
        ;
            InitResult = init_error,
            io.set_exit_status(1, !IO)
        )
    ).

%-----------------------------------------------------------------------%
% Action execution
%-----------------------------------------------------------------------%

:- pred run_actions(options::in, operator_table::in,
    string_table::in, array(int)::array_di, int::in, env::in,
    array(value)::array_di, int::in, io::di, io::uo) is cc_multi.

run_actions(Opts, OpTable, ST0, BC0, BCSz0, Env0, Array0, Ptr0, !IO) :-
    Actions = Opts ^ opt_actions,
    (
        Actions = [],
        % No actions: start REPL
        start_repl(Opts, OpTable, ST0, BC0, BCSz0, Env0, Array0, Ptr0, !IO)
    ;
        Actions = [_ | _],
        execute_actions(Actions, OpTable, ST0, ST, BC0, BC, BCSz0, BCSz,
            Env0, Env, Array0, Array, Ptr0, Ptr, yes, Success, !IO),
        (
            Success = yes,
            ( if Opts ^ opt_interactive = yes then
                start_repl(Opts, OpTable, ST, BC, BCSz, Env, Array, Ptr, !IO)
            else
                true
            )
        ;
            Success = no,
            io.set_exit_status(1, !IO)
        )
    ).

:- pred execute_actions(list(action)::in, operator_table::in,
    string_table::in, string_table::out,
    array(int)::array_di, array(int)::array_uo,
    int::in, int::out,
    env::in, env::out,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out,
    bool::in, bool::out, io::di, io::uo) is cc_multi.

execute_actions([], _, !ST, !BC, !BCSz, !Env, !Array, !Ptr, !Success, !IO).
execute_actions([Action | Rest], OpTable, !ST, !BC, !BCSz, !Env, !Array, !Ptr, !Success, !IO) :-
    (
        !.Success = no
        % Skip remaining actions if we already failed
    ;
        !.Success = yes,
        (
            Action = exec_code(Code),
            execute_code(OpTable, Code, !ST, !BC, !BCSz, !Env, !Array, !Ptr, !Success, !IO)
        ;
            Action = exec_file(File),
            execute_file(OpTable, File, !ST, !BC, !BCSz, !Env, !Array, !Ptr, !Success, !IO)
        ),
        execute_actions(Rest, OpTable, !ST, !BC, !BCSz, !Env, !Array, !Ptr, !Success, !IO)
    ).

:- pred execute_code(operator_table::in, string::in,
    string_table::in, string_table::out,
    array(int)::array_di, array(int)::array_uo,
    int::in, int::out,
    env::in, env::out,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out,
    bool::in, bool::out, io::di, io::uo) is cc_multi.

execute_code(OpTable, Code, ST0, ST, BC0, BC, BCSz0, BCSz, Env0, Env,
        Array0, Array, Ptr0, Ptr, _, Success, !IO) :-
    lexer.tokenize(Code, ST0, LexResult),
    (
        LexResult = ok(Tokens, ST1),
        parser.parse(Tokens, ParseResult),
        (
            ParseResult = ok(Terms),
            % Use "." as BaseDir for code strings (current directory)
            try_io(eval_terms_wrapper(OpTable, ".", ST1, BC0, BCSz0, Terms,
                Env0, Array0, Ptr0), EvalResult, !IO),
            (
                EvalResult = succeeded({ST, BC, BCSz, Env, Array, Ptr}),
                Success = yes
            ;
                EvalResult = exception(Exn),
                ( if univ_to_type(Exn, EvalError) then
                    io.format("Runtime error: %s\n",
                        [s(types.format_error(ST1, EvalError))], !IO),
                    ST = ST1, Env = Env0,
                    bytecode.init(BC, BCSz),
                    datastack.init(Array, Ptr),
                    Success = no
                else
                    rethrow(EvalResult)
                )
            )
        ;
            ParseResult = error(ParseError),
            report_parse_error(ParseError, !IO),
            ST = ST1, Env = Env0,
            BC = BC0, BCSz = BCSz0, Array = Array0, Ptr = Ptr0,
            Success = no
        )
    ;
        LexResult = error(LexError),
        report_lex_error(LexError, !IO),
        ST = ST0, Env = Env0,
        BC = BC0, BCSz = BCSz0, Array = Array0, Ptr = Ptr0,
        Success = no
    ).

:- pred execute_file(operator_table::in, string::in,
    string_table::in, string_table::out,
    array(int)::array_di, array(int)::array_uo,
    int::in, int::out,
    env::in, env::out,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out,
    bool::in, bool::out, io::di, io::uo) is cc_multi.

execute_file(OpTable, Filename, ST0, ST, BC0, BC, BCSz0, BCSz, Env0, Env,
        Array0, Array, Ptr0, Ptr, _, Success, !IO) :-
    io.read_named_file_as_string(Filename, ReadResult, !IO),
    (
        ReadResult = ok(Content),
        BaseDir = dir.dirname(Filename),
        lexer.tokenize(Content, ST0, LexResult),
        (
            LexResult = ok(Tokens, ST1),
            parser.parse(Tokens, ParseResult),
            (
                ParseResult = ok(Terms),
                try_io(eval_terms_wrapper(OpTable, BaseDir, ST1, BC0, BCSz0,
                    Terms, Env0, Array0, Ptr0), EvalResult, !IO),
                (
                    EvalResult = succeeded({ST, BC, BCSz, Env, Array, Ptr}),
                    Success = yes
                ;
                    EvalResult = exception(Exn),
                    ( if univ_to_type(Exn, EvalError) then
                        io.format("Runtime error: %s\n",
                            [s(types.format_error(ST1, EvalError))], !IO),
                        ST = ST1, Env = Env0,
                        bytecode.init(BC, BCSz),
                        datastack.init(Array, Ptr),
                        Success = no
                    else
                        rethrow(EvalResult)
                    )
                )
            ;
                ParseResult = error(ParseError),
                report_parse_error(ParseError, !IO),
                ST = ST1, Env = Env0,
                BC = BC0, BCSz = BCSz0, Array = Array0, Ptr = Ptr0,
                Success = no
            )
        ;
            LexResult = error(LexError),
            report_lex_error(LexError, !IO),
            ST = ST0, Env = Env0,
            BC = BC0, BCSz = BCSz0, Array = Array0, Ptr = Ptr0,
            Success = no
        )
    ;
        ReadResult = error(Error),
        io.format("Error reading '%s': %s\n",
            [s(Filename), s(io.error_message(Error))], !IO),
        ST = ST0, Env = Env0,
        BC = BC0, BCSz = BCSz0, Array = Array0, Ptr = Ptr0,
        Success = no
    ).

%-----------------------------------------------------------------------%
% REPL
%-----------------------------------------------------------------------%

:- pred start_repl(options::in, operator_table::in, string_table::in,
    array(int)::array_di, int::in, env::in, array(value)::array_di, int::in,
    io::di, io::uo) is cc_multi.

start_repl(Opts, OpTable, ST, BC, BCSz, Env, Array, Ptr, !IO) :-
    ( if Opts ^ opt_quiet = no then
        io.write_string("Froth REPL. Press Ctrl-D to exit.\n", !IO)
    else
        true
    ),
    repl_loop(OpTable, ST, BC, BCSz, Env, Array, Ptr, !IO).

:- pred repl_loop(operator_table::in, string_table::in,
    array(int)::array_di, int::in, env::in,
    array(value)::array_di, int::in, io::di, io::uo) is cc_multi.

repl_loop(OpTable, ST0, BC0, BCSz0, Env0, Array0, Ptr0, !IO) :-
    io.write_string("> ", !IO),
    io.flush_output(!IO),
    io.read_line_as_string(ReadResult, !IO),
    (
        ReadResult = ok(Line),
        repl_eval(OpTable, Line, ST0, ST, BC0, BC, BCSz0, BCSz, Env0, Env,
            Array0, Array, Ptr0, Ptr, !IO),
        repl_loop(OpTable, ST, BC, BCSz, Env, Array, Ptr, !IO)
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
    array(int)::array_di, array(int)::array_uo,
    int::in, int::out,
    env::in, env::out,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out, io::di, io::uo) is cc_multi.

repl_eval(OpTable, Input, ST0, ST, BC0, BC, BCSz0, BCSz, Env0, Env,
        Array0, Array, Ptr0, Ptr, !IO) :-
    lexer.tokenize(Input, ST0, LexResult),
    (
        LexResult = ok(Tokens, ST1),
        parser.parse(Tokens, ParseResult),
        (
            ParseResult = ok(Terms),
            try_io(eval_terms_wrapper(OpTable, ".", ST1, BC0, BCSz0, Terms,
                Env0, Array0, Ptr0), Result, !IO),
            (
                Result = succeeded({ST, BC, BCSz, Env, Array, Ptr})
            ;
                Result = exception(Exn),
                ( if univ_to_type(Exn, EvalError) then
                    io.format("Runtime error: %s\n",
                        [s(types.format_error(ST1, EvalError))], !IO),
                    ST = ST1, Env = Env0,
                    bytecode.init(BC, BCSz),
                    datastack.init(Array, Ptr)
                else
                    rethrow(Result)
                )
            )
        ;
            ParseResult = error(ParseError),
            report_parse_error(ParseError, !IO),
            ST = ST1, Env = Env0,
            BC = BC0, BCSz = BCSz0, Array = Array0, Ptr = Ptr0
        )
    ;
        LexResult = error(LexError),
        report_lex_error(LexError, !IO),
        ST = ST0, Env = Env0,
        BC = BC0, BCSz = BCSz0, Array = Array0, Ptr = Ptr0
    ).

%-----------------------------------------------------------------------%
% Help
%-----------------------------------------------------------------------%

:- pred print_usage(io::di, io::uo) is det.

print_usage(!IO) :-
    io.write_string("Usage: froth [OPTIONS] [FILE]\n", !IO),
    io.write_string("\n", !IO),
    io.write_string("Options:\n", !IO),
    io.write_string("  -n, --no-stdlib    Don't auto-load standard library\n", !IO),
    io.write_string("  -q, --quiet        Suppress REPL banner\n", !IO),
    io.write_string("  -e, --exec CODE    Execute CODE\n", !IO),
    io.write_string("  -f, --file FILE    Execute FILE\n", !IO),
    io.write_string("  -i, --interactive  Start REPL after -e/-f\n", !IO),
    io.write_string("  -h, --help         Show this help\n", !IO),
    io.write_string("\n", !IO),
    io.write_string("If FILE given without -f, treat as -f FILE.\n", !IO),
    io.write_string("If no -e/-f/FILE, start REPL.\n", !IO),
    io.write_string("Multiple -e/-f processed in order.\n", !IO).

%-----------------------------------------------------------------------%
% Evaluation wrapper
%-----------------------------------------------------------------------%

:- pred eval_terms_wrapper(operator_table::in, string::in, string_table::in,
    array(int)::array_di, int::in, list(term)::in, env::in,
    array(value)::array_di, int::in,
    {string_table, array(int), int, env, array(value), int}::out,
    io::di, io::uo) is det.

eval_terms_wrapper(OpTable, BaseDir, ST0, BC0, BCSz0, Terms, Env0, Array0, Ptr0,
        {ST, BC, BCSz, Env, Array, Ptr}, !IO) :-
    eval.eval_terms(OpTable, BaseDir, Terms, Env0, Env, Array0, Array,
        Ptr0, Ptr, ST0, ST, BC0, BC, BCSz0, BCSz, !IO).

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
