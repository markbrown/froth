%-----------------------------------------------------------------------%
% parser.m
% Parser for the Froth programming language.
% Converts tokens to terms.
%-----------------------------------------------------------------------%

:- module parser.
:- interface.

:- import_module lexer.
:- import_module list.
:- import_module types.

%-----------------------------------------------------------------------%

:- type parse_error
    --->    unexpected_token(position, token)
    ;       unexpected_close_curly(position)
    ;       unexpected_close_square(position)
    ;       unclosed_curly(position)
    ;       unclosed_square(position)
    ;       quote_at_end(position)
    ;       junk_token(position, string).

:- type parse_result
    --->    ok(list(term))
    ;       error(parse_error).

    % parse(Tokens, Result):
    % Parse a list of tokens into a list of terms.
    %
:- pred parse(list(located(token))::in, parse_result::out) is det.

%-----------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------%

parse(Tokens, Result) :-
    parse_terms(Tokens, [], no, TermsResult),
    (
        TermsResult = ok({Terms, _}),
        Result = ok(Terms)
    ;
        TermsResult = error(Error),
        Result = error(Error)
    ).

    % Internal result type to avoid ambiguity.
:- type maybe_result(T, E)
    --->    ok(T)
    ;       error(E).

    % What kind of close bracket are we expecting?
:- type expecting
    --->    no                  % Top level, expecting nothing
    ;       curly(position)     % Expecting close_curly
    ;       square(position).   % Expecting close_square

    % parse_terms(Tokens, RevTerms, Expecting, Result):
    % Parse tokens into terms until end or matching close bracket.
    %
:- pred parse_terms(list(located(token))::in, list(term)::in,
    expecting::in,
    maybe_result({list(term), list(located(token))}, parse_error)::out) is det.

parse_terms([], RevTerms, Expecting, Result) :-
    (
        Expecting = no,
        Result = ok({list.reverse(RevTerms), []})
    ;
        Expecting = curly(Pos),
        Result = error(unclosed_curly(Pos))
    ;
        Expecting = square(Pos),
        Result = error(unclosed_square(Pos))
    ).
parse_terms([located(Pos, Token) | Rest], RevTerms, Expecting, Result) :-
    (
        Token = name(N),
        Term = identifier(N),
        parse_terms(Rest, [Term | RevTerms], Expecting, Result)
    ;
        Token = slash_name(N),
        Term = binder(N),
        parse_terms(Rest, [Term | RevTerms], Expecting, Result)
    ;
        Token = number(I),
        Term = value(intval(I)),
        parse_terms(Rest, [Term | RevTerms], Expecting, Result)
    ;
        Token = string(S),
        Term = value(stringval(S)),
        parse_terms(Rest, [Term | RevTerms], Expecting, Result)
    ;
        Token = quote,
        parse_quoted(Pos, Rest, RevTerms, Expecting, Result)
    ;
        Token = bang,
        Term = apply_term,
        parse_terms(Rest, [Term | RevTerms], Expecting, Result)
    ;
        Token = open_curly,
        parse_terms(Rest, [], curly(Pos), InnerResult),
        (
            InnerResult = ok({InnerTerms, Rest2}),
            Term = function(InnerTerms),
            parse_terms(Rest2, [Term | RevTerms], Expecting, Result)
        ;
            InnerResult = error(Error),
            Result = error(Error)
        )
    ;
        Token = close_curly,
        (
            Expecting = curly(_),
            Result = ok({list.reverse(RevTerms), Rest})
        ;
            Expecting = no,
            Result = error(unexpected_close_curly(Pos))
        ;
            Expecting = square(_),
            Result = error(unexpected_close_curly(Pos))
        )
    ;
        Token = open_square,
        parse_terms(Rest, [], square(Pos), InnerResult),
        (
            InnerResult = ok({InnerTerms, Rest2}),
            Term = generator(InnerTerms),
            parse_terms(Rest2, [Term | RevTerms], Expecting, Result)
        ;
            InnerResult = error(Error),
            Result = error(Error)
        )
    ;
        Token = close_square,
        (
            Expecting = square(_),
            Result = ok({list.reverse(RevTerms), Rest})
        ;
            Expecting = no,
            Result = error(unexpected_close_square(Pos))
        ;
            Expecting = curly(_),
            Result = error(unexpected_close_square(Pos))
        )
    ;
        Token = junk(S),
        Result = error(junk_token(Pos, S))
    ).

%-----------------------------------------------------------------------%
% Quote handling: parse one term and wrap it in quoted()
%-----------------------------------------------------------------------%

:- pred parse_quoted(position::in, list(located(token))::in, list(term)::in,
    expecting::in,
    maybe_result({list(term), list(located(token))}, parse_error)::out) is det.

parse_quoted(QuotePos, [], _RevTerms, _Expecting, Result) :-
    Result = error(quote_at_end(QuotePos)).
parse_quoted(_QuotePos, [located(Pos, Token) | Rest], RevTerms, Expecting,
        Result) :-
    parse_single_term(Pos, Token, Rest, SingleResult),
    (
        SingleResult = ok({InnerTerm, Rest2}),
        Term = quoted(InnerTerm),
        parse_terms(Rest2, [Term | RevTerms], Expecting, Result)
    ;
        SingleResult = error(Error),
        Result = error(Error)
    ).

    % Parse a single term (not a sequence).
    %
:- pred parse_single_term(position::in, token::in, list(located(token))::in,
    maybe_result({term, list(located(token))}, parse_error)::out) is det.

parse_single_term(_Pos, name(N), Rest, Result) :-
    Result = ok({identifier(N), Rest}).
parse_single_term(_Pos, slash_name(N), Rest, Result) :-
    Result = ok({binder(N), Rest}).
parse_single_term(_Pos, number(I), Rest, Result) :-
    Result = ok({value(intval(I)), Rest}).
parse_single_term(_Pos, string(S), Rest, Result) :-
    Result = ok({value(stringval(S)), Rest}).
parse_single_term(Pos, quote, Rest, Result) :-
    % Nested quote: 'foo
    (
        Rest = [],
        Result = error(quote_at_end(Pos))
    ;
        Rest = [located(Pos2, Token2) | Rest2],
        parse_single_term(Pos2, Token2, Rest2, InnerResult),
        (
            InnerResult = ok({InnerTerm, Rest3}),
            Result = ok({quoted(InnerTerm), Rest3})
        ;
            InnerResult = error(Error),
            Result = error(Error)
        )
    ).
parse_single_term(Pos, open_curly, Rest, Result) :-
    parse_terms(Rest, [], curly(Pos), InnerResult),
    (
        InnerResult = ok({InnerTerms, Rest2}),
        Result = ok({function(InnerTerms), Rest2})
    ;
        InnerResult = error(Error),
        Result = error(Error)
    ).
parse_single_term(Pos, close_curly, _Rest, Result) :-
    Result = error(unexpected_close_curly(Pos)).
parse_single_term(Pos, open_square, Rest, Result) :-
    parse_terms(Rest, [], square(Pos), InnerResult),
    (
        InnerResult = ok({InnerTerms, Rest2}),
        Result = ok({generator(InnerTerms), Rest2})
    ;
        InnerResult = error(Error),
        Result = error(Error)
    ).
parse_single_term(Pos, close_square, _Rest, Result) :-
    Result = error(unexpected_close_square(Pos)).
parse_single_term(Pos, junk(S), _Rest, Result) :-
    Result = error(junk_token(Pos, S)).
parse_single_term(_Pos, bang, Rest, Result) :-
    Result = ok({apply_term, Rest}).

%-----------------------------------------------------------------------%
:- end_module parser.
%-----------------------------------------------------------------------%
