%-----------------------------------------------------------------------%
% eval.m
% Evaluator for the Froth programming language.
%-----------------------------------------------------------------------%

:- module eval.
:- interface.

:- import_module io.
:- import_module list.
:- import_module types.

%-----------------------------------------------------------------------%

    % eval_terms(InternTable, Terms, !Env, !Stack, !IO):
    % Evaluate a list of terms, updating the environment and stack.
    % InternTable is read-only (all strings/names known at lex time).
    % Throws eval_error on failure.
    %
:- pred eval_terms(intern_table::in, list(term)::in,
    env::in, env::out, stack::in, stack::out, io::di, io::uo) is det.

    % Environment operations.
    %
:- pred get_env(string_id::in, value::out, env::in) is semidet.
:- pred set_env(string_id::in, value::in, env::in, env::out) is det.

%-----------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module operators.
:- import_module exception.
:- import_module map.

%-----------------------------------------------------------------------%
% Environment operations
%-----------------------------------------------------------------------%

get_env(Name, Value, Env) :-
    map.search(Env, Name, Value).

set_env(Name, Value, !Env) :-
    map.set(Name, Value, !Env).

%-----------------------------------------------------------------------%
% Main evaluation
%-----------------------------------------------------------------------%

eval_terms(_, [], !Env, !Stack, !IO).
eval_terms(IT, [Term | Terms], !Env, !Stack, !IO) :-
    eval_term(IT, Term, !Env, !Stack, !IO),
    eval_terms(IT, Terms, !Env, !Stack, !IO).

:- pred eval_term(intern_table::in, term::in, env::in, env::out,
    stack::in, stack::out, io::di, io::uo) is det.

eval_term(IT, Term, !Env, !Stack, !IO) :-
    (
        Term = identifier(NameId),
        eval_identifier(IT, NameId, !.Env, !Stack, !IO)
    ;
        Term = binder(NameId),
        eval_binder(NameId, !Env, !Stack)
    ;
        Term = function(Terms),
        eval_function(Terms, !.Env, !Stack)
    ;
        Term = generator(Terms),
        eval_generator(IT, Terms, !Env, !Stack, !IO)
    ;
        Term = quoted(T),
        push(termval(T), !Stack)
    ;
        Term = value(V),
        push(V, !Stack)
    ;
        Term = apply_term,
        eval_apply(IT, !Env, !Stack, !IO)
    ).

%-----------------------------------------------------------------------%
% Identifier evaluation
%-----------------------------------------------------------------------%

:- pred eval_identifier(intern_table::in, string_id::in, env::in,
    stack::in, stack::out, io::di, io::uo) is det.

eval_identifier(IT, NameId, Env, !Stack, !IO) :-
    ( if get_env(NameId, V, Env) then
        push(V, !Stack)
    else if map.search(IT ^ it_operators, NameId, Info) then
        operators.eval_operator(IT, Info ^ oi_operator, Env, !Stack, !IO)
    else
        throw(undefined_name(NameId))
    ).

%-----------------------------------------------------------------------%
% Binder evaluation
%-----------------------------------------------------------------------%

:- pred eval_binder(string_id::in, env::in, env::out,
    stack::in, stack::out) is det.

eval_binder(NameId, !Env, !Stack) :-
    pop("binder", V, !Stack),
    set_env(NameId, V, !Env).

%-----------------------------------------------------------------------%
% Function evaluation
%-----------------------------------------------------------------------%

:- pred eval_function(list(term)::in, env::in,
    stack::in, stack::out) is det.

% A closure is represented as: consval(mapval(Env), termval(function(Terms)))
eval_function(Terms, Env, !Stack) :-
    Closure = consval(mapval(Env), termval(function(Terms))),
    push(Closure, !Stack).

%-----------------------------------------------------------------------%
% Generator evaluation
%-----------------------------------------------------------------------%

:- pred eval_generator(intern_table::in, list(term)::in,
    env::in, env::out, stack::in, stack::out, io::di, io::uo) is det.

eval_generator(IT, Terms, !Env, !Stack, !IO) :-
    % Save current stack, evaluate with empty stack
    SavedStack = !.Stack,
    !:Stack = [],
    eval_terms(IT, Terms, !Env, !Stack, !IO),
    % Convert stack to array (reverse since stack is LIFO)
    Array = array.from_reverse_list(!.Stack),
    % Restore stack and push array
    !:Stack = SavedStack,
    push(arrayval(Array), !Stack).

%-----------------------------------------------------------------------%
% apply (!)
%-----------------------------------------------------------------------%

:- pred eval_apply(intern_table::in, env::in, env::out,
    stack::in, stack::out, io::di, io::uo) is det.

eval_apply(IT, Env, Env, !Stack, !IO) :-
    pop("!", V, !Stack),
    ( if V = consval(mapval(ClosureEnv), termval(function(Terms))) then
        % Evaluate with closure's env, then discard env changes (lexical scoping)
        eval_terms(IT, Terms, ClosureEnv, _, !Stack, !IO)
    else if V = termval(identifier(Id)), map.search(IT ^ it_operators, Id, Info) then
        % Apply a quoted operator
        operators.eval_operator(IT, Info ^ oi_operator, Env, !Stack, !IO)
    else
        throw(type_error("closure or operator", V))
    ).

%-----------------------------------------------------------------------%
:- end_module eval.
%-----------------------------------------------------------------------%
