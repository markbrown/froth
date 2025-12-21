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
:- pred get_env(name_id::in, value::out, env::in) is semidet.
:- pred set_env(name_id::in, value::in, env::in, env::out) is det.

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
        eval_identifier(IT, NameId, !Env, !Stack, !IO)
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

:- pred eval_identifier(intern_table::in, name_id::in,
    env::in, env::out, stack::in, stack::out, io::di, io::uo) is det.

eval_identifier(IT, NameId, !Env, !Stack, !IO) :-
    NameStr = lookup_name(IT ^ it_names, NameId),
    ( if operators.operator(NameStr, Op) then
        eval_operator(IT, Op, !Env, !Stack, !IO)
    else if get_env(NameId, V, !.Env) then
        push(V, !Stack)
    else
        throw(undefined_name(NameId))
    ).

:- pred eval_operator(intern_table::in, operators.operator::in,
    env::in, env::out, stack::in, stack::out, io::di, io::uo) is det.

eval_operator(IT, Op, !Env, !Stack, !IO) :-
    (
        Op = operators.op_print,
        operators.operator_print(IT, !Stack, !IO)
    ;
        Op = operators.op_dump,
        operators.operator_dump(IT, !.Stack, !IO)
    ;
        Op = operators.op_env,
        operators.operator_env(!.Env, !Stack)
    ;
        Op = operators.op_add,
        operators.operator_add(!Stack)
    ;
        Op = operators.op_sub,
        operators.operator_sub(!Stack)
    ;
        Op = operators.op_mul,
        operators.operator_mul(!Stack)
    ;
        Op = operators.op_gt,
        operators.operator_gt(!Stack)
    ;
        Op = operators.op_lt,
        operators.operator_lt(!Stack)
    ;
        Op = operators.op_gte,
        operators.operator_gte(!Stack)
    ;
        Op = operators.op_lte,
        operators.operator_lte(!Stack)
    ;
        Op = operators.op_get,
        operators.operator_get(!Stack)
    ;
        Op = operators.op_length,
        operators.operator_length(!Stack)
    ;
        Op = operators.op_eq,
        operators.operator_eq(!Stack)
    ;
        Op = operators.op_ite,
        operators.operator_ite(!Stack)
    ;
        Op = operators.op_nil,
        operators.operator_nil(!Stack)
    ;
        Op = operators.op_cons,
        operators.operator_cons(!Stack)
    ;
        Op = operators.op_fst,
        operators.operator_fst(!Stack)
    ;
        Op = operators.op_snd,
        operators.operator_snd(!Stack)
    ;
        Op = operators.op_write,
        operators.operator_write(IT, !Stack, !IO)
    ;
        Op = operators.op_fwrite,
        operators.operator_fwrite(IT, !Stack, !IO)
    ;
        Op = operators.op_empty,
        operators.operator_empty(!Stack)
    ;
        Op = operators.op_keys,
        operators.operator_keys(!Stack)
    ;
        Op = operators.op_store,
        operators.operator_store(!Stack)
    ;
        Op = operators.op_in,
        operators.operator_in(!Stack)
    ;
        Op = operators.op_is_int,
        operators.operator_is_int(!Stack)
    ;
        Op = operators.op_is_string,
        operators.operator_is_string(!Stack)
    ;
        Op = operators.op_is_array,
        operators.operator_is_array(!Stack)
    ;
        Op = operators.op_is_map,
        operators.operator_is_map(!Stack)
    ;
        Op = operators.op_is_nil,
        operators.operator_is_nil(!Stack)
    ;
        Op = operators.op_is_cons,
        operators.operator_is_cons(!Stack)
    ;
        Op = operators.op_is_ident,
        operators.operator_is_ident(!Stack)
    ;
        Op = operators.op_is_binder,
        operators.operator_is_binder(!Stack)
    ;
        Op = operators.op_is_func,
        operators.operator_is_func(!Stack)
    ;
        Op = operators.op_is_gen,
        operators.operator_is_gen(!Stack)
    ;
        Op = operators.op_is_quote,
        operators.operator_is_quote(!Stack)
    ;
        Op = operators.op_is_apply,
        operators.operator_is_apply(!Stack)
    ;
        Op = operators.op_to_binder,
        operators.operator_to_binder(!Stack)
    ;
        Op = operators.op_to_ident,
        operators.operator_to_ident(!Stack)
    ).

%-----------------------------------------------------------------------%
% Binder evaluation
%-----------------------------------------------------------------------%

:- pred eval_binder(name_id::in, env::in, env::out,
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
    else
        throw(type_error("closure", V))
    ).

%-----------------------------------------------------------------------%
:- end_module eval.
%-----------------------------------------------------------------------%
