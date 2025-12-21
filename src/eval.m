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
:- import_module builtins.
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
    ( if builtins.builtin(NameStr, Builtin) then
        eval_builtin(IT, Builtin, !Env, !Stack, !IO)
    else if get_env(NameId, V, !.Env) then
        push(V, !Stack)
    else
        throw(undefined_name(NameId))
    ).

:- pred eval_builtin(intern_table::in, builtins.builtin::in,
    env::in, env::out, stack::in, stack::out, io::di, io::uo) is det.

eval_builtin(IT, Builtin, !Env, !Stack, !IO) :-
    (
        Builtin = builtins.bi_print,
        builtins.builtin_print(IT, !Stack, !IO)
    ;
        Builtin = builtins.bi_dump,
        builtins.builtin_dump(IT, !.Stack, !IO)
    ;
        Builtin = builtins.bi_env,
        builtins.builtin_env(!.Env, !Stack)
    ;
        Builtin = builtins.bi_add,
        builtins.builtin_add(!Stack)
    ;
        Builtin = builtins.bi_sub,
        builtins.builtin_sub(!Stack)
    ;
        Builtin = builtins.bi_mul,
        builtins.builtin_mul(!Stack)
    ;
        Builtin = builtins.bi_gt,
        builtins.builtin_gt(!Stack)
    ;
        Builtin = builtins.bi_lt,
        builtins.builtin_lt(!Stack)
    ;
        Builtin = builtins.bi_gte,
        builtins.builtin_gte(!Stack)
    ;
        Builtin = builtins.bi_lte,
        builtins.builtin_lte(!Stack)
    ;
        Builtin = builtins.bi_get,
        builtins.builtin_get(!Stack)
    ;
        Builtin = builtins.bi_length,
        builtins.builtin_length(!Stack)
    ;
        Builtin = builtins.bi_eq,
        builtins.builtin_eq(!Stack)
    ;
        Builtin = builtins.bi_ite,
        builtins.builtin_ite(!Stack)
    ;
        Builtin = builtins.bi_nil,
        builtins.builtin_nil(!Stack)
    ;
        Builtin = builtins.bi_cons,
        builtins.builtin_cons(!Stack)
    ;
        Builtin = builtins.bi_fst,
        builtins.builtin_fst(!Stack)
    ;
        Builtin = builtins.bi_snd,
        builtins.builtin_snd(!Stack)
    ;
        Builtin = builtins.bi_write,
        builtins.builtin_write(IT, !Stack, !IO)
    ;
        Builtin = builtins.bi_fwrite,
        builtins.builtin_fwrite(IT, !Stack, !IO)
    ;
        Builtin = builtins.bi_empty,
        builtins.builtin_empty(!Stack)
    ;
        Builtin = builtins.bi_keys,
        builtins.builtin_keys(!Stack)
    ;
        Builtin = builtins.bi_store,
        builtins.builtin_store(!Stack)
    ;
        Builtin = builtins.bi_in,
        builtins.builtin_in(!Stack)
    ;
        Builtin = builtins.bi_is_int,
        builtins.builtin_is_int(!Stack)
    ;
        Builtin = builtins.bi_is_string,
        builtins.builtin_is_string(!Stack)
    ;
        Builtin = builtins.bi_is_array,
        builtins.builtin_is_array(!Stack)
    ;
        Builtin = builtins.bi_is_map,
        builtins.builtin_is_map(!Stack)
    ;
        Builtin = builtins.bi_is_nil,
        builtins.builtin_is_nil(!Stack)
    ;
        Builtin = builtins.bi_is_cons,
        builtins.builtin_is_cons(!Stack)
    ;
        Builtin = builtins.bi_is_ident,
        builtins.builtin_is_ident(!Stack)
    ;
        Builtin = builtins.bi_is_binder,
        builtins.builtin_is_binder(!Stack)
    ;
        Builtin = builtins.bi_is_func,
        builtins.builtin_is_func(!Stack)
    ;
        Builtin = builtins.bi_is_gen,
        builtins.builtin_is_gen(!Stack)
    ;
        Builtin = builtins.bi_is_quote,
        builtins.builtin_is_quote(!Stack)
    ;
        Builtin = builtins.bi_is_apply,
        builtins.builtin_is_apply(!Stack)
    ;
        Builtin = builtins.bi_to_binder,
        builtins.builtin_to_binder(!Stack)
    ;
        Builtin = builtins.bi_to_ident,
        builtins.builtin_to_ident(!Stack)
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
