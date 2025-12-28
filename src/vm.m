%-----------------------------------------------------------------------%
% vm.m
% Bytecode virtual machine for the Froth programming language.
%-----------------------------------------------------------------------%

:- module vm.
:- interface.

:- import_module array.
:- import_module io.
:- import_module list.
:- import_module types.

%-----------------------------------------------------------------------%

    % run(Bytecode, IP, Context, OpTable, ST, Env, !Stack, !SP, FP, GenStack, !IO):
    % Execute bytecode starting at IP until return.
    % Context is the closure's captured environment array.
    % Env is passed for operators that need it (like env).
    % FP is the frame pointer (top of frame stack, grows downward).
    % GenStack is a list of saved stack pointers for generators.
    %
:- pred run(
    array(int)::in,
    int::in,
    array(value)::in,
    operator_table::in,
    string_table::in,
    env::in,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out,
    int::in,
    list(int)::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------%
% Opcode constants
%-----------------------------------------------------------------------%

:- func oc_pushInt = int.
:- func oc_op = int.
:- func oc_return = int.
:- func oc_pushString = int.
:- func oc_pushContext = int.
:- func oc_popUnused = int.
:- func oc_pushLocal = int.
:- func oc_popLocal = int.
:- func oc_enterFrame = int.
:- func oc_leaveFrame = int.
:- func oc_startArray = int.
:- func oc_endArray = int.

%-----------------------------------------------------------------------%

:- implementation.

:- import_module datastack.
:- import_module exception.
:- import_module int.
:- import_module operators.

%-----------------------------------------------------------------------%
% Opcodes
% NOTE: These numbers must be kept in sync with lib/bytecode.froth
%-----------------------------------------------------------------------%

oc_pushInt = 0.
oc_op = 1.
oc_return = 2.
oc_pushString = 3.
oc_pushContext = 4.
oc_popUnused = 5.
oc_pushLocal = 6.
oc_popLocal = 7.
oc_enterFrame = 8.
oc_leaveFrame = 9.
oc_startArray = 10.
oc_endArray = 11.

%-----------------------------------------------------------------------%
% VM execution
%-----------------------------------------------------------------------%

run(BC, IP, Context, OpTable, ST, Env, !Stack, !SP, FP, GenStack, !IO) :-
    array.lookup(BC, IP, Opcode),
    execute(Opcode, BC, IP, Context, OpTable, ST, Env, !Stack, !SP, FP, GenStack, !IO).

:- pred execute(int::in, array(int)::in, int::in, array(value)::in,
    operator_table::in, string_table::in, env::in,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out, int::in, list(int)::in, io::di, io::uo) is det.

execute(Opcode, BC, IP, Context, OpTable, ST, Env, !Stack, !SP, FP, GenStack, !IO) :-
    ( if Opcode = oc_pushInt then
        % pushInt n: push integer n onto stack
        array.lookup(BC, IP + 1, N),
        datastack.push(intval(N), !Stack, !SP),
        run(BC, IP + 2, Context, OpTable, ST, Env, !Stack, !SP, FP, GenStack, !IO)
    else if Opcode = oc_op then
        % op n: execute operator n
        array.lookup(BC, IP + 1, OpNum),
        ( if operators.int_to_operator(OpNum, Op) then
            operators.eval_operator(OpTable, ST, Op, Env, !Stack, !SP, !IO),
            run(BC, IP + 2, Context, OpTable, ST, Env, !Stack, !SP, FP, GenStack, !IO)
        else
            throw(vm_error("invalid operator number"))
        )
    else if Opcode = oc_return then
        % return: stop execution
        true
    else if Opcode = oc_pushString then
        % pushString n: push string with intern ID n
        array.lookup(BC, IP + 1, StrId),
        datastack.push(stringval(StrId), !Stack, !SP),
        run(BC, IP + 2, Context, OpTable, ST, Env, !Stack, !SP, FP, GenStack, !IO)
    else if Opcode = oc_pushContext then
        % pushContext n: push value from context slot n
        array.lookup(BC, IP + 1, Slot),
        array.lookup(Context, Slot, Val),
        datastack.push(Val, !Stack, !SP),
        run(BC, IP + 2, Context, OpTable, ST, Env, !Stack, !SP, FP, GenStack, !IO)
    else if Opcode = oc_popUnused then
        % popUnused: pop and discard top of stack
        datastack.pop("popUnused", _, !Stack, !SP),
        run(BC, IP + 1, Context, OpTable, ST, Env, !Stack, !SP, FP, GenStack, !IO)
    else if Opcode = oc_pushLocal then
        % pushLocal n: push value from frame slot n
        array.lookup(BC, IP + 1, Slot),
        array.lookup(!.Stack, FP + Slot, Val),
        datastack.push(Val, !Stack, !SP),
        run(BC, IP + 2, Context, OpTable, ST, Env, !Stack, !SP, FP, GenStack, !IO)
    else if Opcode = oc_popLocal then
        % popLocal n: pop value into frame slot n
        array.lookup(BC, IP + 1, Slot),
        datastack.pop("popLocal", Val, !Stack, !SP),
        array.set(FP + Slot, Val, !Stack),
        run(BC, IP + 2, Context, OpTable, ST, Env, !Stack, !SP, FP, GenStack, !IO)
    else if Opcode = oc_enterFrame then
        % enterFrame n: allocate n frame slots (FP -= n)
        array.lookup(BC, IP + 1, N),
        NewFP = FP - N,
        ( if NewFP < !.SP then
            throw(vm_error("stack overflow: frame collision"))
        else
            run(BC, IP + 2, Context, OpTable, ST, Env, !Stack, !SP, NewFP, GenStack, !IO)
        )
    else if Opcode = oc_leaveFrame then
        % leaveFrame n: deallocate n frame slots (FP += n)
        array.lookup(BC, IP + 1, N),
        NewFP = FP + N,
        run(BC, IP + 2, Context, OpTable, ST, Env, !Stack, !SP, NewFP, GenStack, !IO)
    else if Opcode = oc_startArray then
        % startArray: save current SP for later array extraction
        run(BC, IP + 1, Context, OpTable, ST, Env, !Stack, !SP, FP, [!.SP | GenStack], !IO)
    else if Opcode = oc_endArray then
        % endArray: extract values since saved SP as array
        (
            GenStack = [SavedSP | RestGenStack],
            datastack.extract_range(!.Stack, SavedSP, !.SP, ResultArray),
            !:SP = SavedSP,
            datastack.push(arrayval(ResultArray), !Stack, !SP),
            run(BC, IP + 1, Context, OpTable, ST, Env, !Stack, !SP, FP, RestGenStack, !IO)
        ;
            GenStack = [],
            throw(vm_error("endArray without matching startArray"))
        )
    else
        throw(vm_error("unknown opcode"))
    ).

%-----------------------------------------------------------------------%
:- end_module vm.
%-----------------------------------------------------------------------%
