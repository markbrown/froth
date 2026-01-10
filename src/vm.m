%-----------------------------------------------------------------------%
% vm.m
% Bytecode virtual machine for the Froth! programming language.
%-----------------------------------------------------------------------%

:- module vm.
:- interface.

:- import_module array.
:- import_module hash_table.
:- import_module io.
:- import_module list.
:- import_module types.

%-----------------------------------------------------------------------%

    % run(Ctx, Env, Context, GenStack, !Store,
    %     IP, RP, FP, !SP, !Stack, !Pool, !Bytecode, !HashTable, !IO):
    %
    % Execute bytecode starting at IP until return.
    %
    % Execution context (read-only):
    %   Ctx - execution context (operator table, base dir)
    %   Env - environment for 'env' operator
    %   Context - closure's captured environment array
    %   GenStack - saved stack pointers for generators
    %
    % Non-unique storage (threaded):
    %   Store - bundled PP (pool count) and ST (string table)
    %
    % Machine registers (input only, except SP):
    %   IP - instruction pointer
    %   RP - return pointer (-1 means return to caller)
    %   FP - frame pointer (top of frame, grows downward)
    %   SP - stack pointer (threaded, caller needs final value)
    %
    % Memory (unique, threaded):
    %   Stack - data/frame stack
    %   Pool - constant pool
    %   Bytecode - compiled bytecode array
    %   HashTable - value to pool index mapping
    %   IO - I/O state
    %
:- pred run(
    exec_context::in, env::in, array(value)::in, list(int)::in,
    eval_store::in, eval_store::out,
    int::in, int::in, int::in,
    int::in, int::out,
    array(value)::array_di, array(value)::array_uo,
    array(value)::array_di, array(value)::array_uo,
    array(int)::array_di, array(int)::array_uo,
    hash_table(value, int)::hash_table_di, hash_table(value, int)::hash_table_uo,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------%
% Opcode constants
%-----------------------------------------------------------------------%

:- func oc_abort = int.
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
:- func oc_call = int.
:- func oc_tailCall = int.
:- func oc_saveReturnPtr = int.
:- func oc_restoreReturnPtr = int.
:- func oc_saveContextPtr = int.
:- func oc_restoreContextPtr = int.
:- func oc_pushQuotedApply = int.

%-----------------------------------------------------------------------%

:- implementation.

:- import_module bytecode.
:- import_module datastack.
:- import_module eval.
:- import_module exception.
:- import_module int.
:- import_module operator_table.
:- import_module operators.
:- import_module string.

%-----------------------------------------------------------------------%
% Opcodes
% NOTE: These numbers must be kept in sync with lib/bytecode.froth
%-----------------------------------------------------------------------%

oc_abort = 0.
oc_pushInt = 1.
oc_op = 2.
oc_return = 3.
oc_pushString = 4.
oc_pushContext = 5.
oc_popUnused = 6.
oc_pushLocal = 7.
oc_popLocal = 8.
oc_enterFrame = 9.
oc_leaveFrame = 10.
oc_startArray = 11.
oc_endArray = 12.
oc_call = 13.
oc_tailCall = 18.
oc_saveReturnPtr = 14.
oc_restoreReturnPtr = 15.
oc_saveContextPtr = 16.
oc_restoreContextPtr = 17.
oc_pushQuotedApply = 19.

%-----------------------------------------------------------------------%
% VM execution
%-----------------------------------------------------------------------%

run(Ctx, Env, Context, GenStack, !Store, IP, RP, FP, !SP,
        !Stack, !Pool, !Bytecode, !HashTable, !IO) :-
    OpTable = Ctx ^ ec_op_table,
    array.lookup(!.Bytecode, IP, Opcode),
    ( if Opcode = oc_abort then
        % abort: throw an exception (useful for catching jumps to uninitialized memory)
        throw(vm_error(IP, vm_abort))
    else if Opcode = oc_pushInt then
        % pushInt n: push integer n onto stack
        array.lookup(!.Bytecode, IP + 1, N),
        datastack.push(intval(N), !Stack, !SP),
        run(Ctx, Env, Context, GenStack, !Store, IP + 2, RP, FP, !SP,
            !Stack, !Pool, !Bytecode, !HashTable, !IO)
    else if Opcode = oc_op then
        % op n: execute operator n
        array.lookup(!.Bytecode, IP + 1, OpNum),
        ( if operator_table.int_to_operator(OpNum, Op) then
            ( if Op = op_ref then
                vm_ref(!Stack, !SP, !Pool, !HashTable),
                run(Ctx, Env, Context, GenStack, !Store, IP + 2, RP, FP, !SP,
                    !Stack, !Pool, !Bytecode, !HashTable, !IO)
            else if Op = op_deref then
                vm_deref(IP, !Stack, !SP, !.Pool),
                run(Ctx, Env, Context, GenStack, !Store, IP + 2, RP, FP, !SP,
                    !Stack, !Pool, !Bytecode, !HashTable, !IO)
            else if Op = op_peek then
                vm_peek(IP, !Stack, !SP, !.Bytecode),
                run(Ctx, Env, Context, GenStack, !Store, IP + 2, RP, FP, !SP,
                    !Stack, !Pool, !Bytecode, !HashTable, !IO)
            else if Op = op_poke then
                vm_poke(IP, !Stack, !SP, !Bytecode),
                run(Ctx, Env, Context, GenStack, !Store, IP + 2, RP, FP, !SP,
                    !Stack, !Pool, !Bytecode, !HashTable, !IO)
            else
                operators.eval_operator(OpTable, !.Store ^ es_string_table, Op, Env,
                    !Stack, !SP, !IO),
                run(Ctx, Env, Context, GenStack, !Store, IP + 2, RP, FP, !SP,
                    !Stack, !Pool, !Bytecode, !HashTable, !IO)
            )
        else
            throw(vm_error(IP, vm_invalid_opcode))
        )
    else if Opcode = oc_return then
        % return: if RP is -1, stop execution (return to caller)
        % otherwise jump to return address
        ( if RP = -1 then
            true
        else
            run(Ctx, Env, Context, GenStack, !Store, RP, RP, FP, !SP,
                !Stack, !Pool, !Bytecode, !HashTable, !IO)
        )
    else if Opcode = oc_pushString then
        % pushString n: push string with intern ID n
        array.lookup(!.Bytecode, IP + 1, StrId),
        datastack.push(stringval(StrId), !Stack, !SP),
        run(Ctx, Env, Context, GenStack, !Store, IP + 2, RP, FP, !SP,
            !Stack, !Pool, !Bytecode, !HashTable, !IO)
    else if Opcode = oc_pushContext then
        % pushContext n: push value from context slot n
        array.lookup(!.Bytecode, IP + 1, Slot),
        array.lookup(Context, Slot, Val),
        datastack.push(Val, !Stack, !SP),
        run(Ctx, Env, Context, GenStack, !Store, IP + 2, RP, FP, !SP,
            !Stack, !Pool, !Bytecode, !HashTable, !IO)
    else if Opcode = oc_popUnused then
        % popUnused: pop and discard top of stack
        datastack.pop("popUnused", _, !Stack, !SP),
        run(Ctx, Env, Context, GenStack, !Store, IP + 1, RP, FP, !SP,
            !Stack, !Pool, !Bytecode, !HashTable, !IO)
    else if Opcode = oc_pushLocal then
        % pushLocal n: push value from frame slot n
        array.lookup(!.Bytecode, IP + 1, Slot),
        array.lookup(!.Stack, FP + Slot, Val),
        datastack.push(Val, !Stack, !SP),
        run(Ctx, Env, Context, GenStack, !Store, IP + 2, RP, FP, !SP,
            !Stack, !Pool, !Bytecode, !HashTable, !IO)
    else if Opcode = oc_popLocal then
        % popLocal n: pop value into frame slot n
        array.lookup(!.Bytecode, IP + 1, Slot),
        datastack.pop("popLocal", Val, !Stack, !SP),
        array.set(FP + Slot, Val, !Stack),
        run(Ctx, Env, Context, GenStack, !Store, IP + 2, RP, FP, !SP,
            !Stack, !Pool, !Bytecode, !HashTable, !IO)
    else if Opcode = oc_enterFrame then
        % enterFrame n: allocate n frame slots (FP -= n)
        array.lookup(!.Bytecode, IP + 1, N),
        NewFP = FP - N,
        ( if NewFP < !.SP then
            throw(vm_error(IP, vm_frame_collision))
        else
            run(Ctx, Env, Context, GenStack, !Store, IP + 2, RP, NewFP, !SP,
                !Stack, !Pool, !Bytecode, !HashTable, !IO)
        )
    else if Opcode = oc_leaveFrame then
        % leaveFrame n: deallocate n frame slots (FP += n)
        array.lookup(!.Bytecode, IP + 1, N),
        NewFP = FP + N,
        run(Ctx, Env, Context, GenStack, !Store, IP + 2, RP, NewFP, !SP,
            !Stack, !Pool, !Bytecode, !HashTable, !IO)
    else if Opcode = oc_startArray then
        % startArray: save current SP for later array extraction
        run(Ctx, Env, Context, [!.SP | GenStack], !Store, IP + 1, RP, FP, !SP,
            !Stack, !Pool, !Bytecode, !HashTable, !IO)
    else if Opcode = oc_endArray then
        % endArray: extract values since saved SP as array
        (
            GenStack = [SavedSP | RestGenStack],
            datastack.extract_range(!.Stack, SavedSP, !.SP, ResultArray),
            !:SP = SavedSP,
            datastack.push(arrayval(ResultArray), !Stack, !SP),
            run(Ctx, Env, Context, RestGenStack, !Store, IP + 1, RP, FP, !SP,
                !Stack, !Pool, !Bytecode, !HashTable, !IO)
        ;
            GenStack = [],
            throw(vm_error(IP, vm_unmatched_end_array))
        )
    else if Opcode = oc_call then
        % call: pop closure, set RP to return address, switch context, jump
        datastack.pop("call", V, !Stack, !SP),
        ( if V = bytecodeval(CalleeContext, CodeAddr) then
            run(Ctx, Env, CalleeContext, GenStack, !Store, CodeAddr, IP + 1, FP, !SP,
                !Stack, !Pool, !Bytecode, !HashTable, !IO)
        else if V = closureval(ClosureEnv, Terms) then
            % Call interpreter closure, then continue at return address
            eval.eval_terms(Ctx, Terms, ClosureEnv, _, !Store, !SP, !Stack, !Pool,
                !Bytecode, !HashTable, !IO),
            run(Ctx, Env, Context, GenStack, !Store, IP + 1, RP, FP, !SP,
                !Stack, !Pool, !Bytecode, !HashTable, !IO)
        else
            throw(vm_error(IP, vm_type_error("closure", V)))
        )
    else if Opcode = oc_tailCall then
        % tail-call: pop closure, preserve RP, switch context, jump
        datastack.pop("tail-call", V, !Stack, !SP),
        ( if V = bytecodeval(CalleeContext, CodeAddr) then
            run(Ctx, Env, CalleeContext, GenStack, !Store, CodeAddr, RP, FP, !SP,
                !Stack, !Pool, !Bytecode, !HashTable, !IO)
        else if V = closureval(ClosureEnv, Terms) then
            % Tail-call interpreter closure, then return
            eval.eval_terms(Ctx, Terms, ClosureEnv, _, !Store, !SP, !Stack, !Pool,
                !Bytecode, !HashTable, !IO),
            ( if RP = -1 then
                true
            else
                run(Ctx, Env, Context, GenStack, !Store, RP, RP, FP, !SP,
                    !Stack, !Pool, !Bytecode, !HashTable, !IO)
            )
        else
            throw(vm_error(IP, vm_type_error("closure", V)))
        )
    else if Opcode = oc_saveReturnPtr then
        % saveReturnPtr: push current RP onto data stack
        datastack.push(intval(RP), !Stack, !SP),
        run(Ctx, Env, Context, GenStack, !Store, IP + 1, RP, FP, !SP,
            !Stack, !Pool, !Bytecode, !HashTable, !IO)
    else if Opcode = oc_restoreReturnPtr then
        % restoreReturnPtr: pop RP from data stack
        datastack.pop("restoreReturnPtr", V, !Stack, !SP),
        ( if V = intval(NewRP) then
            run(Ctx, Env, Context, GenStack, !Store, IP + 1, NewRP, FP, !SP,
                !Stack, !Pool, !Bytecode, !HashTable, !IO)
        else
            throw(vm_error(IP, vm_type_error("int", V)))
        )
    else if Opcode = oc_saveContextPtr then
        % saveContextPtr: push current context array onto data stack
        datastack.push(arrayval(Context), !Stack, !SP),
        run(Ctx, Env, Context, GenStack, !Store, IP + 1, RP, FP, !SP,
            !Stack, !Pool, !Bytecode, !HashTable, !IO)
    else if Opcode = oc_restoreContextPtr then
        % restoreContextPtr: pop context array from data stack
        datastack.pop("restoreContextPtr", V, !Stack, !SP),
        ( if V = arrayval(NewContext) then
            run(Ctx, Env, NewContext, GenStack, !Store, IP + 1, RP, FP, !SP,
                !Stack, !Pool, !Bytecode, !HashTable, !IO)
        else
            throw(vm_error(IP, vm_type_error("array", V)))
        )
    else if Opcode = oc_pushQuotedApply then
        % pushQuotedApply: push quoted apply term ('!)
        datastack.push(termval(apply_term), !Stack, !SP),
        run(Ctx, Env, Context, GenStack, !Store, IP + 1, RP, FP, !SP,
            !Stack, !Pool, !Bytecode, !HashTable, !IO)
    else
        throw(vm_error(IP, vm_unknown_opcode))
    ).

%-----------------------------------------------------------------------%
% ref: ( value -- int ) Store value in constant pool, return index
%-----------------------------------------------------------------------%

:- pred vm_ref(
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out,
    array(value)::array_di, array(value)::array_uo,
    hash_table(value, int)::hash_table_di,
    hash_table(value, int)::hash_table_uo) is det.

vm_ref(!Stack, !SP, !Pool, !HashTable) :-
    datastack.pop("ref", V, !Stack, !SP),
    ( if hash_table.search(!.HashTable, V, ExistingIdx) then
        datastack.push(intval(ExistingIdx), !Stack, !SP)
    else
        Idx = array.size(!.Pool),
        array.resize(Idx + 1, V, !Pool),
        hash_table.det_insert(V, Idx, !HashTable),
        datastack.push(intval(Idx), !Stack, !SP)
    ).

%-----------------------------------------------------------------------%
% deref: ( int -- value ) Retrieve value from constant pool
%-----------------------------------------------------------------------%

:- pred vm_deref(int::in,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out,
    array(value)::in) is det.

vm_deref(IP, !Stack, !SP, Pool) :-
    datastack.pop("deref", V, !Stack, !SP),
    ( if V = intval(Idx) then
        ( if Idx >= 0, Idx < array.size(Pool) then
            array.lookup(Pool, Idx, Value),
            datastack.push(Value, !Stack, !SP)
        else
            throw(vm_error(IP, vm_index_out_of_bounds(Idx, array.size(Pool))))
        )
    else
        throw(vm_error(IP, vm_type_error("int", V)))
    ).

%-----------------------------------------------------------------------%
% peek: ( addr -- int ) Read value from bytecode address
%-----------------------------------------------------------------------%

:- pred vm_peek(int::in,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out,
    array(int)::in) is det.

vm_peek(IP, !Stack, !SP, Bytecode) :-
    datastack.pop("peek", V, !Stack, !SP),
    ( if V = intval(Addr) then
        Value = bytecode.peek(Addr, Bytecode),
        datastack.push(intval(Value), !Stack, !SP)
    else
        throw(vm_error(IP, vm_type_error("int", V)))
    ).

%-----------------------------------------------------------------------%
% poke: ( value addr -- ) Write value to bytecode address
%-----------------------------------------------------------------------%

:- pred vm_poke(int::in,
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out,
    array(int)::array_di, array(int)::array_uo) is det.

vm_poke(IP, !Stack, !SP, !Bytecode) :-
    datastack.pop("poke", AddrVal, !Stack, !SP),
    datastack.pop("poke", ValueVal, !Stack, !SP),
    ( if AddrVal = intval(Addr), ValueVal = intval(Value) then
        bytecode.poke(Addr, Value, !Bytecode)
    else if AddrVal = intval(_) then
        throw(vm_error(IP, vm_type_error("int", ValueVal)))
    else
        throw(vm_error(IP, vm_type_error("int", AddrVal)))
    ).

%-----------------------------------------------------------------------%
:- end_module vm.
%-----------------------------------------------------------------------%
