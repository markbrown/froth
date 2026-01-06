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

    % Non-unique storage bundled together for threading.
    %
:- type vm_store
    --->    vm_store(
                vs_pool_count   :: int,         % PP: next available pool slot
                vs_string_table :: string_table % ST: string intern table
            ).

    % run(IP, RP, FP, Context, GenStack, !SP,
    %     !Store, OpTable, Env,
    %     !Bytecode, !Stack, !Pool, !HashTable, !IO):
    %
    % Execute bytecode starting at IP until return.
    %
    % Machine registers (input only, except SP):
    %   IP - instruction pointer
    %   RP - return pointer (-1 means return to caller)
    %   FP - frame pointer (top of frame, grows downward)
    %   Context - closure's captured environment array
    %   GenStack - saved stack pointers for generators
    %   SP - stack pointer (threaded, caller needs final value)
    %
    % Non-unique storage (threaded):
    %   Store - bundled PP (pool count) and ST (string table)
    %
    % Execution context (read-only):
    %   OpTable - operator dispatch table
    %   Env - environment for 'env' operator
    %
    % Memory (unique, threaded):
    %   Bytecode - compiled bytecode array
    %   Stack - data/frame stack
    %   Pool - constant pool
    %   HashTable - value to pool index mapping
    %   IO - I/O state
    %
:- pred run(
    int::in, int::in, int::in, array(value)::in, list(int)::in,
    int::in, int::out,
    vm_store::in, vm_store::out,
    operator_table::in, env::in,
    array(int)::array_di, array(int)::array_uo,
    array(value)::array_di, array(value)::array_uo,
    array(value)::array_di, array(value)::array_uo,
    hash_table(value, int)::hash_table_di, hash_table(value, int)::hash_table_uo,
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
:- func oc_call = int.
:- func oc_tailCall = int.
:- func oc_saveReturnPtr = int.
:- func oc_restoreReturnPtr = int.
:- func oc_saveContextPtr = int.
:- func oc_restoreContextPtr = int.
:- func oc_pushQuotedApply = int.

%-----------------------------------------------------------------------%

:- implementation.

:- import_module datastack.
:- import_module exception.
:- import_module int.
:- import_module operator_table.
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
oc_call = 12.
oc_tailCall = 17.
oc_saveReturnPtr = 13.
oc_restoreReturnPtr = 14.
oc_saveContextPtr = 15.
oc_restoreContextPtr = 16.
oc_pushQuotedApply = 18.

%-----------------------------------------------------------------------%
% VM execution
%-----------------------------------------------------------------------%

run(IP, RP, FP, Context, GenStack, !SP, !Store, OpTable, Env,
        !Bytecode, !Stack, !Pool, !HashTable, !IO) :-
    array.lookup(!.Bytecode, IP, Opcode),
    ( if Opcode = oc_pushInt then
        % pushInt n: push integer n onto stack
        array.lookup(!.Bytecode, IP + 1, N),
        datastack.push(intval(N), !Stack, !SP),
        run(IP + 2, RP, FP, Context, GenStack, !SP, !Store, OpTable, Env,
            !Bytecode, !Stack, !Pool, !HashTable, !IO)
    else if Opcode = oc_op then
        % op n: execute operator n
        array.lookup(!.Bytecode, IP + 1, OpNum),
        ( if operator_table.int_to_operator(OpNum, Op) then
            ( if Op = op_ref then
                vm_ref(!Stack, !SP, !Pool, !HashTable),
                run(IP + 2, RP, FP, Context, GenStack, !SP, !Store, OpTable, Env,
                    !Bytecode, !Stack, !Pool, !HashTable, !IO)
            else if Op = op_deref then
                vm_deref(!Stack, !SP, !.Pool),
                run(IP + 2, RP, FP, Context, GenStack, !SP, !Store, OpTable, Env,
                    !Bytecode, !Stack, !Pool, !HashTable, !IO)
            else
                operators.eval_operator(OpTable, !.Store ^ vs_string_table, Op, Env,
                    !Stack, !SP, !IO),
                run(IP + 2, RP, FP, Context, GenStack, !SP, !Store, OpTable, Env,
                    !Bytecode, !Stack, !Pool, !HashTable, !IO)
            )
        else
            throw(vm_error("invalid operator number"))
        )
    else if Opcode = oc_return then
        % return: if RP is -1, stop execution (return to caller)
        % otherwise jump to return address
        ( if RP = -1 then
            true
        else
            run(RP, RP, FP, Context, GenStack, !SP, !Store, OpTable, Env,
                !Bytecode, !Stack, !Pool, !HashTable, !IO)
        )
    else if Opcode = oc_pushString then
        % pushString n: push string with intern ID n
        array.lookup(!.Bytecode, IP + 1, StrId),
        datastack.push(stringval(StrId), !Stack, !SP),
        run(IP + 2, RP, FP, Context, GenStack, !SP, !Store, OpTable, Env,
            !Bytecode, !Stack, !Pool, !HashTable, !IO)
    else if Opcode = oc_pushContext then
        % pushContext n: push value from context slot n
        array.lookup(!.Bytecode, IP + 1, Slot),
        array.lookup(Context, Slot, Val),
        datastack.push(Val, !Stack, !SP),
        run(IP + 2, RP, FP, Context, GenStack, !SP, !Store, OpTable, Env,
            !Bytecode, !Stack, !Pool, !HashTable, !IO)
    else if Opcode = oc_popUnused then
        % popUnused: pop and discard top of stack
        datastack.pop("popUnused", _, !Stack, !SP),
        run(IP + 1, RP, FP, Context, GenStack, !SP, !Store, OpTable, Env,
            !Bytecode, !Stack, !Pool, !HashTable, !IO)
    else if Opcode = oc_pushLocal then
        % pushLocal n: push value from frame slot n
        array.lookup(!.Bytecode, IP + 1, Slot),
        array.lookup(!.Stack, FP + Slot, Val),
        datastack.push(Val, !Stack, !SP),
        run(IP + 2, RP, FP, Context, GenStack, !SP, !Store, OpTable, Env,
            !Bytecode, !Stack, !Pool, !HashTable, !IO)
    else if Opcode = oc_popLocal then
        % popLocal n: pop value into frame slot n
        array.lookup(!.Bytecode, IP + 1, Slot),
        datastack.pop("popLocal", Val, !Stack, !SP),
        array.set(FP + Slot, Val, !Stack),
        run(IP + 2, RP, FP, Context, GenStack, !SP, !Store, OpTable, Env,
            !Bytecode, !Stack, !Pool, !HashTable, !IO)
    else if Opcode = oc_enterFrame then
        % enterFrame n: allocate n frame slots (FP -= n)
        array.lookup(!.Bytecode, IP + 1, N),
        NewFP = FP - N,
        ( if NewFP < !.SP then
            throw(vm_error("stack overflow: frame collision"))
        else
            run(IP + 2, RP, NewFP, Context, GenStack, !SP, !Store, OpTable, Env,
                !Bytecode, !Stack, !Pool, !HashTable, !IO)
        )
    else if Opcode = oc_leaveFrame then
        % leaveFrame n: deallocate n frame slots (FP += n)
        array.lookup(!.Bytecode, IP + 1, N),
        NewFP = FP + N,
        run(IP + 2, RP, NewFP, Context, GenStack, !SP, !Store, OpTable, Env,
            !Bytecode, !Stack, !Pool, !HashTable, !IO)
    else if Opcode = oc_startArray then
        % startArray: save current SP for later array extraction
        run(IP + 1, RP, FP, Context, [!.SP | GenStack], !SP, !Store, OpTable, Env,
            !Bytecode, !Stack, !Pool, !HashTable, !IO)
    else if Opcode = oc_endArray then
        % endArray: extract values since saved SP as array
        (
            GenStack = [SavedSP | RestGenStack],
            datastack.extract_range(!.Stack, SavedSP, !.SP, ResultArray),
            !:SP = SavedSP,
            datastack.push(arrayval(ResultArray), !Stack, !SP),
            run(IP + 1, RP, FP, Context, RestGenStack, !SP, !Store, OpTable, Env,
                !Bytecode, !Stack, !Pool, !HashTable, !IO)
        ;
            GenStack = [],
            throw(vm_error("endArray without matching startArray"))
        )
    else if Opcode = oc_call then
        % call: pop closure, set RP to return address, switch context, jump
        datastack.pop("call", V, !Stack, !SP),
        ( if V = bytecodeval(CalleeContext, CodeAddr) then
            run(CodeAddr, IP + 1, FP, CalleeContext, GenStack, !SP, !Store,
                OpTable, Env, !Bytecode, !Stack, !Pool, !HashTable, !IO)
        else
            throw(type_error("bytecode closure", V))
        )
    else if Opcode = oc_tailCall then
        % tail-call: pop closure, preserve RP, switch context, jump
        datastack.pop("tail-call", V, !Stack, !SP),
        ( if V = bytecodeval(CalleeContext, CodeAddr) then
            run(CodeAddr, RP, FP, CalleeContext, GenStack, !SP, !Store,
                OpTable, Env, !Bytecode, !Stack, !Pool, !HashTable, !IO)
        else
            throw(type_error("bytecode closure", V))
        )
    else if Opcode = oc_saveReturnPtr then
        % saveReturnPtr: push current RP onto data stack
        datastack.push(intval(RP), !Stack, !SP),
        run(IP + 1, RP, FP, Context, GenStack, !SP, !Store, OpTable, Env,
            !Bytecode, !Stack, !Pool, !HashTable, !IO)
    else if Opcode = oc_restoreReturnPtr then
        % restoreReturnPtr: pop RP from data stack
        datastack.pop("restoreReturnPtr", V, !Stack, !SP),
        ( if V = intval(NewRP) then
            run(IP + 1, NewRP, FP, Context, GenStack, !SP, !Store, OpTable, Env,
                !Bytecode, !Stack, !Pool, !HashTable, !IO)
        else
            throw(type_error("int", V))
        )
    else if Opcode = oc_saveContextPtr then
        % saveContextPtr: push current context array onto data stack
        datastack.push(arrayval(Context), !Stack, !SP),
        run(IP + 1, RP, FP, Context, GenStack, !SP, !Store, OpTable, Env,
            !Bytecode, !Stack, !Pool, !HashTable, !IO)
    else if Opcode = oc_restoreContextPtr then
        % restoreContextPtr: pop context array from data stack
        datastack.pop("restoreContextPtr", V, !Stack, !SP),
        ( if V = arrayval(NewContext) then
            run(IP + 1, RP, FP, NewContext, GenStack, !SP, !Store, OpTable, Env,
                !Bytecode, !Stack, !Pool, !HashTable, !IO)
        else
            throw(type_error("array", V))
        )
    else if Opcode = oc_pushQuotedApply then
        % pushQuotedApply: push quoted apply term ('!)
        datastack.push(termval(apply_term), !Stack, !SP),
        run(IP + 1, RP, FP, Context, GenStack, !SP, !Store, OpTable, Env,
            !Bytecode, !Stack, !Pool, !HashTable, !IO)
    else
        throw(vm_error("unknown opcode"))
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

:- pred vm_deref(
    array(value)::array_di, array(value)::array_uo,
    int::in, int::out,
    array(value)::in) is det.

vm_deref(!Stack, !SP, Pool) :-
    datastack.pop("deref", V, !Stack, !SP),
    ( if V = intval(Idx) then
        ( if Idx >= 0, Idx < array.size(Pool) then
            array.lookup(Pool, Idx, Value),
            datastack.push(Value, !Stack, !SP)
        else
            throw(index_out_of_bounds(Idx, array.size(Pool)))
        )
    else
        throw(type_error("int", V))
    ).

%-----------------------------------------------------------------------%
:- end_module vm.
%-----------------------------------------------------------------------%
