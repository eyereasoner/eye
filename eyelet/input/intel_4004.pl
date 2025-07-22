/*
 * Intel 4004 Microprocessor Emulator
 * 
 * This emulator models the architecture and instruction set of the historic Intel 4004,
 * the world's first commercially available microprocessor (released in 1971).
 *
 * Key Features:
 * - 4-bit data path and 12-bit addressing
 * - 16 x 4-bit general purpose registers
 * - 4-bit accumulator (ACC) and carry flag (CY)
 * - 12-bit program counter (PC)
 * - 3-level subroutine stack
 * - 4096 x 4-bit RAM
 * - Supports core instruction set (NOP, JCN, FIM, ADD, LDM, JUN, FIN, BBL, JIN, CLB, CALL)
 *
 * Processor State Representation:
 * state(PC, Stack, ACC, CY, Registers, RAM)
 * - PC: Program Counter (current address in ROM)
 * - Stack: List of return addresses (max 3 levels)
 * - ACC: Accumulator (4-bit value 0-15)
 * - CY: Carry Flag (0 or 1)
 * - Registers: List of 16 registers (each 4-bit)
 * - RAM: List of 4096 nibbles (4-bit values)
 *
 * Instruction Set Implementation:
 * 00: NOP        - No operation
 * 1C: JCN        - Conditional jump based on flags
 * 2R: FIM        - Load immediate into register pair
 * 3R: FIN        - Indirect load from ROM to register pair
 * 4A: JUN        - Absolute jump
 * 5A: CALL       - Call subroutine
 * 6R: ADD        - Add register to accumulator
 * CR: BBL        - Return from subroutine with value
 * D : LDM        - Load immediate into accumulator
 * F0: CLB        - Clear accumulator and carry
 * FR: JIN        - Indirect jump via register pair
 *
 * Usage:
 * 1. Define ROM content (list of integers representing instructions)
 * 2. Initialize state with init_state/1
 * 3. Execute program with run/2
 *
 * Notes:
 * - Values automatically wrap to 4-bit precision
 * - Carry flag set on arithmetic overflow
 * - Subroutine stack limited to 3 levels
 */

:- op(1200, xfx, :+).

:- use_module(library(lists)).

% Helper: Set value at index in a list
set_nth0(Index, List, Value, NewList) :-
    length(Prefix, Index),
    append(Prefix, [_ | Suffix], List),
    append(Prefix, [Value | Suffix], NewList).

% Helper: Get value at index in a list
get_nth0(Index, List, Value) :-
    nth0(Index, List, Value).

% Helper: Set register pair (High and Low values)
set_reg_pair(Regs, RegIndex, High, Low, NewRegs) :-
    RegIndex1 is RegIndex + 1,
    set_nth0(RegIndex, Regs, High, TempRegs),
    set_nth0(RegIndex1, TempRegs, Low, NewRegs).

% Helper: Truncate stack to max 3 elements
truncate_stack(Stack, NewStack) :-
    length(Stack, Len),
    (   Len > 3 
    ->  length(NewStack, 3),
        append(NewStack, _, Stack)
    ;   NewStack = Stack
    ).

% Execute one instruction
instruction_step(state(PC, Stack, ACC, CY, Regs, RAM), 
    state(NewPC, NewStack, NewACC, NewCY, NewRegs, RAM), ROM) :-
    % Calculate all indices first
    PC1 is PC + 1,
    PC2 is PC + 2,
    PC3 is PC + 3,
    
    % Fetch opcodes
    get_nth0(PC, ROM, Op1),
    get_nth0(PC1, ROM, Op2),
    ( 
        % NOP: 00 - No operation
        Op1 =:= 0, Op2 =:= 0 -> 
            NewPC is PC + 2, NewStack = Stack, NewACC = ACC, NewCY = CY, NewRegs = Regs
        ;
        % JCN: 1C ADDR - Conditional jump
        Op1 =:= 1 -> 
            get_nth0(PC2, ROM, AddrHigh),
            get_nth0(PC3, ROM, AddrLow),
            Address is (AddrHigh << 4) \/ AddrLow,
            Condition = Op2,
            C1 is (Condition >> 1) /\ 1,  % Test ACC=0 condition
            C2 is (Condition >> 2) /\ 1,  % Test CY=1 condition
            C3 is (Condition >> 3) /\ 1,  % Test condition polarity
            (   (C1 =:= 1, ACC =:= 0) -> Flag = true ; 
                (C2 =:= 1, CY =:= 1) -> Flag = true ; 
                Flag = false
            ),
            (   (C3 =:= 0, Flag ; C3 =:= 1, \+ Flag) -> 
                NewPC is (PC /\ 0xF00) \/ Address  % Jump within current page
            ; 
                NewPC is PC + 4  % Skip jump
            ),
            NewStack = Stack, NewACC = ACC, NewCY = CY, NewRegs = Regs
        ;
        % FIM: 2R HH LL - Load immediate into register pair
        Op1 =:= 2 -> 
            get_nth0(PC2, ROM, High),
            get_nth0(PC3, ROM, Low),
            set_reg_pair(Regs, Op2, High, Low, NewRegs),
            NewPC is PC + 4, NewStack = Stack, NewACC = ACC, NewCY = CY
        ;
        % ADD: 6R - Add register to accumulator
        Op1 =:= 6 -> 
            get_nth0(Op2, Regs, RVal),
            Sum is ACC + RVal,
            NewACC is Sum /\ 0xF,        % Keep 4-bit result
            NewCY is Sum >> 4,           % Set carry flag if overflow
            NewPC is PC + 2, NewStack = Stack, NewRegs = Regs
        ;
        % LDM: D IMM - Load immediate into accumulator
        Op1 =:= 13 -> 
            NewACC = Op2,
            NewPC is PC + 2, NewStack = Stack, NewCY = CY, NewRegs = Regs
        ;
        % JUN: 4 AAA - Absolute jump
        Op1 =:= 4 -> 
            get_nth0(PC2, ROM, A1),
            get_nth0(PC3, ROM, A2),
            NewPC is (Op2 << 8) \/ (A1 << 4) \/ A2,
            NewStack = Stack, NewACC = ACC, NewCY = CY, NewRegs = Regs
        ;
        % FIN: 3R - Indirect load from ROM to register pair
        Op1 =:= 3, 0 is Op2 /\ 1 ->  % Only even registers allowed
            get_nth0(0, Regs, R0),    % Get address from register pair 0
            get_nth0(1, Regs, R1),
            Addr is (PC /\ 0xF00) \/ (R0 << 4) \/ R1,
            Addr1 is Addr + 1,
            get_nth0(Addr, ROM, High),
            get_nth0(Addr1, ROM, Low),
            set_reg_pair(Regs, Op2, High, Low, NewRegs),
            NewPC is PC + 2, NewStack = Stack, NewACC = ACC, NewCY = CY
        ;
        % BBL: C IMM - Return from subroutine with value
        Op1 =:= 12 -> 
            Stack = [ReturnAddr | NewStack],
            NewACC = Op2,
            NewPC = ReturnAddr, NewCY = CY, NewRegs = Regs
        ;
        % CLB: F0 - Clear accumulator and carry (MUST COME BEFORE JIN!)
        Op1 =:= 15, Op2 =:= 0 -> 
            NewACC = 0, NewCY = 0,
            NewPC is PC + 2, NewStack = Stack, NewRegs = Regs
        ;
        % JIN: FR - Indirect jump via register pair
        Op1 =:= 15, 0 is Op2 /\ 1 ->  % Only even registers allowed
            get_nth0(Op2, Regs, Rhigh),
            Op21 is Op2 + 1,
            get_nth0(Op21, Regs, Rlow),
            NewPC is (PC /\ 0xF00) \/ (Rhigh << 4) \/ Rlow,
            NewStack = Stack, NewACC = ACC, NewCY = CY, NewRegs = Regs
        ;
        % CALL: 5 AAA - Call subroutine
        Op1 =:= 5 -> 
            get_nth0(PC2, ROM, A1),
            get_nth0(PC3, ROM, A2),
            NewPC is (Op2 << 8) \/ (A1 << 4) \/ A2,
            ReturnAddr is PC + 4,
            truncate_stack([ReturnAddr | Stack], NewStack),
            NewACC = ACC, NewCY = CY, NewRegs = Regs
        ;
        % Default: Unknown instruction
        format('Unknown opcode: ~w ~w at address ~w~n', [Op1, Op2, PC]),
        fail
    ).

% Initialize processor state
init_state(State) :-
    % 16 registers initialized to 0
    length(Regs, 16), maplist(=(0), Regs),
    % 4096 RAM nibbles initialized to 0
    length(RAM, 4096), maplist(=(0), RAM),
    State = state(0, [], 0, 0, Regs, RAM).

% Run program until PC is out of bounds or instruction_step fails
run(State, ROM, ReturnState) :-
    State = state(PC, _, _, _, _, _),
    length(ROM, RomSize),
    (   PC >= RomSize
    ->  ReturnState = State
    ;   (   instruction_step(State, NextState, ROM)
        ->  run(NextState, ROM, ReturnState)
        ;   throw(execution_failed_at_PC(PC))
        )
    ).

% Example program: Add 5 + 15
rom_add([2,0,0,5,   % FIM R0,R1: R0=0, R1=5
        2,2,0,15,   % FIM R2,R3: R2=0, R3=15
        6,1,        % ADD R1 (ACC = 0 + 5)
        6,3]).      % ADD R3 (ACC = 5 + 15 = 20 = 4, CY=1)

add(R) :-
    init_state(S),
    rom_add(ROM),
    run(S, ROM, R).

% query
true :+ add(_).

