from lispy2.lispy import *
# import lispy2.lispy

import pdb
# DEBUG = True
DEBUG = False





from typing import Dict, Optional, Sequence, Tuple, Union


# Opcodes
OpcodeGasCost = Union[int, Tuple]
OpcodeValue = Tuple[Optional[int], int, int, OpcodeGasCost]
OpcodeMap = Dict[str, OpcodeValue]
OpcodeRulesetValue = Tuple[Optional[int], int, int, int]
OpcodeRulesetMap = Dict[str, OpcodeRulesetValue]



# opcode as hex value
# number of values removed from stack
# number of values added to stack
# gas cost (byzantium, constantinople, istanbul, berlin)
OPCODES: OpcodeMap = {
    "STOP": (0x00, 0, 0, 0),
    "ADD": (0x01, 2, 1, 3),
    "MUL": (0x02, 2, 1, 5),
    "SUB": (0x03, 2, 1, 3),
    "DIV": (0x04, 2, 1, 5),
    "SDIV": (0x05, 2, 1, 5),
    "MOD": (0x06, 2, 1, 5),
    "SMOD": (0x07, 2, 1, 5),
    "ADDMOD": (0x08, 3, 1, 8),
    "MULMOD": (0x09, 3, 1, 8),
    "EXP": (0x0A, 2, 1, 10),
    "SIGNEXTEND": (0x0B, 2, 1, 5),
    "LT": (0x10, 2, 1, 3),
    "GT": (0x11, 2, 1, 3),
    "SLT": (0x12, 2, 1, 3),
    "SGT": (0x13, 2, 1, 3),
    "EQ": (0x14, 2, 1, 3),
    "ISZERO": (0x15, 1, 1, 3),
    "AND": (0x16, 2, 1, 3),
    "OR": (0x17, 2, 1, 3),
    "XOR": (0x18, 2, 1, 3),
    "NOT": (0x19, 1, 1, 3),
    "BYTE": (0x1A, 2, 1, 3),
    "SHL": (0x1B, 2, 1, (None, 3)),
    "SHR": (0x1C, 2, 1, (None, 3)),
    "SAR": (0x1D, 2, 1, (None, 3)),
    "SHA3": (0x20, 2, 1, 30),
    "ADDRESS": (0x30, 0, 1, 2),
    "BALANCE": (0x31, 1, 1, (400, 400, 700)),
    "ORIGIN": (0x32, 0, 1, 2),
    "CALLER": (0x33, 0, 1, 2),
    "CALLVALUE": (0x34, 0, 1, 2),
    "CALLDATALOAD": (0x35, 1, 1, 3),
    "CALLDATASIZE": (0x36, 0, 1, 2),
    "CALLDATACOPY": (0x37, 3, 0, 3),
    "CODESIZE": (0x38, 0, 1, 2),
    "CODECOPY": (0x39, 3, 0, 3),
    "GASPRICE": (0x3A, 0, 1, 2),
    "EXTCODESIZE": (0x3B, 1, 1, (700, 700, 700, 2600)),
    "EXTCODECOPY": (0x3C, 4, 0, (700, 700, 700, 2600)),
    "RETURNDATASIZE": (0x3D, 0, 1, 2),
    "RETURNDATACOPY": (0x3E, 3, 0, 3),
    "EXTCODEHASH": (0x3F, 1, 1, (None, 400, 700, 2600)),
    "BLOCKHASH": (0x40, 1, 1, 20),
    "COINBASE": (0x41, 0, 1, 2),
    "TIMESTAMP": (0x42, 0, 1, 2),
    "NUMBER": (0x43, 0, 1, 2),
    "DIFFICULTY": (0x44, 0, 1, 2),
    "GASLIMIT": (0x45, 0, 1, 2),
    "CHAINID": (0x46, 0, 1, (None, None, 2)),
    "SELFBALANCE": (0x47, 0, 1, (None, None, 5)),
    "POP": (0x50, 1, 0, 2),
    "MLOAD": (0x51, 1, 1, 3),
    "MSTORE": (0x52, 2, 0, 3),
    "MSTORE8": (0x53, 2, 0, 3),
    "SLOAD": (0x54, 1, 1, (200, 200, 800, 2100)),
    "SSTORE": (0x55, 2, 0, 20000),
    "JUMP": (0x56, 1, 0, 8),
    "JUMPI": (0x57, 2, 0, 10),
    "PC": (0x58, 0, 1, 2),
    "MSIZE": (0x59, 0, 1, 2),
    "GAS": (0x5A, 0, 1, 2),
    "JUMPDEST": (0x5B, 0, 0, 1),
    "PUSH1": (0x60, 0, 1, 3),
    "PUSH2": (0x61, 0, 1, 3),
    "PUSH3": (0x62, 0, 1, 3),
    "PUSH4": (0x63, 0, 1, 3),
    "PUSH5": (0x64, 0, 1, 3),
    "PUSH6": (0x65, 0, 1, 3),
    "PUSH7": (0x66, 0, 1, 3),
    "PUSH8": (0x67, 0, 1, 3),
    "PUSH9": (0x68, 0, 1, 3),
    "PUSH10": (0x69, 0, 1, 3),
    "PUSH11": (0x6A, 0, 1, 3),
    "PUSH12": (0x6B, 0, 1, 3),
    "PUSH13": (0x6C, 0, 1, 3),
    "PUSH14": (0x6D, 0, 1, 3),
    "PUSH15": (0x6E, 0, 1, 3),
    "PUSH16": (0x6F, 0, 1, 3),
    "PUSH17": (0x70, 0, 1, 3),
    "PUSH18": (0x71, 0, 1, 3),
    "PUSH19": (0x72, 0, 1, 3),
    "PUSH20": (0x73, 0, 1, 3),
    "PUSH21": (0x74, 0, 1, 3),
    "PUSH22": (0x75, 0, 1, 3),
    "PUSH23": (0x76, 0, 1, 3),
    "PUSH24": (0x77, 0, 1, 3),
    "PUSH25": (0x78, 0, 1, 3),
    "PUSH26": (0x79, 0, 1, 3),
    "PUSH27": (0x7A, 0, 1, 3),
    "PUSH28": (0x7B, 0, 1, 3),
    "PUSH29": (0x7C, 0, 1, 3),
    "PUSH30": (0x7D, 0, 1, 3),
    "PUSH31": (0x7E, 0, 1, 3),
    "PUSH32": (0x7F, 0, 1, 3),
    "DUP1": (0x80, 1, 2, 3),
    "DUP2": (0x81, 1, 2, 3),
    "DUP3": (0x82, 1, 2, 3),
    "DUP4": (0x83, 1, 2, 3),
    "DUP5": (0x84, 1, 2, 3),
    "DUP6": (0x85, 1, 2, 3),
    "DUP7": (0x86, 1, 2, 3),
    "DUP8": (0x87, 1, 2, 3),
    "DUP9": (0x88, 1, 2, 3),
    "DUP10": (0x89, 1, 2, 3),
    "DUP11": (0x8A, 1, 2, 3),
    "DUP12": (0x8B, 1, 2, 3),
    "DUP13": (0x8C, 1, 2, 3),
    "DUP14": (0x8D, 1, 2, 3),
    "DUP15": (0x8E, 1, 2, 3),
    "DUP16": (0x8F, 1, 2, 3),
    "SWAP1": (0x90, 2, 2, 3),
    "SWAP2": (0x91, 2, 2, 3),
    "SWAP3": (0x92, 2, 2, 3),
    "SWAP4": (0x93, 2, 2, 3),
    "SWAP5": (0x94, 2, 2, 3),
    "SWAP6": (0x95, 2, 2, 3),
    "SWAP7": (0x96, 2, 2, 3),
    "SWAP8": (0x97, 2, 2, 3),
    "SWAP9": (0x98, 2, 2, 3),
    "SWAP10": (0x99, 2, 2, 3),
    "SWAP11": (0x9A, 2, 2, 3),
    "SWAP12": (0x9B, 2, 2, 3),
    "SWAP13": (0x9C, 2, 2, 3),
    "SWAP14": (0x9D, 2, 2, 3),
    "SWAP15": (0x9E, 2, 2, 3),
    "SWAP16": (0x9F, 2, 2, 3),
    "LOG0": (0xA0, 2, 0, 375),
    "LOG1": (0xA1, 3, 0, 750),
    "LOG2": (0xA2, 4, 0, 1125),
    "LOG3": (0xA3, 5, 0, 1500),
    "LOG4": (0xA4, 6, 0, 1875),
    "CREATE": (0xF0, 3, 1, 32000),
    "CALL": (0xF1, 7, 1, (700, 700, 700, 2100)),
    "CALLCODE": (0xF2, 7, 1, (700, 700, 700, 2100)),
    "RETURN": (0xF3, 2, 0, 0),
    "DELEGATECALL": (0xF4, 6, 1, (700, 700, 700, 2100)),
    "CREATE2": (0xF5, 4, 1, (None, 32000)),
    "SELFDESTRUCT": (0xFF, 1, 0, 25000),
    "STATICCALL": (0xFA, 6, 1, (700, 700, 700, 2100)),
    "REVERT": (0xFD, 2, 0, 0),
    "INVALID": (0xFE, 0, 0, 0),
    "DEBUG": (0xA5, 1, 0, 0),
}

PSEUDO_OPCODES: OpcodeMap = {
    "CLAMP": (None, 3, 1, 70),
    "UCLAMPLT": (None, 2, 1, 25),
    "UCLAMPLE": (None, 2, 1, 30),
    "CLAMP_NONZERO": (None, 1, 1, 19),
    "ASSERT": (None, 1, 0, 85),
    "ASSERT_UNREACHABLE": (None, 1, 0, 17),
    "PASS": (None, 0, 0, 0),
    "DUMMY": (None, 0, 1, 0),  # tell LLL that no, there really is a stack item here
    "BREAK": (None, 0, 0, 20),
    "CONTINUE": (None, 0, 0, 20),
    "SHA3_32": (None, 1, 1, 72),
    "SHA3_64": (None, 2, 1, 109),
    "SLE": (None, 2, 1, 10),
    "SGE": (None, 2, 1, 10),
    "LE": (None, 2, 1, 10),
    "GE": (None, 2, 1, 10),
    "CEIL32": (None, 1, 1, 20),
    "SET": (None, 2, 0, 20),
    "NE": (None, 2, 1, 6),
    "DEBUGGER": (None, 0, 0, 0),
    "LABEL": (None, 1, 0, 1),
    "GOTO": (None, 1, 0, 8),
}

COMB_OPCODES: OpcodeMap = {**OPCODES, **PSEUDO_OPCODES}

CONVERT_TABLE = {
    "+": "ADD",
    "*": "MUL",
    "-": "SUB",
    "/": "DIV"
    # "contract": []
}





_quote, _if, _set, _define, _lambda, _begin, _definemacro, = map(Sym, 
"quote   if   set!  define   lambda   begin   define-macro".split())


def inport_to_opcode(inport=InPort(sys.stdin), out=sys.stdout):
    "Convert a scheme source file into opcodes"
    while True:
        try:
            x = parse(inport)
            if x is not eof_object or DEBUG == True:
                print(f"x = {x}")
            if x is eof_object: return
            val = ast_to_opcode(x)       # TODO
            if val is not None and out: print(val, file = out)
        except Exception as e:
            print('%s: %s' % (type(e).__name__, e))


def ast_to_opcode(x, env=global_env):
    "Convert an expr into opcode."
    while True:
        if isa(x, Symbol):
            # exps = [ast_to_opcode(exp, env) for exp in x]
            # proc = exps.pop(0)
            # print(f"=E= {env.find(x)[x]}")
            require(x, x in CONVERT_TABLE, "x not in CONVERT_TABLE")
            return CONVERT_TABLE[x]
        elif not isa(x, list):
            return x
        elif x[0] is _quote:
            (_, exp) = x
            return exp
        elif x[0] is _if:
            (_, test, conseq, alt) = x
            x = (conseq if ast_to_opcode(test, env) else alt)
        elif x[0] is _set:
            (_, var, exp) = x
            env.find(var)[var] = ast_to_opcode(exp, env)
            return None
        elif x[0] is _define:
            (_, var, exp) = x
            env[var] = ast_to_opcode(exp, env)
            return None
        elif x[0] is _lambda:
            (_, vars, exp) = x
            return Procedure(vars, exp, env)
        elif x[0] is _begin:
            for exp in x[1:-1]:
                ast_to_opcode(exp, env)
            x = x[-1]
        else:
            # pdb.set_trace()
            exps = [ast_to_opcode(exp, env) for exp in x]
            opcode = exps.pop(0)
            _, rem_st, add_st, _ = OPCODES[opcode]
            if DEBUG:
                print(f"opcode: {opcode}, rem: {rem_st}, add_st: {add_st}")
            require(exps, len(exps) == rem_st, "Wrong number of arguments")
            out = "\n".join([f"PUSH32 {hex(int(exp))}" for exp in exps])
            return out + "\n" + opcode
            # if isa(proc, Procedure):
                # x = proc.exp
                # env = Env(proc.parms, exps, proc.env)
            # else:
                # return proc(*exps)

def opcode_to_bytecode(filename):
    filename_out = ".".join([filename.split(".")[0], "bytecode"])
    def op2by(token):
        if token in OPCODES:
            out = hex(OPCODES[token][0]).split('x')[-1]
        else:
            out = token.split('x')[-1]
        out = out.zfill(2)
        return out

    with open(filename_out, "w") as fo:
        with open(filename, "r") as fi:
            # fo.write("0x")
            for line in fi.readlines():
                tokens = line.split()
                if DEBUG:
                    print(f"op2by : {op2by(tokens[0])}")
                # tokens = [op2by(token) if token in OPCODES else token for token in tokens]
                tokens = [op2by(token) for token in tokens]
                fo.write("".join(tokens))


def main():
    inport_to_opcode(InPort(open("add.ss")), open("res.asm", "w"))
    opcode_to_bytecode("res.asm")

if __name__ == '__main__':
    main()

