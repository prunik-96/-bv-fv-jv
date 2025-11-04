# casm_lang.py
# CAsm — small educational language mixing C-like syntax and inline assembly.
# English keywords, semicolon-terminated statements, braces, int type, print().
# Inline assembly block: asm { ... } with registers r0..r7 and simple instructions:
# MOV dst, src
# ADD dst, src
# SUB dst, src
# INC reg
# DEC reg
# CMP a, b        -> sets zero_flag if a==b
# JMP label
# JE label        -> jump if equal
# JNE label
# LOAD reg, var   -> load variable into register
# STORE var, reg  -> write register value to variable
# Labels inside asm: label:
#
# Example:
# int x = 5;
# int y = 3;
# asm {
#   LOAD r0, x;
#   LOAD r1, y;
#   ADD r0, r1;
#   STORE x, r0;
# }
# print(x);

import re

# ---------- Lexer ----------
TOKEN_SPEC = [
    ('NUMBER',   r'\d+'),
    ('ID',       r'[A-Za-z_][A-Za-z0-9_]*'),
    ('OP',       r'==|!=|<=|>=|\+|\-|\*|/|%|<|>|\='),
    ('SEMIC',    r';'),
    ('COMMA',    r','),
    ('LBRACE',   r'\{'),
    ('RBRACE',   r'\}'),
    ('LPAREN',   r'\('),
    ('RPAREN',   r'\)'),
    ('COLON',    r':'),
    ('NEWLINE',  r'\n'),
    ('SKIP',     r'[ \t\r]+'),
    ('MISMATCH', r'.'),
]
TOK_REGEX = re.compile('|'.join(f'(?P<{n}>{p})' for n, p in TOKEN_SPEC))
KEYWORDS = {'int', 'print', 'if', 'else', 'while', 'asm', 'return'}

class Token:
    def __init__(self, type_, val):
        self.type = type_
        self.val = val
    def __repr__(self):
        return f"Token({self.type},{self.val})"

def lex(src):
    pos = 0
    tokens = []
    while pos < len(src):
        # comments // to end of line
        if src.startswith('//', pos):
            nl = src.find('\n', pos)
            if nl == -1:
                break
            pos = nl + 1
            continue
        m = TOK_REGEX.match(src, pos)
        if not m:
            raise SyntaxError(f"Lex error at {pos}")
        kind = m.lastgroup
        val = m.group(kind)
        pos = m.end()
        if kind == 'NUMBER':
            tokens.append(Token('NUMBER', int(val)))
        elif kind == 'ID':
            if val in KEYWORDS:
                tokens.append(Token(val.upper(), val))
            else:
                tokens.append(Token('ID', val))
        elif kind in ('OP','SEMIC','COMMA','LBRACE','RBRACE','LPAREN','RPAREN','COLON'):
            tokens.append(Token(kind, val))
        elif kind in ('NEWLINE','SKIP'):
            pass
        elif kind == 'MISMATCH':
            raise SyntaxError(f"Unexpected char {val}")
    tokens.append(Token('EOF',''))
    return tokens

# ---------- Parser ----------
class Parser:
    def __init__(self, tokens):
        self.toks = tokens
        self.i = 0
    def peek(self):
        return self.toks[self.i]
    def next(self):
        t = self.toks[self.i]
        self.i += 1
        return t
    def expect(self, kind, val=None):
        t = self.peek()
        if t.type != kind and t.val != kind:
            raise SyntaxError(f"Expected {kind} but got {t}")
        if val is not None and t.val != val:
            raise SyntaxError(f"Expected {val} but got {t.val}")
        return self.next()

    def parse(self):
        stmts = []
        while self.peek().type != 'EOF':
            stmts.append(self.parse_stmt())
        return ('prog', stmts)

    def parse_stmt(self):
        t = self.peek()
        if t.type == 'INT':
            return self.parse_decl()
        if t.type == 'PRINT':
            self.next()
            self.expect('LPAREN')
            expr = self.parse_expr()
            self.expect('RPAREN')
            self.expect('SEMIC')
            return ('print', expr)
        if t.type == 'IF':
            self.next()
            self.expect('LPAREN')
            cond = self.parse_expr()
            self.expect('RPAREN')
            thenb = self.parse_block()
            elseb = None
            if self.peek().type == 'ELSE':
                self.next()
                elseb = self.parse_block()
            return ('if', cond, thenb, elseb)
        if t.type == 'WHILE':
            self.next()
            self.expect('LPAREN')
            cond = self.parse_expr()
            self.expect('RPAREN')
            body = self.parse_block()
            return ('while', cond, body)
        if t.type == 'ASM':
            return self.parse_asm_block()
        # assignment or expression statement
        if t.type == 'ID':
            # lookahead for '='
            name = t.val
            self.next()
            if self.peek().type == 'OP' and self.peek().val == '=':
                self.next()
                expr = self.parse_expr()
                self.expect('SEMIC')
                return ('assign', name, expr)
            else:
                raise SyntaxError("Only assignments allowed as starting ID stmt")
        if t.type == 'LBRACE':
            return self.parse_block()
        raise SyntaxError(f"Unexpected token {t}")

    def parse_decl(self):
        # int x = expr;
        self.expect('INT')
        t = self.expect('ID')
        name = t.val
        val = None
        if self.peek().type == 'OP' and self.peek().val == '=':
            self.next()
            val = self.parse_expr()
        self.expect('SEMIC')
        return ('decl', name, val)

    def parse_block(self):
        self.expect('LBRACE')
        stmts = []
        while self.peek().type != 'RBRACE':
            if self.peek().type == 'EOF':
                raise SyntaxError("Unclosed block")
            stmts.append(self.parse_stmt())
        self.expect('RBRACE')
        return ('block', stmts)

    # parse inline assembly block (raw text until matching '}')
    def parse_asm_block(self):
        self.expect('ASM')
        self.expect('LBRACE')
        # collect raw tokens until matching RBRACE at same level
        # but easier: build text from tokens until RBRACE
        asm_tokens = []
        depth = 1
        while depth > 0:
            tok = self.next()
            if tok.type == 'EOF':
                raise SyntaxError("Unclosed asm block")
            if tok.type == 'LBRACE':
                depth += 1
            elif tok.type == 'RBRACE':
                depth -= 1
                if depth == 0:
                    break
            asm_tokens.append(tok)
        # convert tokens back to text (simple)
        pieces = []
        for tk in asm_tokens:
            pieces.append(tk.val)
            # keep token spacing for ids/numbers/operators
            pieces.append(' ')
        asm_text = ''.join(pieces).strip()
        return ('asm', asm_text)

    # Expression parser (precedence)
    def parse_expr(self):
        return self.parse_or()
    def parse_or(self):
        node = self.parse_and()
        while self.peek().type == 'OP' and self.peek().val == '||':
            op = self.next().val
            right = self.parse_and()
            node = ('binop', op, node, right)
        return node
    def parse_and(self):
        node = self.parse_eq()
        while self.peek().type == 'OP' and self.peek().val == '&&':
            op = self.next().val
            right = self.parse_eq()
            node = ('binop', op, node, right)
        return node
    def parse_eq(self):
        node = self.parse_rel()
        while self.peek().type == 'OP' and self.peek().val in ('==','!='):
            op = self.next().val
            right = self.parse_rel()
            node = ('binop', op, node, right)
        return node
    def parse_rel(self):
        node = self.parse_add()
        while self.peek().type == 'OP' and self.peek().val in ('<','>','<=','>='):
            op = self.next().val
            right = self.parse_add()
            node = ('binop', op, node, right)
        return node
    def parse_add(self):
        node = self.parse_mul()
        while self.peek().type == 'OP' and self.peek().val in ('+','-'):
            op = self.next().val
            right = self.parse_mul()
            node = ('binop', op, node, right)
        return node
    def parse_mul(self):
        node = self.parse_unary()
        while self.peek().type == 'OP' and self.peek().val in ('*','/','%'):
            op = self.next().val
            right = self.parse_unary()
            node = ('binop', op, node, right)
        return node
    def parse_unary(self):
        if self.peek().type == 'OP' and self.peek().val == '-':
            self.next()
            node = self.parse_unary()
            return ('unary', '-', node)
        return self.parse_primary()
    def parse_primary(self):
        t = self.peek()
        if t.type == 'NUMBER':
            self.next()
            return ('number', t.val)
        if t.type == 'ID':
            # поддержка input()
            if t.val == "input":
                self.next()
                self.expect('LPAREN')
                self.expect('RPAREN')
                return ('call_input',)
            self.next()
            return ('var', t.val)
        if t.type == 'LPAREN':
            self.next()
            node = self.parse_expr()
            self.expect('RPAREN')
            return node
        raise SyntaxError(f"Unexpected in expr: {t}")


# ---------- Interpreter ----------
class Env:
    def __init__(self):
        self.vars = {}  # name -> int
    def declare(self, name, val=0):
        self.vars[name] = val
    def set(self, name, val):
        if name not in self.vars:
            # implicit declare (like C++) — we choose to error; simpler: declare
            self.vars[name] = val
        else:
            self.vars[name] = val
    def get(self, name):
        if name not in self.vars:
            raise NameError(f"Undefined variable '{name}'")
        return self.vars[name]

def eval_expr(node, env):
    typ = node[0]
    if typ == 'number':
        return node[1]
    if typ == 'var':
        return env.get(node[1])
    if typ == 'unary':
        op = node[1]
        v = eval_expr(node[2], env)
        if op == '-':
            return -v
    if typ == 'binop':
        op = node[1]; a = eval_expr(node[2], env); b = eval_expr(node[3], env)
        if op == '+': return a + b
        if op == '-': return a - b
        if op == '*': return a * b
        if op == '/': return a // b if b != 0 else 0
        if op == '%': return a % b
        if op == '==': return 1 if a == b else 0
        if op == '!=': return 1 if a != b else 0
        if op == '<': return 1 if a < b else 0
        if op == '<=': return 1 if a <= b else 0
        if op == '>': return 1 if a > b else 0
        if op == '>=': return 1 if a >= b else 0
        if op == '&&': return 1 if (a and b) else 0
        if op == '||': return 1 if (a or b) else 0
    if typ == 'call_input':  # <--- новая конструкция для input()
        import tkinter as tk
        from tkinter import simpledialog
        root = tk.Tk()
        root.withdraw()
        val = simpledialog.askstring("Input", "Enter value:")
        try:
            return int(val)
        except:
            try:
                return float(val)
            except:
                return val
    raise RuntimeError(f"Unknown expr node {node}")


# assembly emulator
class AsmEmu:
    def __init__(self, env):
        self.regs = {f"r{i}":0 for i in range(8)}
        self.env = env
        self.zero = False
        self.labels = {}
        self.instructions = []  # list of (raw_line, parsed)
        self.ip = 0

    def parse_lines(self, text):
        # split by ';' or newlines, keep labels ending with :
        raw_lines = []
        # allow both ';' and newline as separators
        parts = re.split(r';|\n', text)
        for p in parts:
            line = p.strip()
            if not line:
                continue
            raw_lines.append(line)
        # store lines and scan labels
        self.instructions = []
        for idx, line in enumerate(raw_lines):
            if line.endswith(':'):
                lbl = line[:-1].strip()
                self.labels[lbl] = len(self.instructions)
            else:
                self.instructions.append(line)
        # labels map to instruction indices

    def get_val(self, token):
        token = token.strip()
        if token in self.regs:
            return self.regs[token]
        if re.match(r'^[0-9]+$', token):
            return int(token)
        # variable
        return self.env.get(token)

    def set_reg(self, reg, val):
        if reg not in self.regs:
            raise RuntimeError(f"Unknown register {reg}")
        self.regs[reg] = val

    def exec_all(self):
        self.ip = 0
        steps = 0
        while self.ip < len(self.instructions):
            line = self.instructions[self.ip]
            # parse instruction tokens
            # format: OPC <a>, <b>  or OPC <a>
            parts = [p.strip() for p in re.split(r'[ ,]+', line) if p.strip()!='']
            op = parts[0].upper()
            # simple ops
            if op == 'MOV':
                dst = parts[1]; src = parts[2]
                val = self.get_val(src)
                if dst in self.regs:
                    self.set_reg(dst, val)
                else:
                    # store to variable
                    self.env.set(dst, val)
                self.ip += 1
            elif op == 'ADD':
                dst = parts[1]; src = parts[2]
                x = self.get_val(dst)
                y = self.get_val(src)
                if dst in self.regs:
                    self.set_reg(dst, x + y)
                else:
                    self.env.set(dst, x + y)
                self.ip += 1
            elif op == 'SUB':
                dst = parts[1]; src = parts[2]
                x = self.get_val(dst); y = self.get_val(src)
                if dst in self.regs:
                    self.set_reg(dst, x - y)
                else:
                    self.env.set(dst, x - y)
                self.ip += 1
            elif op == 'INC':
                r = parts[1]
                self.set_reg(r, self.get_val(r) + 1)
                self.ip += 1
            elif op == 'DEC':
                r = parts[1]
                self.set_reg(r, self.get_val(r) - 1)
                self.ip += 1
            elif op == 'CMP':
                a = self.get_val(parts[1]); b = self.get_val(parts[2])
                self.zero = (a == b)
                self.ip += 1
            elif op == 'JMP':
                lbl = parts[1]
                if lbl not in self.labels:
                    raise RuntimeError(f"Unknown label {lbl}")
                self.ip = self.labels[lbl]
            elif op == 'JE':
                lbl = parts[1]
                if self.zero:
                    if lbl not in self.labels:
                        raise RuntimeError(f"Unknown label {lbl}")
                    self.ip = self.labels[lbl]
                else:
                    self.ip += 1
            elif op == 'JNE':
                lbl = parts[1]
                if not self.zero:
                    if lbl not in self.labels:
                        raise RuntimeError(f"Unknown label {lbl}")
                    self.ip = self.labels[lbl]
                else:
                    self.ip += 1
            elif op == 'LOAD':
                reg = parts[1]; var = parts[2]
                val = self.env.get(var)
                self.set_reg(reg, val)
                self.ip += 1
            elif op == 'STORE':
                var = parts[1]; reg = parts[2]
                self.env.set(var, self.get_val(reg))
                self.ip += 1
            else:
                raise RuntimeError(f"Unknown asm op: {op}")
            steps += 1
            if steps > 10000:
                raise RuntimeError("Asm: too many steps (possible infinite loop)")

# Run program
def run_program(src, output_callback):
    toks = lex(src)
    p = Parser(toks)
    tree = p.parse()
    env = Env()
    # interpret
    for stmt in tree[1]:
        exec_stmt(stmt, env, output_callback)

def exec_stmt(stmt, env, output_callback):
    typ = stmt[0]
    if typ == 'decl':
        name = stmt[1]
        val_node = stmt[2]
        if val_node is None:
            env.declare(name, 0)
        else:
            v = eval_expr(val_node, env)
            env.declare(name, v)
    elif typ == 'assign':
        name = stmt[1]
        v = eval_expr(stmt[2], env)
        env.set(name, v)
    elif typ == 'print':
        v = eval_expr(stmt[1], env)
        output_callback(str(v))
    elif typ == 'block':
        for s in stmt[1]:
            exec_stmt(s, env, output_callback)
    elif typ == 'if':
        cond = eval_expr(stmt[1], env)
        if cond:
            exec_stmt(stmt[2], env, output_callback)
        elif stmt[3] is not None:
            exec_stmt(stmt[3], env, output_callback)
    elif typ == 'while':
        while eval_expr(stmt[1], env):
            exec_stmt(stmt[2], env, output_callback)
    elif typ == 'asm':
        asm_text = stmt[1]
        emu = AsmEmu(env)
        emu.parse_lines(asm_text)
        emu.exec_all()
    else:
        raise RuntimeError(f"Unknown stmt {typ}")
