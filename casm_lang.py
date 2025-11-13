# casm_lang.py — CAsm + squads/dft + мини-3D движок (tkinter) + box (lists)
# Примеры:
#   box a = [1,2,3];
#   a.add(5);
#   a[1] = 7;
#   print(a);         // [1, 7, 3, 5]
#   print(a.len());   // 4
#   a.sort();
#   print(a[0]);      // 1
#
# Совместимые команды 3D:
#   window <w> <h> <title>;
#   camera <x> <y> <z>;
#   object cube|sphere <name> <x> <y> <z> <size> <color>;
#   move <name> <dx> <dy> <dz>;
#   rotate <name> <rx> <ry> <rz>;
#   render;

import re
import math
import tkinter as tk
import engine3d as engine3d
# ---------------- Mini 3D Engine (с безопасным повторным открытием) ----------------

from engine3d import Mini3DEngine
ENGINE = Mini3DEngine()

# ---------------- Lexer ----------------
TOKEN_SPEC = [
    ('STRING',   r'"[^"\n]*"'),
    ('NUMBER',   r'\d+(\.\d+)?'),
    ('DOT',      r'\.'),
    ('LBRACK',   r'\['),
    ('RBRACK',   r'\]'),
    ('ID',       r'[A-Za-z_][A-Za-z0-9_]*'),
    ('OP',       r'==|!=|<=|>=|\|\||&&|\+|\-|\*|/|%|<|>|\='),
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

KEYWORDS = {
    'int', 'box', 'print', 'if', 'else', 'while', 'asm', 'return',
    'squad', 'dft'
}

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
            v = float(val) if '.' in val else int(val)
            tokens.append(Token('NUMBER', v))
        elif kind == 'STRING':
            tokens.append(Token('STRING', val[1:-1]))
        elif kind == 'ID':
            if val in KEYWORDS:
                tokens.append(Token(val.upper(), val))
            else:
                tokens.append(Token('ID', val))
        elif kind in ('DOT','OP','SEMIC','COMMA','LBRACE','RBRACE','LPAREN','RPAREN','COLON','LBRACK','RBRACK'):
            tokens.append(Token(kind, val))
        elif kind in ('NEWLINE','SKIP'):
            pass
        elif kind == 'MISMATCH':
            raise SyntaxError(f"Unexpected char {val}")
    tokens.append(Token('EOF',''))
    return tokens

# ---------------- Parser ----------------
class Parser:
    CMD_WORDS = {'window','camera','object','move','rotate','render'}

    def __init__(self, tokens):
        self.toks = tokens
        self.i = 0

    def peek(self): return self.toks[self.i]
    def peekn(self, n):
        j = self.i + n
        return self.toks[j] if j < len(self.toks) else Token('EOF','')
    def next(self):
        t = self.toks[self.i]; self.i += 1; return t
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
            stmts.append(self.parse_toplevel())
        return ('prog', stmts)

    def parse_toplevel(self):
        t = self.peek()
        if t.type == 'SQUAD':
            return self.parse_squad()
        if t.type == 'DFT':
            return self.parse_function_def()
        return self.parse_stmt()

    def parse_squad(self):
        self.expect('SQUAD')
        name = self.expect('ID').val
        self.expect('LBRACE')
        fields = []
        methods = []
        while self.peek().type != 'RBRACE':
            if self.peek().type == 'INT':
                fields.append(self.parse_decl())
            elif self.peek().type == 'DFT':
                methods.append(self.parse_function_def(in_class=True))
            else:
                raise SyntaxError("Only int fields and dft methods allowed inside squad")
        self.expect('RBRACE')
        return ('squad', name, fields, methods)

    def parse_function_def(self, in_class=False):
        self.expect('DFT')
        fname = self.expect('ID').val
        self.expect('LPAREN')
        params = []
        if self.peek().type != 'RPAREN':
            while True:
                p = self.expect('ID').val
                params.append(p)
                if self.peek().type == 'COMMA':
                    self.next()
                    continue
                break
        self.expect('RPAREN')
        body = self.parse_block()
        return ('funcdef', fname, params, body, in_class)

    def parse_stmt(self):
        t = self.peek()

        if t.type == 'ID' and t.val in self.CMD_WORDS:
            return self.parse_command()

        if t.type == 'INT' or t.type == 'BOX':
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
            self.expect('LPAREN'); cond = self.parse_expr(); self.expect('RPAREN')
            thenb = self.parse_block()
            elseb = None
            if self.peek().type == 'ELSE':
                self.next()
                elseb = self.parse_block()
            return ('if', cond, thenb, elseb)

        if t.type == 'WHILE':
            self.next()
            self.expect('LPAREN'); cond = self.parse_expr(); self.expect('RPAREN')
            body = self.parse_block()
            return ('while', cond, body)

        if t.type == 'ASM':
            return self.parse_asm_block()

        if t.type == 'RETURN':
            self.next()
            if self.peek().type == 'SEMIC':
                self.next()
                return ('return', None)
            expr = self.parse_expr()
            self.expect('SEMIC')
            return ('return', expr)

        if t.type == 'ID' and self.peekn(1).type == 'ID' and self.peekn(2).type == 'SEMIC':
            class_name = self.next().val
            var_name = self.next().val
            self.expect('SEMIC')
            return ('declobj', class_name, var_name)

        if t.type == 'ID':
            left = self.parse_postfix()
            if self.peek().type == 'OP' and self.peek().val == '=':
                self.next()
                expr = self.parse_expr()
                self.expect('SEMIC')
                return ('assign_any', left, expr)
            self.expect('SEMIC')
            return ('evalstmt', left)

        if t.type == 'LBRACE':
            return self.parse_block()

        raise SyntaxError(f"Unexpected token {t}")

    def parse_decl(self):
        if self.peek().type == 'INT':
            self.expect('INT'); dtype = 'int'
        else:
            self.expect('BOX'); dtype = 'box'
        t = self.expect('ID')
        name = t.val
        val = None
        if self.peek().type == 'OP' and self.peek().val == '=':
            self.next()
            val = self.parse_expr()
        self.expect('SEMIC')
        return ('decl', name, val if dtype == 'box' else val)

    def parse_block(self):
        self.expect('LBRACE')
        stmts = []
        while self.peek().type != 'RBRACE':
            if self.peek().type == 'EOF':
                raise SyntaxError("Unclosed block")
            stmts.append(self.parse_stmt())
        self.expect('RBRACE')
        return ('block', stmts)

    def parse_asm_block(self):
        self.expect('ASM'); self.expect('LBRACE')
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
                if depth == 0: break
            asm_tokens.append(tok)
        pieces = []
        for tk in asm_tokens:
            pieces.append(str(tk.val)); pieces.append(' ')
        asm_text = ''.join(pieces).strip()
        return ('asm', asm_text)

    # --- выражения ---
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
        return self.parse_postfix()

    def parse_list_literal(self):
        self.expect('LBRACK')
        items = []
        if self.peek().type != 'RBRACK':
            while True:
                items.append(self.parse_expr())
                if self.peek().type == 'COMMA':
                    self.next(); continue
                break
        self.expect('RBRACK')
        return ('list', items)

    def parse_primary(self):
        t = self.peek()
        if t.type == 'NUMBER':
            self.next(); return ('number', t.val)
        if t.type == 'STRING':
            self.next(); return ('string', t.val)
        if t.type == 'ID':
            self.next(); return ('var', t.val)
        if t.type == 'LPAREN':
            self.next()
            node = self.parse_expr()
            self.expect('RPAREN')
            return node
        if t.type == 'LBRACK':
            return self.parse_list_literal()
        raise SyntaxError(f"Unexpected in expr: {t}")

    def parse_postfix(self):
        node = self.parse_primary()
        while True:
            if self.peek().type == 'LPAREN':
                self.next()
                args = []
                if self.peek().type != 'RPAREN':
                    while True:
                        args.append(self.parse_expr())
                        if self.peek().type == 'COMMA':
                            self.next(); continue
                        break
                self.expect('RPAREN')
                node = ('call', node, args)
                continue
            if self.peek().type == 'DOT':
                self.next()
                attr = self.expect('ID').val
                node = ('getattr', node, attr)
                continue
            if self.peek().type == 'LBRACK':
                self.next()
                idx = self.parse_expr()
                self.expect('RBRACK')
                node = ('index', node, idx)
                continue
            break
        return node

    def parse_command(self):
        name = self.expect('ID').val
        args = []
        while self.peek().type not in ('SEMIC','EOF'):
            tk = self.peek()
            if tk.type in ('NUMBER','ID','STRING'):
                args.append(self.next().val)
            else:
                self.next()
        if self.peek().type == 'SEMIC':
            self.next()
        return ('command', name, args)

# ---------------- Runtime / Interpreter ----------------
class ReturnSignal(Exception):
    def __init__(self, value):
        self.value = value

class Env:
    def __init__(self):
        self.vars = {}
        self.funcs = {}
        self.classes = {}

    def declare(self, name, val=0):
        self.vars[name] = val

    def set(self, name, val):
        self.vars[name] = val

    def get(self, name):
        if name in self.vars:
            return self.vars[name]
        if 'self' in self.vars:
            self_obj = self.vars['self']
            if isinstance(self_obj, dict) and '__class__' in self_obj and 'fields' in self_obj:
                if name in self_obj['fields']:
                    return self_obj['fields'][name]
        raise NameError(f"Undefined variable '{name}'")

def is_instance(obj):
    return isinstance(obj, dict) and '__class__' in obj and 'fields' in obj

def as_int(v):
    if isinstance(v, (int, float)):
        return int(v)
    raise RuntimeError("Expected integer index")

def eval_expr(node, env):
    typ = node[0]

    if typ == 'number':
        return node[1]
    if typ == 'string':
        return node[1]
    if typ == 'list':
        return [eval_expr(x, env) for x in node[1]]

    if typ == 'var':
        return env.get(node[1])

    if typ == 'getattr':
        obj = eval_expr(node[1], env)
        attr = node[2]
        if is_instance(obj):
            if attr not in obj['fields']:
                raise RuntimeError(f"Unknown field '{attr}' in object of class '{obj.get('__class__','?')}'")
            return obj['fields'][attr]
        return ('attr_on_value', obj, attr)  # отдаём вверх для вызова метода

    if typ == 'index':
        seq = eval_expr(node[1], env)
        idx = as_int(eval_expr(node[2], env))
        if not isinstance(seq, list):
            raise RuntimeError("Indexing works only on box (list)")
        if idx < 0 or idx >= len(seq):
            raise RuntimeError("Index out of range")
        return seq[idx]

    if typ == 'call':
        callee = node[1]
        args_nodes = node[2]

        if callee[0] == 'var':
            fname = callee[1]
            arg_vals = [eval_expr(a, env) for a in args_nodes]
            return call_function(fname, arg_vals, env, self_obj=None)

        if callee[0] == 'getattr':
            base = eval_expr(callee[1], env)
            attr = callee[2]
            argv = [eval_expr(a, env) for a in args_nodes]

            if is_instance(base):
                return call_method(base, attr, argv, env)

            # методы box (списки)
            if isinstance(base, list):
                if attr == 'add':
                    if len(argv) != 1: raise RuntimeError("box.add(x) needs 1 arg")
                    base.append(argv[0]); return 0
                if attr == 'pop':
                    if len(argv) != 0: raise RuntimeError("box.pop() no args")
                    if not base: return 0
                    return base.pop()
                if attr == 'len':
                    if len(argv) != 0: raise RuntimeError("box.len() no args")
                    return len(base)
                if attr == 'clear':
                    if len(argv) != 0: raise RuntimeError("box.clear() no args")
                    base.clear(); return 0
                if attr == 'sort':
                    if len(argv) != 0: raise RuntimeError("box.sort() no args")
                    try:
                        base.sort()
                    except Exception:
                        # попробуем как строки
                        base.sort(key=lambda x: str(x))
                    return 0
                raise RuntimeError(f"Unknown box method '{attr}'")
            # строковые методы? пока не надо
            raise RuntimeError("Method call on non-object/non-box")

        if callee[0] == 'attr_on_value':
            # теоретически не должно сюда дойти (перехватываем выше)
            raise RuntimeError("Invalid call target")

        raise RuntimeError("Unsupported function call target")

    if typ == 'unary':
        v = eval_expr(node[2], env)
        if not isinstance(v, (int, float)):
            raise RuntimeError("Unary operator on non-number")
        return -v

    if typ == 'binop':
        op, aN, bN = node[1], node[2], node[3]
        a = eval_expr(aN, env)
        b = eval_expr(bN, env)

        def num(x): return x if isinstance(x, (int, float)) else 0

        if op == '+': return num(a) + num(b)
        if op == '-': return num(a) - num(b)
        if op == '*': return num(a) * num(b)
        if op == '/': return num(a) / num(b) if b != 0 else 0
        if op == '%': return num(a) % num(b)
        if op == '==': return 1 if a == b else 0
        if op == '!=': return 1 if a != b else 0
        if op == '<':  return 1 if num(a) < num(b) else 0
        if op == '<=': return 1 if num(a) <= num(b) else 0
        if op == '>':  return 1 if num(a) > num(b) else 0
        if op == '>=': return 1 if num(a) >= num(b) else 0
        if op == '&&': return 1 if (a and b) else 0
        if op == '||': return 1 if (a or b) else 0

    raise RuntimeError(f"Unknown expr node {node}")

def call_function(fname, arg_vals, env, self_obj):
    if fname not in env.funcs:
        raise RuntimeError(f"Unknown function '{fname}'")
    params, body = env.funcs[fname]
    saved = {}
    created = []
    try:
        for p, v in zip(params, arg_vals):
            if p in env.vars:
                saved[p] = env.vars[p]
            else:
                created.append(p)
            env.vars[p] = v
        try:
            exec_stmt(body, env, lambda s: None)
        except ReturnSignal as rs:
            return rs.value if rs.value is not None else 0
        return 0
    finally:
        for p in created:
            env.vars.pop(p, None)
        for p,v in saved.items():
            env.vars[p] = v

def call_method(obj, mname, arg_vals, env):
    cls = obj['__class__']
    cdef = env.classes.get(cls)
    if not cdef or mname not in cdef['methods']:
        raise RuntimeError(f"Unknown method {cls}.{mname}")
    params, body = cdef['methods'][mname]
    saved = {}
    created = []
    try:
        if 'self' in env.vars:
            saved['self'] = env.vars['self']
        else:
            created.append('self')
        env.vars['self'] = obj
        for p, v in zip(params, arg_vals):
            if p in env.vars:
                saved[p] = env.vars[p]
            else:
                created.append(p)
            env.vars[p] = v
        try:
            exec_stmt(body, env, lambda s: None)
        except ReturnSignal as rs:
            return rs.value if rs.value is not None else 0
        return 0
    finally:
        for p in created:
            env.vars.pop(p, None)
        for p,v in saved.items():
            env.vars[p] = v

# ---------------- Assembly emulator ----------------
class AsmEmu:
    def __init__(self, env):
        self.regs = {f"r{i}":0 for i in range(8)}
        self.env = env
        self.zero = False
        self.labels = {}
        self.instructions = []
        self.ip = 0

    def parse_lines(self, text):
        raw_lines = []
        parts = re.split(r';|\n', text)
        for p in parts:
            line = p.strip()
            if not line: continue
            raw_lines.append(line)
        self.instructions = []
        for line in raw_lines:
            if line.endswith(':'):
                lbl = line[:-1].strip()
                self.labels[lbl] = len(self.instructions)
            else:
                self.instructions.append(line)

    def get_val(self, token):
        token = token.strip()
        if token in self.regs:
            return self.regs[token]
        if re.match(r'^[0-9]+$', token):
            return int(token)
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
            parts = [p.strip() for p in re.split(r'[ ,]+', line) if p.strip()!='']
            op = parts[0].upper()
            if op == 'MOV':
                dst, src = parts[1], parts[2]
                val = self.get_val(src)
                if dst in self.regs: self.set_reg(dst, val)
                else: self.env.set(dst, val)
                self.ip += 1
            elif op == 'ADD':
                dst, src = parts[1], parts[2]
                x = self.get_val(dst); y = self.get_val(src)
                if dst in self.regs: self.set_reg(dst, x+y)
                else: self.env.set(dst, x+y)
                self.ip += 1
            elif op == 'SUB':
                dst, src = parts[1], parts[2]
                x = self.get_val(dst); y = self.get_val(src)
                if dst in self.regs: self.set_reg(dst, x-y)
                else: self.env.set(dst, x-y)
                self.ip += 1
            elif op == 'INC':
                r = parts[1]; self.set_reg(r, self.get_val(r)+1); self.ip += 1
            elif op == 'DEC':
                r = parts[1]; self.set_reg(r, self.get_val(r)-1); self.ip += 1
            elif op == 'CMP':
                a = self.get_val(parts[1]); b = self.get_val(parts[2])
                self.zero = (a == b); self.ip += 1
            elif op == 'JMP':
                lbl = parts[1]
                if lbl not in self.labels: raise RuntimeError(f"Unknown label {lbl}")
                self.ip = self.labels[lbl]
            elif op == 'JE':
                lbl = parts[1]
                if self.zero:
                    if lbl not in self.labels: raise RuntimeError(f"Unknown label {lbl}")
                    self.ip = self.labels[lbl]
                else:
                    self.ip += 1
            elif op == 'JNE':
                lbl = parts[1]
                if not self.zero:
                    if lbl not in self.labels: raise RuntimeError(f"Unknown label {lbl}")
                    self.ip = self.labels[lbl]
                else:
                    self.ip += 1
            elif op == 'LOAD':
                reg, var = parts[1], parts[2]
                val = self.env.get(var)
                self.set_reg(reg, val); self.ip += 1
            elif op == 'STORE':
                var, reg = parts[1], parts[2]
                self.env.set(var, self.get_val(reg)); self.ip += 1
            else:
                raise RuntimeError(f"Unknown asm op: {op}")
            steps += 1
            if steps > 10000:
                raise RuntimeError("Asm: too many steps (possible infinite loop)")

# ---------------- Program execution ----------------
def run_program(src, output_callback):
    toks = lex(src)
    p = Parser(toks)
    tree = p.parse()
    env = Env()

    for node in tree[1]:
        if node[0] == 'squad':
            cname = node[1]
            fields_ast = node[2]
            methods_ast = node[3]
            fields = []
            for f in fields_ast:
                fields.append( (f[1], f[2]) )
            methods = {}
            for m in methods_ast:
                mname, params, body = m[1], m[2], m[3]
                methods[mname] = (params, body)
            env.classes[cname] = {'fields': fields, 'methods': methods}

        if node[0] == 'funcdef' and not node[4]:
            fname, params, body = node[1], node[2], node[3]
            env.funcs[fname] = (params, body)

    for node in tree[1]:
        if node[0] in ('squad','funcdef') and (node[0]=='squad' or node[4]):
            continue
        exec_stmt(node, env, output_callback)

def make_instance(class_name, env):
    cdef = env.classes.get(class_name)
    if not cdef:
        raise RuntimeError(f"Unknown class '{class_name}'")
    inst = {'__class__': class_name, 'fields': {}}
    for fname, default_ast in cdef['fields']:
        inst['fields'][fname] = eval_expr(default_ast, env) if default_ast is not None else 0
    return inst

def assign_to_lvalue(lv, value, env):
    if lv[0] == 'var':
        env.set(lv[1], value)
        return
    if lv[0] == 'getattr':
        obj = eval_expr(lv[1], env)
        field = lv[2]
        if not is_instance(obj):
            raise RuntimeError("Assign to attribute of non-object")
        obj['fields'][field] = value
        return
    if lv[0] == 'index':
        seq = eval_expr(lv[1], env)
        idx = as_int(eval_expr(lv[2], env))
        if not isinstance(seq, list):
            raise RuntimeError("Indexing assignment only for box (list)")
        if idx < 0 or idx >= len(seq):
            raise RuntimeError("Index out of range")
        seq[idx] = value
        return
    raise RuntimeError("Unsupported lvalue")

def exec_stmt(stmt, env, output_callback):
    typ = stmt[0]

    if typ == 'decl':
        name, val_node = stmt[1], stmt[2]
        v = eval_expr(val_node, env) if val_node is not None else 0
        env.declare(name, v)
        return

    if typ == 'declobj':
        class_name, var_name = stmt[1], stmt[2]
        inst = make_instance(class_name, env)
        env.declare(var_name, inst)
        return

    if typ == 'assign_any':
        lv, expr = stmt[1], stmt[2]
        v = eval_expr(expr, env)
        assign_to_lvalue(lv, v, env)
        return

    if typ == 'evalstmt':
        _ = eval_expr(stmt[1], env)
        return

    if typ == 'print':
        v = eval_expr(stmt[1], env)
        output_callback(str(v))
        return

    if typ == 'block':
        for s in stmt[1]:
            exec_stmt(s, env, output_callback)
        return

    if typ == 'if':
        cond = eval_expr(stmt[1], env)
        if cond: exec_stmt(stmt[2], env, output_callback)
        elif stmt[3] is not None: exec_stmt(stmt[3], env, output_callback)
        return

    if typ == 'while':
        while eval_expr(stmt[1], env):
            exec_stmt(stmt[2], env, output_callback)
        return

    if typ == 'return':
        if stmt[1] is None:
            raise ReturnSignal(0)
        raise ReturnSignal(eval_expr(stmt[1], env))

    if typ == 'asm':
        asm_text = stmt[1]
        emu = AsmEmu(env)
        emu.parse_lines(asm_text)
        emu.exec_all()
        return

    if typ == 'command':
        name, args = stmt[1], stmt[2]
        if name == 'window':
            w = args[0] if len(args)>0 else 800
            h = args[1] if len(args)>1 else 600
            title = args[2] if len(args)>2 else "CAsm3D"
            ENGINE.window(w, h, title)
            return
        if name == 'camera':
            if len(args) < 3: raise RuntimeError("camera needs 3 args")
            ENGINE.set_camera(args[0], args[1], args[2]); return
        if name == 'object':
            if len(args) < 7: raise RuntimeError("object needs 7 args")
            ENGINE.add_object(args[0], args[1], args[2], args[3], args[4], args[5], args[6]); return
        if name == 'move':
            if len(args) < 4: raise RuntimeError("move needs 4 args")
            ENGINE.move_object(args[0], args[1], args[2], args[3]); return
        if name == 'rotate':
            if len(args) < 4: raise RuntimeError("rotate needs 4 args: name rx ry rz")
            ENGINE.rotate_object(args[0], args[1], args[2], args[3]); return
        if name == 'render':
            ENGINE.render(); return
        raise RuntimeError(f"Unknown command {name}")

    if typ in ('squad','funcdef'):
        return

    raise RuntimeError(f"Unknown stmt {typ}")
