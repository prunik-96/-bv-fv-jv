
#   window <width> <height> <title_id>;
#     Пример: window 800 600 Demo;

import re
import math
import tkinter as tk

# ---------- Мини 3D-движок ----------
class Mini3DEngine:
    def __init__(self):
        self.objects = {}  # name -> dict
        self.camera = [0.0, 0.0, -5.0]  # позиция камеры в мире
        self.root = None
        self.canvas = None
        self.width = 800
        self.height = 600
        self.bg = "black"

    def window(self, w, h, title_id="CAsm3D"):
        self.width, self.height = int(w), int(h)
        if self.root is None:
            self.root = tk.Tk()
            self.root.title(str(title_id))
            self.canvas = tk.Canvas(self.root, width=self.width, height=self.height, bg=self.bg, highlightthickness=0)
            self.canvas.pack()
            self.root.update()
        else:
            # переинициализация размера/заголовка
            self.root.title(str(title_id))
            self.canvas.config(width=self.width, height=self.height)
            self.root.update()

    def set_camera(self, x, y, z):
        self.camera = [float(x), float(y), float(z)]

    def add_object(self, type_, name, x, y, z, size, color_id):
        self.objects[name] = {
            "type": str(type_),
            "x": float(x), "y": float(y), "z": float(z),
            "rx": 0.0, "ry": 0.0, "rz": 0.0,  # пока «логические» углы
            "size": float(size),
            "color": str(color_id)
        }

    def move_object(self, name, dx, dy, dz):
        if name in self.objects:
            self.objects[name]["x"] += float(dx)
            self.objects[name]["y"] += float(dy)
            self.objects[name]["z"] += float(dz)

    def rotate_object(self, name, rx, ry, rz):
        if name in self.objects:
            self.objects[name]["rx"] += float(rx)
            self.objects[name]["ry"] += float(ry)
            self.objects[name]["rz"] += float(rz)

    # примитивная перспективная проекция (без настоящего поворота по осям)
    def project(self, x, y, z):
        # сдвигаем относительно камеры: камера смотрит вдоль +Z (условно)
        # добавим смещение по Z, чтобы всё не делилось на 0
        rel_z = (z - self.camera[2])
        if rel_z == 0:
            rel_z = 1e-3
        f = 300.0 / rel_z
        sx = self.width * 0.5 + (x - self.camera[0]) * f
        sy = self.height * 0.5 - (y - self.camera[1]) * f
        return sx, sy, f

    def render(self):
        if self.root is None or self.canvas is None:
            # если окна ещё нет — насоздаём дефолтное
            self.window(self.width, self.height, "CAsm3D")

        self.canvas.delete("all")

        # простая отрисовка — сперва отсортируем по глубине, чтобы дальнее рисовалось раньше
        # глубина: (z - cam.z)
        def depth(obj):
            return obj["z"] - self.camera[2]

        for obj in sorted(self.objects.values(), key=depth, reverse=True):
            sx, sy, f = self.project(obj["x"], obj["y"], obj["z"])
            s = max(1.0, obj["size"] * f)  # «визуальный размер» зависит от расстояния (очень грубо)
            color = obj["color"]

            if obj["type"] == "cube":
                self.canvas.create_rectangle(sx - s, sy - s, sx + s, sy + s, outline=color, width=2)
                # лёгкая «псевдо-3D» грань
                self.canvas.create_line(sx - s, sy - s, sx - s*0.6, sy - s*1.4, fill=color)
                self.canvas.create_line(sx + s, sy - s, sx + s*0.6, sy - s*1.4, fill=color)
                self.canvas.create_line(sx - s*0.6, sy - s*1.4, sx + s*0.6, sy - s*1.4, fill=color)
            elif obj["type"] == "sphere":
                self.canvas.create_oval(sx - s, sy - s, sx + s, sy + s, outline=color, width=2)

        self.root.update()

ENGINE = Mini3DEngine()

# ---------- Lexer ----------
TOKEN_SPEC = [
    ('NUMBER',   r'\d+(\.\d+)?'),
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
            # Поддержка целых и дробных чисел
            if '.' in val:
                try:
                    tokens.append(Token('NUMBER', float(val)))
                except ValueError:
                    raise SyntaxError(f"Invalid float number '{val}'")
            else:
                try:
                    tokens.append(Token('NUMBER', int(val)))
                except ValueError:
                    raise SyntaxError(f"Invalid integer number '{val}'")

        elif kind == 'ID':
            if val in KEYWORDS:
                tokens.append(Token(val.upper(), val))
            else:
                tokens.append(Token('ID', val))

        elif kind in ('OP', 'SEMIC', 'COMMA', 'LBRACE', 'RBRACE', 'LPAREN', 'RPAREN', 'COLON'):
            tokens.append(Token(kind, val))

        elif kind in ('NEWLINE', 'SKIP'):
            pass

        elif kind == 'MISMATCH':
            raise SyntaxError(f"Unexpected char {val}")

    tokens.append(Token('EOF', ''))
    return tokens
 

# ---------- Parser ----------
class Parser:
    CMD_WORDS = {'window','camera','object','move','rotate','render'}

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

        # декларации / print / if / while / asm
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

        # наши 3D-команды — это ID, не входящие в KEYWORDS
        if t.type == 'ID' and t.val in self.CMD_WORDS:
            return self.parse_command()

        # присваивание
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
                raise SyntaxError("Only assignments or commands allowed when starting with identifier")

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
        pieces = []
        for tk in asm_tokens:
            pieces.append(tk.val)
            pieces.append(' ')
        asm_text = ''.join(pieces).strip()
        return ('asm', asm_text)

    # Простая команда: собираем аргументы до ';'
    def parse_command(self):
        name = self.expect('ID').val  # window|camera|object|move|rotate|render
        args = []
        while self.peek().type not in ('SEMIC', 'EOF'):
            tk = self.peek()
            if tk.type in ('NUMBER', 'ID'):
                args.append(self.next().val)
            else:
                # пропускаем разделители/прочее
                self.next()
        if self.peek().type == 'SEMIC':
            self.next()
        return ('command', name, args)

    # Expression parser (минимальная версия)
    def parse_expr(self):
        # из исходника — полная иерархия приоритетов; оставим как было
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
    raise RuntimeError(f"Unknown expr node {node}")

# assembly emulator
class AsmEmu:
    def __init__(self, env):
        self.regs = {f"r{i}":0 for i in range(8)}
        self.env = env
        self.zero = False
        self.labels = {}
        self.instructions = []  # list of lines
        self.ip = 0

    def parse_lines(self, text):
        raw_lines = []
        parts = re.split(r';|\n', text)
        for p in parts:
            line = p.strip()
            if not line:
                continue
            raw_lines.append(line)
        self.instructions = []
        for idx, line in enumerate(raw_lines):
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
                dst = parts[1]; src = parts[2]
                val = self.get_val(src)
                if dst in self.regs:
                    self.set_reg(dst, val)
                else:
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

    elif typ == 'command':
        name, args = stmt[1], stmt[2]

        if name == 'window':
            # window <w> <h> <title_id>;
            w = int(args[0]) if len(args) > 0 else 800
            h = int(args[1]) if len(args) > 1 else 600
            title = args[2] if len(args) > 2 else "CAsm3D"
            ENGINE.window(w, h, title)

        elif name == 'camera':
            # camera <x> <y> <z>;
            if len(args) < 3: raise RuntimeError("camera needs 3 args")
            ENGINE.set_camera(args[0], args[1], args[2])

        elif name == 'object':
            # object <type> <name> <x> <y> <z> <size> <color_id>;
            if len(args) < 7: raise RuntimeError("object needs 7 args")
            ENGINE.add_object(args[0], args[1], args[2], args[3], args[4], args[5], args[6])

        elif name == 'move':
            # move <name> <dx> <dy> <dz>;
            if len(args) < 4: raise RuntimeError("move needs 4 args")
            ENGINE.move_object(args[0], args[1], args[2], args[3])

        elif name == 'rotate':
            # rotate <name> <rx> <ry> <rz>;
            if len(args) < 3: raise RuntimeError("rotate needs 3 args")
            obj_name = args[0]
            rx = args[1] if len(args) > 1 else 0
            ry = args[2] if len(args) > 2 else 0
            rz = args[3] if len(args) > 3 else 0
            ENGINE.rotate_object(obj_name, rx, ry, rz)

        elif name == 'render':
            ENGINE.render()

        else:
            raise RuntimeError(f"Unknown command {name}")

    else:
        raise RuntimeError(f"Unknown stmt {typ}")
