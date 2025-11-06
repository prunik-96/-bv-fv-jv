# casm_app.py
import tkinter as tk
from tkinter import scrolledtext, filedialog, messagebox
from casm_lang import run_program
import re

class CAsmIDE:
    def __init__(self, root):
        self.root = root
        self.root.title("CAsm IDE — C-like + Assembly (English)")
        self.root.geometry("1000x700")

        self._build_ui()
        self._insert_example()
        self._add_highlighting()  # добавили подсветку

    def _build_ui(self):
        top = tk.Frame(self.root)
        top.pack(fill=tk.BOTH, expand=True)

        self.editor = scrolledtext.ScrolledText(top, font=("Consolas", 13), wrap=tk.NONE, bg="#111", fg="#ffd9b3", insertbackground="#ff784e")
        self.editor.pack(fill=tk.BOTH, expand=True, side=tk.LEFT)

        right = tk.Frame(top, width=320, bg="#1a1a1a")
        right.pack(fill=tk.Y, side=tk.RIGHT, padx=6, pady=6)

        run_btn = tk.Button(right, text="▶ Run", command=self.run_code, width=12, height=2, bg="#e2544b", fg="white", relief=tk.FLAT)
        run_btn.pack(pady=6)

        save_btn = tk.Button(right, text="💾 Save", command=self.save_file, width=12, bg="#ffb457", fg="#111", relief=tk.FLAT)
        save_btn.pack(pady=4)
        load_btn = tk.Button(right, text="📂 Load", command=self.load_file, width=12, bg="#ffb457", fg="#111", relief=tk.FLAT)
        load_btn.pack(pady=4)

        clear_btn = tk.Button(right, text="Clear Output", command=self.clear_output, width=12, bg="#e2544b", fg="white", relief=tk.FLAT)
        clear_btn.pack(pady=8)

        self.output = scrolledtext.ScrolledText(self.root, height=10, bg="#0b0a0a", fg="#ffa8c3", font=("Consolas", 12))
        self.output.pack(fill=tk.X, padx=6, pady=6)
        self.output.config(state=tk.DISABLED)

    def _insert_example(self):
        example = """// Example: mix of C-like and inline assembly
int x = 5;
int y = 10;
int z = 0;

print(x);
print(y);

asm {
  // load variables into registers, add, store back
  LOAD r0, x;
  LOAD r1, y;
  ADD r0, r1;
  STORE z, r0;
}

print(z);

// loop example in C-style
int i = 0;
while (i < 5) {
  print(i);
  i = i + 1;
}

// assembly with jump and label
asm {
  MOV r0, 0;
start:
  INC r0;
  CMP r0, 3;
  JE end;
  JMP start;
end:
  STORE x, r0;
}
print(x);
"""
        self.editor.delete(1.0, tk.END)
        self.editor.insert(tk.END, example)

    # 🔥 --- Подсветка синтаксиса ---
    def _add_highlighting(self):
        self.colors = {
            "keyword": "#ff4f87",   # розовый
            "command": "#ff6f61",   # коралловый
            "number": "#ffb84c",    # тёплый жёлтый
            "string": "#ff7a45",    # оранжевый
            "comment": "#c77dff",   # фиолетовый
            "asm": "#ff3c38"        # красный
        }

        for tag, color in self.colors.items():
            self.editor.tag_configure(tag, foreground=color)

        self.editor.bind("<KeyRelease>", self._highlight_code)
        self.editor.bind("<ButtonRelease>", self._highlight_code)
        self._highlight_code()

    def _highlight_code(self, event=None):
        text = self.editor.get("1.0", "end-1c")
        for tag in self.colors:
            self.editor.tag_remove(tag, "1.0", "end")

        rules = {
            "keyword": r"\b(int|if|else|while|asm|return|print)\b",
            "command": r"\b(window|camera|object|move|rotate|render)\b",
            "number": r"\b\d+(\.\d+)?\b",
            "string": r'"[^"\n]*"',
            "comment": r"//[^\n]*",
            "asm": r"\b(MOV|ADD|SUB|INC|DEC|CMP|JMP|JE|JNE|LOAD|STORE)\b",
        }

        for tag, pattern in rules.items():
            for match in re.finditer(pattern, text, flags=re.IGNORECASE):
                start = f"1.0+{match.start()}c"
                end = f"1.0+{match.end()}c"
                self.editor.tag_add(tag, start, end)

    # --- остальное без изменений ---
    def run_code(self):
        src = self.editor.get(1.0, tk.END)
        self.output.config(state=tk.NORMAL)
        self.output.delete(1.0, tk.END)
        def out_cb(text):
            self.output.insert(tk.END, text + "\n")
        try:
            run_program(src, out_cb)
        except Exception as e:
            self.output.insert(tk.END, f"Error: {e}\n")
        self.output.config(state=tk.DISABLED)

    def save_file(self):
        path = filedialog.asksaveasfilename(defaultextension=".casm", filetypes=[("CAsm files","*.casm"),("All files","*.*")])
        if not path:
            return
        with open(path, "w", encoding="utf-8") as f:
            f.write(self.editor.get(1.0, tk.END))
        messagebox.showinfo("Saved", f"Saved to {path}")

    def load_file(self):
        path = filedialog.askopenfilename(filetypes=[("CAsm files","*.casm"),("All files","*.*")])
        if not path:
            return
        with open(path, "r", encoding="utf-8") as f:
            txt = f.read()
        self.editor.delete(1.0, tk.END)
        self.editor.insert(tk.END, txt)
        messagebox.showinfo("Loaded", f"Loaded {path}")

    def clear_output(self):
        self.output.config(state=tk.NORMAL)
        self.output.delete(1.0, tk.END)
        self.output.config(state=tk.DISABLED)

if __name__ == "__main__":
    root = tk.Tk()
    app = CAsmIDE(root)
    root.mainloop()
