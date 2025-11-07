import tkinter as tk
from tkinter import scrolledtext, filedialog, messagebox
from casm_lang import run_program
import re

class CAsmIDE:
    def __init__(self, root):
        self.root = root
        self.root.title("CAsm IDE — Warm Theme")
        self.root.geometry("1000x700")
        self.running = False

        self._build_ui()
        self._insert_example()
        self._add_highlighting()

    # ---------- UI ----------
    def _build_ui(self):
        self.root.configure(bg="#1b1a1a")
        top = tk.Frame(self.root, bg="#1b1a1a")
        top.pack(fill=tk.BOTH, expand=True)

        # редактор
        self.editor = scrolledtext.ScrolledText(
            top, font=("Consolas", 13), wrap=tk.NONE,
            bg="#111", fg="#F6FFC3", insertbackground="#fffc61"
        )
        self.editor.pack(fill=tk.BOTH, expand=True, side=tk.LEFT)

        # правая панель
        right = tk.Frame(top, bg="#1b1a1a", width=280)
        right.pack(fill=tk.Y, side=tk.RIGHT, padx=10, pady=10)

        tk.Button(right, text="▶ Run", command=self.run_code,
                  bg="#c6ff71", fg="white", relief="flat", width=12, height=2).pack(pady=5)

        tk.Button(right, text="⏹ Stop", command=self.stop_code,
                  bg="#ff4f4f", fg="white", relief="flat", width=12, height=2).pack(pady=5)

        tk.Button(right, text="💾 Save", command=self.save_file,
                  bg="#ececec", fg="black", relief="flat", width=12).pack(pady=4)

        tk.Button(right, text="📂 Load", command=self.load_file,
                  bg="#ececec", fg="black", relief="flat", width=12).pack(pady=4)

        tk.Button(right, text="🧹 Clear Output", command=self.clear_output,
                  bg="#ececec", fg="black", relief="flat", width=12).pack(pady=8)

        self.output = scrolledtext.ScrolledText(
            self.root, height=10, bg="#0f0f0f", fg="#ffa8c3", font=("Consolas", 12)
        )
        self.output.pack(fill=tk.X, padx=8, pady=8)
        self.output.config(state=tk.DISABLED)

    # ---------- пример ----------
    def _insert_example(self):
        example = """// Example program
int x = 5;
int y = 10;
asm {
  LOAD r0, x;
  LOAD r1, y;
  ADD r0, r1;
  STORE x, r0;
}
print(x);
"""
        self.editor.delete(1.0, tk.END)
        self.editor.insert(tk.END, example)

    # ---------- подсветка ----------
    def _add_highlighting(self):
        self.colors = {
            "keyword": "#90dcff",
            "command": "#e7e476",
            "number": "#98ffdb",
            "string": "#ff9fed",
            "comment": "#aaffc9",
            "asm": "#7390b8"
        }
        for tag, color in self.colors.items():
            self.editor.tag_configure(tag, foreground=color)

        self.editor.bind("<KeyRelease>", self._highlight_code)
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
                self.editor.tag_add(tag, f"1.0+{match.start()}c", f"1.0+{match.end()}c")

    # ---------- выполнение ----------
    def run_code(self):
        if self.running:
            messagebox.showinfo("CAsm IDE", "Program is already running.")
            return
        self.running = True
        src = self.editor.get(1.0, tk.END)
        self.output.config(state=tk.NORMAL)
        self.output.delete(1.0, tk.END)

        def out_cb(text):
            if not self.running:
                raise RuntimeError("Stopped")
            self.output.insert(tk.END, text + "\n")

        try:
            run_program(src, out_cb)
        except Exception as e:
            self.output.insert(tk.END, f"Error: {e}\n")

        self.output.config(state=tk.DISABLED)
        self.running = False

    def stop_code(self):
        self.running = False
        self.output.config(state=tk.NORMAL)
        self.output.insert(tk.END, "[Execution stopped]\n")
        self.output.config(state=tk.DISABLED)

    # ---------- файлы ----------
    def save_file(self):
        from tkinter import filedialog
        path = filedialog.asksaveasfilename(defaultextension=".casm", filetypes=[("CAsm files", "*.casm"), ("All files", "*.*")])
        if not path:
            return
        with open(path, "w", encoding="utf-8") as f:
            f.write(self.editor.get(1.0, tk.END))
        messagebox.showinfo("Saved", f"Saved to {path}")

    def load_file(self):
        from tkinter import filedialog
        path = filedialog.askopenfilename(filetypes=[("CAsm files", "*.casm"), ("All files", "*.*")])
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
