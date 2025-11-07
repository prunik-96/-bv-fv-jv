 import tkinter as tk
from tkinter import scrolledtext, filedialog, messagebox
from casm_lang import run_program, ENGINE
import threading, re

class CAsmIDE:
    def __init__(self, root):
        self.root = root
        self.root.title("CAsm IDE — Warm Theme")
        self.root.geometry("1000x700")

        self.run_thread = None
        self.stop_flag = False

        self._build_ui()
        self._insert_example()
        self._add_highlighting()

    def _build_ui(self):
        top = tk.Frame(self.root)
        top.pack(fill=tk.BOTH, expand=True)

        self.editor = scrolledtext.ScrolledText(top, font=("Consolas", 13), wrap=tk.NONE,
                                                bg="#111", fg="#ffd9b3", insertbackground="#ff784e")
        self.editor.pack(fill=tk.BOTH, expand=True, side=tk.LEFT)

        right = tk.Frame(top, width=320, bg="#1a1a1a")
        right.pack(fill=tk.Y, side=tk.RIGHT, padx=6, pady=6)

        tk.Button(right, text="▶ Run", command=self.run_code,
                  width=12, height=2, bg="#e2544b", fg="white", relief=tk.FLAT).pack(pady=6)
        tk.Button(right, text="■ Stop", command=self.stop_code,
                  width=12, height=2, bg="#ff6666", fg="white", relief=tk.FLAT).pack(pady=4)

        tk.Button(right, text="💾 Save", command=self.save_file, width=12,
                  bg="#ffb457", fg="#111", relief=tk.FLAT).pack(pady=4)
        tk.Button(right, text="📂 Load", command=self.load_file, width=12,
                  bg="#ffb457", fg="#111", relief=tk.FLAT).pack(pady=4)
        tk.Button(right, text="Clear Output", command=self.clear_output, width=12,
                  bg="#e2544b", fg="white", relief=tk.FLAT).pack(pady=8)

        self.output = scrolledtext.ScrolledText(self.root, height=10, bg="#0b0a0a",
                                                fg="#ffa8c3", font=("Consolas", 12))
        self.output.pack(fill=tk.X, padx=6, pady=6)
        self.output.config(state=tk.DISABLED)

    def _insert_example(self):
        example = """// Warm Theme Demo
window 800 600 Demo;
camera 0 0 -8;
object cube c1 0 0 3 0.5 red;
object cube c2 2 0 6 0.7 orange;

while (1) {
  rotate c1 1 2 3;
  rotate c2 2 1 0;
  render;
}
"""
        self.editor.insert(tk.END, example)

    def run_code(self):
        if self.run_thread and self.run_thread.is_alive():
            return
        self.stop_flag = False
        self.output.config(state=tk.NORMAL)
        self.output.delete(1.0, tk.END)
        self.output.config(state=tk.DISABLED)
        code = self.editor.get(1.0, tk.END)
        ENGINE.reset_scene()

        def target():
            try:
                run_program(code, lambda t: self.output_insert(t))
            except Exception as e:
                self.output_insert(f"Error: {e}")

        self.run_thread = threading.Thread(target=target, daemon=True)
        self.run_thread.start()

    def stop_code(self):
        self.stop_flag = True
        ENGINE._on_close()
        self.output_insert("[Program stopped]\n")

    def output_insert(self, text):
        self.output.config(state=tk.NORMAL)
        self.output.insert(tk.END, text + "\n")
        self.output.config(state=tk.DISABLED)
        self.output.see(tk.END)

    def save_file(self):
        p = filedialog.asksaveasfilename(defaultextension=".casm",
                                         filetypes=[("CAsm files", "*.casm")])
        if not p:
            return
        open(p, "w", encoding="utf-8").write(self.editor.get(1.0, tk.END))
        messagebox.showinfo("Saved", f"Saved to {p}")

    def load_file(self):
        p = filedialog.askopenfilename(filetypes=[("CAsm files", "*.casm")])
        if not p:
            return
        self.editor.delete(1.0, tk.END)
        self.editor.insert(tk.END, open(p, "r", encoding="utf-8").read())

    def clear_output(self):
        self.output.config(state=tk.NORMAL)
        self.output.delete(1.0, tk.END)
        self.output.config(state=tk.DISABLED)

    def _add_highlighting(self):
        self.colors = {
            "keyword": "#90dcff", "command": "#e7e476", "number": "#98ffdb",
            "string": "#ff9fed", "comment": "#aaffc9", "asm": "#7390b8"
        }
        for tag, col in self.colors.items():
            self.editor.tag_configure(tag, foreground=col)
        self.editor.bind("<KeyRelease>", self._highlight)
        self._highlight()

    def _highlight(self, event=None):
        text = self.editor.get("1.0", "end-1c")
        for tag in self.colors:
            self.editor.tag_remove(tag, "1.0", "end")
        rules = {
            "keyword": r"\b(int|if|else|while|asm|return)\b",
            "command": r"\b(window|camera|object|move|rotate|render)\b",
            "number": r"\b\d+(\.\d+)?\b",
            "string": r'"[^"\n]*"',
            "comment": r"//[^\n]*",
            "asm": r"\b(MOV|ADD|SUB|INC|DEC|CMP|JMP|JE|JNE|LOAD|STORE)\b"
        }
        for tag, pat in rules.items():
            for m in re.finditer(pat, text, flags=re.IGNORECASE):
                self.editor.tag_add(tag, f"1.0+{m.start()}c", f"1.0+{m.end()}c")

if __name__ == "__main__":
    root = tk.Tk()
    CAsmIDE(root)
    root.mainloop()
