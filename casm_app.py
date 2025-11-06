# casm_app.py
import tkinter as tk
from tkinter import scrolledtext, filedialog, messagebox
from casm_lang import run_program

class CAsmIDE:
    def __init__(self, root):
        self.root = root
        self.root.title("CAsm IDE — C-like + Assembly (English)")
        self.root.geometry("1000x700")

        self._build_ui()
        self._insert_example()

    def _build_ui(self):
        top = tk.Frame(self.root)
        top.pack(fill=tk.BOTH, expand=True)

        self.editor = scrolledtext.ScrolledText(top, font=("Consolas", 13), wrap=tk.NONE)
        self.editor.pack(fill=tk.BOTH, expand=True, side=tk.LEFT)

        right = tk.Frame(top, width=320)
        right.pack(fill=tk.Y, side=tk.RIGHT, padx=6, pady=6)

        run_btn = tk.Button(right, text="▶ Run", command=self.run_code, width=12, height=2, bg="#4CAF50", fg="white")
        run_btn.pack(pady=6)

        save_btn = tk.Button(right, text="💾 Save", command=self.save_file, width=12)
        save_btn.pack(pady=4)
        load_btn = tk.Button(right, text="📂 Load", command=self.load_file, width=12)
        load_btn.pack(pady=4)

        clear_btn = tk.Button(right, text="Clear Output", command=self.clear_output, width=12)
        clear_btn.pack(pady=8)

        self.output = scrolledtext.ScrolledText(self.root, height=10, bg="#111", fg="#0f0", font=("Consolas", 12))
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
