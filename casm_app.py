class Mini3DEngine:
    def __init__(self):
        self.objects = {}
        self.camera = [0.0, 0.0, -5.0]
        self.root = None
        self.canvas = None
        self.width = 800
        self.height = 600
        self.bg = "black"

    def is_alive(self):
        try:
            return self.root is not None and bool(self.root.winfo_exists())
        except Exception:
            return False

    def _on_close(self):
        try:
            if self.root is not None:
                root = self.root
                self.root = None
                self.canvas = None
                try:
                    root.after(1, root.destroy)
                except Exception:
                    pass
        except Exception:
            self.root = None
            self.canvas = None

    def reset_scene(self):
        self.objects = {}
        if not self.is_alive():
            self.root = None
            self.canvas = None

    def window(self, w, h, title_id="CAsm3D"):
        self.width, self.height = int(w), int(h)
        if not self.is_alive():
            self.root = tk.Tk()
            self.root.title(str(title_id))
            self.root.protocol("WM_DELETE_WINDOW", self._on_close)
            self.canvas = tk.Canvas(self.root, width=self.width, height=self.height, bg=self.bg, highlightthickness=0)
            self.canvas.pack()
            self.root.update_idletasks()
            self.root.update()
        else:
            self.root.title(str(title_id))
            self.canvas.config(width=self.width, height=self.height)
            self.root.update_idletasks()
            self.root.update()

    def set_camera(self, x, y, z):
        self.camera = [float(x), float(y), float(z)]

    def add_object(self, type_, name, x, y, z, size, color_id):
        self.objects[name] = {
            "type": str(type_),
            "x": float(x), "y": float(y), "z": float(z),
            "rx": 0.0, "ry": 0.0, "rz": 0.0,
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

    def project(self, x, y, z):
        rel_z = (z - self.camera[2])
        if rel_z == 0:
            rel_z = 1e-3
        f = 300.0 / rel_z
        sx = self.width * 0.5 + (x - self.camera[0]) * f
        sy = self.height * 0.5 - (y - self.camera[1]) * f
        return sx, sy, f

    def render(self):
        if not self.is_alive():
            return
        try:
            self.canvas.delete("all")

            def depth(o):
                return o["z"] - self.camera[2]

            for o in sorted(self.objects.values(), key=depth, reverse=True):
                sx, sy, f = self.project(o["x"], o["y"], o["z"])
                s = max(1.0, o["size"] * f)
                c = o["color"]

                if o["type"] == "cube":
                    self.canvas.create_rectangle(sx - s, sy - s, sx + s, sy + s, outline=c, width=2)
                    self.canvas.create_line(sx - s, sy - s, sx - s * 0.6, sy - s * 1.4, fill=c)
                    self.canvas.create_line(sx + s, sy - s, sx + s * 0.6, sy - s * 1.4, fill=c)
                    self.canvas.create_line(sx - s * 0.6, sy - s * 1.4, sx + s * 0.6, sy - s * 1.4, fill=c)
                elif o["type"] == "sphere":
                    self.canvas.create_oval(sx - s, sy - s, sx + s, sy + s, outline=c, width=2)

            self.root.update_idletasks()
            self.root.update()
        except tk.TclError:
            self._on_close()
