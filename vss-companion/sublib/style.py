# TODO: full ASS v4.00+ style support


class Style:
    DEFAULT_FONT_NAME = "Verdana"
    DEFAULT_FONT_SIZE = 18
    DEFAULT_FONT_BOLD = True
    STYLE_FORMAT = ("Style: {name},{font_name},{font_size},"
                    "&H00FFFFFF,&H0000FFFF,&H00000000,&H80000000,"
                    "{font_bold},0,0,0,100,100,0,0,1,1,1,2,15,15,15,0")

    def __init__(self, name="Default",
                 font_name=DEFAULT_FONT_NAME,
                 font_size=DEFAULT_FONT_SIZE,
                 font_bold=DEFAULT_FONT_BOLD,
                ):
        self.name = name
        self.font_name = font_name
        self.font_size = font_size
        self.font_bold = font_bold

        self.primary_color = 0x00ffffff
        self.secondary_color = 0x00000000
        self.outline_color = 0x00000000
        self.back_color = 0x80000000
        self.font_italic = False
        self.font_underline = False
        self.font_strikeout = False
        self.scale_x = 1.0
        self.scale_y = 1.0
        self.spacing = 0
        self.angle = 0
        self.bolder_style = 1
        self.outline = 1
        self.shadow = 1
        self.alignment = 2
        self.margin_lelft = 15
        self.margin_right = 15
        self.margin_vertical = 15

    @classmethod
    def get_color_str(cls, color):
        d, r = divmod(color, 0x100)
        d, g = divmod(d, 0x100)
        a, b = divmod(d, 0x100)
        return "&H{:02X}{:02X}{:02X}{:02X}&".format(a, b, g, r)

    def __repr__(self):
        return "{}({!r}, {!r}, {!r}, {!r})".format(self.__class__.__name__,
             self.name, self.font_name, self.font_size, self.font_bold)

    def __str__(self):
        return self.STYLE_FORMAT.format(name=self.name,
            font_name=self.font_name, font_size=self.font_size,
            font_bold=(-1 if self.font_bold else 0))
