import codecs
import locale


FAILSAFE_ENCODING = "cp1252"


def detect_encoding(buf, default=None):
    if buf.startswith(codecs.BOM_UTF8):
        encoding = "utf-8-sig"
    elif (buf.startswith(codecs.BOM_UTF16_LE) or
          buf.startswith(codecs.BOM_UTF16_BE)):
        encoding = "utf-16"
    elif (buf.startswith(codecs.BOM_UTF32_LE) or
          buf.startswith(codecs.BOM_UTF32_BE)):
        encoding = "utf-32"
    else:
        encoding = (default or locale.getpreferredencoding(False) or
                    FAILSAFE_ENCODING)
    return encoding
