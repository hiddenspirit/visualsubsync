import difflib
import re

DEBUG = False
UPDATE_ISJUNK = lambda x: x == " "
UPDATE_AUTOJUNK = False
UNSTRIP_ISJUNK = None
UNSTRIP_AUTOJUNK = False


if DEBUG:
    def print_opcodes(name, opcodes, a, b):
        seq = []
        for tag, i1, i2, j1, j2 in opcodes:
            t = tag[0]
            if t == "e":
                continue
            seq.append((t, i1, i2, j1, j2) +
                       ("".join(a[i1:i2]), "".join(b[j1:j2])))
        print(name, "=", repr(seq))
else:
    def dummy_print(*args, **kwargs):
        pass
    print_opcodes = dummy_print


def update_text(text, stripped_text, new_stripped_text):
    if text == stripped_text:
        return new_stripped_text
    elif stripped_text == new_stripped_text:
        return text

    update_opcodes = difflib.SequenceMatcher(
        UPDATE_ISJUNK, stripped_text,
        new_stripped_text, UPDATE_AUTOJUNK).get_opcodes()
    print_opcodes("update_opcodes",
                  update_opcodes, stripped_text, new_stripped_text)

    unstrip_opcodes = difflib.SequenceMatcher(
        UNSTRIP_ISJUNK, stripped_text,
        text, UNSTRIP_AUTOJUNK).get_opcodes()
    print_opcodes("unstrip_opcodes", unstrip_opcodes, stripped_text, text)

    # Decompose update_opcodes for new_opcodes.
    new_opcodes = decompose_opcodes(update_opcodes)
    print_opcodes("new_opcodes", new_opcodes, stripped_text, new_stripped_text)

    original_indexes = [e[2] for e in new_opcodes]

    # Displace new_opcodes according to unstrip_opcodes.
    for tag, i1, i2, j1, j2 in unstrip_opcodes:
        t = tag[0]
        if t == "e":
            pass
        elif t in "ird":
            p = len(new_opcodes) - 1
            while p >= 0 and original_indexes[p] > i2:
                d = (j2 - j1) - (i2 - i1)
                new_opcodes[p][1] += d
                new_opcodes[p][2] += d
                p -= 1
        else:
            raise ValueError(
                "unexpected tag in unstrip_opcode: {!r}".format(tag))
    print_opcodes("displaced new_opcodes",
                  new_opcodes, text, new_stripped_text)

    # Apply new_opcodes to text.
    d = 0
    text = list(text)
    for t, i1, i2, j1, j2 in new_opcodes:
        text[i1+d:i2+d] = new_stripped_text[j1:j2]
        d += (j2 - j1) - (i2 - i1)

    text = "".join(text)

    # Postprocessing
    text = re.sub(r"(\w+)(<\w+?>)(\w+)", r"\2\1\3", text)
    text = re.sub(r"(\w+)(</\w+?>)(\w+)", r"\1\3\2", text)

    return text


def decompose_opcodes(opcodes):
    """Decompose opcodes.
    """
    decomposed = []
    for tag, i1, i2, j1, j2 in opcodes:
        t = tag[0]
        if t == "e":
            pass
        elif t == "r":
            len1 = i2 - i1
            len2 = j2 - j1

            if len1 < len2:
                while i1 < i2:
                    decomposed.append(["replace", i1, i1 + 1, j1, j1 + 1])
                    i1 += 1
                    j1 += 1
                while j1 < j2:
                    decomposed.append(["insert", i1, i1, j1, j1 + 1])
                    j1 += 1
            else:
                while j1 < j2:
                    decomposed.append(["replace", i1, i1 + 1, j1, j1 + 1])
                    i1 += 1
                    j1 += 1
                while i1 < i2:
                    decomposed.append(["delete", i1, i1 + 1, j1, j1])
                    i1 += 1
        elif t == "d":
            while i1 < i2:
                decomposed.append(["delete", i1, i1 + 1, j1, j1])
                i1 += 1
        elif t == "i":
            while j1 < j2:
                decomposed.append(["insert", i1, i1, j1, j1 + 1])
                j1 += 1
        else:
            raise ValueError("unexpected tag in opcode: {!r}".format(tag))
    return decomposed

