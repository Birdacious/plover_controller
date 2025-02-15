import re
from dataclasses import dataclass
from .util import get_keys_for_stroke


@dataclass
class Stick:
    name: str
    x_axis: str
    y_axis: str
    offset: float
    segments: list[str]


@dataclass
class Trigger:
    name: str
    axis: str
    segments: list[str]


@dataclass
class Alias:
    renamed: str
    actual: str


@dataclass
class Mappings:
    sticks: dict[str, Stick]
    triggers: dict[str, Trigger]
    buttons: dict[str, Alias]
    hats: dict[str, Alias]
    unordered_mappings: list[tuple[list[str], tuple[str, ...]]] # RM
    ordered_mappings: dict[tuple[str, ...], tuple[str, ...]] # RM
    mappings: dict[tuple[tuple[str, ...], ...], tuple[str, ...]]
    #                ^     ^                      `-steno phonemes
    # n items in combo     n directions in stick motion, or could just be 1 button

    @classmethod
    def empty(cls) -> "Mappings":
        return Mappings(
            sticks={},
            hats={},
            buttons={},
            triggers={},
            ordered_mappings={}, # RM
            unordered_mappings=[], # RM
            mappings={}
        )

    @classmethod
    def parse(cls, text: str) -> "Mappings":
        m = Mappings.empty()
        for line in text.splitlines():
            if not line or line.startswith("//"): continue
            if match := re.match( r"(\w+) stick has segments \(([a-z,]+)\) on axes (\d+) and (\d+) offset by ([0-9-.]+) degrees", line):
                stick = Stick(
                    name=match[1],
                    x_axis=f"a{match[3]}",
                    y_axis=f"a{match[4]}",
                    offset=float(match[5]),
                    segments=match[2].split(","),
                )
                m.sticks[stick.name] = stick

            # Best explained by example:
            #   lstick(dl,l,ul) + button_a + rtrig(l) -> SKR-
            #   would get added to mapping like so:
            #   m.mappings[(('lstickdl','lstickl','lstickul'), ('button_a'), ('rtrigl'))] = ('S-','K-','R-')
            elif match := re.match(r"(\w+(?:\([a-z,]+\))?(?: *\+ *\w+(?:\([a-z,]+\))?)*) -> ([A-Z-*#]+)", line): # disgusting *barfs*
                inputs = [thing.strip() for thing in match[1].split('+')]
                result = []
                for input in inputs:
                    match2 = re.match(r"(\w+)\(([a-z,]+)\)", input)
                    if match2:
                          stick_or_trigger, positions = match2.groups()
                          result.append(tuple(f"{stick_or_trigger}{pos}" for pos in positions.split(",")))
                    else: result.append(input) # Button
                m.mappings[tuple(result)] = get_keys_for_stroke(match[2])

            elif match := re.match(r"([a-z0-9]+) -> ([A-Z-*#]+)", line): # RM
                lhs = match[1] # RM
                rhs = get_keys_for_stroke(match[2]) # RM
                m.unordered_mappings.append((lhs, rhs)) # RM
            elif match := re.match(r"(\w+)\(([a-z,]+)\) -> ([A-Z-*#]+)", line): # RM
                m.ordered_mappings[ tuple(f"{match[1]}{pos}" for pos in match[2].split(",")) ] = get_keys_for_stroke(match[3]) # RM
            elif match := re.match(r"button (\d+) is ([a-z0-9]+)", line):
                alias = Alias( renamed=match[2], actual=f"b{match[1]}")
                m.buttons[alias.actual] = alias
            elif match := re.match(r"hat (\d+) is ([a-z0-9]+)", line):
                alias = Alias( renamed=match[2], actual=f"h{match[1]}" )
                m.hats[alias.actual] = alias
            elif match := re.match(r"trigger on axis (\d+) is ([a-z0-9]+)", line):
                trigger = Trigger( name=match[2], axis=f"a{match[1]}", segments=["l","h"] )
                m.triggers[trigger.name] = trigger
            else:
                print(f"don't know how to parse '{line}', skipping")

        # Sort so that longest combos come first.
        #sorted_keys = sorted(m.mappings.keys(), key=len, reverse=True)
        m.mappings = {k: m.mappings[k] for k in sorted(m.mappings.keys(), key=len, reverse=True)}

        return m
