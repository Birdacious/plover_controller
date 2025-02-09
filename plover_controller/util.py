from math import atan2, floor, hypot, sqrt, tau
from typing import Optional


def get_keys_for_stroke(stroke_str: str) -> tuple[str, ...]:
    keys = list[str]()
    passed_hyphen = False
    no_hyphen_keys = set("!@#$%^&*")
    for key in stroke_str:
        if key == "-":
            passed_hyphen = True
            continue
        if key in no_hyphen_keys:
            keys.append(key)
        elif passed_hyphen:
            keys.append(f"-{key}")
        else:
            keys.append(f"{key}-")
    return tuple(keys)

def stick_segment(
    stick_deadzone: float,
    offset: float,
    segment_count: int,
    lr: float,
    ud: float,
) -> Optional[int]:
    if hypot(lr, ud) < stick_deadzone * sqrt(2):
        return None
    offset = offset / 360 * tau
    angle = atan2(ud, lr) - offset
    while angle < 0:
        angle += tau
    while angle > tau:
        angle -= tau
    segment = floor(angle / tau * segment_count)
    return segment % segment_count
