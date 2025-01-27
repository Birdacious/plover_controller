from plover.system.english_stenotype import *

# From https://github.com/aerickt/plover-lapwing-aio/blob/main/plover_lapwing/system.py to not have auto-conversion to numbers in dict entries.
NUMBER_KEY = None
NUMBERS = {}
FERAL_NUMBER_KEY = False

# Only difference from default: separate RHS -K from -BG
KEYS = (
    '#',
    'S-', 'T-', 'K-', 'P-', 'W-', 'H-', 'R-',
    'A-', 'O-',
    '*',
    '-E', '-U',
    '-F', '-R', '-P', '-B', '-L', '-K', '-C', '-G', '-T', '-S', '-D', '-Z',
)

KEYMAPS = {
    'Controller': {
        '#'    : '#',
        'S-'   : 'S-',
        'T-'   : 'T-',
        'K-'   : 'K-',
        'P-'   : 'P-',
        'W-'   : 'W-',
        'H-'   : 'H-',
        'R-'   : 'R-',
        'A-'   : 'A-',
        'O-'   : 'O-',
        '*'    : '*',
        '-E'   : '-E',
        '-U'   : '-U',
        '-F'   : '-F',
        '-R'   : '-R',
        '-P'   : '-P',
        '-B'   : '-B',
        '-L'   : '-L',
        '-K'   : '-K',
        '-C'   : '-C',
        '-G'   : '-G',
        '-T'   : '-T',
        '-S'   : '-S',
        '-D'   : '-D',
        '-Z'   : '-Z',
    }
}
