#!/bin/python

import sys

REPEAT_PAIRS = ["00", "11", "22", "33", "44", "55", "66", "77", "88", "99"]
DECREASE_PAIRS = set(["90", "91", "92", "93", "94", "95", "96", "97", "98",
    "80", "81", "82", "83", "84", "85", "86", "87",
    "70", "71", "72", "73", "74", "75", "76",
    "60", "61", "62", "63", "64", "65",
    "50", "51", "52", "53", "54",
    "40", "41", "42", "43",
    "30", "31", "32",
    "20", "21",
    "10",
    ])

def count_if_valid(x):
    x_string = str(x)
    char_pairs = [x_string[i : i + 2] for i in range(0, len(x_string) - 1)]
    if DECREASE_PAIRS.intersection(char_pairs):
        return 0
    else:
        return 1 if contains_isolated_pair(x_string) else 0

def contains_isolated_pair(s):
    for p in REPEAT_PAIRS:
        try:
            o = 0
            while True:
                o = s.index(p, o)
                if (o == 0 or s[o - 1] != p[0]) and (o + 2 >= len(s) or s[o + 2] != p[1]):
                    return True
                o += 1
        except:
            pass
    return False

def count_codes(min_inclusive, max_inclusive):
    ret = 0
    n = min_inclusive
    while n <= max_inclusive:
        ret += count_if_valid(n)
        n += 1
    return ret

min_inclusive = int(sys.argv[1])
max_inclusive = int(sys.argv[2])

print(count_codes(min_inclusive, max_inclusive))
