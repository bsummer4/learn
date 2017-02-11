import sys
import random

def quick_sort(l):
    l.sort()

class UsageException(Exception):

    def __init__(self, s=None):
        self.s = s

    def __str__(self):
        result = ""
        result += "usage: sort_driver size iterations seed double-check(yes|no) print(yes|no)\n"
        if self.s:
            result += "Bad " + self.s + "\n"
        return result
        
if len(args) != 6:
    raise UsageException()

[size, iterations, seed, check, prnt] = sys.args

def argCheck(cond):
    if not cond[0]:
        raise UsageException(cond[1])

map(argCheck,
    ((size > 0, "size"),
     (iterations > 0, "iterations"),
     (seed >= 0, "seed"),
     (check == "yes" or check == "no", "double-check"),
     (prnt == "yes" or prnt == "no", "print")))

[check, prnt] = map(yes_or_no, [check, prnt])

random.seed(seed)
lst = [random.randint(0,10) for x in range(size)]

quick_sort(lst)
