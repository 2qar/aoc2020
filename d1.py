from math import prod
from itertools import combinations

nums = [int(n) for n in open('in1').read().strip().split('\n')]

def sumt(t):
    return prod(t) if sum(t) == 2020 else 0

silver = sum(map(sumt, combinations(nums, 2)))
gold   = sum(map(sumt, combinations(nums, 3)))

print(f"Silver: {silver}")
print(f"Gold:   {gold}")
