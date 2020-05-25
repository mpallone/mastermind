

def basicHeuristic(reds, whites, pegs):
#    if reds + whites > pegs:
#        return 0 # this should never happen.
#    if whites == 1 and reds == pegs - 1:
#        return 0 # this shouldn't happen; it means the code is broken
    
    score = (whites ** 2 + whites) / 2.0
    score += reds * whites
    score += (reds ** 2 + reds) / 2.0
    score += reds
    return score

def printTable():
    pegs = input("Enter number of pegs: ")

    for w in range(pegs):
        for r in range(pegs):
            print "%4d" % (basicHeuristic(r, w, pegs)),
        print
    print


def printExplicitTable():
    pegs = input("Enter number of pegs: ")

    for w in range(pegs):
        for r in range(pegs):
            score = bh(r, w, pegs)
            print "(%d, %d): %4d |" % (r, w, score),
        print
    print

def bh(reds, whites, pegs):
    return basicHeuristic(reds, whites, pegs)

printExplicitTable()
