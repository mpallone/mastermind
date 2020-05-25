# some basic statistic functions


import math


def mean(l):
    return sum(l) / float(len(l))


def fileToList(filename):
    l = []

    file = open(filename, "r")
    
    for line in file:
        thisLine = line.split()
        for i in thisLine:
            l.append(float(i))

    return l

def stddev(l):
    avg = mean(l)
    
    summation = 0
    for i in l:
        value = avg - i
        value = value ** 2
        summation += value

    summation /= (len(l) - 1)
    
    return math.sqrt(summation)


def main():
    
    filename = raw_input("enter filename: ")
    
    l = fileToList(filename)
    print l

    print "mean:   ", mean(l)
    print "stddev: ", stddev(l)


main()
    
    
