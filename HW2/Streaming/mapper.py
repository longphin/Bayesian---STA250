#!/usr/bin/env python

import sys
import math

# input comes from STDIN (standard input)
for line in sys.stdin:
    # Remove leading and trailing whitespace
    line = line.strip()
    # Split the line into words
    words = line.split("\t")

    # Get x, y
    x=float(words[0])
    y=float(words[1])

    # Round x and y to 1st decimal.
    x=math.ceil(x*10)/10.0
    y=math.ceil(y*10)/10.0

		# Print (key, value) as (x,y ,1)
    print '%s,%s\t,1' % (x,y)
