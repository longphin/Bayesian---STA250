#!/usr/bin/env python

import sys

# Initialize a bunch of variables.
x, y, x_lo, x_hi, y_lo, y_hi = None, None, None, None, None, None
current_count = 0 # Counts the number of observations in current bin.

# Input comes from STDIN.
for line in sys.stdin:
	# Remove leading and trailing whitespace
	line = line.strip()
	# Parse the input we got from mapper.py, which has format (x,y ,count)
	x, y, count = line.split(',')

	# Convert count, x, y to appropriate type.
	try:
		count = int(count)
		x = float(x)
		y = float(y)
	except ValueError:
		# Conversion failed. Just ignore it, then.
		continue

  # Since Hadoop sorts by key, we can take bins the be in order.
	if x_lo < x <= x_hi and y_lo < y <= y_hi: # (x,y) is in current box
		current_count += count
	else: # update to a new box
		if current_count > 0: # need this check so we don't accidentally print first line always
			print '%.1f,%.1f,%.1f,%.1f,%.0f' % (x_lo, x_hi, y_lo, y_hi, current_count)

    # A new box!
		x_lo, x_hi = x-.1, x
		y_lo, y_hi = y-.1, y

		# Number of observations in box increases.
		current_count = count

# print the last group
print '%.1f,%.1f,%.1f,%.1f,%.0f' % (x_lo, x_hi, y_lo, y_hi, current_count)
