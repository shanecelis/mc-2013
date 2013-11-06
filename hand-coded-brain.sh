
# One Pendulum
# ============



# The below line runs the individual that was evolved in FODE from
# results-13/f2b2-2.dat with this sandwich:
#
# [ -.1  0 ] [ FODE-motor-1 ]   [ Bullet-angle-1 ]
# [   0  0 ] [ FODE-motor-2 ] = [ Bullet-angle-2 ]
#
# Matrices are represented as #(a b c d) -> [ a b ]
#                                           [ c d ]

# one pendulum (only requires one motor and a gain)
eracs -s run-individual -s '#(-.1 0 0 0)' results-13/f2b2-2.dat
#


# Two Pendula
# ===========

# two pendula (cheating?  This controls both pendula with only one
# motor, so it's essentially a single pendulum)
eracs -s run-individual -s '#(-.1 .0 -.1 .0)' results-12/f2b2-1.dat
# two pendula (uses both pendula, seems to require a linear
# combination of motors)
eracs -s run-individual -s '#(-.1 .1 -.1 -.1)' results-12/f2b2-1.dat
