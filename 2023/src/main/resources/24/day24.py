#! /bin/python3
from z3 import Real, RealVal, Solver
import sys

# print(sys.argv)

x1 = RealVal(sys.argv[1])
y1 = RealVal(sys.argv[2])
z1 = RealVal(sys.argv[3])
dx1 = RealVal(sys.argv[4])
dy1 = RealVal(sys.argv[5])
dz1 = RealVal(sys.argv[6])

x2 = RealVal(sys.argv[7])
y2 = RealVal(sys.argv[8])
z2 = RealVal(sys.argv[9])
dx2 = RealVal(sys.argv[10])
dy2 = RealVal(sys.argv[11])
dz2 = RealVal(sys.argv[12])

x3 = RealVal(sys.argv[13])
y3 = RealVal(sys.argv[14])
z3 = RealVal(sys.argv[15])
dx3 = RealVal(sys.argv[16])
dy3 = RealVal(sys.argv[17])
dz3 = RealVal(sys.argv[18])

x = Real('x')
dx = Real('dx')
y = Real('y')
dy = Real('dy')
z = Real('z')
dz = Real('dz')
t1 = Real('t1')
t2 = Real('t2')
t3 = Real('t3')

result = Real('result')

s = Solver()

# s.add(	x1 == 19, y1 == 13, z1 == 30 dx1 == -2, dy1 == 1, dz1 == -2,)
# s.add(	x2 == 18, y2 == 19, z2 == 22, dx2 == -1, dy2 == -1, dz2 == -2,)
# s.add(	x3 == 20, y3 == 25, z3 == 34, dx3 == -2, dy3 == -2, dz3 == -4,)
# s.add(x1 == 275325627102914, y1 == 177556324137106, z1 == 279758114394131, dx1 == 249, dy1 == 405, dz1 == -531)
# s.add(x2 == 284428334220238, y2 == 231958436807561, z2 == 189800593445547, dx2 == 237, dy2 == 140, dz2 == -111)
# s.add(x3 == 208260774362545, y3 == 354915461185166, z3 == 308973039318009, dx3 == 128, dy3 == -159, dz3 == -65)

s.add(t1 > 0)
s.add(t2 > 0)
s.add(t3 > 0)

s.add(x + dx * t1 == x1 + dx1 * t1)
s.add(x + dx * t2 == x2 + dx2 * t2)
s.add(x + dx * t3 == x3 + dx3 * t3)

s.add(y + dy * t1 == y1 + dy1 * t1)
s.add(y + dy * t2 == y2 + dy2 * t2)
s.add(y + dy * t3 == y3 + dy3 * t3)

s.add(z + dz * t1 == z1 + dz1 * t1)
s.add(z + dz * t2 == z2 + dz2 * t2)
s.add(z + dz * t3 == z3 + dz3 * t3)
s.add(result == x + y + z)

s.check()

# print(s.model())
print(s.model()[result])
