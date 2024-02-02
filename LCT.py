import sys

from ctypes import *
#------------------------------------------------------------
# Configuration

# Choose implementation
#lib = cdll.LoadLibrary('../splayLCT.so')
#lib = cdll.LoadLibrary('./pointerLCT.so')
lib = cdll.LoadLibrary(sys.argv[1])
sys.argv = []

#------------------------------------------------------------

LCT = POINTER(c_char)
nodeT = c_uint
# Choose cost type if changed in LCT.h
costT = c_double

lib.sizeofStruct.argtypes = []
lib.sizeofStruct.restype = c_uint

Access = lib.Access
Access.argtypes = [LCT, nodeT]
Access.restype = c_uint

getNode = lib.getNode
getNode.argtypes = [LCT, nodeT, c_int]
getNode.restype = nodeT

edgeQ = lib.edgeQ
edgeQ.argtypes = [LCT, nodeT, nodeT]
edgeQ.restype = c_bool

cut = lib.cut
cut.argtypes = [LCT, nodeT, nodeT]
cut.restype = nodeT

reRoot = lib.reRoot
reRoot.argtypes = [LCT, nodeT]

LCA = lib.LCA
LCA.argtypes = [LCT, nodeT, nodeT]
LCA.restype = nodeT

if hasattr(lib, 'Link'):
    Link = lib.Link
    Link.argtypes = [LCT, nodeT, nodeT]
else:
    LinkW = lib.LinkW
    LinkW.argtypes = [LCT, nodeT, nodeT, costT]

    getCost = lib.getCost
    getCost.argtypes = [LCT, nodeT, nodeT]
    getCost.restype = costT

    update = lib.update
    update.argtypes = [LCT, nodeT, costT]

    getMin = lib.getMin
    getMin.argtypes = [LCT, nodeT]
    getMin.restype = nodeT

#print("Size of struct is " + str(lib.sizeofStruct(None)))

import fileinput

lnCt = 0

for line in fileinput.input():
#    print(line)
    lnCt+=1
    if 0 != lnCt and line[0] != "#" :
        print("# " + str(lnCt) + " : " + line.rstrip())
    tok = line.split()
#    print(tok)
    match tok[0]:
       case "allocLCT":
          n = int(tok[1])
          F = create_string_buffer((n+1)*lib.sizeofStruct(None))
          lib.initLCT(F, n)
       case "Access":
          print(Access(F, int(tok[1])))
       case "getNode":
          print(getNode(F, int(tok[1]), int(tok[2])))
       case "edgeQ":
          print(edgeQ(F, int(tok[1]), int(tok[2])))
       case "cut":
          print(cut(F, int(tok[1]), int(tok[2])))
       case "reRoot":
          reRoot(F, int(tok[1]))
       case "LCA":
          print(LCA(F, int(tok[1]), int(tok[2])))
       case "Link":
           Link(F, int(tok[1]), int(tok[2]))
# These are only for w version, but should not exist on input otherwise
       case "LinkW":
           LinkW(F, int(tok[1]), int(tok[2]), float(tok[3]))
       case "getCost":
          print('%f' % getCost(F, int(tok[1]), int(tok[2])))
       case "update":
          update(F, int(tok[1]), float(tok[2]))
       case "getMin":
          u = getMin(F, int(tok[1]))
          v = getNode(F, u, -1)
          print('%f' % getCost(F, u, v))
       case "end":
          break

quit()
