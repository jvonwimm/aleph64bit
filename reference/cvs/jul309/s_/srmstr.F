      SUBROUTINE SRMSTR(DM,N,MST,IMST,CST)
C----------------------------------------------------------------------
C! Calculate a minimal spanning tree
C!
C!    From Communications of the ACM, April 1972, Vol. 15, Numb. 4
C!    Modified:     H. Meinhard         21-Mar-1987
C!
C!    This subroutine finds a set of edges of a linear graph
C!    comprising a tree with minimal total edge length. The
C!    graph is specified as an array of inter-node edge lengths.
C!    The edges of the minimal spanning tree of the graph are
C!    placed in array MST. Execution time is proportional to
C!    the square of the number of nodes.
C!
C!    Calling sequence variables are:
C!
C!    DM    Array of inter-node edge lengths.
C!          DM (I, J) (1 .LE. I, J .LE. IN) is the length of
C!          an edge from node I to node J. If there is no
C!          edge from node I to node J, set DM (I, J) = 1.E10
C!    N     Nodes are numbered 1, 2, ..., N.
C!
C!    MST   Array in which edge list of MST is placed. MST (1, I)
C!          is the original node and MST (2, I) is the terminal
C!          node of edge I for 1 .LE. I .LE. IMST.
C!    IMST  Number of edges in array MST.
C!    CST   Sum of edge lengths of edges of tree.
C!
C!    Program variables:
C!
C!    NIT   Array of nodes not yet in tree.
C!    NITP  number of nodes in array NIT.
C!    JI(I) Node of partial MST closest to node NIT (I).
C!    UI(I) Length of edge from NIT (I) to JI (I).
C!    KP    Next node to be added to array MST.
C!
C?
C!======================================================================
#ifndef DOC
#include "sparam.h"
      DIMENSION DM(MNOSP,*),MST(2,*)
      DIMENSION UI(MNOSP),JI(MNOSP),NIT(MNOSP)
C----------------------------------------------------------------------
C initialize node label arrays
      CST = 0.
      NITP = N-1
      KP = N
      IMST = 0
      DO 300 I = 1, NITP
      NIT(I) = I
      UI(I) = DM(I,KP)
  300 JI(I) = KP
C
C update labels of nodes not yet in tree
  310 DO 320 I = 1, NITP
      NI = NIT(I)
      D = DM(NI,KP)
      IF (UI(I).LE.D)                                      GOTO 320
      UI(I) = D
  320 JI(I) = KP
C
C find node outside tree nearest to tree
      UK = UI(1)
      DO 330 I = 1, NITP
      IF (UI(I).GT.UK)                                     GOTO 330
      UK = UI(I)
  330 K = I
C
C put nodes of appropriate edge into array MST
      IMST = IMST+1
      MST(1,IMST) = NIT(K)
      MST(2,IMST) = JI(K)
      CST = CST+UK
      KP = NIT(K)
C
C delete new tree node from array IT
      UI(K) = UI(NITP)
      NIT(K) = NIT(NITP)
      JI(K) = JI(NITP)
      NITP = NITP-1
  340 IF (NITP.NE.0)                                       GOTO 310
C
C when all nodes are in tree, quit
      GOTO 999
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
#endif
