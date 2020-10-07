      SUBROUTINE CMPSUM(IA,N,L)
C----------------------------------------------------------------------
C!    Checksum subroutine
CKEY PRESS CMPSUM CHECKSUM COMPRESS /INTERNAL
C!
C!    AUTHOR: D.Harvatis    JUL 89
C!
C!    INPUT :    IA : Integer array
C!                N : Number of items in IA array
C!
C!    OUTPUT:     L : checksum
C!
C!---------------------------------------------------------------------
#ifndef DOC
      SAVE
      INTEGER   IA(*),N,L,J
C----------------------------------------------------------------------
      L=0
      DO 10 J=1,N
   10     L=IEOR(IA(J),L)
      RETURN
      END
#endif
