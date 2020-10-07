      SUBROUTINE TSNEXT(NIN,ISLOT,MXOUT,NOUT,LIST)
C
C-----------------------------------------------------------------------
C! Return list of sectors adjacent to an input list of sectors
CKEY TPCDES SECTORS / USER
C
C  R. Johnson  28-11-90
C
C  Input:         NIN       /I     Number of sectors in input list
C                 ISLOT(*)  /I     List of input sectors
C                 MXOUT     /I     Maximum length of output LIST
C  Output:        NOUT      /I     Number of adjacent sectors
C                 LIST(*)   /I     List of adjacent sectors
C
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "tpgpar.h"
#include "tpgeom.h"
C
      DIMENSION ISLOT(NIN),LIST(MXOUT)
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      PARAMETER (MXNXT=5)
      DIMENSION ISNXT(MXNXT,LTSECT)
      DATA ISNXT/2,6,7,8,9,
     &           1,3,9,10,11,
     &           2,4,11,12,13,
     &           3,5,13,14,15,
     &           4,6,15,16,17,
     &           1,5,17,18,7,
     &           1,6,8,18,0,
     &           1,7,9,0,0,
     &           1,2,8,10,0,
     &           2,9,11,0,0,
     &           2,3,10,12,0,
     &           3,11,13,0,0,
     &           3,4,12,14,0,
     &           4,13,15,0,0,
     &           4,5,14,16,0,
     &           5,15,17,0,0,
     &           5,6,16,18,0,
     &           1,5,7,17,0,
     &           90*0/
C
      IF (FIRST) THEN
        LTEND=LTSECT/2
        FIRST=.FALSE.
        DO 23 I=1,LTEND
          K=I+LTEND
          DO 21 J=1,MXNXT
            ISNXT(J,K)=ISNXT(J,I)+LTEND
   21     CONTINUE
   23   CONTINUE
      ENDIF
C
      NOUT=0
      DO 50 I=1,NIN
C
C++     Find the adjacent sector in the other TPC half
C
        IF (IENDTP(ISLOT(I)).EQ.2) THEN
          IOP=ISLOT(I)-LTEND
        ELSE
          IOP=ISLOT(I)+LTEND
        ENDIF
C
C++     Check that this one is not already in the list
C
        DO 40 J=1,NOUT
          IF (LIST(J).EQ.IOP) GO TO 41
   40   CONTINUE
        DO 140 J=1,NIN
          IF (ISLOT(J).EQ.IOP) GO TO 41
  140   CONTINUE
        IF (NOUT.GE.MXOUT) GO TO 51
        NOUT=NOUT+1
        LIST(NOUT)=IOP
   41   CONTINUE
C
C++     Find the adjacent sectors in the same half
C
        DO 45 J=1,MXNXT
          IF (ISNXT(J,ISLOT(I)).EQ.0) GO TO 46
          DO 48 K=1,NOUT
            IF (LIST(K).EQ.ISNXT(J,ISLOT(I))) GO TO 49
   48     CONTINUE
          DO 148 K=1,NIN
            IF (ISLOT(K).EQ.ISNXT(J,ISLOT(I))) GO TO 49
  148     CONTINUE
          IF (NOUT.GE.MXOUT) GO TO 51
          NOUT=NOUT+1
          LIST(NOUT)=ISNXT(J,ISLOT(I))
   49     CONTINUE
   45   CONTINUE
   46   CONTINUE
C
   50 CONTINUE
   51 CONTINUE
C
      END
#endif
