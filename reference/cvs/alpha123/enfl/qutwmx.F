      SUBROUTINE QUTWMX(IPH,NTWPH,ITWMX,ETWMX)
C----------------------------------------------------------------
C! Number of towers and energy of the more energetic tower
C     Input     IPH   PHCO index of the cluster
C     Output    NTWPH number of fired towers (PHST)
C               ( -1 if PHST does not exist )
C               ITWMX PHST index of the more ener. tower
C               ETWMX energy of the more ener. tower
C     G. Ganis - Oct 1990
C----------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
#include "qhac.h"
#include "qmacro.h"
C----------------------------------------------------------------
      NTWPH= 0
      ITWMX= 0
      ETWMX= 0.
C
      IPHST= IW(NAPHST)
      IF(IPHST.EQ.0) THEN
        NTWPH= -1
        GOTO 999
      END IF
C
      NPHST= LROWS(IPHST)
      NTW= 0
      IT0= 0
      ET0= 0.
      DO 10 L1=1,NPHST
        ICL= ITABL(IPHST,L1,JPHSPH)
        IF(ICL.NE.IPH) GOTO 10
        NTW= NTW+1
        ETW= RTABL(IPHST,L1,JPHSCE)
        IF(ETW.GT.ET0) THEN
          IT0= L1
          ET0= ETW
        END IF
10    CONTINUE
C
      NTWPH= NTW
      ITWMX= IT0
      ETWMX= ET0
C
999   CONTINUE
      RETURN
      END
#endif
