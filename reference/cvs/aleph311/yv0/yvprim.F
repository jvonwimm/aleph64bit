      SUBROUTINE YVPRIM(KPOI,PRIV,SIPRI)
C-------------------------------------------
C! Finds the mean value and the cov mat. of the primary
CKEY YV0 PRIMARY VERTEX / USER
C   author:    M.A.Ciocci 21/2/90
C   modified:  M.A.Ciocci 20/1/93 Included the out diagonal
C                                 terms in the covariance
Cmatrix of the primary vertex
C            input:
C                   kpoi/i     pointer to the jsum bank
C            output:
C                   priv(3)/r  coordinates of the primary vertex
C                   sipri(3,3)/r covariance matrix of the primary
C                                vertex.
C
C     Banks:
C             jsum
C-------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "jsumjj.h"
            INTEGER KPOI
            REAL PRIV(3),SIPRI(3,3),PRIV2(3)
      EXTERNAL NLINK,NAMIND,NBANK,CHAINT,INTCHA,NDROP
      CHARACTER*4 CHAINT
      INTEGER NLINK,NAMIND,NBANK,INTCHA,NDROP
#include "bmacro.h"
C
C+ FINDS THE NUMBER OF EVENT WITH PRIMARY VERTEX RECONSTRUCTED
C
         INEVV=ITABL(KPOI,1,JJSUNV)
C
C+  CALCULATES THE MEAN VALUE OF THE PRIMARY VERTEX FOR RUN
C
         PRIV(1)=RTABL(KPOI,1,JJSUXV)/INEVV
         PRIV(2)=RTABL(KPOI,1,JJSUYV)/INEVV
         PRIV(3)=RTABL(KPOI,1,JJSUZV)/INEVV
C
C+  CALCULATES THE MEAN VALUE OF THE PRIMARY VERTEX SQUARE
C
         PRIV2(1)=RTABL(KPOI,1,JJSUXS)/INEVV
         PRIV2(2)=RTABL(KPOI,1,JJSUYS)/INEVV
         PRIV2(3)=RTABL(KPOI,1,JJSUZS)/INEVV
C
C+   AND FINALLY CALCULATES THE SIGMA ON THE MEAN VALUE OF PRIMARY
C+   VERTEX FOR RUN
C
        DO 1 J=1,3
          DO 2 K=1,3
           IF(J.EQ.K)THEN
              SIPRI(J,K)=(PRIV2(J)-PRIV(J)**2)*INEVV/(INEVV-1)
           ELSE
           SIPRI(J,K)=0
           ENDIF
  2       CONTINUE
  1     CONTINUE
        RETURN
        END
#endif
