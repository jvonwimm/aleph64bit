      SUBROUTINE TSATRK
C!------------------------------------------------------------
C!  Steering routine for finding chains from new coordinates.
C?  After track finding redefine and resequence TPCO, TBCO
C?  banks and update TCTC (TPCO -> TCHA relation)
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "tsawrk.h"
#include "tchain.h"
#include "tchajj.h"
#include "tctcjj.h"
C
      DIMENSION NCROW(21),JCROW(20,21)
C
#include "bmacro.h"
C
      DATA NDCHA/10/,NDTCT/200/
C
      NTWCO = LROWS(JTNWCO)
      IF (NTWCO.EQ.0) GO TO 999
C
C  Book work banks for new chains
C
      CALL WBANK(IW,JTTCHA,LMHLEN+NDCHA*LTCHAA,*800)
      IW(JTTCHA+LMHCOL) = LTCHAA
      IW(JTTCHA+LMHROW) = 0
      IW(JTTCHA-3) = INTCHA('JTP5')
C
      CALL WBANK(IW,JTTCTC,LMHLEN+NDTCT*LTCTCA,*800)
      IW(JTTCTC+LMHCOL) = LTCTCA
      IW(JTTCTC+LMHROW) = 0
      IW(JTTCTC-3) = INTCHA('JTP6')
C
C  Pick coordinates belonging to each cluster in turn
C  (These are stored in increasing cluster number in JTNWCO)
C  First create a "row list"
C  NCROW(ir) = no of coordinates row ir
C  JCROW(NCROW(ir),IROW) = indices of coordinates on row ir
C
      CALL VZERO(NCROW,21)
      NLIS = 0
      IRMIN = 999
      IRMAX = 0
      ICLAS = ITABL(JTNWCO,1,JTNWQW)/1000
      DO 20 J =1,NTWCO
         IC = ITABL(JTNWCO,J,JTNWQW)/1000
         IROW = ITABL(JTNWCO,J,JTNWIR)
         IF (IC.EQ .ICLAS) THEN
            IF (IROW.LT.IRMIN) IRMIN = IROW
            IF (IROW.GT.IRMAX) IRMAX = IROW
            NLIS = NLIS + 1
            NCROW(IROW) = NCROW(IROW) + 1
            JCROW(NCROW(IROW),IROW) = J
            IF (NCROW(IROW).GE.20) THEN
               CALL RERROR('TSATRK',1,'Truncate no of hits on row')
               GO TO 25
            ENDIF
         ELSE
C
C  Find "CHAINS" from hits of last cluster when we find a break
C  and prepare new hit list for next cluster
C
            CALL TSACHA(IRMIN,IRMAX,NCROW,JCROW)
            CALL VZERO(NCROW,21)
            IRMIN = IROW
            IRMAX = IROW
            NLIS = 1
            NCROW(IROW) = 1
            JCROW(1,IROW) = J
            ICLAS = IC
         ENDIF
   20 CONTINUE
C This is the last cluster
   25 CALL TSACHA(IRMIN,IRMAX,NCROW,JCROW)
C
  999 RETURN
  800 CALL RERROR('TSATRK',4,'Error booking JTTCHA or JTTCTC bank')
      GO TO 999
      END
#endif
