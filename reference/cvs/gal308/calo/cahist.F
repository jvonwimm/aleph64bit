*DK cahist
      SUBROUTINE CAHIST(NAMI,IAD,IST,ISIG)
C--------------------------------------------------------------------
C     M.N MINARD 14-11-88
C! Calorimeter history banks
C
C  INPUT : NAME = NAME-INDEX OF BANK TO BE FILLED
C          IAD  = ADDRESS
C          IST  = STACK OR PLANE NUMBER
C          ISIG = SIGNAL
C  OUTPUT: History Banks ESHI EWHI / LSHI LWHI
C
C  Called by EHSITW, LCTRAK, LCSHOW
C--------------------------------------------------------------------
#ifndef DOC
      SAVE
      EXTERNAL LOCTAB
#include "bcs.h"
#include "trkcom.h"
      PARAMETER (JHISPT=1, JHISAD=2, JHISDE=3)
      CHARACTER *4 TNAME , CHAINT
      DATA IFI /0/
#include "bmacro.h"
C-------------------------------------------------------------------
C
C
C - get bank index
C
      JHIST = IW(NAMI)
      IF (JHIST.EQ.0) RETURN
C
C     Look if this track already in bank
C
      ITRK  = ITRKEL(1)
      LHIST = LCOLS(JHIST)
      NWRD  = LHIST - JHISDE + 1
      NHIST = LROWS(JHIST)
C
      IF (NHIST .EQ. 0 ) GOTO 100
C
      IRTRK = 0
      KHIST = KROW(JHIST,NHIST)
      DO 10 IROW = NHIST,1,-1
         IF ( IW (KHIST+JHISPT) .EQ. ITRK) THEN
            IF (IRTRK.EQ.0) IRTRK = IROW
            IF (IW (KHIST+JHISAD) .NE. IAD) GOTO 10
            IW(KHIST+JHISDE+IST-1) = IW(KHIST+JHISDE+IST-1) +ISIG
            GOTO 999
         ELSE
            IF (IRTRK .EQ. 0) GOTO 10
            KHIST = KROW (JHIST,IRTRK+1)
            CALL UCOPY2 (IW(KHIST+1),IW(KHIST+LHIST+1),
     &                   (NHIST-IRTRK)*LHIST)
            DO 5 K = 3,LHIST
  5         IW(KHIST+K) = 0
            GOTO 200
         ENDIF
 10   KHIST = KHIST - LHIST
C
C - add new row
C
 100  KHIST = KNEXT (JHIST)
C
C - store a row
C
 200  IW (KHIST + JHISPT ) = ITRK
      IW (KHIST + JHISAD ) = IAD
      IW (KHIST + JHISDE + IST -1) = ISIG
      IW (JHIST + LMHROW ) = LROWS(JHIST) + 1
C
C - check bank length for next entry
C
       IF (LFRROW(JHIST).EQ.0) THEN
          TNAME = CHAINT (IW(JHIST-3))
          CALL ALBOS (TNAME,0,IW(JHIST)+IW(JHIST)/2,JHIST,IGARB)
       ENDIF
C
 999   CONTINUE
       END
#endif
