      INTEGER FUNCTION ALKRUN (IDGEN,NOTRK,TITLE)
C -------------------------------------------------------------------
C - F.Ranjard - 870331
C! Build kine run header KRUN
C  call ALSEED to get seeds of random generator number
C - Input  :  IDGEN   = generator identifier
C             NOTRK   = NOtracking marker word
C             TITLE   = run title (up to 48 characters)
C - Output :  ALKRUN  = KRUN bank index
C                       0 means not enough space to book the bank
C  ---------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "krunjj.h"
      CHARACTER*(*) TITLE
      EXTERNAL INTCHA
      PARAMETER (LINTE = 2, LCHA4 = 12)
#include "bmacro.h"
C --------------------------------------------------------------------
C - Book the bank 'KRUN'
      CALL AUBOS ('KRUN',0,LMHLEN+LKRUNA,JKRUN,IGARB)
      IF (JKRUN.EQ.0) GOTO 999
      IW(JKRUN+LMHCOL) = LKRUNA
      IW(JKRUN+LMHROW) = 1
      CALL BKFMT ('KRUN','4I,12A,2I')
C
C - fill the 1st 14 words of the row
      KKRUN = JKRUN + LMHLEN
      IW(KKRUN+JKRUGI) = IDGEN
      IW(KKRUN+JKRUNO) = NOTRK
      DO 10 I = 1,LCHA4
         IW(KKRUN+JKRURT+I-1) = INTCHA (TITLE(I*4-3:I*4))
 10   CONTINUE
C
C - get the random number seeds and the random number generator type
      CALL ALSEED (IRNDT,ISD1,ISD2)
C   if RANMAR generator then store lab seeds
      IF (IRNDT .EQ. 3) THEN
         IW(KKRUN+JKRUFS) = ISD1
         IW(KKRUN+JKRUSS) = ISD2
      ENDIF
C
 999  CONTINUE
      ALKRUN = JKRUN
      END
#endif
