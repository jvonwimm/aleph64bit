      SUBROUTINE SRBFTR(IER)
C----------------------------------------------------------------------
C! Build the SFTR bank
C!
C!    Author:     H. Meinhard       02-Mar-1990
C!    Modified:   H. Meinhard       05-Mar-1990  (1)
C!
C!    Input:      - bank 'STRK'
C!    Output:     - IER       /I    error flag: 0 = no error
C!                                              1 = no space for new bk
C!                - bank 'SFTR'
C!
C!    Description
C!    ===========
C!    Convert tracks being stored in bank STRK into SFTR format, fill
C!    SFTR bank
C?
C!======================================================================
#ifndef DOC
#ifndef DOC
#include "alcons.h"
#include "bcs.h"
#include "rlunit.h"
#include "sanamc.h"
#include "sftrjj.h"
#include "sgeomc.h"
#include "srecpc.h"
#include "strkjj.h"
#include "bmacro.h"
C----------------------------------------------------------------------
      DPHI = DPHBSR * (ZZERSG - ZOFFSG - REAL(MLAYSG-1)*ZDELSG/2.)
C link to STRK bank
      JSTRK = IW(NASTRK)
      IF (JSTRK .EQ. 0)                                     GOTO 901
      NTRK = LROWS(JSTRK)
C create SFTR bank
      CALL AUBOS('SFTR',0,LMHLEN+NTRK*LSFTRA,JSFTR,IGARB)
      IF (JSFTR .EQ. 0 .OR. IGARB .EQ. 2)                   GOTO 902
      IF (IGARB .EQ. 1) JSTRK = IW(NASTRK)
      IW(JSFTR+LMHCOL) = LSFTRA
      IW(JSFTR+LMHROW) = NTRK
C top of loop over tracks
      DO 300 ITRK = 1, NTRK
        KSFTR = KROW(JSFTR,ITRK)
        IQF = ITABL(JSTRK,ITRK,JSTRQF)
        IF (IQF .EQ. 1) THEN
          IW(KSFTR+JSFTNF) = ITABL(JSTRK,ITRK,JSTRNP) - 2
        ELSE
          IW(KSFTR+JSFTNF) = 0
        ENDIF
        RW(KSFTR+JSFTC2) = RTABL(JSTRK,ITRK,JSTRC2)
        IW(KSFTR+JSFTQF) = ITABL(JSTRK,ITRK,JSTRQF)
        THETA = RTABL(JSTRK,ITRK,JSTRTH)
        PHI   = RTABL(JSTRK,ITRK,JSTRPH)
        RW(KSFTR+JSFTTH) = THETA
        IF (THETA .LT. PIBY2) THEN
          RW(KSFTR+JSFTPH) = PHI - DPHI
        ELSE
          RW(KSFTR+JSFTPH) = PHI + DPHI
        ENDIF
        RW(KSFTR+JSFTCM) = RTABL(JSTRK,ITRK,JSTRTT)
        RW(KSFTR+JSFTCM+1) = RTABL(JSTRK,ITRK,JSTRTP)
        RW(KSFTR+JSFTCM+4) = RTABL(JSTRK,ITRK,JSTRPP)
        IW(KSFTR+JSFTLI) = ITABL(JSTRK,ITRK,JSTRLI)
C bottom of loop over tracks
  300 CONTINUE
      GOTO 999
C----------------------------------------------------------------------
  901 IER = 1
      GOTO 999
  902 IER = 2
      CALL RERROR('SRBFTR',-IER,'No space for new bank SFTR')
      GOTO 999
  999 CONTINUE
      RETURN
      END
#endif
#endif
