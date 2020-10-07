      SUBROUTINE ICRCCO
C-----------------------------------------------------------------------
C! Create corrected coord. bank
C!
C!   Author     :-  J.Sedgbeer 16/01/90
C!   Modified   :-  J.Sedgbeer 24/10/91 Use function IRESRP to set r-phi
C!                                      resolution in ICCO
C!   Input:
C!     need commons /BCS/    for BOS banks
C!          params: ICCOJJ
C!                  ITCOJJ
C!                  ITFTJJ
C!                  IGTLJJ
C!
C!   Output: ICCO
C!
C!   called by : ITCREC
C!   calls     : none
C!
C!   Libraries required: BOS
C!
C!  Description: Use track trajectories (ITFT 1&2) to make corrected
C!  coords. - these are stored in bank ICCO. ICCO used by UFTTRK
C!  track fitting routine. Note that only coords. associated with
C!  tracks can be corrected. Also, only coords. on the first arc
C!  of the track are corrected (i.e. not coords. on further spirals).
C!
C? Check for ITCO and ITFT etc banks
C? Loop over track banks (ITFT(1) and ITFT(2))
C?   Set pointers
C?   Loop over tracks with ITC coords in 1st arc
C?     Get track crossing angle and z at each ITC layer (UTCXNG)
C?     Loop over coords. on track
C?       Get sense wire coord (UTSWCO)
C?       Correct for alignment (ITROTN)
C?       Get the drift distance and hence coord.
C?       Fill ICCO
C?     Endloop
C?   Endloop
C? Endloop
C-----------------------------------------------------------------------
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
#include "rlunit.h"
C----------------------------------------------------------------------
#include "alcons.h"
#include "bcs.h"
#include "bosext.h"
#include "iccojj.h"
#include "itcojj.h"
#include "itftjj.h"
#include "igtljj.h"
C----------------------------------------------------------------------
      EXTERNAL IRESRP
      REAL IRESRP
      REAL ZLYR(8),CTHN(8)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
#include "bmacro.h"
C----------------------------------------------------------------------
      IF(FIRST) THEN
        NICCO = NAMIND('ICCO')
        FIRST =.FALSE.
      ENDIF
C
C Check for banks: If no ITFT (tracks), or no ITCO bank then
C no need for ICCO.
C
      KITCO = NLINK('ITCO',0)
      IF(KITCO.LE.0) GOTO 999
      NCO = LROWS(KITCO)
      IF(NCO.LE.0) GOTO 999
C
      KITF1 = NLINK('ITFT',1)
      KITF2 = NLINK('ITFT',2)
      KIGC1 = NLINK('IGCL',1)
      KIGC2 = NLINK('IGCL',2)
      KIGT1 = NLINK('IGTL',1)
      KIGT2 = NLINK('IGTL',2)
C
      IF(KITF1.LE.0) THEN
        NT1 = 0
      ELSE
        IF(KIGT1.LE.0.OR.KIGC1.LE.0) GOTO 997
        NT1 = LROWS(KITF1)
      ENDIF
C
      IF(KITF2.LE.0) THEN
        NT2 = 0
      ELSE
        IF(KIGT2.LE.0.OR.KIGC2.LE.0) GOTO 997
        NT2 = LROWS(KITF2)
      ENDIF
C
      IF(NT1+NT2.EQ.0) GOTO 999
C
C Create ICCO bank
C
      LICCO  = NCO*LICCOA + LMHLEN
      CALL AUBOS('ICCO',0,LICCO,KICCO,IGARB)
      IF (IGARB.EQ.2) GOTO 998
      IW(KICCO+LMHROW) = NCO
      IW(KICCO+LMHCOL) = LICCOA
      CALL BLIST(IW,'E+','ICCO')
      IF(IGARB.EQ.1) THEN
        KITCO = NLINK('ITCO',0)
        KITF1 = NLINK('ITFT',1)
        KITF2 = NLINK('ITFT',2)
        KIGC1 = NLINK('IGCL',1)
        KIGC2 = NLINK('IGCL',2)
        KIGT1 = NLINK('IGTL',1)
        KIGT2 = NLINK('IGTL',2)
      ENDIF
C
C Loop over track banks ITFT
C
      DO 200 IB=1,2
        IF(IB.EQ.1) THEN
          NTRK  = NT1
          KITFT = KITF1
          KIGTL = KIGT1
          KIGCL = KIGC1
        ELSE
          NTRK  = NT2
          KITFT = KITF2
          KIGTL = KIGT2
          KIGCL = KIGC2
        ENDIF
        IF(NTRK.EQ.0) GOTO 200
C
C Loop over tracks (only with ITC coords. in 1st arc)
        DO 110 N=1,NTRK
          KT = KROW(KIGTL,N)
          N1 = IW(KT+JIGTN1)
          IO = IW(KT+JIGTIO)
          IF(N1.LE.0) GOTO 110
C
C Get the track crossing angle and z position at each ITC layer.
          CALL UTKXNG( RW(KROW(KITFT,N)+JITFIR), CTHN, ZLYR)
C
C Loop over coords. on track
          DO 100 I = 1,N1
            ICO = IW(KIGCL+LMHLEN+IO+I)
            KI = KROW(KITCO,IABS(ICO))
            KK = KROW(KICCO,IABS(ICO))
            IWIR = MOD(IW(KI+JITCWN),1000)
            ILAY = IW(KI+JITCWN)/1000
            TIME = RW(KI+JITCDT)
            ZOLD = RW(KI+JITCZH)
C
C Get the R and Phi of the sense wire, including sag correction
            ZSW=ZLYR(ILAY)
            CALL UTSWCO(ILAY,IWIR,ZSW,RSW,PHSW)
C
C Add the alignment correction to the wire position
            CALL ITROTN(RSW,PHSW,ZSW)
C
C Get the drift distance and hence coord.
            CALL ICDRIF(ILAY,IWIR,TIME,RSW,ZOLD,ZSW,DISTP,DISTN,SIGRP)
            DIST = DISTP
            IF(ICO.LT.0) DIST = DISTN
            DCOR= DIST/CTHN(ILAY)
            DFI= DCOR/RSW
            PHCOR= PHSW + DFI
            PHCOR= AMOD(PHCOR+TWOPI,TWOPI)
C
C Fill ICCO
            RW(KK+JICCRV) = RSW
            RW(KK+JICCPH) = PHCOR
            RW(KK+JICCZV) = ZOLD
            RW(KK+JICCSR) = ( IRESRP(ILAY,DIST,CTHN(ILAY)) )**2
            RW(KK+JICCSZ) = RW(KI+JITCSZ)
  100     CONTINUE
  110   CONTINUE
  200 CONTINUE
C
      IF(FDEBRF.AND.JDBDRF(JULIT).GE.4) THEN
        WRITE(LDEBRL,2000)
 2000   FORMAT(' ICRCCO: ICCO bank')
        DO 220 I=1,NCO
          KK = KROW(KICCO,I)
          WRITE(LDEBRL,2001) I,(RW(KK+J),J=1,5)
 2001     FORMAT(1X,I6,3F8.3,2E12.4)
  220   CONTINUE
      ENDIF
C
      GOTO 999
C----------------------------------------------------------------------
C Errors
  997 CALL RERROR('ICRCCO',1,'Track bank mismatch ITFT-IGTL-IGCL')
      GOTO 999
C
  998 CALL RERROR('ICRCCO',2,'No room for BOS banks')
C----------------------------------------------------------------------
  999 CONTINUE
      END
#endif
