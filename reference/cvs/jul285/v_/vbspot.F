      SUBROUTINE VBSPOT
C----------------------------------------------------------------------
C!  - Measure the beamspot using the ALEPH VDET
C!
C!   Author   :- Dave Brown, George Redlinger           8-APR-1993
C!
C!   Modified :- Steve Wasserbaech
C!       Nov 1994: Avoid taus to measure the beam spot
C!       Apr 1995: Use ABS on d0 and z0 cuts; remove call to VDAMB
C!   Modified: SW, 18 October 1995
C!   Fix an unbelievable ancient bug in inline function NW;
C!   switch to VBPC; introduce VBSP card; allow FRFT 0 if no VDET
C!   hits are required; allow tautau rejection and VDET hit
C!   definition to be switchable with VBPC; add some features
C!   to facilitate use in Alpha.
C?
C!======================================================================
#ifndef DOC
C     IMPLICIT NONE
      SAVE FIRST, NAVBPC, NAVBSP
#include "bcs.h"
#include "rcurnt.h"
#include "vbpcjj.h"
#include "frftjj.h"
#include "frtljj.h"
#include "revhjj.h"
#include "alcons.h"
#include "vbscom.h"
C
C     Maximum Number of tracks per event:
      INTEGER MAXTR
      PARAMETER (MAXTR = 200)
C
C     Magnetic field conversion factor:
      REAL XKG
      PARAMETER (XKG=0.00001)
C
      INTEGER NAVBPC, KVBPC, KALPB, KFRFT, KFRTL, NTRK
      INTEGER NAVBSP, KVBSP
      INTEGER KREVH, KCLASW, NDO, IP, NDAT
      INTEGER NAMIND, NLINK
      INTEGER NVDET(MAXTR)
      REAL DUM, CP1, SP1, CP2, SP2, TL1, TL2, RI1, RI2, P1, P2
      REAL EFRAC, ACOL, CHI2, CHIDOF, TL, RI, PTRK, SPH, CPH, D0B
      REAL ALFIEL
      LOGICAL FIRST, GOOD, VDETOK, ALLEP1
      EXTERNAL VDETOK
      DATA FIRST / .TRUE. /
C
C  Inline functions
C
      INTEGER ITRK, ILAY
      INTEGER NU, NW, NHITM, NHIT
#include "bmacrod.h"
C
      NU(ITRK,ILAY) = MIN(IAND(ISHFT(NVDET(ITRK),-2*(ILAY-1)),3),1)
      NW(ITRK,ILAY) = MIN(IAND(ISHFT(NVDET(ITRK),-4-2*(ILAY-1)),3),1)
C  "matched" hits (= at least one r-phi and one z hit in a layer):
      NHITM(ITRK) = NU(ITRK,1)*NW(ITRK,1) + NU(ITRK,2)*NW(ITRK,2)
C  r-phi hits only:
      NHIT(ITRK) = NU(ITRK,1) + NU(ITRK,2)
#include "bmacro.h"
C-----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        NAVBPC = NAMIND('VBPC')
        NAVBSP = NAMIND('VBSP')
      ENDIF
      KVBPC = IW(NAVBPC)
      IF (KVBPC .LE. 0) GO TO 999
C
C   Do nothing for MC unless VBSP card is present:
C
      KVBSP = IW(NAVBSP)
      IF (FMCRUN .AND. (KVBSP .LE. 0)) GO TO 999
C
C  Make sure the VBPC row number (= LEP energy regime) is valid:
C
      IF (IVBPC .LE. 0) GO TO 999
C
C  Make sure that an ALPB has been created:
C
      KALPB = NLINK('ALPB',IRUNRC)
      IF (KALPB .LE. 0) GO TO 999
C
C  If we require VDET hits, make sure the VDET is on
C  and FRFT 2 is available:
C
      IF (ITABL(KVBPC,IVBPC,JVBPVM) .GT. 0) THEN
        GOOD = VDETOK(DUM)
        IF (.NOT. GOOD) GO TO 999
        KFRFT = NLINK('FRFT',2)
C
C  otherwise FRFT 0 is good enough:
C
      ELSE
        KFRFT = NLINK('FRFT',2)
        IF (KFRFT .LE. 0) KFRFT = NLINK('FRFT',0)
      ENDIF
C
C  Make sure we have FRFT and FRTL:
C
      KFRTL = IW(NAMIND('FRTL'))
      IF ((KFRFT .LE. 0) .OR. (KFRTL .LE. 0)) GO TO 999
      NTRK = LROWS(KFRFT)
C
C   Get the number of VDET hits:
C
      CALL VDHITS(NVDET,MAXTR)
C
C  For LEP 1:
C  Different action for class 24 events (to reject taus):
C  accept only if the event has two tracks and
C  is inconsistent with tautau.
C
      IF (ITABL(KVBPC,IVBPC,JVBPTR) .EQ. 1) THEN
        KREVH = IW(NAMIND('REVH'))
        IF (KREVH .LE. 0) GO TO 999
        KCLASW = IW(KREVH+LMHLEN+JREVEC)
        IF ((IAND(KCLASW,ISHFT(1,23)) .NE. 0) .OR. (NTRK .LT. 5)) THEN
          IF (NTRK .NE. 2) GO TO 999
          CP1 = COS(RTABL(KFRFT,1,JFRFP0))
          SP1 = SIN(RTABL(KFRFT,1,JFRFP0))
          CP2 = COS(RTABL(KFRFT,2,JFRFP0))
          SP2 = SIN(RTABL(KFRFT,2,JFRFP0))
          TL1 = RTABL(KFRFT,1,JFRFTL)
          TL2 = RTABL(KFRFT,2,JFRFTL)
          RI1 = RTABL(KFRFT,1,JFRFIR)
          RI2 = RTABL(KFRFT,2,JFRFIR)
          P1 = ABS(ALFIEL(DUM) * CLGHT * SQRT(1.+TL1*TL1) * XKG / RI1)
          P2 = ABS(ALFIEL(DUM) * CLGHT * SQRT(1.+TL2*TL2) * XKG / RI2)
          EFRAC = (P1+P2)/RENLEP
          ACOL = SQRT(MAX(0.,2.*(1.+(CP1*CP2 + SP1*SP2 + TL1*TL2)/
     &                (SQRT(1.+TL1*TL1)*SQRT(1.+TL2*TL2)))))
          GOOD = (RI1*RI2 .LE. 0.)
          GOOD = GOOD .AND. (EFRAC .GT. 0.9) .AND. (EFRAC .LT. 1.1)
          GOOD = GOOD .AND. (ACOL .LT. 0.01)
          IF (.NOT. GOOD) GO TO 999
        ENDIF
      ENDIF
C
C  Generic track selection
C
      DO ITRK=1,NTRK
        CHI2 = RTABL(KFRFT,ITRK,JFRFC2)
        NDO = ITABL(KFRFT,ITRK,JFRFDF)
        IF (NDO .GT. 0) THEN
          CHIDOF = CHI2/NDO
        ELSE
          CHIDOF = CHI2
        ENDIF
        TL = RTABL(KFRFT,ITRK,JFRFTL)
        RI = RTABL(KFRFT,ITRK,JFRFIR)
        PTRK = ABS(ALFIEL(DUM) * CLGHT * SQRT(1.+TL*TL) * XKG / RI)
        D0B = RTABL(KFRFT,ITRK,JFRFD0)
C
C   For MC only, measure the d0 with respect to (XBMC,YBMC).
C   This allows the beam size to be controlled (in ALPHA).
C
        IF (FMCRUN) THEN
          CPH = COS(RTABL(KFRFT,ITRK,JFRFP0))
          SPH = SIN(RTABL(KFRFT,ITRK,JFRFP0))
          D0B = D0B - XBMC*SPH + YBMC*CPH
        ENDIF
C
C   Momentum cut:
        GOOD = (PTRK .GE. RTABL(KVBPC,IVBPC,JVBPPM))
C
C   d0 cut:
        GOOD = GOOD .AND. (ABS(D0B) .LT. RTABL(KVBPC,IVBPC,JVBPD0))
C
C   z0 cut:
        GOOD = GOOD .AND.
     &   (ABS(RTABL(KFRFT,ITRK,JFRFZ0)) .LT. RTABL(KVBPC,IVBPC,JVBPZ0))
C
C   Cut on TPC hits:
        GOOD = GOOD .AND.
     &   (ITABL(KFRTL,ITRK,JFRTNT) .GE. ITABL(KVBPC,IVBPC,JVBPTM))
C   Cut on ITC hits:
        GOOD = GOOD .AND.
     &   (ITABL(KFRTL,ITRK,JFRTNI) .GE. ITABL(KVBPC,IVBPC,JVBPIM))
C
C   Cut on matched VDET hits or r-phi hits only:
        IF (ITABL(KVBPC,IVBPC,JVBPVH) .EQ. 1) THEN
          GOOD = GOOD .AND. (NHITM(ITRK) .GE. ITABL(KVBPC,IVBPC,JVBPVM))
        ELSE
          GOOD = GOOD .AND. (NHIT(ITRK) .GE. ITABL(KVBPC,IVBPC,JVBPVM))
        ENDIF
C
C   Cut on chi**2/dof:
        GOOD = GOOD .AND. (CHIDOF .LE. RTABL(KVBPC,IVBPC,JVBPC2))
C
C   If it is a good track, save the necessary information in work bank:
C
        IF (GOOD) THEN
          IP = IBSTRK(IBSBUF)
          IF (LFRROW(IP) .LT. 1) THEN
            NDAT = IW(IP) + 100*LCOLS(IP)
            CALL WBANK(IW, IBSTRK(IBSBUF), NDAT, *800)
            IP = IBSTRK(IBSBUF)
          ENDIF
          RW(KNEXT(IP)+1) = RTABL(KFRFT,ITRK,JFRFD0)
          RW(KNEXT(IP)+2) = RTABL(KFRFT,ITRK,JFRFP0)
          RW(KNEXT(IP)+3) = RTABL(KFRFT,ITRK,JFRFEM+9)
          IW(KNEXT(IP)+4) = ISQNRC
          IW(IP+LMHROW) = IW(IP+LMHROW) + 1
        ENDIF
      ENDDO
C
C  If a VBSP card is given, don't call VBMFIT until the end of the job:
C
      IF (NCHOP .NE. 0) GO TO 999
C
C  If we have enough tracks, send them on to be fitted.  We can now
C  safely drop the previous batch of tracks.  Instruct the fit
C  to add a new row to the output bank.
C
      IF (LROWS(IBSTRK(IBSBUF)) .GE. ITABL(KVBPC,IVBPC,JVBPNC)) THEN
        IW(IBSTRK(JBSBUF)+LMHROW) = 0
        IF (IBSFEV .EQ. 1) THEN
          CALL VBMFIT(.FALSE.)
        ELSE
          CALL VBMFIT(.TRUE.)
        ENDIF
C
C  Change buffers, set flag
C
        KBSBUF = IBSBUF
        IBSBUF = JBSBUF
        JBSBUF = KBSBUF
C
C  Assume the next block will start with the next event of this run:
C
        IBSFEV = IEVTRC + 1
      ENDIF
C
      GO TO 999
C
  800 CALL RERROR ('VBSPOT', -1, 'No space to increase work bank!')
      GO TO 999
C
  999 RETURN
      END
#endif
