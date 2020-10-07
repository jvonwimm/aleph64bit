      SUBROUTINE TCORES(IEND,IROW,R,PHI,Z,RCR,PHICR,ZCR)
C
C-----------------------------------------------------------------------
C! Correct TPC Coordinates for field distortions caused by a short
C! circuit in the field cage.
C! The correction  function is based on an expansion of the distortions
C! in a Fourier Bessel Series.
C!
C!
C!  Author    :   W. Wiedenmann  09/07/92
C!  Modified  :   W. Wiedenmann  971017
C!                Don't make corrections for new alignment 
C!                (already incorpoated in TNFC bank)
C!                
C!
CKEY TPC FIELD-CORRECTION
C!
C!  Input     :
C!                IEND  /I  : TPC side A (=1), B (=2)
C!                IROW  /I  : TPC pad row number
C!                R     /R  : radius of TPC coordinate  [cm]
C!                PHI   /R  : angle  of TPC coordinate  [radian]
C!                Z     /R  : z of TPC coordinate [cm]
C!
C!  Output     :  RCR   /R  : corrected R coordinate
C!                PHICR /R  : corrected PHI coordinate
C!                ZCR   /R  : corrected Z coordinate
C!
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE IFICAG, DPOVP, ZSHORT, SSHORT, JROW
C
#include "alcons.h"
#include "bcs.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "t4rrjj.h"
C
C++   Definitions
C
      LOGICAL  FCORR
      PARAMETER (ARGMAX=13.,OM=9.,OMT=1./(1.+OM*OM))
C
      DOUBLE PRECISION TINY
      DOUBLE PRECISION SUMK, SUM, BESFAC
      DOUBLE PRECISION ARG, ARGI, ARGO, ARGZ, ARGZS
      DOUBLE PRECISION BI1, BK1, BI0RI, BI0RO, BK0RI, BK0RO
      DOUBLE PRECISION CZ, CZS
      DOUBLE PRECISION DTERM, DTERM1, FACT
C
      DATA TINY  / 1.D-3 /
      DATA FCORR /.FALSE./
      DATA NTNLO, NT4RR, IRLST, ISLST /4*0/
C
#include "bmacro.h"
C ---------------------------------------------------------------------
C - 1st entry
      IF (NT4RR .EQ. 0) THEN
        NT4RR=NAMIND('T4RR')
        LDBAS = JUNIDB(0)
        JT4RR = MDARD (IW,LDBAS,'T4RR',1)
        NTNLO = NAMIND('TNLO')
      ENDIF
C - next entry ======================================================
C
C - Initialisation
      RCR   = R
      PHICR = PHI
      ZCR   = Z
C
C - Don't make corrections for new alignment 
      IF (IW(NTNLO).GT.0) RETURN
C
C - If T4RR does not exist Return
      JT4RR = IW(NT4RR)
      IF (JT4RR.EQ.0) RETURN
C
C++   Get the current run number
C
      CALL ABRUEV (IRUN,IEVT)
C
C++   IF it is a new run/end THEN Link to the TPC Rphi-correction bank
C
      IF ((IRLST.NE.IRUN).OR.(ISLST.NE.IEND)) THEN
         IRLST=IRUN
         ISLST=IEND
         DO 10 I=1,LROWS(JT4RR)
            JROW = I
            IRUN1 = ITABL(JT4RR,I,JT4RR1)
            IRUN2 = ITABL(JT4RR,I,JT4RR2)
            IENDC = ITABL(JT4RR,JROW,JT4RSI)
            IF ((IRUN.GE.IRUN1) .AND. (IRUN.LE.IRUN2) .AND.
     &          (IENDC.EQ.IEND)) GOTO 20
 10      CONTINUE
C - run # IRUN is not in the run range of corrections - Return
         FCORR = .FALSE.
         RETURN
C
C - run # IRUN is in the run range of row # JROW
 20      CONTINUE
         FCORR  = .TRUE.
         IFICAG = ITABL(JT4RR,JROW,JT4RFC) ! inner/outer field cage
         DPOVP  = RTABL(JT4RR,JROW,JT4RDU) ! voltage drop
         ZSHORT = RTABL(JT4RR,JROW,JT4RZS) ! short position
         SSHORT = RTABL(JT4RR,JROW,JT4RCS) ! Scale factor
      ENDIF
C
C - normal entry   ===================================================
C
C++   Check if coordinate has to be corrected
C
      IF (.NOT.FCORR) RETURN
      IF (IEND.NE.ITABL(JT4RR,JROW,JT4RSI)) RETURN
C
C++   Set variables
C
      RMIN = RTPCMN
      RMAX = RTPCMX
      IF (IFICAG.EQ.2) THEN
         RMIN = RTPCMX
         RMAX = RTPCMN
      ENDIF
      SUM  = 0.
      K    = 0
      FACT = -1.
C
      ARG   = PI/ZTPCMX
      ARGZ  = ARG*Z
      ARGZS = ARG*ZSHORT
      ARGI  = ARG*RMIN
      ARGO  = ARG*RMAX
      ARG   = ARG*R
C
C++   Calculate fourier coefficients for correction
C
  100 CONTINUE
C
      K    = K + 1
      AK   = FLOAT(K)
      FACT = - FACT
C
      DTERM = MOD(AK*ARGZ,DBLE(TWOPI))
      CZ    = COS(DTERM)
      DTERM = MOD(AK*ARGZS,DBLE(TWOPI))
      CZS   = COS(DTERM)
C
      SUMK = CZS * (FACT + CZ)
C
      IF ( AK*ARGI .GT. ARGMAX )  THEN
        DTERM  = -AK*(ARGO-ARG)
        DTERM1 = AK*(ARGO-ARGI)
        BESFAC = COSH(DTERM)/TANH(DTERM1) + SINH(DTERM)
        BESFAC = SQRT(RMAX/RMIN) * BESFAC / AK
      ELSE
        TERM  = AK*ARG
        BI1   = BESI1( TERM )
        BK1   = BESK1( TERM )
        TERM  = AK*ARGI
        BI0RI = BESI0( TERM )
        BK0RI = BESK0( TERM )
        TERM  = AK*ARGO
        BI0RO = BESI0( TERM )
        BK0RO = BESK0( TERM )
        BESFAC = ( BI1*BK0RI + BI0RI*BK1 ) /
     +           ( BI0RO*BK0RI - BI0RI*BK0RO ) / AK
      ENDIF
      SUMK = SUMK * BESFAC
C
      SUM  = SUM + SUMK
C
      IF ( K.EQ.1 )                  GOTO 100
      IF ( ABS(BESFAC) .GT. TINY )   GOTO 100
c
      SUM = SUM - 0.5*SUMK
C
      FCA = -2.*DPOVP*ZTPCMX/PI * SUM
      FCA = SSHORT*FCA
C
C++   Correct coordinates
C
      PHICR = PHI - FCA/R*OM*OMT
      IF (PHICR.GT.TWOPI) THEN
        PHICR=PHICR-TWOPI
      ELSEIF (PHICR.LT.0.) THEN
        PHICR=PHICR+TWOPI
      ENDIF
      RCR = R + FCA*OMT
C
      RETURN
      END
#endif
