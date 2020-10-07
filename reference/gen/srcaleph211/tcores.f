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
      SAVE IFICAG, DPOVP, ZSHORT, SSHORT, JROW
C
C! define universal constants
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (LTPDRO=21,LTTROW=19,LTSROW=12,LTWIRE=200,LTSTYP=3,
     +           LTSLOT=12,LTCORN=6,LTSECT=LTSLOT*LTSTYP,LTTPAD=4,
     +           LMXPDR=150,LTTSRW=11)
      COMMON /TPGEOM/RTPCMN,RTPCMX,ZTPCMX,DRTPMN,DRTPMX,DZTPMX,
     &               TPFRDZ,TPFRDW,TPAVDZ,TPFOF1,TPFOF2,TPFOF3,
     &               TPPROW(LTPDRO),TPTROW(LTTROW),NTSECT,NTPROW,
     &               NTPCRN(LTSTYP),TPCORN(2,LTCORN,LTSTYP),
     &               TPPHI0(LTSECT),TPCPH0(LTSECT),TPSPH0(LTSECT),
     &               ITPTYP(LTSECT),ITPSEC(LTSECT),IENDTP(LTSECT)
C
      PARAMETER (JT4RR1=1,JT4RR2=2,JT4RSI=3,JT4RFC=4,JT4RDU=5,
     +           JT4RZS=6,JT4RCS=7,LT4RRA=7)
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
      DATA NT4RR, IRLST, ISLST /3*0/
C
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C ---------------------------------------------------------------------
C - 1st entry
      IF (NT4RR .EQ. 0) THEN
        NT4RR=NAMIND('T4RR')
        LDBAS = JUNIDB(0)
        JT4RR = MDARD (IW,LDBAS,'T4RR',1)
      ENDIF
C - next entry ======================================================
C
C - Initialisation
      RCR   = R
      PHICR = PHI
      ZCR   = Z
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
