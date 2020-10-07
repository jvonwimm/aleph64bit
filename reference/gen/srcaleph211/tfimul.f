      FUNCTION TFIMUL(IRUN,NBNK,IMUL,IEND,IT3,IROW,XCOR,SHIFT)
C
C-----------------------------------------------------------------------
C! Correct TPC Coordinates for field distortions.
C! This code is called by TFICOR. It allows for more complicated
C! corrections than TFICOR alone can provide, depending for example
C! on phi or on the TPC currents.
C!
C!  Author    :   I. Tomalin  94/06/29
CKEY TPC FIELD-CORRECTION
C!
C!  Input     :   IRUN    /I  : Current run number.
C!                NBNK    /I  : T3CC/T3FC bank number.
C!                IMUL    /I  : Code number of correction to apply.
C!                IEND    /I  : TPC side A (=1), B (=2)
C!                IT3     /I  : Correction based upon the IT3'th row
C!                              referring to this TPC side, in the
C!                              T3FC/T3CC banks
C!                IROW    /I  : TPC pad row number
C!                XCOR(3) /R  : R, PHI and Z of TPC coord. respectively,
C!                              with R and Z scaled to have modulus less
C!                              than 1.
C!                SHIFT   /R  : Correction prior to calling TFIMUL.
C!
C!  Output     :  TFIMUL  /R  : Correction to phi/z coord. previously
C!                              obtained by TFICOR is to be multiplied
C!                              by TFIMUL.
C!
C-----------------------------------------------------------------------
      SAVE
C
      DIMENSION XCOR(*)
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
      PARAMETER(JT3CSI=1,JT3CNC=2,JT3CCO=3,LT3CCA=20)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      PARAMETER(MAXCO=LT3CCA-JT3CCO+1,MAXT3=4)
      DIMENSION NUMT3(2),NCOEFF(2,MAXT3),COEFF(MAXCO,2,MAXT3)
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
C Initialize.
      IF (FIRST) THEN
        FIRST = .FALSE.
        LBNK  = 0
      ENDIF
C
      TFIMUL = 1.0
      IF (IMUL.LE.0) GOTO 999
C
C If NBNK has changed, look at new T3CC bank.
      IF (NBNK.NE.LBNK) THEN
        IF (LBNK.GT.0) KT3CC = NDROP ('T3CC',LBNK)
        LBNK  = NBNK
        LDBAS = JUNIDB(0)
        KT3CC = MDARD (IW,LDBAS,'T3CC',NBNK)
        IF (KT3CC.EQ.0) THEN
          WRITE(IW(6),25) IRUN,NBNK
   25     FORMAT(' FATAL TFIMUL ERROR: T3CC bank missing. ',2I7)
           CALL EXIT
        END IF
C
        IF (LROWS(KT3CC).GT.MAXT3) THEN
          WRITE(IW(6),35) IRUN,NBNK,LROWS(KT3CC),MAXT3
   35     FORMAT(' FATAL TFIMUL ERROR: Please increase array size. ',
     +    4I7)
           CALL EXIT
        END IF
C
        NUMT3(1) = 0
        NUMT3(2) = 0
        DO 75 J = 1,LROWS(KT3CC)
          JT3CC  = KROW(KT3CC,J)
          IS             = IW(JT3CC + JT3CSI)
          NUMT3(IS)      = NUMT3(IS) + 1
          JT3            = NUMT3(IS)
          NCOEFF(IS,JT3) = IW(JT3CC + JT3CNC)
          DO 50 I = 1,NCOEFF(IS,JT3)
            COEFF(I,IS,JT3) = RW(JT3CC + JT3CCO - 1 + I)
   50     CONTINUE
   75   CONTINUE
C
      END IF
C
C Unpack scaled coordinates.
      RR  = XCOR(1)
      PHI = XCOR(2)
      ZZ  = XCOR(3)
C
C+++ Calculate multiplicative factor for phi correction of this coord.
C
      IF (IMUL.EQ.1) THEN
C
C+++  Correction for 1994 TPC gating problems in sectors 24 and 33.
C
C++ Unpack coefficients.
C Amplitude of correction in phi (approx. unity by definition).
        AMPPHI = COEFF(1,IEND,IT3)
C Phi of centre of sector.
        CENTPH = COEFF(2,IEND,IT3)
C r.m.s. width of distortion in phi.
        WIDPHI = COEFF(3,IEND,IT3)
C Additive offset.
        OFFSET = COEFF(4,IEND,IT3)
C Coefficients of multiplicitive correction in terms of TPC current.
        P1CUR  = COEFF(5,IEND,IT3)
        P2CUR  = COEFF(6,IEND,IT3)
        P3CUR  = COEFF(7,IEND,IT3)
C
C++ Get TPC current.
        CALL TGTCUR(TCURA,TCURB,ADOUBT,BDOUBT,IER)
        IF (IEND.EQ.1) THEN
          TCUR = TCURA
        ELSE
          TCUR = TCURB
        END IF
C
C++ Find correction factor.
        DELPHI = MOD(PHI - CENTPH + 5.0*PI,TWOPI) - PI
        PARG = (DELPHI/WIDPHI)**2
        IF (PARG.LT.100.0) THEN
          TFIMUL = OFFSET + AMPPHI*EXP(-0.5*PARG)*
     +             (P1CUR - (MIN(TCUR - P2CUR,0.0)/P3CUR)**2)
        ELSE
          TFIMUL = OFFSET
        END IF
C
C Correct explicitely a rather sharp distortion near TPC inner/outer
C sector boundary that couldn't be well fitted with polynomial.
        SHARP = 0.0
        IF (IROW.EQ.9.OR.IROW.EQ.10) THEN
          IF (IT3.EQ.1) THEN
C This is the correction to phi in sector 24.
            SHARP = -0.012
          ELSE IF (IT3.EQ.2) THEN
C This is the correction to phi in sector 33.
            SHARP =  0.016
          END IF
        END IF
C
        PSHIFT = SIGN(MAX(ABS(SHIFT),1.0E-20),SHIFT)
        TFIMUL = TFIMUL*(1.0 + SHARP/PSHIFT)
C
      ELSE
C Unknown correction.
        WRITE(IW(6),900) IRUN,NBNK,IMUL
  900   FORMAT(' FATAL TFIMUL ERROR: Unknown correction. ',3I7)
         CALL EXIT
      END IF
C
  999 CONTINUE
      END
