      SUBROUTINE TLACOR( R,PHI,Z , RP,PHIP,ZP , MODE)
C-----------------------------------------------------------------------
C! Corrections for B-field distortions
C ====================================
CKEY   TPC,LASER,B-FIELD,DISTORTIONS
C
C Author : Michael Schmelling   / Mainz   15-Jan-1990
C Mod.   : Werner Wiedenmann 10-Sep-91
C          use TLACOR as driver for
C          1) phi independent laser corrections (done if
C             bank TLCP is present)
C          2) phi dependent laser corrections (done if
C             bank TLFC is present)
C
C Input:    : real*4    R        : TPC coordinate - radius  [cm]
C             real*4    PHI      : TPC coordinate - azimuth [rad]
C             real*4    Z        : TPC coordinate - Z       [cm]
C             integer*4 MODE     : radial correction mode
C
C                       MODE.EQ.0 => corrections based exclusively
C                                    on laser data
C                       MODE.NE.0 => for full compensation use radial
C                                    corrections based on the field map
C             bank TLCP/TLFC from data base
C
C
C Output:     real*4  RP,PHIP,ZP : corrected coordinates
C
C Common blocks       : BCS
C External references : AGETDB,JUNIDB,AOPDBS,TFCINI,TCOREC /ALEPHLIB
C
C-----------------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JEVEEN=1,JEVERN=2,JEVERT=3,JEVEDA=4,JEVETI=5,JEVEEV=6,
     +          JEVEM1=7,JEVEM2=8,JEVEM3=9,JEVEM4=10,JEVETY=11,
     +          JEVEES=12,JEVETE=13,LEVEHA=13)
C
      LOGICAL FIRST
      INTEGER AGETDB
C
      DATA FIRST/.TRUE./
      DATA MXPRT/2/
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
C-----------------------------------------------------------------------
C
C      initialization
C
      IF(FIRST) THEN
        FIRST = .FALSE.
        NAEVEH= NAMIND('EVEH')
        NATLCP= NAMIND('TLCP')
        NATLFC= NAMIND('TLFC')
      ENDIF
C
      RP   = R
      PHIP = PHI
      ZP   = Z
C
      IF(R.LE.0.) THEN
         CALL ALTELL(
     &     ' TLACOR: TPC coordinate with radius .le. 0  => RETURN',
     &     0,'RETURN')
         RETURN
      ENDIF
C
C      check that we have the right constants for the current run
C
      KEVEH=IW(NAEVEH)
      IF(KEVEH.EQ.0) THEN
         CALL ALTELL(' TLACOR: event header bank missing; no run #',
     &              0,'RETURN')
         RETURN
      ENDIF
      KRUN=IW(KEVEH+JEVERN)
C
C     Don't make any correction for Monte Carlo
C
      IF (KRUN.LT.2001) THEN
         RETURN
      ENDIF
C
      IF(KRUN.NE.LRUN) THEN
         NPRT=0
         LRUN=KRUN
C
C         get coefficients for this run
C
         IIII =  AGETDB('TLCP',KRUN)
         IIPP =  AGETDB('TLFC',KRUN)
         IF ((IIII.EQ.0).AND.(IIPP.EQ.0)) THEN
           CALL ALTELL(
     &     ' TLACOR: '//
     &     'Bank TLCP and TLFC missing, cannot correct TPC coordinates',
     &     0,'RETURN')
           RETURN
         ENDIF
      ENDIF
C
C         if TLFC is present call phi dependent corrections
C
      KTLFC = IW(NATLFC)
      IF (KTLFC.NE.0) THEN
         CALL TLACOP( R,PHI,Z , RP,PHIP,ZP , MODE)
      ELSE
         CALL TLACOC( R,PHI,Z , RP,PHIP,ZP , MODE)
      ENDIF
C
      RETURN
      END
