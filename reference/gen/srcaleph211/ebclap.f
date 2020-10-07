      SUBROUTINE EBCLAP( ILAP , NUST , INDX , ESTO , EVLP , IVLP )
C ----------------------------------------------------
C   AUTHOR   : J. Badier     05/10/89
C! Overlap correction to the cluster energy.
CKEY PHOTONS OVERLAP / INTERNAL
C
C   An estimation of the energy deposited in the overlap is calculated
C   accordingly tothe method defined by the control word ILAP.
C
C   Input     : ILAP    Control word.
C                       = 0 JULIA method.
C                       = 1 No correction.
C                       = 2 Shape dependant method.
C               NUST    Number of storeys of the cluster ICLN.
C               INDX(1,IST) Theta index of the storey IST.
C               INDX(2,IST) Phi index of the storey IST.
C               INDX(3,IST) Stack number of the storey IST.
C               ESTO(IST)   Raw content of the storey IST.
C                           IST = 1 , NUST
C
C   Output    : EVLP    Estimated energy in the 5 theta overlap.
C               IVLP    Number of storeys in the overlap.
C
C   BANKS :
C     INPUT   : EGVP
C     OUTPUT  : NONE
C
C ----------------------------------------------------
      SAVE
      PARAMETER( LEC1 = 46 , LBA1 = 55 , LBA2 = 174 , LEC2 = 183 )
      PARAMETER( GAPA = 5.7 , GAPB = .23  , PTIT = .0001 )
      PARAMETER( PATA = .8 , PATB = 3.5 , PATC = .9 )
      DIMENSION INDX(3,*) , ESTO(*) , EVLP(*) , TWSM(10) , RDLE(10)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JEGVRL=1,LEGVPA=16)
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
C   Initialisation
      IVLP = 0
      DO 4 I = 1 , 5
    4 EVLP(I) = 0.
C   Control word.
      IF( ILAP .EQ. 1 ) GO TO 98
C   EGVP bank.
      KEGVP = IW( NAMIND('EGVP') )
      IF( KEGVP .EQ. 0 ) GO TO 98
C   JULIA treatmaent.
      DO 2 I = 1 , 10
        RDLE(I) = 0.
        TWSM(I) = 0.
    2 CONTINUE
C Loop over clusters.
      DO 1 IST = 1 , NUST
        IT = INDX(1,IST)
        JF = INDX(2,IST)
C   Overlap storey ?
        IF( IT .LT. LEC1 .OR.  IT .GT. LEC2 ) GO TO 1
        IF( IT .LE. LBA1 ) THEN
C   Overlap , side Z > 0.
          LIT = IT - LEC1 + 1
          KLEC = 1
        ELSE
          IF( IT .GE. LBA2 ) THEN
C   Overlap , side Z < 0.
            LIT = LEC2 - IT + 1
            KLEC = 3
          ELSE
C   Barrel
            GO TO 1
          ENDIF
        ENDIF
C   Count stories in the overlap.
        IVLP = IVLP + 1
C   Sommation over phi alone.
        TWSM( LIT ) = TWSM( LIT ) + ESTO( IST )
C   One searches in the EGVP table the mean radiation length of
C   the matter between barrel and endcap as a function of the azimuth.
        ICC = MOD( JF - 1 , 16 )
        ICR = 1 + ( JF - 1 ) / 16
        IF( KLEC .NE. 1 ) ICR = ICR + 24
        IF( ICR .LE. 0 .OR. ICR .GT. LROWS(KEGVP) ) GO TO 1
C   Weighted mean radiation length.
        RDLE(LIT) = RDLE(LIT) + ESTO(IST) / RTABL(KEGVP,ICR,ICC+JEGVRL)
    1 CONTINUE
      IF( IVLP .EQ. 0 ) GO TO 98
      DO 3 I = 1 , 5
C   TWSM(I) is the endcap sum.
C   TWSM(I+5) is the barrel sum.
C   CORR is the Patricia correction.
        RADL = RDLE(I) + RDLE(I+5)
        IF( RADL .LE. PTIT ) GO TO 3
        TOT = TWSM(I) + TWSM(I+5)
        IF( TOT .LT. PTIT ) GO TO 3
        RAP = ABS( TWSM(I+5) - TWSM(I) ) / TOT
        IF( RAP .LT. PATC ) THEN
          CPAT = 1. - PATA * RAP * RAP
        ELSE
          CPAT = PATB * ( 1. - RAP )
        ENDIF
        EVLP(I) = CPAT * ( GAPA * RADL + GAPB * TOT )
    3 CONTINUE
   98 CONTINUE
      RETURN
      END
