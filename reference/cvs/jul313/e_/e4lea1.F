      SUBROUTINE E4LEA1(NUST,INDX,ESTO,ITVLP,EVLP,IVLP,COEFF)
C----------------------------------------------------------------------
C!  - MODIFIED VERSION OF ROUTINE ECLEAK  (FOR ELECTRON ID)
C!   AUTHOR   : D. PALLIN     07/07/90
C!
C! Overlap correction to the cluster energy.
C!
C!   Input       NUST    NUMBER OF STOREYS OF THE CLUSTER ICLN.
C!               INDX(1,IST) Theta index of the storey IST.
C!               INDX(2,IST) Phi index of the storey IST.
C!               INDX(3,IST) Stack number of the storey IST.
C!               ESTO(IST)   Raw content of the storey IST.
C!                           IST = 1 , NUST
C!               COEFF       Correction to overlap radiation length
C!                           due to charged track curvature
C!   Output    : EVLP    Estimated energy in the 5 theta overlap.
C!               ITVLP   Teta index of storeys in the overlap.
C!               IVLP    Number of storeys in the overlap.
C!
C!   BANKS :
C!     INPUT   : EGVP
C!     OUTPUT  : NONE
C!
C?
C!======================================================================
#ifndef DOC
      PARAMETER( LEC1 = 46 , LBA1 = 55 , LBA2 = 174 , LEC2 = 183 )
      PARAMETER( GAPA = 5.7 , GAPB = .23  , PTIT = .0001 )
      PARAMETER( PATA = .8 , PATB = 3.5 , PATC = .9 )
      DIMENSION INDX(3,*) , ESTO(*) , EVLP(*) , TWSM(10) , RDLE(10)
     *,ITVLP(*)
#include "bcs.h"
#include "egvpjj.h"
#include "bmacro.h"
C   Initialisation
      IVLP = 0
      DO 4 I = 1 , 5
      ITVLP(I)=0
    4 EVLP(I) = 0.
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
C         TETA  ENDCAP
          IF(LIT.LE.5)ITVLP(LIT)=IT
          KLEC = 1
        ELSE
          IF( IT .GE. LBA2 ) THEN
C   Overlap , side Z < 0.
            LIT = LEC2 - IT + 1
C         TETA  ENTREE ENDCAP
          IF(LIT.LE.5)ITVLP(LIT)=IT
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
      EBAR=0.
      EEND=0.
      RADL=0.
      TOT =0.
      IF( IVLP .EQ. 0 ) GO TO 98
      DO 33 I = 1 , 5
C   TWSM(I) is the endcap sum.
C   TWSM(I+5) is the barrel sum.
C   CORR IS THE PATRICIA CORRECTION.
      EEND=TWSM(I)+EEND
      EBAR=TWSM(I+5)+EBAR
        RADL = RDLE(I) + RDLE(I+5)+RADL
        TOT = TWSM(I) + TWSM(I+5) +TOT
  33    CONTINUE
        IF( RADL .LE. PTIT ) GO TO 3
C CORRECTION  DUE TO CHARGED TRACK CURVATURE
        GAPAA=GAPA*COEFF
        GAPBB=GAPB*COEFF
        IF( TOT .LT. PTIT ) GO TO 3
        RAP = ABS( EBAR      - EEND    ) / TOT
        IF( RAP .LT. PATC ) THEN
          CPAT = 1. - PATA * RAP * RAP
        ELSE
          CPAT = PATB * ( 1. - RAP )
        ENDIF
        EVLP(1) = CPAT * ( GAPAA * RADL + GAPBB * TOT )
    3 CONTINUE
   98 CONTINUE
      RETURN
      END
#endif
