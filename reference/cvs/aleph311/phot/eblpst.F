      SUBROUTINE EBLPST( STAC , TETA , PHI , KLAS , PROB , IER )
C ----------------------------------------------------------------------
C   AUTHOR   : J.Badier    17/04/89
C!  Conditional probability deduced from stacks configurations.
CKEY PHOTONS GAMMA PROBABILITY / INTERNAL
C   The 3 stacks are compared to fixed thresholds in order to find
C   the configuration index of the cluster. The proportion of gammas
C   belonging to a same configuration depends of the total energy and
C   of the incidence angle. The EGST table contains these proportions
C   deduced from a Monte Carlo simulation. This routine performs an
C   interpolation into the EGST table.
C   This algorithm is not valid in the cracks and overlaps regions.
C
C   Input : STAC(3)     Stacks content in Gev.
C           TETA        Polar angle in radians.
C           PHI         Azimuthal angle in radians.
C
C   Output: KLAS         Configuration class.
C           PROB         Estimated probability.
C           IER          Error code.
C                   = 0  No error.
C                   > 0  Fatal error.
C                   < 0  Overlap.
C                   = 1  Missing bank.
C                   = 4  Energy < EMIN = .05
C                   = 5  Energy > EMAX = 80.
C                   = 6  Petal inner edge
C                   = -1 Truncated endcap.
C                   = -3 Overlap.
C                   = -6 Truncated barrel.
C
C   BANKS :
C     INPUT   : EGST  Proportion of gammas per configuration.
C               EGTH  Limits on the theta angle.
C
C     OUTPUT  : NONE
C     CREATED : NONE
C
C   Calls EBCONF , EBCOSI
C   Called by EBLONG
C-----------------------------------------------------
#ifndef DOC
      SAVE
      DIMENSION STAC(*)
C       The bank EGST gives the logarithm of the probability of a
C       configuration as a function of the logarithm of the energy with
C       the bining define by BIN and as a function of EPS /.0,.2,.4/.
C              EPS = 1./ cos(Incidence angle) - 1.
C       The array COEF is used to perform a cubic interpolation.
      PARAMETER ( KBIN = 10 , KPOL = 4 , KOEF = KBIN - KPOL + 1 )
      DIMENSION BIN(KBIN) , XI(KPOL) , Q(KPOL) , COEF(KPOL,KOEF)
      DIMENSION PR(3)
C   EMIN , EMAX : Validity range for the energy. STEP : EPS steps.
      PARAMETER( EMIN = .050 , EMAX = 100. , STEP = .2 )
C    XMAX : Protection for EXP function.
      PARAMETER( XMAX = 50. )
#include "egthjj.h"
#include "egstjj.h"
#include "bcs.h"
      EXTERNAL NAMIND
      DATA KDEB / 0 /
#include "bmacro.h"
C
      IER = 0
C   First call.
      IF( KDEB .EQ. 0 ) THEN
        KDEB = 1
        NEGST = NAMIND( 'EGST' )
        NEGTH = NAMIND( 'EGTH' )
C   Initialisations.
C   Bining (.05,.1,......10.,20.,50.)
        BIN(5) = .0
        BIN(6) = ALOG( 2. )
        BIN(7) = ALOG( 5. )
        BIN(8) = BIN(6) + BIN(7)
        BIN(9) = BIN(6) + BIN(8)
        BIN(10) = BIN(7) + BIN(8)
        BIN(4) = - BIN(6)
        BIN(3) = - BIN(7)
        BIN(2) = - BIN(8)
        BIN(1) = - BIN(9)
C   Interpolation coefficients.
        DO 11 IDEB = 1 , KOEF
          DO 12 I = 1 , KPOL
            IFIN = IDEB + 3
            IB1 = IDEB + I - 1
            COEF(I,IDEB) = 1.
            DO 13 IB2 = IDEB , IFIN
              IF(IB1 .NE. IB2) COEF(I,IDEB) =
     +        COEF(I,IDEB) / ( BIN(IB1) - BIN(IB2) )
   13       CONTINUE
   12     CONTINUE
   11   CONTINUE
      ENDIF
C   Initialisation ended.
C
C   Energy.
      ENER = STAC(1) + STAC(2) + STAC(3)
C   Energy out of range.
      IF( ENER .LT. EMIN ) GO TO 104
      IF( ENER .GT. EMAX ) GO TO 105
      ENLG = ALOG( ENER )
C   The EGST and EGTH banks are necessary.
      KEGST = IW( NEGST )
      IF( KEGST .EQ. 0 ) GO TO 101
      KEGTH = IW( NEGTH )
      IF( KEGTH .EQ. 0 ) GO TO 101
C   Subdetector ISC.
      ISC = 0
      IF( TETA .GE. RTABL(KEGTH,1,JEGTT2)   .AND.
     +    TETA .LE. RTABL(KEGTH,1,JEGTT4) ) ISC = 1
      IF( TETA .GE. RTABL(KEGTH,1,JEGTT5)   .AND.
     +    TETA .LE. RTABL(KEGTH,2,JEGTT5) ) ISC = 2
      IF( TETA .GE. RTABL(KEGTH,2,JEGTT4)   .AND.
     +    TETA .LE. RTABL(KEGTH,2,JEGTT2) ) ISC = 3
C   Validity range for the theta angle :
      IF( ISC .EQ. 0 ) GO TO 103
C   The stack 3 information may be wrong :
      IF( ( TETA .GE. RTABL(KEGTH,1,JEGTT3)      .AND.
     +      TETA .LE. RTABL(KEGTH,1,JEGTT6) )    .OR.
     +    ( TETA .GE. RTABL(KEGTH,2,JEGTT3)      .AND.
     +      TETA .LE. RTABL(KEGTH,2,JEGTT6) ) )  THEN
        IER = -1
        IF( ISC .EQ. 2 ) IER = -6
      ENDIF
C   COSI is the cosinus of the incidence angle on the module.
      CALL EBCOSI( TETA , PHI , ISC , COSI )
C   Configuration class.
      CALL EBCONF( STAC , KLAS )
C    Searches interpolation intervals.
      KB2 = KBIN - 2
      DO 2 IB = 3 , KB2
        IF (ENLG .LT. BIN(IB)) GO TO 3
    2 CONTINUE
      IB = KBIN - 1
    3 CONTINUE
      IF ( RTABL(KEGST,KLAS+1,JEGSPR + IB) .EQ. 0. ) THEN
        IF ( RTABL(KEGST,KLAS+1,JEGSPR + IB - 1) .EQ. 0. ) GO TO 97
        IB = IB - 1
      ENDIF
      IDEB = IB - 2
C   Loop over incidences.
      DO 4 IEPS = 1 , 3
        LROW = 8 * IEPS + KLAS - 7
C   Cubic interpolation.
        PR(IEPS) = 0.
        DO 1 I = 1 , KPOL
          ICOL = IDEB + I - 1
          XI(I) = ENLG - BIN(ICOL)
    1   CONTINUE
        Q12 = XI(1) * XI(2)
        Q34 = XI(3) * XI(4)
        Q(4) = Q12 * XI(3) * COEF(4,IDEB)
        Q(3) = Q12 * XI(4) * COEF(3,IDEB)
        Q(1) = Q34 * XI(2) * COEF(1,IDEB)
        Q(2) = Q34 * XI(1) * COEF(2,IDEB)
        ICOL = JEGSPR + IDEB - 2
        DO 6 I = 1 , KPOL
          ICOL = ICOL + 1
          PR(IEPS) = PR(IEPS) + Q(I) * RTABL(KEGST,LROW,ICOL)
    6   CONTINUE
    4 CONTINUE
C   The table of the EGST bank was built for EPS = .0,.2,.4
      EPS = 1. / COSI - 1.
C   Quadratic interpolation coefficients.
      X = EPS / STEP - 1.
      X2 = X ** 2
C   Quadratic interpolation.
      B = ( PR(3) - PR(1)) *.5
      A = ( PR(3) + PR(1)) *.5 - PR(2)
      PRLG = A * X2 + B * X + PR(2)
      IF (PRLG .GT. XMAX) PRLG = XMAX
      IF (PRLG .LT. 0.) PRLG = 0.
      PROB = EXP( -PRLG )
      GO TO 98
C ====================== error ===================================
C   The EGST bank does not exist.
 101  IER = 1
      GO TO 97
C   Energy out of range.
 104  IER = 4
      GO TO 97
 105  IER = 45
      GO TO 97
C   Angle out of range.
 103  IER = 3
      IF( TETA .GT. RTABL(KEGTH,1,JEGTT1)   .AND.
     +    TETA .LT. RTABL(KEGTH,2,JEGTT1) ) IER = 6
      IF( TETA .GT. RTABL(KEGTH,1,JEGTT4)   .AND.
     +    TETA .LT. RTABL(KEGTH,2,JEGTT4) ) IER = -3
C ======== probability cannot be computed: set to 0. =============
   97 PROB =0.
   98 CONTINUE
      RETURN
      END
#endif
