      SUBROUTINE EBPRGA( STAC , TETA , PHI , ITY3 , SHAP , IER )
C ---------------------------------------------------------------------
C   AUTHOR   : J.Badier    17/04/89
C!  Longitudinal analysis of a neutral cluster.
CKEY PHOTONS GAMMA DEPTH PROBABILITY / INTERNAL
C
C   Using the stacks content one estimates the materialisation depth and
C   the deviation from average shower parameters. These quantities are
C   are named shape parameters and stored into SHAP(1) and SHAP(2). The
C   information that they cannot be calculated is stored in ITY3.
C
C   If 3 stacks are above the thresholds ( KLAS = 7 ) , SHAP(1) and
C        SHAP(2) are estimated, the electromagnetic nature of the cluster
C        is deduced from their values. ITY3 is set to 0 .
C
C        The distribution in the 3 stacks may be such incompatible with
C        an electromagnetic shower that the shape parameters are not
C        calculable. ITY3 is set to 3 .
C
C   If stacks 1 and 2 ( KLAS = 3 ) or stacks 2 and 3 ( KLAS = 6 ) are
C        above the thresholds the materialisation depth is evaluated from
C        a parametrisation .
C        Class 3 : SHAP(1) = exp( A + B * R )
C        Class 6 : SHAP(1) = A + B * R
C        R is the ratio of the stack 2 to the total energy.
C        A and B are linear functions of AI and ENLG with coefficients
C        stored in the EGMD bank.
C        AI = 1. / cos(incidence) - 1. , ENLG = ALOG( energy )
C        SHAP(2) cannot be calculated, ITY3 is set to 1.
C
C   If only one stack is above the threshold the materialsation depth
C        may be estimated with a precision of the stack size. ITY3 is
C        set to 1.
C
C   If the stack 2 is under the threshold the cluster is considered
C        as hadronic. ITY3 is set to 3.
C
C   Using KLAS , ITY3 , SHAP(1) and SHAP(2) a decision concerning the
C   cluster nature can be performed.
C
C   Input  :  STAC(3)   3 stacks contents.
C             TETA      Polar angle expressed in radians.
C             PHI       Azimuthal angle in radians.
C
C   Output :  ITY3      Gives the available identifiers.
C                       0 : SHAP(1) and SHAP(2) are available.
C                       1 : SHAP(1) is available.
C                       3 : The two identifiers are not available.
C             SHAP(2)   Identification parameters.
C                       1 : Materialisation depth.
C                       2 : Deviation from the mean shape.
C             IER       Error code.
C                       0 : No error
C                       1 : Missing bank.
C                       2 : Too many storeys
C                       3 : No storey in this cluster.
C                       4 : Energy > EMIN = .05
C                       5 : Energy < EMIN = 80.
C                      -1 : Truncated endcap , 3 stacks.
C                      -2 : Truncated endcap , 2 stacks.
C                      -3 : Overlap near endcap , 2 stacks.
C                      -4 : Overlap near endcap , 3 stacks.
C                      -5 : Overlap near barrel.
C                      -6 : Truncated barrel , 2 stacks.
C                      -7 : Truncated barrel , 3 stacks.
C                     -11 : Crack.
C
C   BANKS :
C     INPUT   : EGMD , EGTH
C     OUTPUT  : NONE
C     CREATED : NONE
C
C   Called by  EBLONG
C   Calls EBCONF , EBSLIM , EBPARA , EBDEVI , EBCOSI
C
C----------------------------------------------------------------------
#ifndef DOC
      DIMENSION  STAC(*) , SHAP(*) , SLIM(6) , ETAG(6)
C   Limits on energy.
      PARAMETER( EMIN = .050 , EMAX = 100. , NATU = 2 )
C   Precision on minimum CHI2.
      PARAMETER( EPSC =.02 , MXIT = 30 , DSMX = .5 , VMIN = .1 )
C   Protection for EXP.
      PARAMETER( XMAX = 50. , PTIT = .001 )
      DIMENSION SVAL( MXIT ) , CHIA( MXIT )
      DIMENSION CMT(4,4)
      DIMENSION ECAR(2,MXIT)
#include "egthjj.h"
#include "bcs.h"
      EXTERNAL NAMIND
      SAVE KDEB , CMT , THE1 , THE3
C     for safety SAVE all
      SAVE
      DATA KDEB / 0 /
#include "bmacro.h"
C
C   Initialisation. EGMD bank is stored in a local array.
      IF( KDEB .EQ. 0 ) THEN
        KEGMD = IW(NAMIND( 'EGMD' ) )
        IF( KEGMD .EQ. 0 ) GO TO 101
        DO 1 J = 1 , 4
        DO 1 I = 1 , 4
        CMT( I , J ) = RTABL( KEGMD , J , I )
    1   CONTINUE
C   Subcomponents theta limits from EGTH.
        KEGTH = IW(NAMIND( 'EGTH' ) )
        IF( KEGTH .EQ. 0 ) GO TO 101
        THE1 = .5 * ( RTABL( KEGTH , 1 , JEGTT4 ) +
     +  RTABL( KEGTH , 1 , JEGTT5) )
        THE3 = .5 * ( RTABL( KEGTH , 2 , JEGTT4 ) +
     +  RTABL( KEGTH , 2 , JEGTT5) )
        KDEB = 1
      ENDIF
C
C   next entry =======================================
C   Energy.
      IF( STAC(1) .LT. -.001  .OR.
     +    STAC(2) .LT. -.001  .OR.
     +    STAC(3) .LT. -.001 ) GO TO 102
      ENER = STAC(1) + STAC(2) + STAC(3)
C   Energy out of range.
      IF( ENER .LT. EMIN .OR. ENER .GT. EMAX ) GO TO 102
C
C   Configuration class.
C   A configuration class is defined by respect to the thresholds :
C   Stack 1 : .050 Gev , Stack 2 : .070 Gev , Stack 3 : .100 Gev
      CALL EBCONF( STAC , KLAS )
C
C   Stacks widths.
      CALL EBSLIM( TETA , PHI , SLIM , JER )
      IF( JER .GT. 0 ) GO TO 104
      IF( JER .EQ. -3 .OR. JER .EQ. -4 ) SLIM(3) = SLIM(2) + PTIT
      IF( JER .EQ. -5 ) SLIM(5) = SLIM(4) + PTIT
C
C   Not electromagnetic
      IF( KLAS .LE. 0 .OR. KLAS .EQ. 5 ) GO TO 105
C
      IF (KLAS .EQ. 7 .AND.
     +  JER. NE. -2 .AND. JER .NE. -3 .AND. JER .NE. -6 ) THEN
C      3 Stacks above the thresholds : KLAS = 7
C      Stack 3 width is 0.
C
C      Search materialisation depth whith the smallest Chi**2.
C      Initial step
          ITER = 0
C      The SVAL origin is the TPC exit.
          SVAL(1) = .001
C
C      Search of the initial range.
   71     CONTINUE
          ITER = ITER + 1
          DO 75 I = 1 , 6
             ETAG(I) = SLIM(I) - SVAL(ITER)
   75     CONTINUE
          IF( ETAG(1) .LT. .001 ) ETAG(1) = .001
C
C      Alpha and beta parameters estimation.
          CALL EBPARA( STAC , ETAG , ALPH , BETA , IBON )
          IF ( IBON .GT. 0 ) GO TO 105
C      Deviation from the mean shower.
          CALL EBDEVI( NATU , ENER , ALPH , BETA , ECAR(1,ITER) )
C      Chi2 calculation.
          CHIA(ITER) = ECAR(1,ITER) ** 2 + ECAR(2,ITER) ** 2
C
C      ITER may be equal to 1 , 2 , 3 or 4.    ==================
          IF( ITER .NE. 4 ) THEN
             IF( ITER .EQ. 3 ) THEN
                IF((CHIA(2).LT.CHIA(3)) .AND. (CHIA(2).LT.CHIA(1)))THEN
                  I2 = 2
                  GO TO 72
                ENDIF
                IF( CHIA(3) .LE. CHIA(1) ) THEN
C               The more frequent situation.
                  I1 = 1
                  I2 = 3
                  I3 = 2
                  GO TO 73
                ELSE
C               A 4th point at a very negative abcissa.
                  SVAL(4) = 2. * SVAL(1) - SVAL(3)
                ENDIF
             ELSE
C            iter .ne. 3
                IF( ITER .EQ. 1 ) THEN
                   SVAL(2) =  SLIM(2) - DSMX
                ELSE
                   SVAL(3) = .5 * ( SVAL(1) + SVAL(2) )
                ENDIF
             ENDIF
C            iter = 1,2,or 3 iterate
             GOTO 71
C
          ELSE
C         iter = 4
C
             IF((CHIA(1).GT.CHIA(3)) .OR. (CHIA(1).GT.CHIA(4))) THEN
C            bad configuration ?  Yes
                SVAL(5) = .5 * ( SVAL(1) + SVAL(4) )
                ITER = 5
                DO 76 I = 1 , 6
                   ETAG(I) = SLIM(I) - SVAL(ITER)
   76           CONTINUE
                IF( ETAG(1) .LT. .001 ) ETAG(1) = .001
C
C               Alpha and beta parameters estimation.
                CALL EBPARA( STAC , ETAG , ALPH , BETA , IBON )
                IF ( IBON .GT. 0 ) GO TO 105
C
C               Deviation from the mean shower.
                CALL EBDEVI( NATU,ENER,ALPH,BETA,ECAR(1,ITER) )
C
C               Chi2 calculation.
                CHIA(ITER) = ECAR(1,ITER) ** 2 + ECAR(2,ITER) ** 2
                IF( CHIA(5) .GT. CHIA(4) ) THEN
                   I2 = 4
                   GO TO 72
                ELSE
                  IF( CHIA(5) .GT. CHIA(1) ) GO TO 105
                  I1 = 1
                  I2 = 5
                  I3 = 4
               ENDIF
            ELSE
               I1 = 3
               I2 = 1
               I3 = 4
            ENDIF
C           bad configuration ?  No
         ENDIF
C        Iterations.  =============================
C
C        I1, I2, I3 are 3 steps such as CHIA(I1) > CHIA(I2) < CHIA(I3)
   73    CONTINUE
         KPAS = 1
C        New step between I1 and I2.
         IF( ITER .GE. MXIT ) GO TO 105
         SVAL( ITER+1 ) = .5 * ( SVAL(I1) + SVAL(I2) )
C
C        Chi2 calculation.
   74    CONTINUE
         ITER = ITER + 1
         DO 77 I = 1 , 6
            ETAG(I) = SLIM(I) - SVAL(ITER)
   77    CONTINUE
         IF( ETAG(1) .LT. .001 ) ETAG(1) = .001
C
C        Alpha and beta parameters estimation.
         CALL EBPARA( STAC , ETAG , ALPH , BETA , IBON )
         IF ( IBON .GT. 0 ) GO TO 105
C
C        Deviation from the mean shower.
         CALL EBDEVI( NATU , ENER , ALPH , BETA , ECAR(1,ITER) )
         CHIA(ITER) = ECAR(1,ITER) ** 2 + ECAR(2,ITER) ** 2
C
C        New system of 3 points.
C        Pass number : KPAS
         IF( CHIA(ITER) .LE. CHIA(I2) ) THEN
            IF( KPAS .EQ. 1 ) THEN
               I3 = I2
            ELSE
               I1 = I2
            ENDIF
            I2 = ITER
         ELSE
C        chia(iter) > chia(i2)
            IF( KPAS .EQ. 1 ) THEN
               KPAS = 2
               IF( ITER .GE. MXIT ) GO TO 105
               SVAL( ITER+1 ) = .5 * ( SVAL(I3) + SVAL(I2) )
               IT1 = ITER
               GO TO 74
            ELSE
C           kpas .ne. 1
               I1 = IT1
               I3 = ITER
            ENDIF
         ENDIF
C
         IF( .5*(CHIA(I1)+CHIA(I3)) - CHIA(I2) .GT. EPSC  .AND.
     +      ABS( SVAL(I3) - SVAL(I1) ) .GT. VMIN ) GO TO 73
C
C        Minimum is found.
   72    CONTINUE
         SHAP(1) = SVAL( I2 )
         SHAP(2) = SIGN( SQRT( CHIA(I2) ) , ECAR(2,I2) )
         ITY3 = 0
C
C
      ELSE
C
C     KLAS .ne. 7   :   1 or 2 stacks above the threshold.
C
         IF( TETA .LT. THE1 ) THEN
            ISC = 1
         ELSE
            IF( TETA .GT. THE3 ) THEN
               ISC = 3
            ELSE
               ISC = 2
            ENDIF
         ENDIF
         CALL EBCOSI( TETA , PHI , ISC , COSI )
         AINE = 1. / COSI - 1.
         ENLG = ALOG( ENER )
C
         IF( KLAS .LE. 3 .OR. KLAS .EQ. 7 ) THEN
C        KLAS = 1 , 2 or 3 , STAC(3) < .070 Gev.
C        KLAS = 7 and SLIM(5) = SLIM(6)
            K = 1
         ELSE
C        KLAS = 4 or 6 , STAC(1) < .050 Gev.
            K = 3
         ENDIF
C
C        Materialisation depth computed from a parametrisation.
   78    CONTINUE
         PRP = STAC(2) / ( STAC(2) + STAC(K) )
         ACO = CMT(1,K) + AINE * CMT(2,K) + ENLG * CMT(3,K) +
     +        AINE * ENLG * CMT(4,K)
         K = K + 1
         BCO = CMT(1,K) + AINE * CMT(2,K) + ENLG * CMT(3,K) +
     +        AINE * ENLG * CMT(4,K)
         SHAP(1) =  ACO + BCO * PRP
         IF( ABS( SHAP(1)). GT. XMAX ) SHAP(1) = SIGN( XMAX , SHAP(1) )
         IF( KLAS .NE. 4 .AND. KLAS .NE. 6 ) SHAP(1) = EXP( SHAP(1) )
         IF( KLAS .EQ. 1 .AND. STAC(2) .EQ. 0. ) SHAP(1) = .5 * SHAP(1)
         IF( KLAS .EQ. 1 .AND. SHAP(1) .GT. 1. ) SHAP(1) = 1.
         SHAP(2) = 0.
         ITY3 = 1
C
C
      ENDIF
C
      IER = JER
      IF( ABS( SHAP(2) ) .GT. 10. ) SHAP(2) = SIGN( 10. , SHAP(2) )
      GO TO 98
C
C   Uncalculable identifiers. Has to be considered as a hadron.
  105 CONTINUE
      IER = JER
      SHAP(1) = 0.
      SHAP(2) = 10.
      ITY3 = 2
      GO TO 98
C ================== error ==================================
C   The EGMD bank don't exist.
  101 CONTINUE
      IER = 1
      GO TO 97
C   Energy out of range.
  102 CONTINUE
      IER = 2
      GO TO 97
  104 CONTINUE
      IER = JER
   97 SHAP(2) = 0.
      SHAP(1) = 0.
      ITY3 = 3
   98 CONTINUE
      RETURN
      END
#endif


