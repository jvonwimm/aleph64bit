        SUBROUTINE TFCHEK(VV0,LTASSO,IERR)
C------------------------------------------------------------
C
C! Check TPC helix parameters for basic sanity
C
C  Input:       VV0(5)    helix fit(R*4)
C               LTASSO    helix convention(L)
C                         .TRUE. if TASSO convention used
C                         .FALSE. if ALEPH (standard) convention used
C  Output:      IERR      error code (I*4)
C                         0 = no problem
C                  non-zero = fit appears to be garbage
C
C               D. Casper  09/07/97
C
C-----------------------------------------------------------
#ifndef DOC
#include "alcons.h"
        parameter (DMAX = 180., ZMAX=225., PMAX=1000.)
        DIMENSION VV0(*)
        LOGICAL LTASSO
        DIMENSION VV1(5)
        CHARACTER*40 REASON(6)
        DATA REASON/'(Field is zero)','(d0 too large)',
     &              '(1/Radius is zero)','(Momentum > 1 TeV)',
     &              '(Points away from Aleph)','(misses in z)'/
C
C  Get into a coordinate system we understand, if necessary
C
        IF (LTASSO) THEN
            VV1(1) = - VV0(1)
            VV1(2) = VV0(2)
            VV1(3) = VV0(3)
            VV1(4) = VV0(4)*SIGN(1.,VV1(1))
            VV1(5) = VV0(5)
        ELSE
            CALL UCOPY(VV0,VV1,5)
        ENDIF
        IERR = 1
C  Did dummy call us with no field?
        FIELD = ALFIEL(DUM)
        IF (FIELD.EQ.0) GOTO 999
        ROVERP = 1./(0.29979*FIELD/10.)*100.
C  Does the track pass within the TPC radius?
        IERR = 2
        IF (ABS(VV1(4)).GT.DMAX) GOTO 999
C  Does the track have an absurd momentum?
        IERR = 3
        IF (VV1(1).EQ.0.) GOTO 999
        IERR = 4
        PT = 1./(ROVERP*ABS(VV1(1)))
        XLAMBDA = ATAN(VV1(2))
        P = PT/COS(XLAMBDA)
        IF (P.GT.PMAX) GOTO 999
C  Does the track pass within the TPC endplates?
        IF (ABS(VV1(5)).GT.ZMAX) THEN
            IERR = 5
            IF (VV1(2)*VV1(5) .GE. 0.) GOTO 999
            IERR = 6
            RSQEND = VV1(4)**2 + 4.*((1.-VV1(4)*VV1(1))/VV1(1)**2)
     &          *SIN(VV1(1)*(ABS(VV1(5))-ZMAX)/(2.*VV1(2)))**2
            IF (RSQEND.GT.DMAX**2) GOTO 999
        ENDIF
C  Track is probably OK
        IERR = 0
        RETURN
C  Come here on trouble
 999    CONTINUE
        CALL RERROR('TFCHEK',IERR,
     &      'Track fails sanity check '//REASON(IERR))
        RETURN
        END
#endif
