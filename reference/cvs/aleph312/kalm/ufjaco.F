      INTEGER FUNCTION UFJACO(R_OLD,R_NEW,STATE,DFDX)
C! Get Jacobian of transport from one radius to another
#ifndef DOC
      IMPLICIT NONE

      INTEGER I, J

      DOUBLE PRECISION R_OLD, R_NEW, STATE(5), DFDX(5,5)
      DOUBLE PRECISION T, DELTA_PHI, DT_DRPHI, DT_DPHI, DT_DOMEGA
      DOUBLE PRECISION PHI_NEW, FACTOR, DENOM
      DOUBLE PRECISION INV_R_OLD, INV_STATE5, SINPHINEW

      INTEGER UTPROC

C  Initialize Jacobian
C don't bother using the symmetry; the optimizer can do better if the
C     loop limits are fixed.
      DO I = 1, 5
        DO J = 1, 5
          DFDX(J,I) = 0.D0
        END DO
        DFDX(I,I) = 1.D0
      ENDDO

C  Get the advance in process time

      UFJACO = UTPROC(R_OLD,R_NEW,STATE,T)
      IF(UFJACO .NE. 0) RETURN

C Calculate the Jacobian

C double precision divides are 2-5 times slower than multiplies, so
C     create some temps.
      UFJACO = -1
      IF (ABS(R_OLD).LT.1.E-15 .OR. ABS(STATE(5)).LT.1.E-15) RETURN
      INV_R_OLD = 1.0D0 / R_OLD
      INV_STATE5 = 1.0D0 / STATE(5)

      DELTA_PHI = STATE(3) - STATE(1) * INV_R_OLD
      DENOM = ( DSIN(T)*INV_STATE5 + R_OLD*DCOS( DELTA_PHI + T ) )
      IF (DENOM .EQ. 0.) RETURN
      DT_DRPHI = ( DCOS( DELTA_PHI + T ) - DCOS(DELTA_PHI) ) / DENOM
      DT_DPHI = - R_OLD * DT_DRPHI
      DENOM =   STATE(5) * ( DSIN(T)*INV_STATE5 + 
     +              R_OLD*DCOS(DELTA_PHI + T ) ) 
      IF (DENOM .EQ. 0.) RETURN
      DT_DOMEGA = ( 4.D0 * DSIN(T/2.D0)**2 * INV_STATE5
     +             + R_OLD*(DSIN(DELTA_PHI + T)-DSIN(DELTA_PHI)) ) /
     +               DENOM

      PHI_NEW = STATE(3) + T
      SINPHINEW = DSIN(PHI_NEW)
      DENOM = R_OLD*DCOS(STATE(1)*INV_R_OLD)-(DSIN(STATE(3))
     &  - SINPHINEW)*INV_STATE5
      IF (DENOM .EQ. 0.) RETURN
      FACTOR = R_NEW / DENOM

      DFDX(1,1) = FACTOR*( DCOS( STATE(1)*INV_R_OLD ) +
     $     DT_DRPHI * SINPHINEW * INV_STATE5 )
      DFDX(1,3) = FACTOR*INV_STATE5*( (1.D0+DT_DPHI)*SINPHINEW -
     $     DSIN(STATE(3)) )
      DFDX(1,5) = FACTOR*INV_STATE5*( (DCOS(STATE(3)+T)
     +     - DCOS(STATE(3)) )*INV_STATE5 + SINPHINEW * DT_DOMEGA )

      FACTOR = DTAN(STATE(4))*INV_STATE5

      DFDX(2,1) = FACTOR * DT_DRPHI
      DFDX(2,3) = FACTOR * DT_DPHI
      DENOM = STATE(5) * (DCOS(STATE(4)))**2
      IF (DENOM .EQ. 0.) RETURN
      DFDX(2,4) = T / DENOM
      DFDX(2,5) = FACTOR * ( DT_DOMEGA - T*INV_STATE5 )
      DFDX(3,1) = DT_DRPHI
      DFDX(3,3) = 1.D0 + DT_DPHI
      DFDX(3,5) = DT_DOMEGA

      UFJACO = 0
      RETURN
      END
#endif
