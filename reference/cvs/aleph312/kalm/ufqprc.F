      SUBROUTINE UFQPRC(FIELD, INNER, OUTER, DBL_TRK, R_COORD, XY_ARC,
     +                  X_SMOOTH, AT_VTX, MASS,
     +                  Q_PROC, DO_ELOSS_IN, ELOSS)
C!  Create process noise array and energy loss for Kalman filter
#ifndef DOC
      INTEGER INNER, OUTER

      DOUBLE PRECISION OMEGA_TRK,D0_TRK,PHI0_TRK,TANL_TRK,Z0_TRK
      DOUBLE PRECISION DBL_TRK(5)

      DOUBLE PRECISION R_COORD(*), XY_ARC(*), X_SMOOTH(5,*), Q_PROC(5,5,
     &  *), ELOSS(2,*), MASS
      INTEGER DO_ELOSS_IN
      LOGICAL AT_VTX

#include "vrldcom.h"
#include "uftcom.h"

      DOUBLE PRECISION Q_LOCAL(2,2), R_IN, R_OUT, L_ARC, L_TPC,
     +                 L_ITC, ARC_TPC, ARC_ITC, SEC2_LAM, SCAT_CON,
     +                 L_WALL, SEC_LAM, SUM, DEN, SIN_LAM, SEC_DPHI,
     +                 SEC_LOC, TAN_DPHI, FACT, P_TRK, BETA, GAMMA,
     +                 BETA_GAMMA, E_TRK, Q_DEDX


      DOUBLE PRECISION UFBEBL, UFBERR
      DOUBLE PRECISION DE_DX, X_RAD, DE, DE_TOT(2), SIG_DE, L_CORR,
     &   DSIG_DE

C  Load local variables

      OMEGA_TRK = DBL_TRK(1)
      TANL_TRK = DBL_TRK(2)
      PHI0_TRK = DBL_TRK(3)
      D0_TRK = DBL_TRK(4)
      Z0_TRK = DBL_TRK(5)
      IF(FIXED_OME .OR. FIXED_P)THEN
        DO_ELOSS = 0
      ELSE
        DO_ELOSS = DO_ELOSS_IN
      ENDIF

C  Zero out multiple scattering covariance matrices

      DO  I = OUTER, INNER
        CALL DVZERO(Q_PROC(1,1,I),25)
        ELOSS(1,I) = 0.D0
        ELOSS(2,I) = 0.D0
      ENDDO

C Get secant^2 of dip angle and scattering constant

      SEC2_LAM = 1.D0 + TANL_TRK**2
      SEC_LAM = DSQRT(SEC2_LAM)
      SCAT_CON = (DBLE(SCATTR)*OMEGA_TRK)**2 / SEC2_LAM

C  Beta of track for dE/dx (assuming pion hypothesis)

      P_TRK = DABS(1.D0/(DBLE(ROVERP) * OMEGA_TRK / SEC_LAM))
      BETA = P_TRK/DSQRT(P_TRK**2 + MASS**2)
      BETA_GAMMA = P_TRK/MASS
      GAMMA = BETA_GAMMA/BETA
      E_TRK = MASS * GAMMA

C  Correct scattering for low momentum tracks using beta

      SCAT_CON = SCAT_CON / BETA**2

C  Loop over coordinate pairs

      DO  I = OUTER, INNER

C  Zero out energy loss variables

        DE_TOT(1) = 0.D0
        DE_TOT(2) = 0.D0
        SIG_DE = 0.D0

C  Only calculate scattering and energy loss if swimming to vertex

         IF ( AT_VTX .OR. I.NE.INNER )THEN
          IF ( I .LT. INNER ) THEN
            R_IN = R_COORD(I+1)
          ELSE
            R_IN = MAX(DBLE(RAD_CUT), ABS( D0_TRK ) + 0.00001D0)
          ENDIF
          R_OUT = R_COORD(I)
          Q_LOCAL(1,1) = 0.D0
          Q_LOCAL(1,2) = 0.D0
          Q_LOCAL(2,1) = 0.D0
          Q_LOCAL(2,2) = 0.D0

          L_ARC = XY_ARC(I)
          IF (I.LT.INNER) L_ARC = XY_ARC(I)-XY_ARC(I+1)

C  Calculate multiple scattering in each region.
C  Gas of tpc and tpc/itc wall:

          L_TPC = 0.D0
          IF ( R_OUT .GE. DBLE(UKRTPC) ) THEN
            L_TPC = L_ARC
            IF ( R_IN .LE. DBLE(UKRTPC) ) THEN
              ARC_TPC = 2.D0/OMEGA_TRK*DASIN( 0.5D0*OMEGA_TRK*
     +                     DSQRT(DABS((DBLE(UKRTPC)**2-D0_TRK**2)/
     +                     (1.D0-OMEGA_TRK*D0_TRK))) )
C  L_TPC in cm
              L_TPC = XY_ARC(I)-ARC_TPC
            ENDIF
            IF (R_IN .LT. DBLE(UKRTPC))THEN
              L_WALL = DBLE(UKSTPC) * DBLE(UKRTPC)*OMEGA_TRK*SEC_LAM/
     +                           DSIN(OMEGA_TRK*ARC_TPC)/
     +                           (1.D0-D0_TRK*OMEGA_TRK)
C  L_WALL in rad. lengths
            ELSE
              L_WALL = 0.D0
            ENDIF
            L_TPC = L_TPC * SEC_LAM
C  L_TPC in cm
            X_RAD = L_TPC * DBLE(UKSPTPC)
C  X_RAD in rad lengths

C  A correction term to the mean multiple scattering based on the total
C  thickness of material passed through.

            IF(X_RAD.GT.0.)THEN
              L_CORR = MAX(0.7D0,1.D0 + 0.038D0*DLOG(X_RAD))**2
            ELSE
              L_CORR = 1.D0
            ENDIF
            Q_LOCAL(1,1) = L_CORR*SCAT_CON*DBLE(UKSPTPC)*L_TPC**3/3.D0
            Q_LOCAL(1,2) = L_CORR*SCAT_CON*DBLE(UKSPTPC)*L_TPC*L_TPC*0.
     &        5D0
            Q_LOCAL(2,1) = Q_LOCAL(1,2)
            Q_LOCAL(2,2) = L_CORR*SCAT_CON*DBLE(UKSPTPC)*L_TPC

C  Energy loss per radiation length in TPC gas

            IF(L_TPC .GT. 0.)THEN
C  Energy loss in TPC gas (GeV)
              DE_DX = UFBEBL(BETA_GAMMA, MASS, 3, DO_ELOSS)
              DE = DE_DX * X_RAD
              SIG_DE = SIG_DE + DE**2 * UFBERR(X_RAD)
              DE_TOT(2) = DE_TOT(2) + DE
            ENDIF

C  Add scattering in ITC/TPC wall, if applicable

            IF ( R_IN .LT. DBLE(UKRTPC) ) THEN
              L_CORR = MAX(0.7D0,1.D0 + 0.038D0*DLOG(L_WALL))**2
              Q_LOCAL(2,2) = Q_LOCAL(2,2) +
     +                          L_CORR*SCAT_CON*L_WALL
C  Energy loss in ITC/TPC wall

              DE_DX = UFBEBL(BETA_GAMMA, MASS, 2, DO_ELOSS)
              DE = DE_DX * L_WALL
              SIG_DE = SIG_DE + DE**2 * UFBERR( L_WALL )
              DE_TOT(1) = DE_TOT(1) + DE
            ENDIF
          ENDIF

C  Gas of itc and itc inner wall

          L_ITC = 0.D0
          IF ( R_IN .LT. DBLE(UKRTPC) ) THEN
             IF ( R_IN .LT. DBLE(UKRITC) .AND. R_OUT .GE. DBLE(UKRITC) )
     &        THEN
              ARC_ITC = 2.D0/OMEGA_TRK*DASIN( 0.5D0*OMEGA_TRK*
     +                          DSQRT(DABS((DBLE(UKRITC)**2-D0_TRK**2)/
     +                          (1.D0-OMEGA_TRK*D0_TRK))) )
              L_ITC = XY_ARC(I) - ARC_ITC
C  L_ITC in cm
              IF ( R_OUT .GT. DBLE(UKRTPC) ) L_ITC = ARC_TPC - ARC_ITC
              L_WALL = DBLE(UKSITC) * DBLE(UKRITC)*OMEGA_TRK*SEC_LAM/
     +                          DSIN(OMEGA_TRK*ARC_ITC)/
     +                          (1.D0-D0_TRK*OMEGA_TRK)
C  L_WALL in rad lengths
            ELSE IF ( R_IN .GE. DBLE(UKRITC) .AND. R_OUT .GE.
     &        DBLE(UKRTPC)) THEN
              L_ITC = L_ARC - XY_ARC(I) + ARC_TPC
              L_WALL = 0.D0
            ELSE IF ( R_IN .GE. DBLE(UKRITC) .AND. R_OUT .LT.
     &        DBLE(UKRTPC)) THEN
              L_ITC = L_ARC
              L_WALL = 0.D0
            ENDIF
            L_ITC = L_ITC * SEC_LAM
C  L_ITC in cm
            X_RAD = L_ITC * DBLE(UKSPITC)
C  X_RAD in rad lengths

C  A correction term to the mean multiple scattering based on the total
C  thickness of material passed through.

            IF(X_RAD.GT.0.)THEN
              L_CORR = MAX(0.7D0,1.D0 + 0.038D0*DLOG(X_RAD))**2
            ELSE
              L_CORR = 1.D0
            ENDIF
            Q_LOCAL(1,1) = Q_LOCAL(1,1) + 2.D0*L_ITC*Q_LOCAL(1,2)
     +                                  + L_ITC*L_ITC*Q_LOCAL(2,2)
            Q_LOCAL(1,2) = Q_LOCAL(1,2) + L_ITC*Q_LOCAL(2,2)
            Q_LOCAL(1,1) = Q_LOCAL(1,1) + L_CORR*SCAT_CON*DBLE(UKSPITC)
     &        *L_ITC**3/3.D0
            Q_LOCAL(1,2) = Q_LOCAL(1,2) + L_CORR*SCAT_CON*DBLE(UKSPITC)
     &        *L_ITC*L_ITC*0.5D0
            Q_LOCAL(2,1) = Q_LOCAL(1,2)
            Q_LOCAL(2,2) = Q_LOCAL(2,2) + L_CORR*SCAT_CON*DBLE(UKSPITC)
     &        *L_ITC

C  Energy loss per radiation length in ITC gas

            IF (L_ITC .GT. 0.)THEN
              DE_DX = UFBEBL(BETA_GAMMA, MASS, 3, DO_ELOSS)
C  Energy loss in ITC gas (GeV)
              DE = DE_DX * X_RAD
              SIG_DE = SIG_DE + DE**2 * UFBERR(X_RAD)
              DE_TOT(2) = DE_TOT(2) + DE
            ENDIF

C  Add scattering in ITC inner wall, if appropriate

            IF ( R_IN .LT. DBLE(UKRITC) .AND. R_OUT .GE. DBLE(UKRITC) )
     &        THEN
              L_CORR = MAX(0.7D0,1.D0 + 0.038D0*DLOG(L_WALL))**2
              Q_LOCAL(2,2) = Q_LOCAL(2,2) +
     +                          L_CORR*SCAT_CON*L_WALL
C  Energy loss in ITC inner wall

              DE_DX = UFBEBL(BETA_GAMMA, MASS, 2, DO_ELOSS)
              DE = DE_DX * L_WALL
              SIG_DE = SIG_DE + DE**2 * UFBERR(L_WALL)
              DE_TOT(1) = DE_TOT(1) + DE
            ENDIF
          ENDIF

C  Multiple scattering in VDET

          IF ( R_OUT .GT. DBLE(UKRITC) ) R_OUT = DBLE(UKRITC)

C  If no swimming is required flag last coordinate by negative radius

          IF ( R_OUT .GT. R_IN ) THEN
            CALL UFVDMM( FIELD, OMEGA_TRK, TANL_TRK, PHI0_TRK, D0_TRK,
     +                  Z0_TRK, R_IN, R_OUT, MASS, Q_LOCAL, DO_ELOSS,
     +                  DE, DSIG_DE )
            DE_TOT(1) = DE_TOT(1) + DE
            SIG_DE = SIG_DE + DSIG_DE
          ENDIF

C  Convert covariance matrices into local helix coordinate system

          IF ( I .EQ. INNER ) THEN

C  These are the multiple scattering parameters for the global fit parameters
C  on the way to the origin.

            SUM = 1.D0 - OMEGA_TRK*D0_TRK/SEC2_LAM
            DEN = 1.D0 - D0_TRK*OMEGA_TRK
            SIN_LAM = 1.D0/DSQRT(1.D0+1.D0/TANL_TRK**2)
            IF ( TANL_TRK .LT. 0.D0 ) SIN_LAM = - SIN_LAM
            IF ( .NOT. FIXED_OME ) THEN
              Q_PROC(1,1,I) = (OMEGA_TRK*TANL_TRK)**2*Q_LOCAL(2,2)
              IF (DO_ELOSS .NE. 0) THEN
                Q_DEDX = SIG_DE*E_TRK**2/(DBLE(ROVERP)
     &            **2*OMEGA_TRK*P_TRK**4)**2
                Q_PROC(1,1,I) = Q_PROC(1,1,I) + Q_DEDX
              ENDIF
              IF ( .NOT. FIXED_TANL ) THEN
                Q_PROC(1,2,I) = OMEGA_TRK*TANL_TRK*SEC2_LAM *
     +                          Q_LOCAL(2,2)
                Q_PROC(2,1,I) = Q_PROC(1,2,I)
              ENDIF
              Q_PROC(1,3,I) = -OMEGA_TRK**2*TANL_TRK*SIN_LAM/DEN *
     +                          Q_LOCAL(1,2)
              Q_PROC(3,1,I) =  Q_PROC(1,3,I)
              Q_PROC(1,5,I) = -OMEGA_TRK*TANL_TRK*SUM/DEN*SEC_LAM *
     +                          Q_LOCAL(1,2)
              Q_PROC(5,1,I) = Q_PROC(1,5,I)
            ENDIF
            IF ( .NOT. FIXED_TANL )  THEN
              Q_PROC(2,2,I) = SEC2_LAM**2 * Q_LOCAL(2,2)
              Q_PROC(2,3,I) = -OMEGA_TRK*SIN_LAM*SEC2_LAM/DEN *
     +                                  Q_LOCAL(1,2)
              Q_PROC(3,2,I) = Q_PROC(2,3,I)
              Q_PROC(2,5,I) = -SEC_LAM**3*SUM/DEN * Q_LOCAL(1,2)
              Q_PROC(5,2,I) = Q_PROC(2,5,I)
            ENDIF
            Q_PROC(3,3,I) = (SEC_LAM/DEN)**2 * Q_LOCAL(2,2) +
     +                   (OMEGA_TRK*SIN_LAM/DEN)**2 * Q_LOCAL(1,1)
            Q_PROC(3,4,I) = +SEC_LAM/DEN * Q_LOCAL(1,2)
            Q_PROC(4,3,I) = Q_PROC(3,4,I)
            Q_PROC(3,5,I) = D0_TRK*TANL_TRK*SEC2_LAM/DEN**2 *
     +                  Q_LOCAL(2,2) + OMEGA_TRK * TANL_TRK *
     +                  SUM/DEN**2*Q_LOCAL(1,1)
            Q_PROC(5,3,I) = Q_PROC(3,5,I)
            Q_PROC(4,4,I) = Q_LOCAL(1,1)
            Q_PROC(4,5,I) = +D0_TRK*TANL_TRK*SEC_LAM/DEN *
     +                  Q_LOCAL(1,2)
            Q_PROC(5,4,I) = Q_PROC(4,5,I)
            Q_PROC(5,5,I) = (D0_TRK*TANL_TRK*SEC_LAM/DEN)**2 *
     +              Q_LOCAL(2,2) + (SEC_LAM*SUM/DEN)**2 * Q_LOCAL(1,1)
          ELSE

C  This is for local helix parameters

            SEC_DPHI = 1.D0/DCOS(X_SMOOTH(1,I+1)/R_COORD(I+1) -
     +                  X_SMOOTH(3,I+1))
            SEC_LOC = 1.D0/DCOS(X_SMOOTH(4,I+1))
            SIN_LAM = DSIN(X_SMOOTH(4,I+1))
            TAN_DPHI  = DTAN(X_SMOOTH(1,I+1)/R_COORD(I+1) -
     +                  X_SMOOTH(3,I+1))
            FACT = DTAN(X_SMOOTH(4,I+1)) * TAN_DPHI

C  Note: ms is being done backwards ===> angles have to be counted negative

            Q_PROC(1,1,I) =  Q_LOCAL(1,1) * SEC_DPHI * SEC_DPHI
            Q_PROC(1,3,I) = -Q_LOCAL(1,2) * SEC_DPHI * SEC_LOC
     +               - Q_LOCAL(1,1)*X_SMOOTH(5,I+1)*SEC_DPHI*TAN_DPHI
            Q_PROC(3,1,I) =  Q_PROC(1,3,I)
            Q_PROC(3,3,I) = Q_LOCAL(2,2)*SEC_LOC**2 +
     +                  X_SMOOTH(5,I+1)**2 *
     +                  (SIN_LAM**2+TAN_DPHI**2) * Q_LOCAL(1,1) +
     +                  2.D0 * X_SMOOTH(5,I+1) * TAN_DPHI * SEC_LOC *
     +                  Q_LOCAL(1,2)

C  Multiple scattering in lambda only if lambda is not fixed

            Q_PROC(2,2,I) =  Q_LOCAL(1,1) *
     +                  ( SEC_LOC*SEC_LOC + FACT*FACT )
            IF ( .NOT. FIXED_TANL ) THEN
              Q_PROC(2,4,I) = -Q_LOCAL(1,2) * SEC_LOC
              Q_PROC(4,2,I) =  Q_PROC(2,4,I)
              Q_PROC(4,4,I) =  Q_LOCAL(2,2)
              Q_PROC(3,4,I) = -X_SMOOTH(5,I+1)*SIN_LAM *
     +                          Q_LOCAL(1,2)
              Q_PROC(4,3,I) =  Q_PROC(3,4,I)
            ENDIF
            Q_PROC(2,3,I) = +Q_LOCAL(1,2)*FACT*SEC_LOC +
     +                  Q_LOCAL(1,1)*X_SMOOTH(5,I+1) *
     +                  DTAN(X_SMOOTH(4,I+1))*SEC_DPHI**2
            Q_PROC(3,2,I) =  Q_PROC(2,3,I)
            Q_PROC(1,2,I) = -Q_LOCAL(1,1)*FACT*SEC_DPHI
            Q_PROC(2,1,I) =  Q_PROC(1,2,I)

C  Correlated change in curvature if omega is not fixed

            IF ( .NOT. FIXED_OME ) THEN
              FACT = X_SMOOTH(5,I+1) * DTAN(X_SMOOTH(4,I+1))
              Q_PROC(5,5,I) = FACT*FACT*Q_PROC(4,4,I)
              IF (DO_ELOSS .NE. 0) THEN
                Q_DEDX = SIG_DE*E_TRK**2/(DBLE(ROVERP)
     &            **2*OMEGA_TRK*P_TRK**4)**2
                Q_PROC(5,5,I) = Q_PROC(5,5,I) + Q_DEDX
              ENDIF
              Q_PROC(5,4,I) = FACT*Q_PROC(4,4,I)
              Q_PROC(4,5,I) = Q_PROC(5,4,I)
              Q_PROC(5,2,I) = FACT*Q_PROC(4,2,I)
              Q_PROC(2,5,I) = Q_PROC(5,2,I)
              Q_PROC(5,3,I) = FACT*Q_PROC(4,3,I)
              Q_PROC(3,5,I) = Q_PROC(5,3,I)
            ENDIF
          ENDIF
        ENDIF
        ELOSS(1,I) = DE_TOT(1)
        ELOSS(2,I) = DE_TOT(2)
      ENDDO

      RETURN
      END
#endif
