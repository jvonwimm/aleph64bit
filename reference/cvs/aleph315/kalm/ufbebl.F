      DOUBLE PRECISION FUNCTION UFBEBL(BETA_GAMMA, MASS, MATERIAL, MODE)
C!  Function to calculate energy loss for Kalman filter.
C!
C!  Uses Bethe-Bloch expression from Particle Data Book, and references
C!
C!  Input:      BETA_GAMMA      Beta * Gamma
C!              MASS            Particle mass hypothesis in GeV
C!              MATERIAL        Material code:
C!                                              1       Silicon
C!                                              2       Carbon
C!                                              3       Argon
C!                                              4       Beryllium
C!              MODE            Energy loss mode:
C!                                              0       NO energy loss
C!                                              1       Use BB curve
C!                                              2       Scale by measured
C!                                                      ionization
C!  Output:     UFBEBL          Energy loss per radiation length (in GeV)
C!
#ifndef DOC

      DOUBLE PRECISION BETA_GAMMA, MASS
      INTEGER MATERIAL

      PARAMETER (N_MATERIAL = 4)
      DOUBLE PRECISION I_TAB(N_MATERIAL), A_TAB(N_MATERIAL),
     1          Z_TAB(N_MATERIAL), E_TAB(N_MATERIAL),
     1          X0_TAB(N_MATERIAL),
     2          M_E, MIN_ION(N_MATERIAL)
      DOUBLE PRECISION I, A, Z, E_PLASMA, X0

      DATA I_TAB/173.D0, 78.D0, 188.D0, 63.7D0/       !Excitation energy(eV)
      DATA A_TAB/28.09D0, 12.01D0, 39.95D0, 9.01D0/   !Atomic mass
      DATA Z_TAB/14.D0, 6.D0, 18.D0, 4.0D0/           !Atomic number
      DATA E_TAB/31.055D0, 30.652D0, 0.789D0, 26.098D0/ !Plasma energy(eV)
      DATA X0_TAB/21.82D0, 42.70D0, 19.55D0, 65.19D0/ !Rad length (g/cm^2)
      DATA M_E /0.510999D-3/                          !electron mass
      DATA MIN_ION/0.001664D0, 0.001745D0, 0.001519D0, 0.001594D0/
                                                      !dE/dx at min.ionization
      DOUBLE PRECISION BETA, GAMMA, DELTA_HALF, T_MAX

      UFBEBL = 0.D0

      IF (MODE.EQ.0)RETURN

      IF(MATERIAL.GT.N_MATERIAL .OR. MATERIAL.LE.0)THEN
        CALL ALTELL('UFBEBL - invalid material code',MATERIAL,'RETURN')
        RETURN
      ENDIF

      X0 = X0_TAB(MATERIAL)
C      IF(MODE .EQ. 2)THEN  !Scaled energy loss mode
C        UFBEBL = X0 * MIN_ION(MATERIAL) * DBLE(RI_KALMAN)
C        RETURN
C      ENDIF

      IF(BETA_GAMMA .LT. 0.D0)THEN
        CALL ALTELL('UFBEBL - negative momentum track',0,'RETURN')
        UFBEBL = 0.D0
        RETURN
      ENDIF

      I = I_TAB(MATERIAL)
      A = A_TAB(MATERIAL)
      Z = Z_TAB(MATERIAL)
      E_PLASMA = E_TAB(MATERIAL)

      BETA = BETA_GAMMA/SQRT(1.D0+BETA_GAMMA**2)
      GAMMA = 1.D0/SQRT(1.D0+BETA_GAMMA**2)

      DELTA_HALF = DLOG(E_PLASMA/I * BETA_GAMMA) - 0.5D0
      IF (DELTA_HALF .LT. 0.D0) DELTA_HALF = 0.D0

      T_MAX = 2.D0*M_E*BETA_GAMMA**2/(1.D0+2.
     &  D0*GAMMA*M_E/MASS+(M_E/MASS)**2)

      UFBEBL = X0 * ((0.307075D-3)*Z/A/BETA**2) *
     1  ( 0.5D0*DLOG((2.D0*M_E*BETA_GAMMA**2)*T_MAX / (I/1.D9)**2)
     2  - BETA**2 - DELTA_HALF )

      RETURN
      END
#endif
