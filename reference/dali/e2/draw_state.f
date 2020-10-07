      REAL FUNCTION GET_KALMAN_T(I)

      PARAMETER (MAX_PTS = 40)
      COMMON / INFDAL / FIRST_COORD, LAST_COORD, X_SMOOTH, R_MEAS,
     +                 R_COORD, COORD_USED, V_MEAS

      INTEGER FIRST_COORD, LAST_COORD
      DOUBLE PRECISION X_SMOOTH(5,MAX_PTS), R_MEAS(2,MAX_PTS),
     +                 R_COORD(MAX_PTS), V_MEAS(2,2,MAX_PTS)
      INTEGER COORD_USED(MAX_PTS)


      DOUBLE PRECISION T_PROC, LAST_R, X_USE(5)
      INTEGER UTPROC

C  IF I > # coords, return zeroes

      GET_KALMAN_T=0

      N_COORD = LAST_COORD - FIRST_COORD + 1
      IF( I .GE. N_COORD )THEN
        I = 0
        RETURN
      ENDIF

CCC   .........................      J = LAST_COORD - I + 1
      J = LAST_COORD - I +1
      LAST_R = R_COORD(J)

      IERR = UTPROC(R_COORD(J),R_COORD(J-1),X_SMOOTH(1,J),T_PROC)
      IF(IERR.NE.0) GOTO 999

      GET_KALMAN_T = SNGL(T_PROC)
Cold      CALL UMOVEX(X_SMOOTH(1,J),X_USE)
      CALL DVMOVE(X_SMOOTH(1,J),X_USE,5)
      I = I + 1
      RETURN

      ENTRY GET_KALMAN_X(T)

      GET_KALMAN_X = LAST_R * COS(X_USE(1)/LAST_R) + (SIN(X_USE(3)+T) -
     &  SIN(X_USE(3)))/X_USE(5)
      RETURN

      ENTRY GET_KALMAN_Y(T)

      GET_KALMAN_Y = LAST_R * SIN(X_USE(1)/LAST_R) + (-COS(X_USE(3)+T) +
     &  COS(X_USE(3)))/X_USE(5)
      RETURN

      ENTRY GET_KALMAN_Z(T)

      GET_KALMAN_Z = X_USE(2) + T/X_USE(5) * TAN(X_USE(4))
      RETURN

  999 CONTINUE
      CALL DWRT('Error in process time calculation#')
      RETURN
      END

