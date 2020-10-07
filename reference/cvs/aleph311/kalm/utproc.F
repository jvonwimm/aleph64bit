      INTEGER FUNCTION UTPROC(R_OLD,R_NEW,STATE,T)
C!  Calculate change in process time from one radius to another
#ifndef DOC
      DOUBLE PRECISION R_OLD,R_NEW,STATE(*),T,TOLD

      DOUBLE PRECISION RHO, DELTA_PHI, DELTA_R2
      DOUBLE PRECISION A,B,C,DISCR
      DOUBLE PRECISION COST, SINT

      DELTA_R2 = R_NEW**2 - R_OLD**2
      DELTA_PHI = STATE(3) - STATE(1)/R_OLD
      RHO = (1.D0/STATE(5) - R_OLD*DSIN(DELTA_PHI))
      A = DELTA_R2-2.D0*RHO/STATE(5)
      B = 2.D0*RHO/STATE(5)
      C = 2.D0*R_OLD*DCOS(DELTA_PHI)/STATE(5)

      DISCR = C**2 + B**2 - A**2
      IF (DISCR .LT. 0.D0)THEN
        UTPROC = 2
        RETURN
      ELSE
        DISCR = DSQRT(DISCR)
        IF (STATE(5).GT.0.)THEN
            T = 2.D0*DATAN((C-DISCR)/(A-B))
        ELSE
            T = 2.D0*DATAN((C+DISCR)/(A-B))
        ENDIF
c        IF (STATE(5)*(R_OLD-R_NEW) .GT. 0.D0)THEN
c          COST = (-A*B + C * DISCR)!/(B**2 + C**2)
c          SINT = (-A*C - B * DISCR)!/(B**2 + C**2)
c          TOLD = -DATAN2(SINT,COST)
c          IF (TOLD*STATE(5)*(R_OLD-R_NEW).GT.0.D0)THEN
c            COST = (-A*B - C * DISCR)!/(B**2 + C**2)
c            SINT = (-A*C + B * DISCR)!/(B**2 + C**2)
c            TOLD = -DATAN2(SINT,COST)
C            TYPE *,'IF 1'
c          ENDIF
c        ELSE
c          COST = (-A*B - C * DISCR)!/(B**2 + C**2)
c          SINT = (-A*C + B * DISCR)!/(B**2 + C**2)
c          TOLD = -DATAN2(SINT,COST)
c          IF (TOLD*STATE(5)*(R_OLD-R_NEW).GT.0.D0)THEN
c            COST = (-A*B + C * DISCR)!/(B**2 + C**2)
c            SINT = (-A*C - B * DISCR)!/(B**2 + C**2)
c            TOLD = -DATAN2(SINT,COST)
C            TYPE *,'IF 2'
c          ENDIF
c        ENDIF
c        IF (ABS(T-TOLD).GT.1.D-06)THEN
c            TYPE *,SNGL(R_NEW-R_OLD),SNGL(STATE(5)),
c     &          SNGL(DTAN(STATE(4))),SNGL(TOLD), SNGL(T)
c        ENDIF
      ENDIF
      UTPROC = 0
      RETURN
      END
#endif
