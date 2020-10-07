      SUBROUTINE TPFIDS(X,ITYPE,LIN)
C-----------------------------------------------------------------------
C!  Determine whether the point (x(1),x(2)) lies in the extended
C!  sector of type itype.
C
C  Called from:  TPGBRT
C  Calls:        None
C
C  Inputs:   PASSED:      --X,      the coordinates of a point
C                         --ITYPE,  the sector type under consideration
C            /SCTBND/     --x-limits, slopes, and intercepts of each
C                           boundary line of each type of extended
C                           sector; see TPBGEO
C
C  Outputs:  PASSED:      --LIN,    logical variable, true if X lies
C                                   within the extended sector, false
C                                   otherwise
C  A. Caldwell, D. DeMille
C-----------------------------------------------------------------------
      COMMON /SCTBND/ NLINES(3),SLOPES(10,3),YCEPTS(10,3),
     1                XUPLIM(10,3),XLWLIM(10,3),PHIMAX(3)
C
      DIMENSION X(2)
      LOGICAL LIN
C
      NINT = 0
      NLN = NLINES(ITYPE)
C
C  Loop over the line segments forming the boundary of the extended
C  sector to see if a ray drawn from X to infinity, parallel to the
C  Y-axis, crosses the line segment; if it does, increment NINT
C
      DO 10 I = 1, NLN
C
         IF ( X(1) .LT. XLWLIM(I,ITYPE) .OR. X(1) .GT. XUPLIM(I,ITYPE) )
     *      GOTO 10
         YINT = SLOPES(I,ITYPE) * X(1) + YCEPTS(I,ITYPE)
         IF ( YINT .GT. X(2) ) NINT = NINT + 1
C
 10   CONTINUE
C
C  Simple topology tells us whether X is in or out
C
      IF ( MOD( NINT, 2 ) .EQ. 0 ) THEN
         LIN = .FALSE.
      ELSE
         LIN = .TRUE.
      ENDIF
C
      RETURN
      END
