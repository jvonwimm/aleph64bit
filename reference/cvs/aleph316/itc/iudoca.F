      SUBROUTINE IUDOCA (X1,T1,X2,T2,DCA,DX1,DX2)
C----------------------------------------------------------------------
CKEY ITC
C! Calculates dist. of closest approach for two straight lines.
C
C UDOCA
C  If the lines are parallel, the doca is calcuated, but DX1 and DX2
C  are returned zero.
C
C ARGUMENTS:
C ----------
C   X1(3)               Point on first line
C   X2(3)               point on second line
C   T1(3)             Direction cosines for first line
C   T2(3)             direction cosines for second line
C   DCA                 distance of closest approach
C   DX1                 distance along line 1 from X1 to DOCA vector
C   DX2                 distance along line 2 from X2 to DACA vector
C
C
C Written by WBA 2-79, mocified by CCY 2-83 to take care of nearly
C parallel lines.
C
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
      REAL X1(*), T1(*), X2(*), T2(*), DCA, DX1, DX2
C INTERNAL VARIABLES
C    UNIT VECTOR ALONG DOCA VECTOR
C     VECTOR: X1-X2
C     NORMALIZED U CROSS T1
      REAL  V(3),  U(3),  W(3),  VNORM, VSQ, TT, XNORM, UT1, UT2
      INTEGER I, IER
C
C**********************************************************************
      TT=0.
      DCA=0.
      DX1=0.
      DX2=0.
      UT1 = 0.0
      UT2 = 0.0
      DO 21 I=1,3
         TT=TT+T1(I)*T2(I)
         U(I)=X1(I)-X2(I)
   21 CONTINUE
C
C Lines are parallel
      CALL IUVCRS(T1, T2, V, IER)
      IF (IER.EQ.2) THEN
         CALL IUVCRS(U,T1,W,IER)
         CALL IUVCRS(W,T1,V,IER)
      ENDIF
      VSQ=V(1)**2+V(2)**2+V(3)**2
      IF (VSQ .EQ. 0.0) THEN
         VNORM = 0.0
C   Doca is always zero if V is zero
      ELSE
         VNORM=1./SQRT(VSQ)
      END IF
      DO 31 I = 1,3
         V(I)=V(I)*VNORM
         DCA=DCA+U(I)*V(I)
         UT1=UT1+U(I)*T1(I)
         UT2=UT2+U(I)*T2(I)
   31 CONTINUE
C
      IF ((ABS(TT).LT.1.)) THEN
         XNORM=1./((1.-TT)*(1.+TT))
C More accurate form of (1-tt**2)
      ELSE
         XNORM = 0.0
      ENDIF
      DX1 = (TT*UT2 - UT1)*XNORM
C Symmetry of solution is obvious
      DX2 = -(TT*UT1 - UT2)*XNORM
C
      END
#endif
