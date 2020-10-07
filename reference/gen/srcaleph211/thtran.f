      SUBROUTINE THTRAN(P,ORIG,DPHI,PA)
C
C----------------------------------------------------------------------
C! Transform helix to another coordinate system with z axis parallel
C! to the original
CKEY TPCDES HELIX TRANSFORM / USER
C     Author:   R. Johnson   28-12-90
C
C     Input:
C       - P(5)       /R     Helix parameters in original frame
C                           1/r,tanl,phi0,d0,z0
C                           (d0>0 = positive ang. mom. about z axis)
C                           (r>0  = counterclockwise rotation)
C       - ORIG(3)    /R     Origin of the new coordinate system
C                           in the old system
C       - DPHI       /R     Phi of the x axis of the new coordinate
C                           system in the old system
C     Output:
C       - PA(5)     /R      Helix parameters in new frame.
C
C
C   Remarks:  only translations and rotations about the z axis are
C             possible in this case.  Rotations about x and y are
C             ignored since the helix model always assumes a helix
C             about the z axis.
C
C----------------------------------------------------------------------
      SAVE
C! define universal constants
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C
      DIMENSION P(*),PA(*),ORIG(*)
      DOUBLE PRECISION XC,YC
      DATA EPS/0.001/
C
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C
      DX=ORIG(1)
      DY=ORIG(2)
      DZ=ORIG(3)
      IF (DX.EQ.0. .AND. DY.EQ.0.
     &        .AND. DZ.EQ.0. .AND. DPHI.EQ.0.) THEN
        DO 11 I=1,5
          PA(I)=P(I)
   11   CONTINUE
      ELSE
        SP0=SIN(P(3))
        CP0=COS(P(3))
C
C++     Translate the point of closest approach to old z axis to
C++     a point in the new coordinate system
C
        X=P(4)*SP0 - DX
        Y=-P(4)*CP0 - DY
        Z= P(5) - DZ
C
C++     Get the radius of curvature
C
        IF (P(1).EQ.0.) THEN
          RHO=1.0E22*SGN
          SGN=1.0
          CALL ALTELL ('THTRAN: Inverse radius of curvature=0.',
     &                  0,'RETURN')
        ELSE
          RHO=1.0/P(1)
          SGN=SIGN(1.0,P(1))
        ENDIF
C
C++     Find the point of closest approach to the new z axis.
C++     Get D0.  For high momentum tracks, expand the sqare root
C++     in order to avoid numerical problems.
C
        B=(X**2 + Y**2)*(P(1)**2)
        A=2.0*P(1)*(CP0*Y - SP0*X)
        IF (ABS(A).LT.EPS) THEN
          D0=-RHO*0.5*(A+B)
        ELSEIF (ABS(B).LT.EPS) THEN
          RADC=SQRT(1.0+A)
          D0=RHO*(1.0-RADC-0.5*B/RADC)
        ELSE
          D0=RHO*(1.0-SQRT(1.0+A+B))
        ENDIF
C
C++     Find the center of the circle and get phi0
C
        XC=-DBLE(RHO*SP0)+DBLE(X)
        YC= DBLE(RHO*CP0)+DBLE(Y)
        PHIC=DATAN2(YC,XC)
        PHI0=PHIC-SGN*PIBY2
        IF (PHI0.LT.0.) THEN
          PHI0=PHI0+TWOPI
        ELSEIF (PHI0.GE.TWOPI) THEN
          PHI0=PHI0-TWOPI
        ENDIF
C
C++     Find the turning angle from the old point to the new
C
        ALPHA=P(3)-PHI0
        IF (ALPHA.LT.-PI) THEN
          ALPHA=ALPHA+TWOPI
        ELSEIF (ALPHA.GE.PI) THEN
          ALPHA=ALPHA-TWOPI
        ENDIF
C
C++     Get the new Z at the point of closest approach
C
        Z0= Z - RHO*ALPHA*P(2)
C
C++     Finally rotate around new z axis and output results
C
        PA(1)=P(1)
        PA(2)=P(2)
        PA(3)=PHI0-DPHI
        PA(4)=D0
        PA(5)=Z0
      ENDIF
C
      END
