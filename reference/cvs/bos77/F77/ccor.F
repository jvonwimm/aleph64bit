      SUBROUTINE CCOR(IND)
      COMMON/HCS/KW(100)
      REAL XW(1)
      EQUIVALENCE (XW(1),KW(1))
      EQUIVALENCE (R,K),(S,L)
      INTEGER IP1(10),IP2(10)
      REAL Y(315),X(315)
      DATA    IP1    /1,8,64,512,4096,32768,262144,2097152,
     +                16777216,134217728/
      DATA    IP2    /7,56,448,3584,28672,229376,1835008,14680064,
     +                117440512,939524096/
      N=KW(IND+21)
      K=IND+50
      DO 10 I=1,N
      Y(I)=XW(K+1)
      X(I)=XW(K+2)
      KW(K+1)=0
      KW(K+2)=0
   10 K=K+2
      IF(XW(IND+23).EQ.0.0) THEN
         CALL BINSX(Y,N,50,XW(IND+22),XW(IND+23))
         XW(IND+23)=(XW(IND+23)-XW(IND+22))/50.0
      END IF
      IF(XW(IND+25).EQ.0.0) THEN
         CALL BINSX(X,N,100,XW(IND+24),XW(IND+25))
         XW(IND+25)=(XW(IND+25)-XW(IND+24))/100.0
      END IF
      XW(IND+33)=XW(IND+22)+25.0*XW(IND+23)
      XW(IND+34)=XW(IND+24)+50.0*XW(IND+25)
      XW(IND+35)=Y(1)
      XW(IND+36)=Y(1)
      XW(IND+43)=X(1)
      XW(IND+44)=X(1)
      DO 20 I=1,N
      NY=(Y(I)-XW(IND+22))/XW(IND+23)
      NX=(X(I)-XW(IND+24))/XW(IND+25)
      IF(NX.LT.0.OR.NX.GT.99) NX=-51
      IF(NY.LT.0.OR.NY.GT.49) NY=-2
      XW(IND+35)=AMIN1(XW(IND+35),Y(I))
      XW(IND+36)=AMAX1(XW(IND+36),Y(I))
      XW(IND+43)=AMIN1(XW(IND+43),X(I))
      XW(IND+44)=AMAX1(XW(IND+44),X(I))
      IF(NX+51.EQ.0.OR.NY+2.EQ.0) GOTO 20
      KW(IND+NX+101)=KW(IND+NX+101)+1
      KW(IND+NY+51)=KW(IND+NY+51)+1
      YR=Y(I)-XW(IND+33)
      XR=X(I)-XW(IND+34)
      XW(IND+37)=XW(IND+37)+YR
      XW(IND+38)=XW(IND+38)+XR
      XW(IND+39)=XW(IND+39)+YR*YR
      XW(IND+40)=XW(IND+40)+XR*XR
      XW(IND+41)=XW(IND+41)+XR*YR
      II=NX/10
      J=NX-II*10
      IJ=IND+201+NY*10+II
      IF(IAND(KW(IJ),IP2(J+1)).NE.IP2(J+1)) KW(IJ)=KW(IJ)+IP1(J+1)
      KW(IND+26)=KW(IND+26)+1
   20 CONTINUE
      RETURN
      END
