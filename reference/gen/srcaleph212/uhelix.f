      SUBROUTINE UHELIX(KPOI,IFL,RXY,PSIB,XV,FIELRC,X,COVX,P,COVP,IFAIL)
C-------------------------------------------------------------
C GIVES X,Y,Z AND PX,PY,PZ FOR A GIVEN POINT IN A TRACK
C
C  AUTHOR: LL.GARRIDO     17/3/89
C
C  WARNING: THE CHANGE OF THE PARAMETERS AND ERROS DUE TO
C           MULTIPLE SCATTERING EFFECTS ARE NOT TAKEN INTO
C           ACCOUNT IN THE PRESENT VERSION. THESE EFFECTS ARE
C           SMALL BUT THEY ARE GOING TO BE INTRODUCED IN
C           THE NEXT VERSION.
C  INPUT:
C             KPOI: TRACK NUMBER IN FRFT BANK
C             IFL : FLAG THAT DETERMINES HOW THE POINT
C                   OF THE HELIX IS DEFINED,
C                  IF IFL.EQ.1 THE POINT THAT HAS RXY  IS USED
C                  IF IFL.EQ.2 THE POINT THAT HAS PSIB IS USED
C                  IF IFL.EQ.3 THEN THE CLOSEST POINT
C                     OF THE HELIX (IN X-Y PLANE) TO THE POINT XV IS USE
C             RXY : RADIUS OF THE POINT IN THE X-Y PLANE
C                   (ONLY POINTS THAT ARE IN THE FIRST ARC OF
C                    THE HELIX ARE CONSIDERED)
C             PSIB: ANGLE PSI FOR THE HELIX POIN
C             XV  : A GIVEN POINT IN THE SPACE (FOR INSTANCE
C                   A VERTEX POINT)
C             FIELRC: MAGNETIC FIELD
C  OUTPUT:
C             PSIB: ANGLE PSI FOR THE HELIX POINT
C             X   : VECTOR THAT CONTAINS X,Y,Z OF THE POINT
C             COVX: COVARIANCE MATRIX OF X,Y,Z IN CM.
C             P   : MOMENTUM VECTOR PX,PY,PZ
C             COVP: COVARIANCE MATRIX OF THE MOMENTUM IN GEV
C             IFAIL: IF NOT 0 ERROR SOMEWHERE
C---------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JFRFIR=1,JFRFTL=2,JFRFP0=3,JFRFD0=4,JFRFZ0=5,JFRFAL=6,
     +          JFRFEM=7,JFRFC2=28,JFRFDF=29,JFRFNO=30,LFRFTA=30)
       DOUBLE PRECISION Z(5,6),XM(6,6),BV(6)
       REAL PSIB,X(3),XV(3),COVX(3,3),P(3),COVP(3,3),XSTO(6),PSTO(6)
       REAL R(36),ALFAC,FIELRC, LAM
       INTEGER ITK,IFL,IFAIL
       LOGICAL FIRST
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
       DATA FIRST/.TRUE./
       DATA ALFAC/0.29979E-3/
       IF(FIRST) THEN
         PIT2=4.*ASIN(1.)
         NFRFT=NAMIND('FRFT')
         IF(NFRFT.EQ.0) GOTO 999
         FIRST=.FALSE.
       ENDIF
       IFAIL=0
       IF(IFL.EQ.1) THEN
       KFRFT=IW(NFRFT)
       IF(KFRFT.EQ.0) GOTO 999
       RIN=RTABL(KFRFT,KPOI,JFRFIR)
       LAM=RTABL(KFRFT,KPOI,JFRFTL)
       FI0=RTABL(KFRFT,KPOI,JFRFP0)
       D0 =RTABL(KFRFT,KPOI,JFRFD0)
       Z0 =RTABL(KFRFT,KPOI,JFRFZ0)
       SST=SIGN(1.,RIN)
       PSIB=(RXY**2-D0**2)/(1.-D0*RIN)
       IF(PSIB.LT.0.) GOTO 999
       PSIB=-RIN/2.*SQRT(PSIB)
       IF(ABS(PSIB).GT.1.) GOTO 999
       PSIB=-2.*SST*ASIN(PSIB)
       ENDIF
       IF(IFL.EQ.3) THEN
       KFRFT=IW(NFRFT)
       IF(KFRFT.EQ.3) GOTO 999
       RIN=RTABL(KFRFT,KPOI,JFRFIR)
       LAM=RTABL(KFRFT,KPOI,JFRFTL)
       FI0=RTABL(KFRFT,KPOI,JFRFP0)
       D0 =RTABL(KFRFT,KPOI,JFRFD0)
       Z0 =RTABL(KFRFT,KPOI,JFRFZ0)
       SST=SIGN(1.,RIN)
       X0=D0*SIN(FI0)
       Y0=-D0*COS(FI0)
       XC=X0-SIN(FI0)/RIN
       YC=Y0+COS(FI0)/RIN
       XL=SST*SQRT((XV(1)-XC)**2+(XV(2)-YC)**2)
       SINFI= (XV(1)-XC)/XL
       COSFI=-(XV(2)-YC)/XL
       FI=ACOS(COSFI)
       IF(SINFI.LT.0.) FI=PIT2-FI
       DIFFI=FI-FI0
       PSIB=SST*DIFFI
       ENDIF
       CALL YV0ONE(KPOI,PSIB,XM,BV,Z,IFAIL)
       IF(IFAIL.NE.0) GOTO 999
C
      ICTR = 0
      DO 1 I1=1,3
         X(I1)=BV(I1)
         P(I1)=BV(I1+3)*ALFAC*FIELRC
         DO 1 I2=1,I1
            ICTR = ICTR + 1
            XSTO(ICTR)=XM(I1,I2)
            PSTO(ICTR)=XM(I1+3,I2+3)/(ALFAC*FIELRC)**2
    1 CONTINUE
C
      CALL SMINV(XSTO,R,3,0,NRANK)
      IF (NRANK.LT.3) GO TO 999
      CALL SMINV(PSTO,R,3,0,NRANK)
      IF (NRANK.LT.3) GO TO 999
C
      ICTR = 0
      DO 2 I1 = 1,3
         DO 2 I2 = 1,I1
            ICTR = ICTR + 1
            COVX(I1,I2) = XSTO(ICTR)
            COVX(I2,I1) = XSTO(ICTR)
            COVP(I1,I2) = PSTO(ICTR)
            COVP(I2,I1) = PSTO(ICTR)
    2 CONTINUE
C
       RETURN
999    IFAIL=1
       RETURN
       END
