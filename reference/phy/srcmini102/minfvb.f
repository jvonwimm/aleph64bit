      SUBROUTINE MINFVB(KDTRA)
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill vertex bit pattern in DTRA.
C
C     Author: Stephen Haywood      10-Feb-93
C
C     Input  : PYER,PYFR,YV0V banks
C              KDTRA  = index of DTRA bank
C     Output : vertex bits of DTRA bank
C
C     Called by MINTRA
C-----------------------------------------------------------------------
C
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JPYFTN=1,JPYFVN=2,LPYFRA=2)
      PARAMETER(JYV0K1=1,JYV0K2=2,JYV0VX=3,JYV0VY=4,JYV0VZ=5,JYV0VM=6,
     +          JYV0PX=12,JYV0PY=13,JYV0PZ=14,JYV0PM=15,JYV0X1=21,
     +          JYV0X2=22,JYV0XM=23,JYV0C2=26,JYV0IC=27,JYV0P1=28,
     +          JYV0P2=31,JYV0EP=34,JYV0DM=55,JYV0S1=56,JYV0S2=57,
     +          LYV0VA=57)
      PARAMETER(JDTRCH=1,JDTRP0=2,JDTRTH=3,JDTRPH=4,JDTRD0=5,JDTRZ0=6,
     +          JDTRER=7,JDTRTF=12,JDTRHO=13,JDTRHM=14,JDTRVB=15,
     +          JDTRQF=16,JDTREA=17,JDTRVI=27,LDTRAA=27)
C
      DIMENSION INDV0(100),CHIV0(100)
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
      NDTRA = LROWS(KDTRA)
C
C++   Find the number of main vertices.
C
      KPYER = NLINK('PYER',0)
      IF (KPYER.GT.0) THEN
         NPYER = LROWS(KPYER)
      ELSE
         NPYER = 0
      ENDIF
C
C++   Fill vertex bits corresponding to PYER vertices.
C
      KPYFR = NLINK('PYFR',0)
      IF (KPYFR.GT.0) THEN
         NPYFR = LROWS(KPYFR)
      ELSE
         NPYFR = 0
      ENDIF
C
      DO 100 I=1,NPYFR
         IVER = ITABL(KPYFR,I,JPYFVN)
         IF (IVER.GT.30) GOTO 100
         IDTRA = ITABL(KPYFR,I,JPYFTN)
         IF (IDTRA.GT.NDTRA) GOTO 100
         IND = KROW(KDTRA,IDTRA) + JDTRVB
         CALL SBIT1(IW(IND),IVER)
  100 CONTINUE
C
C++   Next, YV0V vertices.
C
      KYV0V = IW(NAMIND('YV0V'))
      IF (KYV0V.GT.0) THEN
         NYV0V = LROWS(KYV0V)
      ELSE
         NYV0V = 0
      ENDIF
C
C++   Check that there are not too many V0's. If the total number of
C++   vertices will excede 30, then the V0's are sorted in increasing
C++   chi-squared.
C
      IF (NPYER+NYV0V.LE.30) THEN
         DO 150 I=1,NYV0V
  150    INDV0(I) = I
      ELSE
         DO 160 I=1,NYV0V
  160    CHIV0(I) = RTABL(KYV0V,I,JYV0C2)
         CALL SORTZV(CHIV0,INDV0,NYV0V,+1,0,0)
      ENDIF
C
C++   Fill vertex bits corresponding to YV0V vertices.
C
      DO 200 I=1,NYV0V
         IYV0V = INDV0(I)
         IVER = NPYER + I
         IF (IVER.GT.30) GOTO 200
         DO 210 IDAU=0,1
            IDTRA = ITABL(KYV0V,IYV0V,JYV0K1+IDAU)
            IF (IDTRA.GT.NDTRA) GOTO 210
            IND = KROW(KDTRA,IDTRA) + JDTRVB
            CALL SBIT1(IW(IND),IVER)
  210    CONTINUE
  200 CONTINUE
C
      RETURN
      END
