      SUBROUTINE MHISTQ(C,N,XA,XE,Q)
      REAL C(1),Q(16),QC(16)
      REAL R(7),RT(7)
#include "doubls.h"
      DATA RT   /0.0228,0.1577,0.27,0.5,0.73,0.8423,0.9772/
C
C      I    Q(I)
C      1    MEAN
C      2    TRIMMED MEAN
C      3    MEDIAN
C      4     2.3 P.C. QUANTILE
C      5    15.8 P.C. QUANTILE
C      6    27.0 P.C. QUANTILE
C      7    50.0 P.P. QUANTILE
C      8    73.0 P.C. QUANTILE
C      9    84.2 P.C. QUANTILE
C     10    97.7 P.C. QUANTILE
C     11    SIGMA
C     12    SIGMA AVERAGE FROM QUANTILES
C     13    SIGMA LEFT  FROM QUANTILES
C     14    SIGMA RIGHT FROM QUANTILES
C     15    SKEWNESS
C     16    KURTOSIS
C
C
      NJU=1
      GOTO 1
      ENTRY MHISTP(C,N,XA,XE)
      CALL GLUNP(LUNP)
      IF(LUNP.EQ.0) GOTO 100
      NJU=2
    1 HX=0.5*FLOAT(N)
C
C     CLEAR
C
      DO 10 J=1,8
      QC(J)=0.0
      QC(8+J)=0.0
   10 S(J)=0.0
C
C     SUM FOR TOTAL CONTENT AND MEAN
C
      DO 20 I=1,N
      S(1)=S(1)+C(I)
      X=FLOAT(I)-0.5-HX
   20 S(2)=S(2)+C(I)*X
      STOT=S(1)
C     IF(NJU.EQ.2) WRITE(LUNP,102) STOT,N,XA,XE
      IF(S(1).LT.1.0) GOTO 98
      QC(1)=S(2)/S(1)+HX
      IF(S(1).LT.2) GOTO 98
C
C     SUM FOR STANDARD DEVIATION, SKEWNESS, KURTOSIS
C
      DO 30 I=1,N
      X=FLOAT(I)-0.5-QC(1)
      XX=X*X
      XXX=XX*X
      XXXX=XX*XX
      S(3)=S(3)+C(I)*XX
      S(4)=S(4)+C(I)*XXX
   30 S(5)=S(5)+C(I)*XXXX
      DO 40 J=3,5
   40 S(J)=S(J)/(S(1)-1.0)
C
C     CALCULATION OF QUANTILES Q(4) ... Q(10)
C
      DO 50 J=1,7
   50 R(J)=S(1)*RT(J)
      J=1
      DO 60 I=1,N
   55 IF(S(6)+C(I).LT.R(J)) GOTO 60
      QC(3+J)=FLOAT(I-1)+(R(J)-S(6))/C(I)
      IF(J.EQ.7) GOTO 70
      J=J+1
      GOTO 55
   60 S(6)=S(6)+C(I)
C     MEDIAN Q(3)
   70 QC(3)=QC(7)
C
C     CALCULATION OF TRIMMED MEAN = MEAN OF 27 - 73 PERCENT DATA
C
      NQ=QC(6)
      QQ=QC(6)-FLOAT(NQ)
      X=FLOAT(NQ)+0.5*(1.0+QQ)-HX
      S(7)=S(7)+(1.0-QQ)*C(NQ+1)
      S(8)=S(8)+(1.0-QQ)*C(NQ+1)*X
      NA=NQ+2
      NQ=QC(8)
      QQ=QC(8)-FLOAT(NQ)
      IF(NQ.GT.NA) THEN
         DO 80 I=NA,NQ
         S(7)=S(7)+C(I)
         X=FLOAT(I)-0.5-HX
   80    S(8)=S(8)+C(I)*X
      END IF
   85 X=FLOAT(NQ)+0.5*QQ-HX
      S(7)=S(7)+QQ*C(NQ+1)
      S(8)=S(8)+QQ*C(NQ+1)*X
      QC(2)=S(8)/S(7)+HX
C
C     CALCULATION OF STANDARD DEVIATIONS
C
      QC(11)=SQRT(S(3))
      QC(12)=0.5*(QC(9)-QC(5))
      QC(13)=QC(7)-QC(5)
      QC(14)=QC(9)-QC(7)
C
C     CALCULATION OF SKEWNESS AND KURTOSIS
C
      IF(S(3).NE.0.0) THEN
         QC(15)=S(4)/S(3)**1.5
         QC(16)=S(5)/S(3)**2-3.0
      END IF
C
C     TRANSFORMATION TO UNITS OF X
C
      BN=(XE-XA)/FLOAT(N)
      DO 90 J=1,10
   90 QC(J)=XA+QC(J)*BN
      DO 95 J=11,14
   95 QC(J)=QC(J)*BN
C
      IF(NJU.EQ.2) WRITE(LUNP,101) QC
C
   98 IF(NJU.EQ.2) GOTO 100
      DO 99 I=1,16
   99 Q(I)=QC(I)
  100 RETURN
  101 FORMAT('0      MEAN     TRMEAN     MEDIAN       2 PC      16 PC',
     1   6X,'27 PC      50 PC      73 PC      84 PC      98 PC'/
     2   4X,10G11.4/10X,'SIGMA',G11.4,' AV/LFT/RHT',3G11.4,
     3   '   SKEWNESS',G11.4,'   KURTOSIS',G11.4/)
C 102 FORMAT('0HISTOGRAM CONTENT',G12.4,' IN',I4,' BINS BETWEEN',
C    1   G12.4,' AND',G12.4)
      END
