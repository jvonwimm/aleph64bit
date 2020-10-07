      SUBROUTINE ECFILS
C----------------------------------------------------------------------
C!   WIRES BANKS ANALYSIS
C!
C!   AUTHORS : B. MICHEL, D. PALLIN  25-05-89
C!   REVISED : 01-09-89
C!   Modified : 03-04-95  M.N Minard
C!
C!   ECFIBT called for 1995 and above
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "ecmcjj.h"
#include "pewijj.h"
#include "efipr2.h"
#include "ajobjj.h"
#include "rcurnt.h"
      DIMENSION T(8),S(8)
      DIMENSION PLACO(36)
      PARAMETER (TOFSET=1708.)
      PARAMETER (THRESH=50000.)
C
C      T(I) : SAMPLING TIME (IN NANOSECONDS)
C      TOFSET : OFFSET (TIME AT HALF INTEGRATION IN NANOSECONDS)
C
      DATA T/0.,512.,1024.,1536.,2048.,2560.,3072.,3584./
      DATA IFIRST/0/
      DATA ETHMIN / 0.1 /
#include "bmacro.h"
C
C
C
C-    Check  the Pastis configuration - 1995 and after call ECFIBT
C
      IF(IRUNRC.GT.35000) THEN
        CALL ECFIBT
        GO TO 999
      ENDIF
      IGAL=IW(NAMIND('AJOB'))
      IGVER=0
      IGCOR=0
      IF(IGAL.NE.0)THEN
        IGVER=ITABL(IGAL,1,JAJOGV)
        IGCOR=ITABL(IGAL,1,JAJOCV)
      ENDIF
C
C     LOOP ON MODULES (ABOVE THRESHOLD)
C
      NAEWHE=NAMIND('EWHE')
      KEWHE=IW(NAEWHE)
C
      IF(KEWHE.EQ.0) THEN
        GOTO 999
      ENDIF
C
      NAEWDI=NAMIND('EWDI')
      KEWDI=IW(NAEWDI)
      IF(KEWDI.EQ.0)GOTO 666
      NEWDI=LROWS(KEWDI)
      IF (NEWDI.GT.36.OR.NEWDI.LT.0) GO TO 667
C
      CALL AUBOS('PEWI',0,LMHLEN+NEWDI*LPEWIA,KPEWI,IGARB)
      IF (IGARB.EQ.1)KEWDI=IW(NAEWDI)
      IF (IGARB.EQ.1)KEWHE = IW (NAEWHE)
      IF (IGARB.EQ.2) GOTO 888
C? GET ON_LINE CALIBRATIONS
C? IF ESCA PRESENT ,BANK ECMC IS READ
C? IF RUN NUMBER LOWER THAN 6000 (1989 DATA),
C?        DO NOT APPLY PLATINES CORECTIONS
C
      IECMC = 1
      KECMC=IW(NAMIND('ECMC'))
      IF (KECMC.NE.0) THEN
        IF (LROWS(KECMC).NE.36 ) THEN
C
C-     Check bank length
C
          LENG = IW(KECMC)
          IF ( LENG.EQ.LCOLS(KECMC)*36+LMHLEN) THEN
            IW(LMHLEN + LMHROW) = 36
          ELSE
            IECMC = 0
          ENDIF
        ENDIF
      ENDIF
      IF (IW(NAMIND('ESCA')).EQ.0 .OR. IW(NAMIND('ECMC')).EQ.0.
     +  OR.IRUNRC.LT.6000.OR.IECMC.EQ.0) THEN
        DO 50 I=1,36
   50   PLACO(I)=1.
      ELSE
        KECMC=IW(NAMIND('ECMC'))
        DO 51 I=1,36
   51   PLACO(I)=RTABL(KECMC,I,JECMCO)
      ENDIF
      CALL BLIST(IW,'E+','PEWI')
      IW(KPEWI+LMHROW)=0
      J1=1
      IW(KPEWI+LMHCOL)=LPEWIA
      DO 1 I=1,NEWDI
        III=IW(KROW(KEWDI,I)+1)
        TOFSE1=TOFSET+TM(III)
        KLINE=KROW(KEWDI,I)
        KLIN2=KROW(KEWHE,III)
        KLIN1=KROW(KPEWI,J1)
        SUMHM=FLOAT(IW(KLINE+54))/2.
        T0=0.
        DO 2 J= 47, 54
          JJ=J-46
          S(JJ)=FLOAT(IW(KLINE+J))
          IF (J.EQ.47.OR.S(JJ).LT.SUMHM) GOTO2
          IF ((S(JJ)-S(JJ-1)).LT.0.1) GOTO2
          T0=T(JJ-1)+(T(JJ)-T(JJ-1))*(SUMHM-S(JJ-1))/(S(JJ)-S(JJ-1))
          GOTO3
    2   CONTINUE
    3   CONTINUE
        EMOD=0.
        EMOD3 = 0.
        DO 4 J = 2 , 46
          CONST  = 1.
C!CORRECTION FOR STACK 3 APPLIED IF GALEPH VERSION=231 AND VERSION<2
          IF(IGVER.EQ.231.AND.IGCOR.LE.200)THEN
            IF ( J.GT.34 ) THEN
              IF (IW(KLINE+1).GT.12. AND .IW(KLINE+1).LT.25) THEN
                CONST=1.932
              ELSE
                CONST=1.901
              ENDIF
            ENDIF
          ENDIF
C CORRECT ALSO HERE WITH PLATINE COEFFS
          IW(KLIN1+J)=NINT(IW(KLINE+J)*CONST*PLACO(III))
          EMOD=EMOD+IW(KLIN1+J)
          IF (J.GT.34) EMOD3 = EMOD3 + IW(KLIN1+J)
    4   CONTINUE
        IW(KLIN1+1)=IW(KLINE+1)
        DO 5 K=47,54
          IW(KLIN1+K)=IW(KLINE+K)
    5   CONTINUE
C T0 CORRECTION CORRELATED TO ENERGY  DEPOSITION
        ENER=FLOAT(IW(KLIN2+1))
        DT0 = 0.
        IF (ENER.GT.ETHMIN.AND..NOT.FMCRUN) THEN
          DT0 = -150.*EMOD3/ENER
        ENDIF
        IW ( KLIN1+JPEWTI) = NINT(T0-TOFSE1+DT0)
        IF(EMOD.GT.THRESH)THEN
          IW(KPEWI+LMHROW)=J1
          J1=J1+1
        ENDIF
    1 CONTINUE
      CALL AUBOS('PEWI',0,LMHLEN+(J1-1)*LPEWIA,KPEWI,IGARB)
C
      GOTO 999
  666 CALL RERROR('ECFILS',1,' EWDI DOES NOT EXIST         ')
      GOTO 999
  667 CALL RERROR('ECFILS',-2,' EWDI wrong mini header      ')
      GOTO 999
  888 CALL RERROR('ECFILS',-3,' NO SPACE TO CREATE PEWI BANK')
      GOTO 999
  999 CONTINUE
      RETURN
      END
#endif
