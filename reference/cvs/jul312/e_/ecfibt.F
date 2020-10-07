      SUBROUTINE ECFIBT
C----------------------------------------------------------------------
C!   Wires BANKS analysis
C!
C!   Authors : M.N Minard            10-04-95
C!
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "ecmcjj.h"
#include "pweijj.h"
#include "efipr2.h"
#include "ajobjj.h"
#include "rcurnt.h"
      PARAMETER ( NSUM = 16 , NLPLAN =45 )
      DIMENSION T(NSUM),S(NSUM)
      DIMENSION PLACO(36)
      PARAMETER (TOFSET=1708.)
      PARAMETER (THRESH=50000.)
C
C      T(I) : SAMPLING TIME (IN NANOSECONDS)
C      TOFSET : OFFSET (TIME AT HALF INTEGRATION IN NANOSECONDS)
C
      DATA T/0.,256.,512.,768.,1024.,1280.,1536.,1792.
     &      ,2048.,2304.,2560.,2816.,3072.,3328.,3584.,3840./
      DATA IFIRST/0/
      DATA ETHMIN / 0.1 /
#include "bmacro.h"
C
C
      CALL RQBUNC(IBUN,INBU,NWAG,IQUA)
      IBUNCH = 1
      IF ( IBUN.GT.0) IBUNCH = IBUN
c
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
      NAEDWI=NAMIND('EDWI')
      NAECCA = NAMIND ('ECCA')
      KECCA = IW(NAECCA)
      KEDWI=IW(NAEDWI)
      IF(KEDWI.EQ.0)GOTO 666
      NEDWI=LROWS(KEDWI)
      IF (NEDWI.GT.36.OR.NEDWI.LT.0) GO TO 667
C
      CALL AUBOS('PWEI',0,LMHLEN+NEDWI*LPWEIA,KPWEI,IGARB)
      IF (IGARB.EQ.1)KEDWI=IW(NAEDWI)
      IF (IGARB.EQ.1)KEWHE = IW (NAEWHE)
      IF (IGARB.EQ.2) GOTO 888
C
C? GET ON_LINE CALIBRATIONS
C? IF ESCA PRESENT ,BANK ECMC IS READ
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
      CALL BLIST(IW,'E+','PWEI')
      IW(KPWEI+LMHROW)=0
      J1=1
      IW(KPWEI+LMHCOL)=LPWEIA

      DO 1 I=1,NEDWI
        JEDWI = KROW(KEDWI,I)
        III=IW(JEDWI+1)
        IF ( KECCA.GT.0) THEN
          CAL =  RTABL(KECCA,(IBUNCH-1)*36+III,2)
        ELSE
          CAL = 1.
        ENDIF
        TOFSE1=TOFSET+TM(III)
        KLINE=KROW(KEDWI,I)
        KLIN2=KROW(KEWHE,III)
        KLIN1=KROW(KPWEI,J1)
        SUMHM=FLOAT(IW(KLINE+46+NSUM))/2.
        T0=0.
        DO 2 J= NLPLAN + 2, NLPLAN + NSUM + 1
          JJ=J-NLPLAN-1
          S(JJ)=FLOAT(IW(KLINE+J))
          IF (J.EQ.(NLPLAN+2).OR.S(JJ).LT.SUMHM) GOTO2
          IF ((S(JJ)-S(JJ-1)).LT.0.1) GOTO2
          T0=T(JJ-1)+(T(JJ)-T(JJ-1))*(SUMHM-S(JJ-1))/(S(JJ)-S(JJ-1))
          GOTO3
    2   CONTINUE
    3   CONTINUE
        EMOD=0.
        EMOD3 = 0.
        DO 4 J = 2 , NLPLAN + 1
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
          IW(KLIN1+J)=NINT(IW(KLINE+J)*CONST*PLACO(III)*CAL)
          EMOD=EMOD+IW(KLIN1+J)
          IF (J.GT.34) EMOD3 = EMOD3 + IW(KLIN1+J)
    4   CONTINUE
        IW(KLIN1+1)=IW(KLINE+1)
C**
        DO 5 K=NLPLAN+2,NLPLAN+NSUM+1
          IW(KLIN1+K)=IW(KLINE+K)
    5   CONTINUE
C T0 CORRECTION CORRELATED TO ENERGY  DEPOSITION
        ENER=FLOAT(IW(KLIN2+1))
        DT0 = 0.
        IF (ENER.GT.ETHMIN.AND..NOT.FMCRUN) THEN
          DT0 = -150.*EMOD3/ENER
        ENDIF
Cn
        IW ( KLIN1+JPWETI) = NINT(T0-TOFSE1+DT0)
        IF(EMOD.GT.THRESH)THEN
          IW(KPWEI+LMHROW)=J1
          J1=J1+1
        ENDIF
    1 CONTINUE
      CALL AUBOS('PWEI',0,LMHLEN+(J1-1)*LPWEIA,KPWEI,IGARB)
C**
      GOTO 999
  666 CALL RERROR('ECFIBT',1,' EDWI DOES NOT EXIST         ')
      GOTO 999
  667 CALL RERROR('ECFIBT',-2,' EDWI WRONG MINI HEADER      ')
      GOTO 999
  888 CALL RERROR('ECFIBT',-3,' NO SPACE TO CREATE PWEI BANK')
      GOTO 999
  999 CONTINUE
      RETURN
      END
#endif
