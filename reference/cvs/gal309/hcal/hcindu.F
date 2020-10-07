*DK hcindu
      SUBROUTINE HCINDU
C------------------------------------------------
C!  Consider effects of inductions between tubes
CKEY HCAL INDUCTION
C!      Author       : G. Iaselli  880119
C!                     F.Ranjard   920115
C!
C!      Input  Bank    : JDHCHI  McHcHits
C!                       JDHCSE  McHcTubesSegment
C!                       HWHT    McHcTubeSignal
C!
C!      Output Bank    : HWHT
C!      temporary Bank : JDHTEM
C!      Called by      : HCASIG
C!
C!
C ----------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "hcnamc.h"
#include "hcgega.h"
#include "hcsejj.h"
#include "hchijj.h"
#include "hwhtjj.h"
#include "iocom.h"
#include "hccong.h"
      PARAMETER (LTUB=320)
      DIMENSION IVET(LTUB)
#include "bmacro.h"
C  ---------------------------------------------------------------
C - return if no HWHT bank
      JHWHT=IW(NAHWHT)
      IF(JHWHT.EQ.0) RETURN
C - initialize temporary storing space
      JDHTEM = 0
      CALL WBANK(IW,JDHTEM,3*IW(JHWHT),*10)
      IW(JDHTEM+1)=LHWHTA
      IW(JDHTEM+2)=0
C
      NRHI = 1
C
      IPL0=-1
      IPOR0=-1
      NSTRE=0
      NTR=0
C?    Loop on fired tubes
      DO 90 ITUBE= 1,LROWS(JHWHT)
         ITADR=ITABL (JHWHT,ITUBE,1)
         NSEG=ITABL(JDHCHI,NRHI,JHCHTS)
         KHCSE=KROW(JDHCSE,NSEG)
C
C?    Count number of streamers
         NSTRE=0
   30    CONTINUE
         IF(NRHI.GT.LROWS(JDHCHI)) GO TO 40
         NSEG = ITABL(JDHCHI,NRHI,JHCHTS)
         ITADR1=ITABL(JDHCSE,NSEG,JHCSTA)
         IF(ITADR.NE.ITADR1) GOTO 40
         NSTRE=NSTRE+ITABL(JDHCHI,NRHI,JHCHNS)
         NRHI = NRHI+1
         GO TO 30
   40    CONTINUE
C
C?    Compute number of tubes to be fired
         CALL POISSN(NSTRE*HPINDU,NT,IER)
         IS=SIGN(1.,(RNDM(DUMMY)-.5))
C
C
         IPL=IW(KHCSE+2)
         IPOR = IW(KHCSE+4)
         IF (IPL.NE.IPL0 .OR. IPOR.NE.IPOR0)THEN
            IPL0=IPL
            IPOR0=IPOR
            IF (IPOR.EQ.LPBAR) THEN
               ITLAS = NHCTU1(IPL)
            ELSE
               ITLAS = NHCTRE
            ENDIF
C
            IF (NTR.GT.0) THEN
C            Fill the temporary bank with tubes addresses
               IF(NTR.GT.1)CALL SORTIQ(IVET,1,NTR,1)
               IF (LFRROW(JDHTEM).LT.NTR) THEN
                  IF (NTR.GT.50) THEN
                     CALL ALTELL ('HCINDU: POISSN is corrupted-STOP',0,
     &                            'END')
                  ENDIF
                  CALL WBANK (IW,JDHTEM,IW(JDHTEM)+50,*10)
               ENDIF
               KHTEM = KNEXT(JDHTEM)
               CALL UCOPY(IVET,IW(KHTEM+1),NTR)
               IW(JDHTEM+2)=LROWS(JDHTEM) + NTR
               NTR=0
            ENDIF
C
         ENDIF
C
         ITEM=MOD(ITADR,1000)
         MTEM = (ITEM-1) / ITLAS
         NEW = 0
C     fill IVET but suppress double tube addresses
         DO 80 IT=1,NT+1
            JADR=ITADR+IS*(IT-1)
            DO 60 NV=1,NTR
               IF(JADR.EQ.IVET(NV)) GO TO 80
   60       CONTINUE
            ITUL=ITEM+IS*(IT-1)
            MTUL = (ITUL-1) / ITLAS
            IF (ITUL.GE.1.AND.MTUL.EQ.MTEM)THEN
               NEW=NEW+1
               IVET(NTR+NEW)=JADR
            ENDIF
   80    CONTINUE
         NTR = NTR + NEW
   90 CONTINUE
C - copy last set of tube addresses into the temporary buffer
      IF (NTR.GT.0) THEN
C       Fill the temporary bank with tubes addresses
         IF(NTR.GT.1)CALL SORTIQ(IVET,1,NTR,1)
         IF (LFRROW(JDHTEM).LT.NTR) THEN
            CALL WBANK (IW,JDHTEM,IW(JDHTEM)+50,*10)
         ENDIF
         KHTEM = KNEXT(JDHTEM)
         CALL UCOPY(IVET,IW(KHTEM+1),NTR)
         IW(JDHTEM+2)=LROWS(JDHTEM) + NTR
      ENDIF
C
C - copy the temporary buffer into HWHT
      CALL WBANK(IW,JDHTEM,IW(JDHTEM+1)*IW(JDHTEM+2)+LMHLEN,*10)
      CALL BKFRW(IW,'HWHT',0,IW,JDHTEM,*10)
      CALL WDROP(IW,JDHTEM)
      RETURN
C
C - not enough space to book temporary space
   10 CONTINUE
      CALL ALTELL('HCINDU:not enough space to book JDHTEM ',1,'NEXT')
      END
#endif
