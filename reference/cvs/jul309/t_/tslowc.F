      SUBROUTINE TSLOWC
C----------------------------------------------------
C! Handle slow control banks for the TPC
C
C  R. Johnson    23-09-89
C
C?
C!======================================================================
#ifndef DOC
C
#include "rparac.h"
#include "rflags.h"
#include "rlunit.h"
#include "tpgpar.h"
#include "bcs.h"
#include "tslcon.h"
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTSCI=NAMIND('TSCI')
        NTSSR=NAMIND('TSSR')
        NNEVE=NAMIND('NEVE')
      ENDIF
      IF (FDEBRF.AND.JDBDRF(JULTP).GE.2) THEN
        CALL TSCIDP(LDEBRL)
      ENDIF
C
      KTSCI=IW(NTSCI)
      IF (KTSCI.EQ.0) THEN
        KTSCI=IW(NTSSR)
        IF (KTSCI.EQ.0) RETURN
      ENDIF
C
C++   Get the date and time from the NEVE bank
C
      KNEVE=IW(NNEVE)
      IF (KNEVE.NE.0) THEN
        TSLDAT=IW(KNEVE+1)
        TSLTIM=IW(KNEVE+2)
      ELSE
        TSLDAT=0
        TSLTIM=0
      ENDIF
C
C++   Loop over internal structure of TSCI to get the hardware status
C
      TSCSTT='TSCI'
      IPT=2
   10 IF (IPT.GE.IW(KTSCI)) GO TO 800
        IPT=IPT+1
        IDXH=IW(KTSCI+IPT)
        IF (IDXH.EQ.2) THEN
C
C++       Preamp voltages
C
          IPT=IPT+1
          NW=IW(KTSCI+IPT)
          DO 20 I=1,LTSECT
            TSLPAV(1,I)=FLOAT(IW(KTSCI+IPT+(I-1)*2+1))/100.
            TSLPAV(2,I)=FLOAT(IW(KTSCI+IPT+(I-1)*2+2))/100.
   20     CONTINUE
          IPT=IPT+NW
        ELSEIF (IDXH.EQ.3) THEN
C
C++       Gating voltages
C
          IPT=IPT+1
          NW=IW(KTSCI+IPT)
          DO 30 I=1,LTSECT
            TSLGTV(1,I)=FLOAT(IW(KTSCI+IPT+(I-1)*3+1))/100.
            TSLGTV(2,I)=FLOAT(IW(KTSCI+IPT+(I-1)*3+2))/100.
            TSLGTV(3,I)=FLOAT(IW(KTSCI+IPT+(I-1)*3+3))/100.
   30     CONTINUE
          IPT=IPT+NW
        ELSEIF (IDXH.EQ.4) THEN
C
C++       High voltage current
C
          IPT=IPT+1
          NW=IW(KTSCI+IPT)
          DO 40 I=1,LTSECT
            TSLCRN(I)=FLOAT(IW(KTSCI+IPT+I))
   40     CONTINUE
C
C++       High voltage levels
C
          DO 50 I=1,LTSECT
            TSLVTG(I)=FLOAT(IW(KTSCI+IPT+LTSECT+I))
   50     CONTINUE
          IPT=IPT+NW
        ELSEIF (IDXH.EQ.5) THEN
C
C++       Sector temperatures
C
          IPT=IPT+1
          NW=IW(KTSCI+IPT)
          DO 60 I=1,LTSECT
            TSLSTP(1,I)=FLOAT(IW(KTSCI+IPT+(I-1)*5+1))/10.
            TSLSTP(2,I)=FLOAT(IW(KTSCI+IPT+(I-1)*5+2))/10.
            TSLSTP(3,I)=FLOAT(IW(KTSCI+IPT+(I-1)*5+3))/10.
            TSLSTP(4,I)=FLOAT(IW(KTSCI+IPT+(I-1)*5+4))/10.
            TSLSTP(5,I)=FLOAT(IW(KTSCI+IPT+(I-1)*5+5))/10.
   60     CONTINUE
          IPT=IPT+NW
        ELSEIF (IDXH.EQ.7) THEN
C
C++       Pressure and temperature of the gas
C
          IPT=IPT+1
          NW=IW(KTSCI+IPT)
          TSLPRS=FLOAT(IW(KTSCI+IPT+1))/10.
          TSLPDF=FLOAT(IW(KTSCI+IPT+4))/10.
          DO 70 I=1,6
            TSLTMP(I)=FLOAT(IW(KTSCI+IPT+6+I))/10.
   70     CONTINUE
          IPT=IPT+NW
        ELSEIF (IDXH.EQ.8) THEN
C
C++       FE55 peak position from the monitoring chamber
C
          IPT=IPT+1
          TSLGGN=FLOAT(IW(KTSCI+IPT+4))/100.
          NW=IW(KTSCI+IPT)
          IPT=IPT+NW
        ELSE
C
C++       Skip over other equipment
C
          IPT=IPT+1
          NW=IW(KTSCI+IPT)
          IPT=IPT+NW
        ENDIF
        GO TO 10
  800 CONTINUE
C
      RETURN
      END
#endif
