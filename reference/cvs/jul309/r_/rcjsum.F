      SUBROUTINE RCJSUM
C--------------------------------------------------------------------
C! Create and fill JSUM bank
C!
C!  Author       J. Knobloch 14-NOV-1989
C!
C!   Modified :- E. Lancon             18-SEP-1991
C!        DO not call BMSUM anymore, use result stored in BSUMCO
C!   Modified :- B. Bloch-Devaux       25-MAY-1993
C!        Define Lumi coincidence as LCAL before 93 , Sical from 93 on
C!   Modified :- I. Tomalin             9-MAY-1995
C!        Improve PASS0 drift velocity calculation: Only sum t0 for
C!        events where bunch number is certain, and call TPCT0N
C!        instead of TPCT0.
C!   Modified :- P. Comas              13-JUN-1995
C!        Add as many rows as wagons in a train and fill them.
C!        The information per bunch will be used to get the beam
C!        spot parameters per wagon.
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "bcs.h"
#include "bosext.h"
#include "rflags.h"
#include "rstate.h"
#include "rlunit.h"
#include "jsumjj.h"
#include "dheajj.h"
#include "evehjj.h"
#include "revhjj.h"
#include "pyerjj.h"
#include "pecojj.h"
#include "bsumco.h"
#include "rmonco.h"
#include "revent.h"
#include "rcurnt.h"
#include "rtimes.h"
      LOGICAL LVERT,LCOIN,LHVON,LBHAB,LZZER,BUNCH_OK
C     PARAMETER( MASK = 5828 )
C     PARAMETER( LMASK =  2**14+2**15 )
      DATA TWIND, TWINE / 0.5, 0.05 /
      DIMENSION NTR(2)
      DATA RLIM, ZLIM /1., 10./
#include "bmacro.h"
C
C     Need to know number of wagons per train
      CALL RQBUNC(IBUN,INBU,NWAG,IQUA)
      IF ( (NWAG.LT.1).OR.(NWAG.GT.4) ) NWAG=1
      BUNCH_OK=NWAG.GT.1.AND.IBUN.GT.0.AND.IBUN.LE.NWAG.AND.IQUA.EQ.3
C
C     JSUM bank already there?
C
      IF(IW(NAMIND('JSUM')).EQ.0)THEN
C
C     Create JSUM bank
        IF (NWAG.EQ.1) THEN
           LEN=LMHLEN+LJSUMA
        ELSE
           LEN=LMHLEN+LJSUMA*(NWAG+1)
        ENDIF
        CALL AUBOS('JSUM',IRUNRC,LEN,KJSUM,IGARB)
        IF(IGARB.EQ.2) THEN
          CALL RERROR ('RCJSUM', -1, ' Unable to fill JSUM ')
          GOTO 999
        ENDIF
        IW(KJSUM+LMHCOL)=LJSUMA
        IF (NWAG.EQ.1) THEN
          IW(KJSUM+LMHROW)=1
        ELSE
          IW(KJSUM+LMHROW)=NWAG+1
        ENDIF
        CALL DATIME(IDAY,ITIM)
        IW(KJSUM+LMHLEN+JJSUDA) = IDAY
        IW(KJSUM+LMHLEN+JJSUTI) = ITIM

        KS=KJSUM+LMHLEN
        KS1=KJSUM+1

        IF (NWAG.GT.1) THEN
          DO 10 I=2,NWAG+1
             IW(KS+(I-1)*IW(KS1)+JJSUDA) = IDAY
             IW(KS+(I-1)*IW(KS1)+JJSUTI) = ITIM
   10     CONTINUE
        ENDIF
      ENDIF
C
C     Fill JSUM bank
      KJSUM=IW(NAMIND('JSUM'))
      KDHEA=IW(NAMIND('DHEA'))
      KPYER=IW(NAMIND('PYER'))
      KEVEH=IW(NAMIND('EVEH'))
      KREVH=IW(NAMIND('REVH'))
      KPECO=IW(NAMIND('PECO'))
      KS=KJSUM+LMHLEN
      KS1=KJSUM+1
C
      IW(KS+JJSUNT)=IW(KS+JJSUNT)+1
      IF (BUNCH_OK) THEN
        IW(KS+IBUN*IW(KS1)+JJSUNT) = ITABL(KJSUM,IBUN+1,JJSUNT)+1
      ENDIF
C
C?   Vertex ?
C
      LVERT = .FALSE.
      X0 = 0.
      Y0 = 0.
      Z0 = 0.
      NVTX = 0
      IF(KDHEA.NE.0) NVTX = IW(KDHEA+LMHLEN + JDHENX)
      IF( NVTX.GT.0)THEN
        IF(KPYER.NE.0)THEN
          IF(IW(KPYER).GT.2)THEN
            IPYER=KPYER+LMHLEN
            X0 = RW(IPYER+JPYEVX)
            Y0 = RW(IPYER+JPYEVY)
            Z0 = RW(IPYER+JPYEVZ)
          ENDIF
        ENDIF
        R2 = X0**2 + Y0**2
        LVERT=R2.LT.RLIM.AND.ABS(Z0).LT.ZLIM
      ENDIF
      IF(LVERT)THEN
        IW(KS+JJSUNV)=IW(KS+JJSUNV)+1
        RW(KS+JJSUXV)=RW(KS+JJSUXV)+X0
        RW(KS+JJSUYV)=RW(KS+JJSUYV)+Y0
        RW(KS+JJSUZV)=RW(KS+JJSUZV)+Z0
        RW(KS+JJSUXS)=RW(KS+JJSUXS)+X0**2
        RW(KS+JJSUYS)=RW(KS+JJSUYS)+Y0**2
        RW(KS+JJSUZS)=RW(KS+JJSUZS)+Z0**2
        IF (BUNCH_OK) THEN
          IW(KS+IBUN*IW(KS1)+JJSUNV) = ITABL(KJSUM,IBUN+1,JJSUNV)+1
          RW(KS+IBUN*IW(KS1)+JJSUXV) = RTABL(KJSUM,IBUN+1,JJSUXV)+X0
          RW(KS+IBUN*IW(KS1)+JJSUYV) = RTABL(KJSUM,IBUN+1,JJSUYV)+Y0
          RW(KS+IBUN*IW(KS1)+JJSUZV) = RTABL(KJSUM,IBUN+1,JJSUZV)+Z0
          RW(KS+IBUN*IW(KS1)+JJSUXS) = RTABL(KJSUM,IBUN+1,JJSUXS)+X0**2
          RW(KS+IBUN*IW(KS1)+JJSUYS) = RTABL(KJSUM,IBUN+1,JJSUYS)+Y0**2
          RW(KS+IBUN*IW(KS1)+JJSUZS) = RTABL(KJSUM,IBUN+1,JJSUZS)+Z0**2
        ENDIF
      ENDIF
C
C?   Z0 ?
C
      ECHFL = 0.
      IF(KDHEA.NE.0) ECHFL = RW(KDHEA+LMHLEN+JDHEEF)
      LZZER=LVERT.AND.ECHFL.GT.40
      IF(LZZER)THEN
        IW(KS+JJSUNZ)=IW(KS+JJSUNZ)+1
        IF (BUNCH_OK) THEN
          IW(KS+IBUN*IW(KS1)+JJSUNZ) = ITABL(KJSUM,IBUN+1,JJSUNZ)+1
        ENDIF
      ENDIF
C
C?   Bhabha ?, Z event ? with HV on
C
      LCOIN = .FALSE.
      LHVON = .FALSE.
      CALL RLUTRF(IRUNRC,LMASK,MASK,MSKPC)
      IF (KEVEH.GT.0) LCOIN=IAND( IW(KEVEH+JEVEM1) , LMASK ).NE.0
      IF(LCOIN) THEN
        IW(KS+JJSUNL)=IW(KS+JJSUNL)+1
        IF (BUNCH_OK) THEN
          IW(KS+IBUN*IW(KS1)+JJSUNL) = ITABL(KJSUM,IBUN+1,JJSUNL)+1
        ENDIF
      ENDIF
      IF(IRUNRC.GT.4539) THEN
        IF (KREVH.GT.0)
     &     LHVON=IAND( IW(KREVH+LMHLEN+JREVDS) , MASK).EQ.MASK
      ENDIF
      IF(LHVON)THEN
        IW(KS+JJSUVT)=IW(KS+JJSUVT)+1
        IF (BUNCH_OK) THEN
          IW(KS+IBUN*IW(KS1)+JJSUVT) = ITABL(KJSUM,IBUN+1,JJSUVT)+1
        ENDIF
        IF(LVERT) THEN
          IW(KS+JJSUVV)=IW(KS+JJSUVV)+1
          IF (BUNCH_OK) THEN
            IW(KS+IBUN*IW(KS1)+JJSUVV) = ITABL(KJSUM,IBUN+1,JJSUVV)+1
          ENDIF
        ENDIF
        IF(LZZER) THEN
          IW(KS+JJSUVZ)=IW(KS+JJSUVZ)+1
          IF (BUNCH_OK) THEN
            IW(KS+IBUN*IW(KS1)+JJSUVZ) = ITABL(KJSUM,IBUN+1,JJSUVZ)+1
          ENDIF
        ENDIF
        IF(LCOIN) THEN
          IW(KS+JJSUVL)=IW(KS+JJSUVL)+1
          IF (BUNCH_OK) THEN
            IW(KS+IBUN*IW(KS1)+JJSUVL) = ITABL(KJSUM,IBUN+1,JJSUVL)+1
          ENDIF
        ENDIF
      ENDIF
C
C?   Bhabha in LCAL ?
C
      ELCAL = 0.
      IF(KPECO.NE.0)THEN
        DO 50 I=1,LROWS(KPECO)
          IF(ITABL(KPECO,I,JPECKD).EQ.MSKPC) ELCAL=ELCAL+RTABL(KPECO,
     &      I,JPECEC)
   50   CONTINUE
      ENDIF
      LBHAB=LCOIN.AND.ELCAL.GT.65.
      IF(LBHAB) THEN
        IW(KS+JJSUNB)=IW(KS+JJSUNB)+1
        IF (BUNCH_OK) THEN
          IW(KS+IBUN*IW(KS1)+JJSUNB) = ITABL(KJSUM,IBUN+1,JJSUNB)+1
        ENDIF
      ENDIF

      IF(LBHAB.AND.LHVON) THEN
        IW(KS+JJSUVB)=IW(KS+JJSUVB)+1
        IF (BUNCH_OK) THEN
          IW(KS+IBUN*IW(KS1)+JJSUVB) = ITABL(KJSUM,IBUN+1,JJSUVB)+1
        ENDIF
      ENDIF
C
C?   Size Input, Output, Time per event
C
      RW(KS+JJSULI)=RW(KS+JJSULI) + SIZERE/4.
      IF (BUNCH_OK) THEN
        RW(KS+IBUN*IW(KS1)+JJSULI) = RTABL(KJSUM,IBUN+1,JJSULI)+
     >        SIZERE/4.
      ENDIF
      IF(LZZER)THEN
        RW(KS+JJSUIZ)=RW(KS+JJSUIZ) + SIZERE/4.
        IF (BUNCH_OK) THEN
          RW(KS+IBUN*IW(KS1)+JJSUIZ) = RTABL(KJSUM,IBUN+1,JJSUIZ)+
     >        SIZERE/4.
        ENDIF
      ENDIF
      RLPOT = LENLIS ('E')/250.
      CALL ALTIME(TIM)
      TIME = TIM-AEVSRT
      RW(KS+JJSULO)=RW(KS+JJSULO)+RLPOT
      IF (BUNCH_OK) THEN
        RW(KS+IBUN*IW(KS1)+JJSULO) = RTABL(KJSUM,IBUN+1,JJSULO)+RLPOT
      ENDIF
      IF(LZZER)THEN
        RW(KS+JJSULZ)=RW(KS+JJSULZ)+RLPOT
        IF (BUNCH_OK) THEN
          RW(KS+IBUN*IW(KS1)+JJSULZ) = RTABL(KJSUM,IBUN+1,JJSULZ)+RLPOT
        ENDIF
      ENDIF
      RW(KS+JJSUTT)=RW(KS+JJSUTT)+TIME
      IF (BUNCH_OK) THEN
        RW(KS+IBUN*IW(KS1)+JJSUTT) = RTABL(KJSUM,IBUN+1,JJSUTT)+TIME
      ENDIF
      IF(LZZER)THEN
        RW(KS+JJSUTZ)=RW(KS+JJSUTZ)+TIME
        IF (BUNCH_OK) THEN
          RW(KS+IBUN*IW(KS1)+JJSUTZ) = RTABL(KJSUM,IBUN+1,JJSUTZ)+TIME
        ENDIF
      ENDIF
      IF(LBHAB)THEN
        RW(KS+JJSUTB)=RW(KS+JJSUTB)+TIME
        IF (BUNCH_OK) THEN
          RW(KS+IBUN*IW(KS1)+JJSUTB) = RTABL(KJSUM,IBUN+1,JJSUTB)+TIME
        ENDIF
      ENDIF
C
C     killed storeys in ECAL
C
      IW(KS+JJSUKB) = NSUPEB
      IW(KS+JJSUKW) = NSUPEC
      IF (BUNCH_OK) THEN
        IW(KS+IBUN*IW(KS1)+JJSUKB) = NSUPEB
        IW(KS+IBUN*IW(KS1)+JJSUKW) = NSUPEC
      ENDIF
C
C++   Fill t0 information in JSUM.
C++   DT is (negative) change to t0
C
      DT = 9999999.
C     CALL TPCT0(IER,DT)
      CALL TPCT0N(IER,DT,EDTN,NTR)
      IF(IER.EQ.0) THEN
        DT = 0.5*DT
        EDTN = 0.5*EDTN
C Require DT to fall inside window, error to be not too large and
C at least three tracks used in each TPC half.
        IF(ABS(DT).LE.TWIND.AND.EDTN.LT.TWINE.AND.
     +  MIN(NTR(1),NTR(2)).GE.3) THEN
C
C If in bunch train mode, require bunch number to be certain.
          IF (NWAG.EQ.1.OR.IQUA.EQ.3) THEN
            IW(KS+JJSUTN) = IW(KS+JJSUTN) + 1
            RW(KS+JJSUTS) = RW(KS+JJSUTS) + DT
            RW(KS+JJSUTV) = RW(KS+JJSUTV) + DT*DT
            IF (BUNCH_OK) THEN
              IW(KS+IBUN*IW(KS1)+JJSUTN)=ITABL(KJSUM,IBUN+1,JJSUTN)+1
              RW(KS+IBUN*IW(KS1)+JJSUTS)=RTABL(KJSUM,IBUN+1,JJSUTS)+DT
              RW(KS+IBUN*IW(KS1)+JJSUTV)=RTABL(KJSUM,IBUN+1,JJSUTV)
     +        + DT*DT
            ENDIF
          END IF
C
        END IF
      ENDIF
C
C     get beam position from all tracks (d0,z0)
      DO 60 I =  JJSUA0,  JJSUB9
        RW (KS+I) = BSUM (I-JJSUA0+1)
   60 CONTINUE
      IF (BUNCH_OK) THEN
        DO 70 I =  JJSUA0,  JJSUB9
          RW(KS+IBUN*IW(KS1)+I)=BSUMBU (I-JJSUA0+1,IBUN)
   70   CONTINUE
      ENDIF

C
  999 RETURN
      END
#endif
