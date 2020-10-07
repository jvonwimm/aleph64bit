      SUBROUTINE LSINGL(ITYPE)
C-----------------------------------------------------------------------
C! Store single arm triggers
C!
C!   Author   : P. H. Hansen  890701
C!   Modified : P. H. Hansen  960501
C!       (restrict to one-cluster evts, check LCAL HV)
C!
C!     Input  : ITYPE            Trigger type:
C!                               1: Exclusive single low threshold
C!                               2: Exclusive single high threshold
C!              'LIDT'           Lumi electron candidate bank
C!              'LIFL'           Lumi track bank
C!     Output :
C!              'LSIN'           Singles storage
C!
C!   Description :
C!   ===========
C?   Book or expand singles bank if needed
C?   Find highest energy cluster
C?   Transfer the relevant attributes to singles bank
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "rflags.h"
#include "rconds.h"
#include "lcnamc.h"
#include "lidtjj.h"
#include "lifljj.h"
#include "lsinjj.h"
C
#include "bmacro.h"
C----------------------------------------------------------------------
C
C Book or expand singles bank if needed
      KLIDT = IW(NALIDT)
      IF(KLIDT.LE.0)                                   GOTO 999
      CALL LHVSTA(IHV)
      IF(IHV.EQ.0)                                     GOTO 999
      KLSIN = IW(NALSIN)
      IF(KLSIN.LE.0) THEN
        NSING = 1
        LEN = LMHLEN + 12*LLSINA
        CALL AUBOS('LSIN',0,LEN,KLSIN,IGARB)
        IF(IGARB.EQ.2)                                 GOTO 998
      ELSE
        NSING = IW(KLSIN+2) + 1
        IF(NSING*LLSINA+2.GE.IW(KLSIN)) THEN
          LEN = IW(KLSIN) + 12*LLSINA
          CALL AUBOS('LSIN',0,LEN,KLSIN,IGARB)
          IF(IGARB.EQ.2)                               GOTO 998
        ENDIF
      ENDIF
      IW(KLSIN+1) = LLSINA
      IW(KLSIN+2) = NSING
C
C Only one cluster above 30% of beam energy allowed
      KLIDT = IW(NALIDT)
      KLIFL = IW(NALIFL)
      IDT = 0
      NDT = 0
      DO 100 I=1,LROWS(KLIDT)
        IF(RTABL(KLIDT,I,JLIDEC).GT.0.15*ENERRC) THEN
          NDT = NDT+1
          IDT = I
        ENDIF
  100 CONTINUE
      IF(NDT.NE.1)                                     GOTO 999
C
C Tranfer cluster attributes to LSIN
      KLSI = KROW(KLSIN,NSING)
      IW(KLSI+JLSIK1) = ITABL(KLIFL,IDT,JLIFK1)
      IW(KLSI+JLSIK2) = ITABL(KLIFL,IDT,JLIFK2)
      IW(KLSI+JLSIKH) = ITABL(KLIFL,IDT,JLIFKH)
      IW(KLSI+JLSIKL) = ITABL(KLIFL,IDT,JLIFKL)
      RW(KLSI+JLSITH) = RTABL(KLIDT,IDT,JLIDTC)
      RW(KLSI+JLSIPH) = RTABL(KLIDT,IDT,JLIDPC)
      RW(KLSI+JLSIEN) = RTABL(KLIDT,IDT,JLIDEC)
      IW(KLSI+JLSIIF) = ITABL(KLIDT,IDT,JLIDIF)
      IW(KLSI+JLSIIT) = ITYPE
C
      GOTO 999
C
  998 CALL REPORT('LSINGL','AUBOS failure',1)
  999 CONTINUE
      END
#endif
