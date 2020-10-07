      SUBROUTINE LPREPO
C-----------------------------------------------------------------------
C! Unpack Lcal POT banks
C!
C!   Author   : P. H. Hansen   880801
C!   Input    : 'PLSD' 'PLPD'
C!   Output   : 'LSDA' 'LPDA'
C!   Created  : 'LSDA' 'LPDA'
C!
C!   Description :
C!   ===========
C?   Use POT banks to recreate standard input for Lcal reconstruction
C?
C-----------------------------------------------------------------------
#ifndef DOC
#include "rparac.h"
#include "bcs.h"
#include "rflags.h"
#include "lcnamc.h"
#include "lpotun.h"
#include "lsdajj.h"
#include "lpdajj.h"
#include "plsdjj.h"
#include "plpdjj.h"
      DIMENSION ESUM(4)
#include "bmacro.h"
C----------------------------------------------------------------------
C
C Recreate LSDA
      KPLSD = IW(NAPLSD)
      IF(KPLSD.LE.0)                                    GOTO 999
      NLSD = LROWS(KPLSD)
      LEN = LMHLEN + LLSDAA*NLSD
      CALL AUBOS('LSDA',0,LEN,KLSDA,IGARB)
      IF(IGARB.EQ.2)                                    GOTO 997
      CALL BLIST(IW,'T+','LSDA')
      IF(IGARB.NE.0) THEN
         KPLSD = IW(NAPLSD)
      ENDIF
      IW(KLSDA+1) = LLSDAA
      IW(KLSDA+2) = NLSD
      DO 100 I = 1,NLSD
         KPLS = KROW(KPLSD,I)
         KLSD = KROW(KLSDA,I)
         IW(KLSD+JLSDAD) = IW(KPLS+JPLSAD)
         RW(KLSD+JLSDEN) = FLOAT(IW(KPLS+JPLSE1))*UNITES
         RW(KLSD+JLSDEN+1) = FLOAT(IW(KPLS+JPLSE2))*UNITES
         RW(KLSD+JLSDEN+2) = FLOAT(IW(KPLS+JPLSE3))*UNITES
  100 CONTINUE
C
C
C Recreate LPDA
      KPLPD = IW(NAPLPD)
      IF(KPLPD.LE.0)                                    GOTO 999
      NPLP = LROWS(KPLPD)
      LEN = LMHLEN + LLPDAA*4
      CALL AUBOS('LPDA',0,LEN,KLPDA,IGARB)
      IF(IGARB.EQ.2)                                    GOTO 997
      CALL BLIST(IW,'T+','LPDA')
      IF(IGARB.NE.0) THEN
         KPLPD = IW(NAPLPD)
      ENDIF
      IW(KLPDA+1) = LLPDAA
      IW(KLPDA+2) = 4
      DO 200 MODU = 1,4
         KLPD = KROW(KLPDA,MODU)
         IW(KLPD+JLPDMO) = MODU
         CALL VZERO(RW(KLPD+JLPDEN),38)
         ESUM(MODU) = 0.
  200 CONTINUE
      DO 210 I = 1,NPLP
        IADDR = ITABL(KPLPD,I,JPLPAD)
        MODU = IADDR/64 + 1
        IPL = IADDR - 64*(MODU-1)
        KLPD = KROW(KLPDA,MODU)
        E = FLOAT(ITABL(KPLPD,I,JPLPEN))*UNITEW
        RW(KLPD+JLPDEN+IPL-1) = E
        ESUM(MODU) = ESUM(MODU) + E
  210 CONTINUE
C
      GOTO 999
  997 CALL REPORT('LPREPO','AUBOS failure',1)
  999 CONTINUE
      END
#endif
