      SUBROUTINE MINPCP
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Fill PCPA bank from DNEU and DRES.
C
C     Author: Stephen Haywood      17-Jan-91
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "dneujj.h"
#include "dresjj.h"
#include "pcpajj.h"
#include "minfac.h"
#include "bmacro.h"
C
      KDNEU = NLINK('DNEU',0)
      IF(KDNEU.GT.0) THEN
         NDNEU = LROWS(KDNEU)
      ELSE
         NDNEU = 0
      ENDIF
C
      KDRES = NLINK('DRES',0)
      IF(KDRES.GT.0) THEN
         NDRES = LROWS(KDRES)
      ELSE
         NDRES = 0
      ENDIF
C
C++   Create PCPA bank.
C
      NPCPA = NDNEU + NDRES
      IF(NPCPA.LE.0) RETURN
      LEN = LMHLEN + LPCPAA * NPCPA
      CALL AUBOS('PCPA',0,LEN, KPCPA,IGARB)
      CALL BLIST(IW,'S+','PCPA')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDNEU = NLINK('DNEU',0)
         KDRES = NLINK('DRES',0)
      ENDIF
      IW(KPCPA+LMHCOL) = LPCPAA
      IW(KPCPA+LMHROW) = NPCPA
C
C++   Fill PCPA bank from DNEU.
C
      J = 0
      DO 100 I=1,NDNEU
         J = J + 1
         IW(KROW(KPCPA,J)+JPCPNA) = ITABL(KDNEU,I,JDNENA)
         RW(KROW(KPCPA,J)+JPCPEN) = FLOAT(ITABL(KDNEU,I,JDNEE0))/EFACTM
         RW(KROW(KPCPA,J)+JPCPTE) = FLOAT(ITABL(KDNEU,I,JDNETH))/AFACTM
         RW(KROW(KPCPA,J)+JPCPFI) = FLOAT(ITABL(KDNEU,I,JDNEPH))/AFACTM
         RW(KROW(KPCPA,J)+JPCPR1) = FLOAT(ITABL(KDNEU,I,JDNER1))/10.
         RW(KROW(KPCPA,J)+JPCPR2) = FLOAT(ITABL(KDNEU,I,JDNER2))/10.
  100 CONTINUE
C
C++   Fill PCPA bank from DRES.
C
      DO 200 I=1,NDRES
         J = J + 1
         IW(KROW(KPCPA,J)+JPCPNA) = ITABL(KDRES,I,JDRENA)
         RW(KROW(KPCPA,J)+JPCPEN) = FLOAT(ITABL(KDRES,I,JDREE0))/EFACTM
         RW(KROW(KPCPA,J)+JPCPTE) = FLOAT(ITABL(KDRES,I,JDRETH))/AFACTM
         RW(KROW(KPCPA,J)+JPCPFI) = FLOAT(ITABL(KDRES,I,JDREPH))/AFACTM
         RW(KROW(KPCPA,J)+JPCPR1) = FLOAT(ITABL(KDRES,I,JDREP0))/EFACTM
         RW(KROW(KPCPA,J)+JPCPR2) = FLOAT(ITABL(KDRES,I,JDREPS))/EFACTM
  200 CONTINUE
C
      RETURN
      END
#endif
