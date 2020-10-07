      SUBROUTINE MINPCR
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Fill PCRL bank from DCRL.
C
C     Author: Stephen Haywood      03-Apr-90
C
C     It appears that the PCRL bank is essential for the operation of
C     ALPHA.
C     If no DCRL bank exists, a dummy PCRL with one empty row is made.
C     This is necessary since, if there are simple isolated calobjects,
C     they will not appear in the DCRL bank, and if all objects are of
C     this form, then the DCRL bank will be empty.
C     It would be better to create rows in the PCRL bank with the
C     trivial relations in (ie. one object, and the other entries zero).
C     However, this requires looking at PECO/PHCO banks and reduces
C     simplicity.
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "dcrljj.h"
#include "pcrljj.h"
#include "bmacro.h"
C-----------------------------------------------------------------------
C
      IF (IW(NAMIND('PCRL')).GT.0) RETURN
      KDCRL = NLINK('DCRL',0)
      IF(KDCRL.GT.0) THEN
         NDCRL = LROWS(KDCRL)
      ELSE
         NDCRL = 0
      ENDIF
      IF(NDCRL.GT.0) THEN
         NPCRL = NDCRL
      ELSE
         NPCRL = 1
      ENDIF
C
C++   Create PCRL bank.
C
      LEN = LMHLEN + LPCRLA * NPCRL
      CALL AUBOS('PCRL',0,LEN, KPCRL,IGARB)
      CALL BLIST(IW,'S+','PCRL')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDCRL = NLINK('DCRL',0)
      ENDIF
      IW(KPCRL+LMHCOL) = LPCRLA
      IW(KPCRL+LMHROW) = NPCRL
C
C++   Fill PCRL bank.
C
      DO 100 I=1,NDCRL
         IW(KROW(KPCRL,I)+JPCRPE) = ITABL(KDCRL,I,JDCRDE)
         IW(KROW(KPCRL,I)+JPCRPF) = ITABL(KDCRL,I,JDCRDT)
         IW(KROW(KPCRL,I)+JPCRPH) = ITABL(KDCRL,I,JDCRDH)
  100 CONTINUE
C
      RETURN
      END
#endif
