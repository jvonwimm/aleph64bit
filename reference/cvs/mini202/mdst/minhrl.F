      SUBROUTINE MINHRL
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill Hcal tower-digital relation DHRL for Mini-DST.
C
C     Author: Stephen Haywood      22-Jan-90
C
C     Input  : PCRL bank
C     Output : DHRL bank
C
C     Called by MINDST
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "pcrljj.h"
#include "dhrljj.h"
C
      COMMON / DHRWRK / KPCRLL
C
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST / .TRUE. /
C
#include "bmacro.h"
C
C++   Initialise work bank.
C
      IF(FIRST) THEN
         KPCRLL = 0
         FIRST = .FALSE.
      ENDIF
C
C++   Pick up PCRL bank.
C
      KPCRL = NLINK('PCRL',0)
      IF(KPCRL.GT.0) THEN
         NPCRL = LROWS(KPCRL)
      ELSE
         NPCRL = 0
      ENDIF
      IF(NPCRL.LE.0) RETURN
C
C++   Create DHRL bank.
C
      NDHRL = NPCRL
      LEN = LMHLEN + LDHRLA * NDHRL
      CALL AUBOS('DHRL',0,LEN, KDHRL,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINHRL: Cannot create DHRL bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KPCRL = NLINK('PCRL',0)
      ENDIF
      IW(KDHRL+LMHCOL) = LDHRLA
      IW(KDHRL+LMHROW) = NDHRL
C
C++   Identify non-repeated relations between PHCO and PPOB.
C
C++   First step is to assign each PCRL an identifier containing
C++   PHCO/PPOB - this should be unique.
C++   In practice, use a decimal code, which may not be unique,
C++   but the likelihood of this must be neglible.
C++   The code is negated if it does not represent an interesting
C++   relation.
C
      CALL WBANK(IW,KPCRLL,2*NPCRL+1,*999)
C
      DO 100 I=1,NPCRL
         NREL = 0
         IPHCO = ITABL(KPCRL,I,JPCRPH)
         IPPOB = ITABL(KPCRL,I,JPCRPP)
         IF(IPHCO.GT.0) NREL = NREL + 1
         IF(IPPOB.GT.0) NREL = NREL + 1
         KODE = 100*IPHCO + IPPOB
         IF(NREL.LE.1) KODE = -KODE
         IW(KPCRLL+I) = KODE
  100 CONTINUE
C
C++   Sort the PCRL banks according to the PHCO/PPOB code.
C
      CALL SORTZV(IW(KPCRLL+1),IW(KPCRLL+NPCRL+1),NPCRL,-1,0,0)
C
C++   Loop over sorted PCRL bank, according to index.
C++   Fill DHRL provided entry is different from previous one.
C++   PHCO and PPOB are parallel to DHCO and DPOB.
C
      LAST = -1
      IDHRL = 0
C
      DO 200 I=1,NPCRL
         INDX = IW(KPCRLL+NPCRL+I)
         KODE = IW(KPCRLL+INDX)
         IF(KODE.LE.   0) GOTO 200
         IF(KODE.EQ.LAST) GOTO 200
         LAST = KODE
         IDHRL = IDHRL + 1
         IW(KROW(KDHRL,IDHRL)+JDHRDH) = ITABL(KPCRL,INDX,JPCRPH)
         IW(KROW(KDHRL,IDHRL)+JDHRDP) = ITABL(KPCRL,INDX,JPCRPP)
  200 CONTINUE
C
C++   Compress bank to required size, and add it to the Mini list.
C
      NDHRL = IDHRL
      IF(NDHRL.GT.0) THEN
         LEN = LMHLEN + LDHRLA * NDHRL
         CALL AUBOS('DHRL',0,LEN, KDHRL,IGARB)
         IW(KDHRL+LMHROW) = NDHRL
         CALL MINLIS('DHRL')
      ELSE
         CALL BDROP(IW,'DHRL')
      ENDIF
C
C++   Drop work banks.
C
      CALL WDROP(IW,KPCRLL)
C
      RETURN
C
C++   Faillure.
C
  999 WRITE(IW(6),'('' MINHRL: Cannot create work bank for DHRL'')')
C
      RETURN
      END
#endif
