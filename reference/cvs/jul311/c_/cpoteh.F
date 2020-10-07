      SUBROUTINE CPOTEH(IER)
C--------------------------------------------------------------------
C! Create banks for POT output
C!
C!   Authors:  M.N.Minard, M.Pepe
C!
C!       Banks  :
C!        Input : ECLU,ECOB,HCLU,EHYP
C!        Output: PECO,PHCO,PEPT
C!
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "alcons.h"
#include "ecnamc.h"
#include "hcnamc.h"
#include "ccnamc.h"
#include "pcnamc.h"
#include "eclujj.h"
#include "ecobjj.h"
#include "ehypjj.h"
#include "hclujj.h"
#include "chrljj.h"
#include "pecojj.h"
#include "phcojj.h"
#include "peptjj.h"
      EXTERNAL NAMIND
#include "bmacro.h"
C
C Clear previous events banks ?
C
      KPECO = NDROP('PECO',0)
      KPHCO = NDROP('PHCO',0)
      KPEPT = NDROP('PEPT',0)
C
C Create and fill PECO
C
      KECOB = IW(NAMIND('ECOB'))
      IF (KECOB.EQ.0) GO TO 50
      NPECO = LROWS(KECOB)
      CALL AUBOS('PECO',0,NPECO*LPECOA+LMHLEN,KPECO,IGARB)
      IF(IGARB.EQ.2)GO TO 998
      CALL BLIST(IW,'E+','PECO')
      IW(KPECO+LMHCOL)=LPECOA
      IW(KPECO+LMHROW)=NPECO
      IF(IGARB.EQ.1) KECOB= IW(NAECOB)
      KEHYP = IW(NAEHYP)
      NEHYP = 0
      IF (KEHYP.NE.0) NEHYP = LROWS(KEHYP)
      KECLU = IW(NAECLU)
C
C Fill PECO data
C
      DO 10 IC=1,NPECO
        JPECO = KROW(KPECO,IC)
        ERAW = RTABL(KECOB,IC,JECOEN)+RTABL(KECOB,IC,JECOEN+1)
     &       + RTABL(KECOB,IC,JECOEN+2)
        ETOT = RTABL(KECOB,IC,JECOEG)
        RW (JPECO+JPECER) = ERAW
        RW (JPECO+JPECE1) = RTABL(KECOB,IC,JECOEN) / ERAW
        RW (JPECO+JPECE2) = RTABL(KECOB,IC,JECOEN+1) / ERAW
C
C Fill energy weighted theta and phi in PECO
C
        RW (JPECO+JPECTH) = RTABL(KECLU,IC,JECLT4)
        PHI = RTABL(KECLU,IC,JECLF4)
        IF(PHI.LT.0.) PHI = PHI + TWOPI
        RW (JPECO+JPECPH) = PHI
C
C Corrected energy set for the moment equal to raw energy
C
        RW (JPECO+JPECEC) = ETOT
        IW (JPECO+JPECKD) = ITABL(KECOB,IC,JECORC)
C
C Get correction code from EHYP
C
        IHYP = ITABL(KECOB,IC,JECOEH)
        IF (IHYP.GT.0.AND.IHYP.LE.NEHYP) THEN
           KETY = ITABL(KEHYP,IHYP,JEHYTY)/64
           IF( KETY .GT. 0 ) KETY = KETY /10
           IW(JPECO+JPECCC) = KETY
        ENDIF
C
C
C Check whether PECO-Track relation exists
C
        IF (ITABL(KECOB,IC,JECONT).GT.0) IW(JPECO+JPECRB) = 1
C
C Check whether PECO-HCAL relation exists
C
        IREF = JCHREC
        IREL = JCHRHC
        CALL CALREL(IREF,IC,IREL,NREL)
        IF (NREL.GT.0) IW(JPECO+JPECRB) = IW(JPECO+JPECRB) + 2
C
C Get Calobject number
C
        IREL = JCHRCA
        CALL CALREL(IREF,IC,IREL,NREL)
        IW(JPECO+JPECPC) = NREL
 10   CONTINUE

      NPEPT=NPECO
C
C Fill PEPT for all PECO
C
      CALL AUBOS('PEPT',0,NPEPT*LPEPTA+LMHLEN,KPEPT,IGARB)
      IF(IGARB.EQ.2)GO TO 998
      CALL BLIST(IW,'E+','PEPT')
      IW(KPEPT+LMHCOL) = LPEPTA
      IW(KPEPT+LMHROW) = NPEPT
      IF(IGARB.EQ.1) KPECO = IW(NAPECO)
C
C Fill PEPT data
C
      KECLU=IW(NAECLU)
      IF (KECLU.EQ.0) GOTO 50

      DO 30 IC=1,NPEPT
        JPEPT = KROW(KPEPT,IC)
C
C Calculate  theta and phi for stacks 1 & 3
C
        RW(JPEPT+JPEPT1) = RTABL(KECLU,IC,JECLT1)
        PHI = RTABL(KECLU,IC,JECLF1)
        IF(PHI.LT.0.) PHI = PHI+TWOPI
        RW(JPEPT+JPEPP1) = PHI
        RW(JPEPT+JPEPT3) = RTABL(KECLU,IC,JECLT3)
        PHI = RTABL(KECLU,IC,JECLF3)
        IF(PHI.LT.0.) PHI = PHI+TWOPI
        RW(JPEPT+JPEPP3) = PHI
C
C-      Store dead storeys information in PECO
C
        IF ( ITABL(KECLU,IC,JECLCH).NE.0 ) THEN
           IW(KROW(KPECO,IC)+JPECCC) = 4
        ENDIF
 30   CONTINUE

 50   CONTINUE
C
C Create and fill PHCO
C
      KHCLU = IW(NAMIND('HCLU'))
      IF (KHCLU.LE.0) GOTO 70
      NHCLU = LROWS(KHCLU)
      CALL AUBOS('PHCO',0,NHCLU*LPHCOA+LMHLEN,KPHCO,IGARB)
      IF (IGARB.EQ.2) GO TO 997
      CALL BLIST(IW,'E+','PHCO')
      IF (IGARB.EQ.1) KHCLU = IW(NAHCLU)
      IW(KPHCO+LMHCOL) = LPHCOA
      IW(KPHCO+LMHROW) = NHCLU

      DO 60 IHCL=1,NHCLU
        JPHCO = KROW (KPHCO,IHCL)
        RW(JPHCO+JPHCER) = RTABL(KHCLU,IHCL,JHCLER)
C
C Corrected energy set for the moment equal to raw energy
C
        RW(JPHCO+JPHCEC) = RTABL(KHCLU,IHCL,JHCLEC)
        IW(JPHCO+JPHCKD) = ITABL(KHCLU,IHCL,JHCLKD)
        IW(JPHCO+JPHCNF) = ITABL(KHCLU,IHCL,JHCLNF)
        RW(JPHCO+JPHCTH) = RTABL(KHCLU,IHCL,JHCLTC)
        PHI = RTABL(KHCLU,IHCL,JHCLPC)
        IF(PHI.LT.0.) PHI = PHI + TWOPI
        RW(JPHCO+JPHCPH) = PHI
C
C Correction code not existing for the moment
C
        IW(JPHCO+JPHCCC) = 0
C
C Check whether PHCO-Track relation exists
C
        IREF = JCHRHC
        IREL = JCHRFR
        CALL CALREL(IREF,IHCL,IREL,NREL)
        IF (NREL.GT.0) IW(JPHCO+JPHCRB) = 1
C
C Check whether PHCO-ECAL relation exists
C
        IREL = JCHREC
        CALL CALREL(IREF,IHCL,IREL,NREL)
        IF (NREL.GT.0) IW(JPHCO+JPHCRB) = IW(JPHCO+JPHCRB) + 2
C
C Get Calobject number
C
        IREL = JCHRCA
        CALL CALREL(IREF,IHCL,IREL,NREL)
        IW(JPHCO+JPHCPC) = NREL
 60   CONTINUE

 70   CONTINUE
      IER=0
      RETURN

  997 CALL RERROR(' CPOTEH',1,' no space for bank PHCO')
      IER=1
      RETURN
  998 CALL RERROR(' CPOTEH',2,' no space for bank PECO or PEPT')
      IER=1
      RETURN
      END
#endif
