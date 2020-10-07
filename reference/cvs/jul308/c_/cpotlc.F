      SUBROUTINE CPOTLC
C------------------------------------------------------------------
C!   Add Lcal to PEOB and PEPT banks
C!
C!   Authors  :  M.N.Minard, M.Pepe
C!   Input    : LCLU LOBJ PECO PEPT CHRL CALO
C!   Output   : PECO PEPT CHRL CALO
C!
C!   Modified :- M.N. Minard              9-DEC-1991
C!          Use LPES and LPSV to fill banks in case of POT->POT repro.
C!   Modified :- E. Lancon             23-JUN-1992
C!          In case of POT->POT repro. PCRL and PCOB should also be
C!          mofified! this should have been done in LCRPOT for
C!          normal processing.
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "bcs.h"
#include "rflags.h"
#include "lcnamc.h"
#include "pcnamc.h"
#include "lobjjj.h"
#include "lclujj.h"
#include "chrljj.h"
#include "calojj.h"
#include "pecojj.h"
#include "peptjj.h"
#include "pcrljj.h"
#include "alcons.h"
      EXTERNAL NAMIND
      LOGICAL REPRO
      DATA NALPSV / 0 /
#include "bmacro.h"
C-----------------------------------------------------------------
C
C Check if LCalobjects exist
C
      IF ( NALPSV.EQ.0) THEN
        NALPSV = NAMIND('LPSV')
        NALPES = NAMIND('LPES')
      ENDIF
C
      REPRO = IW(NAMIND('REPR')).NE.0
      KLPSV = IW(NALPSV)
      KLOBJ = IW(NALOBJ)
      LENGL  = 0
      IF ( KLPSV.GT.0 ) THEN
        LENGL = LROWS(KLPSV)
        IF ( LENGL.EQ.0 ) GOTO 999
      ELSE
        IF ( KLOBJ.GT.0 ) LENGL = LROWS(KLOBJ)
        IF ( LENGL.EQ.0 ) GOTO 999
      ENDIF
C
C
C Create or expand CHRL or PCRL bank
C
      IF (REPRO) THEN
        KPCRL = IW(NAMIND('PCRL'))
        NPCRL = 0
        IF(KPCRL.GT.0) NPCRL= LROWS(KPCRL)
        LEN = LMHLEN + (NPCRL+LENGL)*LPCRLA
        CALL AUBOS('PCRL',0,LEN,KPCRL,IGARB)
        IF(IGARB.EQ.2)                              GOTO 997
        CALL BLIST(IW,'E+','PCRL')
        IW(KPCRL+LMHCOL) = LPCRLA
        IW(KPCRL+LMHROW) = NPCRL+LENGL
      ELSE
        KCHRL = IW(NAMIND('CHRL'))
        NCHRL = 0
        IF(KCHRL.GT.0) NCHRL= LROWS(KCHRL)
        LEN = LMHLEN + (NCHRL+LENGL)*LCHRLA
        CALL AUBOS('CHRL',0,LEN,KCHRL,IGARB)
        IF(IGARB.EQ.2)                              GOTO 997
        CALL BLIST(IW,'E+','CHRL')
        IW(KCHRL+LMHCOL) = LCHRLA
        IW(KCHRL+LMHROW) = NCHRL+LENGL
      ENDIF
C
C Create or expand CALO bank
C
      KCALO = IW(NAMIND('CALO'))
      NCALO = 0
      IF(KCALO.GT.0) NCALO= LROWS(KCALO)
      LEN = LMHLEN + (NCALO+LENGL)*LCALOA
      CALL AUBOS('CALO',0,LEN,KCALO,IGARB)
      IF(IGARB.EQ.2)                              GOTO 997
      CALL BLIST(IW,'E+','CALO')
      IW(KCALO+LMHCOL) = LCALOA
      IW(KCALO+LMHROW) = NCALO+LENGL
C
C
C Create or expand PECO bank
C
      KPECO = IW(NAMIND('PECO'))
      NECO = 0
      IF(KPECO.GT.0) NECO = LROWS(KPECO)
      LEN = LMHLEN + (NECO+LENGL)*LPECOA
      CALL AUBOS('PECO',0,LEN,KPECO,IGARB)
      IF(IGARB.EQ.2)                              GOTO 997
      CALL BLIST(IW,'E+','PECO')
      IW(KPECO+LMHCOL) = LPECOA
      IW(KPECO+LMHROW) = NECO+LENGL
C
C Create or expand PEPT bank
C
      KPEPT = IW(NAMIND('PEPT'))
      NPEP = 0
      IF(KPEPT.GT.0) NPEP = LROWS(KPEPT)
      LEN = LMHLEN + (NPEP+LENGL)*LPEPTA
      CALL AUBOS('PEPT',0,LEN,KPEPT,IGARB)
      IF(IGARB.EQ.2)                              GOTO 997
      CALL BLIST(IW,'E+','PEPT')
      IW(KPEPT+LMHCOL) = LPEPTA
      IW(KPEPT+LMHROW) = NPEP+LENGL
C
C Restore bank pointers
C
      KLPSV = IW(NALPSV)
      KLPES = IW(NALPES)
      KLOBJ = IW(NALOBJ)
      KLCLU = IW(NALCLU)
      KCHRL = IW(NAMIND('CHRL'))
      KPCRL = IW(NAMIND('PCRL'))
      KPECO = IW(NAMIND('PECO'))
      KPCOB = IW(NAMIND('PCOB'))
      KPEPT = IW(NAMIND('PEPT'))
      KCALO = IW(NAMIND('CALO'))
      NPCOB = IW (KPCOB+LMHROW)
C
C Fill PECO banks
C

      DO 500 I=1,LENGL

        NECO    = NECO  +   1
        NPEP    = NPEP  +   1
        NCHRL   = NCHRL +   1
        NPCOB   = NPCOB +   1
        NPCRL   = NPCRL +   1
C
C-   CHRL
C
        IF (REPRO) THEN
          IW(KROW(KPCRL,NPCRL)+JPCRPC) = NCALO + I
          IW(KROW(KPCRL,NPCRL)+JPCRPE) = NECO
        ELSE
          IW(KROW(KCHRL,NCHRL)+JCHRCA) = NCALO + I
          IW(KROW(KCHRL,NCHRL)+JCHREC) = NECO
        ENDIF
C
C-   CALO / PCOB
C
        IF (REPRO) THEN
          RW(KROW(KCALO,NCALO+I)+JCALER) = RTABL(KLPSV,I,1)
          IW (KPCOB+LMHROW) = NPCOB
        ELSE
          RW(KROW(KCALO,NCALO+I)+JCALER) = RTABL(KLCLU,I,JLCLE4)
        ENDIF
C
C-   PECO
C
        JPECO = KROW(KPECO,NECO)
        IF (REPRO) THEN
          JLPSV = KROW(KLPSV,I)
          CALL UCOPY(RW(JLPSV+1),RW(JPECO+1),LPECOA)
        ELSE
          ERAW = RTABL(KLCLU,I,JLCLE4)
          RW(JPECO+JPECER) = ERAW
          RW(JPECO+JPECE1) = RTABL(KLCLU,I,JLCLE1)/ERAW
          RW(JPECO+JPECE2) = RTABL(KLCLU,I,JLCLE2)/ERAW
          RW(JPECO+JPECTH) = RTABL(KLCLU,I,JLCLT4)
          PHI = RTABL(KLCLU,I,JLCLF4)
          IF(PHI.LT.0.) PHI = PHI + TWOPI
          RW(JPECO+JPECPH) = PHI
          RW(JPECO+JPECEC) = RTABL(KLOBJ,I,JLOBEC)
          RW(JPECO+JPECKD) = RTABL(KLOBJ,I,JLOBKD)
        ENDIF
        IREF = JCHREC
        NREF = NECO
        IREL = JCHRCA
        CALL CALREL(IREF,NREF,IREL,NREL)
        IF ( NREL.NE.0) IW(JPECO+JPECPC) = NREL
        IW(JPECO+JPECRB) = 4
C
C PEPT
C
C
C Calculate  theta and phi for stacks 1 and 3
C
        JPEPT = KROW(KPEPT,NECO)
        IF (REPRO) THEN
          JLPES = KROW(KLPES,I)
          CALL UCOPY (IW(JLPES+1),IW(JPEPT+1),LPEPTA)
        ELSE
          RW(JPEPT+JPEPT1) = RTABL(KLCLU,I,JLCLT1)
          PHI = RTABL(KLCLU,I,JLCLF1)
          IF(PHI.LT.0.) PHI = PHI+TWOPI
          RW(JPEPT+JPEPP1) = PHI
          RW(JPEPT+JPEPT3) = RTABL(KLCLU,I,JLCLT3)
          PHI = RTABL(KLCLU,I,JLCLF3)
          IF(PHI.LT.0.) PHI = PHI+TWOPI
          RW(JPEPT+JPEPP3) = PHI
        ENDIF
  500 CONTINUE
C
      IF(FDEBRF.AND.JDBDRF(JULLC).GE.3) THEN
        CALL PRTABL('PECO',0)
        CALL PRTABL('PEPT',0)
      ENDIF
C
      GOTO 999
  997 CALL RERROR ('CPOTLC',1,'AUBOS failure')
  999 CONTINUE
      END
C
#endif
