      SUBROUTINE CEXHIS
C*************************************************************
C! EXTENDED CALOB HIST FILLING                               *
C!                                                           *
C! AUTHOR :  R. TENCHINI      880511                         *
C!                                                           *
C! Version 1.0  880506                                       *
C!                                                           *
C! INPUT   CEXT BANK                                         *
C!                                                           *
C! NSPTT0=Number  of superpatterns                           *
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rparac.h"
#include "rflags.h"
#include "crl3jj.h"
#include "cct1jj.h"
#include "hsphjj.h"
#include "cextjj.h"
      LOGICAL LNEUC
#include "bmacro.h"
C
      JOFF = JULHC * 1000
      ICRL3=NLINK('CRL3',0)
      IF(ICRL3.EQ.0) THEN
         CALL REPORT(' CEXHIS','CRL3 BANK NOT FOUND',   0)
         GOTO 30
      ENDIF
      ICCT1=NLINK('CCT1',0)
      IF(ICCT1.EQ.0) THEN
         CALL REPORT(' CEXHIS','CCT1 BANK NOT FOUND',   0)
         GOTO 30
      ENDIF
      NEUCX = 0
      NSPN0 = 0
      NCEXX=NAMIND('CEXT')
      ICEXT=IW(NCEXX)
      IF(ICEXT.EQ.0) GOTO 20
 101     NPATT = 0
         NHCLU = 0
         NECLU = 0
         NSPN0 = NSPN0 + 1
         NELEM=LROWS(ICEXT)
         LNEUC=.TRUE.
         DO 10 J=1,NELEM
            IDCOD=IW(KROW(ICEXT,J)+JCEXCA)
            ICHEK=IDCOD/10000
            ICLUS=MOD(IDCOD,10000)
C
C We check what is it
C
            IF(ICHEK.EQ.1) THEN
               NPATT=NPATT+1
            ELSEIF(ICHEK.EQ.2) THEN
               NECLU=NECLU+1
            ELSEIF(ICHEK.EQ.3) THEN
               NHCLU=NHCLU+1
               JRELA=ITABL(ICCT1,ICLUS,JCCTCR)
               IF(JRELA.NE.0) THEN
 70               ITRCK=ITABL(ICRL3,JRELA,JCRLCT)
                  IF(ITRCK.NE.0) LNEUC=.FALSE.
                  INEXH=ITABL(ICRL3,JRELA,JCRLNH)
                  JRELA=INEXH
                  IF(JRELA.NE.0) GO TO 70
               ENDIF
            ENDIF
   10    CONTINUE
         IF(LNEUC) NEUCX=NEUCX+1
      CALL HFILL(JOFF+312,FLOAT(NPATT),0.,1.)
      CALL HFILL(JOFF+313,FLOAT(NECLU),0.,1.)
      CALL HFILL(JOFF+314,FLOAT(NHCLU),0.,1.)
      CALL HFILL(JOFF+316,FLOAT(NPATT),FLOAT(NECLU),1.)
      CALL HFILL(JOFF+317,FLOAT(NPATT),FLOAT(NHCLU),1.)
      ICEXT= IW(ICEXT-1)
      IF(ICEXT.NE.0) GOTO 101
   20 CONTINUE
      CALL HFILL(JOFF+311,FLOAT(NSPN0),0.,1.)
      CALL HFILL(JOFF+315,FLOAT(NEUCX),0.,1.)
   30 RETURN
      END
#endif
