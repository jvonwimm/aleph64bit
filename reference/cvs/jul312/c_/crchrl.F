      SUBROUTINE CRCHRL(NSPNH)
C***********************************************************
C! Create calob CHRL relation bank                         *
C! Author: R.Tenchini           881129                     *
C!                                                         *
C! NSPNH = H Superpattern Number                           *
C! INPUT Banks : CRL3, HSPH, COCR                          *
C! OUTPUT Banks : CHRL                                     *
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "ecnamc.h"
#include "hcnamc.h"
#include "ccnamc.h"
#include "cct1jj.h"
#include "ecobjj.h"
#include "eclujj.h"
#include "hclujj.h"
#include "hpcojj.h"
#include "ctc2jj.h"
#include "crl3jj.h"
#include "erl3jj.h"
#include "cocrjj.h"
#include "calojj.h"
#include "cextjj.h"
#include "etp1jj.h"
#include "hspejj.h"
#include "hsphjj.h"
#include "etckjj.h"
#include "chrljj.h"
      PARAMETER (MAXPAT=100)
      COMMON/WPCRL/IPCRL,IPTWK
      LOGICAL TROVAT
      LOGICAL FIRST,GARFL
      DATA FIRST/.TRUE./
#include "bmacro.h"
C
C Indices initialization and return point for garbage collection
C
      GARFL=.FALSE.
 500  CONTINUE
      KCCT1=IW(NACCT1)
      KCTC2=IW(NACTC2)
      KCRL3=IW(NACRL3)
      KERL3=IW(NAERL3)
      KCOCR=IW(NACOCR)
      KCALO=IW(NACALO)
      KETP1=IW(NAETP1)
      KETCK=IW(NAETCK)
      KECOB=IW(NAECOB)
      KHPCO=NLINK('HPCO',0)
      NHCLU=0
      IF(KCCT1.NE.0) THEN
         NHCLU=LROWS(KCCT1)
      ENDIF
      NECOB=0
      IF(KCOCR.NE.0) THEN
         NECOB=LROWS(KCOCR)
      ENDIF
      NCRL3=0
      IF(KCRL3.NE.0) THEN
         NCRL3=LROWS(KCRL3)
      ENDIF
      NERL3=0
      IF(KERL3.NE.0) THEN
         NERL3=LROWS(KERL3)
      ENDIF
      NCALO=0
      IF(KCALO.NE.0) THEN
         NCALO=LROWS(KCALO)
      ENDIF
      NHPCO=0
      IF(KHPCO.NE.0) THEN
         NHPCO=LROWS(KHPCO)
      ENDIF
      NTRCK=0
      IF(KETP1.NE.0) THEN
         NTRCK=LROWS(KETP1)
      ENDIF
C
C  We define a working bank of maximum length
C
      IF(FIRST) THEN
         FIRST=.FALSE.
         IPCRL=0
         IPTWK=0
      ENDIF
      LNGTH=(NCRL3*(NHPCO+1)+NECOB+NERL3+(NHCLU+1)*(NHPCO+1))
     &       *LCHRLA+LMHLEN
      CALL WBANK(IW,IPCRL,LNGTH,*998)
      LMAXI=MAXPAT
      CALL WBANK(IW,IPTWK,LMAXI,*998)
C
C Headers
C
      IW(IPCRL+LMHCOL)=LCHRLA
      IW(IPCRL+LMHROW)=0
C
C Loop over CRL3 relations
C
      DO 100 I=1,NCRL3
         IECAL=ITABL(KCRL3,I,JCRLCO)
         IHCAL=ITABL(KCRL3,I,JCRLCC)
C
C Relations without HCAL (... should never happen...)
C
         IF(IHCAL.EQ.0) THEN
            INDPCR=KNEXT(IPCRL)
            ICALO=ITABL(KCOCR,IECAL,JCOCCA)
            IW(INDPCR+JCHREC)=IECAL
            IW(INDPCR+JCHRFR)=ITABL(KCRL3,I,JCRLCT)
            IW(INDPCR+JCHRHC)=IHCAL
            IW(INDPCR+JCHRCA)=ICALO
            IW(INDPCR+JCHRHP)=0
            IW(IPCRL+LMHROW)=IW(IPCRL+LMHROW)+1
         ELSE
C
C Relations with HCAL
C
            TROVAT=.FALSE.
            DO 101 J=1,NSPNH
               IF(TROVAT) GO TO 101
               IHSPH=NLINK('HSPH',J)
               NELEM=LROWS(IHSPH)
               INPATT=0
               DO 115 K=1,NELEM
                  IDCOD=ITABL(IHSPH,K,JHSPHR)
                  ICHEK=IDCOD/10000
                  ICLUS=MOD(IDCOD,10000)
                  IF(ICHEK.EQ.1) THEN
                     INPATT=INPATT+1
                     IF(INPATT.GT.MAXPAT) THEN
                        CALL RERROR('CRCHRL',1,' TOO MANY PATT')
                        GO TO 115
                     ENDIF
                     IW(IPTWK+INPATT)=ICLUS
                  ELSEIF(ICHEK.EQ.3) THEN
                     IF(ICLUS.EQ.IHCAL) TROVAT=.TRUE.
                  ENDIF
 115           CONTINUE
               IF(TROVAT) THEN
                  DO 102 L=1,INPATT
                     INDPCR=KNEXT(IPCRL)
                     ICALO=ITABL(KCCT1,IHCAL,JCCTCA)
                     IW(INDPCR+JCHREC)=IECAL
                     IW(INDPCR+JCHRFR)=ITABL(KCRL3,I,JCRLCT)
                     IW(INDPCR+JCHRHC)=IHCAL
                     IW(INDPCR+JCHRCA)=ICALO
                     IW(INDPCR+JCHRHP)=IW(IPTWK+L)
                     IW(IPCRL+LMHROW)=IW(IPCRL+LMHROW)+1
 102              CONTINUE
               ENDIF
 101        CONTINUE
            IF(.NOT.TROVAT) THEN
               INDPCR=KNEXT(IPCRL)
               ICALO=ITABL(KCCT1,IHCAL,JCCTCA)
               IW(INDPCR+JCHREC)=IECAL
               IW(INDPCR+JCHRFR)=ITABL(KCRL3,I,JCRLCT)
               IW(INDPCR+JCHRHC)=IHCAL
               IW(INDPCR+JCHRCA)=ICALO
               IW(INDPCR+JCHRHP)=0
               IW(IPCRL+LMHROW)=IW(IPCRL+LMHROW)+1
            ENDIF
         ENDIF
 100  CONTINUE
C
C    Create relations for isolated ECOBS
C
      DO 200 IEC=1,NECOB
      IRELA=ITABL(KCOCR,IEC,JCOCCR)
      IF(IRELA.EQ.0)THEN
         ITRAC=ITABL(KECOB,IEC,JECOET)
         IF(ITRAC.NE.0)THEN
  210       CONTINUE
            INDPCR=KNEXT(IPCRL)
            IW(INDPCR+JCHREC)=IEC
            IW(INDPCR+JCHRFR)=ITRAC
            IW(INDPCR+JCHRHC)=0
            IW(INDPCR+JCHRCA)=ITABL(KCOCR,IEC,JCOCCA)
            IW(INDPCR+JCHRHP)=0
            IW(IPCRL+LMHROW)=IW(IPCRL+LMHROW)+1
            ITRAC=ITABL(KETCK,ITRAC,JETCET)
            IF(ITRAC.NE.0)GO TO 210
         ELSE
            INDPCR=KNEXT(IPCRL)
            IW(INDPCR+JCHREC)=IEC
            IW(INDPCR+JCHRFR)=0
            IW(INDPCR+JCHRHC)=0
            IW(INDPCR+JCHRCA)=ITABL(KCOCR,IEC,JCOCCA)
            IW(INDPCR+JCHRHP)=0
            IW(IPCRL+LMHROW)=IW(IPCRL+LMHROW)+1
         ENDIF
      ENDIF
  200 CONTINUE
C
C   Create relations for isolated hclusters
C
      DO 300 IHCAL=1,NHCLU
         IRELA=ITABL(KCCT1,IHCAL,JCCTCR)
         IF(IRELA.EQ.0)THEN
            TROVAT=.FALSE.
            DO 301 J=1,NSPNH
               IF(TROVAT) GO TO 301
                  IHSPH=NLINK('HSPH',J)
                  NELEM=LROWS(IHSPH)
                  INPATT=0
                  DO 315 K=1,NELEM
                     IDCOD=ITABL(IHSPH,K,JHSPHR)
                     ICHEK=IDCOD/10000
                     ICLUS=MOD(IDCOD,10000)
                     IF(ICHEK.EQ.1) THEN
                        INPATT=INPATT+1
                        IF(INPATT.GT.MAXPAT) THEN
                           CALL RERROR('CRCHRL',2,' TOO MANY PATT')
                           GO TO 315
                        ENDIF
                        IW(IPTWK+INPATT)=ICLUS
                     ELSEIF(ICHEK.EQ.3) THEN
                        IF(ICLUS.EQ.IHCAL) TROVAT=.TRUE.
                     ENDIF
 315              CONTINUE
                  IF(TROVAT) THEN
C
C Hclus + patterns
C
                     DO 302 L=1,INPATT
                        INDPCR=KNEXT(IPCRL)
                        IW(INDPCR+JCHREC)=0
                        IW(INDPCR+JCHRFR)=0
                        IW(INDPCR+JCHRHC)=IHCAL
                        IW(INDPCR+JCHRCA)=ITABL(KCCT1,IHCAL,JCCTCA)
                        IW(INDPCR+JCHRHP)=IW(IPTWK+L)
                        IW(IPCRL+LMHROW)=IW(IPCRL+LMHROW)+1
 302                 CONTINUE
                  ENDIF
 301           CONTINUE
C
C Hclus without patterns
C
            IF(.NOT.TROVAT) THEN
               INDPCR=KNEXT(IPCRL)
               IW(INDPCR+JCHREC)=0
               IW(INDPCR+JCHRFR)=0
               IW(INDPCR+JCHRHC)=IHCAL
               IW(INDPCR+JCHRCA)=ITABL(KCCT1,IHCAL,JCCTCA)
               IW(INDPCR+JCHRHP)=0
               IW(IPCRL+LMHROW)=IW(IPCRL+LMHROW)+1
            ENDIF
         ENDIF
 300  CONTINUE
C
C   Create relations for isolated patterns
C
      DO 400 IPATT=1,NSPNH
         IHSPH=NLINK('HSPH',IPATT)
         NELEM=LROWS(IHSPH)
         IF(NELEM.EQ.1) THEN
            IDCOD=ITABL(IHSPH,1,JHSPHR)
            ICHEK=IDCOD/10000
            ICLUS=MOD(IDCOD,10000)
            IF(ICHEK.EQ.1) THEN
               INDPCR=KNEXT(IPCRL)
               IW(INDPCR+JCHREC)=0
               IW(INDPCR+JCHRFR)=0
               IW(INDPCR+JCHRHC)=0
               IW(INDPCR+JCHRCA)=0
               IW(INDPCR+JCHRHP)=ICLUS
               IW(IPCRL+LMHROW)=IW(IPCRL+LMHROW)+1
            ENDIF
         ENDIF
 400  CONTINUE
C
C THE WORKING BANK CONTENT IS COPIED TO CHRL
C
      LENGG=LMHLEN+LCHRLA*LROWS(IPCRL)
      CALL AUBOS('CHRL',0,LENGG,KCHRL,IGARB)
      IF(IGARB.EQ.2) THEN
         CALL RERROR('CRCHRL',3,' NOT ENOUGH SPACE AFTER G.C.')
         GO TO 999
      ENDIF
      CALL UCOPY(IW(IPCRL+1),IW(KCHRL+1),LENGG)
 999  CONTINUE
      CALL WDROP(IW,IPCRL)
      CALL WDROP(IW,IPTWK)
      RETURN
 998  CONTINUE
      IF(.NOT.GARFL) THEN
         GARFL=.TRUE.
         CALL BGARB(IW)
         CALL WGARB(IW)
         GO TO 500
      ELSE
         CALL RERROR('CRCHRL',4,' NOT ENOUGH SPACE FOR WORKING BANK')
      ENDIF
      CALL WDROP(IW,IPCRL)
      CALL WDROP(IW,IPTWK)
      RETURN
      END
#endif
