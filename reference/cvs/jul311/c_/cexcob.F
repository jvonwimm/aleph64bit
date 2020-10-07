      SUBROUTINE CEXCOB(NSPN0)
C*************************************************************
C! Extended Calob Reconstruction                             *
C!                                                           *
C! Author :  R. TENCHINI      880506                         *
C!                                                           *
C! Version 1.0  880506                                       *
C!                                                           *
C! Input   HSPH FAMILY OF BANKS, CRL3 CCT1 BANKS             *
C! Output  CEXT BANK                                         *
C!                                                           *
C! NSPTT0=Number  of superpatterns                           *
C!                                                           *
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "hwbnkw.h"
#include "rlunit.h"
#include "crl3jj.h"
#include "cct1jj.h"
#include "hsphjj.h"
#include "cextjj.h"
      LOGICAL DUMP
      LOGICAL DUPLI
      LOGICAL FIRST
      DATA FIRST/.TRUE./
#include "bmacro.h"
C
      DUMP=.FALSE.
      IF(NSPN0.EQ.0) GOTO 30
C
C CEXT is initialized
C
      NR=0
C
C  We link the proper banks
C
      NCRL3=NAMIND('CRL3')
      ICRL3=IW(NCRL3)
      IF(ICRL3.EQ.0) THEN
         GOTO 30
      ELSE IF (LROWS(ICRL3).EQ.0) THEN
         CALL RERROR('CEXCOB',1,'CRL3 BANK NO ROWS')
         GOTO 30
      ENDIF
      NCCT1=NAMIND('CCT1')
      ICCT1=IW(NCCT1)
      IF(ICCT1.EQ.0) THEN
         GOTO 30
      ELSE IF (LROWS(ICCT1).EQ.0) THEN
         CALL RERROR('CEXCOB',2,'CCT1 BANK NO ROWS')
         GOTO 30
      ENDIF
      NECLU=0
      IECLU=NLINK('ECLU',0)
      IF(IECLU.NE.0) THEN
         NECLU=LROWS(IECLU)
      ENDIF
      NHCLU=0
      IHCLU=NLINK('HCLU',0)
      IF(IHCLU.NE.0) THEN
         NHCLU=LROWS(IHCLU)
      ENDIF
      NHPCO=0
      IHPCO=NLINK('HPCO',0)
      IF(IHPCO.NE.0) THEN
         NHPCO=LROWS(IHPCO)
      ELSE
         GO TO 30
      ENDIF
      LNGTH=LMHLEN+NECLU+NHCLU+NHPCO
      IF(FIRST) THEN
         FIRST=.FALSE.
         ICEXHW = 0
      ENDIF
      CALL WBANK(IW,ICEXHW,LNGTH,*230)
C
C We loop over H Superpatterns and for each HCLU that
C belongs to them we search in CRL3 to check if a relation
C to an ECOB exists. If we find it it is added to CEXT
C
      DO 20 I=1,NSPN0
         IHSPH=NLINK('HSPH',I)
         ND=IW(IHSPH)
         NNOLD=ND
         NELEM=LROWS(IHSPH)
C
C CEXT is defined and all the content of HSPH is copied to it
C
         NR=NR+1
         CALL UCOPY(IW(IHSPH+1),IW(ICEXHW+1),ND)
         DO 10 J=1,NELEM
         IDCOD=IW(KROW(IHSPH,J)+JHSPHR)
         ICHEK=IDCOD/10000
         ICLUS=MOD(IDCOD,10000)
C
C We check if it is an HCLU (ICHEK=30000)
C
            IF(ICHEK.EQ.3) THEN
               JRELA=ITABL(ICCT1,ICLUS,JCCTCR)
               IF(JRELA.NE.0) THEN
 21               IECOB=ITABL(ICRL3,JRELA,JCRLCO)
C
C If a really new ECOB is found we add it to CEXT
C
                  DUPLI=.FALSE.
                  DO 40 K=NNOLD+1,ND
                     JCLUS=MOD(IW(ICEXHW+K),10000)
                     IF(IECOB.EQ.JCLUS) DUPLI=.TRUE.
 40               CONTINUE
                  IF(IECOB.NE.0.AND..NOT.DUPLI) THEN
                     ND=ND+1
                     IW(KNEXT(ICEXHW)+1)=20000+IECOB
                     IW(ICEXHW+LMHROW)=IW(ICEXHW+LMHROW)+1
                  ENDIF
C
C We look for another relation with ECOB
C
                  INNNH=ITABL(ICRL3,JRELA,JCRLNH)
                  IF(INNNH.NE.0) THEN
                     JRELA=INNNH
                     GO TO 21
                  ENDIF
               ENDIF
            ENDIF
   10    CONTINUE
      CALL AUBOS('CEXT',NR,ND,LCEXT,IGARB)
      IF(IGARB.EQ.2) THEN
         CALL RERROR('CEXCOB',3,'INSUFF. SPACE FOR CEXT')
         GOTO 30
      ELSEIF(IGARB.EQ.1) THEN
         ICRL3=IW(NCRL3)
         ICCT1=IW(NCCT1)
         IHSPH=NLINK('HSPH',I)
      ENDIF
      CALL UCOPY(IW(ICEXHW+1),IW(LCEXT+1),ND)
   20 CONTINUE
      IF(DUMP) THEN
         DO 220 I=1,NSPN0
            NR=I
            IND=NPRNT('CEXT',NR)
  220    CONTINUE
      ENDIF
      CALL WDROP(IW,ICEXHW)
   30 RETURN
  230    CALL RERROR('CEXCOB',4,'INSUFF. SPACE FOR WBANKS')
      END
#endif
