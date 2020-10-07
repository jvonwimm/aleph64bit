      SUBROUTINE CLUDIS(V1,IECOB,IHCL,DIST)
C***********************************************************************
C! Compute  ecob-hclu distance with respect to direction of track itr  *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870831                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : ECOB,EBOS,ESDA,HSDA,ESTO,HSTO,ETP1                     *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!   Itr     : track number                                            *
C!   IECOB   : Ecob number                                             *
C!   IHCL    : Hclu number                                             *
C!   DIST    : angular distance (radians)                              *
C!                                                                     *
C***********************************************************************
#ifndef DOC
#include "hcnamc.h"
#include "ecnamc.h"
#include "ecobjj.h"
#include "ebosjj.h"
#include "hclujj.h"
#include "estojj.h"
#include "hstojj.h"
#include "esdajj.h"
#include "hsdajj.h"
#include "bcs.h"
      DIMENSION XFACE(3,4),XFACH(3,4),V1(*)
#include "bmacro.h"
      KECOB=IW(NAECOB)
      KEBOS=IW(NAEBOS)
      KESDA=IW(NAESDA)
      KHCLU=IW(NAHCLU)
      KHSDA=IW(NAHSDA)
      KESTO=IW(NAESTO)
      KHSTO=IW(NAHSTO)
C
C?    Protect against missing banks.    R. Johnson  10-08-89
C
      IF (KECOB.EQ.0 .OR. KEBOS.EQ.0 .OR. KESDA.EQ.0 .OR. KHCLU.EQ.0
     +    .OR. KHSDA.EQ.0 .OR. KESTO.EQ.0 .OR. KHSTO.EQ.0) THEN
        DIST=3.14
        RETURN
      ENDIF
      DIST=-1000.
C
C?   see if ecob has energy at level 3
C
      EEC3=RTABL(KECOB,IECOB,JECOEN+2)
      IF(EEC3.GT.0.)THEN
C
C?   see if cluster Hcal has energy at level 1
C
         EHC1=RTABL(KHCLU,IHCL,JHCLE1)
         IF(EHC1.GT.0.)THEN
            DIST=1000.
C
C?    get number of ecal and hcal storeys
C
            NESTO=1
            IEBOF=ITABL(KECOB,IECOB,JECOEB)
            IEBO=IEBOF
   10       IEBO=ITABL(KEBOS,IEBO,JEBOEB)
            IF(IEBO.NE.0)THEN
               NESTO=NESTO+1
               GO TO 10
            ENDIF
            NHSTO=1
            ISTFH=ITABL(KHCLU,IHCL,JHCLFS)
            ISTH=ISTFH
   11       ISTH=ITABL(KHSDA,ISTH,JHSDNS)
            IF(ISTH.NE.0)THEN
               NHSTO=NHSTO+1
               GO TO 11
            ENDIF
C            WRITE(6,*)' NESTO,NHSTO :',NESTO,NHSTO
C
C?   loop on storeys
C
            IEBO=IEBOF
            DO 100 IE=1,NESTO
            ISTE=ITABL(KEBOS,IEBO,JEBOES)
            LESTO=KROW(KESTO,ISTE)
C
C?  skip storeys which are not at level 3 ecal
C
            KSTE=ITABL(KESDA,ISTE,JESDDK)
            IF(KSTE.NE.3)GO TO 100
            DO 111 IC=1,4
            XFACE(1,IC)=RW(LESTO+JESTXO+IC-1)
            XFACE(2,IC)=RW(LESTO+JESTYO+IC-1)
            XFACE(3,IC)=RW(LESTO+JESTZO+IC-1)
  111       CONTINUE
            ISTH=ISTFH
            DO 200 IH=1,NHSTO
            LLSTO=KROW(KHSTO,ISTH)
C
C?  skip storeys which are not at level 1 hcal
C
            KSTH=ITABL(KHSDA,ISTH,JHSDSN)
            IF(KSTH.NE.1)GO TO 200
            DO 110 IC=1,4
            XFACH(1,IC)=RW(LLSTO+JHSTXI+IC-1)
            XFACH(2,IC)=RW(LLSTO+JHSTYI+IC-1)
            XFACH(3,IC)=RW(LLSTO+JHSTZI+IC-1)
  110       CONTINUE
            ISUBH=ITABL(KHSDA,ISTH,JHSDSC)
            IFENC=0
            IF(ISUBH.NE.2)IFENC=1
C
C?   Compute angular distance
C
            CALL CDANG(XFACE,XFACH,V1,ANGL,IFENC)
            IF(ANGL.LT.DIST)THEN
               DIST=ANGL
C               WRITE(6,*)'ISTE,ISTH,DIST :',ISTE,ISTH,DIST
               IF(DIST.EQ.0.)GO TO 999
            ENDIF
  200       ISTH=ITABL(KHSDA,ISTH,JHSDNS)
  100       IEBO=ITABL(KEBOS,IEBO,JEBOEB)
         ENDIF
      ENDIF
  999 CONTINUE
      RETURN
      END
#endif
