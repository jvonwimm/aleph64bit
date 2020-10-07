      SUBROUTINE EPRSTO (IER)
C***********************************************************************
C!Create bank esto                                                     *
C!                                                                     *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870420                                    *
C!                                                                     *
C!  IER = RETURN CODE , 0=OK                                           *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : ESDA                                                   *
C!    OUTPUT  : ESTO                                                   *
C!    CREATED : ESTO                                                   *
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "ecnamc.h"
#include "esdajj.h"
#include "estojj.h"
      DIMENSION CORNR(3,10),XCF(3,2)
#include "bmacro.h"
      KESDA=IW(NAESDA)
C
C?  get number of storeys
C
      NSDA=LROWS(KESDA)
C
C?    create bank esto
C
      NDATA=NSDA*LESTOA+LMHLEN
      CALL AUBOS('ESTO',0,NDATA,KESTO,IGARB)
      IF(IGARB.EQ.2)GO TO 998
C
C?   Put bank in list T
C
      CALL BLIST(IW,'T+','ESTO')
C
C?   fill miniheader
C
      IW(KESTO+LMHCOL)=LESTOA
      IW(KESTO+LMHROW)=NSDA
C
      KESDA=IW(NAESDA)
C
C?   loop on storeys
C
      DO 10 I=1,NSDA
      LESTO=KROW(KESTO,I)
      LESDA=KROW(KESDA,I)
C
C?  Skip storeys which are not in a cluster or which are not at level 3
C
      ICCLU=ITABL(KESDA,I,JESDEC)
      IF(ICCLU.EQ.0)GO TO 10
      KSTAC=ITABL(KESDA,I,JESDDK)
      IF(KSTAC.NE.3)GO TO 10
C
C?   get module number (1:36) from esda bank
C
      IMD32=IW(LESDA+JESDER)
C
C?   make subcomponent number
C
      ISCM=(IMD32-1)/12
      IW(LESTO+JESTSN)=ISCM+1
C
C?   make module number
C
      IMOD=IMD32-ISCM*12
      IW(LESTO+JESTMN)=IMOD
C*****************************************
C
C?   Here, the overlap flag will have to be filled
C
C*****************************************
C
C?  get i,j,k
C
      IPH=IW(LESDA+JESDFI)
      JTH=IW(LESDA+JESDTJ)
      K=IW(LESDA+JESDDK)
C
C?   Make storeys corners
C
      CALL ESRCR('ALEPH',JTH,IPH,K,LP,CORNR)
      DO 130 IC=1,4
C
C?  fill inner face
C
      LEXCO=LESTO+IC-1+JESTXI
      LEYCO=LESTO+IC-1+JESTYI
      LEZCO=LESTO+IC-1+JESTZI
      RW(LEXCO)=CORNR(1,IC)
      RW(LEYCO)=CORNR(2,IC)
      RW(LEZCO)=CORNR(3,IC)
C
C?  fill outer face
C
      LEXCO=LESTO+IC-1+JESTXO
      LEYCO=LESTO+IC-1+JESTYO
      LEZCO=LESTO+IC-1+JESTZO
      RW(LEXCO)=CORNR(1,IC+4)
      RW(LEYCO)=CORNR(2,IC+4)
      RW(LEZCO)=CORNR(3,IC+4)
  130 CONTINUE
C
C?  compute faces center
C
      DO 131 IX=1,3
      DO 131 IFC=1,2
  131 XCF(IX,IFC)=0.
      DO 140 IFC=1,2
      DO 140 IC=1,4
      DO 140 IX=1,3
  140 XCF(IX,IFC)=XCF(IX,IFC)+CORNR(IX,IC+(IFC-1)*4)
C
C?   fill faces center
C
      RW(LESTO+JESTIX)=XCF(1,1)/4.
      RW(LESTO+JESTOX)=XCF(1,2)/4.
      RW(LESTO+JESTIY)=XCF(2,1)/4.
      RW(LESTO+JESTOY)=XCF(2,2)/4.
      RW(LESTO+JESTIZ)=XCF(3,1)/4.
      RW(LESTO+JESTOZ)=XCF(3,2)/4.
C
C?   make barycenter coordinates
C
      CALL ESRBC('ALEPH',JTH,IPH,K,CORNR)
C
C?    fill esto bank with barycenter corrds.
C
      RW(LESTO+JESTSX)=CORNR(1,1)
      RW(LESTO+JESTSY)=CORNR(2,1)
      RW(LESTO+JESTSZ)=CORNR(3,1)
   10 CONTINUE
      IER=0
      RETURN
  998 CONTINUE
      CALL RERROR('EPRSTO',1,' Too little space for bank ESTO')
      INDEX=NDROP('ESTO',0)
      IER=1
      RETURN
      END
#endif
