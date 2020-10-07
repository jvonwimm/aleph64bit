      SUBROUTINE E4GTSO(ISTO,RESTIX,RESTIY,RESTIZ)
C----------------------------------------------------------------------
C!  - ELECTRON IDENTIFICATION : GET STOREYS GEOMETRICAL PARAMETERS
C!
C!    AUTHOR  : D. PALLIN
C!    MODIFIED:
C!
C!      ISTO :ROW NUMBER OF THE STOREY IN BK ESDA
C!      REXCO REYCO REZCO X YZ CORNER VALUES
C!      RESTIX RESTIY RESTIZ XYZ FACES CENTER
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "ecnamc.h"
#include "esdajj.h"
      DIMENSION CORNR(3,10),XCF(3),REXCO(10),REYCO(10),REZCO(10)
#include "bmacro.h"
      KESDA=IW(NAESDA)
      LESDA=KESDA+2+LESDAA*(ISTO-1)
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
      DO 130 IC=1,LP
C
C?  fill inner face
C
      REXCO(IC)=CORNR(1,IC)
      REYCO(IC)=CORNR(2,IC)
      REZCO(IC)=CORNR(3,IC)
 130  CONTINUE
C
C?  compute faces center
C
      DO 131 IX=1,3
  131 XCF(IX)=0.
      DO 140 IC=1,LP
      DO 140 IX=1,3
  140 XCF(IX)=XCF(IX)+CORNR(IX,IC)
C
C?   fill faces center
C
      RESTIX=XCF(1)/LP
      RESTIY=XCF(2)/LP
      RESTIZ=XCF(3)/LP
C
      RETURN
      END
#endif
