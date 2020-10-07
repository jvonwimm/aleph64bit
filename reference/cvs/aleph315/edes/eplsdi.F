      INTEGER FUNCTION EPLSDI(PLAN,NPLANS,LISPL,POINT)
C.----------------------------------------------------------------------
CKEY ECALDES DICHOTOMIC SEARCH   /INTERNAL
C     H.Videau      Creation 15/12/85   Modification le 27/12/85
C! Dichotomic plane search
C  Makes a dichotomic search among planes of a pencil to find the
C  ones surrounding the point.
C  Effectue une recherche dichotomique parmi les plans d'un faisceau
C  pour determiner ceux qui encadrent le point.
C  A n'utiliser que pour une suite monotone!
C  To be used only for monotonic series!
C   Calls: none
C   Called internally
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C  Input variables
      INTEGER NPLANS
      REAL PLAN(ECALLS,ECALLP),LISPL(ECALLP,NPLANS),POINT(ECALLS-1)
C  Locales
      INTEGER I,IC,SEARCH,LO
      REAL CL1,CL2,TEMP
C
C Calcul des coordonnees locales
      CL1=PLAN(1,1)*POINT(1)+PLAN(2,1)*POINT(2)+
     &    PLAN(3,1)*POINT(3)+PLAN(4,1)
      CL2=PLAN(1,2)*POINT(1)+PLAN(2,2)*POINT(2)+
     &    PLAN(3,2)*POINT(3)+PLAN(4,2)
C Recherche des plans limitant
      IC=(NPLANS+1)/2
      SEARCH=IC
      LO=ALOG(FLOAT(NPLANS))/ALOG(2.)
      DO 2032 I=1 ,LO
         TEMP=-CL2*LISPL(1,IC)+CL1*LISPL(2,IC)
         SEARCH=(SEARCH+1)/2
         IC=IC+SEARCH
         IF(TEMP.GT.0.) IC=IC-2*SEARCH
 2032 CONTINUE
      TEMP=-CL2*LISPL(1,IC)+CL1*LISPL(2,IC)
      IF(TEMP.GT.0) IC=IC-1
      EPLSDI=IC
      END
#endif
