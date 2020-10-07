C!======================================================================
      SUBROUTINE BOOST(ZM,E,PX,PY,PZ,XP,YP,ZP,EP,PP)
C!======================================================================
#ifndef DOC
#include "qcde.h"
      REAL PP(4),E,PX,PY,PZ,XP,YP,ZP,EP,ZM
      REAL*8 BETA,BETAX,BETAY,BETAZ,P,GAMMA
#include "qmacro.h"
C!======================================================================
C!.ON BOOST LA TRACE D'IMPULSIONS ( XP YP ZP EP ) DANS LE CENTRE DE
C!.MASSE DU SYSTEME D'IMPULSIONS (PX PY PZ E ) ET DE MASSE ZM .
C**
        IF(E.EQ.0..OR.ZM.EQ.0.) THEN
           PP(1)=XP
           PP(2)=YP
           PP(3)=ZP
           PP(4)=ZP
           RETURN
        ENDIF
        P=SQRT(PX**2+PY**2+PZ**2)
        BETA=P/E
        BETAX=PX/E
        BETAY=PY/E
        BETAZ=PZ/E
        GAMMA=E/ZM
      PP(1)=XP+(GAMMA-1.)*(XP*BETAX+YP*BETAY+ZP*BETAZ)*BETAX/(BETA**2)
     >       -GAMMA*BETAX*EP
      PP(2)=YP+(GAMMA-1.)*(XP*BETAX+YP*BETAY+ZP*BETAZ)*BETAY/(BETA**2)
     >       -GAMMA*BETAY*EP
      PP(3)=ZP+(GAMMA-1.)*(XP*BETAX+YP*BETAY+ZP*BETAZ)*BETAZ/(BETA**2)
     >       -GAMMA*BETAZ*EP
      PP(4)=SQRT(PP(1)**2+PP(2)**2+PP(3)**2)
        RETURN
        END
*AF
#endif
