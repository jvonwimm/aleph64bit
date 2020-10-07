      SUBROUTINE ELIMSC (ZA,ZB)
C.-----------------------------------------------------------------
CKEY ECALDES SUBCOMPONENT PLANE LIMIT / USER
C  M.Rumpf                                   September 87
C! Get Subcomp. Z limiting planes
C   Input :
C           none
C   Output:
C           ZA,ZB
C    All points in Barrel have a Z coord : ZB < Z < ZA
C    All points in End cap A have a Z coord  : Z > ZA > 0
C    All points in End cap B have a Z coord  : Z < ZB < 0
C  Can be used to compute quickly Sub Comp # knowing z coord of a
C  point IN ALEPH general system.
C   Called by USER
C.-----------------------------------------------------------------
#ifndef DOC
      SAVE
#include "ecccom.h"
C
      ZA = ESCOLI(4,3)
      ZB = ESCOLI(4,2)
      END
#endif
