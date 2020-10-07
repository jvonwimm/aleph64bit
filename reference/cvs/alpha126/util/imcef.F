      INTEGER FUNCTION IMCEF(ITK)
C***********************************************************************
C Search array of previous inputs to see if we already know this object.
C Author: Paul Bright-Thomas
C Mods:     03/03/97 PBT Created.
C***********************************************************************
#ifndef DOC
#include "qdecl.h"
#include "qcde.h"
#include "mcef.h"
      INTEGER II,JJ,KK,ITK
#include "qmacro.h"
C***********************************************************************
      IMCEF = 0
      DO 100 II = 1,MIN(NMCIN,MXMC)
        IF( ITMCIN(II).EQ.ITK ) THEN
          IMCEF = II
          RETURN
        ENDIF
 100  END DO
C
 999  RETURN
      END
#endif
