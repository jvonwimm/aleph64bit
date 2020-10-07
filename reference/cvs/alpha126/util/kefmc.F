      INTEGER FUNCTION KEFMC(ITK,I)
C***********************************************************************
C Find Ith alpha EF objects matching input MC track or vice versa.
C Author: Paul Bright-Thomas
C Mods:     03/03/97 PBT Created.
C***********************************************************************
#ifndef DOC
#include "qdecl.h"
#include "qcde.h"
#include "mcef.h"
      INTEGER I,II,JJ,KK,ITK,IT
      INTEGER IEFMC
#include "qmacro.h"
C***********************************************************************
      KEFMC = 0
C-- Check for valid MC track number
      IF( ITK.LT.KFEFT .OR. ITK.GT.KLEFT ) THEN
        RETURN
C-- Retrieve the MC match info
      ELSE
C-- Do we know this object ?
        JT    = IEFMC(ITK)
C-- If so, retrieve its matches.
        IF( JT.GT.MXEF ) GOTO 999
        IF( JT.GT.0 .AND. JT.LE.MXEF ) GOTO 500
C-- If unknown, start over.
        NUM   = KNEFMC(ITK)
C-- If there are no matches, say so
        IF( NUM.LE.0 ) THEN
          KEFMC = 0
          RETURN
        ENDIF
      ENDIF
C-- The test ...
 500  JT    = IEFMC(ITK)
      IF( JT.GT.0 .AND. JT.LE.MXEF .AND.
     +    I.LE.MIN(NEFMC(JT),MXMTCH) ) THEN
        KEFMC = ITEFMC(JT,I)
      ELSE
        KEFMC = 0
      ENDIF
C
 999  RETURN
      END
#endif
