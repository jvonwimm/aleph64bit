      INTEGER FUNCTION KMCEF(ITK,I)
C***********************************************************************
C Find Ith alpha EF objects matching input MC track or vice versa.
C Author: Paul Bright-Thomas
C Mods:   03/03/97 PBT Created.
C***********************************************************************
#ifndef DOC
#include "qdecl.h"
#include "qcde.h"
#include "mcef.h"
      INTEGER I,II,JJ,KK,ITK,IT
      INTEGER IMCEF
#include "qmacro.h"
C***********************************************************************
      KMCEF = 0
C-- Check for valid MC track number
      IF( ITK.LT.KFMCT .OR. ITK.GT.KLMCT ) THEN
        RETURN
C-- Retrieve the MC match info
      ELSE
C-- Do we know this object ?
        JT    = IMCEF(ITK)
C-- If so, retrieve its matches.
        IF( JT.GT.MXMC ) GOTO 999
        IF( JT.GT.0 .AND. JT.LE.MXMC ) GOTO 500
C-- If unknown, start over.
        NUM   = KNMCEF(ITK)
C-- If there are no matches, say so
        IF( NUM.LE.0 ) THEN
          KMCEF = 0
          RETURN
        ENDIF
      ENDIF
C-- The test ...
 500  JT    = IMCEF(ITK)
      IF( JT.GT.0 .AND. JT.LE.MXMC .AND.
     +    I.LE.MIN(NMCEF(JT),MXMTCH) ) THEN
        KMCEF = ITMCEF(JT,I)
      ELSE
        KMCEF = 0
      ENDIF
C
 999  RETURN
      END
#endif
