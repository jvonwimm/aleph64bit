      INTEGER FUNCTION KNTRU(ITK)
C***********************************************************************
C Find number of alpha EF objects matching input MC track or vice versa.
C Author: Paul Bright-Thomas
C Mods:     03/03/97 PBT Created.
C***********************************************************************
#ifndef DOC
#include "qdecl.h"
#include "qcde.h"
#include "mcef.h"
      INTEGER II,JJ,KK,ITK
      INTEGER KNEFMC,KNMCEF
#include "qmacro.h"
C***********************************************************************
      IF( KNEVT.NE.LEVT ) THEN
        CALL QTRUZ
        LEVT = KNEVT
      ENDIF
C
      IF( ITK.GE.KFEFT .AND. ITK.LE.KLEFT ) THEN
        KNTRU = KNEFMC( ITK )
      ELSE IF( ITK.GE.KFMCT .AND. ITK.LE.KLMCT ) THEN
        KNTRU = KNMCEF( ITK )
      ELSE
        KNTRU = 0
      ENDIF
C
 999  RETURN
      END
#endif
