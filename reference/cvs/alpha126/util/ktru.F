      INTEGER FUNCTION KTRU(ITK,I)
C***********************************************************************
C Find Ith alpha EF object matching input MC track or vice versa.
C Author: Paul Bright-Thomas
C Mods:     03/03/97 PBT Created.
C***********************************************************************
#ifndef DOC
#include "qdecl.h"
#include "qcde.h"
#include "mcef.h"
      INTEGER II,JJ,KK,ITK
      INTEGER KEFMC,KMCEF
#include "qmacro.h"
C***********************************************************************
      IF( ITK.GE.KFEFT .AND. ITK.LE.KLEFT ) THEN
        KTRU = KEFMC( ITK, I )
      ELSE IF( ITK.GE.KFMCT .AND. ITK.LE.KLMCT ) THEN
        KTRU = KMCEF( ITK, I )
      ELSE
        KTRU = 0
      ENDIF
C
 999  RETURN
      END
#endif
