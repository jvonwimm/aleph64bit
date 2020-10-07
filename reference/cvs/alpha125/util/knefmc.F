      INTEGER FUNCTION KNEFMC(ITK)
C***********************************************************************
C Find number of alpha EF objects matching input MC track or vice versa.
C Author: Paul Bright-Thomas
C The results from counting are kept for quick access when the matches are
C required.
C Mods:   03/03/97 PBT Created.
C***********************************************************************
#ifndef DOC
#include "qdecl.h"
#include "qcde.h"
#include "mcef.h"
      INTEGER II,JJ,KK,ITK
      INTEGER KNCHMC,KNECMC,KNHCMC,KNSIMC
      INTEGER IEFMC
#include "qmacro.h"
C***********************************************************************
      KNEFMC = 0
C-- Check for valid MC track number
      IF( ITK.LT.KFEFT .OR. ITK.GT.KLEFT ) THEN
        RETURN
C-- See if we already know this object ...
      ELSE
        INDX = IEFMC(ITK)
C-- ... if we do, look up the desired info.
        IF( INDX.GT.0 .AND. INDX.LE.MXEF ) THEN
          KNEFMC = NEFMC(INDX)
          RETURN
C-- ... if we don't, count its matches.
        ELSE
          IF( NEFIN.GE.MXEF ) THEN
            CALL QWMESE(' KNEFMC - exceeded max. EF tracks - problems.')
            RETURN
          ENDIF
          NEFIN  = NEFIN  + 1
          ITEFIN(NEFIN) = ITK
          KNEFMC = KNEFMC + KNCHMC(ITK) ! Count charged matches
          KNEFMC = KNEFMC + KNECMC(ITK) ! Count ECAL/LCAL matches
          KNEFMC = KNEFMC + KNHCMC(ITK) ! Count HCAL matches
          KNEFMC = KNEFMC + KNSIMC(ITK) ! Count SICAL matches
        ENDIF
      ENDIF
C
 999  RETURN
      END
#endif
