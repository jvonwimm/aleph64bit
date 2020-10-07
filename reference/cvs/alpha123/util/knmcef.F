      INTEGER FUNCTION KNMCEF(ITK)
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
      INTEGER KNMCCH,KNMCEC,KNMCHC,KNMCSI
      INTEGER IMCEF
#include "qmacro.h"
C***********************************************************************
      KNMCEF = 0
C-- Check for valid MC track number
      IF( ITK.LT.KFMCT .OR. ITK.GT.KLMCT ) THEN
        RETURN
C-- We can only match to stable particles
      ELSE IF( KSTABC(ITMC).LT.0 ) THEN
        RETURN
C-- See if we already know this object ...
      ELSE
        INDX = IMCEF(ITK)
C-- ... if we do, look up the desired info.
        IF( INDX.GT.0 .AND. INDX.LE.MXMC ) THEN
          KNMCEF = NMCEF(INDX)
          RETURN
C-- ... if we don't, count its matches.
        ELSE
          IF( NMCIN.GE.MXMC ) THEN
            CALL QWMESE(' KNMCEF - exceeded max. MC tracks - problems.')
            RETURN
          ENDIF
          NMCIN  = NMCIN  + 1
          ITMCIN(NMCIN) = ITK
          KNMCEF = KNMCEF + KNMCCH(ITK) ! Count charged matches
          KNMCEF = KNMCEF + KNMCEC(ITK) ! Count ECAL/LCAL matches
          KNMCEF = KNMCEF + KNMCHC(ITK) ! Count HCAL matches
          KNMCEF = KNMCEF + KNMCSI(ITK) ! Count SICAL matches
        ENDIF
      ENDIF
C
 999  RETURN
      END
#endif
