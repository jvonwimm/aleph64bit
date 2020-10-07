      SUBROUTINE QUREFIT
CKEY INIT /USER
C-----------------------------------------------------------------------
C! user refit of tracks
C called from QMEVNT before QFILL
C-----------------------------------------------------------------------
#ifndef DOC
      LOGICAL REFIT, CORRVD, UNITC, UNANY
      CHARACTER*16 PRGBK
      CHARACTER*80 LSTUN
      LOGICAL FIRST
      INTEGER NAREFT, NAUNPK
      SAVE FIRST, NAREFT
      INTEGER VDYEAR
      DATA FIRST /.TRUE./
#include "reftjj.h"
#include "rhahjj.h"
#include "qcde.h"
#include "qmacro.h"
C------------------------------------------------------------------------
      IF(FIRST)THEN
        FIRST = .FALSE.
        NARHAH = NAMIND('RHAH')
        NAREFT = NAMIND('REFT')
        NAUNPK = NAMIND('UNPK')
        KREFT = IW(NAREFT)
        IF(KREFT .NE. 0)THEN
          NDATA = IW(KREFT)
          IF(NDATA .LT. LREFTA)THEN
            CALL QMTERM(' qurefit --> Wrong REFT data card, exiting')
          ENDIF
        ENDIF
      ENDIF
C
C Defaults
C Refit if :
C   - Real data
C   - Lep2 (VDYEAR = 95)
C 
      IF(IW(NAMIND('AJOB')).EQ.0
     >     .AND. VDYEAR().EQ.95)THEN
        REFIT = .TRUE.
        CORRVD = .TRUE.
      ELSE
        REFIT = .FALSE.
        CORRVD = .FALSE.
      ENDIF
C
C Look at REFT bank
C
      KREFT = IW(NAREFT)
      IF(KREFT .NE. 0)THEN
        JVD = IW(KREFT+JREFVC)
        IRFT = IW(KREFT+JREFRF)
        IF(IRFT .EQ. 0)THEN
           REFIT = .FALSE.
        ELSE
           REFIT = .TRUE.
           IF(JVD .NE. 0)CORRVD = .TRUE.
        ENDIF
      ENDIF
      IF (REFIT) THEN
C
C One can only refit POTs or DSTs:
C Make sure that we are not reading a MINI
C this test must be skipped if one is writing a MINI
C
       if (.not.xwmini) then
         KRHAH = IW(NARHAH)
         NRHAH = LROWS(KRHAH)
         DO IRHAH=1,NRHAH
           CALL ALSTIN(IW(KROW(KRHAH,IRHAH)+JRHAPN),2,PRGBK)       
           IF(PRGBK(1:5) .EQ. 'MINI')REFIT = .FALSE.
         ENDDO
       endif
      ENDIF
      IF (REFIT) THEN
C 
C Correct Vdet coordinates if requested
C 
        IF(CORRVD)CALL VDFIXZ          
C
        CALL VTRKEX(QMFLD,IERR)
C Refit tracks
        CALL QFREFIT
      ENDIF
      RETURN
      END
#endif
