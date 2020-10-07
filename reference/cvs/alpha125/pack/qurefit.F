      SUBROUTINE QUREFIT
CKEY INIT /USER
C-----------------------------------------------------------------------
C! user refit of tracks
C called from QMEVNT before QFILL
C-----------------------------------------------------------------------
#ifndef DOC
      LOGICAL REFIT, CORRVD, UNITC, UNANY
      INTEGER JVERS
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
C
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
C   - Jul > 312
C   - Real data
C   - Lep2 (VDYEAR = 95)
C
      JVERS = IQVERP('JULIA') 
      IF(JVERS.GT.312 .AND. IW(NAMIND('AJOB')).EQ.0
     >                  .AND. VDYEAR().EQ.95)THEN
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
      IF(REFIT)THEN
C
C One can only refit POts, make sure that we are not on a MINI
C
        KRHAH = IW(NARHAH)
        NRHAH = LROWS(KRHAH)
        DO IRHAH=1,NRHAH
          CALL ALSTIN(IW(KROW(KRHAH,IRHAH)+JRHAPN),2,PRGBK)       
          IF(PRGBK(1:5) .EQ. 'MINI')REFIT = .FALSE.
        ENDDO
      ENDIF
      IF(REFIT)THEN
C
C Look if there was an UNPK data card which requests for ITC,
C in which case we do not want to do it twice (program bombs if we do !0
C
        UNITC = .TRUE.
        UNANY = .TRUE.
        NDATA = IW(KUNPK)
        IF(INDEX(CQUNPK,'IT') .NE. 0)UNITC = .FALSE.          
        IF(INDEX(CQUNPK,'AL') .NE. 0)UNANY = .FALSE.          
C
C Unpack necessary banks
C
        IER = 0
        IF(UNANY)THEN
          IF(UNITC)THEN
            CALL AUNPCK('E','VD IT TP FI SO', IER)
          ELSE
            CALL AUNPCK('E','VD TP FI SO', IER)
          ENDIF
        ENDIF
        IF(IER .LE. 1)THEN
C 
C Correct Vdet coordinates if requested
C 
          IF(CORRVD)CALL VDFIXZ          
C
          CALL VTRKEX(QMFLD,IERR)
C Refit tracks
          CALL QFREFIT
        ENDIF
      ENDIF
      RETURN
      END
#endif









