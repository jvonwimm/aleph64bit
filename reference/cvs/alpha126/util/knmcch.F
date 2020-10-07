      INTEGER FUNCTION KNMCCH(ITK)
C***********************************************************************
C Find number of charged EF objects that go with input MC track.
C Use standard ALPHA routines, KNMTCH,KMTCH,KSMTCH
C Author: Paul Bright-Thomas
C Mods: 03/03/97 PBT Created.
C       06/03/97 PBT Tidied, to match KNCHMC.
C***********************************************************************
#ifndef DOC
#include "qdecl.h"
#include "qcde.h"
#include "mcef.h"
      INTEGER ITK
      INTEGER II,JJ,KK,NUM,NEFUSE
      INTEGER ITCH,ITFR,ITEF,ITEFFR
      INTEGER IMCEF
      REAL    SHARE
#include "qmacro.h"
C***********************************************************************
      KNMCCH = 0
      NUM    = KNMTCH(ITK)
      IF( NUM.LE.0 ) RETURN
C-- Protection
      IF( NMCIN.GT.MXMC ) THEN
        CALL QWMESE(' KNMCCH - increase MXMC limit.')
        GOTO 999
      ENDIF
C-- Save all matches
      DO 200 II = 1,NUM
C-- Protection on number of matches -- abort if we exceed array limit
        IF( II.GT.MXMTCH ) THEN
          CALL QWMESE(' KNMCCH - increase MXMTCH limit.')
          GOTO 999
        ENDIF
C-- Get ALPHA CHT number ....
        ITCH = KMTCH(ITK,II)
        ITFR = KTN(ITCH)
C-- Find the EFLW object matching this track.
        ITEF   = KPDIR('EFLW',KRECO)
 220    IF( ITEF.LE.0 ) GOTO 200
C-- Get FRFT number of EFLW object ...
        ITEFFR = KEFOLT(ITEF)
        IF( ITEFFR.LE.0 .OR. ITEFFR.NE.ITFR ) GOTO 230
C-- Check for repetition in matching to this input
        NEFUSE = 0
        DO 250 KK = 1,MIN(NMCEF(NMCIN),MXMTCH)
          IF( ITMCEF(NMCIN,KK).EQ.ITEF ) NEFUSE = NEFUSE + 1
 250    END DO
        IF( NEFUSE.GT.0 ) GOTO 230
C-- Protection on number of matches -- abort if we exceed array limit
        IF( NMCEF(NMCIN).GE.MXMTCH ) THEN
          CALL QWMESE(' KNMCCH - increase MXMTCH limit.')
          GOTO 999
        ENDIF
C-- Save this EFLW object as a match
        KNMCCH = KNMCCH + 1
        NMCEF(NMCIN)     = NMCEF(NMCIN) + 1
        ITMCEF(NMCIN,NMCEF(NMCIN)) = ITEF
        SHARE = -FLOAT(KSMTCH(ITK,II))
        SMCEF(NMCIN,NMCEF(NMCIN))  = SHARE
C-- Next EFLW object.
 230    ITEF   = KFOLLO(ITEF)
        GOTO 220
C
 200  END DO
C
 999  RETURN
      END
#endif
