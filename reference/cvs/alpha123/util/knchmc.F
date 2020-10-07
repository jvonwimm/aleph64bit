      INTEGER FUNCTION KNCHMC(ITK)
C***********************************************************************
C Find number of MC tracks that go with a charged EF object
C Use standard ALPHA routines, KNMTCH,KMTCH,KSMTCH
C Author: Paul Bright-Thomas
C Mods:   03/03/97 PBT Created.
C         06/03/97 PBT Faster for ITCH, take straight from ITEFFR and FKCHT
C***********************************************************************
#ifndef DOC
#include "qdecl.h"
#include "qcde.h"
#include "mcef.h"
      INTEGER ITK
      INTEGER II,JJ,KK,NUM,NMCUSE
      INTEGER ITCH,ITFR,ITEF,ITEFFR,ITCH2
      INTEGER IEFMC
      REAL    SHARE
#include "qmacro.h"
C***********************************************************************
      KNCHMC = 0
C-- Get the FRFT component of input EF object
      ITEFFR = KEFOLT(ITK)
      IF( ITEFFR.LE.0 ) GOTO 999
C-- Get ALPHA charged track which matches this FRFT number (only 1)
C      DO 100 ITCH = KFCHT,KLCHT
C        ITCH2 = KFCHT-1+KTN(ITCH)
C        IF( FCDEBU .AND. ITCH2.NE.ITCH ) WRITE(6,*)
C     +        ' KNCHMC W: itch .NE. kfcht-1+itfr !!!',ITCH,ITCH2
C        IF( KTN(ITCH).EQ.ITEFFR ) GOTO 110
C 100  END DO
      ITCH = KFCHT-1+ITEFFR
 110  IF( ITCH.LT.KFCHT .OR. ITCH.GT.KLCHT ) GOTO 999
C-- Get the number of MC matches to this charged track
      NUM    = KNMTCH(ITCH)
      IF( NUM.LE.0 ) GOTO 999
C-- Protection
      IF( NEFIN.GT.MXEF ) THEN
        CALL QWMESE(' KNCHMC - increase MXEF limit.')
        GOTO 999
      ENDIF
C-- Save all matches
      DO 200 II = 1,NUM
C-- Get MC number ....
        ITMC = KMTCH(ITCH,II)
C-- Check for repetition in mathcing to this input
        NMCUSE = 0
        DO 250 KK = 1,MIN(NEFMC(NEFIN),MXMTCH)
          IF( ITEFMC(NEFIN,KK).EQ.ITMC ) NMCUSE = NMCUSE + 1
 250    END DO
        IF( NMCUSE.GT.0 ) GOTO 200
C-- Protection on number of matches -- abort if we exceed array limit
        IF( NEFMC(NEFIN).GE.MXMTCH ) THEN
          CALL QWMESE(' KNCHMC - increase MXMTCH limit.')
          GOTO 999
        ENDIF
C-- Save this MC track as a match
        IF( ITMC.GE.KFMCT .AND. ITMC.LE.KLMCT ) THEN
          KNCHMC       = KNCHMC + 1
          NEFMC(NEFIN) = NEFMC(NEFIN) + 1
          ITEFMC(NEFIN,NEFMC(NEFIN)) = ITMC
          SHARE = -FLOAT(KSMTCH(ITCH,II))
          SEFMC(NEFIN,NEFMC(NEFIN))  = SHARE
        ENDIF
 200  END DO
C
 999  RETURN
      END
#endif
