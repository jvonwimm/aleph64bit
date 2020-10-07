*DK asieve
      SUBROUTINE ASIEVE
C ----------------------------------------------------------------------
C. - F.RANJARD - 850328
C! event initialization
C. - Increment event # .
C. - Reset all flags, variables and banks for this event
C. - Reset random generator if required.
C. - Set DEBUG flag.
C. - Initialize ZBOOK event partition .
C
C. - modified by : F.Ranjard - 911002
C                  supress reference to LMOD which is replaced by LDET
C                  in the definition of FBEGJO
C
C. - called from    ASPRUN                             from this .HLB
C. - calls          RDMIN, RDMOUT, TIMED, TIMEL        from KERNLIB
C.                  MZBOOK                             from ZEBRA lib
C.                  ASREVE                             from this .HLB
C.                  ABRUEV                             from ALEPHLIB
C -----------------------------------------------------------------------
#ifndef DOC
#include "gcbank.h"
#include "gcflag.h"
#include "gcnum.h"
#include "jqcom.h"
#include "jobcom.h"
#include "iocom.h"
      DATA IFI/0/ , NANEVT, NASEVT/2*0/
C ----------------------------------------------------------------------
      IF (NANEVT.EQ.0) NANEVT = NAMIND('NEVT')
C
C - get one record or initialize BOS
C
    1 IF (MGETJO .GT.0) THEN
   10   CALL ABRSEL ('E','    ',IRET)
        IF (IRET.EQ.1) GOTO 100
        IF (IRET.GT.3) THEN
          IF (IRET.EQ.9) THEN
            CALL ALTELL ('ASIEVE: Time limit reached',0,'END')
          ELSEIF (IRET.EQ.8) THEN
            CALL ALTELL ('ASIEVE:  End of selected events',0,'END')
          ELSEIF (IRET.EQ.7) THEN
            CALL ALTELL ('ASIEVE:  Last event reached',0,'END')
          ELSEIF (IRET.EQ.6) THEN
            CALL ALTELL ('ASIEVE:  No more input file',0,'END')
          ELSEIF (IRET.LE.16) THEN
            CALL ALTELL ('ASIEVE:  Cannot open input or output file'
     &                              ,0,'END')
          ELSEIF (IRET.EQ.19) THEN
            CALL ALTELL ('ASIEVE:  Not enough space for unpacking',0
     &                              ,'RETURN')
          ELSEIF (IRET.EQ.17) THEN
            CALL ALTELL ('ASIEVE:  Read error-try again',0,'RETURN')
          ELSEIF (IRET.EQ.18) THEN
            CALL ALTELL ('ASIEVE: Error in decompressing-next',0
     &                              ,'RETURN')
          ENDIF
          GOTO 10
        ENDIF
C
        IF (IRET.EQ.3) THEN
C        unknown record
          IF (IPRIJO(15).EQ.1) CALL AUBLIS ('E')
          IF (MSAVJO.GT.0) CALL ASWRTP('E')
C
        ELSEIF (IRET.EQ.2) THEN
          IF (IPRIJO(15).EQ.1) CALL AUBLIS ('C')
C        run record : header or end
          IF (IW(NARUNE).NE.0) THEN
            CALL ASCRUN
          ELSEIF (IW(NARUNH).NE.0) THEN
            CALL ASIRUN
          ENDIF
        ENDIF
        GOTO 10
      ELSE
C      no input file
        CALL BDROP (IW,'T')
        CALL BDROP (IW,'E')
        CALL BLIST (IW,'T=','0')
        CALL BLIST (IW,'E=','EVEH')
        CALL BGARB (IW)
        IF (NEVTJO.GE.IW(IW(NANEVT)+1)) THEN
           CALL ALTELL ('ASIEVE: last event reached ',0,'END')
        ENDIF
        CALL TIMEL(TIMEJO(2))
        IF (TIMEJO(2).LT.TIMLJO) THEN
           CALL ALTELL ('ASIEVE: time limit ',0,'END')
        ENDIF
      ENDIF
C
C - Initialize event : reset flags and variables for this event
C   get input event if any.
C   set random generator if required, reset banks.
C
 100  CONTINUE
      IF (NASEVT.EQ.0) NASEVT = NAMIND('SEVT')
      NEVTJO = NEVTJO + 1
C
C - get trigger # from input file if any or from serial number
      IF (MGETJO.GT.0) THEN
         CALL ABRUEV (IRUNJO,ITRIG)
      ELSE
         ITRIG = NEVTJO
      ENDIF
C
C - if not 1st entry reset ZEBRA partition
      IF (IFI .NE. 0 ) CALL GTRIGC
C
C - reset GEANT3 banks and counters
      CALL MZBOOK (IGXDIV,JGHEAD,JGHEAD,1,'HEAD',1,1,NGHEAD,2,0)
      NGTRAC=0
      NGVERT=0
      IGEOTR=0
      IGDEBU=0
      IGDEVT = NEVTJO
C
C - reset counters and flags
      KERRJO = 0
      FDEBJO=.FALSE.
      DO 3 I =1,LDET
 3    FBEGJO(I) = .TRUE.
C
C - this event has to be processed:  set debug flag
      IF(ITRIG.GE.IDB1JO.AND.ITRIG.LE.IDB2JO) FDEBJO=.TRUE.

C                                    set display flag
#if defined(INTER)
      FDISJO = .TRUE.
#else
      FDISJO=.FALSE.
      IF(ITRIG.GE.IDS1JO.AND.ITRIG.LE.IDS2JO) FDISJO=.TRUE.
#endif
C                                    set random generator and time
      IF(IFI.EQ.0) THEN
         IFI=1
         DO 4 I=1,LPRO
            IF (IRNDJO(1,I) .EQ. 0) GOTO 4
               CALL RDMIN (IRNDJO(1,I))
               GOTO 5
 4       CONTINUE
 5       CONTINUE
         CALL RDMOUT (NRNDJO(1))
         CALL TIMED (TIMEJO(5))
      ENDIF
C                                    output random generator root
      IF (FDEBJO .AND. MGETJO.EQ.0) THEN
        WRITE (LOUTIO,802) NEVTJO,IRUNJO,ITRIG,NRNDJO
      ELSEIF (IDB3JO.GT.0) THEN
        IF (MOD(NEVTJO,IDB3JO).EQ.0) THEN
          WRITE (LOUTIO,802) NEVTJO,IRUNJO,ITRIG,NRNDJO
  802     FORMAT (/3X,'+++ASIEVE+++ EVENT# ',I5,'  (run ',I5,
     &                  ' trigger ',I5,')  random number =',3I12,
     &                  ' ++++++++')
        ENDIF
      ENDIF
C
C - print E-list and decode EVEH bank if any
      IF (MGETJO.GT.0) THEN
         IF (FDEBJO .AND. IPRIJO(15).EQ.1) CALL AUBLIS ('E')
         CALL ASREVE
      ENDIF
C
C - Call USER routine
      CALL USIEVE
C
      RETURN
      END
#endif
