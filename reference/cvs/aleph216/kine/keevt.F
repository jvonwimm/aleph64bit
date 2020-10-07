      SUBROUTINE KEEVT (IEVT,ISTAT,NVX,NTRK,WEIT,IDPRO,ECMS,LWRT)
C -----------------------------------------------------------------
C - F.Ranjard - 870505
C! KINGAL end of event
CKEY KINE KINGAL FILL BANK /  INTERNAL
C   if it is a good event fill the EVEH and KEVH banks and write
C   the event on a file if required . Then reset the BOS array (drop
C   the event list and garbage collect)
C
C - structure: SUBROUTINE program
C              User Entry Name: KEEVT
C              External references: ALEVEH/ALKEVH(ALEPHLIB)
C                                   NAMIND/BWRITE/BLIST/BDROP/BGARB(BOS77
C                                   RDMOUT(CERNLIB)
C              Comdecks referenced: BCS, BMACRO
C
C - usage   : CALL KEEVT (IEVT,ISTAT,NVX,NTRK,WEIT,IDPRO,ECMS,LWRT)
C - input   : IEVT   = event#
C             ISTAT  = status word ( = 1 means OK)
C                      the event is written out only if ISTAT=1
C             NVX    = # of vertices in the event
C             NTRK   = # of tracks in the event
C             IDPRO  = event process identification
C             WEIT   = event weight
C             ECMS   = beam energy in center of mass
C             LWRT   = output logical unit ( 0 means do not write)
#ifndef DOC
      SAVE
#include "bcs.h"
      INTEGER ALEVEH,ALKEVH,NRDN(3)
      DATA NARUN/0/
#include "bmacro.h"
C ------------------------------------------------------------------
C
C - Get RUNH name-index
      IF (NARUN.EQ.0) NARUN = NAMIND ('RUNH')
C
      IF (ISTAT .EQ. 1) THEN
C     Compress KINE and VERT banks
      CALL KIBPRS ('VERTKINE')
C     Fill EVEH and KEVH banks
         IEXP = IW (IW(NARUN) + 1)
         IRUN = IW (IW(NARUN) + 2)
         IPRO = IW (IW(NARUN) + 3)
         JEVEH = ALEVEH (IEVT,IEXP,IRUN,ECMS,IPRO,ISTAT)
         IF (JEVEH .EQ. 0) GOTO 100
C
         CALL RDMOUT (NRDN)
         JKEVH = ALKEVH (NRDN,NTRK,NVX,IDPRO,WEIT)
         IF (JKEVH .EQ. 0) GOTO 100
C
C     Write event if required
         IF (LWRT .NE. 0) THEN
            IW(1) = 0
            CALL BWRITE (IW,LWRT,'C')
            CALL BLIST (IW,'C=','0')
            IW(1) = 1
            CALL BWRITE (IW,LWRT,'E')
         ENDIF
      ENDIF
C
C - Drop 'E' list and garbage collection
C
 100  CONTINUE
      CALL BDROP (IW,'E')
      CALL BGARB (IW)
C
      END
#endif
