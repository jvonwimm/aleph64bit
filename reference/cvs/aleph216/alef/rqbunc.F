      SUBROUTINE RQBUNC(IBUN,INBU,NWAG,IQUA)
C---------------------------------------------------------------------
CKEY ALEF BUNCH
C - P.Comas, 28-APR-95
C! Read the bunch number to be used for reconstruction from EVEH.
C! The routine provides:
C!      1 The wagon identification
C!      2 The interbunch distance from LZZT
C!      3 The number of wagons per train from LZZT
C!      4 The quality of the wagon identification
C!
C!  Input  : EVEH, LZZT banks
C!  Output : IBUN
C!           Wagon identification in the bunch
C!              0         no measure
C!              1,2,3,4
C!
C!           INBU
C!           Interbunch distance in ns
C!
C!           NWAG
C!           Number of wagons per train
C!
C!           IQUA
C!           Quality flag of the wagon identification
C!              0         no information, bunch number is zero
C!              1         ambiguous, inconsistent
C!              2         probable
C!              3         sure, no discussion
C!
C! Modified: P.Comas, 24-MAY-96
C!            Set NWAG and INBU for '96.
C!            Avoid trying to get the info from the
C!             unreliable LZZT as the bunch train scheme
C!             doesn't change wihtin the year.
C!
C---------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "evehjj.h"
C
      LOGICAL BUNCH
      SAVE IBUNS,INBUS,NWAGS,IQUAS,NEVEH,BUNCH
      SAVE NWADEF,INBDEF
      DATA IEOLD,IROLD/2*0/
C
#include "bmacro.h"
C---------------------------------------------------------------------

      IF (IROLD.EQ.0) THEN
        NEVEH = NAMIND('EVEH')
      ENDIF
C
      CALL ABRUEV (IRUN,IEVT)
C
C - Once per run ===========================================
C
      IF (IRUN.NE.IROLD) THEN
         IROLD = IRUN
         BUNCH = .TRUE.

         IF (IRUN.GE.41000) THEN
C '96 running
           NWADEF = 2
           INBDEF = 335
         ELSE
C '95 running
           NWADEF = 4
           INBDEF = 247
         ENDIF
C
C - NO bunch trains for MC and data < '95 except Nov 94 test
         IF (IRUN.LT.35000) THEN
           IF ((IRUN.NE.29989).AND.
     .         (IRUN.NE.29990).AND.
     .         (IRUN.NE.29991).AND.
     .         (IRUN.NE.30038).AND.
     .         (IRUN.NE.30042).AND.
     .         (IRUN.NE.30043).AND.
     .         (IRUN.NE.30044).AND.
     .         (IRUN.NE.30045).AND.
     .         (IRUN.NE.30046).AND.
     .         (IRUN.NE.30060).AND.
     .         (IRUN.NE.30248).AND.
     .         (IRUN.NE.30249).AND.
     .         (IRUN.NE.30250).AND.
     .         (IRUN.NE.30251).AND.
     .         (IRUN.NE.30252).AND.
     .         (IRUN.NE.30253).AND.
     .         (IRUN.NE.30254).AND.
     .         (IRUN.NE.30255).AND.
     .         (IRUN.NE.30380).AND.
     .         (IRUN.NE.30381) ) THEN
              BUNCH = .FALSE.
           ELSE
C          Run of the bunch trains test of November 1994
              INBUS = INBDEF
              NWAGS = NWADEF
           ENDIF
C
         ELSE
           INBUS = INBDEF
           NWAGS = NWADEF
         ENDIF
C
      ENDIF
C
C - Once per event ==================================================
C
      IF (IEVT.NE.IEOLD) THEN
         IEOLD = IEVT
         IF (BUNCH) THEN
C        bunch trains get bunch number and quality flag
            KEVEH = IW(NEVEH)
            KBUNCH = IW(KEVEH+JEVEM4)
            IF (IRUN.LT.35000) THEN
               IBUNS = IBITS (KBUNCH,0,3)
               IQUAS = 3
C        tagging of level 3 was not working at the beginning of 95
C        so set IBUN=1, IQUA=3 for runs in 4x1 configuration and
C        leave as before those in 4x2 configuration
            ELSE IF ( (IRUN.GE.35000).AND.(IRUN.LT.35718).AND.
     .         (IRUN.NE.35600).AND.
     .         (IRUN.NE.35601).AND.
     .         (IRUN.NE.35602).AND.
     .         (IRUN.NE.35603).AND.
     .         (IRUN.NE.35604).AND.
     .         (IRUN.NE.35609).AND.
     .         (IRUN.NE.35610).AND.
     .         (IRUN.NE.35611).AND.
     .         (IRUN.NE.35612).AND.
     .         (IRUN.NE.35613).AND.
     .         (IRUN.NE.35662) ) THEN
               IBUNS = 1
               IQUAS = 3
            ELSE
               IBUNS = IBITS (KBUNCH,27,3)
               IQUAS = IBITS (KBUNCH,30,2)
            ENDIF
         ELSE
C        NO bunch trains
            IBUNS = 1
            INBUS = 0
            NWAGS = 1
            IQUAS = 3
         ENDIF
      ENDIF
C
C - end ===========================================================
C
999   IBUN=IBUNS
      IQUA=IQUAS
      INBU=INBUS
      NWAG=NWAGS

      RETURN

      END
#endif
