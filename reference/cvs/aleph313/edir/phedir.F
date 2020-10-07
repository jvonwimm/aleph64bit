      SUBROUTINE PHEDIR (SNGPHO)
C-----------------------------------------------------------------------
CKEY EDIR DEF CLASS21
C! Selects events containing one or more photons for ALEPH
C! event directories.
C-
C   Input  : None
C   Output : SNGPHO = Class 21 logical flag
C-
C   Called by   : SELEVT
C   Calls  : MODFND
C   Input banks : REVH,PFRT,XTEB,PECO,EWHE
C-
C                               Author: Lee Thompson, 31st January 1990
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C --
#include "bcs.h"
#include "revhjj.h"
#include "pfrtjj.h"
#include "xtebjj.h"
#include "pecojj.h"
#include "pewijj.h"
      INTEGER ALTRIG
      LOGICAL LHVTPC,SNGPHO
      LOGICAL BTEST
#include "bmacro.h"
C --
      SNGPHO = .FALSE.
C --
C   If the event has TPC HT off or then do no further processing.
C   do no further processing.
C --
C     LHVTPC = .FALSE.
C     JREVH = IW(NAMIND('REVH'))
C     IF (JREVH .NE. 0) THEN
C         KHVSTA = ITABL(JREVH,1,JREVDS)
C         LHVTPC = BTEST(KHVSTA,15)
C     END IF
C     IF (.NOT. LHVTPC) RETURN
C --
C   If the event has at least one good TPC track (more than 4 space
C   points) then do no further processing.
C --
      JPFRT = IW(NAMIND('PFRT'))
      IF (JPFRT .NE. 0) THEN
          NTRACK = LROWS(JPFRT)
          DO 1 ITRACK = 1 , NTRACK
              IF (ITABL(JPFRT,ITRACK,JPFRNT) .GE. 4) RETURN
 1        CONTINUE
      END IF
C --
C   Next check the trigger - require that the SNG_N_EM trigger
C   is present in the Level 1 trigger word otherwise return.
C --
      ISNGEM = 2
      ICODE = ALTRIG(ITLVL1,ITLVL2,ITLVL3)
C       IF(ICODE.EQ.0) RETURN
C       IF (.NOT. BTEST(ITLVL1,ISNGEM)) RETURN
C
C     JXTEB = IW(NAMIND('XTEB'))
C     IF (JXTEB .NE. 0) THEN
C         ITLVL1 = ITABL(JXTEB,1,JXTET1)
C         IF (.NOT. BTEST(ITLVL1,ISNGEM)) RETURN
C     END IF
C --
C Check the wire energy
C --
      IEWOK = 0
      KPEWI = IW(NAMIND('PEWI'))
      IF (KPEWI.EQ.0) KPEWI = IW(NAMIND('PWEI'))

      IF (KPEWI.NE.0) THEN
         NWI = LROWS(KPEWI)
 
C -- Loop over modules:
        DO 30 IP=1,NWI
          IMOD = ITABL(KPEWI,IP,JPEWMN)
 
C -- Add energies of all planes in this module.
C -- Notice that PEWI has only the modules above 50 MeV.
          ENSUM = 0.
          DO 20 IADC=1,45
            ENSUM = ENSUM + FLOAT(ITABL(KPEWI,IP,JPEWPD+IADC-1))/1.E6
   20     CONTINUE

C -- Check if this energy is above the wire cut
          IF (ENSUM.GT.0.5) IEWOK = 1

   30   CONTINUE
 
      ENDIF
C --
C   Now start a loop over ECAL clusters
C --
      JPECO = IW(NAMIND('PECO'))
      IF (JPECO .EQ. 0) GOTO 999
      NCLUST = LROWS(JPECO)
      DO 10 ICLUST = 1 , NCLUST
C --
C   If the cluster has :
C      a) Energy > 1.0 GeV
C      b) Cluster energy is not all in one stack
C      c) 20 degrees < Theta < 160 degrees
C   then continue, otherwise go to the next cluster.
C --
          ECLUST = RTABL(JPECO,ICLUST,JPECEC)
          IF (ECLUST .LT. 1.0) GOTO 10
          S1 = RTABL(JPECO,ICLUST,JPECE1)
          S2 = RTABL(JPECO,ICLUST,JPECE2)
          S3 = 1.0 - S1 - S2
          IF (S1.GT.0.99.OR.S2.GT.0.99.OR.S3.GT.0.99) GOTO 10
C         IF (ECLUST*S2 .LT. 0.1) GOTO 10
C         IF (ECLUST*S1 .LT. 0.1 .AND. ECLUST*S3 .LT. 0.1) GOTO 10
          THETAR = RTABL(JPECO,ICLUST,JPECTH)
          PHIR   = RTABL(JPECO,ICLUST,JPECPH)
          THETAD = THETAR * 180.0 / ACOS(-1.0)
          IF (THETAD .LT. 13.0 .OR. THETAD .GT. 167.0) GOTO 10
C --
C   If wire energy OK flag event and return,
C   otherwise, go to next cluster.
C --
          IF (IEWOK.EQ.1) THEN
             SNGPHO = .TRUE.
             RETURN
          END IF
C --
   10 CONTINUE
C --
  999 RETURN
      END
#endif
