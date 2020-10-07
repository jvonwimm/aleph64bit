      SUBROUTINE MCMATCH(IMCTRK,IALTRK,IFLAG)
CKEY MATCHING / USER
C-------------------------------------------------------------------------------
C! Match a MC truth with reconstructed, and classify the track.
C  Author  Dave Brown, 7-6--94
C
C  Good tracks are classified according to the origin of the true track
C
C  Input argument :
C   IMCTRK = Input ALPHA track number
C  Output arguments :
C   IALTRK = Associated ALPHA MC true track if there's a reasonable match
C          = 0 otherwise
C   IFLAG = -3    No Match (lost track)
C           -2    Track decays before making hits
C           -1    No well matching track
C            0     mc track double counted
C            1     Normal good match from primary vertex
C            2     Normal good match from secondary vertex
C            3     Normal good match from V0 decay
C            4     Kink fit as 2 tracks
C            5     Kink fit as a single track
C            6     Normal good match from material interaction
C-------------------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C
C  alpha variables
C
#include "qdecl.h"
#include "qcde.h"
C
C  IO variables
C
      INTEGER IALTRK,IFLAG,IMCTRK
C
C  Local variables
C
      INTEGER JALTRK,JMCTRK,JFLAG
      INTEGER NDAU
      INTEGER NMATCH,IMATCH
C
C  Functions
C
      INTEGER NAMIND
C
C  Inline functions
C
#include "qmacro.h"
C-------------------------------------------------------------------------------
C  Initialize variables
C
      IALTRK = 0
C
C  Get the MC true ITC/TPC hit matching
C
      NMATCH = KNMTCH(IMCTRK)
      IF (NMATCH.EQ.0) THEN
C
C  Lost track; Check if the particle decays
C
        NDAU = KNDAU(IMCTRK)
        IF (NDAU.GT.0) THEN
          IFLAG = -2
        ELSE
          IFLAG = -3
        END IF
        GOTO 999
      END IF
C
C  Look at the matches, and take the best one
C
      IFLAG = -1
      DO IMATCH=1,NMATCH
        JALTRK = KMTCH(IMCTRK,IMATCH)
        CALL JULMATCH(JALTRK,JMCTRK,JFLAG)
        IF (JMCTRK.EQ.IMCTRK) THEN
C
C  Check the history code and classify
C
          IF (JFLAG.GE.0) THEN
C
C  Good match; check that it hasn't already been matched
C
            IALTRK = JALTRK
            IF (IFLAG.GE.0) THEN
              IFLAG = 0
            ELSE
              IFLAG = JFLAG
            END IF
          END IF
        END IF
      END DO
 999  CONTINUE
C
      RETURN
      END
#endif
