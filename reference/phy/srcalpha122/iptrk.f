      SUBROUTINE IPTRK(NTRK,JTRK,IP,IP_ERR,NUSED)
CKEY   QIPBTAG / INTERNAL
C ----------------------------------------------------------------------
C! Entry point for calculating the effect of removing a single track
C  from an existing vertex.  This relies on a valid vertex to have alrea
C  been found.  Dave Brown, 29-1-93
C  Called from FINDDMIN or MAKE2D
C
C  INPUTS:
C     NTRK     =  # of tracks to exclude
C     JTRK     =  FRFT track number of tracks to exclude from vertex
C                 (=ALPHA track number - KFCHT + 1).
C  Outputs:
C
C      IP      =  New primary vertex position.  If the given track wasn'
C                 used in the vertex fit, this will be the same as the
C                 original.
C    IP_ERR    =  Error matrix of above vertex.
C    NUSED     =  Number of tracks used in the 2 different directions.
C                 Compared with the NUSED returned by QFNDIP, this allow
C                 you to know in which degrees of freedom the given
C                 track participated for the original vertex fit.
C ----------------------------------------------------------------------
      IMPLICIT NONE
C
C --     MAX. NUMBER OF TRACKS, jets, V0s ALLOWED
C
      INTEGER    MAXTRK,MAXJET,MAXV0,MAXDAU
      PARAMETER ( MAXTRK = 400, MAXJET = 40 , MAXV0 = 20 , MAXDAU = 10 )
      SAVE
C
C  Inputs and outputs
C
      INTEGER NTRK,JTRK(*),NUSED(2)
      REAL IP(3),IP_ERR(3,3)
C
C  Common variables; these transmit the necessary information from FINDI
C
      REAL ALPHA_SAVE, BETA_SAVE(3), GAMMA_SAVE(3,3)
      REAL HAT(3,4,MAXTRK)
      REAL DT0(4,MAXTRK),ERR2(5,MAXTRK)
      INTEGER IFLAG(MAXTRK)
      INTEGER NUSED_SAVE(2)
      LOGICAL USEIT
C
      COMMON/IPVTX/DT0,HAT,ERR2,IFLAG,
     &      ALPHA_SAVE,BETA_SAVE,GAMMA_SAVE,NUSED_SAVE
C
C  Local variables
C
      REAL ALPHA, BETA(3), GAMMA(3,3)
      INTEGER ICOR,JCOR,ITRK,ITYP,JFLAG,IERR
C
C  Inlines for defining good tracks
C
      USEIT(JFLAG,ITYP) = IAND(JFLAG,ISHFT(1,ITYP-1)).GT.0
C ----------------------------------------------------------------------
C
C  Copy the saved tensors, counters
C
      ALPHA = ALPHA_SAVE
      DO ICOR=1,3
        BETA(ICOR) = BETA_SAVE(ICOR)
        DO JCOR=1,3
          GAMMA(ICOR,JCOR) = GAMMA_SAVE(ICOR,JCOR)
        END DO
      END DO
      NUSED(1) = NUSED_SAVE(1)
      NUSED(2) = NUSED_SAVE(2)
C
C  Subtract off the degrees of freedom for this track that were
C  actually used
C
      DO ITRK=1,NTRK
        DO ITYP=1,3
          IF(USEIT(IFLAG(JTRK(ITRK)),ITYP))THEN
            ALPHA = ALPHA - DT0(ITYP,JTRK(ITRK))**2/
     &           ERR2(ITYP,JTRK(ITRK))
            DO ICOR=1,3
              BETA(ICOR) = BETA(ICOR) -
     &             DT0(ITYP,JTRK(ITRK))*HAT(ICOR,ITYP,JTRK(ITRK))/
     &             ERR2(ITYP,JTRK(ITRK))
              DO JCOR=1,3
                GAMMA(JCOR,ICOR) = GAMMA(JCOR,ICOR) -
     &               HAT(ICOR,ITYP,JTRK(ITRK))*
     &               HAT(JCOR,ITYP,JTRK(ITRK))/
     &               ERR2(ITYP,JTRK(ITRK))
              END DO
            END DO
C
C  Correct the number of degrees of freedom
C
            IF(ITYP.EQ.1)THEN
              NUSED(2) = NUSED(2) - 1
            ELSE
              NUSED(1) = NUSED(1) - 1
            END IF
          END IF
        END DO
      END DO
C
C  Compute the new position and error
C
      CALL RSINV(3,GAMMA,3,IERR)
      IF(IERR .NE. 0)THEN
        NUSED(1) = -1
        NUSED(2) = -1
        GOTO 1100
      END IF
      DO ICOR=1,3
        IP(ICOR) =  0.0
        DO JCOR=1,3
          IP(ICOR) = IP(ICOR) + BETA(JCOR)*GAMMA(ICOR,JCOR)
          IP_ERR(ICOR,JCOR) = GAMMA(ICOR,JCOR)
        END DO
      END DO
 1100 CONTINUE
      RETURN
      END
