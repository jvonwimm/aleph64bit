C--   output variables from routine QIPBTAG:
C     These varaibles are not stored in COMMON/BTAGRAW/( ... )
C     they are directly passed out by the routine...
C     IRET : return CODE
C     NJET:  #of jets used for analysis
C     NTRACK #of tracks used for analysis
C     FRF2TRK: FRFT/2 track number of tracks used foe analysis
C     TRKJET : ALPHA track number of jets used for analysis
C     PROBEVT: event probabilities
C     PROBJET: jet   probabilities
C     PROBTRK: track probabilities
      INTEGER IRET
      INTEGER NJET,NTRACK
      INTEGER FRF2TRK(MAXTRK),TRKJET(MAXJET)
      REAL    PROBEVT,PROBHEMI(2),PROBJET(MAXJET),PROBTRK(MAXTRK)
