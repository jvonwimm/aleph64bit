#ifdef DOC
C! Statistics common for VGLOB algorithm
C
C NEvtVD = number of events processed
C TimTot = total CPU time used
C TimHit = CPU time in processing hit combinations
C TimKal = CPU time in full (iterated) Kalman fit
C TimSol = CPU time in solving combinatorics
C
C NHitVD(NVIEW) = Number of hits in each view (1=z, NVIEW=r-phi)
C NHitAs(NVIEW) = Number of hits which were assigned
C NHitDb(NVIEW) = Number of double hits
C
C note: NVIEW (=2) is defined in VGLBCM
C
#endif
      INTEGER NEvtVD
      REAL TimTot, TimHit, TimKal, TimSol
      INTEGER NHitVD(NVIEW), NHitAs(NVIEW), NHitDb(NVIEW)
C
      COMMON /VGLBST/ NEvtVD, TimTot, TimHit, TimKal, TimSol,
     $     NHitVD, NHitAs, NHitDb 
