      SUBROUTINE CALHIT
C -----------------------------------------------------
C - J.Badier F.Ranjard - 870817
C! create geantino in calorimeters
C  called by hadron stopping because of a hadronic interaction
C - called from GUSTEP                      from this .HLB
C - calls       CACUTG, CATINO              from this .HLB
C ------------------------------------------------
      SAVE
C
C - decide wheither Yes/No a geantino is created
C   IF Yes THEN create the geantino
C
      CALL CACUTG (MFLAG)
      IF (MFLAG.EQ.1) CALL CATINO
C
      END
