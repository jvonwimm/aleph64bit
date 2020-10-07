*CD geomnews
C! 1st entry in GEOM set which builds the geometry using GEANT.
   detailed documentation can be found in *DK AGDOC.

 ! GALEPH 30.2
   remove calls to GSORD which is now a 'do nothing' routine.
   AGGORD - remove the routine.
   AGSATT - new routine , copied from GUINTI which is removed.
            set volume attributes which are not set by *ATT bank.
   AGGORD - remove the routine, code is included into AGGEAN.
   AGGEAN - read *ATT banks and call GSATT.
   AGEOME - call AGSATT before closing the geometry.
   AGVF92 - '92 vdet face geometry.
   AGVF95 - '95 vdet face geometry.
   AGVDET - call AGVF92 or AGVF95 depending oN the year.

 ! GALEPH 25.6
    AGMATE : add a SAVE statment ( this routine is never called)

 ! GALEPH 25.5
    AGEOME : update description of central region     (B.Bloch May 1993)
    AGITCH : implement geometry from Data base banks  (B.Bloch May 1993)
    AGHCAL : update materials (L.Silvestris)

 ! GALEPH 25.5
    AGEOME : update description of central region     (B.Bloch May 1993)
    AGITCH : implement geometry from Data base banks  (B.Bloch May 1993)
    AGHCAL : update materials (L.Silvestris)


 ! GAL254
   AGCHCK, AGGEAN, AGGATT, AGGORD:
   calls ALGTDB instead of MDARD with the setup code to avoid to
   repeat on the data base geometry banks with different NR but
   with identical content.
   AGCHCK, AGSCAL, AGVDET:
   change test on setupcode from .EQ.last to .GE.last

