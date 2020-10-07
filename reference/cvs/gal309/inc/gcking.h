C J. von Wimmersperg 2020 - change LGKINE from 100 to 250 (v321.13 -> v321.14)
      PARAMETER (LGKINE=250)
      COMMON/GCKING/KGCASE,NGKINE,GKIN(5,LGKINE),GTOFD(LGKINE)
     &             ,IGFLGK(LGKINE)
C
      COMMON/GCKIN3/GPOS(3,LGKINE)
      REAL GPOS
C
#if defined(DOC)
      KGCASE             process name (4 Hollerith char.)
      NGKINE             number of secondaries produced
      GKIN      1,I      Px of Ith-secondary
                2,I      Py
                3,I      Pz
                4,I      E
                5,I      Geant particle #
       GTOFD             filled by Gheisha
                         additional delay introduced by some
                         nuclear processes
       IGFLGK            = 0 means store the track in STAK
                         = 1 means store the track in KINE/VERT
       GPOS              vertex position of Ith-secondary
#endif
