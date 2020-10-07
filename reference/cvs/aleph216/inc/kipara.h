CKEY KINE KINGAL DEFAULT
      PARAMETER (LHKIN=3, LPKIN=5, LKVX=2, LHVER=3, LPVER=5, LVKI=50)
      PARAMETER (LGDCA=32)
      PARAMETER (LRPART=200, LCKLIN=1)
      PARAMETER (LRECL=16020, LRUN=1, LEXP=1001, LRTYP=1000)
      CHARACTER*60 LTITL
      PARAMETER (LUCOD=0, LNOTRK=100, LTITL='KINGAL run')
      PARAMETER (LUTRK=450)
      PARAMETER (BFIEL=15., CFIEL=BFIEL*3.E-4)
#if defined(DOC)
          Define parameters needed by the KINGAL package
      LHKIN        = KINE header length
      LPKIN        = # of fix parameters in KINE bank
      LKVX         = initial # of vertices in KINE bank
      LHVER        = VERT header length
      LPVER        = # of fix parameters in VERT bank
      LVKI         = initial # of tracks in VERT bank
      LNOTRK       = NO tracking marker word (GEANT)
      LGDCA        = part. with geant# up to NODEC should not be
                     decayed by the generator
      LRPART      = # of rows to increase PART and KLIN banks
      LCKLIN      = # of columns in KLIN bank
      LRECL       = BOS EPIO record length (16380 16bit words)
      LRUN        = default run#
      LEXP        = default experiment#
      LRTYP       = default run type
      LUCOD       = default user generator code
      LTITL       = default run title
      LUTRK       = maximum # of tracks in a lund event
      BFIEL       = magnetic field (Kgauss)
#endif
