      COMMON /HEND/  HCDREC,HCDSTP,HCAPSL,HFSPEC,NHCSEX
     &           ,NHCEFS,NHCEBS,NHCTRE,NHCINL,NHCOUL
     &           ,NHCIND,NHCOUD
C
#if defined(DOC)
C!  Hadron Calorimeter End-Cap Costants
        commons used to load data base HEND constants
        HCDREC  thickness of end-cap outer wall (2.5 cm)
        HCDSTP  distance between iron spacers in end-cap sextants (82. c
        HCAPSL  thickness of iron spacers in end-cap (2. cm)
        HFSPEC  position of first sensitive plane in end-cap (5. cm)
        NHCSEX  number of sextants in an end-cap (6)
        NHCEFS  number of pads in end-cap towers front stack (11)
        NHCEBS  number of pads in end-cap towers back  stack (11)
        NHCTRE  number of double eightfolds read toghether in sextant (1
        NHCINL  number of layers in inner part of sextant (7)
        NHCOUL  number of layers in outer part of sextant (15)
        NHCIND  number of double eightfolds in inner part (10)
        NHCOUD  number of double eightfolds in outer part (20)
#endif
