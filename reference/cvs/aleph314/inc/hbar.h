      COMMON / HBAR / NHCBMO,NHCBFS,NHCBBS,NHCBLA,HCLTNO(LHCNO)
     &              , HCWINO(LHCNO),HCDEWI,NHBLA2,NHBLI3,NHBLO3
     &              , NEITHC(LHCNL),NEITSP(LHCNL,LHCSP)
     &              , HCSPLT(LHCNL,LHCSP), HFSPBL,HFSRBL,HCPHOF
C
#if defined(DOC)
C! Hadron Calorimeter Barrel Costants
        NHCBMO # of modules   (24)
        NHCBFS number of pads in barrel towers front stack (11)
        NHCBBS number of pads in barrel towers back  stack (12)
        NHCBLA # of planes (23)
        HCLTNO(LHCNO) length(z) of notch # 1,2,3
        HCWINO(LHCNO)  width of notch # 1,2,3
        HCDEWI  dead zone width on barrel module border (2. cm)
        NHBLA2  # of eigh. of type 2 (4)
        NHBIL3  # of eigh. of type 3 (inner) (4)
        NHBLO3  # of eigh. of type 4 (outer) (1)
        NEITHC(LHCNL) # of eigh. per layer [9-13]
        NEITSP(LHCNL,LHCSP) # of eigh. befor first and second spacer [-1
        HCSPLT(LHCNL,LHCSP) spacer width [0.,8.]
        HFSPBL  position of first sensitive plane in barrel (.5 cm)
        HFSRBL  radius of the first active layer (298.9 cm)
        HCPHOF  phi offset (0. cm)
#endif
