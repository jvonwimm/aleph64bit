      COMMON /HCALGE/HCRMIN(LSHC),HCRMAX(LSHC),HCZMIN(LSHC),HCZMAX(LSHC)
     &              , NHCSUB,NHCTWR,NHCPHC,NHCREF,IHCTID(LHCRE-1)
     &              , HCTUTH,HCIRTH,HCLSLA,NHCPLA(LPHC),HCTIRF(LPHC)
     &              , HCSMTH
#if defined(DOC)
C! General Geometrical Constants for the Hadron Calorimeter
C I=1,lphc (I=1 half-barrel,I=2 inner end-cap,I=3 outer end-cap)
        HCRMIN(LPSHC) subpart minimum radius (297.3,45.,45.)
        HCRMAX(LPSHC) subpart maximum radius (468.4,210.,435.)
        HCZMIN(LPSHC) subpart minimum Z  (0.,315.,365.4)
        HCZMAX(LPSHC) subpart maximum Z  (365.4,365.4,483.4)
        NHCSUB number of subdetectors for HCAL (3)
        NHCTWR number of tower rows (62)
        NHCPHC max number of phi col. (96)
        NHCREF # of phi regions (3)
        IHCTID(2) Theta Tower adress of lower border of region number (4
        HCTUTH  sensitive plane thickness  (2.2 cm)
        HCIRTH  iron slab thickness (5. cm)
        HCLSLA  thickness of iron slab 23 (10 cm)
        NHCPLA(LPHC) number of planes in each portion (22,23,22)
        HCTIRF(LPHC) position of first sensitive plane in portion (5.,.5
        HCSMTH  sampling thickness (7.2 cm)
#endif
