C! Tubes and Towers Constants
       PARAMETER (LHCBL=4,LHCEI=10,LHCEO=20,LHNLA=4)
      COMMON /HCCONS/ HCTHRF,HCRSIZ,HCZSIZ,NHCBAR,NHCECA
     &               ,NHCEIT,HCEIWI,HCDOWI,HCTUGA,HCSEPO
     &               ,HCSABL,HCSAEC,HCTUEN,XLNHCE(LHCBL)
     &               ,HCTLEI(LHNLA,LHCEI),HCTLEO(LHNLA,LHCEO)
     &               ,HCTAEI(LHCEI),HCTAEO(LHCEO)
#if defined(DOC)
    commons used to load HCCO and HCTC banks from the data base
    LHCBL  # of different lengths of eightfold in barrel (4)
    LHCEI  # of different lengths of double eigthfold in (inner) end-cap
    LHCEO  # of different lengths of double eigthfold in (outer) end-cap
    LHNLA  # of tube lengths used to define a double-eigthfold (4)
    HCTHRF  theta  reference value for theta boundaries computation (.72
    HCRSIZ  radial reference value for theta boundaries computation (377
    HCZSIZ    z    reference value for theta boundaries computation (394
    NHCBAR  number of theta boundaries in half barrel (18)
    NHCECA  number of theta boundaries in hal end-cap (15)
    NHCEIT  number of tubes in eightfold (8) (HCTC)
    HCEIWI  width of eightfold tube (8.2 cm) (HCTC)
    HCDOWI  double eightfold width  (16.4 cm) (HCTC)
    HCTUGA thickness of streamer tube gap (1 cm) (HCTC)
    HCSEPO  position of second eightfold in double eightfold (8. cm) (HC
    HCSABL  average tube dimension in barrel (1.025 cm) (HCTC)
    HCSAEC  average tube dimension in end-cap (1.025 cm) (HCTC)
    HCTUEN  size of insensitive zone at the end of the tube (3 cm) (HCTC
    XLNHCE(LHCBL)  length of eightfold in barrel  (HCTC)
    HCTLEI(LHNLA,LHCEI) lengths of double-eight.in end-cap inner part(HC
    HCTLEO(LHNLA,LHCEO) lengths of double-eight.in end-cap outer part(HC
    HCTAEI(LHCEI) tube profile angle in inner end-cap (HCTC)
    HCTAEO(LHCEO) tube profile angle in outer end-cap (HCTC)
#endif
