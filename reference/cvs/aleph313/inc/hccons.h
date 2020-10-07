       PARAMETER (LHCBL=4,LHCEI=10,LHCEO=20,LHNLA=4)
      COMMON /HCCONS/ HCTHRF,HCRSIZ,HCZSIZ,NHCBAR,NHCECA
     &               ,NHCEIT,HCEIWI,HCDOWI,HCTUGA,HCSEPO
     &               ,HCSABL,HCSAEC,HCTUEN,XLNHCE(LHCBL)
     &               ,HCTLEI(LHNLA,LHCEI),HCTLEO(LHNLA,LHCEO)
     &               ,HCTAEI(LHCEI),HCTAEO(LHCEO),HTINBL,HTINEC(2)
     &               ,HTPIEC,HTPOEC,HBWREC,HBWCEC(2),HBSREC
     &               ,HBSCEC,HBWRBL,HBSCBL,NHMBDF(2),NHTY4D
     &               ,NHTY3D,NHTY2D,NHMBFL(2),NHBDOU,NHDLEC
     &               ,NHDET0,NHDEBS,NHL8EC(LHCNL-1),HCTUSH,XHCSHI(LHCBL)

#if defined(DOC)
C! Tubes and Towers Costants
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
    HTINBL  tub's intrusion barrel (2. cm)
    HTINEC(2)  tub's intrusion endcap (1=read_out side 2.9,2=blind side 2.0)
    HTPIEC  inner side endcap tub's cap (2.0 cm)
    HTPOEC  outer side endcap tub's cap (.5 cm)
    HBWREC  plastic plug  +card support ,read_out side endcap(3.6 cm)
    HBWCEC(2) plastic plug +card support ,blind side endcap(2.6(30),2.5(0))
    HBSREC  plastic plug card support only ,read_out side endcap (1.7 cm)
    HBSCEC  plastic plug card support only ,blind side endcap (1.4 cm)
    HBWRBL  plastic plug +card support ,barrel (2.5 cm)
    HBSCBL  plastic plug card support only ,barrel (1.4 cm)
    NHMBDF(2) modules with not-standard sequence of 8fold, barrel (6,7)
    NHTY4D  # of 8fold of type 4 for modules 6 and 7 (3)
    NHTY3D  # of 8fold of type 3 for modules 6 and 7 (1)
    NHTY2D  # of 8fold of type 2 for modules 6 and 7 (4)
    NHMBFL(2) barrel modules with a reduced first plane (15,22)
    NHBDOU  # of 8fold killed in the first plane of modules 15 et 22 (2)
    NHDLEC  endcap layer with not-standard sequence of double_8fold. (8)
    NHDET0  # of Double_8fold with 0 degress inclination in endcap_layer(3)
    NHDEBS  # of Double_8fold between iron frames in endcap (5)
    NHL8EC(LHCNL-1) tub's type in the layer 8 in endcap's
    HCTUSH         total tube shift in barrel
    XHCSHI(LHCBL)  shift of sensitive part of eightfold I in barrel
#endif
