*CD hctrig
      PARAMETER (LPHCTR= 12,LPHCTF=24,LHCRT=62)
      COMMON /HCTRIG/ NHCEPR,NHCBPR,NHCETR,NHCBTR , IHCTRG(LHCRT),NHCBTS
     +(LHCRT),NHCETS(LHCRT) , IYHCFI(LPHCTF),IXHCFI(LPHCTR),IXHCSE
     +(LPHCTR) , MHCETR,MHCBTR

#if defined(DOC)
C! Trigger costants for HCAL
        commons used to load HCTG,HSBA,HSEC banks from the data base
        LPHCTR   # of electronic controllers (one end-cap)  (12)
        LPHCTF   # of electronic controllers (barrel) (24)
        LHCRT    # of tower row (for trigger pourpose) (62)
        NHCEPR number of end-cap phi trigger regions (HCTG)(12)
        NHCBPR number of barrel  phi trigger regions (HCTG)(24)
        NHCETR number of end-cap theta trigger regions (HCTG)(4)
        NHCBTR number of barrel   theta trigger regions (HCTG)(4)
        IHCTRG(LHCRT) theta trigger region number for towers in row (HCT
        NHCBTS(LHCRT) number of barrel trigger segments in row (HCTG)
        NHCETS(LHCRT) number of end cap trigger segments in row (HCTG)
        IYHCFI(LPHCTF) electronic module number (barrel) (HSBA)
        IXHCFI(LPHCTR) electronic number of controller of first half
                       of sextant (end-cap) (HSEC)
        IXHCSE(LPHCTR) electronic number of controller of second half
                       of sextant (end-cap) (HSEC)
      MHCETR = (NHCEPR-1) * NHCETR
      MHCBTR = (NHCBPR-1) * NHCBTR
#endif
