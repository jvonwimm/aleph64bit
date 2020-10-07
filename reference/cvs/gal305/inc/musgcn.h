*CD musgcn
C! The current constants to create MU signals
      COMMON/MUSGCN/WDMDMU,XLMDMU,NMEIMU,XLEIMU,KPMDMU,ZPSWMU(2),
     *              NASLMU,NUSLMU,NUEMMU,NYSTMU,NXSTMU,XSOFMU(2),
     *              YSOFMU(2),GP16MU,WD16MU,YCUTSL(2),YCUTSB(2)
      COMMON/MUSGKN/   TMUCVO,TMU3VO
      CHARACTER*4 TMUCVO,TMU3VO
C
#if defined(DOC)
      WDMDMU -- width of the current module
      XLMDMU -- length of the current module
      NMEIMU -- # of eightfold tubes in the module
      XLEIMU -- length of eightfold tube in the module
                (used for barrel and middle angle module)
      KPMDMU -- pointer of 'MCRD' to the module
                (used for endcap module to find the length
                 of eightfold tube)
      XOFFMU -- X-offset of two tube planes
      YOFFMU -- Y-offset of two tube planes
      ZPSWMU -- Z-coordinate of wires of two tube plane in
                module reference system
      NASLMU -- slot name of the module
      NUSLMU -- slot # of the module
      NUEMMU -- electronics module # of this module
      NPEMMU -- order # of the module in the electronics module
                it belongs to (in the case of one electronics module
                has more than one module, NPEMMU could be great than 1)
      NXSTMU -- # of X-strips in the module
      NYSTMU -- # of Y-strips in the module
      NXSOMU -- offset # of two X-strip planes
      NYSOMU -- offset # of two Y-strip planes
      XSOFMU -- position offset of two X-strip plane
      YSOFMU -- position offset of two Y-strip plane
      GP16MU -- width of gap between 16-fold tubes
      WD16MU -- 2*WDEIMU + GP16MU
      YCUTSB(IPLN) -- Position of the first active Y-strip
      YCUTSL(IPLN) -- Position of the last active Y-strip
#endif
