      PARAMETER (NIGN = 3,NPEXI = 11,NPUSE = 8,IOS = 2,NBMAX = 62,
     +   NVMAX = 31,NTMAX = 256,ILM = 15,IUM = 31,ISMX = 9,ICHX = 4,
     +   MSKLN = 60 )
      COMMON /X2CONS/ TDVELO,ADVELO,IGNPAD(NIGN),IPDCON(NPEXI)
     +  ,RADPAD(NPUSE,IOS),CLOCKR
     +  ,NTBINS(IOS),ZACPMM,IZBMAX
     +  ,ITHETA(IOS,0:NTMAX,NPUSE),ITHSUB(IOS,0:NTMAX,NPUSE)
     +  ,ITHOVR(IOS,0:NTMAX,NPUSE),ITHOSB(IOS,0:NTMAX,NPUSE)
     +  ,IHTMAX(IOS,NBMAX),ITHRSH(NPUSE),ITVOTE(0:NVMAX,0:NVMAX)
     +  ,IPADPR(IOS,ICHX),IZRDLK(ILM,IUM,ISMX,ICHX,IOS)
     +  ,IX2PRL,IRWDTH,IDIGNZ
     +  ,MASCON(NBMAX,IOS,IOS),IX2MSK(MSKLN),IX2HIS,IX2RUN
#if defined(DOC)
!   general constants and tables for level 2 trigger
  TDVELO   :   true drift velocity
  ADVELO   :   assumed drift velocity
  IOS      :   number of sectors = ordinal number of the outermost.
  NPEXI    :   number of padrows in outer sector
  NPUSE    :   number of padrows used = ordinal number of outermost.
  IGNPAD   :   (1-3) ignored padrows
  NBMAX    :   maximum number of complete theta-bins
  NVMAX    :   number of checks for voting possiblity in 'roads' algori
  NTMAX    :   maximum number of time-bins.
  ILM      :   maximum number of sub-theta bins in a theta-bin.
  IUM      :   maximum number of sub-theta bins in theta-bin + overlap.
  ISMX     :   index to the storage of z-acceptance values.
  ICHX     :   number of choices of pad pairs in making 'roads'
  MSKLN    :   number of words in trigger mask.
  IPDCON   :   converts row 1-19 to 1-8
  RADPAD   :   radii of used pads
  CLOCKR   :   clock rate
  NTBINS   :   (1-2) number of theta bins in inner/outer
  ZACPMM   :   z-acceptance in mm
  IZBMAX   :   z-bin maximum
  ITHETA   :   time-theta look up table
  ITHSUB   :   time-subtheta look up table
  ITHOVR   :   time-overlap look up table
  ITHOSB   :   time-overlap subtheta look up table
  IHTMAX   :   maxhits in a theta bin
  ITHRSH   :   hit threshold for theta bins
  ITVOTE   :   comparison array for voting
  IPADPR   :   pad pair choices for road analysis
  IZRDLK   :   z-road look up table
  IX2PRL   :   print output level
  IRWDTH   :   road width
  IDIGNZ   :   digital noise level
  MASCON   :   theta - mask segment conversion
  IX2MSK   :   level 2 mask
  IX2HIS   :   histogram flag (0 = off, 1 = on)
  IX2RUN   :   current run number
#endif
