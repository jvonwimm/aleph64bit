      PARAMETER (LWBNK = 500,JWPHT = 3,IDFLEN = 1000
     +    ,NUMWB = 24, NZONE = 2 )
      COMMON /X2NAMC/ NAX2DF,JWORKB(NUMWB),NAX2MS,NAX2TB,
     +                JX2TRK(NZONE), JX2SOR(NZONE)
#if defined(DOC)
!  bos bank names for level2 trigger
  NAX2TF  :  tracks found
  NAX2HF  :  hits found
  NAX2MS  :  LEVEL 2 mask
  NAX2TB  :  theta bin data for X3
  JWORKB  :  sector work banks
  JX2TRK  :  track-counting work banks
  JX2SOR  :  sorted track work banks
  X2RU    :  data card
#endif
