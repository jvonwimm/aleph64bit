*CD channl
C
C  CHANNL carries the analog signals on all channels due to one
C  wire avalanche
C
C  Wire avalanche information
C  NAVELE     -- The charge from an avalanche
C  IBIN1  -- The first bin to get charge
C
      COMMON / AVALAN / NAVELE,IBIN1
C
C  Long pad coupling information
C  MPPUL   -- The array containing the coupled avalanches
C  NAMP   -- Pad 'name' generating this pulse
C            = (pad row-1)*150 + pad number
C
      PARAMETER (MXPHIT=15)
      COMMON / PADPUL / MPPUL(MXPHIT),NAMP(MXPHIT)
C
C  Trigger pad coupling information
C  MTPUL   -- The array containing the coupled avalanches
C  NAMT   -- Pad 'name' generating this pulse
C            = (tpad row# - 1)*(max # tpads/row) + tpad number
C
      PARAMETER (MXTHIT=2)
      COMMON / TRGPUL / MTPUL(MXTHIT),NAMT(MXTHIT)
