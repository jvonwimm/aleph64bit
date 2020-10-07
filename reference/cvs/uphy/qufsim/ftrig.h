      integer MinITCHits
      parameter(MinITCHits=4)              !  # ITC hits for trigger
      real LambdaECAL,LambdaHCAL
      parameter(LambdaECAL=0.65)           !  interaction length of ECAL
      parameter(LambdaHCAL=7.)             !  interaction length of HCAL
      real E_ECAL(37)                      !  Energy deposit in ECAL
      integer T_HCAL(36)                   !  # plane hits in HCAL
      integer ITCseg(48)                   !  ITC segments fired
      real LimITCseg(2,48)                 !  phi limits of ITC segments
      common / FTRIG / E_ECAL,T_HCAL,ITCseg,LimITCseg
C
