*CD satrco
      COMMON/SATRCO/NSECSA,NCRASA,NCARSA,NTDCSA,NSIDSA,NLAYSA,NBRASA,
     .      RMINSA,RMAXSA,BRTHSA,ZLUMSA,DEADSA,DGASSA,COSTSA,
     .      NHITSA,NLOSSA,NWIRSA,XHITSA,XLOSSA,XWIRSA,XEVTSA,
     .      NHLASA(9),NHLDSA(9),PHIBSA(9)
C
#if defined(DOC)
      NHITSA hits per event
      NLOSSA lost hits in dead regions per event
      NHLASA hit multiplicity per layer (including double hits)
      NHLDSA wire cells fired per layer (excluding double hits)
      XHITSA .. are the corresponding number summed up to get
                over all XEVTSA events which hit the lumi monitor
                used to determine the averages for SAWSUM
#endif
