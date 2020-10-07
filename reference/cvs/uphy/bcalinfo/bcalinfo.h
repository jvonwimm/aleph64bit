      common /BCINFO/ KNBCR,KFBCR,KLBCR,KNBCT,KFBCT,KLBCT
      common /BCRECO/ NbR,IMbR(4),ENbR(4),THbR(4),PHbR(4)
      common /BCTRUE/ NbT,IMbT(4),ENbT(4),THbT(4),PHbT(4)
      common /BCBANK/ MskTrg,ModTrg(4),Gain_PMT(4),Gain_APD(4)
      common /BCRAWC/ BErawAPD(4),BErawPMT(4),BRcen(4),BPcen(4) 
      common /BCHIGS/ Sch_PMT(4),Sch_APD(4)
      common /BCCALI/ PMT_lumitofb(4),APD_lumitofb(4), 
     .                PMT_fbtosc(4),APD_fbtosc(4)	
