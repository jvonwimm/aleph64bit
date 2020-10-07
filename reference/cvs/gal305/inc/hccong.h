*CD hccong
      COMMON /HCCONG/ HCTUAC,HCSTDT,HCADCE,HADCMX,HCPFAC,
     &                HCTINS,RHBAMN,ZHECMN,ZHBAMX,HSTREA,HSTUST,
     +                NHCFSS,HCFSS1,HCFSS2,HCFLSS(100)
     &               ,HTLEMX,HCTEFF(3),HPINDU
C
#if defined(DOC)
C!    Hcal Streamer Costants (used only in Galeph)
        HCTUAC  sensitive zone size in tube (.9 cm)
        HCSTDT  obscuration length of streamer  (0.34 cm)
        HCADCE  conversion factor from #ADC channel to energy (GeV) (.00
        HADCMX  maximum #ADC permitted (400)
        NHCFSS  # of bins in exper. distr. of single streamer (100)
        HCFSS1  low edge of single streamer distribution (0.)
        HCFSS2  upper edge of single streamer distribution   (10.)
        HCFLSS  conten of bins of single streamer distribution
        HCPFAC  scale factor
        HCTINS  thickness of insensitive medium in sensitive gap
        RHBAMN  average minimum radius of barrel
        ZHECMN  average minimum Z of endcap
        ZHBAMX  average maximum Z of barrel
        HSTREA  total thickness of streamer tube layer
        HSTUST  = HCTUAC/HSTREA
        HTLEMX  maximim length of a track element ( HCCOIN)
        HDEADT  mim length of the y-proj. trk ele. to have inefficiency
        HCTEFF  tube efficiency
        HPINDU  induction factor
#endif
