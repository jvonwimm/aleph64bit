 SUBSCHEMA VdetJULBanks
 : 'VDET banks used internally in JULIA'

 AUTHOR 'A.Bonissent,P.Rensing,D.Casper'
 REVIEWER 'M.Thulasidas'
 VERSION '8.0'
 DATE   '09/07/98'

 DEFINE ESET

 VTXT
      :      'VDET track extrapolation, NR=TRACK# in FRFT (JUL)\
              Number of words/hit\
              Number of hits per track(<=4)'
           STATIC

      = (WaferIdent   = INTE [0,20000] : 'wafer identifier',
         HitFlag      = INTE [0,1024] : 'Flag for extrapolation conditions',
         UCor         = REAL [-5.0,5.0] : 'point u wafer coordinate',
         WCor         = REAL [-5.0,5.0] : 'point w wafer coordinate',
         SigmaU       = REAL [0.0,5.0] : 'sigma on u coord',
         SigmaW       = REAL [0.0,5.0] : 'sigma on w coord',
         corrUW       = REAL [-5.0,5.0]: 'dimensionless correlation uw term',
         XCor         = REAL [-99.0,99.0]: 'x coordinate    ',
         YCor         = REAL [-99.0,99.0]: 'y coordinate    ',
         ZCor         = REAL [-99.0,99.0]: 'z coordinate',
         PVvec        = REAL [-99.0,99.0]: 'track momentum in v',
         PUvec        = REAL [-99.0,99.0]: 'track momentum in u',
         PWvec        = REAL [-99.0,99.0]: 'track momentum in w',
         dURho        = REAL [-99.0,99.0]: ' du/d(invers radius) ',
         dUTl         = REAL [-99.0,99.0]: ' du/d(Tl)         ',
         dUP0         = REAL [-99.0,99.0]: ' du/d(Phi0)       ',
         dUD0         = REAL [-99.0,99.0]: ' du/d(D0)         ',
         dUZ0         = REAL [-99.0,99.0]: ' du/d(Z0)         ',
         dWRho        = REAL [-99.0,99.0]: ' dw/d(invers radius) ',
         dWTl         = REAL [-99.0,99.0]: ' dw/d(Tl)         ',
         dWP0         = REAL [-99.0,99.0]: ' dw/d(Phi0)       ',
         dWD0         = REAL [-99.0,99.0]: ' dw/d(D0)         ',
         dWZ0         = REAL [-99.0,99.0]: ' dw/d(Z0)         ')
      ;

 VTER
      :  'VDET Track extrapolation errors,  NR=TRACK# in FRFT(JUL)\
              Number of words/matrix\
              Number of matrices (=1)'
           STATIC

      = (ErrMat(36)   = REAL [*,*]         : 'Triangular
                                              error matrix
                                              1 2 4 7 11 16 22 29
                                                3 5 8 12 17 23 30
                                                  6 9 13 18 24 31
                                                   10 14 19 25 32
                                                      15 20 26 33
                                                         21 27 34
                                                            28 35
                                                               36 ')
      ;

 VTMA
        : 'VDET multiple track/cluster association NR=0 (JUL)\
           number of words/link\
           number of possible link'
           STATIC
           SIZE 1,1

         = (NbrLayer   = INTE [1,4]          :'Nr of layer',
            NUcluster  = INTE [1,4]          :'Nbr of U clusters',
            NWcluster  = INTE [1,4]          :'Nbr of W clusters',
            Chi2       = REAL [0.0,*]        :'chi**2 of association',
            IdTrack    = INTE [1,*]          :'FRFT track number',
            FRee       = INTE [0,*]          :'free',
            UWafer(4)  = INTE [1,29999]      :'Nr of VDXY bank',
            WWafer(4)  = INTE [10000,29999]  :'Nr of VDZT bank',
            IU(4)      = INTE [0,1000]       :'U hit number   ',
            IW(4)      = INTE [0,1000]       :'W hit number   ',
            WaferId(4) = INTE [10000,29999]  :'wafer identifier',
            R0(4)      = REAL [0.0,20.0]     :'R coordinate    ',
            PHi(4)     = REAL [0.0,6.30]     :'phi coordinate  ',
            Z0(4)      = REAL [-20.0,20.0]   :'Z coordinate    ',
            UCluster(4)= REAL [-20.0,20.0]   :'U coordinate    ',
            WCluster(4)= REAL [-20.0,20.0]   :'W coordinate    ',
            SigU(4)    = REAL [0.0,*]        :'sigma of R-phi squared',
            SigW(4)    = REAL [0.0,*]        :'sigma of z squared',
            COrrel(4)  = REAL [-20.0,20.0]   :'R-phi/Z correlation')
            ;

 VTUC
        : 'VDET U cluster candidate for one track NR=0 (JUL)\
           number of words/clusterlink\
           number of cluster'
           STATIC
           SIZE 1,1

         = (WaferId(4)  = INTE [10000,29999]  :'wafer identifier',
            ClusterId(4)= INTE [10000,29999]  :'cluster identifier',
            UCluster(4) = REAL [-20.0,20.0]   :'U coordinate',
            SigmaU(4)   = REAL [0.0,*]        :'sigma of R-phi squared',
            RCoord(4)   = REAL [0.0,20.0]     :'R coordinate',
            PHi(4)      = REAL [0.0,6.30]     :'phi coordinate',
            REsidu(4)   = REAL [-20.0,20.0]   :'Residu')
            ;

 VTSC
        : 'VDET set of cluster candidates for one track (JUL) NR=0,1 for U,W\
           number of words/set\
           number of set'
           STATIC
           SIZE 1,1

         = (Chi2        = INTE [0,100000]    :'chisquare',
            ClusterId(4)= INTE [0,29999]     :'cluster identifier')
            ;


 VTWC
        : 'VDET W cluster candidate for one track NR=0 (JUL)\
           number of words/clusterlink\
           number of cluster'
           STATIC
           SIZE 1,1

         = (WaferId(4)  = INTE [10000,29999]  :'wafer identifier',
            ClusterId(4)= INTE [10000,29999]  :'cluster identifier',
            WCluster(4) = REAL [-20.0,20.0]   :'W coordinate',
            SigmaW(4)   = REAL [0.0,*]        :'sigma of W squared',
            ZC(4)       = REAL [0.0,6.30]     :'ZCi coordinate',
            REsidu(4)   = REAL [-20.0,20.0]   :'Residu')
         ;


 VGTL
      :      'TPC track MVD hit reference NR=0 (JUL)\
              Number of words/track\
              Number of trcks'
           STATIC

      = (IOff         = INTE [0,10000]     : 'offset of first hit in
                                              VGCL bank',
         NAss         = INTE [0,4]         : 'number of associated hits')

      ;


 VGCL
      :      'Mini vertex associated hits NR=0 (JUL)\
              Number of words/hit\
              Number of hits'
           STATIC

      = (NRow         = INTE [0,10000]     : 'hit number in  VDCO bank')

      ;


 VDMS
      :      'VDET multiple scattering/face extrapolation bank\
              Number of words/hit\
              Number of hits per track(<=4)'
           STATIC

      = (WaferIdent   = INTE [0,20000] : 'wafer identifier',
         FLag         = INTE [0,1024] : 'Flag for extrapolation conditions',
         RAdius       = REAL [0.0,20.0] : 'global XY radius of extrapolation',
         UCor         = REAL [-5.0,5.0] : 'point u face coordinate',
         WCor         = REAL [-5.0,5.0] : 'point w face coordinate',
         PVvec        = REAL [-99.0,99.0]: 'track direction unit vector v',
         PUvec        = REAL [-99.0,99.0]: 'track direction unit vector u',
         PWvec        = REAL [-99.0,99.0]: 'track direction unit vector w',
         CUrve        = REAL [-99.0,99.0]: 'Track inverse radius',
         SiGma        = REAL [0.0,99.0]: 'Track Multiple scattering sigma')

      ;

VCSG
      :      'VDET readout channel signal, NR=0\
              Number of words/readout channel\
              max number of readout channel in one readout module'
           STATIC

      = (RawPulse       = INTE [0,*]   : 'Raw pulse height',
         MappedPulse(3) = REAL [0.,*]  : 'Mapped pulse height',
         RawFlag        = INTE [0,*]   : 'In routine Vdecod : Raw flag',
         StripGain      = REAL [0.,*]  : 'Gain for this channel/strip',
         FullFlag       = INTE [0,*]   : 'In routine Vdecod : full channelflag\
                                          In routine VCALCM : usage flag',
         StripAddress(3)= INTE [0,*]   : 'Address of the strip to which the \
                                          readout channel is connected',
         MappedFlag(3)  = INTE [0,*]   : 'Mapped strip flag',
         CommonMode     = INTE [0,*]   : 'Common mode')
      ;

 VREG
        : 'VDET region description bank NR=readout module address(encoded)\
           number of words/region\
           number of regions'
           STATIC

         = (FirstCam      = INTE [0,*]     :'Address of first CAMEX channel',
            LastCam       = INTE [0,*]     :'Address of last CAMEX channel',
            CamexStrip(3) = INTE [1,512]   :'Strip of first CAMEX channel\
                                             in each wafer of module',
            RegionFlag(3) = INTE [0,*]     :'Region flag\
                                             in each wafer of module')
            ;

 VPEC
        : 'VDET peculiar strip description bank NR=readout module address(encoded)\
           number of words/strip\
           number of strips'
           STATIC

           SIZE 1,*                                                    
 
         = (CamAdd     = INTE [0,*]          :'Address of CAMEX channel',
            StripFlag  = INTE [0,*]          :'Strip flag')
            ;

 VGAN
        : 'VDET wafer gain bank NR=readout module address(encoded)\
           number of words/wafer\
           number of wafers'
           STATIC

           SIZE 1,108                                                    
 
         =( FirstStrip = INTE [0,*]          :'first strip with this gain',
            LastStrip  = INTE [0,100]        :'last strip with this gain',
            WafGain    = REAL [0.,10.]       :'Relative gain of the region')
            ;

 VPES
        : 'VDET hot strip list bank NR=readout module address(encoded)\
           number of words/hot strip (=2)\
           number of hot strips'
           STATIC

           SIZE 1,*
 
         = (HotAddr  = INTE [0,*]         :'Hot strip address',
            HotFlag  = INTE [0,*]         :'Hot strip flag')
            ;

 VRLC
      :      'Local hit remaining charge (bank is parallel to VDXY/VDZT)
              NR = same, view OK\
              Number of words/hit\
              Number of hits in VDXY/VDZT'
           STATIC

      = (CHarge      = INTE [0,*] : 'Remaining charge; decreased each time a track is associated')
      ;

 VRGC
      :      'Global hit remaining charge (bank is parallel to VDGC) NR = 0\
              Number of words/hit\
              Number of hits in VDGC'
           STATIC

      = (CHarge      = INTE [0,*] : 'Remaining charge; decreased each time a track is associated')
      ;

 VWRL
      :      'VDET working bank for relations, NR=0\
              Number of words/readout channel\
              max number of readout channels in one readout module'
           STATIC

      = (AssStrips(3)     = INTE [1,1024]  : 'strip number of the strip\
                                              in each wafer of the module\
                                              which is bonded to this El. channel',
         GlobalCluster    = INTE [1,1024]  : 'Index of global cluster (at module level)',
         StripPulse(3)    = REAL [0.,*]    : 'Pulseheight in the strip channel',
         ChannelPulse     = REAL [0.,*]    : 'Pulseheight in the el. channel',
         LocalCluster(3)  = INTE [1,1024]: 'Index of local cluster (at wafer level)')
      ;

 VPHN
      :  'VDET pulseheight on N strips
          (parallel to VCSG)\  
          Max cluster size+1 \
          Number rows in VCSG' 
            STATIC

      = (BestPulse     = REAL[*,*]   : 'Best pulseonN for one strip',
         PulseonN(10)  = REAL [*,*]  : 'sum pulseheight on N strips')
      ;

 VCHL
        : 'VDET Pat. Rec. component hit list (JULIA)\
           number of words/row\
           number of hits'
           STATIC
           SIZE 1,1

         = (HiT = INTE : 'Global Hit Number (VDGC index)')
            ;

 VCTL
        : 'VDET Pat. Rec. component track list (JULIA)\
           number of words/row\
           number of tracks'
           STATIC
           SIZE 1,1

         = (TRack = INTE : 'FRFT Track Number')
            ;

 VGGW
        : 'VDET Pat. Rec. work bank for combinatorics (JULIA)\
           number of words/row\
           number of VDET clusters'
           STATIC
           SIZE 1,1

         = (PulseHeight     = REAL [0.0,*] :'Hit Pulse Height',
            NAssign         = INTE [0,2]   :'Number of times assigned already',
            HitCount        = INTE [0,2]   :'Number of times assigned now',
            HitLimit        = INTE [0,2]   :'Max. assignments allowed',
            Xhit1           = INTE [0,*]   :'First hit in other view',
            Xhit2           = INTE [0,*]   :'Second hit in other view')
            ;

 VHMP
        : 'VDET Pat. Rec. hit to track map (JULIA)\
           number of words/row\
           number of tracks'
           STATIC
           SIZE 1,1

         = (TRack = INTE : 'FRFT Track Number')
            ;

 VMTC
        : 'VDET single layer hit association NR=layer number (JUL)\
           number of words/row\
           number of hits (possibly pairs of hits) possibly
           associated with this track'
           STATIC
           SIZE 1,1

         = (HitandWafer(2)  = INTE [*,*]          :'Combined hit and wafer
                                                   address',
            HitResid(2)     = REAL [*,*]          :'Hit-track residuals',
            HitErrors(2)    = REAL [*,*]          :'Hit position error',
            PulseHeight(2)  = REAL [*,*]          :'Normalized pulseheight')
            ;

 VMUC
        : 'VDET r-phi hit association NR=track number (JUL)\
           number of words/row\
           number of hit combinations possibly associated with this track'
           STATIC
           SIZE 1,1

         = (ChiPenalty      = REAL [*,*]          :'Chisquared with penalty',
            ChiKalman       = REAL [*,*]          :'Chisquared from track fit',
            HitandWafer(4)  = INTE [*,*]          :'Combined hit and wafer
                                                   address',
            PulseHeight(4)  = REAL [*,*]          :'Normalized pulseheight')
            ;

 VMWC
        : 'VDET Z hit association NR=track number (JUL)\
           number of words/row\
           number of hit combinations possibly associated with this track'
           STATIC
           SIZE 1,1

         = (ChiPenalty      = REAL [*,*]          :'Chisquared with penalty',
            ChiKalman       = REAL [*,*]          :'Chisquared from track fit',
            HitandWafer(4)  = INTE [*,*]          :'Combined hit and wafer
                                                   address',
            PulseHeight(4)  = REAL [*,*]          :'Normalized pulseheight')
            ;

 VM5C
        : 'VDET hit association NR=track number (JUL)\
           number of words/row\
           number of hit combinations possibly associated with this track'
           STATIC
           SIZE 1,1

         = (ChiPenalty      = REAL [0.0,*]  :'Chisquared with penalty',
            ChiKalman       = REAL [0.0,*]  :'Chisquared from track fit',
            HitandWafer(8)  = INTE [*,*]    :'Combined hit and wafer address',
            PulseHeight(8)  = REAL [*,*]    :'Normalized pulseheight')
            ;

 VKIL
      :      'Internal Julia banks, keeps track of suppressed global clusters\
              Number of columns \
              Number of global clusters'
           STATIC

      = (STatus       = INTE [0,1] : '0 for good; 1 for suppressed')
      ;

 VSPL
      :      'VDET split clusters (JUL), NR = same as VDXY/VDZT \
              Number of columns\
              Number of split clusters in the event'
           STATIC

      = (NH           = INTE [0,   *] : 'Row # of split cluster in VDXY/VDZT',
         FirstStrip   = INTE [1,1024] : 'beginning of cluster (wafer strib nb)',
         LastStrip    = INTE [1,1024] : 'end of cluster (wafer strib nb)',
         SharedFirst  = INTE [0,   1] : '1 if first strip is shared with an other cluster\
                                         0 otherwise',
         SharredLast  = INTE [0,   1] : '1 if last strip is shared with an other cluster\
                                         0 otherwise')
      ;

 VDCE
      :      ' VDET Chip efficiency map for MC, NR=0, roughly parallel to VDEM\
              Number of columns = number of chips+1\
              Number of rows = Number of periods'
           STATIC

           SIZE 1,*

      = ( PRoportion      = INTE [0,100]       : ' Proportion of total time /period ',
          EFficiency(768) = REAL [0.00,100.00] : 'Eff of detector in region of chip')
          ;
 

 END ESET

 END SUBSCHEMA


 SUBSCHEMA VdetPOTBanks
 : 'VDET banks written to POT,DST'

 AUTHOR 'B.Mours,HG.Moser,A.Bonissent'
 REVIEWER 'P.Rensing'
 VERSION '6.0'
 DATE   '07/02/96'

 DEFINE ESET

 VFHL
       : 'VDET final hit list bank, NR=VHLS number (POT)
          Hit address is computed as in VHLS
          packed/unpacked by VADDPK/VADDUN
          This bank is needed to unpack the VFPH,VFLG banks\
          number of words/hit\
          number of final hits'
       STATIC

        = (HitAdd      = INTE [0,*]         :'Hit address
           =Nstrip*2**18 + Ilayer*2**17 + Iwafer*2**15 + Iview*2**10 + Istrip')
           ;

 VFPH
       : 'VDET final pulse height bank, NR=VHLS number (POT)
          This bank links up the VFHL bank
          Pulseheight is stored in 250 e units\
          number of words/strip\
          number of strips'
       STATIC

        = (PuHeight    = INTE [0,*]         :'Pulseheight')
           ;

 VFLG
       : 'VDET strip flag bank, NR=VHLS number (POT)
          This bank is parallel to the VFPH bank
          Flag bits are:
           40 X        Online pulseheight overflow
           80 X        Online suppressed channel
           800 X       TPD channel in wafer of strip (0=first, 1= second)
           10000 X     100 micron bonding
           20000 X     200 micron bonding
           40000 X     unmapped channel
           80000 X     unbonded channel
           100000 X    noisy channel
           200000 X    dead channel
           400000 X    channel 50 microns AHEAD of normal
           800000 X    channel 50 microns BEHIND normal
           1000000 X    missing  channel (placekeeper)
           2000000 X   1/2 bonded strip (phi side only)
           4000000 X    Unuseable  channel
           8000000 X    Swapped  bonding
           10000000 X  Every strip bonded Z side
           20000000 X  Second phi wafer 50U ahead
           40000000 X  Second phi wafer 50U behind\
          number of words/strip\
          number of strips'
       STATIC

        = (FlaG        = INTE [0,*]         :'Strip Flag')
           ;

 VCOM
       : 'VDET common mode monitor bank, NR=VHLS number (POT)
          This bank is parallel to the VFHL bank
          storing common mode mean and sigma per hit in 250 e units
          ComMode = CM mean + ISHFT(CM sigma,16)\
          number of words/hit\
          number of hit'
       STATIC

        = (ComMode     = INTE [0,*]         :'Common mode of cluster')
           ;

 VDXY
      :      'MVD hits in r-phi wafer. (POT)
              NR=ILAYER*10000+IZED*1000+IPHI*10+(IVIEW=1)
              Bank number is packed/unpaked by VAENWA,VADEWA
              computed with values starting from 0\
              Number of words/hit\
              Number of hits per wafer'
           STATIC

      = (XCor         = REAL [-99.0000,99.0] : 'x coordinate of hit',
         YCor         = REAL [-99.0000,99.0] : 'y coordinate of hit',
         UCor         = REAL [-5.,5.00000] : 'local wafer coordinate',
         SigmaX       = REAL [0.0,5.00000] : 'sigma on x coord',
         SigmaY       = REAL [0.0,5.00000] : 'sigma on y coord',
         SigmaU       = REAL [0.0,5.00000] : 'sigma on u coord',
         PulseHeight  = REAL [0.,*]        : 'pulseheight     ',
         QualityFlag  = INTE [0,9999999]   : 'quality flag    ',
         NAss         = INTE [0,1000000]   : 'number of ass. tracks',
         IPoint       = INTE [0,1000000]   : 'pointer to first
                                              associated track in VLST',
         IWaf         = INTE [0,1000000]   : 'VFHL bank number',
         IHit         = INTE [0,1000000]   : 'Hit number in VFHL bank')
      ;

 VDZT
      :      'MVD hits in z wafer. (POT)
              NR=ILAYER*10000+IZED*1000+IPHI*10+(IVIEW=1)
              Bank number is packed/unpaked by VAENWA,VADEWA
              computed with values starting from 0\
              Number of words/hit\
              Number of hits per wafer'
           STATIC

      = (ZCor         = REAL [-99.0000,99.0] : 'z coordinate of hit',
         WCor         = REAL [-5.0000,5.0] : 'local wafer coordinate',
         SigmaZ       = REAL [0.0,5.00000] : 'sigma on x coord',
         SigmaW       = REAL [0.0,5.00000] : 'sigma on w coord',
         PulseHeight  = REAL [0.0000,9999.]: 'pulseheight     ',
         QualityFlag  = INTE [0,9999999]   : 'quality flag    ',
         NAss         = INTE [0,1000000]   : 'number of ass. tracks',
         IPoint       = INTE [0,1000000]   : 'pointer to first
                                              associated track in VLST',
         IWaf         = INTE [0,1000000]   : 'VFHL bank number',
         IHit         = INTE [0,1000000]   : 'Hit number in VFHL bank')
      ;

 VCPL
      :  'association of a track, NR=NTRACK (POT)\
              Number of words/hit\
              Number of hits'
           STATIC

      = (nrXyBank     = INTE [10000,29999] : 'Nr of VDXY bank',
         NXy          = INTE [0,1000]      : 'XY hit number',
         nrZBank      = INTE [10000,30000] : 'Nr of VDZT bank',
         NZ           = INTE [0,1000]      : 'Z hit number',
         Chi2         = REAL [0.,*]        : 'chi**2 of association')

      ;

 VDCO
      :      'VDET coordinated  NR=0 (POT)\
              Number of words/coord\
              Number of coords'
           STATIC

      = (WaferIdent   = INTE [10000,29999] : 'wafer identifier',
         R            = REAL [0.,20.00000] : 'R coordinate',
         PHi          = REAL [0.,6.3000000]: 'phi coordinate',
         Z            = REAL [-20.,20.0000]: 'Z coordinate',
         SigRphi2     = REAL [0.,*]        : 'sigma of R-phi squared',
         SigZ2        = REAL [0.,*]        : 'sigma of z squared',
         QualityFlag  = INTE [0,100000]    : 'Qualtity flag',
         TrackNumber  = INTE [0,100000]    : 'associated track in FRFT')

      ;

 VLST
      :      'track reference for associated points in VDXY,VDZT NR=0 (POT)\
              Number of words/track\
              Number of tracks'
           STATIC

      = (NTrack       = INTE [0,10000]     : 'bank number of VPRT
                                              = code of TPC/ITC track')
      ;

 VZMR
      :      'Hit multiplexing relation (bank is parallel to VDZT)\
              Number of words/hit\
              Number of hits in VDZT'
           STATIC

      = (NR                 = INTE [0,*] : 'Bank number of the companion',
         RW                 = INTE [0,*] : 'Row number of the companion')
      ;

 VDMR
      :      'Hit multiplexing relation (bank is parallel to VDXY/VDZT)
              NR = same as VDXY/VDZT + view OK\
              Number of words/hit\
              Number of hits in VDXY/VDZT'
           STATIC

      = (VD           = INTE [0,*] : 'Index of global cluster in VDGC ')
      ;

 VDGC
      :      'VDET global clusters : not multiplexed NR=0\
              Number of words/cluster\
              Number of clusters'
           STATIC

      = (MoDule         = INTE [-24,24] : 'Signed module number; sign=Z',
         VIew           = INTE [1,2]    : '1=Z; 2=rphi',
         PulseHeight    = REAL [0.,*]   : 'Total charge',
         NAss           = INTE [0,*]    : 'Number of associated tracks')
      ;

 VGHC
        : 'VDET Pat. Rec. solution per track NR=component number (POT)\
           number of words/row\
           number of tracks * number of solutions'
           STATIC
           SIZE 1,1

         = (SolutionNumber  = INTE [1,*] :'Solution Number',
            TrackNumber     = INTE [1,*] :'FRFT track number',
            Hitpair1(2)     = INTE [0,*] :'W,U Hit layer 1',
            Hitpair2(2)     = INTE [0,*] :'2nd possible W,U Hit layer 1',
            Hitpair3(2)     = INTE [0,*] :'W,U Hit layer 2',
            Hitpair4(2)     = INTE [0,*] :'2nd possible W,U Hit layer 2')
            ;

 VGXC
        : 'VDET Pat. Rec. solution per component NR=component number (POT)\
           number of words/row\
           number of ambiguous solutions'
           STATIC
           SIZE 1,1

         = (CHi2            = REAL [0.0,*] :'Chisquared of solution',
            NTrack          = INTE [1,*]   :'Number of tracks',
            SolNused        = INTE [1,*]   :'Solution number used')
            ;


 END ESET

 END SUBSCHEMA


 SUBSCHEMA VdetGALBanks
 : 'VDET banks produced by GALEPH'

 AUTHOR 'A.Bonissent,G.Taylor'
 REVIEWER 'A.Bonissent'
 VERSION '5.1'
 DATE   '22/8/95'

 DEFINE ESET

 VDSS
      :      'VDet Silicon Segment NR=0 (GAL)\
              Number of words/hit\
              Number of hits'
           STATIC

      = (TrackNumber   = INTE [1,999]   :'Galeph track number',
         LayerNumber   = INTE           :'Layer number',
         PhiNumber     = INTE           :'phi coordinate',
         XEntry        = REAL           :'X-entry point',
         YEntry        = REAL           :'Y-entry point',
         ZEntry        = REAL           :'Z-entry point',
         XLast         = REAL           :'X-exit point',
         YLast         = REAL           :'Y-exit point',
         ZLast         = REAL           :'Z-exit point',
         EnergyReleased= REAL           :'Energy released',
         RowNumber     = INTE           :'Row number of hit in VDHT (=0 means noise or delta ray)',
         ExitSilicon   = INTE           :'Flag set non-zero when a track exits the active volume')
       ;


 VDHT
      :      'VDet HiT list NR=0 (GAL)\
              Number of words/hit\
              Number of hits'
           STATIC

      = (TrackNumber   = INTE [1,999]   :'Galeph track number',
         LayerNumber   = INTE           :'Layer number',
         PhiNumber     = INTE           :'phi coordinate',
         XEntry        = REAL           :'X-entry point',
         YEntry        = REAL           :'Y-entry point',
         ZEntry        = REAL           :'Z-entry point',
         XLast         = REAL           :'X-exit point',
         YLast         = REAL           :'Y-exit point',
         ZLast         = REAL           :'Z-exit point',
         EnergyReleased= REAL           :'Energy released')
       ;


 VDFK
      :      'Vdco to FKIN truth relation (monte Carlo)
              (hits associated to reconstructed tracks)\
              Number of words/relation\
              Number of relations '
           STATIC

      = (PercentCharge(2) = REAL [0.,2.00] : 'Fraction of the cluster charge\
                                          due to this track for the 2 views',
         StripsCount(2)   = INTE [1,*]    : 'Number of strips in this cluster\
                                          fired by this track for the 2 views')
      ;

 VUFK
      :      'Vdxy/vdzt to FKIN truth relation (monte Carlo)
              (hits unused by pattern reconstruction)\
              Number of words/relation\
              Number of relations '
           STATIC

      = (BankNumber      = INTE [0,29999] : 'VDXY or VDZT bank number',
         HitNumber       = INTE [0,*] : 'VDXY or VDZT hit number',
         View             = INTE [1,2] : 'View',
         PercentCharge    = REAL [0.,1.00] : 'Fraction of the cluster charge\
                                          due to this track',
         StripsCount      = INTE [1,*]    : 'Number of strips in this cluster\
                                          fired by this track')
      ;

 VDLH
      :      'Local hits : intercept between monte carlo tracks and Vdet
              wafers (local coords.)\
              Number of words/hit\
              Number of hits '
           STATIC

      = (Xin(3)       = REAL [0,10.0000] : 'Entry point',
         Xout(3)      = REAL [0,10.0000] : 'Exit point',
         Module       = INTE [-25,25]    : 'Module number \
                                            1 to 9 : first layer;\
                                            10 to 24 : second layer\
                                            sign = sign of Z coord.',
         Wafer        = INTE [1,3]       : 'Wafer number \
                                            number 1 is closest to xy plane',
         Code         = INTE [-50000,50000] : 'Encoded wafer address\
                                              1000*IMOD+ISIGN(IWAF,IMOD)',
         Ereleased    = REAL [0.,1.000000] : 'Released energy',
         Track        = INTE [1,1000]    : 'Kinematic track #')
      ;

 VTRS
      :      'Monte carlo truth : tracks to strips relation\
              Number of words/relation\
              Number of relations '
           STATIC

      = (Address      = INTE [1,*]       : 'Strip address, encoded by routine VAENCL',
         Charge       = REAL [0.,10000.] : 'Charge deposited by this track in\
                                            this strip')
      ;

 VDTE
      :      'Monte Carlo track element\
              Number of words/track element\
              Number of track elements '
           STATIC

      = (Xbar(3)       = REAL [0,10.0000] : 'barycenter coordinates (local ref.)',
         Relene       = REAL [0.,10000.] : 'Energy released in the small track\
                                            element')
      ;

 VWS1
      :      'Wafer strips array (view 1)
              VWS1 and VWS2 are identical (same HAC parameters) \
              Number of words/strip\
              Number of strips '
           STATIC

      = (StripContent  = REAL [1,45.] : 'Signal in one strip(Gev)')
      ;

 VWS2
      :      'Wafer strips array (view 2)
              VWS1 and VWS2 are identical (same HAC parameters) \
              Number of words/strip\
              Number of strips '
           STATIC

      = (StripContent  = REAL [1,45.] : 'Signal in one strip(Gev)')
      ;

 VWC1
      :      'Wafer strips array (view 1)
              VWC1 and VWC2 are identical (same HAC parameters) \
              Number of words/strip\
              Number of strips '
           STATIC

      = (ChannelContent  = REAL [1,45.] : 'Signal in one r/o channel(Gev)',
         SeriesNoise     = REAL [0.,1.]  : 'Parallel noise for this channel\
                                            (Gev)')
      ;

 VWC2
      :      'Wafer strips array (view 2)
              VWC1 and VWC2 are identical (same HAC parameters) \
              Number of words/strip\
              Number of strips '
           STATIC

      = (ChannelContent  = REAL [1,45.] : 'Signal in one r/o channel(Gev)',
         ParallelNoise   = REAL [0.,1.]  : 'Parallel noise for this channel\
                                            (Gev)')
      ;

 VCLU
      :      'Vdet clusters (Monte Carlo)\
              Number of words/cluster\
              Number of clusters '
           STATIC

      = (View  = INTE [1,2] : 'view 1 = Z; 2 = R phi',
         FirstStrip   = INTE [1,1000]  : 'numero of the first strip in the cluster',
         ClusterSize  = INTE [1,1000]  : 'Number of strips in the cluster')
      ;

 VTSK
      :      'Vdet truth strip to track relation (Monte Carlo)\
              Number of words/relations\
              Number of relations '
           STATIC

      = (Charge = REAL [0.,45.] : 'Charge deposited by this track in this strip (Gev)')
      ;

 VTCK
      :      'Vdet truth readout channel to track relation (Monte Carlo)\
              Number of words/relations\
              Number of relations '
           STATIC

      = (Charge = REAL [0.,45.] : 'Charge deposited by this track in this channel (Gev)')
      ;


 END ESET
 DEFINE RSET

   (VDLH [1,1] -> [1,1] VDHT BY HT)
     : 'Index of VDHT for this hit';

   (VWS1 [0,1] -> [1,1] VTSK)
     : 'Index of first relation to track for this strip';
 
   (VWS2 [0,1] -> [1,1] VTSK)
     : 'Index of first relation to track for this strip';
 
   (VTSK [1,1] -> [1,1] VDHT BY HT)
     : 'Index of VDHT for this relation';

   (VTRS [1,1] -> [1,1] VDHT BY HT)
     : 'Index or row in VDHT bank';

   (VTRS [1,1] -> [0,1] VTRS)
     : 'Index of next relation for same track';

   (VTSK [1,1] -> [0,1] VTSK)
     : 'Index of next relation for same track';
 
   (VWC1 [0,1] -> [1,1] VTCK)
     : 'Index of first relation to track for this readout channel';
 
   (VWC2 [0,1] -> [1,1] VTCK)
     : 'Index of first relation to track for this readout channel';
 
   (VTCK [1,1] -> [1,1] KINE)
     : 'Index of track for this relation';

   (VTCK [1,1] -> [0,1] VTCK)
     : 'Index of next relation for same track';
 
   (VDFK [1,1] -> [0,1] FKIN)
     : 'Index of FKIN track';
 
   (VUFK [1,1] -> [0,1] FKIN)
     : 'Index of FKIN track';
 
   (VDFK [1,1] -> [0,1] VDCO)
     : 'Index of Vdet cluster in VDCO bank';
 

 END RSET

 END SUBSCHEMA


 SUBSCHEMA VdetEVEntBanks
 : 'VDET event record banks written to raw data'

 AUTHOR 'HG.Moser'
 REVIEWER ''
 VERSION '4.4'
 DATE   '21/5/91'

 DEFINE ESET


 VHLS
      :      'Vdet Hit LiSt NR=module code (RAW)\
              Number of words/hit\
              Number of coords'
           STATIC

      = (HCode    = INTE [0,*]    : 'Hit code of first strip of cluster')

      ;

 VPLH
      :      'Vdet PuLse Hight NR=module code (RAW)\
              Number of words/hit\
              Number of coords'
           STATIC

      = (PCode    = INTE [0,*]  : 'Pulse Code of two pulses')

      ;

 VOCM
       : 'VDET online common mode monitor bank, NR=module code (RAW)
          storing common mode mean and sigma per line driver
          ComMode = CM sigma + ISHFT(CM mean,16)\
          number of words/line driver=2\
          number of line drivers/module=6'
       STATIC

       = (HCode      = INTE [0,*]    : 'Hit code of first channel of
                                        line driver',
          ComMode    = INTE [0,*]    :'Common mode of line driver')
           ;

 VRRR
      :      'MVD online error bank (RAW)
              NR=FASTBUS crate number (F8 or F9)\
              Number of words/row \
              Number of errors'
           STATIC

      = (MCode        = INTE [*,*] : ' MCODE = \
                                       ILAYER*64 + IZED*16 + IPHI\
                                       or (hex) FFFFFFFF (general)',
         ErrCode      = INTE [0,255] : 'error code')
      ;

 VEVH
      :      'MVD online event header (RAW)
              NR=FASTBUS crate number (F8 or F9)'
           STATIC

      = (EvCount      = INTE [0,*] : 'internal event count',
         AcNumber     = INTE [0,255] : 'internal account number',
         ERrcount     = INTE [0,*] : 'error count',
         ErrFlag      = INTE [*,*] : 'error flag (0= ok)',
         Res          = INTE [*,*] : 'reserved')
      ;

 END ESET

 END SUBSCHEMA

 SUBSCHEMA VdetRUNBanks
 : 'VDET run record banks written to raw data'

 AUTHOR 'S.Menary,H.Seywerd'
 REVIEWER ''
 VERSION '5.0'
 DATE   '23/10/95'

 DEFINE ESET

 VDRO
      :      'VDet ReadOut run header bank (1990) NR=0 (RUN)\
              Number of columns\
              Number of rows =1'
           STATIC

      = (FLag         = INTE [0,1]          : '0/1 if tpd_route was not/was
                                                   read without error',
         NumTPDs      = INTE [0,10]         : 'number of TPDs in readout',
         TPDslots(2)  = INTE [0,134217728]  : 'packed word of TPD slot addresses',
         CHannels(20) = INTE [0,134217728]  : 'packed TPD channel maps
                                                    -- 1 bit (on/off) per channel',
         FAces(8)     = INTE [0,134217728]  : 'packed word describing which
                                                               faces were readout',
         WAfers(25)   = INTE [0,134217728]  : 'packed wafer maps
                                                    -- 1 bit (on/off) per wafer')
      ;

 VORD
      :      'Vdet Online ReaDout run header bank (1991) NR=0 (RUN)
              Used in raw data from 1991 up to 1995 end of VDET I \
              Number of columns=11\
              Number of rows = NumModsReadout'
           STATIC

      = (MCode        = INTE [0,126]         : 'MCODE of the module =\
                                                ILAYER*64 + IZED*16 + IPHI',
         DacValue     = INTE [0,1000000]     : 'VDSP.DACValue in VMDB',
         OverFlow     = INTE [0,1000000]     : 'VDSP.Overflow in VMDB',
         Threshold0   = INTE [0,1000000]     : 'Threshold for T1',
         Threshold1   = INTE [0,1000000]     : 'Threshold for T2',
         Threshold2   = INTE [0,1000000]     : 'Threshold for Phi1',
         Threshold3   = INTE [0,1000000]     : 'Threshold for Phi2',
         Threshold4   = INTE [0,1000000]     : 'Threshold for Z1',
         Threshold5   = INTE [0,1000000]     : 'Threshold for Z2',
         Threshold6   = INTE [0,1000000]     : 'Threshold for Z3',
         Threshold7   = INTE [0,1000000]     : 'Threshold for Z4')
      ;

 VOFL
      :      'Vdet Online FiLes run header bank NR=0 (RUN)\
              Number of columns\
              Number of rows =1'
           STATIC

      = (PedFileused       = CH64           : 'Name of the pedestal file used',
         MvdrcuFillingfile = CH64           : 'MVDRCU filling file used',
         DsploadFile       = CH64           : 'File used containing DSP code ',
         NumModsreadout    = INTE [0,48]    : 'Number of modules in readout')
      ;

  VORP

 :      'Vdet Online Readout Parameters run header bank (1995) 
        VDET II, Sircco Parameters, thresholds overflow, etc\
        Number of columns \
        Number of rows = NumModsReadout'
      STATIC

      = ( McoDe       = INTE      : 'Module Code',
          DAcvalue    = INTE      : 'DAC Value loaded for this mod',
          OVerflow    = INTE      : 'Overflow value used online',
          threshPhi1 = INTE      : 'Phi threshold for MX 1',
          threshPhi2 = INTE      : 'Phi threshold for MX 2',
          threshPhi3 = INTE      : 'Phi threshold for MX 3',
          threshPhi4 = INTE      : 'Phi threshold for MX 4',
          threshPhi5 = INTE      : 'Phi threshold for MX 5',
          threshPhi6 = INTE      : 'Phi threshold for MX 6',
          threshPhi7 = INTE      : 'Phi threshold for MX 7',
          threshPhi8 = INTE      : 'Phi threshold for MX 8',
          threshZ1   = INTE      : 'Z threshold for MX 1',
          threshZ2   = INTE      : 'Z threshold for MX 2',
          threshZ3   = INTE      : 'Z threshold for MX 3',
          threshZ4   = INTE      : 'Z threshold for MX 4',
          threshZ5   = INTE      : 'Z threshold for MX 5',
          threshZ6   = INTE      : 'Z threshold for MX 6',
          threshZ7   = INTE      : 'Z threshold for MX 7',
          threshZ8   = INTE      : 'Z threshold for MX 8')
       ;

 END ESET

 END SUBSCHEMA


 SUBSCHEMA VdetONLBanks
 : 'VDET banks internal to the online system'

 AUTHOR 'HG.Moser,J.Rothberg'
 REVIEWER 'J.Rothberg'
 VERSION '5.1'
 DATE   '09/10/95'

 DEFINE ESET

 VFEB
      :      'MVD FEB raw data (ONL)
              NR=ILAYER*64 + IZED*16 + IPHI
              computed with values starting from 0\
              Number of words/channel\
              Number of channels (=2048)'
           STATIC

      = (RawFeb      = INTE [0,*] : 'multiplexed FEB data')
      ;

 VOPD
      :      'MVD online pedestals (ONL)
              NR=ILAYER*64 + IZED*16 + IPHI
              computed with values starting from 0\
              Number of words/row\
              Number of hits rows (=2048)'
           STATIC

      = (PEdestal      = INTE [*,*] : 'pedestals')
      ;

 VONS
      :      'MVD online noise (ONL)
              NR=ILAYER*64 + IZED*16 + IPHI
              computed with values starting from 0\
              Number of words/row\
              Number of rows (=2048)'
           STATIC

      = (NOise      = INTE [*,*] : 'variances on pedestals')
      ;

 VTRA
      :      'MVD sirocco trailor bank (ONL)
              NR=0\
              Number of words/row \
              Number of rows (=1)'
           STATIC

      = (ResA         = INTE [*,*] : 'reserved',
         ResB         = INTE [*,*] : 'reserved',
         ResC         = INTE [*,*] : 'reserved',
         NCluster     = INTE [0,*] : 'number of clusters found',
         NStrips      = INTE [0,*] : 'number of strips read',
         NWords       = INTE [0,*] : 'number of words',
         EvCount      = INTE [0,*] : 'internal event count',
         AccNum       = INTE [0,255] : 'account number')
      ;



 VOVC
      :      'MVD online common mode covariance matrix (ONL)
              NR=ILAYER*64 + IZED*16 + IPHI
              computed with values starting from 0\
              Number of words/row\
              Number of hits rows (=36)'
           STATIC

      = (ComCov      = INTE [*,*] : 'common mode covariance matrix\
                                     in the order:\
                                     1   9  10  12  15  19  24  30\
                                         2  11  13  16  20  25  31\
                                             3  14  17  21  26  32\
                                                 4  18  22  27  33\
                                                     5  23  28  34\
                                                         6  29  35\
                                                             7  36\
                                                                 8')

      ;

 VDSP
      :      'DSP constants to be downloaded (ONL)
              NR=ILAYER*64 + IZED*16 + IPHI'

           STATIC

      = (BaseAddr      = INTE [*,*] : 'Sirocco address to load the
                                       following',
         VerNum        = INTE [0,*] : 'version number of DSP program',
         DspnNum       = INTE [0,*] : 'DSP number = bank number',
         DacVal        = INTE [0,*] : 'DAC value to be loaded in DSP',
         OverFlow      = INTE [0,*] : 'ADC overflow value',
         ModLabel(8)   = INTE [*,*] : '8 module labels',
         ModStart(8)   = INTE [*,*] : '8 module start addresses in FEB',
         ModThresh(8)  = INTE [*,*] : '8 module threshold values',
         ComHist       = INTE [0,*] : 'lower edge of common mode histogram',
         ComBin        = INTE [0,*] : 'bin width of Common Mode histogram',
         CombiT        = INTE [0,*] : 'bit number corresponding to bin')
      ;

  VMBE
      :      'VDET Module Bonding Errors, NR=Module Serial number\
              Number of words/fault\
              Number of faults'

           STATIC

      = (ADdress    = INTE [0,*] : 'Encoded address \
                                    =  (View-1) * 10^9    (view 1 = Z, 2 = r-phi -> 0 and 1)\
                                        +  Bond nb * 10^8\
                                        + Readout channel nb start * 10000\
                                        + Readout channel nb stop \
                                        Readout channels are between 0 and 1023',
         FaultCode  = INTE[0,*]  : 'Description of bonding error \
                                    = 1000000*parameter (optional) + fault code\
                 code = 0   No fault , strip is OK. Comment may be meaningful\
                        1   Saturated\
                        2   Dead\
                        3   Noisy/hot\
                        4   Pinhole disconnected\
                        5   Pinhole alive\
                        6   Shorted  (nb stop contains the addresses of other strip)\
                        7   Unbonded\
                        8   Dislocation +n (in units of 50 microns)\
                        9   Dislocation -n\
                       10   Dislocation odd +n\
                       11   Dislocation odd -n\
                       12   Dislocation even +n\
                       13   Dislocation even -n\
                            Dislocation sign is sign of : \
                         (Readout channel to which the strip should be attached)\
                       - (readout channel to which the strip is actually attached)\
                       14   Two strips connected to same channel\
                       15   Superceded, ignore\
                       100+code  Added at CERN\
                      bond numbers :\
                     view 1 : bond 1 between capacitance and first kapton\
                          bond 2 between first kapton and first Si strip\
                          bond 3 between first Si strip and second kapton strip\
                          bond 4 between second kapton strip and second Si strip\
                     view 2 : bond 1 between capacitance and first wafer strip\
                          bond 2 between wafers 1 and 2\
                          bond 3 between wafers 2 and 3')
      ;

  VMUN
      :      'VDET UNconnected channels, NR=setup code\
              Number of columns \
              Number of rows '

           STATIC
 
      = (FirstChan    = INTE [*,*]         : 'First Data Channel',
         LastChan     = INTE [*,*]         : 'Last Data Channel',
         RegionFlag   = INTE [*,*]         : 'Region Flag')
       ;

 VREJ
       :     'VDET Region, NR=module,view\
              Number of columns \
              Number of rows '

           STATIC
  
       = (FIrstchan    = INTE [*,*]         : 'First Data Channel',
          LAstchan     = INTE [*,*]         : 'Last Data Channel',
          ReGionFlag   = INTE [*,*]         : 'Region Flag')
       ;

 VPCH
       :     'VDET Peculiar channels, NR=module,view\
              Number of columns \
              Number of rows '

           STATIC

       = (DataChan     = INTE [*,*]         : 'Data Channel',
          ChannelFlag  = INTE [*,*]         : 'Channel Flag')
       ;

 VSBD
       :     'VDET Bad Channels, Stripmon, NR=setup code\
              Number of columns \
              Number of rows '

           STATIC

       = (CHannel      = INTE [*,*]         : 'Data Channel',
          ChannelFlag  = INTE [*,*]         : 'Channel Flag')
       ;

 VMBU
       :     'VDET Unpacked Bonding errors, NR=Module serial No.\
              Number of columns \
              Number of rows '

           STATIC

       = (VieW        = INTE [1,2]         : 'View, z=1, rphi=2',
          FIrstchan   = INTE [0,1023]      : ' First Channel ',
          LastChan    = INTE [0,1023]      : ' Last Channel ',
          BondNumb    = INTE [1,4]         : ' Bond Number  ',
          FauLt       = INTE [*,*]         : ' Fault ',
          PArameter   = INTE [*,*]         : ' Parameter ')
       ;

 VCUT
       :     'VDET pedestal,noise cuts by MX chip;stripmon, NR=cut setup code\
              Number of columns \
              Number of rows=48x2x8=768 '

           STATIC

       = (MCode       = INTE [0,*]         : ' Module, view code',
          MXchipno    = INTE [1,8]         : ' MX7 chip number ',
          LowPed      = INTE [0,4095]      : ' Low Pedestal cut ',
          HighPed     = INTE [0,4095]      : ' High Pedestal cut  ',
          LowNoise    = INTE [0,4095]      : ' Low Noise cut ',
          HighNois    = INTE [0,4095]      : ' High Noise cut ')
        ;


 END ESET

 END SUBSCHEMA


