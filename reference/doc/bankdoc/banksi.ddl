 SUBSCHEMA ScalJULBanks
 : 'Description of JULIA Sical banks'
 
 AUTHOR   'B.Bloch-Devaux '
 REVIEWER '           '
 VERSION  '3.2'
 DATE     '12/04/99'
 
 DEFINE ESET
 
  SPDA
      :      'Sical Pad DAta after preparation NR=0.(JUL)\
              Number of words/hit pad\
              Number of hit pads'
           STATIC
 
      = (Energy       = REAL [-1.,90.]     : 'Energy in Gev',
         ADdress      = INTE [1,12288]     : 'Add  :encoded i,j,k,n address\
                                             bit 0-1 = triplet index 0 to 3\
                                             bit 2-5 = radius index 0 to 15\
                                             bit 6-10= Phi index 0 to 31\
                                             bit 11  =  Module index 0=A 1=B\
                                             bit 12-13= Z index inside triplet 0 to 2\
                                             then 1 unit is added to get address in range',
         ERaw         = REAL [-1.,90.]     : 'Raw energy of pad before calibration',
         OVerflow     = INTE [0,1]         : 'Overflow bit')
 
     ;
 
  SMPD
     :      'Sical Missing PaDs NR=0. (JUL)   \
             Number of words/pad\
             Number of dead pads'
            STATIC
 
      = (ADdress      = INTE [1,12288]     : 'Add  :encoded i,j,k,n address\
                                             bit 0-1 = triplet index 0 to 3\
                                             bit 2-5 = radius index 0 to 15\
                                             bit 6-10= Phi index 0 to 31\
                                             bit 11  =  Module index 0=A 1=B\
                                             bit 12-13= Z index inside triplet 0 to 2\
                                             then 1 unit is added to get address in range')
  ;
 
 SCLS
      :      'Sical CLuSters NR=0. (JUL)\
              Number of words/cluster\
              Number of clusters'
           STATIC
 
      = (EnergyCluster= REAL [0.,100.000]  : 'Cluster energy',
         RadiusCluster= REAL [5.,15.000]   : 'Average cluster radius (polar coordinates)',
         ZedCluster   = REAL [200.,262.000]: 'Average cluster z ',
         PhiCluster   = REAL [0.,6.3]      : 'Average cluster phi (polar coordinates)',
         ThetaCluster = REAL [0.,6.3]      : 'Average cluster theta (polar coordinates)',
         WidthPhi     = REAL [0.,999.]     : 'Cluster phi width ',
         WidthRadial  = REAL [0.,999.]     : 'Cluster radial width ',
         EnergyRaw    = REAL [0.,999.]     : 'Cluster raw energy before calibration',
         CorrRadius   = REAL [5.000,*]     : 'Corrected cluster radius',
         CorrPhi      = REAL [0.000,6.3]   : 'Corrected cluster phi',
         CorrTheta    = REAL [0.000,6.3]   : 'Corrected cluster theta',
         Rsums3(3)     = INTE [1,16]       : '3 Radius bins of highest sums over 2 stacks',
         Esums1(5)     = REAL [0.000,*]    : 'Energy sums in 5 zplane doublets in low Rbin',
         Esums2(5)     = REAL [0.000,*]    : 'Energy sums in 5 zplane doublets in mid Rbin',
         Esums3(5)     = REAL [0.000,*]    : 'Energy sums in 5 zplane doublets in high Rbin',
         OVerflownum  = INTE [0,*]         : 'Number of overflow pads')
      ;
 
 SMAP
      :      'Sical pad map\
              Number of words/pad\
              Number of pads'
           STATIC
      ;

 SIID
     :       'SIcal cluster IDentifier\
              Number of words/cluster\
              Number of clusters'
           STATIC

      = (ClusterIdent = INTE [-1,100]       : 'Cluster identifier\  
                                               -2= not enough info\
                                               -1= not a Sical cluster\
                                                0= good cluster\
                                                1= electronic noise\
                                                2= beam loss\
                                                3= backward particle\
                                                4= scraping particle\
                                                5= off-momentum particle',
         PECO         = INTE [1,100]        : 'Cluster Index in the corresponding PECO bank',
         SCLS         = INTE [1,100]        : 'Cluster Index in the corresponding SCLS bank',
         SILU         = INTE [1,2]          : 'Cluster Index in the corresponding SILU bank')
      ;

 END ESET
 
 DEFINE RSET
 
   ( SPDA [0,1] -> [1,*] SCLS)
     : 'Index of cluster to which the pad belongs';
 
   ( SPDA [0,1] -> [0,1] SMPD)
     : 'Index of dead pad in same position';
 
 
   ( SPDA [0,1] -> [0,1] SPDA)
     : 'Index of the Next Storey of the same cluster';
 
   ( SCLS [0,1] -> [0,1]  SPDA)
     : 'Index of the first pad in a cluster ';
 
    ( SMAP [0,1] -> [1,1] SPDA)
             : 'Index of hit pad';
 
 END RSET
 
 END SUBSCHEMA


 SUBSCHEMA ScalGALBanks
 
  : ' Scal banks used in GALEPH only'
 
 AUTHOR   'B.Bloch-Devaux'
 REVIEWER '         '
 VERSION  '1.0'
 DATE     '23/10/91'
 
 
 DEFINE ESET
 
 
 SIHT
             : 'Galeph SCAL pad hits
               NR=0.(GAL)\
                number of words/triplet\
                number of triplets'
            STATIC
             = (ADdress           =       INTE    : 'triplet ADdress ',
                E1                =       INTE    : 'Energy in plane 1',
                E2                =       INTE    : 'Energy in plane 2',
                E3                =       INTE    : 'Energy in plane 3')
            ;
 
 
 
 SIHI
      :      'Galeph SCAL pad hits HIstory bank
               NR=0.(GAL)\
              Number of words per prim track energy deposit \
              Number of prim tracks energy deposits'
           STATIC
 
    = (PrimaryTrack    = INTE [1,999]    : 'primary track number ',
       TripletAddress  = INTE [0,2047]   : 'triplet coded address ',
       DepositEnerg(3) = INTE [0,100000] : 'energy (Mev) deposited by the \
                                            primary track in the 3 triplet planes')
      ;
 
 
  END ESET
 
END SUBSCHEMA


 SUBSCHEMA ScalRAWBanks
 :'SCAL Detector RAW DATA banks Description'
 
 AUTHOR   'B.Bloch-Devaux'
 REVIEWER '   '
 VERSION  '1.3'
 DATE     '6/06/95'
 
 DEFINE ESET
 
 
 SIDI
      :      'SIcal triplet DIgits
              NR=0. (RAW)\
              Number of words/triplet \
              Number of triplet'
           STATIC
 
      = (ADdress      = INTE              : 'triplet address in  bit 0-15 \
                                             bit 0-1 = triplet index 0 to 3\
                                             bit 2-5 = radius index 0 to 15\
                                             bit 6-10= Phi index 0 to 31\
                                             bit 11  =  Module index 0=A 1=B\
                                             bit 12-14= Ovflow bit in info1-2-3\
                                             bit 15 = unused',
         Energy1      = INTE              : 'Energy of triplet plane 1 [Mev]',
         Energy2      = INTE              : 'Energy of triplet plane 2 [Mev]',
         Energy3      = INTE              : 'Energy of triplet plane 3 [Mev]')
      ;
 
 SIFO
      :      'SIcal Fast Or info (Amplex sums)
              NR=0. (RAW)\
              Number of words/triplet \
              Number of triplet'
           STATIC
 
      = (ADdress      = INTE              : 'triplet address in  bit 0-15 \
                                             bit 0-1 = triplet index 0 to 3\
                                             bit 2-5 = not used \
                                             bit 6-10= Phi index 0 to 31\
                                             bit 11  =  Module index 0=A 1=B\
                                             bit 12-14= Ovflow bit in info1-2-3\
                                             bit 15 = unused',
         Adccount1    = INTE              : 'ADC count of triplet plane 1',
         Adccount2    = INTE              : 'ADC count of triplet plane 2',
         Adccount3    = INTE              : 'ADC count of triplet plane 3')
      ;
 
 SIX2
 
     :       'SIcal Trigger X2 bank
              NR=0. (RAW)\
              Words per row \
              Number of rows'
     STATIC
     =     (TriggerWord  = INTE : 'Trigger Word \
                                   (bit 0 = A_high_B_low\
                                    bit 1 = A_low_B_high\
                                    bit 2 = A_very_high\
                                    bit 3 = B_very_high\
                                    bit 4 = A_very_low\
                                    bit 5 = B_very_low,\
                                    bit 6-9 = End-A odd thresh 1-4\
                                    bit 10-13 = End-A even thresh 1-4\
                                    bit 14-17 = End-B odd thresh 1-4\
                                    bit 18-21 = End-B even thresh 1-4\
                                    bit 22 = A_very_high_B_low\
                                    bit 23 = B_very_high_A_low\
                                    bits 24-31 reserved')
      ;                                
 
 SIXA
 
      :      'SIcal Trigger sectors Adc bank
              (RAW)\
              Words per trigger sector   \
              Number of trigger sectors '
      STATIC
      = (AdcOddlayers         = INTE     : 'Trigger ADC value, odd layers\
                                             bit 0-15 road 1, bit 16-31 road 2',
         AdcEvenlayers        = INTE     : 'Trigger ADC value, even layers\
                                             bit 0-15 road 1, bit 16-31 road 2')
      ;
 
 SCHU
 
      :      'Sical Bunch train (CHU chu chu....) identifier
              (RAW)\
              Words per identifier       \
              Number of identifiers'
      STATIC
      = (WagonIdent         = INTE[0 ,*]     : 'Wagon time in ADC counts')
      ;
 
 END ESET
 
 END SUBSCHEMA



 SUBSCHEMA ScalSLOWControl
 :'SCAL Detector SLOW control banks Description'
 
 AUTHOR   'B.Bloch-Devaux'
 REVIEWER '   '
 VERSION  '1.0'
 DATE     '22/05/92'
 
 DEFINE ESET
 
 
 SIPO
      :      'SIcal POsition probes info ,NR=0. (SLOW)\
              Number of words/probe\
              Number of probes '
            STATIC
 
      = (ProbeNum     = INTE           : 'probe number',
         XXpos        = REAL           : 'X position from probe  [unit]',
         YYpos        = REAL           : 'Y position from probe  [unit]')
      ;
 
 END ESET
 
 END SUBSCHEMA


 SUBSCHEMA ScalLUMI
 : 'Description of Scal LUMINOSITY info  '
 
 AUTHOR   'B.Bloch-Devaux'
 REVIEWER ''
 VERSION  '1.2'
 DATE     '18/10/94'
 
 DEFINE ESET
 SLUM
             : 'Sical  Luminosity run statistics (successive cuts)\
                number of words / selection\
                number of selections'

             STATIC
             =(SIthr      = REAL[0.,*]  :' E1,E2 > Side  Thresh.',
               SUthr      = REAL[0.,*]  :' E1+E2 > Sum Thresh.',
               DPhi       = REAL[0.,*]  :' within DeltaPHI cut',
               TInner     = REAL[0.,*]  :' inside inner tight R cut',
               TOuter     = REAL[0.,*]  :' inside outer tight R cut',
               LInner     = REAL[0.,*]  :' inside inner loose R cut',
               LOuter     = REAL[0.,*]  :' inside outer loose R cut',
               VeRtical   = REAL[0.,*]  :' within Vertical method cut',
               HoRizontal = REAL[0.,*]  :' within Horizontal method cut',
               LUminosity = REAL[0.,*]  :' Luminosity',
               LumiVert   = REAL[0.,*]  :' Luminosity within Vertical cuts ',
               LumiHori   = REAL[0.,*]  :' Luminosity within Horizontal cuts',
               StatError  = REAL[0.,*]  :' Statistical error',
               SYstematic = REAL[0.,*]  :' Estimated systematic error')
             ;
 
 SILH
      :      'Sical Luminosity parameter bank: this bank should contain
              all relevant information from SICAL and Headers to compute
              luminosity.Together with SILU ,and SIXA when present,they
              are the only banks which are written out to DST and mini. \
              Number of words/row \
              Number of rows  '
 
       STATIC
    = (GBxcnt     = INTE [0    ,*]  : 'GBX counter',
       HVolt      = INTE [*    ,*]  : 'HV bit pattern ',
       Trlvl1     = INTE [*    ,*]  : 'Level 1 trigger bit pattern',
       Trlvl2     = INTE [*    ,*]  : 'Level 2 trigger bit pattern',
       TEnabl     = INTE [*    ,*]  : 'Trigger enable  bit pattern',
       LOlewd     = INTE [*    ,*]  : 'LOLE information word',
       Six2wd     = INTE [*    ,*]  : 'SIX2 trigger word',
       DoWnsc(3)  = INTE [*    ,*]  : 'Down scale factors for the 3 triggers',
       SCaler(3)  = INTE [0    ,*]  : 'on line scalers for the 3 triggers',
       AccMeth    = INTE [*    ,*]  : 'Acceptance Method bit pattern',
      WagonIdent  = INTE [0    ,*]  : 'Wagon Identifier ')
      ;
 
 SILU
      :      'Sical Luminosity parameter bank: this bank should contain
              all relevant information from SICAL to compute luminosity.
              Together with SILH ,and SIXA when present,they are the only
              banks which are written out to DST and mini.
              Side 1 is the tight cut side , Side 2 the loose cut side. \
              Number of words/cluster\
              Number of clusters'
 
       STATIC
    = (ISide      = INTE [1 ,   2]   : 'Fiducial Side of the cluster  ',
       EClust     = REAL [0.,200.]   : 'Energy of most energic cluster on \
                                        that side ( GEV)',
       ERaw       = REAL [0.,200.]   : 'Raw Energy of cluster ',
       EFrac(12)  = REAL [0.,  1.]   : 'Energy fraction in the 12 plans',
       RClus      = REAL [0., 15.]   : 'Barycenter Radius of cluster',
       CorrR      = REAL [0., 15.]   : 'Corrected Radius of  cluster',
       PhiClus    = REAL [0.,6.3 ]   : 'Barycenter Phi (rad) of cluster',
       CorrPhi    = REAL [0., 15.]   : 'Corrected Phi (rad) of cluster',
       TheClus    = REAL [-.2,0.2]   : 'Barycenter Theta(rad) of cluster',
       CorrThe    = REAL [-.2,02.]   : 'Corrected Theta(rad) of cluster',
       ZClus      = REAL [-300.,300.]: 'Barycenter cluster Z of cluster',
       DRad       = REAL [0.  ,100.] : 'radial width of cluster',
       DPhi       = REAL [0.  ,999.] : 'phi width of cluster',
       RadIndx    = INTE [1   ,15]   : 'R index of cluster corrected radius ',
       PhiIndx    = INTE [1   ,32]   : 'Phi index of cluster corrected phi ',
       XposI      = REAL [*,    *]   : 'X position extrapolated at Z0',
       YposI      = REAL [*,    *]   : 'Y position extrapolated at Z0',
       XposF      = REAL [*,    *]   : 'X position extrapolated at Front face ',
       YposF      = REAL [*,    *]   : 'Y position extrapolated at Front face ',
       NclPad     = INTE [1 ,1000]   : 'Npads per cluster ',
       NDead      = INTE [1 ,1000]   : 'N dead pads used per cluster ',
       EPads(45)  = REAL [0.,   *]   : 'Energy of 9 central pads in 5 planes',
       EcluS      = REAL [0.,200.]   : 'Energy of second most energic cluster \
                                       on  same side ( GEV)',
       RSclu      = REAL [0., 15.]   : 'Corrected Radius of second highest clu',
       PSclu      = REAL [0., 6.3]   : 'Corrected Phi of second highest clu',
       ZSclu      = REAL [-300.,300.]: 'Barycenter Z of second highest clu',
       NSecpad    = INTE [1 ,1000]   : 'Npads per second cluster ')
      ;
 

     END ESET

 END SUBSCHEMA
 
