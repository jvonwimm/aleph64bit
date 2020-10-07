 SUBSCHEMA HcalJULBanks
 : 'Description of HCAL JULIA banks'
 
 AUTHOR   'G. Capon,G.Bagliesi,A.Venturi'
 REVIEWER 'A.Sciaba'
 VERSION  '4.1'
 DATE     '29/04/97'
 
 DEFINE ESET
 
 HSDA
      :      'Hcal Storey DAta ( Raw Data ) \
              Number of words/storey\
              Number of fired storeys'
           STATIC
 
      = (ThetaIndex   = INTE [1,62]        : 'theta index',
         PhiIndex     = INTE [1,96]        : 'phi index',
         StackNumber  = INTE [1,2]         : 'stack number',
         DeposEnergy  = REAL [0.,100.]     : 'deposited energy (Gev)',
         ModuleNumbe  = INTE [1,24]        : 'module number',
         SubComp      = INTE [1,3]         : 'subcomponent number=1,2,3\
                                              endcapA,barrel,endcapB',
         RegionNumbe  = INTE [1,3]         : 'region number (for phi subdivision)',
         ClusterNumb  = INTE [1,999]       : 'cluster number',
         NextStorey   = INTE [0,999]       : 'Number of next storey\
                                              in this cluster')
      ;
 
 HSTO
      :      'Hcal STOrey geometrical data \
              Number of words/storey\
              Number of fired storeys'
           STATIC
 
      = ( XI(4)       = REAL [-500.,500.]   : 'X coordinate storey corners inner face',
          XO(4)       = REAL [-500.,500.]   : 'X coordinate storey corners outer face',
          InnerX      = REAL [-500.,500.]   : 'X coordinate center of inner face',
          OuterX      = REAL [-500.,500.]   : '    "     "         of outer face',
          StoreyX     = REAL [-500.,500.]   : '    "     "         of storey',
          YI(4)       = REAL [-500.,500.]   : 'Y coordinate corners inner face',
          YO(4)       = REAL [-500.,500.]   : 'Y coordinate corners outer face',
          InnerY      = REAL [-500.,500.]   : 'Y coordinate center of inner face',
          OuterY      = REAL [-500.,500.]   : '    "     "         of outer face',
          StoreyY     = REAL [-500.,500.]   : '    "     "         of storey',
          ZI(4)       = REAL [-500.,500.]   : 'Z coordinate corners inner face',
          ZO(4)       = REAL [-500.,500.]   : 'Z coordinate corners outer face',
          InnerZ      = REAL [-500.,500.]   : 'Z coordinate center of inner face',
          OuterZ      = REAL [-500.,500.]   : '    "     "         of outer face',
          StoreyZ     = REAL [-500.,500.]   : '    "     "         of storey',
          SubcompNumb = INTE [1,3]          : 'Subcomponent number',
          ModuleNumbe = INTE [1,24]         : 'Module number',
          OverlapFlag = INTE [0,1]          : 'Flag (=1) for endcap/barrel\
                                              overlap region')
      ;
 
 HCLU
      :      'Hcal CLUster \
              Number of words/cluster\
              Number of Hcal clusters'
           STATIC
 
      = (ClusterCharge= INTE [-1,1]        : 'cluster charge',
         EnergyCluster= REAL [0.,100.]     : 'corrected energy in cluster (Gev)',
         Energy1      = REAL [0.,100.]     : '     "             stack 1 (Gev)',
         Energy2      = REAL [0.,100.]     : '     "             stack 2 (Gev)',
         RhoCluster   = REAL [250.,1000.]  : 'average rho  for  cluster',
         Rho1         = REAL [250.,1000.]  : '     "            stack 1',
         Rho2         = REAL [250.,1000.]  : '     "            stack 2',
         ThetaCluster = REAL [0.,3.1416]   : 'average theta for cluster',
         Theta1       = REAL [0.,3.1416]   : '   "       "      stack 1',
         Theta2       = REAL [0.,3.1416]   : '   "       "      stack 2',
         PhiCluster   = REAL [-3.2,3.1416] : 'average  phi for  cluster',
         Phi1         = REAL [-3.2,3.1416] : '   "      "       stack 1',
         Phi2         = REAL [-3.2,3.1416] : '   "      "       stack 2',
         SigmaClust   = REAL [0.,6.2832]   : 'r.m.s. angular width     ',
         Sigma1       = REAL [0.,6.2832]   : 'r.m.s. ang width  stack 1',
         Sigma2       = REAL [0.,6.2832]   : 'r.m.s. ang width  stack 2',
         KDregion     = INTE [0,1]         : 'region code (=0 for standard case\
                                              = 1 if cluster in overlap region\
                                              = 2 if above an ECAL crack\
                                              = 3 if overlap and above an ECAL crack)',
         EnergyRaw    = REAL [0.,100.]     : 'raw energy in cluster (Gev)',
         FirstStorey  = INTE [1,999]       : 'Storey # of first storey of cluster',
         NoiseFlag    = INTE [0,64]        : 'Noise Flag from Topological Analysis\
                                              and Digital Pattern matching\
                                              bits 0-4 Top. An. (0=OK,1=Noise)\
                                              bit  5 Dig. Matching (0=OK,1=Noise)')
      ;
 
 HTUB
      :      'Hcal TUBes data
              (from Julia prepare data)\
              Number of words/tube \
              Number of fired tubes'
           STATIC
 
      = (SubcompNumbe = INTE [1,3]         : 'subcomponent number',
         ModuleNumber = INTE [1,24]        : 'module number (1-6 endcaps, 1-24 barrel)',
         LayerNumber  = INTE [1,23]        : 'layer number (1-23)',
         LocalCoordinate= REAL [0.,400.]   : 'local coordinate of center of cluster',
         ClusterWidth = REAL [0.,20.]      : 'cluster width ( cm )',
         PointerRawdata = INTE [0,*]       : 'Pointer to raw data bank HWDI',
         PointerPattern = INTE [0,*]       : 'Pointer to Pattern bank HPCO/PPOB' )
      ;

  HMAD
      :      'Hcal Muon tracks Association Data \
              Number of words/associated track\
              Number of associated tracks'
           STATIC
 
 = (NplaFired = INTE [0,23]        : 'number of fired planes',
    NplaExp   = INTE [0,23]        : 'number of expected fired planes',
    NplaLast  = INTE [0,10]        : 'number of fired planes in last 10',
    MultHits  = INTE [0,30]        : 'number of clusters in last ten planes',
    IGeomflag = INTE [-1,8]        : 'geometry flag :\
                                      -1 : error condition\
                                       0 : track in active zone\
                                       1 : dead zone between two barrel modules\
                                       2 : dead zone between two endcap modules\
                                       3 : barrel module notch\
                                       4 : spacer in endcap module\
                                       5 : not active zone of tube\
                                       6 : first plane in thin endcap\
                                       7 : masked digital chain\
                                       8 : masked HV channel ',
    EnerDep   = REAL [0.0,100.]    : 'energy deposit in corresponding Hcal storey',
    ChiSquare = REAL [0.00,200.00] : 'chisquare for hits inside the Hcal road\
                                      - not filled in jul222',
    NumbDeg   = INTE [0,23]        : 'degrees of freedom -not filled in jul222',
    IExpbmap  = INTE [0,11000000]  : 'expected bit map for Hcal planes',
    ITruebmap = INTE [0,11000000]  : 'observed bit map for Hcal planes \
                                      Note: bit# = plane# only for barrel \
                                      regions away from endcap-overlap ',
    IdenFlag  = INTE [-1,1]        : 'preliminary identification flag \
                                     -1 : hadron\
                                      0 : not classified (track near dead zone)\
                                      1 : muon candidate',
    TrackNo   = INTE [1,999]       : 'associated track # ')
      ;
 
 HPCO                                                                           
      :      'Pattern COnstants \                                               
              Number of words/pattern\                                          
              Number of patterns'                                               
           STATIC                                                               
      = (DirectionIndex  = INTE [-1,5]       : ' -1 = pattern too large,\       
                                                  1 = barrel,\                  
                                                  2 = endcap Y-right,\          
                                                  3 = endcap Y-left,\           
                                                  4 = endcap S-up,\             
                                                  5 = endcap S-down',           
         DigitalEnergy   = REAL [0.,*]       : 'Digital Energy',                
         Cor1            = REAL [-400.,400.] : ' (Cor1,DirectionIndex)\         
                                                 (R,1) (Z,2-5)',                
         Cor2            = REAL [-500.,500.] : ' (Cor2,DirectionIndex)\         
                                                 (PHI,1) (Y,2-3)\               
                                                 (S-up,4) (S-down,5)',          
         NumberDigital   = INTE [0,*]        : 'Number of digital hits',        
         InteractPlane   = INTE [0,24]       : 'Interaction Plane',             
         FirstPlane      = INTE [0,24]       : 'First Plane',                   
         LastPlane       = INTE [0,24]       : 'Last Plane',                    
         MaxDistance     = REAL [0.,500.]    : 'Max pattern width',             
         PattDensity     = REAL [0.,100.]    : 'Pattern Density',               
         CentrX          = REAL [-500.,500.] : 'Pattern x-centroid',            
         CentrY          = REAL [-500.,500.] : 'Pattern y-centroid',            
         BordFlag        = INTE [1,2]        : 'Border flag ',                  
         HTpabank        = INTE [0,*]        : 'Linked HTPA bank',              
         HPdsbank        = INTE [0,*]        : 'Linked HPDS bank')              
      ;                                                                         
                                                                                
 HPDS                                                                           
       :     'Patterns DiScriminant analysis (NR=linked index in HPCO)\         
              Number of words/layer\                                            
              Number of fired layers'                                           
           STATIC                                                               
                                                                                
       =  (NumLayers     = INTE [0,25]       : 'Number of current layer',       
           FiredLength   = REAL [0.,500.]    : 'Fired length',                  
           MaxDistance   = REAL [0.,500.]    : 'Max layer Distance')            
      ;                                                                         
                                                                                
 HTPA                                                                           
       :     'Tubes to PAttern  (NR=linked index in HPCO)\                      
              Number of words/cluster\                                          
              Number of clusters in the current pattern'                        
           STATIC                                                               
                                                                                
       =  (LinkedRow     = INTE [0,*]        : 'Row in HTUB linked to pattern') 
      ;                                                                         
                                                                                
 HSPE                                                                           
       :     'SuperPattern of E-type (NR=Number of superpattern)\               
              Number of words/superpattern\                                     
              Number of elements in superpattern NR'                            
           STATIC                                                               
                                                                                
       =  (EcalRow       = INTE [0,*]   : 'Row in ECLU linked to superpattern') 
      ;                                                                         
                                                                                
 HSPH                                                                           
       :     'SuperPattern of H-type (NR=Number of superpattern)\               
              Number of words/superpattern\                                     
              Number of elements in superpattern NR'                            
           STATIC                                                               
                                                                                
       =  (HcalRow       = INTE [0,*]   : 'Row in HCLU linked to superpattern') 
      ;                                                                         
                                                                                
 HSPT                                                                           
       :     'SuperPaTtern quantities\                                          
              Number of words/superpattern\                                     
              Number of superpatterns'                                          
           STATIC                                                               
                                                                                
       = (NumberCharged  = INTE [0,*]       : 'Number of charged ECLU',         
          FirstPlane     = INTE [-4,26]     : 'First Plane Generalized',        
          LastPlane      = INTE [-4,26]     : 'Last Plane Generalized',         
          DigitalEnergy  = REAL [0.,*]      : 'Digital Energy',                 
          EcluEnergy     = REAL [0.,*]      : 'ECLU Energy')                    
      ;                                                                         

 HROA
      :     'Hcal road data for current track\
             Number of words/track\
             Number of extrapolated tracks'
          STATIC

      = (DiffX    = REAL [-999.,999.] :'hit distance to road axis ',
         SiGma    = REAL [-999.,999.] :'r.m.s scatt. displacement',
         StatFlag = INTE [0,99999]    :'status flag :            \
                                       bit 0 :hit inside road    \
                                           1 :hit in MULT cone   \
                                           2 :hit in road and last10\
                                       bits 8-15: HMAD bitmap bit number',   
         HitInd   = INTE [0,99999]    :'hit index in bank HTUB')

     ;

 HLTU
      :     'True MC longit. coordinate of Hcal hit\
             Number of words/Hcal hit\
             Total number of Hcal hits'
          STATIC

      = (XLong    = REAL [-999.,999.] :'longitudin. hit coordinate     \
                                        = z if hit in barrel           \
                                        = abs(x) if sextants 1,3,4,6   \
                                        = rotated coord if sextant 2,5 ',
         HitInd   = INTE [0,99999]    :'hit index in bank HTUB')

 ;

 END ESET

 END SUBSCHEMA


 SUBSCHEMA HcalGALBanks
 : 'Description of HCAL GALEPH banks'
 
 AUTHOR   'G.Capon, G.Catanesi, L.Silvestris'
 REVIEWER 'F.Ranjard'
 VERSION  '3.5'
 DATE     '05/12/91'
 
 DEFINE ESET
 
  HPHT
      :      'McHcPlanes \
              Number of words/plane\
              Number of fired planes'
           STATIC
 
      = (PlaneAddres  = INTE [10101,30622] : 'plane address  *',
         EnergyDepos  = REAL  [0.,100.]    : 'energy deposited (Gev)')
      ;
 
 
  HTHT
      :      'McHcStoreys
              NR=0. (GAL)\
              Number of words/storey\
              Number of fired storeys'
           STATIC
 
      = (StoreyAddre  = INTE [10101,622402]: 'storey address  *',
         PhiTrigger   = INTE  [1,24]       : 'phi trigger region index',
         ThetaTrigge  = INTE  [1,12]       : 'theta trigger region index',
         EnergyDepos  = REAL  [0.,100.]    : 'energy deposited (Gev)')
      ;
 
 
 HWHT
      :      'McHcTubeSignal \
             Number of words/signal\
             Number of tube signals'
          STATIC
 
      = (TubeAddress  = INTE  [10101001,30622320]  : 'tube address  *')
      ;
 
  
 HLWD
      :      'Hcal Longitudinal Wire Digit\
              Number of words/wire hit\
              Number of wire hits'
          STATIC

      = (WireAddr       = INTE  [*,*]     : 'Wire address \
                                             bits:  0- 7 : address of the tube [1,255]\
                                                    8-15 : cluster width  [1]\
                                                   16-23 : layer number [0,22]\
                                                   24-31 : electronic module number [1,36]\
                                                           1-12 end-cap A\
                                                           13-24 barrel\
                                                           25-36 end-cap B\
                                                           attention ! the electronic 
                                                           module number is different 
                                                           for data, look HPRDIG in Julia',
         HitPosition   = REAL [*,*]       : 'position of the hit along the wire' ) 
      ;
        
  HTTR
      :      'Hcal Tower TRigger signals \
              Number of words/trigger segment\
              Number of trigger segments ( = 192 )'
           STATIC
 
      = (EnergyDepos  = INTE  [0,100000]   : 'energy deposited in trigger segment (MeV)')
      ;
 
 
 HWTR
      :      'Hcal Wire(tube) TRigger signals \
              Number of words/module\
              Number of modules ( = 36 )'
           STATIC
 
      = (NumberOfFir  = INTE [0,23]        : 'number of fired planes')
      ;
 
 END ESET

 END SUBSCHEMA

 
 SUBSCHEMA HcalRAWBanks
 : 'Description of HCAL Raw data banks'
 
 AUTHOR   'G. Capon, L. Silvestris'
 REVIEWER 'A. Venturi'
 VERSION  '4.0'
 DATE     '11/10/00'
 
 DEFINE ESET
 
 
 HTDI
      :      'Hcal Tower DIgits (RawData)\
              Number of words / tower\
              Number of towers'
           STATIC
 
      = (TowerAddres  = INTE [0,*]         : 'Tower address\
                                                 bits:    0-15 : phi-index [1,96]\
                                                         16-23 : theta-index [1,62]\
                                                         24-31 : region number [1,3]',
         EnergyDepos  = INTE  [0,100000]   : 'Energy ( MeV)')
      ;
 
 
 HWDI
      :      'Hcal Wire (tube) DIgits(RawData)\
              Nb of words/wirecluster\
              Number of clusters'
           STATIC
 
      = (ClusterAddr  = INTE  [*,*]     : 'Cluster address and width\
                                             bits:  0- 7 : address of last tube\
                                                           in cluster [1,255]\
                                                    8-15 : cluster width  [1,15]\
                                                   16-23 : layer number [0,22]\
                                                   24-31 : electronic module number\
                                                                 [1,36]\
                                                               1-12 end-cap A\
                                                              13-24 barrel\
                                                              25-36 end-cap B\
                                                           attention ! the electronic\ 
                                                           module number is different\ 
                                                           for data, look HPRDIG in Julia')         
     ;
 
 HPDI
      :      'Hcal Plane DIgits (RawData)\
              Number of words / plane\
              Number of planes'
           STATIC
 
      = (PlaneAddres  = INTE [10101,306022]: 'Plane address \
                                 bits:  0- 7 : plane number \
                                               endcaps[1,12] 89-90-91 data\
                                               endcaps[1,22] 92 data\   
                                               barrel[1,23] 89-90-91-92 data\
                                        8-15 : module number \
                                               endcaps[1,6],barrel[1,24]\
                                       16-23 : subdetector number [1,3]\
                                                 endcapA=1,Barrel=2,endcapB=3',
         EnergyDepos  = INTE  [0,100000]   : 'Energy in plane ( MeV)')
 
      ;
 
  HCLB
      :      'Hcal CaLiBration information\
              Number of columns\
              Number of rows (=1)'
           STATIC
 
      = (CalibFactor   = REAL [0.,10.] : 'Calibration Factor',
         DateTime(3)   = CHA8          : 'Date and Time',
         NumCalib      = INTE [0,48]   : 'Number of Calibration Tubes',
         NumfiringTube = INTE [0,96]   : 'Number of Firing Tubes')
      ;
 
 
 HCTB
      :      'Hcal Calibration TuBes\
              Number of words / Tube\
              Number of Tubes'
           STATIC
 
     =  (Charge       = REAL [-1.,100.] : 'Charge Value (pC)',
         Variance     = REAL [0.,1.]    : 'Variance of the Charge Distribution',
         SigmaFit     = REAL [0.,100.]  : 'Sigma of the Fit',
         ChiSquare    = REAL [0.,10.]   : 'Chi Square of the Fit',
         NumEvents    = INTE [0,9999]   : 'Number of the Events',
         TubeStatus   = INTE [0,11]     : 'Tube Status Value')
      ;
 
 HTDO
      :      'Hcal Tower DIgits Test version (RawData)\
              Number of words / tower\
              Number of towers'
           STATIC
 
      = (TOweraddress  = INTE [0,*]         : 'Tower address\
                                                 bits:    0-7 : phi-index [1,96]\
                                                          8-15 : =0 barrel,=1 endcap [0,1]\
                                                         16-23 : theta-index [1,62]\
                                                         24-31 : region number [1,3]',
         ENergydepos  = INTE  [0,100000]   : 'Energy ( MeV)')
      ;

 
 
 END ESET
 
 END SUBSCHEMA


 SUBSCHEMA HcalSLOWControl
 : 'HCAL Slow Control Banks'
 
 AUTHOR   'R. Tenchini'
 REVIEWER 'L. Silvestris'
 VERSION  '1.4'
 DATE     '29/04/91'
 
 DEFINE ESET
 
 
HSSR
      :      'Hcal Slow controls at Start
                 of Run\
              Number of columns \
              Number of rows (=1)'
           STATIC
 
      = (VbarreL(240) = INTE [0,*]     : 'Nominal high Voltages and
                                              active channels mask
                                              barrel (Volt)',
         VendcapA(160) = INTE [0,*]     : 'Nominal high Voltages and
                                              active channels mask
                                              endcap A (Volt)',
         VendcapB(200) = INTE [0,*]     : 'Nominal high Voltages and
                                              active channels mask
                                              endcap B (Volt)')
      ;
 
HSCW
      :      'Hcal Slow Control Warning\
              Number of columns\
              Number of warnings'
           STATIC
 
      = (WarnType    = INTE [0,*]     : 'Warning Type',
         DeviNumb    = INTE [0,*]     : 'Device Number',
         DeviCurr    = INTE [0,*]     : 'Device Current value',
         DeviDesi    = INTE [0,*]     : 'Device Desired value')
      ;
 
HSCE
      :      'Hcal Slow Control End of warning\
              Number of columns\
              Number of end of warnings'
           STATIC
 
      = (WarNtype    = INTE [0,*]     : 'Warning Type',
         DevinumB    = INTE [0,*]     : 'Device Number')
      ;
HSCI
      :      'Hcal Slow Control Information\
              Number of words/HCslow  \
              Number of HCslow event (=1)'
           STATIC
 
      = (Flag0              = INTE [0,1]     : 'Hcslow Flag',
         SuppMixe(96)       = INTE [0,*]     : 'Mixer boxes low voltage (mV)',
         Flag1(3)           = INTE [0,*]     : 'Flag',
         ThreShol(72)       = INTE [0,*]     : 'SGS cards ThreSholds (mV)',
         Flag2(3)           = INTE [0,*]     : 'Flag',
         Suppspli1(72)       = INTE [0,*]     : 'Splitter Board low voltage
                                                     after the fuse (mV)',
         Flag3(3)           = INTE [0,*]     : 'Flag',
         Suppspli2(72)       = INTE [0,*]     : 'Splitter Board low voltage
                                                 before the fuse (mV)',
         Flag4(3)           = INTE [0,*]     : 'Flag',
         SuppHv(60)       = INTE [0,*]     : 'High Voltages distribution box
                                                  power supply (mV)',
         Flag5(3)           = INTE [0,*]     : 'Flag',
         FastSt(10)       = INTE [0,*]     : 'Fastbus Status',
         TemP(96)        = INTE [0,*]     : 'mixer Box and detector
                                             TemPerature (0.1 degree)',
         TrigRat(30)     = INTE [0,*]     : 'Trigger Rates', 
         Flag6(3)           = INTE [0,*]     : 'Flag',
         MarKer             = INTE [0,*]     : 'Final Marker =
                                                Hexadecimal AAAAAAAA',
         BarrVm(240)      = INTE [0,*]     : 'High Voltages monitoring
                                                  (barrel) (Volt)',
         Flag7(6)           = INTE [0,*]     : 'Flag',
         EndAvm(160)      = INTE [0,*]     : 'High Voltages monitoring
                                                  (endcap A) (Volt)',
         Flag8(4)           = INTE [0,*]     : 'Flag',
         EndBvm(200)      = INTE [0,*]     : 'High Voltages monitoring
                                                  (endcap B) (Volt)',
         Flag9(5)           = INTE [0,*]     : 'Flag',
         barRIm(240)      = INTE [0,*]     : 'Currents monitoring
                                                  (barrel) (0.1 microAmps)',
         FlagA(6)           = INTE [0,*]     : 'Flag',
         endAIm(160)      = INTE [0,*]     : 'Currents monitoring
                                                  (endcap A) (0.1 microAmps)',
         FlagB(4)           = INTE [0,*]     : 'Flag',
         endBIm(200)      = INTE [0,*]     : 'Currents monitoring
                                                  (endcap B) (0.1 microAmps)',
         FlagC(5)           = INTE [0,*]     : 'Flag')
      ;
 
HHPS
      :      'Hcal Decoded Pedestals\
              Number of columns \
              Number of towers (5556)'
           STATIC
 
      = (TowerAdd = INTE [0,*]     : 'Address of the tower',
         PedValue = INTE [0,*]     : 'Pedestal value')
      ;
HPEA
      :      'Hcal  Endcap A Pedestals\
              Number of columns\
              Number of rows (8070)'
           STATIC
 
      = (adcpedEA  = INTE [0,*]     : 'ADc pedestals')
      ;
 
HPEB
      :      'Hcal  Endcap B Pedestals\
              Number of columns\
              Number of rows (8070)'
           STATIC
 
      = (adcpedEB  = INTE [0,*]     : 'ADc pedestals')
      ;
 
HPB1
      :      'Hcal  Barrel 1 Pedestals\
              Number of columns\
              Number of rows (10374)'
           STATIC
 
      = (adcpedB1  = INTE [0,*]     : 'ADc pedestals')
      ;
 
HPB2
      :      'Hcal  Barrel 2 Pedestals\
              Number of columns \
              Number of rows (12102)'
           STATIC
 
      = (adcpedB2  = INTE [0,*]     : 'ADc pedestals')
      ;
 
 END ESET
 END SUBSCHEMA
 

 SUBSCHEMA HcalONLBanks
 : 'Unforseen HCAL raw data banks'
 
 AUTHOR   'E. Mannelli, A.Venturi'
 REVIEWER 'L. Silvestris'
 VERSION  '3.0'
 DATE     '29/05/95'
 
 DEFINE ESET
 
 
HINF
      :      'Hcal Online Error bank\
              Number of columns \
              Number of rows = 1'
           STATIC
 
      = (ErrID = INTE [0,*]     : 'Error condition \ 
         Presence of bank in an event signals \ an error. 
         There is so far
         only one \ generic error condition with value 1.')
      ;

HCCV
      :      'Hcal  Online Calibration Constant\
              Number of columns \
              Number of rows = 1'
           STATIC
 
      = (CalibCons  = INTE [0,*]     : 'Actual value of\
             calibration constant used \ online for
             this event* 1000.\')
      ;
 
HSTP
      :      'Hcal TDC stop times\
              Number of columns \
              Number of channels recorded'
           STATIC
 
      = (TdCstop  = INTE [0,*]     : 'TDC stop datum\
           Trailing Edge Data Memory\
           bit  0     = stop phase\
           bits 1-9   = stop time\
           bits 16-22 = channel number\
           bits 27-31 = geographic address')
      ;
 
HSTR
      :      'Hcal TDC start times\
              Number of columns \
              Number of channels recorded'
           STATIC
 
      = (TDcstart  = INTE [0,*]     : 'TDC start datum\
           Leading Edge Data Memory\
           bit  0     = stop phase\
           bits 1-9   = start time\
           bits 16-22 = channel number\
           bits 27-31 = geographic address')
      ;
 
HLOD
      :      'HCAL Time settings.(STC)\
              Number of columns    \
              Number of rows'
           STATIC
      = (CLear       = INTE [0,*] : 'ADC Clear ',
         BArrelload  = INTE [0,*] : 'Barrel load',
         EndcapAload = INTE [0,*] : 'End cap A load',
         EndcapBload = INTE [0,*] : 'End cap B load')
      ;


 END ESET

 END SUBSCHEMA
