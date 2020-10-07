 SUBSCHEMA EcalRUNConsts
 : 'Description of Ecal RUN Constants '
 
 AUTHOR   'O.Callot,L.Thomson'
 REVIEWER 'B.Bloch-Devaux,M.N.Minard'
 VERSION  '2.0'
 DATE     '29/04/94'
 
 DEFINE ESET


    ECHE
     :  'ECal HEader calibration constants \
          Number of words/module   \
          Number of modules '
         STATIC
      = (  CalibPad  = REAL [0.,2.0000] : 'Calibration constant for pads ',
           CalibWire = REAL [0.,2.000]  : 'Calibration constant for wires'
        )

     ;

 
 EZTH
      :      'Ec On line thresholds applied in ROC for each stack
              NR=setup code or run number \
              Number of words/component\
              Number of components  '
 
       STATIC
      = (ADdress      = INTE [65536,*]  : 'subcomponent address coded as :
                                           65536*module [1-36] + box [1-4]
                                          box 0 , module 12 means all end cap A
                                          box 0 , module 24 means all Barrel
                                          box 0 , module 36 means all ens cap B ',
         Thst1        = REAL [0.00,* ]  : 'Threshold stack 1 in Mev     ',
         Thst2        = REAL [0.00,* ]  : 'Threshold stack 2 in Mev     ',
         Thst3        = REAL [0.00,* ]  : 'Threshold stack 3 in Mev      ')
      ;
 
 ECRQ
      :      'ECal Run Quality information\
              Number of words/module\
              Number of module with run quality information'
           STATIC
 
      = (ModnumAutopeds = INTE   : 'Module number and Autopeds\
                                    bits 1-6: module number (1-36)\
                                    bits 7-20: Autopeds activity',
         CleanedStorey  = INTE   : ' Number of cleaned storeys \
                                    bits 1-14 : Storeys cleaned by LCAL \
                                    bits 15-28: Storeys cleaned by wires')

    
     ;
 
 ERRR
      :      'Ecal Read out & eRRor bank\
              Number of words/Roc\
              Number of Roc/Adc with read out information'
           STATIC
 
      = (infoTYpe       = INTE   : 'Information type  \
                                      <0        = Event Builder error\
                                      >0 & <100 = Roc error\
                                      100       = Autopeds info ',
          RoCnumber     = INTE   : ' Roc number [1-8] ',
          ADnumber      = INTE   : ' ADC number [0-23] ',
          Increases1    = INTE   : ' # Increased storeys stack1 [0-1024]',
          Increases2    = INTE   : ' # Increased storeys stack2 [0-1024]',
          Increases3    = INTE   : ' # Increased storeys stack3 [0-1024]',
          Decreases1    = INTE   : ' # Decreased storeys stack1 [0-1024]',
          Decreases2    = INTE   : ' # Decreased storeys stack2 [0-1024]',
          Decreases3    = INTE   : ' # Decreased storeys stack3 [0-1024]',
          freeWord1     = INTE   : ' Free word [0]',
          freeWord2     = INTE   : ' Free word [0]')


     ;
 
 
ERRF                                                                            
    :          'Ecal Read out eRror Fatal bank\                                 
                Number of words/Roc\                                            
                Number of Roc or Adc with read out information'                 
             STATIC                                                             
       = (ErreurFlag     = INTE   : 'ERREUR TYPE \                                    
                                      <0        = Event Builder error\                         
                                      >0 & <100 = Roc error',                                   
          ROcnumber      = INTE   : ' Roc number [1-8] ',                        
          AdCnumber      = INTE   : ' ADC number [0-23] ',                       
          inCreases1     = INTE   : ' # Increased storeys stack1 [0-1024]',       
          inCreases2     = INTE   : ' # Increased storeys stack2 [0-1024]',       
          inCreases3     = INTE   : ' # Increased storeys stack3 [0-1024]',       
          dEcreases1     = INTE   : ' # Decreased storeys stack1 [0-1024]',       
          dEcreases2     = INTE   : ' # Decreased storeys stack2 [0-1024]',       
          dEcreases3     = INTE   : ' # Decreased storeys stack3 [0-1024]',       
          Freeword1      = INTE   : ' Free word [0]',                             
          Freeword2      = INTE   : ' Free word [0]' )                           
     ;                                                                          
                                                                                
                                                                                
 ETKC
      :      'Ecal Tower Killed by Cleaning (eclamp)\
              Number of words/Tower\
              Number of tower'
           STATIC
 
      = (TowerLabel     = INTE   : 'Tower Label (as ETDI) ',
         energyinS1     = INTE   : 'Stack1 energy removed (Kev)',
         energyinS2     = INTE   : 'Stack2 energy removed (Kev)',
         energyinS3     = INTE   : 'Stack3 energy removed (Kev)',
         KilledReason   = INTE   : 'Source of Killing \
                                    1 = Noisy amplificator\
                                    11= isolated storey')
     ;
 
 EROR
      :      'Ecal Read Out eRror message bank\
              Number of word per element \
              Number of elements '
           STATIC
 
      = (messWOrd      = CHA4   : '1 element = 4 characters')
     ;
 
 EHPA
      :      'Ecal History PAd pedestaux'
           STATIC
 
      = (PAdelement  = INTE    :' bit 0-11 pedestal value\
                                     12-13 stack (0-2)\
                                     14-18 channel (0-31)\
                                     19-31 cable (0-288)')
 
 
   ;
 
 EHWI
      :      'Ecal History PAd pedestaux'
           STATIC
 
      = (WIreelement = INTE    :' bit 0-11 pedestal value\
                                     12-13 not used   \
                                     14-19 plan number+pastis (0-52)\
                                     20-31 module number (1-36)')
 
 
   ;
 
 
 END ESET
 
 END SUBSCHEMA


 SUBSCHEMA EcalRAWBanks
 
 AUTHOR   'M.N.Minard, C.Goy'
 REVIEWER 'F.Ranjard'
 VERSION  '3.0'
 DATE     '17/05/96'
 
 DEFINE ESET
 
 
 ETDI
      :      'Ecal Tower DIgits 
              NR=0. (RAW)\
              Number of words/tower \
              Number of towers'
           STATIC
 
      = (TowerLabel   = INTE               : 'tower label, see ETHT',
         Stack1       = INTE               : 'stack 1 content ( keV )',
         Stack2       = INTE               : 'stack 2 content ( keV )',
         Stack3       = INTE               : 'stack 3 content ( keV )')
      ;
 
 EWDI
      :      'Ecal Wire plane DIgits 
              NR=0. (RAW) \
              Number of words/module\
              Number of modules above threshold'
           STATIC
 
      = (ModuleNumbe  = INTE [1,36]        : 'module number ( 1-36 )',
         PlanDig(45)  = INTE               : 'Plane digit ( keV )',
         SSumval(8)   = INTE               : 'Sum sampling value (ADC count)')
       ;
 
 
 EWHE
      :      'Ecal Wire HEader bank
              NR=0. (RAW)\
              Number of words/module\
              Number of modules (36)'
           STATIC
 
      = (SumDig       = INTE               : 'digital sum ( keV )',
         moduleNUmbe  = INTE [1,36]        : 'module number ( 1-36 )')
       ;
 
 EDWI
       :    'Ecal Digital Wire Information \
               Number of words per Module \
               Number of modules'
         SIZE 1,36
         STATIC
 
       = (ModuleNumber    = INTE [1,36] : 'Module Number [1,36]',
          PD(45)          = INTE [*,*]  : 'Plane Digit for each of the 45 planes (kev)',
          SS(16)          = INTE [*,*]  : ' Sum Sampling value ')
       ;
 
 ESPA    
       :    'Ecal Saturation PAds 
             NR=0. (RAW)\
             Number of words/tower \
             Number of towers'                     
         STATIC

       = (TowerLabel     = INTE       : 'Tower label (b 2-11 phi , 16-24 teta)', 			
          STacknumber    = INTE [1,3] : 'Stack number')
       ;
 
 ESWI    
       :    'Ecal Saturation  WIre 
             NR=0. (RAW)\
             Number of words/module \
             Number of modules'
         STATIC

       = (ModuleNumber   = INTE [1,36] : 'Module number (1-36)',
          PLanenumber    = INTE [1,61] : 'Plane number (1-61)')
       ; 


 END ESET

 END SUBSCHEMA

    
 SUBSCHEMA EcalSLOWControl
                                                                                
 AUTHOR   'D. Pallin'                                                           
 REVIEWER ' '                                                                   
 VERSION  '1'                                                                   
 DATE     '11/03/90'                                                            
                                                                                
 DEFINE ESET                                                                    
                                                                                
                                                                                
 ESLW                                                                           
      :      'Ecal SLoW control event (SLOW)\ 
              Number of words/event\                                            
              Number of slow control events'                                    
           STATIC                                                               
                                                                                
      = (DaTe         = INTE               : 'date year*10000+month*100+day',   
         TiMe         = INTE               : 'time hour*10000+min*100+sec',     
         ChanneL      = INTE               : 'channel number (for endcaps-hv:   
                                              channel nber # module nber)',     
         DetectorComp = CHA8               : 'detector component :\             
                                                                   ENDA_LV\     
                                                                   ENDA_HV\     
                                                                   ENDB_LV\     
                                                                   ENDB_HV\     
                                                                   BARL_LV\     
                                                                   BARL_HV',    
                                                                                
         AlarmType    = INTE               : ' = 1     ghost_boot  \            
                                               = 2     prealarm    \            
                                               = 3     alarm+start \            
                                               = 4     alarm+disjunction\       
                                               = 5     lv_alarm_temperature\    
                                               = 6     lv_alarm_stop  \         
                                               = 7     lv_alarm_tension\        
                                               = 8     lv_alarm_wires',         
         AlarmDiag    = INTE               : ' = 0     no diagnostic \          
                                               = 1     hv_drift  \              
                                               = 2     overload    \            
                                               = 3     multi_error \            
                                               = 4     over range\              
                                               = 5     no credit\               
                                               = 6     temperature\             
                                               = 7     tension\                 
                                               = 8     wires_warning')          
                                                                                
    ;                                                                           
                                                                                
                                                                                
 ESRH                                                                           
      :      'Ecal Start of Run High voltage summary (SLOW)\
              Number of words\                                                  
              Number of hv channel(28)'                                         
           STATIC                                                               
                                                                                
      = (SuBcomponent   = INTE               : '1 = barrel\                     
                                                2 = endcap A\                   
                                                3 = endcap B\                   
                                                4 = lcal',                      
         ModuleNumber   = INTE               : 'module2*100+module1\            
                                                (for endcaps 2 modules per      
                                                 channels. for barrel ,lcal     
                                                 module2=0)',                   
         HighVoltage    = REAL               : 'high voltage value at start     
                                                of run',                        
         STatus         = CHA4               : 'hv status at start of run\      
                                                  ON\                           
                                                  OFF\                          
                                                  UP\                           
                                                  DOWN\                         
                                                  DISC\                         
                                                  ALAR',                        
         RelayDetector  = CHA4               : 'detector relay status at start  
                                                of run\
                                                  ON\                           
                                                  OFF')                         
      ;                                                                         
                                                                                
 ESRL                                                                           
      :      'Ecal Start of Run Low voltage summary (SLOW)\
              Number of words\                                                  
              Number of lv channel(100)'                                        
           STATIC                                                               
      = (SUbcomponent   = INTE               : '1 = barrel\                     
                                                2 = endcap A\                   
                                                3 = endcap B\                   
                                                4 = lcal',                      
         BoxNumber      = INTE                  :  ' ',                         
                                                                                
                                                                                
         StAtus         = CHA4               : 'lv status at start of run\      
                                                  ON\                           
                                                  OFF\                          
                                                  ALAR')                        
      ;                                                                         
                                                                                
 ECRS                                                                           
      :      'ECal Run Summary (SLOW)\
              Number of words\                                                  
              Number of'                                                        
           STATIC                                                               
                                                                                
                                                                                
                                                                                
      = (Quality1       = REAL               : 'barrel+lcal low voltages.       
                                                  quality value in              
                                                 percent (average on the run)', 
         Quality2       = REAL               : 'endcap a low voltages.          
                                                  quality value in              
                                                 percent (average on the run)', 
         Quality3       = REAL               : 'endcap b low voltages.          
                                                  quality value in              
                                                 percent (average on the run)', 
         Quality4       = REAL               : 'barrel+lcal low voltages.       
                                                  min quality value in          
                                                 percent (minimal value in the  
                                                  run)',
         Quality5       = REAL               : 'endcap a low voltages.          
                                                  min quality value in          
                                                 percent (minimal value in the  
                                                  run)', 
         Quality6       = REAL               : 'endcap b low voltages.          
                                                  min quality value in          
                                                 percent (minimal value in the  
                                                  run)',
         Flag1          = INTE               : 'lv barrel+lcal alarm flag.      
                                                 set to 1 if at least one       
                                                 alarm during run',             
         Flag2          = INTE               : 'lv endap a alarm flag.          
                                                 set to 1 if at least one       
                                                 alarm during run',             
         Flag3          = INTE               : 'lv endap b alarm flag.          
                                                 set to 1 if at least one       
                                                       alarm during run')       
     ;                                                                          
                                                                                
                                                                                
 END ESET                                                                       
                                                                                
                                                                                
 END SUBSCHEMA                                                                  
 
 
 SUBSCHEMA EcalGALBanks

 
 AUTHOR   'A. Putzer'
 REVIEWER 'A. Putzer'
 VERSION  '1'
 DATE     '15/12/86'
 
 DEFINE ESET
 
 ESHI
      :      'Ecal Storey HIstory  bank \
              Number of words per prim track energy deposit \
              Number of prim tracks energy deposits'
           STATIC
 
    = (PrimaryTrack    = INTE [1,999]       : 'primary track number ',
       TowerIndex      = INTE [1,*]         : 'I,J compressed index of tower',
       DepositEnerg(3) = INTE [0,100000]    : 'energy (Kev) deposited by the \
                                               primary track in the 3 storeys')
      ;
 
 EWHI
      :      'Ecal Wire plane HIstory  bank \
              Number of words per prim track energy deposit \
              Number of prim tracks energy deposits'
           STATIC
 
    = (PrimaryTrack     = INTE [1,999]      : 'primary track number ',
       ModuleIndex      = INTE [1,36]       : 'Module # ',
       DepositEnerg(45) = INTE [0,100000]   : 'energy (Kev) deposited by the \
                                               primary track in the 45 planes')
      ;
 
 ETHT 
      :      'Ecal Tower HiTs 
              NR=0. (GAL) \
              Number of words/tower\
              Number of towers'
           STATIC
 
      = (TowerAddres  = INTE               : 'tower address ( b 2-11=phi, 16-24=teta )',
         Stack1       = INTE               : 'stack 1 signal ( keV )',
         Stack2       = INTE               : 'stack 2 signal ( keV )',
         Stack3       = INTE               : 'stack 3 signal ( keV )')
      ;
 
 

 
 EDNO
      :      'Ecal Digits NOise
              NR=0. (GAL) \
              Nnumber of words/tower\
              number of tower ( same as in ETDI )'
           STATIC
 
      = (TowerLabel   = INTE               : 'tower label, see ETHT',
         Noise1       = INTE               : 'noise in stack 1 ( keV )',
         Noise2       = INTE               : 'noise in stack 2 ( keV )',
         Noise3       = INTE               : 'noise in stack 3 ( keV )',
         Gain1        = INTE               : 'gain in stack 1, in keV(digit)/keV(hits)\
                                              multiplied by 10**6',
         Gain2        = INTE               : 'gain in stack 2',
         Gain3        = INTE               : 'gain in stack 3')
      ;
 
 
 ETTR
      :      'Ecal Tower TRigger signals 
              NR=0. (GAL)\
              Number of words/theta region \
              Number of theta regions (=12)'
         STATIC
 
      = (TrigSig(72)  = INTE               : 'first phi region, stack 1 ( keV )\ 
                                                "    "    "       "   2 ( keV )\
                                                "    "    "       "   3 ( keV )\
                                              second phi region, stack 1 ( keV)\
                                              second phi region, stack 1 ( keV )\
                                                     ...........                \
                                              last   phi region, stack 3 ( keV )')
      ;
 
 
 EWTR
      :      'Ecal Wire TRigger signals 
              NR=0. (GAL)\
              Number of words/module \
              Number of modules ( = 36 )'
           STATIC
 
      = (SumOdd       = INTE               : 'sum of odd planes signals ( keV )',
         SumEven      = INTE               : 'sum of even planes signals ( keV )')
      ;
 
 
 EWHT
      :      'Ecal Wire-planes HiTs 
              NR=0. (GAL)\
              Number of words/module\
              Number of modules with hits'
           STATIC
 
      = (ModuleNumbe  = INTE               : 'module number ( 1-12 ECA, 13-24 barrel\
                                              25-36 ECB )',
         Signal(45)   = INTE               : 'signal in planes (keV).Planes of stack 3
                                              multiplied by a factor 2')
      ;
 

 END ESET
 
 
 END SUBSCHEMA
 

 SUBSCHEMA EcalJULBanks
 : 'Description of EC banks'

 AUTHOR   'A. Bonissent','C.Goy','P.Janot'
 REVIEWER 'M.N. Minard'
 VERSION  '3.0'
 DATE     '04/10/94'

 DEFINE ESET

 ESDA
      :      'Ec storey data\
              Number of words/hit storey\
              Number of hit storeys'
           STATIC

      = (ThetaJ       = INTE [1,228]       : 'J (theta) index',
         FhiI         = INTE [1,384]       : 'I (phi) index',
         DepthK       = INTE [1,3]         : 'Stack number',
         MEnergy      = REAL [-1.,90.]     : 'Energy in Gev',
         SubComponent = INTE [1,5]         : 'subcomponent number',
         IOaddress    = INTE [1,221184]    : 'Add  :  encoded i,j,k address',
         DIrection    = INTE [1,25000]     : 'Ndeb :  start add in roseve',
         EcalRegion   = INTE [1,36]        : 'Region number')
         
     ;
 
 EDST
     :      'Ecal Dead SToreys \
             Number of words/storey\
             Number of dead storeys'
            STATIC

      = (ThetaJ       = INTE [1,228]       : 'J (theta) index',
         FhiI         = INTE [1,384]       : 'I (phi) index',
         DepthK       = INTE [1,3]         : 'Stack number',
         MEnergy      = REAL [-1.,90.]     : 'Energy in Gev',
         SubComponent = INTE [1,5]         : 'subcomponent number',
         IOaddress    = INTE [1,221184]    : 'Add  :  encoded i,j,k address',
         DIrection    = INTE [1,25000]     : 'Ndeb :  start add in roseve',
         EcalRegion   = INTE [1,36]        : 'Region number')
  ;

 ETCO
     :      'Ec Towers COrrections \
             Number of words/tower\
             Number of towers'
           STATIC

      =(oVerlAploss  = REAL [0.,1.]       : 'Energy correction in overlap',
        LEakageloss  = REAL [0.,1.]       : 'Energy correction for leakage')

     ;      

 ECLU
      :      'Ec Clusters \
              Number of words/cluster\
              Number of clusters'
           STATIC

      = (CHeckdeadstoreys = INTE [0,1000]  : 'Number of dead storeys',
         Energyclust4 = REAL [0.,90.]      : 'Cluster energy',
         Energy1      = REAL [0.,90.]      : 'Energy of stack 1',
         Energy2      = REAL [0.,90.]      : 'Energy of stack 2',
         Energy3      = REAL [0.,90.]      : 'Energy of stack 3',
         Radiusclust4 = REAL [0.,300.]     : 'Average cluster radius (polar coordinates)',
         Radius1      = REAL [0.,300]      : 'Average radius of stack 1',
         Radius2      = REAL [0.,300]      : 'Average radius of stack 2',
         Radius3      = REAL [0.,300.]     : 'Average radius of stack 3',
         Thetaclust4  = REAL [0.,6.3]      : 'Average cluster theta (polar coordinates)',
         Theta1       = REAL [0.,6.3]      : 'Average theta of stack 1',
         Theta2       = REAL [0.,6.3]      : 'Average theta of stack 2',
         Theta3       = REAL [0.,6.3]      : 'Average theta of stack 3',
         Fhiclust4    = REAL [0.,6.3]      : 'Average cluster phi (polar coordinates)',
         Fhi1         = REAL [0.,6.3]      : 'Average phi of stack 1',
         Fhi2         = REAL [0.,6.3]      : 'Average phi of stack 2',
         Fhi3         = REAL [0.,6.3]      : 'Average phi of stack 3',
         Sig1         = REAL [0.,999.]     : 'Shower width in stack 1',  
         Sig2         = REAL [0.,999.]     : 'Shower width in stack 2',  
         Sig3         = REAL [0.,999.]     : 'Shower width in stack 3')
      ;


 ETP1
      :      'ECAL entry point for extrapolated TPC fitted tracks \
              Number of words/track\
              Number of tracks in event'
           STATIC

      = (X3(3)        = REAL [-300.,300.]  : 'X(3) coordinate (cm, in the MRS)\
                                              at ecal entry (extrapolated)',
         P3(3)        = REAL [-1.,1.]      : 'direction cosines of momentum',
         PTotal       = REAL [0,*]         : 'Momentum',
         CHarge       = REAL -1.|0.|+1.    : 'Charge',
         ExtrapolFlag = INTE [0,1]         : 'Extrapolation flag (1 if track\
                                              should be extrapolated, 0 else)')
      ;


 EFET
      :      'ECAL exit point of tracks (coordinates) \
              Number of words/track\
              Number of tracks in event'
           STATIC

      = (X3(3)        = REAL [*,*]         : 'X(3) coordinate (cm, in the MRS)',
         P3(3)        = REAL [*,*]         : 'u,v,w',
         PTotal       = REAL [*,*]         : 'P',
         CHarge       = REAL -1.|0.|+1.    : 'Charge',
         ExtrapolFlag = INTE [0,1]         : 'Extrapolation flag (1 if track\
                                              should be extrapolated)')
      ;


 ECT1
      :      'Relation from clusters to tracks \
              Number of words/cluster \
              Number of clusters'
           STATIC

      ;


 ETC2
      :      'Relation from tracks to clusters\
              Number of words/track\
              Number of tracks'
           STATIC

      ;



 ERL3
      :      'Relation between tracks and clusters\
              Number of words/relation\
              Number of relations'
           STATIC

      ;


 ESTO
      :      'Ec STOreys geometrical data \
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
          SubcompNumb = INTE [1,3]         : 'Subcomponent number',
          ModuleNumbe = INTE [1,24]        : 'Module number',
          OverlapFlag = INTE [0,1]         : 'Flag (=1) for endcap/barrel\
                                              overlap region')
      ;

 EGID
         :    'Ecal Gamma IDentified particles\
               Number of words\
               Number of particles'
         STATIC
      = (  IFlag    = INTE [0,100]     :  'Quality flag\
                               0 : Good\
                               1 : Near a crack\
                               2 : Into a crack\
                               3 : Near an overlap\
                               4 : Into an overlap\
                              10 : Bad cluster',
           Estim1    = REAL [-10.,100.] :  'Materialisation depth',
           Estim2    = REAL [-100.,100.] : 'Longitudinal shape\
                             = -99. if uncalculated',
           Estim3    = REAL [0.,100.] :  'Compactness :\
                       Proportion of energy in 2 by 2 towers',
           Estim4    = REAL [-100.,100.] :  'Transverse shape\ 
                         (Deviation from expected trans. shape) ',
           Estim5    = REAL [0.,15.] :  'Small sigma of cluster \
                         = 0 if not calculated  ',
           Estim6    = REAL [0.,15.] :  'Large sigma of cluster \
                         = 0 if not calculted ',
           Estim7    = REAL [-2000.,2000.] : 'Third moment of cluster',
           Estim8    = REAL [0.,50.] :  'Mass of cluster (GeV)',
           NAture    = INTE [0,18] : 'Identification code from the first\
                            four estimators only (EBNEUT)\
                          ( one raw per photon in case pi0 identified) \
                             1 : Isolated gamma\
                             2 : First gamma from a multi gamma\
                             3 : First gamma from a pi0\
                             4 : Second gamma from a multi gamma\
                             5 : Second gamma from a pi0\
                            10 : Unresolved multi gamma\
                            12 : Residual e.m. energy\
                            17 : Ecal fraction of an hadron\
                            18 : Satellite',
    CorrEc    = REAL[0.,100.] : 'Corrected energy including saturation\
                                (first or second photon of the cluster)',
    THeta  = REAL[0.,3.14] : 'Polar theta angle with respect to the beam\
                                (first or second photon of the cluster) ',
    PHi    = REAL[0.,6.28] : 'Azimuthal phi angle\
                                (first or second phooton of the cluster) ')
 
         ;
 
 EGNE                                                                           
      :      'Electromagnetic Gamma Normalised Estimator\                       
              Number of words/Gamma \                                           
              Number of gamma '                                                 
           STATIC                                                               
                                                                                
                                                                                
      = (RegionCode     = INTE   : 'Region Code (defined as PECO)',             
         clusterNAture  = INTE   : 'Cluster nature code ',                      
         F4estimator    = REAL   : 'Normalised F4(fraction energy in 4 towers)',
         W1estimator    = REAL   : 'Normalised W1(1st estimator from Bulos)',
         W2estimator    = REAL   : 'Normalised W2(2nd estimator from Bulos)',
         L1estimator    = REAL   : 'Normalised L1(1st Longitudinal estimator from Ebneut)',
         L2estimator    = REAL   : 'Normalised L2(2st Longitudinal estimator from Ebneut)',
         S1estimator    = REAL   : 'Normalised S1(shape estimator \           
                                    (not yet filled)',                          
         S2estimator    = REAL   : 'Normalised S2(shape estimator \           
                                    (not yet filled)',                          
         BulosMass      = REAL   : 'Bulos mass of cluster ')                    
      ;                                                                          
                                                                                
                                                                                
EGPC
     : 'Photons from GAMPEC\
        Number of words per photon\
        Number of photons'
        STATIC
 
     = (PX              = REAL [*,*]   :'Momentum along x axis (GeV/c)',
        PY              = REAL [*,*]   :'Momentum along y axis (GeV/c)',
        PZ              = REAL [*,*]   :'Momentum along z axis (GeV/c)',
        R1              = REAL [0.,1.] :'Energy fraction in stack 1',
        R2              = REAL [0.,1.] :'Energy fraction in stack 2',
        F4              = REAL [0.,1.] :'Energy fraction in the 4 central towers',
        DMin            = REAL [0.,*]  :'Distance to the closest track (cm)',
        SToreys         = INTE [0,*]   :'NST1 + 100*NST2 + 10000*NST3\
                                         NSTi = # storeys in stack i',
        QUality         = INTE [0,*]   :'CRCK + 10*DST1 + 100*DST2 + 1000*DST3
                                         DSTi = 1 if dead storey(s) in stack i
                                         CRCK = 1 if photon in crack region')

      ;
 
                                                                                
 EGPR                                                                           
      :      'Electromagnetic Gamma(gampek) 
              Pest Row component \                                  
              number of words/pest element\                                     
              number of pest element'                                           
           STATIC                                                               
                                                                                
      = (PestRow        = INTE   : 'Row of element in PEST bank ',              
         GammaRow       = INTE   : 'Gamma row in EGPC bank ')                   
     ;                                                                          
                                                                                
                                                                                
 PGID                                                                           
      :      'Pot electomagnetic Gamma IDentification \
              Number of words/Gamma (=10) \
              Number of gamma '                                                 
           STATIC                                                               
                                                                                
                                                                                
      = (IFlag          = INTE               : 'Quality flag \
                                                0 :  Good \
                                                1 :  Near a crack \
                                                2 :  Into a crack \
                                                3 :  Near an overlap \
                                                4 :  Into an overlap \
                                               10 :  Bad cluster',                           
         DepthEstimator = REAL               : 'Estimator for depth ',                      
         CoMpactness    = REAL               : 'Compactness estimator',                     
         Moment1        = REAL               : '1st moment estimator \                       
                                                Small sigma of cluster[-10.0,15.0]',         
         Moment2        = REAL [-10.0,15.0]  : '2nd Moment estimator . Large sigma of cluster \                       
                                                = 0 if not calculated ',
         M3estimator    = REAL               : '3rd moment estimator not normalised',
         CorrectedEnergy= REAL [0.,100.]     : 'Corrected energy including saturation',
         THeta          = REAL               : 'Polar Theta angle with respect to the beam including S-shape',
         PHi            = REAL               : 'Azimuthal phi angle including S-shape correction')
       ;                                                                          
                                                                                
                                                                                
 PGPC                                                                           
      :      'Photon from GamPeC \                                              
              Number of words/Gamma (=18)\                                      
              Number of photon'                                                 
           STATIC                                                               
                                                                                
                                                                                
      = (EnergyCorrected = REAL  : 'Energy corrected for photon hypothesis',
         ThetaCorrected  = REAL  : 'Theta corrected for photon hypothesis',
         PhiCorrected    = REAL  : 'Phi corrected for photon hypothesis',
         fRactions1      = REAL  : 'Energy fraction in stack 1',
         fRactions2      = REAL  : 'Energy fraction in stack 2',
         Fraction4tower  = REAL  : 'Energy fraction in the 4 towers',
         DMin            = REAL  : 'Distance to the closest track',
         SToreys         = REAL  : 'NST1+100*NST2+10000*NST3 \
                                    Nsti = number of storeys stack i',
         QUalityflag     = INTE  : 'CRCK + 10*DST1 + 100*DST2 +1000*DST3\
                                    DSTi = 1 if dead storey stack i\
                                    CRCK = 1 if photon in crack region',
         Quality1        = REAL  : 'Quality estimator 1 for photon',
         Quality2        = REAL  : 'Quality estimator 2 for photon',
         Moment1         = REAL  : '1st moment form CLMOMS analysis',
         Moment2         = REAL  : '2nd moment form CLMOMS analysis',
         MAss            = REAL  : 'Pi0 mass estimated from CLMOMS',
         EnergyRaw       = REAL  : 'Raw energy for photon',
         ThetaRaw        = REAL  : 'Raw Theta for photon ',
         PhiRaw          = REAL  : 'Raw Phi for photon  ')
     ;                                                                          
 
    PGAC
         :    'Photons from GAMPEX (oct 94)\
               Number of words per photon \
               Number of photons'
         SIZE 1,1

         STATIC
 
         = (EnergyCorrected  = REAL [*,*]   : 'Energy corrected for photon hypothesis',
            ThetaCorrected   = REAL [*,*]   : 'Theta corrected for photon hypothesis',
            PhiCorrected     = REAL [*,*]   : ' Phi corrected for photon hypothesis',
            R1               = REAL [0.,1.] : 'Energy fraction in stack 1',
            R2               = REAL [0.,1.] : 'Energy fraction in stack 2',
            F4               = REAL [0.,1.] : 'Energy fraction in the 4 central towers',
            DistanceMinimun  = REAL [0.,*]  : 'Distance to the closest track (cm)',
            SToreyflag       = INTE  : 'NST1+100*NST2+10000*NST3 \
                                        NSTi = number of storey stack i',
            QUalityflag      = INTE  : 'CRCK+10*DST1+100*DST2+1000*DST3 \
                                        DSTi = 1 if dead storey(s) stacki\
                                        CRCK = 1 if photon in crack region',
            Q1               = REAL  : 'Quality estimator for photon 1',
            Q2               = REAL  : 'Quality estimator for photon 2',
            Moment1          = REAL  : '1st moment from CLMONS analysis',
            Moment2          = REAL  : '2nd moment from CLMONS analysis',
            MAss             = REAL  : 'Pi0 mass estmated from clmoms  ',
            EnergyRaw        = REAL  : 'Raw energy for photon',
            ThetaRaw         = REAL  : 'Uncorrected theta for photon ',
            PhiRaw           = REAL  : 'Uncorrected phi for photon ',
            ExpectedFraction = REAL  : 'Expected fraction in 4 towers',
            GeometricalCorr  = REAL  : 'Geomtrical correction ',
            ZeroSupression   = REAL  : 'Zero suppresion correction from Coradoc',
            ProbafakeeLm     = REAL  :'Probability to be a fake photon
                                       from Electromagnetic origine ',
            ProbafakeHad     = REAL  :'Probability to be a fake photon
                                       from Hadronic origine ',
            ParentNumber     = INTE  : 'Row number(PGAC) of parent giving a fake photon',
            FAkequality      = INTE  : 'Flag for fake determination')
 
         ;
 
 END ESET

 DEFINE RSET

   (ESDA [0,1] -> [1,*] ECLU)
     : 'Index of cluster to which the storey belongs';
 
   (ESDA [0,1] -> [0,1] EDST)
     : 'Index of dead storey in same position';

   (EDST [0,1] -> [0,1] EDST)
     : 'Index of next dead storey with the same reduced encoded (I,J):"banana"';

   (EDST [0,1] -> [0,1] ESDA)
     : ' Index  in ESDA of rescued storeys ';
 
   (ESDA [0,1] -> [0,1] ESDA)
     : 'Index of the Next Storey of the same cluster';

   (ECLU [0,1] -> [0,1] ESDA)
     : 'Index of the first storey in a cluster ';

   (ERL3 [0,1] -> [0,1] ERL3)
     : 'Address of the next relation for the same cluster';

   (ERL3 [0,1] -> [0,1] ECLU)
     : 'Index of cluster';

   (ERL3 [0,1] -> [0,1] ETP1)
     : 'Index of track ';

   (ECT1 [1,1] -> [1,*]ERL3)
     : 'Address (in ERL3) of first (cluster to track) relation for cluster';

   (ETC2 [1,1] -> [1,*]ERL3)
     : 'Address (in ERL3) of first (cluster to track) relation for track';

   (EGID[1,1] -> [1,*]PECO)
         : 'pointeur to PECO bank';

   (EGNE[1,1] -> [1,*]PECO)
         : 'pointeur to PECO bank';

   (EGPC[1,1] -> [1,*]PECO)
         : 'pointeur to PECO bank';

   (PGID[1,1] -> [1,*]PECO)
         : 'pointeur to PECO bank';

   (PGPC[1,1] -> [1,*]PECO)
         : 'pointeur to PECO bank';

   (PGAC[1,1] -> [1,*]PECO)
         : 'pointeur to PECO bank';

 END RSET

 END SUBSCHEMA


 SUBSCHEMA EobjJULBanks
 : 'Description of EcObject banks'

 AUTHOR   'F. Etienne','A. Bonissent'
 VERSION  '1.3'
 DATE     '04/07/90'

 DEFINE ESET


 CRL3
      :      'Relation between tracks, Ecob and HcClusters\
              Number of words/relation\
              Number of relations'
           STATIC

      = ( Pt       = REAL [-45.,45.]   : 'Transverse momentum')
      ;

 CCT1
      :      'Relation from Hclu to tracks and Ecob\
              Number of words/cluster \
              Number of clusters'
           STATIC

      ;


 CTC2
      :      'Relation from tracks to Hclu and Ecob\
              Number of words/track\
              Number of tracks'
           STATIC

      ;
 COCR
      :      'Relation from Ecob to HcClusters and tracks\
              Number of words/EcObject\
              Number of EcObjects'
           STATIC

      ;
 ETCK
      :      'Ecob track\
              Number of words/track\
              Number of tracks'
           STATIC

      ;
ECOB
      :      'Ec Object\
              Number of words/track\
              Number of tracks'
           STATIC

      = ( Energy(3)    = REAL [0.,90.]     : 'Deposited energy in stacks 1,2,3',
          EnergyGlo    = REAL [0.,90]      : 'Total energy for the cluster',
          RegionCode   = INTE [0,999]      : 'type of region the ecob is in\
                                              ( barrel, ecap, overlap...)',
          IdenT(2)     = REAL [*,*]        : 'Identification Parameters',
          CrackVar(3)  = REAL [*,*]        : 'Variables specific for crack\
                                              analysis',
          NHyp         = INTE [0,7]        : 'Number of hypotheses',
          NTrak        = INTE [0,31]       : 'Number of associated tracks',
          PhotonFlag   = INTE [0,15]       : 'Flag to tag small cluster ')
      ;
 EBOS
      :      'Ecob to esda relation\
              Number of words/relation\
              Number of tracks'
           STATIC

      = ( Fraction     = REAL [0.,1.]      : 'Fraction of esda energy\
                                              which belongs to this ecob')

      ;
 EHYP
      :      'Ecob Hypothesis\
              Number of words/hypothesis\
              Number of hypotheses'
           STATIC

      = ( Type         = INTE [1,*]        : 'type of hypothesis (coded)',
          Energy       = REAL [-45.,45.]   : 'Corrected Energy',
          Radius       = REAL [180.,400.]  : 'Line of flight polar radius',
          Theta        = REAL [0.,3.15]    : 'Line of flight theta angle',
          Phi          = REAL [-6.3,+6.3]  : 'Line of flight phi angle')

      ;
 EPAR
      :      'Particle associated to an Ecob hypothesis\
              Number of words/particle\
              Number of particles'
           STATIC

      = ( Type         = INTE [1,*]        : 'type of particle',
          Theta        = REAL [0.,3.15]    : 'Line of flight theta angle',
          Phi          = REAL [-6.3,+6.3]  : 'Line of flight phi angle',
          DTheta       = REAL [0.,3.15]    : 'theta deviation',
          DPhi         = REAL [-6.3,+6.3]  : 'phi deviation',
          IdenT(2)     = REAL [*,*]        : 'Identification Parameters',
          Energy       = REAL [0.,45.]     : 'Energy of the particle')

      ;
  
 
  EMIP
      :      'M.I.P identified tracks(NR=0)\
              Number of words/track\
              Number of M.I.P tracks'
           STATIC
      ; 

 EIGA
      :      'Extracted photons using EIDT estimators\
              Number of words/extracted photon\
              Number of extracted photons'
           STATIC

      = ( Energy       = REAL [0.,45.]     : 'Energy of the particle',
          Teta         = REAL [0.,3.15]    : 'Line of flight theta angle',
          Phi          = REAL [-6.3,+6.3]  : 'Line of flight phi angle')
      ;


 EGMA
      :      'Extracted photons from complex charged clusters\
              Number of words/extracted photon\
              Number of extracted photons'
           STATIC

      = ( Energy       = REAL [0.,45.]     : 'Energy of the particle',
          Teta         = REAL [0.,3.15]    : 'Line of flight theta angle',
          Phi          = REAL [-6.3,+6.3]  : 'Line of flight phi angle')
      ;


 EIBR
      :      'Extracted Bremstrahlung photons \
              Number of words/extracted photon\
              Number of extracted photons'
           STATIC

      = ( Energy       = REAL [0.,45.]     : 'Energy of the particle',
          Teta         = REAL [0.,3.15]    : 'Line of flight theta angle',
          Phi          = REAL [-6.3,+6.3]  : 'Line of flight phi angle',
          Px           = REAL [*,*]        : 'electron impulsion at start point',
          Py           = REAL [*,*]        : 'electron impulsion at start point',
          Pz           = REAL [*,*]        : 'electron impulsion at start point')

      ;


 END ESET

 DEFINE RSET

   (CRL3 [0,1] -> [0,1] CRL3 BY NE)
     : 'Address of the next relation for the same Ecob';

   (CRL3 [0,1] -> [0,1] CRL3 BY NH)
     : 'Address of the next relation for the same HcCluster';

   (CRL3 [0,1] -> [0,*] CCT1)
     : 'Index of HcCluster';

   (CRL3 [0,1] -> [0,*] COCR)
     : 'Index of EcObject';

   (CRL3 [0,1] -> [0,*] CTC2)
     : 'Index of Track';

   (CCT1 [0,1] -> [0,1]CRL3)
     : 'Address (in CRL3) of first (Track to HcCluster) relation for HcCluster';

   (CTC2 [0,1] -> [0,1]CRL3)
     : 'Address (in CRL3) of first (Track to HcCluster) relation for Track';

   (COCR [0,1] -> [0,1]CRL3)
     : 'Address (in CRL3) of first (Track to HcCluster) relation for EcObject';

   (ECOB [0,1] -> [0,1]ETCK)
     : 'Index of first track for an EcObject';

   (ETCK [0,1] -> [0,1]ETCK)
     : 'Index of next track for the same EcObject';

   (ETCK [0,1] -> [0,*]ECOB)
     : 'Index of EcObject for a Track';

   (ECOB [0,1] -> [0,1]EHYP)
     : 'Index of first hypothesis for an ecObject';

   (EHYP [0,1] -> [1,*]ECOB)
     : 'Index of EcObject for an EcObjectHypothesis';

   (EHYP [0,1] -> [0,1]EHYP)
     : 'Index of next hypothesis for the same EcObject';

   (EHYP [0,1] -> [0,1]EPAR)
     : 'Index of first Particle for an EcObjectHypothesis';

   (EPAR [0,1] -> [1,*]EHYP)
     : 'Index of Ehyp for an EcObjectParticle';

   (EPAR [0,1] -> [0,1]EPAR)
     : 'Index of next EcParticle for the same EcHypothesis';

   (EBOS [0,1] -> [1,*]ECOB)
     : 'Index of Ecob ';

   (EBOS [0,1] -> [0,1]EBOS)
     : 'Index of next relation for the same EcObject';

   (ECOB [0,1] -> [0,1]EBOS)
     : 'Index of first relation to esda for an EcObject';

   (EBOS [0,1] -> [0,*]ESDA)
     : 'Index of Esda';

   (CCT1 [0,1] -> [0,1]CALO)
     : 'Calobject number';

   (CTC2 [0,1] -> [0,1]CALO)
     : 'Calobject number';

   (COCR [0,1] -> [0,1]CALO)
     : 'Calobject number';

   (EIGA [0,1] -> [0,1] PEOB)
     : 'Index of cluster to which the photon belongs';

   (EGMA [0,1] -> [0,1] PEOB)
     : 'Index of cluster to which the photon belongs';

   (EIBR [0,1] -> [0,1] PEOB)
     : 'Index of cluster to which the photon belongs';

   (EIBR [0,1] -> [0,1] EIDT)
     : 'Index of track to which the photon belongs';

   (EMIP [0,1] -> [0,1] FRFT)
     : 'Address of the track in FRFT';

 END RSET

 END SUBSCHEMA
 
 
 SUBSCHEMA EcalPOTBanks
 
 AUTHOR   'A. Putzer,D. Pallin,M.N. Minard'
 REVIEWER 'F.Ranjard'
 VERSION  '3.3'
 DATE     '10/05/95'
 
 DEFINE ESET
 
 
 PEWI
      :      'Ecal Wire plane Digits.
              NR=0 (POT)\
              Number of words/module\
              Number of modules above threshold'
           STATIC
 
      = (ModuleNumbe  = INTE [1,36]        : 'module number ( 1-36 )',
         PlanDig(45)  = INTE               : 'Plane digit ( keV )',
         SSumval(8)   = INTE               : 'Sum sampling value (ADC count)',
         TIME0        = INTE               : 'T0 time crossing from ECAL wires(ns)')
       ;
 
 
 PEWD
      :      'Ecal Wire plane Digits.
              NR=0. (POT)\
              Number of words/module\
              Number of modules above threshold'
           STATIC
 
      = (ModuleNumbe  = INTE [1,36]        : 'module number ( 1-12 ECA, 13-24 barrel\
                                              25-36 ECB )',
         PlanDig(45)  = INTE [-200,20000]  : 'Plane digit ( MeV )',
         TIME0        = INTE [-2000,2000]  : 'T0 time crossing from ECAL wires')
       ;
 
 PEWH
      :      'Ecal Wire Header bank.
              NR=0. (POT)\
              Number of words/module\
              Number of modules (36)'
           STATIC
 
      = (SumDig       = INTE [-1000,100000]    : 'digital sum ( MeV )')
       ;
 
 
 PRPW
      :      'Pot bank for Relation Pad/Wire\
              Number of words/Relation\
              Number of Relation'
           STATIC
 
      = (PecoNumber     = INTE   : 'Peco number ',
         ModuleRange    = INTE   : 'Module range in PEWI ',
         ModuleNumber   = INTE   : 'Wire Module number  ',
         F1             = REAL   : 'Fraction of energy contained\
                                    in this module in stack1 (%)',
         F2             = REAL   : 'Fraction of energy contained\
                                    in this module in stack2 (%)',
         ER             = REAL   : 'Raw energy contained in module\
                                    for PECO (Gev)')
     ;
 
 PWEI
      :    'Pot Wire Energy Information(former PEWI)\
               Number of words per Module \
               Number of modules'
         SIZE 1,36
         STATIC
 
      = (ModuleNumber    = INTE [1,36] : 'Module Number [1,36]',
         PD(45)          = INTE [*,*]  : 'Plane Digit for each of the 45 planes (kev)',
         SS(16)          = INTE [*,*]  : 'Sum Sampling value',
         TIme            = INTE [*,*]  : 'T0 time from Ecal wires')
      ;
 
 
 END ESET
 
 END SUBSCHEMA
 

