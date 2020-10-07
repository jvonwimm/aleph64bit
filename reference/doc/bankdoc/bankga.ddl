 SUBSCHEMA StatusGALBanks
  : 'GALEPH status banks'

 AUTHOR   'A.Putzer'                                                           
 REVIEWER 'F.Ranjard'                                                          
 VERSION  '1.6'                                                                 
 DATE     '11/10/91'
                                                                                
 DEFINE ESET                                                                    
                                                                                
 ASEV                                                                           
       :     'Aleph Simulation EVent header (NR=0)\                             
              Number of words/process\                                          
              Number of processes'                                              
           STATIC                                                               
                                                                                
       = (RandomGener(3)      = INTE   : 'root of the random generator\         
                                          for a given process')                 
                                                                                
       ;                                                                        
                                                                                
 ASIM
       :     'Aleph Simulation date (NR=0)\
              Number of words/date\
              Number of dates'
           STATIC

       = (YearMonth       = INTE [8800,9912] :'period number given as yymm\
                                              to be simulated')
       ;

 ATIT                                                                           
       :     'galeph run TITl (NR=0)\                                           
              =1\                                                               
              Number of 4char.-words'                                           
           STATIC                                                               
                                                                                
       = (RunTitlename    = CHA4     : 'run title')                             
                                                                                
       ;                                                                        
                                                                                
 AKIN                                                                           
       :     'galeph KINematic parameters (NR=0)\                               
              Number of parameters/kine\                                        
              Number of kine=1'                                                 
           STATIC                                                               
                                                                                
       = (KineTitle          = CHA4     : 'kinematic name',                     
          KineParam(8)       = REAL     : 'kinematic parameters')               
                                                                                
       ;                                                                        
                                                                                
 ACUT                                                                           
       :     'galeph tracking CUTs (NR=0)\                                      
              Number of words/medium\                                           
              Number of media'                                                  
           STATIC                                                               
                                                                                
       = (MediumName       = CHA4    : 'medium name',                           
          GammaCut         = REAL    : 'cut on gammas',                         
          ElectronCut      = REAL    : 'cut on e+e-',                           
          HadronCut        = REAL    : 'cut on charged hadrons',                
          NeutralCut       = REAL    : 'cut on neutral',                        
          MuonCut          = REAL    : 'cut on muons')                          
                                                                                
       ;                                                                        
                                                                                
 AFID                                                                           
       :     'galeph FIDucial parameters(NR=0)\                                 
              Number of words/row\                                              
              Number of rows=1'                                                 
           STATIC                                                               
                                                                                
       = (AlephmaximumRadius = REAL   : 'ALEPH maximum radius',                 
          AlephmaximumZ      = REAL   : 'ALEPH maximum Z',                      
          alephMagneticField = REAL   : 'ALEPH magnetic field',                 
          lepBeamEnergy      = REAL   : 'LEP beam energy')                      
                                                                                
       ;                                                                        
                                                                                
 AJOB                                                                           
       :     'galeph JOB conditions (NR=0)\                                     
              Number of words/job\                                              
              Number of jobs=1'                                                 
           STATIC                                                               
                                                                                
       = (BremsstrahlungMode    = INTE    : 'Bremsstrahlung mode',              
          RunningMode           = INTE    : 'Running mode : \                   
                                             rdst*100+fast*10+had.pack',        
          GetInput              = INTE    : '=1 if input is read in',           
          SaveOutput            = INTE    : '=1 if output is saved',            
          SensitiveDetector     = INTE    : 'detectors set (1bit)',            
          GeometriComponent     = INTE    : 'present components (1bit)',        
          JobDate               = INTE    : 'date of the job',                  
          JobTime               = INTE    : 'time of the job',                  
          GalephVersion         = INTE    : 'galeph version * 10',              
          AlephlibVersion       = INTE    : 'alephlib version * 10',            
          FyxxTrackflag         = INTE    : 'FYXX drop track flag',             
          FyxxShowerflag        = INTE    : 'FYXX drop shower flag',            
          FyxxmomentumCut       = INTE    : 'FYXX momentum cut (MeV)',          
          DafVersion            = INTE    : 'ADBSCONS version',                 
          DafDate               = INTE    : 'ADBSCONS creation date',           
          TpcsimVersion         = INTE    : 'TPCSIM version * 100',             
          CorrectionVersion     = INTE    : 'corr.file version * 100',
          GeantversionNumber    = INTE    : 'GEANT version * 100') 
       ;                                                                        
                                                                                
 ARUN                                                                           
       :     'galeph RUN conditions (NR=0)\                                     
              Number of words/detector\                                         
              Number of detectors'                                              
           STATIC                                                               
                                                                                
       =  (RunCondition(10)  = INTE  : 'detector run conditions')               
                                                                                
       ;                                                                        
                                                                                
 APRO                                                                           
       :     'galeph PROcess conditions (NR=0)\                                 
              Number of conditions/process\                                     
              Number of processes'                                              
           STATIC                                                               
                                                                                
       = (ProcessFlag      = INTE    : '=1 if process is executed',             
          RandomGener(3)   = INTE    : 'random generator root')                 
                                                                                
       ;                                                                        
                                                                                
 END ESET

 END SUBSCHEMA

 
 SUBSCHEMA InternalGALBanks
  : 'Internal GALEPH  banks'

 AUTHOR   'F.Ranjard'                                                           
 REVIEWER 'L.Silvestris'                                                          
 VERSION  '1.6'                                                                 
 DATE     '13/02/95'                                                            
                                                                                
 DEFINE ESET                                                                    
                                                                                
 CAPA                                                                           
       :     'CAlorimetry PArameters (NR = Track number)\                       
              Number of parameters/geantino\                                    
              Number of geantinos=1'                                            
           STATIC                                                               
                                                                                
       = (ShowerType       = INTE   : 'shower type (elec=1,phot=2,had=3)',      
          ParametrizType   = INTE   : 'parametrization type (off=1,on=2)',      
          TotalEnergy      = REAL   : 'total energy',                           
          geantinoTotalX0  = REAL   : 'total rad.length seen by geantino',      
          geantinoTotalL0  = REAL   : 'total abs.length seen by geantino',      
          EMenergypart     = REAL   : 'e.m. energy',                            
          EAlphaparameter  = REAL   : 'Alpha e.m. parameter',                   
          EBetaparameter   = REAL   : 'Beta e.m. parameter',                    
          Alphaminus1      = REAL   : 'Alpha - 1.',                             
          ERadialparameter = REAL   : 'Radial e.m. parameter',                  
          EFactor          = REAL   : 'R0 / Z',                                 
          EDepthconstant   = REAL   : 'Depth shower size dependance,cst term',  
          EdepthZ2         = REAL   : 'Depth shower,Z**2 term',                 
          HadrEnergy       = REAL   : 'hadronic energy',                        
          HAlphaparameter  = REAL   : 'Alpha hadronic parameter',               
          HBetaparameter   = REAL   : 'Beta hadronic energy',                   
          HDray            = REAL   : 'Hadronic dray',                          
          HPuis            = REAL   : 'Hadronic puis',
          hGAmma           = REAL   : 'free parameter',
          SmaXlr           = REAL   : 'Radiation length tracked so far',
          SmaxlA           = REAL   : 'Absorption length tracked so far',
          emNAme           = REAL   : 'Last volume name')                          
                                                                                
       ;                                                                        

 HCSE                                                                           
       :     'working bank JDHCSE\                                              
              number of words/track segment\                                    
              number of track segments'                                         
          STATIC                                                                
                                                                                
       = ( TubeNumber        = INTE  : 'Tube # in a layer',                     
           LayerNumber       = INTE  : 'Layer # in a module',                   
           ModuleNumber      = INTE  : 'Module #',                              
           PortionNumber     = INTE  : 'Portion #',                             
           TubeAddress       = INTE  : 'Tube address:\                          
                                        tube# + layer#*1000 + mod#*10000\       
                                        +portion#*10000000',                    
           TubeXposition     = REAL  : 'tube x-position ',           
           TubeYposition     = REAL  : 'tube y-position (wire dir.)',           
           ProjonWiredir     = REAL  : 'proj.on wire direction\                 
                                        if parametrization = spot energy',      
           XinAlephsystem    = REAL  : 'X in Aleph system',                     
           YinAlephsystem    = REAL  : 'Y in Aleph system',                     
           ZinAlephsystem    = REAL  : 'Z in Aleph system',                     
           TRacknumber       = INTE  : 'track # = KINE bank#',                  
           HiTnumber         = INTE  : 'hit # = JDHCHI row#',                   
           FiredTubenumber   = INTE  : 'fired tube # = HWHT row#',              
           FiredStoreynumber = INTE  : 'fired storey # = HTHT row#')            

         ;                                                                      
                                                                                
 HCHI                                                                           
         : 'working bank JDHCHI\                                                
            Number of words/hit\                                                
            Number of hits'                                                     
          STATIC                                                                
                                                                                
         = (TrackSegmentnumber = INTE  : 'track seg. # = JDHCSE row#',          
            SegmentYposition   = REAL  : 'seg.position in wire dir.',           
            SegmentProjection  = REAL  : 'seg.projection on wire dir.\          
                                          = spot energy in case of param.',     
            NumberofStreamers  = INTE  : '# of generated streamers\             
                                          = 1 in case of parametrization')      
         ;                                                                      
                                                                                
 HCTH                                                                           
       :    'working bank JDHCTH\                                               
             Number of words/track\                                             
             Number of tracks entering HCAL'                                    
          STATIC                                                                
                                                                                
       = (TrackNumber     = INTE    : 'Track number',                           
          ParticleNumber  = INTE    : 'Particle number',                        
          EnergyHadcalo   = REAL    : 'Energy at HC border',                    
          EnergyVertex    = REAL    : 'Energy at vertex',                       
          EnergyMeasured  = REAL    : 'Energy measured',                        
          VertexFlag      = INTE    : 'Vertex localisation flag\                
                                       =0 if vertex inside HCAL\                
                                       =1 if vertex outside HCAL')              
       ;                                                                        
                                                                                
 HCVL
       :     'HCAL VoLume parameters (NR=geantino track#)\
              Number of words/geantino\
              Number of geantinos(=1)'
           STATIC

       = (UpdateHcvlcontent   = INTE   : 'Update HCVL flag\
                                         =0 if HCVL is reset\
                                         =1 if HCVL is updated',
          TracklengthinAbslen = REAL   : 'track length in absorption\
                                          length units',
          TracklengthinRadlen = REAL   : 'track length in radiation\
                                          length units',
          TrackLength         = REAL   : 'track length',
          AbslenVariation     = REAL   : 'variation in absorption length',
          RadlenVariation     = REAL   : 'variation in radiation length',
          TrackElement(27)    = REAL   : 'track element')

       ;   
 VOLU                                                                           
       :     'VOLUme names (NR=0)\                                              
              Number of words/volume\                                           
              Number of sensitive volumes'                                      
           STATIC                                                               
                                                                                
       = (SensitiveVolume  = CHA4    : 'Sensitive volume name')                 
                                                                                
       ;                                                                        
                                                                                
 IMPA                                                                           
       :     'IMPAct at entrance of detectors                                   
              (NR = Track number)\                                              
              Number of parameters\                                             
              Number of detectors seen by this track'                           
           STATIC                                                               
                                                                                
      = (NamDetector  = CHA4           : 'Name of seen detector',               
         ImpactX      = REAL           : 'impact X position',                   
         ImpactY      = REAL           : 'impact Y position',                   
         ImpactZ      = REAL           : 'impact Z position',                   
         UDirection   = REAL           : 'u direction of track',                
         VDirection   = REAL           : 'v direction of track',                
         WDirection   = REAL           : 'w direction of track',                
         ENergy       = REAL           : 'Energy of the track\                  
                                          at this point')                       
      ;                                                                         
                                                                                
 END ESET                                                                       
                                                                                
 END SUBSCHEMA                                                                  
                                                                                
                                                                                
 SUBSCHEMA EventKINGALBanks
 : 'Event generator interface banks'

 AUTHOR 'F.Ranjard,B.Bloch,J.Boucrot,A.Waananen'
 REVIEWER 'B.Bloch'
 VERSION '2.1'
 DATE    '1/12/00'

 DEFINE ESET

 KREF
      :      'Kingal input REFerence parameters set.
              (NR=KINGAL setup code)'
           STATIC

      = (TeXtcards              = CHA4        : 'reference input cards ')
      ;

 KCAR
      :      'Kingal job input CARDs (NR=0)'
           STATIC

      = (TeXtcards              = CHA4        : 'job input cards ')
      ;
          
 KEVH
      : 'Event generator status(NR=0)\
         Number of words/event\
         Number of event (=1)'
       STATIC

      =(firstRandomNu  = INTE        : '1st random number',
        NumberofTrac   = INTE        : '# of tracks generated',
        NumberofVert   = INTE        : '# of vertices generated',
        ProcessId      = INTE        : 'process identification',
        WeighT         = REAL        : 'weight',
        SeconRandomnu  = INTE        : '2nd random number',
        ThirdRandomnu  = INTE        : '3rd random number')
      ;

 KMAR       : 'Random generator status(NR=0)\
         Number of words/sequence\
         Number of sequences '
       STATIC

      =(firstRandomNu  = INTE[0,*]        : '1st random seed',
        SeconRandomnu  = INTE[0,*]        : '2nd random seed',
        ThirdRandomnu  = INTE[0,*]        : '3rd random seed')
      ;

 KJOB
       :     'Kingal JOB conditions (NR=0)\
              Number of words/job\
              Number of jobs=1'
           STATIC

       = (JobDate               = INTE    : 'date of the job',
          JobTime               = INTE    : 'time of the job',
          AlephlibVersion       = INTE    : 'alephlib version * 10',
          DatabaseVersion       = INTE    : 'database version ',
          DatabaseChange        = INTE    : 'Database last date of change')
       ;

 KLIN
      : 'PARTicle # to generator # relation(NR=0)\
         Number of words / PARTicle #\
         Number of PARTicles'
       STATIC

      =(GeneratorNum   = INTE        : 'generator particle #')
      ;
                                                              
 KHIS
      : 'History of the event(NR=0)\
         Number of words/generated particle\
         Number of generated particles'
       STATIC

      =(HistoryCode    = INTE        : 'history code (generator
                                       dependent)')
      ;

 KZFR
      : 'History of the event fragmentation(NR=0)\
         Number of words/generated particle\
         Number of generated particles'
       STATIC

      =(EnergyFraction = REAL        : 'energy fraction taken by
                                       the heavy meson or baryon)')
      ;

 KRAN
      : 'Event generator random seed run header(NR=0)\
         Number of words/sequence\
         Number of sequences'
       STATIC

      =(FirstseqSeed   = INTE[0,30000] : '1st seed of sequence ',
        SecondseqSeed  = INTE[0,30000] : '2nd seed of sequence ')
      ;

 KRUN
      : 'Event generator run header(NR=0)\
         Number of words/run\
         Number of runs (=1)'
       STATIC

      =(GeneratorId    = INTE        : 'generator identifier',
        NOtrackingmark = INTE        : 'NOtracking marker word',
        RunTitle(12)   = CHA4        : 'Run title',
        FirstlabSeed   = INTE        : '1st lab. seed',
        SecondlabSeed  = INTE        : '2nd lab. seed')
      ;

 KLUN
      : 'JETSET Event generator run header(NR=0)\
         Number of words/row\
         Number of rows (=1)'
       STATIC

      =(VersionNumber      = INTE : 'JETSET version number as 703 for 7.3',
        LastModification   = INTE : 'Last modification date as YYMMDD'  )
      ;

 KINE
      :  'KINEmatical track info (NR = Track number)\
          header length(=lhd)\
          # of parameters(=lpk)\
          # of vertices on the track(=nvx)'
        STATIC

      = (PX             = REAL          : 'Px',
         PY             = REAL          : 'Py',
         PZ             = REAL          : 'Pz',
         MAss           = REAL          : 'Mass',
         ParticleNumb   = INTE          : 'particle #',
         VertexNumlist  = INTE          : 'list of vertex #s\
                                           from word(lhd+lpk+1)\
                                           to word(lhd+lpk+nvx)')
      ;

 KW4F
      :       "Kinematical info for KORLW W's 4Fermions and ISR (NR = 0)\
              Number of words per particle\
              Number of particles   " 
           STATIC

      = (PX             = REAL          : 'Px',
         PY             = REAL          : 'Py',
         PZ             = REAL          : 'Pz',
         MAss           = REAL          : 'Mass',
         ParticleNumb   = INTE          : 'Jetset particle code ')

      ;

 KORL
       :      'KORAL bank for Tau decays (NR=0)\
               Number of words per run\
               Number of runs   '
           STATIC

       = (VerNum       = REAL[0.00,*]   : 'Version number of the generator',
          BranRat(25)  = REAL[0.0000,*] : 'Branching Ratios of the various
          modes')

       ;

 KPOL
       :      'POLArisation bank          (NR=0)\
               Number of words per polarized particle\
               Number of polarized particles   '
           STATIC

       = (KineIdent    = INTE [-10,*]   : 'KINE number of the track',
          HelictyX     = REAL[*,*]      : 'Helicity or polarization xcomponent',
          HelictyY     = REAL[*,*]      : 'Helicity or polarization ycomponent',
          HelictyZ     = REAL[*,*]      : 'Helicity or polarization zcomponent')

       ;

 KSHO
            :  'parton SHOwer bank (NR=0)\
                number of words/shower\
                number of partons showers'
           STATIC

       = (KineIdent     = INTE[1,*]  : 'KINE number of shower',
          Kineiden1     = INTE[1,*]  : 'KINE number of first parton of shower',
          Kineiden2     = INTE[1,*]  : 'KINE number of last parton of shower',
          ColRecflag    = INTE[0,*]  : 'Color Reconnection Flag I=done in\
                                       scheme I, 0=notdone')

       ;

 KVOL
      :  'Kinematics vertex VOLume names(NR=0)\
          number of words/vertex\
          number of vertices'
       STATIC

      = (vertexVolumeName = CHA4     : 'vertex volume name',
         VertMechanismname= CHA4     : 'vertex mechanism name' )

      ;
                              
 VERT
      :  'VERTex information (NR = Vertex Number)\
          header length (=lhd)\
          # of parameters (=lpv)\
          # of outgoing tracks (=ntr)'
       STATIC

     = (VertexX              = REAL     : 'vertex X position',
        VertexY              = REAL     : 'vertex Y position',
        VertexZ              = REAL     : 'vertex Z position',
        Timeofflight         = REAL     : 'time of flight',
        IncomingTracknum     = INTE     : 'incoming track #',
        OutgoingTracknumlist = INTE     : 'outgoing track #s\
                                           from word(lhd+lpv+1) \
                                           to word(lhd+lpv+ntr)')
      ;

 KXME
      : 'Excalibur Matrix Elements |A|^2 = |Ap|^2 + |Ar|^2 + int (NR=0)\
          number of words/process class\
          number of process classes'
       STATIC

      = (ProcessClass     = INTE[0,*] : 'Process Class:\
                                         0 = Pairing , 1 = WW \
                                         2 = ZZ      , 3 = W e nu \
                                         4 = Z e+e- ,  5 = Z nu nu \
                                         6.. = Others',
         AmplitudeProcess = REAL[0,*]  : 'Amplitude of Diagrams in Process Class
         squared',
         AmplitudeRest    = REAL[0,*]  : 'Amplitude of Remaining diagrams
         squared',
         InterferenceTerm = REAL[0,*]  : 'Interference Term')
      ;

 KWTK
      : 'Korl09 WeighTs Kalinowski (NR=0)\
          number of words/process\
          number of process'
       STATIC

      = (Weight1  = REAL[0,*] : 'Weight 1 Standard Model No Anomalous Couplings\
                                          Delta_kappa_gamma = 0.\
                                          Lambda_gamma = 0.',
         Weight2  = REAL[0,*] : 'Weight 2 Anomalous Couplings\
                                          Delta_kappa_gamma = -10.\
                                          Lambda_gamma = 0.',
         Weight3  = REAL[0,*] : 'Weight 3 Anomalous Couplings\
                                          Delta_kappa_gamma = +10.\
                                          Lambda_gamma = 0.',
         Weight4  = REAL[0,*] : 'Weight 4 Anomalous Couplings\
                                          Delta_kappa_gamma = 0.\
                                          Lambda_gamma = -10.',
         Weight5  = REAL[0,*] : 'Weight 5 Anomalous Couplings\
                                          Delta_kappa_gamma = 0.\
                                          Lambda_gamma = +10',
         Weight6  = REAL[0,*] : 'Weight 6 Anomalous Couplings\
                                          Delta_kappa_gamma = +10.\
                                          Lambda_gamma = +10.')
      ;

 KWGT
      : 'Kingal event partial WeiGhTs (NR=0)\
          number of words/weight\
          number of weights'
       STATIC
                                                                                
      = (WeightNumber  = INTE[0,*] : 'Weight number ', 
         WeightValue   = REAL[0.000,*] : 'Weight value')

      ;

 KPAR 
      : 'Kingal run parameters setup (NR=0)\
          number of words/setup\
          number of setups'
       STATIC

      = (ParameterValue   = REAL[*,*] : 'Weight value')

      ;
          
 KSEC
      : 'Kingal event generator production cross-SECtion (NR=0)\
          number of words/process\
          number of processes'
       STATIC

      = (GeneratorId  = INTE[0,*] : 'Generator identifier ',
         GeneratorVn  = INTE[0,*] : 'Generator version',
         GenNumofevts = INTE[0,*] : 'number of generated events',
         AccNumofevts = INTE[0,*] : 'number of accepted events',
         GeneratedXsec= REAL[0.000,*] : 'generated cross-section',
         GeneratedErro= REAL[0.000,*] : 'generated cross-section error',
         AcceptedXsec = REAL[0.000,*] : 'accepted cross-section',
         AcceptedErro = REAL[0.000,*] : 'accepted cross-section error')
                                        
      ;  
 END ESET

 END SUBSCHEMA
