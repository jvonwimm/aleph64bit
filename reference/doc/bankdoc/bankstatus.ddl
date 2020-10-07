  SUBSCHEMA StatusJULBanks

 AUTHOR   'J.Knobloch,E.Lancon'
 REVIEWER 'S.Wasserbaech'
 VERSION  '2.5'
 DATE     '19/09/95'

 DEFINE ESET

RREP
      :      'JULIA error REPorting\
              number of words/error code\
              number of error codes reported'
           STATIC

      = (MessageCode   = INTE [1,*] :  'Code for the error message\
                                       (unique within the subroutine)',
         NumberCalls   = INTE [1,*] :  'Number of times that this\
                                       error was encountered',
         Byte1         = INTE [*,*] :  'First byte of the subroutine\
                                       name where error was encountered',
         Byte2         = INTE [*,*] :  'Second byte of subroutine name')
       ;

REVH
      :      'Reconstructio EVent Header\
              Number of Words\
              Number of Headers(=1)'
           STATIC

      = (DetectorStatus   = INTE [*,*] :  'Detector status bits',
         FatalErr(2)      = INTE [*,*] :  'Fatal error bits',
         NonfatalErr(2)   = INTE [*,*] :  'Non-fatal error bits',
         SelectionBits    = INTE [*,*] :  'Selection bits',
         TIme             = REAL [0.,*] :  'Time for reconstruction',
         ReadoutBits(2)   = INTE [*,*] :  'Readout Bits, shows existence\
                                           of detector banks in raw data\
                                            bit 0 - VDET\
                                            bit 1 - ITC\
                                            bit 2 - TPC pad\
                                            bit 3 - TPC wire\
                                            bit 4 - ECAL pad\
                                            bit 5 - ECAL wire\
                                            bit 6 - HCAL pad\
                                            bit 7 - HCAL wire\
                                            bit 8 - MUON\
                                            bit 9 - SATR\
                                            bit 10- LCAL pad\\
                                            bit 11- LCAL wire\
                                            bit 12- BCAL\
                                            bit 13- Trigger\
                                            bit 14- BOM\
                                            bit 15- SICAL',
         EdirClass        = INTE [0,*] :  'EDIR class word')
                                          
        ;

JBER
       :     'Corrupted Bank (NR = sequence in event)'
           STATIC

      = (Content      = INTE           : 'Content of the corrupted bank')
      ;
 
JEST
      :      'BOS error status\
              Number of Words\
              Number of banks'
           STATIC

      = (ErrorType        = INTE [1,4] :  'Error type\
                                             = 1 Name corrupted and fixed\
                                             = 2 Name corrupted, not fixed\
                                             = 3 Length corrupted, and fixed\
                                             = 4 Format corrupted',
         OrigName         = INTE [*,*] :  'Original Bank name',
         NewName          = INTE [*,*] :  'New bank name, JBER for type=2',
         OrigLength       = INTE [*,*] :  'Original length',
         CorrLength       = INTE [0,*] :  'Corrected length',
         PrecName         = INTE [*,*] :  'Name of previous bank',
         BosPos           = INTE [*,*] :  'Postion of corrupted bank in record'
        )
                                          
        ;

 JCON : 'JULIA Constants.
         NR = run number \
         Number of Words\
         Number of rows(=1)' 
         STATIC                                                               
         SIZE 1,1

       = (DriftX            = REAL [0.,*]         : 'Drift velocity along X (cm/musec)',
          DriftY            = REAL [0.,*]         : 'Drift velocity along Y (cm/musec)',
          DriftZ            = REAL [0.,*]         : 'Drift velocity along Z (cm/musec)',
          GlobalT0          = REAL [-512.00,512.] : 'Constant time offset for conversion from buckets to
                                                     drift length, in microseconds.\
                                                     drift_len=(buckets*T-GlobalT0)*v_drift, where T is the clock period.',
          DriftB(3)         = REAL                : 'Drift velocity side B [cm/musec]',
          ErrorZ(2)         = REAL                : 'Error on drift VZ side A and B when VZ was coming from PASS0 otherwise 0.',
          ClockFreq         = INTE                : 'Tpc Clock Frequency in khz',
          TpcPressure       = INTE                : 'Tpc Pressure' ,
          NoRmalization     = REAL [0.000,*]      : 'overall dE/dx NoRmalization coefficient used for Julia processing',
          SectorNorm(36)    = REAL [0.000,*]      : 'dE/dx Corrections to tune the sector to sector normalization.')

       ;

 JPAS : 'JULIA PASS0 Summary.
         NR = run number \
         Number of Words\
         Number of rows(=1)' 
         STATIC                                                               
         SIZE 1,1

       = (ZOnl          = INTE [0,*]      : 'number of Z flagged a la Online (before 1993) or class 16 (after 1993) ',
          NumberUsed    = INTE [0,*]      : 'Number of events Used to compute Delta T',
          NumberCut     = INTE [0,*]      : 'Number of events Cut this is the minimum number of events required 
                                             to derived a new drift velocity value from Delta T distribution',
          DeltaT(3)     = REAL [0.,*]     : 'average Delta T in mus in TPC with is error and the rms of the distribution',
          VzTlas        = REAL [0.,*]     : 'Drift velocity (Vz) from TLAS',
          VzOld         = REAL [0.,*]     : 'Old drift velocity (Vz) used as input ',
          VzNew(2)      = REAL [0.,*]     : 'New drift velocity (Vz) and is error determined from Delta T distribution',
          TimeCut       = REAL [0.,*]     : 'Elapsed Time Cut on TLAS if elapsed time is smaller take TLAS 
                                             as input Vz, TDPV otherwise',
          ElapsedTime   = REAL            : 'TLAS elapsed time -999. if no TLAS', 
          DAte          = INTE [0,*]      : 'Date of PASS0 processing (yymmdd)',
          TIme          = INTE [0,*]      : 'Time (hhmm)',
          NumberMax     = INTE [0,*]      : 'Max number of events to Compute new Drift velocity',
          HotchanPrun   = INTE [0,*]      : 'Number of Vdet Hot channels found in Previous run or in reference file',
          HotChancrun   = INTE [0,*]      : 'Number of Vdet Hot channels for Current run')
       ;
 
 ALPB
      :      'Aleph chunk-by-chunk beam position (NR=Run number) \ 
              number of columns \
              number of chunks in run'
              STATIC
 
  =  (FirstEvent   = INTE [0,*] : 'Event number of first event in chunk',
      XPoslumreg   = INTE       : 'X Position of luminous region centroid in units of 0.5 micron',
      XposError    = INTE [0,*] : 'X position Error in units of 0.1 micron',
      YPoslumreg   = INTE       : 'Y Position of luminous region centroid in units of 0.5 micron',
      YposError    = INTE [0,*] : 'Y position Error in units of 0.1 micron')
      ;
 
 ALP1
      :   'Corrections to ALPB, Julia version < 276.01 (NR=BE setup code)\
           number of columns\
           number of rows = number of VD runs in setup'
           STATIC
 
   = (Column1     = INTE        : 'Packed run/event number;
                                    bits 0-15: event;
                                    bits 16-30: run (must be less than 32768!);
                                    if event = 0, then ALPB on POT is OK,
                                    otherwise event = corrected first event
                                    number for the last row of ALPB')
      ;


 END ESET

 END SUBSCHEMA


 SUBSCHEMA InternalJULBanks
 :   'Internal JULIA banks'
                                                                                
 AUTHOR   'J.Knobloch,S.Wasserbach'                                                          
 REVIEWER 'P.Comas'
 VERSION  '2.0'
 DATE     '12/06/95'
                                                                                
 DEFINE ESET                                                                    
/*---------------------------------------------------------------------*/
                                                                                
 MONE : 'Monitor bank containing global event information\
         Number of Words\
         Number of Headers(=1)' 
         STATIC                                                               
         SIZE 1,1

       = (RunNumber  = INTE [1,9999]        : 'Run number',
          EVentnumber= INTE [1,9999]        : 'Event number',
          Mask1      = INTE [*,*]           : 'Trigger mask 1',
          Mask2      = INTE [*,*]           : 'Trigger mask 2',
          Mask3      = INTE [*,*]           : 'Trigger mask 3',
          Mask4      = INTE [*,*]           : 'Trigger mask 4',
          ERrorstatus= INTE [0,999]         : 'Error status (OK = 1)',
          NverteX    = INTE [0,10]          : 'Nr of Vertices',   
          NPostra    = INTE [0,50]          : 'Nr of Positive Tracks',  
          NNegtra    = INTE [0,50]          : 'Nr of Negative Tracks',  
          Nv0        = INTE [0,20]          : 'Nr of V0s',
          NJets      = INTE [0,15]          : 'Nr of Jets',           
          VPost      = INTE [0,20]          : 'Nr of Positive Tracks from Vtx',
          VMint      = INTE [0,20]          : 'Nr of Negative Tracks from Vtx',
          NTpco      = INTE [0,1000]        : 'Nr of TPC coordinates',
          NBtpc      = INTE [0,1000]        : 'Nr of bad TPC coordinates',
          NItco      = INTE [0,1000]        : 'Nr of ITC coordinates',
          NVdco      = INTE [0,1000]        : 'Nr of VDET coordinates',
          NMuco      = INTE [0,50]          : 'Nr of MUON coordinates',
          UTpco      = INTE [0,1000]        : 'Nr of TPC coordinates used',
          UItco      = INTE [0,1000]        : 'Nr of ITC coordinates used',
          UVdco      = INTE [0,1000]        : 'Nr of VDET coordinates used',
          UMuco      = INTE [0,20]          : 'Nr of tracks with MUON hits',
          NEsto      = INTE [0,2000]        : 'Nr of ECAL storeys fired',
          NHsto      = INTE [0,200]         : 'Nr of HCAL storeys fired',
          NLsto      = INTE [0,50]          : 'Nr of LCAL storeys fired',
          EcalcL     = INTE [0,50]          : 'Nr of Ecal clusters found',
          HcalcL     = INTE [0,50]          : 'Nr of Hcal clusters found',
          LcalcL     = INTE [0,20]          : 'Nr of Lcal clusters found',
          MUcand     = INTE [0,20]          : 'Nr of Muon cand found',
          ElEctrons  = INTE [0,20]          : 'Nr of Electron cand found',
          ECtrack    = REAL [0.,200.00]     : 'Total Energy of Charged Tracks',
          EPtrack    = REAL [0.,200.00]     : 'Total Energy of Positive Tracks',
          EMtrack    = REAL [0.,200.00]     : 'Total Energy of Negative Tracks',
          EcalO      = REAL [0.,200.00]     : 'Total Energy of Neutral Calobjects',
          PFlow      = REAL [0.,200.00]     : 'Pabs of Energy Flow',  
          THflow     = REAL [0.,6.2832]     : 'Theta of Energy Flow',
          PHflow     = REAL [0.,6.2832]     : 'Phi of Energy Flow',
          EFlow      = REAL [0.,200.00]     : 'Energy Flow',   
          ETabs      = REAL [0.,200.00]     : 'Abs value of Et',
          X          = REAL [-1.0,1.0]      : 'X of fitted Vertex',
          Y          = REAL [-1.0,1.0]      : 'Y of fitted Vertex',
          Z          = REAL [-10.,10.]      : 'Z of fitted Vertex',
          EStoryEn   = REAL [0.,200.]       : 'Energy of ECAL storeys fired',
          HStoryen   = REAL [0.,200.]       : 'Energy of HCAL storeys fired',
          LStoryEn   = REAL [0.,100.]       : 'Energy of LCAL storeys fired',
          Energy1    = REAL [0.,100.]       : 'Energy in ECAL layer 1',
          Energy2    = REAL [0.,100.]       : 'Energy in ECAL layer 2',
          EcalenergyA= REAL [0.,100.]       : 'Energy in ECAL endcap A',
          EcalenergyB= REAL [0.,100.]       : 'Energy in ECAL endcap B',
          HcalenergyA= REAL [0.,100.]       : 'Energy in HCAL endcap A',
          HcalenergyB= REAL [0.,100.]       : 'Energy in HCAL endcap B',
          LstoryA    = REAL [0.,100.]       : 'Energy of LCAL storeys side A',
          LstoryB    = REAL [0.,100.]       : 'Energy of LCAL storeys side B',
          CluenergyE = REAL [0.,100.]       : 'Energy of clusters Ecal',
          CluenergyH = REAL [0.,100.]       : 'Energy of clusters Hcal',
          CluenergyL = REAL [0.,200.]       : 'Energy of clusters Lcal',
          QcluenergyE = REAL [0.,100.]      : 'Energy of charged clusters Ecal',
          QcluenergyH = REAL [0.,100.]      : 'Energy of charged clusters Hcal',
          TIme        = REAL [0.,200.]      : 'Time for reconstruction (exc.I/O',
          NumSuppr    = REAL [0.,1000.]     : 'Number of suppressed ECAL-st Brig',
          SupprNumber = REAL [0.,2000.]     : 'Number of suppressed ECAL-st EWIN',
          FileLength  = REAL [0.,400.]      : 'Length of input file',
          WireCluster = REAL [0.,100.]      : 'Number of wire clusters in HCAL',
          DigPatterns = REAL [0.,100.]      : 'Number of digital patterns in HCAL',
          HitsPerPatt = REAL [0.,80.]       : 'Number of Hits per Pattern',
          EcalWireen  = REAL [0.,100.]      : 'ECAL wire energy',
          Wireen1     = REAL [0.,100.]      : 'ECAL wire energy- Endcap A',
          Wireen2     = REAL [0.,100.]      : 'ECAL wire energy- Barrel',
          Wireen3     = REAL [0.,100.]      : 'ECAL wire energy- Endcap B',
          DetStat     = INTE [*,*]          : 'Detector status bits',
          Fatalerr1   = INTE [*,*]          : 'Fatal error bits 1(JULIA)',
          Fatalerr2   = INTE [*,*]          : 'Fatal error bits 2(JULIA)',
          Nonfatal1   = INTE [*,*]          : 'Non fatal Error Bits 1(JULIA)',
          Nonfatal2   = INTE [*,*]          : 'Non fatal Error Bits 2(JULIA)',
          DEdx        = REAL [0.,8.]        : 'dE/dx (pion=1)',
          TimeDiffer  = REAL [-.5,.5]       : 'Time diff of TPC sides [microsec]',
          GoodTrack   = INTE [0,50]         : 'Nb. of good tracks',
          GoodBoth    = INTE [0,50]         : 'Nb. of good tracks TPC+ITC',
          GoodITC     = INTE [0,50]         : 'Nb. of good ITC tracks',
          TpcGood     = INTE [0,50]         : 'Nb. of good tracks TPC',
          TimeEcal    = REAL [-500.,500.]   : 'Tzero in ECAL',
          LenPot      = REAL [0,20000]      : 'Length of POT banks',
          LenComp     = REAL [0,20000]      : 'Length of compressed POT'
          )
       ;                                                                        
 
JSUM : 'JULIA run summary.
         NR = run number (STC)\
         Number of Words\
         Number of rows' 
         STATIC                                                               
         SIZE 1,1

       = (NumTriggers  = INTE [0,99999]  : 'Number of triggers',
          NumVertex    = INTE [0,99999]  : 'Number of events  with vertex',
          NumZ         = INTE [0,99999]  : 'Number of hadronic events',
          NumL         = INTE [0,99999]  : 'Number of Lumin coincidences',
          NumBha       = INTE [0,99999]  : 'Number of BhaBhas',
          VnuTriggers  = INTE [0,99999]  : 'Number of triggers, HVs on',
          VnuVertex    = INTE [0,99999]  : 'Number of events with vertex,HVs on',
          VnuZ         = INTE [0,99999]  : 'Number of hadronic events,HVs on',
          VnuL         = INTE [0,99999]  : 'Number of Lumin coincidences,HVs on',
          VnuBha       = INTE [0,99999]  : 'Number of BhaBhas,HVs on',
          TimTrig      = REAL [0.,*]     : 'Total time for reconstruction',
          TimZ         = REAL [0.,*]     : 'Total time for hadr events',
          TimB         = REAL [0.,*]     : 'Total time for Bhabhas',
          LInput       = REAL [0.,*]     : 'Total length of input records [kb]',
          InputZ       = REAL [0.,*]     : 'Total length of input for Z',
          LOut         = REAL [0.,*]     : 'Total length of POT records [kb]',
          LZ           = REAL [0.,*]     : 'Length of POT for hadr. events [kb]',
          XVertex      = REAL [*,*]      : 'Sum of vertex X positions', 
          YVertex      = REAL [*,*]      : 'Sum of vertex Y positions', 
          ZVertex      = REAL [*,*]      : 'Sum of vertex Z positions', 
          XSvertex     = REAL [*,*]      : 'Sum of vertex X positions squared' ,
          YSvertex     = REAL [*,*]      : 'Sum of vertex Y positions squared', 
          ZSvertex     = REAL [*,*]      : 'Sum of vertex Z positions squared',
          KilstorB     = INTE [0,99999]  : 'Nb. of killed storeys -Birgitte',
          KilstorW     = INTE [0,99999]  : 'Nb. of killed storeys - Wires',
          TzerNum      = INTE [0,99999]  : 'Num of t0s',
          TzerSum      = REAL [*,*]      : 'Sum of t0s',
          TzerVar      = REAL [*,*]      : 'Sum of t0s squared - for variance',
          Asum0        = REAL [0.,*]     : 'Group A, number of tracks used for beam position;
                                            Group A = tracks with at least one VDET r-phi
                                            coordinate, Group B = all other tracks',
          Asum1        = REAL [0.,*]     : 'Group A, sum cos phi',
          Asum2        = REAL [0.,*]     : 'Group A, sum sin phi',
          Asum3        = REAL [0.,*]     : 'Group A, sum cos(phi)*sin(phi)',
          Asum4        = REAL [0.,*]     : 'Group A, sum sin(phi)**2',
          Asum5        = REAL [0.,*]     : 'Group A, sum cos(phi)**2',
          Asum6        = REAL [0.,*]     : 'Group A, sum d0 * sin(phi)',
          Asum7        = REAL [0.,*]     : 'Group A, sum d0 * cos(phi)',
          Asum8        = REAL [0.,*]     : 'Group A, sum d0',
          Asum9        = REAL [0.,*]     : 'Group A, sum d0**2',
          Bsum0        = REAL [0.,*]     : 'Group B, number of tracks used for beam position',
          Bsum1        = REAL [0.,*]     : 'Group B, sum cos phi',
          Bsum2        = REAL [0.,*]     : 'Group B, sum sin phi',
          Bsum3        = REAL [0.,*]     : 'Group B, sum cos(phi)*sin(phi)',
          Bsum4        = REAL [0.,*]     : 'Group B, sum sos(phi)**2',
          Bsum5        = REAL [0.,*]     : 'Group B, sum sin(phi)**2',
          Bsum6        = REAL [0.,*]     : 'Group B, sum d0 * sin(phi)',
          Bsum7        = REAL [0.,*]     : 'Group B, sum d0 * cos(phi)',
          Bsum8        = REAL [0.,*]     : 'Group B, sum d0',         
          Bsum9        = REAL [0.,*]     : 'Group B, sum d0**2',
          DAte         = INTE [0,*]      : 'Date of JULIA processing',
          TIme         = INTE [0,*]      : 'Time of JULIA processing')
       ;

JEDS : 'JULIA Edir statistic bank (NR=Run number).
         Row 2 = Number of events with SLUMOK (STC)\
         Number of columns\
         Number of rows(=2)'
 
         STATIC                                                               
                                                                                
        = (CLass(30)   = INTE [0,*]     : 'Number of Events in corresponding Class number')
      ;

JTRE
      :      'TPC Reconstruction errors reported in Julia.
              The numbers give  the frequency an error was
              reported in Julia for this run per sector.
              Bank number = Run number.\
              Number of columns\
              Number of sectors(=36)'

            STATIC
 
      = (WirTru      = INTE [0,*]  : 'Truncated wire data',
         PadTru      = INTE [0,*]  : 'Truncated pad  data',
         MisDat      = INTE [0,*]  : 'Missing data',
         TppWir      = INTE [0,*]  : 'wire TPP out of sequence',
         TppPad      = INTE [0,*]  : 'pad  TPP out of sequence',
         DatFor      = INTE [0,*]  : 'Data format errors',
         HitRow      = INTE [0,*]  : '# hits/padrow too large',
         RanRow      = INTE [0,*]  : 'padrow # out of range',
         BadHit      = INTE [0,*]  : 'bad hit',
         TpaTpd      = INTE [0,*]  : 'TPAD,TPDI mismatch',
         DatPad      = INTE [0,*]  : 'pad data truncated in Julia',
         BadCor      = INTE [0,*]  : 'bad coordinates in sector',
         FreA        = INTE [0,*]  : 'free',
         FreB        = INTE [0,*]  : 'free',
         FreC        = INTE [0,*]  : 'free' )
      ;
 
JTDX
      :      'JULIA dE/dx summary bank. 
              NR=Run number.\
              Number of columns\
              Number of rows (=2), 1=Average of measurement,
                                   2=Error on measurement '

            STATIC
 
      = (AV       = REAL  : 'Corrected dE/dx for mips',
         SN(36)   = REAL  : 'dE/dx for mips in Sector Number')
      ;
 
JHDX
      :      'JULIA dE/dx run summary bank (from 1994).
              Filled using DEDX histograms per sector
              and total.
              Used offline to obtain the TCGX and TCSX
              sector normalization banks.
              NR=Run number.\
              Number of columns \
              Number of rows (= number of sectors+1 = 37)'
 
            STATIC
 
      = (BIncontent(100) = REAL  : 'Bin content, 1 column per bin')
      ;

  
  END ESET                                                                       
                                                                                
 END SUBSCHEMA                                                                  


 SUBSCHEMA StatusRAWBanks

 AUTHOR   'A. Putzer,J. Pater'
 REVIEWER 'F. Ranjard'
 VERSION  '2.0'
 DATE     '02/05/94'

 DEFINE ESET

 RUNR
       :     'RUN Record header (NR = 0)'
           STATIC

      = (ExpNumber    = INTE           : 'Experiment Number\
                                          Online     = [0,1000]\
                                          MonteCarlo =1001',
         RunNumber    = INTE [1,*]     : 'Unique run number')
      ;

 RUNH
       :     'RUN Header (NR = 0)'
           STATIC

      = (ExpNumber    = INTE           : 'Experiment Number\
                                          Online     = [0,1000]\
                                          MonteCarlo =1001',
         RunNumber    = INTE [1,*]     : 'Unique run number',
         RunType      = INTE           : 'Monte Carlo run type\
                                          KINE "LUND" : [1000,1999]\
                                           "SJET" : [2000,2999]\
                                           "PART" : [3000,3999]\
                                           "USER" : [10000,99999]\
                                           undefined : 100000',
         StartDate    = INTE           : 'run start date  : yymmdd',
         StartTime    = INTE           : 'run start time  : hhmmss',
         TriggerType  = CHA8           : 'trigger mask description\
                                          usually RUN_DFLT',
         RunCondition = CHA8           : 'general run conditions description\
                                          usually ALEPH   ')
      ;

 RUNE
       :     'Run END (NR = 0)'
           STATIC

      = (eXpNumber    = INTE           : 'Experiment Number\
                                          Online     = [0,1000]\
                                          MonteCarlo =1001',
         rUnNumber    = INTE [1,*]     : 'Unique run number',
         rUnType      = INTE           : 'Monte Carlo run type\
                                          KINE "LUND" : [1000,1999]\
                                           "SJET" : [2000,2999]\
                                           "PART" : [3000,3999]\
                                           "USER" : [10000,99999]\
                                           undefined : 100000',
         EndDate      = INTE           : 'run end date  : yymmdd',
         EndTime      = INTE           : 'run time time  : hhmmss',
         NumEvents    = INTE           : 'Number of events for\
                                          this run')
      ;

 EVEH
       :     'EVEnt Header (NR = 0)'
           STATIC

      = (ExpNumber    = INTE           : 'Experiment Number\
                                          Online     = [0,1000]\
                                          MonteCarlo =1001',
         RunNumber    = INTE [1,*]     : 'Unique run number',
         RunType      = INTE           : 'Monte Carlo run type\
                                          KINE "LUND" : [1000,1999]\
                                           "SJET" : [2000,2999]\
                                           "PART" : [3000,3999]\
                                           "USER" : [10000,99999]\
                                           undefined : 100000',
         DAte         = INTE           : 'event date : yymmdd',
         TIme         = INTE           : 'event time : hhmmss',
         EVentnumber  = INTE           : 'event number',
         Mask1        = INTE           : 'Trigger mask 1',
         Mask2        = INTE           : 'Trigger mask 2',
         Mask3        = INTE           : 'Trigger mask 3',
         Mask4        = INTE           : 'Trigger mask 4',
         TYpe         = INTE           : 'Event type\
                                          Real event = 1',
         ErrorStatus  = INTE           : 'Error status (OK = 1)',
         TotalEnergy  = INTE           : '2*Beam energy (keV)')
      ;
     
 OROP
    : 'Correlates the resource name of a readout processor with the
       resource id used to identify it in the event data\
       number of columns/row\
       number of rows'

        STATIC

        = ( ResourceId       = INTE : 'Generated from primary address',
            ResourceName     = CH32 : 'eg. TPC_SIDEA_EB')
        ;

 OSTS
    : 'Event readout status bank\
      Number of words per row \
      Number of rows '

      STATIC

      = (EIdent    = INTE : 'Event source identifier',
         ORder     = INTE : 'Order in readout (0 if missing)',
         EvtLen    = INTE : 'Event length',
         TrgNum    = INTE : 'Trigger number',
         TrgMsk    = INTE : 'Trigger mask',
         EvtTyp    = INTE : 'Event type')
    ;

RTLO
   : 'Online Trailer Bank.
      NR=primary address in FASTBUS of Module creating bank.
      NR=0 is the last RTLO from the main EB.'

      STATIC

   =(partid	= INTE	: 'Partition ID',
     EvType	= INTE	: 'Event Type from AL0VOL::DAQ$LIBRARY:EVENT_TYPES.INC',
     TrNum	= INTE	: 'Trigger Number from TS',
     TrMask	= INTE	: 'Trigger Mask from trigger system',
     Trmask1	= INTE  : 'Trigger Mask from Level 3, 1st word',
     Trmask2	= INTE  : 'Trigger Mask from Level 3, 2nd word',
     Trmask3	= INTE  : 'Trigger Mask from Level 3, 3rd word',
     Res1	= INTE  : 'reserved. Must be Zero',
     Res2	= INTE  : 'reserved. Must be Zero',
     EvLen	= INTE  : 'Event lenght in bytes',
     CTrlfl	= INTE  : 'Control flags',
     CHksum	= INTE  : 'Checksum of event')
    ;

 YSBO
      :      'Online readout controller busytimes and associated informations.
              For more detailed information about this bank see the online documentation.'
           STATIC

      = (Sample = INTE   : 'The first word of this bank gives the trigger mask
                            for the event. The following words give bitpacked       
                            information for each readout controller (one word
                            per controller). The last word of the bank contains
                            the number of sources described in the bank and the
                            total length of the bank in bytes.')
      ;

 END ESET

 END SUBSCHEMA


 SUBSCHEMA StatusOFFLBanks
                                                                                
 AUTHOR   'J. Boucrot'                                                          
 REVIEWER 'P. Comas'                                                                  
 VERSION  '3.0'
 DATE     '10/06/94'
                                                                                
 DEFINE ESET                                                                    
                                                                                
 RHOH                                                                           
       :      'Run Header Offline History (DROP)\                               
               Number of words per Offline step\                                
               Number of Offline steps '                                        
           STATIC                                                               
                                                                                
       = (ProgName(2)  = CHA4           : 'Program name : KINGAL,ONLINE,GALEPH,\
                                           JULIA',
          ProgDate     = INTE[0,*]      : 'Run date of processing : yymmdd',
          ProgHour     = INTE[0,*]      : 'Run hour of processing : hhmmss',
          ProgVers     = INTE[0,*]      : 'Program version number',
          AlVers       = INTE[0,*]      : ' Alephlib Version Number',
          DbVers       = INTE[0,*]      : ' Database Version Number',
          DbDate       = INTE[0,*]      : 'Database : Last Date of change',     
          NatIn        = INTE[0,15]     : 'Nature of Input :\
                                             1   KINGAL output\
                                             2   RAW data     \
                                             3   POT          \
                                             4   DST          \ 
                                             5   MDST         \
                                          + 10   if selected data',
         NatOu         = INTE[0,15]     : 'Nature of Output : same code as above')
       ;                                                                        
                                                                                
RHAH
       :      'Run Header Analysis History (NR=0)\
               Number of words per Offline step\                                
               Number of Offline steps '                                        
           STATIC                                                               
                                                                                
       = (ProgName(2)  = CHA4           : 'Program name : KINGAL,ONLINE,GALEPH,\
                                           JULIA,ALPHA',
          ProgDate     = INTE[0,*]      : 'Run date of processing : yymmdd',
          ProgHour     = INTE[0,*]      : 'Run hour of processing : hhmmss',
          ProgVers     = INTE[0,*]      : 'Program version number',
          AlVers       = INTE[0,*]      : ' Alephlib Version Number',
          DbVers       = INTE[0,*]      : ' Database Version Number',
          DbDate       = INTE[0,*]      : 'Database : Last Date of change',     
          NatIn        = INTE[0,15]     : 'Nature of Input :\
                                             1   KINGAL output\
                                             2   RAW data     \
                                             3   POT          \
                                             4   DST          \ 
                                             5   MDST         \
                                          + 10   if selected data',
          NatOu        = INTE[0,15]     : 'Nature of Output : same code as above',
          CorrVers     = INTE[0,*]      : 'Correction file version number',
          machiNeUsed  = CHA4           : 'Machine name where program was run')
       ;                                                                        
                                                                                
 END ESET                                                                       
                                                                                
 END SUBSCHEMA                                                                  


 SUBSCHEMA LepstatONLBanks

 AUTHOR   'J. Rothberg'
 REVIEWER 'J.Knobloch'
 VERSION  '1.1'
 DATE     '23/09/92'

 DEFINE ESET

RLEP
     :   'Run Header for LEP parameters\

          number of words per row\
          number of rows = 1 '


    STATIC
    SIZE 1,1

 =  (LepEnergy   = INTE [0,*]  : 'Lep Energy in MeV',
     
     LepBeam     = CHA4 'NONE'|'E+  '|'E-  '|'DISP'|'COLL' : ' Beam type',

     LepperioD   = INTE [0,*]  : 'Number of Bunches for Lep operation',

     LepFill     = INTE [0,*]  : 'Lep Fill number',

     LepPolar    = INTE [-1,1] : 'Code for polarization')

  ;


RALE
     :   'Run Header for ALEPH running conditions\

          number of words per row\
          number of rows = 1 '


    STATIC
    SIZE 1,1


 =  (RunNumb    = INTE[0,*]  :  'Run number',

     LocRunNum  = INTE[0,*]  :  ' Local run number',

     RunSeries  = INTE[0,*]  :  'First run number of series',

     RunType(2) = CHA4       :   'Run type',

     TrigType(2)= CHA4       :   'Trigger type',

     TrigMask   = BITP[0,*]  :   'Trigger Mask',

     EvPrcType(4) = CHA4     : 'Event processor configuration',

     MagPolar   = INTE[-1,1] :  'Magnet polarity',

     MagCurr    = INTE[0,*]  :  'Main magnet current in mA',     

     MagcurrcA  = INTE[0,*]  :  'Magnet current correction coil A in mA',

     MagcurrcB  = INTE[0,*]  :  'Magnet current correction coil B in mA',

     NmrA       = INTE[0,*]  :  'NMR reading side A',

     NmrB       = INTE[0,*]  :  'NMR reading side B',

     QuadcurrA  = INTE[0,*]  :  'Superconducting Quadrupole Current side A in mA',

     QuadcurrB  = INTE[0,*]  :  'Superconducting Quadrupole Current side B in mA',

     DetUse     = BITP[0,*]   :  'Subpartition mask in order of next words/
                                  bit 2 = COMMON/
                                  bit 3 = VDET/
                                  bit 4 = ITC/
                                  bit 5 = TPC/
                                  bit 6 = ECAL/
                                  bit 7 = HCAL/
                                  bit 8 = SATR/
                                  bit 9 = LCAL/
                                  bit 10= BCAL ',

     Common(2)  = BITP[0,*]   :  'Common subpartition',

     VDET(2)    = BITP[0,*]   :  'VDET',

     ITC        = BITP[0,*]   :  'ITC',

     TPC(2)     = BITP[0,*]   :  'TPC, TPC(1) bit 0-17 = sector  1-18/
                                       TPC(2) bit 0-17 = sector 19-36 ',

     ECAL(3)    = BITP[0,*]   :  'ECAL, ECAL(1) bit 0-11 = endcap A/
                                        ECAL(2) bit 0-11 = barrel/
                                        ECAL(3) bit 0-11 = endcap B/',

     HCAL(3)    = BITP[0,*]   :  'HCAL, HCAL(1) bit 0-5 = endcap A/
                                        HCAL(2) bit 0-23 = barrel/
                                        HCAL(3) bit 0-5 = endcap B/',

     MUON(6)    = BITP[0,*]   :  'MUON, see ALEPH handbook',

     SATR(2)    = BITP[0,*]   :  'SATR',

     LCAL(2)    = BITP[0,*]   :  'LCAL',

     BCAL(2)    = BITP[0,*]   :  'BCAL')

   ;


RL01
     :   'LEP page 101 parameters\

          number of words per row\
          number of rows = 1 '


    STATIC
    SIZE 1,1

 =  (LepRead     = INTE [0,*]  : 'time of last readout hhmmss',

     LepFill     = INTE [0,*]  : 'Lep Fill number',

     LepEnergy   = REAL [0,*]  : 'Lep Energy in GeV',
     
     Pcurr1      = REAL [0,*]  : 'positron current bunch 1 [microAmp]',
     Pcurr2      = REAL [0,*]  : 'positron current bunch 2 [microAmp]',
     Pcurr3      = REAL [0,*]  : 'positron current bunch 3 [microAmp]',
     Pcurr4      = REAL [0,*]  : 'positron current bunch 4 [microAmp]',

     Ecurr1      = REAL [0,*]  : 'electron current bunch 1 [microAmp]',
     Ecurr2      = REAL [0,*]  : 'electron current bunch 2 [microAmp]',
     Ecurr3      = REAL [0,*]  : 'electron current bunch 3 [microAmp]',
     Ecurr4      = REAL [0,*]  : 'electron current bunch 4 [microAmp]',

     CoastTime   = REAL [0,*]  : 'coast time [hours]',

     PosCur      = REAL [0,*]  : 'positron current [ mA ] ',

     EleCur      = REAL [0,*]  : 'electron current [ mA ]',

     PosLife     = REAL [0,*]  : 'positron lifetime [hours]',

     EleLife     = REAL [0,*]  : 'electron lifetime [hours]',

     IlumL3      = REAL [0,*]  : 'initial luminosity in L3 ',
     IlumAleph   = REAL [0,*]  : 'initial luminosity in ALEPH ',
     IlumOpal    = REAL [0,*]  : 'initial luminosity in OPAL ',
     IlumdElphi  = REAL [0,*]  : 'initial luminosity in DELPHI ',

     LumL3       = REAL [0,*]  : 'current luminosity in L3 ',
     LumAleph    = REAL [0,*]  : 'current luminosity in ALEPH ',
     LumOpal     = REAL [0,*]  : 'current luminosity in OPAL ',
     LumDelphi   = REAL [0,*]  : 'current luminosity in DELPHI ',

     SlumL3      = REAL [0,*]  : 'integrated luminosity in L3 ',
     SlumAleph   = REAL [0,*]  : 'integrated luminosity in ALEPH ',
     SlumOpal    = REAL [0,*]  : 'integrated luminosity in OPAL ',
     SlumDelphi  = REAL [0,*]  : 'integrated luminosity in DELPHI ',

     BackgL3     = REAL [0,*]  : 'current background in L3 ',
     BackgAleph  = REAL [0,*]  : 'current background in ALEPH ',
     BackgOpal   = REAL [0,*]  : 'current background in OPAL ',
     BackgDelphi = REAL [0,*]  : 'current background in DELPHI ')

  ;
    END ESET    

 END SUBSCHEMA
  

 SUBSCHEMA LepTABle
 : 'LEP tables'

 AUTHOR   'J. Rothberg,O.Callot'
 VERSION  '3.2'
 DATE     '23/10/95'

 DEFINE ESET


 LYZT
      :      'LEP Standard0\
              Number of columns \
              Number of rows = 1'

           STATIC

           SIZE 1,1

  
= (IndeX     = INTE [*,*]           : 'Bank header index STANDARD0',
DatestamP    = INTE [*,*]           : 'Bank header date',
ReadTime     = INTE [*,*]           : 'Bank header time',
VersionNum   = INTE [*,*]           : 'Bank header version,mode',

FiLlid	     = INTE [*,*]	    : ' Fill Identification Number',
RunNumber    = INTE [*,*]           : 'Runnumber,Fillnumber ',

ModeTime     = INTE [*,*]           : ' Mode Time',
MOde         = INTE [*,*]           : ' Mode, Pad M0',

PNom	     = REAL [*,*]           : ' Nominal beam momentum',
BVnom        = REAL [*,*]           : ' Nominal Low beta vertical',
NbunchPlmn   = INTE [*,*]           : ' Number of bunches in +,- beam',


eHallTime   = INTE [*,*]           : ' time of HALL reading ',
eHaLl       = REAL [*,*]           : ' Beam Energy - Hall probe ',
eFlipTime   = INTE [*,*]           : ' time of flip coil reading',
EFlip       = REAL [*,*]           : ' Beam Energy - Flip Coil',
eNmrTime    = INTE [*,*]           : ' time of NMR reading ',
enMR        = REAL [*,*]           : ' Beam Energy, NMR',
FlipInhib   = INTE [*,*]           : ' Flip Inhibit, Error',

MttiMe		= INTE [*,*]	: ' time of magnet temperature reading ',
MagtemP(34)	= REAL [*,*]	: ' Magnet Temperatures ',

EmTime		= INTE [*,*]     : ' time of measurement of emittance ',
EHplus(32)      = REAL [*,*]     : '              ',

ITime		= INTE [*,*]	 : '  time of measurement of intensity ',
IPlus(16)       = REAL [*,*]     : '              ',

TauTime	        = INTE [*,*]     : '              ',
TauPlus(16)     = REAL [*,*]     : '              ',


PolTime	        = INTE [*,*]	 : ' time of measurement of polarisation ',
PolPlus(16)	= REAL [*,*]     : '              ',

LUm(16)	        = REAL [*,*]     : ' Luminosity   ',


ColTime	        = INTE [*,*]     : ' Collimators  ',
CoLls(12)  	= REAL [*,*]     : '              ',

SaspTime(4)	= INTE [*,*]     : ' SPARES ',
SbSpare(16)     = REAL [*,*]     : ' end of standard0')
;

LYLT

     :      'LEP StandardL\
              Number of columns =  \
              Number of rows = 1'

           STATIC

           SIZE 1,1
 

= (IndeX     = INTE [*,*]           : 'Bank header index STANDARDL',
DatestamP    = INTE [*,*]           : 'Bank header date',
ReadTime     = INTE [*,*]           : 'Bank header time',
VersionNum   = INTE [*,*]           : 'Bank header version',
 

rflpQTime	= INTE [*,*]		: ' Measurement time.      ',
BasefreQ   	= REAL [*,*]		: ' Base LEP RF frequency. ',
DeltafreQ	= REAL [*,*]		: ' Offset   RF frequency. ',
Af1(8)		= REAL [*,*]		: ' Spare ',

QsePlus	        = REAL [*,*]     : 'QS PLUS       ',
QseMinus	= REAL [*,*]     : 'QS MINUS      ',


LrfA(96)        = REAL [*,*]     : '  Cavities    ',

maGnTime	= INTE [*,*]     : ' Mag meas Time',

MbS(2)		= REAL [*,*]	: ' Main supplies ',
MbI(2)		= REAL [*,*]	: ' Insertion     ',
QQ(3)           = REAL [*,*]    : ' QD, QF',

PWig3t(10)	= REAL [*,*]		: ' Wigglers ',


BdlSum(18)	= REAL [*,*]		: ' correctors ',

BunchlTime      = INTE [*,*]     : ' Bunch Length Time',
BUnchlng(16)	= REAL [*,*]     : ' Bunch length ',

ZXTime          = INTE [*,*]     : ' ZX Time',
pretZeL(16)	= REAL [*,*]	: ' Pretzel Separators ',

sDmoDe          = REAL [*,*]    : 'server analysis status',

Txl1Time(8)     = INTE [*,*]     : ' SPARES       ',
SiSpare(16)	= REAL [*,*]     : ' end of StandardL')
;

 LYFT
      :      'LEP Standard4\
              Number of columns \
              Number of rows = 1'

           STATIC

           SIZE 1,1


= (IndeX     = INTE [*,*]           : 'Bank header index STANDARD4',
DatestamP    = INTE [*,*]           : 'Bank header date',
ReadTime     = INTE [*,*]           : 'Bank header time',
VersionNum   = INTE [*,*]           : 'Bank header version',


PmsepTime	= INTE [*,*]     : ' Meas time of PMSEP   ',
PmSep		= REAL [*,*]     : '       ',


QlbTime  	= INTE [*,*]     : ' Low beta quads meas time ',
QS(11)   	= REAL [*,*]     : '       ',


CqTime		= INTE [*,*] : ' meas time of dipole currents ',
Cvh(16)		= REAL [*,*]     : '       ',

BhaTime         = INTE [*,*] :  ' Bhabha Time',
BHabha(4)       = REAL [*,*]  : ' Bhabha monitor de.e+	',

ElectronintA(4) = REAL [*,*]	: ' Single rate internal monitor ',


coLTime         = INTE [*,*]  : ' Time of last collimator update       ',
coLh(30)	= REAL [*,*]	: ' Collimators	',

VacTime	        = INTE [*,*]	: ' Time of vacuum reading   ',
VacpA(8)	= REAL [*,*]	: ' Vacuum 	',


IspecTime	= INTE [*,*]     : ' ISPEC measurement time 	',
IspeC           = REAL [*,*]     : '       ',



TimePlus    	= INTE [*,*]	: ' Timestamp of positron readings	',
XPuplus(38)     = REAL [*,*]	: ' Pick-up raw data, 		',


TimeMinus	= INTE [*,*]	: ' Timestamp of electron readings	',
XpuMinus(38)	= REAL [*,*]	: ' Pick-up raw data, 		',

SsptiMe(4)      = INTE [*,*]     : ' Spares ',
SSpare(16)      = REAL [*,*]     : '  end of standard4 ')
;

 LPZS
      :      'LEP Standard0\
              Number of columns \
              Number of rows = 1'

           STATIC

           SIZE 1,1

  
= (IndeX     = INTE [*,*]           : 'Bank header index STANDARD0',
DatestamP    = INTE [*,*]           : 'Bank header date',
ReadTime     = INTE [*,*]           : 'Bank header time',
VersionNum   = INTE [*,*]           : 'Bank header version',

FiLlid	     = INTE [*,*]	    : ' Fill Identification Number',
RunNumber    = INTE [*,*]           : 'Runnumber,Fillnumber ',

FillTime     = INTE [*,*]           : ' Date and Time of Fill',

PNom	     = REAL [*,*]           : ' Nominal beam momentum',
BVnom        = REAL [*,*]           : ' Nominal Low beta vertical',
NbunchPlus   = INTE [*,*]           : ' Number of bunches in + beam',
NbunchMinus  = INTE [*,*]           : ' Number of bunches in - beam',

RftiMe       = INTE [*,*]           : 'time of measurement of RF params',
RfhV(4)      = REAL [*,*]           : ' High Voltage.',
RffreQ(2)    = REAL [*,*]           : ' RF Frequency. DOUBLE Precision',

eHalltIme   = REAL [*,*]           : ' time of HALL reading ',
eHaLl       = REAL [*,*]           : ' Beam Energy - Hall probe ',
EfliptIme   = REAL [*,*]           : ' time of flip coil reading',
EFlip       = REAL [*,*]           : ' Beam Energy - Flip Coil',

MtTime		= REAL [*,*]	: ' time of magnet temperature reading ',
MtA137(2)	= REAL [*,*]	: ' Magnet 137 ',
MtB163(2)	= REAL [*,*]    : ' Magnet 163 ',
MtC237(2)	= REAL [*,*]	: ' Magnet 237 ',
MtD263(2)	= REAL [*,*]	: ' Magnet 263 ',
MtE337(2)	= REAL [*,*]	: ' Magnet 337 ',
MtF363(2)	= REAL [*,*]	: ' Magnet 363 ',
MtG437(2)	= REAL [*,*]	: ' Magnet 437 ',
MtH463(2)	= REAL [*,*]	: ' Magnet 463 ',
MtI537(2)	= REAL [*,*]	: ' Magnet 537 ',
MtJ563(2)	= REAL [*,*]	: ' Magnet 563 ',
MtK637(2)	= REAL [*,*]	: ' Magnet 637 ',
MtL663(2)	= REAL [*,*]	: ' Magnet 663 ',
MtM737(2)	= REAL [*,*]	: ' Magnet 737 ',
MtN763(2)	= REAL [*,*]	: ' Magnet 763 ',
MtO837(2)	= REAL [*,*]	: ' Magnet 837 ',
MtP863(2)	= REAL [*,*]	: ' Magnet 863 ',


EmTime		= REAL [*,*]     : ' time of measurement of emittance ',
eHPlus(8)	= REAL [*,*]     : '              ',
eVPlus(8)	= REAL [*,*]     : '              ',
eHMinus(8)      = REAL [*,*]     : '              ',
eVMinus(8)    	= REAL [*,*]     : '              ',


ITime		= REAL [*,*]	 : '  time of measurement of intensity ',
IPlus(8)	= REAL [*,*]     : '              ',
IMinus(8)	= REAL [*,*]     : '              ',


TauTime	        = REAL [*,*]     : '              ',
TauPlus(8)	= REAL [*,*]     : '              ',
TauMinus(8)	= REAL [*,*]     : '              ',


PolTime	        = REAL [*,*]	 : ' time of measurement of polarisation ',
PolPlus(8)	= REAL [*,*]     : '              ',
PolMinus(8)	= REAL [*,*]     : '              ',


Lum2(4)		= REAL [*,*]     : '              ',
Lum4(4)		= REAL [*,*]     : '              ',
Lum6(4)		= REAL [*,*]     : '              ',
Lum8(4)		= REAL [*,*]     : '              ',


ColTime	        = REAL [*,*]     : '              ',
ColvAqd42ld	= REAL [*,*]     : '              ',
ColvBqd42lu	= REAL [*,*]     : '              ',
ColhCqf33li	= REAL [*,*]     : '              ',
ColvDqd32ld	= REAL [*,*]     : '              ',
ColvEqd32lu	= REAL [*,*]     : '              ',
ColhFqf31lo	= REAL [*,*]     : '              ',
ColvGqd22ld	= REAL [*,*]     : '              ',
ColvHqd22lu	= REAL [*,*]     : '              ',
ColhIql9li	= REAL [*,*]     : '              ',
ColhJql9lo	= REAL [*,*]     : '              ',
ColhKip3i	= REAL [*,*]     : '              ',
ColhLip3o	= REAL [*,*]     : '              ',
ColhMql9ri	= REAL [*,*]     : '              ',
ColhNql9ro	= REAL [*,*]     : '              ',


SA0sptime	= REAL [*,*]     : ' SPARES ',
SB0spare01	= REAL [*,*]     : '              ',
SC0spare02	= REAL [*,*]     : '              ',
SD0spare03	= REAL [*,*]     : '              ',
SE0spare04	= REAL [*,*]     : ' end of standard0')
;

LPLS

     :      'LEP StandardL\
              Number of columns =  \
              Number of rows = 1'

           STATIC

           SIZE 1,1
 

= (IndeX     = INTE [*,*]           : 'Bank header index STANDARDL',
DatestamP    = INTE [*,*]           : 'Bank header date',
ReadTime     = INTE [*,*]           : 'Bank header time',
VersionNum   = INTE [*,*]           : 'Bank header version',
 

RflptiMe	= INTE [*,*]		: ' Measurement time.      ',
MainfreQ(2)	= REAL [*,*]		: ' Main LEP RF frequency. ',
Af2(2)		= REAL [*,*]		: ' Spare ',
Af3(2)		= REAL [*,*]		: ' Spare ',

QsePlus	        = REAL [*,*]     : 'QS PLUS       ',
QseMinus	= REAL [*,*]     : 'QS MINUS      ',


LrfA231(4)	= REAL [*,*]     : '              ',
LrfB232(4)	= REAL [*,*]     : '              ',
LrfC271(4)	= REAL [*,*]     : '              ',
LrfD272(4)	= REAL [*,*]     : '              ',
LrfE631(4)	= REAL [*,*]     : '              ',
LrfF632(4)	= REAL [*,*]     : '              ',
LrfG671(4)	= REAL [*,*]     : '              ',
LrfH672(4)	= REAL [*,*]     : '              ',


LrfI233(4)	= REAL [*,*]     : '              ',
LrfJspa(4)	= REAL [*,*] 	: ' spare ',

maGntiMe	= REAL [*,*]     : '              ',


mB2(2)		= REAL [*,*]	: ' Main supplies ',
mBI(2)		= REAL [*,*]	: ' Insertion     ',


PWig3t		= REAL [*,*]		: ' IP3 Pol. ',
PwiG3		= REAL [*,*]		: ' IP3 Pol. ',
mWetAl5	        = REAL [*,*]		: ' IP5 Em.  ',
mWetBr5	        = REAL [*,*]            : ' IP5 Em.  ',
mWeC		= REAL [*,*]		: ' IP5 Em.  ',
mWdtDl7	        = REAL [*,*]		: ' IP7 Damp ',
mWdtEr7  	= REAL [*,*]		: ' IP7 Damp ',
mWdF		= REAL [*,*]		: ' IP7 Damp ',
pWigG7		= REAL [*,*]		: ' IP7 Pol. ',
pWigH7		= REAL [*,*]		: ' IP7 Pol. ',


BdlSum		= REAL [*,*]		: ' Sum over all octants ',
BdlArc(8)	= REAL [*,*]          	: ' Octants.             ',


BUnchlng(8)	= REAL [*,*]     : '              ',


ZxA10qs11l2(2)	= REAL [*,*]	: ' Downstream(e+) IP2 ',
ZxB20qs11r2(2)	= REAL [*,*]	: ' Upstream(e+)   IP2 ',
ZxC10qs11l4(2)	= REAL [*,*]	: ' Downstream(e+) IP4 ',
ZxD20qs11r4(2)	= REAL [*,*]	: ' Upstream(e+)   IP4 ',
ZxE10qs11l6(2)	= REAL [*,*]	: ' Downstream(e+) IP6 ',
ZxF20qs11r6(2)	= REAL [*,*]	: ' Upstream(e+)   IP6 ',
ZxG10qs11l8(2)	= REAL [*,*]	: ' Downstream(e+) IP8 ',
ZxH20qs11r8(2)	= REAL [*,*]	: ' Upstream(e+)   IP8 ',


TXl1time 	= REAL [*,*]     : ' SPARES       ',
TYl2time	= REAL [*,*]     : '              ',
SFl1spare	= REAL [*,*]     : '              ',
SGl2spare	= REAL [*,*]     : '              ',
SHl3spare	= REAL [*,*]     : '              ',
SIl4spare	= REAL [*,*]     : ' end of StandardL')
;

 LPFS
      :      'LEP Standard4\
              Number of columns \
              Number of rows = 1'

           STATIC

           SIZE 1,1


= (IndeX     = INTE [*,*]           : 'Bank header index STANDARD4',
DatestamP    = INTE [*,*]           : 'Bank header date',
ReadTime     = INTE [*,*]           : 'Bank header time',
VersionNum   = INTE [*,*]           : 'Bank header version',


PmsepTime	= INTE [*,*]     : ' Meas time of PMSEP   ',
PmSep		= REAL [*,*]     : '       ',


QlbTime  	= REAL [*,*]     : ' Low beta quads meas time ',
Qs0r		= REAL [*,*]     : '       ',
QAs0l		= REAL [*,*]     : '       ',
Qs1r		= REAL [*,*]     : '       ',
QBs1l		= REAL [*,*]     : '       ',
Qs2r		= REAL [*,*]     : '       ',
QCs2l		= REAL [*,*]     : '       ',
Qs3		= REAL [*,*]     : '       ',
QDs4		= REAL [*,*]     : '       ',
Qs5r		= REAL [*,*]     : '       ',
QEs5l		= REAL [*,*]     : '       ',
Qs6		= REAL [*,*]     : '       ',


CqTime		= REAL [*,*] : ' meas time of dipole currents ',
Cvcqs0r		= REAL [*,*]     : '       ',
CAvcqs0l	= REAL [*,*]     : '       ',
Cvbqs1ar        = REAL [*,*]     : '       ',
CBvbqs1al	= REAL [*,*]     : '       ',
Chbqs2ar	= REAL [*,*]     : '       ',
CChbqs2al	= REAL [*,*]     : '       ',
Cvaqs4r		= REAL [*,*]     : '       ',
CDvaqs4l	= REAL [*,*]     : '       ',
Chaqs5r		= REAL [*,*]     : '       ',
CEhaqs5l	= REAL [*,*]     : '       ',
Cvaqs6r		= REAL [*,*]     : '       ',
CFvaqs6l	= REAL [*,*]     : '       ',
Chaqs7r		= REAL [*,*]     : '       ',
CGhaqs7l	= REAL [*,*]     : '       ',
Cvaqs8r		= REAL [*,*]     : '       ',
CHvaqs8l	= REAL [*,*]     : '       ',
CIhaqs11r	= REAL [*,*]     : '       ',
CJhaqs11l	= REAL [*,*]     : '       ',

BhabhaA41	= REAL [*,*]  : ' monitor inside.e-/outside.e+	',
BhabhaerrorB41	= REAL [*,*]  : ' Counting Error	',
BhabhaC42	= REAL [*,*]  : ' monitor inside.e+/outside.e-	',
BhabhaerrorD42	= REAL [*,*]  : ' Counting Error	',


ElectronintA41	= REAL [*,*]	: ' Single rate internal monitor 41',
ElectronextB41	= REAL [*,*]	: ' Single rate external monitor 41',
ElectronintC42	= REAL [*,*]	: ' Single rate internal monitor 42',
ElectronextD42	= REAL [*,*]	: ' Single rate external monitor 42',


coLTime         = REAL [*,*]  : ' Time of last collimator update       ',
coLhA8li	= REAL [*,*]	: ' At 8.5m Left Inside	Horiz	',
coLhB8lo	= REAL [*,*]	: ' At 8.5m Left Outside  Horiz	',
coLhC8ri	= REAL [*,*]	: ' At 8.5m Right Inside  Horiz	',
coLhD8ro	= REAL [*,*]	: ' At 8.5m Right Outside Horiz	',
coLvE8ld	= REAL [*,*]	: ' At 8.5m Left Down	Vertical',
coLvF8lu	= REAL [*,*]	: ' At 8.5m Left Upper	Vertical',
coLvG8rd	= REAL [*,*]	: ' At 8.5m Right Down	Vertical',
coLvH8ru	= REAL [*,*]	: ' At 8.5m Right Upper	Vertical',
coLvI100ld	= REAL [*,*]	: ' Same at 100 m ...		',
coLvJ100lu	= REAL [*,*]    : '       ',
coLvK100rd	= REAL [*,*]    : '       ',
coLvL100ru	= REAL [*,*]	: '  ... but only vertical	',
coLhM120li	= REAL [*,*]	: ' At 120m only Horiz		',
coLhN120lo	= REAL [*,*]     : '       ',
coLhO120ri	= REAL [*,*]     : '       ',
coLhP120ro	= REAL [*,*]     : '       ',
coLhQ420li	= REAL [*,*]     : ' At 420m inner and horiz only	',
coLhR420ri	= REAL [*,*]     : '       ',
coLhS640li	= REAL [*,*]     : ' And at 640m as well	',
coLhU640ri	= REAL [*,*]     : '       ',


VacTime	        = REAL [*,*]		: ' Time of vacuum reading   ',
VacpA431	= REAL [*,*]	: ' Q18 to Q11.L	',
VacpB439	= REAL [*,*]	: ' Q11 to Q05.L	',
VacgC445	= REAL [*,*]	: ' ZL2.L		',
VacpD447	= REAL [*,*]	: ' Q02 to QSC.L	',
VacpE451	= REAL [*,*]	: ' QSC to Q02.R	',
VacgF453	= REAL [*,*]	: ' ZL1.R	',
VacpG455	= REAL [*,*]	: ' ZL2.R	',
VacpH462	= REAL [*,*]	: ' Q18 to Q11.R',


IspecTime	= REAL [*,*]     : ' ISPEC measurement time 	',
IspeC           = REAL [*,*]     : '       ',



TimePlus	= REAL [*,*]	: ' Timestamp of positron readings	',
XPuplus(8)      = REAL [*,*]	: ' Pick-up raw data, X values		',
SXpuplus(8)	= REAL [*,*]	: ' Sigma X 			',
YPuplus(8)      = REAL [*,*]	: ' Pick-up raw data, Y values		',
SYpuplus(8)	= REAL [*,*]	: ' Sigma Y			',

XplUs		= REAL [*,*]	: ' IP Beam position X micrometers	',
XpRimeplus	= REAL [*,*]	: ' IP Beam angle X microrad		',
YplUs		= REAL [*,*]	: ' IP Beam position Y micrometers	',
YpRimeplus	= REAL [*,*]	: ' IP Beam angle Y microrad		',

cHX2xplus	= REAL [*,*]	: ' Quality of fit, X	',
cHY2yplus	= REAL [*,*]	: ' Quality of fit, Y	',


TimEminus	= REAL [*,*]	: ' Timestamp of electron readings	',
XpumiNus(8)	= REAL [*,*]	: ' Pick-up raw data, X values		',
sXpuminuS(8)	= REAL [*,*]	: ' Sigma X 				',
YpumiNus(8)	= REAL [*,*]	: ' Pick-up raw data, Y values		',
sYpuminuS(8)	= REAL [*,*]	: ' Sigma Y				',

XmInus		= REAL [*,*]	: ' IP Beam position X micrometers	',
XXprimeminus	= REAL [*,*]	: ' IP Beam angle X microrad		',
YmInus		= REAL [*,*]	: ' IP Beam position Y micrometers	',
YYprimeminus	= REAL [*,*]	: ' IP Beam angle Y microrad		',

ZXch2xminus	= REAL [*,*]	: ' Quality of fit, X			',
ZYch2yminus	= REAL [*,*]	: ' Quality of fit, Y			',

SsptiMe	        = REAL [*,*]     : ' Spares ',
Sspare1	        = REAL [*,*]     : '       ',
Sspare2	        = REAL [*,*]     : '       ',
Sspare3	        = REAL [*,*]     : '       ',
Sspare4	        = REAL [*,*]     : 'End of Standard4 ')
;

 LPZT
      :      'LEP Standard0 - Replaces bank LPZS\
              Number of columns \
              Number of rows = 1'

           STATIC

           SIZE 1,1

  
= (IndeX     = INTE [*,*]           : 'Bank header index STANDARD0',
DatestamP    = INTE [*,*]           : 'Bank header date',
ReadTime     = INTE [*,*]           : 'Bank header time',
VersionNum   = INTE [*,*]           : 'Bank header version,mode',

FiLlid	     = INTE [*,*]	    : ' Fill Identification Number',
RunNumber    = INTE [*,*]           : 'Runnumber,Fillnumber ',

PNom	     = REAL [*,*]           : ' Nominal beam momentum',
BVnom        = REAL [*,*]           : ' Nominal Low beta vertical',
NbunchPlus   = INTE [*,*]           : ' Number of bunches in + beam',
NbunchMinus  = INTE [*,*]           : ' Number of bunches in - beam',


eHallTime   = INTE [*,*]           : ' time of HALL reading ',
eHaLl       = REAL [*,*]           : ' Beam Energy - Hall probe ',
eFlipTime   = INTE [*,*]           : ' time of flip coil reading',
EFlip       = REAL [*,*]           : ' Beam Energy - Flip Coil',
eNmrTime    = INTE [*,*]           : ' time of NMR reading ',
enMR        = REAL [*,*]           : ' Beam Energy, NMR',

MtTime		= INTE [*,*]	: ' time of magnet temperature reading ',
MagtemP(34)	= REAL [*,*]	: ' Magnet Temperatures ',

cvhUM(16)       = REAL [*,*]    : ' CV group humidity, temp octs', 

EmTime		= INTE [*,*]     : ' time of measurement of emittance ',
EHplus(32)      = REAL [*,*]     : '              ',

ITime		= INTE [*,*]	 : '  time of measurement of intensity ',
IPlus(16)       = REAL [*,*]     : '              ',

TauTime	        = INTE [*,*]     : '              ',
TauPlus(16)     = REAL [*,*]     : '              ',


PolTime	        = INTE [*,*]	 : ' time of measurement of polarisation ',
PolPlus(16)	= REAL [*,*]     : '              ',

LUm(16)	        = REAL [*,*]     : ' Luminosity   ',


ColTime	        = INTE [*,*]     : ' Collimators  ',
CoLls(20)  	= REAL [*,*]     : '              ',

SaspTime(4)	= INTE [*,*]     : ' SPARES ',
SbSpare(16)     = REAL [*,*]     : ' end of standard0')
;

LPLT

     :      'LEP StandardL - Replaces bank LPLS\
              Number of columns =  \
              Number of rows = 1'

           STATIC

           SIZE 1,1
 

= (IndeX     = INTE [*,*]           : 'Bank header index STANDARDL',
DatestamP    = INTE [*,*]           : 'Bank header date',
ReadTime     = INTE [*,*]           : 'Bank header time',
VersionNum   = INTE [*,*]           : 'Bank header version',
 

rflpQTime	= INTE [*,*]		: ' Measurement time.      ',
BasefreQ   	= REAL [*,*]		: ' Base LEP RF frequency. ',
DeltafreQ	= REAL [*,*]		: ' Offset   RF frequency. ',
Af1(8)		= REAL [*,*]		: ' Spare ',

QsePlus	        = REAL [*,*]     : 'QS PLUS       ',
QseMinus	= REAL [*,*]     : 'QS MINUS      ',


LrfA(68)        = REAL [*,*]     : '  Cavities    ',

maGnTime	= INTE [*,*]     : ' Mag meas Time',

MbS(2)		= REAL [*,*]	: ' Main supplies ',
MbI(2)		= REAL [*,*]	: ' Insertion     ',


PWig3t(10)	= REAL [*,*]		: ' Wigglers ',


BdlSum(18)	= REAL [*,*]		: ' correctors ',


BUnchlng(8)	= REAL [*,*]     : ' Bunch length ',


pretZeL(16)	= REAL [*,*]	: ' Pretzel Separators ',

sDmoDe          = REAL [*,*]    : 'server analysis status',

Txl1Time(8)     = INTE [*,*]     : ' SPARES       ',
SiSpare(16)	= REAL [*,*]     : ' end of StandardL')
;

 LPFT
      :      'LEP Standard4 - Replaces bank LPFS\
              Number of columns \
              Number of rows = 1'

           STATIC

           SIZE 1,1


= (IndeX     = INTE [*,*]           : 'Bank header index STANDARD4',
DatestamP    = INTE [*,*]           : 'Bank header date',
ReadTime     = INTE [*,*]           : 'Bank header time',
VersionNum   = INTE [*,*]           : 'Bank header version',


PmsepTime	= INTE [*,*]     : ' Meas time of PMSEP   ',
PmSep		= REAL [*,*]     : '       ',


QlbTime  	= INTE [*,*]     : ' Low beta quads meas time ',
QS(11)   	= REAL [*,*]     : '       ',


CqTime		= INTE [*,*] : ' meas time of dipole currents ',
Cvh(16)		= REAL [*,*]     : '       ',

BHabha(4)       = REAL [*,*]  : ' Bhabha monitor de.e+	',

ElectronintA(4) = REAL [*,*]	: ' Single rate internal monitor ',


coLTime         = INTE [*,*]  : ' Time of last collimator update       ',
coLh(22)	= REAL [*,*]	: ' Collimators	',

VacTime	        = INTE [*,*]	: ' Time of vacuum reading   ',
VacpA(8)	= REAL [*,*]	: ' Vacuum 	',


IspecTime	= INTE [*,*]     : ' ISPEC measurement time 	',
IspeC           = REAL [*,*]     : '       ',



TimePlus    	= INTE [*,*]	: ' Timestamp of positron readings	',
XPuplus(38)     = REAL [*,*]	: ' Pick-up raw data, 		',


TimeMinus	= INTE [*,*]	: ' Timestamp of electron readings	',
XpuMinus(38)	= REAL [*,*]	: ' Pick-up raw data, 		',

SsptiMe(4)      = REAL [*,*]     : ' Spares ',
SSpare(16)      = REAL [*,*]     : '  end of standard4 ')
;


 BKGT
      :      'Background scalers\
              Number of columns \
              Number of rows = 1'

           STATIC

           SIZE 1,1

  
    = (Time1bkg     = INTE [*,*]           : 'Time word 1',
       Time2bkg     = INTE [*,*]           : 'Time word 2',
       bkgREads     = INTE [*,*]           : 'Read cycle number',
       LepcurP      = REAL [*,*]           : 'LEP current, positrons',
       LepcurE      = REAL [*,*]           : 'LEP current, electrons',
       BKgscal(192) = INTE [*,*]           : 'Bkg scalers')
      ;

 LZZT
      :      'LEP Standard0\
              Number of columns \
              Number of rows = 1'

           STATIC

           SIZE 1,1

  
=(DatestamP  = INTE [*,*]         : 'Bank read date, YYMMDD',
HourstamP    = INTE [*,*]         : 'Bank read time, HHMMSS',
ReadTime     = INTE [*,*]         : 'Bank header time, offset in sec',
VersionNum   = INTE [*,*]         : 'Bank header version,mode',

FiLlid	     = INTE [*,*]         : ' Fill Identification Number',
RunnUm       = INTE [*,*]         : 'Run no.',
FillnUm      = INTE [*,*]         : 'Fill no.',

ModeTime     = INTE [*,*]         : 'Time of Mode update, offset in sec',
MOde         = INTE [*,*]         : 'Machine Mode, spare',
ModeSpare    = INTE [*,*]         : 'Spare Mode',

PNom	     = REAL [*,*]         : ' Nominal beam momentum',
BVnom        = REAL [*,*]         : ' Nominal Low beta vertical',
nTrainplUs   = INTE [*,*]         : 'no. of trains, + beams ',
nTrainMinus  = INTE [*,*]         : 'no. of trains, - beams ',
nBunchPlus   = INTE [*,*]         : 'no. of bunches + trains ',
nBunchMinus  = INTE [*,*]         : 'no. of bunches - trains ',
BunchSepar   = INTE [*,*]         : ' Separation of bunches in train',

eHallTime   = INTE [*,*]          : 'time of HALL reading, offset in sec',
eHaLl       = REAL [*,*]          : 'Beam Energy - Hall probe ',
eFlipTime   = INTE [*,*]          : 'time of flip coil reading, offset in sec',
EFlip       = REAL [*,*]          : 'Beam Energy - Flip Coil',
eNmrtiMe    = INTE [*,*]          : 'time of NMR reading, offset in sec ',
NmrValue(6) = REAL [*,*]          : 'NMR readings ' ,
FlipInhib   = INTE [*,*]          : 'Flip Inhibit, Error',
enerGyeRror = INTE [*,*]          : 'Energy error',

MttiMe      = INTE [*,*]	: ' time of magnet temperature reading ',
MagtemP(34) = REAL [*,*]	: ' Magnet Temperatures ',

EmTime		= INTE [*,*]     : ' time of measurement of emittance ',
EHevpm(64)      = REAL [*,*]     : ' bunch emittances, H,V, +,-   ',

ITime		= INTE [*,*]	 : '  time of measurement of intensity ',
CurrE(16)       = REAL [*,*]     : ' bunch currents electrons  ',
CurrP(16)       = REAL [*,*]     : ' bunch currents positrons  ',

TcurrE(4)      = REAL [*,*]     : ' train currents electrons  ',
TcurrP(4)      = REAL [*,*]     : ' train currents positrons  ',

ScurrE(2)       = REAL [*,*]     : ' single beam currents electrons  ',
ScurrP(2)       = REAL [*,*]     : ' single beam currents positrons  ',
SumCurr         = REAL [*,*]     : ' DC current  ',

LifeTrain(8)    = REAL [*,*]     : ' Lifetime of bunch trains -,+ ',
LifeFamil(8)    = REAL [*,*]     : ' Lifetime of families -,+ ',
LifeElshort     = REAL [*,*]     : ' mean lifetime electrons, short ' ,
LifePoshort     = REAL [*,*]     : ' mean lifetime positrons, short ' ,
LifeNeg         = REAL [*,*]     : ' mean lifetime electrons  ' ,
LifepoS         = REAL [*,*]     : ' mean lifetime positrons  ' ,
LifeBoth        = REAL [*,*]     : ' mean lifetime both beams ' ,

BlTime	        = INTE [*,*]	 : ' measurement time, bunch length ',
BLength(32)     = REAL [*,*]     : ' Bunch length, streak cam +,- ',

GlobLumin(16)   = REAL [*,*]     : ' Global Luminosity   ',

GlumTime        = INTE [*,*]	 : ' time measurement, global lumin ',
GlumiA          = REAL [*,*]     : ' Ave Lumi, all IP s ',
GlumiErra       = REAL [*,*]     : ' Ave Lumi error, all IP s ',

AveLumipi(4)    = REAL [*,*]     : ' Average Luminosity, by IP  ',
AvelumErri(4)   = REAL [*,*]     : ' Average Lumi error, by IP   ',

SpeclumA        = REAL [*,*]     : ' Specific Ave Lumi, all IP s ',
SpeclumeRr      = REAL [*,*]     : ' Specific Ave Lumi error, all IP s ',

SpecLumi(4)     = REAL [*,*]     : ' Specific Ave Luminosity, by IP   ',
SpeclumeI(4)    = REAL [*,*]     : ' Specific Ave Lumi error, by IP ',


ColTime	        = INTE [*,*]     : ' measurement time, Collimators  ',
CoLls(12)  	= REAL [*,*]     : ' collimators  ',

SaspTime(4)	= INTE [*,*]     : ' SPARES ',
SbSpare(16)     = REAL [*,*]     : ' end of standard0')
;

LZLT

     :      'LEP StandardL\
              Number of columns =  \
              Number of rows = 1'

           STATIC

           SIZE 1,1
 

=(DatestamP  = INTE [*,*]         : 'Bank read date, YYMMDD',
HourstamP    = INTE [*,*]         : 'Bank read time, HHMMSS',
ReadTime     = INTE [*,*]         : 'Bank header time, offset in sec',
VersionNum   = INTE [*,*]         : 'Bank header version',
 

rflpQTime	= INTE [*,*]      : ' Measurement time. RF, offset in sec ',
BasefreQ   	= REAL [*,*]      : ' Base LEP RF frequency. ',
DeltafreQ	= REAL [*,*]      : ' Offset   RF frequency. ',
Af1(8)		= REAL [*,*]      : ' Spare ',

QsePlus	        = REAL [*,*]     : 'Q_s PLUS,  Synch freq/Rev freq  ',
QseMinus	= REAL [*,*]     : 'Q_s MINUS      ',


LrfA(96)        = REAL [*,*]     : '  Cavities    ',

maGnTime	= INTE [*,*]     : ' Mag meas Time',

MbS(2)		= REAL [*,*]	: ' Main supplies, reading, setting ',
MbI(2)		= REAL [*,*]	: ' Insertion     ',
QQ(3)           = REAL [*,*]    : ' Main quads, QD, QF, diff',

PWig3t(10)	= REAL [*,*]	: ' Wigglers ',


BdlSum(18)	= REAL [*,*]	: ' correctors ',

ZTime           = INTE [*,*]    : ' Separators,  Time, ZLV ',
vertZeL(16)	= REAL [*,*]	: ' Vertical Separators ',

sDmoDe          = INTE [*,*]    : 'server analysis status',
TabcOnd         = INTE [*,*]    : 'server analysis condition',

Txl1Time(8)     = INTE [*,*]    : ' SPARES       ',
SiSpare(16)	= REAL [*,*]    : ' end of StandardL')
;

 LZFT
      :      'LEP Standard4\
              Number of columns \
              Number of rows = 1'

           STATIC

           SIZE 1,1


=(DatestamP  = INTE [*,*]         : 'Bank read date, YYMMDD',
HourstamP    = INTE [*,*]         : 'Bank read time, HHMMSS',
ReadTime     = INTE [*,*]         : 'Bank header time, offset in sec',
VersionNum   = INTE [*,*]         : 'Bank header version',


PmsepTime	= INTE [*,*]    : ' Meas time of PM_SEP, offset in sec ',
PmSep		= REAL [*,*]    : ' PM_SEP      ',


QlbTime  	= INTE [*,*]    : ' Low beta quads meas time, offset in sec ',
QS(11)   	= REAL [*,*]    : '       ',


CqTime		= INTE [*,*]    : ' meas time, corr dipole curr, offset in sec',
Cvh(14)		= REAL [*,*]    : ' corrector currents      ',

ZqTime		= INTE [*,*]    : ' meas time, separators, offsetin sec ',
ZLvh(6)	        = REAL [*,*]    : ' separators, voltage, field  ',


BhaTime          = INTE [*,*]   :  ' Luminosity Time, offset in sec',
BHabha(10)       = REAL [*,*]   :  ' Luminosities, family, err  ',

SpecifLum(10)    = REAL [*,*]   : ' Specific Lum, family, err ',

Bkgrate(4)       = REAL [*,*]   : ' Bkg, interaction rate monitors  ',



coLTime         = INTE [*,*]    : ' Time of collimator update, offset in sec ',
coLh(30)	= REAL [*,*]	: ' Collimators	',

VacTime	        = INTE [*,*]	: ' Time of vacuum reading, offsetin sec',
VacpA(8)	= REAL [*,*]	: ' Vacuum 	',


IspecTime	= INTE [*,*]    : ' Spectrom magnet measurement time, offset ',
IspeC           = REAL [*,*]    : ' Spectrom magnet   ',


TimePlus    	= INTE [*,*]	: ' Timestamp of positron readings, offset',
XPuplus(38)     = REAL [*,*]	: ' Pick-up raw data, 		',


TimeMinus	= INTE [*,*]	: ' Timestamp of electron readings, offset',
XpuMinus(38)	= REAL [*,*]	: ' Pick-up raw data, 		',

SsptiMe(4)      = INTE [*,*]    : ' Spares times',
SSpare(16)      = REAL [*,*]    : '  end of standard4 ')
;
 
 LZAT
      :      'LEP Aleph1\
              Number of columns \
              Number of rows = 1'

           STATIC

           SIZE 1,1

  
=(DatestamP  = INTE [*,*]         : 'Bank read date YYMMDD',
HourstamP    = INTE [*,*]         : 'Bank time time HHMMSS',
ReadTime     = INTE [*,*]         : 'Bank header time offset in sec',
VersionNum   = INTE [*,*]         : 'Bank header version,mode',

ITccur(3)    = REAL [*,*]	  : 'ITC, inner,outer,layers',
ItcLyr(8)    = REAL [*,*]         : 'ITC currents by Layer',

VDet(5)      = REAL [*,*]         : 'VDET',
VdetSpr(16)  = REAL [*,*]         : 'Spares, VDET RAD MON',

TPc(10)      = REAL [*,*]         : 'TPC ',

LcaL(10)     = REAL [*,*]         : 'LCAL ',

SaMba(10)    = REAL [*,*]         : 'SAMBA',
SambaSp(10)  = REAL [*,*]         : 'SAMBA spares ',

BCal(6)      = REAL [*,*]         : 'BCAL ',

LCurr(6)     = REAL [*,*]         : 'LCAL current  ',

bKgOne(2)    = REAL [*,*]         : 'Bkg 1, interval  ',
bKoTime      = INTE [*,*]         : 'Bkg1 update time, offset in sec',

bKgtWo(2)    = REAL [*,*]         : 'Bkg 2, interval  ',
bKttiMe      = INTE [*,*]         : 'Bkg2 update time, offset in sec',

lUmiN(5)    = REAL [*,*]          : 'Luminosity ',
lUminTime   = INTE [*,*]          : 'time of LUM reading, offset in sec',

lUminBt(4)  = REAL [*,*]          : 'LUM bunch trains',

ISpec 	    = REAL [*,*]          : 'Magnet current ',
IcorR(2)    = REAL [*,*]          : 'Magnet Correction currents ',
 
PoSition(8) = REAL [*,*]          : 'ALEPH IP data    ',

PhDiode(4)  = REAL [*,*]          : 'Photo diodes  ',

TimeSpr(2)  = INTE [*,*]	  : 'spares time, offset in sec ',
SspaRe(20)  = REAL [*,*]          : 'end of aleph1')
;
 
 LZPT
      :      'LEP Page101\
              Number of columns \
              Number of rows = 1'

           STATIC

           SIZE 1,1

  
=(DatestamP  = INTE [*,*]    : 'Bank read date, YYMMDD',
HourstamP    = INTE [*,*]    : 'Bank read time, HHMMSS',
ReadTime     = INTE [*,*]    : 'Bank header time, offset in sec',
VersionNum   = INTE [*,*]    : 'Bank header version,mode',

FIllid       = INTE [*,*]    : 'Fill identification no.',
RunnUm       = INTE [*,*]    : 'Run no.',
FillnUm      = INTE [*,*]    : 'Fill no.',

ModeTime     = INTE [*,*]    : 'Time of Mode update, offset in sec',
MOde         = INTE [*,*]    : 'Machine Mode, spare',
ModeSpare    = INTE [*,*]    : 'Spare Mode',

ENom         = REAL [*,*]    : 'Beam momentum ',
BVnom        = REAL [*,*]    : 'Low beta vertical ',
nTrainplUs   = INTE [*,*]    : 'no. of trains, + beams ',
nTrainMinus  = INTE [*,*]    : 'no. of trains, - beams ',
nBunchPlus   = INTE [*,*]    : 'no. of bunches + trains ',
nBunchMinus  = INTE [*,*]    : 'no. of bunches - trains ',
BunchSep     = INTE [*,*]    : ' Bunch Separation ',

comMtimE     = INTE [*,*]    : 'Comment Time, offset in sec ',
coMMent(100) = INTE [*,*]    : 'Comment, 10 lines, 40 characters',

LCurr        = REAL [*,*]    : 'L3 solenoid current ',
LcurrTime    = INTE [*,*]    : 'L3 current update time, offset in sec',
ACurr        = REAL [*,*]    : 'ALEPH solenoid current ',
AcurrTime    = INTE [*,*]    : 'ALEPH current update time,offset in sec',
OCurr        = REAL [*,*]    : 'OPAL solenoid current ',
OcurrTime    = INTE [*,*]    : 'OPAL current update time,offset in sec',
DCurr        = REAL [*,*]    : 'DELPHI solenoid current ',
DcurrTime    = INTE [*,*]    : 'DELPHI current update time,offset in sec',

SpsinttIme   = INTE [*,*]    : ' SPS Intensity Time, offset in sec',
SpSint(4)    = REAL [*,*]    : ' SPS Intensity ',

StackTime    = INTE [*,*]    : ' Stack Intensity Time, offset in sec',

IntNeg(2)    = REAL [*,*]    : ' electron current, mA ',
IntPos(2)    = REAL [*,*]    : ' positron current, mA ',
IntTotal     = REAL [*,*]    : ' Total Current, mA ',

TauE         = REAL [*,*]    : ' electron lifetime, hrs ',
TauP         = REAL [*,*]    : ' positron lifetime, hrs ',
TotaltAu     = REAL [*,*]    : ' Average lifetime hrs ',

CoastTime    = REAL [*,*]    : ' Time into coast, hrs',

LumInit(4)   = REAL [*,*]    : ' Initial Luminosities: L,A,O,D ',
LumPres(4)   = REAL [*,*]    : ' Present Luminosities: L,A,O,D ',
LuminteG(4)  = REAL [*,*]    : ' Integrateed Luminosities: L,A,O,D ',


bKgL(2)      = REAL [*,*]    : ' Bkg 1 and bkg 2,  L3     ',
bKgA(2)      = REAL [*,*]    : ' Bkg 1 and bkg 2,  ALEPH  ',
bKgO(2)      = REAL [*,*]    : ' Bkg 1 and bkg 2,  OPAL   ',
bKgD(2)      = REAL [*,*]    : ' Bkg 1 and bkg 2,  DELPHI ',

LSmode       = INTE [*,*]    : 'Last Mode',
LsRunnr      = INTE [*,*]    : 'Last Run number',
lstImE       = INTE [*,*]    : 'Time, offset in sec',
FillTime     = INTE [*,*]    : 'Time of fill, offset in sec',

spareFLg(4)  = INTE [*,*]    : ' spare flags ',

CollState    = INTE[*,*]     : 'Collimator State',

spaRetimE(4) = INTE[*,*]     : ' Spare Times, offset in sec',
spAreS(16)   = REAL [*,*]    : ' Spare, end of PAGE101 ')
;

LZVT

     :      'LEP Vernier\
              Number of columns =  \
              Number of rows(experiment) = 4'

           STATIC


     = (xyslopePosA(4) = REAL [*,*] : 'x, xprime, y, yprime positrons side A ',
        xyslopeEleA(4) = REAL [*,*] : 'x, xprime, y, yprime electrons side A ',
        xyslopePosB(4) = REAL [*,*] : 'x, xprime, y, yprime positrons side B ',
        xyslopeEleB(4) = REAL [*,*] : 'x, xprime, y, yprime electrons side B ')
     ;


 END ESET

 END SUBSCHEMA


 SUBSCHEMA LepTABle96
 : 'LEP Data, Oracle '

 AUTHOR   'J. Rothberg,P.Bright'
 VERSION  '1.2'
 DATE     '28/06/96'

 DEFINE ESET


 LXDA
      :      'Lep bank date,time \
              Number of columns \
              Number of rows = number of banks'

           STATIC

= (BAnk           = CHA4       :' Bank name     ',
   DAte(2)        = INTE [*,*] :' Date,time     ' )

;

 LXIP
      :      'Lep Turbo IP, in LEP coordinates \
              Number of columns \
              Number of rows = 2'

           STATIC

= (SIde           = INTE [*,*] :' side=0,1',
   BUnch          = INTE [*,*] :' bunch   ',
   GainFlag       = INTE [*,*] :' gain flag, -1=no info',
   STatus         = INTE [*,*] :' status 0=OK, 2=no data ',
   XPos           = REAL [*,*] :'x, positrons, microns   ',
   SxPos          = REAL [*,*] :'sl x positrons, microradians  ',
   YPos           = REAL [*,*] :'y positrons, microns    ',
   SypoS          = REAL [*,*] :'sl y positrons, microradians  ',
   XEle           = REAL [*,*] :'x electrons, microns    ',
   SxEle          = REAL [*,*] :'sl x electrons, microradians  ',
   YEle           = REAL [*,*] :'y electrons, microns    ',
   SyeLe          = REAL [*,*] :'sl y electrons, microradians  ')

;
 
 LXUN
      :      'Lep Bunch parameters \
              Number of columns \
              Number of rows=number of bunches'

           STATIC

=( TRainnumber    = INTE [1,8]  :' Train number',
   WAgonnumber    = INTE [1,4]  :' Wagon number',
   CurrentElectr  = REAL [*,*]  :' Current, electrons   ',
   LifetimeElect  = REAL [*,*]  :' Lifetime,electrons ',
   QsElectrons    = REAL [*,*]  :' Synch tune,electrons ' ,  
   CurrentPositr  = REAL [*,*]  :' Current, positrons   ',
   LifetimePosit  = REAL [*,*]  :' Lifetime,positrons ',
   QsPositrons    = REAL [*,*]  :' Synch tune,positrons ' )

;
 
 LXLY
      :      'Lep Family parameters \
              Number of columns \
              Number of rows=number of families'

           STATIC

= (CurrentElectr  = REAL [*,*]  :' Current, electrons   ',
   LifetimeElect  = REAL [*,*]  :' Lifetime,electrons ',
   QsElectrons    = REAL [*,*]  :' Synch tune,electrons ' ,  
   CurrentPositr  = REAL [*,*]  :' Current, positrons   ',
   LifetimePosit  = REAL [*,*]  :' Lifetime,positrons ',
   QsPositrons    = REAL [*,*]  :' Synch tune,positrons ' )

;
 
 LXTR
      :      'Lep Train parameters \
              Number of columns \
              Number of rows=number of trains'

           STATIC

= (CurrentElectr  = REAL [*,*]  :' Current, electrons   ',
   LifetimeElect  = REAL [*,*]  :' Lifetime,electrons ',
   QsElectrons    = REAL [*,*]  :' Synch tune,electrons ' ,  
   CurrentPositr  = REAL [*,*]  :' Current, positrons   ',
   LifetimePosit  = REAL [*,*]  :' Lifetime,positrons ',
   QsPositrons    = REAL [*,*]  :' Synch tune,positrons ' )

;
 
 LXBM
      :      'Lep Total Beam parameters \
              Number of columns \
              Number of rows'

           STATIC

= (CurrentElectr  = REAL [*,*]  :' Current, electrons   ',
   LifetimeElect  = REAL [*,*]  :' Lifetime,electrons ',
   QsElectrons    = REAL [*,*]  :' Synch tune,electrons ' ,  
   CurrentPositr  = REAL [*,*]  :' Current, positrons   ',
   LifetimePosit  = REAL [*,*]  :' Lifetime,positrons ',
   QsPositrons    = REAL [*,*]  :' Synch tune,positrons ',
   HorizElec(2)   = REAL [*,*]  :' Horiz emittance,el',   
   HorizPosit(2)  = REAL [*,*]  :' Horiz emittance,pos',  
   VerticElec(2)  = REAL [*,*]  :' Vert emittance,el', 
   VerticPosit(2) = REAL [*,*]  :' Vert  emittance,pos'   )

;
 
 LXGB
      :      'Lep Global parameters \
              Number of columns \
              Number of rows '

           STATIC

= (MOde(5)        = CHA4        :' LEP Mode  eg. ACCELERATING   ',
   ModeNumber     = INTE [1,50] :' Mode number ',
   FIll           = INTE [1,*]  :' Fill number ' ,  
   NumberTrains   = INTE [1,8]  :' number of trains   ',
   NumberWagons   = INTE [1,8]  :' number of bunches per train  ',
   WagonSep       = INTE [*,*]  :' wagon spacing, RF wavelengths  ',
   ENominal       = REAL [*,*]  :' Energy, GeV ',
   BetaNominal    = REAL [*,*]  :' Vertical beta star',
   RFfreq         = REAL [*,*]  :' RF freq (units?)     ',
   EnergyHall     = REAL [*,*]  :' Energy, Hall probe   ',
   EnergyFlip     = REAL [*,*]  :' Energy, Flip coil    ',
   LUminosity     = REAL [*,*]  :' LEP Luminosity      ',
   LumiError      = REAL [*,*]  :' LEP Luminosity Error     ',
   SeParation     = REAL [*,*]  :' Nominal    ', 
   VErnier        = REAL [*,*]  :' Separation, fine tuning',
   LumFamily(2)   = REAL [*,*]  :' Luminosity per family   ',
   LumfameRr(2)   = REAL [*,*]  :' Luminosity per family, error   ',
   coMmentDate(2) = INTE [*,*]  :' Comment Date  ',    
   coMMent(100)   = CHA4        :' Operator Comment   ')

;
 
 LXER
      :      'Lep Vernier \
              Number of columns \
              Number of rows =number of points'

           STATIC

= (DAte(2)        = INTE [*,*] :' time of measurements',
   POsition       = REAL [*,*] :' position',
   LUmin          = REAL [*,*] :' lumin     ',
   ERrlum         = REAL [*,*] :' err lumin   ')

;
 
 LXFC
      :      'Lep point Four Collimators\
              Number of columns \
              Number of rows = number of collimators'

           STATIC

= (NAme(5)    = CHA4          :' Collimator name, eg. COLH.QS1B.R4.1 ',
 ColPosition  = REAL [*,*]    :' Collimator postion [mm] ')
;

 LXAC
      :      'Lep Aperture Collimators\
              Number of columns \
              Number of rows = number of collimators'

           STATIC

= (NAme(5)      = CHA4          :' Collimator name, eg. COLH.QS1B.R4.1 ',
   ColPosition  = REAL [*,*]    :' Collimator postion [mm] ')
;

 LXNM
      :      'Lep NMR\
              Number of columns \
              Number of rows = number of NMR readings'

           STATIC

= (NAme(5)      = CHA4          :' NMR name, eg. NMR_RE12 ',
   VAlue        = REAL [*,*]    :' Value ')
;

 LXSP
      :      'Lep Separators\
              Number of columns \
              Number of rows = number of Separators'

           STATIC

= (NAme(5)      = CHA4          :' Separator name, eg. CP4_GENM2P ',
   VAlue        = REAL [*,*]    :' Value ')
;

 LXVA
      :      'Lep Vacuum\
              Number of columns \
              Number of rows = number of Vacuum readings'

           STATIC

= (NAme(5)      = CHA4          :' Vacuum name, eg. VAC_P431 ',
   VAlue        = REAL [*,*]    :' Value ')
;

 LXWG
      :      'Lep Wigglers\
              Number of columns \
              Number of rows = number of Wigglers'

           STATIC

= (NAme(5)      = CHA4          :' Wiggler name, eg. MWDT_L7 ',
   VAlue        = REAL [*,*]    :' Value ')
;

 LXCR
      :      'Lep Correctors\
              Number of columns \
              Number of rows = number of Correctors'

           STATIC

= (NAme(5)      = CHA4          :' Corrector name, eg. CHA_QS11_L4 ',
   VAlue        = REAL [*,*]    :' Value ')
;

 LXQS
      :      'Lep Quadrupoles\
              Number of columns \
              Number of rows = number of Quadrupoles'

           STATIC

= (NAme(5)      = CHA4          :' Quadrupole name, eg. QS0_L4 ',
   VAlue        = REAL [*,*]    :' Value ')
;

 LXRF
      :      'Lep RF system \
              Number of columns \
              Number of rows = number of RF units'

           STATIC

= (NAme(5)        = CHA4       :' RF unit name, eg.         ',
   HEater(3)      = CHA4       :' Heater1 ',
   HeAterr(3)     = CHA4       :' Heater2 ',
   CircuitBkr(3)  = CHA4       :'        ',
   VoltageLoop(3) = CHA4       :'        ',
   DiffLoop(3)    = CHA4       :'        ',
   PowerSupply    = REAL [*,*] :' kV     ',
   KlystronFwd(2) = REAL [*,*] :' kW     ',
   KlystronRef    = REAL [*,*] :' kW     ',
   RFvoltage      = REAL [*,*] :' MV     ',
   DetSum         = REAL [*,*] :' cavity det sum (%)  ',
   CavityFwd      = REAL [*,*] :' Power (kW)   ',
   CavityReflect  = REAL [*,*] :' Reflected Power (kW)   ')

;
 
 LXHA
      :      'Lep bank date,time in SOR \
              Number of columns \
              Number of rows = number of banks'

           STATIC



= (BAnk           = CHA4       :' Bank name     ',
   DAte(2)        = INTE [*,*] :' Date,time     ' )

;

 LXKG
      :      'ALEPH BKG bank  \
              Number of columns \
              Number of rows '

           STATIC


= (ITccurrent     = REAL [*,*]  :' ITC current ',
   TpccurrLeft    = REAL [*,*]  :' TPC curr Left (A)',
   TpccurrRight   = REAL [*,*]  :' TPC curr Right(B)',
   SambaPhotl     = REAL [*,*]  :' Samba Photons, Left   ',
   SambaphotR     = REAL [*,*]  :' Samba Photons, Right  ',
   SambaElecl     = REAL [*,*]  :' Samba Electrons Left  ',
   SambaeleCr     = REAL [*,*]  :' Samba Electrons Right ',
   LcalLeft       = REAL [*,*]  :' LCAL rate Left     ',
   LcalRight      = REAL [*,*]  :' LCAL rate Right      ' ,  
   BcalLeft       = REAL [*,*]  :' BCAL rate Left     ',
   BcalRight      = REAL [*,*]  :' BCAL rate Right      ' ,  
   Bkg1           = REAL [*,*]  :' BKG1 figure of merit(TPC) ',
   Bkg2           = REAL [*,*]  :' BKG2 figure of merit ',
   Bkg3           = REAL [*,*]  :' BKG3 figure of merit ',
   RadmonU        = REAL [*,*]  :' Radmon Up ',
   RadmonD        = REAL [*,*]  :' Radmon Down ',
   RadmonI        = REAL [*,*]  :' Radmon In ',
   RadmonO        = REAL [*,*]  :' Radmon Out',
   RadmonAve      = REAL [*,*]  :' Radmon Ave',
   RadmonMax      = REAL [*,*]  :' Radmon Max')
;
 
 RFVC
      :      'ELEPRF RF Voltage Checks \
              Number of columns \
              Number of rows'

           STATIC

= (LEft(4)        = REAL [*,*] :' Left sum  (MV)',
   RIght(4)       = REAL [*,*] :' Right sum (MV)',
   ASym           = REAL [*,*] :' Asymmetry over all IPs (MV)')
;

 RFSA 
      :      'ELEPRF measured SAwtooth \
              Number of columns \
              Number of rows'

           STATIC

= (LeftMeas(4)    = REAL [*,*] :' Left Sawtooth per IP, Measured',
   RightMeas(4)   = REAL [*,*] :' Right Sawtooth per IP, Measured',
   AllMeas        = REAL [*,*] :' Total Sawtooth all IPs, Measured')
;

 RFWT 
      :      'ELEPRF calculated saWTooth \
              Number of columns \
              Number of rows'

           STATIC

= (LeftCalc(4)    = REAL [*,*] :' Left Sawtooth per IP, Calculated',
   RightCalc(4)   = REAL [*,*] :' Right Sawtooth per IP, Calculated',
   AllCalc        = REAL [*,*] :' Total Sawtooth all IPs, Calculated')
;

 RFRB 
      :      'ELEPRF measured RBom \
              Number of columns \
              Number of rows'

           STATIC
                                                                 
= (RbomMeas(4)    = REAL [*,*] :' Rbom per IP, Measured',
   rbomTotalMeas  = REAL [*,*] :' Rbom Total (denominator), Measured')
;

 RFOM 
      :      'ELEPRF calculated rbOM \
              Number of columns \
              Number of rows'

           STATIC
                                                                 
=  (RbomCalc(4)    = REAL [*,*] :' Rbom per IP, Calculated',
    rbomTotalCalc  = REAL [*,*] :' Rbom Total (denominator), Calculated')
;

 RFUN
      :      'ELEPRF calculated bunch parameters \
              Number of columns \
              Number of rows=number of bunches'

           STATIC

= (TRainnumber    = INTE [1,8]  :' Train number',
   WAgonnumber    = INTE [1,4]  :' Wagon number',
   QsElecalc      = REAL [*,*]  :' Synch tune,electrons ' ,  
   PhaseElecalc   = REAL [*,*]  :' Phase,electrons ',
   QsPoscalc      = REAL [*,*]  :' Synch tune,positrons ' ,  
   PhasePoscalc   = REAL [*,*]  :' Phase,positrons ',
   PZcalc(4)      = REAL [*,*]  :' Z-direction boost per IP (MeV/c)',
   DEcmcalc(4)    = REAL [*,*]  :' Energy offset per IP (MeV)',
   dZVtxcalc(4)   = REAL [*,*]  :' Z Vertex shift per IP (mm)')
;
 
 RFLY
      :      'ELEPRF calculated Family parameters \
              Number of columns \
              Number of rows=number of families'

           STATIC

= (QsElectrons    = REAL [*,*]  :' Synch tune,electrons ' ,  
   PhaseElect     = REAL [*,*]  :' Phase,electrons ',
   QsPositr       = REAL [*,*]  :' Synch tune,positrons ' ,  
   PhasePositr    = REAL [*,*]  :' Phase,positrons ',
   PZcalc(4)      = REAL [*,*]  :' Z-direction boost per IP (MeV/c)',
   DEcmcalc(4)    = REAL [*,*]  :' Energy offset per IP (MeV)',
   dZVtxcalc(4)   = REAL [*,*]  :' Z Vertex shift per IP (mm)')
;
 
 RFTR
      :      'ELEPRF calculated Train parameters \
              Number of columns \
              Number of rows=number of trains'

           STATIC

= (QsElecalc      = REAL [*,*]  :' Synch tune,electrons ' ,  
   PhaseElecalc   = REAL [*,*]  :' Phase,electrons ',
   QsPoscalc      = REAL [*,*]  :' Synch tune,positrons ' ,  
   PhasePoscalc   = REAL [*,*]  :' Phase,positrons ',
   PZcalc(4)      = REAL [*,*]  :' Z-direction boost per IP (MeV/c)',
   DEcmcalc(4)    = REAL [*,*]  :' Energy offset per IP (MeV)',
   dZVtxcalc(4)   = REAL [*,*]  :' Z Vertex shift per IP (mm)')
;
 
 RFBM
      :      'ELEPRF calculated Total Beam parameters \
              Number of columns \
              Number of rows'

           STATIC

= (QsElecalc      = REAL [*,*]  :' Synch tune,electrons ' ,  
   PhaseElecalc   = REAL [*,*]  :' Phase,electrons ',
   QsPoscalc      = REAL [*,*]  :' Synch tune,positrons ' ,  
   PhasePoscalc   = REAL [*,*]  :' Phase,positrons ',
   PZcalc(4)      = REAL [*,*]  :' Z-direction boost per IP (MeV/c)',
   DEcmcalc(4)    = REAL [*,*]  :' Energy offset per IP (MeV)',
   dZVtxcalc(4)   = REAL [*,*]  :' Z Vertex shift per IP (mm)')
;
 
 RFQU
      :      'ELEPRF QUality of inputs and outputs \
              Number of columns \
              Number of rows \'

           STATIC

= (FCall          = INTE [0,10] :' Flag for Calling ELEPRF',
   WQual          = INTE [0,512]:' Word for Quality of outputs',
   WPhys          = INTE [0,512]:' Word for Physics usage',
   WSpare         = INTE [*,*]  :' Word Spare')
;
 
 
 END ESET

 END SUBSCHEMA
   

