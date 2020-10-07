 SUBSCHEMA InternalBOOKkeep
 :   ' Internal Book keeping production banks'
 
 AUTHOR   'J. Boucrot'
 REVIEWER 'A. Putzer'
 VERSION  '3.0'
 DATE     '28/11/97'
 
 DEFINE ESET
 BDES
       :      'Internal description of some Bookeeping bank properties (NR=0)\
               Number of words per bank\
               Number of banks for which there is a description'
           STATIC
 
       = (BankName     = CHA4           : 'Name of bank',
          NumbElem     = INTE [0,100]   : 'Number of rows in bank',
          NumbChar    = INTE [0,100]    : 'Number of character variables + 1 in the bank',
          ListSt(17)  = INTE [0,100]    : 'List of starting addresses for char. var.')
 
       ;
 
 
 BPRO
       :      'PROgram conditions for the production of a Dataset(NR=Period Number)\
               Number of words/program condition\
               Number of program conditions'
           STATIC
 
       = (PName        = CHA4           : 'Program Name : ONL,KING,GAL,JUL,ALPH',
          OPsys(2)     = CHA4           : 'Operating System : IBM VM,VAX VMS,CRAY',
          NodNam(2)    = CHA4           : 'computer node name where the\
                                            production program has been run',
          VersNumb     = INTE [1,1000]  : 'Program Version number',
          CorrVers     = INTE [1,*]     : '1000*JULIA Version Number ( for all datasets )\
                                           + GALEPH Version Number ( for MCarlo datasets )',
          AlVers       = INTE [1,9999]  : 'Alephlib Version number',
          DbasNumb     = INTE [1,9999]  : 'Database Version number',
          DfilDbas     = INTE [0,*]     : 'Definition of file number for Database for multifile volumes',
          NatInp       = INTE [0,15]    : 'Nature of Input data , if any:\
                                            + 10 , if selected data \
                                             0  No input\
                                             1  Kingal output\
                                             2  RAW   \
                                             3  POT  \
                                             4  DST \
                                             5  MINI \
                                             6  Micro \
                                             7  NANO ')
       ;
 
 
    BRLI
    :     'List of runs for multi-run datasets (NR=period number)\
           Number of words per period\
           Number of periods'
       STATIC
 
         =   ( RunNum        =   INTE [0,*] : 'Run number',
               NumbEven      =   INTE [0,*] : 'Number of events in this run',
               NumbByt       =   INTE [0,*] : 'Number of bytes written for this run')
 
        ;
 
 
 BSET
       :      'Data SET description (NR=Period Number)\
               Number of words per dataset\
               Number of datasets'
           STATIC
 
       = (ADname(5)    = CHA4            : 'Unique Aleph Data Set Name\
                                            built automatically by the program creating the dataset',
          AUthprod(2)  = CHA4            : 'Name of author of production',
          FRunnumb     = INTE [1,*]      : 'First run number on the set',
          FEvnumb      = INTE [0,*]      : 'First event number of first run',
          LRunnumb     = INTE [1,*]      : 'Last run number on the set',
          LEvnumb      = INTE [0,*]      : 'Last event number of last run',
          WritDate     = INTE [0,*]      : 'Date of writing : yymmdd',
          WritHour     = INTE [0,*]      : 'Hour of writing : hhmmss',
          NatData      = INTE [0,15]     : 'Nature of data of this dataset:\
                                             1  Kingal output\
                                             2  RAW   \
                                             3  POT  \
                                             4  DST \
                                             5  MINI \
                                             6  Micro \
                                             7  NANO ',
          BeamNom      = INTE [0,*]      : 'Beam Nominal energy in Mev ( defined for MCarlo only ) ',
          DiscFlag     = INTE [0,1]      : 'Discard flag . Set to 1  if this\
                                            data set has to be deleted during\
                                            the next database update',
          BankAdded    = INTE [0,*]      : 'first row # in BRLI for forst run\
                                            = 0 if only 1 run on the dataset')
       ;
 
 
    BSPF
    :     'List of specifications for dataset (NR=Period Number)\
           Number of words per specification\
           Number of specifications'
       STATIC
 
         =   ( COmment (20)  =   CHA4       : 'Comments to explain what is on the dataset',
               KinCode       = INTE[0,64000]: 'Kingal event generator code , = 0 for real data',
               FirstRanm     = INTE[0,30000]: 'First Ranmar seed in KINGAL , = 0 for real data',
               SecondRanm    = INTE[0,*]    : '100000*Special Particle Code +Second Ranmar seed in KINGAL\
                                               = 0 for Real Data',
               RunGeometry   = INTE[*,*]    : '100*Year of Galeph Geometry + Geometry version number\
                                                only for MCarlo data , = 0 for Real data ')
 
        ;
 
 BTAP
       :      'content of TAPe or Cartridge from production jobs(NR=Period Number)\
               Number of words per tape/cartridge\
               Number of written tape/cartridges in dataset'
           STATIC
 
       = (VSn(2)       = CHA4           : 'Visual Serial Number',
          FOrmat       = CHA4           : 'Format : EPIO or NATV',
          RecNumb      = INTE [0,*]     : 'Number of records',
          LRecl        = INTE [0,*]     : 'Length of record ( in bytes)',
          BLksize      = INTE [0,*]     : 'Block size',
          NumbFiles    = INTE [0,*]     : 'Number of files on tape/cartridge')
 
       ;
 
 
 END ESET
 
    DEFINE RSET
 
       (BSET [0,1] -> [1,*] BTAP)
         : 'Index in BTAP of the tape/cartridge on which this data set is stored';
 
 
       (BSET [0,1] -> [1,1] BRLI)
        : 'Index in BRLI of the last run # belonging to this dataset';
 
       (BSET [0,1] -> [1,1] BSPF)
         : 'Index in BSPF of the specifications of this dataset';
 
       (BSET [1,1] -> [0,*] BPRO)
         : 'Index in BPRO of the production conditions of this data set';
 
 
    END RSET
 
 END SUBSCHEMA
 
 
 SUBSCHEMA DafBOOKkeeping
 :  '  DAF book keeping banks'
 
 AUTHOR   'J. Boucrot'
 REVIEWER 'A. Putzer'
 VERSION  '4.7'
 DATE     '25/04/01'
 
 DEFINE ESET
 BCAR
       :      'Cartridge list for TMS update , bank built in UPDABOOK job
               NR = 1000\
               Number of words per tape/cartridge\
               Number of tape/cartridges '
           STATIC
 
       = (VSn(2)       = CHA4           : 'Visual Serial Number',
          VId(2)       = CHA4           : 'Visual Identifier ( needed in some Computer centres)',
          LAbel(2)     = CHA4           : 'Tape/cartridge label',
          TYpe(3)      = CHA4           : 'Type :tape or cartridge ,\
                                           includes density value',
          LOcat(3)     = CHA4           : 'Location name',
          MaName(2)    = CHA4           : 'Name of manufacturer',
          InName(3)    = CHA4           : 'Name of institute which purchased it',
          PDate(2)     = CHA4           : 'Date of purchase : DD/MM/YY',
          COmpbook     = CHA4           : 'Name of computer system  from which it\
                                           can be booked : IBM or VAX',
          OwName(3)    = CHA4           : 'Name of the user who has reserved it',
          ResDate(2)   = CHA4           : 'Date (DD/MM/YY ) of last status modification',
          ResTime(2)   = CHA4           : 'Time (hh.mm.ss ) of last status modification',
          CoMment(15)  = CHA4           : 'Comment to explain what is written on this device\
                                           when it has been booked for "private" use',
          DataName(5)    =  CHA4        :  'Aleph Unique DatasetName of the dataset written on this device\
                                            if it contains an "official" Aleph dataset ',
          WriteNumb    = INTE [0,100]   : 'Status during TMS processing\
                                           = 0 if the tape was NOT YET processed successfully by TMS\
                                           = 100 if it was processed successfully by TMS',
          ResFlag      = INTE [0,100]   : 'Reservation Flag \
                                           = 0 if the tape has to to be processed by TMS\
                                           = 100 if the tape has to be FREED by TMS\
                                           = 10000 if the tape has to be SCRATCHED by TMS')
       ;
 
 BDAD
       :      'List and description of the split ALEBOOK databases
               NR = 1\
               Number of words per database\
               Number of databases '
           STATIC
 
       = (bankNumbeR   = INTE [0,*]     : 'Bank number of BDFS bank for this database',
          PeriodMin    = INTE [0,*]     : 'Minimum period number for this database',
          PeriodmaX    = INTE [0,*]     : 'Maximum period number for this database',
          NamdB(2)     = CHA4           : 'name of database',
          LisBank(25)  = CHA4           : 'List of banks written on this database')
       ;

 BKM7
        :    'Description of 1997 MCarlo datasets, NR = KINGAL generator code\
              Number of words per dataset\
              Number of datasets'
         STATIC

         = ( KeyWord(3)     =   CHA4   : 'Keyword used for alprod comment',
             DEscrip(15)    =   CHA4   : 'Detailed description of keyword')

   ;

 BKK7
        :    'Description of LEP2 MCarlo datasets, NR = KINGAL generator code\
              Number of words per dataset\
              Number of datasets'
         STATIC

         = ( IYeog          =   INTE[0,2001]  : ' GALEPH Year of Geometry',
             KeyWord(15)    =   CHA4          : 'Keyword used for alprod comment',
             DEscrip(15)    =   CHA4          : 'Detailed description of keyword',
             XSection       =   REAL[0.,*]    : 'Cross-section of production in picobarns',
             XsecError      =   REAL[0.,*]    : 'Error on Cross-section of production in picobarns')
   ;

 
 BDAT
        :    'Dates of  DAF updates\
              Number of words per DAF update\
              Number of DAF updates'
         STATIC
 
         = ( DayUpdate(2)   =   CHA4   : 'Day of last DAF update MM/DD/YY',
             TimUpdate(2)   =   CHA4   : 'Time of last DAF update hh.mm.ss')
 
        ;
 
 BRDL
       :      'summary bank for DLT copies of LEP1 RAW data up to 1994 included    
               NR = IYEAR, from 1991 to 1994\
               Number of words per dataset\
               Number of datasets'
           STATIC

       = (TapeCart(2)  = CHA4            : 'DLT name for this dataset ',
          LOcation     = CHA4            : 'Physical location of DLT  , from TMS ',
          FileNum      = INTE[0,*]       : 'File number on DLT',                                
          RunNum       = INTE [0,*]      : 'Run number stored in this file')             
     
        ;

 BDFS
       :      'summary bank for fast scan of datasets in the ALEBOOK database
               NR = 1 for real data
               NR = 2 for simulated data\
               Number of words per dataset\
               Number of datasets'
           STATIC
 
       = (TapeCart(2)  = CHA4            : 'Tape or cartridge name for this dataset ',
          LOcation     = CHA4            : 'Physical location of tape , from TMS ',
          COpyflag     = INTE[0,1]       : ' = 1 if this dataset is a copy of another dataset',
          NPer         = INTE [0,*]      : 'BSET bank number of original dataset',
          NRow         = INTE [0,*]      : 'Row number in bank BSET NR=NPER , of original dataset',
          DiscFlg      = INTE [0,*]      : '=1 if dataset to be discarded',
          FileNumb     = INTE [0,*]      : '= File number for datasets written on multifile tapes')
 
        ;
 
 BDRQ
       :      'Detector Run_Quality bank for data of 1994 onwards ,
               NR = Period number\
               Number of words/run\
               Number of runs'
           STATIC
 
       = (RunNumber    = INTE [0,*]      : 'Run Number',
          DetecPatt(3) = CHA4            : 'Detector Pattern for Run_Quality , 12 detectors max')
 
        ;
 
 
 BEXA
       :      'Description of Exabytes contents in the ALEBOOK database\
               Number of words per Exabyte\
               Number of Exabytes'
           STATIC
 
       = (AXtape       = INTE [0,9999]   : 'AX Exabyte tape number',
          NPer         = INTE [0,*]      : 'BSET bank number of original dataset',
          DiscFlg      = INTE [0,1]      : '=1 if dataset to be discarded',
          FrowbX       = INTE [0,*]      : 'Row number in bank BXLI , of first cassette copy',
          LrowbX       = INTE [0,*]      : 'Row number in bank BXLI , of last  cassette copy',
          NRow         = INTE [0,*]      : 'Row number in bank BDFS , of first cassette copy')
 
        ;
 
 BKLI
        :    'KINGAL codes List\
              Number of words per Kingal generator\
              Number of KINGAL generators'
         STATIC
 
         = ( KinName(2)   =  CHA4        : 'Kingal generator Name in Aleph convention',
             KinCode      =  INTE [0,*]  : 'Kingal Code in Aleph convention')
 
        ;
 
 
 BKMC
        :    'MCarlo tape description\
              Number of words per MCarlo tape\
              Number of MCarlo tapes'
         STATIC
 
         = ( VSn(2)       =  CHA4        : 'Visual Serial Number of MCarlo tape',
             MDpoint      =  INTE [0,*]  : 'Pointer to BKMD row')
 
        ;
 
 
 BKMD
        :    'Mcarlo Dataset description\
              Number of words per MCarlo dataset\
              Number of MCarlo datasets'
         STATIC
 
         = ( IYear        =  INTE [0,*]  : 'Year of GALEPH geometry',
             NEvents      =  INTE [0,*]  : 'Number ofgenerated events in dataset',
             XSection     =  INTE [0,*]  : '1000* generated cross section in nb**-1',
             JulVers      =  INTE [0,*]  : '100*  JULIA version number including correction',
             GalVers      =  INTE [0,*]  : '100* GALEPH version number including correction',
             MNpoint      =  INTE [0,*]  : 'Pointer to BKMN row')
 
        ;
 
 BKMN
        :    'MCarlo dataset Notice\
              Number of words per MCarlo notice\
              Number of MCarlo Notices'
         STATIC
 
         = ( SPecial(4)   =  CHA4        : 'Special notice for 2-Photon events',
             NOtice(60)   =  CHA4        : 'Notice from MCPROD description file')
 
        ;
 
 
 BLAS
       :      'Values of LASt runs known from various sources\
               Number of words per series of runs\
               Number of series'
           STATIC
 
      = (LastOnl          = INTE [0,*]    : 'Last run known in the ALEBOOK database',
         lastOnlUpd       = INTE [0,*]    : 'Last run updated from Online Logbook',
         lastMAkj         = INTE [0,*]    : 'Last run with provisional Lumi from Julia',
         lastRunQ         = INTE [0,*]    : 'Last run with Run_Quality determined',
         LastlEp          = INTE [0,*]    : 'Last run with exact , updated LEP energy',
         LastlUmi         = INTE [0,*]    : 'Last run with definitive Luminosity',
         LastFill         = INTE [0,*]    : 'Last Fill known in the ALEBOOK database',
         lastP4           = INTE [0,*]    : 'Last 4-bunch run with provisional Lumi from Julia',
         lastD4           = INTE [0,*]    : 'Last 4-bunch run with definitive  Luminosity',
         lastMIni         = INTE [0,*]    : 'Last version number of the MINI making program',
         lastNAno         = INTE [0,*]    : 'Last version number of the NANO making program')
       ;
 BLPR
       :      'Description of RUN ranges for data reprocessings\
               Number of words per reprocessing \
               Number of reprocessings'
           STATIC
 
      = (NadaT            = INTE [0,*]    : 'Nature of Data 1 = KIN , 2 = RAW , 3 = POT , 4 = DST , 5 = MINI',
         FirstRun         = INTE [0,*]    : 'First Run Number ',
         LastRun          = INTE [0,*]    : 'Last Run Number ',
         FirstDay         = INTE [0,*]    : 'First Day of reprocessing , yymmdd ',
         LastDay          = INTE [0,*]    : 'Last Day of reprocessing , yymmdd',
         ReprLev          = INTE [0,*]    : 'Reprocessing level 1 = first , 2 = second etc',
         TrackQual        = INTE [0,*]    : 'Flag for tracking quality for e.g 1992 data',
         JulVers          = INTE [0,*]    : 'JULIA version number for this processing')
 
       ;
 BMCL
       :      'Statistics on data and Monte-CarLo productions\
               Number of words per series of prod\
               Number of prods'
           STATIC
 
      = (KinGal           = INTE [0,*]    : 'KINGAL Generator Aleph number , = 0 for data',
         LAbident         = INTE [0,*]    : 'Homelab internal identifier as defined in BKINSC',
         TOtaldset        = INTE [0,*]    : 'Total number of datasets for this Homelab',
         KInsets          = INTE [0,*]    : 'Number of KINGAL datasets',
         RAwsets          = INTE [0,*]    : 'Number of RAW datasets',
         POtsets          = INTE [0,*]    : 'Number of POT datasets',
         DStsets          = INTE [0,*]    : 'Number of DST datasets',
         MInisets         = INTE [0,*]    : 'Number of MINI datasets',
         miCRsets         = INTE [0,*]    : 'Number of MICRO datasets',
         NAnosets         = INTE [0,*]    : 'Number of NANO datasets',
         OThersets        = INTE [0,*]    : 'Number of Other datasets')
       ;
 
 BSCL
       :      'Size in megabytes of data and Monte-CarLo productions\
               Number of words per series of prod\
               Number of prods'
           STATIC
 
      = (KinGal           = INTE [0,*]    : 'KINGAL Generator Aleph number , = 0 for data',
         LAbident         = INTE [0,*]    : 'Homelab internal identifier as defined in BKINSC',
         TOtaldset        = INTE [0,*]    : 'Total size of datasets for this Homelab',
         KInsets          = INTE [0,*]    : 'size of KINGAL datasets',
         RAwsets          = INTE [0,*]    : 'size of RAW datasets',
         POtsets          = INTE [0,*]    : 'size of POT datasets',
         DStsets          = INTE [0,*]    : 'size of DST datasets',
         MInisets         = INTE [0,*]    : 'size of MINI datasets',
         miCRsets         = INTE [0,*]    : 'size of MICRO datasets',
         NAnosets         = INTE [0,*]    : 'size of NANO datasets',
         OThersets        = INTE [0,*]    : 'size of Other datasets')
       ;
 
 
 BTHO
       :      'Homelabs list and tapes properties\
               Number of words per Homelab\
               Number of labs'
           STATIC
 
      = (NameLab(2)       = CHA4          : 'Name of Homelab',
         TApident         = CHA4          : '2 first letters = Tape identifier e.g. AM or AS\ 
                                             3rd letter = 1st letter of Tape category : Cart or Dlt or Exab\ 
                                             4th letter = Tape Label :\
                                             A for ANSI , S for Standard , N for No label',             
         FIrstnumb        = INTE [0,9999] : 'First tape number in list',
         Lastnumb         = INTE [0,9999] : 'Last tape number in list',
         HOmenumb         = INTE [0,9999] : 'Homelab number in SCANBOOK database')
       ;
 
 
 BNAM
    :     'List of Aleph authorized users for Bookkeeping,
           NR = 1 for IBM computers,
           NR = 2 for DEC computers and workstations,
           NR = 3 for CRAY,
           NR = 4 for other computers\
           Number of words per user\
           Number of users'
       STATIC
 
         =   ( UserName(9)   =   CHA4       : 'User Name and first name (36 char max) ',
               UserLogin(3)  =   CHA4       : 'USER login name on this computer',
               ReserF(2)     =   CHA4       : 'Reserved for future use')
 
        ;
 BOFS
    :     'Offsets of periods description in BDFD banks,
           NR = 1 for Monte - Carlo data,
           NR = 2 for real data\
           Number of words per period\
           Number of periods'
       STATIC
 
         =   ( NPer          =   INTE [*,*] : 'Period number ',
               OFset         =   INTE [1,*] : 'Offset in BFSD or BFSM for this period',
               LenPer        =   INTE [0,*] : 'Length of data in this period')
 
        ;
 BPER
       :      'Description of valid data taking PERiod numbers(NR=0)\
               Number of words per period number\
               Number of period numbers'
           STATIC
 
      = (PeriodnAme(10)   = CHA4          : 'Period Name',
         PeriodNumb       = INTE [0,*]    : 'Period Number for the Bookkeeping System',
         LepPernum        = INTE [0,*]    : 'Lep Period Number ',
         FirstExp         = INTE [0,*]    : 'First Lep Fill Number in Period',
         LastExp          = INTE [0,*]    : 'Last Lep Fill Number in Period',
         FirstRun         = INTE [0,*]    : 'First Run Number in Period',
         LastRun          = INTE [0,*]    : 'Last Run Number in Period',
         FirstDay         = INTE [0,*]    : 'First Day of period , yymmdd ',
         LastDay          = INTE [0,*]    : 'Last Day of period , yymmdd')
 
        ;
 
 BPHY
        :    'list of PHYsics group\
              Number of words per PHY group\
              Number of PHY groups'
         STATIC
 
         = ( KEyword        =   CHA4   : 'Keyword for Physics Group',
             NAmegrou(5)    =   CHA4   : 'Name of Physics Group',
             MApofgr(2)     =   CHA4   : 'Map of groups/tools setup',
             DeFkeys(30)    =   CHA4   : 'Definition of Lep II physics groups')
        ;
 
 BPLI
       :      'Particle list to be known for MCarlo datasets(NR=0)\
               Number of words per particle\
               Number of particles'
           STATIC
 
      = (ParticleNa(3)    = CHA4          : 'Particle Name',
         PartiCode        = INTE [0,*]    : 'Particle code from ALPHA',
         AntipCode        = INTE [0,*]    : 'Antiparticle code ')
 
       ;
 
 BRAN
        :    'Ranmar allocations \
              Number of words per Ranmar allocation\
              Number of labs with Ranmar allocation'
         STATIC
 
         = ( InstName(4)   =   CHA4         : 'Name of institute',
             NumInst       =   INTE[0,64]   : 'Number allocated to the institute')
 
        ;
 
 
 BRUC
       :      'RUN Conditions (NR=Period Number),
               defined only for REAL DATA\
               Number of words/run\
               Number of runs'
           STATIC
 
       = (VSnraw(2)    = CHA4                : 'VSN of the cartridge for RAW data',
          TrigType(2)  = CHA4                : 'Trigger type for this run',
          LepFill      = INTE [1,10000]      : 'LEP fill number for this run',
          RunNumb      = INTE [1,*]          : 'Run Number ',
          MagCur       = INTE [0,*]          : 'Current in the Aleph Magnet , in units of 0.1 Amperes',
          WritDate     = INTE [0,100000]     : 'Date of start of run yymmdd',
          WritHour     = INTE [0,100000]     : 'Hour of start of run : hhmmss',
          EVnumb       = INTE [0,*]          : 'Number of events in this run , from Online',
          EvnumbF      = INTE [0,*]          : 'Number of events in this run , from FALCON 1',
          TotEn        = INTE [0,*]          : 'LEP Total energy in Mev ( verified value when LepFlag = 1 , \
                                                nominal otherwise) ',
          NomEn        = INTE [0,*]          : 'LEP nominal energy in Mev , as given at run time',
          lepFLag      = INTE [0,1]          : ' = 1 if the TotEn value is the final ,verified one',
          DetecCond    = INTE [0,*]          : 'Bitmap describing detector running conditions',
          NumZ0        = INTE [0,*]          : 'Number of Z0 - > Hadrons , from FALCON 1 processing\
                                                = -1 if not yet known',
          NumBh        = INTE [0,*]          : 'Number of Bhabhas\
                                                = -1 if not yet known',
          LenDat       = INTE [0,*]          : 'length of written Raw Data , in Kbytes',
          CrosSec      = INTE [*,*]          : '   Defines the LUMI and RUN_QUAL flags , as follows :\
                                                     word = LUMI+10*IRUN_QUAL
                                                     LUMI = -1 : not yet known ;  = 0 : provisional  : = 1 : definitive\
                                                     IRUN_QUAL= -1 : UNKN ;  = 0 : PERF  ; =1 : MAYB  = 2 : DUCK',
          BitpatRunsel = INTE [*,*]          : 'Bit pattern for Run Selection by Physics Groups\
                                                  Bit 0  Run DUCK\
                                                  Bit 1  Run MAYB\
                                                  Bit 2  Run PERF\
                                                  Bit 3  Run selected by QQBar Physics Goup\
                                                  Bit 4  Run selected by Heavy Flavour , ECAL Physics Group\
                                                  Bit 5  Run selected by Heavy Flavour , HCAL Physics Group\
                                                  Bit 6  Run selected by Heavy Flavour , MUON Physics Group\
                                                  Bit 7  Run selected by Heavy Flavour , DEDX Physics Group\
                                                  Bit 8  Run selected by Searches , Photon Physics Group\
                                                  Bit 9  Run selected by Searches , Muon Physics Group\
                                                  Bit 10 Run selected by Searches , Standard Physics Group\
                                                  Bit 11 Run selected by Searches , Multihadronic Physics Group\
                                                  Bit 12 Run selected by VDET\
                                                  Bit 13 Run selected by Electroweak Asymmetries Group\
                                                  Bit 14 Run selected by Energy Flow Tool\
                                                  Bit 15 Run selected by dE/dx Tool\
                                                  Bit 16 Run selected by Electron Identification Tool\
                                                  Bit 17 Run selected by Muon Identification Tool\
                                                  Bit 18 Run selected by Lumi Cross Section Tool\
                                                  Bit 19 Run selected by Lumi as Veto Tool\
                                                  Bit 20 Run selected by Photon Identification Tool\
                                                  Bit 21 Run selected by ITC Tool',
         LUmin        = REAL [-1.000,*]     : 'Present best estimate of luminosity , in nb**-1\
                                                = -1. if not yet determined ,  = 0. if run discarded',
          BacKground   = REAL [0.,*]         : 'Number of Background Bhabha events in Luminosity Sample',
          BhabTrig     = REAL [0.,1.0000]    : 'Bhabha Trigger Efficiency',
          VaultLoc     = REAL [-1.000,*]     : 'For 1989->1994 : Vault or TMS location of AA tapes, position in TMSLIB\
                                                For 1995->1999 : Luminosity of run with XVDEOK (VDET OK lumi),\
                                                For Year 2000  : Luminosity with miniramp events included')
 
      ;
 
 
 BSLU
      :      'RUN luminosity from SICAL. The table is filled by SICAL specialist one row per run.
              NR=Period Number\
              Number of words/run\
              Number of runs'
 
       STATIC
      = (RunNumber    = INTE [0,*]         : 'Run number',
         LumQuality   = INTE [*,*]         : 'Flag for Sical Lumi status by Sical expert\
                                                   =  0  unknown\
                                                   =  1  temporary\
                                                   =  2  Final ',
         NumbBhab     = INTE [0,*]         : 'Number of Bhabha Events used in the Luminosity Calculation ,\
                                              i.e. with good HV/trigger conditions including SICAL',
         LUmi         = REAL [0.,1000.00]  : 'Best estimate of Luminosity in nb**-1',
         BacKground   = REAL [0.,*]        : 'Number of Background Bhabha events in Luminosity Sample',
         ElecWeak     = REAL [0.,5.0000]   : 'Electroweak correction to Xsection')
 
      ;
 
 
 
 BS4B
      :      'RUN Luminosity from SICAL for 4-bunch runs, filled by SICAL specialist one row per run.
              NR=Period Number\
              Number of words/run\
              Number of runs'
 
       STATIC
      = (RunNumber    = INTE [0,*]         : 'Run number',
         LumQuality   = INTE [*,*]         : 'Flag for Sical Lumi p[er bunch status by Sical expert\
                                                   =  0  unknown\
                                                   =  1  temporary\
                                                   =  2  Final ',
         numbBhab1    = INTE [0,*]         : 'Number of Bhabha Events used in Luminosity Calculation , bunch 1',
         Lumi1        = REAL [0.,1000.00]  : 'Best estimate of Luminosity in nb**-1, bunch 1',
         bacKground1  = REAL [0.,*]        : 'Number of Background Bhabhas , bunch 1',
         numbBhab2    = INTE [0,*]         : 'Number of Bhabha Events used in Luminosity Calculation , bunch 2',
         Lumi2        = REAL [0.,1000.00]  : 'Best estimate of Luminosity in nb**-1, bunch 2',
         bacKground2  = REAL [0.,*]        : 'Number of Background Bhabhas , bunch 2',
         numbBhab3    = INTE [0,*]         : 'Number of Bhabha Events used in Luminosity Calculation , bunch 3',
         Lumi3        = REAL [0.,1000.00]  : 'Best estimate of Luminosity in nb**-1, bunch 3',
         bacKground3  = REAL [0.,*]        : 'Number of Background Bhabhas , bunch 3',
         numbBhab4    = INTE [0,*]         : 'Number of Bhabha Events used in Luminosity Calculation , bunch 4',
         Lumi4        = REAL [0.,1000.00]  : 'Best estimate of Luminosity in nb**-1, bunch 4',
         bacKground4  = REAL [0.,*]        : 'Number of Background Bhabhas , bunch 4')
 
      ;
 
 BTPN
       :      'content of TAPe or Cartridge written for production(NR=Period Number)\
               Number of words per tape/cartridge\
               Number of written tape/cartridges in PERIOD'
           STATIC
 
       = (VSn(2)       = CHA4           : 'Visual Serial Number',
          FOrmat       = CHA4           : 'Format : EPIO or NATV',
          RecNumb      = INTE [0,*]     : 'Number of records')
 
       ;
 
 BXLI
       :      'List of tapes corresponding to a given Exabyte original\
               Number of words per tape\
               Number of tapes'
           STATIC
 
       = (TApecart(2)  = CHA4            : 'Tape or cartridge name for this dataset ')
 
        ;
 
 
 
 BXYZ
       :      'Parameters for Beam Position , run by run
               NR = Period Number\
               Number of words per run\
               Number of runs  '
           STATIC
 
       = (RunNumer     = INTE [0,*]          : 'Run Number',
          Beamword0    = REAL [0.,*]         : 'Copy of word A0 of JSUM bank for this run',
          Beamword1    = REAL [0.,*]         : 'Copy of word A1 of JSUM bank for this run',
          Beamword2    = REAL [0.,*]         : 'Copy of word A2 of JSUM bank for this run',
          Beamword3    = REAL [0.,*]         : 'Copy of word A3 of JSUM bank for this run',
          Beamword4    = REAL [0.,*]         : 'Copy of word A4 of JSUM bank for this run',
          Beamword5    = REAL [0.,*]         : 'Copy of word A5 of JSUM bank for this run',
          Beamword6    = REAL [0.,*]         : 'Copy of word A6 of JSUM bank for this run',
          Beamword7    = REAL [0.,*]         : 'Copy of word A7 of JSUM bank for this run',
          Beamword8    = REAL [0.,*]         : 'Copy of word A8 of JSUM bank for this run',
          Beamword9    = REAL [0.,*]         : 'Copy of word A9 of JSUM bank for this run',
          Veamword0    = REAL [0.,*]         : 'Copy of word B0 of JSUM bank for this run',
          Veamword1    = REAL [0.,*]         : 'Copy of word B1 of JSUM bank for this run',
          Veamword2    = REAL [0.,*]         : 'Copy of word B2 of JSUM bank for this run',
          Veamword3    = REAL [0.,*]         : 'Copy of word B3 of JSUM bank for this run',
          Veamword4    = REAL [0.,*]         : 'Copy of word B4 of JSUM bank for this run',
          Veamword5    = REAL [0.,*]         : 'Copy of word B5 of JSUM bank for this run',
          Veamword6    = REAL [0.,*]         : 'Copy of word B6 of JSUM bank for this run',
          Veamword7    = REAL [0.,*]         : 'Copy of word B7 of JSUM bank for this run',
          Veamword8    = REAL [0.,*]         : 'Copy of word B8 of JSUM bank for this run',
          Veamword9    = REAL [0.,*]         : 'Copy of word B9 of JSUM bank for this run',
          PositionZ    = REAL [-10.,10.000]  : 'Mean beam position in Z direction',
          eRrorZ       = REAL [0.,10.000]    : 'Error on PositionZ')
       ;
 
 BYEA
       :      'Description of RUN ranges for each YEAR of data\
               Number of words per year \
               Number of years'
           STATIC
 
      = (YeaR             = INTE [0,*]    : 'Year',
         FirstRun         = INTE [0,*]    : 'First Run Number for GOOD data in the current year',
         LfilRlum         = INTE [0,*]    : 'first run number for LFIL/RLUM banks for this year')
 
       ;
    END ESET
 
 END SUBSCHEMA
 
 
