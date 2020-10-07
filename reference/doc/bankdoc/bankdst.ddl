 SUBSCHEMA MiniDSTBanks
 
 AUTHOR   'S. Haywood,J.Boucrot,A.Bonissent'
 REVIEWER 'J.Boucrot'
 VERSION  '12.2'
 DATE     '12/01/01'
 
 DEFINE ESET
/*---------------------------------------------------------------------*/
 
 
    DDLT
         :    'Reconstructed Tagged Lepton from QSELEP on the MINI\
               Number of words per lepton \
               Number of leptons'
         SIZE 1,1
         STATIC
 
         = (PArticletype    = INTE [2,23] : ' Particle type :   \
                                              2 = e+                \
                                             12 = e+ in crack region\
                                             22 = e+ in overlap region\
                                              3 = e-                \
                                             13 = e- in crack region\
                                             23 = e- in overlap region\
                                              5 = mu+ QMUIDO IDF=14 \
                                             15 = mu+ QMUIDO IDF=13 \
                                              6 = mu- QMUIDO IDF=14 \
                                             16 = mu- QMUIDO IDF=13 ',
            JeTnumber       = INTE [1,*] : ' Nearest jet number in \
                                             jet section',
            PtInclusif      = INTE [0,*] : '10**5 Transverse momentum \
                                             respect to jet including \
                                             lepton ',
            PtExclusif      = INTE [0,*] : '10**5 Transverse momentum \
                                              respect to jet lepton \
                                              excluded ',
            ValidityPt      = INTE [0,30] : ' Flag giving validity of\
                                              Pt calculation \
                                              0 =  OK    \
                                              10=  No track E-flow track\
                                                   found  \
                                              20=  No valid jet \
                                              30=  Not enough object in \
                                                   jet ')
 
         ;
    DMLT
         :    'History of tagged  Lepton from QTRUTH for MCarlo events \
               Number of words per lepton \
               Number of leptons'
         SIZE 1,1
         STATIC
 
         = (FLavorofquark   = INTE [0,5] : ' Primary quark flavour \
                                              from QTRUTH           \
                                             1 - 5 Primary quark flavour',
            POppingflag     = INTE [0,5] : ' IPOP from QTRUTH      \
                                             0 = Direct production \
                                             >0= flavor of the popped qq',
            flagCHain       = INTE [1,4] : ' ICHAIN from QTRUTH ',
            flagSPlit       = INTE [1,10] : 'ISPLIT from QTRUTH ',
            flagLEpton      = INTE [0,4]  : 'LEPID from QTRUTH ',
            flagMEss        = INTE [-2,3] : 'IBMES from QTRUTH',
            KineTrack       = INTE [1,*]  : 'FKIN track number')
 
 
        ;
 
    DLJT
         :    'Jet for Heavy Flavor lepton from QSELEP\
               Number of words per jet \
               Number of jets'
         SIZE 1,1
         STATIC
 
         = (PX              = INTE [*,*] : ' 10**5 X component of jet momentum',
            PY              = INTE [*,*] : ' 10**5 Y component of jet momentum',
            PZ              = INTE [*,*] : ' 10**5 Z component of jet momentum',
            PE              = INTE [*,*] : ' 10**5 Jet enenergy ',
            NO              = INTE [1,*] : ' Number of object in jet ')
 
        ;
 
 
 DGAC : 'Photon Identification Information - Derived from PGAC.
         See DDL of PGAC for more complete description.\
         Number of Words per Photon\
         Number of Photons'
         STATIC
 
       = (ECorr      = INTE [0,*]            : 'Corrected Energy (photon hypothesis) (MeV)',
          ThetaCorr  = INTE [0,*]            : 'Corrected Theta (photon hypothesis) (mrad/10)',
          PhiCorr    = INTE [0,*]            : 'Corrected Phi (photon hypothesis) (mrad/10)',
          R1         = INTE [0,1000]         : 'Energy Fraction in Stack 1 (per mille)',
          R2         = INTE [0,1000]         : 'Energy Fraction in Stack 2 (per mille)',
          F4         = INTE [0,1000]         : 'Energy Fraction in 4 Central Towers (per mille)',
          DistMin    = INTE [0,*]            : 'Distance to closest track (mm)',
          SToreys    = INTE [0,*]            : 'Storey Information',
          QUality    = INTE [0,*]            : 'Quality Flag',
          Q1         = INTE [*,*]            : '1st Quality Estimator * 100',
          Q2         = INTE [*,*]            : '2nd Quality Estimator * 100',
          M1         = INTE [0,*]            : '1st Moment from CLMONS * 100',
          M2         = INTE [0,*]            : '2nd Moment from CLMONS * 100',
          MA         = INTE [0,*]            : 'Pi0 Mass from CLMONS (MeV)',
          ERaw       = INTE [0,*]            : 'Raw Energy (MeV)',
          ThetaRaw   = INTE [0,*]            : 'Raw Theta (mrad/10)',
          PhiRaw     = INTE [0,*]            : 'Raw Phi (mrad/10)',
    ExpectedFraction = INTE [0,*]            : '10**4 Expected fraction in 4 towers',
    GeometricalCorr  = INTE [0,*]            : '10**4 Geomtrical correction ',
    ZeroSupression   = INTE [0,*]            : '10**4 Zero suppresion correction from Coradoc',
    ProbafakeeLm     = INTE [0,*]            : '10**4 Probability to be a fake photon
                                                from Electromagnetic origine ',
    ProbaFakehad     = INTE [0,*]            : '10**4 Probability to be a fake photon
                                                from Hadronic origine ',
    ParentNumber     = INTE [0,*]            : 'Row number(PGAC) of parent giving a fake photon',
    FAkequality      = INTE [0,*]            : 'Flag for fake determination')
 
       ;
 
 DTHR : 'Thrust value for Mini-DST , integerised .
         - Obtained from charged tracks AND Energy Flow \
         Number of Words\
         Number of Headers(=1)'
         STATIC
         SIZE 1,1
 
       = (THrust     = INTE [*,*]           : '10**5 Thrust value',
          PX         = INTE [*,*]           : '10**5 Projection  of thrust , X axis',
          PY         = INTE [*,*]           : '10**5 Projection  of thrust , Y axis',
          PZ         = INTE [*,*]           : '10**5 Projection  of thrust , Z axis',
          PE         = INTE [*,*]           : '10**5 Projection  of thrust , Energy')
       ;
 

 DE14 : 'E14 results for Mini-DST , integerised \
         Number of Words\
         Number of Headers(=1)'
         STATIC
         SIZE 1,1

       = (ECalener   = INTE [*,*]           : '10**6 Energy from Ecal/Lcal/Sical in GeV in 14 degree cone',
          HCalener   = INTE [*,*]           : '10**6 Energy from Hcal in GeV in 14 degree cone',
          LVeto      = INTE [*,*]           : 'Maximum pulse height in LCAL veto counters')
       ;
     
 DYV0 : 'V0 track numbers for Mini-DST\
         Number of Words\
         Number of V0s'
         STATIC
         SIZE 1,1

       = (K1         = INTE [*,*]           : 'Number of the positive track coming from V0 in the DTRA bank',
          K2         = INTE [*,*]           : 'Number of the negative track coming from V0 in the DTRA bank')
       ;

  
 DBTG : 'QIPBTAG Results for  Mini-DST , integerised \
         Number of Words\
         Number of Tracks'
         STATIC
         SIZE 1,1
 
       = (TrackFlag  = INTE [*,*]           : 'Track Flag from QIPBTAG :\
                                               Bits  1 to  8   :  FRFT Track Number\
                                               Bits  9 to 22   : QIPBTAG Flag\
                                               Bits 23 to 26   : Jet Numbers Assignment\
                                               Bits 27 to 28   : Hemisphere  Assignment',
          TrkSignif  = INTE [*,*]           : '10**6 Impact Parameter Significance oof this track')
 
       ;
 
 
 DMSC : 'Mult Scatt. angle for Mini-DST , integerised ,
         bank parallel to DTRA , defined for CLAS 24 events only\
         Number of Words\
         Number of Tracks'
         STATIC
         SIZE 1,1
 
       = (ScattAng   = INTE [*,*]           : 'Multiple Scattering Angle between TPC and ITC (mrad/10)')
 
       ;
 
 D4CD : 'Flags for Muon chambers 4c and 4d , 1993 MCarlo Events\
         Number of Words\
         Number of Flags\'
         STATIC
         SIZE 1,1
 
       = (ISet      = INTE [-1,1]          : '= 1 if good MC event \,
                                              = -1 if hits deleted in muon chambers 4c/4d')
 
    ;
 
 DSIC : 'Sical clusters for Mini-DST\
         Number of Words\
         Number of Clusters'
         STATIC
         SIZE 1,1
 
       = (ISide      = INTE [0,2]           : 'Fiducial side',
          EClust     = INTE [0,*]           : 'Energy of most energetic Cluster on that side (MeV)',
          CorrR      = INTE [0,*]           : 'Corrected Radius of Cluster (micron)',
          CorrPhi    = INTE [0,*]           : 'Corrected Phi of Cluster (mrad/10)',
          ZClus      = INTE [*,*]           : 'Z of Cluster Barycentre (micron)',
          XposI      = INTE [*,*]           : 'X position extrapolated at Z0 (micron)',
          YposI      = INTE [*,*]           : 'Y position extrapolated at Z0 (micron)',
          NclPad     = INTE [0,1000]        : 'Number of Pads per Cluster',
          NDead      = INTE [0,1000]        : 'Number of Dead Pads used per Cluster')
       ;
 
 
 DEVT : 'Event Header for Mini-DST\
         Number of Words\
         Number of Headers(=1)'
         STATIC
         SIZE 1,1
 
       = (LcalErr    = INTE [0,*]           : 'Lcal Error Flag from LOLE bank',
          WireTime   = INTE [-2000,+2000]   : 'Time from Ecal Wires (ns)',
          NumXing    = INTE [0,*]           : 'Number of Beam Crossings since last Trigger',
          NumVdet    = INTE [0,*]           : 'Number of Vdet Hits',
          WirebaRrel = INTE [0,*]           : 'Summed Wire (PEWI) Energy in Ecal Barrel (MeV)',
          WireendA   = INTE [0,*]           : 'Summed Wire (PEWI) Energy in Ecal Endcap A (MeV)',
          WireendB   = INTE [0,*]           : 'Summed Wire (PEWI) Energy in Ecal Endcap B (MeV)',
          PadbaRrel  = INTE [0,*]           : 'Summed Pad (PECO) Energy in Ecal Barrel (MeV)',
          PadendA    = INTE [0,*]           : 'Summed Pad (PECO) Energy in Ecal Endcap A (MeV)',
          PadendB    = INTE [0,*]           : 'Summed Pad (PECO) Energy in Ecal Endcap B (MeV)',
          LumiendA   = INTE [0,*]           : 'Summed Pad (PECO) Energy in Lumi Endcap A (MeV)',
          LumiendB   = INTE [0,*]           : 'Summed Pad (PECO) Energy in Lumi Endcap B (MeV)',
          HcalbaRrel = INTE [0,*]           : 'Summed Pad (PHCO) Energy in Hcal Barrel (MeV)',
          HcalendA   = INTE [0,*]           : 'Summed Pad (PHCO) Energy in Hcal Endcap A (MeV)',
          HcalendB   = INTE [0,*]           : 'Summed Pad (PHCO) Energy in Hcal Endcap B (MeV)')
       ;
 
 DVRS : 'Program Versions for Mini-DST\
         Number of Words\
         Number of Headers(=1)'
         STATIC
         SIZE 1,1
 
       = (MiniVers   = INTE [0,1000]        : 'Mini-DST version number',
          V0num      = INTE [0,100]         : 'V0 (YV0V) bank number')
       ;
 
 DHEA : 'Header containing General Event Information from Reconstruction.
         Quantities from NJets to Eigen3
         are obtained from the Energy Flow method.
         NJets=-10 indicates that the Energy Flow has failed.\
         Number of Words\
         Number of Headers(=1)'
         STATIC
         SIZE 1,1
 
       = (FlagPhys   = INTE [0,*]           : 'Flag for Physics Identification\
                                               Not yet filled',
          NverteX    = INTE [0,255]         : 'Num of Recons Vertices',
          NPostra    = INTE [0,255]         : 'Num of Positive Recons Tracks\
                                               Count Good Tracks: UFITQL = 1 or 3',
          NMintra    = INTE [0,255]         : 'Num of Negative Recons Tracks\
                                               Count Good Tracks: UFITQL = 1 or 3',
          NVzer      = INTE [0,255]         : 'Num of Recons V0s',
          NJets      = INTE [0,15]          : 'Num of Recons Jets (EJET)',
          ECtrack    = REAL [0.,200.00]     : 'Total Energy of Charged Tracks',
          EcaLo      = REAL [0.,200.00]     : 'Total Energy of Calobjects',
          PFlow      = REAL [0.,200.00]     : 'Pabs of Energy Flow',
          THflow     = REAL [0.,3.1416]     : 'Theta of Energy Flow',
          PHflow     = REAL [0.,6.2832]     : 'Phi of Energy Flow',
          EFlow      = REAL [0.,200.00]     : 'Energy Flow',
          ETabs      = REAL [0.,200.00]     : 'Sum of Abs Values of Et',
          Thtens1    = REAL [0.,3.1416]     : 'Theta of Thrust Axis',
          Phtens1    = REAL [0.,6.2832]     : 'Phi of Thrust Axis',
          Thtens2    = REAL [0.,3.1416]     : 'Theta of Major Axis',
          Phtens2    = REAL [0.,6.2832]     : 'Phi of Major Axis',
          Eigen1     = REAL [0.,1.0000]     : 'Eigenvalue for Thrust Axis, ie Thrust',
          Eigen2     = REAL [0.,1.0000]     : 'Eigenvalue for Major Axis',
          Eigen3     = REAL [0.,1.0000]     : 'Eigenvalue for Minor Axis, ie Oblateness',
          RecStat    = INTE [0,*]           : 'Flag for Reconstruction Status\
                                               Contains LOLE error flag',
          WireTime   = INTE [-2000,+2000]   : 'Time from Ecal Wires (ns)')
       ;
 
 DMJT : 'Reconstructed Jet
         - Obtained from the Energy Flow Method, using Tracks and Calobjects
         and the LUCLUS Algorithm\
         Number of Words per Jet\
         Number of Jets'
         STATIC
 
       = (NPostra    = INTE [0,255]          : 'Num of Positive Recons Tracks',
          NMintra    = INTE [0,255]          : 'Num of Negative Recons Tracks',
          P          = REAL [0.,200.00]      : 'Pabs',
          THeta      = REAL [0.,3.1416]      : 'Theta',
          PHi        = REAL [0.,6.2832]      : 'Phi',
          E          = REAL [0.,200.00]      : 'Energy')
       ;
 
 DTBP : 'Trigger Bit Pattern - Derived from XTRB or XTEB\
         Number of Words per Row\
         Number of Rows'
         STATIC
 
       = (Trig1      = INTE [*,*]           : 'Level 1 Bit Pattern - see XTEB',
          Trig2      = INTE [*,*]           : 'Level 2 Bit Pattern - see XTEB',
          Level2     = INTE [*,*]           : 'Level 2 Bit Pattern after mask - see XTEB')
       ;
 
 DVER : 'Fitted Vertex - Derived from PYER\
         Number of Words per Vertex\
         Number of Vertices'
         STATIC
 
       = (X          = INTE [*,*]           : 'X of Fitted Vertex (micron)',
          Y          = INTE [*,*]           : 'Y of Fitted Vertex (micron)',
          Z          = INTE [*,*]           : 'Z of Fitted Vertex (micron)',
          FitProb    = INTE [0,1000]        : 'Fit Probability (per mille)',
          MainVert   = INTE [0,1]           : '1 for Main Vertex, 0 otherwise')
       ;
 
 DTRA : 'Fitted Track - Derived from FRFT (and PFRF)\
         Number of Words per Track\
         Number of Tracks'
         STATIC
 
       = (CHarge     = INTE [-1,+1]         : 'Charge',
          P          = INTE [0,*]           : 'Momentum of Fitted Track (MeV)',
          THeta      = INTE [0,*]           : 'Theta of Fitted Track (mrad/10)',
          PHi        = INTE [0,*]           : 'Phi of Fitted Track (mrad/10)',
          D0         = INTE [*,*]           : 'Distance of Closest Approach to z-Axis (micron)',
          Z0         = INTE [*,*]           : 'Z Coordinate at D0 (micron)',
          ERrors(5)  = INTE [0,*]           : 'Eigen-values as in PFRF',
          TFitprob   = INTE [0,1000]        : 'Track Fit: Chisq/DoF * 10',
          HitsObs    = INTE [0,*]           : 'Hit Pattern Observed in Tracking Devices.\
                                               Bits are set if there is a Hit Associated to Track.\
                                               Bits 01 - 02 for Vdet,\
                                               Bits 03 - 10 for ITC,\
                                               Bits 11 - 31 for TPC.',
          HitsMiss   = INTE [0,*]           : 'Hit Pattern for Missing Hits - Corresponds to
                                               Observed Pattern.\
                                               If a Hit is expected but Missing, Bit is Set',
          VBitmap    = INTE [0,*]           : 'Vertex Bit Map\
                                               Bit i is Set if Track Associated to
                                               Vertex i; i=1,30',
          QualFlag   = INTE [0,*]           : 'Quality Flag from UFITQL',
          EAngles(10)= INTE [0,*]           : 'Error Matrix. New Scheme: Euler-angles as in PFRF',
          VdetInfo   = INTE [0,*]           : 'Vdet layer information \
                                               Bits  1 -  8  for VDHITS\
                                               Bits  9 - 10 for VDAMB\
                                               Bits 11 - 19 for VTRUH')
       ;
 
 DEID : 'Electron Identification Information - Derived from EIDT - Exists for Tracks with p > 1 GeV\
         Number of Words per Row\
         Number of Rows'
         STATIC
 
       = (R2         = INTE [-1023,1024]     : 'R2 * 100',
          R3         = INTE [-1023,1024]     : 'R3 * 100',
          QFlag      = INTE [0,255]          : 'Quality Flag - see EIDT')
       ;
 
 DNEU : 'Neutral Calorimeter Particle - Derived from PCPA\
         Number of Words per Particle\
         Number of Particles'
         STATIC
 
       = (NAture     = INTE [0,255]          : 'Identical to Nature in PCPA',
          E          = INTE [*,*]            : 'Energy',
          THeta      = INTE [0,*]            : 'Theta (mrad/10)',
          PHi        = INTE [0,*]            : 'Phi (mrad/10)',
          R1         = INTE [*,*]            : 'R1 * 10',
          R2         = INTE [*,*]            : 'R2 * 10',
          PCob       = INTE [0,*]            : 'Pointer to PCOB')
       ;
 
 DRES : 'Residual Calorimeter Particle - Derived from PCPA\
         Number of Words per Particle\
         Number of Particles'
         STATIC
 
       = (NAture     = INTE [0,255]          : 'Identical to Nature in PCPA',
          E          = INTE [*,*]            : 'Energy of Residual - may be < 0 (MeV)',
          THeta      = INTE [0,*]            : 'Theta (mrad/10)',
          PHi        = INTE [0,*]            : 'Phi (mrad/10)',
          P          = INTE [0,*]            : 'Momentum of Residual (Mev)',
          PSum       = INTE [0,*]            : 'Sum of Momenta of Tracks Associated to Calobject (MeV)',
          PCob       = INTE [0,*]            : 'Pointer to PCOB')
       ;
 
 DCRL : 'Calorimter Relations - Derived from PCRL\
         Number of Words per Relation\
         Number of Relations'
         STATIC
 
       ;
 
 DECO : 'Ecal Calorimeter Cluster - Derived from PECO/1\
         Number of Words per Cluster\
         Number of Clusters'
         STATIC
 
       = (E          = INTE [0,*]            : 'Energy - corrected (MeV)',
          THeta      = INTE [0,*]            : 'Theta (mrad/10)',
          PHi        = INTE [0,*]            : 'Phi (mrad/10)',
          EFract(2)  = INTE [0,*]            : 'Energy Fraction in Stacks 1,2 (per mille)',
          CorrCode   = INTE [0,255]          : 'Correction Code from PECO')
       ;
 
 DEWI : 'Ecal Wire Energy in Module - Derived from PEWI\
         Number of Words per Row\
         Number of Rows'
         STATIC
 
       = (ModuleNum  = INTE [1,36]           : 'Module Number',
          E          = INTE [0,*]            : 'Energy in Module (MeV)\
                                                Note: there is a threshold for PEWI',
          EFract(2)  = INTE [0,*]            : 'Energy Fraction in Stacks 1,2 (per mille)')
       ;
 
 DHCO : 'Hcal Calorimeter Cluster - Derived from PHCO\
         Number of Words per Cluster\
         Number of Clusters'
         STATIC
 
       = (E          = INTE [0,*]            : 'Energy - corrected (MeV)',
          THeta      = INTE [0,*]            : 'Theta (mrad/10)',
          PHi        = INTE [0,*]            : 'Phi (mrad/10)',
          BitsNoise  = INTE [0,*]            : 'Bits 0 to 5 :\
                                                 Noise Flag from Topological Analysis\
                                                 and Digital Pattern matching\
                                                 bits 0-4 Top. An. (0=OK,1=Noise)\
                                                 bit 5 Dig. Matching (0=OK,1=Noise)\
                                                 Bits 10 and 11: Relation bits\
                                                 see word RB of PHCO bank')
 
 
 
       ;
 
 DHRL : 'Hcal Relations - Derived from PCRL\
         Number of Words per Relation\
         Number of Relations'
         STATIC
 
       ;
 
 DPOB : 'Hcal Digital Pattern Object - Derived from PPOB\
         Number of Words per Object\
         Number of Objects'
         STATIC
 
       = (E          = INTE [0,*]            : 'Energy (MeV)')
       ;
 
 DTMC : 'Tracks from MC Truth - Derived from FKIN\
         Number of Words per Object\
         Number of Objects'
         STATIC
 
       = (PX         = INTE [0,*]            : 'Px (MeV)',
          PY         = INTE [0,*]            : 'Py (MeV)',
          PZ         = INTE [0,*]            : 'Pz (MeV)',
          MAss       = INTE [0,*]            : 'Mass (MeV)',
          PArt       = INTE [0,*]            : 'ALEPH Particle Code',
          OVrt       = INTE [0,*]            : '(FVER) Origin Vertex No.',
          EVrt       = INTE [0,*]            : '(FVER) End Vertex No.',
          HCode      = INTE [0,*]            : 'History Code')
       ;
 
 DVMC : 'Vertices from MC Truth - Derived from FVER\
         Number of Words per Object\
         Number of Objects'
         STATIC
 
       = (VX         = INTE [0,*]            : 'Vertex X Position (micron)',
          VY         = INTE [0,*]            : 'Vertex Y Position (micron)',
          VZ         = INTE [0,*]            : 'Vertex Z Position (micron)',
          IPrim      = INTE [0,*]            : 'FKIN Track No. which Produced this Vertex',
          VolNam     = CHA4                  : 'Vertex Volume Number',
          VertMec    = INTE [0,*]            : 'Code for Vertex Mechanism Name')
       ;
 
 DFMC : 'Fragmentation from MC Truth - Derived from FZFR\
         Number of Words per Object\
         Number of Objects'
         STATIC
 
       = (ZF         = INTE [0,1000]         : 'Z Fragmentation Value * 1000')
       ;
 
 DMUO : 'Muon Identification Information - Derived from AMUIDO - Exists for Tracks with p > 1 GeV\
         Number of Words per Row\
         Number of Rows'
         STATIC
 
       = (HitsObs    = INTE [0,*]            : 'Hit Pattern Observed:\
                                                Bits 01 - 23 Digital Pattern in Hcal\
                                                Bits 24 - 26 Number of Hits in Inner Mu Chambers\
                                                Bits 27 - 29 Number of Hits in Outer Mu Chambers',
          HitsExpec  = INTE [0,*]            : 'Hit Pattern Expected:\
                                                Bits 01 - 23 Digital Pattern in Hcal',
          SHadowinfo = INTE [0,*]            : 'Shadow Information:\
                                                Bits 01 - 09 contain Average Multiplicity of Hcal Hits
                                                * 100 upto 5.11\
                                                Bits 10 - 10 set if Muon Track is Shadowed\
                                                Bits 11 - 11 set if this Track (DTRA) has Best Match')
       ;
 
 DGAM : 'Photon Identification Information - Derived from EGID - Exists for Clusters with E > 0.5 GeV.
         If Cluster is Broken down to Two Neutrals, eg pi0, there is an Entry for Each Neutral\
         Number of Words per Neutral\
         Number of Neutrals'
         STATIC
 
       = (NAture     = INTE [0,*]            : 'Nature * 16 + Flag - see EGID',
          Estim1     = INTE [-127,128]       : 'Materialisation Depth * 10',
          Estim2     = INTE [-127,128]       : 'Longitudinal Shape * 10',
          Estim3     = INTE [0,1000]         : 'Compactness (per mille)',
          Estim5     = INTE [0,*]            : '1st Moment of Cluster (mm)',
          Estim6     = INTE [0,*]            : '2nd Moment of Cluster (mm)',
          Estim8     = INTE [0,*]            : 'Cluster Mass (MeV)',
          E          = INTE [0,*]            : 'Energy of Neutral (MeV)',
          THeta      = INTE [0,*]            : 'Theta of Neutral (mrad/10)',
          PHi        = INTE [0,*]            : 'Phi of Neutral (mrad/10)')
       ;
 
 DFOT : 'Photon Identification Information - Derived from EGPC - Exists for Clusters with E > 0.25 GeV.\
         Number of Words per Photon\
         Number of Photons'
         STATIC
 
       = (PX         = INTE [0,*]            : 'Px (MeV)',
          PY         = INTE [0,*]            : 'Py (MeV)',
          PZ         = INTE [0,*]            : 'Pz (MeV)',
          QualFlag   = INTE [0,3]            : 'Quality Flag:\
                                                Bits 01 - 01 set if Photon in Crack Region\
                                                Bits 02 - 02 set if any Dead Storeys in any of 3 Stacks')
       ;
 
 DGID : 'Photon Identification Information - Derived from PGID.
         See DDL of PGID for more complete description.\
         Number of Words per Neutral\
         Number of Neutrals'
         STATIC
 
       = (IFlag      = INTE [0,100]          : 'Quality Flag',
          DepthEst   = INTE [-511,+512]      : 'Materialisation Depth (normalised) * 100',
          CoMpest    = INTE [-511,+512]      : 'Compactness (normalised) * 100',
          Moment1    = INTE [-511,+512]      : '1st Moment (normalised) * 100',
          Moment2    = INTE [-511,+512]      : '2nd Moment (normalised) * 100',
          Moment3    = INTE [0,*]            : '3rd Moment (MeV)',
          CorrE      = INTE [0,*]            : 'Corrected Energy including Saturaration (MeV)',
          THeta      = INTE [0,*]            : 'Theta including S-shape (mrad/10)',
          PHi        = INTE [0,*]            : 'Phi including S-shape (mrad/10)')
       ;
 
 DGPC : 'Photon Identification Information - Derived from PGPC.
         See DDL of PGPC for more complete description.\
         Number of Words per Photon\
         Number of Photons'
         STATIC
 
       = (ECorr      = INTE [0,*]            : 'Corrected Energy (photon hypothesis) (MeV)',
          ThetaCorr  = INTE [0,*]            : 'Corrected Theta (photon hypothesis) (mrad/10)',
          PhiCorr    = INTE [0,*]            : 'Corrected Phi (photon hypothesis) (mrad/10)',
          R1         = INTE [0,1000]         : 'Energy Fraction in Stack 1 (per mille)',
          R2         = INTE [0,1000]         : 'Energy Fraction in Stack 2 (per mille)',
          F4         = INTE [0,1000]         : 'Energy Fraction in 4 Central Towers (per mille)',
          DistMin    = INTE [0,*]            : 'Distance to closest track (mm)',
          SToreys    = INTE [0,*]            : 'Storey Information',
          QUality    = INTE [0,*]            : 'Quality Flag',
          Q1         = INTE [*,*]            : '1st Quality Estimator * 100',
          Q2         = INTE [*,*]            : '2nd Quality Estimator * 100',
          M1         = INTE [0,*]            : '1st Moment from CLMONS * 100',
          M2         = INTE [0,*]            : '2nd Moment from CLMONS * 100',
          MA         = INTE [0,*]            : 'Pi0 Mass from CLMONS (MeV)',
          ERaw       = INTE [0,*]            : 'Raw Energy (MeV)',
          ThetaRaw   = INTE [0,*]            : 'Raw Theta (mrad/10)',
          PhiRaw     = INTE [0,*]            : 'Raw Phi (mrad/10)')
       ;
 
 DENF : 'Energy Flow Objects\
         Number of Words per Object\
         Number of Objects'
         STATIC
 
       = (PX         = INTE [0,*]            : 'Px (MeV)',
          PY         = INTE [0,*]            : 'Py (MeV)',
          PZ         = INTE [0,*]            : 'Pz (MeV)',
          E          = INTE [0,*]            : 'Energy (old scheme) or Mass (new scheme) (MeV)',
          WEight     = INTE [0,*]            : 'Weight * 1000/
                                                This Quantity is internal to ENFLW and should not be
                                                used',
          TYpe       = INTE [0,*]            : 'Type of Object\
                                                0 = Track\
                                                1 = Electron\
                                                2 = Muon\
                                                3 = Track from V0\
                                                4 = EM cluster\
                                                5 = Ecalobject - not EM\
                                                6 = Hcalobject\
                                                7 = Lcalobject',
          PointCal   = INTE [0,*]            : 'Pointer to Calobject:\
                                                Positive => points to DECO\
                                                Negative => points to DHCO',
          DTRA       = INTE                  : 'Pointer to Associated Track')
       ;
 
 DJET : 'Jet Obtained from Energy Flow Objects
         using Jade Algorithm with a very low y-cut\
         Number of Words per Jet\
         Number of Jets'
         STATIC
 
       = (PX         = INTE [0,*]            : 'Px (MeV)',
          PY         = INTE [0,*]            : 'Py (MeV)',
          PZ         = INTE [0,*]            : 'Pz (MeV)',
          E          = INTE [0,*]            : 'Energy (MeV)')
       ;
 
 DKNK
      :      'MiniDST Kink List \
              Number of words/Kink\
              Number of Kinks'
           STATIC
 
      = (InnerTrack    = INTE [1,*] : 'FRFT No. of track producing the kink',
         OuterTrack    = INTE [0,*] : 'FRFT No. of track coming from kink',
         VerteX(3)     = INTE [*,*] : 'Fitted position of kink (microns)',
         PatKink       = INTE [*,*] : 'Momentum of parent at kink position\
                                       (MeV)',
         PTouter       = INTE [*,*] : 'Pt of outgoing charged particle\
                                       relative to parent (MeV)',
         PLouter       = INTE [*,*] : 'Pl of outgoing charged particle\
                                       relative to parent (MeV)',
         Chi2          = INTE [0,*] : 'Chi**2 of fit*10',
         BitHypoth     = INTE [0,*] : 'Bitmask of mass cuts passed by\
                                       this decay.  Identical to values\
                                       in YKNK')
 
      ;
 
 DWES
      :      'MiniDST Wires in ECAL modules\
              Number of words/modules\
              Number of modules'
           STATIC
 
      = (ModulNumber   = INTE [1,36] : 'Module Number (1-36)',
         SumonOddw     = INTE [0,*] : 'Sum of energy on odd wires (in KeV)',
         SumonEvenw    = INTE [0,*] : 'Sum of energy on even wires(in KeV)',
         EFract(2)     = INTE [0,*] : 'Energy Fraction in Stacks 1,2 (per mille)')
 
       ;
 
 PTHR : 'Thrust value from Mini-DST  .
         - Obtained from charged tracks AND Energy Flow \
         Number of Words\
         Number of Headers(=1)'
         STATIC
         SIZE 1,1
 
       = (THrust     = REAL [*,*]           : 'Thrust value',
          PX         = REAL [*,*]           : 'Projection  of thrust , X axis',
          PY         = REAL [*,*]           : 'Projection  of thrust , Y axis',
          PZ         = REAL [*,*]           : 'Projection  of thrust , Z axis',
          PE         = REAL [*,*]           : 'Projection  of thrust , Energy')
       ;
 
 
 NBIP : 'QIPBTAG Results from Mini-DST or Nano-Dst\
         Number of Words\
         Number of Tracks)'
         STATIC
         SIZE 1,1
 
       = (TrackFlag  = INTE [*,*]           : 'Track Flag from QIPBTAG :\
                                               Bits  1 to  8   :  FRFT Track Number\
                                               Bits  9 to 22   : QIPBTAG Flag\
                                               Bits 23 to 26   : Jet Numbers Assignment\
                                               Bits 27 to 28   : Hemisphere  Assignment',
          TrkSignif  = INTE [*,*]           : '10**6 Impact Parameter Significance oof this track')
 
       ;
 
 
 PMSC : 'Mult Scatt. angle for CLAS 24 candidates in Mini-DST ,
         bank parallel to FRFT\
         Number of Words\
         Number of Tracks)'
         STATIC
         SIZE 1,1
 
       = (ScattAng   = REAL [*,*]           : 'Multiple Scattering Angle between TPC and ITC ')
 
         ;
 
    PLSC
         :    'Lepton selection cut summary written on Mini-DST run header\
               Number of words in selection\
               Number of rows = (1)'
         SIZE 1,1
         STATIC
 
         = (gooDTrack      = INTE [0,999] : 'Number of Good tracks \
                                           to define a selected event',
            vISiblenergy   = REAL [0,1] : ' Minimum visible charged \
                                            energy to select an event',
            numberhIT      = INTE [0,21] : ' Minimum number of TPC hits\
                                            to select a track',
            maximumD0      = REAL [0,*]  : ' Maximum D0 to select \
                                             any track ',
            maximumZ0      = REAL [0,*]  : ' Maximum Z0 to select \
                                             any track',
            maximumCOs     = REAL [0,*]  : ' Maximum cosine of polar\
                                             angle to select any track',
            miNElectron    = INTE [0,20] : ' Minimum number of hits for\
                                             electron track',
            DzeroElectron  = REAL [0,*]  : ' Maximum D0 for electron \
                                             track',
            ZzeroElectron  = REAL [0,*]  : ' Maximum Z0 for electron \
                                             track',
            CosineElectron = REAL [0,*]  : ' Maximum cosine for electron\
                                             track',
            PElectron      = REAL [0,*]  : ' Minimum electron momentum',
            pTElectron     = REAL [0,*]  : ' Minimum electron transverse\
                                             momentum (excluded)',
            r2miNimum      = REAL [0,*]  : ' R2 minimum for electron \
                                             (transverse estimator)',
            r2maXimum      = REAL [0,*]  : ' R2 maximum for electron ',
            r3miNimum      = REAL [0,*]  : ' R3 minimum for electron \
                                             (longitudinal estimator)',
            r3maXimum      = REAL [0,*]  : ' R3 maximum for electron ',
            mIniMumpoint   = INTE [0,*]  : ' Minimum number of points \
                                             to define electron \
                                             ionisation ',
            r5miNimum      = REAL [0,*]  : ' R5 minimum for electron \
                                             (ionisation estimator)',
            r5maXimum      = REAL [0,*]  : ' R5 maximum for electron ',
            XYdistance     = REAL [0,*]  : ' Maximum distance in XY  \
                                             plane for conversions ',
            ZConversion    = REAL [0,*]  : ' Maximum distance in Z \
                                             for conversions ',
            MassInvariant  = REAL [0,*]  : ' Maximum invariant mass \
                                             of tracks in conversion',
            SelectionFlag  = INTE [0,1]  : ' Flag for conversions \
                                             selection : 0 removed \
                                                         1 kept ',
            miNimumMu      = INTE [0,20] : ' Minimum number of hits \
                                             to define a muon track',
            DzeroMuon      = REAL [0,*]  : ' Maximum D0 for muon     \
                                             track',
            ZzeroMuon      = REAL [0,*]  : ' Maximum Z0 for muon     \
                                             track',
            CosineMuon     = REAL [0,*]  : ' Maximum cosine for muon    \
                                             track',
            PMuon          = REAL [0,*]  : ' Minimum muon momentum',
            pTMuon         = REAL [0,*]  : ' Minimum muon transverse\
                                             momentum (excluded)',
            QM             = INTE [0,*]  : ' Number of QMUIDO return \
                                             allowed',
            Q1             = REAL [0,*]  : ' QMUIDO return flag 1 ',
            Q2             = REAL [0,*]  : ' QMUIDO return flag 2 ',
            NumberobJect   = INTE [0,*]  : ' Minimum number of objects \
                                             in a jet ',
            EnergyJet      = REAL [0,1]  : ' Maximal fraction of energy\
                                             carried by lepton in jet',
            JetNumber      = INTE [0,*]  : ' Minimum number of jets in\
                                             event',
            YcutJet        = REAL [0,*]  : ' Ycut used to build jets',
            KeepJet        = INTE [0,*]  : ' Flag to select leptons \
                                             associated to events not \
                                             satisfying jet cuts\
                                             0 = lepton removed \
                                             1 = leptons kept ')
         ;
 
    PDLT
         :    'Reconstructed Tagged Lepton from Mini-DST\
               Number of words per lepton \
               Number of leptons'
         SIZE 1,1
         STATIC
 
         = (PArticletype    = INTE [2,23] : ' Particle type :   \
                                              2 = e+                \
                                             12 = e+ in crack region\
                                             22 = e+ in overlap region\
                                              3 = e-                \
                                             13 = e- in crack region\
                                             23 = e- in overlap region\
                                              5 = mu+ QMUIDO IDF=14 \
                                             15 = mu+ QMUIDO IDF=13 \
                                              6 = mu- QMUIDO IDF=14 \
                                             16 = mu- QMUIDO IDF=13 ',
            JeTnumber       = INTE [1,*] : ' Nearest jet number in \
                                             jet section',
            PtInclusif      = REAL [0,*] : ' Transverse momentum \
                                             respect to jet including \
                                             lepton ',
            PtExclusif      = REAL [0.,*] : 'Transverse momentum \
                                              respect to jet lepton \
                                              excluded ',
            ValidityPt      = INTE [0,30] : ' Flag giving validity of\
                                              Pt calculation \
                                              0 =  OK    \
                                              10=  No track E-flow track\
                                                   found  \
                                              20=  No valid jet \
                                              30=  Not enough object in \
                                                   jet ')
 
         ;
 
    PMLT
         :    'History of tagged  Lepton for Mcarlo events from Mini-DST\
               Number of words per lepton \
               Number of leptons'
         SIZE 1,1
         STATIC
 
         = (FLavorofquark   = INTE [0,5] : ' Primary quark flavour \
                                              from QTRUTH           \
                                             1 - 5 Primary quark flavour',
            POppingflag     = INTE [0,5] : ' IPOP from QTRUTH      \
                                             0 = Direct production \
                                             >0= flavor of the popped qq',
            flagCHain       = INTE [1,4] : ' ICHAIN from QTRUTH',
            flagSPlit       = INTE [1,10] : 'ISPLIT from QTRUTH',
            flagLEpton      = INTE [0,4]  : 'LEPID from QTRUTH',
            flagMEss        = INTE [-2,3] : 'IBMES from QTRUTH',
            KineTrack       = INTE [1,*]  : 'FKIN track number')
 
         ;
 
 
    PLJT
         :    'Jets for Heavy Flavor Leptons from Mini-DST\
               Number of words per jet \
               Number of jets'
         SIZE 1,1
         STATIC
 
         = (PX              = REAL [*,*] : ' X component of jet momentum',
            PY              = REAL [*,*] : ' Y component of jet momentum',
            PZ              = REAL [*,*] : ' Z component of jet momentum',
            PE              = REAL [*,*] : ' Jet energy ',
            NObjects        = INTE [1,*] : ' Number of objects in jet ')
 
        ;

 DENW : 'Energy Flow Wordcal\
         Number of Words per Object\
         Number of Objects'
         STATIC
 
       = (LC         = INTE [0,*]     : 'LinkCalo\
                                         calobject # associated to energy flow object')
       ;
 
 END ESET
 
 
 DEFINE RSET
/*---------------------------------------------------------------------*/
      (DVER [0,1] -> [0,1] DTRA BY DTRA)
       : 'Pointer to Parent Track';
      (DEID [1,1] -> [0,1] DTRA BY DTRA)
       : 'Pointer to Associated Track';
      (DEID [1,1] -> [0,1] DECO BY DECO)
       : 'Pointer to Associated Ecalobject';
      (DCRL [0,1] -> [0,*] DECO BY DECO)
       : 'Pointer to Row Index in DECO';
      (DCRL [0,1] -> [0,*] DTRA BY DTRA)
       : 'Pointer to Row Index in DTRA';
      (DCRL [0,1] -> [0,*] DHCO BY DHCO)
       : 'Pointer to Row Index in DHCO';
      (DHRL [0,1] -> [0,*] DHCO BY DHCO)
       : 'Pointer to Row Index in DHCO';
      (DHRL [0,1] -> [0,*] DPOB BY DPOB)
       : 'Pointer to Row Index in DPOB';
      (DMUO [0,1] -> [0,*] DTRA BY DTRA)
       : 'Pointer to Associated Track';
      (DGAM [0,1] -> [0,*] DECO BY DECO)
       : 'Pointer to Associated Ecalobject';
      (DFOT [0,1] -> [0,*] DECO BY DECO)
       : 'Pointer to Associated Ecalobject';
      (DGID [0,1] -> [0,*] DECO BY PECO)
       : 'Pointer to Associated Ecalobject';
      (DGPC [0,1] -> [0,*] DECO BY PECO)
       : 'Pointer to Associated Ecalobject';
      (DNEU [0,1] -> [0,*] DECO BY DECO)
       : 'Pointer to Associated Ecalobject';
      (DENF [1,1] -> [0,1] DJET BY DJET)
       : 'Pointer to Associated Jet';
      (DDLT [1,1] -> [1,*] DTRA BY FRFT)
       : 'Pointer to Associated Track';
      (DMLT [1,1] -> [1,*] DTRA BY FRFT)
       : 'Pointer to Associated Track';
      (DGAC [0,1] -> [0,*] DECO BY PECO)
       : 'Pointer to Associated Ecalobject';
      (PDLT [1,1] -> [1,*] DTRA BY FRFT)
       : 'Pointer to Associated Track';
      (PMLT [1,1] -> [1,*] DTRA BY FRFT)
       : 'Pointer to Associated Track';
 
 END RSET
 
 END SUBSCHEMA


 SUBSCHEMA NanoDSTBanks
 :'Nano Data Summary Tape banks'

 
 AUTHOR   'J.Boucrot'
 REVIEWER 'F.Loverre'
 VERSION  '1.1'
 DATE     '26/02/96'
 
 DEFINE ESET
 
 
    NDAR
         :    'Nano Dst Particle bank (from PART)\
               Number of Columns \
               Number of particles'

         STATIC
 
       = (PArticletype  = INTE [*,*] : 'Particle type :   \
                                        bit   0- 7 : Geant number\
                                        bit   8-15 : Geant tracking code\
                                        ..... 1 = Photons\
                                        ..... 2 = Electrons\
                                        ..... 3 = Neutr. Hadrons + Neutrinos\
                                        ..... 4 = Charged Hadrons\
                                        ..... 5 = Muons\
                                        ..... 6 = Geantinos\
                                        ... 100 = Not tracked particle\   
                                        bit  16-20 : charge\
                                        bit  21-30 : ANtiparticle\
                                        ..... Corresponding antipart. number',
          NAme(3)       = INTE       : 'Particle names',
          MAss          = INTE [*,*] : 'Particle Mass\
                                        bit   0- 8 : exponent\
                                        bit   9-32 : mantissa',
          LifeTime      = INTE [*,*] : 'Particle Lifetime\
                                        bit   0- 8 : exponent\
                                        bit   9-32 : mantissa',
          MassWidth     = INTE [*,*] : 'Particle mass width\
                                        bit   0- 8 : exponent\
                                        bit   9-32 : mantissa');

    NDBM
         :    'Nano Dst leptag output bank\
               Number of Columns \
               Number of leptons'

         STATIC

       = (TA            = INTE [*,*]  : 'bit  0- 7 : NANO track number\
                                         ........... max (255)\    
                                         bit  8-15 : Particle type:\
                                         ... 2 -> e+\
                                         .. 12 -> e+ in crack region\
                                         .. 22 -> e+ in overlap region\
                                         ... 3 -> e-\
                                         .. 13 -> e- in crack region\
                                         .. 23 -> e- in overlap region\
                                         ... 5 -> mu+\
                                         ... 6 -> mu-\
                                         bit 16-23 : Pointer to jet in jet\
                                         ........... section\
                                         bit 24-31 : IDF/Truth flag\
                                         ... bit 24 : Muon IDF 13\
                                         ... bit 25 : Muon IDF 14\
                                         ... bit 26 : Genuine electron positron',
           PI           = INTE [0,*]  : 'Transverse momentum  (keV)\        
                                            lepton inclusive',
           PE           = INTE [0,*]  : 'Transverse momentum  (keV)\
                                            lepton exclusive',
           DA           = INTE [*,*]  : 'bit  0- 3 : Primary quark flavour from\
	                                 ........... FINLEP\
                                         ... 0 -> not a MC q-qbar event\
                                         ... 1 -> d quark\
                                         ... 2 -> u quark\
                                         ... 3 -> s quark\
                                         ... 4 -> c quark\
                                         ... 5 -> b quark\
                                         bit  4-11 : Decay category from FINLEP\
                                         ... 1 -> b -> mu + charmed hadrons\
                                         ... 2 -> b -> mu + non charmed hadrons\
                                         ... 3 -> b -> tau -> mu\
                                         ... 4 -> b -> c -> mu\
                                         ... 5 -> b -> cbar -> mu\
                                         ... 6 -> c -> mu\
                                         ... 7 -> c -> tau -> mu\
                                         ... 8 -> b -> c -> tau -> mu\
                                         ... 9 -> K -> mu or pi -> mu\
                                         .. 10 -> gamma -> mu\
                                         .. 11 -> J/psi -> mu\
                                         .. 12 -> psi prime -> mu\
                                         .. 13 -> other decays to muon\
                                         .. 14 -> tau decay\
                                         .. 15 -> muon from other sources\
                                         .. 16 -> misidentified hadron\
                                         .. 17 -> muon -> electron\
                                         .. 18 -> others\
                                         .. 19 -> error in finding some mother\
                                         .. 20 -> assoc. Kingal track not found\
                                         .. 21-35 -> as 1-15 but e instead\
                                         .. negtve -> parent quark is from a\
                                         ............ gluon\
                                         bit 12-21 : Code of the lepton parent
                                         bit 22-31 : Energy flow object number');
   
    NDCL :    'Nano Dst Charged Leptons (from QSELEP output banks)\
               Number of Columns \
               Number of leptons'

         STATIC

       = (TrAckattributes  = INTE [*,*] : 'Track Attributes\
                                           bit  0- 1 : Particle type\
                                           ... 0 - e+\
                                           ... 1 - e-\
                                           ... 2 - mu+\
                                           ... 3 - mu-\
                                           bit  2- 3 : Region code\
                                           ... if particle type e+ or e- :\
                                           ...... 0 - in ative areas\
                                           ...... 1 - crack region\
                                           ...... 2 - overlap region\
                                           .... if particle type mu+ or mu- :\
                                           ...... 0 - QMUIDO IDF=14\
                                           ...... 1 - QMUIDO IDF=13\
                                           bit  4- 5 : Flag giving validity of Pt calculation\
                                           ... 0 = OK\
                                           ... 1 = No track E-flow track found\
                                           ... 2 = No valid jet\
                                           ... 3 = Not enough object in jet\
                                           bit  6-13 : NANO track number max (255)\
                                           bit 14-16 : Primary quark type from\
                                           ........... QTRUTH (MC)\
                                           bit 17-19 : Popping Flag (MC)\
                                           ... 0  = primary quark\
                                           ... >0 = flavor of the popped qq\
                                           bit 20-21 : flagCHain  (MC)\
                                           ........... (ICHAIN from QTRUTH)-1\
                                           bit 22-25 : flagSPlit  (MC)\
                                           ........... ISPLIT from QTRUTH\
                                           bit 26-28 : flagLEpton (MC)\
                                           ........... LEPID from QTRUTH\
                                           bit 29-31 : flagMEss   (MC)\
                                           ........... (IBMES from QTRUTH)+2',
           PI              = INTE [0,*] : 'Transverse momentum  (keV)\
                                             lepton inclusive',
           PE              = INTE [0,*] : 'Transverse momentum  (keV)\
                                             lepton exclusive');

    NDDE :    'Nano Dst DE/dx\
               Number of Columns \
               Number of selected tracks with dE/dx info'

         STATIC

       = ( LenandIon  = INTE [*,*] : 'Track length and ionisation\
                                      bit 0 -11 : Track length in [mm]\
                                      bit 12-31 : Measured truncated mean\
                                      ........... ionisation * 100000',
           SampandErr = INTE [0,*] : 'Samples and relative error\
                                      bit 0 - 8 : Number of samples\
                                      bit 9 -31 : Relative error on the\
                                      ........... dE/dx * 100000');

    NDEJ :    'Nano Dst reconstructed Eflow Jets\
               Number of Columns \
               Number of reconstructed jets'

         STATIC

       = ( PX          = INTE [*,*] : 'Momentum X component (MeV)',
           PY          = INTE [*,*] : 'Momentum Y component (MeV)',
           PZ          = INTE [*,*] : 'Momentum Z component (MeV)',
           EnergyofJet = INTE [0,*] : 'Energy of the Jet (MeV)\
                                        ==========================\
                                        CHANGES vs version 111.4 :\
                                       integerized and was EJET before');

    NDGC :    'NDST Gamma-Conversion-Bank\
               Number of Columns \
               Number of selected Gamma Conversion candidates'

         STATIC

       = ( DAughtertracks = INTE [*,*] : 'DAughter tracks\
                                          bit  0 - 7 : nano track number of\
                                          ............ positron candidate\
                                          bit  8 -15 : nano track number of\
                                          ............ electron candidate\ 
                                          bit  16-17 : origin of conversion:\
                                          ... 0 : from QPAIRF\
                                          ... 1 : from QACONV\
                                          ... 2 : from QACONV\
                                          ... 3 : from QACONV\
                                          bit 18-31 : free',
           DXy            = INTE [*,*] : 'distance between the two tracks in the xy-plane
                                          at the closest approach to the conversion point
                                          (microns)',
           DZ2            = INTE [*,*] : 'z separation of the tracks at the closest approach
                                          to the conversion point (microns)',
           XMa            = INTE [*,*] : 'invariant mass of the tracks at the conversion
                                          point assuming they are both electrons (keV)',
           CX             = INTE [*,*] : 'Conversion coordinate (X)
                                          x-component of the conversion point (microns)',
           CY             = INTE [*,*] : 'Conversion coordinate (Y)
                                          y-component of the conversion point (microns)',
           CZ             = INTE [*,*] : 'Conversion coordinate (Z)
                                          z-component of the conversion point (microns)',
           PX             = INTE [*,*] : 'Photon momentum (X)\
                                          x-component of the momentum of the gamma candidate (MeV)',
           PY             = INTE [*,*] : 'Photon momentum (Y)\
                                          y-component of the momentum of the gamma candidate (MeV)',
           PZ             = INTE [*,*] : 'Photon momentum (Z)\
                                          z-component of the momentum of the gamma candidate (MeV)');

    NDHE :    'Nano Dst event HEader (up to version 115)\
               Number of Columns \
               Number of rows'

         STATIC

       = ( KRun          = INTE [0,*]  : 'Run number',
           KEvt          = INTE [0,*]  : 'Event number',
           ThRust        = INTE [*,*]  : '* 1 000 000 Thrust',
           TrkPointer    = INTE [*,*]  : 'Pointers to track groups\
                                          bit 0 - 7 First photon in hemisphere 2\
                                          bit 8 -15 First negative track in\
                                          ......... hemisphere 1\
                                          bit 16-23 First positive track in\
                                          ......... hemisphere 2\
                                          bit 24-31 First negative track in\
                                          ......... hemisphere 2',
           PhysicsFlag   = INTE [*,*] : 'Flag\
                                         bit 0 ... XLUMOK\
                                         bit 1 ... XMCEV\
                                         bit 2 - 5 Event flavor\
                                         ......... (0 for real data)\
                                         bit 6 -14 Theta of thrust axis in\
                                         ......... radians * 100 [0,3.14]\
                                         bit 15-24 Phi of thrustaxis in\
                                         ......... radians * 100 [0,6.28]\
                                         bit 25 .. 1 = ITC and TPC voltage OK\
                                         bit 26 .. 1 = TPC dE/dx voltage and\
                                         ............. calibration OK\
                                         bit 27 .. 1 = VDET data OK\
                                         bit 28 .. 0 = Error in QMUIDO/MUREDO\
                                         bit 29 .. 0 = Error in LEPTAG\
                                         bit 30 .. 0 = Error in JETSPH\
                                         ............. (charged tracks only)\
                                         bit 31 .. 0 = Error in JETSPH\
                                         ............. (ENFW objects)',
           SphprodCh     = INTE [*,*] : '* 1 000 000\
                                         Boosted jet sphericity product
                                         using charged tracks only',
           SphprodEnflw  = INTE [*,*] : '* 1 000 000\
                                         Boosted jet sphericity product
                                         using ENFW objects',
           DetStatus     = INTE [*,*] : 'Detector status word KREVDS',
           trkPointer2   = INTE [*,*] : 'Pointers\
                                         bit 0 - 7 First bad V0 track\
                                         bit 8 ... 0 = Error in BEETAG\
                                         bit 9 ... 0 = Error in QIPBTAG\
                                         bit 10 .. 0 = Event not useful for\
                                         ............. QIPBTAG analysis\
                                         bit 11 .. 0 = Error in GAMPEX\
                                         bit 12 - 15 = Return code from\
                                         ............. QIPBTAG+8\
                                         bit 16 - 24 = Number of Jets from\
                                         ............. QIPBTAG\
                                         bit 25 .. 0 = Mainvertex from JULIA\
                                         ......... 1 = Mainvertex from QFNDIP\
                                         bit 26 .. 0 = QFNDIP OK.\
                                         ......... 1 = QFNDIP failed\
                                         bit 27-31 Free',
           mainVtxXcoord = INTE [*,*] : 'X coordinate of main vertex\
                                         (microns)',
           mainVtxYcoord = INTE [*,*] : 'Y coordinate of main vertex\
                                         (microns)',
           mainVtxZcoord = INTE [*,*] : 'Z coordinate of main vertex\
                                         (microns)',
           Sphertag1     = INTE [*,*] : '* 1 000 000\
                                         Boosted sphericity hemi1',
           Sphertag2     = INTE [*,*] :  '* 1 000 000\
                                          Boosted sphericity hemi2',
           Transvmass1   = INTE [*,*] : '* 1 000 000\
                                         BEETAG transverse mass hemi 1',
           Transvmass2   = INTE [*,*] : '* 1 000 000\
                                         BEETAG transverse mass hemi 2',
           Mominertia1   = INTE [*,*] : '* 1 000 000\
                                         BEETAG moment of inertia hemi 1',
           Mominertia2   = INTE [*,*] : '* 1 000 000\
                                         BEETAG moment of inertia hemi 2\
                                         ==========================\
                                         CHANGES vs version 111.4 :\
                                         integerized\
                                         removed QIPBTAG probabilities\
                                         from word 10 - 12\
                                         put main vertex coordinates in\
                                         word 10 - 12\
                                         P2 : added bits 12-26');
 
    NEHE :    'Nano Dst event HEader ------ (> version 115)\
               Number of Columns \
               Number of rows'

         STATIC

       = ( KRun          = INTE [0,*]  : 'Run number',
           KEvt          = INTE [0,*]  : 'Event number',
           ThRust        = INTE [*,*]  : '* 1 000 000 Thrust',
           TrkPointer    = INTE [*,*]  : 'Pointers to track groups\
                                          bit 0 - 7 First photon in hemisphere 2\
                                          bit 8 -15 First negative track in\
                                          ......... hemisphere 1\
                                          bit 16-23 First positive track in\
                                          ......... hemisphere 2
                                          bit 24-31 First negative track in\
                                          ......... hemisphere 2',
           PhysicsFlag   = INTE [*,*] : 'Flag\
                                         bit 0 ... XLUMOK\
                                         bit 1 ... XMCEV\
                                         bit 2 - 5 Event flavor\
                                         ......... (0 for real data)\
                                         bit 6 -14 Theta of thrust axis in\
                                         ......... radians * 100 [0,3.14]\
                                         bit 15-24 Phi of thrust axis in\
                                         ......... radians * 100 [0,6.28]\
                                         bit 25 .. 1 = ITC and TPC voltage OK\
                                         bit 26 .. 1 = TPC dE/dx voltage and\
                                         ............. calibration OK\
                                         bit 27 .. 1 = VDET data OK\
                                         bit 28 .. 0 = Error in QMUIDO/MUREDO\
                                         bit 29 .. 0 = Error in QSELEP\
                                         bit 30 .. 0 = Error in QVSRCH\
                                         ............. (return code <  11)\
                                         bit 31 .. 0 = Serious error in QVSRCH\
                                         ............. (return code >= 11))',
           E1            = INTE       : 'Error on decaylength of 1. secondary
                                         vertex from QVSRCH (microns)',
           E2            = INTE       : 'Error on decaylength of 2. secondary
                                         vertex from QVSRCH (microns)',
           DetStatus     = INTE [*,*] : 'Detector status word KREVDS',
           trkPointer2   = INTE [*,*] : 'Pointers\
                                         bit 0 - 7 First bad V0 track\
                                         bit 8 ... 0 = Error in BEETAG\
                                         bit 9 ... 0 = Error in QIPBTAG\
                                         bit 10 .. 0 = Event not useful for\
                                         ............. QIPBTAG analysis\
                                         bit 11 .. 0 = Error in GAMPEX\
                                         bit 12 - 15 = Return code from\
                                         ............. QIPBTAG+8\
                                         bit 16 - 24 = Number of Jets from\
                                         ............. QIPBTAG\
                                         bit 25 .. 0 = Mainvertex from JULIA\
                                         ......... 1 = Mainvertex from QFNDIP\
                                         bit 26 .. 0 = QFNDIP OK.\
                                         ......... 1 = QFNDIP failed\
                                         bit 27-31 Free',
           mainVtxXcoord = INTE [*,*] : 'X coordinate of main vertex\
                                         (microns)',
           mainVtxYcoord = INTE [*,*] : 'Y coordinate of main vertex\
                                         (microns)',
           mainVtxZcoord = INTE [*,*] : 'Z coordinate of main vertex\
                                         (microns)',
           X1            = INTE       : 'X- coordinate of 1. secondary vertex\
                                         (QVSRCH) (microns)',
           Y1            = INTE       : 'Y- coordinate of 1. secondary vertex\
                                         (QVSRCH) (microns)',
           Z1            = INTE       : 'Z- coordinate of 1. secondary vertex\
                                         (QVSRCH) (microns)',
           X2            = INTE       : 'X- coordinate of 2. secondary vertex\
                                         (QVSRCH) (microns)',
           Y2            = INTE       : 'Y- coordinate of 2. secondary vertex\
                                         (QVSRCH) (microns)',
           Z2            = INTE       : 'Z- coordinate of 2. secondary vertex\
                                         (QVSRCH) (microns)',
           Btag1         = INTE       : '10000*btag(1)\
                                         btag - variable of 1.secondary vertex (QVSRCH)',
           Btag2         = INTE       : '10000*btag(2)\
                                         btag - variable of 2.secondary vertex (QVSRCH)');
 
    NDJT :    'Nano Dst reconstructed JeTs\
               Number of Columns \
               Number of reconstructed jets'

         STATIC

       = ( PX            = INTE [*,*] : 'Momentum X component (MeV)',
           PY            = INTE [*,*] : 'Momentum Y component (MeV)',
           PZ            = INTE [*,*] : 'Momentum Z component (MeV)',
           EnergyofJet   = INTE [0,*] : 'Energy of the Jet (MeV)\
                                         ==========================\
                                         CHANGES vs version 111.4 :\
                                         integerized');
 
    NDLV :    'Nano Dst Monte-Carlo V0 true decay vertex\
               Number of Columns \
               Number of Monte-Carlo V0'

         STATIC

       = ( POinter       = INTE [*,*] : 'Pointer to MC track',
           DecayvertexX  = INTE [*,*] : 'X coord of decay vertex in microns',
           DecayvertexY  = INTE [*,*] : 'Y coord of decay vertex in microns',
           DecayvertexZ  = INTE [*,*] : 'Z coord of decay vertex in microns\
                                         ==========================\
                                         CHANGES vs version 111.4 :\
                                         integerized');

    NDMS :    'Nano Dst Monte-Carlo Stable tracks\
               Number of Columns \
               Number of stable Monte-Carlo tracks'

         STATIC

       = ( PX            = INTE [*,*] : 'Momentum X component (MeV)',
           PY            = INTE [*,*] : 'Momentum Y component (MeV)',
           PZ            = INTE [*,*] : 'Momentum Z component (MeV)',
           HIstory       = INTE [*,*] : 'Track history\
                                         bit 0 - 9 Particle code\
                                         bit 10-17 Pointer to matched track\
                                         bit 18-21 KSTABC+4\
                                         bit 22-30 Pointer to mother\
                                         ......... [in NDMS if > rows(NDNT)]\
                                         bit 31 .. Type of matched track:\
                                         ......... 0=Photon,1=Charged\
                                         ==========================\
                                         CHANGES vs version 111.2 :\
                                         bit 18-21 Number of daughters\
                                         ==========================\
                                         CHANGES vs version 111.1 :\
                                         bit 22-29 Pointer to mother\
                                         bit 30-31 Type of matched track:\
                                         ......... 0=Neutral,1=Photon,2=Charged');

    NDNT :    'Nano Dst Monte-Carlo uNstable Tracks\
               Number of Columns \
               Number of unstable Monte-Carlo tracks'

         STATIC

       = ( HIstory    = INTE [*,*] : 'Track history\
                                      bit 0 - 9 Particle code\
                                      bit 10-17 Fragmentation variable z\
                                      ......... INT((z+0.004)*250)-0.0001)\
                                      bit 18-21 KSTABC+4\
                                      bit 22-30 Pointer to mother\
                                      ......... [in NDMS if > rows(NDNT)]\
                                      ==========================\
                                      CHANGES vs version 111.2 :\
                                      bit 18-21 Number of daughters\
                                      ==========================\
                                      CHANGES vs version 111.1 :\
                                      bit 22-29 Pointer to mother');

    NDOB :    'Nano Dst energy flow OBjects\
               Number of Columns \
               Number of energy flow objects'

         STATIC

       = ( PX         = INTE [*,*] : 'Momentum X component (MeV)',
           PY         = INTE [*,*] : 'Momentum Y component (MeV)',
           PZ         = INTE [*,*] : 'Momentum Z component (MeV)',
           EN         = INTE [*,*] : 'Corrected ENergy',
           TYpe       = INTE [*,*] : 'Track history\
                                      bit  0- 3 Particle type\
                                      bit  4-11 jet number if particle was a\
                                      ...... charged track (0 otherwise)\
                                      bit 12-19 Nano track number if particle
                                      ...... was a charged track\
                                      ...... (0 otherwise)\
                                      bit 20-27 FRFT track number if particle
                                      ...... was a charged track\
                                      ...... (0 otherwise)\
                                      bit 28-31 free');

    NDPH :    'Nano Dst PHotons\
               Number of Columns \
               Number of selected photons'

         STATIC

       = ( PX           = INTE [*,*] : 'Momentum X component (MeV)',
           PY           = INTE [*,*] : 'Momentum Y component (MeV)',
           PZ           = INTE [*,*] : 'Momentum Z component (MeV)',
           ParticleAttr = INTE [*,*] : 'Photon attributes\
                                        bit 0 - 7 PECO number\
                                        bit 8 - 9 Region code: \
                                        ......... 0=crack,1=barrel,\
                                        ......... 2=endcap,3=overlap\
                                        bit 10-11 Number of photons in this\
                                        ......... PECO (3 = 3 or more)\
                                        bit 12 .. 1 = energy in stack 1 > 0\
                                        bit 13 .. 1 = no minima in stack 2\
                                        bit 14-31 Free\
                                        Note: if >4 photons found in a PECO,\
                                        only first 4 kept.\
                                        ==========================\
                                        CHANGES vs version 111.2 :\
                                        bit 12-31 Free\
                                        ==========================\
                                        CHANGES vs version 111.4 :\
                                        integerized');

    NDST :    'Nano DST definition bank\
               Number of Columns \
               Number of rows'

         STATIC

       = ( MinTrk          = INTE [1,255]         : 'Minimum number of tracks',
           LowerPcut       = INTE [0,20]          : 'Lower momentum cut for NDTK (MeV)',
           UpperPcut       = INTE [0,200000]      : 'Upper momentum cut for NDTK (MeV)',
           CosTheta        = INTE [*,*]           : 'Cos theta cut for NDTK * 1000000',
           ItcHits         = INTE [0,255]         : 'ITC hits cut for NDTK',
           TpcHits         = INTE [0,255]         : 'TPC hits cut for NDTK',
           D0cut           = INTE [0,*]           : 'D0 (beam axis) cut (microns)',
           Z0cut           = INTE [0,*]           : 'Z0 (beam axis) cut (microns)',
           JetLowerpcut    = INTE [0,20000]       : 'Lower p cut before clustering (MeV)',
           JetUpperpcut    = INTE [0,200000]      : 'Upper p cut before clustering (MeV)',
           JetCostheta     = INTE [*,*]           : 'Cos theta cut before cluster\
                                                      * 1000000',
           JetItchits      = INTE [0,255]         : 'ITC hits cut before cluster.',
           JetTpchits      = INTE [0,255]         : 'TPC hits cut before cluster.',
           JetD0cut        = INTE [0,*]           : 'D0 cut before clustering (microns)',
           JetZ0cut        = INTE [0,*]           : 'Z0 cut before clustering (microns)',
           JYcut           = INTE [0,10000]       : 'Mass cut for jets (MeV)',
           VisibleEner     = INTE [-10000,200000] : 'Visible energy for jets (MeV)',
           RecoOption      = INTE                 : 'Reconstructed object option',
           LowerEcut       = INTE [0,20]          : 'Lower energy cut for NDPH (MeV)',
           PartHyp(75)     = INTE [*,*]           : 'Parameters for Particle Hyp * 1000',
           NumPotbanks     = INTE [0,10]          : 'Number of POT/DST/MINI banks',
           PotBanks(10)    = INTE                 : 'List of POT/DST/MINI banks',
           NumRunbanks     = INTE [0,10]          : 'Number of run banks',
           RunBanks(10)    = INTE                 : 'List of run banks',
           NumNdstbanks    = INTE [0,20]          : 'Number of NDST banks',
           NdstBanks(20)   = INTE                 : 'List of NDST banks',
           PRoduction      = INTE                 : 'Version of NDSTPROD',
           AlphaVers(2)    = INTE                 : 'Alpha version and corr file',
           DAte(2)         = INTE                 : 'Date of the production',
           MCoptions       = INTE [*,*]           : 'Options for MC data',
           MinMomentum     = INTE [0,100]         : 'Min. mom. in fragmentation (MeV)',
           ALephlibvers    = INTE [*,*]           : 'Alephlib version * 1000',
           ChargedEnergy   = INTE [0,10]          : 'Minimum charged energy (MeV)',
           MinimumEnergy   = INTE [0,*]           : 'Min. energy for ENFW objects (MeV)',
           EnfwjetMcut     = INTE [0,*]           : 'ENFW jets mass cut (MeV)',
           ChisincrSnglTrk = INTE [0,*]           : 'Cut on chi square increase for\
                                                     a single V0 track coming from\
                                                     main vertex',
           ChisincrBothTrk = INTE [0,*]           : 'Cut on chi square increase for\
                                                     both V0 tracks coming from\
                                                     main vertex\
                                                     ==========================\
                                                     CHANGES vs version 111.4 :\
                                                     integerized\
                                                     added words 149,150');
 
    NDTK :    'Nano Dst charged TracKs\
               Number of Columns \
               Number of selected charged Tracks'

         STATIC

       = ( PX           = INTE [*,*] : 'Momentum X component (MeV)',
           PY           = INTE [*,*] : 'Momentum Y component (MeV)',
           PZ           = INTE [*,*] : 'Momentum Z component (MeV)',
           DzeroZzero   = INTE [*,*] : 'D0 and Z0\
                                        bit 0 -15 D0 in [10^-3 cm]\
                                        bit 16-31 Z0 in [10^-3 cm]',
           TrkAttr      = INTE [*,*] : 'Track attributes\
                                        bit 0 - 7 JULIA track number (max 255)\
                                        bit 8 - 9 Number of VDET hits\
                                        ... (3 = 3 or more)\
                                        bit 10 .. Charge (1=+1,0=-1)\
                                        bit 11-14 Jet number = row of NDEJ\
                                        ......... (max 15)\
                                        bit 15-16 QVSRCH jet assignment\
                                        ...  0 - jet 1\
                                        ...  1 - jet 2\
                                        bit 17-18 QVSRCH vertex assignment\
                                        ...  0 - track failing cuts\
                                        ...  1 - track passing cuts but\
                                        ........ close to either vertex\
                                        ...  2 - track assigned to primary\
                                        ...  3 - track assigned to secondary\
                                        ........ in above jet\
                                        bit 19-26 Pointer to row in NDDE\
                                        ........ (max 255)\
                                        bit 27 .. Electron hypothesis\
                                        bit 28 .. Muon hypothesis\
                                        bit 29 .. Pion hypothesis\
                                        bit 30 .. Kaon hypothesis\
                                        bit 31 .. Proton hypothesis',
           TrkQuality   = INTE [*,*] : 'Track Quality\
                                        bit 0 -13 Chi square per degree of\
                                        ......... freedom * 100\
                                        bit 14-22 Relative error on\
                                        ......... momentum * 10000\
                                        bit 23-26 Number of ITC hits\
                                        bit 27-31 Number of TPC hits\
                                        ==========================\
                                        CHANGES vs version 111.4 :\
                                        integerized\
                                        CHANGES vs version 115.0 :\
                                        TA, bit 15-18 were lepton candidate number');

    NDV0 :    'NDST V0-Bank\
               Number of words/V0\
               Number of V0'

         STATIC

       = ( HYpothesis   = INTE [*,*] : 'bit  0-3  : Hypothesis (mass compat.)\
                                        bit  4    :  kinematic ambiguity\
                                        bit  5    : free\
                                        bit  6    : common track with another\
                                        ........ V0 candidate\
                                        bit  7    : free\
                                        bit  8-15 : Icodev0 + 32 (s.YV0V bank)\
                                        bit 16-23 : positive NANO track number\
                                        bit 24-31 : negative NANO track number',
           VXcoor       = INTE [*,*] : 'Fitted V0 x coordinate (micron)',
           VYcoor       = INTE [*,*] : 'Fitted V0 y coordinate (micron)',
           VZcoor       = INTE [*,*] : 'Fitted V0 z coordinate (micron)',
           VMom         = INTE [*,*] : 'Fitted V0 momentum (MeV)',
           THeta        = INTE [*,*] : 'Theta of V0 (mrad/10)',
           PHi          = INTE [*,*] : 'Phi   of V0 (mrad/10)',
           PPos         = INTE [*,*] : 'Momentum of positive 
                                        particle from V0 (see VM,TH,PH)',
           C2chisquare  = INTE [0,*] : 'Chisquare of V0 vertex fit*100',
           DL           = INTE [0,*] : 'Error on decay length (micron)',
           TT           = INTE [0,*] : 'bit  0-15 (chi for track1)*10 (QV0CHK)\
                                        bit 16-31 (chi for track2)*10 (QV0CHK)\
                                        ==========================\
                                        CHANGES vs version 111.4 :\
                                        bank completly redefined');

    NVEC :    'SANDY work bank (not in NanoDST files)\
               Number of Columns\
               Number of selected Tracks'

         STATIC

       = ( QP            = REAL [0.0,*]    : 'Momentum',
           QX            = REAL [*,*]      : 'Momentum X component',
           QY            = REAL [*,*]      : 'Momentum Y component',
           QZ            = REAL [*,*]      : 'Momentum Z component',
           QEnergy       = REAL [0.0,*]    : 'Energy',
           QMass         = REAL [0.0,*]    : 'Mass',
           CHarge        = REAL [*,*]      : 'Charge',
           DB            = REAL [*,*]      : 'D0',
           ZB            = REAL [*,*]      : 'Z0',
           TrackNumber   = INTE [*,*]      : 'Track number',
           PointerJet    = INTE [0,15]     : 'PointerJet',
           PointerLepton = INTE [0,15]     : 'Pointer to lepton cand. NDEC/UC',
           POinter       = INTE [0,15]     : 'Pointer to other bank',
           PointerIon    = INTE [0,255]    : 'Pointer to dE/dx (NDDE bank)',
           HYpothesis    = INTE [*,*]      : 'Particle HYpothesis\
                                              bit 0 ... Hypothesis 1 = e\
                                              bit 1 ... Hypothesis 2 = mu\
                                              bit 2 ... Hypothesis 3 = pi\
                                              bit 3 ... Hypothesis 4 = K\
                                              bit 4 ... Hypothesis 5 = p\
                                              bit 5 -31 Free',
           Chi2perdof    = REAL [0.0,*]    : 'Chi square per degree of freedom',
           RelSigma      = REAL [0.0,*]    : 'Relative error on momentum',
           ItcHits       = INTE [0,15]     : 'ITC Hits',
           TpcHits       = INTE [0,31]     : 'TPC Hits',
           VdetHits      = INTE [0,3]      : 'VDET Hits',
           McCode        = INTE [0,1023]   : 'Monte Carlo particle code',
           MAtchedtrack  = INTE [0,255]    : 'Matched reconstructed track',
           McDaughters   = INTE [0,15]     : 'Number of daughters',
           McMother      = INTE [0,511]    : 'Pointer to mother',
           McType        = INTE [0,2]      : 'Type of MC particle\
                                              0=Neutral,1=Photon,2=Charged',
           ZFragment     = REAL [0.0,1.00] : 'Fragmentation variable z',
           McKstabc      = INTE [-4,11]    : 'Stability code KSTABC',
           McFirstdaug   = INTE [0,*]      : 'Pointer to first daughter',
           McNextsister  = INTE [0,*]      : 'Pointer to next sister');





END ESET

END SUBSCHEMA

