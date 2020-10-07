 SUBSCHEMA JULPOTFitTrack
 : 'Description of global TPC+ITC+VDET banks for fitted tracks'
 
 AUTHOR   'F.Ranjard,T.Lohse,D.Casper'
 REVIEWER 'P.Rensing'
 VERSION  '4.2'
 DATE     '18/04/97'
 
 DEFINE ESET
 
  FKAL
      :      'Kalman filter fit constants
              NR=0.(JUL)\
              Number of words\
              Number of rows=1'
           STATIC
 
      = (meanBpipRad(2)      = REAL       : 'radius of the middle of the beam pipe \
                                             1=89-90,2=91',
         BpipradLength(2)    = REAL       : 'radiation length of the beam pipe \
                                             1=89-90,2=91',
         VdetradLength(2)    = REAL       : 'radiation length seen by one VDET face \
                                             1=89-90,2=91',
         ItcinnerRad         = REAL       : 'radius of the middle of the ITC inner wall',
         ItcradLength        = REAL       : 'radiation length seen by the ITC inner wall',
         ItcGasradlen        = REAL       : 'radiation length in the ITC gas per cm',
         meanItcTpcrad       = REAL       : 'radius of a wall between ITC and TPC',
         TpcradLength        = REAL       : 'radiation length seen by this wall',
         TpcGasradlen        = REAL       : 'radiation length in the TPC gas per cm',
         ZInnVdetEndc(2)   = REAL       : 'abs(Z) of discs for inner VDET endcaps \
                                             1=89-90,2=91',
         ZOutVdetEndc(2)   = REAL       : 'abs(Z) of discs for outer VDET endcaps \
                                             1=89-90,2=91',
         InnradInnEndc(2)  = REAL       : 'radius inner hole of inner endcap disc \
                                             1=89-90,2=91',
         InnradOutEndc(2)  = REAL       : 'radius inner hole of outer endcap disc \
                                             1=89-90,2=91',
         OutradInnEndc(2)  = REAL       : 'outer radius of inner endcap disc \
                                             1=89-90,2=91',
         OutradOutEndc(2)  = REAL       : 'outer radius of outer endcap disc \
                                             1=89-90,2=91',
         ScatparInEndc(2)  = REAL       : 'rad len inner VDET endcap disc \
                                             1=89-90,2=91',
         ScatparOuEndc(2)  = REAL       : 'rad len outer VDET endcap disc \
                                             1=89-90,2=91')
      ;
 
  FRFT
      :      'Global Geometrical track FiT
              NR=0.(JUL)\
              Number of words/track\
              Number of tracks'
           STATIC
 
      = (InverseRadi  = REAL [*,*]         : 'Inverse radius of curvature in x-y projection\
                                              signed +ve if track bends counterclockwise\
                                                     -ve if tracks bends clockwise',
         TanLambda    = REAL  [*,*]        : 'tangent of dip angle',
         Phi0         = REAL  [0.,6.3]     : 'Phi at closest point of approach to line x=y=0',
         D0           = REAL  [-180.,180.] : 'Closest distance of approach to line x=y=0\
                                              in the x-y plane (impact parameter)\
                                             (signed +ve if particle has a positive angular\
                                              momemtum around the origin, -ve otherwise)',
         Z0           = REAL  [-220.,220.] : 'Z-coordinate at D0',
         Alpha        = REAL  [-3.15,3.15] : 'Multiple scattering angle between tpc and itc',
         EcovarM(21)  = REAL  [*,*]        : 'Triangular covariance matrix stored in the order:\
                                                      1\
                                                      2  3\
                                                      4  5  6\
                                                      7  8  9 10\
                                                     11 12 13 14 15\
                                                     16 17 18 19 20 21',
         Chis2        = REAL [0.,*]          : 'Chisquare of helix fit',
         numDegFree   = INTE [0,63]          : 'Numer of degr. of freedom',
         nopt         = INTE [0,149]         : 'nopt=nmult*100+iopt*10+ierr\
                                                IOPT=1  Circle+line fit with UFITMS\
                                                IOPT=2  3-D iteration with UFITMS\
                                                IOPT=3  M.S. A*(R-R0)/R with UFITMS\
                                                IOPT=4  M.S. I-DD0(A) I-PH0(A) with UFITMS\
                                                IOPT=9  Kalman filter fit UFTKAL\
                                                NMULT=1 Increased TPC errors for M.S. in UFITMS\
                                                NMULT=0 TPC errors left alone\
                                                IERR=1 Circle+line fit failed in UFITMS\
                                                IERR=2 Cannot invert cov matrix from UFITMS\
                                                IERR=3 Cov. matrix from UFITMS not pos. def.\
                                                IERR=4 Kalman filter fit was tried but failed;
                                                The old fit from UFITMS was retained')
      ;
 
  FRTL
      :     'Tpc+Itc+Vdet Geometry Track point List.
             Rows correspond with rows in FRFT bank.
             NR=0. (JUL)\
             Number of words per track \
             Number of Tpc+Itc+Vdet geometry tracks '
           STATIC
 
      = (IoffV     = INTE [0,10000]   : ' offset in FVCL',
         NarcV     = INTE [0,4]       : ' numb. coord. in Vdet',
         IoffI     = INTE [0,10000]   : ' offset in FICL',
         NarcI     = INTE [0,8]       : ' numb. coord. in Itc',
         NrEsti    = INTE [0,1000]    : ' numb. coord. in following spirals in ITC',
         IoffT     = INTE [0,10000]   : ' offset in FTCL',
         NarcT     = INTE [0,21]      : ' numb. coord. in first arc in Tpc',
         NRestt    = INTE [0,1000]    : ' numb. coord. in following spirals, in Tpc')
      ;
 
  FVCL
      :     'Vdet Geometry track Coordinate List.
             Use FRTL to index into this bank
             NR=0. (JUL)\
             Number of words per coordinate \
             Number of coordinates associated with tracks '
           STATIC
 
      = (Ivdco     = INTE [1,10000]  : 'coordinate number in VDCO')
      ;
 
  FICL
      :     'Itc Geometry track Coordinate List.
             Use FRTL to index into this bank
             NR=0. (JUL)\
             Number of words per coordinate \
             Number of coordinates (signed) associated with tracks (-ve means 2nd  Phi hit used) '
           STATIC
 
      = (Iitco     = INTE [-10000,10000]  : 'coordinate number in ITCO')
      ;
 
  FTCL
      :     'Tpc Geometry track Coordinate List.
             Use FRTL to index into this bank
             NR=0. (JUL) \
             Number of words per coordinate \
             Number of coordinates associated with tracks '
           STATIC
 
      = (Itpco     = INTE [1,10000]  : 'coordinate number in TPCO')
      ;
 
  FRID
      :     'Particle identification bank for charged tracks.
             Rows correspond with rows in FRFT bank
             NR=0. (POT)\
             Number of words per track \
             Number of Tpc+Itc+Vdet geometry tracks '
           STATIC
 
      = (BitPat     = INTE [*,*]    :  ' Bit Patter for traking devices.\
                                       Bits correspont to layers on tracking devices,
                                       1 means that the layer has been used in the
                                       fit of this track,  0 not used.
                                       The first Bit corresponds to the information
                                       about the inner layer of VDET (2 layers in
                                       VDET, 8 in ITC and 21 in TPC)',
         DeadZone   = INTE [*,*]    :  ' Dead Zone Patter for tracking devices.\
                                       Bits correspont to layers on tracking
                                       devices, 1 means that this track cross a
                                       sensitive zone, 0 dead zone.
                                       The first Bit corresponds to the information
                                       about the inner layer of VDET (2 layers in
                                       VDET, 8 in ITC and 21 in TPC)',
         BitpatC    = INTE [*,*]    :  ' Bit Patter for calorimeters.\
                                       Bits correspont to layers on calorimeters,
                                       1 means that the layer has been associated
                                       to the track,  0 not associated.
                                       The first Bit corresponds to the information
                                       about the inner layer of ECAL (3 layers in
                                       ECAL, 23 in HCAL and 4 in MUON Chambers)',
         DeadzoneC  = INTE [*,*]    :  ' Dead Zone Patter for calorimeters.\
                                       Bits correspont to layers on calorimeters
                                       1 means the extrapolated track cross a
                                       sensitive zone, 0 cross a dead zone.
                                       The first Bit corresponds to the information
                                       about the inner layer of ECAL (3 layers in
                                       ECAL, 23 in HCAL and 4 in MUON Chambers)',
         ProbElec   = REAL [0.,*]   :  ' Probability density that a electron give what \
                                       has been observed in TPC , Ecal and\
                                       Hcal and Muon chambers',
         ProbMuon    = REAL [0.,*]  :  ' muon probability',
         ProbpIon    = REAL [0.,*]  :  ' pion probability',
         ProbKaon    = REAL [0.,*]  :  ' kaon probability',
         ProbProton  = REAL [0.,*]  :  ' proton probability',
         NoKinkprob  = REAL [0.,1.] :  ' probability of having no kink',
         QualityFlag = INTE [*,*]   :  ' Track Quality Flag from UFITQL')
      ;
 
  PFRF
      :      'Global Geometrical track FiT
              NR=0. (POT)\
              Number of words/track\
              Number of tracks'
           STATIC
 
      = (InverseRadi  = REAL [*,*]         : 'Inverse radius of curvature in x-y projection\
                                              signed +ve if track bends counterclockwise\
                                                     -ve if tracks bends clockwise',
         TanLambda    = REAL  [*,*]        : 'tangent of dip angle',
         Phi0         = REAL  [0.,6.3]     : 'Phi at closest point of approach to line x=y=0',
         D0           = REAL  [-180.,180.] : 'Closest distance of approach to line x=y=0\
                                              in the x-y plane (impact parameter)\
                                             (signed +ve if particle has a positive angular\
                                              momemtum around the origin, -ve othewise)',
         Z0           = REAL  [-220.,220.] : 'Z-coordinate at D0',
         Alpha        = REAL  [-3.15,3.15] : 'Multiple scattering angle between tpc and itc',
         ErrOrs(6)    = REAL  [*,*]        : 'Errors of parameters OR Eigenvalues of covariance matrix',
         EcorrelM(15) = INTE  [0,50000]    : '(Correlation terms)*25000+25000, stored in the order:\
                                                        2\
                                                        4   5\
                                                        7   8  9\
                                                        11 12 13 14\
                                                        16 17 18 19 10,\
                                               OR (Euler angles normalized to pi)*25000+25000\
                                                  from diagonalization',
         Chis2        = INTE [0,255]         : 'Chisquare of helix fit',
         nopt         = INTE [0,100000]      :'nopt=IFG*100000+NDF*1000+NMULT*100+IOPT*10+IERR,\
                                                where IFG tells what kind of packing was used
                                                for the covariance matrix\
                                                NDF is no.of degrees of freedom\
                                                from fit (NB. JUL240.20 and earlier have\
                                                NDF=0 always). NMULT, IOPT are the flags \
                                                used when calling UFITMS or UFTKAL,\
                                                and IERR is the output error code')
      ;
 
  PFRT
      :     'Tpc+Itc+Vdet Geometry Track point List.
             Rows correspond with rows in PFRF bank
             NR=0. (POT)\
             Number of words per track \
             Number of Tpc+Itc+Vdet geometry tracks '
           STATIC
 
      = (NarcV     = INTE [0,4]       : ' numb. coord. in Vdet',
         NarcI     = INTE [0,8]       : ' numb. coord. in first arc in Itc',
         NrEsti    = INTE [0,255]     : ' numb. coord. in following spirals, in Itc',
         NarcT     = INTE [0,21]      : ' numb. coord. in first arc in Tpc',
         NRestt    = INTE [0,255]     : ' numb. coord. in following spirals, in Tpc')
      ;
 
  ZPFR
      :      'Set of constants to write/read POT
              NR=0. (POT)\
              Number of costants\
              Number of sets'
           STATIC
 
      = (Constant1   = REAL [*,*]         : ' c1 where corre=corre*c1+c2',
         Constant2   = REAL [*,*]         : ' c2')
      ;
 
  FXTR
      :      'Fit eXtra TRack information
              NR=0.(JUL)\
              Number of words/track\
              Number of tracks'
           STATIC
 
      = (V0flag              = INTE [0,1]      : '=1 if long V0',
         NuCflag             = INTE [0,1]      : '=1 if nuclear interaction',
         KiNkflag            = INTE [0,1]      : '=1 if kink',
         VDetflag            = INTE [0,1]      : '=1 if track failed VDET\
                                                  internal preselection')
      ;
 
  PFXT
      :      'POT Fit eXtra Track information
              NR=0.(POT)\
              Number of words/track\
              Number of tracks'
           STATIC
 
      = (TrackNumber         = INTE [1,*]      : 'FRFT track number',
         V0flag              = INTE [0,1]      : '=1 if long V0',
         NuCflag             = INTE [0,1]      : '=1 if nuclear interaction',
         KiNkflag            = INTE [0,1]      : '=1 if kink',
         VDetflag            = INTE [0,1]      : '=1 if track failed VDET\
                                                  internal preselection')
      ;
 
  UFG2
      :      'Kalman filter scattering and dE/dx
              NR=0.(JUL)\
              Number of words\
              Number of rows=1'
           STATIC
 
      = (CoreFraction        = REAL [0.,1.] : 'Weight of the core scattering Gaussian',
         CoreWidth           = REAL [0.,*]  : 'Width of core scattering Gaussian \
                                                (nominal width = 1)',
         TailWidth           = REAL [0.,*]  : 'Width of tail scattering Gaussian',
         FilterProbability   = REAL [0.,1.] : 'Probability at which to remove coord \
                                                (= 0. for no outlier removal)',
         DEdxFlag            = INTE [0,1]   : 'Flag to turn on energy loss during fitting \
                                                (= 0 for no energy loss)')
      ;
 
 YKNK
      :      'Kink List \
              Number of words/Kink\
              Number of Kinks'
           STATIC
 
      = (InnerTrack    = INTE [1,*]   : 'FRFT No. of track producing the kink',
         OuterTrack    = INTE [0,*]   : 'FRFT No. of track coming from kink',
         VerteX(3)     = REAL [*,*]   : 'Fitted position of kink',
         VertexCov(6)  = REAL [*,*]   : 'Covariance matrix of kink position:\
                                         1\
                                         2 3\
                                         4 5 6',
         HelixInner(5) = REAL [*,*]   : 'Helix parameters of inner track after\
                                         vertex fit',
         HelixCov(15)  = REAL [*,*]   : 'Covariance matrix of inner helix',
         PatKink       = REAL [*,*]   : 'Momentum of parent at kink position',
         PTouter       = REAL [*,*]   : 'Pt of outgoing charged particle\
                                         relative to parent',
         PLouter       = REAL [*,*]   : 'Pl of outgoing charged particle\
                                         relative to parent',
         PCov(6)       = REAL [*,*]   : 'Covariance matrix of PatKink PTouter\
                                         PLouter stored in the order\
                                                1\
                                                2  3\
                                                4  5  6',
         Chi2          = REAL [0.0,*] : 'Chi**2 of fit',
         MindistXY     = REAL [0.0,*] : 'Minimum distance in XY plane',
         MindistZ      = REAL [0.0,*] : 'Minimum distance along Z',
         BitHypoth     = INTE [0,*]   : 'Bitmask of mass cuts satisfied\
                                         by this decay. Starting at bit 0,\
                                         each bit corresponds to a row in\
                                         YKSP:\
                                         0 = pi -> mu nu\
                                         1 = K -> mu nu\
                                         2 = K -> pi pi0\
                                         3 = Sigma+ -> p pi0\
                                         4 = Sigma+ -> pi+ n\
                                         5 = Sigma- -> pi- n\
                                         6 = Xi- -> pi- Lambda\
                                         7 = Omega- -> K- Lambda\
                                         8 = Omega- -> pi- Xi0\
                                         9 = Omega- -> Xi- pi0',
         VdetHits(8)   = INTE [0,*]   : 'VDet Hits on inner track',
         ChiVdet       = REAL [0.0,*] : 'Chi**2 of VDET hit association',
         ChiDiff       = REAL [0.0,*] : 'Chi**2 between VDET comb.\
                                         and next best')
       ;  
  


 END ESET
 
 END SUBSCHEMA
