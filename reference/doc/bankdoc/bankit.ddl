 SUBSCHEMA ItcJuliaBanks
 : 'ITC JULIA banks'
 
 AUTHOR   'J.Sedgbeer I.Tomalin R.Johnson,B.Atwood, J.Carr'
 REVIEWER 'R. Beuselinck J.Sedgbeer'
 VERSION  '3.7'
 DATE     '28/04/93'
 
 DEFINE ESET


 ICCO :    'Itc Corrected COordinates for the final track fit.
            This bank is parallel to ITCO. Use track trajectory
            to correct used coords. (for Z, TOF, signal time and
            entrance angle thru cell) Used coords. correspond to lists
            in IGCL or FICL. Unused coords. cannot be corrected and have
            null entries. (JUL) \
            Number of words per coordinate\
            Number of coordinates.'
            STATIC

      = (RValue       = REAL [15.000,27.3] : 'radius of coord.(cm.)',
         PHi          = REAL [0.,6.30000]  : 'Phi of coord.',
         ZValue       = REAL [-150.,150.0] : 'Z of coord (cm.)',
         SigmaRphi    = REAL [.000000,1.0] : 'Sigma(r-phi)**2  (cm**2)',
         SigmaZ       = REAL [0.0,*]       : 'Sigma(z)**2      (cm**2)')
      ;

  IDCR
      :      'Itc Digitising to Coordinate Relation (NR=0) (JUL)\
              Number of words/digit \
              Number of digits'
           STATIC
 
      = (CoordNumber  = INTE [0,960]       : 'Coord number in ITCO bank\
                                              of coord. constructed from this\
                                              digitising. Zero if no coord.') 
      ;

 IGCL
      :      'Itc track coord. list.(Recon.bank)
              (banks 1 (ITC-TPC) and 2 (ITC alone)) (JUL) \
              Number of words/coord.\
              Number of coords. associated with tracks'
           STATIC
 
      = (ITco         = INTE [-960,960]    : 'Coord. number in ITCO.\
                                              +ve if 1st coord of pair\
                                              -ve if 2nd coord of pair' )
      ;
                                                                 

 IGTL
      :      'Itc track point list (Recon.bank)
              (banks 1 (ITC-TPC) and 2 (ITC alone)) (JUL) \
              Number of words/track \
              Number of ITC tracks'
           STATIC
 
      = (IOff         = INTE [0,1000]      : 'Offset in IGCL',
         N1           = INTE [0,8]         : 'Number of coords.',
         NR           = INTE [0,100]       : 'Number of coords in\
                                              following spirals')
      ;

 ILIV
      :      'Itc LIVe wires - wire (channel) status (JUL) \
              Number of words/wire  \
              Number of wires.'
           STATIC
 
      = (FLag         = INTE [0,*]         : 'Status of wire (channel)\
                                              Value of Flag same as in\
                                              IWST bank. Zero if O.K.')
      ;

 IPJT
      :      'Itc Julia/MC track list (Recon.bank)
              (banks 1 (ITC-TPC) and 2 (ITC alone)) (JUL) \
              Number of words/track \
              Number of ITC reconstructed tracks'
           STATIC
 
      = (NHit         = INTE [0,1000]      : 'Number of ITC coords. on track',
         NShare       = INTE [0,1000]      : 'Number of MC(Kine) tracks sharing\
                                              coords with this track',
         RowNumber    = INTE [1,1000]      : 'Row number, in IRJT bank, of\
                                              first of MC tracks.')
      ;
                                                                 
 IQXT :    'Itc Quality of cross(X)ings on a Track. (JUL) \
            Number of words per layer\
            Number of layers (=8).'
            STATIC

      = (FLag       = INTE [-10,10]       : 'Flag',
         WireNumber = INTE [0,960]        : 'Wire number (=0 if no crossing).
                                             If no crossing then only the 1st
                                             3 columns (flag,wire=0,coord=0)are
                                             filled - the contents of the other
                                             columns is UNDEFINED.',
         COordinate = INTE [0,960]        : 'Coordinate number (in ITCO),
                                             =0 if no coord and then the coord.
                                             info. (DriftTime to ErrorZ
                                             inclusive) is UNDEFINED.',
         PhiWire    = REAL [0.,6.300000]  : 'Phi of wire',
         WireXyz(3) = REAL [-120.,120.0000] : 'Wire position (x,y,z)',
         HitXyz(3)  = REAL [-120.,120.0000] : 'Track position at DOCA point',
         DoCa       = REAL [-5.,5.00000]  : 'Dist. of closest approach',
         DriftTime  = REAL [0.,300.000]   : 'Drift Time (ns.)',
         DriftDist  = REAL [-5.,5.00000]  : 'Drift Distance (signed)',
         ErrorRphi  = REAL [0.,1.00000]   : 'Sigma Rphi, error on driftdist',
         ResidDoca  = REAL [-1.,1.00000]  : 'Resid=abs(driftdist)-abs(doca)',
         ResidFit   = REAL [-1.,1.00000]  : 'Resid=driftdist-Doca',
         ResidZ     = REAL [-200.,200.00] : 'Resid=Zcoord-Ztrack(at DOCA)',
         ErrorZ     = REAL [0.,3000.0]    : 'Error on Z (cm)')
       ;

 IRJT
      :      'Itc MC track list (Recon.bank)
              (banks 1 (ITC-TPC) and 2 (ITC alone)) (JUL) \
              Number of words/track \
              Number of ITC MC (Kine) tracks'
           STATIC
 
      = (GeantNumber  = INTE [*,*]         : 'Geant(kine) number of track.\
                                              Note that Geant Stack tracks\
                                              have negative track numbers.',
         NHit         = INTE [0,1000]      : 'Number of digits. on KINE track',
         SHare        = INTE [0,1000]      : 'Number of digits (coords) which\
                                              this MC track shares with the recon.\
                                              track in IPJT')
      ;
                                                                 
 ITCO
      :      'ITc COordinates (Recon. Bank)
              (Banks 0 (corrected coords.) and 1 (raw coords.).
              If only bank 0 exists, it gives the raw coords.)
              (Data is ordered by increasing wire number). (JUL) \
              Number of words/coordinate\
              Number of coordinates'
           STATIC
 
      = (WireNumber   = INTE [1001,8960]   : '1000*Layer + Wire No.',
         RAdius       = REAL [16.000,26.3] : 'radius (cm.)',
         Phi1         = REAL [0.,6.30000]  : 'Phi of hit (or ambiguity)(rads.)',
         Phi2         = REAL [0.,6.30000]  : 'Phi of ambiguity (or hit)(rads.)',
         ZHit         = REAL [-150.,150.0] : 'Z of hit (cm.)',
         SigmaRphi    = REAL [.000000,1.0] : 'Sigma(r-phi)**2  (cm**2)',
         SigmaZ       = REAL [0.0,*]       : 'Sigma(z)**2      (cm**2)',
         DriftTime    = REAL [0.,512.0]    : 'Drift Time calc. from TDC (ns.)')
      ;
     
 ITFT
      :      'ITC-TPC track FiT (NR=1,2) (Recon. bank)
              (banks 1 (ITC-TPC) and 2 (ITC alone)) (JUL) \
              Number of words/track\
              Number of tracks'
           STATIC
 
      = (InverseRadi  = REAL [*,*]         : 'Inverse radius of curvature in x-y projection\
                                              (signed as in FRFT bank)',
         TanLambda    = REAL [*,*]         : 'tangent of dip angle',
         Phi0         = REAL [0.,6.30000]  : 'Phi at closest point of approach to line x=y=0',
         D0           = REAL [-180.,180.0] : 'Closest distance of approach to line x=y=0
                                              in the x-y plane (impact parameter)\
                                              (signed as in FRFT bank)',
         Z0           = REAL [-220.,220.0] : 'Z-coordinate at D0',
         ScattAngle   = REAL [-1.,1.0000]  : 'Multiple scattering angle between ITC and TPC',
         CovMat(21)   = REAL [*,*]         : 'Triangular covariance matrix stored in the order:\
                                              . 1  2  4  7 11 16\
                                              . .  3  5  8 12 17\
                                              . .  .  6  9 13 18\
                                              . .  .  . 10 14 19\
                                              . .  .  .  . 15 20\
                                              . .  .  .  .  . 21',
         ChisquareD   = REAL [0.,*]        : 'Chisquare of fit (R-phi + z)',
         NumDegfree   = INTE [0,100]       : 'Number of degr. of freedom = 2N - P \
                                                where N= no. of coords and P = no. of fit params.',
         FitType      = INTE [1,14]        : 'Fit type: depends on options used in fit routine FITMS\
                                                  Fit type  = NMULT*100 + IOPT*10 + IERR')
      ;

 ITMA
      :      'JULIA track to FKIN track association
              (using ITC + TPC coordinate information). (JUL) \
              Number of words/FRFT track.\
              Number of FRFT tracks.'
           STATIC

      = (NumberAssociated  = INTE [0,*]   : 'Number of associated FKIN tracks',
         OFfset            = INTE [0,*]   : 'Offset to 1st FKIN track in IASL\
                                             bank',
         NumberCoords      = INTE [1,*]   : 'No. of ITC + TPC coords. on this\
                                             FRFT track')
      ;

 IASL
      :      'JULIA track to FKIN track association list
              (accessed via ITMA bank).
              (ordered according to increasing FRFT track number) (JUL) \
              Number of words/associated FKIN track.\
              Number of associated FKIN tracks.'
           STATIC

      = (FKin             = INTE [1,*]    : 'No. of FKIN track associated with\
                                             the FRFT track',
         NumberHits       = INTE [1,*]    : 'No. of hits shared with this FKIN\
                                             track',
         ChiSquared       = REAL [0.,*]   : 'Chi**2 of comparison of helix\
                                             params. of FRFT and FKIN track')
      ;
        
 IWCR
      :      'Itc Wire to Coordinate Relation (NR=0) (JUL) \
              Number of words/wire  \
              Number of wires.'
           STATIC
 
      = (CoordNumber  = INTE [0,960]       : 'Coord number in ITCO bank\
                                              of coord. on this wire.\
                                              Zero if no coord.') 
      ;

 ICTR
      :      'Itc Coordinate to TRack relation (NR=0) (JUL) \
              Number of words/coord.  \
              Number of coords.'
           STATIC
 
      = (TrackNumber  = INTE [0,1000]    : 'Track number of track closest\
                                            to this coord. Zero if no track.', 
         DeViation    = REAL [*,*]       : 'Absolute value of Residual/sigma',
         AMbiguity    = INTE [-1,1]      : 'Coord. ambiguity flag\
                                            +1 if on +ve phi side of wire\
                                            -1 if on -ve phi side of wire')
      ;

 ICMR
      :      'Itc Coordinate to teMporary tRack relation (NR=0) (JUL) \
              Number of words/coord.  \
              Number of coords.'
           STATIC
 
      = (ItcLayer    = INTE [0,1000]    : 'Layer of this coordinate.', 
         useCouNt    = INTE [0,1000]    : 'Number of tracks using this coord',
         Track1      = INTE [0,1000]    : 'Track number 1',
         Track2      = INTE [0,1000]    : 'Track number 2')
      ;

 IEXC 
      :    'Itc EXtension Candidates in tpc.
            This bank is parallel to the TPCO bank
            and flags those TPC points which are
            candidates for joining onto ITC tracks. (JUL) \
            Number of words per coordinate\
            Number of TPC coordinates.'
            STATIC

      = (FLag         = INTE [0,1]         : '1 for candidate,0 otherwise')
      ;

 ITPC  
      :      'TPC coordinates to be joined to current
             ITC track (JUL)\
             Number of words per coordinate \
             Maximum number of possible TPC points (=100)'
           STATIC
 
      = (CoordNumber  = INTE [0,*]         : 'TPC coord number')  
      ;

 END ESET
 
 END SUBSCHEMA


 SUBSCHEMA ItcPOTBanks
 : 'ITC POT banks'
 
 AUTHOR   'J.Sedgbeer I.Tomalin'
 REVIEWER 'R. Beuselinck'
 VERSION  '1.2'
 DATE     '16/04/91'
 
 DEFINE ESET
                             
 PIDI
      :      'Packed Itc DIgitisings (NR=0) (POT) \
              Number of words/digit.\
              Number of digits '
           STATIC
 
      = (PackedDigit  = INTE [*,*]  :'ITC digits. info. packed into\
                                       a 32 bit word.         \
                                     10 bits wire number   - bits  0 -  9,\
                                      9 bits r-phi TDC     - bits 10 - 18,\
                                      9 bits Z TDC         - bits 19 - 27,\
                                      2 bits quality flag  - bits 28 - 29,\
                                      1 bit ambiguity flag - bit  30,\
                                      1 bit version flag   - bit  31 (not
                                      used from 1991 onwards')
      ;

 PITM 
      :      'JULIA track to FKIN track association
              (using ITC + TPC coordinate information). (POT) \
              Number of words/FRFT track.\
              Number of FRFT tracks.'
           STATIC
 
      = (NumberAssociated = INTE [0,*]    : 'Number of associated FKIN tracks')
      ;

 PASL
      :      'JULIA track to FKIN track association list
              (accessed via PITM bank).
              (ordered according to increasing FRFT track number) (POT) \
              Number of words/associated FKIN track.\
              Number of associated FKIN tracks.'
           STATIC

      = (FKin             = INTE [1,*]    : 'No. of FKIN track associated with\
                                             the FRFT track',
         NumberHits      = INTE [1,*]    : 'No. of hits shared with this FKIN\
                                             track')
      ;


 END ESET

 END SUBSCHEMA


 SUBSCHEMA ItcEventRecord
 : 'ITC Raw Data Event Record banks'
 
 AUTHOR   'J. Sedgbeer'
 REVIEWER 'R. Beuselinck, M.Cattaneo'
 VERSION  '1.6'
 DATE     '23/09/91'
 
 DEFINE ESET

 IDIG
      :      'Itc DIGitisings (ordered by increasing wire number) (RAW) \
              Number of words/digit.\
              Number of digits '
           STATIC
 
      = (PackedDigit  = INTE [*,*]   :'ITC digits. info. packed into\
                                       a 32 bit word.         \
                                       10 bits wire number - bits  0 -  9,\
                                        9 bits r-phi TDC   - bits 10 - 18,\
                                        9 bits Z TDC       - bits 19 - 27,\
                                        2 bits quality flag- bits 28 - 29,\
                                        2 bits spare       - bits 30 - 31.
                                        (least signif. bit = bit 0).')
      ;

 ISTA
      :      'Itc STAtus bank. Only exits before run 5300, after this 
              the ITC HT status is read out in the XTCN bank. (RAW) \
              Number of words/row.\
              Number of rows (=1).'
           STATIC

      = (HTstatus   = INTE  0|1    : ' = 1 if HT on,\
                                       = 0 if HT tripped or off.\
                                       Note that the status is derived\
                                       from the HT switch but that it takes\
                                       about 5s. for the HT to ramp to its\
                                       proper value.')
      ;
 
 ISWB
      :      'Itc Suppressed Wire Blocks (RAW) \
              Number of words/suppressed block\
              Number of blocks'
           STATIC
     
      = (Wire1        = INTE   [1,960] : 'First wire of suppressed block',
         BlockLength  = INTE   [1,144] : 'Length of suppressed block')
      ;

 IXRP
      :      'Itc Trigger R-Phi Mask bits. (RAW) \
              Number of words/R-phi trigger\
              Number of rows (=1).'
           STATIC

      = (TrigBits(6)  = INTE  [*,*]    : '192 ITC r-phi trigger mask bits:\
                                          first word contains bits 1-32,\
                                          second word bits 33-64, etc.\
                                          Trigger bit  1 in bit  0 of word 1,\
                                          Trigger bit 32 in bit 31 of word 1,\
                                          Trigger bit 33 in bit  0 of word 2,\
                                          ...etc.(least signif. bit = bit 0).')
      ;
 
 IXTR
      :      'Itc R-Phi Trigger Board words. This bank contains
              all info. read out from the R-phi trigger board packed
              into 75 words (rows). For exact details of what each
              word contains see an expert! (RAW) \
              Number of words/row\
              Number of rows (=75).'
           STATIC

      = (TriggerInfo  = INTE  [*,*]    : 'R-phi trigger info. - packed')
      ;
 
 IXSI
    : 'Space point processor integral board information. Each word has 24 
       significant bits corresponding to fired masks, 48 masks per board, 
       288 masks total. (RAW)\
        No. of words/board\
        No. of boards (=6)'

        STATIC
        = (FiredMasks(2) = INTE [*,*] : '24 significant bits in each word\
                                         corresponding to fired masks 1-24\
                                         and 25-48')
        ;

 IXST
    : 'Space point processor trigger board words. 30 significant bits 
       per word corresponding to 60 trigger segments. (RAW)\
        No. of words/board\
        No. of boards (=1)'

        STATIC
        = (TrigSegments(2) = INTE [*,*] : '30 significant bits in each\
                                          word corresponding to trigger\
                                          segments 1-30 and 31-60')
        ;

 IZDB
    : 'Space point processor Debug Timings. (RAW)\
        No. of cols. \
        No. of rows  (=96)'

        STATIC
        = (TimingInfo  = INTE [*,*] : 'Bits 0-9 Leading Edge,\
                                       Bits 10-19 Trailing edge,\
                                       Bits 20-30 Channel no.')
        ;

 END ESET

 END SUBSCHEMA

 SUBSCHEMA ItcGalephBanks

 AUTHOR   'R. Beuselinck'
 REVIEWER 'J.K.Sedgbeer'
 VERSION  '3.5'
 DATE     '30/04/90'

 DEFINE ESET

 ITDR
     :      'Itc Track(kine) to Digit Relation (NR=0). (GAL) \
             Number of words per track\
             Number of tracks'
          STATIC

     = (TrackNumber     = INTE [*,*]   : 'Number of KINE track (with digits).\
                                          Note that Geant Stack tracks have\
                                          negative track number.',
        NumberofDigits  = INTE [1,960] : 'Number of digitisings listed in ITDL',
        RowNumber       = INTE [1,960] : 'Pointer to start of list in ITDL')
     ;

 ITDL
     :      'Itc Track(Kine) to Digit List (NR=0).
  Ordering for each track is by hit ordering,
 i.e. by increasing path length along track.
 Digitisings are only counted for a track if that track
 causes the signal at the A-end (+Z) of the wire. If a different
 track causes the B-end (-Z) signal then it does not count as a
 digitising for the second track. (GAL) \
             Number of words per digit\
             Number of digits'
          STATIC

     = (DigitNumber = INTE [1,960] : 'Number of ITC digit associated to track.')
     ;

 IDHR
     :      'Itc Digit to Hit Relation (NR=0). (GAL) \
             Number of words per digit\
             Number of digits'
          STATIC

     = (HitA  = INTE [0,*]  : 'Hit giving signal at +Z end of wire',
        HitB  = INTE [0,*]  : 'Hit giving signal at -Z end of wire')
     ;


 ITWP
     :      'ITc Wire Pulse bank (NR=0). (GAL) \
             Number of words per pulse\
             Number of pulses'
          STATIC

     = (HitNumber    = INTE  [0,*]        : 'hit ID. Number of corresponding hit in\
                                             bank IHIT',
        WireNumber   = INTE  [1,960]      : 'wire number ( 1-960 )',
        TimeInduced  = REAL  [.00,*]      : 'time of induced pulse at wire ( tof\
                                             + drift time)  (ns)')
     ;

 ITFN
     :      'ITc FroNt end signals (NR=0).
  The data is ordered as for ITWP. For each pulse in ITWP
 results are stored for +Z and -Z consecutively, i.e. the
 arrival times are either end of a wire due to a single
 induced pulse are adjacent in ITFN and ITFP.
 This is a temporary bank used during the digitising stage. (GAL) \
             Number of words per signal\
             Number of signals'
          STATIC

     = (HitNumber    = INTE [0,*]         : 'hit ID. Number of corresp. hit in IHIT',
        WireNumber   = INTE [1,960]       : 'wire number ( 1-960 )',
        EndIdent     = INTE -1|1          : 'end identifier (1=+z, -1=-z )',
        TimePulse    = REAL [.00,*]       : 'time of pulse at end of the wire',
        NumPulse     = INTE [1,*]         : '# of pulse height values stored as shape\
                                             description in ITFP ( =1 )',
        ZeroAddress  = INTE [0,*]         : 'zero address of first pulse height data\
                                             in ITFP')
     ;


 ITFP
     :      'ITc Front end Pulse shape (NR=0).
  The data consist of a series of pulse heights, each of
 specified duration which may be used to describe an arbitrary
 profile at the input to the preamps. This is NOT used in the
 present version but is reserved for possible detailed
 simulation of the signal propagation effects.
 This is a temporary bank used during the digitising stage. (GAL) \
             Number of words per pulse height\
             Number of pulse heights'
        STATIC

     = (PulseHeight  = REAL  [0.0,*]      : 'pulse height',
        PulseLength  = REAL  [0.0,*]      : 'pulse length')
     ;


 ITHT
     :      'ITc HiTs (NR=0). The hits for each track
             are in order of increasing path length. (GAL)
             This bank is only used until GALEPH 23.7.
             After that it is replaced by IHIT. \
             Number of words/hit\
             Number of hits'
          STATIC

     = (TrackNumber  = INTE [*,*]         : 'Geant(Kine) track number.\
                                             Note that Geant stack tracks \
                                             have negative track numbers.',
        LayerNumber  = INTE [1,8]         : 'layer number in ITC ( 1-8 )',
        WireNumber   = INTE [1,960]       : 'unique wire number ( 1-960 )',
        PhiInter     = REAL [0.,6.30000]  : 'Phi of intersection with wire cylinder',
        ZInter       = REAL [-110.0,110.] : 'Z of intersection with wire cylinder',
        dPhidR       = REAL [*,*]         : 'dPhi/dR, change of phi with radius',
        dZdR         = REAL [*,*]         : 'dZ/dR, change of Z with radius',
        PhiDiff      = REAL [-.1,.10000]  : 'Phi(hit)-Phi(closest wire) signed value',
        TimeFlight   = REAL [.000,*]      : 'Time of flight to intersection(ns.)')
     ;


 IHIT
     :      'Itc HITs (NR=0). The hits for each track
             are in order of increasing path length. (GAL)
             This bank replaces ITHT which is obsolete. \
             Number of words/hit\
             Number of hits'
          STATIC

     = (TrackNumber   = INTE [*,*]         : 'Geant(Kine) track number.\
                                              Note that Geant stack tracks \
                                              have negative track numbers.',
        LayerNumber   = INTE [1,8]         : 'layer number in ITC ( 1-8 )',
        WireNumber    = INTE [1,960]       : 'unique wire number ( 1-960 )',
        DriftDistance = REAL [0.,2.0000]   : 'distance of closest approach to wire',
        DriftAngle    = REAL [0.,6.30000]  : 'angle of perpendicular from sense wire\
                                              to track element, measured from the\
                                              tangent to the wire cylinder.\
                                              Coincides with global phi angle for\
                                              wires on positive Y axis.',
        ZHit          = REAL [-110.0,110.] : 'Z coordinate at closest approach to wire.',
        ThetaHit      = REAL [0.,6.300000] : 'dip angle of track at hit point.',
        TimeFlight    = REAL [.000,*]      : 'Time of flight to hit point (ns.)')
     ;


 ITTR
     :      'ITc TRigger (NR=0).
   The 72 latch bits correspond to the trigger segments
 matched to the calorimeters. Latch bit 1 is stored in the
 least significant bit of word 1 and latch bit 32 in the most
 significant bit. Words 2 and 3 are filled similarly. Latch
 bit 1 corresponds to the 1st phi bin of the +Z endcap. Latch
 bit 72 corresponds to the last phi bin of the -Z endcap. The
 phi index varies fastest. (GAL) \
             Number of words\
             = 1'
          STATIC

     = (Latch1       = INTE [*,*]         : 'trigger latch bits 1 to 32',
        Latch2       = INTE [*,*]         : 'trigger latch bits 33 to 64',
        Latch3       = INTE [*,*]         : 'trigger latch bits 65 to 72',
        SpecTrig     = INTE [*,*]         : '32 bits reserved for special triggers')
     ;

 END ESET

 END SUBSCHEMA


 SUBSCHEMA ItcRunRecord

     :'ITC Raw Data Run Record. ITC calibration constants 
       (for front-end and trigger) which may be present in 
       some Run records.'

 AUTHOR    'M.Cattaneo'
 REVIEWER  'J.Sedgbeer'
 VERSION   '1.5'
 DATE      '11/02/92'

 DEFINE ESET

 IAUT
    : 'Current TDC Autotrim constants. For 1989 and 1990 data only.(RUN)'

        SIZE 960,960
        = ( AutotrimConst = INTE [0,*]    : 'Autotrim Constants')
        ;

 ICAL
    : 'ITC Calibration constants. Row 1 for r-phi, row 2 for Z.
       This bank replaces IAUT and IZCA from 1991 onwards.(RUN)'

        SIZE 2,2
        = ( TIme          = INTE[0,*]:'Time of calib (seconds since 14/08/89)',
            VErsion       = INTE[0,*]:'Serial number of calibration',
            CalConst(960) = INTE[*,*]:'Calibration constant per wire' )
        ;

 ICTL
    : 'ITC trigger board control words. In 1989,1990,1991 data only.(RUN) \
         Number of words/row.\
         Number of rows'
          STATIC

        = ( ProcId        = INTE [0,1]    : 'Processor ID (RPP or SPP)',
            ClustWord     = INTE [0,511]  : 'Clustering word (integral boards)',
            BbSpread      = INTE [0,7]    : 'Back to back spread',
            HitThresh(4)  = INTE [*,*]    : 'Hit thresholds',
            TrackThresh   = INTE [0,255]  : 'Track threshold')
        ;

 IHDQ
    : 'ITC Hardware Quality information. Available in 1989 and 1990
       data only. Replaced by IHOC bank. (RUN)'

        SIZE 34,34
        = ( HardwareQuality  = INTE [*,*]    : 'Hardware quality info.')
        ;

 IHOC
    : 'Itc Hardware Online Cuts. Available from 1991 onwards. 
       Replaces IHDQ bank. (RUN).\
           Number of words per row \
           Number of rows ( = 1 ) '

        SIZE 1,1
 = ( AutoMean    = INTE : 'Expected mean channel of Autotrim',
     AutomeanTol = INTE : 'Max. deviation of measured mean from expected mean',
     AutoWiretol = INTE : 'Max. deviation of   single wire from measured mean',
     AutomaxOut  = INTE : 'Max. number of r-phi channels out of tolerance',
     AutomaxDead = INTE : 'Max. number of dead r-phi channels',
     ZMeanzero   = INTE : 'Expected Z=0  channel',
     ZtolZero    = INTE : 'Max. deviation of measured Z=0 from expected Z=0',
     ZWirezero   = INTE : 'Max. deviation of  single wire from measured Z=0',
     ZwireTol    = INTE : 'Max. variance of a single Z channel',
     ZmaxBad     = INTE : 'Max. number of bad Z channels',
     RppmaxDead  = INTE : 'Max. number of dead  RPP channels',
     RppmaxNoisy = INTE : 'Max. number of noisy RPP channels',
     RppEffrate  = INTE : 'Min. efficiency of "live" RPP channel (%)',
     RppnoisRate = INTE : 'Max. noise rate of "non-noisy" RPP channel (%)',
     RppreserVed = INTE : 'RPP cut Reserved',
     rPhiDead    = INTE : 'Number of dead r-phi channels',
     rPhiNotauto = INTE : 'Number of r-phi channels not autotrimmed',
     rPhiMean    = INTE : 'Measured mean autotrim value',
     rPhiRes(2)  = INTE : 'R-phi Reserved',
     ZmeaN       = INTE : 'Measured mean Z=0 channel',
     ZDead       = INTE : 'Number of dead Z channels',
     ZRes(3)     = INTE : 'Z Reserved',
     RPpdead     = INTE : 'Number of dead RPP channels',
     RppnoisY    = INTE : 'Number of noisy RPP channels',
     RppreS(3)   = INTE : 'RPP measurement Reserved',
     HTcurr(4)   = REAL : 'HT base currents' )
    ;

 ITRM
    : 'Current trigger mask definition words. In 1989,1990,1991 data only (RUN)'

        SIZE 289,289
        = ( TriggerMask   = INTE  [*,*]   : 'Trigger mask definition word')
        ;


 IZCA
    : 'Latest Z calibration constants. For 1989 and 1990 data only.(RUN)' 

        SIZE 960,960

        = ( DacContents   = INTE [*,*]    : 'Contents of Az board DACs')
        ;


 END ESET

 END SUBSCHEMA


 SUBSCHEMA ItcSlowControl

     :'ITC Slow Control Record. Some or all of these banks may
       appear in Slow control records interspersed between 
       the event records. There are 4 different bank names but the
       content is essentially the same for all of them. Each bank
       consists of a standard ALEPH header followed by a variable
       number of information blocks, one block per group of hardware.
       The number of information blocks appearing in each bank is not
       fixed, neither is the order in which they appear. Due to the
       structure of the banks the ALEPH 2 word header defines all the
       banks to have only one column. The number of rows is the sum
       of the sizes of all blocks present. The content of the blocks
       is described elsewhere.' 

 AUTHOR    'R.Beuselinck'
 REVIEWER  'J.Sedgbeer'
 VERSION   '1.0'
 DATE      '11/10/89'

 DEFINE ESET

 ISSR
    : 'Itc Slow control Start of Run bank. Contains all information
       blocks of interest.\
       Number of words/row  \
       Number of rows (variable).'

        STATIC
        = ( SlowInfo      = INTE [*,*]    : 'Slow control info.') 
        ;

 ISCI
    : 'Itc Slow Control Info bank. Written at fixed time intervals.
       Identical to ISSR.\
       Number of words/row  \
       Number of rows (variable).'

        STATIC
        = ( SlowInfo      = INTE [*,*]    : 'Slow control info.') 
        ;

 ISCE
    : 'Itc Slow Control Error bank. Written when warning or alarm
       occurs - only relevant blocks are output.\
       Number of words/row \
       Number of rows (variable).'

        STATIC
        = ( SlowError     = INTE [*,*]    : 'Slow control info.') 
        ;

 ISEE
    : 'Itc Slow control End of Error bank. Written when warning or alarm
       is cleared. As ISCE.\
       Number of words/row \
       Number of rows (variable).'

        STATIC
        = ( SlowInfo       = INTE [*,*]    : 'Slow control info.') 
        ;


 END ESET

 END SUBSCHEMA
