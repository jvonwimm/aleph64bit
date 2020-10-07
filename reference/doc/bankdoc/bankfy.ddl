 SUBSCHEMA FXXX
 : 'Description of FXXX DST output banks'
 
 AUTHOR   'J. Hilgart,B.Bloch'
 REVIEWER 'F. Ranjard,B.Bloch'
 VERSION  '1.4'
 DATE     '980313'
 
 DEFINE ESET
 
 FKIN
      :      'Fxxx monte carlo track bank.
              All secondary tracks from   
              a vertex as specified by FVER are contiguous. \
              Number of words/track\
              Number of tracks'
           STATIC
 
      = (PX= REAL [*,*]         : 'Px',
         PY= REAL [*,*]         : 'Py',
         PZ= REAL [*,*]         : 'Pz',
         MAss = REAL [0.,*]     : 'mass',
         PArt = INTE [0,*]      : 'ALEPH particle code',
         OVrt = INTE [0,*]      : '(FVER) Origin vertex no.',
         EVrt = INTE [0,*]      : '(FVER) End vertex no.',
         HCode = INTE [0,*]     : 'History code (generator dependant)')
      ;

  
 FVER
      :      'Fxxx monte carlo VERtex bank.
              All secondary tracks from   
              a vertex as specified by FVER are listed contiguously
              in the FKIN bank. \
              Number of words/vertex\
              Number of vertices'
           STATIC
 
      = (VX = REAL [*,*]        : 'Vertex X position',
         VY = REAL [*,*]        : 'Vertex Y position',
         VZ = REAL [*,*]        : 'Vertex Z position',
         TOf = REAL [*,*]       : 'Time of flight',
         IPrim = INTE [0,*]     : 'FKIN track no. which produced this vertex',
         ISec = INTE [0,*]      : 'Offset to first FKIN outgoing track',
         NSec = INTE [0,*]      : 'no. of outgoing tracks',
         VolNam = CHA4          : 'vertex volume name',
         VertexMechanism = CHA4 : 'vertex mechanism name')
      ;  
  
  
 FSTR
      :      'Fxxx TRack bank\
              Number of words/track\
              Number of tracks'
           STATIC
 
      = (PXmin = REAL [*,*]     : 'Px at minimum distance of approach to beam',
         PYmin = REAL [*,*]     : 'Py at minimum distance of approach to beam',
         PZmin = REAL [*,*]     : 'Pz at minimum distance of approach to beam',
         IQ    = INTE [-2,2]    : 'Charge',
         IHyp  = INTE [0,8]     : 'Hypothesis number',
         CHi2  = REAL [0.000,*]    : 'chi squared from track fit',
         NHt   = INTE [0,*]     : 'Bits 0 - 30 are 1 (0) if there is \
                                   (not) a hit in the corresponding \
                                   layer in the tracking detectors',
         D0Tr  = REAL [-1000.,1000] : 'Distance of closest approach in r-phi\
                                      (signed), non-TASSO convention',
         PHi0 = REAL [-6.3,6.3] : 'phi angle of tangent to track at pt.o.c.a.',
         Z0   = REAL [-1000.,1000.] : 'z coordinate at pt.o.c.a.',
         AVio = REAL [*,*]      : 'average ionization from dE/dx',
         S2Io = REAL [0.,*]     : 'sigma**2 of ionization',
         NWio = INTE [0,*]      : 'number of wires in ionization measurement',
         Mchi2 = REAL [0.,*]    : 'Chi**2: track extrapolation to muon \
                                   chamber hit(s)',
         NMu  = INTE [0,2]      : 'no. of muon planes hit',
         NhCal = INTE [0,23]    : 'no. of HCAL strips hit (not yet filled)',
         MCpt = INTE [0,*]      : '1000*(no. of ambiguities)\
                                   + pointer to MCAD bank',
         ICal = INTE [0,*]    : 'no. of the CAL object this track points to')

      ;  
  
  
 FSCO
      :      'Fxxx Cal Object bank\
              Number of words/cal object\
              Number of cal objects'
           STATIC
 
      = (EN =   REAL [*,*] : 'Energy',
         IHyp = INTE [0,8]       : 'hypothesis number\
                                    0  no assignment\
                                    1  muon\
                                    2  e+/-\
                                    3  charged hadron\
                                    4  complex charged\
                                    5  neut. hadron\
                                    6  complex neut.\
                                    7  gamma\
                                    8  multi-gamma',
         E1   = REAL [*,*] : 'ECAL front stack energy',
         E2   = REAL [*,*] : 'ECAL middle stack energy',
         E3   = REAL [*,*] : 'ECAL back stack energy',
         H1   = REAL [*,*] : 'HCAL front stack energy',
         H2   = REAL [*,*] : 'HCAL back stack energy\
                                (currently H1 contains total HCAL en.)',
         THeta = REAL [0.,3.15]    : 'Theta of CAL object',
         PHi  = REAL [0.,6.3]    : 'Phi of CAL object\
                                    Theta and phi are avg. of the values in \
                                    the 3 ECAL stacks, weighted by energy',
         IPoi = INTE [0,*]        : 'Row index of 1st pointer in FTOC - 1\
                                     + 65536*(no. of ptrs in FTOC for\
                                     this CAL obj)')
      ;  
  
 FLTR
      :      'Fxxx List of offsets into table (bank) fttm for charged TRacks\
              Number of words/list item\
              Number of charged tracks'
           STATIC

     =   (ILis = INTE [0,*]    : 'Row index of 1st pointer in FTTM - 1\
                                  + 65536*(no. of pointers)')
      ;
 
 FLCO
      :      'Fxxx List of offsets into table (bank) ftcm for Cal Objects\
              Number of words/list item\
              Number of cal objects'
           STATIC

     =   (ILis = INTE [0,*]    : 'Row index of 1st pointer in FTCM - 1\
                                  + 65536*(no. of pointers)')
      ;
 
 FTOC
      :      'Fxxx Table relating cal Objects to Charged tracks\
              Number of columns/pointer\
              Number of pointers'
           STATIC

     =   (IPoi = INTE [0,*]    : 'pointer into FSTR\
                                  = row index of one charged track contributing\
                                    to a CAL object')
      ;
 
 FTTM
      :      'Fxxx Table relating charged Tracks to kinematics track\
              Number of columns/pointer\
              Number of pointers'
           STATIC

     =   (IPoi = INTE [0,*]    : 'pointer into FKIN\
                                  = row index of kinematics track contributing\
                                    to a charged track')
      ;
 
 FTCM
      :      'Fxxx Table relating Cal objects to kinematics track\
              Number of columns/pointer\
              Number of pointers'
           STATIC

     =   (IPoi = INTE [0,*]    : 'pointer into FKIN\
                                  = row index of kinematics track contributing\
                                    to a CAL object')
      ;
 
 FPOI
      :      'Fxxx POInters bank relating KINE track number to FKIN number\
              Number of words/KINE track\
              Number of KINE tracks'
           STATIC

     =   (IPoi = INTE [0,*]    : 'FKIN track no. of KINE track OR first\
                                  surviving predecessor of KINE track if\
                                  original track was not saved',
          ISaved = INTE [0,1]  : 'Flag = 0 if original KINE track was\ 
                                  saved, 1 otherwise')
      ;
 
FPOL
       :      'Fxxx POLarisation bank          (NR=0)\
               Number of words per polarized particle\
               Number of polarized particles   '
           STATIC                                                               
                                                                                
       = (KineIdent    = INTE [1,*]      : 'FKIN number of the track',
          HelictyX     = REAL [*,*]      : 'Helicity or polarization xcomponent',
          HelictyY     = REAL [*,*]      : 'Helicity or polarization ycomponent',
          HelictyZ     = REAL [*,*]      : 'Helicity or polarization zcomponent')
                                                                                
       ;                                                                        
 
FZFR
       :      'Fxxx ZFRagmentation bank        (NR=0)\
               Number of words per  particle\
               Number of  particles   '
           STATIC                                                               
                                                                                
       = (ZFragmentation = REAL [0.,1.0000]   : 'z fragmentation value')
                                                                                
       ;         

FSHO
       :      'Fxxx parton SHOwer bank          (NR=0)\                                                               
                number of words/shower\
                number of partons showers'
           STATIC

       = (KineIdent     = INTE[1,*]  : 'KINE number of shower',
          Kineiden1     = INTE[1,*]  : 'KINE number of first parton of shower',
          Kineiden2     = INTE[1,*]  : 'KINE number of last parton of shower',
          ColRecflag    = INTE[0,*]  : 'Color Reconnection Flag I=done in\
                                       scheme I, 0=notdone')

       ;
         
 END ESET
 
 END SUBSCHEMA
