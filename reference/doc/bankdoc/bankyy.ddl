 SUBSCHEMA YvertexJULBanks
 : 'Description of VERTEX fit output banks'
 
 AUTHOR   'F. James'
 REVIEWER 'D.Casper'
 VERSION  '2.1'
 DATE     '08/04/97'
 
 DEFINE ESET
 
 YVXL
      :      'VerteX fit List. 
              NR=0. (JUL)\
              Number of words/vertex\
              Number of vertices'
           STATIC
 
      = (VertexTyp  = INTE [0,3]         : 'Type 0= Main vertex,\
                                              -    1= neutral decay,\
                                              -    2= charged decay or interaction,\
                                              -    3= unknown',
         XPosition  = REAL  [-180.,180.] : 'Fitted vertex position',
         YPosition  = REAL  [-180.,180.] : 'Fitted vertex position',
         ZPosition  = REAL  [-300.,300.] : 'Fitted vertex position',
         ErrMat(6)   = REAL  [*,*]       : 'Triangular inverse covariance matrix stored in the order:\
                                                      1  2  4\
                                                      -  3  5\
                                                      -  -  6',     
         CHisquare   = REAL [0.,*]       : 'Chisquare of vertex fit',
         DegreesFr  = INTE [1,64]        : 'Number of DF for vertex fit',
         OFfsettrks = INTE [0,64]        : 'Offset of first track in YTRL',
         NumberofTrks=INTE [1,64]        : 'Number of outgoing charged tracks')
      ;

 YTRL
      :      'vertex-fitted TRack List.
              NR=0. (JUL) \
              Number of words/track\
              Number of tracks'
           STATIC
 
      = (InverseRad = REAL [*,*]         : 'Inverse radius of curvature in x-y projection\
                                              signed +ve if track bends clockwise\
                                                     -ve if tracks bends )',
         TanLambda   = REAL  [*,*]        : 'tangent of dip angle',
         Phi0        = REAL  [0.,6.3]     : 'Phi at point of closest approach to x=y=0',
         D0          = REAL  [-180.,180.]  : 'Distance of closest approach to x=y=0',
         Z0         = REAL  [0.,6.3]     : 'Z coordinate at point D0',
         ErrMat(6)   = REAL  [*,*]        : 'Triangular inverse covariance matrix stored in the order:\
                                                      1  2  4\
                                                      -  3  5\
                                                      -  -  6',
         VertexNumbr= INTE [1,64]        : 'Number of the row in YVXL which
                                              identifies the vertex to which
                                              this refitted track belongs',
         TrackNumbr = INTE [1,256]       : 'Number of the row in TGFT which
                                              identifies the original track
                                              refitted to yield these values')  
      ;  
  
 YNVH
      :      'Nuclear interactions tracks for Vdet Hit preselection. 
              NR=0. (JUL)\
              Number of words/track\
              Number of tracks'
           STATIC
 
      = (TrackNumber    = INTE  [1,*] : 'Track number in FRFT (if positive)\
                                         or V0 number in YLV0 (if negative)',
         VertexNumber   = INTE  [1,*] : 'Vertex number in YNLI\
                                         If negative, it means this track is\
                                         *incoming* to the vertex (usually an\
                                         ITC-only track), otherwise the track\
                                         is outgoing from the vertex')
      ;

 YNLI
      :      'Nuclear interaction vertex LIst. 
              NR=0. (JUL)\
              Number of words/vertex\
              Number of vertices'
           STATIC
 
      = (XPosition  = REAL  [-180.,180.] : 'Fitted vertex x position',
         YPosition  = REAL  [-180.,180.] : 'Fitted vertex y position',
         ZPosition  = REAL  [-300.,300.] : 'Fitted vertex z position',
         CHisquare   = REAL [0.,*]       : 'Chisquare of vertex fit')
      ;

 
 END ESET
 
 END SUBSCHEMA


 SUBSCHEMA YvertexPOTBanks
 : 'Description of vertex banks for  POT'
 
 AUTHOR   'G.Lutz'
 VERSION  '1.4'
 DATE     '05/05/93'
 
 DEFINE ESET
 
 
  PYER
      :      'Vertex position.
              NR=0. (POT)\
              Number of words/vertex\
              Number of vertices'
           STATIC
 
      = (TYpe         = INTE [0,255]       : ' Type of vertex:\
                                              =1 if it is the main vertex\
                                              =2 if V0\
                                              =3 if main vtx from 2 trks + beam spot (Bhabha)\
                                              =4 if photon conversion\
                                              =5 if main vtx from beam spot and 2nd step only\
                                              >5 not yet defined',
         VX           = REAL  [*,*]        : 'X coordinate',
         VY           = REAL  [*,*]        : 'Y coordinate',
         VZ           = REAL  [*,*]        : 'Z coordinate',
         VM(6)        = REAL  [*,*]        : 'Triangular covariance matrix stored in the order:\
                                                      1  2  4\
                                                      -  3  5\
                                                      -  -  6',
         Chis2        = REAL [0.,255.]     : 'Chisquare of vertex fit',
         numDegFree   = INTE [0,255]       : 'Numer of degr. of freedom')
      ;
 
 
  PYFR
      :     'Eset to hold n:m relation between vertices and FRFT
             as well as YNFT tracks.\
             Number of words per relation \
             Number of relations '
           STATIC
      ;
 
 
  PYNE
      :     'Eset to hold n:m relation between vertices and neutral tracks.\
             Number of words per relation \
             Number of relations '
           STATIC
      ;
 
  PYCH
      :     'Eset to hold n:m relation between vertices and those
             charged tracks not contained in FRFT.\
             Number of words per relation \
             Number of relations '
           STATIC
      ;
 
 
  PNEU
      :      'Fitted parameters for neutral tracks(NR=0)\
              Number of words/ neutral track\
              Number of neutral tracks'
           STATIC
 
      = (PX           = REAL [*,*]         : 'Momentum in x direction',
         PY           = REAL [*,*]         : 'Momentum in y direction',
         PZ           = REAL [*,*]         : 'Momentum in z direction',
         PM(6)        = REAL [*,*]         : 'Triangular covariance matrix stored in the order:\
                                                      1  2  4\
                                                      -  3  5\
                                                      -  -  6',
         MasS         = REAL [0,*]         : 'Fitted mass for this neutral',
         EMass        = REAL [0,*]         : 'error on the mass')
      ;
 
  PCHA
      :      'Reconstructed charged tracks not contained in FRFT (NR=0)\
              Number of words/ charged track\
              Number of chaged tracks'
           STATIC
 
      = (PX           = REAL [*,*]         : 'Momentum in x direction',
         PY           = REAL [*,*]         : 'Momentum in y direction',
         PZ           = REAL [*,*]         : 'Momentum in z direction',
         PM(6)        = REAL [*,*]         : 'Triangular covariance matrix stored in the order:\
                                                      1  2  4\
                                                      -  3  5\
                                                      -  -  6',
         MasS         = REAL [0,*]         : 'Fitted mass for this charged',
         EMass        = REAL [0,*]         : 'error on the mass')
      ;
 
 
 END ESET
 
 
  DEFINE RSET
 
    (PYFR [1,1] -> [0,*] FRFT  BY TrackNum) : '1st part of n:m rel
          (+ 10000 if referring to YNFT track) ';
    (PYFR [1,1] -> [0,*] PYER  BY VertexNum): '2nd part of n:m rel';
 
    (PYNE [1,1] -> [0,*] PNEU  BY TrackNum) : '1st part of n:m rel';
    (PYNE [1,1] -> [0,*] PYER  BY VertexNum): '2nd part of n:m rel';
 
    (PYCH [1,1] -> [0,*] PCHA  BY TrackNum) : '1st part of n:m rel';
    (PYCH [1,1] -> [0,*] PYER  BY VertexNum): '2nd part of n:m rel';
 
  END RSET
 
 END SUBSCHEMA
 
 
 SUBSCHEMA YtopJULPOTBanks
 : 'Description of parameters and banks used in YTOPOL package'
 
 AUTHOR   'M. Bosman,G.Lutz'
 VERSION  '2.7'
 DATE     '06/04/93'
 
 
 DEFINE ESET
 
 
 
 PBCR
      :      'Beam crossing region : centroid and shape'
 
      = (ValRng(2),
         ValX         = REAL  [*,*]           : 'beam crossing position in x',
         ValY         = REAL  [*,*]           : 'beam crossing position in y',
         ValZ         = REAL  [*,*]           : 'beam crossing position in z',
         EcovarM(6)   = REAL  [*,*]           : 'covariance matrix stored in the order:\
                                                           1  2  4\
                                                           .  3  5\
                                                           .  .  6')
      ;
 
 
  YNFT
      :      'Reconstructed mother tracks
              NR=0.\
              Number of words/track\
              Number of tracks'
           STATIC
 
      = (MOmentum     = REAL  [*,*]        : ' particle momentum for neutral mothers \
                        inverse radius of curvature for charged',
         TanLambda    = REAL  [*,*]        : 'tangent of dip angle',
         Phi0         = REAL  [0.,6.3]     : 'Phi at closest point of approach to line x=y=0',
         D0           = REAL  [-180.,180.] : 'Closest distance of approach to line x=y=0\
                                              in the x-y plane (impact parameter)\
                                             (signed +ve if particle has a positive angular\
                                              momemtum around the origin, -ve otherwise)',
         Z0           = REAL  [-220.,220.] : 'Z-coordinate at D0',
         EcovarM(15)  = REAL  [*,*]        : 'Triangular covariance matrix stored in the order:\
                                                      1  2  4  7 11\
                                                         3  5  8 12\
                                                            6  9 13\
                                                              10 14\
                                                                 15',
         Chis2        = REAL  [0.,*]       : 'Chisquare of vertex fit',
         numDegFree   = INTE  [0,23]       : 'Number of degr. of freedom/
                                              2*ntr - 3',
         CHarge       = INTE  [-2,2]       : 'Particle charge',
         NumDaughter  = INTE  [0,10]       : 'Number of daughter tracks',
         PointTrack   = INTE  [0,*]        : 'Pointer to 1rst daughter track',
         NumMass      = INTE  [0,*]        : 'Number of mass assignments',
         PointMass    = INTE  [0,*]        : 'Pointer to 1rst mass',
         PointVertex  = INTE  [0,*]        : 'Pointer to the vertex bank',
         PointChis2   = REAL  [0.,*]       : 'Pointing chisq to origin vertex')
      ;
 
 
  YCFT
      :      'Reconstructed charged mother tracks
              NR=0.\
              Number of words/track\
              Number of tracks'
           STATIC
 
      = (InvRadius    = REAL  [*,*]        : 'Inverse radius of curvature in x-y projection\
                                              signed +ve if track bends counterclockwise\
                                                     -ve if tracks bends clockwise',
         TanLambda    = REAL  [*,*]        : 'tangent of dip angle',
         Phi0         = REAL  [0.,6.3]     : 'Phi at closest point of approach to line x=y=0',
         D0           = REAL  [-180.,180.] : 'Closest distance of approach to line x=y=0\
                                              in the x-y plane (impact parameter)\
                                             (signed +ve if particle has a positive angular\
                                              momemtum around the origin, -ve otherwise)',
         Z0           = REAL  [-220.,220.] : 'Z-coordinate at D0',
         EcovarM(15)  = REAL  [*,*]        : 'Triangular covariance matrix stored in the order:\
                                                      1  2  4  7 11\
                                                         3  5  8 12\
                                                            6  9 13\
                                                              10 14\
                                                                 15',
         Chis2        = REAL  [0.,*]       : 'Chisquare of fit',
         numDegFree   = INTE  [0,23]       : 'Number of degr. of freedom/
                                              3 for vertex + 2*3 per track',
         CHarge       = INTE  [-2,2]       : 'Particle charge',
         NumDaughter  = INTE  [0,10]       : 'Number of daughter tracks',
         PointTrack   = INTE  [0,*]        : 'Pointer to 1rst daughter track',
         NumMass      = INTE  [0,*]        : 'Number of mass assignments',
         PointMass    = INTE  [0,*]        : 'Pointer to 1rst mass',
         PointVertex  = INTE  [0,*]        : 'Pointer to the vertex bank',
         PointChis2   = REAL  [0.,*]       : 'Pointing chisq to origin vertex')
      ;
 
  YNMA
      :      'Mass assignment of reconstructed tracks
              NR=0.\
              Number of words/mass assignment\
              Number of mass assignments'
           STATIC
 
      = (MotherTrack      = INTE  [0,*]  : 'Mother track number',
         PartAss          = INTE  [0,*]  : 'Particle Assignment',
         MAss             = REAL  [0.,*] : 'Mass (GeV)',
         ErrorMass        = REAL  [0.,*] : 'Error on the mass',
         VertexDistance   = REAL [0.0,*] : 'Distance prod.-dec. vtx',
         VertexSeparation = REAL [0.0,*] : 'Prod.-dec. vtx separation chisq',
         ImpactChisq      = REAL [0.0,*] : 'Chisq dist. mother track from prod.vx',
         NumbAddtracks    = INTE [0,*]   : '# of add.trks in dec.vtx',
         PRobability(2)   = REAL [0.0,*] : 'Mass ass. prob. for pos. and neg.\
                                            dec. trks')
      ;
 
  YCMA
      :      'Mass assignment of rec.charged tracks
              NR=0.\
              Number of words/mass assignment\
              Number of mass assignments'
           STATIC
 
      = (MotherTrack  = INTE  [0,*]        : 'Mother track number',
         PartAss      = INTE  [0,*]        : 'Particle Assignment',
         MAss         = REAL  [0.,*]       : 'Mass (GeV)',
         ErrorMass    = REAL  [0.,*]       : 'Error on the mass')
      ;
 
 
  YNTR
      :      'Daughter tracks of reconstructed tracks
              NR=0.\
              Number of words/tracks\
              Number of tracks'
           STATIC
 
      = (MotherTrack  = INTE  [0,*]        : 'Mother track number',
         DaughterTr   = INTE  [0,*]        : 'Daughter track number',
         dTrkType     = INTE  [0,*]        : 'Daughter track type\
                                              1 - normal track       \
                                              2 - rec. neutral track \
                                              3 - rec. charged track ',
         Mass         = INTE  [0,*]       : 'Mass assigned to daughter',
         BitPattern   = INTE  [0,*]       : 'VDET bitpattern',
         IMpact       = REAL  [*,*]       : 'impact par.to beam spot',
         PartId       = REAL  [0.,*]      : 'Particle ID prob for assign mass')
      ;
 
 
  YCTR
      :      'Daughter tracks of rec. charged tracks
              NR=0.\
              Number of words/tracks\
              Number of tracks'
           STATIC
 
      = (MotherTrack  = INTE  [0,*]        : 'Mother track number',
         DaughterTr   = INTE  [0,*]        : 'Daughter track number',
         dTrkType     = INTE  [0,*]        : 'Daughter track type\
                                              1 - normal track       \
                                              2 - rec. neutral track \
                                              3 - rec. charged track ',
         Mass         = INTE  [0,*]       : 'Mass assigned to daughter')
      ;
 
 
  YCPE
      :      '3-momentum of rec. charged tracks
              NR=0.\
              Number of words/tracks\
              Number of tracks'
           STATIC
 
      = (PX           = REAL  [*,*]        : 'X-proj. of rec. momentum',
         PY           = REAL  [*,*]        : 'Y-proj. of rec. momentum',
         PZ           = REAL  [*,*]        : 'Z-proj. of rec. momentum')
      ;
 
 
  YNPE
      :      '3-momentum of reconstructed tracks
              NR=0.\
              Number of words/tracks\
              Number of tracks'
           STATIC
 
      = (PX           = REAL  [*,*]        : 'X-proj. of rec. momentum',
         PY           = REAL  [*,*]        : 'Y-proj. of rec. momentum',
         PZ           = REAL  [*,*]        : 'Z-proj. of rec. momentum')
      ;
 
 
 END ESET
 
 END SUBSCHEMA

 
 SUBSCHEMA YtrackDALIBanks
 : 'Description of neutral or charged track to secondary vertex calculated in DALI'
 
 AUTHOR   'H.Drevermann,H.G.Moser,G.Waltermann '
 VERSION  '1.0'
 DATE     '04/08/93'
 
 DEFINE ESET
 
  CRFT
      :      'Charged Track to secondary vertex
              NR=0 \
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
         RowPyer      = INTE  [*,*]        : 'absolute row# in PYER bank\
                                              (=correlated secondary vertex) ',
         EcovarM(15)  = REAL  [*,*]        : 'Triangular covariance matrix stored in the order:\
                                                      1\
                                                      2  3\
                                                      4  5  6\
                                                      7  8  9 10\
                                                     11 12 13 14 15',
         NulL(9)      = REAL [0.,0.]         : 'set to ZERO\
                                                to be compatible with FRFTbank')
      ;
 


  NRFT
      :      'Neutral Track to secondary vertex
              NR=0 \
              Number of words/track\
              Number of tracks'
           STATIC

      = (PNeutral     = REAL [*,*]         : 'momentum of neutral track',
         TanLambda    = REAL  [*,*]        : 'tangent of dip angle',
         Phi0         = REAL  [0.,6.3]     : 'Phi at closest point of approach to line x=y=0',
         D0           = REAL  [-180.,180.] : 'Closest distance of approach to line x=y=0\
                                              in the x-y plane (impact parameter)\
                                             (signed +ve if particle has a positive angular\
                                              momemtum around the origin, -ve otherwise)',
         Z0           = REAL  [-220.,220.] : 'Z-coordinate at D0',
         RowPyer      = INTE  [*,*]        : 'absolute row# in PYER bank\
                                              (=correlated secondary vertex) ',
         EcovarM(15)  = REAL  [*,*]        : 'Triangular covariance matrix stored in the order:\
                                                      1\
                                                      2  3\
                                                      4  5  6\
                                                      7  8  9 10\
                                                     11 12 13 14 15',
         NulL(9)      = REAL [0.,0.]         : 'set to ZERO\
                                                to be compatible with FRFTbank')
      ;

 
 
 
 END ESET
 
 END SUBSCHEMA


 SUBSCHEMA YvsJULBanks
 : 'Description of Secondary VERTEX fit output banks'
 
 AUTHOR   'D.Casper'
 REVIEWER 'D.Casper'
 VERSION  '1.5'
 DATE     '18/08/97'
 
 DEFINE ESET
 
 YSTL
      :      'Secondary vertex track list \
              Number of words per track \
              Number of tracks'
            STATIC
      = (QualityFlg  = INTE [*,*]       : 'Informational Flag',
         TrackNum    = INTE [1,*]       : 'FRFT track number',
         Helix0(5)   = REAL [*,*]       : 'Helix parameters at origin\
                                           (as in FRFT/2)',
         Error0(15)  = REAL [*,*]       : 'Triangular covariance matrix\
                                           at origin (as in FRFT/2)',
         Chisq0      = REAL [*,*]       : 'Chi**2 of helix fit to origin',
         Numdegfree0 = INTE [0,63]      : 'Number of degrees of freedom\
                                           including all points to origin',
         Helix1(5)   = REAL [*,*]       : 'Helix parameters in ITC\
                                           (as in FRFT/0)',
         Error1(15)  = REAL [*,*]       : 'Triangular covariance matrix\
                                           in ITC (as in FRFT/0)',
         Chisq1      = REAL [*,*]       : 'Chi**2 of helix fit into ITC',
         Numdegfree1 = INTE [0,63]      : 'Number of degrees of freedom\
                                            including all points to ITC',
         Helix2(5)   = REAL [*,*]       : 'Helix parameters in TPC\
                                           (as in TGFT)',
         Error2(15)  = REAL [*,*]       : 'Triangular covariance matrix\
                                           in TPC (as in TGFT/2)',
         Chisq2      = REAL [*,*]       : 'Chi**2 of helix fit in TPC',
         Numdegfree2 = INTE [0,63]      : 'Number of degrees of freedom\
                                           in TPC',
         RInner      = REAL [0.,180.]   : 'Radius of innermost coord',
         ROuter      = REAL [0.,180.]   : 'Radius of outermost coord',
         NVdetpts    = INTE [0,4]       : 'Number of VDET coordinates',
         NItcpts     = INTE [0,4]       : 'Number of ITC coordinates',
         NTpcpts     = INTE [0,21]      : 'Number of TPC coordinates',
         ChisqPrim   = REAL [*,*]       : 'Chi**2/DOF with primary vertex')
    ;

 YSCL
      :      'Secondary vertex ITC track list \
              Number of words per track \
              Number of tracks'
            STATIC
      = (QualityFlg  = INTE [*,*]       : 'Informational Flag',
         TrackNum    = INTE [1,*]       : 'FRFT track number',
         Helix0(5)   = REAL [*,*]       : 'Circle parameters at origin\
                                           (as in FRFT)',
         Error0(15)  = REAL [*,*]       : 'Triangular covariance matrix',
         Chisq0      = REAL [*,*]       : 'Chi**2 of circle fit to origin',
         Numdegfree0 = INTE [0,63]      : 'Number of degrees of freedom\
                                           including all points to origin',
         RInner      = REAL [0.,180.]   : 'Radius of innermost coord',
         ROuter      = REAL [0.,180.]   : 'Radius of outermost coord',
         NItcpts     = INTE [0,4]       : 'Number of ITC coordinates',
         ChisqPrim   = REAL [*,*]       : 'Chi**2/DOF with primary vertex')
    ;

 YS0L
      :      'Secondary vertex V0 List (parallel to YLV0)\
              Number of words/V0\
              Number of V0s'
           STATIC

      = (QualityFlg = INTE [*,*]           : 'V0 quality flag',
         K1         = INTE [1,*]           : 'Number of positive track',
         K2         = INTE [1,*]           : 'Number of negative track',
         VXcoordin  = REAL  [-180.0,180.0] : 'Fitted V0 x coordinate',
         VYcoordin  = REAL  [-180.0,180.0] : 'Fitted V0 y coordinate',
         VZcoordin  = REAL  [-220.0,220.0] : 'Fitted V0 z coordinate',
         VerrMat(6) = REAL  [*,*]          : 'Triangular   covariance  matrix\
                                              of V0s coordinates stored in the\
                                              order:\
                                                      1  2  4\
                                                      -  3  5\
                                                      -  -  6',     
         PXmomentum = REAL  [*,*]          : 'Fitted V0 momentum px',
         PYmomentum = REAL  [*,*]          : 'Fitted V0 momentum py',
         PZmomentum = REAL  [*,*]          : 'Fitted V0 mometum  pz',
         PerrMat(6) = REAL  [*,*]          : 'Triangular  covariance matrix\
                                              of V0s momentum stored\
                                              in the order:\
                                                      1  2  4\
                                                      -  3  5\
                                                      -  -  6',  
         PmomVtx(9) = REAL  [*,*]          : 'Correlation matrix between\
                                              momentum and vertex, in order\
                                                VX.PX  VY.PX  VZ.PX \
                                                VX.PY  VY.PY  VZ.PY \
                                                VX.PZ  VY.PZ  VZ.PZ',
         Helixv0(5) = REAL  [*,*]          : 'Pseudo-helix parameters\
                                              for V0',
         Errorv0(15)= REAL  [*,*]          : 'Error matrix for pseudo-helix',
         C2chisquare= REAL [0.000,*]       : 'Chisquare of V0 vertex fit',
         ChisqPrim  = REAL [*,*]           : 'Chi**2/DOF with primary vertex')
      ;
        
 YSVX
      :     'Secondary vertex candidates (NR=multiplicity)\
             Number of vertices\
             Number of words/vertex'
          STATIC
      = (VXcoordin  = REAL  [-180.0,180.0] : 'Fitted V0 x coordinate',
         VYcoordin  = REAL  [-180.0,180.0] : 'Fitted V0 y coordinate',
         VZcoordin  = REAL  [-220.0,220.0] : 'Fitted V0 z coordinate',
         VerrMat(6) = REAL  [*,*]          : 'Triangular   covariance  matrix\
                                              of V0s coordinates stored in the\
                                              order:\
                                                      1  2  4\
                                                      -  3  5\
                                                      -  -  6',     
        Chi2        = REAL [*,*]           : 'Chi**2 of vertex fit',
        numDegFree  = INTE [*,*]           : 'Number degrees of freedom',
        OFfset      = INTE [0,*]           : 'First track of this vertex in\
                                              YSVT',
        TrackMsk(3) = INTE [*,*]          : 'Bit mask of tracks in vertex',
        CircleMsk   = INTE [*,*]          : 'Bit mask of circles in vertex',
        v0Mask      = INTE [*,*]          : 'Bit mask of V0s in vertex',
        PXmomentum = REAL  [*,*]          : 'Total momentum px',
        PYmomentum = REAL  [*,*]          : 'Total momentum py',
        PZmomentum = REAL  [*,*]          : 'Total momentum  pz',
        PerrMat(6) = REAL  [*,*]          : 'Triangular  covariance matrix\
                                              of momentum stored\
                                              in the order:\
                                                      1  2  4\
                                                      -  3  5\
                                                      -  -  6',  
        QUality    = INTE [*,*]           : 'Quality flag')
      ;

 YSTV
      :     'FRFT Track -> secondary vertex relation (NR=track)\
             Number of words/vertex\
             Number of vertices track is associated with'
          STATIC
      = (NumbeR     = INTE [0,*]           : 'Bank number of associated vertex\
                                              (=multiplicity)',
         VertexNum  = INTE [1,*]           : 'Row of vertex in YSVX bank')
      ;

 YSMO 

      :      'Secondary vertex outgoing track momenta (NR=row in YNLI) \
              Number of words per track \
              Number of tracks'
            STATIC
      = (TrackNum   = INTE [*,*]        : 'FRFT track number (if positive)\
                                           or YLV0 row (if negative); the\
                                           remainder of the row is valid\
                                           only if track number is positive',
         SenseCode  = INTE [-1,12]      : 'Flag indicating the direction\
                                           of the track:\
                                           0 = incoming\
                                           1 = outgoing\
                                           2 = outgoing but radially inward\
                                           3 = incoming after a loop\
                                           4 = throughgoing\
                                          -1 is an error or suspicious case',
         PsiInner   = REAL [*,*]        : 'Psi (turning angle) at the \
                                           innermost coordinate',
         PsiOuter   = REAL [*,*]        : 'Psi at the outermost coordinate',
         PsiSecndry = REAL [*,*]        : 'Psi at the secondary vertex',
         PrimChi2   = REAL [*,*]        : 'Goodness of fit with the primary\
                                           event vertex',
         InorOut    = INTE [-1,1]       : 'Flag for DALI, indicating which\
                                           radial direction (-1 = in,\
                                           +1 = out,0 = both) to draw the\
                                           track from the vertex.  This is\
                                           not equivalent to the track being\
                                           incoming or outgoing...',
         XPmom      = REAL [*,*]        : 'Track x momentum at secondary\
                                           vertex, taking into account the\
                                           best guess as to incoming/outgoing',
         YPmom      = REAL [*,*]        : 'Track y momentum at secondary \
                                           vertex',
         ZPmom      = REAL [*,*]        : 'Track z momentum at secondary\
                                           vertex')
      ;
                
 YSVT

      :      'Internal secondary vertex/track relation (NR=multiplicity) \
              Number of words per track \
              Number of tracks'
            STATIC
      = (TrackNum   = INTE [*,*]        : 'FRFT track number (if positive)\
                                           or YLV0 row (if negative); the\
                                           remainder of the row is valid\
                                           only if track number is positive',
         SenseCode  = INTE [-1,12]      : 'Flag indicating the direction\
                                           of the track:\
                                           0 = incoming\
                                           1 = outgoing\
                                           2 = outgoing but radially inward\
                                           3 = incoming after a loop\
                                           4 = throughgoing\
                                          -1 is an error or suspicious case',
         PsiInner   = REAL [*,*]        : 'Psi (turning angle) at the \
                                           innermost coordinate',
         PsiOuter   = REAL [*,*]        : 'Psi at the outermost coordinate',
         PsiSecndry = REAL [*,*]        : 'Psi at the secondary vertex',
         PrimChi2   = REAL [*,*]        : 'Goodness of fit with the primary\
                                           event vertex',
         InorOut    = INTE [-1,1]       : 'Flag for DALI, indicating which\
                                           radial direction (-1 = in,\
                                           +1 = out,0 = both) to draw the\
                                           track from the vertex.  This is\
                                           not equivalent to the track being\
                                           incoming or outgoing...',
         XPmom      = REAL [*,*]        : 'Track x momentum at secondary\
                                           vertex, taking into account the\
                                           best guess as to incoming/outgoing',
         YPmom      = REAL [*,*]        : 'Track y momentum at secondary \
                                           vertex',
         ZPmom      = REAL [*,*]        : 'Track z momentum at secondary\
                                           vertex')
      ;
                
 YSFT
      :     'FRFT -> {YSTL, YSCL} track relation -
             Positive: YSTL, Negative: YSCL\
             Number of words/track\
             Number of tracks'
          STATIC
      = (TrackNumber = INTE[*,*]        : 'Track row number in YSTL (positive)\
                                           or YSCL (negative)')
      ;

 END ESET
 
 END SUBSCHEMA
                
