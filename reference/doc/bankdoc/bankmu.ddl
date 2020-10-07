 SUBSCHEMA MuonRAWGALJULPOT
 
 AUTHOR   'G. Capon, G. Taylor'
 REVIEWER 'F.Loverre'
 VERSION  '4.0'
 DATE     '28/03/95'
 
 DEFINE ESET
 
 PHMA
      :      'Hcal Muon tracks Association Data (POT)\
              Number of words/associated track\
              Number of associated tracks'
           STATIC
 
 = (MultHits  = INTE [0,30]        : 'number of clusters in last\
                                     ten planes',
    IGeomflag = INTE [-1,6]         : 'flag of possible dead zone',
    EnerDep   = REAL [0.0,100.]            : 'energy deposit in nearest\
                                     Hcal storey',
    ChiSquare = REAL [0.00,200.00] : 'chisquare',
    NumbDeg   = INTE [0,23]        : 'number of degrees of freedom',
    IExpbmapInHcal  = INTE [0,11000000]  : 'expected bit map in Hcal',
    ITruebmapInHcal = INTE [0,11000000]  : 'observed bit map in Hcal',
    IdenFlag  = INTE [-1,1]        : 'preliminary identification flag\
                                      1=muon,0=not classif,-1=hadron',
    TrackNo   = INTE [1,999]       : 'index of associated track ')
      ;
 
 
 
 MUHT
      :      'MUon detector HiTs. 
              NR=0. (GAL) \
              Number of words / hit\
              Number of hits'
           STATIC
 
      = (TrackNum     = INTE [1,999]       : 'track # that generates the hit',
         Electronics  = INTE [1,99]        : 'electronics module #',
         StripPlane   = INTE [1,4]         : 'strip plane # in this electronics module',
         StripAddres  = INTE [0,700]       : 'strip address in this strip plane\
                                              ( start from 0 )')
      ;
 
 MUDI
      :      'MUon detector DIgits (DROP)
              ----------- W A R N I N G ------
              this bank will become obsolete.
              use bank MUDG. G. Capon july 89\ 
              Number of words / strips-cluster\
              Number of digits'               
           STATIC
 
      = (Electronics  = INTE [1,99]        : 'electronics module #',
         StripPlane   = INTE [1,4]         : 'strip plane # ',
         LengthClus   = INTE [0,15]        : 'length of cluster ',
         FirstAddres  = INTE [0,700]       : 'electr addr of first strip in the\
                                              cluster( start from 0 )')
      ;
 
 MUDG
      :      'MUon detector DIgits (RAW)\
              Number of words/digit (=stripscluster)\
              Number of digits               '
           STATIC
 
      = (EncodAddr    = INTE [1,*]         : 'byte4(most signif)=Astros #  \
                                              byte3=pseudolayer #     \
                                              byte2=cluster width     \
                                              byte1=addr of first strip in the\
                                              cluster( starts from 0 )')
      ;
 
 MCUT
      :      'Cuts used in muon-identification routines (JUL)'
            STATIC
 
 = (PcutMa   = REAL [0.00,10.]     : 'cutoff on track momentum for assoc\
                                       to muon chambers',
    PcutHa   = REAL [0.00,10.]     : 'cutoff on track momentum for assoc\
                                       to Hcal digital pattern',
    AccLay(2)= REAL [0.00,15.]     :'cutoff on the ratio (obs dist)/(expected\ 
                                     dist) for layer int=1/ext=2',
    AccAng   = REAL [0.00,15.]     :'cutoff for ratio (obs exit angle)/\
                                     (expected exit angle)',
    RoadWid  = REAL [0.00,15.]     : 'road width in Hcal (in sigma m.s. units)',
    TEtass   = REAL [0.00,45.]     :' min angle to associate a tower(degrees)',
    Nexnfo   = INTE [0,23]         : 'cutoff on (nb of expected planes -\
                                      nb of fired planes',
    NLas10   = INTE [0,10]         : 'cutoff on nb of fired planes in last 10',
    NMinfp   = INTE [0,23]         : 'cutoff on minimum nb of fired planes',
    WClumx   = REAL [0.,20.]       : 'max width of a tubes cluster')
      ;
                                      
 MCAD
      :      'Muon Chambers Association Data 
              (variables in columns 3-8 are set to zero 
              if the corresponding association test
              could not be done or was not passed) (JUL)\
              Number of words/associated track\
              Number of associated tracks'
           STATIC
 
 = (NassHit(2)= INTE [0,99]        : 'number of associated hits  \
                                      1,2 -> Int/Ext chambers',
    DistHit(2)= REAL [0.00,1000.]  : 'minimum distance hit-track',
    DistCut(2)= REAL [0.00,1000.]  : 'cutoff on hit-track distance',
    AngMin    = REAL [0.000,*]     : 'minimum angle between extrap\
                                     and measured (in muon ch) track',
    AngCut    = REAL [0.000,*]     : 'cutoff on mimimum angle',
    TrackNo   = INTE [1,999]       : 'associated track # ')
      ;
 
 
 MTHR
      :      'Muon - Track to Hit Relation (JUL)\
              Number of words/relation \
              Number of relations'
           STATIC
 
= (HitNumber= INTE  [1,*]       :'Hit number in bank MHIT',
   DistDcut = REAL  [0.,999.99] :'(hit-track distance)/(cutoff-distance)',
   TrackNum = INTE  [1,*]       :'Track number in bank FRFT')
     ;
 
 MHIT
      :      'Muon HIT data (JUL)\
              Number of words/hit\
              Number of reconstructed muon hits'
           STATIC
 
      = (SubComp      = INTE [1,3]         : 'subcomp number (1=endcap,\
                                              2=middleangle,3=barrel)',
         SlotNumber   = INTE [1,78]        : 'slot number (1-24 barrel,\
                                              25-62 middleangle,63-78 endcap)\
                                              new numbering if DAF ge 114 : \
                                              1-16 endcap, 1-38 middle angle\
                                              1-34 barrel',
         LayerNumber  = INTE [1,2]         : 'layer number (=1,2) in the slot',
         XLocalcoord  = REAL [-999.,999.]  : 'X local coordinate',
         YLocalcoord  = REAL [-999.,999.]  : 'Y  "       "',
         Rho          = REAL [400.,1200.]  : 'rho  -  in aleph main coordinate system',
         Theta        = REAL [0.,3.1416]   : 'theta              "              "',
         Phi          = REAL [0.,6.2832]   : 'phi                "               "')
      ;
 
 
 TREX
      :      'Extrapolated points for tpc tracks
              NR=track index. (JUL)\
              Number of words/point\
              Number of extrapolated points along the track'
           STATIC
 
      = (X3(3)        = REAL [-600.,600.]  : 'X,Y,Z coordinates (cm, in the MRS)',
         P3(3)        = REAL [-55.,55.]    : 'Px,Py,Pz',
         RMs          = REAL [0.,300.]     : 'rms proj lateral displacement
                                              due to mult. scattering. For first
                                              row only rms proj is replaced by a
                                              flag =
                                              1.  : extrapolation ok
                                              2.  : p(in) < pcut
                                              3.  : track spirals in Hcal
                                              4.  : E < 2.*muon-mass
                                              5.  : track parallel to slabs',
         ReGion       = INTE [0,3999]      : '100*Region number + module #
                                             region # = 0/10-19/20-29/30-39/
                                             for air/ecal/coil/hcal/',
         LAyer        = INTE [0,23]        : 'Hcal layer number')
      ;
 
 
 MUEX
      :     'MU chambers EXtrapol data for current track\
             Number of words/track\
             Number of extrapolated tracks'
          STATIC
 
      = (XExit(3) = REAL [-999.,999.] :'coordinates at exit point from Hcal  ',
         PExit(3) = REAL [-999.,999.] :'momenta x,y,z at exit point from Hcal',
         C0       = REAL [   0.,*   ] :'first coefficient for mult scattering',
         C1       = REAL [   0.,*   ] :'second coefficient for mult scattering',
         C2       = REAL [   0.,*   ] :'third coefficient for mult scattering')
 
 ;
 
 MUID
      :     'MUon IDdentification data for current track\
             Number of words/track\
             Number of extrapolated tracks'
          STATIC
 
      = (IdFlag      = INTE [ -20, 20  ] :'Identification Flag: 
                                           = 1 if muon flagged only by HCAL         
                                           = 2 if muon flagged only by MUON         
                                           = 3 if muon flagged by both HCAL and MUON     
                                                [3 is the .AND. of 1 and 2]              
                                           = 10 is one hit in each layer of MUON  
                                                but failing tight matching criteria   
                                           = 11 is good HCAL pattern                
                                           = 12 is one and only one MUON hit        
                                           = 13 is good HCAL + one and only one muon
                                           = 14 is good HCAL + one hit in each layer
                                           = 15 is one hit in each layer of MUON  
                                                passing tight matching criteria   
                                           = 0 not a muon                           
                                               -1 to -15 as above but 
                                                 lost shadowing contest',           
         SumResid    = REAL [   0.,999.] :'Sum of HCAL (hit -track) residuals',
         DistMuch    = REAL [   0.,*   ] :'distance between track and closest mu ch hit',
         ShadTrack   = INTE [    0,*   ] :'track number of shaoowing track (FRFT)',
         TrackNumber = INTE [    0,*   ] :'track number (FRFT)')
 
 ;
 
 END ESET
 
 END SUBSCHEMA
 
