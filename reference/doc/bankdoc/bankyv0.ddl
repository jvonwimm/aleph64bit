 SUBSCHEMA Yv0JULBanks
 : 'Description of V0 VERTEX fit output banks'
 
 AUTHOR   'M.A. Ciocci,J.Sedgbeer,D.Casper'
 REVIEWER 'none'
 VERSION  '2.0'
 DATE     '04/03/97'
 
 DEFINE ESET
 
 YV0V
      :      'V0 vertex fit List \
              Number of words/V0\
              Number of V0'
           STATIC
 
      = (K1         = INTE [1,*]           : 'Number  of the  positive  track\
                                              coming from V0 in the frft bank',
         K2         = INTE [1,*]           : 'Number  of the  negative  track\
                                              coming from V0 in the frft bank',
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
         X1consmass = REAL [*,*]           : 'First  constraint of mass\
                                              r on the aleph note',
         X2consmass = REAL [*,*]           : 'Second  constraint of mass \
                                              b on the aleph note',
         XerrMat(3) = REAL [*,*]           : 'Triangular    covariance matrix\
                                              of V0s constraint stored in the\
                                              order:\
                                                      1  2\
                                                      -  3',
         C2chisquare= REAL [0.000,*]       : 'Chisquare of V0 vertex fit',
         ICodevo    = INTE [-22,22]        :'(Fit hypothesis*10+abs(IND))*sign.\
                                              IND in range -2 to 2 (see YV0\
                                              package write-up). IND=-2 or -1\
                                              if tracks do not intersect in xy.\
                                              IND=0 - intersect at one point.\
                                              IND=1 or 2  - intersect twice.\
                                              Sign is positive if IND is\
                                              positive. Fit hyp.=1 if tracks\
                                              not parallel at vertex (ie. IND=1\
                                              or diff. in tan(dip) not small).\
                                              Fit hyp=2 if tracks approx\
                                              parallel at vertex (ie. IND.NE.1\
                                              and diff.in tan(dip) is small).',
         P1(3)      = REAL [*,*]           :  'Momentum of positive charged\
                                               particle coming from V0\
                                               candidate',
         P2(3)      = REAL [*,*]           :  'Momentum of negative charged\
                                               particle coming from V0\
                                               candidate',
         EP(21)     = REAL [*,*]           :   'Coovariance matrix of P1 P2\
                                                stored in the order\
                                                1   2  4  7  11  16\
                                                -   3  5  8  12  17\
                                                -   -  6  9  13  18\
                                                -   -  -  10 14  19\
                                                -   -  -  -  15  20\
                                                -   -  -  -  -   21',
        DM         =  REAL [*,*]           :    'Minimum distance between\
                                                 helices  (see aleph note)',
        S1         =  REAL [*,*]           :     'Psi angle for positive\
                                                  charged particle coming\
                                                  from V0',
        S2         =  REAL [*,*]           :     'Psi angle for negative\
                                                  charged particle coming\
                                                  from V0')
   
 ;  
  
 YLV0
      :      'High purity V0 vertex fit List \
              Number of words/V0\
              Number of V0'
           STATIC
 
      = (K1         = INTE [1,*]           : 'Number  of the  positive  track\
                                              coming from V0 in the frft bank',
         K2         = INTE [1,*]           : 'Number  of the  negative  track\
                                              coming from V0 in the frft bank',
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
         X1consmass = REAL [*,*]           : 'First  constraint of mass\
                                              r on the aleph note',
         X2consmass = REAL [*,*]           : 'Second  constraint of mass \
                                              b on the aleph note',
         XerrMat(3) = REAL [*,*]           : 'Triangular    covariance matrix\
                                              of V0s constraint stored in the\
                                              order:\
                                                      1  2\
                                                      -  3',
         C2chisquare= REAL [0.000,*]       : 'Chisquare of V0 vertex fit',
         ICodevo    = INTE [-22,22]        : '(Fit hypothesis*10+abs(IND))*sign.\
                                              IND in range -2 to 2 (see YV0\
                                              package write-up). IND=-2 or -1\
                                              if tracks do not intersect in xy.\
                                              IND=0 - intersect at one point.\
                                              IND=1 or 2  - intersect twice.\
                                              Sign is positive if IND is\
                                              positive. Fit hyp.=1 if tracks\
                                              not parallel at vertex (ie. IND=1\
                                              or diff. in tan(dip) not small).\
                                              Fit hyp=2 if tracks approx\
                                              parallel at vertex (ie. IND.NE.1\
                                              and diff.in tan(dip) is small).',
         P1(3)      = REAL [*,*]           : 'Momentum of positive charged\
                                              particle coming from V0\
                                              candidate',
         P2(3)      = REAL [*,*]           : 'Momentum of negative charged\
                                              particle coming from V0\
                                              candidate',
         EP(21)     = REAL [*,*]           : 'Coovariance matrix of P1 P2\
                                              stored in the order\
                                              1   2  4  7  11  16\
                                              -   3  5  8  12  17\
                                              -   -  6  9  13  18\
                                              -   -  -  10 14  19\
                                              -   -  -  -  15  20\
                                              -   -  -  -  -   21',
         DM         =  REAL [*,*]          : 'Minimum distance between\
                                              helices  (see aleph note)',
         S1         =  REAL [*,*]          : 'Psi angle for positive\
                                              charged particle coming\
                                              from V0',
         S2         =  REAL [*,*]          : 'Psi angle for negative\
                                              charged particle coming\
                                              from V0')
   
;  
 
 
 END ESET
 
 END SUBSCHEMA


 SUBSCHEMA Yv0POTBanks
 : 'Description of V0 VERTEX fit P.O.T banks'
 
 AUTHOR   'M.A. Ciocci'
 REVIEWER 'to be selected'
 VERSION  '1.1'
 DATE     '90/09/18'
 
 DEFINE ESET
 
 PYV0
      :      'V0 vertex fit List \
              Number of words/V0\
              Number of V0'
           STATIC
 
      = (K1         = INTE [1,*]          : 'Number  of the  positive  track\
                                             coming from V0 in the frft bank',
         K2         = INTE [1,*]          : 'Number  of the  negative  track\
                                            coming from V0 in the frft bank',
         VXcoordin   = REAL  [-180.,180.] : 'Fitted V0 x coordinate',
         VYcoordin   = REAL  [-180.,180.] : 'Fitted V0 y coordinate',
         VZcoordin   = REAL  [-220.,220.] : 'Fitted V0 z coordinate',
         VmatErr(3)  = REAL  [*,*]        : 'Error on V0 vertex stored\
                                             in 1% precision',
         VmatCor(3)  = INTE  [0,200]      : '(Correlation off-diagonal)*100+100\
                                             stored in the order:\
                                                      2  4\
                                                      -  5',     
         PXmomentum  = REAL  [*,*]        : 'Fitted V0 momentum px',
         PYmomentum  = REAL  [*,*]        : 'Fitted V0 momentum py',
         PZmomentum  = REAL  [*,*]        : 'Fitted V0 mometum  pz',
         PmatErr(3)  = REAL  [*,*]        : 'Error on V0 momentum stored\
                                             in 1% precision',
         PmatCor(3)  = INTE  [0,200]      : '(Correlation off-diagonal)*100+100\
                                             stored in the order:\
                                                      2  4\
                                                      -  5',     
         X1consmass  = REAL [*,*]        : 'First  constraint of mass',
         X2consmass  = REAL [*,*]        : 'Second  constraint of mass',
         XmatErr(2)  = REAL  [*,*]       : 'Error on V0 constraints stored\
                                             in 1% precision',
         XCorr       = INTE  [0,200]     : '(Correlation off-diagonal)*100+100',
         C2chisquare = INTE  [0,*]       : '(Chisquare of V0 vertex fit)*10')
       ;  
  
  
 
 
 END ESET
 
 END SUBSCHEMA
                
