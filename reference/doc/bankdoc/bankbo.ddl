 SUBSCHEMA BomONLBanks
 : 'BOM raw data banks'
 
 AUTHOR   'J.Nash, O.Schneider'
 VERSION  '2.0'
 DATE     '28/04/95'
 
 DEFINE ESET
 
 
 BOMP
      :      'Positions at BOMs\
              Number of columns \
              Number of rows = 2'

           STATIC

           SIZE 2,2
 
      = (XA = INTE [*,*]       : 'X position at BOM A',
         YA = INTE [*,*]       : 'Y position at BOM A',
         SA = INTE [*,*]       : 'Sigma position at BOM A',
         CA = INTE [*,*]       : 'Current at BOM A',
         XB = INTE [*,*]       : 'X position at BOM B',
         YB = INTE [*,*]       : 'Y position at BOM B',
         SB = INTE [*,*]       : 'Sigma position at BOM B',
         CB = INTE [*,*]       : 'Current at BOM B',
         BI = INTE [0,5]       : 'Bunch ID',
         PU = INTE [0,4096]    : 'Calibration pulser setting')
      ;

 BOMR
      :      'BOM raw data\
              Number of columns =  \
              Number of rows = 16'

           STATIC

           SIZE 16,16
 
      = (MeaN  = INTE [0,*]     : 'Mean value',
         SIgma = INTE [0,*]     : 'Sigma',
         PEd   = INTE [0,*]     : 'Latest pedestal',
         CaLi  = INTE [0,*]     : 'Latest calibration')
      ;

 BOMC
      :      'BOM magnet and separator currents\
              Number of columns =  \
              Number of rows = 1'

           STATIC

           SIZE 1,1
 
      = (CUrrent(29) = INTE [0,*]     : 'LEP magnet or separator current')
       ;

 BOMQ
      :      'QS0 quadrupole positions \
              Number of probes or columns \
              Number of sides or rows = 2 
              (row 1 corresponds to side A, row 2 corresponds to side B)'

           STATIC

           SIZE 2,2
 
      = (QuadX          = REAL [*,*]  : 'X position (in microns)',
         QuadY          = REAL [*,*]  : 'Y position (in microns)',
         QuadZ          = REAL [*,*]  : 'Z position (in microns)',
         QuadRedundantY = REAL [*,*]  : 'Redundant Y position (in microns)')
       ;
 

 END ESET

 END SUBSCHEMA


 SUBSCHEMA BomJULBanks
 : 'BOM Julia banks'
 
 AUTHOR   'R.Forty,O.Schneider'
 VERSION  '3.0'
 DATE     '14/06/96'
 

 DEFINE ESET

 BOME
      :      'BOM analysis results\
              Number of columns =  \
              Number of rows = 2'

           STATIC

           SIZE 2,2
 
      = (XI = REAL [*,*]      : 'X position',
         XP = REAL [*,*]      : 'X angle',
         YI = REAL [*,*]      : 'Y position',
         YP = REAL [*,*]      : 'Y angle',
         BU = INTE [1,8]      : 'Bunch id',
         IA = REAL [0.,*]     : 'Beam current BOM A',
         IB = REAL [0.,*]     : 'Beam current BOM B',
         R1 = REAL [*,*]      : 'Gain ratio pair 1',
         R2 = REAL [*,*]      : 'Gain ratio pair 2',
         R3 = REAL [*,*]      : 'Gain ratio pair 3',
         R4 = REAL [*,*]      : 'Gain ratio pair 4',
         ER = INTE [*,*]      : 'Error flag')
      ;

 BOMB
      :      'BOM beam position\
              Number of columns =  \
              Number of rows = 1'

           STATIC

           SIZE 1,1
 
      = (XX = REAL [*,*]      : 'X position',
         YY = REAL [*,*]      : 'Y position',
         EE = INTE [*,*]      : 'Error flag')
      ;

 BLQP
      :      'Beam positions at IP4 (using LEP BOM data 
              from TURBOIP) and QS0 quadrupole positions. 
              [bank number = run number]\
              Number of columns \
              Number of rows = number of "mini-chunks" in run'

           STATIC
 
  =  (FirstEvent     = INTE [0,*] : 'Number of first event in "mini-chunk"',
      StatusFlags(2) = INTE [*,*] : '1st word: side A status bits \
                                     2nd word: side B status bits',
      lepBOm(2,2)    = REAL [*,*] : 'IP4 beam positions (in microns) \ 
                                     from LEP wide-band BOM: \
                                     x side A, y side A, x side B, y side B',
      QSzero(2,2)    = REAL [*,*] : 'QS0 positions (in microns): \
                                     x side A, y side A, x side B, y side B')
      ;

 END ESET

 END SUBSCHEMA

