SUBSCHEMA SATRMC

  : 'Description of BOS banks for the Small Angle Tracking Device MC data'

  AUTHOR        'H. Meinhard'
  REVIEWER      ' '
  VERSION       '2.2'
  DATE          '20-Jun-1988'

  DEFINE ESET

    SADI     : 'Small Angle Digitization Bank
                This bank contains all the hit information read out by
                the SATR electronics. Compared to the SAHI bank, multiple
                hits are not resolved and certain pairs of wires are
                put to the same TDC, as defined by the oring stored on
                the database (Raw data bank)\
                Number of words / electronics hit\
                Number of electronics hits'
             STATIC
             = (IntRawdata = INTE :'Raw data word: 
                 bits 0...15: TDC contents [0,1023], 16...20: TDC card [0,23],
                 21...23: TDC crate [0,2], 24...27: TDC channel [0,15]')
             ;

    SAHT     : 'Small Angle Monte Carlo hits
                This bank contains the information of all the hits in the SATR.
                The wire address and distance to wire is packed into a single
                word. (MC truth bank)\
                Number of words / wire hit\
                Number of wire hits'
             STATIC
             = (IntrawWord = INTE :'Raw data word:
                bits 0...15: distance between hit and wire,
                bits 16...19: Wire [1,14], 20...23: Sector [1,8],
                bits 24...27: Layer [1,9], 28...31: Side [1,2]')
              ;

  END ESET

END SUBSCHEMA



SUBSCHEMA SATRREC
 
  : 'Description of BOS banks for the Reconstruction for the Small Angle 
     Tracking Device'
 
  AUTHOR   'H. Meinhard'
  REVIEWER ' '
  VERSION  '2.4'
  DATE     '31-Jan-1990'
 
  DEFINE ESET
    
    SRTD
	     : 'Preprocessed SATR Raw data\
		Number of words / wire hit\
		Number of wire hits'
	     STATIC
	     = (IntRawdata  = INTE :'Raw data word:
                 bits 0...15: TDC contents [0,1023], 16...20: TDC card [0,23],
                 21...23: TDC crate [0,2], 24...27: TDC channel [0,15]',
		TimeRawdata  = INTE [0,1000000] :'Raw time information (TDC time 
                                in ps)')
	     ;
 
    SCOO
	     : 'Small angle tracking device coordinates\
		number of words / coordinate\
		number of coordinates'
	     STATIC
	     = (SIde  = INTE [1,2] :'coordinate side',
	        LAyer  = INTE [1,9] :'coordinate layer',
	        SeCtor  = INTE [1,8] :'coordinate sector',
	        TanTheta(2) = REAL [.03900,.1] :'tangent of polar angle of paral.')
	     ;
    
    SCRP
	     : 'Pointers from coordinates (SCOO) to raw data (SRTD)\
		number of words / coordinate\
		number of coordinates'
	     STATIC
	     = (pointerCR  = INTE [1,*] :'raw data number')
	     ;
    
    SSCP
	     : 'Pointers from layers to coordinates (SCOO)\
		number of words / layer\
		number of layers'
	     STATIC
	     = (PointBegin = INTE [1,*] :'number of first coord. in layer',
	        PointEnd = INTE [0,*] :'number of last coord. in layer')
	     ;
    
    SCLU
             : 'Small angle tracking device clusters of fired wire tubes\
                number of words / cluster\
                number of clusters'
             STATIC
             = (SIde = INTE [1,2] :'Side of cluster',
                ORientation = INTE [1,3] :'Spacial orientation of cluster',
                SeCtor = INTE [1,8] :'Sector number of cluster',
                NumWires = INTE [2,3] :'Number of fired wire tubes in cluster',
                ThetaLower = REAL [.03900,.1] :'Lower theta border of cluster',
                ThetaUpper = REAL [.03900,.1] :'Upper theta border of cluster')
             ;
 
    SCCP
             : 'Small angle tracking device pointers from clusters (SCLU)
                to coordinates (SCOO)\
                number of words / cluster\
                number of clusters'
             STATIC
             = (pointerCC(3) = INTE [0,*] :'coordinate numbers')
             ;
 
    SSBP
             : 'Small angle tracking device pointers from orientations to
                clusters (SCLU)\
                number of words / orientation\
                number of orientations'
             STATIC
             = (PointBegin = INTE [1,*] :'number of first cluster in orient.',
                PointEnd = INTE [0,*] :'number of last cluster in orient.')
             ;
 
    SPAT
             : 'Small angle tracking device patches of intersecting fired
                wire tubes\
                number of words / patch\
                number of patches'
             STATIC
             = (SIde = INTE [1,2] :'Side of patch',
                NumCorner = INTE [3,8] :'Number of corners of patch',
                NumWires = INTE [4,9] :'Number of fired wire tubes in patch',
                AX(8) = REAL [-.10000,.1] :'x/z coordinates of patch corners',
                AY(8) = REAL [-.10000,.1] :'y/z coordinates of patch corners')
             ;
 
    SPCP
             : 'Small angle tracking device pointers from patches (SPAT)
                to coordinates (SCOO)\
                number of words / patch\
                number of patches'
             STATIC
             = (pointerPC(9) = INTE [0,*] :'coordinate numbers')
             ;
 
    SSPP
             : 'Small angle tracking device pointers from sides to patches
                (SPAT)\
                number of words / side\
                number of sides'
             STATIC
             = (PointBegin = INTE [1,*] :'number of first patch on side',
                PointEnd = INTE [0,*] :'number of last patch on side')
             ;
 
    SWPA
             : 'Small angle tracking devices samples of wire paralleles\
                number of words / sample\
                number of samples'
             STATIC
             = (SIde = INTE [1,2] :'Side of sample',
                NumParall = INTE [0,9] :'Number of wire paralleles in sample',
                pointerMP = INTE [1,*] :'Pointer from sample to patch',
                pointerMC(9) = INTE [0,*] :'Pointer from sample to coordinates')
             ;

    SACO
             : 'SATR alignment corrected coordinates\
                number of words / coordinate\
                number of coordinates'
             STATIC
             = (RAdius = REAL [9.500,30.] :'Distance of wire parallel to z-axis
                                            (local coordinates)',
                XF(3) = REAL [-300.000,300.] :'Foot point of radius in ALEPH
                                               coordinates',
                ER(3) = REAL [-1.000,1.] : 'Unit vector in radius direction,
                                            ALEPH coordinates',
                XW(3) = REAL [-300.000,300.] :'Direction of wire parallel in
                                               ALEPH coordinates',
                BF(3) = REAL [-300.000,300.] :'Foot point of radius, corrected
                                               for B field, ALEPH coordinates',
                BR(3) = REAL [-1.000,1.] :'Unit vector in radius direction,
                                           corrected for B, ALEPH coordinates',
                BW(3) = REAL [-300.000,300.] :'Direction of wire parallel,
                                               corrected for B, ALEPH coordinates')
             ;

    SKAN
	     : 'Small angle tracking device track candidates\
		number of words / track candidate\
		number of track candidates'
	     STATIC
	     = (SIde = INTE [1,2] :'Side of track candidate',
                NumParal  = INTE [0,9] :'Number of wire par. in track cand.',
		Chi2  = REAL [0.000,*] :'Chisquare of track fit, NDF = NumParal - 2',
		AX  = REAL [-.10000,.1] :'dx/dz of fitted track',
		AY  = REAL [-.10000,.1] :'dy/dz of fitted track',
		covXX  = REAL :'covariance element (x,x) of fit',
		covXY  = REAL :'covariance element (x,y) of fit',
		covYY  = REAL :'covariance element (y,y) of fit')
	     ;
 
    SKCP
	     : 'Pointers from track candidates (SKAN) to coordinates (SCOO)\
		number of words / track candidate\
		number of track candidates'
	     STATIC
	     = (pointerKC(9) = INTE [0,*] :'coordinates numbers',
                pointerKM = INTE [1,*] :'number of patch')
	     ;
 
    SSKP
	     : 'Pointer from sides to track candidates (SKAN)\
		number of words / side\
		number of sides'
	     STATIC
	     = (PointBegin = INTE [1,*] :'number of first track cand. in side',
	        PointEnd = INTE [0,*] :'number of last track cand. in side')
	     ;
 
    STRK
	     : 'Small angle tracking device tracks/patches\
		number of words / track\
		number of tracks'
	     STATIC
	     = (NumParal  = INTE [0,9] :'Number of wire par. in track',
		Chi2  = REAL [0.000,*] :'Chisquare of track fit, NDF = NumParal - 2',
		THeta  = REAL [.03900,.1]|[3.04000,3.103] :'polar angle of fitted track',
		PHi  = REAL [0.0000,6.29] :'azimuth angle of fitted track',
		covTT  = REAL :'covariance element (theta,theta) of fit',
		covTP  = REAL :'covariance element (theta,phi) of fit',
		covPP  = REAL :'covariance element (phi,phi) of fit',
                QualFlag = INTE [0,1] :'Quality flag: 1 = fitted track, 
                                               0 = patch parameter',
                pointerLI = INTE [0,*] :'Pointer to row in LIDT')
                                        
	     ;
    
    STCP
	     : 'Pointers from tracks (STRK) to coordinates (SCOO)\
		number of words / track\
		number of tracks'
	     STATIC
	     = (pointerTC(9) = INTE [0,*] :'coordinates numbers')
	     ;
    
    SSTP
	     : 'Pointer from sides to tracks (STRK)\
		number of words / side\
		number of sides'
	     STATIC
	     = (PointBegin = INTE [1,*] :'number of first track in side',
	        PointEnd = INTE [0,*] :'number of last track in side',
                TrackEnd = INTE [0,*] :'number of last track in side',
                PatchStart = INTE [1,*] :'number of first patch in side')
	     ;
 
    SUPA
	     : 'Small angle tracking device patch parameters
                (in case that no track found)\
		number of words / patch\
		number of patches'
	     STATIC
	     = (NumParal  = INTE [0,9] :'Number of wire par. in patch',
		Chi2  = REAL [0.000,*] :'Chisquare of patch, NDF = NumParal - 2',
		THeta  = REAL [.03900,.1]|[3.04000,3.103] :'polar angle of patch',
		PHi  = REAL [0.0000,6.29] :'azimuth angle of patch',
		covTT  = REAL :'covariance element (theta,theta) of patch',
		covTP  = REAL :'covariance element (theta,phi) of patch',
		covPP  = REAL :'covariance element (phi,phi) of patch')
	     ;
    
    SUCP
	     : 'Pointers from patches (SUPA) to coordinates (SCOO)(temporary)\
		number of words / patch\
		number of patches'
	     STATIC
	     = (pointerUC(9) = INTE [0,*] :'coordinates numbers',
                pointerUP    = INTE [1,*] :'patch number')
	     ;
    
    SSUP
	     : 'Pointer from sides to patches (SUPA)\
		number of words / side\
		number of sides'
	     STATIC
	     = (PointBegin = INTE [1,*] :'number of first patch in side',
	        PointEnd = INTE [0,*] :'number of last patch in side')
	     ;
 
  END ESET
 
END SUBSCHEMA

SUBSCHEMA SATRPOT

  : 'Description of BOS banks used for the SATR part of the POT'

  AUTHOR        'H. Meinhard'
  REVIEWER      'F.Ranjard '
  VERSION       '1.3'
  DATE          '20/05/94'

  DEFINE ESET

    SFTR     : 'SATR fitted tracks
                *** New POT bank for 1990 data ***\
                number of words / track\
                number of tracks'
             STATIC
             = (NumdegFree = INTE [0,14] :'Number of degrees of freedom for fit',
                Chi2 = REAL [0.00,*] : 'Chisquare of fit',
                QualFlag = INTE [0,2] : 'Quality flag: 0 = single arm patch,
                                1 = single arm fitted track, 2 = double arm fitted
                                track',
                THeta = REAL [.03000,3.15] :'Track direction polar angle at z = 0',
                PHi = REAL [0.000,6.29] :'Track direction azimuth angle at z = 0',
                X0 = REAL [-10.000,10.] :'Track impact point x at z = 0',
                Y0 = REAL [-10.000,10.] :'Track impact point y at z = 0',
                CovMatrix(10) = REAL : 'Elements of covariance matrix of TH,PH,X0,Y0',
                LIdtpoint = INTE [0,*] : 'Pointer to row in LIDT')
             ;

    PSCO
                : 'Small Angle Tracking Device Coordinates\
                   Number of words / coordinate\
                   Number of coordinates'
                STATIC
                = (SIde = INTE [0,1] :'Side of coordinate - 1',
                   LAyer = INTE [1,9] :'Layer of coordinate',
                   SeCtor = INTE [0,7] :'Sector of coordinate - 1',
                   TanThet(2) = INTE [3900,10000] :'tan theta of wire parallel * 100000')
                ;

    PSPO
                : 'Small Angle Tracking Device Pointers from Tracks to Coordinates\
                   Number of pointers / track\
                   Number of tracks'
                STATIC
                = (pointerTC(9) = INTE [0,511] :'Pointers from tracks to coordinates')
                ;

  END ESET

END SUBSCHEMA

SUBSCHEMA SATRCAL
 
  : 'Description of BOS banks for the calibration of the Small Angle Tracking
     Device data (non-database banks)'
 
  AUTHOR        'H. Meinhard'
  REVIEWER      'H. Seywerd '
  VERSION       '1.3'
  DATE          '02-May-1991'
 
  DEFINE ESET
 
    SPED     : 'Pedestals and slopes for the SATR TDCs\
                Number of words / TDC\
                Number of TDCs'
             STATIC
             = (SLope = INTE [*,*]: 'Slope in ps / count',
                PEdes = INTE [*,*]: 'Pedestal in ps',
                ESlope = INTE [*,*]: 'Error on slope in ps / count',
                EPedes = INTE [*,*]: 'Error on pedestal in ps')
             ;
 
    SPHD     : 'Header for bank SPED\
                number of words / row\
                number of rows'
             STATIC
             = (FLag = INTE [*,*]: 'flag',
                RUnn = INTE [1,*]: 'run number',
                TimSta = INTE [0,*]: 'timer start (ps)',
                DelTim = INTE [0,*]: 'timer delta (ps)',
                TimCnt = INTE [2,*]: 'timer count',
                PulCnt = INTE [0,*]: 'pulse count')
             ;
 
    STHI     : 'Histogram of SATR TDC times\
                number of words / row\
                number of rows'
             STATIC
             = (NumChn = INTE [1,*]: 'number of histogram channels',
                AbsSta = INTE [0,1023]: 'abscissa of left edge of first
                                         channel (ps)',
                AbsDel = INTE [0,1023]: 'step of abscissa (ps)',
                NumEnt = INTE [0,*]: 'number of entries in histogram',
                NumUnd = INTE [0,*]: 'number of underflows',
                NumOve = INTE [0,*]: 'number of overflows',
                ChaCon(100) = INTE [0,*]: 'channel contents')
             ;

    SGMN     : 'SATR gas monitor histogram\
                number of words / row\
                number of rows'
             STATIC
             = (AccTim = INTE [0,*]: 'accumulation time (s)',
                NumEnt = INTE [0,*]: 'total number of entries',
                HisCont(1024) = INTE [0,*]: 'histogram contents')
             ;
 
    SHOT     : 'SATR list of bad channels as determined from the online
                SOR bank\
                number of words / hot channel\
                number of channels'
             STATIC
             = (CRate = INTE[1,3]  :'Electronic crate number',
                CArd  = INTE[0,23] :'TDC card in the crate',
                CHan  = INTE[0,15] :'TDC channel on the card')
             ;
 
  END ESET
 
END SUBSCHEMA
