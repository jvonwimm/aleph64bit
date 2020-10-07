C--   COMMON / BTAGRAW /
C     common includes all information after call of qipbtag
C     necessary for analysis:
C
C        JJET(ntrack)     array(ntrack) with track # of ass.jet # found
C                         value(jjet(i))=1,...,njet
C        JHEMI(ntrack)    array(njet) with track # of ass.hemispere # found
C                         value(jhemi(i))=1,2
C        IP(3)/ERR_IP(3,3),CHI2_DOF:  IP and error on IP & CHI**2
C        BP(3),ERR_BP(3),SIZE_BP(3),ERR2_BP(3) Beamspot & size & errors
C        TPAR             track parameters
C        TERR             errors on track impact parameters (TANL,P0,D0,Z0)
C        JETS             unitized jet-3-momenta
C        DMIN / S_DMIN    min distance IP-track + error
C        PERP             Unit vector along impact parameter direction
C        PHIP             Direction of DMIN in thetahat-phihat plane
C        JDIST            Distance along jet direction of closest approach
C                         between track helix and jet axis.
C        LDIST / S_LDIST  Distance between track helix and jet axis at
C                         point of closest approach, with error
C        NPROBEVT         Event probability calculated using NPROBTRK.
C        NPROBHEMI(2)     Hemisphere probability calculated using NPROBTRK.
C        NPROBJET(MAXJET) Jet probability calculated using NPROBTRK.
C        NPROBTRK(MAXTRK) Negative Track probability (-PROBTRK).
C        NV0              Number of V0 candidates found
C        V0TYPE           V0 type 1=photon conversion, 2=Ks, 3=Lambda
C        V0PAIR           FRFT track numbers of V0 constituents
C        V0PAT            OR of VDET hit patterns of V0 daughter tracks
C        V0DK             Space point of V0 decay
C        V0DERR           Error matrix on decay point
C        V0PROB           Chisquared probability of combined mass+vertex fit
C        V0PAR            Track parameters of V0 (1st element=momentum)
C        V0ERR            Error matrix of V0 track paramters
C        HAXIS            Axis defining hemisphere separation
C        NDAU             # of daughters associated with a user track
C        TRKDAU           FRFT track number of user track daughters
C
      INTEGER TFLAG(MAXTRK)
      INTEGER JHEMI(MAXTRK),JJET(MAXTRK)
      INTEGER NV0,V0TYPE(MAXV0),V0PAIR(2,MAXV0),V0PAT(MAXV0)
      INTEGER NDAU(MAXTRK),TRKDAU(MAXDAU,MAXTRK)
      REAL V0DK(3,MAXV0),V0DERR(3,3,MAXV0),V0PROB(MAXV0)
      REAL V0PAR(5,MAXV0),V0ERR(5,5,MAXV0),HAXIS(3)
      REAL    TPAR(5,MAXTRK),TERR(4,4,MAXTRK),JETS(3,MAXJET)
      REAL    DMIN(MAXTRK),S_DMIN(MAXTRK)
      REAL    IP(3),ERR_IP(3,3),CHI2_DOF
      REAL    BP(3),ERR_BP(3),SIZE_BP(3),ERR2_BP(3),PHIP(MAXTRK)
      REAL    JDIST(MAXTRK),LDIST(MAXTRK)
      REAL    S_LDIST(MAXTRK),PERP(3,MAXTRK)
      REAL    NPROBEVT,NPROBHEMI(2),NPROBJET(MAXJET),NPROBTRK(MAXTRK)
C--   corresponding common block:
      COMMON / BTAGRAW /
     &     BP,ERR_BP,SIZE_BP,ERR2_BP,IP,ERR_IP,CHI2_DOF,
     &     JJET,JHEMI,TPAR,TERR,JETS,
     &     TFLAG,DMIN,S_DMIN,PHIP,JDIST,LDIST,S_LDIST,
     &     PERP,NPROBEVT,NPROBHEMI,NPROBJET,NPROBTRK,
     &     NV0,V0TYPE,V0PAIR,V0PAT,V0DK,V0DERR,V0PROB,
     &     V0PAR,V0ERR,HAXIS,NDAU,TRKDAU
