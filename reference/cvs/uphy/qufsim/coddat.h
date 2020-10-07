C
C Stable and unstable particles. Patrick Janot -- 21 Oct 1993
C Modification to previous code :
C   o Add Deuteron,Alpha and Tritium to the list of stable particle
C   o Interface to JETSET 7.3
C
      PARAMETER ( nstab = 19, istab = 24, istch = 13 )
      INTEGER KCODES(nstab),KCOLU(nstab)
      INTEGER KCOLU1(istab),KCOAL(istab)
      CHARACTER*12 PARNA(istab)
      LOGICAL FSTAB, ASTAB, KINS, KAPI
C
C  These are the stable particles (i.e. eventually detected)
C
C 1.- Their names
C
      DATA PARNA /'e-','e+','mu-','mu+','pi+','pi-','K+','K-',
     .            'p','p#','Alpha','Deuteron','Tritium',
     .            'gamma','nue','nue#','numu','numu#',
     .            'nutau','nutau#','n','n#','K0l','nu'/
C
C 2.- Their codes in ALPHA
C
      DATA KCOAL  /3, 2, 6, 5, 8, 9, 11, 12, 14, 15, 47, 45, 46,
     .             1, 58, 59, 60, 61, 62, 63, 13, 25, 10, 4/
C
C 3.- Their codes in LUND (JETSET 7.3)
C
      DATA KCOLU1 /11, -11, 13, -13, 211,-211,321,-321,2212,-2212,
     .             401,402,403,22,12,-12,14,-14,16,-16,2112,-2112,
     .             130,11/
C
C And now the unstable particles that were not decayed by LUND
C
C 1.- Their codes in ALPHA
C
      DATA KCODES /   16,   18,   19,   20,   21,   22,   23,   24,
     .                      26,   27,   28,   29,   30,   31,   32,
     .                       8,    9,   11,   12/
C
C 2.- Their codes in LUND (JETSET 7.3)
C
      DATA KCOLU  /  310, 3122, 3222, 3212, 3112, 3322, 3312, 3334,
     .                   -3122,-3112,-3212,-3112,-3322,-3312,-3334,
     .                   211,-211,321,-321/
C
C  Logical stables
C
      FSTAB(I)= I.EQ.22   .OR. (I.GE.11 .AND. I.LE.14) .OR.
     .          I.EQ.16   .OR. I.EQ.211  .OR. I.EQ.321 .OR.
     .          I.EQ.2212 .OR. I.EQ.2112 .OR. I.EQ.130 .OR.
     .          I.EQ.401  .OR. I.EQ.402  .OR. I.EQ.403
C
      ASTAB(I)= I.EQ.3  .OR.  I.EQ.2  .OR.  I.EQ.6  .OR.
     .          I.EQ.5  .OR.  I.EQ.8  .OR.  I.EQ.9  .OR.
     .          I.EQ.11 .OR.  I.EQ.12 .OR.  I.EQ.14 .OR.
     .          I.EQ.15 .OR.  I.EQ.47 .OR.  I.EQ.45 .OR.
     .          I.EQ.46 .OR.  I.EQ.1  .OR.  I.EQ.58 .OR.
     .          I.EQ.59 .OR.  I.EQ.60 .OR.  I.EQ.61 .OR.
     .          I.EQ.62 .OR.  I.EQ.63 .OR.  I.EQ.13 .OR.
     .          I.EQ.25 .OR.  I.EQ.10 .OR.   I.EQ.4
C
C  Logical unstables
C
      KINS(I)= I.EQ.21 .OR. I.EQ.29 .OR. I.EQ.20 .OR. I.EQ.28 .OR.
     .         I.EQ.19 .OR. I.EQ.27 .OR. I.EQ.22 .OR. I.EQ.30 .OR.
     .         I.EQ.23 .OR. I.EQ.31 .OR. I.EQ.18 .OR. I.EQ.26 .OR.
     .         I.EQ.24 .OR. I.EQ.32 .OR. I.EQ.16
C
      KAPI(I)= I.EQ. 8 .OR. I.EQ. 9 .OR. I.EQ.11 .OR. I.EQ.12
C
