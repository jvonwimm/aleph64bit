C
C  Reconstruction parameters
C
      COMMON/VRECON/MCEVNT,
     &     CHNOFF,IPHOFF,NRCHAN,IDCLIN,NZEXT,
     &     MNPULS,MXUNBD,MXSIGM,FNLSUM,
     &     MKVDXY,MKVFPH,MKVHOT,
     &     MINCMD,HCUTCM,LCUTCM,NHOTMN,
     &     MXOCUP,DECFAC,SNOISE,
     &     SEPSIG,POSSIG,ERRNOM,
     &     CONFLAG,CONPAR
C
      INTEGER MNPULS,NRCHAN,NHOTMN,IDCLIN,NZEXT
      INTEGER CHNOFF(2),MINCMD,MXUNBD,MXSIGM,FNLSUM,IPHOFF
      INTEGER CONFLAG(10)
      REAL CONPAR(10)
      REAL HCUTCM,LCUTCM,MXOCUP(2),DECFAC,SNOISE(2)
      REAL SEPSIG,POSSIG,ERRNOM(2)
      LOGICAL MCEVNT,MKVDXY,MKVFPH,MKVHOT,LDECAY(2)
C
#if defined(DOC)
C
C!  General flags- only 1 of these should be on at a time
C
C  MCEVNT = yes/no MC
C
C  Channel offsets
C
C  IPHOFF = pulseheight offset
C  CHNOFF = Offset of online readout (should be zero)
C
C  Final hit finding parameters
C
C  MNPULS = Minimum single strip pulse (in 250 electron units)
C  FNLSUM = Minimum sum pulseheight of reclustered strips
C  MXUNBD = Maximum number of consecutive unbonded strips allowed
C  in a cluster
C  MXSIGM = Maximum single-channel sigma to include for final clusters
C
C  Control parameters
C
C  MKVDXY = Make final hit position banks
C  MKVFPH = Make final pulseheight banks
C  MKVHOT = Make VHOT hot channel list- THIS IS A SPECIAL MODE OF RUNNING,
C  NOT COMPATIBLE WITH NORMAL HIT PRODUCTION.
C
C  Common mode subtraction parameters
C
C  MINCMD = Minimum number of strips to make CM calculation
C  HCUTCM = # of sigma used to cut out signal (high)
C  LCUTCM = # of sigma used to cut out signal (low)
C
C  Hot channel selection parameters
C
C  MXZOCC = Maximum allowed occupancy for z-strips
C  MXPOCC = Maximum allowed occupancy for phi-strips
C
C  Pulseheight correction parameter
C
C  DECFAC = Decay time factor for the line driver
#endif
