      COMMON/ITRKPA/MNLINK(2),MXLINK(2),MXJMPD(2),MXJMPC(2),FZTSIT(2),
     +ZFACIT(2),CLMINI(2),CHIELI,CLMNAD,
     +MATCHX,DELMIN,CHI2PM,CHI2T1,CHI2T4
      LOGICAL FZTSIT,MATCHX
#if defined(DOC)
C! Parameters and cuts used in ITC tracking
C  1st component is to describe the TPC track extension phase.
C  2nd component is to describe the ITC stand alone tracking.
C  MNLINK(i) = Min. no. of links allowed on a track.
C                (i.e. Min. no. of hits - 1).
C  MXLINK(i) = Max. no. of links allowed on a track.
C                (Only used internally).
C  MXGAPD(i) = Max. no. of layers over which a link can jump.
C                (The value which one would really like this to have).
C  MXGAPC(i) = The value of MXGAPD(i) which one is forced to have
C                because of limited CPU time.
C  FZTSIT(i) = Flag to indicate whether Z information should be used
C  ZFACIT(i) = Tolerance allowed in Z
C  CLMINI(i) = Min. confidence level on ITC track fit.
C  CHIELI    = Min. CHI**2 contribution of coord. for it to be considered
C              for elimination during ITC-TPC tracking.
C  CLMNAD    = Min. confidence level for increase in CHI**2 on
C                         adding ITC coords to TPC track.
C Parameters for matching unextended TPC and ITC stand-alone tracks.
C   Is matching to be used ?
C  MATCHX    = Flag to indicate whether matching should be done.
C  DELMIN    = Max. diff. in phis at ITC-TPC boundary.
C  CHI2PM    = Max. CHI**2 for comparison of helix params.
C  CHI2T1    = Max. CHI**2 for final track fit (IOPT=1 in UFITMS).
C  CHI2T4    = Max. CHI**2 for final track fit (IOPT=4 in UFITMS).
#endif
