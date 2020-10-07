C! YTOP parameters
      COMMON/YPARTO/DHXLIM,CHISEL,PMINSE,PIDACP,BFIELD,
     &       MNTHPV,MXTSPV, PMINRQ,PMINRA,
     &       CHVXCO,CHPTCO,RVACCO,AMCTCO,DZMXCO,NAMXCO,EPLOCO,EPHICO,
     &       CHVXV0,CHPTV0,CHVSV0,CHMLV0,DZMXV0,NAMXV0,
     &       PIPKV0,PRPLV0,PIPLV0,
     &       LBCRFD,LRYOLD,LRFRF2,
     &       LRPVTX,LRSVTX,LVBCR0,LRMVPV,LRLPVX,LCONVS,LVZERS,LRUSER
      LOGICAL LBCRFD,LRYOLD,LRFRF2
      LOGICAL LRPVTX,LRSVTX,LVBCR0,LRMVPV,LRLPVX,LCONVS,LVZERS,LRUSER
#if defined(DOC)
C  GENERAL PARAMETERS
C     DHXLIM ..... limit of distance from/between helix for vtx fit
C            defined in YTOIJO, used in YFMVTR,YFTVTR
C     CHISEL ..... chi**2 cut to define if a track belongs to a vtx
C            obsolete
C     PMINSE ..... minimum required charged track momentum
C            ..... for vertex search
C            defined in YTOIJO, used YTSTRK
C     PIDACP ..... particle identification probability cut
C            defined in YTOIJO, used in YPIDAS
C     BFIELD ..... magnetic field value
C            defined in YTOIRU, used in YFMVTR,YFTVTR,YTOSTR,YTPAR,
C                          YTSTRK
C  PARAMETERS FOR PRIMARY VERTEX
C     MNTHPV ..... minimum TPC hits required
C     MXTSPV ..... maximum # of tracks used for first step of primary
C            ..... vertex finding (highest momenta are selected)
C     PMINRQ ..... minimum required momentum for first step
C            ..... of primary vertex search
C     PMINRA ..... minimum required momentum for attaching
C            ..... tracks to primary vertex in the second step
C  PARAMETERS FOR YTCONV
C     CHVXCO ... YTCONV MAX.VERTEX CHISQ.
C     CHPTCO ... YTCONV MAX.POINTING CHISQ.
C     RVACCO ... YTCONV MIN.RADIUS
C     AMCTCO ... YTCONV MAX.INVARIANT MASS
C     DZMXCO ... YTCONV MAX.Z-DISTANCE OF TRACK FROM BEAM CROSSING
C     NAMXCO ... YTCONV MAX.# OF ADD. TRKS THROUGH VERTEX
C
C  PARAMETERS FOR YTRV0S
C     CHVXV0 ... YTRV0S MAX.VERTEX CHISQ.
C     CHPTV0 ... YTRV0S MAX.POINTING CHISQ.
C     CHVSV0 ... YTRV0S MIN.CHISQ VTX DIST.
C     CHMLV0 ... YTRV0S MAX.INVARIANT MASS/SIGMA DEV.
C     DZMXV0 ... YTRV0S MAX.Z-DISTANCE OF TRACK FROM BEAM CROSSING
C     NAMXV0 ... YTRV0S MAX.# OF ADD. TRKS THROUGH VERTEX
C     PIPKV0 ... YTRV0S MIN. PION PROB. OF K0 DECAY TRACKS
C     PRPLV0 ... YTRV0S MIN. PROTON PROB. IN LAMBDA DECAY
C     PIPLV0 ... YTRV0S MIN. PION PROB. IN LAMBDA DECAY
C
C  LOGICAL FLAGS
C     LRYOLD ..... use the old version of YTOP
C            defined in YTOIJO, used YTOPOL
C     LRFRF2 ..... use FRFT bank nr 2 with VDET refitted tracks
C            defined in YTOIJO, used YDEFRF
C     LRPVTX ..... reconstruct the primary vertex
C            defined in YTOIJO, used YTOPNW
C     LRSVTX ..... reconstruct secondary vertices
C            defined in YTOIJO, used YTOPNW
C     LVBCR0 ..... include beam crossing into prim vtx fit
C            defined in YTOIJO, used YTOIRU,YTPVTX,YPRIVX
C     LRLPVX ..... reject leptons from primary vertex
C            defined in YTOIJO, used YTOPNW,YTPVTX
C     LRMVPV ..... require mv coord in primary vertex search
C            defined in YTOIJO, used YTOPNW,YTPVTX
C     LCONVS ..... find gamma conversions ahead of pr. vtx search
C            defined in YTOIJO, used YTOPNW,YTPVTX,YHISTO
C     LVZERS ..... find v0's ahead of pr. vtx search
C            defined in YTOIJO, used YTOPNW,YTPVTX,YHISTO
C     LRUSER ..... call the user routine YRUSER at the end
C            defined in YTOIJO, used YTOPNW
C     LBCRFD ..... beam crossing information found
C            defined in YTOIRU, used in YTPVTX
C
#endif
