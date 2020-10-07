C
C!     These are statement functions to be used for accessing data
C     from the TPC BOS banks.
C     NOTE, To include this comdeck, one must in general include the
C           parameter comdecks TPARAC,TCLUJJ,TCRLJJ,TPULJJ,TLRLJJ,
C           although those which are essential depends on which of the
C           functions below are referenced in the routine.  Also, one
C           must include BMACRO for the definition of KROW.
C           Alternatively, one may include TPARAC and  TCDANA instead
C           of this comdeck.  Then TCDANA includes everything needed.
C--------------------------------------------------------------------
C
C          Pulse offsets
C
      KPULS(M1,M2) = KROW(KTPUL,IW(KROW(KTCLU,M1)+JTCLOP)+M2)
C
C          Cluster, Pulse, and Sample Data
C
C          First, to access cluster data by cluster number in a row:
      NRCLU(M1)       = IW(KROW(KTLRL,M1)+JTLRNC)
      KRCLU(M1,M2)    = KROW(KTCLU,IW(KROW(KTLRL,M2)+JTLROC)+M1)
      ICSEC(M1,M2)    = M1+IW(KROW(KTLRL,M2)+JTLROC)
C
C          Second, to access cluster data by cluster number in sector:
      NPULS(M1)       = IW(KROW(KTCLU,M1) + JTCLNP)
      IHITL(M1,M2)    = IW(KTPAD + IW(KPULS(M1,M2) + JTPUOP) +1)
      NSAMP(M1,M2)    = IBITS(IHITL(M1,M2),16,8)
      ICHAN(M1,M2)    = IBITS(IHITL(M1,M2),9,6)
      IPADNR(M1,M2)   = IBITS(IHITL(M1,M2),24,8)
      ITIME0(M1,M2)   = IBITS(IHITL(M1,M2),0,9)
      ISAMPH(M1,M2,M3)= IBITS( IW( KTPDI +
     1           ( IW(KPULS(M1,M2)+2) +M3-1 )*LTPDNB/32+1 ),
     2           24-LTPDNB*MOD( IW(KPULS(M1,M2)+2) +M3-1 ,32/LTPDNB),
     3           LTPDNB)
C
C--> NOTE: offsets KTCLU,KTPUL,KTPAD,KTPDI,KTLRL need to be defined <--
C-----------------------------------------------------------------------
C
C         TPC-coordinates
C
      KCROW(M1,M2)  = KROW(KTPCO,IW(KROW(KTCRL,M1)+JTCROC)+M2)
      NCOOR(M1)     = IW(KROW(KTCRL,M1)+JTCRNC)
C
C--> NOTE: offsets KTPCO,KTCRL need to be defined  <--
C-----------------------------------------------------------------------
#if defined(DOC)
C
C!               Pad clusters and pulses:
C
C     KPULS(ic,ip)  offset into TPUL for pulse 'ip' in cluster 'ic'
C                   (e.g. PH=IW(KPULS(IC,IP)+LTPUCE))
C
C     NRCLU(ir)     number of clusters in TCLU for row 'ir' of sector
C     KRCLU(icr,ir) offset into TCLU for cluster 'icr' of row 'ir'
C     ICSEC(icr,ir) cluster number in sector for clust 'icr' of row 'ir'
C
C     NPULS(ic)     # of pulses in a cluster for clust 'ic' of sector
C     IHITL(ic)     pad pulse in a cluster for clust 'ic' of sector
C     NSAMP(ic,ip)  # of samples in pulse 'ip' of cluster 'ic' of sector
C     ICHAN(ic,ip)     TPD channel # for pulse 'ip' of cluster 'ic'
C     IPADNR(ic,ip)    pad number in padrow for pulse 'ip' of clus 'ic'
C     ITIME0(ic,ip)    t0 of pulse 'ip' of cluster 'ic' of sector
C     ISAMPH(ic,ip,is) pulse height of sample 'is' in pulse 'ip' of
C                      cluster 'ic' in sector
C
C               Coordinates
C
C     KCROW(ir,ic)    offset into TPCO for coordinate 'ic' in row 'ir'
C     NCOOR(ir)       number of coordinates in row 'ir'.
C
C----------------------------------------------------------------------
#endif
