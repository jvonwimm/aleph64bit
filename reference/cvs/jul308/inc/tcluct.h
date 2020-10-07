      COMMON/TCLUCT/ ITLCTA(2),ITLCCA(2),
     &               RTLCZW(2),RTLCDM(3),ITLCPT,ITLCPC,ITLCML,
     &               RTLCTL,ITLCPM,ITLCOC,ITLCOP,ITLCFA,
     &               ITLCNP,ITLCXP,ITLCIB,ITLCAB,
     &               FTLCAF,FTLCOO(2),ITLCRM,FTLCQK,RTLCFT,
     &               RTLCPT,ITLCMS,RTLCZM
      LOGICAL FTLCOO,FTLCAF,FTLCQK
#if defined(DOC)
C!   These are the cuts used in TPC cluster finding and pulse
C   analysis.  They are filled by TRNCON at the job beginning from
C   the BOS bank TLCT and used by TCLCOR and those routines called
C   by TCLCOR.
C
C     ITLCPT =    PulseThresh from bank TLCT
C     RTLCPT =    Floating point conversion of ITLCPT
C     ITLCPC =    PeakCut from bank TLCT
C     RTLCDM =    DefMax from bank TLCT
C     ITLCML =    MinimumLen from bank TLCT
C     RTLCTL =    TanLdivide from bank TLCT
C     ITLCTA =    TimeAlg from bank TLCT
C     ITLCCA =    ChargeAlg from bank TLCT
C     FTLCOO =    OverlapOpt from bank TLCT
C     FTLCAF =    AlgForce from bank TLCT
C     ITLCNP =    miNPulses from bank TLCT
C     ITLCXP =    maXPulses from bank TLCT
C     ITLCIB =    mInBuckets from bank TLCT
C     ITLCAB =    mAxBuckets from bank TLCT
C     RTLCZW =    ZWindow from bank TLCT
C     ITLCPM =    PeakMin from bank TLCT
C     ITLCRM =    RphiMxdef from bank TLCT
C     ITLCOC =    OverCut from bank TLCT
C     ITLCOP =    OverParm from bank TLCT
C     ITLCFA =    FullAnalysis from bank TLCT
C     FTLCQK =    QuicKanalysis from bank TLCT
C     RTLCFT =    FractThresh from bank TLCT
C     ITLCMS =    MaxSaturate from bank TLCT
C     RTLCZM =    Minimum distance of a cluster from the sector end
C                 of the TPC volume
C-----------------------------------------------------------------------
#endif
