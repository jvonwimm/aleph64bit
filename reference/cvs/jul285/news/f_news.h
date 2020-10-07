C! 1st entry in F_set
 ! JULIA 284
   FITALL : Fix bug which can result in losing association to ECAL cluster for
            track following a curling track in the track list 
            (A.Venturi 10/10/96)

 * corr file 281.01
   FITALL : Fix bug which can result in overwriting BOS when an
            ITC track appears which has added a few TPC coordinates. (D.Casper)

 ! JULIA 281
   FPIDEN : Bugfix: TPC bits in NDZ start at 10, not 8,
            Remove duplicate call to NAMIND('FRFT')     (MC).

 ! JULIA 280
   FPIDEN : call TPDHYP('WIRE', instead of TIDHYP (FLR).

 ! JULIA 278
   FITALL, FREFIT : replace references to obsolete UFTTRK by UFTTRA
              (G. Taylor, Sep 95)

