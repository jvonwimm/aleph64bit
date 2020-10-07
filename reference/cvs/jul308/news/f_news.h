C! 1st entry in F_set
 ! JULIA 305
    FPIDEN : Obsolete, replaced by FCFRID                         (M.Cattaneo)
    FCFRID : Same as FPIDEN, but with option to do bit patterns for
             tracks only. Reaccess FRFT if garbage collection occurs 
             when creating FRID                          (D.Casper,M.Cattaneo)

 * corr file 304.01
    FITALL,FREFIT:Remove CALL from function references, for Linux. (A.Waananen)

 ! JULIA 303
    FITALL : Bug fix: KENKF was always 0 (not initialised)         (D.Casper)

 ! JULIA 302 (Tracking upgrade)
    FDETCO : Find DET coord. number for coord. on track            (F.Ranjard)
             (replaces FITCOR,FTPCOR,FVDCOR,TTGCLR by D.Casper)
    FITALL : Call new Kalman filter
             Remove filtered coords. from coordinates list         (D.Casper)
             Call FDETCO,FRMHIT                                    (F.Ranjard)
    FITWRI : Create packed POT bank PFXT from FXTR                 (D.Casper)
    FLNGV0 : New. Select V0 decaying into the TPC volume           (P.Spagnolo)
             Create high purity YLV0 bank for e.g. DALI            (D.Casper)
    FLV0DX : New. Calculate the dE/dX of track ITK assuming 
             Proton, Electron and Pion                             (D.Casper)
    FPMOVE : Redef. track params. relative to a new point in space (D.Brown)
             Use PI,TWOPI from alcons.h                            (F.Ranjard)
    FRMHIT : Remove DET hits contained in LIST from track ITRK     (F.Ranjard)
             (replaces FRMITH,FRMTPH,FRMVDH,TRMHIT by D.Casper)
    FTPCER : Return without error if TPCO 1 already exists         (D.Casper)
    FTRACK : Don't extrapolate tracks from a kink                  (D.Casper)
             
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

