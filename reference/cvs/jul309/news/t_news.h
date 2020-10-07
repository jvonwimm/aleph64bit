C! 1st entry in T_set
 * 19990518 corr file 309.1
    TRNCON : Read    clock frequency in Hz from TSOR bank (W.Wiedenmann)
    TSORDP : Display clock frequency in Hz from TSOR bank (W.Wiedenmann)
    
 ! JULIA 309
    TWIREZ : Bug fix - use IROWP instead of IROW (uninitialised variable) in 
             definition of IROWS. Screwed up check on whether a coordinate
             includes half-pads.                                 (D.Casper)

 * 981118 corr file 308.5
    TPDELS : Change value of logarithmic correction coefficient (J-Y.Nief)

 * 981116 corr file 308.4
    TPADDX : Bug fix - consider a large pad crossing angle (F.Palla)

 * 980928 corr file 308.1
    TMSHEL : Protect against divide by zero when DENOM=0.0, again (M.Cattaneo)

 * 980824 corr file 307.4
    The changes to TFTWTB and TRKELS fix the problem with dE/dx in the 
    reprocessing of 1991 data.
    TFTWTB : suppress missed hits if they will not be used           (D.Casper)
    TRKELS : ensure that zero charge samples remain zero after correction 
                                                                     (D.Casper)
    TTWODX : Bug fix: bank T2XS was not being extended a second time if 
             necessary                                             (M.Cattaneo)

 ! JULIA 307

    TPREDA : Force use of unreduced wire data (raw data banks TWIR/TWDI)
             in run range 10000-11800 (1991)          (W.Wiedenmann,M.Cattaneo)

 * corr file 306.1
    TWIREZ : Up to 1991 the banks (TSIR,TSDI,TSLE) 
             were called (TRIR,TRDI,TRLE)          (W.Wiedenmann)

 ! JULIA 306

    TPSMR1 : Change the Z smearing proportional to the drift distance
             to an average smearing. Fixes cos(theta) dependence (Thulasidas)
    TREFIT : NEW Refit frft 2 tracks after monte carlo smearing (A.Bonissent) 
    TSMFT  : NEW Steering to smear and refit frft tracks in MC  (A.Bonissent)

 * corr file 305.3
    TFITHL : Bug fix: Protect against arg SIN > 1.0 (M.Cattaneo)
    TFLNKS : Fix typo in "long-range" code. 
             Select best link, not first....        (D.Casper)
    TOVRLP : Changed scale factor in ITRKSW(2) to 10000 
             to allow for events with > 999 tracks  (I.Tomalin)
             Prevent track candidates from winding up with more 
             than one hit in a given padrow.        (D.Casper)

 * corr file 305.2
    Add TPC smearing routines for MonteCarlo
    TPDIAG : NEW Finds eigen values and vectors from cov. mat.    (Thulasidas)
    TPHSMR : NEW smears the TPCO coordinates in accordance with the 
                 database bank TPSM for Monte Carlo events        (Thulasidas)
    TPRNDM : NEW generates random vars. from eigen values/vectors (Thulasidas)
    TPSMGT : NEW Used to read TPSM bank from database             (Thulasidas)
    TPSMR1 : NEW adds the smearing vector to the TPCO coordinates (Thulasidas)

 * corr file 305.1
    TMSHEL : Protect against divide by zero when DENOM=0.0 (M.Cattaneo)

 ! JULIA 304
    TSACOR : Bug fix: NLINK was called with TPCO bank index
             rather than name                               (A.Waananen)
    TLIPAK : fix variable type inconsistencies in function calls, 
             for Linux                                            (A.Waananen)

 * corr file 304.3
    TPREDA : Correct 1997 T0 for problems (I.Tomalin)

 * corr file 304.2
    TPAKCO : Create PTNC even if FRTL and FTCL are missing (M.Cattaneo)

 * corr file 304.1
    TFITKF,TOVRLP,TSUMVD,TWIREZ :
                  Remove CALL from function references, for Linux. (A.Waananen)

 ! JULIA 304
    TCOOR,TCOORT,TSAWIR : Replace call to THPFDG with new routine THPCOR, 
                          to correct 1 half-pad coordinates too.   (D.Casper)
    TCSHF1 : New. Copy of TCSHFT to shift coordinates derived from 
                  sub-clusters with one, rather than two, half-pads (D.Casper)
    THPCOR : New. Copy of THPFDG to handle corrections for 
                  1 half-pad coords as well                        (D.Casper)
    TRNCON : Read TNRN from database                               (M.Cattaneo)
    TWIREZ : Protect vs. ABS(B1) .GT. RPAD                         (M.Cattaneo)
             Do not apply Landau to coords with half-pads, since the PH
             response is falling off at the sector edge.           (D.Casper)

 * corr file 303.5
    TFCHEK : New - Check TPC helix parameters for basic sanity  (D.Casper)
    TFITTK : Protect against crazy fits by calling sanity check (D.Casper)
    TFLNK2 : Protect against crazy fits by calling sanity check (D.Casper)

 * corr file 303.4
    TPLANA : Bug - Update IOFFS after calls to WBANK (M.Cattaneo)

 ! JULIA 303
    TGETQT : Restrict tan(lambda) to range +/-3                    (D.Casper)
    TJTOP  : Reverse the order of calls to TPAKCO and TFPCOI       (D.Casper)
    TPK1CO : Recalculate the coordinate errors for pad coordinates (D.Casper)
    TWIREZ : Protect vs. negative VARFIT(3)                        (D.Casper)

 * corr file 302.6
    TPADDX : Add protection for logarithmic correction  (J-Y.Nief,D.Casper)
    TPDELS : Add log. correction to specific ionisation (J-Y.Nief,D.Casper)

 * corr file 302.5
    TWIREZ : Bug fixes - protect against -ve SQRT, initialize EZPAD2 (D.Casper)

 * corr file 302.3
    TFILTJ : Allow errors to be packed into 2 bytes instead of 1, to allow
             accurate refitting.  Scale factors changed accordingly. (D.Casper)
    TPK1CO : Remove requirement that errors fit into a byte, to allow
             scale factors to change and round-off to be reduced     (D.Casper)

 * corr file 302.1
    TWIREZ : Use finer T0 offset correction (D.Casper)

 ! JULIA 302 (Tracking upgrade)
    TCOOR  : Correct z for shaping effects
             Call new alignment routine TCRTRN instead of TCRTRA    (D.Casper)
    TCOORT : Correct subpulses for shaping effects
             Add wire coordinates
             Add TNLC,TNGS,TWNZ steering cards
             Call new alignment routine TCRTRN instead of TCRTRA    (D.Casper)
    TERPAR : Treat wire-corrected coordinates
             Replace TERN bank with TLCE                            (D.Casper)
    TFITHL : Cosmetic changes, add a protection on divide by 0      (D.Casper)
    TFITKF : Replace dummy routine                                  (D.Casper)
             Refit TGFT tracks using Kalman filter
             Remove hits flagged as outliers from coordinate list
             Replace calls to TTGCLR,TRMHIT with FDETCO,FRMHIT      (F.Ranjard)
    TFLNK2 : Treatment of track candidates (2 linked tracks)
             is now consistent with that in TFITTK                  (P.Colrain)
    TFLNKS : Protection added for DPHI > PI                         (P.Colrain)
             Use PI,TWOPI from alcons.h                             (F.Ranjard)
    TFPCOI : Include info on Landau corrections                     (D.Casper)
    TGETQT : **BEWARE** Changed calling sequence
             Support for shaping correction                         (D.Casper)
             Inline code instead of calls to TSHAP1,TSHAP2          (F.Ranjard)
    TINIJO : Default for bank TWPU changed to TRUE                  (D.Casper)
    TPADDX : Improved protection on bad coordinates                 (D.Casper)
    TPCREC : Refit after corrections and removal of filtered coords
             is now default. Switch off with TNFF card              (D.Casper)
    TRNCON : Read constants for pulse-shape correction from 
             TSHP bank on database.                                 (D.Casper)
    TSACOR : Flag coords modified by this routine to avoid
             correcting wire Z                                      (D.Casper)
    TSAWIR : Handle changed calling sequence of TGETQT
             Fix grotesque bugs in field/alignment correction
             Call new alignment routine TCRTRN instead of TCRTRA    (D.Casper)
    TSCFIL : Replace algorithm 1 with new code                      (D.Casper)
    TSHPCO.H New. Constants for correction of TPC pad pulses        (D.Casper)
    TWINCO : Handle changed calling sequence of TGETQT
             Call new alignment routine TCRTRN instead of TCRTRA    (D.Casper)
    TWIREZ : New. Process Landau corrections and wire Z coordinates (D.Casper)
             Inline code instead of call to TFUNL                   (F.Ranjard)

 * corr file 285.1
    TSUMVD : Put TSVDWB COMMON in include file   (M.Cattaneo 4/3/97)
    TVDVEL : Put TSVDWB COMMON in include file   (M.Cattaneo 4/3/97)
    TALIGN.H Removed. Duplicated in Alephlib!!   (M.Cattaneo 4/3/97)
    TSVDWB.H New. Include file for TSVDWB COMMON (M.Cattaneo 4/3/97)
                 
 ! JULIA 284
    TSUMVD : Truncated mean improved to make drift velocity calculation 
             from VDET/gamma-gamma more reliable.
    TVDVEL : Ditto.  (I.Tomalin 09/10/96)

 * corr file 281.2
   TVDVEL : More protections against small statistics (M.C, I.T. July 96)

 ! JULIA 280
   TFITHL : correct documentation: XF,YF,WF double precision (P.Rensing,Jan 96)
   TVDVEL : avoid dividing by zero when just one event is processed
           (M.Cattaneo, I.Tomalin, Feb 96)

 * corr file 279.2
   TPDELS : bug that made track 1 lose its pad dE/dx info fixed (D.Casper,
           Dec 95)
   TPCREC,PADDEDX : call ALTIME only if TPTI card used (O. Callot, Dec 95)
   TPCT0  : aesthetic changes required by LIGHT (P.Comas, Jan 96)
 * corr file 279.1
   TPADFX : remove comments after *CA for LIGHT (P. Comas, Nov 95)
 ! JULIA 279
   TPDXJJ, TPADCL, TPLSJJ, TPXSJJ, TP1XJJ, PTPXJJ :
            new comdecks added for pad dE/dx (D. Casper, Nov 95)
   TPADDX : store subcluster pulseheights in TPDX for measurement
           of pad dE/dx (D. Casper, Nov 95)
   TCQGS3 : compute coordinate charge assuming Gaussian, based on
           TCOGS3 (D. Casper, Nov 95)
   TPDELS : create TPXS bank of truncated means for pad dE/dx
           (D. Casper, Nov 95)
   TFITKF : dummy routine to replace TFITKF (D. Casper, Oct 95)
   TWIREZ : dummy routine to replace TWIREZ (D. Casper, Oct 95)
   FTPCER : get wire correction flag (D. Casper, Jun 95)
   TMONIT, TPTIME, TSTATE : pad dE/dx variables added
                           (D. Casper, Nov 95)
   TACCMN, TCOALG, TCOOR, TCOORT: modified to allow inclusion of
                                 pad dE/dx (D. Casper, Jun 95)
   TERPAR : new argument added for the eventual implementation of
           Landau correction for TPC coordinates (D. Casper, Nov 95)
   TINIJO : add pad dE/dx monitoring (D. Casper, Nov 95)
   TKSGDP : add pad dE/dx info (D. Casper, Nov 95)
   TPAKTX : add pad dE/dx banks (D. Casper, Nov 95)
   TPCREC : calculate dE/dx from pad pulses, refit after corrections
           and remove filtered hits from cord list (D. Casper, Nov 95)
   TPRSUM : add pad dE/dx statistics (D. Casper, Nov 95)
   TRNCON : read pad dE/dx constants, book histos (D. Casper, Oct 95)
   TSAWIR : support for pad dE/dx in TCOALG (D. Casper, Jun 95)

 * corr file 278.4
   TSUMVD : new routine to fill TSVD bank with VDET z residuals
           w.r.t. TPC tracks (I. Tomalin, Nov 95)
   TPCRUN : add call to TVDVEL (I. Tomalin, Nov 95)
   TVDVEL : new routine to create TVVD bank and fill it with TPC drift
           velocity calculated using VDET residuals (I. Tomalin, Nov 95)
   TVVDJJ : new comdeck added (I. Tomalin, Nov 95)
 * corr file 278.1
   TFLNK2 : protect against straight tracks (I. Tomalin, Oct 95)
   TRKELS, TWIRES : move DATA statement just before BMACRO to allow
           compilation in Linox (P. Comas, Oct 95)
 ! JULIA 278

 * corr file 277.2
   TRNCON : call RERROR if no TSOR bank is present (I. Tomalin,
           Jun 95)
 ! JULIA 277
   TPCBUN : Improved (I. Tomalin, May 95)
   TPCT0N : Improved to reduce sensitivity to V0's (I. Tomalin,
           May 95)

 * corr file 276.4
   TPCBUN : new routine to determine TPC bunch number and set the
           quality flag (M.Girone, I.Tomalin, Apr 95)
   TPCT0N : modified from TPCT0 to include error on DT  (M.Girone,
           I.Tomalin, Apr 95)
   TPCREC : calculate TPC bunch number (I.Tomalin, Apr 95)
   TPREDA : correct T0 for bunch trains (I.Tomalin, Apr 95)
   TRNCON,TPCCON : added TVOFS0 and TPCCLK to common TPCCON to cope
           with bunch trains (I.Tomalin, Apr 95)
 ! JULIA 276

 * corr file 275.5
   TCODRV : drop work banks at the end (D.Casper, Feb 95)
 ! JULIA 275
 * corr file 274.5
   see comment on 274.4
 * corr file 274.4
   TRNCON : wire pedestal is set to 0 in absence of TCLB or TSIM bank
            for runs >= 28834. From now on wire pedestals are substracted
            online. Pad pedestals are set to 4. TCLB bank does not
            contain wire pedestals since run 28834 and must be ignored.
            JULIA 274.4 contained a first version of this change and in
            FALCON, JULIA 274.4 contained a correction to RLOOPR. To
            avoid confusions, both are released as 274.5 to avoid mixture
            of versions.
 * corr file 274.2
   TOERJJ : update HAC parameters (G. Ganis)
   TCPTST : fill TOER bank extended with 5 more words (G. Ganis)
   TSAWTK : protect code to prevent JULIA crashes in '93 MC production
            on ALPHAs (C. Georgiopoulos)
 ! JULIA 274
   TMONIT : add 36 words in the common block GAINTM(LTSECT)
   TACCMN : get Sector to Sector calibration information ...
            and store it in TMONIT (Z.Feng)
            fill one histogram per sector
            Store this tracks dE/dX value into the monitoring
            common blocks (TMONIT) if this track is a Mip.(Z.Feng)
            fill one histogram
   TDXMON : obsolete deck, purge it.
   TINIRU : reset /TMONIT/ DEDX_CALibration counters at start of
            run.
   TPCRUN : remove fit of DEDX,
            book, give a format and fill JHDX bank.
            JHDX has LTSECT+1 rows, 1 row per sector plus 1 row for
            the overall gain. The number of columns is the number of
            bins of the histograms.
   TRNCON : remove call to TDEDXV.

 ! JULIA 273
   TCPTST - handle new TOER error bank (G.Ganis).
 * corr file 272.2
   TRNCON : set T0=0. for TPCSIM version number < 218
   TFITTK : requires at least 3 coords. (I.Tomalin)
   TOVRLP : remove bug (I.Tomalin)
 * corr file 272.1 modified in 272.2
   TRNCON : with MC data when TPCSIM version # is > 217
            use the true T0 from T0GL, do not set it to 0.

 ! JULIA 272
   TPCRUN - return if MC data (F.Ranjard).
   TFCAND - get track parameters errors from the diagonal of the
            covariance matrix (I.Tomalin).
   TCLDMP, TPRPOJ - enlarge formats (I.Tomalin).
 * corr file 271.9
   TPCRUN : return if MC data
 * corr file 271.6
   TPCRUN : expand fit interval.

