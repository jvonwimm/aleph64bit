C! 1st entry in T_set
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
   TFITHL : correct documentation: XF,YF,WF double precision (P.Rensing,
           Jan 96)
   TVDVEL : avoid dividing by zero when just one event is processed
           (M.Cattaneo, I.Tomalin, Feb 96)

 * corr file 279.2
   TPDELS : bug that made track 1 lose its pad dE/dx info fixed (D.Casper,
           Dec 95)
   TPCREC,PADDEDX : call ALTIME only if TPTI card used (O. Callot, Dec 96)
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

