#if defined(DOC)
   on AXP/VMS:
        source:  JUL310:[*]*.F,JUL310:[INC]*.h
        library: JUL:JUL310.OLB, _D.OLB, .NEWS
        program: JUL:JUL310.EXE, _D.EXE


   on UNIX :  source:    $ALROOT/jul310/?_
              library:   $ALEPH/jul/libjul310.a  _dbx.a
              program:   $ALEPH/jul/jul310.1

 -----------------------------------------------------------------------------
 * 19991110 corr file 310.1

            In e_news.h
    EFIXI99: Drop run header bank EDDB at start of 1999 (M.-N.Minard)

! 19991027 JULIA 31.0

            In e_news.h
    EFOLMK,ESVETO : Increase energy cut to 250 GeV  (M.-N.Minard)

            In f_news.h
    FCFRID : Set particle probabilities to 0.0 if they are smaller than 1.E-30
                                                                  (M.Cattaneo)
    FLV0DX : Call TPDHYP instead of obsolete TIDHYP (M.Cattaneo)

            In f_news.h
    LBHCOR : Double precision fixes for Linux       (D.Smith)

            In o_news.h
    OCBPRE : Double precision fixes for Linux       (D.Smith)

            In r_news.h
    RMONIT : Increase max PMOM to 150 GeV           (M.Cattaneo)

            In s_news.h
    SICLUS : Apply better calibration in delayed timing mode    (B.Bloch)
    SIEPCR : Introduce bunch dependent phi modulation           (B.Bloch)
    SIINIRU: Load SCBP instead of SCPH when runnning in HE mode (B.Bloch)
    SIPEDFX: Correct wrong pedestals in runs 50118-50122        (B.Bloch) 
    SICLC2,SRFIT0,SRFIT2,SRSELO,SRUPAR : 
             Double precision fixes for Linux                   (D.Smith)

            In t_news.h
    TFITHL,TMSHEL,TRHLX2,TWIREZ : Double precision fixes for Linux (D.Smith)

            In u_news.h
    UFITMS,UMXINV,UVPFIT,UVPMAT,UXSFIT,UXSMAT :
             Double precision fixes for Linux       (D.Smith)

            In v_news.h
    VDKLCP : Bug fix - Modification of the logic of the VKIL bank to 
                       delete the correct hits.
             Add protections for garbage collection. (P.Coyle, T.Kachelhoffer)
    VDKMCE : Add protections for garbage collection. (T.Kachelhoffer)
    VSPXYZ : Bug fix - Apply VRECOR correction only to real data
                                                     (P.Coyle, T.Kachelhoffer)

            In y_news.h
    YKVDET : Double precision fixes for Linux       (D.Smith)

 -----------------------------------------------------------------------------
 * 19990518 corr file 309.1
            In r_news.h
    RFJCON : Use clock frequency in Hz from TSOR bank (exists from run 49394)
                                                            (W.Wiedenmann)

            In t_news.h
    TRNCON : Read    clock frequency in Hz from TSOR bank (W.Wiedenmann)
    TSORDP : Display clock frequency in Hz from TSOR bank (W.Wiedenmann)

! 19990426 JULIA 30.9

  New version for 1999 data taking.

            In r_news.h
    RDEFIO : Add banks KWGT,SIID,BLUM to POT                (M.Cattaneo)
    RFORMA : Define SIID format                             (B.Bloch)
    RPOTCL : Drop BLUM from POT for all but random triggers (M.Cattaneo)

            In s_news.h
    SICLEAN: Extend background identification to other types,
             create and fill SIID  bank                       (B.Bloch)
    SICRPOT: Update pointer to PECO bank in SIID. 
             All clusters are transmitted                     (B.Bloch)
    SILUMI : Update pointer to SILU bank in SIID. Only valid
             clusters are considered to build the SILU bank   (B.Bloch)
    SICLUS : Move call to SICLEAN to the end                  (B.Bloch)

            In t_news.h
    TWIREZ : Bug fix - use IROWP instead of IROW (uninitialised variable) in 
             definition of IROWS. Screwed up check on whether a coordinate
             includes half-pads.                                 (D.Casper)

 -----------------------------------------------------------------------------
 * 981202 corr file 308.6
            In r_news.h
    RECEVT : Remove TPC hit smearing (A.Bonissent)

 * 981118 corr file 308.5
            In t_news.h
    TPDELS : Change value of logarithmic correction coefficient (J-Y.Nief)

 * 981116 corr file 308.4
            In t_news.h
    TPADDX : Bug fix - consider a large pad crossing angle (F.Palla)

 * 981109 corr file 308.3
            In b_news.h
    BCALJU : New - Create BCPO (BCAL POT bank) (P.Morawitz,G.Merino,M.Cattaneo)
    BCAREC : Obsolete. Will be removed from library in JULIA 309
    BCFILE : New - Translate BCAL raw data from ADC to Module mapping
                                               (P.Morawitz,G.Merino,M.Cattaneo)
    BCGETE : New - Read in BCAL raw data event (P.Morawitz,G.Merino,M.Cattaneo)

            In l_news.h
    LPREMC : Remove offline pedestal follower after run 45000 (P.Hansen)

            In r_news.h
    RDEFIO : New bank BCPO to POT, remove BLCT (can be made from BCPO)
                                                      (M.Cattaneo,P.Morawitz)
    RECEVT : Replace call to BCAREC (obsolete) with call to BCALJU (M.Cattaneo)

            In r_news.h
    SIPEDFX: New - Fix pedestal of specific runs          (B.Bloch)
    SIPREDA: Call SIPEDFX to fix pedestal problem if any  (B. Bloch)

 * 981014 corr file 308.2
            In e_news.h
    EPRTPC : Protect against divide by zero (M.Cattaneo)

 * 980928 corr file 308.1
            In r_news.h
    RECEVT : Bug fix: VDYEAR was declared as LOGICAL (A.Waananen)

            In t_news.h
    TMSHEL : Protect against divide by zero when DENOM=0.0, again (M.Cattaneo)

! 980914 JULIA 30.8
            In a_news.
    AAMAIN : Change TIMEST value to get better timing resolution (O.Callot)

            In r_news.h
    RLOOPR : Modify format statement (M.Cattaneo)

            In v_news.h
    Following 4 routines implement chip specific efficiencies in MonteCarlo
    for VDET95. Needs ADBS 240, Alephlib 308.3 or greater   (M.Thulasidas)
    VDKLCP : Introduced the new bank VDCE instead of the VDEM bank.
             (VDet Chip Efficiency map)
    VDMCEF : Move the VDKLCP section before the test on NOTDO
    VDMKCE : NEW - Makes the Chip efficiency map file VDCE from VDPR and VDCM
    VINIRU : Add VDPR, VDCM to list of banks to be loaded with the setup code

    VSPXYZ : Correct coordinate for additional bonding problems (VRECOR)
                                                                  (J.Rothberg)

 -----------------------------------------------------------------------------
 * 980824 corr file 307.4
            In r_news.h
    RLOOPR : Print also the processing time of the previous event (M.Cattaneo)

            In t_news.h
    The changes to TFTWTB and TRKELS fix the problem with dE/dx in the 
    reprocessing of 1991 data.
    TFTWTB : suppress missed hits if they will not be used           (D.Casper)
    TRKELS : ensure that zero charge samples remain zero after correction 
                                                                     (D.Casper)
    TTWODX : Bug fix: bank T2XS was not being extended a second time if 
             necessary                                             (M.Cattaneo)

 * 980708 corr file 307.3
            In r_news.h
    RECEVT : Change defaults for VGLOB. If VDYEAR < 95, VGLOB is
             default, otherwise not. Steering card VGLO forces VGLOB on
             NVGL forces VGLOB off                             (M.Cattaneo)

            In v_news.h
    VPRSUM : Improve test on VGLOB execution                   (M.Cattaneo)

 * 980608 corr file 307.2
            In s_news.h
    SICLEAN: NEW. Checks for quality flag of SiCAL cluster     (B.Bloch)
    SICLUS : Extend LMAXL to 400, 
             Compute quality flag for each cluster             (B.Bloch)
    SICRPOT: Exclude from PECO/PCRL clusters identified as
             electronic noise                                  (B.Bloch)
    SIINIRU: Load SNOI bank from database                      (B.Bloch)
    SILUMI : Skip identified electronic noise                  (B.Bloch)
    SIPREDA: Apply bunch dependent calibration also after 1997 (B.Bloch)

 * 980528 corr file 307.1
            In h_news.h
    HCORFC : NEW. HCAL tower energy correction for the bug that was 
             present on the online software in 1996, 1997 and early 1998 data
                                                                   (A.Sciaba)
    HPRANA : Add call to HCORFC                                    (A.Sciaba)

            In m_news.h
    MPREDG : If max no. of hits reached, fill MHIT up to max no. of hits
             rather than return empty MHIT                         (G.Bagliesi)

! 980502 JULIA 30.7
    This version will be used in Falcon for the 1998 data

            In e_news.h
    ECSW94 : EXIT JULIA if EAGC cards are missing when required
             (1994 raw data, run range 25000-25893)                (M.Cattaneo)

            In t_news.h
    TPREDA : Force use of unreduced wire data (raw data banks TWIR/TWDI)
             in run range 10000-11800 (1991)          (W.Wiedenmann,M.Cattaneo)

            In v_news.h
    VPREDA : Skip VDET reconstruction if VDOK (HV) not on         (A.Bonissent)

 * 980417 corr file 306.3
            In r_news.h
    RDEFIO : New bank BCHG to POT     (M.Cattaneo)

            In v_news.h
    VDECOD : Protect against corrupted raw data (A.Bonissent)

 * 980401 corr file 306.2
            In r_news.h
    RDEFIO : Banks FSHO, KMAR, KWTK to POT (M.Cattaneo)

 * 980324 corr file 306.1
            In t_news.h
    TWIREZ : Up to 1991 the banks (TSIR,TSDI,TSLE) 
             were called (TRIR,TRDI,TRLE)          (W.Wiedenmann)

! 980226 JULIA 30.6
    This version reimplements the TPC hit smearing for MonteCarlo. FRFT/0 and
    FRFT/2 contain the track parameters after refit with the smeared hits but 
    keeping the error matrix from the unsmeared fit. This gives the best 
    agreement between data and MonteCarlo. The track fit using the unsmeared 
    hits is still available in FRFT/10, FRFT/12. Note that the V0s, kinks etc.
    are done with the  smeared tracks.

            In r_news.h
    RECEVT : Move TPC hit smearing to after track fit and
             refit tracks with smeared hits (MonteCarlo only)  (A.Bonissent)

            In t_news.h
    TPSMR1 : Change the Z smearing proportional to the drift distance
             to an average smearing. Fixes cos(theta) dependence (Thulasidas)
    TREFIT : NEW Refit frft 2 tracks after monte carlo smearing (A.Bonissent) 
    TSMFT  : NEW Steering to smear and refit frft tracks in MC  (A.Bonissent)

 -----------------------------------------------------------------------------
 * 980226 corr file 305.4
            In s_news.h
    SICREC : Skip if setup code is zero (< 9209)                   (M.Cattaneo)

            In u_news.h
    UFITMS : Tighten numerical checks                              (D.Casper)
             Bring phi back into 0-2*pi range after Newtonian step (M.Cattaneo)

 * 980209 corr file 305.3
            In t_news.h
    TFITHL : Bug fix: Protect against arg SIN > 1.0 (M.Cattaneo)
    TFLNKS : Fix typo in "long-range" code. 
             Select best link, not first....        (D.Casper)
    TOVRLP : Changed scale factor in ITRKSW(2) to 10000 
             to allow for events with > 999 tracks  (I.Tomalin)
             Prevent track candidates from winding up with more 
             than one hit in a given padrow.        (D.Casper)

            In u_news.h
    UCTGFT : Test error return of UFITMS                          (D.Casper)
    UFITMS : Make sure all tracks have sensible covariance matrix (D.Casper)
             Fix precision problem for GG0P variable            (M.Cattaneo)

 * 971209 corr file 305.2
            In e_news.h
    ECLAMP : Rescale cut to effective threshold. Affects MC only (M.N.Minard)
    ECMOD  : Bug fix for overlap region (G.Taylor)

            In r_news.h
    RECEVT : Add TPC hit smearing (M.Thulasidas)

            In s_news.h
    SIINIRU: Suppress error message for missing SECT when setup<7  (M.Cattaneo)

            In t_news.h
    Add TPC smearing routines for MonteCarlo
    TPDIAG : NEW Finds eigen values and vectors from cov. mat.    (Thulasidas)
    TPHSMR : NEW smears the TPCO coordinates in accordance with the 
                 database bank TPSM for Monte Carlo events        (Thulasidas)
    TPRNDM : NEW generates random vars. from eigen values/vectors (Thulasidas)
    TPSMGT : NEW Used to read TPSM bank from database             (Thulasidas)
    TPSMR1 : NEW adds the smearing vector to the TPCO coordinates (Thulasidas)

 * 971205 corr file 305.1
            In l_news.h
    LHVSTA : use XLUMOK from ALEPHLIB (version>306)  (B.Bloch)

            In o_news.h
    OBSPOT : Call xvdeok instead of VDETOK (Needs Alephlib>306) (M.Cattaneo)

            In r_news.h
    RLOOPR : Fix format statement when event length > 1Mbyte (M.Cattaneo)
    RREVHE : Use xlumok from Alephlib (version>306)          (B.Bloch)

            In s_news.h
    SICAOK : use XLUMOK from alephlib (>306) for complete checks   (B.Bloch)

            In t_news.h
    TMSHEL : Protect against divide by zero when DENOM=0.0 (M.Cattaneo)

            In v_news.h
    VBSPOT : Call xvdeok (from Alephlib > 306) instead of VDETOK (M.Cattaneo)
    VDETOK : Obsolete. Use XVDEOK instead (Alephlib version>306) (M.Cattaneo)

! 971106 JULIA 30.5

            In a_news.h
    AJMMCL : fix variable type inconsistencies in function calls,
             for Linux                                           (A.Waananen)

            In b_news.h
    BCAREC : New - Create and fill BLCT bank  (G.Boix, I.C.Park, M.Cattaneo)

            In c_news.h
    CRCHRL : fix variable type inconsistencies in function calls,
             for Linux                                           (A.Waananen)

            In e_news.h
    EINIRU : Remove 'Temporary' (since before 1993!) call to EFIX  (M.Cattaneo)
    EFIX   : DELETED from library (was supposed to be temporary fix pending
             Alephlib fix, and did not have intended effect)       (M.Cattaneo)
    ETHRES : Rename 'LOCAL' common for work bank indices to 
             something more unique (CMETHR)                        (M.Cattaneo)
    E4BARY,E4FDIJ,EFIJOB : fix variable type inconsistencies in 
                           function calls, for Linux               (A.Waananen)

            In f_news.h
    FPIDEN : Obsolete, replaced by FCFRID                         (M.Cattaneo)
    FCFRID : Same as FPIDEN, but with option to do bit patterns for
             tracks only. Reaccess FRFT if garbage collection occurs 
             when creating FRID                          (D.Casper,M.Cattaneo)

            In g_news.h
    GASTEER : fix variable type inconsistencies in function calls, 
              for Linux                                           (A.Waananen)

            In h_news.h
    HCCONS.H : COMMON block is also defined in Alephlib with longer
               length. COPYed Alephlib definition to JULIA        (M.Cattaneo)
    HCLCRA : fix variable type inconsistencies in function calls, 
             for Linux                                            (A.Waananen)

            In i_news.h
    ITEXTN : fix variable type inconsistencies in function calls, 
             for Linux                                            (A.Waananen)

            In l_news.h
    LENLIS,LSELBA : fix variable type inconsistencies in 
                    function calls, for Linux                  (A.Waananen)
    LUPACR : Store X1RG, XTPB, XTCN, SFTR name indices locally (M.Cattaneo)
    LUPAIN : Suppress filling of x1namc COMMON due to clash with
             Alephlib in definition of same common             (M.Cattaneo)

            In r_news.h
    RDEFIO : New bank BLCT to POT     (M.Cattaneo)
    RECEVT : Call BCAL reconstruction (M.Cattaneo)
             Create track hit bit patters for secondary vtx. search (D.Casper)
    RLOOPR,RPRSUM,RREVEH,RUNKNO,RWREVT: fix variable type 
             inconsistencies in function calls, for Linux         (A.Waananen)

            In s_news.h
    SIBUN.H: New common block                                      (B.Bloch)
    SICLUS : In 1997 correct for time dependent gain if not 
             already done in SIPREDA                               (B.Bloch)
    SIINIRU: Load SECT bank                                        (B.Bloch)
    SIPREDA: Apply a bunch dependant calibration for delayed 
             timimg in 1997                                        (B.Bloch)
    SILHCR : Store XTBP,XTCN,X1SC name indices locally          (M.Cattaneo)
    STMASK,STRIGF : Deleted from Julia library. Identical routines
                    exist in Alephlib                           (M.Cattaneo)

            In t_news.h
    TSACOR : Bug fix: NLINK was called with TPCO bank index
             rather than name                               (A.Waananen)
    TLIPAK : fix variable type inconsistencies in function calls, 
             for Linux                                            (A.Waananen)

            In u_news.h
    UFITMS : fix variable type inconsistencies in function calls, 
             for Linux                                            (A.Waananen)

            In v_news.h
    VDECOD,VPREDA : fix variable type inconsistencies in 
                    function calls, for Linux                  (A.Waananen)

            In vglobnews.h
    VFTALL,VGBRAN,VGBRUT,VGFEVT,VGTCUT,VMCHIP,VMCLLD : fix variable 
             type inconsistencies in function calls, for Linux    (A.Waananen)

            In x_news.h
    X1NAMC.H : Deleted. COMMON block is also defined in Alephlib with 
               DIFFERENT variables                          (M.Cattaneo)

            In y_news.h
    YKSFIT : fix variable type inconsistencies in function calls, 
             for Linux                                            (A.Waananen)
 -----------------------------------------------------------------------------
 * 971027 corr file 304.3
            In t_news.h
    TPREDA : Correct 1997 T0 for problems (I.Tomalin)

 * 971017 corr file 304.2
            In t_news.h
    TPAKCO : Create PTNC even if FRTL and FTCL are missing (M.Cattaneo)
            In u_news.h
    UFITMS : Protect against divide by zero (M.Cattaneo)

 * 971001 corr file 304.1
            In e_news.h
    EBOXHI : Remove CALL from function references, for Linux. (A.Waananen)
            In f_news.h
    FITALL,FREFIT:Remove CALL from function references, for Linux. (A.Waananen)
            In r_news.h
    RTLSUM,RWRUNH:Remove CALL from function references, for Linux. (A.Waananen)
            In t_news.h
    TFITKF,TOVRLP,TSUMVD,TWIREZ :
                  Remove CALL from function references, for Linux. (A.Waananen)
            In v_news.h
    VDECOD,VDFSUP,VDRTAN,VHINIT,VSPXYZ :
                  Remove CALL from function references, for Linux. (A.Waananen)
            In y_news.h
    YKINIT : Remove CALL from function references, for Linux. (A.Waananen)

! 970819 JULIA 30.4
            In h_news.h
    HVMASK : Get database from LRGEOM rather than LRCONS (A.Sciaba')

            In l_news.h
    LPREMC : Resurrection of dead wires at storey 2/3 interface corrected,
             Storey energy redistributed in case of dead wires
             Suppress negative energies                          (P.Hansen)
    LPRLUM : Compensate for dead module in early 1997 running    (P.Hansen)

            In o_news.h
    OLSPOT : Remove apostrophes from strings passed to RERROR (D.Casper)

            In r_news.h
    RDEFIO : Add YSMO to POT                      (D.Casper)
    RFORMA : Add YSMO, correct YKNK,YSVT formats  (D.Casper)

            In s_news.h
    SIINIRU,SIINIJO : Move booking of SSTA internal bank from siiniru to
                      siinijo, to avoid problems when processing multiple
                      runs with different bunch train setups.   (M.Cattaneo)

            In t_news.h
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

            In v_news.h
    VDECOD : Protect against corrupted raw data (A.Bonissent, M.Cattaneo)

            In vglobnews.h
    VFTALL : Tracks identified as originating at a secondary vertex 
             have their errors and energy loss propagated only to the 
             corresponding point.                                  (D.Casper)

            In y_news.h
    YKFFIT : Suppress Kalman filter error message               (D.Casper)
    YKSRCH : Allow more than one bit to be set in the hypothesis mask, 
             if candidate passes more than one cut; if is INCOMING
             to two kink candidates, save only the best candidate 
             (smallest chi^2) to avoid confusing DALI, ENFLW, etc.  
             Also drop output bank at entry, if present.         (D.Casper)
 -----------------------------------------------------------------------------
 * 970718 corr file 303.7
            In a_news.h
    ALGTWA : Run 29977 also had bunch trains             (M.Cattaneo)

   	    In l_news.h
    LINIRU : Enable taking LDST from start of run record (P.Hansen,M.Cattaneo)

   	    In v_news.h
    VDJACB : Fix the missing JACOB(3) bug (in IV=2) (M.Thulasidas)

   	    In vglobnews.h
    VFTALL : Do not generate "Fatal" error if no track banks found (D.Casper)

 * 970716 corr file 303.6
   	    In e_news.h
    EPRTPC : Comment call to UFITQL (results are never used!!) (M.Cattaneo)

 * 970710 corr file 303.5
   	    In r_news.h
    RINCND : Protect against zero magnetic field: Set FIELRC to 0.0001
             if ALFIEL returns 0., produce a "fatal" error        (M.Cattaneo)

   	    In t_news.h
    TFCHEK : New - Check TPC helix parameters for basic sanity  (D.Casper)
    TFITTK : Protect against crazy fits by calling sanity check (D.Casper)
    TFLNK2 : Protect against crazy fits by calling sanity check (D.Casper)

   	    In u_news.h
    UCTGFT : Protect against crazy fits by calling sanity check (D.Casper)

 * 970707 corr file 303.4
   	    In l_news.h
    LIDCOD : Bug - ref. energy for method<9, run>40000 was wrong (P.Hansen)
    LSELBH : Bug - ref. energy for method<9, run>40000 was wrong (P.Hansen)

   	    In h_news.h
    HVMASK : Get run number with ABRUEV rather than from rcurnt.h  (A.Sciaba)

   	    In t_news.h
    TPLANA : Bug - Update IOFFS after calls to WBANK (M.Cattaneo)

 * 970616 corr file 303.3
   	    In l_news.h
    LCNAMC.H Add name index for BHZ0 bank                 (P.H.Hansen)
    LBHCOR : Use BHZ0 bank for LEP1 runs during LEP2 era  (P.H.Hansen)
    LBINDX : Initialize name index for BHZ0 bank          (P.H.Hansen)
    LIDCOD : Use BHZ0 bank for LEP1 runs during LEP2 era  (P.H.Hansen)
    LINIRU : Get BHZ0 bank from database                  (M.Cattaneo)
    LPRLUM : Use BHZ0 bank for LEP1 runs during LEP2 era  (P.H.Hansen)

   	    In vglobnews.h
    VGLINK : Create FXTR if it doesn't exist; primarily to 
             facilitate ALPHA interface                   (D.Casper)

 * 970609 corr file 303.2
   	    In r_news.h
    RECEVT : Bug fix - RDMIN was called with SEED=0. every time!  (M.Cattaneo)

   	    In y_news.h
    YKSFIT : Check basic quality cuts (minimum distance and #bad hits) 
             here rather than in calling routine, to avoid asking YTOP 
             for impossible fits                                 (D.Casper)
    YKSRCH : Remove basic quality cuts                           (D.Casper)

 * 970603 corr file 303.1
   	    In r_news.h
    RECEVT : Add call to RDMIN to initialise RANNOR (and RNDM) random
             numbers once per event.                              (F.Ranjard)

            In v_news.h
    VDOSMR : Remove call to NORRIN, replace RANNOR by VDGAUS      (M.Cattaneo)
    VDISMR,VDOSMR,VDWSMR : replace WRITE(6,*) by WRITE(IW(6),*)   (M.Cattaneo)
    VDXYZT : Replace call exit() by RERROR,
             Move 999 label to include BDROP of work banks        (M.Cattaneo)

! 970514 JULIA 30.3

   	    In f_news.h
    FITALL : Bug fix: KENKF was always 0 (not initialised)         (D.Casper)

   	    In h_news.h
    HCMASK.H New - COMMON containing MASKHV array            (A.Sciaba)
    HINIRU : Add call to HVMASK                              (A.Sciaba)
    HVMASK : New - Fills MASKHV with HV masking information from HSSR 
                   Slow Control bank                         (A.Sciaba)
    HMROAD : Take into account info in MASKHV when calculating expected
             number of hits in HCAL                          (A.Sciaba)

   	    In r_news.h
    RCPAS0, RIPAS0 : If PVGG card present, PASS0 drift velocity is taken
             from VDET/gamma-gamma for all runs taken with new VDET 
             (and not just LEP2 runs)                              (I.Tomalin)
    RDEFIO : Add new banks BCSC,BCSL,BCTR to output list          (M.Cattaneo)
    RDETEC : In PASS0 mode, force YNSV,YNLV,YNKF,TNPX,ENKF,VNSL   (M.Cattaneo)
             In PASS0 mode, switch on use of TPC wires by default (I.Tomalin) 
    RECEVT : Call VDET global pattern recognition if VGLO card found (D.Casper)
             Moved call to TSUMVD from RECONS to RECEVT           (I.Tomalin)
    RECONS : Moved call to TSUMVD from RECONS to RECEVT           (I.Tomalin)

   	    In t_news.h
    TGETQT : Restrict tan(lambda) to range +/-3                    (D.Casper)
    TJTOP  : Reverse the order of calls to TPAKCO and TFPCOI       (D.Casper)
    TPK1CO : Recalculate the coordinate errors for pad coordinates (D.Casper)
    TWIREZ : Protect vs. negative VARFIT(3)                        (D.Casper)

            In u_news.h
    UFITMS : Disable Newtonian for 3-hit tracks (D.Casper)

   	    In v_news.h
    VDRCUN, VINIRU : Mods to PASS0 to allow drift velocity to be taken for
             all data taken with new VDET (and not just LEP2 runs) (I.Tomalin)
    VPRSUM : Print end of job statistics for VGLOB code (P.Rensing)

   	    In vglobnews.h
    New package in JULIA to do VDET global pattern recognition in the context
    of the tracking upgrade. The package is called as an option in JULIA, by
    adding the steering card VGLO.

    IMPORTANT NOTE. This software is currently not officially supported in
    Aleph, since its authors (P.Rensing, J-F.Pusztaszeri) have left and noone
    has been found to take over responsibility.

   	    In y_news.h
    YKSFIT : Bug fix - do not call YFTVTC with identical input
             and output vertex arguments                       (M.Cattaneo)

 -----------------------------------------------------------------------------
 * corr file 302.6
   	    In t_news.h
    TPADDX : Add protection for logarithmic correction  (J-Y.Nief,D.Casper)
    TPDELS : Add log. correction to specific ionisation (J-Y.Nief,D.Casper)

   	    In y_news.h
    YKFFIT : Add protections against negative SQRT               (D.Casper)
    YKSRCH : Store a record of what mass cuts are passed in YKNK (D.Casper)

 * corr file 302.5
    In this correction file we implement the kinks package by P.Rensing
    with minor corrections by D.Casper

   	    In r_news.h
    RVDPRE : Protect against garbage collection in called routines (F.Ranjard)

   	    In t_news.h
    TWIREZ : Bug fixes - protect against -ve SQRT, initialize EZPAD2 (D.Casper)

   	    In v_news.h
    VDMSUP2: New. Update face extrapolation bank VDMS for true VDET
             hit position. Based on VDMSUP from ALEPHLIB. Used by
             kinks package (and VGLOB)

   	    In y_news.h
    YKNKCM.H New - Common blocks for kinks
    YKNKPT.H New - Common block for kinks
    YKFFIT : New - Full fit of two tracks to seach for a kink
    YKINIT : New - Initialize the cuts used in the Kink analysis
    YKLDC  : New - Load kink coordinate arrays for an FRFT track
    YKNEM2 : New - Find the mass^2 of a kink's neutral daughter
    YKSFIT : New - Simple fit of two tracks to seach for a kink
    YKSRCH : Main Kink search routine, replaces dummy routine from JULIA 302
    YKVDET : New - Look for free VDET hits which match the inner track
             of a potential kink. This routine is inserted in the library
             for completenes but is never called because it relies on VGLOB.
             The code calling it (in YKSRCH) is commented out.

 * corr file 302.4
   	    In v_news.h
    VRECLT : Bug fix. Do not reduce clusters of < 2 strips (A.Bonissent 020497)

 * corr file 302.3
   	    In t_news.h
    TFILTJ : Allow errors to be packed into 2 bytes instead of 1, to allow
             accurate refitting.  Scale factors changed accordingly. (D.Casper)
    TPK1CO : Remove requirement that errors fit into a byte, to allow
             scale factors to change and round-off to be reduced     (D.Casper)

 * corr file 302.2
   	    In r_news.h
    RDEFIO : Add KXME (Kingal output) to POT      (B.Bloch)
    RFORMA : Add KXME format                      (M.Cattaneo)

   	    In s_news.h
    SIPREDA : Apply bunch train correction only to 1995 data (B.Bloch)

 * corr file 302.1
   	    In r_news.h
    RDEFIO : Add LCRA (LCAL scintillator raw data) to POT      (M.Cattaneo)
    RFORMA : Add LCRA, VDTD formats                            (M.Cattaneo)

   	    In t_news.h
    TWIREZ : Use finer T0 offset correction (D.Casper)

! 970307 JULIA 30.2
   This version is the first official release of the tracking upgrade code.
   It does NOT contain the VGLOB global VDET pattern recognition package, nor
   the kink finding package, both by P.Rensing

   	    In a_news.h
    AAMAIN : Increase length of IW array to 3M words (from 2M) (D.Casper)

   	    In e_news.h
    EPRTPC : Extrapolate to ECAL from outermost TPC coordinate. (D.Casper)
             Do not extrapolate incoming tracks of kink.
             Can be disabled with ENKF card.

   	    In f_news.h
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
             
   	    In i_news.h
    IREFIN : Don't drop old ICCO, to keep valid coordinates (D.Casper)
    ITCINF.H Increase JRESMX from 80 to 200                 (D.Casper)

   	    In r_news.h
    RDEFIO : Tracking upgrade banks added to output list       (D.Casper)
    RECEVT : Call new modules VTRPRE, KINKS, NUCVTX (M.Cattaneo,D.Casper)
    RFORMA : Add PFXT,TRIK,TWZA,TWZZ,VGHC,VGXC,YKNK,YLV0,YNLI,
             YNVH,YS0L,YSTL,YSTC,YSTV,YSVT,YSVX,YSCL formats   (D.Casper)
    RVDPRE : New. Steer VDET track preselection                (M.Cattaneo)
    RPARAC.H Added modules VTRPRE, KINKS, NUCVTX               (M.Cattaneo)
    
   	    In t_news.h
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

   	    In v_news.h
    VTRUTH : Removed from library (replaced by VTRURL in JUL285) (M.Cattaneo)

   	    In y_news.h
    YKSRCH : Main Kink search routine. Dummy for now   (M.Cattaneo)
    	
   	
 * corr file 285.1
        In r_news.h
    RDEFIO : Add PHMH to output list (P.Bright-Thomas 4/3/97)

        In t_news.h
    TSUMVD : Put TSVDWB COMMON in include file   (M.Cattaneo 4/3/97)
    TVDVEL : Put TSVDWB COMMON in include file   (M.Cattaneo 4/3/97)
    TALIGN.H Removed. Duplicated in Alephlib!!   (M.Cattaneo 4/3/97)
    TSVDWB.H New. Include file for TSVDWB COMMON (M.Cattaneo 4/3/97)


        In l_news.h
   LUPACR : Reinstate filling of trigger enable mask in LUPA (M.Cattaneo Jan96)

        In m_news.h
   MUREDO : Moved to Julia library from Alephlib (version 216) because it 
            calls Julia routines (A.Waananen 5/12/96, M.Cattaneo 19/02/97)

        In r_news.h
   RFORMA : Add VSPL bank (A.Bonissent, Dec96)

        In v_news.h
   VDXYZT :     keep track of strips which belong to two clusters
                (after splitting) (A.Bonissent, Dec96)
   VUMCTR, VDMCTR, VDGTHT : handle properly strips which
                belong to two clusters (A.Bonissent, Dec96)
   VTRURL : New routine, gives truth MC relation between hits and tracks
            (A.Bonissent, Dec96). 
            IMPORTANT NOTE: VTRURL supersedes VTRUTH (changed calling sequence)
            VTRUTH is obsolete and will be removed in a future JULIA version.

 * corr file 284.1

        In o_news.h
   OLSPOD : Minor bug fix (O.Schneider, November 1996)
   OLSPOT : Minor bug fix (O.Schneider, November 1996)

        In r_news.h
   RCPAS0 : ALLEP1 declared as logical, not integer (A.Waanenen 16/10/96)
   RIPAS0 : ALLEP1 declared as logical, not integer (A.Waanenen 16/10/96)

        In v_news.h
   VDECOD : Do not check for cable swap in MonteCarlo (PBT, Nov96) 

 -----------------------------------------------------------------------------
! 961009 JULIA 28.4

        In f_news.h
   FITALL : Fix bug which can result in losing association to ECAL cluster for
            track following a curling track in the track list 
            (A.Venturi 10/10/96)

        In m_news.h
   MPREDG : Ignore bits 8-11 in data from new Astros (G.Bagliesi 9-10-96)

        In r_news.h
   RIPAS0 : Detects if PVGG card is being used. (I.Tomalin 9/10/96)
   RCPAS0 : Fill's PASS0 banks with drift velocity from VDET + 
            gamma-gamma events if requested by PVGG card.(I.Tomalin 9/10/96)

        In t_news.h
   TSUMVD.F    : Truncated mean improved to make drift velocity calculation 
                  from VDET/gamma-gamma more reliable.
   TVDVEL.F    : Ditto.  (I.Tomalin 09/10/96)

        In v_news.h
   VINIRU.F : Request creation of VDZT banks during PASS0 if PVGG
              option being used. (I.Tomalin 09/10/96)
   VDRCUN.F : Ditto.             (I.Tomalin 09/10/96)


 * corr file 283.01

        In e_news.h
   ECCLUS : Change level of error 1 from Fatal to Warning

        In o_news.h
    Replace specific intrinsic functions with generic intrinsic functions
    obspot.F,ocbpre.F,ogtblq.F,olspod.F,olspot.F,olspou.F

        In r_news.h
   REPORT : Change message for non-fatal errors (M.Cattaneo 7 October 1996)

 -----------------------------------------------------------------------------
! 960924 - JULIA 28.3

        In g_news.h
   GAPECO : create PECO NR=1 bank also when PECO NR=0 has zero rows 
            (P.Janot 24/9/96)

        In o_news.h
   Upgraded OLSPOT to perform the alignment of the BOMs on a
   run-by-run basis; OLSPOT now makes a beam spot fit (using all the tracks
   in the run) with respect to the BOM+QS0 estimate. A new routine 
   OBSPOT, which is a modified copy of VBSPOT, is called by OLSPOT to do 
   that job. (O.Schneider, September 1996)
     olspot.F updated
     olspou.F updated
     olspod.F updated
     obsinr.F new: modified copy of vbsinr.F
     obspot.F new: modified copy of vbspot.F
     obsclr.F new: modified copy of vbsclr.F
     obmfit.F new: modified copy of vbmfit.F
     ocbfit.F new: modified copy of vcbfit.F
     obscom.h new: modified copy of vbscom.h
     ocbpre.F new: called in ocbfit.F to compute new track parameters
     ogtblq.F new: called in ocbpre.F to get unaligned BOM+QS0 position

        In v_news.h
   Replace expicit BOS macro declaration by #include bmacrod.h (O.Schneider)
   Affects: vbmfit, vbsclr, vbsinr, vbspot, vcbfit, vdosmr

 * corr file 282.01
   Fixes of comments and missing #ifndef DOCs for documentation. Affects
   documentation only.  (M.Cattaneo, 3 September 1996)

 -----------------------------------------------------------------------------
! 960719 - JULIA 28.2
        In e_news.h
   EGETDS : Replace LENOCC by LNBLNK (M.Cattaneo, July 1996)

        In l_news.h
   LINIRU : Force picking up of LDST bank from database (F.Ranjard, July 96)

        In o_news.h
   OLSPOD, OLSPOT, OLSPOU : New routines to get beam spot from LEP boms
                            (O.Schneider, June 1996)

        In r_news.h
   RLUNIT.H : Increase size of output bank list to 200 banks 
              (M.Cattaneo, July 96, requested by D.Casper)
   RCLRUN : Add call to OLSPOT (M.Cattaneo, June 1996)
   RECONS : Add call to OLSPOT (M.Cattaneo, June 1996)
   RFORMA : Add format for BLQP bank (M.Cattaneo, June 1996)
   RINJOB : Add BLQP to SUMLIS (M.Cattaneo, June 1996)
   RLTOCH : Replace LENOCC by LNBLNK (M.Cattaneo, July 1996)
   RSMJOB : Add call to OLSPOT (M.Cattaneo, June 1996)
   RUNKNO : Add call to OLSPOT (M.Cattaneo, June 1996)

 * corr file 281.03
        In l_news.h
   LOLERR : Fix cut against "sparks" at LEP 2 energy (P.H.Hansen 11-Jul-96)

        In s_news.h
   SILUMI : Fix energy cuts for high energy data (B.Bloch 11-Jul-96)

 * corr file 281.02
        In l_news.h
   LHVSTA : Make same event selection as XLUMOK for lumi. (P.H.Hansen July 96)

        In r_news.h
   RERROR : Fix bug: error was not reported on autoinitialising first call 
            (M.Cattaneo, June 96)

        In s_news.h
   SILUMI : Add protection for new acceptance cuts (M.Cattaneo 30-Jun-96)

        In t_news.h
   TVDVEL : More protections against small statistics (M.C, I.T. July 96)

        In v_news.h
     - VDMCEF : move the treatment of the new VDET to VDKLCP. It can not be 
                done in VDMCEF because of multiplexing. (M.Thulasidas)
     - VDKLCP : New routine to take care of the efficiency map for the new
                VDET, making sure that the multiplexed hits get killed
                together. (M.Thulasidas)

        In x_news.h
   XTPENB : Make same event selection as XLUMOK for lumi. (P.H.Hansen July 96)

 * corr file 281.01
        In f_news.h
   FITALL : Fix bug which can result in overwriting BOS when an
            ITC track appears which has added a few TPC coordinates. (D.Casper)

        In o_news.h
   OMCORR : Allow beam energy greater than 50 GeV (O.Schneider)
   OSLOWC : Input data comes from banks LXCR,LXSP for LEP 2 (O.Schneider)
   OINIRU : Call OSLOWC also for run header banks (O.Schneider)

 -----------------------------------------------------------------------------
! 960603 - JULIA 28.1
        In a_news.h
   AAMAIN - Increase max number of BOS banks to 3000
   ABOLDR : remove #include version.h, since we don't have correction files
            with CVS. JULRUN makes sure JULIA version of routine is picked up.

        In f_news.h
   FPIDEN : Bugfix: TPC bits in NDZ start at 10, not 8,
            Remove duplicate call to NAMIND('FRFT')     (MC).

        In l_news.h
   LBDLBA, LSELBH, LTRIGS : replace WRITE(6, by WRITE(IW(6),
                                               (H.Drevermann, Feb 96)
   LBLDBA : Enable debug print                  (P.H. Hansen, May 96)
   LBHCOR : Fix reference energy from BHAB card (P.H. Hansen, May 96)
   LBNKPR : Only print when Lcal clusters exist (P.H. Hansen, May 96)
   LCALIB : Calibration sample is now method 10
            Flexible low edges on energy histos (P.H. Hansen, May 96)
   LCBOOK, LCHIST: Histogram Bhabha, single arm and other trigs
                   Do not use dropped banks from 'E' list
                                                (P.H. Hansen, May 96)
   LELECT : Put LIDT back on 'T' list           (P.H. Hansen, May 96)
   LGNGBX : Use X1RG bank                       (P.H. Hansen, May 96)
   LHVSTA : Use X1RG bank                       (P.H. Hansen, May 96)
   LIDCOD : Fix reference energy from BHAB card (P.H. Hansen, May 96)
   LINIRU : Flexible low edges on energy histos (P.H. Hansen, May 96)
   LPRCAL : Print also expected energy          (P.H. Hansen, May 96)
   LPRLUM : Exact Luminosity calculation (if energy calib is OK):
            #bhabhas = method 10 with delta_phi>150 with HVs on)
            #background = (( -"-      delta_phi<30   -"-       ) -
                           (MC method 10  -"-      ))*
                          (singles combinations with delta_phi>150)/
                          (  -"-                     delta_phi<30)*
            lumi = (#babhas - #background)/((sigma from BHAB card*
                   (correction factor from LBHCOR))
            Nicer end-of-run print, also for print level 2
                                                (P.H. Hansen, May 96)
   LSELBA : Fix reference energy from BHAB card
            Count combinations of single arm triggers without weights
            Drop cut on summed energy to gain statistics
                                                (P.H. Hansen, May 96)
   LSINGL : check that HV is on for single arm triggers
            allow only one Lcal cluster
                                                (P.H. Hansen, May 96)
   LTGBIT : Install a default 1996 trigger bit assignment
                                                (P.H. Hansen, May 96)
   LTRIGS : Form combinations every 10 single arm trigger
                                                (P.H. Hansen, May 96)
   LUPACR : Replace obsolete trigger banks by X1RG and X1TT
                                                (P.H. Hansen, May 96)

        In r_news.h
   RWRUNH : RUNE added to the C list (P.Comas, Mar 96) 

        In v_news.h
   Changes to VDET alignment smearing routines to work with new VDET geometry
   (M.Thulasidas)
   Affects:
     - VDALSG, VDGTXL, VDSIGM, VDWSDX, VDWSMR
   New Routines:
     - VDINCP, VDJACB, VDRTAN

   Changes to MonteCarlo stream, to take care of efficiencies properly
   (M.Thulasidas)
   Affects:
     - VPREDA, VDMCEF
   New routine:
     - VDFSUP (replaces VDMCEF95)

   Rewrite fix for cabling errors using new bank VCAB (P.Bright-Thomas)
   Affects:
     - VDECOD, VINIRU

 * corr file 280.01
        In *CD -> E_NEWS
   E4FNEC : protect against divide by A(1)=0.   (D.Pallin, Mar 96)
   ECGFLW : protect against bad MonteCarlo data (M.Cattaneo, Mar 96)
   EPADCL : protect against bad MonteCarlo data (M.Cattaneo, Mar 96)

 -----------------------------------------------------------------------------
! 960305 - JULIA 28.0
        In *CD -> V_NEWS

   New clustering algorithm for low theta tracks : wafers 2 and 3 of
              1st layer; wafer 3 of 2nd layer (A. Bonissent, Jan 96)
   We exploit the fact that we know approximately the cluster size
      (nstrips) and pulse height.
   Affected : cluster finding and cluster splitting, subroutines :

    - RFORMA : define format for new bank VPHN
    - VDECOD : creat bank VPHN; declare variables types for implicit
              none
    - VDXYZT : declare variables types for implicit none; switch to
              low theta cluster separation when necessary
    - VRECLU : declare variables types for implicit none; switch to
              low theta cluster separation when necessary
    - VSEPAR : declare variables types for implicit none

      New subroutines :

    - VRECLT : find clusters in the low theta region (Z view)
    - VCLBNK : Put clusters in banks
    - VSEPLT : Split clusters in the low theta region (Z view)

    VBSPOT,VDSTCH : cosmetic changes to move to cvs: integer declara-
           tion no longer needed for HAC parameters (F.Ranjard, Feb 96)

        In *CD -> E_NEWS
   EIDEDX : call TPDHYP('WIRE', instead of TIDHYP. (FLR)
   ECSW94,ECBOOK,ELECID : opening "'" should have a closing "'" within
           the same line for cvs (F. Ranjard, Feb 96)
        In *CD -> F_NEWS
   FPIDEN : call TPDHYP('WIRE', instead of TIDHYP (FLR).

        In *CD -> L_NEWS
   LBDLBA, LSELBH, LTRIGS : replace WRITE(6, by WRITE(IW(6),
                           (H.Drevermann, Feb 96)

        In *CD -> U_NEWS
   UFITMS : replace WRITE(6, by WRITE(IW(6), (H.Drevermann, Feb 96)

        In *CD -> H_NEWS
   HPRANA,HPRANP,HPRDIG,HRCPAT,HTUBFI : opening "'" should have a
       closing "'" within the same line for cvs (F. Ranjard, Feb 96)


        In *CD -> P_NEWS
   PRYFIT : cosmetic changes to move to cvs (F. Ranjard, Feb 96)
        In *CD -> S_NEWS
   SICLUS,SIPREDA : opening "'" should have a closing "'" within
           the same line for cvs (F. Ranjard, Feb 96)

 * corr file 279.1
   ALGTWA : fix number of wagons for some fills (B. Bloch, Nov 95)
        In *CD -> C_NEWS
   CFPNMP, CRCJOB, CTHCLU : replace WRITE(6, by WRITE(IW(6),
                           (H. Drevermann, Feb 96)
   CTKCHG : opening "'" should have a closing "'" within the same
           line for cvs (F. Ranjard, Feb 96)

 * corr file 279.1
   CASHET : remove comments after *CA for LIGHT (P. Comas, Nov 95)

        In *CD -> I_NEWS

   IFIND1,IFIND2,IGETTI,IMATCH,IREOPD,ITBOOK,ITCTRK,ITLNK1,ITLNK2,
   ITRECF,ITRECI,ITREE,ITRKCI : opening "'" should have a closing "'"
        within the same line for cvs (F. Ranjard, Feb 96)

 * corr file 279.2
   IENANG, IPHCOR : avoid use of NLINK (O.Callot, Dec 95)
   IFITP  : optimize code as CHIELI always .ge.0 (O.Callot, Dec 95)
   IFIND1,IFIND2,IGICHA,IPREDA,ITCREC,ITRAK : call ALTIME only if ITTI
           card used (O.Callot, Dec 95)
   ITTIME : common modified to include logical IFITTI (O.Callot, Dec 95)

        In *CD -> M_NEWS

   MUASS,MUTEST : opening "'" should have a closing "'" within the same
           the same line for cvs (F. Ranjard, Feb 96)
   MUNEWR : change LOUTRL and LDEBRL to IW(6) instead of 6 (P.Comas,
           Feb 96)

 * corr file 279.2
   MUSLCM : bug which rejected 2% of the hits in the middle angle muon
           chambers fixed (A.Venturi, Dec 95)

        In *CD -> R_NEWS

   RINCND,RIPAS0,RMONII : opening "'" should have a closing "'" within
           the same line for cvs (F. Ranjard, Feb 96)
   RINJOB : change LOUTRL to IW(6) instead of 6 (P. Comas, Feb 96)

 * corr file 279.2
   AAMAIN : double size of BOS array from 1 to 2 Mw (O.Callot, Dec 95)
   RDETEC : set time measurement flags: for ITC, presence of ITTI
           (O.Callot, Dec 95)
   RNXMOD : call ALTIME only if MXTI card used (O.Callot, Dec 95)
   RSHRIN : avoid use of LENOCC (O.Callot, Dec 95)

        In *CD -> T_NEWS

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
 -----------------------------------------------------------------------------
! 951116 - JULIA 27.9
        In *CD -> A_NEWS
   make several cosmetic changes in several sets to move easily to CVS
   (F. Ranjard, Nov 95)

        In *CD -> V_NEWS
   VDRCUN : never produce hot channels for new Vdet (A. Bonissent,
           Nov 95)

 * corr file 278.3
   VRECON : change number of channels (A. Bonissent, Nov 95)
   VDECOD : make max. number of channels in z view, useful
           for wraparound cluster (A. Bonissent, Nov 95)
   VINIRU : offset of online channels is diffent for old and
           new VDET (A. Bonissent, Nov 95)
   VMKR95 : initialize package which handles bonding maps
           (A. Bonissent, Nov 95)
   VDSM95 : handling of the nominal bonding scheme (A. Bonissent,
           Nov 95)
   VMGCRL : decode wafer number, apply channel to strip mapping and
           fill relation from data channel to readout strip
           (A. Bonissent, Nov 95)
   VTRSUP : transform electronic channel number from Galeph
           into data channel (A. Bonissent, Nov 95)
 * corr file 278.2
   VDMCEF95 : suppress hits in dummy faces in Oct 95 installation of
             new VDET. Supersedes routine of last correction file (A.
             Bonissent, Oct 95)
   VHOTIN   : control usage of VHOT bank with a data card VNHT and
             protection against VHOT inconsistency (A. Bonissent, Oct95)
   VDSM95   : protect for nonsense in VHOT (A. Bonissent, Oct 95)
   VINIRU   : initialization of beam position finding moved to RINIRUN
             (S.Wasserbaech, Nov 95)
 * corr file 278.1
   VDMCEF95 : new routine for MC efficiency of new  Vdet (A. Bonissent
             Oct 95)
   VRECLU   : fix a bug in clustering (only for new Vdet) (A. Bonissent
             Oct 95)
   ALCBJJ, ALLRJJ : comdecks dropped (S. Wasserbaech, Oct 95)
   ALPBJJ, VBSCOM : comdecks modified (S. Wasserbaech, Oct 95)
   ALRPJJ, VBPCJJ : comdecks added (S. Wasserbaech, Oct 95)
   VBSPOT   : huge bug fixed : number of VDET z hits was not correctly
             calculated (S. Wasserbaech, Oct 95)
   VBMFIT, VBSCLR, VBSINR, VCBFIT : change from to ALLR and ALCB to ALRP
             and VBPC; allow tau-tau rejection and VDET hit definition to
             be switchable for LEP1 and LEP2, little protection for LEP2
             against cosmics, better handling of failures in VCBFIT, add
             features to allow JULIA routines to be used in ALPHA without
             modification (S. Wasserbaech, Oct 95)
   VDNOIS : move DATA statement just before BMACRO to allow compilation in
           Linox (P. Comas, Oct 95)

        In *CD -> T_NEWS
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

        In *CD -> R_NEWS
   RCPAS0 : for VDET95, avoid messing around with VHOT and TOHV
           (A. Bonissent, Nov 95)
   RPOTCL : on popular request , for SICAL or LCAL only triggers,
           keep as well banks PCOB,PCRL,EWHE (B. Bloch, Nov 95)
   RFORMA : define format new pad dE/dx banks: PTPX, TPDX, TPXS,
           TPLS (P. Comas, Nov 95)
   RDEFIO : add pad dE/dx bank PTPX to the POT (P. Comas, Nov 95)

 * corr file 278.4
   RGETDV : make 'ONL' option always equivalent to PASS0 (I. Tomalin,
           Nov 95)
   RCPAS0 : interpret new TLAS bank correctly when creating PASS0
           cards (I. Tomalin, Nov 95)
   RFJCON : write error on TPC drift velocity to JCON bank using
           TLAS if TDPV is missing (I.Tomalin, Nov 95)
   RINJOB : add TVVD, TDPV and TLAS banks to run summary bank list
           (the latter two maybe temporarily) (I.Tomalin, Nov 95)
   RECONS : add call to TSUMVD (I. Tomalin, Nov 95)
 * corr file 278.2
   RINRUN : initialize beam position finding (S. Wasserbaech, Nov 95)
   RCLRUN : always close beam position stuff at the end of the run
           (S. Wasserbaech, Nov 95)
 * corr file 278.1
   RECEVT : process VDET regardless the number of tracks if not in
           LEP1 (P.Comas, Oct 95)

        In *CD -> E_NEWS
 * corr file 278.1
   ECSW1093 : move DATA statement just before BMACRO to allow
             compilation in Linox (P. Comas, Oct 95)

        In *CD -> S_NEWS
   SILUAD : automatise the energy dependence of the Lumi x-section
           (B. Bloch, Nov 95)
   SIEVST : pick up a better curve from the Data Base if available
           (E. Lancon, Nov 95)
   SIINIRU: get SHLD bank from Data Base (B. Bloch, Nov 95)
   SHLDJJ : new comdeck added (B. Bloch, Nov 95)

 * corr file 278.2
   SICAL modifications for the high energy run and 95 reprocessing
   SECAJJ, SIBXJJ : new comdecks added (B. Bloch, Nov 95)
   SIEVST : new routine: returns the energy at a given time along
           the Hold timing curve (E. Lancon, Nov 95)
   SIJUKE : new routine: decode juke box configuration (E. Lancon,
           Nov 95)
   SIGTWA : decode SICAL Hold number from SILH or SCHU bank (B. Bloch,
           Nov 95)
   ALGTWA : for the 140 Gev , SICAL will be set with one Hold except
           for 4 runs (B. Bloch, Nov 95)
   SIINIRU: get from Data base SECB and SIBX banks (B. Bloch, Nov 95)
   SILUMI : in Lumi anlysis of 1995 consider only in-time clusters
           (B. Bloch, Nov 95)
   SIPREDA: correct energy of SICAL in physics triggers according to
           bunch number (B. Bloch, Nov 95)

        In *CD -> L_NEWS
 * corr file 278.3
   LBHCOR : check whether we are on the Z (P. Hansen, Nov 95)
   LPRLUM : scale background by ratio of bhabhas with HV on and
           and total bhabha count (P. Hansen, Nov 95)
 -----------------------------------------------------------------------------
! 951009 - JULIA 27.8
        In *CD -> H_NEWS
   HT0CJJ : new common deck added (A.Messineo, Aug 95)
   HTOWCA, HCATOW, HRDCAL: Barrel and endcap normalization
                          per wagon (A.Messineo, Aug 95)
   FTRACK, MUCUTS : protect to avoid overflow in PIBE2 (P.Comas, Aug 95)

        In *CD -> U_NEWS

   UFITMS : rearrange DASIN argument to avoid crash (P.Comas, Sep 95)

        In *CD -> I_NEWS
   INSECT : Add test for intersection. Also tighten sin(A) test to
           not allow angles too near 90 deg (J.Sedgbeer, Aug 95)

        In *CD -> F_NEWS

   FITALL, FREFIT : replace references to obsolete UFTTRK by UFTTRA
              (G. Taylor, Sep 95)

        In *CD -> R_NEWS
   RLOOPR : Filter out TPC laser events writing only the EVEH
           (P.Comas, Aug 95)
   RFORMA : format of new VDET banks VCSG, VWRL, VDMR, VDGC
           (P.Comas, Sep 95)
   RDEFIO : new VDET banks VWRL, VDMR, VDGC to POT (P.Comas, Sep 95)

 * corr file 277.2
   RDEFIO : new MC bank X1TP to POT (P.Comas, July 95)
   RCJSUM : add as many rows as wagons in a train and fill them
           (P.Comas, Jul 95)
   BMSUMCO : common modified to have BMSUM information per wagon
          (P.Comas, Jul 95)
   BMSUM : have the information per wagon in a train (P.Comas
          Jul 95)
        In *CD -> S_NEWS
   ALGTWA : The 95 scan is done with 3 wagons (B.Bloch, P.Comas, Aug 95)

 * corr file 277.2
   ALGTWA : integer function to get the number of Sical wagons per train
   SIINIRU: use ALGTWA instead of RQBUNC
   SIPRLUM: use ALGTWA instead of RQBUNC
   SILUAD : fix the statistical error on Lumi when bunch train operation
        In *CD -> T_NEWS
 * corr file 277.2
   TRNCON : call RERROR if no TSOR bank is present (I. Tomalin,
           Jun 95)
        In *CD -> V_NEWS
   VUMCTR, VDMCTR : change the corrections given by A.B in 277.1 by
                   those from G.Taylor (Aug 1995)
   VHOTIN : Put VHOT on the C list, so it isn't dropped
           (A. Bonissent, Aug 95)
   VDPINH : new routine to simulate peculiar readout channels in MC
           (G. Taylor, Aug 95)
   VDCHST : new function to map electronic readout channel to physi-
           cal readout strip (G. Taylor, Aug 95)
   VDSTCH : new function inverse to the previous (G. Taylor, Aug 95)
   VDMPIN,VINIRU,VDGTHT,VTRUTH: modified for the new MC treatment
           of delta rays (G. Taylor, Aug 95)
   VDGCJJ,VDMRJJ,VWRLJJ : new common decks (A. Bonissent, Sep 95)
   VCSGJJ,VREGJJ : common decks modified (A. Bonissent, Sep 95)
   VDMJLN : SAVE statement added (P. Rensing, Sep 95)
   VDSM95,VTRSUP,VMKR90,VMKR95,VMGCRL : new routines to handle new
           VDET and allow a different treatment of old Vdet (A. Boni-
           ssent, Sep 95)
   VDECOD,VDMPIN,VRECLU,VDXYZT,VSPXYZ,VCRHOT,VHOTIN,
   VDNOIS,VTRUTH,VCALCM,VCMEAN,VDBGCL,VDSMAP,VINIRU
   All routines modified to addapt to the different treatment for old
   and new Vdet (A. Bonissent, Sep 95)

 * corr file 277.2
   VCBFIT : protect RSINV against an error return (P.Comas)
   VMGNJJ : hac parameters modified (A.Bonissent)
   VDMPIN : use changed VMGN modified to contain also first and last
           channel in a region (A.Bonissent)
 * corr file 277.1
   VUMCTR, VDMCTR : correction to process GALEPH events made with old
                    vdet geometry package (A.Bonissent)

        In *CD -> E_NEWS
 * corr file 277.3
   ECFIBT : PWEI bank corrected for calibration and change fatal
           error by warning error when EWDI does not exist
           (M.N.Minard, Aug 95)
   ECFILS : change fatal error by warning error when EWDI does not
           exist (M.N.Minard, Aug 95)
   ECHEDC : bug fix when applying calibration (M.N. Minard, Aug 95)
 -----------------------------------------------------------------------------
! 950530 - JULIA 27.7

        In *CD -> V_NEWS
   VHOTOF, VCALCM, VCMEAN, VCRHOT, VDBOOK, VDCRUN, VDECOD, VDHIST, VDLINK,
   VDMPIN, VDNOIS, VDRCUN, VDXYZT, VDSMAP, VHINIT, VHOTIN, VINIJO, VINIRU,
   VLCPOS, VPREDA, VPRPOT, VPRSUM, VRECLU, VRECON, VREPOS, VSEPAR, VSLOWC,
   VPRTNC, VDFLGS,
   All decks purged and rewritten (Vdet prepare data).
   - Forget about peculiarities for 1990 Vdet data
   - Use geometry package to obtain information about Vdet topology
   - Redesign and clarify the code
           (D. Brown, A. Bonissent, M. Thulasidas, April 1995)
   VDISMR : Check VDHT bank is there (M. Thulasidas, May 95)

 * corr file 276.1
   VBSPOT : Use ABS on d0 and z0 cuts; remove call to VDAMB
           (S.Wasserbaech, Apr 95)
   VBMFIT : Use existing first-event-in-chunk number when
           NEWROW = .FALSE. (S.Wasserbaech, Apr 95)
   VBSCLR : Do not create a new row if the first row is not
           yet filled (S.Wasserbaech, Apr 95)
   VCBFIT : Take beam size from ALLR instead of data statement
           (S.Wasserbaech, Apr 95)
   VBSINR : Always clear the work banks (number of rows = 0)
           Get ALLR from daf (S.Wasserbaech, Apr 95)
   ALLRJJ : Comdeck added to be used in VCBFIT
           (S.Wasserbaech, Apr 95)

        In *CD -> T_NEWS
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

        In *CD -> S_NEWS
 * corr file 276.1
   SIPREDA: Allow reprocessing from POT to POT when SIDI not available
            apply overall energy scale factor in calibration mode only
   SILHCR : Allow reprocessing from POT to POT
   SILUMI : Allow reprocessing from POT to POT

        In *CD -> L_NEWS
   LBUNCA : new routine for bunch-to-bunch calibration in LCAL
           (P.Hansen, Apr 95)
   LPREMC : bunch-to-bunch corrections for bunch trains
           (P.Hansen, Apr 95)

        In *CD -> E_NEWS
 * corr file 276.4
   EDWIJJ,PWEIJJ : new common decks added (M.N. Minard, May 95)
   EWDIFP : addapt to bunch trains and use of PWEI
                  (M.N. Minard, Apr 95)
   ECERAS,CPADWR : use of PWEI (M.N. Minard, Apr 95)
   ECFILS : check the Pastis configuration - 1995 and call ECFIBT
           (M.N. Minard, Apr 95)
   EPADCL,EWIRCL,ETDIFP,ECHEDC,ECHICA : addapt to bunch trains
                                (M.N. Minard, Apr 95)
   ECFIBT : new routine for wires banks analysis in bunch trains
           (M.N. Minard, Apr 95)
   EFIXI94: artificially built EKLS bank of zero length to put to
           zero the number of killed channels (M.N.Minard, Apr 1994)

        In *CD -> R_NEWS
   RCJSUM : improve PASS0 drift velocity calculation (I.Tomalin,
           May 95)
   RDEFIO : new banks HLOD to POT (P.Comas, May 95)

 * corr file 276.4
   RWJUBW : new routine to store bunch number identification used for
           reconstruction in EVEH (P.Comas, Apr 95)
   RCJSUM : only sum up t0 for events where bunch number is certain.
           (I.Tomalin, May 95)
   RDEFIO : new banks PWEI and LPAS to POT (P.Comas, May 95)
   RFORMA : format of new bank PWEI (P.Comas, May 95)
   RINRUN : add call to RWJUBW (bunch number used for reconstruction)
           (P.Comas, Apr 95)
   RLOOPR : add call to RWJUBW (bunch number used for reconstruction)
           (P.Comas, Apr 95)
   RMONIT,MINHEA : addapt to use PWEI (M.N.Minard, Apr 95)
 -----------------------------------------------------------------------
! 950410 - JULIA 27.6

        In *CD -> V_NEWS
   VDLINK : Call VDMCTR to compress Monte Carlo truth info
           (A.Bonissent, Mar 95)
   VDMCTR,VTRUTH,VUMCTR : Compress Monte Carlo truth info
           (A.Bonissent, Mar 95)
   VDALIN,VDALXY,VDALZT,VDALSG,VDGTHT,VDGTMV,VDGTXL,VDOSMR,VDRNDM,
   VDSIGM,VDTAN,VDWSDX,VDWSMR,VDGAUS : New treatment of alignment
   errors for 1994 Monte Carlo production  (M.Thulasiadas, Mar 95).
   VDMCEF : Take out position smearing (M.Thulasiadas, Mar 95).
   VPREDA : Call VDISMR for smearing (M.Thulasiadas, Mar 95).
   VDISMR : Set up appropriate smearing scheme (M.Thulasiadas, Mar 95).

 * corr file 275.3
   VBSPOT : avoid taus to measure the beam spot (S.Wasserbaech).

        In *CD -> E_NEWS
   EFIXI94: artificially built EKLS bank of zero length to put to
           zero the number of killed channels (M.N.Minard, Apr 1994)
 * corr file 275.4
   ECCLUS : ESDA internal storey bank used in clusterisation is dropped
           when cluster bank is dropped (M.N. Minard, Feb 1995)

        In *CD -> R_NEWS
   RFORMA,RDEFIO: add VDFK,VUFK to output list (A.Bonissent,
                 M.Thulasiadas, Mar 95)
 * corr file 275.5
   RDEFPO : call LUPACR when LCAL .and. SATR are required. If one is
            missing the name-indices are not defined (F.Ranjard, Feb 95)
 * corr file 275.4
   RDEFPO : reverse order of processing: first LCAL, after SICAL
           (B. Bloch, P. Comas, Feb 1995)
 * corr file 275.3
   RDEFIO : replace old GAMPEX bank PGPC by the new one PGAC (M.N. Minard,
           (Nov 1994)
   RFORMA : format of new Ecal banks EAGC, PGAC (M.N. Minard, Oct 1994)

        In *CD -> S_NEWS
 * corr file 275.4
   SIPREDA: Process next pad in case of 'Invalid SIDI Address' (B.Bloch,
           Feb 1995)

        In *CD -> T_NEWS
 * corr file 275.5
   TCODRV : drop work banks at the end (D.Casper, Feb 95)

        In *CD -> M_NEWS
 * corr file 275.5
   MPREDG: correction for bad cabling in first runs of 1994 data,
          from 25000 to 25895 (P.Campana, Feb 95)
 ------------------------------------------------------------------------
! 941019 - JULIA 27.5
        In *CD -> C_NEWS
   CDANG : restrict COMN1 to [-1.,1.] to avoid precision problems
          (P. Comas, 18-OCT-1994)
   CTRPAR : avoid division by zero: no need to step in the helix if
           STEP = 0  (A. Bonissant,P. Comas, 29-SEP-1994)
        In *CD -> L_NEWS
   LESTIM : prevent division by zero (P. Hansen)

        In *CD -> E_NEWS
   EAGCJJ : new common deck added (M.N. Minard, Oct 1994)
   EGETDS : for 94 running drop EKLS EKPL wrong on the header (same)
   EPREDA : in case of raw data from run range 1994 (<25854)
           swap gain map of endcap A (same)
   EFIXI94: new subroutine to drop EKLS EKPL banks (same)
   ECSW94 : new subroutine to swap gain for ETDI, active only on 1994
           raw data run<25894 (same)
        In *CD -> G_NEWS

   GASTEER : Build PGID and PGPC (Bulos+Gampex) bank
            (M.N. Minard, Oct 1994)
        In *CD -> R_NEWS

   RFORMA : format of new Ecal banks EAGC, PGAC (M.N. Minard, Oct 1994)
 * corr file 274.1
   RFJCON : remove call to TDEDXV. Fill JCON bank with /TMONIT/GAINTM.

        In *CD -> M_NEWS
 * corr file 274.1
   MUASS: allow to have the MUEX banks in the data even
          if there is no hit in the muon chambers (A.Venturi).

        In *CD -> T_NEWS
 * corr file 274.2
   TOERJJ : update HAC parameters (G. Ganis)
   TCPTST : fill TOER bank extended with 5 more words (G. Ganis)
   TSAWTK : protect code to prevent JULIA crashes in '93 MC production
            on ALPHAs (C. Georgiopoulos)
        In *CD -> V_NEWS
 * corr file 274.3
   VBSINR : create working bank only once (E.Lancon).
        In *CD -> I_NEWS

 * corr file 274.3
   INSECT : protect against square root of negative value (P.Comas).
        In *CD -> R_NEWS
 * corr file 274.5
   see comment on 274.4
 * corr file 274.4
   RLOOPR : only the EVEH of events with DAQ error as evtype (998)
            is passed on to the POT. These events are not recons-
            tructed.
            The original correction on JULIA 274.3 passed the whole
            event without any processing. This final one was JULIA
            274.4 on FALCON but on the other computers JULIA 274.4
            was released with another correction (TRNCON) while I
            (P.Comas) was away.
            corr file 274.5 will contain both final corrections to
            RLOOPR and TRNCON.
        In *CD -> T_NEWS
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
  ---------------------------------------------------------------------------
! 940520 - JULIA 27.4
        In *CD -> X_NEWS
   XPTENB : use of X1RG instead of XTOP dropped in the readout upgrade
   X1TT, X1RG, X1HI : HAC parameters of new trigger banks added as common decks
        In *CD -> L_NEWS
   LPRLUM : use of X1TT instead of XTOP dropped in the readout upgrade
   LSELBA : use of X1RG and X1TT instead of XTOP dropped in the readout
           upgrade
        In *CD -> R_NEWS
   RINJOB : replace JTDX by JHDX in SUMLIS summary output list.
   RMONIT : remove call to TDXMON,Fill the dE/Dx word of the MONE bank
            with a constant 1.0
   RECEVT : remove call to TDXMON, everything is done in TACCMN
 * corr file 273.2
   RFORMA,RDEFIO: add D4CD bank to output list (G.Taylor).
                  remove SOR new trigger banks put by error in
                  corr file # 1
 * corr file 273.1
   RDEFIO: add new trigger banks needed to output list.
        In *CD -> S_NEWS
 * corr file 273.3
   SILUAD : Take into account the downscaling factor of the coincidence
            trigger when computing the preliminary Luminosity
 * corr file 273.1
   SICAOK: fix usage of new trigger banks X1RG, X1TT, X1HI
   SILHCR: adapt to new trigger banks
        In *CD -> E_NEWS
 * corr file 273.1
   EQRUNQ : changes due to new ECAL banks (May 1994)
   EFERRR : changes due to new ECAL banks (May 1994)
   EPREDA : changes due to new ECAL banks (May 1994)
   EAUTOP : new subroutine to decode banks EHWI and EHPA and
           summarize (condense) into arrays NAUTO and NAUTOW
   ECXMOD : (new) decode the EHPA data word and convert into a module
           number
        In *CD -> J_NEWS
   JTDXJJ : is obsolete
   JHDXJJ : is introduced to replace JTDXJJ
        In *CD -> T_NEWS
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

 -----------------------------------------------------------------------------
! 940426 - JULIA 27.3
        In *CD -> T_NEWS
   TCPTST - handle new TOER error bank (G.Ganis).
        In *CD -> O_NEWS
   OMREAD - allow for 1-8 bunches instead of 1-4 (R.Forty).
        In *CD -> R_NEWS
   RFORMA,RINJOB: remove XSGE, XSHI, XSSC trigger banks.
   RINRUN: remove call to XTRSIN.
   RPOTCL: remove call to XTRSEV.
   RCLRUN: remove call to XTRSFI.
        In *CD -> X_NEWS
   X1NAMC : remove XSGE, XSHI, XSSC name-indices.
   X1DCRB,XTUNPK,XTRSIN,XTRSEV,XTRSFI : are purged.
        In *CD -> R_NEWS
 * corr file 272.1
   RDEFIO : write VDTD on POT as it was in previous versions.
        In *CD -> T_NEWS
 * corr file 272.1 modified in 272.2
   TRNCON : with MC data when TPCSIM version # is > 217
            use the true T0 from T0GL, do not set it to 0.
 * corr file 272.2
   TRNCON : set T0=0. for TPCSIM version number < 218
   TFITTK : requires at least 3 coords. (I.Tomalin)
   TOVRLP : remove bug (I.Tomalin)
        In *CD -> S_NEWS
 * corr file 272.3
   SICLUS: apply new cluster correction instead of old one.
   SILUMI: use planes 3 and 4 for loose side selection as for tight side
   SICLC2: new routine
        In *CD -> V_NEWS
 * corr file 272.4
   VDMCEF : remove double correction which created wrong ALEPH
            Z-coordinate pertubating DALI, the local Z-coordinate
            was correct (M.Thulasidas).
 -----------------------------------------------------------------------------
! 940111 - JULIA 27.2

        In *CD -> A_NEWS
   AAMAIN - remove CRAY flag
            open JULIACARDS file if it exists

        In *CD -> C_NEWS
   CINIJO - set POT bank name-indices even no output file is required

        In *CD -> E_NEWS
 * corr file 271.8
   ERCUPI : initialize ICOR=0 which means returns from ERCUPR
            without correcting ETDI energies.
   ERCUPR : returns immediately when ICOR=0
   ESWPST : returns immediately if MC data
 * corr file 271.7
   ECSW1093 : new routine to fix ETDI swapping in end-cap for runs
              22793-22880.
   EPREDA   : call ECSW1093.
 * corr file 271.6
   ECTRAP : protection against invalid storey address.
 * corr file 271.5
   ECLONGI : new stuff.

        In *CD -> H_NEWS
 * corr file 271.6
   HMROAD : add protection.
 * corr file 271.1
   HCBHIS : HBPRO was called with a wrong number of arguments

        In *CD -> R_NEWS
 * corr file 271.9
   RFORMA : set JCON format to 48 columns.
 * corr file 271.8
   RINRUN : in case of '92 MC prod, drop EZTH and get it
            back from ADBSCONS.
 * corr file 271.6
   JCONJJ : extend JCON bank to 48 columns
   RFJCON : add de/dx constants.
 * corr file 271.2
   RWRUNH : bug when handling JCAR bank
 * corr file 271.1
   RDCARD,RDEFIO,RINJOB,RLOOPR : use BANKAL FMT
   RINJOB : VDTD is obsolete , remove this correction later on.
   JCONJJ : extend JCON bank to 13 columns
   RWREVT : suppress writing to moni file when not on VAX
            remove this correction in 271.9
   RFORMA : reset HLTU and JCON formats
   RFJCON : add TPC clock frequency and pressure.

        In *CD -> S_NEWS
 * corr file 271.4
   SIPVOI : array of energies in 9 pads in 5 first layers.
 * corr file 271.1
   SILHCR : Keep integrated SICAL scalers content in SILH
   SILUMI : Make Luminosity energy cuts depend upon CMS energy
   SIBOOK : HBPRO was called with a wrong number of arguments

        In *CD -> T_NEWS
   TFCAND - get track parameter errors from the diagonal of the
            covariance matrix.
   TCLDMP, TPRPOJ - enlarge formats.
   TPCRUN : return if MC data
 * corr file 271.6
   TPCRUN : expand fit interval.

        In *CD -> V_NEWS
 * corr file 271.6
   VBSINR : do not create a new working bank for each run.
 * corr file 271.1
   VDMCDEF : call rannor instead of norran

 -----------------------------------------------------------------------------
! 911108 - JULIA 2.58
..............................................................................

               Julia 2.58 correction file 01 is now installed on

                        ALWS, VXCERN, IBM, CRAY, FALCON




                                 W A R N I N G

          ***** This version requires ALEPHLIB 133 and DAF 161 *****

                                 W A R N I N G




Compared to julia 257 correction file 02, changes are :

        o The SICAL is now in JULIA.

        o GAMPEC  is now  called,  result  is  stored in EGPC
          bank. To turn on  GAMPEC one must  provide in julia
          data cards a card called : GAMP.


+++++++++++++++++++++++++++++++ New  Decks   ++++++++++++++++++++++++++++++++++

    SIBOOK, SIHIST, SIPREDA, SIINIJO, SIINIRU, SISLOWC, SICREC,
    SICRPOT, SIPRSUM, SICRUN
                Dummy SICAL routines

    GAMBNK      Fill EGPC bank (result of GAMPEC)


+++++++++++++++++++++++++++++++ Purged Decks ++++++++++++++++++++++++++++++++++

    RECEV2, GDECJJ

+++++++++++++++++++++++++++++ Modifications in ++++++++++++++++++++++++++++++++

 ITC --------------------------------------------------------------------------

    ICRCCO      use function IRESRP to set the r-phi resolution in ICCO bank
    IREFIN      "     "       "    "    "  "    "        "      "   "   "

 ECAL -------------------------------------------------------------------------

    E4COSZ      Track momemtum added in routines arguments
    EIDENT      Call to E4COSZ modified
    ELONGI      Modification of longitudinal electron estimator for monte-carlo
                events.
 SKELETON ---------------------------------------------------------------------

    RPARAC, RNXMOD
                GAMPEC added
    RECOSN      Call to GAMBNK added
    RINJOB      BOS memory set to 750.000 for CRAY jobs
    RDEFIO      EGPC and SIDI banks added to output
    RECEVT      VDET laser triggers are now reconstructed
    RFLAGS, RPARAC, RNXMOD, RECEVT, RPREDA, RBKHIS, RDETEC, RHSEVT, RINJOB,
    RINRUN, RSMJOB, RUNKNO, RCLRUN, RDEFPO
                SICAL routines are called is 'SICA' is found in the PROC data
                card

..............................................................................
11-NOV-1991 ..................................................................
..............................................................................
               Julia 2.58 correction file 02 is now installed on

                            ALWS, VXCERN, IBM, CRAY


    This version will be installed today on FALCON and used for the
reprocessing.


+++++++++++++++++++++++++++++ Modifications in ++++++++++++++++++++++++++++++++


 BOM  ----------------------------------------------------------------------

    OMREAD      one print statement removed

 LCAL ----------------------------------------------------------------------

    LINIJO, LPRLUM
                Some protections added against undefined variables

 SKELETON ------------------------------------------------------------------

    RINJOB      BOS array size change to 650.000 for cray jobs
    RNXMOD      One format changed

..............................................................................
18-NOV-1991 ..................................................................
..............................................................................
               Julia 2.58 correction file 03 is now installed on

                      ALWS, VXCERN, IBM, CRAY and FALCON


    This  version  together with  Alephlib  13.3 and  Database  version 162
    (released today) is installed on  FALCON and will be used for the FINAL
    reprocessing of 1991 data.


+++++++++++++++++++++++++++++ Modifications in ++++++++++++++++++++++++++++++++


 BOM  ----------------------------------------------------------------------

    OMBREC      Content of BOME bank has been changed, new DDL follows

 MUON ----------------------------------------------------------------------

    MUSLCO      small correction of alignment of the second layer of muon
                chambers.

 TPC  ----------------------------------------------------------------------

    TFILTJ      packing factor for TPC coordinates changed (go back to last year
                values)


..............................................................................
04-DEC-1991 ..................................................................
..............................................................................
               Julia 2.58 correction file 04 is now installed on

                            ALWS, VXCERN, IBM, CRAY


+++++++++++++++++++++++++++++ Modifications in ++++++++++++++++++++++++++++++++


 MUON ----------------------------------------------------------------------

    HMROAD      Correction for muon id in overlap for M.C.

 ECAL ----------------------------------------------------------------------

    ETDIFP, ECHEDC
                Corrections for POT->POT reprocessing

 TPC  ----------------------------------------------------------------------

    TFILTJ      Length of PTUN incorrect (never used in fact)


..............................................................................
11-DEC-1991 ..................................................................
..............................................................................
               Julia 2.58 correction file 05 is now installed on

                            ALWS, VXCERN, IBM, CRAY


+++++++++++++++++++++++++++++ Modifications in ++++++++++++++++++++++++++++++++


 TPC  ----------------------------------------------------------------------

    TCOOR, TCOORT, TSAWIR, TWINCO
                        Call to TFICOR (alephlib routine) to correct coordinates
                        for residual field distortions (short in field cage for
                        '91 September runs).

 SKELETON ------------------------------------------------------------------

    RINJOB      BOS array size set to 600.000 for CRAY.

! 02-MAR-1992 2.59
..............................................................................

               Julia 2.29 correction file 04 is now installed on

                        ALWS, VXCERN, IBM, CRAY, FALCON


    Compared to julia 258 changes are :

            o Julia steering routines for I/O hev been modified to use the
              standard aleph I/O package from the alephlib. Julia can now read
              EDIRs with the standard BOS.

            o SAVE statements have been added in every routine

            o main modifications have been made in view of POT-POT reprocessing.

            o the bookkeeping option have been disabled.

            o This version will be used for the DST-DST reprocessing of 1990
              data.

+++++++++++++++++++++++++++++++ New  Decks   ++++++++++++++++++++++++++++++++++

    ECHICA      Common deck to save ECAL calibration constant fromprevious
                processing in case of POT-POT reprocessing
    LCSAVE      Save LCAL info. from PECO and PEPT in LPSV and LPES in case of
                POT-POT reprocessing
    ITCREP      Steering routine for ITC reconstruction for POT-POT reprocessing
    ICFTOI      Convert FRFT type banks to ITFT type track banks
    ISHUFF      Shuffle/Update FRFT type banks from ITFT type track banks

+++++++++++++++++++++++++++++++ Purged Decks ++++++++++++++++++++++++++++++++++

    RLOOPF

+++++++++++++++++++++++++++++ Modifications in ++++++++++++++++++++++++++++++++

 VDET -------------------------------------------------------------------------

    VDECOD      mods for APOLLO
    VCALCM      "   "   "

 ITC --------------------------------------------------------------------------

    IINIRU      If missing dbase banks then fatal error
    IPREDA      Remove obsolete code for old MC data

 TPC  -------------------------------------------------------------------------

    TRHLX2      mods for APOLLO
    TISLND      "   "   "
    THTROW      "   "   "
    TPPADD      "   "   "
    UFITMS      "   "   "

 LCAL -------------------------------------------------------------------------

    LTGTYP      mods for APOLLO

 ECAL -------------------------------------------------------------------------

    ECERAS      Drop ECAL POT input banks and call LCSAVE in case of POT-POT
                reprocessing
    ECHEDC      mods for POT-POT reprocessing
    ETDIFP      "   "   "   "
    EWDIFP      "   "   "   "
    ELECID      EIDT not created if there is no tracks in the event
    EPADCL      some protection if routine is called before run initialisation
    EHIS        HBOOK arguments updated
    ETRDEV      mods for APOLLO
    EROAD       "   "   "

 CALO -------------------------------------------------------------------------

    CPOTLC      Use LPES and LPSV to fill POT banks in case of POT-POT
                reprocessing
    CDANG       clean up code
    COSLST      "   "   "
    CPTCOM      "   "   "

 HCAL -------------------------------------------------------------------------

    HPRANP      some protections added
    HROAD       mods for APOLLO

 VERTEX -----------------------------------------------------------------------

    YFMAIN      use (FRFT,FRTL) instead of (TGFT,TGTL)
    YFVERT      "   "   "   "

 SKELETON ---------------------------------------------------------------------

    RCJSUM      mods for POT-POT reprocessing
    RCLRUN      "   "   "   "
    RDEFPO      "   "   "   "
    RECEVT      "   "   "   "
    RINRUN      "   "   "   "
    RINJOB      "   "   "   "
    RREVHE      "   "   "   "
    RDETEC      BCAL and EFLO were forgotten
    RDEFIO      one argument added to routine (return code),
                DST output option identical to POT
    RLOOPR      Routine rewritten to call now standard aleph I/O routines
    RDOJOB      Do not call anymore RLOOPF
    RECONS      mods for new I/O package
    RUNKNO      "   "   "   "
    RWREVT      "   "   "   "
    RWRUNH      "   "   "   "
    RLUNIT      "   "   "   "
    RNXMOD      clean-up of code


..............................................................................
11-MAR-1992 ..................................................................
..............................................................................

               Julia 2.59 correction file 05 is now installed on

                        ALWS, VXCERN, IBM, CRAY, FALCON

+++++++++++++++++++++++++++++ Modifications in ++++++++++++++++++++++++++++++++

 TPC  ----------------------------------------------------------------------
    FINIJO      remove BLIST call
 CALO ----------------------------------------------------------------------
    CINIJO      remove BLIST call
 LCAL ----------------------------------------------------------------------
    LBINDX      remove BLIST call
    LPREDA      add BLIST call
 STAR ----------------------------------------------------------------------
    SRINBS      remove BLIST call
    SPREDA      add BLIST call
 HCAL ----------------------------------------------------------------------
    HINIJO      remove BLIST call
    HPREDA      modifications for POT-POT reprocessing
 MUON ----------------------------------------------------------------------
    MINIJO      remove BLIST call
    MPREDA      add BLIST call
 SKELETON ------------------------------------------------------------------
    RWRUNH      mods for POT-POT reprocessing
    RLOOPR      Do not unpack HCAL in case of POT-POT reprocessing

! 17-MAR-1992 2.60
..............................................................................


                        Julia 2.60 is now installed on

                        ALWS, VXCERN, IBM, CRAY, FALCON




                                 W A R N I N G

       ***** This version requires ALEPHLIB >= 136 and DAF >= 165 *****

                                 W A R N I N G




Compared to julia 259 correction file 05, changes are :

        o Simplification of julia data cards, only 2 cards are needed in most
          cases : simply FILI and FILO, all the defaults are in (VFIT, GAMP,
          etc..).
          To disable old VFIT option use NVFI card
                         GAMP            NGAM
                         EWTH            NEWT
                         ESCA            NESC


        o This is the prototype version of julia for 1992 running (PASS0 is
          included, new TPC banks can be handled)


+++++++++++++++++++++++++++++++ New  Decks   ++++++++++++++++++++++++++++++++++

    EGAEST      Built photon normalised estimator bank
    EGACST      Initialisation banks used in photon estimator normalisation
    RGETDV      Get TPC drift velocity
    RWTDPV      Write TDPV bank in PASS0 summary file
    RDCARD      read and decode data cards

+++++++++++++++++++++++++++++ Modifications in ++++++++++++++++++++++++++++++++

 VDET -------------------------------------------------------------------------

    VLCPOS      new definition of sum pulseheight

 TPC --------------------------------------------------------------------------

    TCPTST      handle new banks TE1R,TANF,TWNF,TSIR
    TPKILL      handle the TSIR and TSDI banks
    TPREDA      "   "   "
    TTRUNK      "   "   "
    TWIRES      "   "   "
    TPCT0       use alephlib routine TPDVEL to load drift velocity
    TRNCON      "   "   "

 CALO -------------------------------------------------------------------------

    CNIGHB      do not consider noisy hcal cluster
    FTRACK      obsolete code removed

 ECAL -------------------------------------------------------------------------

    CALREC      call EGAEST
    EINIRU      call EGACST
    GAMBNK      Built EGPR bank (list of storey of gampec row)

 HCAL -------------------------------------------------------------------------

    HTUBFI      call HFBAHI and HFENHI from alephlib
    HPRDIG      produce new HLTU bank (MC true long. coor. hit)
    HMROAD      improved treatment of HCAl barrel/endcap overlap
    HTRACK      fix for precision problem in track extrapolation on IBM

 SKELETON ---------------------------------------------------------------------

    RDEFIO      EGPR, EGNE, HLTU banks added to output
    RCLRUN      mods for PASS0
    RECEVT      "   "   "
    RFJCON      call TPDVEL alephlb routine to get TPC drift velocity
    RINJOB      mods for PASS0, call RDCARD, RERROR init. called after RDETEC
    RINRUN      call RGETDV
    RDETEC      mods for new julia defaults (VFIT , GAMP on by default)

..............................................................................
14-APR-1992 ..................................................................
..............................................................................
               Julia 2.60 correction file 04 is now installed on

                        ALWS, VXCERN, IBM, CRAY, FALCON



+++++++++++++++++++++++++++++ Modifications in ++++++++++++++++++++++++++++++++

 TPC  ----------------------------------------------------------------------
        TLASJJ      updated
        TRNCON      call RGETDV
 CALO ----------------------------------------------------------------------
        CALREC      egaest called at wrong place
 LCAL ----------------------------------------------------------------------
        LINIRU      mods for loading 1992 alignment
 ECAL ----------------------------------------------------------------------
        EPADCL      Return after decoding EPRS if is run not initialized
        EGAEST      one undefined variable
 HCAL ----------------------------------------------------------------------
        HPRDIG      undefined variable
        HMROAD      take into account big notch of barrel 6,7 not sim. in galeph
 VDET ----------------------------------------------------------------------
        VINIRU, VDXYZT, VRECON
                    mods for loading 1992  daf banks
 SKELETON ------------------------------------------------------------------
        RCLRUN      small mod.
        RUNKNO      accumulate stat for 'slow control' records
        RFORMA      some SICAL formats added, TLAS format modified
        RPREDA      small mod.
        RCLJOB      call abwend
        RINRUN      remove call to rgetdv, add call to ripas0
        RWTDPV      writte JPAS bank to pass0 summary file
        RECONS      small mod.
        RINJOB      reduce hbook array size for cray
        RWRUNH      JPAS added to run header
..............................................................................
24-APR-1992 ..................................................................
..............................................................................

               Julia 2.60 correction file 07 is now installed on

                        ALWS, VXCERN, IBM, CRAY, FALCON



+++++++++++++++++++++++++++++ Modifications in ++++++++++++++++++++++++++++++++

 TPC  ----------------------------------------------------------------------
        TPCT0       error code not properly defined
        TPRSUM      one format statement was wrong
 ECAL ----------------------------------------------------------------------
        EGACST      protection against multiplication of too big numbers
        EGAEST      one variable undefined , some protection added
 SKELETON ------------------------------------------------------------------
        RDCARD      bug fixed for multiple READ data card
        RWTDPV      some formats changed

! 18-MAY-1992 2.61
..............................................................................
                        Julia 2.61 is now installed on

                        ALWS, VXCERN, IBM, CRAY, FALCON



+++++++++++++++++++++++++++++ Modifications in ++++++++++++++++++++++++++++++++

 BOM -----------------------------------------------------------------------
    OMBPOS  one sign changed
 ITC  ----------------------------------------------------------------------
    ITCREP  do not call mc-recons matching in case of pot-pot reprocessing
 VDET ----------------------------------------------------------------------
    VINIRU  fix for miss cabling in first run of '92
    VDSMAP  "   "   "   "
 TPC  ----------------------------------------------------------------------
    TPREDA  one SAVE statement added
    TRKFND  some protections added
 CALO ----------------------------------------------------------------------
    CALREC  call to EGAEST miss-placed
 ECAL ----------------------------------------------------------------------
    EGAEST  some protections added
 LCAL ----------------------------------------------------------------------
    LOLERR  mod for filling LOLE error flag
    LUPACR  mod for filling LUPA information word
 SKELETON ------------------------------------------------------------------

               Julia 2.61 correction file 02 is now installed on

                        ALWS, VXCERN, IBM, CRAY, FALCON



+++++++++++++++++++++++++++++ Modifications in ++++++++++++++++++++++++++++++++

 TPC  ----------------------------------------------------------------------
    FPIDEN      some protections added
    FITWRI      "   "   "   "
 CALO ----------------------------------------------------------------------
    CPOTCR      some protections added
 VDET ----------------------------------------------------------------------
    VINIRU      wrong logic for 1992
 SKELETON ------------------------------------------------------------------
    RGETDV      one bug corrected (affect PASS0 only)
    MNHEA       use FRFT instead of PFRF

               Julia 2.61 correction file 04 is now installed on

                        ALWS, VXCERN, IBM, CRAY, FALCON


    To simplify installation on Unix machines, the '\' character has been
replaced by '&' as a line delimiter for the RERROR routine.


+++++++++++++++++++++++++++++ Modifications in ++++++++++++++++++++++++++++++++

 CALO ----------------------------------------------------------------------
    CPOTLC  update PCOB, PCRL in case of POT-POT repro.
 HCAL ----------------------------------------------------------------------
    HCALRD  Load properly HxEC banks
 ECAL ----------------------------------------------------------------------
    ECCLUS  One comment modifed
    ECTEMP  Two calls to RERROR removed
 SKELETON ------------------------------------------------------------------
    RCLJOB  remove call to BKENJO (Bookkeeping option removed from Julia)
    RLOOPR  drop LDST bank for 1991
    RDETEC  Integer compression is now the default (COMP 'INTE' card no more
            needed) GARB card no more needed it is the default, use NGAR card to
            switch it off.

               Julia 2.61 correction file 06 is now installed on

                            ALWS, VXCERN, IBM, CRAY



+++++++++++++++++++++++++++++ Modifications in ++++++++++++++++++++++++++++++++

 TPC  ----------------------------------------------------------------------
    TCOTHR  invisible characters removed
 HCAL ----------------------------------------------------------------------
    HTRACK  removal of machine dependent statement
 MUON ----------------------------------------------------------------------
    MPREDM  modification for muon chambers under the leg in MC
 BCAL ----------------------------------------------------------------------
    BCPACK  non initialized variables
 SKELETON ------------------------------------------------------------------
    MINHEA  mistyping corrected
    RDCARD, RDEFIO
            POT and FILO card are totaly equivalent

! 2-SEP-1992  2.62


                        Julia 2.62 is now installed on

                            ALWS, VXCERN, IBM, CRAY



    o All the SAVE statements (except explicit ones) have been removed.

    o reorganization of the  HISTORIAN library. The  librairy is divided in sets
      corresponding to decks  and common decks  starting with the same letter (T
      for TPC for example). Sets start with a NEWS deck (T_NEWS for example) and
      end by an END deck (T_END for example).

    o the SICAL is really in (steering routines were dummy before)

    o new MUID bank written to the output


+++++++++++++++++++++++++++++++ PURGED Decks ++++++++++++++++++++++++++++++++++

    RWBKRU      Bookkeeping routine

++++++++++++++++++++++++++++++++ NEW Decks ++++++++++++++++++++++++++++++++++++

    VERSION     contains JULIA version and correction file number
    ABOLDR      called at end of run before reading new run record, it calls
                RCLRUN
    MUNEWR      Initialises muon id part, for use outside of the julia context
    TSIRDP      Dump the TSIR, TSLE, and TSDI banks
    ICASWT      function to witch dead storey address in End-caps

SICAL routines :
    SILSUM      Build energy sums in roads of 2 phi pad width for 3 radius bins
    SILVOI      Build list of neighbours used in Energy sums for Luminosity cuts
    SINAMX      Set Scal name indices
    SIBKPR      Print Scal output banks
    SICLCO      Cluster position correction in Sical
    SICLUS      Clustering in Scal
    SIDAMP      Expand addresses of a dead Amplex of address IAD into NPA
                addresse
    SIECOR      For cluster # ICLU in bank SCLS returns the calibration
                corrected
    SILIST      Check if address IAD is among first NROW rows of bank SPDA
    SISWAP      Unswap some cables
    SISTAT      SICAL Accumulate job statistics


+++++++++++++++++++++++++++++ Modifications in ++++++++++++++++++++++++++++++++

 VDET ----------------------------------------------------------------------
    VDRCUN      Overwrite the control flags if reprocessing
    VINIRU      remove duplicated statements

 TPC  ----------------------------------------------------------------------
    FREFIT      modification for DST-DST reprocessing :
                set number of VDET points to 0 since FREFIT produces only FRFT
                0!

    Corrections for dE/dx variations with respect to angles and drift lengths
    due to threshold. Count number of wires which dont fire but should have and
    modify the truncated mean for this effect.

    TFTWTB      include null wire hits
    TKSGDP      Use pulse length banks
    TRDWP2      Return Length of pulse above threshold
    TRKELS      sided truncated mean
    TRKWRA      make list of dead wires for TFTWTB
    TWIRES      handle pulse length banks
    TRKELS      use projected track length rather than track length

 CALO ----------------------------------------------------------------------
    CPADWR      print statement removed

 ECAL ----------------------------------------------------------------------
    EGETDS      call ICASWT to switch dead storey addresses

 HCAL ----------------------------------------------------------------------
    HMROAD      minor modification

 MUON ----------------------------------------------------------------------
    MUASS       some protections for use outside julia
    MUTEST      use MUEX rather than TREX bank to associate hit-tracks

 SICAL ---------------------------------------------------------------------
    SIBOOK,SIHIST
                Book and fill histos.
    SIINIJO     Initialize Job
    SIINIRU     Init.   run
    SIPREDA     prepare data before reconstruction
    SIPRSUM     print end of job summary

 SKELETON ------------------------------------------------------------------
    RWREVT      Drop POT banks if no output is requested
    RDEFIO      MUID bank added to output
    RECEVT      - call HRHTUB and MRMHIT (HCAL and muon chambers efficiency map
                for MC events)
                - call MUIDO, global hcal+mu-chambers for inal muon ID and
                shadowing treatement
                - for DST-DST reprocessing with VDET :
                use FRFT and not TGFT since TGFT is not produce in that case
    RPARAC, RNXMOD
                remove timing estimate of RWBKRU, implemement timing of MUIDO
    RCLRUN      write end of job summary banks only when running on falcon,
                list of banks : 'RUNRLEHILUMILBAKLVHIJSUMXSGEXSHIXSSC'
    RLOOPR      minor modification due to use of ABOLDR from now on
    RWRUNH      minor modification for DST-DST reprocessing outside FALCON



               Julia 2.62 correction file 03 is now installed on

                        ALWS, VXCERN, IBM, CRAY, FALCON



                     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                            THIS IS A TEST VERSION
                                  IT REQUIRES
                                ALEPHLIB >= 140
                                  DAF >= 170
                     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



SICAL Clusters are included in PECO/P... banks as for LCAL clusters.
PECO region code for SICAL clusters is 256 (192 for LCAL).


+++++++++++++++++++++++++++++ Modifications in ++++++++++++++++++++++++++++++++

 TPC  ----------------------------------------------------------------------
    TFLNKS      bug fixed
    TRNCON      access TPC T0 through GTT0GL alephlib function
    TCOOR, TCOORT, TSAWIR, TWINCO
                call alephlib routines TFICOR, TCORES to correct for
                shorts on TPC field cage
 LCAL ----------------------------------------------------------------------
    XPTENB      From now on LCAL wire trigger is used instead of pad trigger
 SICAL ---------------------------------------------------------------------
    SICRPOT     extend PECO/PCOB/PCRL  with Sical clusters region code is 256
    SIGACO      take calibration constants from Data Base
    SIGTDP      handle dead pad information even if missing
    SIPREDA     protect against missing dead pad information
    SIVOIS      fix to treat MC data correctly
 SKELETON ------------------------------------------------------------------
    RWREVT      Drop E list is no julia output is requested
    RMONII      two variables not declared
    RFJCON      access TPC T0 through GTT0GL alephlib function





               Julia 2.62 correction file 08 is now installed on

                        ALWS, VXCERN, IBM, CRAY, FALCON



                     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                             THIS VERSION REQUIRES
                                ALEPHLIB >= 140
                                  DAF >= 170
                     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



Compare to correction file # 3, changes are :


+++++++++++++++++++++++++++++ Modifications in ++++++++++++++++++++++++++++++++

 TPC  ----------------------------------------------------------------------
    TFTWTB      small bug corrected
    TRKWRA      "   "   "   "
 HCAL ----------------------------------------------------------------------
    HPRDIG      minor correction
 MUON ----------------------------------------------------------------------
    MINIJO      obsolete code removed
 LCAL ----------------------------------------------------------------------
    LACCEP, LIDCOD, LOLERR, LPRLUM, LTGBIT, LTGBIT
                New method 10 to compute luminisoty stay clear of Sical shadow
 VERTEX --------------------------------------------------------------------
    YFVERT      some protections added
 SKELETON ------------------------------------------------------------------
    RPASS0, RECONS, RIPAS0, RWTDPV
                mods for PASS0



! 07-OCT-1992  2.63


                        Julia 2.63 is now installed on

                            ALWS, VXCERN, IBM, CRAY


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                            THIS IS A TEST VERSION
                                  IT REQUIRES
                                ALEPHLIB >= 141
                                  DAF >= 171
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



+++++++++++++++++++++++++++++++ PURGED Decks ++++++++++++++++++++++++++++++++++

    ABMAIN

++++++++++++++++++++++++++++++++ NEW Decks ++++++++++++++++++++++++++++++++++++

    ALBITW      get # of bits in a machine word
    ESCDEF      Ecal Small Defect Correction
    ECPLM       Ecal correction for wrong plane killed


+++++++++++++++++++++++++++++ Modifications in ++++++++++++++++++++++++++++++++

 VDET ----------------------------------------------------------------------
    VDMCEF      mods for inefficiency simulation in Monte-Carlo

 TPC  ----------------------------------------------------------------------
    THTROW      one undefinied variable

 MUON ----------------------------------------------------------------------
    MINIJO      PCUTMA (cutoff momentum for track association to mu-ch)
                decreased from 3 to 2.2 GeV

 ECAL ----------------------------------------------------------------------
    EINIRU      call to ESCDEF added
    EPREDA      call to ECPLM  added
    ETHRES      correct for eventual coherent shift of wire energies
    EGETDS, EINIRU, EGACST,E4COSZ
                use GTDBBK common deck to access run header / daf
                setup dep. banks

 SICAL ---------------------------------------------------------------------
    SIGTDP      small modification

 SKELETON ------------------------------------------------------------------
    AAMAIN      contains code from purged routine ABMAIN,
                BOS initialised,
                open data cards
    RDCARD, RINJOB
                code moved to AAMAIN

! 22-OCT-1992  2.64


                        Julia 2.64 is now installed on

                            ALWS, VXCERN, IBM, CRAY




+++++++++++++++++++++++++++++++ PURGED Decks ++++++++++++++++++++++++++++++++++

    RWBKIN      initialisation of bookkeeping (obsolete option)


+++++++++++++++++++++++++++++ Modifications in ++++++++++++++++++++++++++++++++


 MUON ----------------------------------------------------------------------
    MPREDG      obsolete code removed

 ECAL ----------------------------------------------------------------------
    E4COSZ,EGETDS,EINIRU
                Modification to access data base banks
    ELONGI      modification to electron longitudinal estimator in end-cap
    ECFILS      check is DAQ error

 SKELETON ------------------------------------------------------------------
    RREVHE      use parameter statement

               Julia 2.64 correction ile # 7 is now installed on

                      ALWS, VXCERN, IBM, CRAY, SHIFT, CSF


    This is the version which will be used for the 92 Monte-Carlo production
    together with data base 175.


+++++++++++++++++++++++++++++ Modifications in ++++++++++++++++++++++++++++++++


 VDET ----------------------------------------------------------------------
    VDMCEF      handle dead channels
    VINIRU      correction for VDET timing problem in early 92 runs

 TPC ----------------------------------------------------------------------
    TRNCON      mods for new TPCSIM
    TPKILL      idem

 LCAL ----------------------------------------------------------------------
    LUPACR      RTABL should have been ITABL, protection if LOCL not filled

 ECAL ----------------------------------------------------------------------
    ECPLM       minor modification
    ELONGI      modification for Rl computation in MC
    E4COSZ      one uninitialized variable

 SICAL ---------------------------------------------------------------------
    SIGTDP      Add protection against bad addresses from SDPD
    SISWAP      Unswap two cables in a range of runs [ 17537, 17618 ]


               Julia 2.64 correction file # 8 is now installed on

                      ALWS, VXCERN, IBM, CRAY, SHIFT, CSF


             This is the version which will be used together with
                               alephlib 144 and
                               daf      177
                       for the reprocessing of 91 data.


+++++++++++++++++++++++++++++ Modifications in ++++++++++++++++++++++++++++++++


 TPC -----------------------------------------------------------------------
        Two types of correction :
                -   Make the code backward compatible, constants are loaded from
                    the new database bank TC7X (Data and MC).
                -   Two bug fixed in treatement of MC dE/dx. Agreement with data
                    should be better now.

    TCDUMP      Small correction
    TFTWTB      Database control of constants
    TPKILL      Fix bug
    TRDWP2      Fix bug such that LMPH REALLY is returned
    TRKELS      Database control such that old algorithm still works
    TRKWRA      Database control of constants
    TRNCON      Load TC7X from daf


               Julia 2.64 correction file # 9 is now installed on

                      ALWS, VXCERN, IBM, CRAY, SHIFT, CSF



+++++++++++++++++++++++++++++ Modifications in ++++++++++++++++++++++++++++++++


 TPC -----------------------------------------------------------------------
    TRNCON      Bug fix for multi-runs julia processing mod

! 8-APR-1993    2.65


                    Julia 2.65 corr. 07 is now installed on

               CERNVM, ALWS, ALPHA, VXCERN, DECS, SHIFT and CSF


    o   This version will be used for 93 data processing.

    o   There will shortly be a new version : 270 identical to 265 but with the
        HISTORIAN library resequenced!

    o   WARNING this version requires :

            Alephlib >= 15.1    ,   DAF >= 180

    o   Compared to JULIA 264.09, changes are :



    New features :

        -   PASS0 produces VDET hot chanels bank list (VHOT)
        -   FRFT 0 is swapped to FRFT 3 after tracking ==> all code called after
            tracking will now use FRFT 2 by default!!!.
        -   Beam Spot is now computed inside JULIA, result is stored in
            end-of-run bank ALPB
        -   new photon banks PGPC and PGID are produced (old ones EGID, EGPC
            dropped)
        -   PECO bank NR = 1 produced (angles corrected by event vertex
            position)
        -   New variable FMCRUN in RCURNT common deck, .true. if MC run is being
            processed ==> standard definition of MC run in JULIA to avoid the
            run number 2000 border definition ( .LT. or .LE. ????), routines
            with special sequences for MC have been modified to use FMCRUN.



+++++++++++++++++++++++++++++++ PURGED Decks ++++++++++++++++++++++++++++++++++

    Reshuffling of JULIA steering, following decks are purged :
    RWTDPV, RDBSUM, REJOBC, EHIS, RDJINI, RLOOPC, RECEV2, RDOJOB, REDIRC,
    MINHEA, FIDHEA, RDEFMI

+++++++++++++++++++++++++++++++ NEW Decks +++++++++++++++++++++++++++++++++++++

    RCPAS0      Write TDPV and VHOT banks in PASS0 summary file
    RZEJOB      Zero statistics arrays which count inside the JOB
    EFBOOK      replace EHIS (Booking of histograms for mask analysis)
    VDCRUN      VDET end of job printout was in fact an end-of-run routine, call
                VCRHOT to produce hot chanels list
    LPHIST      set of parameter of lcal energy histograms
    SIEPCR      Return SICAL PAD ENERGY with all corrections
    SIESUM      For cluster # ICLU in bank SCLS returns the Cluster energy as
    SIFSUM      For cluster # ICLU in bank SCLS returns the Cluster energy per
    SILHCR      Create and fill the SILH banks
    SILUMI      Apply Lumi cuts and compute Luminosity in SICAL, fill SLUM
    SIPRLUM     SICAL LUMI  Summary
    SIPSUM      For cluster # ICLU in bank SCLS returns the Cluster energy per
    SIPVOI      For pad indices IR,IP,IM find array of 9 pads in first 5 layers
    SILUAD      Fill bank SILUM by increasing counts
    SICAOK      Checks HV bits and Trigger enabled for Sical analysis
    STRIGF      Return trigger type for current event
    STMASK      Return trigger masks for current run
    GAPECO      Built PECO bank NR=1 for modified Energy,Vertex
    GASTEER     Steering for photon
    VBSCOM      Constants for Beam Spot computation
    VBSPOT      Measure the beamspot using the ALEPH VDET
    VBMFIT      Beamspot position fit
    VCBFIT      Close form beamspot fit
    VDETOK      simple XVDEOK to be used in beam pot package
    VBSINR      Initialise Beam Spot position computation for the run
    VBSCLR      End of Run Beam Spot position computation, close bank


+++++++++++++++++++++++++++++ Modifications in ++++++++++++++++++++++++++++++++

 VDET ----------------------------------------------------------------------
    VDRCUN      Set only final P.H. and Hot chanels for PASS0 mode
    VINIRU      get DB banks depending on setup code through ALGTDB instead of
                MDARD
                call VBSINR

 TPC  ----------------------------------------------------------------------
    TPRSUM      make it shorter
    TFITHL      save statement added to make some compiler happy.

 TRACKING ------------------------------------------------------------------
    FTRACK      one protection added for LOG10
    TCOOR       protection added

 MUON ----------------------------------------------------------------------
    MPREDG      Bad cabling in MA6B (in A162 bus#3 inverted with bus#14 only for
                strips 0-95)

 LCAL ----------------------------------------------------------------------
    LPRLUM      make it shorter!!
    LGNGBX      make use of FMCRUN variable
    LCALIB, LINIRU, LPRCAL, LPRLUM
                make use of LPHIST common deck
    LINIRU      montecarlo - use AGETDB instead of MDARD to get LALI
    LTRACK, LTRACE, LIDCOD, LCHIST, LUPACR
                protection added if SATR not present

 SATR ----------------------------------------------------------------------
    SRDEAD      remove obsolete code to allow some banks to be dropped from daf

 ECAL ----------------------------------------------------------------------
    ECFILS      one printout suppressed in case of SICAL only triggers
    ERCUPI      cosmetics
    E4OVRL      protection added
    GAPECO      protection added
    ICASWT      Dead pad addresses are no more swapped in end-cap

 SICAL ---------------------------------------------------------------------
    SIBKPR      Print SILH and SILU banks
    SIBOOK      Histos added
    SICLCO      Z of cluster added as argument
    SICLUS      correct for pad center coordinate, get partial Energy sums
    SICREC      call luminosity computation
    SICRPOT     drop some banks in case of SICAL only triggers
    SICRUN      call SIPRLUM
    SIECOR      correct energies
    SIGACO      protection added
    SIHIST      histos filled
    SIINIJO     call SINAMX
    SIINIRU     get Calibration constants, reset lumi stat., book SLUM bank
    SILSUM      uses 5  Doublets stacks depth
    SILVOI      "       "           "       "
    SIPREDA     apply calibration
    SISWAP      swapped cables handling


 HCAL ----------------------------------------------------------------------
    HCALRD      printout shorter

 V0 ------------------------------------------------------------------------
    YMFV0S      documentation added

 SKELETON ------------------------------------------------------------------
    AAMAIN      remove calls to obsolete routines
    RCURNT      add LOGICAL variable FMCRUN to common deck (.true. if MC
                processing), add RENLEN  = LEP c.m. Energy in GeV variable
    RFLAGS      add LOGICAL variables FREPRO, FPASS0 to common deck (.true. for
                POT-POT reprocessing and PASS0 mode respectively)
    RLUNIT      add PASLIS  variable : list of banks written out by pass0
    RPASS0      variable NHOTBF added to common (number of VDET hot chanels
                found in previous PASS0 run)
    RBKHIS      call EFBOOK instead of EHIS
    RDCARD      decoding DEBU card
    RDEFPO      cosmetics
    RDETEC      switch on VDET by defaults for PASS0 mode
    RECEVT      Swap FRFT 0 <-> FRFT 3 after tracking and back at the end of
                reconstruction !!!! this means ALL code called after tracking
                will now use FRFT 2 instead of FRFT 0 !!!!
                call VBSPOT accumulate statistics for Beam Spot position
                computation
                process VDET if random trigger (trigger bit 31 set)
    RECONS      call GASTEER for new photon banks PGPC and PGID
    RERROR      routine autoinitialized but still backward compatible
    RFORMA      add definition of new produced banks
    RINCND      some modification in start of run printout
    RINJOB      mods for new PASS0
    RINRUN      "   "   "   "      + prinout mods + VBSINR called
    RIPAS0      Get pass0 constants from PASC daf bank
    RLOOPR      cosmetics
    RMONII,RMONIT   cosmetics
    RNXMOD      handle new module GASTEER and VBSPOT
    RPREDA      prepare EFLOW stage only if EFLOW requested
    RPRSTA      new printout formats
    RPRSUM      "       "       "
    RREVHE      use FREPRO variable, create JEDS bank
    RTITLE      call ALMACH alephlib routine to get computer name
    RWREVT      drop E list if no output is requested
    RPOTCL      Dropping detector banks for Sical only triggers
    RCLRUN      call VBSCLR
    RCJSUM      add date and time of processing in JSUM bank


                    Julia 2.65 corr. 15 is now installed


This is the version currently running on FALCON

+++++++++++++++++++++++++++++ Modifications in ++++++++++++++++++++++++++++++++

 SCAL ----------------------------------------------------------------------
    SILUMI      minor mod

 LCAL ----------------------------------------------------------------------
    LGTBIT      changes due to new trigger bit definition
    LINIRU      create LEHI, LUMI, LBak, LVHI, LONL with nr = run number

 TPC -----------------------------------------------------------------------
    FPIDEN      Replace explicit link to FRFT 0 by NAMIND


 VDET ----------------------------------------------------------------------

 SKELETON ------------------------------------------------------------------
    RFORMA      new formats defined
    RIPAS0      swap previous VHOT bank to write it out if not enough event
                at end of run
    RDEFIO      remove call to RFORMA put it in RINRUN
    RINJOB      ADD RLEP to SUMLIS
    RINRUN      Add call to RFORMA
    RCPAS0      If VHOT is Empty (Not enough events drop it and restore previous
                one)
    RWREVT      write out properly compressed banks
    RLOOPR      write our run records which come in middle of events
    RDEFIO      open the file for monitoring if requested
    RCJSUM      Define Lumi coincidence as LCAL before 93 , Sical from 93 on
    RMONIT      Add protection against crazy PYFR bank


                    Julia 2.65 corr. 20 is now installed


This is the version currently running on FALCON

This will be the last correction file for this julia version.

+++++++++++++++++++++++++++++ Modifications in ++++++++++++++++++++++++++++++++

 SCAL ----------------------------------------------------------------------
    SICLUS      Minor modification
    SIPREDA     Protection against invalid SIDI addresses
    SILUMI      Minor modification

 TPC -----------------------------------------------------------------------
    TRKELS      make code backward compatible

 VDET ----------------------------------------------------------------------
    VPREDA      do not call VDMOIS on laser events
    VTRLAS      New function to flag laser events

 SKELETON ------------------------------------------------------------------
    RINRUN      One format modified
    ABOLDR      Make sure that julia version of the routine is used and not the
                alephlib one
    RECEVT      use VTRLAS
    CPOTLC      mod for multi run mode
    RCLRUN      "   "   "       "   "


! 13-JUL-1993    2.70



                    Julia 2.70 corr. 00 is now installed on

               CERNVM, ALWS, ALPHA, VXCERN, DECS, SHIFT and CSF


    This version  is identical to  julia 265.20. The  HISTORIAN library has
    been resequencent. All forthcoming  corrections for Julia must refer to
    this library.





! 15-SEP-1993    2.71



                    Julia 2.70 corr. 03 is now installed

              ALWS, VXCERN, AXAL, CERNVM, SHIFT, CSF and FALCON



                                W A R N I N G

                        alephlib 154 and database 182
                         released today are required

                                W A R N I N G





+++++++++++++++++++++++++++++    New Decks     ++++++++++++++++++++++++++++++++

 TPC -----------------------------------------------------------------------
    TPADFX      synchronize TPAD and TPDI bank when one hit is missing in the
                TPAD bank but not in the TPDI bank.
    TPCRUN      Run summary from TPC reconstruction,
                produces JTRE and JTDX banks


+++++++++++++++++++++++++++++ Modifications in ++++++++++++++++++++++++++++++++

 VDET ----------------------------------------------------------------------
    VCBFIT      protection against negative sqrt

 HCAL ----------------------------------------------------------------------
    HDGPOT      RERROR was called with wrong number of arguments

 IPC -----------------------------------------------------------------------
    IPHCOR      Use alephlib func. IDDIST for calc. of drift-distance.

 TPC -----------------------------------------------------------------------
    TFTWTB      Suppress error message for the channel which is used for the t0
                measurement
    TPREDA      call TPADFX
    TACCMN      Accumulate TPC monitoring information,
                compute raw dE/dx per sector
    TDXMON      compute RI instead of RI/RI_exp
    TPRSUM      some printout modified
    TRNCON      Load TDXC (dE/dx monitoring constants) bank from daf

 TRIGGER -------------------------------------------------------------------
    XTRSFI      drop XSGE, XSHI, XSSC before creating them

 SKELETON ------------------------------------------------------------------
    RCLRUN      call TPCRUN
    RINJOB      add JTRE and JTDX to list of banks written to SUMR file
    RFORMA      add JTRE and JTDX format definition
    RINRUN      drop JEDS before creating it
    RECEVT      call TDXMON to compute mean dE/dx per mip for the event




                    Julia 2.70 corr. 07 is now installed

              ALWS, VXCERN, AXAL, CERNVM, SHIFT, CSF and FALCON



+++++++++++++++++++++++++++++ Modifications in ++++++++++++++++++++++++++++++++

 VDET ----------------------------------------------------------------------
    VCBFIT      protection against negative SQRT

 TPC -----------------------------------------------------------------------
    TPRFX       cleanup to make Unix fsplit working
    TPCRUN      produce JTDX bank
                restrict dE/dx fit to the FWHM interval around maximum
    TRNCON      Load TDXC bank from daf
    TSTATE      common rename to TPCSTA to make shift9 happy

 ECAL ----------------------------------------------------------------------
    ECHEDC      mods for POT-POT reprocessing

 SKELETON ------------------------------------------------------------------
    RDCARD      minor mod.
    RLOOPR      write out slow control records only if one run record has been
                read in
    RCLRUN      mod for POT-POT reprocessing
    ALTIME      use TIMEL instead of TIMEX which does not work on AXPs


                        Julia 2.71 is now installed on

               CERNVM, ALWS, ALPHA, VXCERN, DECS, SHIFT and CSF


    o   This version will be used for 93 data RE-processing.

    o   WARNING this version requires :

            Alephlib >= 15.5    ,   DAF >= 184

    o   Compared to JULIA 270.07, changes are :



+++++++++++++++++++++++++++++++ NEW Decks +++++++++++++++++++++++++++++++++++++
    SIDVOI,SIDSUM,SIDCOR
                new routines for SiCAL dead pad energy correction
    TWRRDP      Dump TPC raw and reduced wire data for sector SLOT
    TRDWP3      Reduce TPC wire pulses not handled by the TPP


+++++++++++++++++++++++++++++ Modifications in ++++++++++++++++++++++++++++++++

 VDET ----------------------------------------------------------------------
    VINIRU      Load VDSM daf Bank
    VDMCEF      hit position smearing for MC
 TPC  ----------------------------------------------------------------------
    TACCMN, TDXMON	
                Call TDEDXV alephlib routine to load dE/dx calib. const.
                TC2X is not used any more


    All of the following changes were necessary to overcome the bias in
    dE/dx caused by tracks being close together.

    TWIRCT      add some extra parameters
    TCDUMP      call to TWRRDP added
    TFTWTB      get rid of TWPU and add DZNLTW
    TKSGDP      use TWRF instead of TWPU
    TRNCON      Load TC7X and TWTC bank, add DZNLTW cut to TC7X
    TWIRES      split double peaked pulses and add reference bank TWRF
                Obsolete TWPU bank removed.

 ECAL ----------------------------------------------------------------------
    ECLAMP       save work bank indices in common

 SICAL ---------------------------------------------------------------------
    SICLUS      introduce dead pads energy correction to cluster attributes
    SIEPCR,SIECOR
                introduce some extra correction to Energy calibration
    SIPREDA     force Energy calibration to be applied for REAL DATA
    SIINIRU     load dead pad energy correction constants from Dbase
    SIGTDP      force loading of dead pad list from Data Base

 SKELETON ------------------------------------------------------------------
    RDCARD      mod for multi pot-pot repro.
    RECEVT      Force VDET reconstruction if VREC data card is found
                (---> this is for Rick and for single particles)
#endif
