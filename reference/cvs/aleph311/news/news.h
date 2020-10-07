   on AXP/VMS:
              source:  ALEPH311:[*]*.F,ALEPH311:[INC]*.h
              library: ALE:ALEPHLIB.OLB, _D.OLB, .NEWS

   on UNIX :  source:  $ALROOT/aleph311/*/*
              library: $ALEPH/gen/libalephlib.a     -> libalephio311.a
              library: $ALEPH/gen/libalephlib_dbx.a -> libalephio311_dbx.a

   --------------------------------------------------------------------
 ! 19990518 correction file no.1 for ALEPHLIB 31.1

   N.B. The fix to the bug in EBECMD reported by Gary Taylor will not be
   implemented until after the summer conferences, in order to keep data and
   MonteCarlo consistent.

   In /prin
    FRFTDP : Read clock frequency in Hz from TSOR bank (W.Wiedenman)

 ! 19990419 ALEPHLIB 311.0

   New version for 1999 data-taking.

   In /kine 
    KCLEAN : Add XVRT bank to list of possibly user modified banks (B.Bloch)
    KEJOB  : Add bank KSEC to list of end of run banks             (B.Bloch)
    KSECBK : New - BOOK and fill bank KSEC with cross section info (B.Bloch)
    KXLUPR : New - print out jetset switches and parameters        (B.Bloch)

   In /ytop
     YFMVTR : Bug fix - Avoid division by zero when cmas=0 (E.Rohne)

   --------------------------------------------------------------------
 ! 981118 correction file no.3 for ALEPHLIB 31.0
   In /xlum
     XHVFIX : Fixes to varaiable declarations (A.Waananen)
 
 ! 981117 correction file no.2 for ALEPHLIB 31.0
   In /xlum
     XHVFIX : Bug fix, database was initialised with period number
              instead of run number                                (M.Cattaneo)

 ! 981103 correction file no.1 for ALEPHLIB 31.0
   In /alef
     GETLEP : Bug fix - Fill number was not filled in certain cases (J.Boucrot)

   In /xlum
     XHVBIT.H : New. Contains HV bit definitions                   (M.Cattaneo)
     XHVFIX : New. Repairs HV bits when they are known to be wrong (B.Bloch)
     XHVSTA : Add call to XHVFIX.                                  (B.Bloch)
              Take HV bit definitions from XHVBIT.H                (M.Cattaneo)
     XLSLUM : Take HV bit definitions from XHVBIT.H                (M.Cattaneo)

 ! 981029 ALEPHLIB 310.0

   In /alef
     ALVERS : Print out version number only once in the job        (M.Cattaneo)
     GETBP  : New. Moved to Alephlib from Alpha                    (J.Boucrot)
     GETLEP : Completely rewritten to give always the best possible
              available LEP energy and beam position for LEP1 and LEP2
              (See Marco's Mainz talk of 980926)                   (J.Boucrot)
     GETLE1 : Replaced with version from Alpha (adds a protection) (J.Boucrot)
     GETLE2 : Replaced with version from Alpha (adds a protection) (J.Boucrot)
     FIXALPB: New. Moved to Alephlib from Alpha                    (J.Boucrot)
     XFMCBP : New. Moved to Alephlib from Alpha                    (J.Boucrot)

   In /dedx
     TDEDXV : Do no apply HV correction from 1998 (KRUN>45000) (F.Cerutti)

   In /gam
     CORAD94: Apply correction from EGAZ bank. 
              Switched off if OLGA data card is present  (M-N.Minard)
 
   In /phy
     PAIRFD : Protection to avoid crashes when reading MINIs with
              the ITCO bank                          (T.Greening, J.Boucrot)

   In /vdes
     VGRDAL : Modified to use laser data also                  (G.Sguazzoni)
     VGRDLS : NEW: Add the laser information to the alignment  (G.Sguazzoni)
   --------------------------------------------------------------------
 ! 980928 correction file no.1 for ALEPHLIB 30.9

   In /comp
    PPCORR : Use .NEQV. instead of .NE. to compare two logicals (A.Waanenen)

 ! 980902 ALEPHLIB 309.0

   Modified ITC routines parameterise drift time-distance relation with
   spline require ADBS 240 or greater

   In /ides
    IINRES : New Drift-time params: use Spline from IDSP bank for 1997 
             data onwards (run > 43000). MC will still use IDRP. (J.Sedgbeer)
    IPRDAF : Add print of new variables in /IDRPCC/              (J.Sedgbeer)

   In /inc
    IDRPCC : Add DSPLID and IFLGID to end this COMMON.           (J.Sedgbeer)

   In /itc
    IDDIST : Modify to use polynom. or spline drift-time coeffs. 
             depending on the flag IFLGID in /IDRPCC/            (J.Sedgbeer)

   In /vdes
    VRECOR : New. Bonding error correction for fault types 301,302
             Returns displacement of local wafer coordinate (cm) (J.Rothberg)
   --------------------------------------------------------------------
 ! 980715 correction file no.3 for ALEPHLIB 30.8
   In /vdes
    VDCHNB : NEW - Given the local coordinate, bank number and view, 
                   returns the chip number.                 (M.Thulasidas)
    VDGTCH : NEW - Given the layer, face, module, view and chip in module, 
                   returns a global chip number             (M. Thulasidas)

   In /xlum
    XLSLUM : keep trying to get Trigger Enable mask if not present 
             for first event of run                         (B.Bloch)

 ! 980608 correction file no.2 for ALEPHLIB 30.8
   In /ytop
    YFTVTC : Bug fix - test on vertex location always failed due
             to uninitialised variable                      (P.Hansen)
    The above bug was introduced in Alephlib 30.7 correction file 2. Data
    processed through Julia with Alephlibs 307.2->308.1 have no secondary
    vertices or kinks!
        
 ! 980605 correction file no.1 for ALEPHLIB 30.8
   In /tdes
    TRDDAF : Print calibration bank style message once only (M.Cattaneo)

   In /tpc
    TPDVEL : Print TNFV/TDFV correction message once only   (M.Cattaneo)

   In /xlum
    XTGENB : Bug fix, NAXTBN,NAXTOP were incorrectly initialised
                                                           (B.Bloch,G.Taylor)

 ! 980502 ALEPHLIB 308.0
   This version will be used in Falcon for the 1998 data.

   IMPORTANT note about the version number: in the past, the Alephlib
   version was coded as a two digit number plus one decimal digit (e.g.30.7)
   A second decimal digit was used to denote correction files (e.g.30.76)

   From now, it is coded as a three digit number plus one decimal digit
   for the correction file (e.g. 308.0)

   The reason for this change is that, in the bank RHAH, the Alephlib version
   number is coded as an integer equal to 10*ALEVER. In the previous scheme,
   information about the correction file version was lost.

   In /xlum
    XLSLUM : For real data: after run 45000 (1998) require VDET ok  (B.Bloch)

   In /yv0
    YFQERQ : Use double precision internally, keep ERQ2 non-zero (M.Cattaneo)


 ! 980401 correction file no.6 for ALEPHLIB 30.7
   In /alef
    ALKRAN : New - Build kine run header KRAN    (B.Bloch)
    ALKMAR : New - Create kine event header KMAR (B.Bloch)

   In /edes
    EXPFRF : Bug fix. Correct treatment of d0 sign (C.Mannert)
             The bug affected mainly the calculation of the impact point on
             ECAL for GAMPEX photons

   In /fyxx
    FYFKIN : Fill FSHO monte carlo bank from KSHO bank (B.Bloch)
    FYIRUN : Add FSHO bank to E list                   (B.Bloch)
    FYKINE : Drop also KSHO bank                       (B.Bloch)
    FYTOKI : Handle also KSHO/FSHO banks               (B.Bloch)

   In /kine
    KP6SHO : New - BOOK and fill bank KSHO with fragmentation info (B.Bloch)
    KSHOBK : New - BOOK and fill bank KSHO with fragmentation info (B.Bloch)

 ! 980313 correction file no.5 for ALEPHLIB 30.7
   In /fyxx
    FYKILL : Don't kill vertices in TPC passive material at large angles
             (membrane et al., resistor chain, laser mirrors and prisms)
                                                                (S.Wasserbaech)

 ! 980310 correction file no.4 for ALEPHLIB 30.7
   In /comp
    THLCIR : Use DOUBLE PRECISION for R, to avoid numerical problems
                                                        (M.Cattaneo)
   In /tdes
    THTRAN : Use DOUBLE PRECISION for SP0,CP0, to avoid numerical problems
                                                              (M.Cattaneo)

 ! 980220 correction file no.3 for ALEPHLIB 30.7
   In /kine
    KXP6CO : Fix a WRITE statement               (B.Bloch)

 ! 980216 correction file no.2 for ALEPHLIB 30.7
   In /ytop
    YFTVTC : Restrict vertices to reasonable volume
             to avoid numerical problems             (D.Casper)

 ! 980209 correction file no.1 for ALEPHLIB 30.7
   In /comp
    PPCORR : New - Correct particle momenta for effects of residual 
             distortions in the central tracking detector.
             This is the so called "sagitta correction".    (I.Tomalin)
   In /kine
    KKMOVE : extend NMX from 100 to 200          (B.Bloch)
    KZFRBK : take booking of bank out of do loop (B.Bloch)
    KP6ZFR,KPYZET,KXP6AL,KXP6BR,KXP6CO,KXP6IN,KXP6ST,KXP6TO,PTY6COM.h :
      c.f. KZFRBK,KLUZET,KXL7AL,KXL7BR,KXL7CO,KXL7PA,KXL7ST,KXL7TO,LUN7COM.h
      New - Kingal interface routines for PYTHIA 6.1        (B.Bloch)

   In /ysv
    YSKLLD : Protect against too many tracks                       (D.Casper)
    YSVTRK : Check object quality flag to protect against too many 
             tracks and other catastrophes                         (D.Casper)

   In /yv0
    YFMV0V : Protect against precision problem (M.Cattaneo)

 ! 971202 ALEPHLIB 30.7
   In /alef
    ALTRHV - Obsolete. Use XHVBIT instead (M.Cattaneo)

   In /dedx
    TCHKHV - Call XHVBIT instead of ALTRHV, 
             return .TRUE. for MonteCarlo      (M.Cattaneo)

   In /xlum
    XLUMOK package
   New package which brings together various XLUMOK related routines from Julia
   and Alpha. Includes also XVDEOK routines

   XLUMOK adaptations by Brigitte Bloch, XVDEOK by Henry Seywerd
   Code reviewed and optimised by Marco Cattaneo
   
   LLUMOK : Checks HV status, enabled triggers, and t0 synch. for LCAL
   SLUMOK : Checks HV status, enabled triggers, and t0 synch. for SICAL
   VBITGD : Checks readout status of VDET
   XHVBIT : Returns raw HV status bits, without run/detector specific repairs
   XHVSTA : Check HV status bits (from QHVSTA in ALPHA)
   XLSLUM : Checks HV status, enabled triggers, and t0 synchronization
   XLUMOK : Checks HV status, enabled triggers, and t0 synchronization, both
            for SICAL and LCAL with year dependent setup
   XRE133 : Flags laser events taken during LEP 1.5 data (from QRE133 in ALPHA)
   XRE94  : Flags laser events hidden in runs 25520 to 25530 taken in 1994
            (from QRE94 in ALPHA)
   XT0SYN : Get t0 synchronization information
   XTGENB : Check which triggers are enabled
   XVDEOK : Determines HV state of VDET

 ! 971105 ALEPHLIB 30.6

   In /alef
    ALBUNCH: New. Get number of bunches from LEP information in 1997 (B.Bloch)

   In /comp
    UFITQL,UFMS,UMSERR,YV0ONE : fix variable type inconsistencies
        in function calls, for Linux                            (A.Waananen)

   In /edir
    SELEVT : fix variable type inconsistencies in function calls, 
             for Linux                            (A.Waananen)

   In /kalm
    UFG2GT,UFTKAL : fix variable type inconsistencies in function calls, 
                    for Linux                            (A.Waananen)
    UFTKAL : Disable removal of outliers if track has 4 or less remaining
             3-D coordinates. Fail if numerical problem would reduce track
             to exactly 3 hits.                               (D.Casper)

   In /phy
    ALTHRU,PIDECY : fix variable type inconsistencies in function calls, 
                    for Linux                            (A.Waananen)

   In /prin
    FRFTDP : fix variable type inconsistencies in function calls, 
             for Linux                            (A.Waananen)

   In /ptoj
    PITMAJ,TUN1NC : fix variable type inconsistencies in function calls, 
                    for Linux                            (A.Waananen)

   In /trig
    X1BOOK,X2TRIG : fix variable type inconsistencies in function calls, 
                    for Linux                            (A.Waananen)

   In /vdes
    VCHNST,VGWFVU,VGWFXY : fix variable type inconsistencies in 
                           function calls, for Linux           (A.Waananen)

   In /ysv
    YSPAIR : Do not put in YSVX/-2 track pairs failing cuts        (D.Casper)
    YSVRTX : Remove cut on max number of FRFT tracks               (D.Casper)
             Fix variable type inconsistencies in function calls, 
             for Linux                                           (A.Waananen)
    YSVTBK : Use hit bitmasks to verify track direction            (D.Casper)
    YSVTRK : Put only JYSVQU=0 objs. in mask of objs. already used (D.Casper)

   In /ytop
    YTCONV : Bug fix - declare WTX, VARWX arrays (W.Manner)
    YCIRCR,YVPOSS : fix variable type inconsistencies in function calls, 
                    for Linux                            (A.Waananen)

   In /yv0
    YMFMIN,YMFV0V : fix variable type inconsistencies in function calls, 
                    for Linux                            (A.Waananen)

   --------------------------------------------------------------------
 ! 971027 correction file no.4 for ALEPHLIB 30.5
   In /tpc
    TFIXT0 : New - Correct for 1997 hardware problem which shifted T0 
                   by several ns. during periods of high luminosity at 
                   start of fill                            (I.Tomalin)

 ! 971017 correction file no.3 for ALEPHLIB 30.5
   In /ptoj
    TFICOR : Don't make corrections for new alignment (W.Wiedenmann)
   In /tpc
    TCORES : Don't make corrections for new alignment (W.Wiedenmann)
   In /vdet
    VTDERV : Improve the protection against rounding errors (A.Bonissent)

 ! 971001 correction file no.2 for ALEPHLIB 30.5
   In /edir
    SNGMTR : Remove CALL from function references, for Linux. (A.Waananen)
   In /kalm
    UFECAL : Remove CALL from function references, for Linux. (A.Waananen)
   In /vdet
    VDTTRA,VTRFIT: Remove CALL from function references, for Linux.(A.Waananen)

 ! 970915 correction file no.1 for ALEPHLIB 30.5
   In /kalm
    UFGAIN - Protect against zero determinant
    UFTKAL - Fit the track (using a zero gain matrix for this point) 
             even if something went wrong in the determinant       (D.Casper)

 ! 970829 ALEPHLIB 30.5
   In /alef
    GETLE1 : New. Gives the exact run energy for 1993 -> 1995 LEP1 runs
             from the latest estimations of the LEP energy        (J.Boucrot)
    GETLUM : get lumi from 'LUMI' if it's 0. in 'RLUM'            (J.Boucrot)
   In /comp
    YHELIX : New - Determine the turning angle and momentum of an 
                   FRFT track at a point                           (D.Casper)
    YSENSE : New - Determine the sense of direction of a track     (D.Casper)
   In /kine
    KBPART : handle properly particles which should be tracked by
             GEANT (flr)
   In /tdes
    TSCINT : Add 201 statement after end of DO loop for Linux (A.Waananen)
   In /tpc
    TDFVRU : Drop TDFV bank if a good row is not found (D.Casper)
   In /ysv
    YSKLLD : Do not check for V0s or ITC tracks in the kink veto (D.Casper)
    YSVFIT : Include only charged tracks in the kink veto        (D.Casper)
             Clear vertex quality flag (bug fix, 090997)         (D.Casper)
    YSCLLD,YSGETS,YSPCUT,YSTLLD,YSVBLD,YSVRTX,YSVTBK,YSVTRK :
      Changes to interface nuclear interactions and kinks to
      energy flow package (ENFLW 300)                            (D.Casper)

   --------------------------------------------------------------------
 ! 970717 correction file no.4 for ALEPHLIB 30.4
   In /vdet
    VDHTER : Fix the bug on WERRA ((3,3) instead of (2,2))        (Manoj T.)
    VHERR1 : Modify the error param. (due to change in S/N in MC) (Manoj T.)

 ! 970716 correction file no.3 for ALEPHLIB 30.4
   In /comp
    UFITQL : Put cut on NTPC hits, COS(theta) earlier in the routine 
                                                         (M.Cattaneo)

 ! 970710 correction file no.2 for ALEPHLIB 30.4
   In /ytop
    YFMVTR : Protect against astronomical chi^2 (D.Casper)

 ! 970708 correction file no.1 for ALEPHLIB 30.4
   In /tdes
    TSCINT : Protect against infinite momentum tracks 
             (e.g. if magnetic field is off)           (M.Cattaneo)

 ! 970704 ALEPHLIB 30.4
   In /vdes
    VGRDAL : Modified to use VNGBVNLC if they exist (F.Ranjard)

   --------------------------------------------------------------------
 ! 970618 correction file no.3 for ALEPHLIB 30.3
   In /ptoj
    PJPFXT - New: Create the FXTR bank and fill it from PFXT  (D.Casper)

 ! 970613 correction file no.2 for ALEPHLIB 30.3
   In /kine
    KCLEAN : Allow user to supersede GKBR bank             (B.Bloch)
    KLUZET : New - transfer fragmentation info from JETSET (B.Bloch)
    KXL74A : Extend particle codes from JETSET             (B.Bloch)
    KXL7AL : Handle correctly Tau's from PYTHIA            (B.Bloch)
    KXL7PA : Extend particle codes from JETSET             (B.Bloch)
    KZFRBK : New - book KZFR bank                          (B.Bloch)

   In /vdes
    VCORMP : modify for case 8 rphi (J.Rothberg)

 ! 970528 correction file no.1 for ALEPHLIB 30.3
   In /kalm
    UFTKAL - protect against incorrect angular subtraction during smoothing;
             add support for tracing the fit of a track by calling 
             UFTRAC(.TRUE.) immediately before fitting.              (D.Casper)

   In /vdet
   VDTTRA - add UFTR card to flag a track for Kalman filter printout during
            fit.  NR=event number, with entries on the card indicating which
            track(s) of that event should be dumped.  
            Example:  UFTR 10 / 1 2 5 dumps tracks 1, 2, and 5 of event 10. 
            Note there is no selection by run number since the user is presumed
            to be dealing with a small set of events when this feature is 
            enabled.                                                 (D.Casper)

 ! 970514 ALEPHLIB 30.3
   In /alef
    GTT0GL - Correct bug in treatment of new alignment (D.Casper)

   In /fyxx
    FYRELA.H : Increase MAXMCX to 2000 (from 1000) to cope with PHOT02
               events with too many KINE banks                   (W.Manner)

   In /kalm
    UFDISC - Set the energy loss to zero if it is more than 5% of
             the track's starting energy                           (D.Casper)
    UFLOSS - Set the energy loss to zero if it is more than 5% of
             the track's starting energy                           (D.Casper)
    UFVDMS - Protect against very high incidence angle tracks      (D.Casper)

   In /tdes
    TCTSEC - Correct for transverse drift velocity                (D.Casper)
    TCTGLB - Disable transverse drift correction if sector number
             is negative (to allow old behavior to be selected)   (D.Casper)

   --------------------------------------------------------------------
 ! 970425   correction file no. 9 for ALEPHLIB 30.2
   In /alef
    GTT0GL - check whether new alignment banks are present before adjusting t0
                                                                    (D.Casper)
   In /kalm
    UFVDMS - fix overwrite of flag enabling energy loss in Kalman filter 
                                                                    (D.Casper)
   In /phot
    EBLEAK,EBLPST,EBPREP,EBPRGA,ECLEAK - increase to 100 GeV the cut on
             maximum energy of a cluster                         (M.N.-Minard)
   In /ptoj
    TUN1NC - fix unintentional disabling of wire coordinates
             print a message indicating whether wire coordinates will be used
             avoid error messages when reading junk events by removing error
             return on absence of PCOI (return silently instead)    (D.Casper)
   In /vdet
    VDHTER - fix incorrect order of track momentum components in call to VHERR1
                                                                    (D.Casper)

 ! 970417   correction file no. 8 for ALEPHLIB 30.2
   In /kalm
    UFTKAL -  Support for constrained fits in UF2PNT.

   In /tdes
    TRDDAF -  Validity range of TNLO checked.

   In /ysv
    YSVBLD - replace CALL NDROP with IRET=NDROP (NDROP is an INTEGER FUNCTION).
    YSVRTX - remove INIT variable (not used), save name-indices.

 ! 970416  correction file no. 7 for ALEPHLIB 30.2
   In /kalm
    UFTKAL, UFSWMC, UFSWMX, UFSTAT, UFGETT, UBANGL - replace PI with ONE_PI
    UFTKAL, UFVDMS, UFREIN - corrections (D.Casper) to make fit with constraints.
    UF2SCA is removed.
    UFG2GT is added.

   In /inc
    UFTCOM - replace PI with ONE_PI.
         
 ! 970415  correction file no. 6 for ALEPHLIB 30.2
   In /alef
    GETLE2 - new routine (J.Boucrot) :  SUBROUTINE GETLE2(ELEP,IFLAG) 
             Get the "instantaneous" exact LEP energy for LEP 2 runs 

   In /ytop
    YFTVTC - correction (D.Casper).
   
 ! 970404 correction file no. 5 for ALEPHLIB 30.2
   In /ysv
    introduction of the nuclear interactions package written by D.Casper.

   In /ytop
    YFTVTC - new routine (D.Casper) to extend YTOP to ITC-only tracks.
    YFMVTR - corrections (D.Casper) to move around some statements into the
             correct places so that the input logical flags would do what 
             they say.

 ! 970402 correction file no. 4 for ALEPHLIB 30.2
   In /tdes
    TRDDAF TALINI - use the right HAC parameters.

   In /kalm
    UFGETT UFJACO UTPROC UFSWMS UFSWMX UFTRAN - correction to please LINUX
             compiler.
    UFQPRC UFVDMS - remove double definition of a variable.
    UFTKAL UFJACO - add some protections.

 ! 970326 correction file no. 3 for ALEPHLIB 30.2
   In /ptoj
    TUN1NC - load errors packed in PTNC bank.
             remove code written to recompute errors.

 ! 970320 correction file no. 2 for ALEPHLIB 30.2
   In /vdet
    VDTTRA - remove coordinate filtering at the beginning, restore it at
             the end.

 ! 970312 correction file no. 1 for ALEPHLIB 30.2
   In /comp
    UFTTRK - removed, obsolete routine replace by UFTTRA.

   In /vdet
    VDTTRA - new routine which contains the VDET pattern recognition part
             of UFTTRA/alephlib_216.
             this part has been removed from UFTTRA/alephlib_302.
    VTRFIT   calls VDTTRA instead of UFTTRA.

 ! 970306 ALEPHLIB 30.2
             This version contains the new Kalman filter which gives better
             Chi2 than the previous one (D.Casper) 
             This version contains the new field map. When reading an old 
             ADBSCONS the old field map is used (W.Wiedenman)
             This version is forseen to run with JUL302.
             ===> the VDET pattern recognition has to be tuned.
             ===> the library is technicaly backward compatible but results
                  could be different.

   In /alef
    DVMOVE - new routine: DVMOVE(A,B,N) 
             copy A(1:N) into B(1:N), A and B are double precision arrays.
    DVZERO - new routine: DVZERO(A,N) 
             reset A(1:N) to zero , A is a double precision array.  

   In /comp
    UFMATX -  new routine : matrix operations in double precision
              res, a, b, c are mdim x mdim double precision matrices, 
              v is mdim vector, s is a double precision scalar
              UFMMUL (RES,A,B,MDIM) res = a * b
              UFMMLT (RES,A,B,MDIM) res = a * b^T
              UFMSCL (RES,A,V,MDIM) res(i,j) = a(i,j) / (v(i)*v(j))
              UFMADD (A,S,B,RES)    res = a + s * b
              UFMULT (A,B,C,RES)    res = a * b * c^T
   
    UHELIX2 - new routine: UHELIX2 (R,VV0, VV0COV, POS, POSCOV, IRET)
              Given helix and covariance, compute pos and error at radius.

    UFTKAL UFTTRA UFVDMS - are removed, replaced by /kalm/.

   In /kalm
     new routines:
     UBANGL  UF2SCA  UFBEBL  UFCOVA  UFGAIN  UFLOSS  UFSCOV  UFSWMS  
     UFTRAN  ULOAD   UF2ARC  UF2TRK  UFBERR  UFDISC  UFGETT  UFQPRC  
     UFSTAT  UFSWMX  UFTTRA  UPCONS  UF2CRD  UF2USE  UFCHI2  UFECAL  
     UFJACO  UFREIN  UFSWMC  UFTKAL  UFVDMS  UTPROC
      
   In /inc
     UFTCOM   - new include file for the new kalman filter package.
     TPCDRIFT - new include file for new alignment.
     TFCORR   - support for new alignment.
     TJJPAR   - get HAC parameters for new alignment banks.

   In /ptoj
     PTPCOJ - support for wire coordinates
     TUN1NC - include wire coord and error when unpacking PTNC

   In /ides
     IINALI - support for new alignment TPC bank TNOS.

   In /tdes
     TRDDAF, TGHPAL, TALINI, TCTGLB - support for new alignment banks.
     TCRTRN - new routine (copied from TCRTRA) for new alignment.
     TMATUT - new routine auxillary to TALINI.

   In /tpc
     TPDVEL, TFCINI, TLACOR, TZCSVD - support for new alignment.
     TNFVRU - new routine (copied from TDFVRU) for new alignment bank TNFV.
      
   In /ysv
     YSVRTX - main routine of the new /ysv package which finds secondary vertices.
              the routine is DUMMY for the moment.
   --------------------------------------------------------------------------------

 ! 970220 ALEPHLIB 21.6 
   In /alef
    ALELEP - Replace previous code by call to identical routine ALEFIL 
             (M.Cattaneo 10/02/1997)
    ALSECS - Remove test on century for leap year (year 2000 is a leap year)
             (M.Cattaneo 27/02/1997)

   In /edir
    CLAS24 - Bug fix: Gampec photons were not being looked for in bank PGAC,
             where they have been since December 1994 (G.Ganis, 17/02/1997)

   In /muid
    MUREDO - Moved to Julia library (version 285) because it calls Julia
             routines (A.Waananen 5/12/96, M.Cattaneo 19/02/97)

   In /prin
    PRWORK - Obsolete. Removed as it calls non-existent routine BPRWRK
             (A.Waananen 5/12/96)

   In /tpc
    TPDVEL - Some comments corrected (I.Tomalin, 9/10/1996)

   In /trig
    X1APTN - Add endcaps to SNG_N_EL trigger definition (A.Putzer 04/02/97)

   In /vdet
    VTRKEX - Place VDMS on E-list for cleanup (D.Casper)

   In /yv0
    YTCONV - Use correct HAC parameter JYNMVS instead of obsolete JYNMSC
    YTRV0S - Use correct HAC parameter JYNMVS instead of obsolete JYNMSC
             (W.Manner 10/02/1997)

 ! 960904 corr.file no.3 to ALEPHLIB 21.5
   In /alef, /comp, /edes, /edir, /gam, /phy, /trig, /vdes
     Add C! description comment to routines where it was missing 
     (affects only documentation)

 ! 960903 corr.file no.2 to ALEPHLIB 21.5
   In /comp, /dedx, /ecal, /edir, /kine, /phot, /ptoj, /tdes, /tpc, 
      /trig, /ytop, /yv0
     Add #ifndef DOC flag to routines where it was missing 
     (affects only documentation)

 ! 960823 corr.file no.1 to ALEPHLIB 21.5
   In /fyxx
    FYTREE - reset the number of kept tracks if some tracks were kept
             without the vertex origin.
    FYFKIN - print FKIN/FVER when FYDEB flag is defined.
           
 ! 960701 ALEPHLIB 21.5
   In /comp
    AUHCYL : Double precision on all platforms (M.Cattaneo)

   In /edir
    CLAS24,LEPTO : protect against possibility of missing banks (M.Cattaneo)
    CLAS24,LEPTO,TRUSLU : Reject tracks with bad TRPFRF ret. code (M.Cattaneo)

   In /fyxx
    FYIRUN : Fix a string concatenation (M.Cattaneo 22 July 1996)

   In /ptoj
    FPTOJ, IPTOJ, PHSTOJ - Replace LENOCC by LNBLNK (M.Cattaneo)

   In /tpc
   TPDVEL - For the option RAW/ONL, check TDFV from Daf before taking TLAS
            from data (P.Comas)

   In /vdes
    VCORMP : modify for cases 17 rphi, 10 Z (J.Rothberg)

   In /vdet
    VTRUH  :  New routine, count the number of properly associated VDET
              hits by layer and view (A.Bonissent).

 ! 960618 ALEPHLIB 21.4

   In /alef
   ALSECS : New routine to convert "Aleph format" date/time to integer
            number of seconds elapsed since 1-JAN-1988  (O.Schneider)

   In /vdet
    VDHTER :  Modified to take the space resolution from MC (Manoj T.)
    VHERR1 :  New routine, compute VDET hit error based on MC (Manoj T.)

   In /edir
   Major changes for LEP 2 EDIRS (R.Edgecock et al)
   Modified routines: ALSUMCL,CLAS24,ESWEH,LEPTO,MUTRGS,PHEDIR,SELBHA,SELEMU,
                      SELEVT,SELTRK,SIEDIR,TIZERO,TRUSLU
   New routines: ENLCL2,GGESUM,GGSEH,GGTRKH,QTRKCH,SELENU,SELGGE,SELWWA,
                 SELWWB,TIZERN,TRKWWS,VDCOSM

   The LEP 1 EDIR classes are unchanged and the code has been written such 
   that the EDIRs for LEP 1 will be the same after a reprocessing, 
   except for the following:

    o class 19 - this now uses QMUIDO instead of Mucalo and accepts many fewer
                 events
    o class 21 - minor cut changes
    o classes 22 and 23 - very small changes as one of the thresholds is now
                          beam energy dependent

    In addition, the following should be noted:

(1) A run at LEP 1 energies during LEP 2 will get a LEP 1 EDIR
(2) As the LEP 1.5 runs already have LEP 1 EDIRs, they will continue to get
    these if they are reprocessed.
(3) The original EDIR code was written by various people and has been
    unsupported for a number of years. As a result, several bugs affecting the
    1995 data have come to light during the development of the LEP 2 code. They
    are:

    (a) for all 1995 running, class 20 always fails 
        (because of the PEWI to PWEI change)
    (b) for all 1995 running, events are rejected by classes 17 and 18 which
        should have been accepted (because of a cut on the Pastis timing, which
        is effected by the bunch trains). This has only a small effect on class
        17, but a large effect on class 18.
    (c) for the 1.5 GeV running, classes 16 and 17 will accept events they
        shouldn't because the wrong cms energy is used (91.1 GeV).

    These will be fixed if/when the data are reprocessed.

(4) There are several classes which are unused.

                                  LEP 2 EDIR
                                 ============

Status key:

   Old     = copied directly from the current LEP 1 EDIR
   Mod     = modified version of LEP 1 EDIR class
   New     = new LEP 2 class

 Class  Status                           Description
 ----- -------- ---------------------------------------------------------------
   1     New    >= 1 ECAL cluster E>1.5GeV + 1 module with Ewire>0.5, |T0|<200
   2     Old    HCAL energy(pads) + ECAL energy(wires) > 15 GeV
   3     New    Cosmics passing through VDET
   4     Old    HCAL energy(pads) > 3 GeV + HCW(4 planes) * ITC trigger
   5     Old    1 to 7 tracks with d0<5cm, z0<20cm, NTPC>=4
   6     Old    >= 8 tracks, cuts as above
   7     Mod    LUM A and LUM B, both E > 30GeV
   8     Mod    LUM A or LUM B, E > 30GeV
   9     New    2-photon: >= 3 trks, Ecalorimeter/ELep<0.5, Echarged/ELep<0.4
  10
  11     New    Low multiplicity WW A: >= 1 track of each sign
  12     New    Low multiplicity WW B: all tracks of same sign
  13
  14
  15     Old    Dileptons
  16     Old    QQbar based on tracks
  17     Old    QQbar based on calorimetry
  18
  19     Mod    Muon candidates (now using QMUIDO)
  20     Old    Bhabha candidates
  21     Mod    Single photon candidates (slightly different cuts)
  22     Mod    Sical A (E>=20 GeV) AND Sical B (E>=20*ELep/91.2 GeV)
  23
  24     Old    Dilepton candidates
  25     Old    Slow control records
  26     Mod    Alignment and calibration (Muon id modified)
  27*    Old    VDET laser events
  28**
  29     Old    Random triggers
  30     Old    Events which fail everything else

 *  On the MINI only, electron selection from QSELEP
 ** On the MINI only, muon selection from QSELEP

 ! 960527 - ALEPHLIB 21.3

   Some packages have been moved from alephlib to alephio (F.Ranjard)
   - some routines from ALEPHLIB/ALEF have been moved to ALEPHIO/IO. 
   - some include files from ALEPHLIB/INC have been moved to ALEPHIO/INC.
   - ALEPHLIB/DBAS has been moved to ALEPHIO/DBAS.
   - ALEPHLIB/ALPH has been moved to ALEPHIO/ALPH.
   - C routines dealing with IO are now stored in ALEPHIO/C.
   - C routines dealing with tracking are stored in ALEPHLIB/C.
           C-compilation is made with -DALEPH_C     
   - The C module and ALC.C, ALC.O are removed.

   - Many multiline strings (i.e. strings stretching over more than one line
     in FORMAT or WRITE statements) have had to be split, since they are not
     accepted by the CPP on VMS.

        In /alef
    ALKJOB : Use kjobjj.h instead of kgjjpar.h  (M.Cattaneo)
    RQBUNC : Set NWAG and INBU for '96. Avoid trying to get the info from the
             unreliable LZZT as the bunch train scheme doesn't change 
             within the year. (P.Comas)

        In /comp
 - UFVDMS fix multiple variable definitions

        In /edes
   replace '#include "implicit.h"' by 'IMPLICIT NONE' (F.Ranjard)
    EPRCOM, ERDDAF : Fix multiline strings (M.Cattaneo)

        In /edir
   ALSUMCL : fix multiline string (M.Cattaneo)
   ULANGL  - Replace call to RLU by call to RNDM
   VUROBO  - Put ULANGL function in separate file, suppress RLU function

        In /fyxx
   FYRELA.H : Remove # as first character of DOC line, breaks CPP (M.Cattaneo)

        In /kine
   KCLEAN, KINIT, KSWAP, KXL7AL, KXL7BR, KXL7ST, KXLUAL, KXLUBR :
     split character string stretching over two lines (M.Cattaneo)

   Interface to JETSET 7.4 and changes for LEP 2 (B.Bloch)
    KIPARA : increase the number of particles/event for LEP2 generators
    KXL7AL : fix format statement
    KXL7PA : fix maximum energy to 205. Gev compatible with LEP2
    KXL74A : interface routine to JETSET 7.4

        In /mdes
    MUPRGE : Fix multiline strings (M.Cattaneo)

        In /satr
    SAPRDI,SAPRHI : Fix multiline strings (M.Cattaneo)

        In /sdes
   new routine STRIGF ( B.Bloch ) to identify trigger type
   new routine STMASK ( B.Bloch ) to build trigger masks

        In /tdes
    TGEPBK : Fix multiline strings (M.Cattaneo)

        In /trig
    X1IRUN : In MonteCarlo, use trigger thresholds which are setup dependent
             (B.Bloch) 

        In /vdes
   Improve correction of time dependent bending of the faces (D.Rousseau)
    VGRDAL: call VALFCO, suppress forced reaccess of VALC,VAGB
    VALFCO: (new deck) Does the correction
    VALCOR: Obsolete, removed.

        In /vdet
    VDMSUP,VTRKEX: fix multiple variable definitions (M.Cattaneo)

        In /vd89
   The old vdetdes package is removed (F.Ranjard)

        In /ytop
    YTOPNW: Replace call to TIMAL by call to TIMEL (M.Cattaneo)
    YVPOSS: Fix multiline strings (M.Cattaneo)

        In /yv0
    YV1INI : Fix multiline strings (M.Cattaneo)
      
   --------------------------------------------------------------------
 ! 960205 - ALEPHLIB 21.2

        In *CD -> MUIDNEWS
         ALPROB Fix a small bug: ALPROB gets the required numbers of
                random numbers according to the run, event number and
                seed. If the routine was called twice or more following
                with the same seed the result was not the same because a
                vector was not defined in the right way. (A.Gregorio)
         MRMHIT Fix a small bug: MRMHIT masks Monte Carlo bank MHIT to
                take into account muon chambers inefficiencies. During
                the 1993 the chambers 4C and 4D were off for a certain
                period and in order to consider this inefficiency inside
                MRMHIT a new bank (D4CD) was filled for 93 Monte Carlo.
                This bank was filled in the wrong position and was not
                present if a muon hit has not been fired. (A.Gregorio)

        In *CD -> PHYNEWS
   FJMMCL - (JADE algorithm) Speed up by doing linear search (O.Callot)

        In *CD -> PTOJNEWS
   FUPKCM - Speed up by computing DSQRT of constants on first call and
            remembering for subsequent calls (O. Callot)

        In *CD -> VDESNEWS
   Correct time dependent bending of the faces of VDET (D.Rousseau)
    VALCOR: (new deck) Does the correction
    VGRDAL: Force reaccess of VALC,VAGB; call VALCOR

        In *CD -> YV0NEWS
   YPSIVE, YV0ONH, YMFMIN, YFPSIN - Speed up by calling NAMIND only on
           first call and remembering for subsequent calls (O. Callot)

   ------------------------------------------------------------------
 ! 951208 - ALEPHLIB 21.1

        In *CD -> ALEFNEWS
   RQBUNC - tagging of level 3 was not working at the beginning of 95
            so set IBUN=1, IQUA=3 for runs in 4x1 configuration and
            leave as before those in 4x2 configuration .

        In *CD -> DEDXNEWS
   add pad dE/dx (D.Casper)
   TBTBLK, TMDEDX, TIDHYP, TDXERR, TXDEDX are obsolete and replaced by
   TBTBLP, TMPDDX, TPDHYP, TPXERR, TXPDDX
   the new routines work for WIRE or PAD dE/dx
   the first argument of the routines is 'PAD' or 'WIRE', the rest
   of the calling sequence stays unchanged apart NS which becomes XNS.

        In *CD -> PTOJNEWS
   PTEXSJ - Also transfer PTPX to TPXS (D.Casper)

   -------------------------------------------------------------------
 ! 951114 - ALEPHLIB 21.0

        In *CD -> ALEFNEWS
   BMACROD - new *CD which contains the necessary declarations for
             BMACRO.
   GETLUM  - get LCAL luminosity for High Energy runs ( LEP II )
   GETS4B  - new routine : Sical lumi per bunch for multibunch runs

        In *CD -> PRINNEWS
   PRRHAH - Modified for LEP II selected Data and printout of Computer

        In *CD -> DEDXNEWS
   TDEDXV -  IF run# >= 40000 THEN
        The sector dE/dx modification for high voltage is only applied
        to the TCSX constants if the voltage is more than 10 volts from
        the nominal value.  If the voltage is at the nominal value
        (1250 volts), the TCSX constants are not modified. (J.Nachtman)

        In *CD -> TPCNEWS
   TPDVEL - allow laser drift velocity from TLAS to be used in
            normal JULIA processing.
            introduce TVOF data card to allow user to offset
            drift velocity. (I.Tomalin)

         In *CD -> VDESNEWS
   many corrections and new features in hadrware error software
   add *CA BMACROD where it is necessary. (J.Rothberg)

         In *CD -> VDETNEWS
  - VTCLLD :  Correct definition of neighbouring wafer for new VDET
  - VTCLAS :  move local work indices setting just before they are used
  - VTRFIT :  Access VDET Setup code using ABRUEV and GTSTUP
              (A.Bonissent)

   ------------------------------------------------------------------
 ! 951010 - ALEPHLIB 20.9

        In *CD -> ALEFNEWS
   ALFMT - IF called for ALL banks : ALFMT (LUFMT,'ALL ',FMT),
           after the 1st call, it will check ALL formats and redefine
           those which are different from those stored on BANKAL.FMT

        In *CD -> DEDXNEWS
 - TDEDXV : IF run# >= 40000 THEN  (140Gev runs and above)
      The global De/Dx normalisation factor is calculated
      using a parametrization given in TCPX bank.
      The sector De/Dx normalisation factor is taken from
      the TCSX bank and maybe modified by the TPHV bank.
      (J.Nachtman)

        In *CD -> VDETNEWS
 - Unmultiplex strips before clustering rather than after;
   handle the relation between "wafer clusters" and
   "global clusters" (at module level); new adress encoding
   scheme.  Affected :
   VTLINK, VRMWF, VTFILL, VTCLLD, VTRFIT
 - Modify VGRDAL so that alignment banks for monte carlo are accessed
   by setup code; for data we keep going with run number
 - VDMSEC
     get multiple scattering constants from setup dependent bank
     VRLD instead of from FKAL bank
 - VRLDGT
     new routine to give access to VRLD bank
 - VDMSUP
     bug fix to a bank index and garbage collection protection
 - VDMSRL
     add more flexibility to allow for layer dependent effects

        In *CD -> COMPNEWS

 - UFMS, UFTKAL, UFVDMS
     get multiple scattering constants from setup dependent bank
     VRLD instead of from FKAL bank
 - UFTTRA
     add a call to vdmsup for consistency

        In *CD -> VDESNEWS
   Add new routines to handle hardware errors.

        In *CD -> YTOPNEWS
  YDIMTO - increase MAXHLX the maximum number of charged tracks
                    MAXNTR the maximum number of neutral tracks
                    MAXTRK = MAXHLX + MAXNTR
                    MKDIMM = (MAXTRK+NMSIZZ-1)/NMSIZZ

   ------------------------------------------------------------------
 ! 950804 - ALEPHLIB 20.8
   remove all *IFn *ELn *EIn to make an AUTONESTed source file
   to be easily translated to CVS on Unix.

        In *CD -> ALEFNEWS
   ADBRJJ, ADBRLIST - add one column for beam position periods.
   ALLEP1 - logical function ALLEP1(irun) is true if energy of
            run # irun is .lt. 100GeV

        In *CD -> EDIRNEWS
   LEPTO  - add a protection against events with > 300 tracks.
            (S.Wasserbach)

        In *CD -> VDETDES
   The changes include the addition of two new subroutines for
   packing and unpacking VDET hit addresses VPKADD, VUNADD.
   The four old routines that did this kind of thing,
           VADDPK, VADDUN, VADESA, VAENSA,
   will henceforth only work for VDET91 because the packing scheme
   will be different for VDET95  (S.Waseerbach)
   VFNEAR - (new deck) find faces near a given phi coordinate
   VPHICF - (new deck) returns phi coordinate of face center
   VPKADD - (new deck) new packing routine
   VUNADD - (new deck) new unpacking routine
   VHLSBT - deleted obsolete stuff about "corruption bit",
           added parameters pertaining to VDET95
   VRDOJJ - added one hac parameter by hand
   VDETGE - added bit shifts and masks (for the things that
           are not identical in VDET91 and VDET95)
   VRDOCO - added one word for number of bits in wafer number
   VDAFRD - initialize new variables and read one more word from VRDO
   VDETGI - fill bit shifts and masks in VDETGE (from 1.8);
           fill in geometry arrays even for empty slots
   VDGDMP - print new variables in VRDOCO and VDETGE
   VRDDAF - print one line when geometry is initialized

        In *CD -> TRIGNEWS
    remove all references to XTBP bank.

        In *CD -> YV0NEWS
   YDISMC - Fix calculation of symmetry point when one center
            lies inside the other circle. This happens for
            like-signed tracks (P.Rensing).
   --------------------------------------------------------------------
 ! 950704 - ALEPHLIB 20.7
        In *CD -> ALEFNEWS
   ALTRIG - get trigger bits from row containing 'TPR ' in 1st word.

        In *CD -> TRIGNEWS
   X1DISN, X1MIXN, X1REDO, X1TRIG - corrections (A.Putzer)

   ------------------------------------------------------------------
 ! 950612 - ALEPHLIB 20.6
        In *CD -> ITCNEWS
   IDDIST, ITDRIF - Protect for v.large times beyond reasonable input
                    range of t-d relation.

        In *CD -> TPCNEWS
   TFICOR - cure some problems encountered in runs 16127-16141
            (I.Tomalin)

        In *CD -> VDESNEWS
   VFACSN - returns the serial slot number of a face.

        In *CD -> ALEFNEWS
   ALFMT  - increase the number of bank formats to 1500.
   BKTOBK - new routine to copy a bank to another bank
            CALL BKTOBK ('ABCD',NR1,'EFGH',NR2)
            'EFGH' format is taken as 'ABCD' format.
   ALK7FIL, ALK7OP, ALK7TRU, ALK7FRU -
            Adapt to new RUNCARTSLIST format

   -----------------------------------------------------------------
 ! 950502 - ALEPHLIB 20.5
   BECAREFUL - this version is using the new VDET geometry package.
               The old one is not called any longer, so access to
               /VDGEOS/, /VDJGEO/ variables is forbidden.
                VDHERR is an OBSOLETE routine using /VDGEOS/, use
                instead VDHTER.
        In *CD -> ALEFNEWS
   GETLEP - add a protection against negative sqrt.
   RQBUNC - new routine to get the LEP bunch # in a bunch train.

        In *CD -> DEDXNEWS
   TIDHYP - rewrite a statment to avoid floating point overflow.

        In *CD -> ECALNEWS
   TIZERO - adapt to new PWEI bank.
   EMTZER - REAL FUNCTION EMTZER (MODU) returns the T0 or -9999.
            for module # MODU

        In *CD -> ITCNEWS
   modifications for bunch-train (J.Sedgbeer)
   IFECON - Remove obsolete Rphi TDC bin-width test
   ICRCOO - Get Bunch-train info. (Call RQBUNC)
   ITDRIF - Use bunch train info to calculate drift-time from TDC value
   IBUNCC - New Comdeck with Bunch train info.

        In *CD -> TPCNEWS
   corrections and new routines to cure the '94 TPC gating problem
   (I.Tomalin).
   TFICOR - Allow several T3FC corrections to be applied in series
            to the same data.
            Extend T3FC bank to allow for corrections to r and z.
            For distortions which can't be well parameterized by
            T3FC alone (shown by JT3FIP column), call TFIMUL to
            multiply T3FC corrections by some additional function
            of phi, TPC current etc.
   TFIMUL, TGTCUR, TIMDIF - new routines

        In *CD -> TRIGNEWS
   implement level 1 trigger in a correct way. ALEPH-note will
   follow (A.Putzer)

        In *CD -> VDESNEWS
   VJWABR - These are used in Julia for
   VJWABW - the simulation of
   VBRJWA - alignment errors for Monte Carlo events
   VDMJLS -  This is a crude version of VDMJLS that works with the new
             geometry package, but only for VDET91!  It is designed to
             reproduce the results of the old version.

        In *CD -> VDETNEWS
   Introduction of the new versions of the VDET alignment routines,
   of the new packing routines.
   Add some routines in the geometry package.
   Remove a bug from VSCMSC.
   VDHTER, VTXNWT, VSTRIP - new versions to cope with new VDET
                            geometry package.
   VTRFIT - remove test on 1993 ==> remove bug for all years.
   VTXTRK -  These are routines which perform the extrapolation
   VDMSUP -  of a track to Vdet wafers and estimate the amount
   VTCLLD -  of scattering material which was encountered. Use
             of old VDGEOS common block was replaced by calls
             to Geom. package routines
   VTRLAS - new routine moved from JULIA :
            VTRLAS(DUM) is TRUE if vdet laser bit is set.
   VDMSRL - read VDRL bank from D.B.
   UFVDMS - get beampipe setup code

        In *CD -> EDIRNEWS
   ALSUMCL - add summary of class 27
   SELEVT  - select class 27 events (POT only)
   CLAS278- select class 27 and/or 28  (MINI only)
   ECAGET, ECALWR, ESUMW, ISPARK, SELBHA, TRIOFF -
   adapt to new PWEI bank.

         In *CD -> PRINNEWS
    PRDTMC - to print MINI bank DTMC/DVMC

   ---------------------------------------------------------------------
 ! 941115 - ALEPHLIB 20.4
        In *CD -> ALEFNEWS
   ALGTDB - make sure that the required bank is there.

        In *CD -> GAMNEWS
   GAMPEX, VOISTO, GIVSTO - add various protections.

        In *CD -> VDESNEWS
   implement new VDET geometry package.
   keep the old one as VD89.

 --------------------------------------------------------------------
 ! 941010 - ALEPHLIB 20.3
        In *CD -> ALEFNEWS
   CAFIHT : remove a bug

        In *CD -> ALPHNEWS
   ABSEVT - correct a bug with SEVT and SRUN cards.

        In *CD -> GAMNEWS
   GAMPEX - Call ECETDI  to correct for gain correction from ECMC
          - New feature added to calculate the probability of
            a Gampex photon to be fake from electromagnetic or
            hadronic origine (GFAKE,GEMFAK,GHAFAK)
   CORAD94- Replace CORADOC for the current version of alephlib
   GAPGAC - Built photon bank PGAC

   ------------------------------------------------------------------
 ! 940802 - ALEPHLIB 20.2
        In *CD -> ALEFNEWS
   ALFIEL : give the correct value of the mag field for runs 25261
            and 25265.
   BKRHAL - get computer name on which the job is run.(P.Comas)
   BKRHAW - change the RHAH bank format.

        In *CD -> VDETNEWS
   VTRFIT - gets DB banks through ALGTDB instead of MDARD.
            (because MDARD was used and the required bank number did
             not exist, default values were used).
            keep the old code (default values) for '93 MC (setup code=5)

   --------------------------------------------------------------------
 ! 940531 - ALEPHLIB 20.1
        In *CD -> ALEFNEWS
   NAMERU : new FUNCTION to get the row# of NAME bank containing
            a given run.
   ALK7COM: increase number of segments from 2 to 3 to handle more
            than 5000 runs (<7500 runs)
        In *CD -> DEDXNEWS
   TBTBLK - set return code to 5 when TBTBLK<0.
   TXDEDX - if TBTBLK<0. then TBTBLK=0.
   TCHKEX - call TDEDXR to get DEDX calibration for IRUN run#
            returns GNR=SNR=0. if run is not calibrated on DAF
   TDEDXV - use only DAF banks TCGX/TCSX or TC2X
   TDEDXR - entry point into TDEDXV to set run number

        In *CD -> EDIRNEWS
   RPECO  - protect ACOS against abs(argument) > 1.(C.Rankin)

   ------------------------------------------------------------------
 ! 940505 - ALEPHLIB 20.0
        In *CD -> ALEFNEWS
   ALFIEL : '92 offset is valid for 92 and 93 only
   ALTRIG : use X1RG
   ALTRHV : new INTEGER FUNCTION to return HV word from X1RG or XTCN

        In *CD -> TPCNEWS
   TCHKHV - get HV word from ALTRHV

        In *CD -> MUIDNEWS
   MRMHIT treat chambers 4c,4d in 93 monte carlo , add bank D4CD to
          show this has been done
   MUIDO  declare format of D4CD
   MUREDO drop old D4CD and add new one to output list
   --------------------------------------------------------------------
 ! 940411 - ALEPHLIB 15.9
        Remove following sets: ALREAD, EPIO, PRESS to make ALEPHLIB
        machine independant.
        the 3 sets are now in ALEPHIO .
   ------------------------------------------------------------------
 ! 940408 - ALEPHLIB 15.8
        In *CD -> ARDNEWS
   ASTAGE, AWRTAP - introduce stageout on VAX machines (U.Schaefer)

        In *CD -> SDESNEWS

   SILTOX, SIXTOL - to go to/from ARS and local sytems .

        In *CD -> VDETNEWS
   VTXNWT - add a protection against asin > 1. (W.Manner)

        In *CD -> ALEFNEWS
   move some routines from ALREAD to ALEF or to ALPHARD to be
   prepared to cut ALEPHLIB into ALEPHIO and ALEPHLIB.
   ALEPHIO will contain the machine dependant code : ALREAD, EPIO, PRESS
   ALEPHLIB will be machine independant

   -----------------------------------------------------------------
 ! 940111 - ALEPHLIB 15.7
        In *CD -> COMPNEWS
   UFTKAL - declare ZZ0 double precision, mandatory on AXP (W.Manner)

        In *CD -> TPCNEWS
   TFICOR - Allow original polynomial parameterization
            to be multiplied by a specified function of the
            coordinates, which could for example constrain
            corrections to be zero at the endplates.

        In *CD -> ALEFNEWS
   TRACBCK : CALL EXIT after the traceback on IBM.
   ALFMT : limit the test on the first 3 characters of NAME to find
           'ALL'
   ALFIEL : the 92 current offset is valid for 92,93,94....
   GTT0GL : T0GL depends on TPC setup code.
   UINDXS : call SORTI instead of SORTX deleted from CERNLIB 94A.

        In *CD -> PRINNEWS
   TPCODP - enlarge a format (I.Tomalin)

        In *CD -> ARDNEWS
   ADSPOS - extend CHNAM to 80 characters
   AOPERD, AOPTAP - on VAX and AXP declare SMG$blabla, LIB$..., SYS$..
                    INTEGER when they are used as function
                    (mandatory on AXP when running in debug mode)
   ACLOSE - when using EPIOc (status word 32 = 2) the EPIO file has to
            be closed before another one is opened to reduce the
            number of opened files (RFIO supports a small number<200
            of opened files)
   AOPTAP - on VAX open the staged NATIVE file in READONLY mode.

        In *CD -> GAMNEWS
   CORADOC - correct a bug
        In *CD -> YTOPNEWS
  YFMVTR, YFTVTR, YFVMAP, YFVMC, YFVMC1 -
           protection against division by zero (G.Lutz).
  YFMVTR, YFVMC, YFVMC1 - correction of two sign errors (G.Lutz).
  YTPAR  - correction of a sign error (G.Lutz).
  YFMVTC - new routine with lagrange multiplier mass constraint in
           vertex fit (G.Taylor).

        In *CD -> YV0NEWS
   YV1INI - replace WRITE(6) with IF (IW(6).GT.0) WRITE(IW(6))
   -----------------------------------------------------------------
 ! 931025 - ALEPHLIB 15.6

   BECAREFUL : SINCE THE CERNLIB 93D HAS BEEN RELEASED IT IS
               POSSIBLE TO USE THE EPIO C VERSION ON UNIX MACHINE.
               SO ALEPHLIB HAS BEEN MODIFIED TO CALL EPIO C.
               MAKE SURE THAT YOUR UNIX SYSTEM HAS GOT THE
               CERNLIB 93D.

        In *CD -> VDESNEWS
   VTXRAW - rewrite the routine to avoid recurrant problem.
            (D.Brown)

        In *CD -> ARDNEWS
   ACLOSE - when using EPIOC (status word 33 = 2) the EPIO file has to
            be rewinded before another one can be opened on the same
            unit.
        In *CD -> FYXXNEWS
   FYKILL : Correction for very rare cases giving a faulty FKIN bank
            (J.Boucrot)

        In *CD -> PRINNEWS
   FRFTDP - adapt code to new banks TWRF     (R.Johnson)

        In *CD -> YTOPNEWS
  YFTVTR , YFVMAP : correct a minor mathemathical bug
  YMKIDT: correct a logical error
  YVPOSS: prohibit overwriting of input track indices

        In *CD -> HDESNEWS
   HFBAHI : correct a bug in the hit finding
   HFBAHIO: new routine which is a copy of HFBAHI before the
            correction of the bug. This routine will be used
            for the '91 and '92 reprocessing to have a coherent
            processing data vs MC.

   -----------------------------------------------------------------
 ! 930910 - ALEPHLIB 15.5
        IN *CD -> HDESNEWS
   HRHTUB : put a protection when running with MC data made with
            JULIA < 271        (A.Venturi)
        In *CD -> ALEFNEWS
   ALEFIL, ALELEP : check that the row # returned by LFILRU is > 0
   GETLEP : check that the row # returned by ALEFIL is > 0
        In *CD -> ARDNEWS
   ACARD2 - rewrite the routine to suppress ASSIGN statments which give
            problems on ALPHA/OSF1 machines.
        In *CD -> COMPNEWS
   TNRHPA - add a protection to avoid division by 0 when a PT=0.
            charged track is processed.     (A.Venturi)
        In *CD -> DEDXNEWS
   TCGXRU, TDEDXV - new routine to handle new banks TCGX/TCSX
                    (W.Wiedenman)
   TCHKEX, TMDEDX - modified to use new banks TCGX/TCSX (W.Wiedenman)
   TBTBLK - choose the parametrization type depending on the presence
            or absence of parametrization coefficients in the TC4X
            bank and not on the data type (MC or real).  (D.Casper)
        In *CD -> TPCNEWS
   TDFVRU - remove a not used variable
            set IPRUN to 0 in data statment
        In *CD -> PRESNEWS
   DEMPBAN - there was a bug in the decompression of characters on VAX
             when it was compressed on UNIX machines.
        In *CD -> COMPNEWS
  UFMS, UFVDMS - protect against unphysical tracks with d0*w > 1
        In *CD -> VDETNEWS
   VDMSUP, VTRFIT - decrease the likelihood of garbage collections
                    to protect against " CALL xxx(IW(...)) "
   VTXRAW - rewrite the routine to avoid recurrant problem.
        In *CD -> YV0NEWS
   YNV0NF - replace the protection of 15.4 with a weaker one
   -----------------------------------------------------------------
 ! 930721 - ALEPHLIB 15.4
        In *CD -> ALPHNEWS
    remove ENTRY ABWSEL, introduce SUBROUTINE ABWSEL to please IBM/VM

        In *CD -> ARDNEWS
   REMEMBER for the moment the use of EPIO/CFIO is yanked until the release
            of the cernlib 93d.
   AOPERD - on ALWS as on UNIX when reading an IBM FILM card for a standard
            EPIO file (a GIME is present) transform it for a standard
            cartdridge:
            i.e. - FILM 'AB1234.EPIO | GIME PUBXU 456'
                   becomes
                   FILM 'ALDATA | EPIO | CART AB1234.1.SL'

   AOPTAP  : remove restriction to 'CART' or 'TAPE' on VAX to allow
             stagein of Robot cartridges through SHIFT.

        In *CD -> EDIRNEWS

   EEWIRS - Bhabha events selected from two ecal modules with uncalibrated
            wire energies above 30 GeV.

        In *CD -> IDESNEWS

   IDTCCC - Comments modified.
   IDRPJJ - New comdeck:
            IDRP HAC params.
   IDRPCC - New comdeck:
            New Drift-time relation parameters.
   IINRES - IDTC   fill drift-time parametrisation (old) into /IDTCCC/
                   and /IDRPCC/. IDTC bank used for 1989-1992 data.
            IDRP   fill drift-time parametrisation (new) into /IDTCCC/
                   and /IDRPCC/. IDRP bank used for 1993 onwards.
   IPRDAF - Add print for IDRP values.

        In *CD -> ITCNEWS

   ICDRIF, ITDRIF - Use function IDDIST for drift-distance calculation
   IDDIST - new function:
            Calculate drift-distance from params. in /IDRPCC/ and
            drift-time.

        In *CD -> GAMNEWS
   GXMCLM - add a limit on the cluster energy to be treated
        In *CD -> PHOTNEWS
   CLMONS - add a limit on the cluster energy to be treated

        In *CD -> YV0NEWS
   YNV0NF - add  a protection against huge values in XMT matrix or
            VT vector which make DEQINV CERNLIB routine crash
   -----------------------------------------------------------------
 ! 930617 - ALEPHLIB 15.3
        In *CD -> GAMNEWS
   various corrections in CORADOC, GAMPEX, GFRAC, GXIMPA
   for the overlap region

        In *CD -> YTOPNEWS
  YTPVTX : add a protection

        In *CD -> ARDNEWS
   ALGTENV : add BEAMPOSITION new environment variable to access
             phy:beam.position file.
   AOPEN,AOPENW,AOPTAP,AWRTAP : set EPIO status word 33 to 2 to use CFIO
                                and RFIO package where it is avalaible,
   AOPTAP  : add access to cartridges on ALWS through SHIFT6
   AOPERD  : on ALWS open staged file on SHIFT6

   -----------------------------------------------------------------
 ! 930513 - ALEPHLIB 15.2
        In *CD -> ALEFNEWS
   ALGTRO : bug correction
   ALTRIG : look for LUPA or SILH bank to get trigger bits when no other
            bank is present.
   ALVSN  : add a protection in case of slow control record (IRUN=0)
   GETLUM : modified to get the lumi / nb of bhabhas from Sical in 1993
   GETSLU : modified to get the lumi from bank 'SLUM' if necessary
   ALFMT  : increase size of bank formats to 75 characters to fit LPZT
            bank format.

        In *CD -> ARDNEWS
   AOPTAP, AWRTAP, AOPERD - let TMS find out where a cartridge resides.
                            on IBM, if SIZE is not given on FILI card
                                    then SIZE 200 is assume.
                            on IBM, if no option is given CHOPT=' '

        In *CD -> PHOTNEWS
   GAPGID, GAPGPC  - corrections
   GVERCR - initialize some variables.
   ENOF4, ENOL12, ENOW12, GAPGID, ECLEAK - get necessary data base banks
   using *CA GTDBBK or MDARD for banks with NR=0

        In *CD -> VDETNEWS
   VTXTRK - check Z cylinder extrapolation
        In *CD -> YV0NEWS
   set keywords on CKEY cards

        In *CD -> YTOPNEWS
1) Changes in YTRV0S to reject V0,s with hits before the vertex
2) New routines YBIPAT YPRVER YSTVTX YCIRCR for this purpose
3) YDEFRF changed to cope with new ALPHA
4) Minor changes in YTSTRK to allow running on MINI
5) 3 words added in bank YNTR
6) New routine YFVMC1. It allows the enforcement of one
   or several submass constraints (e.g. psi mass in the
   decay Bs->psi K+ K-) in the vertex fit.
7) Minor changes in routines YTCON, YTIJOB, YTPVTX, YTSTRK, YVDCOF

   -----------------------------------------------------------------
 ! 930427 - ALEPHLIB 15.1
        In *CD -> GAMNEWS
   CORADO is purged
   CORADOC - new function CORADOC (EN,TH,PHI)
   GAMPEX  - get run number from ABRUEV to distinguish between
             Real and MC data
             call CORADOC with new argument list
   GAPGID  - bug correction
   MORSTO  - suppress reference to BMACRO
   reintroduce old version for backward compatibility:
   GAMPEK, GATRAL, ISPARK, MECRFC behind *DK VOISTO.
   These decks should be deleted when GAMPEK is no longer used.
   the variable E4ETB in routine DEFOVE keeps the new value 0.851
   instead of the old one 0.85

        In *CD -> SDESNEWS
   SIRDAF : use ALGTDB instead of MDARD to get banks with setup code
            to avoid to duplicate banks with different NR but identical
            contents.

        In *CD -> VDETNEWS
   VRDDAF : get VDET geometry banks through ALGTDB instead
            of MDARD to avoid to repeat banks with different NR
            but identical contents.

        In *CD -> EDIRNEWS
   CLAS24  - remove acolinearity computation

   -----------------------------------------------------------------
   ------------------------------------------------------------------
 ! 930405 - ALEPHLIB 15.0
   ALEPHLIB 14.6 has been resequenced. The CRAY code has been removed.
   The YTOP package has been indented and dead code suppressed.
   Obsolete decks ABALFA and ALOPEN have been purged.
   ------------------------------------------------------------------
 ! 930402 - ALEPHLIB 14.6
        In *CD -> ALEFNEWS
   TRACBCK - add a CALL EXIT after CALL ERRTRA in IBM code
   GETSLU  - new routine to get SICAL Lumi and nb of Bhabhas for
             a given run
        In *CD -> ARDNEWS
   ALGTENV - on VAX check that EDIR or EPIO files starting with A or I
             have only 6 characters before prefixing them with
             AL$EDIR or AL$DATA
             on UNIX call GETENVF instead of GETENV to please APOLLO
   ABOPEN, ALOPEN, AOPEN, AOPENW, AOPERD, AOPEWR, AOPTAP, AWRTAP
           - reoganize error codes
        In *CD -> DEDXNEWS
   TIDHYP - remove obsolete code for DTRA bank (Mini)
   TMDSTX - obsolete routine removed (Mini)
        In *CD -> EDIRNEWS

   move ALSUMCL and ALCLASW from ALEF set to EDIR set
   ALSUMCL - add summary of class 24 and 26
   SELEVT  - add calls to CLAS24 and CLAS24
   CLAS24  - new routine to select class 24 events (tau group)
   CLAS26  - new routine to select class 26 events (alignment group)
   CHKCLAS - new routine to check the class word against the mask
             set with the CLAS data card
   ------------------------------------------------------------------

     Class #24: Defined in CLAS24 routine.
     ----------
     Content  : Dilepton candidates (e+ e-, mu+ mu- and tau+ tau-)
                Enlarged class 15 selection.

     Class #26: Defined in CLAS26 routine.
     ----------
     Content  : Events to be used for alignement and
                calibration purposes.
                Muon events selected by mean of trigger bits pattern.
                Bhabha events selected from two ecal modules with wire
                energies above 30 GeV.

        In *CD -> GAMNEWS
    new version backward incompatible. The old version has been purged.
    main routine is called GAMPEX
        In *CD -> PHOTNEWS
   Steering and package added to caracterise photon/pi0

   GAPGID   Steering to built PGID bank using GBNEUT and CLMOMS
   GBNEUT   Modified from EBNEUT to caracterise the transverse
            shape of any neutral cluster . Argument are identical
            to EBNEUT
   GBTRAN   Modified from EBTRAN to caracterise any neutral cluster.
            Argument are identical to EBTRAN
   GBIMPA   Modified from EBIMPA to properly treat the pad in theta and
            impact. Argument are identical to EBIMPA
   GVERCR   Recalculate from individual storeys the cluster barycenter.
 >>>>>> In *CD -> PRESNEWS
   DMPCHR - introduce a local common block /LOCALP/ to please
            ALPHA machines.
        In *CD -> PTOJNEWS
   JPLIST : use LNBLNK instead of LENOCC.
   Add JLIST(1:LNBLNK(JLIST)) or PLIST (1:LNBLNK(PLIST)) to avoid bank
   with blanc name.
        In *CD -> SDESNEWS
   the  ADBSCONS 179 is mandatory
   new version of the SiCAL geometry package, look at SDESNEWS
   for more details .
        In *CD -> TRIGNEWS
   SIX2MK - add a LOGICAL BTEST (mandatory on apollo)
        In *CD -> YTOPNEWS
   YTOIJO : this obsolete routine is purged.
        In *CD -> YV0NEWS
   YDISMC - add a protection against  negative sqrt
   -----------------------------------------------------------------
 ! 930219 - ALEPHLIB 14.5
   mods in : ALFMT in ALEF set
             print on IW(6) if not 0
             ALREAD set
             ACDARG, ACLOSE, ADSPOS, AOPEN, AOPENW, AOPERD, AOPTAP,
             ASTAGE, AWRTAP
             implement stagein/stageout for UNIX machines
             implement fetch/gime/dispose for UNIX machines
             scripts reside on /aleph/bin/
             - FILI 'ALDATA | EPIO | CART AB1234.1.SL'
               alstagein creates a file /aleph/data/ab1234.epio
               for the 1st file of a cartdridge and a file
               /aleph/data/ab1234_n.epio for file no. n
             - FILO 'ALDATA | EPIO | SMCF I12345.n.SL
               alstageout writes on cartdridge a file
               /aleph/data/i12345.n.sl
             - READ 'MY.FILI | CARD | FETCH FLR 192'
               sfetch will fetch from cernvm the file MY FILI FLR.192
             - FILI 'vxalpha.cards | CARD | GIME ALWS RANJARD
               use vxalpha.cards if there
               if not there fetch vxalpha.cards from ALWS id RANJARD
               id RANJARD needs to be given when it is different from
               unix id.
               valid GIME or FETCH are:
               FETCH ALWS::AL1$USER3:[SMITH.ALPHA] RANJARD
               GIME dxal00
               GIME dxal00:~closier
               FETCH dxal10:/home/dxal00/smith/alpha/
             - FILO 'MY.EPIO | EPIO | DISPOSE'
               when use in a batch job sent from a remotehost different
               from the host , send MY.EPIO to the remoteuser reader
               if remotehost is cernvm or to the user main directory if
               remotehost is not cernvm.
             TWRRED in DEDX set
             fix a bug in production of TSDI bank
             FMTRAK in FMTRK set
             remove invalid characters
             YFMVTR, YFVMC in YTOP set
             remove invalid characters
             TRIGGER set
             add SiCAL trigger
             YV0 set
             get cuts from data base
             for more details look at *CD YV0NEWS
   new
   routine : ALVSN in ALEF set
             analizes RHAH bank to return
             ITYP     data type
             IPV(10)  program version #
             IAV(10)  alephlib version #
             IYR      year when data taken
   -------------------------------------------------------------------
 ! 930111 - ALEPHLIB 14.4
   mods in : ALEF, ALPHARD, ALREAD sets
             introduce a UNIX flag valid for all UNIX machines :
             the extraction of the fortran must be done with 2 flags
             *DF UNIX,DECS or UNIX,SGI or UNIX,RS6K or UNIX,APOLLO
             or UNIX,HP
             VAX, IBM, CRAY stay unchanged.
             AOPTAP in ALREAD set
             On UNIX machines introduce stagein through FILI cards
             cartridges are staged on $ALSTAGE/xxnnnn.epio or
             $ALSTAGE/xxnnn.native.
             AOPERD in ALREAD set
             allow to read IBM FILM cards from UNIX machines.
             edir files are stored on $ALEDIR/xxnnnn.edir
             data files are stored on $ALDATA/xxnnnn.epio
             ALGTENV in ALREAD set
             on CRAY returns dbas.bank for DBASBANK
             AOPEN in ALREAD set
             on CRAY store IBM data files in /pool/aleph
             AWRTAP in ALREAD set
             add options after the filename and not before
             ALFIEL in ALEF set
             correction to 92 current
             ALSUMCL in ALEF set
             define BTEST as LOGICAL to please APOLLO compiler
             ALRWEV in EDIR set
             add checks on the return code
             add a call to ACLOSE(0,ier) at end of job to dispose and
             stage out files if any.
             HRHTUB in MUONID set
             correction
             TXDEDX in DEDX set
             more accurate DEDX
             VDMSUP in VDET set
             correction in bank creation
             UFVDMS in COMPUTE set
             correction added
             VDHITS in VDET set
             save name-indices
             YTOP
             see YTOPNEWS for more details
 new
 routine  :  VDAMB in VDETDES set
 --------------------------------------------------------------------
 ! 921030 - ALEPHLIB 14.3
   mods in : ALEF, ALPHARD, ALREAD sets
             add Silicon Graphic flag SGI=DECS
             PRESS sets
             add Silicon Graphic flag SGI=HP
             AOPENW in ALREAD set
             open NATIVE files in direct access (UNIX flag)
             ACLOSE in ALREAD set
             add IOSTAT in a CLOSE statment
             ALGTENV in ALREAD set
             compiler error on RS6K
             AOPERD in ALREAD set
             transform AL$DATA: to /aleph/data/ on RS6K as on DECS
             ABOPEN, ABRSEL, ALOPEN in ALPHARD, ALREAD sets
             add INTEGER ACARD1 to please ALPHA machine
             PRESS set
             remove previous RS6K flag , make RS6K = HP flag
             TRHFLG in TPCDES set
             old bug
             TIDHYP in TPC set
             typing error mentionned in ALEPH142 but not corrected
   ------------------------------------------------------------------
 ! 921021 - ALEPHLIB 14.2
   mods in : VDET set
             corrections in UFTMSO, UFVDMS, VDMSUP, VTRFIT, UFMS
             TIDHYP in DEDX set
             typing error
             MRMHIT in MUID set
             get MC  efficiency from DataBase.
             ALGTENV in ALEF set
             introduce DBASBANK in the list of environment variables
             PRESS set
             introduce HP flag
             YTOP set
             suppress some print statments
   new
   routines: ALMACH (name) in ALEF set
             returns the Historian flag name used to build
             the library  as CHARACTER*8
             TIMEL in ALEF set
             C routine to replace Cernlib routine which does
             not work on DECS, and may be some other UNIX machines
   -----------------------------------------------------------------
 ! 921006 - ALEPHLIB 14.1
   mods in  : YTOPOL in YTOP set
              call YTOPNW
              YFMVTR
              wrong format
              ALGTENV in ALEF set
              on VAX and IBM
              when trying to open a card file with the
              name : ALPHACARDS, GALEPHCARDS, JULIACARDS,
              the following name is assumed
              XXXXXXX.CARDS
              to avoid confusion with existing files.
              FYFKIN, FYINEV in FYXXX set
              calls to BWIND were wrong
              DMPBAN in PRESS set
              mistyping error in RS6K flag
              VDET set
              new treatement of X-scattering : see VDETNEWS
              for more details
              MUID set
              corrections
              PHOTONS set
              purge EGNERI
              EBCLCF calls GTDBBK to get EPCC bank
   -----------------------------------------------------------------
 ! 920825 - ALEPHLIB 14.0

  +++++++++++++++ BECAREFUL ++++++++++++++++++++++++++++++++++++
            REMEMBER FILI/FILO/FILM cards MUST BE of the following
            type:
            FILI 'AB0314.EDIR | GIME PUBXU 209'
      or    FILI 'AB0314 EDIR | GIME PUBXU 209'
            FILM 'ALDATA | EPIO | CART AB0314.1.SL SIZE 200'
      or    FILM 'RUN0012345.DAT | NATIVE | SMCF I12345.1.SL SIZE 200'
            FILO 'MY.EPIO | GIME FLR 192 * MYPASS (MR'
      or    FILO 'MY EPIO | GIME FLR 192 * MYPASS (MR'

            this version requires EPIO 1.68 from CERNLIB 92b.
            on VAX the EPIO 1.68 is NOT part of CERNLIB 92b .So the
            necessary EPIO routines have been put into BOS77 no. 1889.

            Make sure that you have the CERNLIB 92b on your machine,
            then install the new BOS77 no. 1889 (ALEPHLIB 13.9 can
            run with BOS77 no. 1889),
            then install ALEPHLIB 14.0.

            An Historian flag : UNIX has been introduced mainly for
            testing purpose. Please don't use it yet. Use instead the
            machine flag: IBM, CRAY, VAX, APOLLO, HP, RS6K
  ++++++++++++++++++ BECAREFUL +++++++++++++++++++++++++++++++++++++++

  mods in : ALEF, ALPHARD, ALREAD sets
            introduce the following Historian flags:
            BIT64, UNIX, RS6K
            remove setting of number of bits in a machine word,
            it is done in ALBITW depending on the BIT64 flag.
            the READONLY parameter has been added in AOPEN on DECS,
            APOLLO, HP, UNIX.
            EPIO files are always opened by AOPEN and not EPIO.
            Data files are always opened in direct access on DECS.
            EPIO files are always opened in direct access on RS6K (=UNIX)
            ALDAWR, ALFIND, AUBLIS, AUBPRS in ALEF
            replace calls to NLIST by calls to NLISTB to avoid confusion
            with MULTINET or TCP/IP.
            ALK7TRU in ALEF set
            remove a bug
            GETLUM in ALEF
            when getting luminosity from LUMI bank take method # 5
            when no SiCAL and method # 9 when SiCAL.
            DEDX set
            TMDEDX, TCHKEX, TIDHYP, TMDSTX, TBTBLK, TDXERR
            Get TC1X, TC2X, TC3X, TC4X from data base depending on
            the setup code for MC data and on run number for real data.
            ALSUMCL, SELEVT in EDIR set
            add SICAL flags : class 22, 23.
            MUREDO in MUONID set
            remove use of S-list to store TREX bank.
            drop TREX at next entry into MUREDO.
            PRESS set
            CTOLOW, CMPHOST, CMPBAN, DMPBAN, DMPCHR
            introduce RS6K flag.
            PRINT set
            FRFTDP
            print pulse lengths
            call GTT0GL to get T0 depending on run number and
            number of bunches.
            TPC set
            TCRZVD
            call GTT0GL to get T0 depending on run number and
            number of bunches.
            TWRRED, TWPOLD
            create and output TSLE bank  (MC data)
            TUN1NC
            call TCORES to correct coordinates for field distorsions due
            to short circuits in the field cage.
            TWPANA
            update comments
            YTOP set
            YNMAJJ is extended
            look at YTOPNEWS for details.
  purge
  decks   : YADHVX,YBLDVX,YFTHVX,YFUMSC,YTOCON,YTOPAN,YTOPID,YTOPVT
            YPRIMV,YTORVO,YTOSTR,YXVPOS
  new
  comdecks: CRAYTYP  define CRAY type
            GTCHDECL ALREAD variable declarations
            GTCHUNIT get logical unit in characters
            GTFTYP   get file type and record length, close previous file
            GTVSN    get device type, vsn# and options
            PRENVIR  print aleph and full filename
            PRFILM   print FILM card information
            PRFNAME  print fname, ftype, fdevi
            PRMSG    print message
            STBFORMA set BOS file format
            ALBITW   set # of bits in a machine word
            GTDBBK   get DB banks depending on setup code for MC data
                     or on run number for real data.
  new
  routines: EXIT      IBMRS600  EXIT routine (RS6K Historian flag)
            KBYTREC   returns ALEPH record length in bytes
            PRSTAT    prints status message in case of error in OPEN
            ALGTENV   returns full filename from ALEPH filename
                      ADBSCONS, ADBSTEST, ADBS8990
                      BANKALFMT, RUNCARTLIST
                      ALPHACARDS, JULIACARDS, GALEPHCARDS, LOOKCARDS
                      ABxxxx.EPIO, AMxxxx.EPIO
                      AMxxxx.EDIR, AMxxxx.EDIR
                      Ixxxxx.EPIO
            GTT0GL    get T0 depending on run # and number of bunches
            TCORES    corrects TPC cord. for field distorsions
            YFVMAP    faster fit
            SIEDIR    compute class 22,23 (SICAL flags)
                 next 3 routines are under DECS historian flag
            SDRSX     converts DEC RISC floating format to DEC VAX
            SIBSDR    converts IBM to DEC RISC floating format
            SXSDR     converts from DEC VAX to DEC RISC floating format
            EPINIT    special VAX version to make sure that EPBLIN and
                      EPHEAD are loaded from BOS77 no.1889
 -------------------------------------------------------------------
 ! 920813 - ALEPHLIB 13.9
 mods in : ALEF, ALPHARD, ALREAD sets
           remove  unecessary SAVE, add specific SAVE statments
           IANDSH in ALEF set
           remove HP flag
           ACDARG, ACARD1 in ALREAD set
           make a common version for all machines
           UFTKAL in COMPUTE set
           precision problem
           HCCONS, HRDDAF in HCALDES set
           corrections to get the real tube length
 new
 function: HTULEN (ITUB,IMOD,ILAY,IPOR,IPOS) in HCALDES set
           get the real tube length depending on the hit position.
 new set : MUONID set
           old QMUIDO package. More details in *CD MUIDNEWS

 --------------------------------------------------------------------
 ! 920618 - ALEPHLIB 13.8
 mods in : ALK7FIL in ALEF set
           correction of bug
           ALGTDB, USGTDB in ALEF set
           suppress reference to NLIST and AGTLTC
           ALSUMCL in ALEF set
           add an entry point ALSUMGT (IARRAY,NDIM) which returns
           in IARRAY (1-NCLAS)  statistic of class words
                     (NCLAS+1)  no. of events without class word
                     (NCLAS+2)  no. of events with class word
                     (NCLAS+3)  no. of run records
                     the 1st NDIM words are filled
           AOPDBS, AOPENW in ALREAD set
           suppress reference to ALGTLTC and BKOUTF
           AFILMI , AOPERD in ALREAD set
           suppress CRAY dependant code in AFILMI
           modify CRAY dependant code in AOPERD to be able to read
           FILM card of the type CART I12345.1.SL or I12345 FS 1
           ASTAGE, AWRTAP in ALREAD set
           set NSTAGE to 0 in ASTAGE at 1st entry and not in AWRTAP
           to be sure that it is set even when there is no cartridge
           to be stage out.
           ABRSEL in ALPHARD set
           when ABRSEL is called after a IRET=6 (no more input file)
           it means that the user wants to loop over the same set of
           FILI cards.
           TSRTCO in PTOJ set
           correct a bug in FICL and FTCL
           PCPATQ in PTOJ set
           correct a bug
           KEJOB, KINIT in KINE set
           suppress calls to bookkeeping package
           KFEVBK in KINE set
           change the code to avoid machine precision problem
           KNODEC in KINE set
           handle KTMX card to force particle decays at
           generator level below a given life time
           PAIRFD in COMPUTE set
           when FRF2 data card is present use FRFT,NR=2 if it exists
           otherwise use FRFT,NR=0.
           UMSERR in COMPUTE set
           call ABRUEV to get run number, call GTSTUP every new run
           to get VDET setup code
   new
   decks : SIITOR, SIDALI in SCALDES set
   move  : BKCARI, BKINCA, BKRHAB, BKRHAL, BKRHAW from BOOK to ALEF
   purge : BOOK set
           MDARD,AGTLTC,ALSEQR,AREAD,ASEQR,RACCEP,ASEVT
   additional corrections after the release:
           FYXXX set
           increase buffer size MAXMCX in /FYRELA/ to allow 1000
           tracks or vertices.

   ------------------------------------------------------------------
 ! 920413 - ALEPHLIB 13.7
 mods in : YTOP set
           the NEW package becomes the default: look at YTOPNEWS
           for more details
           ENOLWF in PHOTONS set
           add some protections
           HSECO in HCALDES set
           compute correctly the position of the 2nd spacer if any
           ALRWEV in EDIR set
           suppress wrong EXTERNAL statment
           TWRRED in TPC set
           new wire reduction : new TSIR and TSDI banks are created
           instead of TRIR and TRDI.
           DMPLIS in COMPRESS set
           new data card : DLIS nam1 nam2 nam3 ... nami...
           gives the list of banks to be decompressed.
           if no DLIS card all banks are decompressed.
           FPTOJ in PTOJ set
           new data card : NOEM
           does not unpack error matrix
           if no NOEM card unpack error matrix.
           PCPATQ in PTOJ
           increase number of possible gammas in one CALOBJECT
           TPDVEL in TPC
           if montecarlo and TSIM bank is missing try to get TDVV,NR=1
           SCALDES set
           some fix in routines used by JULIA
 new
 routines: VDHITS in VDET set
           call VDHITS (IARRAY,MXARRAY)
           MXARRAY is the length of IARRAY
           MXARRAY is the greatest track number
           IARRAY  is numbered by FRFT track number,
                   only the first 8 bits are used, as described below
           bits:     meaning
           0-1       number of inner layer U hits on track (0,1,or2)
           2-3       number of outer layer U hits on track     "
           4-5       number of inner layer W hits on track     "
           6-7       number of outer layer W hits on track     "
 -------------------------------------------------------------------
 ! 920225 - ALEPHLIB 13.6
 mods in : ITC, ITCDES sets
           IRDDAF   - Remove call to obsolete routine IFEOLD
                      Call ALTELL if missing dbase banks.
           IPRDAF   - Remove obsolete print out.
                      Add print for IZNL values.
           IPTOJ    - Remove obsolete code for old MC data
           PITCOJ   - Remove obsolete code for IFCO
           ICRCOO   - Remove obsolete code for IFCO
           IFECON   - Protection for dbase const. values from
                      IRFE bank. Remove obsolete code for old MC.
                      Call GTSTUP to get run period for MC data
                      Get bank IZNL from dbase.
           IGEOMW   - call GTSTUP to get run period for MC data
           IINALI   - call GTSTUP to get run period for MC data
           IINRES   - call GTSTUP to get run period for MC data
           ITSWCO   - Use new S-bend parametrisation from IZNL.
           IRESRP   - Set default r-phi resolution to be 1990
                      values in case of missing IRRF bank.
           UFTTRK
           add chi2 cut on final fit
           VTLINK
           change definition of large pulses for increased errors
           ALEF,ALREAD,ALPHARD,BOOK,DBASE sets
           replace all occurences of LENOCC (obsolete) by LNBLNK.
           create an Historian flag HP identical to APOLLO.
           AOPTAP in ALREAD
           restore FNAME as it was in ALEPH134
           GAMPEK in GAMPACK
           mods to get list of storeys associated to a photon.
           LEPTO in EDIR
           remove ununsed variables
           AGTLTC in ALEF
           add protection against empty string
           ABRSEL in ALPHARD
           suppress call to ABWEND which must be called by the user.
           ABRREC in ALPHARD
           suppress entry point ABRUEV, replace it by a subroutine
           UFTKAL in COMPUTE
           small correction
           YVXBLD, YTRV0S,YFMVTR in YTOP
           some corrections
 New
 Routines: HFBAHIT, HFENHIT in HCALDES
           extensions of existing routines HFBHIT and  HFEHIT
           EGNERI, ENOF4, ENOL12, ENOLWF,ENOW12 in PHOTONS
           calculate normalized estimator for photon. Look at
           *CD PHOTNEWS for more details.
           TDFVRU, TPDVEL in TPC
           get the right drift velocity for the current input file
           TPDVEL should be called at beginning of a run
           - in AUNPCK for POT or DST : CALL TPDVEL('POT',DVA,DVB,IER)
           - in JULIA for ONL (PASS0) : CALL TPDVEL('ONL',DVA,DVB,IER)
           - in JULIA for RAW         : CALL TPDVEL('RAW',DVA,DVB,IER)
            DVA and DVB must be dimensionned to 3
            TCRZVD in TPC
            to correct Z coordinates for new drift velocity and t0.
            STOGAM in GAMPACK
            to load the list of storeys asociated to photon# IGAM
            of last call to GAMPEK.
            ABRUEV (IRUN,IEVT) in ALPHARD
            to return the current run and event number read by ABRSEL
            new set of routines SCALDES for the SiCAL geometry
 New
 Comdecks: IZNLJJ, IZNLCC, EF4NJJ, EL1NJJ, EL2NJJ, EW1NJJ, EW2NJJ
           JCONJJ, TDFVJJ, TDPVJJ, TLASJJ, TVXYJJ, GASTIN, SICOJJ
           SALIJJ, SIGECO, SIMACR, SIMASK

 Purge   : IFCOJJ, ITDCJJ, IZSCJJ, ITTDIC, ITZDIC, IFEOLD, IFIXDI

 --------------------------------------------------------------------
 ! 920122 - ALEPHLIB 13.5
 mods in : everywhere
           add a SAVE statment at the right place .
           ALREAD
           few modifications for DECS.
           AOPEN in ALREAD
           add ACTION='READ' in IBM code to please compiler 2.5.0
           AOPERD in ALREAD
           data file read in direct access mode are opened on a
           different logical unit than those read in sequential
           mode  to avoid problem on VAX when reading files in
           different modes.
           BOOK
           few mods to include NANOdst in the bookkeeping system
           FPRROW in PRINT
           small bug
           YTOP
           update of the YNEW package which should become the default.
           several routines are obsolete. some of them can be replaced
           by the one given in () :
           YADHVX (YFTVTR), YBLDVX (YVXBLD), YFTHVX (YFTVTR), YFUMSC,
           YPRIMV, YTOCON, YTOPAN, YTOPID, YTOPVT, YTORV0, YTOSTR,
           YVXPOS (YVPOSS).
 new
 routines: HCPRIND (ICODE) in PRINT
           to print nicely HLWD (ICODE=1)
                           HWDI (ICODE=2)
                           both (ICODE=3)
           INTEGER FUNCTION ALK7OP (LUK7) in ALEF
           open and read RUNCARTS LIST file on unit# LUK7
           INTEGER FUNCTION ALK7FRU (LUK7,IRUN,TYPE,TAPE) in ALEF
           return the tape number for a given run and data type
           INTEGER FUNCTION ALK7TRU (LUK7,TYPE,TAPE) in ALEF
           return the list of runs of a given type residing on a
           given tape.
           INTEGER FUNCTION ALK7FIL (TAPE) in ALEF
           return a FILI card to open TAPE .
           GETXYB(IRUN,IFOUN,IFL,XYZ,DXYZ,OFS,VLUM)
           Gets the beam position per run from bank 'RXYZ'

 --------------------------------------------------------------------
 ! 911206 - ALEPHLIB 13.4
 mods in : TRACBCK called in BABEND
           correct the trace back on VAX
           ADBRLIST used in ADBR
           replace SA with PM (for passive material:SAMBA)
           ALGTDB in ALEF
           drop all existing banks (not only the 1st one) with
           a no longer valid number.
           GTDBAS in ALEF
           mistyping error
           PRKINE in PRINT
           suppress double *if .not.doc
           TUN1NC in PTOJ
           call TFICOR to get corrections for mag.field distorsion
           UFVDMS in COMPUTE
           protection against TANL=tan(lambda)=0.
           EBCLCF in PHOTONS
           get the right EPCC bank
 new
 routines: TFICOR and TFIPOL in PTOJ
           to correct phi-cordinates for mag.field distorsion

   ------------------------------------------------------------------
 ! 911106 - ALEPHLIB 13.3
 mods in     : GETEVT in ALEF set
               if run record is read then
                  if event # .ne.0 is required in this run then
                     continue reading
                  else   return the run record
                 elseif slow control record is read then
                     if no event or no run is required then
                        return the slow control record
                     else  continue reading till the next event
                  endif
                  ABRSEL in ALPHARD set
                  resset BATC flag everytime ABMODE is called.
                  ABSEVT in ALPHARD set
                  set initial values to -1 instead of 0 to avoid
                  confusion getting slow control (NEVT=NRUN=0) from
                  EDIR files.
                  ABRREC in ALPHARD set
                  call new routine AREDIR
                  ITC set
                  allow a variable resolution across the ITC cell.
                  IINRES  - get IRRF bank from database
                  IMICCO  and IUPCCO -
                  use function IRESRP to set the r-phi resolution in
                  ICCO bank
                  YTOP set
                  several mods in YNEW
                  EXPFRF in ECALDES set
                  protection of an ACOS argument
                  EBLEAK in PHOTONS set
                  protection against negative correction.
                  VTCLLD in VDET set
                  ptotection against wrong wafer number

 new routines   : FJMMCL,JPCOMB and PJLINK in PHYSICS set
                  should replace QJMMCL ect... in ALPHA
                  and similar routines in JULIA

                  IRESRP  in ITC set
                  new function to give ITC r-phi resolution.

                  AREDIR (IEVT,IRUN,NREC,IRET) in ALPHARD set
                  read an EDIR file in BATCH or INTE mode
                  return next record required following SEVT,SRUN,
                  CLAS or IRUN cards. IEVT, IRUN, NREC are the event#,
                  run#, pointer# of the record to be read.

                  new set GAMPACK contains the GAMPEC algorithm
                  the main subroutine of the package has bee changed
                  to GAMPEK because an argument has been added to
                  give the total length of the GAMVE array.
                  Look at *CD GAMNEWS for more details.
   ------------------------------------------------------------------
 ! 911023 - ALEPHLIB 13.2
 mods in     : ITCDES set
               IDESNEWS - update comments on hierarchy of banks which
               may be found on the run-header or database.
               IGEOMW   - Get IWST bank according to hierarchy
               cards/run-header/dbase.
               IGETDB   - add comment.
               ITQUAL   - Fill IQXT bank with wire info. even if no
               coord. on wire.
             (This routine is not used in JULIA. The mod. was requested
              by those who do coord. and resolution studies in ALPHA-type
              jobs).
                VDET set
               some improvments in VDET fit.
                 ECLEAK in PHOTONS set
                apply energy loss corrections to charged clusters
                 YTIJOB in YTOP set
                set the primary vertex flag to TRUE when there is
                no YOPT data card.
   new routine: TZSCVD in TPC set
                correct TPC coordinates for effects from sector
                misalignment effects in Rz.

   ---------------------------------------------------------------------
 ! 911015 - ALEPHLIB 13.1 contains
            +++++++++++ BECAREFUL from now on data base bank given on
                        data cards and read with ALGTDB must have a
                        bank number NR=-1 ++++++++++++++++++++++++++++++
 mods        :  YTOP set
               new package version 131. Look at YTOPNEWS for details
               and at ALEPHNOTE 91-132  SOFTWR 91-005
                KINE set
               mods in interface to JETSET 7.3
                ADBRPN,ADBRUN,ADBROW,ADBRLIST
               prepare introduction of SiCAL
                ALRWEV in EDIR set
               if the REVH bank does not exist or the class word
               does not exist in REVH call SELEVT to build the
               class word.
                 TPTOJ in PTOJ set
               to allow for reading the PTUN bank from data .This will
               give the possibility to change the packing factors for
               coordinates in the future.
                 ALREAD and COMPRESS sets
               several corrections for DECS stations
                 MUELID in EDIR set
               correction to use PEID instead of EIDT
                  ABRREC in  ALPHREAD set
                modify the test to skip runs
                introduce INTERACTIVE mode
                  ALGTDB, USGTDB in ALEF set
                 data card bank number is set to -1 by default
                  GTDBAS in ALEF set
                 drop BOS internal bank '+DIS' when a new data base is
                 opened.
  new routines:  FPKCM in PTOJ
                allow to pack a covariance matrix. Use FUPKCM to unpack
                 ABOPEN (CASE,IRET) in ALPHARD set
                  to open FILI, FILO or BOTH files
                 GETEVT (FSEQ,LIST,ULIST,NRUN,NEVT,IRET)
                 to get event # NEVT from run # NRUN
                 ENTRY ABGTRCL(ICLASR)
                 to get the read class word set sofar

 ---------------------------------------------------------------------
 ! 910925 - ALEPHLIB 13.0 contains
         ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         BECAREFUL this version should be used with ADBSCONS = or > 158
         +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 mods in       :    TLACOR , TUN1NC in TPC set
                 new sector edge correction for dE/dx.
                 bug fixes and improvements in 2-track dE/dx.
                 new Z corrections to TPC hits to correct for effects
                 measured by VDET.
                 new method for packing covariance matrices.
                 new code for TPC drift field corrections to account
                 for phi dependance.
                     VTRFIT package in VDET set
                 adjust error for large pulses or double pulses.
                 order track using extrap. error instead of momentum.
                 look for cluster in adjacent wafers.
                 add hit error to the extrap. error matrix.
                 use new matrix invertor.
                 cut on residual difference for overlap.
                 normalise pulseheight before cut, select good clusters.
                 track ambiguity.
                      UMSERR in COMPUTE set
                 add VDET material.
                       VTRKEX in VDET set
                  modify track error matrix.
                       AOPTAP in ALREAD set
                  modify a warning message
                        TRUSLU in EDIR set
                  small bug
                         ABRREC in ALPHREAD set
                  make a garbage collection before reading next record
                  in sequential mode
 new routines  : TLACOC, TLACOP, TZCRVD in TPC set
                 VDMJLS in  VDET set
                 internal routine called by JULIA works only for
                 1991 geometry.
 ------------------------------------------------------------------
 ! 910903 - ALEPHLIB 12.9 contains
  mods in      : PTOJ set
                 FPTOJ is modified to unpack the covariance matrix
                 depending on the content of word(29) of the
                 PFRF bank.
                 UFTKAL
                 add some protections
                 BOOK set
                 improve book keeping of MC data
                 ALTRIG
                 get the trigger bit words  from MINI DTBP bank
  new routines : COMPUTE set
                 UTRID
                 Householder reduction of a real, symmetric matrix
                 UVTRID
                 Eigenvalues and eigenvectors of real symmetric matrix
                 PTOJ set
                 internal routine FUPKCM called by FPTOJ
 -----------------------------------------------------------------
 ! 910803 - ALEPHLIB 12.8 contains
          ++++++++++++++++++++++++++++++++++++++++++++++++++
           REMEMBER the normal way to read/write EDIR files
           is through the ALPHARD package calling ABRSEL
           an example is stored in ALPHNEWS and ALRWEV.
           programs calling AREAD, BCLASW will not be able to
           write EDIR files correctly, and will not read them
           right when they contain several FILM cards.
           The BOS77 version 1489 is mandatory.
          ++++++++++++++++++++++++++++++++++++++++++++++++++++
   mods in       : ALPHARD set
                   to read/write several small (1000 words) $PTS
                   banks on EDIR files.
                   ALEF set
                   make a fix in ALFIEL for run 11961 which has a null
                   polarity in RALE bank.
                   DEDX set
                   use directly BOS routines to get data base banks
                   instead of ALGTDB in TBTBLK and TDXERR.
                   COMPUTE set
                   make sure thatcovariance matrices delivered by
                   UFTKAL are always positive definite.
   new entry     : ABGTWCL in ABRREC
                   to get the write class word set at the time of
                   the call : CALL ABGTWCL(KCLASW)
                   ABSTCL in ABRREC
                   to set the write class word to a given value:
                    CALL ABSTCL (KCLASW)
   new routine   :  ALEF set
                    ALCLASW (KCLASW) returns in KCLASW the class word
                    built by the EDIR package.
                    ALSUMCL (KCLASW) accumulates usage of various
                    classes
                    ALSUMCL (-1) will print the table.

 ------------------------------------------------------------------------
 ! 910723 - ALEPHLIB 12.7 contains
   mods in       : DMPCHR, YFTHVX
                   unset variables found by CRAY
                   GTDBAS
                   correction for DECS and APOLLO
                   PTEXSJ
                   small bug
                   HVOBIT,PHEDIR,AOPDBS,OPENDA,OPENDB
                   corrections for APOLLO
                   TDXERR
                   mods mandatory to analize DEDX in '91 data.
   purge           TEDEDEX,TINCAL,TELCAL,TBBLOK,TEXERR
                   these obsolete routines have been purged.

 --------------------------------------------------------------------
 ! 910618 - ALEPHLIB 12.6 contains
   mods in       : UFITQL, PAIRFD, PTEXSJ, EBNEUT, YPRIMV, YTOIJO,
                   YCHIV2,VDPRDI
                   small corrections.
                   TRACBCK to introduce CRAY trace back.
                   UMSERR to take VDET material into account.
                   PAIRFD to choose the bank FRFT with or without VDET
                   TCHKRV
                   ALREAD set
                   to introduce DEC station flag DECS
                   to modify the FILM card of the EDIR produced
                   on IBM.
                   VTRFIT and al. to use work banks instead of
                   named banks whereever it is possible.
   new routine   : ECLEAK in PHOTONS set
                   NVPCOB,NVPECO,PCMECU,PCPATQ in PTOJ set
                   fill PCQA bank from PCPA bank.

 --------------------------------------------------------------------
 ! 910502 - ALEPHLIB 12.5 contains
   mods in       : TRIGGER level 1 summary
                   *CD X1PARA and X1RSC are obsolete
                   *DK X1PREP and X1DCEB are obsolete
                   *DK X1RSUM is modified
                 - ALEF set
                   *CD ADBRLIST is modified to handle
                   detector "DB"
                   ADBRxx , GTSTUP are modified
                   ALGTDB is modified to call GTDBAS when
                   there is a new real data run to check the
                   validity of the data base.
                   GETLEP
                   new entry point to be called after a call to GETLEP
                   ENTRY GETOFS (IRUN,OFSET)
                   returns in OFSET  the average D0 for run = IRUN
                 - ECALDES set : new version  (see EDESNEWS)
                   print the date and version number at first entry
                   introduce treatment of the alignment
                -  DE/DEX set :
                   add gain correction from TC3X bank to TIDHYP.
                   fix bug in TBTBLK.
                   use double precision in TNRHPA (VDET needs it)
                -  PHOTONS set :
                   EBCLCF is modified to get EPCC and ECCA with the
                   right setup code (MC data) or run number (Raw data).
                -  PRESS set :
                   APOLLO version introduced.
                   vectorized version introduced for CRAY.
                   call NLISTF in DMPLIS to speed up.
                   give the facility to prevent some banks (PIDI)
                   to be compressed.
                 - PTOJ set:
                   FPOTJ is modified to unpack PFRF,NR=2.
                   PTEXSJ is modified to unpack PT2X into T2XS.
                   PAIRFD is modified to make the computation
                   of the invariant mass in double precision to
                   avoid negative masses.
                 - VDET set :
                   The VDTRACK package is introduced.
                 - YV0 set:
  YV1CJJ - YV1C bank extended - more HAC parameters.
  YV1INI - extend common/YV0CUT/ for new cuts from YV1C.
           Load cuts into /YV0CUT/. If old YV1C bank then use defaults
           for new cuts
  YV0BOK - 4 histos added. Histos renumbered to be in sequence of cuts.
  YMFV0V - Add entry point YMFV00(NVMAX) to set max. no. of V0
           Use new cuts from /YV0CUT/:
              no. of TPC hits on track
              Minimum track momentum
              No. of coords. before vertex
              Min. distance from prim-to-decay vertices
              d0 of the V0
              Z0 of the V0
              Mass window around K0, Lambda, Gamma.
  YVCAME - modify to give total no. of coords before vx. in VXD+ITC+TPC.
  YCHIV2 - change to use d0 and errors w.r.t. primary vertex instead of
           origin (0,0,0). Uses new routines:-

   new routines :  INTEGER FUNCTION ALTRIG (ITRG1,ITRG2,ITRG3)
                   returns the 1st 3 words of XTEB or XTRB
                   ALTRIG = 0 if no XTRB or XTEB is present
                            1 if XTEB is there
                            2 if XTRB is there
                   INTEGER FUNCTION GTDBAS (LBAS,IRUN)
                   checks that the current data base is valid
                   for this run. If not open the valid one.
                   GTDBAS = 0 if a valid data base is opened.
                   UFITQL
                   subroutine moved from JULIA
                   EDIR set
                   set of subroutines to build EDIR class word
                   PRINT set
                   TPADD (ISECT,ILUN) dumps TPC pad hit bank
                   YVO set
                   YPSIVN YD0NFN YZPHAN YFIXYN

 -------------------------------------------------------------------
 ! 910305 - ALEPHLIB 12.4 contains
      +++++++++++++++++++++++++++++++++++++++++++++++++++++++
      BECAREFUL!!!!!! Some DE/DX routines require ADBSCONS 140
      ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   mods in        : VDET and VDETDES
                    VRDDAF fills a specific common block
                    /VFTKAL/ for UFTKAL and /VDJGEO/ for
                    VDTRAK package.
                    UFTKAL
                    few corrections , introduction of /VFTKAL/
                    and UFVDMS
                    EBSATU,EBRGCD
                    small corrections.
                    AOPEN,CRNATY,AOPTAP
                    on CRAY use /pool/aleph/ instead of /pool/stage/xu/
                    and handle robot cartridges.
                    ADSPOS
                    dispose EPIO files with aldisp instead of dispose
                    CMPINI,CMPIN2,CMPINF
                    add an option to suppress bank formats for
                    compressed banks
                    MDST
                    A new library has been created independantly
                    of ALEPHLIB. It is maintained by S.Haywood.
                    MINI OLDLIB, TXTLIB PHY
                    ALEPH$GENERAL:[MINI]MINI.HLB, .OLB, _D.OLB, .FOR
                    The code has been removed from the ALEPHLIB.
                    DEDX
                    corrections to existing routines and some new
                    routines which need ADBSCONS 140
                    the factor EFACTM is put in a PARAMETER statment
                    it is used to unpack DTRA bank in TIDHYP

   new entry        ABRREC
                    new entry point ABGTCL (ICLAS) returns in ICLAS
                    the class word of the current event.

   ------------------------------------------------------------------
 ! 910122 - ALEPHLIB 12.3 contains
   mods in        : PRPART  update print format
                    BOOK    correction of few bugs and simplification
                    UFTKAL  few corrections, introduce VDET into the fit
                            (UFVDMS is not yet in the ALEPHLIB)
                    ABCHCK  correct a bug in SEVT card.
                    ABALFA  make sure that the old FILM card is dropped
                            before reading a new file.
                    AOPEN   do not call BUNIT and BRWND but BEPRW
                            before calling EPDEFU in case of EPIO files.
                            write messages on IW(6).
                            On VAX it is not possible to read an EPIO file
                            sequentially when the previus EPIO file has been
                            read directly!!!!!!!!!!!!!!!!
                    AOPTAP add DEV parameter just in case of SMCF
                    cartridges.
                    AFILMI, ADSPOS, AOPEN, AOPENW, AOPTAP, AWRTAP, CRNATY
                    CRAY corrections to use transparent (unblocked)
                    binary format
                    be able to read EDIR on cartdriges.
                    ADBROW,ADBRPN,ADBRUN,GTSTUP,ADBRJJ
                    add GEneral detector which defines the overall
                    structure of the apparatus.
                    IGEOMW, IFECON, ILIVEW, ITDRIF, IXHITS
    Ensure IRFE, IZFE and IWST banks are got from correct place. These
    three database banks may be in the run header and/or database. Also
    add some protection for missing banks.
                    ICRCOO  Add protection for missing ICAE bank.
                    IPRDAF  Tidy format.
                    ITROTN  Output angle in range 0 - twopi
                    IPTOJ   Tidy code used for data generated from
                            old versions of JULIA.
                    TGHPAL
               modified by extracting out most of its code into a
               separate routine which is completely arguement driven
               and can be called from a different context.
                    TMDEDX
               check if more than 40% of wire pulses within a sector
               are saturated.
                     PTEXSJ
               take care of additional word added to TEXS and PTEX for
               flagging tracks with too much saturation

    New routines   :  IGETDB
                      ALSTHO (IHOL,NWRDS,CHSTR)
                      transforms an array of hollerith into a char.string
                      CLMONS, BULOS
                      new photon identification package added
                      to the PHOTONS set. Look at PHOTNEWS for
                      more details.
                      KINE
                      add new routines KXL7.. to interface  LUND 7.3
                      TWRRED
       new TPC wire reduction code which can be called
       both from JULIA and TPCSIM.  The call in TPCSIM still needs to
       be added.  This module contains both the old algorthm as well
       as a new one which we are evaluating.
                      TSCINT and TSNEXT
       new geometry routines needed to improve the performance of the
       JULIA module TRKWRA
                       TK2IVM
       utility routine for calculation of the error on the
       invariant mass of a pair
                       UMSERR
       calculate multiple scattering contribution to track errors.
       This is meant to replace UINCOV.

    Purge            : TELOSS,TEDBSP,TEVBSP,TPARID

   -------------------------------------------------------------------
 ! 901213 - alephlib 12.2 contains
   mods in        : VRDDAF
                    use the setup code to get banks from the DAF.
                    ALGTDB
                    if run number is negative make sure that the required
                    banks are in memory.
                    AOPEN AOPTAP ACDARG
                    on IBM use the robot through the keyword SMCF
                    instead of CART.
         BECAREFUL THIS VERSION NEEDS ADBSCONS 138
   -------------------------------------------------------------------
 ! 901109 - ALEPHLIB 12.1 contains
   mods in        : AOPEN, AOPENW
                    mods for APOLLO.
                    ACLOSE,AFILIN,AFILMI,AINQUI,AOPEN,AOPERD, AOPTAP
                    on IBM, keep FILEDEF for EPIO files.
                    Otherwise open files using the facility:
                    OPEN (...,FILE=/fname fmode ftype,...)
                    read FILM card with BTERD directly to avoid a
                    wrong print statment.
                    when closing an EPIO file reset the EPIO access
                    mode flag to sequential.
                    on CRAY delete the tape files which have been staged
                    on $BIG/ (NO  -K in the FILI card) when closing
                    the file.
                    EBTRAN
                    pi0 at azimuthal angle=0.degree were wrong. It happened
                    in 1/1000 case.
                    VDET and VDETDES
                    few corrections.
                    THLCIR
                    make the algorithm more efficient.
                    TBBLOK
                    make it work with charge.ne.1.
                    TGHPTS
                    bug fix. DEDX was sometimes screwed up in JULIA.
   new routines   : 4 integer functions in ALEF set use the new GALEPH
                    ASIM bank and the new data base ADBR bank.
                    NROW = ADBROW (det,irow,IPERIOD,IRUN,ISETUP)
                    returns the run period, 1st run and setup code for
                    a given detector and a given ADBR row number.
                    NPERIOD = ADBRPN (det,IROW,iperiod,IRUN,ISETUP)
                    returns the ADBR row number,the run period,the 1st
                    run and setup code for a given detector and run date.
                    NRUN = ADBRUN (det,IROW,IPERIOD,irun,ISETUP)
                    returns the 1st run, the row number, the run period
                    and the setup code for a given detector and run #.
                    ISETUP = GTSTUP (det,irun)
                    returns the setup code for a given detector and run #.
   -------------------------------------------------------------------
 ! 901003 - ALEPHLIB 12.0 contains

   mods in        : When NOT using ABRSEL to do I/O BUT calling
                    ASEVT, or AREAD directly, please add a
                            CALL ABCHCK (IRET)
                    after reading data cards. IRET=11 means
                     "error in data cards".
                    ALEFIL, ALGTRO, MINTRA, IFECON
                    bugs removed
                    UFTKAL
                    protection added
                    ALPHARD package
                    reorganization, the ABRSEL routine is cut in several
                    pieces.
                    AOPEWR, CRFILM
                    when writing an EDIR file on the output write the
                    FILM bank when opening the EDIR file.
                    ABSEVT
                    a new data card 'ONER' indicates that there is only
                    one run per run. This allows to quit the input data
                    file when the last selected event has been found
                    without looking for another run.
                    new entry points ABSMAX, ABSLIM, ABONER
                    KINE and FYXX packages
                    handle new banks KZFR and FZFR

   new routines   : new set of routines in VDETDES package to handle
                    '90 VDET geometry.
                    JKLUN = ALKLUN (LUVER,LUDAT)
                    fills the KLUN bank with the LUND version # and date
                    of last change.
                    IRET = ALGTLU (LUVER,LUDAT)
                    returns the LUND version # and date of last change of
                    the LUND version used during event generation. IRET=0
                    when LUND was NOT used.
                    CALL ABSMAX (MAXRUN,MAXEVT)
                    resets the greatest run and event number to be selected.
                    CALL ABSLIM (NNMIN,NNMAX)
                    resets the 1st and last events to be selected.
                    CALL ABONER (ONE)
                    resets the flag ONERUN to true or false.
                    CALL ABOLDR (IOLDR)
                    dummy routine called after the end of run IOLDR.
                    CALL ABRSEL (ELIST,ULIST,IRET)
                    this routine is the steering routine of the new ALPHARD
                    package. The package runs in BATCH mode by default.
                    BATCH mode means ALPHA mode, it works exactely as
                    previous routine ABRSEL.
                    calling routine ABMODE('INTE') before calling ABRSEL
                    will switch the mode to INTEractive. But this mode is
                    not yet implemented.
                    CALL ABALFA (JRET)
                    part of old ABRSEL which was handling the opening
                    of files. JRET is a return flag. Called by ABRSEL in
                    BATCH mode every time a file has to be opened.
                    CALL ABCHCK (JRET)
                    checks the syntax of data cards SEVT,SRUN,IRUN,CLAS,
                    TIME. Called by ABRSEL at 1st entry. Can be called at
                    any time.
                    CALL ABRREC (ELIST,ULIST,IRET)
                    previous routine ABRSEL:
                    - writes output list if any before reading next record
                      (ABWEVE).
                    - reads next record
                    - checks that the run or event has been selected (ABSEVT)
                      if not continue reading
                    CALL ABWEND
                    close output files if any. Must be called at end of job.
                    CALL ABWEVE
                    called by ABRREC before reading the next record to write
                    the output list with or without EDIR.
                    The output list is set by a user call to ABWSEL.

 --------------------------------------------------------------------------
 ! 900917 - ALEPHLIB 11.9 contains

   mods in        : CMPINI, CMPIN2, CMPINF
                    to restore CFMT data card
                    IER = ALFMT (LUFMT,NAME,FMT)
                    add the facility to create all BOS formats
                    the 1st time the function is called if NAME='ALL '.
                    ABRSEL
                    move intialization of COMPRESS package right
                    at the beginning. If a garbage collection occurs
                    should not damage anything.
                    ABSEVT
                    decide on selection yes/no every time the
                    routine is called and not the 1st time only.
                    if run # = 0 ten look at the current run #.
                    new ENTRY ABSMAX (LASRUN,LASEVT)
                    to reset maximum run # and evt # to look at.
                    usefull in interactive mode.
                    ASEVT
                    reset LASRUN and LASEVT before calling AREAD.
                    AGTLTC
                    does not try to read LTC file when LTC logical
                    unit = 0.
                    JUNIDB
                    set LTC logical unit = 0.
                    ALFIEL
                    reverse the mag.field only for raw data file
                    with run # > 6000 and no FIEL data card.
                    AOPEN
                    remove bug in the CRAY GIME
                    PRINT package
                    BPRTAB has been re-written and PRTABL has been
                    modified accordingly to use the BANKAL FMT file
                    which contains all the known ALEPH bank formats.
                    PRTWRK has been dropped.
                    PRKINE, PRFKIN
                    update formats according to new KVOL and FVER banks.
                    FYXX package
                    add a word in FVER at the end of the line which
                    gives the mechanism at the origin of the vertex.
                    PTPCOJ, PTGMAJ
                    correct the TPCO format and increase JLIST dimension
                    TIDHYP
                    remove check on TPC HV bits which is better done
                    in TCHKHV.
                    UFTKAL
                    non-fatal bug
                    MDST package
                    look at MDSTNEWS for more details on changes.
                    PHOTONS package
                    look at PHOTNEWS for more details on changes.

   new routines:    INTEGER FUNCTION IGTFMT (NAMI)
                    returns the index of the BOS format bank of a
                    bank known by its name-index NAMI.
                    this routine will be moved to the BOS library
                    and then removed from the ALEPHLIB.
   -----------------------------------------------------------------
 ! 900725 - ALEPHLIB 11.8 contains

   mods in        : TCRTOF, TCRTRA
                    better use of the AGETDB return code (TDVV bank)
                    ALELEP, LFILRU, ALGTRO
                    to adapt to the new set of routines below.
                    ABRSEL
                    allow reinitialization of the package

   new routines   : GETLEP (IRUN,IFOUN,IFILL,NV,ELEP,XYZ,DXYZ)
                    returns the LEP energy for run # IRUN   ELEP
                    if IFOUN>0 returns the LEP fill #       IFILL
                                       the number of events NV
                                       the vertex position  XYZ
                                       the error on XYZ     DXYZ

                    GETLUM (IRUN,IFOUN,IRQFL,NZ,NB,RLUMI,BK,BT)
                    returns luminosity information for run # IRUN
                    if IFOUN=0 no information avalaible
                       IFOUN>0 returns # of Z0               NZ
                                       # of BhaBhas          NB
                                       luminosity            RLUM
                       IFOUN=1 returns run quality           IRQFL
                                       # of background BBs   BK
                                       BB trigger efficiency BT

   new entry     : ELEP =  ALEFIL (IRUN,JLFIL,IROW)    in ALELEP
                   returns ALEFIL = LEP energy for run # IRUN
                           JLFIL  = BOS index of LFIL bank used
                           IROW   = row # containing run # IRUN

                   JNAME = ALGTRO (NAME,NEW,JCOL,IROW)
                   returns ALGTRO = BOS index of the NAME data base
                                    bank which contains NEW in col. JCOL
                           IROW   = row # of the NAME bank which contains
                                    NEW in col. # JCOL

   ------------------------------------------------------------------------
 ! 900706 - ALEPHLIB 11.7 contains

   mods in        : USGTDB, FMINIT, EBRGCD, YV1INI
                    small bugs
                    DEDX, PTOJ (TPC part)
                    some routines are moved from JULIA to ALEPHLIB
                    UFTTRK
                    EBTRAN
                    protection against -ve SQRT
                    ABRSEL
                    in case of error during decompression , print a
                    message and reset the error flag to 0, then
                    go on with the unpacking of decompressed banks.
                    ALREAD
                    few bugs related with the GIME option
                    ALGTDB
                    the initialisation of LRUN is set to -999
                    instead of -1 to allow data bank number = -1.
                    introduce entry point ALGTD1
                    the default data card bank number can be
                    changed by a call to the INTEGER FUNCTION ALGTD1
                          IOLD = ALGTD1 (INEW)
                    will changed the data card bank number from
                    IOLD to INEW.
                    USGTDB
                    introduce entry point USGTD1
                    the default data card bank number can be
                    changed by a call to the INTEGER FUNCTION USGTD1
                          IOLD = USGTD1 (INEW)
                    will changed the data card bank number from
                    IOLD to INEW.
                    MDST
                    few changes

   new routines   : DEDX, TPCDES, PTOJ (TPC part)

                    IER = INTEGER FUNCTION ALFMT (LUFMT,NAME,FMT)
                    get the BOS format FMT of the NAME bank from
                    the BANKAL.FMT file opened on LUFMT
                    BKFMT is called with tthe FMT format
                    IER=1 if there is no format (default I)
                    IER=0 means a format has been found .

                    ENTRY DMPGER (LIST,ERLIST,NER) in DMPLIS
                    this routine can be called in ALPHA after
                    decompression to get the error flag and the
                    number and list of non decompressed banks.

                    IOLD = ALGTD1 (INEW) ENTRY in ALGTDB
                    this function can be called at any time to set
                    the data card bank number to INEW. The default
                    value of INEW is 0. It returns the previous value.

                    ADBSWP (IOLD,INEW)
                    will swap all data base banks present in the BOS
                    array at the time of the call with bank number IOLD
                    to INEW. And will call ALGTD1 (INEW) and USGTD1(INEW)
                    to set the data card bank number used in these 2
                    routines to INEW.

                    ECMS = ALELEP (IRUN)
                    will return the best estimate of the LEP energy
                    for the run # IRUN.

                    JLFIL= LFILRU (IRUN,IROW)
                    will return the row # IROW which contains the
                    run # IRUN in the LFIL bank with BOS index JLFIL.

                    JNAME = ALGTRO (NAME,NR,NEW,JCOL,IROW)
                    will return the row # IROW which contains in the
                    column # JCOL the quantity NEW in the BOS bank
                    NAME,nr=NR.
                    for these routines, look at the subroutine header
                    to understand the return code.

   ------------------------------------------------------------------
 ! 900621 - ALEPHLIB 11.5 contains

   mods in        : AOPTAP
                    to distinguish between VXCERN and VXALUW so that
                    things work properly for ASCII-labelled tapes.
                    ADSPOS, AOPENW
                    to distinguish between IBM and VAX CRAY stations
                    AGTLTC
                    blank input list and internal list
                    MALREC entry into JALREC
                    small bug
                    ECALDES
                    purely historian change: put all 4characters *CD
                    in one *CD ECCCOM.
                    PAIRFD
                    get conversion point coordinates through entry point
                    CALL PAIRCP(XA,YA,ZAV)
                    HDEADM
                    small bug fixed.
                    ITPRDI,ITPRHT,ITDCOR
                    adapt to new hit definition. Still compatible with
                    old Monte Carlo data.
                    PITCOJ
                    modified to produce the ITC corrected coordinates
                   (bank ICCO).
                    HAC
                    update existing HAC parameters with the ADBSCONS 127
                    X1DISC,X1DCEB
                    because of the change in XTEB.DDL (JXTEET does not
                    exist anylonger. It has been replaced by JXTELW)
                    VAENSA, VADESA
                    1st strip is number 1 (and not 0).
                    VDPRDI
                    prints new VDET banks VHLS,VPLH.
                    YV0
                    increases the size of some arrays
                    PHOTONS
                    few corrections
  new subroutines : to make ITC corrected coordinates.
                    IASIGA,ICDRIF,IMICCO,ITCFLG,ITQUAL,
                    ITXFLG,ITXING,IUDOCA,IUPCCO,IUVCRS.
                    VRDDAF(LBAS,IRUN,IFLAG)
                    to read VDET data base and fill geometry commons.
                    some routines for SCANBOOK
                    BKLFIL fills LFIL bank
                    BKRLUM fills RLUM bank
                    ALDAWR writes a LIST of banks on a DAF.
                    ALGTBK gets/creates/or modifies a DAF bank.
                    EBGAGA , EBHAHA in PHOTONS set
 --------------------------------------------------------------------
 ! 900501 - ALEPHLIB 11.2 contains

   mods in        : EBINIT
                    remove mods implemented in version 11.1
                    UFTKAL
                    add some protection
                    TMDEDX, TBBLOK
                    new DE/DX calibration
                    DMPBAN, DMPLIS
                    try to speed up decompression
   new routines   : TCHECK(IRUN)
                    checks for existence of good DE/DX constants
                    on the data base.
                    DMPBIN
                    set of routines written in IBM assembler to
                    speed up decompression on IBM.
   new package    : ALPHARD
                    I/O package called by  ALPHA .

 -------------------------------------------------------------------
 ! 900423 - ALEPHLIB 11.1 contains

   mods in        : ALREAD
                    implement EDIR on CRAY
                    small bug in AOPEN, AREAD
                    PRTABL, X2PRIN, YFTVVX
                    remove printing of unset variables
                    ALFIEL
                    change the sign of the field for run > 6000. when
                    there is no FIEL data card.
                    ECALDES
                    better description of the overlap region and
                    endcaps
                    PHOTONS
                    few bugs
                    TRIGGER LEVEL1
                    corrections to be compatible with old and new
                    data structure.
                    PRESS
                    to use CPRO bank instead of big TAB2,COL2 banks.
                    the use of CPRO is triggered by data card:
                    COMP  'CPRO'

   new routines   : CRENVI
                    to get the HOST machine (IBM or VAX) of the CRAY
                    YV0
                    new package. Look at YV0NEWS for details.
                    CMPIN2
                    called by CMPINI when CPRO option is chosen
                    initializes the COMPRESS package from CPRO bank
                    instead of TAB2,COL2.
                    DMPFMT
                    build a BOS format from compress bank info.

   -----------------------------------------------------------------
 ! 900320 - ALEPHLIB 11.0 contains:

   mods in        : ALREAD
                    DOES NOT CONTAIN MODS TO READ EDIR ON CRAY
                    to be able to read NATIVE files in DIRECT
                    access mode on IBM.
                    AOPEN, GIMEDK
                    bugs
                    calls JULREC to get the record length of an
                    input file
                    AOPENW
                    calls KALREC to get ALEPH record length of
                    an output file
                    AOPTAP
                    installs new facility MOUNT tapes/cartridges.
                    JALREC
                    adds entry points KALREC and MALREC.
                    AUBOS
                    calls USBOS which is dummy on the ALEPHLIB.
                    FYNHIS
                    to implement HERWIG history code
                    FYKILL
                    to keep the history of K0's decays and lambdas
                    in some parts of the ITC.
                    FPTOJ
                    small correction
                    AUHCYL
                    protection against -ve SQRT.
                    UFTKAL
                    small bug and unset variables

   new routines   : USBOS (NAME,NR,LE,KNDX,IGARB)
                    user routine called by AUBOS when there is no
                    space left after a garbage collection

                    JULREC (FTYPE,FDEVI)
                    gets the record length of a file with aleph type
                    FTYPE and device FDEVI.
                    FTYPE must be one of the ALEPHtypes.
                    FDEVI can contain the parameter LREC followed
                    by the record length
              i.e. FILI 'FLRLUN02 EPIO * | GIME PUBXU 407 LREC 3600'

                    KALREC (FTYPE)     entry point in JALREC
                    returns in KALREC the record length of aleph
                    type FTYPE.
                    MALREC (FTYPE,IRECL)   entry point in JALREC
                    the default ALEPH record length can be modified
                    by the function MALREC(FTYPE,IRECL).

                    AMOUNT (LUN,FNAME,ATYPE,FDEVI,IER)
                    mount tape/cartridge on IBM.

   -------------------------------------------------------------------
 ! 900215 - ALEPHLIB 10.8 contains:

   mods in        : GIMEDK
                    uses QDFREE installed on all IBM machines
                    X1TRIG
                    calls X1HIST
                    ALFIEL
                    correction of 1.1% when compensation coils are off
                    YTOPVT, UFTKAL, TLACOR
                    HCALDES
                    take care of the air between tubes in the end caps
                    the correction is made at the level of HRDDAF, the
                    correction introduced in HFEHIT in version 10.6 is
                    removed.
                    correction of few other bugs.
                    MUPRHT, MUPRDI
                    move these routines to PRINT set.
                    ECALDES
                    look at EDESNEWS for details
                    PAIRFD
                    reduce the number of error returns. make the
                    algorithm less sensitive to machine precision.
   new routine    : MUCATR (ITRA,PR1,PR2,IFLAG)
                    probability for a track to be a muon from MUCALO

   ------------------------------------------------------------------
 ! 900124 - ALEPHLIB 10.7 contains:

   mods in        : ALOPEN, DMPCHR
                    reset some variables to 0
                    ADBVER, AOPDBS
                    open LTC file and get version number and date
                    of it.
                    ACLOSE
                    close files depending on the mode (EPIO,FORTRAN)
                    and the machine (IBM,CRAY or VAX).
                    ACDARG
                    call GIMEDK on IBM
                    AOPENW
                    write EPIO files in RECFM F instead of U.
                    IRDDAF
                    get info. from new Dbase banks ICAE,ISFE,IZRS.
                    Must use next version of Dbase (version 120)
                    ICRCOO
                    better coord. determination and data correction
                    using new and existing Dbase banks.
                    PITCOJ
                    add IFCO bank to S-list.
                    HFEHIT
                    small correction of alignment
                    X1INP
                    mistyping error
                    correct few APOLLO fortran bugs in various routines

   new routines   : ILATCH
                    create IWCR (wire-to-coord relation) bank.
                    ILIVEW
                    create ILIV (live wire) bank.
                    TRIGGER package
                    introduction of a level1 trigger summary which
                    can be called in JULIA. Look at TRIGNEWS for more
                    details.
                    GIMEDK internal routine to ALREAD
                    get a free letter to make a GIME in READ or WRITE

   ------------------------------------------------------------------
 ! 900108 - ALEPHLIB 10.6 contains:

   mods in        : ASEVT, AUNPCK
                    remove references to +FIL bank
                    EBCRAD
                    small corrections
                    VDPAJJ
                    update HAC parameters

   ------------------------------------------------------------------
 ! 900100 - ALEPHLIB 10.5 contains:

   mods in        : X1INP
                    a HAC parameter had changed its name.
                    ALFIEL
                    get the magnetic field from 'RALE' raw data bank
                    when it is there
                    ACLOSE
                    new routine using +BUF BOS internal bank to close
                    all files opened in BOS.
                    AFILIN, AFILMI, AFILOU, AIOCAR, AOPERD, AOPEWR
                    new routines using /ALRCOM/. AOPERD and AOPEWR
                    do not call AFILIN and AFILOU
                    AIOCAR (CARIN,NINP,CAROUT,NOUT)
                    uses NINP and NOUT as input arguments to set the
                    length of CARIN and CAROUT
                    PHOTONS
                    several improvments
                    UFTKAL
                    protection against numerical problems .
   new routine    : AGTFIL (CARD,FLAG,LUN,IER)
                    open in FLAG mode ('READ' or 'WRITe) the file
                    given on CARD data card onto logical unit LUN.
                    several new routines in PHOTONS.
   ------------------------------------------------------------------
 ! 891100 - ALEPHLIB 10.4 contains:

   BECAREFUL THE HISTORIAN LIBRARY HAS BEEN RESEQUENCED
             HAC parameters have been replaced with the most recent
             version of BANKAL.HAC found in:
             ALWS::ALEPH$GENERAL:[DOC.BANKDOC]BANKAL.HAC

   mods in        : ALREAD
           ALSEQR : SEVT card is now standard between EDIRs and data files
           AOPERD : EDIRs with cartridges is now automatic. One can now
                    mix with multiple FILI cards, EDIRs refering to data
                    in NATIVE or EPIO format on cartridges, tapes or disks.
           ACLOSE : one can also mix data files and EDIRs in the same card
                    file.
                    fix to avoid crash at end of job when closing NATIve
                    file.
                    CRSTAGE is now part of the package. It is no
                    longer necessary to load it on CRAY: it will
                    be removed from the CRAYXU.401 disk.
              FILI  'FNAME.FTYPE | GIME tid  addr'
                    will assign the file $STAGE/fname.ftype if it
                    exist or will fetch it from IBM disk "tid addr"
              FILI  'FNAME.FTYPE | ACQUIRE tid addr'
              FILI  'FNAME.FTYPE | FETCH tid addr'
                    work as "acquire and fetch" in CRAY JCL.
              FILO  'FNAME.FTYPE | DISPOSE'
                    will dispose the file to the user reader.
                    BOOK
                    various corrections.
                    new routine BKCOPY
                    COMPUTE
                    new fitting routines UFTTRK, UTKXNG
                    look at UFTTRK for more details
                    DEDX
                    protection in TIDHYP and TMDEDX
                    check the TPC dE/dx HV bit in REVH and return an
                    error code if it is not set.
                    return an error code if the dE/dx calibration
                    constant is zero (I now set this constant to zero
                    for a run  if it is impossible to calibrate the run,
                    so this is a method to avoid such runs)
                    ITC
           ICRCOO   modified to create IFCO bank
           ITDRIF   modified to use IET0 Database bank
           IINRES   modified to get IET0 bank from database
           IFCOJJ   new HAC params. for IFCO
           IET0JJ   new HAC params  for IET0
                    PTOJ
                    protections added in PTPCOJ, PECHIC
                    TRIGGER LEVEL 2
                    has been updated to follow ONLINE level2 trigger
                    HAC
                    completely new set of HAC parameters from:
                    ALEPH$GENERAL:[DOC.BANKDOC_NEW]BANKAL.HAC  8-NOV-89

   new routines     ALRLEP will fill RLEP online data bank

   ------------------------------------------------------------------
 ! 891021 - ALEPHLIB 10.3 contains:

   mods in        : ALREAD
                    the flag DIRECT used in AOPEN to open files in
                    direct access mode is set to FALSE at 1st entry
                    into AOPEN or ACDARG by a call to ADIREC.
                    it is set to TRUE by ACDARG in case of a FILM
                    data card od a DAF file type.
                    it is set to FALSE at the end of AOPEN.
                    PRESS
                    corrections for CRAY.

   ------------------------------------------------------------------
 ! 891000 - ALEPHLIB 10.2 contains:

   mods in        : PHOTONS
                    small bugs
                    ALGTDB, USGTDB
                    at 1st entry opens the LongTermConstant file
                    ADBSLTC.NATIVE and fills the list of LTC banks.
                    at next entry if the bank which is required does
                    not belong to the LTC list then
                    the ShortTermConstant direct access file is read
                    JUNIDB
                    2 more entry point JUNISQ, NUNISQ to get
                    the logical unit number of the LTC file :
                    the default is 8 and can be changed with a
                    call to NUNISQ.
                    AOPTAP
                    mod to cope with new density default on CERNVM
                    AUBLIS
                    can print a list of any length
                    ITC
                    make use of the fine-tune constants in the ITC drift-
                    time relation. The new constants are in the IEWP and
                    IEDD data base banks.
                    THTFLG
                    take care of known dead channels.
                    TMDEDX, TC2XJJ
                    New calibration constant for the dE/dx routines.
                    YTOP
                    several corrections
                    ALREAD
                    correction to read several files with event dir.
                    in sequence.
                    do not execute a GIME if it was already done.

   - new routines:  AGTLTC (NLTC,LTCLST,IER)
                    LTCLST(1:NLTC) will be filled with the list of
                    banks read from ADBSLTC.NATIVE or from the file
                    given on the FLTC data card.
                    MDARD from BOS77
                    calls AGTLTC at 1st entry

                    TRHFLG(HPT,IFLG)
                    which padrows is a given helix with parameters HPT
                    expected to hit (IFLG(1:21)=1 if a hit is expected
                    on this row)
   -------------------------------------------------------------------
