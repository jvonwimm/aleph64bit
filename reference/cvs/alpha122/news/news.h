========================================================================
                             ALPHA 122 News
                        Last Update: 17 July  1998
========================================================================
 
  ALPHA version 122 

  All features of the successive correction files are summarized below ,
  starting from the most recent one. The oldest one corresponds to ALPHA 122.30
  All older corrections or new features are described in the ALPHA User's Guide 
  released on June 6th, 1997.

===================================================

  Correction file 122.48 released 17 July 1998
  Modifications w.r.t ALPHA 122.47

  ** Modifications in QFILL, from F. Ligabue
     in order to make easier the work of the LEP 2 W group.
     All these modifications act as do-nothing by default.

     - Call a new user routine QURESC (dummy) 
       the user may call it to make several rescalings of HCAL and ECAL 
       for systematic studies
       see explanations in the comments of the routine:
       /al/reference/cvs/alpha122/pack/quresc.F
     - If a data card PCOR is provided, a new routine QTRESC is called
       by QFILL to rescale all charged track momenta (so-called "sagitta
       correction"). QTRESC calls automatically for all tracks the routine
       QPCORR (see news for ALPHA 122.44 of March 2nd, 1998)
       which does the sagitta correction track by track.

===================================================

  Correction file 122.47 released 4 June 1998
  Modifications w.r.t ALPHA 122.46

  ** Modifications in the Beam position routines, from S. Wasserbach:

     - Mod in qfget_bp to handle the glitch in the ALPB bank for MINI LEP1
       reprocessing and MINI 1997 made with MINI versions 103 and 104
       (a very small fraction of events have the uncertainty on y_beam =0.,
        the correction sets this uncertainty to the average value for the
        corresponding year).

     - Mod in qfilbp: default values of QVTXBP,QVTEBP,QVTSBP are set to
       crazy values (position = 999. cm, uncertainty and luminous region =0.) 
       to force people to make a test on XGETBP=.TRUE.

  ** The 10 routines XLUMOK,..XVDEOK removed from the cvs source code are now 
     really removed from the ALPHA binary libraries (see news for ALPHA 122.42
     of January 20th,1998): they were still on the libraries and used intead of
     the ALEPHLIB ones, by mistake. 


===================================================

  Correction file 122.46 released 15 May 1998
  Modifications w.r.t ALPHA 122.45

  New format to print the ALEPHLIB version number in QADATI

===================================================

  Correction file 122.45 released 02 Mar 1998
  Modifications w.r.t ALPHA 122.44

  ** New Function QPCORR  (from Ian Tomalin).
       
     Purpose :

     Corrects particle momenta for the effects of residual distortions in
     the central tracking detector.
     This is the so called "sagitta correction".
     It depends upon cos(theta) and the year of data taking.
  
     Motivation:

     Every year, even after the detector alignment is finished and
     corrections have been made for field distortions etc., it is found
     that Ebeam/P in Z0 -> mu+mu- events is not precisely 1, presumably
     because of residual distortions.
     (Typically, in the region |cos(theta)| > 0.9, Ebeam/p is about 0.94
     for +ve tracks and 1.06 for -ve tracks. Elsewhere, Ebeam/p is usually
     consistent with 1 to within a percent or so. The effect is not quite
     forward-backward symmetric).

     The relative bias in momentum is proportional to P, so most people
     analysing hadronic events can ignore it. Exceptions include analyses
     using the ECAL electron identifiers in the region |cos(theta)| > 0.9,
     or analyses which are very sensitive to systematic biases in the
     momenta (e.g. jet charge, tau polarization).

     This routine provides a correction for the momenta based upon
     Ebeam/P measurements in Z0 -> mu+mu- events. It assumes that the
     corrections for -ve and +ve particles are equal in size, but of
     opposite sign. This is observed to be true, apart from a constant
     offset, Ebeam/p = 1.002, which is also present in the MC and so not
     corrected for.
     
     In principle, the correction depends upon your track selection
     cuts, but providing that the corrections have a small effect on your
     analysis, you can ignore this. Arguement NVDET does correct for this
     to first approximation however.

     Calling the routine: 

      FUNCTION QPCORR(ITRK,ASCALE,NVDET,ERCORR)

     Input Arguments:
       ITRK    (Integer): ALPHA track number.
       ASCALE  (Logical): Set .TRUE. if ALPHA variables like QX,QY,QZ
                          are to be rescaled by calling QVSCAL.
                          N.B. Don't call this routine twice for the
                          same track with ASCALE = .TRUE. !
       NVDET   (Integer): If your analysis only uses tracks with at
                          least 1 VDET hit, set NVDET=1; otherwise =0.
     Output Arguments:
       QPCORR  (Real)   : Scale factor applied/to be applied to momentum.
       ERCORR  (Real)   : Statistical error on this factor.
 
     If corrections are not available for any year, please contact the 
     tracking group.
   

  ** New data cards FR12 and FR10 for the Monte Carlo smearing of charged
     tracks introduced in JULIA 306:
 
     Purpose:
    
     JULIA 306 implements the TPC hit smearing for MonteCarlo. FRFT/0 (without
     VDET) and FRFT/2 (with VDET) contain the track parameters after refit
     with the smeared hits but keeping the error matrix from the unsmeared fit.
     This gives the best agreement between data and MonteCarlo.
     The track fit using the unsmeared TPC hits is still available in 
     FRFT/10 (no VDET hits in the fit), and FRFT/12 (with VDET).
     Note that the V0s, kinks etc. are done with the  smeared tracks.
    
     Therefore when reading in ALPHA a dataset written by JULIA >= 306, the
     charged tracks obtained by default are as usual the FRFT/2 ones, i.e. 
     the smeared ones with VDET hits in the track fit.   

     The new data cards allow to get in the ALPHA variables the unsmeared tracks
     for checks or comparisons, allowing to redo e.g. the V0 finding using the
     data card REV0, or the pair finding calling the routine QPAIRF. 

     Usage:

     FR12   puts in the ALPHA variables the FRFT/12 tracks : unsmeared, with
            VDET hits in the track fit;

     FR10   puts in the ALPHA variables the FRFT/10 tracks : unsmeared, without
            VDET hits in the track fit.
     

     N.B.1: the old FRF0 data card still works and puts in the ALPHA variables
            the FRFT/10 tracks : SMEARED without VDET hits in the track fit.
               
     N.B.2: one cannot give simultaneously 2 of the 3 data above data cards 
            (FR10,FR12,FRF0): if this is the case, ALPHA stops immediately.

===================================================

  Correction file 122.44 released 10 Feb 1998
  Modifications w.r.t ALPHA 122.42

  ** Increase BOS array to 1,500,000 by default (for LEP2 MCarlos) in QUIBOS

  ** Mod in QFILL, new routines QD0Z0 and KD0FL for new d0,z0 calculation:
     with use of a new data card D0NW as follows :

  1 - by default, one gets the same d0,z0 as in previous versions of ALPHA in
      the variables QDB(ITK) and QZB(ITK). This ensures backward-compatibility,
      but should not be taken as good for LEP2 runs and future LEP1 reprocessing

  2 - if the user puts a new data card:  D0NW       in the  CARDS file,
      QDB(ITK) and QZB(ITK) give new values of d0,z0 obtained as follows:

   ==> if there is no QFND data card, the "beam position" which is used
       is the event-chunk one, if it exists;

   ==> if there is a QFND card, the "beam position" which is used is the event
       main vertex computed by QFNDIP, if it exists.

   This new feature is valid both for LEP1 and LEP2 runs.
     
     In all cases, a new integer function tells which kind of beam position
     was used for a given event to compute d0,z0:

   IFLAG = KD0FL(dummy)

   The return value IFLAG has the following meaning:

   IFLAG = 1 if beam position from the QFNDIP event primary vertex
   IFLAG = 2 if beam position is the event-chunk one
   IFLAG = 3 if beam position from the old LEP 1 run-by-run average
               (= QVXNOM,QVYNOM,QVZNOM)
   IFLAG = 0 if none of the above values available
             in that case the beam position is set to 0.,0.,0. 

===================================================

  Correction file 122.42 released 20 Jan 1998
  Modifications w.r.t ALPHA 122.41

 ** Removal of XLUMOK and XVDEOK from the ALPHA code, since these functions are
    now in the ALEPHLIB. The calls to these routines are of course left in 
    ALPHA and this removal is user-transparent.

    All routines called by XLUMOK and now in the ALEPHLIB are also removed
    from ALPHA: LLUMOK,SLUMOK,XLSLUM,XT0SYN,XPTENB,QRE133,QRE94

    All routines called by XVDEOK and now in the ALEPHLIB are also removed
    from ALPHA: QHVSTA,KVGOOD

 ** Modification in QIPBTAG for LEP2 data and MCarlo: (from M. Thulasidas)
    Mod in BTAG_PAR96, new routine GTIPBK.
    The calibration constants for QIPBTAG are now read from the ADBSCONS
    database bank 'QIPC'. 
    The user MUST remove any old FITP cards from his CARDs file for LEP2 data
    and MCarlo, otherwise ALPHA stops. The use of FITP cards remains possible
    for LEP1 data/MCarlo.

===================================================

  Correction file 122.41 released 27 Nov 1997
  Modifications w.r.t ALPHA 122.40
  
 
 ** New facility: calculation of the beam spot informations using the LEP BOMs
                  for 1996 and 1997 data. 

    Code written by O. Schneider and maintained by C.Loomis.

    The ALPHA routine QFILBP has been modified to call a new routine, KGTBLQ.
    It allows to use the LEP BOMs+QS0 informations to compute the beam spot 
    information, stored in the ALPHA arrays QVTXBP, QVTEBP and QVTSBP
    which are used by QFNDIP and QIPBTAG.

    By default, nothing is changed w.r.t to the previous ALPHA version. 

    The user does'nt need to call any routine. To activate the new facility,
    one has to provide a data card BOBS with the following syntax:

 BOBS sigmax sigmay bom_only debug 

     sigmax   = real number equal to the LEP_BOM+QS0 intrinsic resolution in x; 
     sigmay   = real number equal to the LEP_BOM+QS0 intrinsic resolution in y; 
     bom_only = integer flag to indicate whether one wants to use only
                LEP_BOM+QS0information (bom_only=1)
                or if one wants to use the weighted average between the
                LEP_BOM+QS0 information and the VDET chunk-by-chunk information
                (bom_only=0); 
     debug    = integer flag to switch debug printout ON (debug=1) or OFF
                (debug=0). 

     If any of the above values are missing on the BOBS card, the following 
     default values are used:

     sigmax   = 0.0040 (i.e., 40 microns); 
     sigmay   = 0.0010 (i.e., 10 microns); 
     bom_only = 0 (i.e., use weighted average); 
     debug    = 0 (i.e., no debug printout). 

  ==>  Notes: 

        1 - if sigmax (or sigmay) is given a negative value on the BOBS card,
            then the LEP_BOM+QS0 information is not used for the x (or y)
            coordinate of the beam spot position. 

        2 - This new facility needs a bank, BLQP, which is not available for
            datasets produced with JULIA versions before 284. For these data
            sets, the BOBS card acts as do-nothing.

        3 - When using this new facility, to know the status of the ALPHA beam
            spot position one may call :

                       CALL QFILBP_STATUS(KFILBP) 

     KFILBP is an integer output argument with the following meaning: 

     KFILBP = 0 means that XGETBP=.FALSE.,
                           i.e. chunk-by-chunk VDET beam position not available
                           ---> QVTXBP(I)=QVTEBP(I)=QVTSBP(I)=0 for I=1,3
 
     KFILBP > 0 means that XGETBP=.TRUE.,
                           i.e. chunk-by-chunk VDET beam position     available
                           ---> QVTXBP(I) and QVTEBP(I), I=1,2 are filled with
                                VDET data or BOM+QS0 data or a weighted average
                                of both according to the table below

               KFILBP   I=1 (x)   I=2 (y)
               ------   -------   -------
                 0      -         -
                 1      VDET      VDET
                 2      BOM+QS0   VDET
                 3      VDET      BOM+QS0
                 4      BOM+QS0   BOM+QS0
                 5      Average   VDET
                 6      VDET      Average
                 7      Average   Average
 

      More details can be found if necessary in the following Web page:

       http://alephwww.cern.ch/LEP2ANALYSIS/HTF/code/instructions.html

===================================================

  Correction file 122.40 released 17 Nov 1997
  Modifications w.r.t ALPHA 122.36

 ** Bug fix in QFYLV0 to avoid infinite loops when using KSAME

====================================================

  Correction file 122.36 released 21 Oct 1997
  Modifications w.r.t correction file ALPHA 122.35

 ** New feature in the QFNDIP package, introduced by  N. Konstantinidis for   
    LEP 2 events:
   
   A new data card DWIN allows to control the use of the longitudinal
   component of tracks. 

   Syntax and usage of the data card:

 DWIN USE_LONG WINDOW
 
   If USE_LONG=1, use tracks with: Distance(track-jet) < WINDOW
   If USE_LONG=2, use tracks with: ABS(Distance(track-jet)) < WINDOW
 
   If DWIN is absent or USE_LONG=1 and WINDOW=0.0, the user gets 
   the standard QFNDIP results.
   
   Recommended values for LEP2:

   DWIN  1  0.035
 

 ** New fix in QFYLV0 (Routine which stores the so-called "Long V0s" from
    datasets processed with the new tracking, JULIA version > 300) :
    A bug in the mother-daughter relations was introduced 
    by the previous fix of version 122.35. Thanks to H. Seywerd, who found the 
    bug and checked the corrected version.

====================================================

  Correction file 122.35 released 16 Oct 1997
  Modifications w.r.t correction file ALPHA 122.34

  - Bug fix in QFYLV0 : the pointers to the daughter tracks of "Long V0s" 
    coming from the new tracking were not set correctly

  - Small fix in QNVSTO (internal routine to store new vertices from the
                         new tracking)
  
  - New user printout routine QWNEWTR to get details of the new vertices
    ("Long V0s", Kinks, Nuclear Interactions) from the new tracking.
    This routine is called automatically by QWEVNT but may be called 
    independently for a given event by the user :   CALL QWNEWTR
 
====================================================

  Correction file 122.34 released 08 Oct 1997
  Modifications w.r.t correction file ALPHA 122.33

  - Mod in XVDEOK to take into account a VDET HV bit change for 1997 data
  - Small fix in GETLE1 for LEP1 run-by-run exact energy 
  - Small fix in KNMCHC for the LINUX compiler

====================================================

  Correction file 122.33 released 26 Aug 1997
  Modifications w.r.t correction file ALPHA 122.31
   
  ** - Modification in QFMCPA to avoid  crashes in some ALPHA statement
       functions, due to very low energy photons with Px=Py=0. generated by   
       some 1997 Monte-Carlos.

  ** - New version of GETLUM put provisionnally in ALPHA (to be put in the
       ALEPHLIB in the future) to get the run luminosity from the LUMI bank
       if the luminosity in the RLUM bank is 0.

  ** - New routine QELEP1 to get the exact LEP energy for all EW/EA runs  
       of 1993,1994 and for the runs at the Z0 taken in 1995. 
       
       For more details, see the ALEPH note from Paul Bright-Thomas:
 
       "LEP Energies for 1993-1995", ALEPH 97/082, PHYSIC 97/072
       (11 August 1997).
  
 
      This routine has to be called by the user once per run:   

              CALL QELEP1(ENERGY,IFLAG)

       It gives 2 output arguments:

 
     IFLAG = 2     ENERGY (in GeV) is the exact energy of the current run. 
                 
     IFLAG = 3     ENERGY = QELEP (in GeV) for all other cases: runs which are
                   not good for the EW/EA groups, MCarlo runs, runs of 1989-92.

  The information for IFLAG = 2 has been taken from the official tables provided
  by the LEP experts. For the time being they are available from the temporary
  ALEPH database ADBSTEST. They will be put in the ADBSCONS.DAF in its next
  release. See the ALPHA user's guide p. 15-16 to get the ADBSTEST database.
        
     N.B.   This routine makes no sense for LEP2 runs. For LEP2 runs, please 
            use the routine QELEP2 described in the ALPHA user's guide p. 118.

====================================================

 Correction file 122.31 released 06 Aug 1997
  Modifications w.r.t correction file ALPHA 122.29
   
  ** - Fix in FIXGAEN to call the modification only when necessary

  ** - Fixes in ISP and QTFUNC (QTRUTH/QSELEP package) to avoid endless
       loops due to an  unforeseen feature for electrons coming from
       the conversion of ISR photons in HVFL05 MCarlo datasets. 

  ** - Fixes for multiple declarations of variables for the LINUX compiler
       in the KNTRU package introduced in version 122.30

