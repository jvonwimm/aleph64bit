
========================================================================
                             ALPHA 124 News
                        Last Update: 14 January  2000
========================================================================

  ALPHA version 124 first release on 22 March 1999

  released as default ALPHA version on 16 April 1999 

  All features of the successive correction files are summarized below ,
  starting from the most recent one. 

  For previous corrections, refer to the ALPHA 123 News file

=================================================

 Correction file 124.19 released 14 January 2000
 Modifications w.r.t. ALPHA 124.18 :
                                 
 ** Mod in QMINIT to abort ALPHA if one tries to use ALPHA124 with
    the database ADBSCONS 300 (needs new ALEPHLIB version)


=================================================

 Correction file 124.18 released 28 October 1999
 Modifications w.r.t. ALPHA 124.17 :

 ** Fix two Y2K problems in the ENFLW routines buildal and enfjet
    (code provided by P. Janot)

 ** Fix an unwanted printout for every event in gtipbk (QIPBTAG package)  


=================================================

 Correction file 124.17 released 20 September 1999
 Modifications w.r.t. ALPHA 124.16 :

 ** Modifications in qpcorr.F , from G. Dissertori
    (new printouts and preparation for use with 
     MCarlo datasets when the modifications to the
     ALEPHLIB routine ppcorr.F will be available) 
    
=================================================

 Correction file 124.16 released 7 September 1999
 Modifications w.r.t. ALPHA 124.15 :
                                  
 ** Modifications for the Linux compiler 
    in qipbtag.F  

=================================================

 Correction file 124.15 released 26 August 1999
 Modifications w.r.t. ALPHA 124.13 :
                                  
 ** Modifications to avoid possible Y2K-type problems
    in /pack/fixprl, /pack/fixrtrl, /qlep/gtyear, /nano/mdipbt

=================================================

 Correction file 124.13 released 25 August 1999
 Modifications w.r.t. ALPHA 124.12 :

 ** Small fix in QFPTPX ( wrong printout for 1999 data) 

 ** Modification in the QIPBTAG package to allow the use of QSMEAR
    (see below) : code provided by P-F Giraud and I. Tomalin 
    mods in QIPBTAG,BTAGINIT.BTAGPARNEW1,BTAGPARNEW2,FITDMIN,GTIPBK

 ** New user's routine QSMEAR, to be used after a call to QIPBTAG:

    QIPBTAG Track smearing & deletion :

      CALL QSMEAR(FIRST,QIPVER,SMBIN,RTBIN,
     >     NTRACK,NJET,FRF2TRK,PROBEVT,PROBHEMI,PROBJET,PROBTRK)

      QSMEAR is able to smear the three dimensional impact parameter
      significance and delete extra tracks in the Monte Carlo. It includes
      options for doing this work dependently on the polar angle and
      momentum of the tracks.

   Input Arguments :
        LOGICAL FIRST   = Is this the first call to QSMEAR since
                          calling QIPBTAG (or QIPBTAG2) ?
         CHAR*1 QIPVER  = '1' if being used after call to QIPBTAG.
                        = '2' if being used after call to QIPBTAG2.
         CHAR*1 SMBIN   = '0' no smearing correction at all.
                        = 'N' no binning in theta/p for smearing.
                        = 'T' binning in theta for smearing.
                        = 'P' binning in p for smearing.
                        = 'B' binning in both theta/p for smearing.
         CHAR*1 RTBIN   = '0' no track deletion at all.
                        = 'N' no binning in theta/p for deletion.
                        = 'T' binning in theta for deletion.
                        = 'P' binning in p for deletion.
                        = 'B' binning in both theta/p for deletion.
           Plus same arguments of call to QIPBTAG.

   Output Arguments: 
           Same arguments overwritten with smeared values
           The -ve tag quantities in /BTAGRAW/ are also smeared.

      It is possible to smear Monte Carlo from a different year than the
      data by including the keyword QPYR in the alpha card file with an 
      argument YYMM:

      eg : QPYR 9701

      It is also possible to use private smearing/deletion cards : beware
      the bank number should then be -1. (See the package SMEARFIT on UPHY)
    

=================================================

 Correction file 124.12 released 16 June 1999
 Modifications w.r.t. ALPHA 124.11 :

 ** Mod in qfgetbp.F to allow the smearing of the
    X,Y beam position uncertainties (in Monte Carlo 
    datasets only) according to a new data card:
    
    SIBE  mean x   sigma x   mean y  sigma y 
    (units = 10**-3 cm)

    Code provided by Anne Ealet

=================================================

 Correction file 124.11 released 3 June 1999
 Modifications w.r.t. ALPHA 124.10 :

 ** Small mod in gamixed.F to inhibit the execution
    of the SICAL cleaning routine SICLID if a data card 
    NSID  is provided (for studies on old data)
 

=================================================

 Correction file 124.10 released 27 May 1999
 Modifications w.r.t. ALPHA 124.09 :

 ** Small mod in minbld.F for SIID bank on MINIs

=================================================

  Correction file 124.09 released 26 May  1999
  Modifications w.r.t ALPHA 124.08 :

  ** Mod in the Energy Flow routine gamixed.F to use the SICAL cleaning
     routine SICLID - correction provided by P. Janot.


=================================================

  Correction file 124.08 released 17 May  1999
  Modifications w.r.t ALPHA 124.07 :

  
 ** Small fixes in HCALENSC and QCORFIC (HCAL online bug correction
    routines introduced in ALPHA124.07) :

    - called only for real data POTs/DSTs
    - now HCALENSC corrects also the so-called "offline bug" 
      in the calibration. 


=================================================

  Correction file 124.07 released 12 May  1999
  Modifications w.r.t ALPHA 124.06 :

  ** New SICAL routines SICLID,SIBKCL for SICAL cluster identification
     and flagging

  ** HCAL online bug correction routine HCALENSC systematically called
     in QFILL (useful for 1996 data and more generally for all data
     processed with JULIA versions before 307). 
  
=================================================

  Correction file 124.06 released 28 April 1999
  Modifications w.r.t ALPHA 124.05 :
  
  ** Suppression of obsolete routines btagpar,btagpar96,fitdmin96
     from the libraries on UNIX

  ** Explicit declaration of variables in common gampi0 (which
     transmits the resultsof the qpi0do package), for people
     using IMPLICIT NONE in their program

  ** Call to btag_par suppressed in MDSTWR (ENFLW package) and MDIPBT
     (NANO package), since this routine is obsolete and the QIPBTAG 
     initialisation is done internally now.
 
  ** SIID bank added to the list of banks transmitted to the MINI
     (mod in MINBLM).

  ** Small fix of declaration of variables in QDDX

=================================================

  Correction file 124.05 released 19 April 1999
  Modifications w.r.t ALPHA 124.04 :

  >>> Two new analysis tools : QDDX and QBMTAG <<<

  >>> IMPORTANT modifications to QIPBTAG <<< 

  *******     Please read details below  *******


  1 - New analysis tool in ALPHA : QDDX  - New code from Tommaso BOCCALI
      ==================================================================

      Combined estimation for dE/dx, using TPC Wires and Pads
      informations.

      The final calibration of the pad dE/dx is still underway,
      but the change in the performance is expected not to be dramatic.
  
      Calling sequence :  

      call  qddx(itk, nhyp, rmass, Q, ns,npad,extimator, ier)

      INPUT arguments :
        itk         : ALPHA TRACK
        nhyp        : Number of hypotheses to be tested
        rmass       : vector(1..nhyp) of the masses of the tests
        Q           : vector(1..nhyp) of the charge of the tests
                     
      rmass and Q must be correctly dimensioned in the calling routine

      OUTPUT arguments :
        ns          : number of wires available, -1 if no wires info
        npad        : number of pads (REAL!!!!), -1 if no pad info
        extimator   : (ri-rexp)/sigma for each test particle
        ier         : 1 if no information was available
                    : 0 if an estimator was calculated
                
           rmass, Q and extimator must be correctly dimensioned
           in the calling routine.


      For questions concerning the code or the results of the routine, 
      please send an e-mail to :
             Tommaso.Boccali@cern.ch or Duccio.Abbaneo@cern.ch    


 2 - New tool in ALPHA : QBMTAG - Code from Ian TOMALIN, was in UPHY
     ===============================================================

     2.1 Warning to the users :
         --------------------

       The routine QBMTAG, which can be used in conjunction with QIPBTAG
       to provide extremely pure (99%) b hemisphere tagging, is now        
       available in ALPHA124. It was available since a long time on UPHY.

       The ALPHA124 version of QBMTAG gives identical performance to the
       UPHY one. 
       However, in addition to providing  a btag for each event 
       hemisphere, it now also does so for each jet in the event.

 ===>  The calling arguments of QBMTAG have been CHANGED to output
 ===>  this extra information, so you will need to change your call 
 ===>  if you used the UPHY version.


     2.2 Description :
         -----------
     Invariant mass b tag.

     Can be called after QIPBTAG or QVSRCH, to further increase b purity.

     For each event-half QBMTAG loops over tracks in order of decreasing
     inconsistency with primary vertex, and stops when their combined
     invariant mass exceeds 1.8 GeV. (i.e. The mass of a typical charmed
     hadron). It then returns a tag, CLMASS, indicating the probablity 
     that the "last track" used came from primary vertex.
     For a very high purity tag, CLMASS should be combined with the
     QIPTBAG or QVSRCH tags. The optimal linear combination (together 
     with suggested cuts) is
          0.7*CLMASS - 0.3*log10(QIPBTAG) > 2.4
     or   0.7*CLMASS + 0.3*QVSRCH > 8.3
  
     2.3 Calling sequence : 
         ----------------

      CALL QBMTAG (METHOD,NTRACK,NJET,TRKJET,PROBTRK,FRF2TRK,
     +             PVTX,SVTX,DJET,
     +             CLMASS,CTSTAR,NSEC,CLMASJ,CTSTAJ,NSECJ,
     +             LISTTRK,LISTEH,LISTJET)
  
      2.3.1. Input Arguments:

        INTE METHOD    : =1 if using QIPBTAG, =2 if using QVSRCH.
      Following only needed if METHOD=1 (dummy otherwise):
        INTE NTRACK    : Number of tracks used by QIPBTAG.
        INTE NJET      : Number of jets considered by QIPBTAG.
        INTE TRKJET(*) : ALPHA track number of QIPBTAG jets.
        REAL PROBTRK(*): QIPBTAG track probabilities.
        INTE FRF2TRK(*): Row in FRFT bank of QIPBTAG tracks.
      Following only needed if METHOD=2 (dummy otherwise):
        REAL PVTX(3)   : Position of primary vertex from QVSRCH.
        REAL SVTX(3,2) : Positions of two secondary vertices in jet
                         coordinate system from QVSRCH.
        REAL DJET(3,2) : Unitized jet directions from QVSRCH.
        
      2.3.2 Output Arguments: 
            (Dimensioned 2 to correspond to two event-halves):

        REAL CLMASS(2) : For METHOD=1, = -log10(confidence level) for
                     "last track" to come from primary vertex
                     (c.l. in range 0-1).
                     For METHOD=2, = QSQT(CHI**2 (primary) minus
                     CHI**2 (secondary)) for "last track".
        REAL CTSTAR(2) : Decay angle of "last track" in "b" rest frame.
        INTE NSEC(2)   : Number of tracks used to reach 1.8 GeV mass.
                     (zero if couldn't reach 1.8 GeV).

      Following jet info available only if METHOD=1:
        REAL CLMASJ(*) : Same variables for jets (ordered as in TRKJET)
        REAL CLSTAJ(*) :            "
        INTE NSECJ(*)  :            "

        INTE LISTTRK(*): List of all ALPHA charged tracks and V0s which
                     were considered by QBMTAG.
                     If the track is neutral, it is a V0. If a V0
                     is not in the range KFV0T-KLV0T, then it has
                     been created using YTOP, and its daughters
                     are (refitted) copies of tracks from the
                     KFCHT-KLCHT section.
        INTE LISTEH(*) : If track comtributed to 1.8 GeV hemisphere mass 
                     then =1 or =2 to show which hemisphere it's in.
                     If =0, then track was rejected.
        INTE LISTJET(*): If track comtributed to 1.8 GeV jet mass,
                     then =ALPHA track number of jet.
                     If =0, then track was rejected.

      2.4  Remarks :  
           ---------

      IF METHOD=1, then one can obtain higher purities by using the
      BNEG card. This allows QIPBTAG and QBMTAG to use information from
      both +ve and -ve impact parameter tracks. (See ALNEWS 1389 in
      OFFLINE folder). If one omits this card, however, then in the same
      way as one can measure the uds efficiency of QIPBTAG using the -ve
      hemisphere tag NPROBHEMI, it is possible to create a -ve tag from
      QBMTAG for the same purpose. This can be done by calling it with
      argument NPROBTRK instead of PROBTRK.
 

      Please send all remarks concerning this package to : 
      Ian.Tomalin@cern.ch

 
 3 - Bug fix in TRSEL (in the QIPBTAG package) : thanks to David Smith
     ===========================================                   

     This bug affected the definition of the d0,z0 cuts made in the 
     track selection routine TRSEL. Was not present in ALPHA122.
     Seems to have no effect when using standard QIPBTAG set-up 
     (to be confirmed).
    
     This fix is independent of the modifications to QIPBTAG described
     below.


 4 - Modifications in the QIPBTAG package, from Ian TOMALIN :
     ========================================================           

     QIPBTAG has been modified at the request the heavy flavour group.
 
     4.1 Does this affect you ?
         ----------------------

 These mods. will not affect you unless:

     1) You are running QIPBTAG on LEP1 data and have an NQIP card,
 or  2) You are running QIPBTAG on reprocessed LEP1 data,
 or  3) You have private versions of some QIPBTAG routines.
  
 If one of these three cases applies to you, then to continue obtaining 
 the same results as before you must:

 For case (1):
   The new code is not backwards compatible. Therefore, continue using
   ALPHA123 and adbs246.daf. Also please contact Ian Tomalin.

 For case (2):
   Include an OQIP card in your card file (which you can already do now).

 For case (3):
   You can continue using ALPHA123. Alternatively, before changing to
   ALPHA124, you will need to update any private versions you may have
   of the following routines :
     btagfit.F, btagpar.F, btagpar96.F, codsel.F, fitdmin.F, gtipbk.F,
     qipbtag.F 
   Note also that the QIPBTAG include file btpar.h has changed,
     so you must recompile any code using this.


     4.2 Full list of changes made to QIPBTAG :
         ------------------------------------

      4.2.1 Information :

     (i) Throughout this NEWS, "LEP1" refers to data taken with the
         old VDET and "LEP2" to data taken with the new VDET.

    (ii) This NEWS refers often to the "VDET96" option, which was made
         available inside QIPBTAG a few years ago. In ALPHA123, this
         option was already used by default at LEP2, and could be
         obtained at LEP1 with the NQIP card (see ALPHA manual).

         Compared with the original version of QIPBTAG, the "VDET96"
         option introduced the following changes:
         - (a) Tracks are classified not only according to the number
               of hits they have in the VDET, but also according to
               the number of hits they were expected to have.
         - (b) The cuts used for selecting the tracks used
               by QIPBTAG were retuned.
         - (c) The BNEG option (see ALPHA manual) 
               was switched on by default.
   
      4.2.2 Changes in ALPHA124:
            --------------------

1) By default, the "VDET96" option will be switched on if you run on
   LEP1 data/MC reprocessed with the tracking upgrade JULIA. It will
   still be off by default on old LEP1 data.

2) The defaults can be overridden with cards:
           NQIP    - Switch VDET96 option on
           OQIP    - Switch VDET96 option off
           BNEG    - force BNEG option on
           NOBG    - force BNEG option off (even if VDET96 option is on).

3) The modified QIPBTAG track selection cuts used by the VDET96 option
   in ALPHA123 were tuned on LEP2 data. They do not work well at LEP1.
   Therefore, in ALPHA124, if you are running on LEP1 data, the VDET96
   option will use the original (Dave Brown) track selection cuts.

4) The QIPC database bank which parametrizes the impact parameter 
   resolution at LEP1, has been recalibrated for the new LEP1 VDET96
    option.

5) One cosmetic change: When doing calibrations, the calibration
   histograms were numbered 151-157 which put them in the middle 
   of the user's own histograms. 
   In ALPHA124, they have therefore been offset by 100000.
   Furthermore, this offset can be changed with the CALB card.
 
6) At the beginning of your ALPHA job, QIPBTAG now prints out slightly
   more information explaining which options it is using.

7) The VDET96 option was introduced a few years ago by duplicating
   a few QIPBTAG subroutines. This was unnecessary and makes the 
   QIPBTAG code difficult to maintain. Therefore in ALPHA124:

    a) fitdmin.F and fitdmin96.F have been merged back into a single
       routine.

    b) 75% of btagpar.F and btagpar96.F were in common. Therefore 
       their common code has been extracted an put into a single
       routine btaginit.F
    
     4.3 Performance :
         -------------

 On reprocessed 1994 MC, the performance of QIPBTAG in ALPHA124 
 and ALPHA123 has been compared compared for two sets of cuts:

         a) TIGHT cuts: QIPBTAG(hemi) < 1.0e-4 for events with
                        cos(theta_thrust) < 0.7

         b) LOOSE cuts: QIPBTAG(hemi) < 1.0e-2 for events with
                        cos(theta_thrust) < 0.8

 Hemisphere b, c, and uds efficiencies obtained are as follows:

                           TIGHT cuts
                           ===========

                                epsilon_b    epsilon_c    epsilon_uds
  ALPHA124                        30.0%        1.17%         0.05%
  ALPHA123 (BNEG card)            29.9%        1.10%         0.05%
  ALPHA123 (NQIP card)            30.1%        1.17%         0.08%

                            LOOSE cuts
                            ==========

                                epsilon_b    epsilon_c    epsilon_uds
  ALPHA124                        55.9%        10.2%         0.92%
  ALPHA123 (BNEG card)            55.6%        10.1%         0.95%
  ALPHA123 (NQIP card)            55.8%        10.2%         1.29%

 Please note the following:

 i) The NQIP card used with ALPHA123 switches on the VDET96 option.
    This gives rather poor performance at LEP1 because the 
    QIPBTAG VDET96 track selection cuts were tuned at LEP2.
    (See (3) in 4.2.2 above).

 ii) As ALPHA124 and ALPHA123 (NQIP) both switch on the BNEG option 
     by default, the comparison with the old "Dave Brown" QIPBTAG 
     in ALPHA123 was done using the BNEG card.

 iii) ALPHA124 and ALPHA123 (BNEG) give almost identical performances.

 iv)  The cuts used for these tests were not exactly equal to those 
      quoted above, but were varied slightly so that all 
      QIPBTAG versions gave roughly the same b tagging efficiency.


            Please send any remark on this new QIPBTAG code to :

                   Ian.Tomalin@cern.ch


=================================================

  Correction file 124.04 released 16 April 1999
  Modifications w.r.t ALPHA 124.03

  **  Small FORMAT modifications in MINDTYP,MINBLD,MINRUN to satisfy 
      the cpp preprocessor on ALWS 

==================================================

  Correction file 124.03 released 15 April 1999

  ===>  becomes official ALPHA version

  Modifications w.r.t. alpha 124.02

  **  Modification in MINDTYP to setup as ``Lep2-style" 
      the MINIS of 1995 LEP 1.5 period

  **  Bug fix in QTRESC : call QPCORR as a Function , not as a 
      Subroutine (caused crashes on LINUX, fix provided by A. Waananen). 

==================================================

  Correction file 124.02 released 25 March 1999
  Modifications w.r.t ALPHA 124.01

  **  Small fixes in MINRUN,MINBLD,MINEVT

==================================================

  Correction file 124.01 released 22 March 1999
  Modifications w.r.t ALPHA 123.12

  **  ALPHA version containing the MINI as subset
      90 MINI subroutines in /alpha/mini subdirectory 
      5  MINI commons     in /alpha/inc  subdirectory

  **  New routines to make the ALPHA interface with the MINI:
      MQUINIT,MQUNEWR,MQUEVNT,MQUSREC,MQUTERM

  **  Corrections to call the above interface routines
      in the ALPHA code : 
      in QMINIT,QMNEWR,QMEVNT,QMSREC,QMTERM      
 
  **  Mods in QMININC to define the MINI official production flag
      IMIPRD (in common minprd.h)
