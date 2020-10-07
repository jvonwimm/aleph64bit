C! 1st entry in R_set
 * corr file 308.6
    RECEVT : Remove TPC hit smearing (A.Bonissent)

 * corr file 308.3
    RDEFIO : New bank BCPO to POT, remove BLCT (can be made from BCPO)
                                                      (M.Cattaneo,P.Morawitz)
    RECEVT : Replace call to BCAREC (obsolete) with call to BCALJU (M.Cattaneo)

 * corr file 308.1
    RECEVT : Bug fix: VDYEAR was declared as LOGICAL (A.Waananen)

 ! JULIA 308
    RLOOPR : Modify format statement (M.Cattaneo)

 * corr file 307.4
    RLOOPR : Print also the processing time of the previous event (M.Cattaneo)

 * corr file 307.3
    RECEVT : Change defaults for VGLOB. If VDYEAR < 95, VGLOB is
             default, otherwise not. Steering card VGLO forces VGLOB on
             NVGL forces VGLOB off                             (M.Cattaneo)

 * corr file 306.3
    RDEFIO : New bank BCHG to POT     (M.Cattaneo)

 * corr file 306.2
    RDEFIO : Banks FSHO, KMAR, KWTK to POT (M.Cattaneo)

 ! JULIA 306
    RECEVT : Move TPC hit smearing to after track fit and
             refit tracks with smeared hits (MonteCarlo only)  (A.Bonissent)

 * corr file 305.2
    RECEVT : Add TPC hit smearing (M.Thulasidas)

 * corr file 305.1
    RLOOPR : Fix format statement when event length > 1Mbyte (M.Cattaneo)
    RREVHE : Use xlumok from Alephlib (version>306)          (B.Bloch)

 ! JULIA 305
    RDEFIO : New bank BLCT to POT     (M.Cattaneo)
    RECEVT : Call BCAL reconstruction (M.Cattaneo)
             Create track hit bit patters for secondary vtx. search (D.Casper)
    RLOOPR,RPRSUM,RREVEH,RUNKNO,RWREVT: fix variable type 
             inconsistencies in function calls, for Linux         (A.Waananen)
               
 * corr file 304.1
    RTLSUM,RWRUNH:Remove CALL from function references, for Linux. (A.Waananen)

 ! JULIA 304
    RDEFIO : Add YSMO to POT                      (D.Casper)
    RFORMA : Add YSMO, correct YKNK,YSVT formats  (D.Casper)

 * corr file 303.5
    RINCND : Protect against zero magnetic field: Set FIELRC to 0.0001
             if ALFIEL returns 0., produce a "fatal" error        (M.Cattaneo)

 * corr file 303.2
    RECEVT : Bug fix - RDMIN was called with SEED=0. every time!  (M.Cattaneo)
 
 * corr file 303.1
    RECEVT : Add call to RDMIN to initialise RANNOR (and RNDM) random
             numbers once per event.                              (F.Ranjard)

 ! JULIA 303
    RCPAS0, RIPAS0 : If PVGG card present, PASS0 drift velocity is taken
             from VDET/gamma-gamma for all runs taken with new VDET 
             (and not just LEP2 runs)                              (I.Tomalin)
    RDEFIO : Add new banks BCSC,BCSL,BCTR to output list          (M.Cattaneo)
    RDETEC : In PASS0 mode, force YNSV,YNLV,YNKF,TNPX,ENKF,VNSL   (M.Cattaneo)
             In PASS0 mode, switch on use of TPC wires by default (I.Tomalin) 
    RECEVT : Call VDET global pattern recognition if VGLO card found (D.Casper)
             Moved call to TSUMVD from RECONS to RECEVT           (I.Tomalin)
    RECONS : Moved call to TSUMVD from RECONS to RECEVT           (I.Tomalin)


 * corr file 302.5
    RVDPRE : Protect against garbage collection in called routines (F.Ranjard)

 * corr file 302.2
    RDEFIO : Add KXME (Kingal output) to POT      (B.Bloch)
    RFORMA : Add KXME format                      (M.Cattaneo)

 * corr file 302.1
    RDEFIO : Add LCRA (LCAL scintillator raw data) to POT      (M.Cattaneo)
    RFORMA : Add LCRA, VDTD formats                            (M.Cattaneo)

 ! JULIA 302 (Tracking upgrade)
    RDEFIO : Tracking upgrade banks added to output list       (D.Casper)
    RECEVT : Call new modules VTRPRE, KINKS, NUCVTX (M.Cattaneo,D.Casper)
    RFORMA : Add PFXT,TRIK,TWZA,TWZZ,VGHC,VGXC,YKNK,YLV0,YNLI,
             YNVH,YS0L,YSTL,YSTC,YSTV,YSVT,YSVX,YSCL formats   (D.Casper)
    RVDPRE : New. Steer VDET track preselection                (M.Cattaneo)
    RPARAC.H Added modules VTRPRE, KINKS, NUCVTX               (M.Cattaneo)
    
 * corr file 285.1
    RDEFIO : Add PHMH to output list (P.Bright-Thomas 4/3/97)
                 
 ! JULIA 285
    RFORMA : Add VSPL bank (A.Bonissent, Dec96)

 * corr file 284.1
    RCPAS0 : ALLEP1 declared as logical, not integer (A.Waanenen 16/10/96)
    RIPAS0 : ALLEP1 declared as logical, not integer (A.Waanenen 16/10/96)

 ! JULIA 284
    RIPAS0 : Detects if PVGG card is being used. (I.Tomalin 9/10/96)
    RCPAS0 : Fill's PASS0 banks with drift velocity from VDET + 
             gamma-gamma events if requested by PVGG card.(I.Tomalin 9/10/96)

 * corr file 283.1
   REPORT : Change message for non-fatal errors (M.Cattaneo 7 October 1996)

 ! JULIA 282
   RLUNIT.H : Increase size of output bank list to 200 banks 
              (M.Cattaneo, July 96, requested by D.Casper)
   RCLRUN : Add call to OLSPOT (M.Cattaneo, June 1996)
   RECONS : Add call to OLSPOT (M.Cattaneo, June 1996)
   RFORMA : Add format for BLQP bank (M.Cattaneo, June 1996)
   RINJOB : Add BLQP to SUMLIS (M.Cattaneo, June 1996)
   RLTOCH : Replace LENOCC by LNBLNK (M.Cattaneo, July 1996)
   RSMJOB : Add call to OLSPOT (M.Cattaneo, June 1996)
   RUNKNO : Add call to OLSPOT (M.Cattaneo, June 1996)

 * corr file 281.2
   RERROR : Fix bug: error was not reported on autoinitialising first call 
            (M.Cattaneo, June 96)

 ! JULIA 281
   RWRUNH : RUNE added to the C list (P.Comas, Mar 96) 

 ! JULIA 280
   RINCND,RIPAS0,RMONII : opening "'" should have a closing "'" within
           the same line for cvs (F. Ranjard, Feb 96)
   RINJOB : change LOUTRL to IW(6) instead of 6 (P. Comas, Feb 96)

 * corr file 279.2
   AAMAIN : double size of BOS array from 1 to 2 Mw (O.Callot, Dec 95)
   RDETEC : set time measurement flags: for ITC, presence of ITTI
           (O.Callot, Dec 95)
   RNXMOD : call ALTIME only if MXTI card used (O.Callot, Dec 95)
   RSHRIN : avoid use of LENOCC (O.Callot, Dec 95)
 ! JULIA 279
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
 ! JULIA 278
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
 ! JULIA 277
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
 ! JULIA 276
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
 ! JULIA 275
   RFORMA : format of new Ecal banks EAGC, PGAC (M.N. Minard, Oct 1994)

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
 * corr file 274.1
   RFJCON : remove call to TDEDXV. Fill JCON bank with /TMONIT/GAINTM.
 ! JULIA 274
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
 ! JULIA 273
   RFORMA,RINJOB: remove XSGE, XSHI, XSSC trigger banks.
   RINRUN: remove call to XTRSIN.
   RPOTCL: remove call to XTRSEV.
   RCLRUN: remove call to XTRSFI.
   RDEFIO: add new trigger banks to output list
   RLOOPR: call XTCNCR before RECONS to create XTCN bank from
           X1RG bank.

 * corr file 272.1
   RDEFIO : write VDTD on POT as it was in previous versions.
 ! JULIA 272

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
