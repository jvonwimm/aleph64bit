C! 1st entry in V_set
 ! JULIA 285
   VDXYZT :     keep track of strips which belong to two clusters
                (after splitting) (A.Bonissent, Dec96)
   VUMCTR, VDMCTR, VDGTHT : handle properly strips which
                belong to two clusters (A.Bonissent, Dec96)
   VTRURL : New routine, gives truth MC relation between hits and tracks
            (A.Bonissent, Dec96). 
            IMPORTANT NOTE: VTRURL supersedes VTRUTH (changed calling sequence)
            VTRUTH is obsolete and will be removed in a future JULIA version.

 * corr file 284.1
    VDECOD.F : Do not check for cable swap in MonteCarlo (PBT, Nov96) 

 ! JULIA 284
    VINIRU.F : Request creation of VDZT banks during PASS0 if PVGG
               option being used. (I.Tomalin 09/10/96)
    VDRCUN.F : Ditto.             (I.Tomalin 09/10/96)

 ! JULIA 283
   Replace expicit BOS macro declaration by #include bmacrod.h (O.Schneider)
   Affects: vbmfit, vbsclr, vbsinr, vbspot, vcbfit, vdosmr

 * corr file 281.2
     - VDMCEF : move the treatment of the new VDET to VDKLCP. It can not be 
                done in VDMCEF because of multiplexing. (M.Thulasidas)
     - VDKLCP : New routine to take care of the efficiency map for the new
                VDET, making sure that the multiplexed hits get killed
                together. (M.Thulasidas)

 ! JULIA 281
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

 ! JULIA 280
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

 ! JULIA 279
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

 ! JULIA 278
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

 ! JULIA 277
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

   VDLINK : Call VDMCTR to compress Monte Carlo truth info
           (A.Bonissent, Mar 95)
   VDMCTR,VTRUTH,VUMCTR : Compress Monte Carlo truth info
           (A.Bonissent, Mar 95)
   VDALIN,VDALXY,VDALZT,VDALSG,VDGTHT,VDGTMV,VDGTXL,VDOSMR,VDRNDM,
   VDSIGM,VDTAN,VDWSDX,VDWSMR,VDGAUS : New treatment of alignment
   errors for 1994 Monte Carlo production (M.Thulasiadas, Mar 95).
   VDMCEF : Take out position smearing (M.Thulasiadas, Mar 95).
   VPREDA : Call VDISMR for smearing (M.Thulasiadas, Mar 95).
   VDISMR : Set up appropriate smearing scheme (M.Thulasiadas, Mar 95).

 * corr file 275.3
   VBSPOT : avoid taus to measure the beam spot (S.Wasserbaech).
 ! JULIA 275
 * corr file 274.3
   VBSINR : create working bank only once (E.Lancon).
 ! JULIA 274
 ! JULIA 273
 * corr file 272.4
   VDMCEF : remove double correction which created wrong ALEPH
            Z-coordinate pertubating DALI, the local Z-coordinate
            was correct (M.Thulasidas).
 ! JULIA 272
 * corr file 271.6
   VBSINR : do not create a new working bank for each run.
 * corr file 271.1
   VDMCDEF : call rannor instead of norran


