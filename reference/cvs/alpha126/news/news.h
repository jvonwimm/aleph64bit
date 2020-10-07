
========================================================================
                             ALPHA 126 News
                        Last Update: 10 September 2003  
========================================================================

  ALPHA version 126 first release on 21 November 2000

  All features of the successive correction files are summarized below ,
  starting from the most recent one. 

  For previous corrections, refer to the ALPHA 125 News file

=================================================
 Correction file 126.22 released 10 September 2003
 Modification w.r.t. ALPHA 126.21 :

 * in QUIBOS increase LQBOS from 2.5M to 5M to avoid too frequent
   abandons in QVEC/QLIN extensions for sophisticated analyses

 * change a print format in QRKSEC to avoid **** in the printout
   of some gamma-gamma generator cross-sections

 * provisionally: add a modified version of ALEPHLIB routine GETLE2
   to avoid very rare crashes for some weird configurations 

=================================================
 Correction file 126.21 released 12 March 2003
 Modification w.r.t. ALPHA 126.20 :

 * in mcef.h increase MXMTCH from 10 to 30 to avoid too frequent
   abandons in the matching routines KNTRU,KTRU,QSTRU 
   for simulated WW events
   --> recompilation of the 20 routines of this matching package

=================================================
 Correction file 126.20 released 17 January 2003
 Modification w.r.t. ALPHA 126.19 :

 * in CREFLW fix a small bug introduced in the previous modif 
   of ALPHA126.19


=================================================
 Correction file 126.19 released 17 January 2003
 Modification w.r.t. ALPHA 126.18 :

 * in CREFLW when reading MINIs to replay the Energy Flow: 
   if PECO and PEST banks are absent, 
   create empty PECO 0  PECO 1 and PEST banks
   to avoid problems in rare cases (Mod. provided by B. Bloch)

=================================================
 Correction file 126.18 released 11 October 2002
 Modification w.r.t. ALPHA 126.17 :

 * Fix in QRKSEC for Kingal code 5030 --> 5300
 * Suppress routines SICLID and SIBKCL from /util:
   they are also in ALEPHLIB

=================================================
 Correction file 126.17 released 24 September 2002
 Modification w.r.t. ALPHA 126.16 :

 * Fix in QRKSEC when all RKSE banks are on ADBSCONS.DAF

=================================================
 Correction file 126.16 released 15 March 2002
 Modification w.r.t. ALPHA 126.15 :

 * Mod in FIXPRL to apply the correction to the RL electron estimator
   uniformly to all MCarlo data (all years, all energies)

=================================================

 Correction file 126.15 released 30 January 2002
 Modifications w.r.t. ALPHA 126.14 :

 - Fix in QRKSEC for datasets having same Ranmars
 
=================================================

 Correction file 126.14 released  13 December 2001
 Modifications w.r.t. ALPHA 126.13 :

 - drop the PGAC bank in QFILL instead of QFGAMP if DRGA card present
 - allow to redo PGAC for MINIs > = 205 (mod in QFGAMP)


=================================================

 Correction file 126.13 released  10 September 2001
 Modifications w.r.t. ALPHA 126.12 :

 - put some local vars. in double precision in QETHPH to avoid
   dividing by zero in some very rare MCarlo events.

=================================================

 Correction file 126.12 released  26 July  2001
 Modifications w.r.t. ALPHA 126.11 :

 - replace a TAB character by spaces in FIXPRL to avoid VMS compilation
   errors

=================================================

 Correction file 126.11 released  25 July  2001
 Modifications w.r.t. ALPHA 126.10 :

** In FIXPRL: modifs. from I. Debonis to get a centered RL estimator 
              for Year 2000 Monte Carlo datasets, using the data card UPRL 

=================================================

 Correction file 126.10 released  21 June  2001
 Modifications w.r.t. ALPHA 126.09 :

** In QMINIT: call ADBCHK  for MINI MCarlo official productions 
** In QMRDSB: call ALEVNUM for MINI MCarlo official productions

=================================================

 Correction file 126.09 released  6 June  2001
 Modifications w.r.t. ALPHA 126.08 :

** Mod in qrksec: adaptation to the new structure of the 'RKSE' bank

=================================================

 Correction file 126.08 released  15 March  2001
 Modifications w.r.t. ALPHA 126.06 :

** Bug fix in qfkirev: protection against too many FILI cards
   + increase size of arrays in comdeck krsktrg.h

=================================================

 Correction file 126.06 released  5 March  2001
 Modifications w.r.t. ALPHA 126.05 :

** Protection added in ndigh_e14 (E14 algorithm) which crashed for
   one event in the Year 2000 data

=================================================

 Correction file 126.05 released  6 Feb  2001
 Modifications w.r.t. ALPHA 126.03 :

 ** New MINI version and structure : MINI 205 , allowing 
     * to fully redo the Energy Flow on the MINIs
     * to have all muon identification information for all tracks
     * to have all V0s on the MINIs, not only the first 30 ones  
     * to write the new KINGAL KW4F bank on MINIS

    - Mods in the MINI-writing package: MINBLN,MINBLO,MINMUI,MINTRA,
                                        MINFVB,MINSUM,MINMSC,MINVSN
    - Mods in the MINI-reading package: MINFIL,MINFRF,MINPSC,MINYV0 
                                        new routine MINPFR        
    - Mods in /pack: QFILL,QFCHGD,QFFRFT
    - mods in /enfl: ENFLW                                      

 ** New facility: printout of statistics of KINGAL generated triggers 
    at the end of a job, and user's routine to access these quantities:

    - new internal routines in alpha/pack: QFKIREV,QTKIREV
    - new user-callable routine QUKISTAT

      Usage:

      call qukistat(nfili,tfilis,lirun,nktrg,ifail)

C-----------------------------------------------------------------------
C  user access to  KINGAL statistics on generated triggers 
C  at the end of job
C To be called in QUTERM - Meaningful for MCarlos only !
C Output arguments:
C
C  nfili     = number of FILI CARDS read during the job
C  tfilis(i) = list of tapes_files read during the job, e.g. R01234_125
C              i = 1 , nfili
C              must be declared character*12 tfilis(100) in the
C              calling subroutine
C  lirun(i)  = list of runs read during the job (Integer lirun(100))
C  nktrg(i)  = number of generated KINGAL trigger for dataset read on
C              tfilis(i)   - To be declared as Integer nktrg(100)
C  ifail     = 1 if more than 100 FILI cards read on input

=================================================

 Correction file 126.03 released 20 Dec  2000
 Modifications w.r.t. ALPHA 126.02 :

 ** Bug fix (typo) in QRKSEC (caused a crash on Linux machines only)

=================================================

 Correction file 126.02 released 12 Dec  2000
 Modifications w.r.t. ALPHA 126.01 :

 ** Mods in QDATA,QMNEWR,QRKSEC for the new structure of
            the RKSE bank

 ** New user's routine QURKSEC to access MCarlo's generated
    cross sections.

 Usage:

     CALL QURKSEC(XSEC,XSECE,NEVPR,XPROD,XPRODE)

 Output arguments:
  xsec   = cross section (in pb) generated for the present run
         = -1. if not found
  xsece  = error on xsecru (in pb) , =-1. if not found
  nevpr  = total number of events for the whole prouction
         = 0 if not found
  xprod  = cross section (in pb) for the whole production
         = -1. if not found
  xprode = error on xsecpr (in pb), =-1. if not found


=================================================

 Correction file 126.01 released 21 Nov  2000
 Modifications w.r.t. ALPHA 125.17 :

 ** Fixes for precision problems, from S. Jezequel, in
    routines V0DKPT.F (QFN) and XXP0FL (QPi0) 

 ** New routine QRKSEC.F to give the MCarlo generated 
    cross-sections from bank RKSE (J. Boucrot)
    and call this routine in QMNEWR.F

 ** Modifs in QUREFIT,QFREFIT to get the track refit
    with new tracking corrections by default on POTs/DSTs


