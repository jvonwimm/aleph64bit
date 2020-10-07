
========================================================================
                             ALPHA 125 News
                        Last Update: 8 November  2000
========================================================================

  ALPHA version 125 first release on 14 January 2000

  released as default ALPHA version on 17 January  2000

  All features of the successive correction files are summarized below ,
  starting from the most recent one. 

  For previous corrections, refer to the ALPHA 124 News file

=================================================

 Correction file 125.17 released 8 Nov  2000
 Modifications w.r.t. ALPHA 125.16 :

** Modifs in the routines QUREFIT,QFREFIT
  
** Increase the default BOS common size in QUIBOS

=================================================

 Correction file 125.16 released 7 Nov  2000
 Modifications w.r.t. ALPHA 125.14 :

** Modifs in the routines QUREFIT,QFREFIT
   Withdraw TUNI1NC and TPDZWC which are now in the ALEPHLIB


=================================================

 Correction file 125.14 released 30 Oct  2000
 Modifications w.r.t. ALPHA 125.12 :

** Add new refit of tracks with data card REFT 
   (Code from A. Bonissent and W> Wiedenmann)
   Mods in QUREFIT
   NEW routines QFREFIT,IQVERP,VDFIXZ
   NEW include REFTJJ
   NEW temporary routines TUN!NC,TPDZWC (will go in the ALEPHLIB) 

=================================================

 Correction file 125.12 released 19 Oct  2000
 Modifications w.r.t. ALPHA 125.10 :


 ** Add a new algorithm E14  provided by G. Taylor 
    to get the energy inside a cone of 14 degrees around the beam axis  

    Calling sequence:  CALL QGETE14(e14ec,e14hc,lveto)    
                       which returns the following values:

         e14ec = energy from Ecal/Lcal/Sical (in GeV) in 14 degree cone
         h14ec = energy from Hcal (in GeV) in 14 degree cone
         lveto = Maximum pulse height in LCAL veto counters

    This algorithm works only on POTs or DSTs, not on MINIs version 203
    and below. The E14 results will be available in the future from 
    MINIs version 204 and above, from a new bank (DE14) in which the
    3 above quantities will be stored during the MINI making. 

 ** Modifications to the MINI: new MINI, version 204

    - Build the DE14 bank (see above) for LEP2 MINIs only
    - Keep for LEP2 MINIs all the banks needed for redoing the Energy Flow
      from the MINI.
      
    A new executable miniprod_204 corresponding to the above modifications
    has been built on $ALEPH/phy for checking.
    The official miniprod is still the MINI 203 one.

 ** New data card MEFL 

    When reading future MINI 204 datasets, this new data card will allow
    to redo the Energy Flow from the MINI in the following way:

    - if this data card is put in the CARDs user file,
      the MINI ENergy Flow banks are not decoded, even if EFLW or EFLJ
      data cards are present;

    - the user can modify the conditions of the energy flow (locks, etc..) 
      then redo the energy flow explicitely with the usual calls: 
              CALL QUEFLO   !  execute the Energy Flow algorithm
              CALL QFEFLO   !  store the results in ALPHA variables
              CALL QFJETS   !  perform the Eflow jet-finding 

      The MEFL data card has no effect on older MINI datasets (version 203
      or before), or on POTs/DSTs.

=================================================

 Correction file 125.10 released 17 October 2000
 Modifications w.r.t. ALPHA 125.08 :

 - Fix in QFMCPA to write or read MINIs produced with any
   primary KINGAL generator + Kinagain using HERWIG
   (was possible only before with primary KINGAL = KRLW02)
  

=================================================

 Correction file 125.08 released 17 July 2000
 Modifications w.r.t. ALPHA 125.07 :

                                  
 ** Add a new function MINI_KPECPC provided by A. Venturi   
    to get the PCOB object number correctly on MINIs, which can't be
    obtained from the ALPHA statement function KPECPC

    Calling sequence:

          INTEGER FUNCTION MINI_KPECPC(IECAL)
  
 
      Input: IECAL (I) : ALPHA track number of the ECAL (PECO) object
 
      Output: (I): PCOB object number
 
      error code : -1 = invalid IECAL
                   -2 = IECAL is NOT a PECO object
                   -3 = cannot find PECO Julia number
                   -4 = cannot find PCRL         

=================================================

 Correction file 125.07 released 26 June 2000
 Modifications w.r.t. ALPHA 125.06 :

 ** Add dummy user routine QUREFIT called from QMEVNT before QFILL
    for tracking/alignment improvements


=================================================

 Correction file 125.06 released 8 June 2000
 Modifications w.r.t. ALPHA 125.03 :

 ** New features for Year 2000 data with Mini-Ramps:

    All the new features described below work ONLY on Year 2000 data
    if the data card  MRMP has been provided by the user:
    (N.B. : this data card allows XLUMOK to give events taken during
            Mini-Ramps, otherwise these events are rejected).


    - in QWSUMM, the luminosity WITH MINIRAMP EVENTS is printed
      in addition to the standard one (which is without Miniramp events)
 
    - new routine to get the luminosity WITH MINIRAMP EVENTS 
      for the current run

        CALL QLMINRP(XLUMRMP,NBHAMRMP)

        returns XLUMRMP  = lumi for the current run WITH MINIRAMP EVENTS
                NBHAMRMP = corresponding number of Bhabhas

        If the routine is called for MC data, or real data of 1999 and before,
        or real data of Year 2000 without the use of the MRMP data card, 
        it returns XLUMRMP = -1. and NBHAMRMP = 0. 

 **  New logical function to know, for Year 2000 data, if an event was
     taken during a Mini-ramp or not:

     XMRMP(dummy)  = .true.  when event outside a miniramp ( Energy is stable)
                   = .false. when event taken during a miniramp

     The use of this function is independent of the data card MRMP.


=================================================

 Correction file 125.05 released 6 April 2000
 Modifications w.r.t. ALPHA 125.03 :

 ** Bug fixed in QDDX

 ** Mod in QJTHRU : 
    When one is using NOSC data card to remove SiCal from the Energy flow
    on MINIS, the value obtained for the thrust was wrong. This is because    
    on MINIS the thrust is read from a bank built from the standard thrust
    calculation, which is done when making the MINI and doesn't care about NOSC.

    Now if NOSC is present, and if one is reading a MINI, the thrust is
    recomputed and therefore consistent with the NOSC cleaning.

 ** New facility : automatic stageing in advance of all FILI cards (and only
    them) of an ALPHA job. This is done using a new data card :

 STGI

     All files given in the FILI CARDs are staged at the very beginning
     of the job. Some informations on this stageing are printed
     in the ALPHA standard output.

     --> mods in QMINIT, new routine QMSTGIN

 ** Modification of the stage_clear facility using the SCLR data card:

     now if one puts this data card

 SCLR

     all files staged during the ALPHA job are stage_cleared only if they
     were staged by the owner of the job, and read 1 or 2 times maximum.

     --> mods in QMSTCLR, new routine QANFILI

 The above facilities (STGI and SCLR) are completely independant.

 They are mostly intended for people who have to stage several files which
 are not frequently used (e.g. Monte-Carlo POTs !), and for which it is 
 important to free the corresponding space in the stage pool when the job
 is completed.


=================================================

 Correction file 125.03 released 7 March 2000
 Modifications w.r.t. ALPHA 125.02 :

 ** Bug fix in QGTHRU for Linux compilers
    - a test on the equality of two floating point numbers was
      never executed on Linux --> gave always thrust =0.  !

 ** New data card SCLR to allow the stage clear of the tapes 
    which have been used for the job:

SCLR

      This new facility is intended for jobs which access only once
      datasets which are not frequently used (e.g. MCarlo POTs)
      and which could flood the stageing area if not cleared.  

      The clearing is restricted to files which have been used
      once and only once, to avoid problems with other users.

      - mod in QMTERM, new routine QMSTCLR

 ** Modifications in the MINI code: bank DMSC which gives the multiple
    scattering angle of tracks between ITC and TPC, is filled now for
    all LEP II events and not only for CLAS24 events.

    - mod in routine MINBLD , new MINI version = 203 in mversn.h 
     
      Warning : the Mini executable is not changed yet
 
=================================================

 Correction file 125.02 released 1 February 2000
 Modifications w.r.t. ALPHA 125.01 :
                                  
 ** Use of new FJMMCL routine called by QGJMMC
    - with 'GENEVA' and 'INVMAS' algorithms
    - and possibility of asking for a given number of jets
      with YCUT <0 
      see ALEPHLIB ALNEWS or ALPHA user's guide for ALPHA125

 ** Small fix in QFNDV0

 ** New routines to write on output SEVT cards (intended for the
    "alignment event stripping project"):

      in QUEVNT for a selected event :  CALL QWRSEVT
      in QUTERM to write the resulting SEVT cards on file 'my_file'
                                        CALL QWRFSEVT(my_file) 


=================================================

 Correction file 125.01 released 17 January 2000
 Modifications w.r.t. ALPHA 125.00 :
                                 
 ** Fix in QMINIT (wrong format for ADBSCONS 300 date) 
