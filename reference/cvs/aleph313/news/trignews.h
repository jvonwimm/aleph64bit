C! LEVEL 1 and LEVEL 2 trigger package

 ! ALEPHLIB 30.6
    X1BOOK,X2TRIG : fix variable type inconsistencies in function calls, 
                    for Linux                            (A.Waananen)

 ! ALEPHLIB 21.6
    X1APTN - Add endcaps to SNG_N_EL trigger definition (A.Putzer 04/02/97)

 ! ALEPHLIB 21.3
   X1IRUN : In MonteCarlo, use trigger thresholds which are setup dependent
            (B.Bloch) 

 ! ALEPHLIB 20.8
   remove all references to XTBP which has been deleted from DDL.

 ! ALEPHLIB 20.7
   X1DISN, X1MIXN, X1REDO, X1TRIG - corrections (A.Putzer)

 ! ALEPHLIB 20.5
   implement level 1 trigger in a correct way. ALEPH-note will
   follow (A.Putzer)

 ! ALEPHLIB 15.7
    X1INI (used to re-apply trigger in ALPHA ) : give the right
    initialisation to SICAL banks namindices.  (B.Bloch)

 ! ALEPHLIB 14.6
   SIX2MK - add a LOGICAL BTEST (mandatory on apollo)
 -------------------------------------------------------------
     Level 1 Trigger

 The package is used in GALEPH to generate the online trigger
 information contained in the banks X1AD and XTEB.
 It may also be used to reprocess the Level 1 Trigger decision
 in JULIA or ALPHA for different threshold sets, if the banks
 X1AD and XTEB are present on input. In this case the user has
 to supply a new threshold bank X1TH and call two subroutines:

(1) X1INI(IRUNNO) -- Trigger initialization.  This routine must be
    called once at the start of a job to initialize the trigger routines,
    and to read the trigger threshold bank X1TH (which can be supplied as
    a BOS card file). IRUNNO is the run number used to select the correct
    threshold bank.

(2) X1TRG -- Apply level 1 trigger.  This routine reads the trigger
    banks from the input and reapplies the trigger using the
    trigger thresholds found during initialization.
    The resulting trigger bit pattern is stored in the XTEB bank.

 Reprocessing of MC data updates the trigger information to the latest
 version of the trigger software without any loss of information.
 For backward compatibility MC data containig the bank XTBP instead
 of XTEB can also be reprocessed. Output will be XTEB and a reordered
 bank X1AD (EC wire information only, see below).

 The following routines can be used also.

(1) X1PRNT -- Prints trigger information for a single event.
(2) X1WSUM -- Print trigger summary at end of run; this routine is
    automatically called at the end of a GALEPH run.
(3) X1BOOK and X1HIST -- Book and fill histograms of useful
    trigger information.  X1BOOK must be called during initialization
    and X1HIST called once per event.

 Modifications 890821 (C.Geweniger):
 - The banks X1TI and XTBP are removed from output.
 - XTBP is still allowed on input for reprocessing of previous MC data.
 - On output the bit pattern bank XTBP is replaced by XTEB.
 - The discriminator bits for trigger segments 65 to 72, missing in XTBP,
   are now present in XTEB (also if old data are reprocessed).
 - The order of EC wire information from the endcaps in XTEB and X1AD has
   changed. It is now in agreement with the notation in the ALEPH hand-
   book: in the trigger the first EC endcap module starts at phi=15 deg.
 - All Bhabha triggers are implemented.
 - A total energy trigger using the .AND. of EC wires in the two endcaps
   has been added.
 - the definition of thresholds sets for the charged electromagnetic
   energy trigger and the neutral hadronic energy trigger have been
   changed.

 Modifications 891116 (C.Geweniger):
 - All physics triggers implemented for data taking starting September 89
   are coded. The bit assignment in the level 1 trigger mask (1st word in
   bank XTEB) corresponds to the one valid from physics run #4860
   onwards.
 - At the same time the threshold bank X1TH has been updated
   (use bank no. 2).
 - The threshold bank X1TH provides only one set of thresholds for total
   energies, whereas the actual total energy triggers use two sets for
   ECAL wires. The second set is put into a DATA statement in routine
   X1DISC.

 Modifications 921210 (B.Bloch-Devaux):
   Introduce the new Luminosity trigger connected to the SICAL detector
   when corresponding setup is required ( geometry date > = 9209)
 - Galeph produces a bank SIFO containing the Fast OR signals from the
   readout electronics ,SIXA and SIX2 banks are added by the trigger
   code.
 - X1IRUN loads the proper constants from banks SITC and SRCO on the
   Data Base and calls the initialisation code SITRIN
 - X1MIXI calls SIXAMK to build the bank SIXA of ADC counts from the
   16 trigger sectors of the detector ( as from the Data )
 - X1DISC calls SIX2MK to build the bank SIX2 filled with the bit
   pattern discriminated against the 4 thresholds of SICAL triggers.
   A service function SIDISC is called by SIX2MK.
 - X1APTR applies the trigger and will count triggers passing the three
   possible configurations of SICAL triggers.However the trigger bit
   word of the event is not updated as nobody uses it for Monte-Carlo
   events and it has never been updated since 89.... The SIX2 bank

   contains the relevant information for Luminosity analysis.
 - X1WSUM is modified to give also the summary for SICAL triggers .
 - X1PRNT is modified to give a debug printout for SICAL triggers by
   calling the subroutine SIXAPR.

    Level1 Trigger Summary

        This package consists of 11 subroutines :
    _ 5 subroutines of general interest, to be put in the ALEPHLIB.
    _ 6 subroutines to produce the Trigger Run Summary banks during
      the JULIA processing, to be put in JULIA.


    1. Subroutines for the ALEPHLIB
    -------------------------------

    X1PREP:   to create the bank XTDI for trigger analysis (contains
              all mapped and unmapped trigger signals)
    X1DCEB:   to fill the bank XTDI from the bank XTEB; calls X1ECWM
      X1ECWM: routine for the ECal Wires module-to-segment mapping
    X1TIME:   to decode the time of the event from the bank XTCN
    X1RSUM:   to print the Trigger Run Summary from the banks XSGE,
              XSHI and XSSC


    2. Subroutines for JULIA
    ------------------------

    XTRSIN:   to be called during initialisation, before the first
              event is processed; calls X1PREP to create the bank
              XTDI
    XTRSEV:   to be called for each event; finds the Trigger banks
              and calls XTUNPK to decode and sum them
    XTUNPK:   calls UTDCBP and UNPACK
    UTDCEB:   decode a bit string into an array
    UNPACK:   decode packed 16-bits words into an array
    XTRSFI:   to be called at termination, after the last event in
              the run has been processed; drops the XTDI bank,
              creates and fills the Trigger Run Summary banks XSGE,
              XSHI and XSSC.
              IT DOES NOT WRITE THEM!


    LEVEL 2 Trigger

 The trigger LEVEL 2 routines use the TPC TTHT bank to build necessary
 banks for trigger LEVEL 3.
 The subroutines described below have been installed in ALEPHLIB, and
 can therefore be used in either JULIA or ALPHA without modification as
 long as the TTHT bank is there.

 The user must call two subroutines to reprocess the trigger.

 (1) X2IRUN (IRUN,FHIS,IRET) -- trigger initialization. The routine
     must be called once at beginning of run to initialize the package
     and read the X2RU data card if there.
     input arguments: IRUN / I   current run number
                      FHIS / L   .true. if histograms are required
                      IRET / I   return code (=0 means OK)
 (2) X2TRIG (IRET) -- apply LEVEL 2 trigger. Must be called once per
     event. Reads TTHT bank and produced X2HF, X2MS, X2TB, X2TF.

 The following routines can be used also.

(1) X2PRIN (FLAG) -- Prints trigger information at initialization time
                     (FLAG='INIT') and for a single event (FLAG='EVEN').
(2) X2WSUM -- Print trigger summary at end of run; this routine is
    automatically called at the end of a GALEPH run.
(3) X2BOOK and X2HIST -- Book and fill histograms of useful
    trigger information.  X2BOOK must be called during initialization
    and X2HIST is called internally once per event.

