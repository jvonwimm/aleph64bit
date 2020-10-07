C! First deck in EDIR set
 ! ALEPHLIB 30.6
    SELEVT : fix variable type inconsistencies in function calls, 
             for Linux                            (A.Waananen)

 ! ALEPHLIB 30.5 correction file 2
    SNGMTR : Remove CALL from function references, for Linux. (A.Waananen)

 ! ALEPHLIB 21.6
    CLAS24 - Bug fix: Gampec photons were not being looked for in bank PGAC,
             where they have been since December 1994 (G.Ganis, 17/02/1997)

 ! ALEPHLIB 21.5
   CLAS24, LEPTO : protect against possibility of missing banks (M.Cattaneo)
   CLAS24,LEPTO,TRUSLU : Reject tracks with bad TRPFRF ret. code (M.Cattaneo)

 ! ALEPHLIB 21.4
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

(1) A run at LEP 1 energies during LEP 2 will get a LEP 1 EDIR.
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

 ! ALEPHLIB 21.3
   ALSUMCL - fix multiline string
   ULANGL  - Replace call to RLU by call to RNDM
   VUROBO  - Put ULANGL function in separate file, suppress RLU function

 ! ALEPHLIB 20.8
   LEPTO   - add a protection against events with > 300 tracks
             (S.Wasserbach)

 ! ALEPHLIB 20.5
   ALSUMCL - add summary of class 27
   SELEVT  - select class 27 events using VTRLAS from Julia
   TIZERO  - move to EDIR set
   ECAGET, ECALWR, ESUMW, ISPARK, SELBHA, TRIOFF -
   adapt to new PWEI bank.


 ! ALEPHLIB 20.1
   RPECO  - protect ACOS against abs(argument) > 1.(C.Rankin)

 ! ALEPHLIB 15.4
   EEWIRS - Bhabha events selected from two ecal modules with uncalibrated
            wire energies above 30 GeV.

 ! ALEPHLIB 15.1
   CLAS24  - remove acolinearity computation

 ! ALEPHLIB 14.6
   move ALSUMCL and ALCLASW from ALEF set to EDIR set
   ALSUMCL - add summary of class 24
   SELEVT  - add call to CLAS24
   CLAS24  - new routine to select class 24 events (tau group)
   CLAS26  - new routine to select class 26 events (alignment group)
   CHKCLAS - new routine to check the class word against the mask
             set with the CLAS data card
   ------------------------------------------------------------------

    **** ALEPH events classification Package : EVTCLAS  ***

    EDIR is the Aleph official Package used to create public
    event directories. It contains a set of routines which allow
    to select and to classify events according to a pre-defined
    selection criteria. The list of event directory classes and
    their content are listed bellow :

     Class # 1: Defined in ECALSL routine.
     ---------
     Content : More than 2 Ecal modules with energy(wires) >=2.5GeV
               in each.

     Class # 2: Defined in ECALSL routine.
     ----------
     Content  : Hcal energy(pads) + Ecal energy(wires) >15GeV.

     Class # 3: Defined in ECALSL routine.
     ----------
     Content  : Endcap A and Endcap B both with energy(wires) >2GeV
                or Barrel with energy >6GeV.

     Class # 4: Defined in SNGRND routine.
     ----------
     Content  : Hcal energy(pads) >3GeV + HCW(4 planes) * ITC trigger.

     Class # 5: Defined in TRACKS routine.
     ----------
     Content  : 1--> 7 tracks with >=4 TPC hits, D0 <5cm and Z0 <20cm.

     Class # 6: Defined in TRACKS routine.
     ----------
     Content  : >=8 tracks with the same cuts.

     CLASS # 7: Defined in ENLCAL routine.
     ----------
     Content  : LUM A and LUM B, both E >15GeV.

     CLASS # 8: Defined in ENLCAL routine.
     ----------
     Content  : LUM A or  LUM B, E >15GeV.

     Class # 9: Defined in MUELID routine.
     ----------
     Content  : Muon (HMAD flag) with energy >3GeV.

     Class #10: Defined in MUELID routine.
     ----------
     Content  : Electron with momentum >=2GeV.
                Electron candidates based on (-3.5<R2 ,-3.5<R3<4.0).

     Class #11: Defined in HVOBIT routine.
     ----------
     Content  : ECAL high voltage ON.

     Class #12: Defined in HVOBIT routine.
     ----------
     Content  : TPC  high voltage ON (Logical or between bit 4 and 15
                of the high voltage word).

     Class #13: Defined in HVOBIT routine.
     ----------
     Content  : ITC  high voltage ON.

     Class #14: Defined in HVOBIT routine.
     ----------
     Content  : LCAL high voltage ON.

     Class #15: Defined in LEPTO routine.
     ----------
     Content  : Dilepton candidates (e+ e-, mu+ mu- and tau+ tau-)
                Selections based on TPC tracks only.

     Class #16: Defined in SELTRK routine.
     ----------
     Content  : QQbar candidates based on TPC tracks selections.

     Class #17: Defined in SELCAL routine.
     ----------
     Content  : QQbar candidates based on calorimety selections.

     Class #18: Defined in TIZERO routine.
     ----------
     Content  : Events in time with the beam crossing :
                selection based on T0 information from Ecal wires
                abs(T0) <120ns in End caps and abs(T0) <100ns in Barrel.

     Class #19: Defined in SELEMU routine.
     ----------
     Content  : Muon candidates of all energies based on a logical OR
                of HMAD, MCAD and Mucalo(proba>80%).

     Class #20: Defined in SELBHA routine.
     ----------
     Content  : Bhabha candidates (selection based on Ecal only) :
                -at least 2 non adjacent modules with E(wires) >35GeV each
                -2 Ecal clusters with E(pads) >35GeV and |cos(theta)| <0.95.

     Class #21: Defined in PHEDIR routine.
     ----------
     Content  : Single photon candidates.

     Class #22: Defined in SIEDIR routine.
     ----------
     Content  : Bhabha candidate in Sical A and B
                                            ---
     Class #23: Defined in SIEDIR routine.
     ----------
     Content  : Bhabha candidate in Sical A or B
                                            --

     Class #24: Defined in CLAS24 routine.
     ----------
     Content  : Dilepton candidates (e+ e-, mu+ mu- and tau+ tau-)
                Enlarged class 15 selection.

     Class #25: Slow control records.
     ----------

     Class #26: Defined in CLAS26 routine.
     ----------
     Content  : Events to be used for alignement and
                calibration purposes.
                Muon events selected by mean of trigger bits pattern.
                Bhabha events selected from two ecal modules with uncalibrated
                wire energies above 30 GeV.

     Class #27: set in VTRLAS or CLAS278
     ----------
     POT      : VDET Laser events for VDET alignment / calibration
     MINI     : electron selection (Rt>-3, -3>Rl>3)  MINI only

     Class #28: set in CLAS278
     ---------
     MINI     : muon selection (QMUIDO flag 13 or 14)

     Class #29: Defined in SNGRND routine.
     ----------
     Content  : Random trigger events.

     Events which do not satisfy any of these 25 selections are
     classified as class #30.

     The list above can also be obtained on IBM or ALWS by mean
     of HELP EDIR. This help facility gives detailed descriptions
     of the cuts used to define the classes 15, 16, 17 and 21.

     To use this package in a stand-alone mode, to read and/or write
     public event directories one should use the following lines of
     Fortran :

      PROGRAM EVTCLAS
C --
      COMMON/BCS/IW(800000)
C --
      CALL BNAMES(3000)
      CALL BOS(IW,800000)
C --
C   Opens the cards file EVTCLAS CARDS or whatever cards file name
C   which contain input and output (FILI/FILO) data files.
C --
      CALL AOPEN(5,'EVTCLAS CARDS *',' ',' ',IER)
      CALL BREADC
C --
C   Open input and/or output files and write event directory file.
C   ALRWEV can also be used to write an EPIO file from an EDIR or
C   Native file and vice versa.
C --
      CALL ALRWEV
C --
      END

     Important remark:
     -----------------
     To use this package in a program in which the event reading
     exists already, one has, in order to classify events, to call the
     steering routine of the package : SELEVT(EVTFLG) for each event.
     The argument EVTFLG is a logical array (dimenssioned to 30) which
     contains the different classes (when the corresponding logical flags
     are true) to which the event belongs.

