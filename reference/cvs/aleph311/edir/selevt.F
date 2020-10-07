      SUBROUTINE SELEVT(EVTFLG)
C----------------------------------------------------------------------
CKEY EDIR EVENTS CLASSIFICATION
C! Steering routine for events classification.
C-
C   Input  : None
C   Output : Depends whether it is a LEP 1 or LEP 2 EDIR -
C
C  For LEP 1
C            EVTFLG( 1) : > 2 ECAL modules, > 2.5 GeV each
C            EVTFLG( 2) : E(HCAL(pads) + ECAL(wires)) > 15. GeV
C            EVTFLG( 3) : ECA(wires) and ECB(wires) , both E > 2. GeV
C                         OR E(ECBAR(wires)) > 6. GeV
C            EVTFLG( 4) : E(HCAL(pads)) > 3. GeV + ITC
C            EVTFLG( 5) : 1-->7 tracks (D0 <5 cm and Z0 <20 cm + >4 hits)
C            EVTFLG( 6) : >7 tracks same cuts
C            EVTFLG( 7) : LumA and LumB  , both E > 15. GeV
C            EVTFLG( 8) : LumA or  LumB  , E > 15. GeV
C            EVTFLG( 9) : Muon with energy > 3 GeV
C            EVTFLG(10) : Electron with energy > 2 GeV
C            EVTFLG(11) : ECAL High voltage ON
C            EVTFLG(12) : TPC  High voltage ON
C            EVTFLG(13) : ITC  High voltage ON
C            EVTFLG(14) : LCAL High voltage ON
C            EVTFLG(15) : Lepton groupe selection
C            EVTFLG(16) : QQbar events (selection based on TPC)
C            EVTFLG(17) : QQbar events (selection based on Calorimeters)
C            EVTFLG(18) : Events in time with the beam (cosmic rejection)
C            EVTFLG(19) : Muon events of all energies
C            EVTFLG(20) : Bhabha events based on calorimetry
C            EVTFLG(21) : Single photon candidates
C            EVTFLG(22) : SicalA and SicalB , both E >20 Gev
C            EVTFLG(23) : SicalA or  SicalB , both E >20 Gev
C            EVTFLG(24) : Enlarged class 15, tau group definition
C            EVTFLG(25) : Slow control records
C            EVTFLG(26) : Events for alignement/calibration purposes
C            EVTFLG(27) : VDET LaserEvents for Calibration
C            EVTFLG(28) : not yet filled
C            EVTFLG(29) : Random trigger bit
C
C  For LEP 2
C            EVTFLG( 1) : ECAL selection
C            EVTFLG( 2) : E(HCAL(pads) + ECAL(wires)) > 15. GeV
C            EVTFLG( 3) : Cosmics through VDET
C            EVTFLG( 4) : E(HCAL(pads)) > 3. GeV + ITC
C            EVTFLG( 5) : 1-->7 tracks (D0 <5 cm and Z0 <20 cm + >4 hits)
C            EVTFLG( 6) : >7 tracks same cuts
C            EVTFLG( 7) : LumA and LumB  , both E > 15. GeV
C            EVTFLG( 8) : LumA or  LumB  , E > 15. GeV
C            EVTFLG( 9) : >= 3-prong 2-gamma selection
C            EVTFLG(10) : not yet filled, could be 2-prong 2-gamma
C            EVTFLG(11) : low multiplicity WW: >= 1 track of each sign
C            EVTFLG(12) : low multiplicity WW: all tracks of same sign
C            EVTFLG(13) : not yet filled
C            EVTFLG(14) : not yet filled
C            EVTFLG(15) : Lepton group selection
C            EVTFLG(16) : QQbar events (selection based on TPC)
C            EVTFLG(17) : QQbar events (selection based on Calorimeters)
C            EVTFLG(18) : not yet filled
C            EVTFLG(19) : Muon events of all energies
C            EVTFLG(20) : Bhabha events based on calorimetry
C            EVTFLG(21) : Single photon candidates
C            EVTFLG(22) : SicalA and SicalB , both E >20 Gev
C            EVTFLG(23) : not yet filled
C            EVTFLG(24) : Enlarged class 15, tau group definition
C            EVTFLG(25) : Slow control records
C            EVTFLG(26) : Events for alignment/calibration purposes
C            EVTFLG(27) : VDET LaserEvents for Calibration
C            EVTFLG(28) : not yet filled
C            EVTFLG(29) : Random trigger bit
C-
C   Called by   : ALEVCL or user routine
C   Calls  : ECALSL,SNGRND,TRACKS,ENLCAL,MUELID,HVOBIT,LEPTO,SELTRK,
C            SELCAL,TIZERO,SELEMU,SELBHA,PHEDIR,CLAS24,VTRLAS(JULIA)
C            SELENU,VDCOSMJ,ENLCL2,SELGGE,SELWWA,SELWWB
C   Input banks : None
C-
C                                        Author: M. Talby September 89
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
      LOGICAL EVTFLG(30),HVECAL,HVTPC,HVITC,HVLCAL,IGOOD
      LOGICAL TRKHOK,IBHAEV,SNGPHO,SIARM,SIBHA
      LOGICAL CLAS1,CLAS2,CLAS3,CLAS4,CLAS5,CLAS6,CLAS7,CLAS8
     &       ,CLAS9,CLAS11,CLAS12,CLAS19,CLAS27,CLAS29
      REAL DUMMY
      LOGICAL ALEP2, VDCOSMJ
      DATA LRUN/0/
C
C Functions
C
      LOGICAL VTRLAS

#include "bmacro.h"
C --
      DO 5 IFL = 1,30
        EVTFLG(IFL) = .FALSE.
    5 CONTINUE
C --
C   Decide whether we should be making a LEP 1 EDIR or a LEP 2 EDIR.
C   A little care is required as the CMS energy can sometimes be crap.
C   LEP 1.5 remains as LEP 1, so LEP 2 starts after run 41000.
C --
      KEVEH = IW (NAMIND('EVEH'))
      IRUNC = IW ( KEVEH + 2 )
      IF (IRUNC.NE.LRUN) THEN
         LRUN = IRUNC

         ALEP2 = .FALSE.

         IF (IRUNC.GE.41000.OR.IRUNC.LE.2000) THEN
C --
C   Check on the CMS energy, but make sure we make the same choice for all
C   events in one run!
C --
            ENLEP = ALELEP(IRUNC)
            IF (ENLEP.LT.80. .OR. ENLEP.GT.150.) ALEP2 = .TRUE.
         ENDIF
      ENDIF
C --
C   Selections.....
C --
C   ...for LEP < 2....
C          =======
C --
      IF (.NOT.ALEP2) THEN
C --
C   Classes 1,2 and 3
C --
         CALL ECALSL(CLAS1,CLAS2,CLAS3)
         IF(CLAS1) EVTFLG(1) = .TRUE.
         IF(CLAS2) EVTFLG(2) = .TRUE.
         IF(CLAS3) EVTFLG(3) = .TRUE.
C --
C   Classes 4 and 29
C --
         CALL SNGRND(CLAS4,CLAS29)
         IF(CLAS4)  EVTFLG(4) = .TRUE.
         IF(CLAS29) EVTFLG(29) = .TRUE.
C --
C   Classes 5 and 6
C --
         CALL TRACKS(NTRK,CLAS5,CLAS6)
         IF(CLAS5) EVTFLG(5) = .TRUE.
         IF(CLAS6) EVTFLG(6) = .TRUE.
C --
C   Classes 7 and 8
C --
         CALL ENLCAL(CLAS7,CLAS8)
         IF(CLAS7) EVTFLG(7) = .TRUE.
         IF(CLAS8) EVTFLG(8) = .TRUE.
C --
C   Classes 9 and 10
C --
         CALL MUELID(NMUON,NELEC)
         IF(NMUON.GT.0)  EVTFLG(9) = .TRUE.
         IF(NELEC.GT.0)  EVTFLG(10) = .TRUE.
C --
C   Classes 11, 12, 13 AND 14
C --
         CALL HVOBIT(HVECAL,HVTPC,HVITC,HVLCAL)
         IF(HVECAL) EVTFLG(11) = .TRUE.
         IF(HVTPC)  EVTFLG(12) = .TRUE.
         IF(HVITC)  EVTFLG(13) = .TRUE.
         IF(HVLCAL) EVTFLG(14) = .TRUE.
C --
C   Class 15
C --
         CALL LEPTO(IGOOD)
         IF(IGOOD) EVTFLG(15) = .TRUE.
C --
C   Class 16
C --
         CALL SELTRK(TRKHOK)
         IF(TRKHOK) EVTFLG(16) = .TRUE.
C --
C   Class 17
C --
         CALL SELCAL(IECAL)
         IF(IECAL.EQ.1) EVTFLG(17) = .TRUE.
C --
C   Class 18
C --
         CALL TIZERO(TMEAN,IEVTIM)
         IF(IEVTIM.EQ.1) EVTFLG(18) = .TRUE.
C --
C   Class 19
C --
         CALL SELEMU(IFLAHM,IFLAMC,IFLACA,CLAS19)
         IF(CLAS19) EVTFLG(19) = .TRUE.
C --
C   Class 20
C --
         CALL SELBHA(IBHAEV)
         IF(IBHAEV) EVTFLG(20) = .TRUE.
C --
C   Class 21
C --
         CALL PHEDIR(SNGPHO)
         IF(SNGPHO) EVTFLG(21) = .TRUE.
C --
C   Class 22 and 23
C --
         CALL SIEDIR(SIARM,SIBHA)
         IF(SIARM) EVTFLG(23) = .TRUE.
         IF(SIBHA) EVTFLG(22) = .TRUE.
C --
C   Class 24
C --
         CALL CLAS24(IGOOD)
         IF(IGOOD) EVTFLG(24) = .TRUE.
C --
C   Class 26
C --
         CALL CLAS26(IGOOD)
         IF(IGOOD) EVTFLG(26) = .TRUE.
C --
C   Class 27
C --
         CLAS27 = VTRLAS(DUMMY)
         IF(CLAS27) EVTFLG(27) = .TRUE.
C --
      ELSE
C --
C   ....and for LEP 2....
C               =====
C --
C   Class 1....NEW: neutral ECAL from SPOT
C --
         CALL SELENU(CLAS1)
         IF(CLAS1) EVTFLG(1) = .TRUE.
C --
C   Class2....OLD
C --
         CALL ECALSL(CLAS1,CLAS2,CLAS3)
         IF(CLAS2) EVTFLG(2) = .TRUE.
C --
C   Class 3....NEW: VDET cosmic selection
C --
         CLAS3 = VDCOSMJ(IDUMMY)
         IF(CLAS3) EVTFLG(3) = .TRUE.
C --
C   Classes 4 and 29.....OLD
C --
         CALL SNGRND(CLAS4,CLAS29)
         IF(CLAS4)  EVTFLG(4) = .TRUE.
         IF(CLAS29) EVTFLG(29) = .TRUE.
C --
C   Classes 5 and 6......OLD
C --
         CALL TRACKS(NTRK,CLAS5,CLAS6)
         IF(CLAS5) EVTFLG(5) = .TRUE.
         IF(CLAS6) EVTFLG(6) = .TRUE.
C --
C   Classes 7 and 8.....MODIFIED: new thresholds
C --
C   This routine is just a copy of ENLCAL with the thresholds changed.
C   This is not very nice, but I wanted to avoid changing the arguments or
C   duplicating to code to determine whether it's LEP 1 or LEP 2!
C --
         CALL ENLCL2(CLAS7,CLAS8)
         IF(CLAS7) EVTFLG(7) = .TRUE.
         IF(CLAS8) EVTFLG(8) = .TRUE.
C --
C   Class 9....NEW: 2-gamma selection from Alex
C --
         CALL SELGGE(CLAS9)
         IF(CLAS9)  EVTFLG(9) = .TRUE.
C --
C   Class 10....Reserved: not yet filled. Could be used for 2-prong 2-gamma
C --
C --
C   Classes 11 and 12....NEW: WW->lnu+lnu selections
C --
         CALL SELWWA(CLAS11)
         IF(CLAS11) EVTFLG(11) = .TRUE.
C --
         CALL SELWWB(CLAS12)
         IF(CLAS12)  EVTFLG(12) = .TRUE.
C --
C   Classes 13 and 14....Reserved: not yet filled 
C --
C --
C   Class 15....OLD: but with a mod. to avoid a STOP!
C --
         CALL LEPTO(IGOOD)
         IF(IGOOD) EVTFLG(15) = .TRUE.
C --
C   Class 16....OLD: but with a mod. to the code that determines LEP CM energy
C --
         CALL SELTRK(TRKHOK)
         IF(TRKHOK) EVTFLG(16) = .TRUE.
C --
C   Class 17....OLD: not tizero has been modified for bunch trains
C --
         CALL SELCAL(IECAL)
         IF(IECAL.EQ.1) EVTFLG(17) = .TRUE.
C --
C   Class 18....Reserved: not yet filled
C --
C --
C   Class 19....OLD: same as LEP 1 (using a new version by G.Bagliesi)
C --
         CALL SELEMU(IFLAHM,IFLAMC,IFLACA,CLAS19)
         IF(CLAS19) EVTFLG(19) = .TRUE.
C --
C   Class 20....OLD
C --
         CALL SELBHA(IBHAEV)
         IF(IBHAEV) EVTFLG(20) = .TRUE.
C --
C   Class 21....OLD: but minor mods. to the cuts
C --
         CALL PHEDIR(SNGPHO)
         IF(SNGPHO) EVTFLG(21) = .TRUE.
C --
C   Class 22....OLD: not modified for Ecms > 91.1 by Brigitte
C --
         CALL SIEDIR(SIARM,SIBHA)
         IF(SIBHA) EVTFLG(22) = .TRUE.
C --
C   Class 23....Reserved: not yet filled
C --
C --
C   Class 24
C --
         CALL CLAS24(IGOOD)
         IF(IGOOD) EVTFLG(24) = .TRUE.
C --
C   Class 25....OLD: filled by alrwev
C --
C --
C   Class 26....OLD: but mutrgs has been modified by Marco for tracking group
C --
         CALL CLAS26(IGOOD)
         IF(IGOOD) EVTFLG(26) = .TRUE.
C --
C   Class 27....OLD
C --
         CLAS27 = VTRLAS(DUMMY)
         IF(CLAS27) EVTFLG(27) = .TRUE.
C --
C   Class 28....Reserved: not yet filled
C --
C --
C   Class 30....OLD: filled in alsumcl
C --
      ENDIF

      RETURN
      END
#endif
