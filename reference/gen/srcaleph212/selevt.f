      SUBROUTINE SELEVT(EVTFLG)
C----------------------------------------------------------------------
CKEY EDIR EVENTS CLASSIFICATION
C! Steering routine for events classification.
C-
C   Input  : None
C   Output : EVTFLG( 1) : > 2 ECAL modules, > 2.5 GeV each
C            EVTFLG( 2) : E(HCAL(pads) + ECAL(wires)) > 15. GeV
C            EVTFLG( 3) : ECA(wires) and ECB(wires) , both E > 2. GeV
C                         OR E(ECBAR(wires)) > 6. GeV
C            EVTFLG( 4) : E(HCAL(pads)) > 3. GeV + ITC
C            EVTFLG( 5) : 1-->7 tracks (D0 <5 cm and Z0 <20 cm + >4 hits
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
C            EVTFLG(18) : Events in time with the beam (cosmic rejection
C            EVTFLG(19) : Muon events of all energies
C            EVTFLG(20) : Bhabha events based on calorimetry
C            EVTFLG(21) : Single photon candidates
C            EVTFLG(22) : SicalA and SicalB , both E >20 Gev
C            EVTFLG(23) : SicalA or  SicalB , both E >20 Gev
C            EVTFLG(24) : Enlarged class 15, tau group definition
C            EVTFLG(25) : Slow control records
C            EVTFLG(26) : Events for alignement/calibration purposes
C            EVTFLG(27) : VDET LaserEvents for Calibration
C            EVTFLG(29) : Random trigger bit
C-
C   Called by   : ALEVCL or user routine
C   Calls  : ECALSL,SNGRND,TRACKS,ENLCAL,MUELID,HVOBIT,LEPTO,SELTRK,
C            SELCAL,TIZERO,SELEMU,SELBHA,PHEDIR,CLAS24,VTRLAS(JULIA)
C   Input banks : None
C-
C                                        Author: M. Talby September 89
C----------------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      LOGICAL EVTFLG(30),HVECAL,HVTPC,HVITC,HVLCAL,IGOOD
      LOGICAL TRKHOK,IBHAEV,SNGPHO,SIARM,SIBHA
      LOGICAL CLAS1,CLAS2,CLAS3,CLAS4,CLAS5,CLAS6,CLAS7,CLAS8
     &       ,CLAS19,CLAS27,CLAS29
      LOGICAL DUMMY
C
C Functions
C
      LOGICAL VTRLAS

C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C --
      DO 5 IFL = 1,30
        EVTFLG(IFL) = .FALSE.
    5 CONTINUE
C --
C   Selections
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
      RETURN
      END
