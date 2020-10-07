      SUBROUTINE ALSUMCL (KCLASW)
C----------------------------------------------------------------------
CKEY EDIR CLASS SUMMARY / USER
C!  Accumulate and Print statistics about edir classes
C - F.Ranjard -910801                  from E.Lancon
C              920506  introduce entry point ALSUMGT
C
C - Input   : - KCLASW  I/      30bits class word
C                         --->  Special value = -1 , Print Summary
C - Entry   : ALSUMGT (IARRAY,NDIM)
C             NDIM    = IARRAY dimension
C             IARRAY  = 1-NCLAS   statistic of class words
C                       NCLAS+1   no. of events without class word
C                       NCLAS+2   no. of events with class word
C                       NCLAS+3   no. of run records
C                       the 1st NDIM words are filled
C
C ----------------------------------------------------------------------
#ifndef DOC
      SAVE NAEVEH
#include "bcs.h"
      CHARACTER*1 BAR(80)
      PARAMETER( NCLAS =  30)
      INTEGER ISTAT (NCLAS)
      CHARACTER*60 TXTCL(NCLAS,2)
      INTEGER IARRAY(*)
      LOGICAL BTEST
C----------------------------------------------------------------------
      DATA ISTAT /NCLAS*0/
      DATA BAR /80*'-'/
      DATA TXTCL (1,1) /
     &  'More than 2 Ecal modules with E(wires) >=2.5GeV in each....'/
      DATA TXTCL (2,1) /
     &  'Hcal energy(pads) + Ecal energy(wires) >15GeV..............'/
      DATA TXTCL (3,1) /
     &  'Endcap A & B both with E(wires)>2GeV or Barrel with E>6GeV.'/
      DATA TXTCL (4,1) /
     &  'Hcal energy(pads) >3GeV + HCW(4 planes) * ITC trigger......'/
      DATA TXTCL (5,1) /
     &  '1--> 7 tracks with >=4 TPC hits, D0 <5cm and Z0 <20cm......'/
      DATA TXTCL (6,1) /
     &  '>=8 tracks with the same cuts..............................'/
      DATA TXTCL (7,1) /
     &  'LUM A and LUM B, both E >15GeV.............................'/
      DATA TXTCL (8,1) /
     &  'LUM A or  LUM B, E >15GeV..................................'/
      DATA TXTCL (9,1) /
     &  'Muon (HMAD flag) with energy >3GeV.........................'/
      DATA TXTCL (10,1) /
     &  'Electron with momentum >=2GeV. (-3.5<R2 ,-3.5<R3<4.0)......'/
      DATA TXTCL (11,1) /
     &  'ECAL high voltage ON.......................................'/
      DATA TXTCL (12,1) /
     &  'TPC  high voltage ON.......................................'/
      DATA TXTCL (13,1) /
     &  'ITC  high voltage ON.......................................'/
      DATA TXTCL (14,1) /
     &  'LCAL high voltage ON.......................................'/
      DATA TXTCL (15,1) /
     &  'Dilepton candidates Selections based on TPC tracks only....'/
      DATA TXTCL (16,1) /
     &  'QQbar candidates based on TPC tracks selections............'/
      DATA TXTCL (17,1) /
     &  'QQbar candidates based on calorimety selections............'/
      DATA TXTCL (18,1) /
     &  'Events in time with the beam crossing......................'/
      DATA TXTCL (19,1) /
     &  'Muon all energies, logical OR of HMAD, MCAD and QMUIDO.....'/
      DATA TXTCL (20,1) /
     &  'Bhabha candidates (selection based on Ecal only)...........'/
      DATA TXTCL (21,1) /
     &  'Single photon candidates...................................'/
      DATA TXTCL (22,1) /
     &  'Sical A and Sical B, both E >20GeV.........................'/
      DATA TXTCL (23,1) /
     &  'Sical A or  Sical B, E >20GeV..............................'/
      DATA TXTCL (24,1) /
     &  'Dilepton candidates Selection based on TPC and ECAL .......'/
      DATA TXTCL (25,1) /
     &  'Slow control records.......................................'/
      DATA TXTCL (26,1) /
     &  'events selected for alignment .............................'/
      DATA TXTCL (27,1) /
     &  'VDET Laser events for calibration..........................'/
      DATA TXTCL (28,1) /
     &  '...........................................................'/
      DATA TXTCL (29,1) /
     &  'Random trigger events......................................'/
      DATA TXTCL (30,1) /
     &  'Not Classified.............................................'/
      DATA TXTCL (1,2) /
     &  'At least 1 ECAL module with Ew>0.5 & |T0|<200, Ep>1.5......'/
      DATA TXTCL (2,2) /
     &  'Hcal energy(pads) + Ecal energy(wires) >15GeV..............'/
      DATA TXTCL (3,2) /
     &  'Cosmic within VDET acceptance..............................'/
      DATA TXTCL (4,2) /
     &  'Hcal energy(pads) >3GeV + HCW(4 planes) * ITC trigger......'/
      DATA TXTCL (5,2) /
     &  '1--> 7 tracks with >=4 TPC hits, D0 <5cm and Z0 <20cm......'/
      DATA TXTCL (6,2) /
     &  '>=8 tracks with the same cuts..............................'/
      DATA TXTCL (7,2) /
     &  'LUM A and LUM B, both E >30GeV.............................'/
      DATA TXTCL (8,2) /
     &  'LUM A or  LUM B, E >30GeV..................................'/
      DATA TXTCL (9,2) /
     &  '2-gamma selection..........................................'/
      DATA TXTCL (10,2) /
     &  '...........................................................'/
      DATA TXTCL (11,2) /
     &  'WW selectn A: Ntrk<8, Etrk/ELep>0.05, >= 1 trk of each sign'/
      DATA TXTCL (12,2) /
     &  'WW selectn B: Ntrk<8, Etrk/ELep>0.05, all one sign.........'/
      DATA TXTCL (13,2) /
     &  '...........................................................'/
      DATA TXTCL (14,2) /
     &  '...........................................................'/
      DATA TXTCL (15,2) /
     &  'Dilepton candidates Selections based on TPC tracks only....'/
      DATA TXTCL (16,2) /
     &  'QQbar candidates based on TPC tracks selections............'/
      DATA TXTCL (17,2) /
     &  'QQbar candidates based on calorimety selections............'/
      DATA TXTCL (18,2) /
     &  '...........................................................'/
      DATA TXTCL (19,2) /
     &  'Muon all energies, logical OR of HMAD, MCAD and QMUIDO.....'/
      DATA TXTCL (20,2) /
     &  'Bhabha candidates (selection based on Ecal only)...........'/
      DATA TXTCL (21,2) /
     &  'Single photon candidates...................................'/
      DATA TXTCL (22,2) /
     &  'Sical A and Sical B, both E >20GeV.........................'/
      DATA TXTCL (23,2) /
     &  '...........................................................'/
      DATA TXTCL (24,2) /
     &  'Dilepton candidates Selection based on TPC and ECAL .......'/
      DATA TXTCL (25,2) /
     &  'Slow control records.......................................'/
      DATA TXTCL (26,2) /
     &  'Events selected for alignment and calibration .............'/
      DATA TXTCL (27,2) /
     &  'VDET Laser events for calibration..........................'/
      DATA TXTCL (28,2) /
     &  '...........................................................'/
      DATA TXTCL (29,2) /
     &  'Random trigger events......................................'/
      DATA TXTCL (30,2) /
     &  'Not Classified.............................................'/
      DATA NRUNR, NOCLAS, KCLAS /0,0,0/
      DATA NAEVEH / 0 /
C----------------------------------------------------------------------
      IF (KCLASW .EQ. 0) THEN
         NOCLAS = NOCLAS+1
      ELSEIF (KCLASW.NE.-1 ) THEN
        IF (BTEST(KCLASW,29).AND.BTEST(KCLASW,28)) THEN
           NRUNR = NRUNR+1
        ELSE
           KCLAS = KCLAS+1
           DO 1 ICLAS =1,NCLAS
              IF ( BTEST (KCLASW,ICLAS-1)) THEN
                 ISTAT(ICLAS) = ISTAT(ICLAS) + 1
              ENDIF
 1         CONTINUE
         ENDIF
      ELSE
C --
C   Decide whether a LEP 1 or LEP 2 EDIR was made:.
C --
        IF( NAEVEH .EQ. 0 ) NAEVEH = NAMIND( 'EVEH' )
        KEVEH = IW (NAEVEH)
        IRUNC = IW ( KEVEH + 2 )
        ILEP = 1

        IF (IRUNC.GE.41000.OR.IRUNC.LE.2000) THEN
C --
C   Check on the CMS energy, but make sure we make the same choice for all
C   events in one run!
C --
           ENLEP = ALELEP(IRUNC)
           IF (ENLEP.LT.80. .OR. ENLEP.GT.150.) ILEP = 2
        ENDIF

        LOUTRL =IW(6)
        IF (LOUTRL.EQ.0) RETURN
        WRITE (LOUTRL,'(''1'',/,80A,/,T30,A,/,80A,/)')  BAR,
     &  ' EDIR Statistics ', BAR
        WRITE (LOUTRL,'(5X,''Total number of run records '',I8)')
     &    NRUNR
        WRITE (LOUTRL,1001) KCLAS+NOCLAS, KCLAS, NOCLAS
 1001   FORMAT( 5X,'Total number of events ',I8,5X,
     &   'with class word ',I8,5X,' without class word ',I8 )
        DO 10 ICL =  1,  NCLAS
          WRITE(LOUTRL,'(5X,I2,1X,A,I8)') ICL,TXTCL(ICL,ILEP),ISTAT(ICL)
   10   CONTINUE
        WRITE (LOUTRL,'(80A,/)') BAR
      ENDIF
C
  999 RETURN
C
C ------------------ ALSUMGT entry point ---------------------
      ENTRY ALSUMGT (NDIM,IARRAY)
      NFIL = MIN (NCLAS,NDIM)
      DO I =1,NFIL
         IARRAY(I) = ISTAT(I)
      END DO
      IF (NDIM.GE.NCLAS+1) IARRAY(NCLAS+1) = NOCLAS
      IF (NDIM.GE.NCLAS+2) IARRAY(NCLAS+2) = KCLAS
      IF (NDIM.GE.NCLAS+3) IARRAY(NCLAS+3) = NRUNR
C --------------------------------------------------------------
C
      END
#endif
