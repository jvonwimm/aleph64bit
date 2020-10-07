      SUBROUTINE QFILL
CKEY FILL /INTERNAL
C-----------------------------------------------------------------------
C! fill QVEC and QVRT
C!called from QMEVNT
C!                                                   H.Albrecht 20.09.88
C!                                                   E.Blucher  01.02.92
C!                                                   G.Graefe   22.04.94
C!                                                   P.Perret   11.02.97
C!                                                   J.Boucrot  23.02.98
C!                                                   F.Ligabue  16.07.98
C-----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
C-----------------------------------------------------------------------
C
      CALL QURESC
C
C       If we have a Mini or a Nano, unpack into POT/DST banks.
C
      IF (XMINI) THEN
        CALL MINFIL
      ELSE IF (XNANO) THEN
        CALL NANFIL
      ELSE
C if PCPA banks have to be filled :
        IF (XFILPC) CALL PCPATQ(IER)
      ENDIF
C
C   DEAL with FRFT banks and FRxy data cards:
C
      CALL QFFRFT         
C
C       reset ALPHA buffers if necesssary (normally done in QMREAD)
C
      IF (KFFILL .NE. 0)  CALL QMCLR
C
C       fill header
C
      CALL QFHEAD
C
C       fill reconstructed charged tracks
C
      IF(XFILCH)THEN
        IF (IW(NAFRFT) .NE. 0)  THEN
          CALL QFCHGD
        ENDIF
      ENDIF
C
C
C       fill calorimeter objects ECAL and HCAL
C
      IF(XFILCO)THEN
        IF (IW(NAPECO).NE.0.OR.IW(NAPHCO).NE.0)  THEN
          CALL QFCALD
        ENDIF
        IF(IW(NADEWI).NE.0)THEN
          CALL QFECWM
        ELSEIF (IW(NAPEWI).NE.0.OR.NLINK('PWEI',0).NE.0) THEN
          CALL QFECWD
        ENDIF
      ENDIF
C
C       fill bit masks for reconstructed objects
C
      CALL QFBITM
C
C       fill reconstructed vertices and V0's
C
      IF(XFILV0)THEN
        CALL QFREV0
      ENDIF
C
C       fill special vertices from 1997 new tracking:
C
      CALL QFILNV
C
      KNRET = KLRET - KFRET + 1
      KNCOT = KLCOT - KFCOT + 1
C
C       fill EFT section of QVEC
C
      IF(XFILEF)THEN
        IF ( KEFOPT.NE.2 .AND. (.NOT.XMINI) ) THEN
C correct tracks b4 E-flow is called 
          IF ( NLINK('PCOR',0) .GT. 0 ) CALL QTRESC
          IF ( NLINK('RENF',0) .GT. 0 ) IND = NDROP('EFOL',3)
          IF ( NLINK('EFOL',3) .LE. 0 ) CALL QUEFLO
        ENDIF
        CALL QFEFLO
      ENDIF
C
C       fill NET section of QVEC
C
      IF(XFILPC)THEN
        CALL QFNEOB
      ENDIF
C
C       fill GAT section of QVEC
C
      IF(XFILGA)THEN
        CALL QFGAMP
C Photon energies on NanoDst are already corrected:
        IF(.NOT.XNANO) CALL FIXGAEN
      ENDIF
C
C   fill jet section
C
      IF(XFILJE)THEN
        CALL QFJETS
      ENDIF
C
C       fill Monte Carlo truth
C
      IF(XFILMC)THEN
        CALL QFILMC
      ENDIF
C
C fill tags and jets for leptons from QSELEP when reading a MINI or a NANO
C
      IF (XMINI.OR.XNANO) CALL QFLEPT
C
C        fill the event-by-event beam position:
C
      CALL QFILBP
C
C Recalculate d0,z0 of charged tracks according to beam position:
C
      CALL QFD0Z0
C
C Recalculate RT and RL for electron candidates , when necessary
C
      CALL FIXRTRL
C
C Recalculate Rl for 1996 MC events :
C
      CALL FIXPRL
C
      KFFILL = 1
C
C
C   Heavy flavour lepton selection :
C
        CALL QLEVNT
C
      END
#endif
