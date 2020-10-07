*DK echit
      SUBROUTINE ECHIT
C -------------------------------------------------------------
C - J.Badier  F.Ranjard - 870817
C! ECAL analog signal steering
C - Initialize BOS banks at beginning of event
C - Get sub-component # and module #
C - IF it is a geantino particle THEN treat it and RETURN
C   ELSE treat the McTreackElement stored in /TRKCOM/
C - IF shower parametrization is required AND it is
C         a photon which stops loosing all its energy
C      OR a hadron which stops because of an hadronic
C         mechanism
C   THEN treat the shower parametrization depending of the
C      type of parametrization chosen.
C - called by   GUSTEP                     from GEANT3 lib
C - calls       EHSHOW, EHTRKE, EHCUTF
C               EHCUTG, CATINO             from this .HLB
C               ELIMSC                     from ALEPHLIB
C --------------------------------------------------
#ifndef DOC
      SAVE
#include "jobcom.h"
#include "tmacrod.h"
      EQUIVALENCE (MODUL,ITRKEL(10))
      LOGICAL FNOPA
C
      DATA NCALL /0/
C
#include "tmacrof.h"
C --------------------------------------------------------------
C - NO parametrizatio required
      FNOPA = ICECJO(5).EQ.0
C --------------------------------------------------------------
C
C - Initialize BOS banks at beginning of event
C
      IF (FBEGJO(4)) THEN
          IF (NCALL.EQ.0) THEN
             NCALL = 1
             CALL ELIMSC(ZECA,ZECB)
           ENDIF
           FBEGJO(4) = .FALSE.
           CALL EHIBOS
        ENDIF
C
C - Find sub-component : 1 end cap z>0, 2 barril, 3 end cap z<0
C   the module # is stored in ITRKEL(10)
C
      ISCMP = 1
      IF (TRKELE(3).LT.ZECA) ISCMP = 2
      IF (TRKELE(3).LT.ZECB) ISCMP = 3
C
C -------------------------------------------------------------
C
C - IF it is a geantino THEN
C      IF it is the 1st entry (no shower param. defined) THEN
C         create parameters, compute XYZ, deposit E
C      THEN RETURN
C
      IF (FTINO) THEN
         IF ( ITRKEL(8).GE.2) CALL EHSHOW(ISCMP,MODUL)
         GOTO 999
C - ELSE it is not a geantino : treat the track element
C
      ELSE
         IF (ITRKEL(8).NE.1) CALL EHTRKE (ISCMP,MODUL)
      ENDIF
C
C -------------------------------------------------------------
C - IF NO shower parametrization is required THEN RETURN
      IF (FNOPA) GOTO 999
C
C - IF it is a charged hadron which does not make an had.interaction
C   THEN RETURN
      IF (FHADC .AND. .NOT.FTRHAD) GOTO 999
C
C - IF it is a muon THEN RETURN
      IF (FMUON) GOTO 999
C --------------------------------------------------------------
C
C - shower parametrization is considered
C
C - IF it is an e+/e- THEN decide wheither Yes/No a shower can
C      be developped , IF Yes THEN develop the shower, stop the
C      current particle and RETURN
C
      IF (FELEC) THEN
         IF(ITRKEL(8).EQ.1)  THEN
           DO 1 I=1,LNTRK
             TRKNXT(I) = TRKELE(I)
    1      CONTINUE
         ENDIF
         CALL EHCUTF (MFLAG)
         IF (MFLAG.EQ.1) THEN
            CALL EHSHOW (ISCMP,MODUL)
            ITRKEL(9) = NOMOR
            GOTO 999
         ENDIF
      ENDIF
C
C - last case it is an e+/e- with a shower not fully contained (MFLAF=0)
C    OR a photon OR a hadron : the action depends on the type of
C   parametrization.
C   IF geantino parametrization (FPARJO is true) THEN try to create
C      a geantino particle.
C
      IF (FPARJO) THEN
         CALL EHCUTG (MFLAG)
         IF (MFLAG.EQ.1) THEN
             CALL CATINO
             ITRKEL(9) = NOMOR
          ENDIF
      ENDIF
C
 999  CONTINUE
C
C    length vs energy for the track element of an e+ or e-
C                    or a photon
C
       IF(FHISJO(4)) THEN
           WSKI=1.
           IDSKI=439
           IF(ITRKEL(4).EQ.1) THEN
               CALL HFILL(IDSKI,TRKELE(12),TRKELE(11),WSKI)
           ELSE IF(ITRKEL(4).EQ.2) THEN
               CALL HFILL(IDSKI+1,TRKELE(12),TRKELE(11),WSKI)
           ENDIF
       ENDIF
C
      END
#endif
