      SUBROUTINE GUSTEP
C ----------------------------------------------------------------------
C. - F.Ranjard - 850418
C. - GEANT3 user routine called at the end of every step in GTVOL
C.   and also when entering a new volume in GTRACK.
C.   At this level a track element is avalaible in the GEANT common
C.   block /GCTRAK/, /GCTMED/, /GCKINE/. Secondaries are
C.   stored in /GCKING/.
C.   If the track element is in a selected sensitive volume
C.   (IGSVOL>0 and SETS data card) the common /TRKCOM/ is
C.   filled in and control is transferred  to the corresponding
C.   module xxHIT.
C.   The current material parameters are stored in /GCMATE/
C.   Then in case of production of secondaries during the last tracking
C.   step , control is transferred to ASKTRK to store the secondaries
C.   either in the standard KINEmatics banks or in a temporary STACK.
C. - Called from    GTVOL, GTRACK                        from GEANT3 pam
C. - Calls          GSXYZ, GPCXYZ                        from GEANT3 pam
C.                  VDHIT, ITHIT, TPHIT, ECHIT, HCHIT    from this .HLB
C.                  MDHIT, LCHIT, SAHIT, VDHITE          from this .HLB
C.                  ASKTRK                               from this .HLB
C.                  UCOPY                                from CERN lib
C.                  CHAINT                     from BOS77.hlb
C.
C --------------------------------------------------------
#ifndef DOC
      SAVE
#include "agcons.h"
#include "iocom.h"
#include "jobcom.h"
#include "trkcom.h"
#include "gcking.h"
#include "gcnum.h"
#include "gcmate.h"
      LOGICAL FCALO
#include "gmacro.h"
C - particle which stops with a hadron mechanism
      FTRHAD = IGSTOP.NE.0 .AND. NGKINE.GT.1
     &        .AND. CHAHOL(KGCASE).EQ.'HADR'
     &        .AND. (IGTRTY.EQ.3 .OR. IGTRTY.EQ.4)
C ----------------------------------------------------------------------
C
C - IF the current particle stops for any reason other than
C   lost of its identy or energy below cut THEN  RETURN
C
      IF (IGSTOP.GE.NOMOR .OR. IGNWVO.EQ.3) RETURN
C
C - volume name
      TRKVOL = CHAHOL (NGAMES(NGLEVE))
C - definition of a calorimeter region where shower can be created
      FCALO = (TRKVOL(1:1).EQ.'C' .OR. TRKVOL(1:1).EQ.'E' .OR.
     &         TRKVOL(1:1).EQ.'H' .OR. TRKVOL(1:1).EQ.'L') 
     &          .AND. (TRKVOL.NE.'CDET')
C
C - IF it is a geantino THEN
C      IF 1st entry (bank does not exist) THEN
C         book the bank
C         create shower parameters
C         store parameters in bank 'CAPA',NR=IGTRA
C      ELSE (next entry)
C         get bank CAPA,NR=IGTRA
C         store bank content into /CAHTRA/
C      ENDIF
C   ENDIF
C
      IF (FTINO) THEN
         CALL CAPGET
      ENDIF
C
C - IF NOT in a calorimeter region THEN
C      IF it is a charged particle in a sensitive region THEN
C         fill the McTrackElement in /TRKCOM/
C         and call the right detector
C         and set the stop flag on return from detector module
C
      IF (.NOT. FCALO) THEN
         IF (FCHRG .AND. FSENS) THEN
#include "trkfill.h"
            IF(IGSVOL.EQ.IDETJO(1)) THEN
               CALL VDHIT
            ELSE IF(IGSVOL.EQ.IDETJO(2) ) THEN
               CALL ITHIT
            ELSE IF(IGSVOL.EQ.IDETJO(3) ) THEN
               CALL TPHIT
            ELSE IF(IGSVOL.EQ.IDETJO(9)) THEN
               CALL SIHIT
            ELSE IF(IGSVOL.EQ.IDETJO(8)) THEN
               CALL MUHIT
            ELSE IF(IGSVOL.EQ.IDETJO(6)) THEN
               CALL SAHIT
            ENDIF
            IGSTOP = ITRKEL(9)
         ENDIF
C
C - ELSE IF it is in a calo. sensitive region OR in the HCAL   AND
C           it is a charge particle OR a photon loosing its energy OR
C           it is a hadron stopping by a HADRon mechanism OR
C           it is a geantino THEN
C       fill the McTrackElement in /TRKCOM/
C       and call the current detector
C       and set the stop flag on return from detector module
C
       ELSEIF (FSENS.AND.(IGSVOL.EQ.IDETJO(5)).AND.
     +        (FCHRG.OR.FTINO))THEN
#include "trkfill.h"
         CALL LCHIT
         IGSTOP = ITRKEL(9)
       ELSEIF ((FSENS .OR. (TRKVOL(1:1).EQ.'H'))
     +  .AND. (FCHRG.OR.FGAMS.OR.FTRHAD.OR.FTINO)) THEN
         IF (FGAMS .OR. (.NOT.FCHRG .AND. FTRHAD)) FIRST=.TRUE.
#include "trkfill.h"
         IF (TRKVOL(1:1).EQ.'E') THEN
            IF (FIRST) THEN
               IF(CHAHOL(NGAMES(4)).EQ.'ECMB') THEN
                  ITRKEL(10)=8-NGUMBR(4)+(NGUMBR(4)/8)*12
               ENDIF
            ENDIF
            CALL ECHIT
         ELSE
            IF (FIRST) THEN
               IF(TRKVOL(2:2).EQ.'B') THEN
                  ITRKEL(10) = 2*(NGUMBR(4)-1) + NGUMBR(5)-1
                  IF (ITRKEL(10).EQ.0) ITRKEL(10) = 24
               ELSE
                  IF(CHAHOL(NGAMES(4)).EQ.'HCMB') THEN
                     ITRKEL(10)=4-NGUMBR(4)+(NGUMBR(4)/4)*6
                  ENDIF
               ENDIF
            ENDIF
            CALL HCHIT
         ENDIF
         IGSTOP = ITRKEL(9)
C
C - ELSE IF it is in a cal. non sensitive region AND geantino param.
C    AND it is a hadron stopping by a HADRon mechanism
C    fill the McTrackElement in /TRKCOM/
C    and call CALHIT
C
      ELSE IF (.NOT.FSENS .AND. FPARJO .AND. FTRHAD) THEN
         FIRST = .TRUE.
#include "trkfill.h"
         CALL CALHIT
         IGSTOP = ITRKEL(9)
      ENDIF
C
C - stop neutrons below 50.Mev
      IF (IGPART.EQ.13 .AND. GVECT(7).LT.0.050) IGSTOP = NOMOR
C
C - stop particles after 20 meters of tracking
      IF (GSLENG .GT. 2000.) IGSTOP = NOMOR
C - Keep secondaries
      IF( (FCHRG .AND. IGSTOP.NE.0 .AND. IGSTAK.EQ.0) .OR.
     &    (NGKINE.GT.0) ) THEN
         CALL ASKTRK
      ENDIF
C
C mark the end of a vdet track segment
C
      IF (FCHRG.AND.TRKVOL(1:1).EQ.'V'.AND.
     &    ((.NOT.FSENS).OR.IGSTOP.EQ.NOMOR)) CALL VDHITE

C
C - this is the end of the normal GUSTEP
C   the next routine ASTDEB is called in case of specific
C   work to be done by GALEPH : special output, debug, history
C   FGALJO is set in ASIJOB after reading of data cards
C
       IF (FGALJO) CALL ASTDEB
C
      GOTO 999
C
 999  CONTINUE
      END
#endif
