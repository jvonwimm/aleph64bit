*DK sihit
      SUBROUTINE SIHIT
C-------------------------------------------------------------
C! Controls tracking in SCal
C. - B.Bloch-Devaux 910115
C. - Called by   GUSTEP                          from this .HLB
C. - Calls       BDROP,BLIST                     from BOS lib
C. - Calls       ALBOS                           from ALEPHLIB
C. - Calls       SISHOW,SITRAK                   from this LIB
C.
C --------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "trkcom.h"
#include "jobcom.h"
#include "sistat.h"
#include "siparm.h"
      PARAMETER ( LSROW = 100)
      PARAMETER ( NOMOR = 3)
#include "sihijj.h"
      LOGICAL FSIAC ,FA,FB
C   definition of the SICAL active area (crystal plane)
      FSIAC = TRKVOL.EQ.'SSIL'
C -------------------------------------------------------------
      IF(FBEGJO(9)) THEN
C - Drop existing banks
         CALL BDROP (IW,'SIHI')
C
C - Book hit banks at the beginning of events
C
         LEN = LSROW *LSIHIA+LMHLEN
         CALL ALBOS('SIHI',0,LEN,JSIHI,IGARB)
         IW(JSIHI+LMHCOL) = LSIHIA
C - Add to the 'E' list
         CALL BLIST(IW,'E+','SIHI')
C
C - Set SI print flag
         IF(FDEBJO) THEN
            IF (IPRIJO(9) .NE. 0) NSIPRT = 1+ICSIJO(1)
         ELSE
            NSIPRT = 0
         ENDIF
C
C - Count events with tracks hitting SCAL
         NSICOU(1) = NSICOU(1) +1
         FA = .TRUE.
         FB = .TRUE.
C - Set begin of event flag to false
         FBEGJO(9) = .FALSE.
      ENDIF
C - Count events with tracks hitting SCAL in end A and B
      IF ( TRKELE(3).GT.0.) THEN
         IF ( FA) THEN
            NSICOU(2) = NSICOU(2) +1
            FA = .FALSE.
         ENDIF
      ELSE
         IF ( FB) THEN
            NSICOU(3) = NSICOU(3) +1
            FB = .FALSE.
         ENDIF
      ENDIF
C
C - Parametrize electron/positron if parametrization has been selected
C   otherwise track particle in active region
      IF ( SIPARF .AND. ITRKEL(11).EQ.2) THEN
        CALL SISHOW
        ITRKEL(9) = NOMOR
      ELSE IF (FSIAC) THEN
        CALL SITRAK
      ENDIF
C
  999 CONTINUE
      RETURN
      END
#endif
