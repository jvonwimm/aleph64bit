      SUBROUTINE VSLTRK
C-------------------------------------------------------------
C! Select good outer tracks for use in VDET pattern recognition
CKEY VGLOB VDET TRACK
C
C  Author      : Paul Rensing  27-6-95
C
C  Subroutine to select which outer tracks to extrapolate into
C     the VDET for Pat. Rec.  Use the information in the FXTR bank, 
C     plus D0, Z0, and no. of TPC hits on the track. 
C     The flag is stored in colunm JFXTVD of the FXTR bank. 
C
C  INPUT: 
C     FXTR, FRFT, FRTL banks 
C  OUTPUT:
C     FXTR bank is modified.
C
C-------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C
C  Global variables
C
#include "bcs.h"
#include "vglbcm.h"
C
#include "frftjj.h"
#include "frtljj.h"
#include "fxtrjj.h"
C
C  Function calls
C
      INTEGER NLINK, NBANK, NAMIND
      EXTERNAL NLINK, NBANK, NAMIND
C
C Flag to indicate never look at a track
C
      integer BADFLAG
      parameter (BADFLAG = -1)
C
      INTEGER NTPCCUT
      PARAMETER (NTPCCUT = 4)
      REAL Z0CUT
      PARAMETER (Z0CUT = 10.0)
      REAL D0CUT1, D0CUT2
      PARAMETER ( D0CUT1 = 2.0, D0CUT2 = 12.0 )
C
C  Local variables
C
      INTEGER IndFXTR, IndFRFT, IndFRTL
      INTEGER NamFXTR, NamFRFT, NamFRTL
      SAVE NamFXTR, NamFRFT, NamFRTL
      INTEGER NTRK, ITRK, I
      REAL Z0, D0
      INTEGER JFXTR
C
#include "bmacrod.h"
#include "bmacro.h"
C
C  Find the banks
C
      if (NamFRFT .eq. 0) then
         NamFRFT = NAMIND('FRFT')
         NamFRTL = NAMIND('FRTL')
         NamFXTR = NAMIND('FXTR')
      end if
C
      IndFRFT = IW(NamFRFT)
      IndFRTL = IW(NamFRTL)
      IndFXTR = IW(NamFXTR)
      IF (IndFRFT.LE.0 .OR. IndFRTL.LE.0 .or. IndFXTR .le. 0) Then
         call RERROR('VSLTRK', -1,' Missing Track banks')
         goto 999
      end if
C
C  Loop over the tracks
C
      NTRK = LROWS(IndFRFT)
      DO ITRK = 1, NTRK
         JFXTR = KROW(IndFXTR, ITRK)
         IW(JFXTR + JFXTVD) = 0
C
         IF (PCKTRK) THEN
C
C  Count the number of TPC hits; if less than 4, the track is bad
C
            IF (ITABL(IndFRTL,ITRK,JFRTNT) .LT. NTPCCUT) THEN
               IW(JFXTR + JFXTVD) = BADFLAG
               GOTO 888
            END IF
C
C  Check the point of closest approach to the primary vertex for
C  deciding if this is a good track or not 
C
            Z0 = RTABL(IndFRFT, ITrk, JFRFZ0)
            D0 = RTABL(IndFRFT, ITrk, JFRFD0)
            if (abs(D0) .gt. D0CUT2) then
               IW(JFXTR + JFXTVD) = BADFLAG
               GOTO 888
            else if (abs(Z0) .gt. Z0CUT .or. abs(D0) .gt. D0CUT1) then
               IW(JFXTR + JFXTVD) = 5
            end if
C
C Check V0, Kink, Nucl. Int. searches
C
            if (IW(JFXTR + JFXTV0) .ne. 0) then
               IW(JFXTR + JFXTVD) = BADFLAG
            end if
            if (IW(JFXTR + JFXTNC) .ne. 0) then
               IW(JFXTR + JFXTVD) = BADFLAG
            end if
            if (IW(JFXTR + JFXTKN) .ne. 0) then
               IW(JFXTR + JFXTVD) = BADFLAG
            end if
         END IF
C
C  Done for this track
C
 888     CONTINUE
      END DO
C
C  Done
C
 999  CONTINUE
      RETURN
      END
#endif
