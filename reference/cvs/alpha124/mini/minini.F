      SUBROUTINE MININI(NWANT,WLIST)
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Mini code initialisation.
C
C     Author: Stephen Haywood      06-Jan-93
C
C     Output : NWANT  = number of banks listed on MINI card
C                     = 1 if no MINI card
C              WLIST  = list of banks on MINI card
C                     = ' ' if no MINI card
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "mincom.h"
C
      CHARACTER*800 WLIST
      CHARACTER*4 CHAINT,BANK
      CHARACTER*75 FORM
      INTEGER ALFMT
C
#include "bmacro.h"
C------------------------------------------------------------------------
C++   Print the version number.
C
      WRITE (IW(6),'(//40X,''**** Mini-DST version'',I5,'' ****'')')
     &       MINVSN(DUMMY)
C
C++   Identify which of the standard Mini banks are wanted from MINI
C++   card. Default is all.
C++   Note: EVEH and DVRS are forced to be copied to o/p.
C++   Note: NWANT=1 is necessary even if WLIST is empty because of
C++   statement function in MINBLD.
C
      WLIST = ' '
      KMINI = NLINK('MINI',0)
      NMINI = IW(KMINI)
      IF (NMINI.GT.0) THEN
         WLIST(1:4) = 'EVEH'
         WLIST(5:8) = 'DVRS'
         NWANT = 2
         DO I=1,NMINI
            IF (NWANT.LT.MAXCHA/4) THEN
               BANK = CHAINT(IW(KMINI+I))
               IF (BANK.NE.'EVEH' .AND. BANK.NE.'DVRS') THEN
                  NWANT = NWANT + 1
                  WLIST(4*NWANT-3:4*NWANT) = CHAINT(IW(KMINI+I))
               ENDIF
            ENDIF
         ENDDO
      ELSE
         NWANT = 1
      ENDIF
C
C++   Define run list.
C
      CALL MINRUN
C
      RETURN
      END
C
#endif
