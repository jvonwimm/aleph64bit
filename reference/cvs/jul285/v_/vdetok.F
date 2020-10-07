      LOGICAL FUNCTION VDETOK(DUMMY)
C----------------------------------------------------------------------
C!  - Determine HV state of VDET, derived from XVDEOK ALPHA fnct.
C!
C!   Author   :- HCJ Seywerd            2-AUG-1991
C!
C!   Outputs: Returns true if the HV state of the VDET is OK.
C!            For runs with run numbers below 12033, HV bit is not relia
C!            we use instead a cut on the number of hits per event.
C!            This is set at 500. This method is not absolutely reliable
C!            one cannot for instance tell if although HV is actually on
C!            pedestal updating has not finished acting, and we have an
C!            upward fluctuation. Purists may argue....
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "revhjj.h"
#include "rcurnt.h"
      INTEGER KVFHL, KREVH, JHVST
      LOGICAL FIRST, VBIT, VBIT90
      INTEGER IBVD, IBVD90
C For runs less then this there was only one HV bit:
      INTEGER CHRUN
      PARAMETER( CHRUN = 12089)
      INTEGER MAXHIT
      PARAMETER( MAXHIT = 500 )
C
      INTEGER TOTHIT
      DATA FIRST / .TRUE. /
C
#include "bmacro.h"
      IF (FIRST) THEN
        NAREVH = NAMIND('REVH')
        NAVFHL = NAMIND('VFHL')
      ENDIF
C
C  Detector HV status in word 1 of REVH
C  if REVH is missing the event is bad, declare detector's off.
C
      VDETOK = .FALSE.
      KREVH = IW(NAREVH)
      IF (KREVH.NE.0) THEN
        JHVST = ITABL(KREVH,1,JREVDS)
      ELSE
        JHVST = 0
        GOTO 999
      ENDIF
C Bits meanings are run dependent. Hardwired here!!!!!
      IF (IRUNRC .LT. CHRUN) THEN
        IBVD = 12
C        ibvd90 = 0   !!!NOT MEANINGFUL
      ELSE
        IBVD = 21
      ENDIF
C
      VBIT   = BTEST(JHVST, IBVD)
      IF (IRUNRC .GE. CHRUN) THEN
        IF (VBIT) THEN
          VDETOK = .TRUE.
        ENDIF
        GOTO 999
      ELSE
C
C Count number of VDET hits event.
        TOTHIT = 0
        KVFHL = IW(NAVFHL)
C
C
C Loop over all VFHL banks to get total number of coords
  100   CONTINUE
        IF (KVFHL.NE.0) THEN
          TOTHIT = TOTHIT + LROWS(KVFHL)
C         Next Bank
          KVFHL = IW(KVFHL-1)
        ELSE
          GOTO 200
        ENDIF
        GOTO 100
  200   CONTINUE
        IF (TOTHIT.LT.MAXHIT) THEN
          VDETOK = .TRUE.
          GOTO 999
        ENDIF
      ENDIF
  999 CONTINUE
      END
#endif
