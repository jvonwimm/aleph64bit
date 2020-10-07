      SUBROUTINE ABWEVE
C --------------------------------------------------------------
C - F.Ranjard - 900927      from H.Albrecht
C! write output file with/without EDIR
CKEY ALPHARD WRITE
C ----------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "abrcom.h"
#include "bitconst.h"
C --------------------------------------------------------------
#ifndef DOC
  600 IF (IUTSEL .NE. 0)  THEN
        IF (MASKW .EQ. 0)  THEN
          CALL BCLASW (IUTSEL, 0)
        ELSE
          DO 610 I=1,30
  610     IF (IAND (MASKW, IBITC(I)) .NE. 0)  CALL BCLASW (IUTSEL, I)
        ENDIF
      ENDIF
C       write FILM card if 1st event of EDIR on input file
      IF (FWFILM) CALL CRFILM
      FWFILM = .FALSE.
C
      IF (IUTDAT(1) .NE. 0)  THEN
        CLOSE2 = .TRUE.
        IF(WLIST.EQ.'E') CALL CMPLIS (WLIST, I)
        CALL BWRITE (IW, IUTDAT(1), WLIST)
      ENDIF
      WLIST = ' '
C
      END
#endif
#endif
