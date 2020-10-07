      SUBROUTINE KIBPRS (LIST)
C -----------------------------------------------------------
C - F.RANJARD - 860307
CKEY KINE KINGAL BANK COMPRESS  /   INTERNAL
C!  Compress the banks put on the list 'LIST', following the
C   convention : bank length = 1st word + 2nd word + 3rd word
C   the 1st word is the header length : LHD=3
C   if LHD .ne. 3 then the routine do nothing
C   if LHD .eq. 3 then the bank length is reduced to:
C           LEN = LHD + 2nd word + 3rd word
C   this routine is forseen to reduce the length of VERT and
C   KINE banks.
C
C - structure : SUBROUTINE subprogram
C               User Entry Name: KIBPRS
C               External References: NAMIND/NBANK/NLIST(BOS77)
C               Comdecks refenced: BCS
C
C - usage   : CALL KIBPRS (LIST)
C - input   : LIST  = list of bank names (should be 'VERTKINE')
C
#ifndef DOC
      SAVE
#include "bcs.h"
      EXTERNAL NLIST, NBANK
      CHARACTER LIST*(*), NAME*4, NLIST*4
C -------------------------------------------------------------
      I = 0
 1    I = I+1
      NAME = NLIST (IW,I,LIST)
      IF (NAME.EQ.' ') GOTO 999
      KNAME = NAMIND(NAME)+1
 2    KNAME = IW(KNAME-1)
      IF (KNAME.NE.0) THEN
         LE = IW(KNAME+1)+IW(KNAME+2)+IW(KNAME+3)
         IF (LE .LT. IW(KNAME)) THEN
            KNAME = NBANK (NAME,IW(KNAME-2),LE)
         ENDIF
         GOTO 2
      ENDIF
      GOTO 1
C
 999  CONTINUE
      RETURN
      END
#endif
