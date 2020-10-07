*DK tphsrt
      SUBROUTINE TPHSRT(NAMH,NAMR)
C -------------------------------------------------------------------
C. - M.Mermikides -  860202
C.
C. -  Sorts hit bank and track element ref. bank in increasing radius
C.    NAMH is name index of hit bank to be sorted
C     NAMR  "   "    "      track element ref. bank
C.    (NB It is implied that the 2nd word (ChanID) is the sort key)
C.
C. -  Calls           NDROP,WBANK,WDROP              from BOS77 lib
C.                    UCOCOP,SORTZV                  from CERN lib
C -------------------------------------------------------
#ifndef DOC
      SAVE
#include "jqcom.h"
#include "iocom.h"
      COMMON /JDTLOC/ JDSORT,JDWORK
C
#include "bmacro.h"
      JHIT(IH) = KROW (KHIT,IH)
C
C -------------------------------------------------------------------
      JDSORT = 0
      JDWORK = 0
C
      KHIT = IW(NAMH)
C
C  Exit if no bank
C
      IF (KHIT.EQ.0) GO TO 999
      NWPHT = IW(KHIT + 1)
      NHITS = IW(KHIT + 2)
      IF (NHITS.LT.2) GO TO 999
C
C  Book work banks for sorting
C
      CALL WBANK(IW,JDSORT,NHITS,*800)
      CALL WBANK(IW,JDWORK,NHITS,*800)
C
C Sort according to channel number
C
      DO 150 IH = 1,NHITS
         IW(JDWORK + IH) = IW(JHIT(IH) + 2)
150   CONTINUE
C
      CALL SORTZV(IW(JDWORK+1),IW(JDSORT+1),NHITS,-1,0, 0,0,0)
C
C  Replace bank contents according to key order
      DO 11 K=1,NWPHT
         CALL UCOCOP(IW(KHIT+2+K),IW(JDWORK+1),NHITS,1, NWPHT, 1)
         DO 14 I = 1,NHITS
            IND = IW(JDSORT + I)
            IW(JHIT(I) + K) = IW(JDWORK + IND)
14       CONTINUE
11    CONTINUE
C
C  Now sort track element reference bank (if it exists)
C
      KHITR = IW(NAMR)
      IF(KHITR.EQ.0) GO TO 900
      KHE = KHITR + 2
      CALL UCOPY(IW(KHE+1),IW(JDWORK+1),NHITS)
      DO 175 I=1,NHITS
         IND = IW(JDSORT + I)
         IW(KHE + I) = IW(JDWORK + IND)
175   CONTINUE
C
      GO TO 900
C
C  Error
 800  CONTINUE
      CALL WDROP (IW,JDSORT)
      CALL WDROP (IW,JDWORK)
      CALL ALTELL ('TPHSRT: not enough space to book JDSORT/JDWORK ',
     &              1,'NEXT')
C
C  End
900   CALL WDROP(IW,JDSORT)
      CALL WDROP(IW,JDWORK)
999   RETURN
      END
#endif
