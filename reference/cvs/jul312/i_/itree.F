      SUBROUTINE ITREE(ITRY,MODE,IERR)
C-----------------------------------------------------------------------
C! ITC: form tree
C!
C!    Author      :- J. Sedgbeer 88/03/29
C!    Modified    :- I. Tomalin  89/02/28
C!
C!   Input:
C!     ITRY   /I : Search type flag.
C!     MODE   /I : = 1 if in TPC track extension phase.
C!                 = 2 if in ITC stand alone phase.
C!      commons:     /BCS/
C!                   /ITGAPC/
C!                   /ITRKER/
C!                   /IWBNKT/
C!
C!   Output:
C!      IERR  /I : IERR = 0 if ITREE ran correctly.
C!                 IERR = 1 if no elementary trees were found.
C!                 IERR = 2 if a more serious problem occured.
C!      commons:     /IWBNKT/
C!      work banks:  INNXIW,IOFNIW,INXLIW
C!
C!   called by : IGICHA and ITRAK
C!   calls     : none
C!
C!   Libraries required: none
C!
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
#include "rlunit.h"
C-----------------------------------------------------------------------
C I/O Commons etc.
#include "bcs.h"
#include "itgapc.h"
#include "itrker.h"
#include "iwbnkt.h"
C-----------------------------------------------------------------------
C Dummy common block to save work bank indices.
      COMMON/IDUMYF/ICLRIW
      LOGICAL SETZ
      DATA SETZ/.TRUE./
C-----------------------------------------------------------------------
#include "bmacro.h"
C----------------------------------------------------------------------
      IERR = 1
      IF (SETZ) THEN
        INNXIW = 0
        IOFNIW = 0
        INXLIW = 0
        ICLRIW = 0
        SETZ = .FALSE.
      END IF
      IF (LROWS(ILNKIW).EQ.0) GOTO 1000
C
C Initialise
C
      KITC0 = NLINK('ITCO',0)
      IF (KITC0.EQ.0) GOTO 1000
      NCO = LROWS(KITC0)
C
      IERR = 0
C
      NOLNK = LROWS(ILNKIW)
C The following bank will give the number of "next" links connected to
C each link.
      IF (INNXIW.GT.0) THEN
        CALL WDROP(IW,INNXIW)
        CALL RERROR('ITREE',1,'INNXIW work bank still exists')
      END IF
      CALL WBANK(IW,INNXIW,LMHLEN+NOLNK,*950)
      IW(INNXIW+LMHCOL) = 1
      IW(INNXIW+LMHROW) = NOLNK
C The following bank will give the offset to these connecting links in
C the INXLIW bank.
      IF (IOFNIW.GT.0) THEN
        CALL WDROP(IW,IOFNIW)
        CALL RERROR('ITREE',2,'IOFNIW work bank still exists')
      END IF
      CALL WBANK(IW,IOFNIW,LMHLEN+NOLNK,*950)
      IW(IOFNIW+LMHCOL) = 1
      IW(IOFNIW+LMHROW) = NOLNK
C The following bank will contain a list of the "next" links connecting
C to each link. It is pointed to by the IOFNIW bank. For the moment it
C is set to a sensible length and will be extended later if necessary.
      IF (INXLIW.GT.0) THEN
        CALL WDROP(IW,INXLIW)
        CALL RERROR('ITREE',3,'INNLIW work bank still exists')
      END IF
      CALL WBANK(IW,INXLIW,LMHLEN+NOLNK,*950)
      IW(INXLIW+LMHCOL) = 1
      IW(INXLIW+LMHROW) = 0
C Create coordinate to link relationship work bank.
      IF (ICLRIW.GT.0) THEN
        CALL WDROP(IW,ICLRIW)
        CALL RERROR('ITREE',4,'ICLRIW work bank still exists')
      END IF
      CALL WBANK(IW,ICLRIW,LMHLEN+2*NCO+1,*950)
      IW(ICLRIW+LMHCOL) = 1
      IW(ICLRIW+LMHROW) = 2*NCO + 1
C
      IOFF = ICLRIW + LMHLEN + NCO + 1
C
C Note where each coord. first appears in the link list (if anywhere).
C Use only the 1st coordinate in each link, not the 2nd.
C Also zero the pointer arrays.
      DO 5 I=LROWS(ILNKIW),1,-1
        IPNT = KROW(ILNKIW,I)
C       WRITE(LDEBRL,6001) I,(IW(IPNT+J),J=1,3),RW(IPNT+4)
 6001   FORMAT(' ITREE : Link ',I5,' has coords ',2I5,' IGAP=',I2,
     +  ' and curvature ',E10.3)
        ICO = IW(IPNT+1)
        IW(ICO+IOFF) = I
    5 CONTINUE
C
C Loop over all the links.
      DO 35 JLNK=1,LROWS(ILNKIW)
        JPNT = KROW(ILNKIW,JLNK)
C Find the link "KLNK" which shares a coord. with link JLNK.
        JCO = IW(JPNT+2)
        KLNK = IW(JCO+IOFF)
C If there wasn't one, then give up on link "JLNK".
        IF (KLNK.EQ.0) GOTO 35
        KPNT = KROW(ILNKIW,KLNK)
C Note the offset
        IW(IOFNIW+LMHLEN+JLNK) = LROWS(INXLIW)
C Start of loop over all links "KLNK" sharing coord.'s with link "JLNK".
   10   CONTINUE
C If in the ITC stand-alone phase, then check that links "JLNK" and
C "KLNK" have compatible curvatures.
        IF (MODE.EQ.2) THEN
          TJ = RW(JPNT+4)
          JGAP = IW(JPNT+3)
          TK = RW(KPNT+4)
          KGAP = IW(KPNT+3)
          ERRT = CCRVIT(IOGPIT(JGAP),IIGPIT(JGAP),IIGPIT(KGAP))
          IF (ABS(TJ-TK).GT.SIGNUM(ITRY)*ERRT) GOTO 25
        END IF
C Fill pointers to the next links of the elementary tree.
        IW(INNXIW+LMHLEN+JLNK) = IW(INNXIW+LMHLEN+JLNK) + 1
        IW(KNEXT(INXLIW)+1) = KLNK
        IF(FDEBRF.AND.JDBDRF(JULIT).GE.5) WRITE(LDEBRL,7001)
     +  IW(INNXIW+LMHLEN+JLNK),JLNK,KLNK,IW(JPNT+1),IW(JPNT+2),
     +  IW(KPNT+2),TJ,TK
 7001   FORMAT(' ITREE : Pointers ',6I5,2E13.3)
        IW(INXLIW+LMHROW) = IW(INXLIW+LMHROW) + 1
C Extend work bank if necessary.
        IF(LFRWRD(INXLIW).LE.0) THEN
          LEN = IW(INXLIW)
          IF (LEN.GT.500+50*NOLNK) GOTO 925
          CALL WBANK(IW,INXLIW,LEN+NOLNK,*950)
C Redefine IOFF and JPNT incase there was a garbage collection.
          IOFF = ICLRIW + LMHLEN + NCO + 1
          JPNT = KROW(ILNKIW,JLNK)
        END IF
   25   KLNK = KLNK + 1
        IF (KLNK.GT.NOLNK) GOTO 35
        KPNT = KROW(ILNKIW,KLNK)
        KCO = IW(KPNT+1)
        IF (JCO.EQ.KCO) GOTO 10
   35 CONTINUE
C
C End of loop.
C Compress INXLIW work bank.
      CALL WBANK(IW,INXLIW,LMHLEN+LROWS(INXLIW),*1000)
      GOTO 1000
C
C  Error returns
  925 CONTINUE
      IF(FDEBRF.AND.JDBDRF(JULIT).GE.1) CALL RERROR('ITREE',5,
     +  'Too many elementary trees found.')
      GOTO 975
  950 CONTINUE
      CALL RERROR('ITREE',6,'Insufficient BOS space.')
  975 CONTINUE
      IERR = 2
      IF (INNXIW.NE.0) CALL WDROP(IW,INNXIW)
      IF (IOFNIW.NE.0) CALL WDROP(IW,IOFNIW)
      IF (INXLIW.NE.0) CALL WDROP(IW,INXLIW)
C Normal ending
 1000 CONTINUE
      IF (ICLRIW.NE.0) CALL WDROP(IW,ICLRIW)
      END
#endif
