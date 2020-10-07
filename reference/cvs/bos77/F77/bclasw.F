      SUBROUTINE BCLASW(LUN,ICLASS)

*     add pointer to record on unit LUNA
* modified by F.Ranjard - 910603 - restore original BOS code
*                                  to write several small $PTS
#include "boscom.h"
#include "bitconst.h"
*
*     index       Word content
*     IND + 1     record pointer     )
*         + 2     mask for classes   )
*         + 3     Run number         )
*         + 4     Event number       )
*         + 5     record pointer     and so on
*         ...     ............................
*      last-1     mask for classes (if ref to output data file)
*      last       internal pointer
*
      IF(NPTR.EQ.0) GOTO 100
      IND=NLINK('$PTS',LUN)
      IF(IND.EQ.0) THEN
*        default bank
         IND=NBANK('$PTS',LUN,1002)
         IF(IND.EQ.0) CALL BBSPC(IW,'BCLASW')
         CALL BKFMT('$PTS','I,J,2I')
         IV=0
      ELSE
         IV=IW(IND+IW(IND))
      END IF
*     class bit
      JCLASS=ICLASS
      IF(ICLASS.LT.0.OR.ICLASS.GT.30) JCLASS=0
      MASKW=0
      IF(JCLASS.NE.0) MASKW=IBITC(JCLASS)
*     special case for ref to output data
      IF(LUTDAT.NE.0) THEN
         IV=IW(IND)-1
*        store only class bit(s)
         MASKP=IW(IND+IV)
         IF(MASKP.NE.0) MASKP=MASKP-1
         IW(IND+IV)=IOR(MASKW,MASKP)+1
         GOTO 100
      END IF
      IF(IV.GT.0) THEN
         IF(IW(IND+IV-3)+NPTR.EQ.0) THEN
*           same record
            IW(IND+IV-2)=IOR(MASKW,IW(IND+IV-2))
            GOTO 100
         END IF
      END IF
      IF(IV+5.GT.IW(IND)) THEN
         IND=NSWAP('$PTS',LUN,'$PTS',0)
*        write this bank on unit LUN ...
         CALL BWRSB(IW,LUN,'$PTS',0)
         IND=NSWAP('$PTS',LUN,'$PTS',0)
*        ... and clear bank content
         DO 10 I=1,IW(IND)
   10       IW(IND+I)=0
         IV=0
      END IF
*     store in bank
      IW(IND+IV+1)=-IABS(NPTR)
      IW(IND+IV+2)=MASKW
      IW(IND+IV+3)=NRUN
      IW(IND+IV+4)=NEVT
      IW(IND+IW(IND))=IV+4
  100 RETURN
      END
