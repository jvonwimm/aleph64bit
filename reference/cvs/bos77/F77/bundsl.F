      SUBROUTINE BUNDSL(LUN,NWPTR)
* - modified by F.Ranjard - 910603 - restore BOS original version
*                                    write several short $PTS
#include "boscom.h"
      INTEGER IPTS
      DATA IPTS/0/
*     internal routine to handle select output
*     to be called after data output
      IF(LUTDAT.EQ.0.OR.LUN.NE.LUTDAT) GOTO 100
*     LOOP ON $PTS BANKS
      IF(IPTS.EQ.0) THEN
         IPTS=NAMIND('$PTS')
      ENDIF
      IND=IPTS+1
   10 IND=IW(IND-1)
      IF(IND.EQ.0) GOTO 100
      IF(IW(IND-2).EQ.0) GOTO 10
      IF(IW(IND-2).EQ.LUNSEL) GOTO 10
*     a select output unit
      IV=IW(IND+IW(IND))
      MASKW=IW(IND+IW(IND)-1)
      IF(MASKW.EQ.0) GOTO 10
      IW(IND+IW(IND)-1)=0
*     add entry to bank
      IF(IV+5.GT.IW(IND)) THEN
*        BOS original version : write several small $PTS
            LUS=IW(IND-2)
            IND=NSWAP('$PTS',LUS,'$PTS',0)
*           write this bank on unit LUS ...
            CALL BWRSB(IW,LUS,'$PTS',0)
            IND=NSWAP('$PTS',LUS,'$PTS',0)
*           ... and clear bank content
            DO 20 I=1,IW(IND)
   20       IW(IND+I)=0
            IV=0
      END IF
*     store in bank
*     WRITE(6,123) IV,NWPTR,NRUN,NEVT
 123  FORMAT(' IV NWPTR NRUN NEVT ', 4I8)
      IW(IND+IV+1)=-IABS(NWPTR)
      IW(IND+IV+2)=MASKW-1
      IW(IND+IV+3)=NRUN
      IW(IND+IV+4)=NEVT
      IW(IND+IW(IND))=IV+4
      GOTO 10
  100 RETURN
      END
