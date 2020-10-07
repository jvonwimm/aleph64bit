      integer function KWGTBK(IS,IWEI,wei)
C--------------------------------------------------------------------
C!  BOOK and fill bank KWGT with weight info
C      B. Bloch -Devaux January 1999
C     structure : integer function
C
C     input     : IS   row number to be filled
C                 IWEI weight number to be stored
C                 WEI  weight value  to be stored
C     output    : index of KWGT bank ( should be >0 if OK)
C                 KWGT bank is written to Event list
C
C--------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
C
#include "kwgtjj.h"
      INTEGER NAKWGT / 0 /
#include "bmacro.h"
C
C--------------------------------------------------------------
C
      KWGTBK = -1
      IF( NAKWGT.EQ.0 ) NAKWGT = NAMIND('KWGT')
C
C   Get KWGT index
      JKWGT = IW(NAKWGT)
      IF ( JKWGT.LE.0) THEN
C   Create KWGT bank
         CALL AUBOS('KWGT',0,LKWGTA+LMHLEN,JKWGT,IGARB)
         IF ( JKWGT.LE.0) GO TO 999
         IW(JKWGT+LMHCOL) = LKWGTA
         IW(JKWGT+LMHROW) = 1
         CALL BKFMT('KWGT','2I,(I,F)')
         CALL BLIST(IW,'E+','KWGT')
      ELSE
C  KWGT EXISTS, TEST THE LENGTH AND EXTEND IF NEEDED
         NKWGT=LROWS(JKWGT)
         IF ( IS.GT.NKWGT) THEN
           CALL AUBOS('KWGT',0,LKWGTA*IS+LMHLEN,JKWGT,IGARB)
           IF ( JKWGT.LE.0) THEN
              KWGTBK= -IS
              GO TO 999
           ELSE
              IW(JKWGT+LMHROW) = IS
           ENDIF
         ENDIF
      ENDIF
C  Fill KWGT BANK
      KKWGT = KROW(JKWGT,IS)
      IW(KKWGT+JKWGWN)   = IWEI
      RW(KKWGT+JKWGWV)   = WEI
      KWGTBK = JKWGT
C
 999  RETURN
      END
#endif
