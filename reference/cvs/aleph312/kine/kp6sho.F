      integer function KP6SHO(ISH,ISHO,IP1,IP2,ICR)
C--------------------------------------------------------------------
C!  BOOK and fill bank KSHO with fragmentation info
C      B. Bloch -Devaux November 1997
C     structure : integer function
C
C     input     : ISH  index of shower in event (1,2,...)
C                 ISHO current line number of shower  in LUJETS common
C                 IP1  current line number of parton1 in LUJETS common
C                 IP2  current line number of parton2 in LUJETS common
C                 ICR  Color reconnection flag I= CR happened in scheme I
C                                              0= No CR happened
C
C     output    : index of KSHO bank ( should be >0 if OK)
C                 KSHO bank is written to Event list
C     comdecks referenced : LUN7COM
C--------------------------------------------------------------------
#ifndef DOC
#include "pyt6com.h"
#include "bcs.h"
      INTEGER IBSH(4)
#include "kshojj.h"
#include "bmacro.h"
C
C--------------------------------------------------------------
C
      KP6SHO = -1
C       remove beam particle from numbering if needed
      IBEA = 0
      DO 27 ITR=1,N7LU
        KS = K7LU(ITR,1)
        IF ( KS.EQ.21 .AND. ABS(K7LU(ITR,2)).EQ.11 ) IBEA = IBEA +1
 27   CONTINUE
      IBSH(1) = ISHO - IBEA
      IBSH(2) = IP1  - IBEA
      IBSH(3) = IP2  - IBEA
      IBSH(4) = ICR
C   Get KSHO index
      JKSHO = IW(NAMIND('KSHO'))
      IF ( JKSHO.LE.0) THEN
C   Create KSHO bank
         CALL AUBOS('KSHO',0,LKSHOA+LMHLEN,JKSHO,IGARB)
         IF ( JKSHO.LE.0) GO TO 999
         IW(JKSHO+LMHCOL) = LKSHOA
         IW(JKSHO+LMHROW) = 1
         CALL BKFMT('KSHO','I')
         CALL BLIST(IW,'E+','KSHO')
      ELSE
C  KSHO EXISTS, TEST THE LENGTH AND EXTEND IF NEEDED
         NKSHO=LROWS(JKSHO)
         IF ( ISH.GT.NKSHO) THEN
           CALL AUBOS('KSHO',0,LKSHOA*ISH+LMHLEN,JKSHO,IGARB)
           IF ( JKSHO.LE.0) THEN
              KSHOBK= -ISH
              GO TO 999
           ELSE
              IW(JKSHO+LMHROW) = ISH
           ENDIF
         ENDIF
      ENDIF
C  Fill KSHO BANK
      KKSHO = KROW(JKSHO,ISH)
      DO 10 II=1,4
         IW(KKSHO+II) = IBSH(II)
  10  CONTINUE
      KP6SHO = JKSHO
C
 999  RETURN
      END
#endif
