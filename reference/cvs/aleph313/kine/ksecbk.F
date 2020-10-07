      integer function KSECBK(IS,IDC,IVER,NTOT,NACC,XTOT,RTOT,XACC,RACC)
C--------------------------------------------------------------------
C!  BOOK and fill bank KSEC with cross section info
C      B. Bloch -Devaux December 1998
C     structure : integer function
C
C     input     : IS   index of cross-section to be stored
C                 IDC  generator id
C                 IVER generator version
C                 NTOT generated events
C                 NACC accepted events
C                 XTOT generated cross-section (nb)
C                 RTOT generated cross-section error (nb)
C                 XACC accepted cross-section (nb)
C                 RACC accepted cross-section error (nb)
C
C     output    : index of KSEC bank ( should be >0 if OK)
C                 KSEC bank is written to Run Header list
C
C--------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
      INTEGER IBSH(4)
      REAL RBSH(4)
#include "ksecjj.h"
#include "bmacro.h"
C
C--------------------------------------------------------------
C
      KSECBK = -1
C
      IBSH(1) = IDC
      IBSH(2) = IVER
      IBSH(3) = NTOT
      IBSH(4) = NACC
      RBSH(1) = XTOT
      RBSH(2) = RTOT
      RBSH(3) = XACC
      RBSH(4) = RACC
C   Get KSEC index
      JKSEC = IW(NAMIND('KSEC'))
      IF ( JKSEC.LE.0) THEN
C   Create KSEC bank
         CALL AUBOS('KSEC',0,LKSECA+LMHLEN,JKSEC,IGARB)
         IF ( JKSEC.LE.0) GO TO 999
         IW(JKSEC+LMHCOL) = LKSECA
         IW(JKSEC+LMHROW) = 1
         CALL BKFMT('KSEC','2I,(4I,4F)')
         CALL BLIST(IW,'C+','KSEC')
      ELSE
C  KSEC EXISTS, TEST THE LENGTH AND EXTEND IF NEEDED
         NKSEC=LROWS(JKSEC)
         IF ( IS.GT.NKSEC) THEN
           CALL AUBOS('KSEC',0,LKSECA*IS+LMHLEN,JKSEC,IGARB)
           IF ( JKSEC.LE.0) THEN
              KSECBK= -IS
              GO TO 999
           ELSE
              IW(JKSEC+LMHROW) = IS
           ENDIF
         ENDIF
      ENDIF
C  Fill KSEC BANK
      KKSEC = KROW(JKSEC,IS)
      DO 10 II=1,4
         IW(KKSEC+II)   = IBSH(II)
         RW(KKSEC+II+4) = RBSH(II)
  10  CONTINUE
      KSECBK = JKSEC
C
 999  RETURN
      END
#endif
