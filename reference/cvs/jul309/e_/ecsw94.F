      SUBROUTINE ECSW94
C-
C-    M.N Minard  7/7/94
C-    Active only on 1994 raw data Run<25894
C!     Swap Gain for ETDI
C     Input Bank ETDI , used GECA
C     Bank modified ETDI
C!
C!    Modified M.Cattaneo 980424
C!          EXIT JULIA if EAGC bank is missing when required 
C=============================================
#ifndef DOC
#include "bcs.h"
#include "etdijj.h"
#include "rhahjj.h"
#include "rcurnt.h"
#include "eagcjj.h"
      EXTERNAL CHAINT
      CHARACTER*4 CHAINT
      CHARACTER*4 NAMJUL
      CHARACTER*8 PRNAM
      DIMENSION XCOE(2,3,384,50)
      DIMENSION XCF(115200)
      EQUIVALENCE(XCOE(1,1,1,1),XCF(1))
      DATA IFIR , IFIRI / 0 , 0 /
#include "bmacro.h"
C
C-    Check if Run range to be swapped
C
      IF (IRUNRC.GE.25000.AND.IRUNRC.LT.25894) THEN
C     Check we deal with raw data
C-
      IF ( IFIRI.EQ.0) THEN
      IFIRI = 1
      NARHAH = NAMIND ('RHAH')
      ENDIF
      KRHAH = IW(NARHAH)
      NRHAH=LROWS(KRHAH)
      IRAW  = 0
      DO 10 IRHAH = 1, NRHAH
        PRNAM(1:4) = CHAINT(ITABL(KRHAH,IRHAH,JRHAPN))
        PRNAM(5:8) = CHAINT(ITABL(KRHAH,IRHAH,JRHAPN+1))
        IF (PRNAM.EQ.'ON LINE')THEN
          IRAW  = IRHAH
        ENDIF
 10   CONTINUE
      IF (IRAW.NE.NRHAH-1) GO TO 999
C
C-    Read EAGC bank
C
      IF ( IFIR.EQ.0) THEN
         NAEAGC = NAMIND('EAGC')
         KEAGC = IW(NAEAGC)
         IF (KEAGC.NE.0) THEN
          CALL UCOPY(RW(KEAGC+3),XCF(1),115200)
         ELSE
C
C-  Stop Julia if EAGC is missing for Run range 25000-25893
C
          WRITE(IW(6),1001)
 1001     FORMAT(' JULIA is stopping because an EGCA card is mandatory',
     &      ' for run range 25000-25893',/,
     &      ' Please add the cards ALWS::DBASE:EGCA.CARDS to the',
     &      ' Julia cards' )
          CALL ALTELL('Julia stopping...',1,'FATAL')
         ENDIF
         IFIR = 1
         NAETDI = NAMIND('ETDI')
      ENDIF
      KETDI = IW(NAETDI)
      NETDI = 0
      IF ( KETDI.NE.0) NETDI = LROWS(KETDI)
      ESCAL0 = 0.
      ESCAL1 = 0.
      DO IETDI = 1,NETDI
        JETDI = KROW(KETDI,IETDI)
        IADD = IW(JETDI+1)
        ITET = IBITS(IADD,16,8)
        IPHI = IBITS(IADD,2,9)
        IF ( ITET.LT.50) THEN
          DO KS =1,3
            ERP = FLOAT(ITABL(KETDI,IETDI,JETDTL+KS))/1000000.
            ILEVEL = 2
            IF(ERP.GT.8.) ILEVEL = 1
            COE = XCOE(ILEVEL,KS,IPHI,ITET)
            ERP = ERP*COE*1000000.
            IW(JETDI+JETDTL+KS) = INT(ERP)
          ENDDO
        ENDIF
      ENDDO
      ENDIF
999   CONTINUE
      RETURN
      END
#endif
