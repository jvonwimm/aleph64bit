      SUBROUTINE ECSW1093
C----------------------------------------------------------------------
C!  - Fix ETDI swapping in end-cap for runs 22793 - 22880
C!
C!   Author   :- M.N. Minard              29-NOV-1993
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rcurnt.h"
#include "ecnamc.h"
#include "etdijj.h"
#include "rhahjj.h"
      DIMENSION ITMN(2), ITMX(2)
      DIMENSION IPHCH(2,3)
      EXTERNAL CHAINT,NAMIND
      CHARACTER*4 CHAINT
      CHARACTER*4 PRNAM
      SAVE NARHAH
      SAVE ITYPE, ICVER
      SAVE IRUNO
      DATA ITMN   / 32 , 40 /
      DATA ITMX   / 40 , 51 /
      DATA IPHCH  / 48, 64, -24, -32, -24, -32/
      DATA NL     / 2 /
      DATA ITECH  / 32 /
      DATA IFIR   / 0 /
      DATA ICREF  / 27107 /
      DATA IRUNO  / 0 /
#include "bmacro.h"
      IF ( IFIR.EQ.0)THEN
        NARHAH = NAMIND('RHAH')
        IFIR  = 1
        ITYPE = 0
        ICVER = 0
      ENDIF
C
C-    CHECK DATA RANGE
C
      IF (IRUNRC.LT.22793.OR.IRUNRC.GT.22880) GO TO 999
C
C?   Is there ETDI ????
C
      KETDI = IW(NAETDI)
C
C-   CHECK JULIA VERSION and input type
C
      KRHAH = IW(NARHAH)
      IF ( KRHAH.LE.0 ) GOTO 999
C
      IF ( IRUNRC.NE.IRUNO ) THEN
        IRUNO = IRUNRC
        ICVER = 0
        NRHAH = LROWS(KRHAH)
        DO 10  IRHAH = 1, NRHAH
          PRNAM = CHAINT(ITABL(KRHAH,IRHAH,JRHAPN))
          IF (PRNAM.EQ.'JULI') ICVER = ITABL(KRHAH,IRHAH,JRHACV)
   10   CONTINUE
       ITYPE = ITABL (KRHAH, NRHAH, JRHANI)
      ENDIF
C
C?   Correction is applied if :
C?      - raw data in input : itype =2
C?      - POT/DST processed with julia version < 27107
C
      IF ( (ITYPE.EQ.2) .OR. (ITYPE.NE.2.AND.ICVER.LT.ICREF) ) THEN
C
C-    DECODE ETDI BANK
C
        NETDI = LROWS(KETDI)
        DO IETDI = 1,NETDI
          JETDI = KROW(KETDI,IETDI)
          IADDS = ITABL(KETDI,IETDI,JETDTL)
          ITET = IBITS(IADDS,16,8)
          IPHI = IBITS(IADDS,2,9)
          CALL ECSCMD(IPHI,ITET,IMOD,ISC)
          IMD = (ISC-1)*12+IMOD
          IF ( IMD.GT.9.AND.IMD.LT.13) THEN
            IND = IMD-9
            IF ( ITET.GT.ITECH) THEN
              DO IL =1,NL
                IF ( ITET.GT.ITMN(IL).AND.ITET.LE.ITMX(IL))THEN
                  IPHI = IPHI + IPHCH(IL,IND)
                ENDIF
              ENDDO
            ENDIF
            IW(JETDI+JETDTL) = ISHFT(IPHI,2)+ISHFT(ITET,16)
          ENDIF
        ENDDO
      ENDIF
 999  RETURN
      END
#endif
