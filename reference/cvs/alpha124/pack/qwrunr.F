      SUBROUTINE QWRUNR
CKEY WRITE EVENT /USER
C----------------------------------------------------------------------
C! run record output routine
C                                         Author : H.Albrecht 06.12.89
C                                       Modified : E.Blucher  04.04.90
C                                       Modified : J.Boucrot  05.02.91
C                                       Modified : E.Blucher  04.09.92
C                                       Modified : S.Wasserba 30.10.95
C----------------------------------------------------------------------
#ifndef DOC
      SAVE IROLD, LUSER, PRNAM0, IPVER0, ICVER0, NATOU0
#include "qcdesh.h"
      INTEGER BKINCA
      INTEGER IPVERU, ICVERU, NATOUU
      INTEGER IPVER0, ICVER0, NATOU0
      LOGICAL LUSER
      CHARACTER * 1 LIST
      CHARACTER*8 PRNAMU, PRNAM0
      DATA IROLD /0/
      DATA LUSER / .FALSE. /
C----------------------------------------------------------------------
C
      IF (KNEOUT .GE. KNWRLM)  GO TO 90
      IF (KRUN .EQ. IROLD)  GO TO 10
      IROLD = KRUN
C
C Fill a new row in the RHAH bank :
C
      IF (XCOPYJ .AND. .NOT.XWMINI) GO TO 10
      CALL ALDTYP(ITYP)
      ILVER = BKINCA (CQVERS(1:3))
      ICVER = BKINCA (CQVERS (INDEX (CQVERS, '.') + 1 :))
      IWRHA=0
      IF(XWMINI)THEN
C
C        For new Mini lib (9.0 and later), do not update Mini banks
C        when reading Mini-DST.
C
         MVLIB = MINVSN(DUMMY)
         MVDAT = MINGTV(DUMMY)
         IF (MVLIB.GE.90 .AND. MVDAT.GT.0) THEN
            CALL BKRHAW ('MINIALPH', MVDAT, ILVER, 5, IFAIL)
         ELSE
            CALL BKRHAW ('MINIALPH', MVLIB, ILVER, 5, IFAIL)
         ENDIF
         IWRHA=1
      ELSEIF(XWNANO)THEN
         IFAIL=0
         IWRHA=1
      ELSE
         IFAIL = 0
         IF (ITYP.GE.3) THEN
            IF (LUSER) THEN
              CALL BKRHAW (PRNAM0,IPVER0,ICVER0,NATOU0,IFAIL)
            ELSE
              CALL BKRHAW ('ALPHA   ',ILVER,ICVER,ITYP,IFAIL)
            ENDIF
            IWRHA=1
         ENDIF
      ENDIF
      IF (IFAIL .NE. 0)  THEN
        CALL QWMESS
     +     ('_QWRUNR_ No room for RHAH bank; history not updated')
      ELSE
        CALL BLIST(IW,'C+','RHAH')
        CALL BLIST(IW,'R+','RHAH')
        IF (KDEBUG.GT.0.AND.IWRHA.EQ.1) THEN
          CALL QWMESS('_QWRUNR_ RHAH bank written on output file ')
          CALL PRRHAH
        ENDIF
      ENDIF
C Remove unwanted huge bank 'JCAR' from the run header on output :
      CALL BLIST(IW,'C-','JCAR')
      CALL BLIST(IW,'R-','JCAR')
C
C       If XWRRUN true : write all accumulated run banks
C       For Mini-DST, write limitted number of banks.
C
 10   LIST = 'R'
      IF(XWMINI.OR.XWNANO)THEN
         CALL BLIST(IW,'R=',CQRLST)
      ELSE
         IF (XWRRUN)  LIST = 'C'
      ENDIF
C
      CALL ABWSEL (LIST)
      CALL ABCLAS (-1)
C
      XWRRUN = .FALSE.
      KNREOU = KNREOU + 1
C
 90   RETURN
C
C     Call QURHAH to specify non-standard values of PRNAM, IPVER,
C     ICVER, and NATOU to be passed to BKRHAW in subsequent calls
C     to QWNEWR.
C
      ENTRY QURHAH (PRNAMU, IPVERU, ICVERU, NATOUU)
      LUSER = .TRUE.
      PRNAM0 = PRNAMU
      IPVER0 = IPVERU
      ICVER0 = ICVERU
      NATOU0 = NATOUU
      RETURN
      END
#endif
