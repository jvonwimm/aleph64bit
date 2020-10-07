      FUNCTION KPCOMP (CINP)
CKEY PART /INTERNAL
C----------------------------------------------------------------------
C! Get particle code from QPAR. Input : particle name CINP
C                                                   H.Albrecht 27.11.88
C    Add KPCHEK entry to just test                  T.Burnett  16.16.90
C  Completely rewritten to use LOCTAB and 'QPSP'    J.Boucrot  21.09.93
C----------------------------------------------------------------------
#ifndef DOC
#include "parna.h"
#include "qcdesh.h"
      CHARACTER *(*) CINP
      CHARACTER CNAM * 12, CTAB * 12, CHAINT * 4, SPACES * 12
      LOGICAL CHECK
      COMMON / FINDPA / NAQPSP,NAQAFP,NQAFP
      INTEGER ITB(2)
      DATA CNAM /' '/
#include "bmacro.h"
C-----------------------------------------------------------------------
C
      CHECK = .FALSE.
    1 CONTINUE
C
C       convert lower case into upper case and remove blanks
C
      CNAM = SPACES (CINP,0)
      CALL CLTOU (CNAM)
C
C       go through particle table
C
      KPCOMP = 0
      DO 2 I = 1,2
 2    ITB(I) = INTCHA('    ')
      LCINP = LNBLNK(CNAM)
      NWC = 1+(LCINP-1)/4
      IF (CNAM(1:4).EQ.'NUMU')  NWC=2
      ITB(1) = INTCHA(CNAM(1:4))
      ITB(2) = INTCHA(CNAM(5:8))
      JQAFP = IW(NAQAFP)
      IF (NQAFP.EQ.0) GO TO 10
      J = LOCTAB(IW(JQAFP+LMHLEN+1),JQPANA+1,LROWS(JQAFP),JQPANA,ITB(1))
      IF (J.GT.0.AND.NWC.EQ.1) THEN
         KPCOMP = ITABL(JQAFP,J,JQPAGN)
         GO TO 90
      ENDIF
      IF (J.GT.0.AND.NWC.GE.2) THEN
         KTMIN = MAX0(1,J-25)
         KTMAX = MIN0(LROWS(JQAFP),J+25)
         DO 3 IK = KTMIN,KTMAX
            KQAFP = KROW(JQAFP,IK)
            IF (IW(KQAFP+JQPANA).NE.ITB(1)) GO TO 3
            IF (IW(KQAFP+JQPANA+1).EQ.ITB(2)) THEN
               KPCOMP = ITABL(JQAFP,IK,JQPAGN)
               GO TO 90
            ENDIF
 3       CONTINUE
      ENDIF
C
C CINP not found in 'QAFP' :
C Search for it in bank QPSP , then store it in 'QAFP' :
C
 10   JQPSP = IW(NAQPSP)
      J = LOCTAB(IW(JQPSP+LMHLEN+1),JQPANA+1,LROWS(JQPSP),JQPANA,ITB(1))
      IF (J.GT.0.AND.NWC.EQ.1) THEN
         KPCOMP = ITABL(JQPSP,J,JQPAGN)
         GO TO 70
      ENDIF
      IF (J.GT.0.AND.NWC.GE.2) THEN
         KTMIN = MAX0(1,J-25)
         KTMAX = MIN0(LROWS(JQPSP),J+25)
         DO 30 IK = KTMIN,KTMAX
            KQPSP = KROW(JQPSP,IK)
            IF (IW(KQPSP+JQPANA).NE.ITB(1)) GO TO 30
            IF (IW(KQPSP+JQPANA+1).EQ.ITB(2)) THEN
               KPCOMP = IW(KQPSP+JQPAGN)
               GO TO 70
            ENDIF
 30      CONTINUE
      ENDIF
 60   CONTINUE
C
C CINP was not in the 'QPSP' bank : it is a new particle , which
C has to be stored in 'QPAR' . 'QPSP' and 'QAFP' :
C
      IF (CHECK) GOTO 90
      KNQPAR = KNQPAR + 1
      IF (KNQPAR .GE. IW(KOQPAR+KCQPAR)) THEN
        CALL QSBANK ('QPAR',KNQPAR+500)
        CALL QSBANK ('QPSP',KNQPAR+500)
      ENDIF
      JQPAR = KOQPAR + KNQPAR * KCQPAR
      KPCOMP = KNQPAR
C
      CNAM = SPACES (CINP,0)
      IW(JQPAR+JQPANA)   = INTCHA (CNAM(1:4))
      IW(JQPAR+JQPANA+1) = INTCHA (CNAM(5:8))
      IW(JQPAR+JQPANA+2) = INTCHA (CNAM(9:12))
      IW(JQPAR+JQPAGN) = 100
      IW(JQPAR+JQPACO) = 100
      RW(JQPAR+JQPALT) = 1.E16
      RW(JQPAR+JQPAWI) = 0.
      IW(JQPAR+JQPAAN) = KPCOMP
      RW(JQPAR+JQPAMA) = QPPMMA(CINP)
      IF (KNQPAR.LE.ITPNMX) FLISPA(KNQPAR) = CNAM
      IF (KDEBUG.EQ.0) GO TO 65
      CALL QWMESS ('_KPCOMP_ New particle established : ' // CNAM)
 65   CONTINUE
C Fill a new row in 'QPSP' and 'QAFP'  banks for subsequent fast search :
      JQPSP = IW(NAQPSP)
      IW(JQPSP+LMHROW) = KNQPAR
      KPSP = KROW(JQPSP,KNQPAR)
      IW(KPSP+JQPAGN) = KNQPAR
      IW(KPSP+JQPANA) = ITB(1)
      IW(KPSP+JQPANA+1) = ITB(2)
C  Sort the 'QPSP' bank for subsequent fast search :
      CALL  SORTIQ(IW(JQPSP+LMHLEN+1),LCOLS(JQPSP),LROWS(JQPSP),JQPANA)
C  Fill and Sort the 'QAFP' bank for subsequent fast search :
 70   NQAFP = NQAFP + 1
      JQAFP = IW(NAQAFP)
      NNQ = IW(JQAFP)/LCOLS(JQAFP)
      IF (NQAFP.GE.NNQ) CALL QSBANK('QAFP',NNQ+500)
      IW(JQAFP+LMHROW) = NQAFP
      KQAFP = KROW(JQAFP,NQAFP)
      IW(KQAFP+JQPAGN) = KPCOMP
      IW(KQAFP+JQPANA)   = ITB(1)
      IW(KQAFP+JQPANA+1) = ITB(2)
      CALL SORTIQ(IW(JQAFP+LMHLEN+1),LCOLS(JQAFP),LROWS(JQAFP),
     +                     JQPANA)
      GOTO 90
C
            ENTRY KPCHEK(CINP)
C-----------------------------------------------------------------
      CHECK = .TRUE.
      GOTO 1
C
   90 END
#endif


