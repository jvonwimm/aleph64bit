      SUBROUTINE QFEFLO
CKEY FILL EFLOW /INTERNAL
C----------------------------------------------------------------------
C!  - Fill EFT section from EFOL bank.
C!
C!   Author   :- E. Blucher     15-Jul-1991
C!  Modified  :- E. Blucher     02-Feb-1992
C!  Modified  :- E. Blucher     02-Sep-1992
C!  Modified  :- J. Boucrot     12-Oct-1993
C!     kfeft = first EFLW track
C!     kleft = last EFLW track
C!     kneft = number of EFLW tracks
C!     Objects from EFOL stored as 'EFLW'
C!
C!  Called from QFILL
C ======================================================================
#ifndef DOC
      SAVE INIT,IERR,IEFLW,NEFLW,CUTSI,EPSIL,NRUSI
#include "qcde.h"
      CHARACTER*8 prognam
      CHARACTER*4 CHAINT
C
      INTEGER IRFSI(5),IRLSI(5)
      DATA IRFSI / 14647 , 16600 , 3*0 /
      DATA IRLSI / 14831 , 20000 , 3*0 /
      DATA NRUSI / 2 /
      DATA INIT / 0 / , IERR / 0 / , CUTSI / 0.058 / , EPSIL / 1.E-08 /
#include "qmacro.h"
      JQVEC(I,N) = KOQVEC + I * KCQVEC + N
      JR(I) = KOQVEC + ITK * KCQVEC + I
      JQDET(I,N) = KOQDET + (KFFRD+I-1) * KCQDET + N
C ======================================================================
C
      IF (INIT .EQ. 0) THEN
        INIT = 1
        IEFLW = KPCOMP ('EFLW')
        NEFLW = KFPADR(IEFLW)
      ENDIF
C
C Check on real data runs of 1992 with SICAL in the setup :
C Check also the presence of NOSC cards to kill Sical clusters anyway
C
      IRSIB = 0
      DO 5 IRU = 1,NRUSI
         IF (KRUN.GE.IRFSI(IRU).AND.KRUN.LE.IRLSI(IRU)) IRSIB = IRU
 5    CONTINUE
      IF ( NLINK('NOSC',0) .GT. 0 ) irsib = 999
C
      IF ( XMINI .AND. irsib .GE. 1 ) THEN
        mindat = 0
        narhah = NAMIND('RHAH')
        IF ( narhah .GT. 0 ) irhah = IW(narhah)
        IF ( irhah .GT. 0 ) THEN
          DO istep = 1, LROWS(irhah)
            prognam(1:4) = CHAINT(ITABL(irhah,istep,1))
            prognam(5:8) = CHAINT(ITABL(irhah,istep,2))
            IF ( prognam .EQ. 'MINI    ' ) mindat = ITABL(irhah,istep,3)
          ENDDO
        ENDIF
      ENDIF
C
      INEUT=0
C
      IF(KEFOPT.EQ.2)THEN
        JEFOL = NLINK('EFOL',1)
      ELSE
        JEFOL = NLINK('EFOL',3)
      ENDIF
      IF(JEFOL.EQ.0)THEN
        GOTO 999
      ENDIF
C
C Special patch to kill SICAL clusters from 1992 real data only :
C
      IF ( XFILJE             .AND.
     .     XMINI              .AND.
     .     MINDAT .LT. 931128 .AND.
     .     IRSIB  .NE. 0      .AND.
     .     KEFOPT .NE. 2      ) THEN
        IND = NDROP('EJET',3)
        CALL ENFJET(IER)
      ENDIF
C
      KNEFT = IW(JEFOL+LMHLEN)
      KFEFT = KFFRT
      KLEFT=KFEFT + KNEFT -1
C
      IF (KLEFT .GE. KLFRT)  CALL QSBANK ('QVEC', KLEFT+200)
      IF (KFFRD + KNEFT -1 .GE. IW(KOQDET))
     &   CALL QSBANK ('QDET', KFFRD + KNEFT -1 + 100)
C
C ...and for all data if NOSC card is present (but only if there is
C    SiCAL clusters just not to rebuild the jets for all the events)
C
      iesc = 0
      DO I=1,KNEFT
        ITYP=ITABL(JEFOL,I,JEFOTY)
        IF(ITYP.EQ.8)  iesc=1
      ENDDO
C
      IF ( XFILJE             .AND.
     .     XMINI              .AND.
     .     iESC .EQ. 1        .AND.
     .     IRSIB  .EQ. 999    .AND.
     .     KEFOPT .NE. 2      ) THEN
        IND = NDROP('EJET',3)
        CALL ENFJET(IER)
      ENDIF
C
      ITK = KFEFT - 1
      ISKIL = 0
      DO 10 I=1,KNEFT
        ITYP=ITABL(JEFOL,I,JEFOTY)
        IF(ITYP.LE.3)THEN
C---Charged track
        ITK = ITK + 1
          IT=ITABL(JEFOL,I,JEFOLT)
          IF ( IT.EQ.0) CALL QEFRTR(I,IT)
          IF ( IT.NE.0) THEN
          ITKA=KFCHT+IT-1
          CALL QVCOPY(ITK,ITKA)
          PX = RTABL(JEFOL,I,JEFOPX)
          PY = RTABL(JEFOL,I,JEFOPY)
          PZ = RTABL(JEFOL,I,JEFOPZ)
          EN = RTABL(JEFOL,I,JEFOEW)
          RW(JR(JQVEQX)) = PX
          RW(JR(JQVEQY)) = PY
          RW(JR(JQVEQZ)) = PZ
          RW(JR(JQVEQE)) = EN
          PN = SQRT( PX**2+PY**2+PZ**2 )
          RW(JR(JQVEQP)) = PN
          AM=QSQT(EN**2-PN**2)
          RW(JR(JQVEQM)) = AM
C---store track weight in QVEC (not used after version 114)
          IF (.NOT.XNANO) RW(JQVEC(ITKA,JQVEEW)) = EN/QE(ITKA)
          IW(JR(JQVETN)) = I
C---store EFOL offset in QDET
          IW(KJQDET(ITKA)+JQDEEF)=JEFOL+LMHLEN+(I-1)*LCOLS(JEFOL)
          ENDIF
        ENDIF
          IF(ITYP.GT.3.OR.(ITYP.LE.3.AND.IT.EQ.0))THEN
C---Neutral object
          PX = RTABL(JEFOL,I,JEFOPX)
          PY = RTABL(JEFOL,I,JEFOPY)
          PZ = RTABL(JEFOL,I,JEFOPZ)
          EN = RTABL(JEFOL,I,JEFOEW)
C Special patch to kill SICAL clusters from 1992 real data only,
C and for all data if NOSC card is present
C
          IF ( irsib .GE. 1 ) THEN
            IF ( ityp .EQ. 8 ) THEN
              ISKIL = ISKIL + 1
              GO TO 10
            ENDIF
            IF ( ityp .EQ. 7 .AND.
     .           XMINI .AND. mindat .LT. 931128 ) THEN
              PT = SQRT ( PX**2 + PY**2 )
              ANGN = PIBY2
              IF (ABS(PZ).GT.EPSIL) ANGN = ABS(PT/PZ)
              IF (ANGN.GT.CUTSI) GO TO 8
              IF (IRSIB.GT.1.OR.(IRSIB.EQ.1.AND.PZ.LT.0.)) THEN
                ISKIL = ISKIL + 1
                GO TO 10
              ENDIF
            ENDIF
          ENDIF
 8        ITK = ITK + 1
          INEUT = INEUT + 1
          RW(JR(JQVEQX)) = PX
          RW(JR(JQVEQY)) = PY
          RW(JR(JQVEQZ)) = PZ
          RW(JR(JQVEQE)) = EN
          PN = SQRT( PX**2+PY**2+PZ**2 )
          RW(JR(JQVEQP)) = PN
          AM=QSQT(EN**2-PN**2)
          RW(JR(JQVEQM)) = AM
          RW(JR(JQVECH)) = 0.
          IW(JR(JQVESC)) = 1
          IW(JR(JQVEKS)) = 0
          IW(JR(JQVECL)) = 1
          IW(JR(JQVEQD)) = JQDET(INEUT,0)
          IW(JR(JQVESP)) = ITK
          IW(JR(JQVEOV)) = 0
          IW(JR(JQVEEV)) = 0
          IW(JR(JQVEND)) = 0
          IW(JR(JQVENO)) = 0
          IW(JR(JQVENM)) = 0
          DO 9 IDUM=1,KLOCKM
            IW(JR(JQVEBM+IDUM-1)) = 0
9         CONTINUE
          IW(JR(JQVELK)) = 0
          RW(JR(JQVEEW)) = 0.
          RW(JR(JQVEDB)) = 0.
          RW(JR(JQVEZB)) = 0.
          RW(JR(JQVESD)) = 0.
          RW(JR(JQVESZ)) = 0.
          RW(JR(JQVECB)) = 0.
          IW(JR(JQVETN)) = I
C---Fill QDET
          IW(JQDET(INEUT,JQDEAF)) = KQZER
          IW(JQDET(INEUT,JQDEAL)) = KQZER
          IW(JQDET(INEUT,JQDENT)) = 0
          IW(JQDET(INEUT,JQDEAT)) = KQZER
          IW(JQDET(INEUT,JQDEAE)) = KQZER
          IW(JQDET(INEUT,JQDEAH)) = KQZER
          IW(JQDET(INEUT,JQDEAM)) = KQZER
          IW(JQDET(INEUT,JQDECF)) = 0
          IW(JQDET(INEUT,JQDEEC)) = KQZER
          IW(JQDET(INEUT,JQDEHC)) = KQZER
          IW(JQDET(INEUT,JQDEET)) = KQZER
          IW(JQDET(INEUT,JQDEFI)) = KQZER
          IW(JQDET(INEUT,JQDENF)) = 0
          IW(JQDET(INEUT,JQDENE)) = 0
          IW(JQDET(INEUT,JQDENH)) = 0
          IW(JQDET(INEUT,JQDEEF)) = JEFOL + LMHLEN + (I-1)*LCOLS(JEFOL)
          IW(JQDET(INEUT,JQDEPC)) = KQZER
          IW(JQDET(INEUT,JQDEEG)) = KQZER
          IW(JQDET(INEUT,JQDEMU)) = KQZER
          IW(JQDET(INEUT,JQDEPG)) = KQZER
          IW(JQDET(INEUT,JQDEPG)) = KQZER
C---Set bit mask
          CALL QSBITM(ITK)
        ENDIF
        IF (IT.EQ.0) RW(JR(JQVEEM)) = -1.
        RW(JR(JQVECF)) = -1.
        IW(JR(JQVEPA)) = IEFLW
        IW(JR(JQVENP)) = IW(KOQFPA+NEFLW*KCQFPA+1)
        IW(KOQFPA+NEFLW*KCQFPA+1) = ITK
   10 CONTINUE
      IF (ISKIL.GT.0) THEN
        KNEFT = KNEFT - ISKIL
        KLEFT = KLEFT - ISKIL
      ENDIF
C   UPDATE KLOCUS POINTERS.
      DO 20 N = KSEFT+1,KSMCT
        KLOCUS(1,N) = KLOCUS(2,KSEFT) + 1
   20 KLOCUS(2,N) = KLOCUS(2,KSEFT)
      KFFRT = KLOCUS(2,KSEFT) + 1
      KFFRD = KFFRD + INEUT
  999 RETURN
      END
#endif
