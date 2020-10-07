      SUBROUTINE QMPART
CKEY PART /INTERNAL
C----------------------------------------------------------------------
C! particle table initialization
C! called from QMINIT
C!                                                  H.Albrecht 06.11.88
C!                                                  E.Blucher  10.05.89
C! Mod. I.Tomalin 16/5/96
C!  Create QPSP bank before PMOD card unpacked.
C----------------------------------------------------------------------
#ifndef DOC
#include "parna.h"
#include "qcdesh.h"
      DIMENSION PAR(4)
      INTEGER ACARD1
      CHARACTER * 12 CPAR,CANT
      CHARACTER * 28 CTXT
      CHARACTER * 4 CCRD(3), CHAINT
      DATA CCRD /'PNEW','PMOD','PTRA'/
      COMMON / FINDPA / NAQPSP,NAQAFP,NQAFP
      INTEGER IBD(3)
#include "bmacro.h"
C----------------------------------------------------------------------
C
      IF (XCOPYJ)  GO TO 900
      DO 10 IIP=1,ITPNMX
 10   FLISPA(IIP)=' '
C
C       read particle table
C
      CALL MDARD (IW, KUCONS, 'PART', 0)
      CALL BSWAP (IW, 'PART', 'QPAR')
C
      KQPAR = IW(NAQPAR)
      IF (KQPAR .EQ. 0)
     +  CALL QWSYNT ('_QMPART_ Particle table does not exist')
      KNQPAR = IW(KQPAR+2)
      KNQPST = KNQPAR
      CALL QSBANK ('QPAR', KNQPAR)
      CALL QSBANK ('QPSP', KNQPAR )
      CALL QSBANK ('QAFP', KNQPAR )
C
C Sort the 'QPSP' bank for fast searching in KPCOMP :
C
      JQPAR = NLINK ( 'QPAR' , 0 )
      JQPSP = NLINK ( 'QPSP' , 0 )
      IW(JQPSP+LMHROW) = LROWS(JQPAR)
      DO 50 IPP = 1,LROWS(JQPAR)
         KPAR = KROW(JQPAR,IPP)
         KPSQ = KROW(JQPSP,IPP)
         IW(KPSQ+JQPAGN) = IPP
         CALL ALSTIN (IW(KPAR+JQPANA),3,CPAR)
         IF (IPP.LE.ITPNMX) FLISPA(IPP) = CPAR
         CALL CLTOU (CPAR)
         DO 40 II = 1,3
  40     IBD(II) = INTCHA('    ')
         CALL ALINST (CPAR,IBD(1),IDUMM)
         IW(KPSQ+JQPANA) = IBD(1)
         IW(KPSQ+JQPANA+1) = IBD(2)
  50  CONTINUE
C
      CALL  SORTIQ(IW(JQPSP+LMHLEN+1),LCOLS(JQPSP),LROWS(JQPSP),JQPANA)
      NQAFP = 0

C
C       data cards
C
      DO 300 NCRD=1,3
  100   ICRD = ACARD1 (CCRD(NCRD))
        IF (ICRD .EQ. 0)  GO TO 300
C
C         get name of particle and anti-particle
C
        CTXT = ' '
        L = 0
        DO 110 I = 1, MIN (IW(ICRD), 7)
          CTXT(L+1:L+4) = CHAINT (IW(ICRD+I))
  110   L = L + 4
        DO 120 I=1,L
  120   IF (CTXT(I:I) .NE. ' ')  GO TO 130
        GO TO 290
  130   DO 140 J = I+1, MIN (L, I+13)
  140   IF (CTXT(J:J) .EQ. ' ')  GO TO 150
        GO TO 290
  150   CPAR = CTXT(I:J)
        DO 160 I=J+1,L
  160   IF (CTXT(I:I) .NE. ' ')  GO TO 170
        GO TO 290
  170   DO 180 J = I+1, MIN (L, I+13)
  180   IF (CTXT(J:J) .EQ. ' ')  GO TO 190
        GO TO 290
  190   CANT = CTXT(I:J)
C
C         other paramters
C
        PAR(1) = -9999.
        PAR(2) = -9999.
        PAR(3) = -9999.
        PAR(4) = -9999.
        N = 0
        DO 200 I = (J+7)/4, IW(ICRD)
          IF (N .EQ. 4)  GO TO 280
          N = N + 1
  200   PAR(N) = RW(ICRD+I)
        GO TO (210,220,230), NCRD
C
  210   CALL QCPNEW (CPAR, CANT, PAR(1), PAR(2), PAR(3), PAR(4))
        GO TO 100
C
  220   CALL QCPMOD (CPAR, CANT, PAR(1), PAR(2), PAR(3), PAR(4))
        GO TO 100
C
  230   IF (N .NE. 2)  GO TO 280
        CALL QCPTRA (CPAR, CANT, PAR(1), PAR(2))
        GO TO 100
C
C         syntax error
C
  280   CALL QWSYNT ('_QMPART_ Wrong parameters on data card : ' //
     &    CCRD(NCRD) // ' ''' // CPAR // '...')
        GO TO 100
  290   CALL QWSYNT ('_QMPART_ Wrong particle name on data card : ' //
     &    CTXT)
        GO TO 100
  300 CONTINUE
C
C Sort the 'QPSP' bank for fast searching in KPCOMP :
C
      JQPAR = NLINK ( 'QPAR' , 0 )
      JQPSP = NLINK ( 'QPSP' , 0 )
      IW(JQPSP+LMHROW) = LROWS(JQPAR)
      DO 500 IPP = 1,LROWS(JQPAR)
         KPAR = KROW(JQPAR,IPP)
         KPSQ = KROW(JQPSP,IPP)
         IW(KPSQ+JQPAGN) = IPP
         CALL ALSTIN (IW(KPAR+JQPANA),3,CPAR)
         IF (IPP.LE.ITPNMX) FLISPA(IPP) = CPAR
         CALL CLTOU (CPAR)
         DO 400 II = 1,3
 400     IBD(II) = INTCHA('    ')
         CALL ALINST (CPAR,IBD(1),IDUMM)
         IW(KPSQ+JQPANA) = IBD(1)
         IW(KPSQ+JQPANA+1) = IBD(2)
 500  CONTINUE
C
      CALL  SORTIQ(IW(JQPSP+LMHLEN+1),LCOLS(JQPSP),LROWS(JQPSP),JQPANA)
      NQAFP = 0
C
  900 END
#endif
