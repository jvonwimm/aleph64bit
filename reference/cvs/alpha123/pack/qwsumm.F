      SUBROUTINE QWSUMM
CKEY ALPHA WRITE SUMMARY /USER
C----------------------------------------------------------------------
C!  - Write out luminosity, hadronic event summary.
C!
C!   Author   :- E. Blucher      7-JUN-1990
C!               Modified for Sical Lumi :  J. Boucrot - 3 May 1993
C!======================================================================
#ifndef DOC
#include "qcdesh.h"
      SAVE EFF
      DATA EFF/0.975/
C----------------------------------------------------------------------
      LU = KUPRNT
      DO 100 NU=1,2
        IF (LU .EQ. 0)  GO TO 100
        IF (QINLUM.LT.0.01.AND.KNHDRN.EQ.0) GO TO 50
C  LCAL Luminosity :
        ALUMER=0.
        IF (KNBHAB.NE.0) ALUMER = QINLUM/SQRT(FLOAT(KNBHAB))
        WRITE (LU,1001) QINLUM,ALUMER,KNHDRN,FLOAT(KNHDRN)/EFF
        IF (.NOT.XIOKLU) WRITE(LU,1002)
C  SICAL Luminosity :
 50     ALUSIC=0.
        IF (KSBHAB.NE.0) ALUSIC = QSILUM/SQRT(FLOAT(KSBHAB))
        IF (XROKSI) WRITE (LU,1011) QSILUM,ALUSIC
        IF (.NOT.XIOKSI) WRITE(LU,1003)
C
 100  LU = KUPTER
C
  999 RETURN
C----------------------------------------------------------------------
 1001 FORMAT ('0_QWSUMM_ Job Summary:'/
     +'  ------------------------------------------------',
     +'-------------------'/
     +' |  Integrated luminosity: (',F9.2,'+-',F6.2,')nb-1 (stat.',
     +' err. only) |'/
     +' |  Hadronic events:',I8,'    Eff. corrected:',F10.2,
     +'            |   '/
     +'  ------------------------------------------------',
     +'-------------------'/
     +)
 1002 FORMAT
     +('_QWSUMM_ WARNING:',
     +' Some run information was missing in database;'/
     +,' hadronic event and LCAL LUMI  totals may be unreliable.'/)
 1003 FORMAT
     +('_QWSUMM_ WARNING:',
     +' Some run information was missing in database;'/
     +,'  SICAL LUMI  total may be unreliable.'/)
 1011 FORMAT ('0_QWSUMM_ SICAL Lumi Summary :'/
     +'  ------------------------------------------------',
     +'-------------------'/
     +' |  Integrated luminosity: (',F9.2,'+-',F6.2,')nb-1 (stat.',
     +' err. only)  |'/
     +'  ------------------------------------------------',
     +'-------------------'/ )
      END
#endif
