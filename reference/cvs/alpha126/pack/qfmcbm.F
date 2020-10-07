      SUBROUTINE QFMCBM
CKEY FILL MASKS MC /INTERNAL
C-----------------------------------------------------------------------
C! Fill bit masks
C!called from QFILMC
C!                                                   H.Albrecht 27.11.88
C!                                Modified           E.Blucher  02.02.93
C!                                Add ENTRY QSMCTM   D.Casper   09.01.97
C!                                to set single particles (QUFSIM)
C!                  protection added for mothers     S.Thompson 24.02.99
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE IERR
#include "qcdesh.h"
      DATA IERR /0/
C-----------------------------------------------------------------------
C
      NBIT = 0
      NWRD = 0
      JQVEC = KOQVEC + KFMCT * KCQVEC
C
      DO 20 ITK=KFMCT,KLMCT
        jmoth1 = -1
        IF (IW(JQVEC+JQVEND) .NE. 0)  GO TO 20
        IF (NBIT .LT. 32)  THEN
          NBIT = NBIT + 1
        ELSE
          NBIT = 1
          NWRD = NWRD + 1
          IF (NWRD .GE. KLOCKM)  GO TO 80
        ENDIF
        IW(JQVEC+JQVEBM+NWRD) = KBIT(NBIT)
        JMOTH = JQVEC
   10   IF (IW(JMOTH+JQVENO) .EQ. 0)  GO TO 20
        JMOTH = KOQVEC+IW(KOQLIN+IW(JMOTH+JQVEOL)+1)*KCQVEC
        IF (JMOTH.LE.0.OR.JMOTH.GT.IW(12))  GO TO 90
        if(jmoth.eq.jmoth1) then
          write(iw(6),*) 'QFMCBM looping problem at track',itk,jmoth1
          write(iw(6),*) 'QFMCBM run/event', krun,kevt
          go to 20
        endif
        IW(JMOTH+JQVEBM+NWRD) = IOR (IW(JMOTH+JQVEBM+NWRD), KBIT(NBIT))
        jmoth1 = jmoth
        GO TO 10
C
   20 JQVEC = JQVEC + KCQVEC
C
      GO TO 90
C
      ENTRY QSMCTM(ITK1)

      JQVEC = KOQVEC + ITK1 * KCQVEC
      IF (IW(JQVEC+JQVEND) .NE. 0)  GO TO 90
      IF (NBIT .LT. 32)  THEN
        NBIT = NBIT + 1
      ELSE
        NBIT = 1
        NWRD = NWRD + 1
        IF (NWRD .GE. KLOCKM)  GO TO 80
      ENDIF
      IW(JQVEC+JQVEBM+NWRD) = KBIT(NBIT)
      JMOTH = JQVEC
   30 IF (IW(JMOTH+JQVENO) .EQ. 0)  GO TO 90
      JMOTH = KOQVEC+IW(KOQLIN+IW(JMOTH+JQVEOL)+1)*KCQVEC
      IF (JMOTH.LE.0.OR.JMOTH.GT.IW(12))  GO TO 90
      IW(JMOTH+JQVEBM+NWRD) = IOR (IW(JMOTH+JQVEBM+NWRD), KBIT(NBIT))
      GO TO 30
C
C       more than 448 non-decaying MC objects
C
   80 IERR = IERR + 1
      IF (IERR .GT. 10)  GO TO 90
      CALL QWMESS ('_QFMCBM_ Max. number of LOCK bits exceeded')
C
   90 END
#endif
