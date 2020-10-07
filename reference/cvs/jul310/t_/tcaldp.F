      SUBROUTINE TCALDP(IUNIT)
C
C-------------------------------------------------------------------*
C! Dump contents of TCAL bank
C!
C! Author:    R. Johnson   19-9-88
C!
C! Input:  IUNIT       /I     Fortran output unit number
C!
C!------------------------------------------------------------------*
#ifndef DOC
C
#include "bcs.h"
#include "tcaljj.h"
#include "tpcojj.h"
#include "tpgpar.h"
#include "rcurnt.h"
C
      DIMENSION IR(LTPDRO),IC(LTPDRO)
C
C-------------------------------------------------------------------
C
#include "bmacro.h"
C
C-------------------------------------------------------------------
C
      KTCAL = IW(NAMIND('TCAL'))
      IF (KTCAL.EQ.0) GO TO 999
      KTTCC = IW(NAMIND('TTCC'))
      IF (KTTCC.EQ.0) GO TO 999
      KTPCO = IW(NAMIND('TPCO'))
      IF (KTPCO.EQ.0) GO TO 999
C
      IF (LROWS(KTCAL).LE.0) GO TO 999
      WRITE(IUNIT,1000) LROWS(KTCAL),IEVTRC,IRUNRC
C
      DO 20 J = 1,LROWS(KTCAL)
         JOFF = ITABL(KTCAL,J,JTCAIO)
         NC = ITABL(KTCAL,J,JTCANF)
         NR = ITABL(KTCAL,J,JTCANR)
         WRITE(IUNIT,1001) J,NC,NR
         DO 30 I=1,NC
            IC(I) = IW(KTTCC+LMHLEN+JOFF+I)
            IR(I) = ITABL(KTPCO,IC(I),JTPCIN)/100000
   30    CONTINUE
         WRITE(IUNIT,1003) (IR(M),M=1,NC)
         WRITE(IUNIT,1004) (IC(M),M=1,NC)
         JOFF=JOFF+NC
         NS=0
         DO 40 I=1,NR
           NS=NS+1
           IC(NS)= IW(KTTCC+LMHLEN+JOFF+I)
           IR(NS)= ITABL(KTPCO,IC(NS),JTPCIN)/100000
           IF (NS.EQ.LTPDRO) THEN
             WRITE(IUNIT,1005) (IR(M),M=1,NS)
             WRITE(IUNIT,1006) (IC(M),M=1,NS)
             NS=0
           ENDIF
   40    CONTINUE
         IF (NS.GT.0) THEN
           WRITE(IUNIT,1005) (IR(M),M=1,NS)
           WRITE(IUNIT,1006) (IC(M),M=1,NS)
         ENDIF
   20 CONTINUE
      WRITE(IUNIT,1007) IEVTRC,IRUNRC
C
 1000 FORMAT(/,' Dump of ',I3,' track candidates (TCAL, TTCC)',/
     &         ' for event ',I6,' of run ',I5,':')
 1001 FORMAT(/' Candidate ',I3,', # coords in 1st arc=',I2,
     &       ', # in remaining arcs=',I3)
 1002 FORMAT(5X,E10.3,4F9.4,/)
 1003 FORMAT('  1st arc:  Rows    ',21I4)
 1004 FORMAT('  1st arc:  TPCO ref',21I4)
 1005 FORMAT('            Rows    ',21I4)
 1006 FORMAT('            TPCO ref',21I4)
 1007 FORMAT(' End of TCAL dump for event ',I6,' of run ',I5)
C
  999 RETURN
      END
#endif
