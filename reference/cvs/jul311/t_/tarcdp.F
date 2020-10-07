      SUBROUTINE TARCDP(IUNIT)
C
C-------------------------------------------------------------------*
C! Dump contents of arc bank
C!
C! Author:    R. Johnson   16-9-88
C!
C! Input:  IUNIT       /I     Fortran output unit number
C!
C!------------------------------------------------------------------*
#ifndef DOC
C
#include "bcs.h"
#include "tparac.h"
#include "tarcjj.h"
#include "tpcojj.h"
#include "tpgpar.h"
#include "rcurnt.h"
C
      DIMENSION IR(LTPDRO),IC(LTPDRO)
      DIMENSION ERR(5)
C
C-------------------------------------------------------------------
C
#include "bmacro.h"
C
C-------------------------------------------------------------------
C
      KTARC = IW(NAMIND('TARC'))
      IF (KTARC.EQ.0) GO TO 999
      KTATC = IW(NAMIND('TATC'))
      IF (KTATC.EQ.0) GO TO 999
      KTPCO = IW(NAMIND('TPCO'))
      IF (KTPCO.EQ.0) GO TO 999
C
      IF (LROWS(KTARC).GT.0) WRITE(IUNIT,1000) IEVTRC,IRUNRC
C
      DO 20 J = 1,LROWS(KTARC)
         JOFF = ITABL(KTARC,J,JTAROF)
         NC = ITABL(KTARC,J,JTARNC)
         WRITE(IUNIT,1001) J,(RTABL(KTARC,J,M), M=1,7),
     &         ITABL(KTARC,J,JTARPI),
     &         ITABL(KTARC,J,JTARAN),ITABL(KTARC,J,JTARTN)
         DO 25 I=1,5
            ERR(I) = 1./SQRT(RTABL(KTARC,J,JTARER-1+I))
   25    CONTINUE
         WRITE(IUNIT,1002) ERR
         DO 30 I=1,NC
            IC(I) = IW(KTATC+LMHLEN+JOFF+I)
            IR(I) = ITABL(KTPCO,IC(I),JTPCIN)/100000
   30    CONTINUE
         WRITE(IUNIT,1003) (IR(M),M=1,NC)
         WRITE(IUNIT,1004) (IC(M),M=1,NC)
   20 CONTINUE
C
 1000 FORMAT(/,' Dump of TPC arcs for event ',I6,' of run ',I5,':'/,
     & '      1/Rho       tanl     phi0       d0      z0    ',
     & ' Chi1   Chi2  Hitpat Arc Track')
 1001 FORMAT(/,1X,I3,1X,E10.3,4F9.4,2F7.2,Z7,I4,I4)
 1002 FORMAT(5X,E10.3,4F9.4,/)
 1003 FORMAT('    Rows    :',21I4)
 1004 FORMAT('    TPCO ref:',21I4)
C
  999 RETURN
      END
#endif
