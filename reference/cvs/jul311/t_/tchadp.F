      SUBROUTINE TCHADP(IUNIT)
C
C-------------------------------------------------------------------*
C! Dump contents of chain bank
C!
C! Author:    M. Mermikides   8/9/86
C! Modified:  R. Johnson     17/4/87
C! Modified:  M.Mermikides   23/08/89
C!
C! Input:  IUNIT       /I     Fortran output unit number
C!
C! Called by TRKFND
C!
C!------------------------------------------------------------------*
#ifndef DOC
C
#include "bcs.h"
#include "tparac.h"
#include "tchajj.h"
C
      DIMENSION IR(21),IC(21)
      DIMENSION ERR(5)
C
C-------------------------------------------------------------------
C
#include "bmacro.h"
C
C-------------------------------------------------------------------
C
      KTCHA = IW(NAMIND('TCHA'))
      KTCTC = IW(NAMIND('TCTC'))
      KTPCO = IW(NAMIND('TPCO'))
      IF (KTCHA.EQ.0) GO TO 999
C
      IF (IW(KTCHA+LMHROW).GT.0) WRITE (IUNIT,1000)
C
      DO 20 J = 1,IW(KTCHA+LMHROW)
         JOFF = IW(KROW(KTCHA,J)+JTCHOF)
         NC = IW(KROW(KTCHA,J)+JTCHNC)
         WRITE (IUNIT,1001) J,(RW(KROW(KTCHA,J)+M), M=1,8),
     &         IW(KROW(KTCHA,J)+JTCHPI),NC
C
         WRITE (IUNIT,1002) (RW(KROW(KTCHA,J)+JTCHER+M),M=0,14)
C Coordinates in chain
         DO 30 I=1,NC
            IC(I) = IW(KTCTC+LMHLEN+JOFF+I)
            IR(I) = IW(KROW(KTPCO,IC(I))+1)/100000
   30    CONTINUE
         WRITE(IUNIT,1003) (IR(M),M=1,NC)
         WRITE(IUNIT,1004) (IC(M),M=1,NC)
   20 CONTINUE
C
 1000 FORMAT(/,' Fitted chains '/,
     & '      1/Rho       tanl     phi0       d0      z0     s(xy) ',
     & ' Chi1   Chi2  HITPAT NC')
 1001 FORMAT(/,1X,I3,1X,E10.3,4F9.4,3F7.2,Z7,I4)
 1002 FORMAT(' Error matrix: ',5E12.3,/15X,4E12.3,/15X,3E12.3,
     &            /15X,2E12.3,/15X,E12.3,/)
 1003 FORMAT('    Rows    :',21I4)
 1004 FORMAT('    TPCO ref:',21I4)
C
  999 RETURN
      END
#endif
