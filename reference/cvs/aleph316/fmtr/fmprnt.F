      SUBROUTINE FMPRNT
C-----------------------------------------------------------------
C!  Print out properties of volumes for fast muon tracking
#ifndef DOC
      SAVE
#include "fmgeom.h"
#include "bcs.h"
      LOUT = IW(6)
      WRITE (LOUT, 1000)
 1000 FORMAT(' +++ FMPRNT +++ Volumes for fast muon tracking ',
     &   //,' NR    RMIN    RMAX    ZMIN   ZMAX   X0     C1     C2 ',/)
      DO 20 J=1,NRGION
         WRITE (LOUT,1001) J,FMRMIN(J),FMRMAX(J),FMZMIN(J),
     &                       FMZMAX(J),FMRADL(J),CONBB1(J),CONBB2(J)
 1001 FORMAT (1X,I2,2X,7F8.3)
   20 CONTINUE
      RETURN
      END
#endif
