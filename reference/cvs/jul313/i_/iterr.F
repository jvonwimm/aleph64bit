      SUBROUTINE ITERR (R,ITK,RPHERR,IFLAG)
C-----------------------------------------------------------------------
C!    Calculate tracking error in rphi
C!
C!    Author  J Carr stolen
C!            from IHELER of  M J Phillips with J Sedgbeer    01-Jan-1991
C!
C!    Input                R          radius to calculate error
C!                         ITK        track number
C!
C!    Output               RPHERR     Error in rphi-coordinate at r=RIN
C!                         IFLAG = 0  Successful
C!                                 1  Unsuccessful
C!
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "bosext.h"
#include "bmacro.h"
C
      KITF1 = NLINK('ITFT',1)
C
      W     = RTABL(KITF1,ITK,1)
      D0    = RTABL(KITF1,ITK,4)
      VARWW = RTABL(KITF1,ITK,7)
      VARPP = RTABL(KITF1,ITK,12)
      VARDD = RTABL(KITF1,ITK,16)
      COVWP = RTABL(KITF1,ITK,10)
      COVWD = RTABL(KITF1,ITK,13)
      COVPD = RTABL(KITF1,ITK,15)
C
      Y = ABS(W)/R/2.*(R**2-D0**2)/(D0*W-1.)+ABS(W)/W*D0/R
      IFLAG=1
      IF (ABS(Y).LT.1.) THEN
         DEPW = (R**2-D0**2)/2./R/(D0*W-1.)**2/SQRT(1.-Y**2)
         DEPD = -0.5/R/SQRT(1.-Y**2)*(1.+(1.-(W*R)**2)/(D0*W-1.)**2)
C
         PHERR = DEPW * (      DEPW * VARWW +
     +                 2. *        COVWP +
     +                 2. * DEPD * COVWD ) +
     +          VARPP                       +
     +          DEPD * (      DEPD * VARDD +
     +                 2. *        COVPD )
C
C
C   Rounding errors ?
C
        IF (PHERR.GT.0.0) THEN
           PHERR = SQRT (PHERR)
           RPHERR=PHERR*R
           IFLAG=0
       ENDIF
       ENDIF
C
       RETURN
       END
#endif
