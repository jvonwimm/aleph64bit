      SUBROUTINE TGMADP(IUNIT)
C
C-----------------------------------------------------------------------
C! Dump banks TGMA and TMTL
C
C  Author:  R. Johnson   10-1-89
C
C  Input   IUNIT    /I    FORTRAN output logical unit number
C
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "tgmajj.h"
#include "tmtljj.h"
#include "rcurnt.h"
C
#include "bmacro.h"
C
      KTGMA=IW(NAMIND('TGMA'))
      IF (KTGMA.EQ.0) GO TO 999
      KTMTL=IW(NAMIND('TMTL'))
      IF (KTMTL.EQ.0) GO TO 999
C
      WRITE(IUNIT,10) IEVTRC,IRUNRC
   10 FORMAT(/' Dump of banks TGMA and TMTL for event ',I6,
     &        ' of run ',I5)
      DO 500 ITK=1,LROWS(KTGMA)
        WRITE(IUNIT,30) ITK,ITABL(KTGMA,ITK,JTGMNM),
     &                  ITABL(KTGMA,ITK,JTGMNC),ITABL(KTGMA,ITK,JTGMNA)
   30   FORMAT(' Track ',I3,' # MC trk assoc=',I1,' # coord=',
     &         I2,' # coord assoc=',I2)
        DO 300 II=1,ITABL(KTGMA,ITK,JTGMNM)
          IPT=II+ITABL(KTGMA,ITK,JTGMOM)
          WRITE(IUNIT,35) IPT,ITABL(KTMTL,IPT,JTMTMT),
     &                  ITABL(KTMTL,IPT,JTMTNH),RTABL(KTMTL,IPT,JTMTCS)
   35     FORMAT(10X,I3,'  MC trk=',I4,' # hits=',I2,'  chi2=',E12.5)
  300   CONTINUE
  500 CONTINUE
      WRITE(IUNIT,530)
  530 FORMAT(' End of TGMA/TMTL dump. . .'/)
  999 CONTINUE
      RETURN
      END
#endif
