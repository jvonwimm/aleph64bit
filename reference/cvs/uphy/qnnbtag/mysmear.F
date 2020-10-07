      SUBROUTINE MYSMEAR(IBANK,TTYPE,DS)
C
C  Generate a smearing value for the impact parameter significance,
C  based on a delta function plus exponential fit comparing the
C  negative impact parameter spectrum between data and MC.
C  Dave Brown, 24-3-92
C  Need a SMIR card put in the adequate cards file.
C
C  In/output
C
      INTEGER TTYPE
      REAL DS
C
#include "bcs.h"
C
C  Functions
C
      INTEGER NLINK
      REAL RANF
C
C  Local variables
C
      INTEGER JSMIR,IBANK
      REAL PROB
      LOGICAL FIRST/.TRUE./
C
C  Inlines
C
      INTEGER ID,L,NRBOS,ITRK
      INTEGER LCOLS,LROWS,KNEXT,KROW,LFRWRD,LFRROW,ITABL
      REAL RTABL
#include "bmacro.h"
C
C  Generate a random change to the impact parameter significance
C  based on the measured smearing function
C
      JSMIR = NLINK('SMIR',IBANK)
      IF(JSMIR.GT.0)THEN
        PROB = RANF()
        IF(PROB .LT. RTABL(JSMIR,TTYPE,2).AND.PROB.GT.0.0)THEN
          DS = -RTABL(JSMIR,TTYPE,1)*LOG(PROB/RTABL(JSMIR,TTYPE,2))
        ELSE IF(1.-PROB .LT. RTABL(JSMIR,TTYPE,4)
     &          .AND.PROB.LT.1.0)THEN
          DS = -RTABL(JSMIR,TTYPE,3)*
     &          LOG((1.-PROB)/RTABL(JSMIR,TTYPE,4))
        ELSE
          DS = 0.0
        END IF
      ELSE
        WRITE(6,*)'RANSMIR: No smearing bank'
      END IF
#if defined(DEBUG)
      write(*,*) ' MYSMEAR ',prob,ds,ttype,rtabl(jsmir,ttype,4)
#endif
      RETURN
      END
C
C======================================================================C
*AF
