      SUBROUTINE EPNEU
C***********************************************************************
C!PROPAGETE NEUTRAL TRACKS                                             *
C!                                                                     *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  861128                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : ETP1                                                   *
C!    OUTPUT  : ETP1                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!   DO FOR ALL TRACKS                                                 *
C!      IF NEUTRAL TRACK THEN                                          *
C!         GET X,Y,Z,PX,PY,PZ                                          *
C!         COMPUTE INTERSECTION WITH CYLINDER                          *
C!         REPLACE OLD X,Y,Z BY NEW ONES                               *
C!      ENDIF                                                          *
C!    ENDDO                                                            *
C!                                                                     *
C***********************************************************************
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "ecnamc.h"
#include "ecgeoc.h"
#include "bmacro.h"
C
      KETP1=IW(NAETP1)
      NTRAK=LROWS(KETP1)
      DO 10 ITRAK=1,NTRAK
      IREF=KROW(KETP1,ITRAK)
      IF(RW(IREF+8).EQ.0..AND.IW(IREF+9).EQ.1)THEN
        X=RW(IREF+1)
        Y=RW(IREF+2)
        Z=RW(IREF+3)
        U=RW(IREF+4)
        V=RW(IREF+5)
        W=RW(IREF+6)
        TZ=(SIGN(ZECMX,W)-Z)/W
        A=U**2+V**2
        BP=U*X+V*Y
        C=X**2+Y**2-RECMX**2
        DELTA=BP**2-A*C
        TXY=(-BP+SQRT(DELTA))/A
        T=AMIN1(TZ,TXY)
        RW(IREF+1)=X+T*U
        RW(IREF+2)=Y+T*V
        RW(IREF+3)=Z+T*W
C        WRITE(LDEBRL,*)' TRACE NEUTRE ',ITRAK,' PROPAGEE'
      ENDIF
   10 CONTINUE
      RETURN
      END
#endif
