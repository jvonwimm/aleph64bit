      SUBROUTINE EINITK(IDTPC,IFLAG,X)
C
C***********************************************************************
C!INIT. TRK                                                            *
C!                                                                     *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  860615                                    *
C!  MODIFIED : A. BONISSENT  861108                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : ETP1,EFET (TPC TRACKS)                                 *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!    SUBROUTINE EINITK (INITIALIZE A TRACK                            *
C!                                                                     *
C!    INPUT :                                                          *
C!               IDTPC = TRACK NUMBER                                  *
C!               IFLAG = 1 : TRACK SHOULD BE EXTRAPOLATED              *
C!                     = 0 : -----------NOT BE -----------             *
C!                                                                     *
C!                                                                     *
C!    OUTPUT :    X(8)  = LAST TPC POINT                               *
C!                                                                     *
C***********************************************************************
C
#ifndef DOC
#include "bcs.h"
#include "etp1jj.h"
#include "ectcom.h"
#include "ecnamc.h"
      DIMENSION X(*)
      DOUBLE PRECISION RAD,CFILD
#include "bmacro.h"
      KETP1=IW(NAETP1)
      KLINE=KROW(KETP1,IDTPC)
      IFLAG=0
C
C?TEST IF TPC_EXTRAPOLATION NOT CUT BY MOMEMTUM THRESHOLD,
C?            OR TRACK IN BEAM PIPE
C?
         IF (IW(KLINE+JETPEF).EQ.0)   GO TO 999
C?DO NOT ANALYSE CHARGED PART. WITH MOMENTUM LOWER THAN PMINEC
         IF (RW(KLINE+JETPCH).NE.0. .AND. RW(KLINE+JETPPT).LT.PMINEC)
     +         GOTO 999
      IFLAG=1
      DO 20 I=1,8
   20 X(I)=RW(KLINE+I)
  999 CONTINUE
      RETURN
      END
#endif
