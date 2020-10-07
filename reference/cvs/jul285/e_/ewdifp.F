      SUBROUTINE EWDIFP
C=======================================================================
C!   Built EWDI from POT
C-
C-   Author  : M.N. Minard                 14/07/90
C-
C-   Bank created  EWDI
C-   Banks used PEWI ECHE ECMC
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "ewdijj.h"
#include "pewijj.h"
#include "echejj.h"
#include "ecmcjj.h"
#include "echica.h"
#include "rcurnt.h"
#include "edwijj.h"
#include "bmacro.h"
C
C-    Built conversion table
C
      XECHE = 1.
C
C-    From PEWI  built EWDI
C
      CALL RQBUNC(IBUNCH,NBUN,NWAG,IQUA)
      IF ( IBUNCH.LE.0) IBUNCH =1
      NAPEWI = NAMIND ('PEWI')
      IF ( IRUNRC.GT.35000)NAPEWI = NAMIND('PWEI')
      KPEWI = IW(NAPEWI)
      NPEWI = 0
      IF ( KPEWI.NE.0) NPEWI = LROWS(KPEWI)
      LENG = LMHLEN + LEWDIA*NPEWI
      IF ( IRUNRC.GT.35000) THEN
          LENG0 = LMHLEN + LEDWIA*NPEWI
          CALL AUBOS('EDWI',0,LENG0,KEWDI,IGARB)
          IF ( IGARB.EQ.2) GO TO 900
          IF ( IGARB.EQ.1) KPEWI = IW(NAPEWI)
          IW(KEWDI+LMHCOL) = LEDWIA
      ELSE
         CALL AUBOS ('EWDI',0,LENG,KEWDI,IGARB)
         IF ( IGARB.EQ.2) GO TO 900
         IF ( IGARB.EQ.1) KPEWI = IW(NAPEWI)
         IW(KEWDI+LMHCOL) = LEWDIA
      ENDIF
      DO 20 IL =1,NPEWI
        JEWDI = KROW(KEWDI,IL)
        IMOD = ITABL(KPEWI,IL,1)
        IMODB = (IBUNCH-1)*36+IMOD
        IW(JEWDI+1) = ITABL(KPEWI,IL,1)
        DO 15 IK = 2,46
          XVAL = FLOAT(ITABL(KPEWI,IL,IK))/COLDWR(IMODB)/COLDPL(IMODB)
          IW(JEWDI+IK) = NINT(XVAL)
   15   CONTINUE
        INSUM = 54
        IF ( IRUNRC.GT.35000) INSUM = 62
        DO 16 IK = 47,INSUM
          IW(JEWDI+IK) = ITABL(KPEWI,IL,IK)
   16   CONTINUE
   20 CONTINUE
      IW(KEWDI+LMHROW) = NPEWI
      GO TO 999
  900 CALL RERROR('EWDIFP',1,' Not enough space for EWDI bank')
  999 CONTINUE
      RETURN
      END
#endif
