      SUBROUTINE ETDIFP
C=======================================================================
C!   Recalculate uncorrected raw energy ETDI EWHE
C-
C-   Author  : M.N. Minard                 14/07/90
C-
C-   Bank modified ETDI EWHE
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "etdijj.h"
#include "echejj.h"
#include "ecmcjj.h"
#include "ehgfjj.h"
#include "ecnamc.h"
#include "echica.h"
#include "bmacro.h"
C
C-    Built conversion table
C
      CALL RQBUNC(IBUNCH,NBUN,NWAG,IQUA)
      IF ( IBUNCH.LE.0) IBUNCH =1
      XECHE = 1.
      KETDI = IW(NAETDI)
      NAEHGF = NAMIND('EHGF')
      KEHGF = IW(NAEHGF)
C
C-    Loop on ETDI
C
      NETDI = 0
      IF ( KETDI.NE.0) NETDI = LROWS(KETDI)
      DO 200 IETDI = 1 , NETDI
        JETDI = KROW(KETDI,IETDI)
        ITOW = IW(JETDI+JETDTL)
        ITETA = IBITS(ITOW,16,8)
        IPHI = IBITS(ITOW,2,9)
        CGAS = 1.
        IF (KEHGF.NE.0)THEN
          CALL EMDTOW(ITETA,IPHI,ISCO,IMO,IRG)
          IMOD = (ISCO-1)*12+IMO
          IMODB = (IBUNCH-1)*36+IMOD
          CGAS = 1. -
     &          COLDGF(IMOD)*(FLOAT(ITETA)-114.5)/63.5
        ENDIF
        CALL ECSCMD(IPHI,ITETA,IMOD,ISC)
        IGMOD = (ISC-1)*12+IMOD
        IGMODB = (IBUNCH-1)*36+IGMOD
        GAIN = COLDPD(IGMODB)
        DO 190 IL =1,3
          EN = FLOAT(IW(JETDI+1+IL))/(GAIN*CGAS)
          IW(JETDI+1+IL) = EN
  190   CONTINUE
  200 CONTINUE
C
C-    Update EWHE bank
C
      NAEWHE = NAMIND('EWHE')
      KEWHE = IW(NAEWHE)
      NEWHE = 0
      IF ( KEWHE.NE.0) NEWHE = LROWS(KEWHE)
      DO 300 IEWHE = 1,NEWHE
        IGMOD = ITABL(KEWHE,IEWHE,2)
        IGMODB = (IBUNCH-1)*36+IGMOD
        IF (IGMOD.LT.1.OR.IGMOD.GT.36) GO TO 300
        EN = FLOAT(ITABL(KEWHE,IEWHE,1))/COLDWR(IGMODB)
        IW(KROW(KEWHE,IEWHE)+1) = EN
  300 CONTINUE
  999 CONTINUE
      RETURN
      END
#endif
