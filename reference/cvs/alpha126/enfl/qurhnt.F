      SUBROUTINE QURHNT(KRNEW,IER)
C----------------------------------------------------------------
C! Read Hcal Noisy Towers from database
C----------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
#include "qhac.h"
#include "cuhnto.h"
      PARAMETER( IENMIN= 200 )
      EXTERNAL ALGTDB,JUNIDB
      INTEGER  ALGTDB,JUNIDB
      LOGICAL FIRST
#include "qmacro.h"
      DATA FIRST /.TRUE./
C----------------------------------------------------------------
      IER= 0
C
      IF(FIRST) THEN
C----- Initialize /CUHNTO/
        DO 5 L1=1,NNTMAX
          IPHNTO(L1)= 0
          ITHNTO(L1)= 0
5       CONTINUE
        NNTO= 0
        NRUNTO= 0
        FIRST= .FALSE.
      END IF
C
C----- First look at the S.O.R.
      INEW= 1
      IDAF= 0
      NAHNTO= NAMIND('HNTO')
      IHNTO= IW(NAHNTO)
      NHNTO= 0
      IF(IHNTO.NE.0) THEN
        NNR= IW(IHNTO-2)
        IF(NNR.NE.KRNEW) IDAF= 1
      END IF
C
      IF(IDAF.EQ.1) THEN
        LRGEOM= JUNIDB(0)
        IFLAG = ALGTDB(LRGEOM,'HNTO',KRUN)
        IF(IFLAG.EQ.0) THEN
          IER= 1
          RETURN
        END IF
        IHNTO= IW(NAMIND('HNTO'))
        IF(IHNTO.EQ.0) RETURN
        NNR= IW(IHNTO-2)
        IF(NNR.LT.NRUNTO) INEW= 0
      END IF
C
      IF(INEW.EQ.0) RETURN
C
      NHNTO= LROWS(IHNTO)
      IF(NHNTO.GT.NNTMAX) THEN
        IER= 2
        RETURN
      END IF
      NTOW= 0
      DO 20 L1=1,NHNTO
        IEN= ITABL(IHNTO,L1,2)
        IF(IEN.LT.IENMIN) GOTO 20
        NTOW= NTOW+1
        IADR= ITABL(IHNTO,L1,1)
        IPHNTO(NTOW)= IBITS(IADR,0,16)
        ITHNTO(NTOW)= IBITS(IADR,16,8)
20    CONTINUE
      NRUNTO= NNR
      NNTO= NTOW
C
      RETURN
      END
#endif
