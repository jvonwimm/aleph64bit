      SUBROUTINE QUHNOI(IER)
C----------------------------------------------------------------
C! Look for energy in HCAL noisy towers; fill ENOPH in /CUPHCO/
C
C  Gerardo Ganis -- 1989
C----------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
#include "qhac.h"
#include "cuhnto.h"
#include "cuphco.h"
#include "qmacro.h"
      DATA ITLU /2/
C----------------------------------------------------------------
      IER= 0
C
      IPHCO= IW(NAMIND('PHCO'))
      IF(IPHCO.EQ.0) RETURN
      NHCO= LROWS(IPHCO)
      IF(NHCO.GT.NPHMAX) RETURN
      IPHST= IW(NAPHST)
      IF(IPHST.EQ.0) THEN
        IER= -1
        RETURN
      END IF
      NPHST= LROWS(IPHST)
C
      DO 7 L1=1,NHCO
        ELUPH(L1)= 0.
7     CONTINUE
      DO 10 L=1,NPHST
        ILU= 0
        IHCO= ITABL(IPHST,L,JPHSPH)
        IF(IHCO.LE.0.OR.IHCO.GT.NHCO) GOTO 10
        ITH= ITABL(IPHST,L,JPHSTI)
        IPH= ITABL(IPHST,L,JPHSPI)
        IF(ITH.LE.ITLU.OR.ITH.GE.(63-ITLU)) ILU= 1
        DO 11 L1=1,NNTO
          ITHN= ITHNTO(L1)
          IPHN= IPHNTO(L1)
          IF(ITHN.EQ.ITH.AND.IPHN.EQ.IPH) THEN
            ENOPH(IHCO)= RTABL(IPHST,L,JPHSCE)+ENOPH(IHCO)
            ILU= 0
          END IF
11      CONTINUE
        IF(ILU.EQ.1)
     &      ELUPH(IHCO)= ELUPH(IHCO)+RTABL(IPHST,L,JPHSCE)
10    CONTINUE
C
      RETURN
      END
#endif
