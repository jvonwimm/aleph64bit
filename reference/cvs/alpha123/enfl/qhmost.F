      SUBROUTINE QHMOST(IER)
C-----------------------------------------------------------------------
C! Get the 4 most energetic HCAL towers in PHCO clusters
C  Author   G.Ganis      2 July 1991
C  Input    None
C  Output         following arrays are dimensioned 4x200
C                 for cluster  j-th. COMMON /CUMOST/
C           ETW(i,j) energy in the 4 most energetic towers ( GeV )
C                    i=1,...,4  descending order
C           TWH(i,j) theta of i-th tower ( radians )
C           PWH(i,j) phi   of i-th tower ( radians )
C           JST(i,j) position in PHST bank of i-th tower
C           IER= 0  ok
C                1  PHCO or PHST bank missing
C                2  PHCO too large
C-----------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
#include "qhac.h"
#include "cuhnto.h"
#include "cuphco.h"
      PARAMETER (ltow=50)
      INTEGER NTOW(LCLU),JHST(LTOW,LCLU)
      REAL    ETOW(LTOW,LCLU)
      REAL    AWW(LTOW)
      INTEGER IWW(LTOW)
#include "qmacro.h"
C-----------------------------------------------------------------------
      IER= 0
C - Link to proper banks
      IPHCO= NLINK('PHCO',0)
      IPHST= NLINK('PHST',0)
      IF(IPHCO.EQ.0.OR.IPHST.EQ.0) THEN
       IER= 1
       GOTO 999
      END IF
      NPHCO= LROWS(IPHCO)
      IF(NPHCO.GT.LCLU) THEN
       IER= 2
       GOTO 999
      END IF
      NPHST= LROWS(IPHST)
C - Initialize arrays
      CALL VZERO(NTOW,NPHCO)
      NARRAY= NPHCO*LTOW
      CALL VZERO(ETOW,NARRAY)
      CALL VZERO(JHST,NARRAY)
C - Fill them
      DO 10 L1=1,NPHST
       ITH= ITABL(IPHST,L1,JPHSTI)
       IPH= ITABL(IPHST,L1,JPHSPI)
       DO 11 L2=1,NNTO
        IF(ITH.EQ.ITHNTO(L2).AND.IPH.EQ.IPHNTO(L2)) GOTO 10
11     CONTINUE
       IROW= ITABL(IPHST,L1,JPHSPH)
       IF (IROW.EQ.0) THEN
          IER= 1
          GOTO 999
       ENDIF
       ECOR= RTABL(IPHST,L1,JPHSCE)
       IF(NTOW(IROW).EQ.LTOW) GOTO 10
       NTOW(IROW)= NTOW(IROW)+1
       ETOW(NTOW(IROW),IROW)= ECOR
       JHST(NTOW(IROW),IROW)= L1
10    CONTINUE
C - Sort towers following energy
      NPH= NPHCO
      DO 20 L0=1,NPHCO
       DO L1=1,NTOW(L0)
        AWW(L1)= ETOW(L1,L0)
       END DO
       IF ( ntow(l0) .GT. 0 )
     . CALL SORTZV(AWW,IWW,NTOW(L0),1,1,0)
       DO L1=1,4
        IF(L1.GT.NTOW(L0)) THEN
         ETW(L1,L0)= 0.
         JST(L1,L0)= 0
         TWH(L1,L0)= 0.
         PWH(L1,L0)= 0.
        ELSE
         ETW(L1,L0)= ETOW(IWW(L1),L0)
         JST(L1,L0)= JHST(IWW(L1),L0)
         ITH= ITABL(IPHST,JST(L1,L0),JPHSTI)
         IPH= ITABL(IPHST,JST(L1,L0),JPHSPI)
C-- Get polar coordinates for the tower
         IST= 1
         CALL HUSRAN(ITH,IPH,IST,THW,PHW,DTH,DPH)
         TWH(L1,L0)= THW
         PWH(L1,L0)= PHW
        END IF
       END DO
20    CONTINUE
C
999   CONTINUE
      RETURN
      END
#endif
