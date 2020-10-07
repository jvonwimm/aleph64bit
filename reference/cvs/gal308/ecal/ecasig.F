*DK ecasig
      SUBROUTINE ECASIG
C----------------------------------------------------------------------
C    Y. Karyotakis 11-3-86   End of Tracking
C! Reduce Hits banks
C - Calls : WDROP                                       from BOSlib
C           AUBPRS                                      from ALEPHLIB
C           CAFIHT                                      from this .HLB
C ----------------------------------------------------
#ifndef DOC
      SAVE
#include "jobcom.h"
#include "bcs.h"
#include "ecnamc.h"
#include "ecstat.h"
      INTEGER CAFIHT
      LOGICAL FDEB
#include "bmacro.h"
C ------------------------------------------------------------
      FDEB = FDEBJO .AND. IPRIJO(4).EQ.1
C
C - drop working bank IDPSIG and reduce size to real one
C
      CALL WDROP (IW,IDPSIG)
      CALL AUBPRS('ESHIEWHI')
      IF(FDEB) THEN
        CALL PRTABL('ESHI',0)
        CALL PRTABL('EWHI',0)
      ENDIF
C
C     Create wires bank
C
      JETHT = CAFIHT (NAESHI,'ETHT')
      IF (JETHT.EQ.0) GOTO 998
      IF (FDEB) CALL PRTABL ('ETHT',0)
C
C - Create tower bank
C
      JEWHT = CAFIHT (NAEWHI,'EWHT')
      IF (JEWHT.EQ.0) GOTO 998
      IF (FDEB) CALL PRTABL ('EWHT',0)
C
C Statistics
C
C    if FHISJO(4)=.TRUE, then fill histograms
C
       IF(FHISJO(4)) THEN
           IDSKI=439
           WSKI=1.
           SKIZ=0.
           DO 123 I=21,30,1
               ECCONT(I)=ECCONT(I)/1000
123        CONTINUE
           CALL HFILL(IDSKI+2,ECCONT(21),SKIZ,WSKI)
           CALL HFILL(IDSKI+3,ECCONT(22),SKIZ,WSKI)
           CALL HFILL(IDSKI+4,ECCONT(23),SKIZ,WSKI)
           CALL HFILL(IDSKI+5,ECCONT(24),SKIZ,WSKI)
           CALL HFILL(IDSKI+6,ECCONT(26),ECCONT(25),WSKI)
           CALL HFILL(IDSKI+7,ECCONT(27),SKIZ,WSKI)
           CALL HFILL(IDSKI+10,ECCONT(27),SKIZ,WSKI)
           CALL HFILL(IDSKI+11,ECCONT(28),SKIZ,WSKI)
           CALL HFILL(IDSKI+12,ECCONT(29),SKIZ,WSKI)
           CALL HFILL(IDSKI+13,ECCONT(30),SKIZ,WSKI)
       ENDIF
C
      KETHT = IW(NAETHT)
      NTOWR = LROWS (KETHT)
      NECONT(4) = NECONT(4) + NTOWR
      IF (NTOWR.GT.0)   NECONT(1) = NECONT(1) + 1
      EEVT = VSUM(ECCONT,3)/1000000.
      ECCONT(7) = ECCONT(7) + EEVT
      ECCONT(8) = ECCONT(8) + EEVT**2
      DO 10 I=1,3
   10 ECCONT(I+3) = ECCONT(I+3) + ECCONT(I)/1000000.
      DO 11 I=1,3
   11 ECCONT(I) = 0.
 980  RETURN
C
C - not enough space
C
 998  CONTINUE
      CALL ALTELL ('ECASIG: not enough space for HIT banks ',1,'NEXT')
      END
#endif
