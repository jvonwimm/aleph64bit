        SUBROUTINE YSFTLD(MASK, IER)
C---------------------------------------------------------------------------
C!  Load the tracks in MASK into the list to be fitted
C
C       Author:  D.Casper   17/07/96
C
C---------------------------------------------------------------------------
#ifndef DOC
#include "ysvwrk.h"
#include "bcs.h"
C
        DIMENSION MASK(*)
        INTEGER MASKB(32)
        SAVE MASKB
C
#include "bmacro.h"
C
        IF (MASKB(1).EQ.0)THEN
            DO IB = 1, 32
                MASKB(IB) = IBSET(0,IB-1)
            ENDDO
        ENDIF
C
C+  Determine how many tracks, circles, and V0s are in the fit
C
        IER = 0
        NTRK = 0
        DO IWRD = 1,3
          NTRK = NTRK + NUMBIT(MASK(IWRD))
        ENDDO
        NCIR = NUMBIT(MASK(4))
        NV0 = NUMBIT(MASK(5))
C
C+  Set up the fit
C
        IF (LROWS(INDYSVW).NE.0 .OR. NTRK.NE.LROWS(INDYSTL) .OR.
     &      NCIR.NE.LROWS(INDYSCL) .OR. NV0.NE.LROWS(INDYS0L)) THEN
            CALL YSFSET(0,NTRK,NV0,NCIR,IER)
            IF (IER.NE.0) GOTO 999
        ENDIF
        ITROW = 0
        ICROW = 0
        IVROW = 0
        DO IBIT = 1, 32
          DO IWRD = 1, 3
            IF (IAND(MASK(IWRD),MASKB(IBIT)).NE.0)THEN
              ITROW = ITROW + 1
              CALL YSTADD(ITROW,32*(IWRD-1)+IBIT,IER)
              IF (IER.NE.0) THEN
                GOTO 999
              ENDIF
            ENDIF
          ENDDO
          IF (IAND(MASK(4),MASKB(IBIT)).NE.0)THEN
            ICROW = ICROW + 1
            CALL YSCADD(ICROW,IBIT,IER)
            IF (IER.NE.0) GOTO 999
          ENDIF
          IF (IAND(MASK(5),MASKB(IBIT)).NE.0)THEN
            IVROW = IVROW + 1
            CALL YS0ADD(IVROW,IBIT,IER)
            IF (IER.NE.0) GOTO 999
          ENDIF
        ENDDO
        IER = 0
 999    CONTINUE
        RETURN
        END
#endif
