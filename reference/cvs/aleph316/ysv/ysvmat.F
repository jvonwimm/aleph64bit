        SUBROUTINE YSVMAT
C------------------------------------------------------------------------------
C!  Tag as bad any vertices which are not in a layer of material
C
C       Author - D.Casper   17/07/96
C
C       Modifies:   YSVX/NR(<0)     Sets quality flag for vertices
C                                   not in material
C
C------------------------------------------------------------------------------
#ifndef DOC
C
        PARAMETER(RBP_INNER = 5.30)      ! Beam pipe
        PARAMETER(RBP_OUTER = 5.41)      ! Beam pipe

        PARAMETER(RITC1_INNER = 12.80)   ! ITC Inner Wall
        PARAMETER(RITC1_OUTER = 12.86)   ! ITC Inner Wall

        PARAMETER(RITC2_INNER = 28.50)   ! ITC Outer Wall
        PARAMETER(RITC2_OUTER = 28.83)   ! ITC Outer Wall

        PARAMETER(RTPC1_INNER = 30.95)   ! TPC Inner Wall
        PARAMETER(RTPC1_OUTER = 31.70)   ! TPC Inner Wall

#include "ysvxjj.h"
#include "ysvcut.h"
#include "bcs.h"
C
        DATA NYSVX/0/
        SAVE NYSVX
C
#include "bmacro.h"
C
        IF (NYSVX.EQ.0) THEN
            NYSVX = NAMIND('YSVX')
        ENDIF
        KYSVX = IW(NYSVX)
        DO WHILE (KYSVX.GT.0)
            DO I = 1, LROWS(KYSVX)
                RVX = SQRT(RTABL(KYSVX,I,JYSVVX)**2 +
     &              RTABL(KYSVX,I,JYSVVY)**2)
                RMAT = 1.E10
                IF (RVX.GT.RTPC1_OUTER**2) THEN
                    RMAT = RVX - RTPC1_OUTER
                ELSE IF (RVX.GE.RTPC1_INNER) THEN
                    RMAT = 0.
                ELSE IF (RVX.GT.RITC2_OUTER)THEN
                    RMAT = MIN(ABS(RVX-RITC2_OUTER),
     &                  ABS(RVX-RTPC1_INNER))
                ELSE IF (RVX.GE.RITC2_INNER)THEN
                    RMAT = 0.
                ELSE IF (RVX.GT.RITC1_OUTER)THEN
                    RMAT = MIN(ABS(RVX-RITC1_OUTER),
     &                  ABS(RVX-RITC2_INNER))
                ELSE IF (RVX.GE.RITC1_INNER)THEN
                    RMAT = 0.
                ELSE
                    RMAT = ABS(RVX-RITC1_INNER)
                ENDIF
                IF (ABS(RMAT).GT.YSCTRM) IW(KROW(KYSVX,I)+JYSVQU) =
     &                  IOR(IW(KROW(KYSVX,I)+JYSVQU),256)
            ENDDO
            CALL YSVSRT
            KYSVX = IW(KYSVX-1)
            IF (IW(KYSVX-2).GT.-2) KYSVX = 0
        ENDDO

        RETURN
        END
#endif
