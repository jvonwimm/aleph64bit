        SUBROUTINE YSPCUT(KYSVX,IT1,IT2)
C------------------------------------------------------------------------------
C!  Apply cuts to pair vertices and set vertex quality flag
C
C       Author:  D.Casper 08/07/1996
C
C------------------------------------------------------------------------------
#ifndef DOC
C
        PARAMETER (R_ITC = 12.80)
        PARAMETER (R_TPC = 28.50)
#include "ysvxjj.h"
#include "ystljj.h"
#include "ys0ljj.h"
#include "ysvcut.h"
#include "alcons.h"
#include "bcs.h"
C
        DIMENSION VX(3),P(3)
        DATA NYSTL/0/
        SAVE NYSTL, NYS0L
C
#include "bmacro.h"
C
        IF (NYSTL.EQ.0) THEN
            NYSTL = NAMIND('YSTL')
            NYS0L = NAMIND('YS0L')
        ENDIF
        KYSTL = IW(NYSTL)
        KYS0L = IW(NYS0L)
        IYSVX = KROW(KYSVX,LROWS(KYSVX))

C
C+  Chisq of this vertex must be good enough
C
        IF (RW(IYSVX+JYSVC2).GT.YSCTC2) THEN
            IW(IYSVX+JYSVQU) = IOR(IW(IYSVX+JYSVQU),32)
        ENDIF

        VX(1) = RW(IYSVX+JYSVVX)
        VX(2) = RW(IYSVX+JYSVVY)
        VX(3) = RW(IYSVX+JYSVVZ)
C
C+  Radius must be above the minimum we care about
C
        RVXSQ = VX(1)**2 + VX(2)**2
        IF (RVXSQ.LT.YSCTRL .OR. RVXSQ.GT.YSCTRH) THEN
            IW(IYSVX+JYSVQU) = IOR(IW(IYSVX+JYSVQU),2)
        ENDIF
C
C+  Both tracks must be inconsistent with the primary
C
        IF (IT1.GT.0) THEN
            CHIP1 = RTABL(KYSTL,IT1,JYSTCP)
            D0_1 = RTABL(KYSTL,IT1,JYSTH0+3)
            IOBJ1 = 1
        ELSE
            CHIP1 = RTABL(KYS0L,-IT1,JYS0CP)
            D0_1 = RTABL(KYS0L,-IT1,JYS0H0+3)
            IOBJ1 = 3
        ENDIF
        IF (IT2.GT.0) THEN
            CHIP2 = RTABL(KYSTL,IT2,JYSTCP)
            D0_2 = RTABL(KYSTL,IT2,JYSTH0+3)
            IOBJ2 = 1
        ELSE
            CHIP2 = RTABL(KYS0L,-IT2,JYS0CP)
            D0_2 = RTABL(KYS0L,-IT2,JYS0H0+3)
            IOBJ2 = 3
        ENDIF
        IF (MIN(CHIP1,CHIP2).LT.YSCTPV) THEN
            IW(IYSVX+JYSVQU) = IOR(IW(IYSVX+JYSVQU),4)
        ENDIF
C
C+  Vertex must be at radius greater than d0 for both tracks
C
        IF (RVXSQ.LT.MAX(D0_1**2,D0_2**2))THEN
            IW(IYSVX+JYSVQU) = IOR(IW(IYSVX+JYSVQU),8)
        ELSE
C
C+  Vertex must be at positive arclength for track most consistent with
C+  primary. Only do the potentially tricky calculations for tracks
C+  which might pass.
C
          IF (IW(IYSVX+JYSVQU) .EQ. 0) THEN
            S1 = YSGETS(IYSVX,IT1,IOBJ1)
            S2 = YSGETS(IYSVX,IT2,IOBJ2)
            IF (CHIP1.LT.CHIP2) THEN
                SMIN = S1
            ELSE
                SMIN = S2
            ENDIF
            IF (SMIN.LT.YSCTSM) THEN
                IW(IYSVX+JYSVQU) = IOR(IW(IYSVX+JYSVQU),16)
            ENDIF
C
C+  Momentum of both tracks from the vertex must somewhat forward
C
            RVX = SQRT(RVXSQ)
            IF (RVX .LT. R_ITC) THEN
                JHELIX = JYSTH0
            ELSE IF (RVX .LT. R_TPC) THEN
                JHELIX = JYSTH1
            ELSE
                JHELIX = JYSTH2
            ENDIF
C
C+  Get momentum vector at vertex
C
            BFIELD = ALFIEL(DUM)
            PCONV = BFIELD*CLGHT/1.E5
            VX(1) = VX(1)/SQRT(RVXSQ+VX(3)**2)
            VX(2) = VX(2)/SQRT(RVXSQ+VX(3)**2)
            VX(3) = VX(3)/SQRT(RVXSQ+VX(3)**2)
            IF (RVX.GT.3) THEN
                IF (IT1.GT.0) THEN
                    OMEGA = RW(KROW(KYSTL,IT1)+JHELIX)
                    TANL = RW(KROW(KYSTL,IT1)+JHELIX+1)
                    PHI0 = RW(KROW(KYSTL,IT1)+JHELIX+2)
                    PT = PCONV/ABS(OMEGA)
                    P(1) = PT*COS(OMEGA*S1+PHI0)
                    P(2) = PT*SIN(OMEGA*S1+PHI0)
                    P(3) = PT*TANL
                ELSE
                    PMOM = RW(KROW(KYS0L,-IT1)+JYS0H0)
                    TANL = RW(KROW(KYS0L,-IT1)+JYS0H0+1)
                    PHI0 = RW(KROW(KYS0L,-IT1)+JYS0H0+2)
                    SECL = SQRT(1.+TANL**2)
                    P(1) = PMOM*COS(PHI0)/SECL
                    P(2) = PMOM*SIN(PHI0)/SECL
                    P(3) = PMOM*TANL/SECL
                ENDIF
                PL = (P(1)*VX(1)+P(2)*VX(2)+P(3)*VX(3))
                IF (PL .LT. -0.2) 
     &               IW(IYSVX+JYSVQU) = IOR(IW(IYSVX+JYSVQU),64)
                IF (IT2.GT.0) THEN
                    OMEGA = RW(KROW(KYSTL,IT2)+JHELIX)
                    TANL = RW(KROW(KYSTL,IT2)+JHELIX+1)
                    PHI0 = RW(KROW(KYSTL,IT2)+JHELIX+2)
                    PT = PCONV/ABS(OMEGA)
                    P(1) = PT*COS(OMEGA*S2+PHI0)
                    P(2) = PT*SIN(OMEGA*S2+PHI0)
                    P(3) = PT*TANL
                ELSE
                    PMOM = RW(KROW(KYS0L,-IT2)+JYS0H0)
                    TANL = RW(KROW(KYS0L,-IT2)+JYS0H0+1)
                    PHI0 = RW(KROW(KYS0L,-IT2)+JYS0H0+2)
                    SECL = SQRT(1.+TANL**2)
                    P(1) = PMOM*COS(PHI0)/SECL
                    P(2) = PMOM*SIN(PHI0)/SECL
                    P(3) = PMOM*TANL/SECL
                ENDIF
                PL = (P(1)*VX(1)+P(2)*VX(2)+P(3)*VX(3))
                IF (PL .LT. -0.2) 
     &           IW(IYSVX+JYSVQU) = IOR(IW(IYSVX+JYSVQU),64)
            ENDIF
          ENDIF
        ENDIF
C
C+  Chisq of this vertex must be good enough
C
C        IF (RW(IYSVX+JYSVC2).GT.YSCTC2) THEN
C            IW(IYSVX+JYSVQU) = IOR(IW(IYSVX+JYSVQU),32)
C        ENDIF
C
        RETURN
        END
#endif
