        REAL FUNCTION YSGETS(IYSVX,ITKIN,IOBJ)
C-----------------------------------------------------------------------------
C!   Calculate the position of YSVX vertex along track ITK in cm (+ or -)
C
C   Input:  IYSVX   BOS index of vertex in YSVX bank
C           ITKIN   row number in YSTL, YSCL, or YS0l
C           IOBJ    = 1, 2, 3 for helices, circles, and V0's, respectively
C
C       Author:  D.Casper  08-07-96
C
C-----------------------------------------------------------------------------
#ifndef DOC
C
        PARAMETER (R_ITC = 12.80)
        PARAMETER (R_TPC = 28.50)
        dimension vx(3),PTK(3)
#include "yscljj.h"
#include "ystljj.h"
#include "ys0ljj.h"
#include "ysvxjj.h"
#include "bcs.h"
C
        DATA NYSTL/0/
        SAVE NYSTL, NYS0L, NYSCL
C
#include "bmacro.h"
C
        IF (NYSTL.EQ.0) THEN
            NYSTL = NAMIND('YSTL')
            NYS0L = NAMIND('YS0L')
            NYSCL = NAMIND('YSCL')
        ENDIF
        ITK = IABS(ITKIN)
        YSGETS = -999.
        VX(1) = RW(IYSVX+JYSVVX)
        VX(2) = RW(IYSVX+JYSVVY)
        VX(3) = RW(IYSVX+JYSVVZ)
        SIGZ2 = RW(IYSVX+JYSVVM+5)
        RVX = SQRT(VX(1)**2 + VX(2)**2)
        ZVX = VX(3)
C
C+  Charged track or V0?
C
        IF (IOBJ.LE.2) THEN
            IF (IOBJ.EQ.1) THEN
                KYSTL = IW(NYSTL)
                IFRFT = ITABL(KYSTL,ITK,JYSTTN)
                IF (RVX.LE.R_ITC) THEN
                    OMEGA = RTABL(KYSTL,ITK,JYSTH0)
                ELSE IF (RVX.LE.R_TPC) THEN
                    OMEGA = RTABL(KYSTL,ITK,JYSTH1)
                ELSE
                    OMEGA = RTABL(KYSTL,ITK,JYSTH2)
                ENDIF
            ELSE
                KYSCL = IW(NYSCL)
                IFRFT = ITABL(KYSCL,ITK,JYSCTN)
                OMEGA = RTABL(KYSCL,ITK,JYSCH0)
            ENDIF
            IF (ABS(OMEGA).LE.1.E-10) RETURN
C            IF (RVX.LE.R_ITC) THEN
C                JHELIX = JYSTH0
C            ELSE IF (RVX.LE.R_TPC) THEN
C                JHELIX = JYSTH1
C            ELSE
C                JHELIX = JYSTH2
C            ENDIF
C
C+  Get appropriate track parameters
C
C            OMEGA = RW(IYSTL+JHELIX)
C            D0 = RW(IYSTL+JHELIX+3)
C            TANL = RW(IYSTL+JHELIX+1)
C            Z0 = RW(IYSTL+JHELIX+4)
C            IF (ABS(1.-OMEGA*D0).LE.1.E-10) YSGETS = -999.
C            ARG = (RVX**2-D0**2)/(1.-OMEGA*D0)
C            IF (ARG.LT.0) RETURN
C            ARG = (OMEGA/2.)*SQRT(ARG)
C            IF (ABS(ARG).GT.1.) RETURN
C            YSGETS = SIGN((2./OMEGA)*ASIN(ARG),(ZVX-Z0)/TANL)
            CALL YHELIX(IFRFT,VX,SIGZ2,PSI,PTK,IER)
            IF (IER.NE.0) RETURN
            YSGETS = PSI/ABS(OMEGA)
        ELSE
            KYS0L = IW(NYS0L)
            IYS0L = KROW(KYS0L,ABS(ITK))
            D0 = RW(IYS0L+JYS0H0+3)
            PHI0 = RW(IYS0L+JYS0H0+2)
            IF (ABS(D0).GT.RVX) RETURN
            YSGETS = SQRT(RVX**2-D0**2)
            CP = COS(PHI0)
            SP = SIN(PHI0)
            IF (-VX(1)*SP+VX(2)*CP .GE. 0.) THEN
                IF (VX(1)*CP+VX(2)*SP.GE.0.)THEN
                    IF (D0.GT.0.) YSGETS = -YSGETS
                ELSE
                    IF (D0.LE.0.) YSGETS = -YSGETS
                ENDIF
            ELSE
                IF (VX(1)*CP+VX(2)*SP.GE.0.) THEN
                    IF (D0.LT.0.) YSGETS = -YSGETS
                ELSE
                    IF (D0.GT.0.) YSGETS = -YSGETS
                ENDIF
            ENDIF    
        ENDIF
        RETURN
        END
#endif
