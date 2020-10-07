        SUBROUTINE HSRCR(SYSTM,IT,JF,KS,LHPOI,CORNR)
C------------------------------------------------------------------
CKEY HCALDES HCAL TOWER CORNERS  / USER
C! i,j,k to storey corners conversion
C!
C!      G.Capon               date : 861001
C!   Subroutine to compute coordinates of storey corners
C!     from storey indices IT,JF,NR,KS
C! input :
C!         SYSTM : reference system (not used up to now)
C!           IT  : theta index of storey
C!           JF  : phi     "        "
C!           KS  : stack number
C!          LHPOI: number of storey points (=8 in general case)
C! output:
C!     CORNR(3,K): x,y,z coord of corner K (K=1,LHPOI) in Aleph syst
C!
C------------------------------------------------------------------
#ifndef DOC
#include "alcons.h"
#include "hcgeom.h"
C
        CHARACTER* (*) SYSTM (*)
        REAL YL(2),TP(2),FIL(2),ZE(2),CORNR(3,*)
        INTEGER JTBAR(3)
        DATA JTBAR/14,16,15/
C
        NR=IHCREG(IT)
        TP(1)=HCTHUL(IT)
        TP(2)=HCTHUL(IT+1)
        NPHI=12*(2**NR)
        DPHI=TWOPI/FLOAT(NPHI)
        NSTMO=NPHI/24
        JMOD=(JF-1)/NSTMO+1
        FIMOD=PIBY6*FLOAT(JMOD/2)
        FIL(1)=DPHI*FLOAT(JF-1)-FIMOD
        FIL(2)=FIL(1)+DPHI
        SINF=SIN(FIMOD)
        COSF=COS(FIMOD)
C
C?         set values for limiting planes
C?         y=const for barrel , z= const for endcap
C?         1 : inner storey face , 2 : outer storey face
C
        SIGNZ=1.
        JTETA=IT
        IF (IT.GT.LHCTR/2) THEN
           SIGNZ=-1.
           JTETA=LHCTR1-IT
           ENDIF
        IF(KS.EQ.1) THEN
           YL(1)=HCRMIN(1)
           YL(2)=YBAST1
           ZE(1)=HCZMIN(2)
           ZE(2)=ZENST1
           IF(JTETA.GE.10.AND.JTETA.LE.13)ZE(1)=HCZMIN(3)
           ELSE
           YL(1)=YBAST1
           YL(2)=YBARMX
           ZE(1)=ZENST1
           ZE(2)=ZENDMX
           IF(JTETA.GE.15.AND.JTETA.LE.17) YL(2)=HCRMAX(3)
           IF(JTETA.EQ.18) ZE(2)=HCZMAX(1)
           ENDIF
C
C?       intersect planes with straight lines
C?       phi = const , theta(proj) = const
C
        DO 10 NT=1,2
        DO 10 NF=1,2
        DO 10 IFACE=1,2
        N=NT+2*(NF-1)+4*(IFACE-1)
        LEVEL=KS+IFACE-1
        INTBA=0
        IF(JTETA.GE.JTBAR(LEVEL)) INTBA=1
        IF(LEVEL.EQ.3.AND.JTETA.EQ.18) INTBA=0
        IF (INTBA.GT.0) THEN
C?         intersections for barrel geometry
           XL=-YL(IFACE)*TAN(FIL(NF))
           CORNR(1,N)= XL*SINF+YL(IFACE)*COSF
           CORNR(2,N)=-XL*COSF+YL(IFACE)*SINF
           CORNR(3,N)=YL(IFACE)*COS(TP(NT))/SIN(TP(NT))
           ELSE
           ZZ=ZE(IFACE)*SIGNZ
C?            intersections for endcap geometry
           YLL=ZZ*TAN(TP(NT))
           XL=-YLL*TAN(FIL(NF))
           CORNR(1,N)= XL*SINF+YLL*COSF
           CORNR(2,N)=-XL*COSF+YLL*SINF
           CORNR(3,N)= ZZ
           ENDIF
   10    CONTINUE
C
C
        RETURN
        END
#endif
