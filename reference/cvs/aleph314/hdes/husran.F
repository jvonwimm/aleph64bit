        SUBROUTINE HUSRAN(INTET,INPHI,JSTAK,TETST,PHIST,
     *  DTETS,DFIST)
C------------------------------------------------------------------
CKEY HCALDES HCAL TOWER THETA PHI ANGLE / USER
C! compute angles relevant to storey geometry
C!
C!      G.Capon               date : 861001
C!    compute theta,phi of storey center from indices IT,JF,KS,NR
C!    compute dtheta,dphi of storey
C! input :
C!         INTET : theta index of storey
C!         INPHI : phi     "        "
C!         JSTAK : stack number
C!  output:
C!        TETST  : theta of storey center
C!        PHIST  :  phi       "      "
C!        DTETS  : angular width of storey (in theta) projected !!
C!        DFIST  : angular width of storey (in phi)
C!
C------------------------------------------------------------------
#ifndef DOC
#include "alcons.h"
#include "hcgeom.h"
C
C?        convert address to theta,phi of storey center
C
        NREG=IHCREG(INTET)
        NPHI=12*(2**NREG)
        DFIST=TWOPI/FLOAT(NPHI)
        JMOD=(24*(INPHI-1))/NPHI +1
        FIMOD=PIBY6*FLOAT(JMOD/2)
        DFLOC=DFIST*(0.5+FLOAT(INPHI-1))-FIMOD
        TPRST=0.5*(HCTHUL(INTET)+HCTHUL(INTET+1))
        COTPR=COS(TPRST)/SIN(TPRST)
        TETST=ATAN2( 1./COS(DFLOC) , COTPR )
        DTETS=HCTHUL(INTET+1)-HCTHUL(INTET)
        PHIST=DFIST*(FLOAT(INPHI)-0.5)
C?      ! to be corrected for dead zones at low angles
        RETURN
        END
C
#endif
