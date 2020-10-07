      SUBROUTINE SILSUM(NCL,IVOI,ERLUM)
C----------------------------------------------------------------------
C!  - Build energy sums in roads of 2 phi pad width for 3 radius bins
C!   and 5  Doublets stacks depth for a given cluster
C!   Author   : John Rander , B.Bloch-Devaux February 92,February 93
C!
C!   Inputs:    NCL cluster number
C!              IVOI array of possible pads to be considered
C!   Outputs:   ERLUM(5,3) best energy sums over 5 doublet planes
C!              for 3 highest radius bins
C!
C?---------------------------------------------------------------------
C?   Description
C?   ===========
C?      Sums are done for the two possible roads in each R bin
C?      over 5 possible doublets of planes
C?      One keeps the road  which gives the highest doublet sum in the
C?      central R bin.
C?
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "sinamc.h"
#include "spdajj.h"
        DIMENSION IVOI(3,10,2),ERLUM(5,3)
        DIMENSION LAYR1(5),LAYR2(5)
        DIMENSION ERAD5(5,3,2)
        DATA LAYR1/1,2,2,3,3/
        DATA LAYR2/2,3,4,4,5/
#include "bmacro.h"
C------------------------------------------------------------------
        CALL VZERO(ERAD5(1,1,1),30)
      KSPDA = IW(NASPDA)
      KSMAP = IW(NASMAP)
C
C===> Loop over IVOI ADDRESSES ,check if they belong to cluster
C      and sum energies in either road
C
C
C    Loop over pad-list for radial triplets
      DO 10   IROD =  1,2
        DO 11   KRB =  1,3
        DO 13   JK =  1,10
          IF (IVOI(KRB,JK,IROD).LE.0) GO TO 13
            ITOW= ITABL(KSMAP,IVOI(KRB,JK,IROD),1)
            IF (ITOW.GT.0) THEN
              ICLU= ITABL(KSPDA,ITOW,JSPDSC)
C       if cluster pad address is on the list include the energy
              IF(ICLU.NE.NCL) GOTO 13
          IZ = (JK-1)/2 + 1
          ERAD5(IZ,KRB,IROD)= ERAD5(IZ,KRB,IROD)+
     $                             RTABL(KSPDA,ITOW,JSPDEN)
            ENDIF
   13     CONTINUE
   11   CONTINUE
   10 CONTINUE
C
C==> MAKE UP SUMS FROM EPARSU VALUES....
C   LS=1 SILESU12 = SUM LAYERS 1 & 2 FOR HIGHEST CHOICE IR=2 BIN
C   LS=2 SILESU23 =            2   3
C   LS=3 SILESU24 =            2   4
C   LS=4 SILESU34 =            3   4
C   LS=5 SILESU35 =            3   5
C
C===> Select road (ROAD1 or ROAD2) which contains the highest energy in
C     the central R bin over the plane doublets
          DO 14 ICOM =  1,5
            L1 = LAYR1(ICOM)
            L2 = LAYR2(ICOM)
            IPR=1
            ES1=ERAD5(L1,2,1)+ERAD5(L2,2,1)
            ES2=ERAD5(L1,2,2)+ERAD5(L2,2,2)
            IF(ES1.LT.ES2) IPR=2
            DO 15 KRB =  1,3
              ERLUM(ICOM,KRB)=ERAD5(L1,KRB,IPR)+ERAD5(L2,KRB,IPR)
   15   CONTINUE
   14 CONTINUE
      RETURN
      END
#endif
