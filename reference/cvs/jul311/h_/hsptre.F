      SUBROUTINE HSPTRE(NSPN0)
C*************************************************************
C! Superpattern reconstruction                               *
C!                                                           *
C! Author :  R. TENCHINI      880408                         *
C!                                                           *
C! Version 1.0  880408                                       *
C!                                                           *
C! INPUT   HSPE FAMILY OF BANKS, HPCO ECLU ECT1 BANKS        *
C! Output  HSPT BANK                                         *
C!                                                           *
C! NSPTT0=Number  of superpatterns                           *
C!                                                           *
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "hpcojj.h"
#include "hspejj.h"
#include "hsptjj.h"
#include "eclujj.h"
      LOGICAL HPLINK
#include "bmacro.h"
      IF(NSPN0.EQ.0) GOTO 30
C
C HSPT is initialized
C
      LDSPT    = LHSPTA*NSPN0+LMHLEN
      CALL AUBOS('HSPT',0,LDSPT,IHSPT,IGARB)
      IF(IGARB.EQ.2) THEN
         CALL RERROR('HSPTRE',1,'INSUFF. SPACE FOR HSPT')
         GOTO 30
      ENDIF
      IW(IHSPT + LMHCOL) = LHSPTA
      IW(IHSPT + LMHROW) = NSPN0
C
C  We link the proper banks
C
      IHPCO=NLINK('HPCO',0)
      IF(IHPCO.EQ.0) THEN
         CALL RERROR('HSPTRE',2,'HPCO BANK NOT FOUND')
         GOTO 30
      ENDIF
      NPATT=LROWS(IHPCO)
      IECLU=NLINK('ECLU',0)
      IF(IECLU.EQ.0) THEN
         CALL RERROR('HSPTRE',3,'ECLU BANK NOT FOUND')
         GOTO 30
      ENDIF
      NECLU=LROWS(IECLU)
C
C We recall the TPC tracks connection
C
      IECT1=NLINK('ECT1',0)
      DO 20 I=1,NSPN0
         IHSPA=NLINK('HSPE',I)
         NELEM=LROWS(IHSPA)
C
C We compute the number of charged ECluster in the Superpattern,
C the first plane and the last plane.
C
         NCHA=0
         NFPL=24
         NLPL=-3
         EDIGS = 0.
         ECLUS = 0.
         DO 10 J=1,NELEM
            IDCOD=IW(KROW(IHSPA,J)+JHSPER)
            ICK=IDCOD/10000
            ICLU=MOD(IDCOD,10000)
            IF(ICK.EQ.2) THEN
               IF(IW(IECT1+ICLU+2).NE.0) NCHA=NCHA+1
            ENDIF
            IF(ICK.EQ.1) THEN
               IFP=ITABL(IHPCO,ICLU,JHPCFP)
               ILP=ITABL(IHPCO,ICLU,JHPCLP)
               IF(IFP.LT.NFPL) NFPL=IFP
               IF(ILP.GT.NLPL) NLPL=ILP
               EDIGS = EDIGS + RTABL(IHPCO,ICLU,JHPCDE)
            ELSEIF(ICK.EQ.2) THEN
C
C This is a trick to define first and last plane in ECLU
C
               E1=RTABL(IECLU,ICLU,JECLE1)
               E2=RTABL(IECLU,ICLU,JECLE2)
               E3=RTABL(IECLU,ICLU,JECLE3)
               ECLUS = ECLUS + RTABL(IECLU,ICLU,2)
               IFP=0
               ILP=0
               IF(E1.GT.0) THEN
                  IFP=-3
               ELSE
                  IF(E2.GT.0) THEN
                     IFP=-2
                  ELSE
                     IF(E3.GT.0) THEN
                        IFP=-1
                     ENDIF
                  ENDIF
               ENDIF
               IF(IFP.LT.NFPL) NFPL=IFP
               IF(E3.GT.0) THEN
                  ILP=-1
               ELSE
                  IF(E2.GT.0) THEN
                     ILP=-2
                  ELSE
                     IF(E1.GT.0) THEN
                        ILP=-3
                     ENDIF
                  ENDIF
               ENDIF
               IF(ILP.GT.NLPL) NLPL=ILP
            ENDIF
   10    CONTINUE
         IW(KROW(IHSPT,I)+JHSPNC)=NCHA
         IW(KROW(IHSPT,I)+JHSPFP)=NFPL
         IW(KROW(IHSPT,I)+JHSPLP)=NLPL
         RW(KROW(IHSPT,I)+JHSPDE)=EDIGS
         RW(KROW(IHSPT,I)+JHSPEE)=ECLUS
   20 CONTINUE
   30 RETURN
      END
#endif
