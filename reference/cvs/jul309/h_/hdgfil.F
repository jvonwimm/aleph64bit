      SUBROUTINE HDGFIL
C*************************************************************
C! Digital Pattern histograms                                *
C!                                                           *
C! AUTHOR :  G. Bagliesi      880511                         *
C!                                                           *
C! VERSION 1.1  890126                                       *
C!                                                           *
C! INPUT   HPCO HCLU ECLU                                    *
C!                                                           *
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "alcons.h"
#include "rparac.h"
#include "rflags.h"
#include "hpcojj.h"
C     LOGICAL HCINT,LINKAT,HSLINK
#include "bmacro.h"
C
      JOFF = JULHC * 1000
      IHPCO=NLINK('HPCO',0)
      IF(IHPCO.EQ.0) THEN
         NPATT = 0
      ELSE
         NPATT = LROWS(IHPCO)
      ENDIF
      IHCLU=NLINK('HCLU',0)
      IF(IHCLU.EQ.0) THEN
         NHCLU = 0
      ELSE
         NHCLU = LROWS(IHCLU)
      ENDIF
C
      IECLU=NLINK('ECLU',0)
      IF(IECLU.EQ.0) THEN
         NECLU = 0
      ELSE
         NECLU = LROWS(IECLU)
      ENDIF
C
      EDIGS = 0.
      EANAS = 0.
      NHITS = 0
      DO 100 I=1,NPATT
         IDIR   = ITABL(IHPCO,I,JHPCDI)
         EDIG   = RTABL(IHPCO,I,JHPCDE)
         COR1   = RTABL(IHPCO,I,JHPCC1)
         COR2   = RTABL(IHPCO,I,JHPCC2)
         NHIT   = ITABL(IHPCO,I,JHPCND)
         INTP   = ITABL(IHPCO,I,JHPCIP)
         IPL    = ITABL(IHPCO,I,JHPCFP)
         LPL    = ITABL(IHPCO,I,JHPCLP)
         DMAX   = RTABL(IHPCO,I,JHPCMD)
         DEN    = RTABL(IHPCO,I,JHPCPD)
         CENX   = RTABL(IHPCO,I,JHPCCX)
         CENY   = RTABL(IHPCO,I,JHPCCY)
         IBORD  = ITABL(IHPCO,I,JHPCBF)
         LHTPA  = ITABL(IHPCO,I,JHPCHT)
         LHPDS  = ITABL(IHPCO,I,JHPCHP)
         EDIGS  = EDIGS + EDIG
         NHITS  = NHITS + NHIT
         CALL HFILL(JOFF+205,FLOAT(IPL),0.,1.)
         CALL HFILL(JOFF+206,FLOAT(LPL),0.,1.)
         CALL HFILL(JOFF+207,FLOAT(INTP),0.,1.)
         CALL HFILL(JOFF+209,FLOAT(NHIT),0.,1.)
         CALL HFILL(JOFF+211,FLOAT(IDIR),0.,1.)
         CALL HFILL(JOFF+208,FLOAT(INTP-IPL),0.,1.)
 100  CONTINUE
      DO 200 JJ=1,NHCLU
         CCCLU=ITABL(IHCLU,JJ,1)
         ECCLU=RTABL(IHCLU,JJ,2)
         E1CLU=RTABL(IHCLU,JJ,3)
         E2CLU=RTABL(IHCLU,JJ,4)
         RCCLU=RTABL(IHCLU,JJ,5)
         R1CLU=RTABL(IHCLU,JJ,6)
         R2CLU=RTABL(IHCLU,JJ,7)
         TCCLU=RTABL(IHCLU,JJ,8)
         T1CLU=RTABL(IHCLU,JJ,9)
         T2CLU=RTABL(IHCLU,JJ,10)
         PCCLU=RTABL(IHCLU,JJ,11)
         P1CLU=RTABL(IHCLU,JJ,12)
         P2CLU=RTABL(IHCLU,JJ,13)
         FSCLU=RTABL(IHCLU,JJ,14)
         EANAS = EANAS + ECCLU
 200  CONTINUE
      EECAS = 0.
      EECAS1 = 0.
      EECAS2 = 0.
      EECAS3 = 0.
      DO 300  JJJ=1,NECLU
         E4ECLU = RTABL(IECLU,JJJ,2)
         E1ECLU = RTABL(IECLU,JJJ,3)
         E2ECLU = RTABL(IECLU,JJJ,4)
         E3ECLU = RTABL(IECLU,JJJ,5)
         R4ECLU = RTABL(IECLU,JJJ,6)
         R1ECLU = RTABL(IECLU,JJJ,7)
         R2ECLU = RTABL(IECLU,JJJ,8)
         R3ECLU = RTABL(IECLU,JJJ,9)
         EECAS  = EECAS  + E4ECLU
         EECAS1 = EECAS1 + E1ECLU
         EECAS2 = EECAS2 + E2ECLU
         EECAS3 = EECAS3 + E3ECLU
 300  CONTINUE
C
      CALL HFILL(JOFF+201,FLOAT(NPATT),FLOAT(NHCLU),1.)
      CALL HFILL(JOFF+202,FLOAT(NPATT),FLOAT(NECLU),1.)
      CALL HFILL(JOFF+203,EDIGS,EANAS,1.)
      CALL HFILL(JOFF+204,EANAS-EDIGS,0.,1.)
      CALL HFILL(JOFF+210,FLOAT(NHITS),0.,1.)
      RETURN
      END
#endif
