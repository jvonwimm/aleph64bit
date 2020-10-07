      SUBROUTINE MDSCR(NPLIS,IPLIS,IZNDS,PHDIS,NHPLT,
     *          PR1,ERPR1,PR2,ERPR2)
C***********************************************************
C! Define  discriminant variables and compute probability  *
CKEY MUCAL MUON CALOBJ / INTERNAL                          *
C  Authors: U. Bottigli, A.Messineo - 890310
C                                                          *
C  Input : NPLIS = Number of element of IPLIS              *
C          IPLIS = Vector of patterns index                *
C          IZNDS = Flag of hcal region                     *
C          PHDIS = Pulse hight                             *
C          NHPLT = Vector of hits in hcal planes           *
C  Output:                                                 *
C          PR1  =  Probabilty as jet with    mu-prompt     *
C          ERPR1= Error on the probability  PR1            *
C          PR2  = Probabilty as jet with mu-prompt or decay*
C          ERPR2= Error on the probability PR2             *
C                                                          *
C                                                          *
************************************************************
#ifndef DOC
      SAVE
#include "bcs.h"
#include "mucapa.h"
      PARAMETER(NVAR=6)
      INTEGER NHPLT(*),IPLIS(*)
      REAL V(NVAR)
      REAL CHAD(14),CMIP(14),C(8)
      INTEGER ICLAS(2)
      DATA NCOD/0/
      DATA NCUT/8/
      DATA PCUT/1./
      DATA PRCUT/50./
C
C     La prima riga di CHAD, CMIP e C e' la funzione per i
C     mu corti, la seconda per i mu prompt
C
      DATA CHAD/  0.11,  0.75,  0.06, -0.12, -0.14, -3.39, -3.70,
     *            0.11,  0.73,  0.06, -0.13, -0.12, -3.02, -3.57/
      DATA CMIP/  0.01,  1.42,  0.03, -0.16, -0.16, -2.58,-12.43,
     *            0.01,  1.43,  0.02, -0.24, -0.10,  2.60,-15.25/
      DATA C/    49.44817,    51.87961,     1.00954,     0.62975,
     *           49.59973,    51.69600,     1.01938,     0.63761/
#include "bmacro.h"
C
C We calculate here the set of used discriminant variables  :
C
C    **********************************
C
C   V(1)   PH = Pulse hight
C   V(2)   LASTP = Last  fired  plane
C   V(3)   I1 = hits itegral plane 1  - 7
C   V(4)   I2 = hits itegral plane 8  - 14
C   V(5)   I1 = hits itegral plane 15 - LASPLN
C   V(6)   MU = hadron calorimeter muon flag
C
C    **********************************
C
      PR1  =0.
      ERPR1=0.
      PR2  =0.
      ERPR2=0.
      DO 2000 LL=1,NVAR
       V(LL)=0.
 2000 CONTINUE
C
C    PH = Pulse hight
C
      PH=PHDIS
      V(1)=PH
C
C    LASTP = Last  fired  plane
C
      LASTP=-1
      DO 200 I=1,LASPLN
       IF(NHPLT(I).NE.0) LASTP=I
 200  CONTINUE
      V(2)=LASTP
C
C     Partial integral : 1-7 8-14 15-LASPLN
C
      I1=0
      DO 210 I=1,7
       I1=I1+NHPLT(I)
 210  CONTINUE
      V(3)=I1
      I2=0
      DO 220 I=8,14
       I2=I2+NHPLT(I)
 220  CONTINUE
      V(4)=I2
      I3=0
      DO 230 I=15,LASPLN
       I3=I3+NHPLT(I)
 230  CONTINUE
      V(5)=I3
      MU=0
C
C    MU = hadron calorimeter muon flag
C
      IF(NHPLT(MUFLAG).GE.1) MU=1
      V(6)=MU
      IF(IZNDS.NE.0.AND.MU.EQ.1) LASTP=LASPLN
C
C ICLAS set the row of the hmds bank to be read
C
      NTOT=I1+I2+I3
      IF(NTOT.LT.NCUT.OR.PH.LT.PCUT) GOTO 999
C     ICLAS(2)=3
C     IF(IZNDS.GE.1) ICLAS(1)=1
C     IF(IZNDS.EQ.0) ICLAS(1)=2
      ICLAS(1)=1
      ICLAS(2)=2
C
C We calculate here the scores for the calobjet
C
      DO 1 I=1,2
       IDROW=ICLAS(I)
       SC0=0.
       SC1=0.
       DO 2 K=1,NVAR
        K1=K+(IDROW-1)*(NVAR+1)
        SC0=SC0+CHAD(K1)*V(K)
        SC1=SC1+CMIP(K1)*V(K)
 2     CONTINUE
       NDIM=IDROW*(NVAR+1)
       SC0=SC0+CHAD(NDIM)
       SC1=SC1+CMIP(NDIM)
       DIF=ABS(SC0-SC1)
C
C Here we estimate  the probability
C
       UB=EXP(-C(4*(IDROW-1)+4)*DIF)
       UB1=(1.-C(4*(IDROW-1)+3)*UB)
       PR=C(4*(IDROW-1)+1)+C(4*(IDROW-1)+2)*UB1
C
       IF(SC0.GT.SC1) THEN
        PRHAD=PR
        PRMIP=(100.-PR)
       ELSE
        PRMIP=PR
        PRHAD=(100.-PR)
       ENDIF
       IF(PRHAD.GT.100.) THEN
        PRHAD=100.
        PRMIP=0.
       ENDIF
       IF(PRMIP.GT.100.) THEN
        PRHAD=0.
        PRMIP=100.
       ENDIF
       ERHAD=SQRT(PRHAD*(1.-PRHAD/100.))
       ERMIP=SQRT(PRMIP*(1.-PRMIP/100.))
       IF(I.EQ.1) THEN
        PR2=PRMIP
        ERPR2=ERMIP
       ELSE
        PR1=PRMIP
        ERPR1=ERMIP
       ENDIF
 1    CONTINUE
  999 CONTINUE
      END
#endif
