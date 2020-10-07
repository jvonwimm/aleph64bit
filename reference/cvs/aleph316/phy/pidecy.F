        SUBROUTINE PIDECY(PIM,EM,PV)
CKEY PHYSICS MCARLO /INTERNAL
C. *****************************************************************
C.   Author : Jane Ratcliffe   6/6/88                              *
C! Handle Dalitz Decays  of pi0                                    *
C.   Input arguments :PIM      Mass of parent PI0                  *
C.                     EM       Mass of Electron                   *
C.   Output argument :       PV(4,3)                               *
C                     px,py,pz,energy for Gamma,Positron,Electron  *
C.                                                                 *
C.   Method is Described in ALEPH note ..........                  *
C. *****************************************************************
#ifndef DOC
      SAVE
#include "alcons.h"
        DIMENSION PV(4,3)
        DIMENSION R(9),PG(3),PE1(3),PE2(3),PGP(3),PE1P(3),PE2P(3)
        DATA FLAM,ALPHA,RSQ,A/1.7,8.5,.000057343,0.2/
C
C-- Get X and Y which Define Dalitz decay in the C. of M.
        CALL GETX(FLAM,ALPHA,RSQ,A,X1)
        CALL GETY(RSQ,A,X1,Y1)
C
        EGAM=(PIM/2.)*(1.-X1)
        EL1=0.5*(PIM-EGAM)+(0.25*Y1*PIM)*(1.-X1)
        EL2=0.5*(PIM-EGAM)-(0.25*Y1*PIM)*(1.-X1)
        Q1=SQRT(EL1**2-EM**2)
        Q2=SQRT(EL2**2-EM**2)
        COSTH1=(Q2**2-Q1**2-EGAM**2)/(2.*EGAM*Q1)
        COSTH2=(Q1**2-Q2**2-EGAM**2)/(2.*EGAM*Q2)
        IF(ABS(COSTH1).GT.1.)COSTH1=SIGN(1.,COSTH1)
        IF(ABS(COSTH2).GT.1.)COSTH2=SIGN(1.,COSTH2)
        TH1=ACOS(COSTH1)
        TH2=ACOS(COSTH2)
C
        PG(1)=0.
        PG(2)=0.
        PG(3)=EGAM
C
        PE1(1)=Q1*SIN(TH1)
        PE1(2)=0.
        PE1(3)=Q1*COSTH1
C
        PE2(1)=-Q2*SIN(TH2)
        PE2(2)=0.
        PE2(3)=Q2*COSTH2
C
C-- Rotation: So that the Gamma Direction is Isotropic in the PI0 C. of M.
C
        COSTH=RNDM(0)*2.-1.
        TH=ACOS(COSTH)
        PH1=RNDM(0)*TWOPI
        PH2=RNDM(0)*TWOPI
C
        R(1)=COSTH*COS(PH1)*COS(PH2)-SIN(PH1)*SIN(PH2)
        R(2)=-COSTH*COS(PH1)*SIN(PH2)-SIN(PH1)*COS(PH2)
        R(3)=SIN(TH)*COS(PH1)
        R(4)=COSTH*SIN(PH1)*COS(PH2)+COS(PH1)*SIN(PH2)
        R(5)=-COSTH*SIN(PH1)*SIN(PH2)+COS(PH1)*COS(PH2)
        R(6)=SIN(TH)*SIN(PH1)
        R(7)=-SIN(TH)*COS(PH2)
        R(8)=SIN(TH)*SIN(PH2)
        R(9)=COSTH
C
        CALL RMMLT(3,3,1,R(1),R(2),R(4),PG(1),0.,PG(2),
     R             PGP(1),0.,PGP(2),DMY)
        CALL RMMLT(3,3,1,R(1),R(2),R(4),PE1(1),0.,PE1(2),
     R             PE1P(1),0.,PE1P(2),DMY)
        CALL RMMLT(3,3,1,R(1),R(2),R(4),PE2(1),0.,PE2(2),
     R             PE2P(1),0.,PE2P(2),DMY)
C
        DO 10 I=1,3
          PV(I,1)=PGP(I)
          PV(I,2)=PE1P(I)
          PV(I,3)=PE2P(I)
 10     CONTINUE
        PV(4,1)=EGAM
        PV(4,2)=EL1
        PV(4,3)=EL2
C
        RETURN
        END
#endif
