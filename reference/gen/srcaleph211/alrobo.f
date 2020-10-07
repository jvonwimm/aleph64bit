      SUBROUTINE ALROBO(THE,PHI,BEX,BEY,BEZ,ITYP)
C==================================================================
C!    ALEPHLIB copy of the LUND routine LUROBO .
CKEY PHYSICS LUND THRUST E-FLOW / USER
C     Rotate vector P(i,5) from comdeck LUNDCOM
C           THE : Teta angle of rotation
C           PHI : Phi angle of rotation
C           BEX,BEY,BEZ : Centre of rotation coordinate
C           ITYP        : type of fit (1=Calorimeters, 2=Masks)
C     called by subroutine Althru
C     Adapted by M.N.Minard & M.Pepe
C
C==================================================================
      SAVE
      PARAMETER (L1MST=40, L1PAR=80)
      PARAMETER (L2KTYP=120, L2PMAS=120, L2PWID=60, L2KFR=80, L2CFR=40)
      PARAMETER (L3DPAR=20, L3IDB=120, L3CBR=400, L3KDP=1600)
      PARAMETER (LEMSTE=40, LEPARE=80)
      PARAMETER (LJNPAR=2000)
      COMMON /LUDAT1/   MSTLU1(L1MST),PARLU1(L1PAR)
      COMMON /LUDAT2/   KTYPL2(L2KTYP),PMASL2(L2PMAS),PWIDL2(L2PWID)
     &                , KFRLU2(L2KFR),CFRLU2(L2CFR)
      COMMON /LUDAT3/   DPARL3(L3DPAR),IDBLU3(L3IDB),CBRLU3(L3CBR)
     &                , KDPLU3(L3KDP)
      COMMON /LUDATE/   MSTELE(LEMSTE),PARELE(LEPARE)
      COMMON /LUJETS/   NPARLU,KODELU(LJNPAR,2),PARTLU(LJNPAR,5)
C
      INTEGER MST(L1MST),MSTE(LEMSTE),K(LJNPAR,2),N
      EQUIVALENCE (MST(1),MSTLU1(1)),(MSTE(1),MSTELE(1)),
     &            (K(1,1),KODELU(1,1)),(N,NPARLU)
      REAL PAR(L1PAR),PARE(LEPARE),P(LJNPAR,5)
      EQUIVALENCE (PAR(1),PARLU1(1)),(PARE(1),PARELE(1)),
     &            (P(1,1),PARTLU(1,1))
      DIMENSION ROT(3,3),PV(3)
      DOUBLE PRECISION DP(4),DBEX,DBEY,DBEZ,DGA,DBEP,DGABEP
      IF(THE**2+PHI**2.LT.1E-20) GOTO 130
C...ROTATE (TYPICALLY FROM Z AXIS TO DIRECTION THETA,PHI)
      ROT(1,1)=COS(THE)*COS(PHI)
      ROT(1,2)=-SIN(PHI)
      ROT(1,3)=SIN(THE)*COS(PHI)
      ROT(2,1)=COS(THE)*SIN(PHI)
      ROT(2,2)=COS(PHI)
      ROT(2,3)=SIN(THE)*SIN(PHI)
      ROT(3,1)=-SIN(THE)
      ROT(3,2)=0.
      ROT(3,3)=COS(THE)
      DO 120 I=  1 , N
         IF(K(I,1).NE.ITYP) GOTO 120
         DO 100 J=1,3
  100    PV(J)=P(I,J)
         DO 110 J=1,3
  110    P(I,J)=ROT(J,1)*PV(1)+ROT(J,2)*PV(2)+ROT(J,3)*PV(3)
  120 CONTINUE
  130 IF(BEX**2+BEY**2+BEZ**2.LT.1E-20) RETURN
C...LORENTZ BOOST (TYPICALLY FROM REST TO MOMENTUM/ENERGY=BETA)
      DBEX=DBLE(BEX)
      DBEY=DBLE(BEY)
      DBEZ=DBLE(BEZ)
      DGA=1D0/DSQRT(1D0-DBEX**2-DBEY**2-DBEZ**2)
      DO 150 I= 1 , N
         IF(K(I,1).NE.ITYP) GOTO 150
         DO 140 J=1,4
  140    DP(J)=DBLE(P(I,J))
         DBEP=DBEX*DP(1)+DBEY*DP(2)+DBEZ*DP(3)
         DGABEP=DGA*(DGA*DBEP/(1D0+DGA)+DP(4))
         P(I,1)=SNGL(DP(1)+DGABEP*DBEX)
         P(I,2)=SNGL(DP(2)+DGABEP*DBEY)
         P(I,3)=SNGL(DP(3)+DGABEP*DBEZ)
         P(I,4)=SNGL(DGA*(DP(4)+DBEP))
  150 CONTINUE
      RETURN
      END
