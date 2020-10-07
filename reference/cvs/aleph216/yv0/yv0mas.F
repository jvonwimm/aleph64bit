          SUBROUTINE YV0MAS(V0,PV,PVMAT,P1,P2,EP,XM,CXM)
C--------------------------------------------------------------------
C! Finds the invariant mass and the chisquare for any type of V0
CKEY YV0 MASS CHI2 / USER
C
C      AUTHOR: M.A. Ciocci 29/6/89
C      MODIFIED:
C
C      INPUT:  V0/CHARACTER*2          V0='K0'    K0
C                                      V0='LA'    Lambda
C                                      V0='AL'    Anti-Lambda
C                                      V0='GA'    Gamma
C
C              PV/REAL ARRAY    Mass constraints
C                               (see aleph note...)
C
C              PVMAT/REAL ARRAY  Covariance matrix of mass constraints
C
C              P1/REAL ARRAY    Momentum of positive charged particle
C                               (see aleph note...)
C
C              P2/REAL ARRAY    Momentum of negative charged particle
C                               (see aleph note...)
C
C              EP/REAL ARRAY    Covariance matrix of momenta
C
C      OUTPUT: XM/REAL          Invariant mass (see aleph note...)
C
C              CXM/REAL         Chisquare of mass hypothesis
C
C                    Description
C                    ===========
C        Calculates the invariant mass and the chisquare for
C        a fixed hypothesis on V0 mass (see also Aleph-note...)
C
C
C         Libraries:CERN
C
C------------------------------------------------------------------
#ifndef DOC
      SAVE
          REAL P1(3),P2(3),EP(6,6),CXM,XM,PV(2),PVMAT(2,2)
          DOUBLE PRECISION PD1(3),EPD(6,6),PD2(3)
          DOUBLE PRECISION XMPI,XMPR,XMEL,XMK0,XMLA
          DOUBLE PRECISION XM1,XM2,XMV0,ENE1,ENE2,P1P2,XMD
          DOUBLE PRECISION ZMP(6),DSIG2,CXMD
          CHARACTER*2 V0
          PARAMETER (XMPI=0.13956755, XMPR=0.93827231,
     $               XMEL=0.00051099906, XMK0=0.497671,
     $               XMLA=1.11563)
C
              DO 1 J=1,3
                PD1(J)=P1(J)
                PD2(J)=P2(J)
  1           CONTINUE
              DO 3 L=1,6
               DO 2 K=1,6
                  EPD(L,K)=EP(L,K)
  2            CONTINUE
  3           CONTINUE
          IF(V0.EQ.'K0')THEN
              XM1=XMPI
              XM2=XMPI
              XMV0=XMK0
          ENDIF
          IF(V0.EQ.'LA')THEN
              XM1=XMPR
              XM2=XMPI
              XMV0=XMLA
          ENDIF
          IF(V0.EQ.'AL')THEN
              XM1=XMPI
              XM2=XMPR
              XMV0=XMLA
          ENDIF
          IF(V0.EQ.'AL')THEN
              XM1=XMPI
              XM2=XMPR
              XMV0=XMLA
          ENDIF
          IF(V0.EQ.'GA')THEN
              XM1=XMEL
              XM2=XMEL
          ENDIF
          ENE1=DSQRT(XM1**2+PD1(1)*PD1(1)+PD1(2)*PD1(2)+PD1(3)*PD1(3))
          ENE2=DSQRT(XM2**2+PD2(1)*PD2(1)+PD2(2)*PD2(2)+PD2(3)*PD2(3))
          P1P2=PD1(1)*PD2(1)+PD1(2)*PD2(2)+PD1(3)*PD2(3)
          XMD=XM1**2+XM2**2+2.D0*ENE1*ENE2-2.D0*P1P2
          IF(XMD.LT.0.D0)THEN
              XM=0.
              CXM=0.
              GO TO 9
          ENDIF
          XMD=DSQRT(XMD)
          XM=SNGL(XMD)
C
C+  calculates the derivatives of mass respect to momenta
C
              IF(V0.EQ.'GA')GO TO 9
          ZMP(1)=(2.D0*PD1(1)/ENE1)*ENE2-2.D0*PD2(1)
          ZMP(2)=(2.D0*PD1(2)/ENE1)*ENE2-2.D0*PD2(2)
          ZMP(3)=(2.D0*PD1(3)/ENE1)*ENE2-2.D0*PD2(3)
          ZMP(4)=(2.D0*PD2(1)/ENE2)*ENE1-2.D0*PD1(1)
          ZMP(5)=(2.D0*PD2(2)/ENE2)*ENE1-2.D0*PD1(2)
          ZMP(6)=(2.D0*PD2(3)/ENE2)*ENE1-2.D0*PD1(3)
          DSIG2=0.D0
C
C+  calculates mass error squared
C
              DO 7 I=1,6
              DO 8 J=1,6
                DSIG2=DSIG2+ZMP(I)*EPD(I,J)*ZMP(J)
  8           CONTINUE
  7           CONTINUE
           DSIG2=DSIG2/(4.D0*XMD**2)
              CXMD=((XMV0-XMD)**2)/DSIG2
              CXM=SNGL(CXMD)
  9       CONTINUE
C
C+ in the gamma hypothesis calculates the chi square that transverse
C+ momenta squared is 0
C
              IF(V0.EQ.'GA')THEN
                 CXM=(PV(2)**2)/(4.*PVMAT(2,2))
              ENDIF
          RETURN
          END
#endif
