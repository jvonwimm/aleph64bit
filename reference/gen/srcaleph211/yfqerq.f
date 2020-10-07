      SUBROUTINE YFQERQ(V0,PV,PMAT,PV0,QVAL,ERQ2)
C--------------------------------------------------------------------
C! Finds the Q value and the relative error square for any typ of V0
CKEY YV0 ERROR /USER
C
C     AUTHOR: M.A. Ciocci, L. Rolandi  19/4/88
C     MODIFIED:M.A.CIOCCI 20/2/90
C
C     INPUT:  V0/CHARACTER*2          V0='K0'    K0
C                                     V0='LA'    Lambda
C                                     V0='AL'    Anti-Lambda
C                                     V0='GA'    Gamma
C
C             PV/REAL ARRAY    Mass constraints
C                              (see aleph note...)
C
C             PV0/REAL ARRAY   V0 momentum
C
C             PMAT/REAL ARRAY  Covariance of mass constraints
C
C     OUTPUT: QVAL/REAL        Q value (see aleph note...)
C
C             ERQ2/REAL        Square of error on Q value
C
C                   Description
C                   ===========
C        Find the Q value and the square of error on Q for
C        a fixed V0 hypothesis (see also Aleph-note...)
C
C        Called: YMFV0V
C
C        Calls:  None
C
C        Libraries:CERN
C
C------------------------------------------------------------------
      SAVE
      REAL XK0MA2,PCK0PP,RRK0PP
      REAL XLAMA2,PCLAPM,RRLAPM
      REAL XALMA2,PCALPP,RRALPP
      PARAMETER (XK0MA2=0.24773,PCK0PP=0.04244,
     +           RRK0PP=0.00000)
      PARAMETER (XLAMA2=1.24456,PCLAPM=0.01000,
     +           RRLAPM=0.69172)
      PARAMETER (XALMA2=1.24456,PCALPP=0.01000,
     +           RRALPP=-0.69172)
C
C     XK0MA2 : Square of K0 mass
C     PCK0PP : Square of the momentum in s.c.m
C              for the two pions caming from K0
C     RRK0PP:  Center for the circle described
C              from the constraints of mass when
C              K0 goes in two pions
C     XLAMA2 : Square of LA mass
C     PCLAPM : Square of the momentum in s.c.m for
C              the proton/pion caming from Lambda
C     RRLAPM:  Center of  the circle described
C              from the constraints of mass when
C              Lambda  goes in proton negative-pion
C     XALMA2 : Square of LA mass
C     PCALPP : Square of the momentum in s.c.m for
C              the proton/pion caming from Lambda
C     RRALPP:  Center for the circle described
C              from the constraints of mass when
C              Anti-Lambda  goes in anti-proton pion
C
      CHARACTER*2 V0
      DIMENSION PV(2),PV0(3),PMAT(2,2)
C
C+     THE V0 IS A K0 A LAMBDA OR
C+    A ANTI LAMBDA
C
      IF (V0.NE.'GA') THEN
C
C
C+    K0 HYPOTHESIS
C
      IF (V0.EQ.'K0') THEN
      XM2=XK0MA2
      PCM2=PCK0PP
      RR=RRK0PP
      ENDIF
C
C+    LAMBDA HYPOTHESIS
C
      IF (V0.EQ.'LA') THEN
      XM2=XLAMA2
      PCM2=PCLAPM
      RR=RRLAPM
      ENDIF
C
C+    ANTILAMBDA HYPOTHESIS
C
      IF (V0.EQ.'AL') THEN
      XM2=XALMA2
      PCM2=PCALPP
      RR=RRALPP
      ENDIF
      S2=VDOT(PV0,PV0,3)
      QVAL=(PV(1)-RR)**2-(1./XM2+1./S2)*4.*(PCM2-PV(2)**2)
      DQ1=2.*(PV(1)-RR)
      DQ2=8.*PV(2)*(1./XM2+1./S2)
      ERQ2=DBLE(DQ1*DQ1*PMAT(1,1))+2.D0*DBLE(DQ1*DQ2*PMAT(1,2))+
     $     DBLE(DQ2*DQ2*PMAT(2,2))
      ENDIF
C
C+     GAMMA HYPOTHESIS
C
      IF (V0.EQ.'GA') THEN
      QVAL=PV(2)**2
      DQ=2.*PV(2)
      ERQ2=DQ*DQ*PMAT(2,2)
      ENDIF
      RETURN
      END
