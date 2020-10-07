      SUBROUTINE TFTHEL(NAME,NPT,LIST,IOPT,VV0,EE0,CHI2)
C-----**************************************************---------C
C! FAST FITTING ROUTINE FOR HELICES                              C
C!    AUTHORS:  N. CHERNOV, G. OSOSKOV & M. POPPE                C
C!    INPUT: NAME  = NAME OF THE BANK CONTAINING COORDINATES     C
C!            NPT  = NUMBER OF POINTS TO BE FITTED               C
C!            LIST = LIST OF POINTS TO BE FITTED                 C
C!            IOPT = 0 -> DISTANCE**2=X**2+Y**2 MINIMISED        C
C!                   1 -> WEIGHTED WITH 1/SIMA(R*PHI)**2         C
C!                   2 -> ERROR MATRIX CALCULATED                C
C!                   3 -> 3-DIMENSIONAL ITERATION                C
C!  OUTPUT:   VV0 = 1/R*CHARGE   [1/CM]  POS. IF CLOCKWISE       C
C!                  TAN(LAMBDA)  {=DZ/DS}TAN(ANGLE TO X,Y PLANE) C
C!                  PHI0         {0,2PI} ANGLE TO X-AXIS         C
C!                  D0*SIGN      [CM]    MINIMAL DIST. TO Z-AXIS,C
C!                                       POS. IF AXIS ENCIRCLED  C
C!                  Z0           [CM]    Z POS AT R=D0           C
C!            EE0 = INVERSE ERROR MATRIX IN TRIANG. FORM         C
C!            CHI2= CHI SQUARED = SUM (DEVIATIONS/ERRORS)**2     C
C!  NOTE: DEGREES OF FREEDOM = 2*NPT-5                           C
C----------------------------------------------------------------C
C     BASED ON  SUBROUTINE CIRCLE                                C
C     REFERENCE:  COMPUTER PHYSICS COMMUNICATIONS VOL 33,P329    C
C----------------------------------------------------------------C
C
C   Modified on 8-Jun-89 to allow interface on a lower level to old
C   algorithm by Fred Weber; now calls old algorithm in form of new
C   routine TFITHL.
C-----------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
C
      PARAMETER (MPT=40)
      REAL   PF(MPT),RF(MPT),VV0(*),EE0(*),
     1       ZF(MPT),WZF(MPT)
      DOUBLE PRECISION XF(MPT),YF(MPT),WF(MPT)
      INTEGER LIST(*)
      CHARACTER*4 NAME
      CHARACTER*48 MESSAG
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C     OFFSET FOR COORDINATE "IJ" IN 'TPCO'
      KKTPCO(IJ) = KTPCO+2+(IJ-1)*(IW(KTPCO+1))
C     OFFSET FOR COORDINATE "IJ" IN 'TPCO'
      IF(FIRST) THEN
          NTPCO = NAMIND('TPCO')
          NITCO = NAMIND('ITCO')
          FIRST =.FALSE.
      END IF
      IF(NPT.GT.MPT) THEN
         CALL RERROR('TFTHEL',1,' TFTHEL: Cannot fit > 40 points')
         RETURN
      END IF
C-----> INPUT DATA
      IF(NAME.EQ.'TPCO') THEN
          KTPCO=IW(NTPCO)
          IF (KTPCO.EQ.0) THEN
              CALL RERROR('TFTHEL',2,
     1        ' TFTHEL:  no TPCO bank; returning...')
              RETURN
          END IF
          DO 10 I=1,NPT
          KSTRT = KKTPCO(LIST(I))
          XF(I)  = RW(KSTRT+2)*COS(RW(KSTRT+3))
          YF(I)  = RW(KSTRT+2)*SIN(RW(KSTRT+3))
          RF(I)  = RW(KSTRT+2)
          PF(I)  = RW(KSTRT+3)
          WF(I)  = 1.0/(RW(KSTRT+5)+0.000000001)
          ZF(I)  = RW(KSTRT+4)
          WZF(I) = 1.0/(RW(KSTRT+6)+0.000001)
   10     CONTINUE
      ELSE IF(NAME.EQ.'ITCO') THEN
          CALL RERROR('TFTHEL',3,' TFTHEL: no ITC available yet')
          RETURN
      ELSE
          WRITE (MESSAG,15) NAME
   15     FORMAT(1X,' TFTHEL: I do not understand bank ',A4,
     1        ' returning')
          CALL RERROR('TFTHEL',4, MESSAG)
          RETURN
      END IF
C
      CALL TFITHL(NPT,XF,YF,RF,PF,WF,ZF,WZF,IOPT,
     1            VV0,EE0,CH2PH,CH2Z)
C
C     combine CHI2 together to keep old routine the same
C
      IF (CH2Z.LT.1.0E30) THEN
          CHI2 = CH2Z + CH2PH
      ELSE
          CHI2 = 1.0E30
      END IF
C
      RETURN
      END
#endif
