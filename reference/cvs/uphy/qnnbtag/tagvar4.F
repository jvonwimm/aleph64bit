      SUBROUTINE TAGVAR4
C-------------------------------------------------------
#include "qcde.h"
#include "comnnt.h"
       DIMENSION QPP(50),QXX(50),QYY(50),QZZ(50),QEE(50),LLL(4),
     > C(50,50)
#include "qmacro.h"
C
       IFLAG4 = 0
       KK     = 0
       PP     = 0.
       PL     = 0.
       PT     = 0.
       PTL    = 0.
       DO 20 L = IG , ID
C---- Selection of the most energetic jet ----
       IF (XSAME(L,JJ1)) THEN
       KK = KK + 1
       QPP(KK) = ABS(QP(L))
       QXX(KK) = QX(L)
       QYY(KK) = QY(L)
       QZZ(KK) = QZ(L)
       QEE(KK) = QE(L)
c on definit XJTLJET avec seulement les traces d'impulsion > 1 GeV/c
c      if (qp(l).ge.1.) then
       PP = PP + ABS(QP(L))
       PT = PT + ABS(QPPER(L,JJ1))
       PL = PL + ABS(QPPAR(L,JJ1))
       PTL = PTL + PT * PL
c      endif
       ENDIF
 20    CONTINUE
C---- KK : multiplicity of the most energetic jet ---
       KMAX = KK
C---- KK must be greater than 4 ----
       IF (KMAX.LT.4) THEN
       IFLAG4 = 1
       GOTO 99
       ENDIF
C
       IF (PP.NE.0.) THEN
       XJTLJET = PTL / (PP * PP)
       ELSE
       XJTLJET = 0.
       ENDIF
C
C---- Classification of the tracks ----
C
        DO 30 N = KMAX , 2 , -1
        DO 30 J = KMAX , KMAX-N+2 , -1
        IF (QPP(J-1).LT.QPP(J)) THEN
        P1       = QPP(J)
        QPP(J)   = QPP(J-1)
        QPP(J-1) = P1
        P1       = QXX(J)
        QXX(J)   = QXX(J-1)
        QXX(J-1) = P1
        P1       = QYY(J)
        QYY(J)   = QYY(J-1)
        QYY(J-1) = P1
        P1       = QZZ(J)
        QZZ(J)   = QZZ(J-1)
        QZZ(J-1) = P1
        P1       = QEE(J)
        QEE(J)   = QEE(J-1)
        QEE(J-1) = P1
        ENDIF
 30     CONTINUE
C
C---- Coefficients of the boost ----
C
         DO 40 LVAR=1,11
         CALL SELJET(KMAX,LVAR,NMAX,LLL)
            PXQ=0.
            PYQ=0.
            PZQ=0.
            EQ=0.
            XXNUM=0.
         DO 50 L=1,NMAX
            IL=LLL(L)
            PXQ=PXQ+QXX(IL)
            PYQ=PYQ+QYY(IL)
            PZQ=PZQ+QZZ(IL)
            EQ=EQ+QEE(IL)
 50      CONTINUE
C
            VX=PXQ/EQ
            VY=PYQ/EQ
            VZ=PZQ/EQ
            EMINV(LVAR) = EQ
          IF(ABS(EQ*EQ-(PXQ*PXQ+PYQ*PYQ+PZQ*PZQ)).GE.0.01) THEN
            XMINV(LVAR)=SQRT(ABS(EQ*EQ-(PXQ*PXQ+PYQ*PYQ+PZQ*PZQ)))
          ELSE
            XMINV(LVAR)= 0.01
          ENDIF
            GAM=EQ/XMINV(LVAR)
            XNUM=0.
            XDENO=0.
C
C
             DO 60 L=1,NMAX
               IL=LLL(L)
      CALL PR_SCAL(2,VX,VY,VZ,QXX(IL),QYY(IL),QZZ(IL),PRO)
                PPX=QXX(IL)+GAM*VX*(GAM*PRO/(GAM+1.)-QEE(IL))
                PPY=QYY(IL)+GAM*VY*(GAM*PRO/(GAM+1.)-QEE(IL))
                PPZ=QZZ(IL)+GAM*VZ*(GAM*PRO/(GAM+1.)-QEE(IL))
                PP=SQRT(PPX*PPX+PPY*PPY+PPZ*PPZ)
      CALL PR_SCAL(1,PPX,PPY,PPZ,QX(JJ1),QY(JJ1),QZ(JJ1),UUU)
                IF (UUU.GT.1.)  UUU = 1.
                IF (UUU.LT.-1.) UUU = -1.
                TETA=ACOS(UUU)
                PT=PP*SIN(TETA)
                XNUM=XNUM+PT*PT
                XDENO=XDENO+PP*PP
 60     CONTINUE
C
         SDIR(LVAR)=XNUM/XDENO
 40     CONTINUE
C
C---- Fox-Wolfram variables ----
C
        H0  = 0.
        H1  = 0.
        H2  = 0.
        H3  = 0.
        H4  = 0.
        H5  = 0.
        EQ  = 0.
        PXQ = 0.
        PYQ = 0.
        PZQ = 0.
          DO 65 L=1,KMAX
           PXQ=PXQ+QXX(L)
           PYQ=PYQ+QYY(L)
           PZQ=PZQ+QZZ(L)
           EQ=EQ+QEE(L)
 65       CONTINUE
C
          VX=PXQ/EQ
          VY=PYQ/EQ
          VZ=PZQ/EQ
          IF(ABS(EQ*EQ-(PXQ*PXQ+PYQ*PYQ+PZQ*PZQ)).GE.0.01) THEN
            GAM=EQ/SQRT(ABS(EQ**2-(PXQ**2+PYQ**2+PZQ**2)))
          ELSE
            GAM=EQ
          ENDIF
C
          DO 66 L=1,KMAX
       CALL PR_SCAL(2,VX,VY,VZ,QXX(L),QYY(L),QXX(L),PRO)
        QXX(L)=QXX(L)+GAM*VX*(GAM*PRO*QXX(L)/(GAM+1.)-QEE(L))
        QYY(L)=QYY(L)+GAM*VY*(GAM*PRO*QYY(L)/(GAM+1.)-QEE(L))
        QZZ(L)=QZZ(L)+GAM*VZ*(GAM*PRO*QZZ(L)/(GAM+1.)-QEE(L))
 66       CONTINUE
C
        DO 70 I=1,KMAX
        DO 70 J=1,KMAX
           C(I,J)=QXX(I)*QXX(J)+QYY(I)*QYY(J)+QZZ(I)*QZZ(J)
           A=SQRT(QXX(I)**2+QYY(I)**2+QZZ(I)**2)
           A=A*SQRT(QXX(J)**2+QYY(J)**2+QZZ(J)**2)
           C(I,J)=C(I,J)/A
         X=C(I,J)
         CALL PLEGEN(1,X,P1)
         CALL PLEGEN(2,X,P2)
         CALL PLEGEN(3,X,P3)
         CALL PLEGEN(4,X,P4)
         CALL PLEGEN(5,X,P5)
         U=SQRT(QXX(I)**2+QYY(I)**2+QZZ(I)**2)
         U=U*SQRT(QXX(J)**2+QYY(J)**2+QZZ(J)**2)
         H0 = H0 + U * 1.0D0
         H1 = H1 + U * P1
         H2 = H2 + U * P2
         H3 = H3 + U * P3
         H4 = H4 + U * P4
         H5 = H5 + U * P5
 70    CONTINUE
        IF (H0.NE.0.) THEN
        HJ1 = H1 / H0
        HJ2 = H2 / H0
        HJ3 = H3 / H0
        HJ4 = H4 / H0
        HJ5 = H5 / H0
        ELSE
        HJ1 = H1
        HJ2 = H2
        HJ3 = H3
        HJ4 = H4
        HJ5 = H5
        ENDIF
C
 99     CONTINUE
        RETURN
        END
C
