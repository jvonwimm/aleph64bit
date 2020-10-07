      SUBROUTINE TAGVAR2
C---------------------------------------------------------------
#include "qcde.h"
#include "comnnt.h"
       DIMENSION PL(900),PT(900),Y(900),QPP(50),PPL(50),PPT(50)
#include "qmacro.h"
C
      KK    = 0
      KK1   = 0
      KK2   = 0
      KK3   = 0
      SPT   = 0.
      SPP   = 0.
      RAPJS = 0.
      RAPJM = 0.
      EKK1  = 0.
      PXKK1 = 0.
      PYKK1 = 0.
      PZKK1 = 0.
      EKK2  = 0.
      PXKK2 = 0.
      PYKK2 = 0.
      PZKK2 = 0.
      EKK3  = 0.
      PXKK3 = 0.
      PYKK3 = 0.
      PZKK3 = 0.
      EKKM  = 0.
      PXKKM = 0.
      PYKKM = 0.
      PZKKM = 0.
C for the cone around the axis of the most energetic Jet
      KKC   = 0
      SPTC  = 0.
      SPPC  = 0.
      RAPCS = 0.
      RAPCM = 0.
      EKKC  = 0.
      PXKKC = 0.
      PYKKC = 0.
      PZKKC = 0.
C for the cone around the most energetic track of the hemisphere
      KKI   = 0
      SPTI  = 0.
      SPPI  = 0.
      RAPIS = 0.
      RAPIM = 0.
      EKKI  = 0.
      PXKKI = 0.
      PYKKI = 0.
      PZKKI = 0.
      DO 20 L = IG , ID
C Angle between L and the axis of the most energetic Jet
      UUUU = QCOSA(L,JJ1)
      IF (ABS(UUUU).GT.1.) PRINT *,'PB UUUU ',UUUU,JJ1
      IF (UUUU.GT.1.)  UUUU =  1.
      IF (UUUU.LT.-1.) UUUU = -1.
      TETA    = ACOS(UUUU)
      ANGJL = TETA * 180. / 3.1415
C Angle between L and the most energetic track
      UUUI = QCOSA(L,ITKMAX)
      IF (ABS(UUUI).GT.1.01) PRINT *,'PB UUUI ',UUUI,ITKMAX
      IF (UUUI.GT.1.)  UUUI =  1.
      IF (UUUI.LT.-1.) UUUI = -1.
      TETAI   = ACOS(UUUI)
      ANGIL = TETAI * 180. / 3.1415
c --- Rapidity ---
      PLONG = QPPAR (L,JJ1)
      IF (((QE(L)+PLONG).GT.0.).AND.((QE(L)-PLONG).GT.0.0001)) THEN
      RAP =  0.5 * LOG ( (QE(L)+PLONG) / (QE(L)-PLONG) )
      ELSE
      RAP = 99.
      ENDIF
      PL(L)   = QP(L) * COS(TETA)
      PT(L)   = QP(L) * SIN(TETA)
      IF (XSAME(L,JJ1)) THEN
      SPT     = SPT   + (PT(L))**2
      SPP     = SPP   + PL(L)
      RAPJS   = RAPJS + RAP
      KK   = KK   + 1
      EKKM = EKKM + QE(L)
      PXKKM= PXKKM+ QX(L)
      PYKKM= PYKKM+ QY(L)
      PZKKM= PZKKM+ QZ(L)
C
      IF (QP(L).GE.1.0) THEN
      KK1  = KK1  + 1
      EKK1 = EKK1 + QE(L)
      PXKK1= PXKK1+ QX(L)
      PYKK1= PYKK1+ QY(L)
      PZKK1= PZKK1+ QZ(L)
      ENDIF
      IF (QCH(L).NE.0.) THEN
      KK2  = KK2  + 1
      EKK2 = EKK2 + QE(L)
      PXKK2= PXKK2+ QX(L)
      PYKK2= PYKK2+ QY(L)
      PZKK2= PZKK2+ QZ(L)
      IF (QP(L).GE.1.0) THEN
      KK3  = KK3  + 1
      EKK3 = EKK3 + QE(L)
      PXKK3= PXKK3+ QX(L)
      PYKK3= PYKK3+ QY(L)
      PZKK3= PZKK3+ QZ(L)
      ENDIF
      ENDIF
      PPL(KK) = PL(L)
      PPT(KK) = PT(L)
      QPP(KK) = ABS(QP(L))
      ENDIF
c --- 40 degres cone around the Jet axis ---
      IF (ANGJL.LT.40.AND.(.NOT.XLOCK(L))) THEN
      SPTC    = SPTC  + (PT(L))**2
      SPPC    = SPPC  + PL(L)
      RAPCS   = RAPCS + RAP
      KKC  = KKC  + 1
      EKKC = EKKC + QE(L)
      PXKKC= PXKKC+ QX(L)
      PYKKC= PYKKC+ QY(L)
      PZKKC= PZKKC+ QZ(L)
      ENDIF
c --- 40 degres cone around ITKMAX ---
      IF (ANGIL.LT.40.AND.L.NE.ITKMAX.AND.(.NOT.XLOCK(L))) THEN
      SPTI    = SPTI  + (PT(L))**2
      SPPI    = SPPI  + PL(L)
      RAPIS   = RAPIS + RAP
      KKI  = KKI  + 1
      EKKI = EKKI + QE(L)
      PXKKI= PXKKI+ QX(L)
      PYKKI= PYKKI+ QY(L)
      PZKKI= PZKKI+ QZ(L)
      ENDIF
  20  CONTINUE
      IF (KK.GT.0) THEN
      RAPJM = RAPJS / FLOAT(KK)
      ELSE
      RAPJM = 99.
      ENDIF
C
      IF (KKC.GT.0) THEN
      RAPCM = RAPCS / FLOAT(KKC)
      ELSE
      RAPCM = 99.
      ENDIF
C
      IF (KKI.GT.0) THEN
      RAPIM = RAPIS / FLOAT(KKI)
      ELSE
      RAPIM = 99.
      ENDIF
C
      KMAX = KK
      DO 100 N = KMAX , 2 , -1
      DO 100 J = 2 , N
      IF (QPP(J-1).GT.QPP(J)) THEN
      P1       = PPL(J)
      P2       = PPT(J)
      P3       = QPP(J)
      QPP(J)   = QPP(J-1)
      QPP(J-1) = P3
      PPL(J)   = PPL(J-1)
      PPL(J-1) = P1
      PPT(J)   = PPT(J-1)
      PPT(J-1) = P2
      ENDIF
 100  CONTINUE
C
      DO 200 I = 1 , 4
      PCL(I) = PPL(KMAX-I+1)
      PCT(I) = PPT(KMAX-I+1)
 200  CONTINUE
C
      EJET   = QE(JJ1)
      FRACJ  = EJET / ENERC
      FRACC  = EKKC / ENERC
      FRACI  = EKKI / ENERC
C
      XMKKM  = EKKM**2 - PXKKM**2 - PYKKM**2 - PZKKM**2
      IF (XMKKM.LE.0.) THEN
      XMKKM  = 0.
      ELSE
      XMKKM  = SQRT (XMKKM)
      ENDIF
      IF (SCHEME.EQ.'E') THEN
      YMJET  = QM(JJ1)
      ELSE
      YMJET  = XMKKM
      ENDIF
C
      XMKK1  = EKK1**2 - PXKK1**2 - PYKK1**2 - PZKK1**2
      IF (XMKK1.LE.0.) THEN
      XMKK1  = 0.
      ELSE
      XMKK1  = SQRT (XMKK1)
      ENDIF
C
      XMKK2  = EKK2**2 - PXKK2**2 - PYKK2**2 - PZKK2**2
      IF (XMKK2.LE.0.) THEN
      XMKK2  = 0.
      ELSE
      XMKK2  = SQRT (XMKK2)
      ENDIF
C
      XMKK3  = EKK3**2 - PXKK3**2 - PYKK3**2 - PZKK3**2
      IF (XMKK3.LE.0.) THEN
      XMKK3  = 0.
      ELSE
      XMKK3  = SQRT (XMKK3)
      ENDIF
C
      XMKKC  = EKKC**2 - PXKKC**2 - PYKKC**2 - PZKKC**2
      IF (XMKKC.LE.0.) THEN
      XMKKC  = 0.
      ELSE
      XMKKC  = SQRT (XMKKC)
      ENDIF
CC    print *,'xmkkc=',kevt,angjl,xmkkc
C
      XMKKI  = EKKI**2 - PXKKI**2 - PYKKI**2 - PZKKI**2
      IF (XMKKI.LE.0.) THEN
      XMKKI  = 0.
      ELSE
      XMKKI  = SQRT (XMKKI)
      ENDIF
C
 99   CONTINUE
      RETURN
      END
C
C
