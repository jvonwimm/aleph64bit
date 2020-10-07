      FUNCTION QMASV0(I,CINP)
CKEY V0 KINEMATICS /USER
C----------------------------------------------------------------------
C! Calculate V0 invariant mass from r and b parameters (see ALEPH 88-46)
C  For gamma conversions, calculate invariant mass of daughters.
C
C   Author   : E. Blucher            19-APR-1989
C
C   Inputs:
C     I =  ALPHA V0 track number
C     CINP = V0 particle name:
C            GAMMA OR GA
C            K0s or K0
C            LAM0 or LA
C            LAM#0 or AL
C!======================================================================
#ifndef DOC
      SAVE INIT,AMASPI,AMASEL,AMASPR
#include "qcde.h"
      DATA INIT /0/
      CHARACTER *(*) CINP
      CHARACTER CNAM * 6, CTAB * 6, SPACES * 6
      DATA CNAM /' '/
#include "qmacro.h"
C----------------------------------------------------------------------
      IF (INIT .EQ. 0)  THEN
        INIT = 1
        N = KPCOMP ('PI+')
        AMASPI = RW(KOQPAR+N*KCQPAR+JQPAMA)
        N = KPCOMP ('E+')
        AMASEL = RW(KOQPAR+N*KCQPAR+JQPAMA)
        N = KPCOMP ('P')
        AMASPR = RW(KOQPAR+N*KCQPAR+JQPAMA)
      ENDIF
      CNAM = SPACES (CINP,0)
      CALL CLTOU (CNAM)
      IF(CNAM.EQ.'GAMMA '.OR.CNAM.EQ.'GA    ')THEN
        CALL QVSETM(KDAU(I,1),AMASEL)
        CALL QVSETM(KDAU(I,2),AMASEL)
        QMASV0=QM2(KDAU(I,1),KDAU(I,2))
        CALL QVSETM(KDAU(I,1),AMASPI)
        CALL QVSETM(KDAU(I,2),AMASPI)
        GOTO 999
      ELSEIF(CNAM.EQ.'K0S   '.OR.CNAM.EQ.'K0    ')THEN
        AM1=AMASPI
        AM2=AMASPI
      ELSEIF(CNAM.EQ.'LAM0  '.OR.CNAM.EQ.'LA    ')THEN
        IF(QCH(KDAU(I,1)).GT.0.)THEN
          AM1=AMASPR
          AM2=AMASPI
        ELSE
          AM1=AMASPI
          AM2=AMASPR
        ENDIF
      ELSEIF(CNAM.EQ.'LAM#0  '.OR.CNAM.EQ.'AL    ')THEN
        IF(QCH(KDAU(I,1)).GT.0.)THEN
          AM1=AMASPI
          AM2=AMASPR
        ELSE
          AM1=AMASPR
          AM2=AMASPI
        ENDIF
      ELSE
        CALL QWMESS('_QMASV0_ Incorrect particle name for V0')
        GOTO 999
      ENDIF
      PT=(QP(I)**2)/4.
      R=QYV0X1(I)
      B=QYV0X2(I)
      QMASV0=2.*SQRT((PT*(1.+R)**2 + B**2 + AM1**2)*
     & (PT*(1.-R)**2+ B**2 + AM2**2)) - 2.*(PT*(1-R**2) - B**2)
     & + AM1**2 + AM2**2
      QMASV0=QSQT(QMASV0)
  999 RETURN
      END
#endif
