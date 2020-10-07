      FUNCTION KTLOR (ITK,ITKRST)
CKEY KINEMATICS /USER
C----------------------------------------------------------------------
C! Boost ITK and its daughters into the rest frame of ITKRST.
C                                                    H.Albrecht  2.1.89
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
      DIMENSION MD(30),MT(30),ML(30)
      NDAU(I) = IW(KOQVEC+I*KCQVEC+JQVEND)
      NPAR(I) = IW(KOQVEC+I*KCQVEC+JQVEPA)
      IDAU(I,N) = IW(KOQLIN+IW(KOQVEC+I*KCQVEC+JQVEDL)+N)
C
      ICL = KSCNEW (ITKRST)
C
      KTLOR = KTLORX (ITK,ICL,IFL,ITKRST)
      IF (IFL .NE. 0 .OR. KTLOR .EQ. 0 )  GO TO 90
      LTK = KTLOR
      NG = 1
      NTK = ITK
      GO TO 40
C
   30 LTK = KTLORX (NTK,ICL,IFL,ITKRST)
      CALL QADAU (ML(NG),1,LTK)
      IF (NPAR(ML(NG)) .NE. 0)  CALL QAMOTH (ML(NG),1,LTK)
C
      NG = NG + 1
      IF (IFL .NE. 0)  GO TO 60
      IF (NG .GT. 30)  THEN
        CALL QWMESE ('_QVLOR_ more than 30 generations')
        GO TO 60
      ENDIF
C
   40 ND = 1
   50 IF (ND .GT. NDAU(NTK))  GO TO 60
      MT(NG) = NTK
      MD(NG) = ND
      ML(NG) = LTK
      NTK = IDAU(NTK,ND)
      GO TO 30
C
   60 IF (NG .EQ. 1)  GO TO 90
      NG = NG - 1
      NTK = MT(NG)
      LTK = ML(NG)
      ND = MD(NG) + 1
      GO TO 50
C
   90 END
#endif
