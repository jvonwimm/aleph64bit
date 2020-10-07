      FUNCTION KVSAVC (ITK,CNAM,ICLASS)
CKEY PART CLASS /USER
C----------------------------------------------------------------------
C! Save particle with name CNAM in class ICLASS.
C                                                    H.Albrecht  2.1.89
C   Modified to set bit mask.                        E.Blucher  26.7.90
C----------------------------------------------------------------------
#ifndef DOC
      SAVE IERR
#include "qcdesh.h"
      CHARACTER * (*) CNAM
      DATA IERR /0/
      JR(J) = KOQVEC + KVSAVC * KCQVEC + J
      J1(J) = KOQVEC + ITK    * KCQVEC + J
C
      ICL = KSCNEW (ICLASS)
C
      IF (KTDROP .NE. 0)  THEN
        KVSAVC = KTDROP
        KTDROP = IW(JR(JQVENP))
      ELSE
        KVSAVC = KFFRT
        KFFRT = KFFRT + 1
        IF (KFFRT .GE. KLFRT)  CALL QSBANK ('QVEC', KFFRT+200)
      ENDIF
C
C       basic track attributes :
C
      RW(JR(JQVEQX)) = RW(J1(JQVEQX))
      RW(JR(JQVEQY)) = RW(J1(JQVEQY))
      RW(JR(JQVEQZ)) = RW(J1(JQVEQZ))
      RW(JR(JQVEQE)) = RW(J1(JQVEQE))
      RW(JR(JQVEQM)) = RW(J1(JQVEQM))
      RW(JR(JQVEQP)) = RW(J1(JQVEQP))
      RW(JR(JQVECH)) = RW(J1(JQVECH))
C
C       particle code, class :
C       daughter - mother relation of daughters :
C
      IW(JR(JQVECL)) = ICL
      IF (CNAM .NE. ' ')  THEN
        IPC = KPART (CNAM)
        IW(JR(JQVEPA)) = IPC
        IFP = IW(KOQPLI+IPC)
        IF (IFP .EQ. 0)  IFP = KFPADR (IPC)
        IW(JR(JQVENP)) =
     &      IW(KOQFPA+IFP*KCQFPA+ICL)
        IW(KOQFPA+IFP*KCQFPA+ICL) = KVSAVC
        IF (IW(J1(JQVEND)) .NE. 0)  CALL QAMOTH
     &    (KVSAVC, IW(J1(JQVEND)), IW(KOQLIN+IW(J1(JQVEDL))+1))
      ELSE
        IW(JR(JQVEPA)) = 0
        IW(JR(JQVENP)) = 0
      ENDIF
C
C       bit masks :
C
      DO 9 IB=1,KLOCKM
        IW(JR(JQVEBM+IB-1)) = IW(J1(JQVEBM+IB-1))
    9 CONTINUE
      IMASK=0
      DO 10 IBM=1,KLOCKM
        IF(IW(JR(JQVEBM+IBM-1)).NE.0)IMASK=1
   10 CONTINUE
      IF(IMASK.EQ.0)CALL QSBITM(KVSAVC)
      IW(JR(JQVELK)) = IW(J1(JQVELK))
C
C       class = 0 --> zero attributes
C
      IF (IW(J1(JQVECL)) .EQ. 0)  THEN
        IERR = IERR + 1
C       IF (IERR .LE. 10)  CALL QWMESE
C    &  ('_KVSAVC_ Particle to be saved has zero attributes')
      ELSE
C
C       MC and reco tracks mixed up ?
C
        IF (KCLAFR(ICL) .EQ. 0)  THEN
          KCLAFR(ICL) = IW(J1(JQVECL))
          KCLARM(ICL) = KCLARM(KCLAFR(ICL))
        ELSE
          IF (KCLARM(IW(J1(JQVECL))) .NE. KCLARM(ICL))  THEN
            DO 8 IB=1,KLOCKM
              IW(JR(JQVEBM+IB-1)) = 0
8           CONTINUE
          ENDIF
        ENDIF
      ENDIF
C
C       KSAME pointer :
C
C     IF(ITK.LE.KLUST.OR.IW(J1(JQVESP)).EQ.0)IW(J1(JQVESP))=ITK
C     IW(JR(JQVESP)) = IW(J1(JQVESP))
C     IW(J1(JQVESP)) = KVSAVC
      IW(JR(JQVESP)) = KVSAVC
C
C       mother - daughter relation, match :
C
      IW(JR(JQVEND)) = IW(J1(JQVEND))
      IW(JR(JQVEDL)) = IW(J1(JQVEDL))
      IW(JR(JQVENO)) = 0
      IW(JR(JQVENM)) = IW(J1(JQVENM))
      IW(JR(JQVEML)) = IW(J1(JQVEML))
C
C       vertices :
C
      IW(JR(JQVESC)) = IW(J1(JQVESC))
      IW(JR(JQVEOV)) = IW(J1(JQVEOV))
      IW(JR(JQVEEV)) = IW(J1(JQVEEV))
      RW(JR(JQVEDB)) = RW(J1(JQVEDB))
      RW(JR(JQVEZB)) = RW(J1(JQVEZB))
      RW(JR(JQVESD)) = RW(J1(JQVESD))
      RW(JR(JQVESZ)) = RW(J1(JQVESZ))
      RW(JR(JQVECB)) = RW(J1(JQVECB))
C
C       other pointers etc.
C
      IW(JR(JQVETN)) = IW(J1(JQVETN))
      IW(JR(JQVEQD)) = IW(J1(JQVEQD))
      IW(JR(JQVEKS)) = IW(J1(JQVEKS))
C
C        error matrix :
C
      IF (RW(J1(JQVEEM)) .GE. 0.)  THEN
        RW(JR(JQVEEM)) = RW(J1(JQVEEM))
        RW(JR(JQVEEM+1)) = RW(J1(JQVEEM+1))
        RW(JR(JQVEEM+2)) = RW(J1(JQVEEM+2))
        RW(JR(JQVEEM+3)) = RW(J1(JQVEEM+3))
        RW(JR(JQVEEM+4)) = RW(J1(JQVEEM+4))
        RW(JR(JQVEEM+5)) = RW(J1(JQVEEM+5))
        RW(JR(JQVEEM+6)) = RW(J1(JQVEEM+6))
        RW(JR(JQVEEM+7)) = RW(J1(JQVEEM+7))
        RW(JR(JQVEEM+8)) = RW(J1(JQVEEM+8))
        RW(JR(JQVEEM+9)) = RW(J1(JQVEEM+9))
      ELSE
        RW(JR(JQVEEM)) = -1.
      ENDIF
      RW(JR(JQVECF)) = RW(J1(JQVECF))
C
      END
#endif
