      SUBROUTINE QJSAVE (CNAM,NTB,ICLASS,IDR,IRE,IAL)
CKEY JETS SHAPE /INTERNAL
C----------------------------------------------------------------------
C! Save vector from topological buffer QCTBUF with name CNAM.
C  NTB : output buffer number in QCTBUF.
C  ICL : sequential class number.
C  IDR = 0 : drop existing particles.
C  IRE = 0 : no moth/dau relation.
C  IAL = 0 : all particles contribute to CNAM.
C
C                                                    H.Albrecht 22.2.89
C                                   Modified:        E.Blucher  27.9.91
C----------------------------------------------------------------------
#ifndef DOC
      CHARACTER *(*) CNAM
#include "qcde.h"
#include "qctbuf.h"
      DIMENSION IBUF(KTBIMX)
C-----------------------------------------------------------------------
      JR(J) = KOQVEC + ITK      * KCQVEC + J
      J1(J) = KOQVEC + KTBIT(I) * KCQVEC + J
C-----------------------------------------------------------------------
      ICL = KSCOLD (ICLASS)
C
C       new track number :
C
      IF (KTDROP .NE. 0)  THEN
        ITK = KTDROP
        KTDROP = IW(JR(JQVENP))
      ELSE
        ITK = KFFRT
        KFFRT = KFFRT + 1
        IF (KFFRT .GT. KLFRT)  CALL QSBANK ('QVEC', KFFRT+200)
      ENDIF
C
C       basic track attributes :
C
      RW(JR(JQVEQX)) = QTBOX(NTB)
      RW(JR(JQVEQY)) = QTBOY(NTB)
      RW(JR(JQVEQZ)) = QTBOZ(NTB)
      RW(JR(JQVEQE)) = QTBOE(NTB)
      RW(JR(JQVEQP)) = SQRT (QTBOX(NTB)**2 + QTBOY(NTB)**2 +
     &   QTBOZ(NTB)**2)
      RW(JR(JQVEQM)) = SQRT (AMAX1((RW(JR(JQVEQE)) + RW(JR(JQVEQP))) *
     &  (RW(JR(JQVEQE)) - RW(JR(JQVEQP))), 0.))
      RW(JR(JQVECH)) = 0.
C
C       drop old pseudo-particle
C
      IPC = KPART (CNAM)
      IFP = IW(KOQPLI+IPC)
      IF (IFP .EQ. 0)  IFP = KFPADR (IPC)
      IF (IDR .EQ. 0)  THEN
        IF (IW(KOQFPA+IFP*KCQFPA+ICL) .NE. 0)  THEN
          IF (IAND (IW(KOQPBT+IPC), KBIT(4)) .EQ. 0)  THEN
            CALL QWMESE
     &  ('_QJSAVE_ Pseudo particle name is used for a real particle')
          ELSE
            CALL QVDROA (CNAM,ICL)
          ENDIF
        ENDIF
        IW(KOQPBT+IPC) = IOR (IW(KOQPBT+IPC), KBIT(4))
      ENDIF
C
C       particle code, class :
C
      IW(JR(JQVECL)) = ICL
      IW(JR(JQVEPA)) = IPC
      IW(JR(JQVENP)) = IW(KOQFPA+IFP*KCQFPA+ICL)
      IW(KOQFPA+IFP*KCQFPA+ICL) = ITK
C
C       KSAME pointer :
C
      IW(JR(JQVESP)) = ITK
C
C       bit masks, daughters :
C
      DO 8 IB=1,KLOCKM
        IW(JR(JQVEBM+IB-1)) = 0
  8   CONTINUE
      IW(JR(JQVELK)) = 0
      RW(JR(JQVECH))=0.
      IW(JR(JQVENM)) = 0
      IW(JR(JQVENO)) = 0
      IW(JR(JQVEND)) = 0
C
      ICL = KSCOLD (ICLASS)
      IRM = KCLARM(ICL)
      N = 0
      DO 10 I=1,KTBI
        IF (IAL .NE. 0 .AND. NTB .NE. KTBOF(I))  GO TO 10
        N = N + 1
        IBUF(N) = KTBIT(I)
        RW(JR(JQVECH)) = RW(JR(JQVECH)) + RW(J1(JQVECH))
        DO 9 IB=1,KLOCKM
        IW(JR(JQVEBM+IB-1))
     &  =IOR(IW(JR(JQVEBM+IB-1)),IW(J1(JQVEBM+IB-1)))
    9   CONTINUE
   10 CONTINUE
C
C       daughter - mother relation, match :
C
      IF (IRE .NE. 0)  CALL QADAU (ITK, N, IBUF)
C
C       vertices :
C
      IW(JR(JQVESC)) = -1
      IW(JR(JQVEOV)) = 0
      IW(JR(JQVEEV)) = 0
      RW(JR(JQVEDB)) = 0.
      RW(JR(JQVEZB)) = 0.
      RW(JR(JQVESD)) = 0.
      RW(JR(JQVESZ)) = 0.
      RW(JR(JQVECB)) = 0.
C
C       other pointers etc.
C
      IW(JR(JQVETN)) = 0
      IW(JR(JQVEQD)) = KOQDET
      IW(JR(JQVEKS)) = 0
C
C        error matrix :
C
      RW(JR(JQVEEM)) = -1.
      RW(JR(JQVECF)) = -1.
C
   90 END
#endif
