      INTEGER FUNCTION KVFITC(NTKD,ITKD,RMAS,DRM,CNAM)
CKEY YTOP VERTEX   /USER
C----------------------------------------------------------------------
C!  - Fit NTKD tracks in ITKD(I) to a common vertex with mass constr.
C!
C!   Author   :- E. Blucher, H.G.Moser         22-Jan-1993
C!
C!   Inputs:
C!        - Number of tracks to be fit to a vertex
C!        - Array of track numbers
C!        - mass constraint
C!        - error on mass**2 (= 2 * m * sigma(m) !)
C!
C!   Outputs:
C!        - track number of incoming track created by routine
C!        - new vertex in QVRT is created.
C?
C!======================================================================
#ifndef DOC
      SAVE IERR
#include "qcde.h"
      CHARACTER * (*) CNAM
      DATA IERR /0/
      DIMENSION ITKD(*)
      REAL PSUM(5),VPSUM(10),VOUT(3),VVOUT(6),VPSVX(3,3)
#include "qmacro.h"
C----------------------------------------------------------------------
      JQVRT(I,N) = KOQVRT + I * KCQVRT + N
      JR(N) = KOQVEC + ITK* KCQVEC + N
      JE(N) = KOQVEC + ITKD(IDAU)* KCQVEC + N
      IQVEC(I,N) = IW (KOQVEC + I * KCQVEC + N)
      IQVRT(I,N) = IW (KOQVRT + I * KCQVRT + N)
C----------------------------------------------------------------------
C---Protections against too few daughters and mixed classes.
C
      KVFITC = -1
      IF (XNANO) GO TO 999
      IF(NTKD.LT.2)THEN
         CALL QWMESS
     +   ('_KVFITC_ Less than two daughters -- no fit done.')
         GOTO 999
      ENDIF
      ICLASS=IQVEC(ITKD(1),JQVECL)
      IF(ICLASS.EQ.0)THEN
         CALL QMTERM
     +   ('_KVFITC_ Particle with class = 0.')
      ENDIF
      DO 11 I=2,NTKD
      IF(IQVEC(ITKD(I),JQVECL).NE.ICLASS)THEN
         CALL QMTERM
     +   ('_KVFITC_ Particles of different classes mixed up.')
      ENDIF
11    CONTINUE
C
C--- Call vertexing routine
C
      CALL QYFITC(ITKD,NTKD,RMAS,DRM,
     +PSUM,VPSUM,ICHR,VOUT,VVOUT,VPSVX,CHI2)
C
C    check return code
C
        IF(CHI2.LT.0.) GOTO 999
C
C--- Store new vertex
C
        KVRTX  = KFFRV
        KFFRV = KFFRV + 1
        IF (KFFRV .GE. KLFRV)  CALL QSBANK ('QVRT', KFFRV+200)
C
        RW(JQVRT(KVRTX,JQVRVX))=VOUT(1)
        RW(JQVRT(KVRTX,JQVRVY))=VOUT(2)
        RW(JQVRT(KVRTX,JQVRVZ))=VOUT(3)
        IW(JQVRT(KVRTX,JQVRVN))= 0
        IW(JQVRT(KVRTX,JQVRTY))= 2
        IW(JQVRT(KVRTX,JQVRIP))=KFFRT
        IW(JQVRT(KVRTX,JQVRND))=NTKD
        RW(JQVRT(KVRTX,JQVREM  )) = VVOUT(1)
        RW(JQVRT(KVRTX,JQVREM+1)) = VVOUT(2)
        RW(JQVRT(KVRTX,JQVREM+2)) = VVOUT(3)
        RW(JQVRT(KVRTX,JQVREM+3)) = VVOUT(4)
        RW(JQVRT(KVRTX,JQVREM+4)) = VVOUT(5)
        RW(JQVRT(KVRTX,JQVREM+5)) = VVOUT(6)
        RW(JQVRT(KVRTX,JQVRET  )) = VPSVX(1,1)
        RW(JQVRT(KVRTX,JQVRET+1)) = VPSVX(1,2)
        RW(JQVRT(KVRTX,JQVRET+2)) = VPSVX(1,3)
        RW(JQVRT(KVRTX,JQVRET+3)) = VPSVX(2,1)
        RW(JQVRT(KVRTX,JQVRET+4)) = VPSVX(2,2)
        RW(JQVRT(KVRTX,JQVRET+5)) = VPSVX(2,3)
        RW(JQVRT(KVRTX,JQVRET+6)) = VPSVX(3,1)
        RW(JQVRT(KVRTX,JQVRET+7)) = VPSVX(3,2)
        RW(JQVRT(KVRTX,JQVRET+8)) = VPSVX(3,3)
        RW(JQVRT(KVRTX,JQVRCF  )) = CHI2
        IW(JQVRT(KVRTX,JQVRAY))=0
        IW(JQVRT(KVRTX,JQVRAF))=0
C
C--- Store new track
C
        KVFITC=KFFRT
        ITK=KFFRT
        KFFRT=KFFRT+1
        IF (KFFRT .GE. KLFRT)  CALL QSBANK ('QVEC', KFFRT+200)
C
        RW(JR(JQVEQX))= PSUM(1)
        RW(JR(JQVEQY))= PSUM(2)
        RW(JR(JQVEQZ))= PSUM(3)
        RW(JR(JQVEQE))= PSUM(4)
        RW(JR(JQVEQM))= PSUM(5)
        RW(JR(JQVEQP))= SQRT(PSUM(1)**2+PSUM(2)**2+PSUM(3)**2)
        RW(JR(JQVECH))= FLOAT(ICHR)
        IW(JR(JQVETN))= 0
        IW(JR(JQVESC))= 1
        IW(JR(JQVEKS))= 0
        IW(JR(JQVEQD))= KOQDET
        IW(JR(JQVESP)) = ITK
        RW(JR(JQVEEM))=   VPSUM(1)
        RW(JR(JQVEEM+1))= VPSUM(2)
        RW(JR(JQVEEM+2))= VPSUM(3)
        RW(JR(JQVEEM+3))= VPSUM(4)
        RW(JR(JQVEEM+4))= VPSUM(5)
        RW(JR(JQVEEM+5))= VPSUM(6)
        RW(JR(JQVEEM+6))= VPSUM(7)
        RW(JR(JQVEEM+7))= VPSUM(8)
        RW(JR(JQVEEM+8))= VPSUM(9)
        RW(JR(JQVEEM+9))= VPSUM(10)
        RW(JR(JQVEDB  ))= 0.
        RW(JR(JQVEZB  ))= 0.
        RW(JR(JQVESD  ))= 0.
        RW(JR(JQVESZ  ))= 0.
        RW(JR(JQVECB  ))= 0.
        IW(JR(JQVEOV))=0
        IW(JR(JQVEEV))=KVRTX
        IW(JR(JQVENO))=0
        IW(JR(JQVENM))=0
        IW(JR(JQVELK))=0
        IW(JR(JQVEND))=0
        RW(JR(JQVECF)) = -1.
C
C---Set bit masks equal to or of daughter track bit masks
C
        DO 9 IDUM=1,KLOCKM
            IW(JR(JQVEBM+IDUM-1)) = 0
9       CONTINUE
        DO 7 IDAU=1,NTKD
              DO 8 IDUM=1,KLOCKM
                IW(JR(JQVEBM+IDUM-1)) =
     &          IOR(IW(JR(JQVEBM+IDUM-1)),IW(JE(JQVEBM+IDUM-1)))
 8         CONTINUE
 7         CONTINUE
      IW(JR(JQVECL)) = ICLASS
      IF (CNAM .NE. ' ')  THEN
        IPC = KPART (CNAM)
        IW(JR(JQVEPA)) = IPC
        IFP = IW(KOQPLI+IPC)
        IF (IFP .EQ. 0)  IFP = KFPADR (IPC)
        IW(JR(JQVENP)) = IW(KOQFPA+IFP*KCQFPA+ICLASS)
        IW(KOQFPA+IFP*KCQFPA+ICLASS) = KVFITC
      ELSE
        IW(JR(JQVEPA)) = 0
      ENDIF
C
C---Set up mother to daughter relationship between incoming track
C---and outgoing tracks.
C
      CALL QADAU(KVFITC,NTKD,ITKD)
C---SET DAUGHTER LIST FOR VERTEX EQUAL TO DAUGHTER LIST FOR IP
      IW(JQVRT(KVRTX,JQVRDL))=IW(JR(JQVEDL))
  999 RETURN
      END
#endif
