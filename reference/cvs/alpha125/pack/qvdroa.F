      SUBROUTINE QVDROA (CNAM,ICL)
CKEY CLASS TRACKS /INTERNAL
C----------------------------------------------------------------------
C! Drop tracks with name CNAM in class ICL. CNAM = ' ' : all tracks.
C                                                    H.Albrecht  2.1.89
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
      CHARACTER *(*) CNAM
C----------------------------------------------------------------------
      JJ(I,J) = KOQVEC + I * KCQVEC + J
      JF(I)  = KOQFPA + I * KCQFPA + ICL
C----------------------------------------------------------------------
C       special call : CNAM = ' '
C
      IF (CNAM .EQ. ' ')  THEN
        IFP1 = 1
        IFP2 = KNQFPA
      ELSE
        IFP1 = IW(KOQPLI+KPART(CNAM))
        IF (IFP1 .EQ. 0)  GO TO 190
        IFP2 = IFP1
      ENDIF
C
C        loop over all particles in QFPA
C
      DO 100 IFP = IFP1, IFP2
        ITK = IW(JF(IFP))
        IW(JF(IFP)) = 0
   10   IF (ITK .EQ. 0)  GO TO 100
        ITKN = IW(JJ(ITK,JQVENP))
C
C         unviolable track ?
C
        IF (ITK .LE. KLMCT)  THEN
          IW (JJ(ITK,JQVENP)) = IW(JF(IFP))
          IW(JF(IFP)) = ITK
          GO TO 90
        ENDIF
C
C         list of dropped tracks
C
        IW(JJ(ITK,JQVENP)) = KTDROP
        KTDROP = ITK
C
C         drop KSAME pointer
C
        ITK0 = IW(JJ(ITK,JQVESP))
        ITK1 = ITK0
   20   ITK2 = IW(JJ(ITK1,JQVESP))
        IF (ITK2 .NE. ITK)  THEN
          ITK1 = ITK2
          GO TO 20
        ENDIF
        IW(JJ(ITK1,JQVESP)) = ITK0
        IW(JJ(ITK,JQVESP)) = ITK
C
C          daughter - mother relation
C
        DO 50 N=1,IW(JJ(ITK,JQVENO))
          ITK1 = IW(KOQLIN+IW(JJ(ITK,JQVEOL))+N)
          IN = IW(JJ(ITK1,JQVEND))
          IL = IW(JJ(ITK1,JQVEDL))
          ID = 0
          DO 40 I=1,IN
            IF (IW(KOQLIN+IL+I) .EQ. ITK)  ID = 1
   40     IW(KOQLIN+IL+I) = IW(KOQLIN+IL+I+ID)
          IF (ID .NE. 0)  IW(JJ(ITK1,JQVEND)) = IW(JJ(ITK1,JQVEND)) - 1
   50   CONTINUE
C
C          mother - daughter relation
C
        DO 70 N=1,IW(JJ(ITK,JQVEND))
          ITK1 = IW(KOQLIN+IW(JJ(ITK,JQVEDL))+N)
          IN = IW(JJ(ITK1,JQVENO))
          IL = IW(JJ(ITK1,JQVEOL))
          ID = 0
          DO 60 I=1,IN
            IF (IW(KOQLIN+IL+I) .EQ. ITK)  ID = 1
   60     IW(KOQLIN+IL+I) = IW(KOQLIN+IL+I+ID)
          IF (ID .NE. 0)  IW(JJ(ITK1,JQVENO)) = IW(JJ(ITK1,JQVENO)) - 1
   70   CONTINUE
C
        IW(JJ(ITK,JQVEND)) = 0
        IW(JJ(ITK,JQVENO)) = 0
C
   90   ITK = ITKN
        GO TO 10
C
  100 CONTINUE
C
C         class empty ?
C
      IF (ICL .LE. 2)  GO TO 190
      IF (IFP1 .EQ. IFP2)  THEN
        DO 110 IFP = 1, KNQFPA
  110   IF (IW(JF(IFP)) .NE. 0)  GO TO 190
      ENDIF
C
      KCLACO(ICL) = KUNDEF
      KCLAFR(ICL) = 0
      KCLARM(ICL) = 0
C
  190 END
#endif
