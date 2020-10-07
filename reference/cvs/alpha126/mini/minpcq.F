      SUBROUTINE MINPCQ
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Fill PCQA bank from DNEU and DRES.
C
C     Author: J.Carr    14 May 1991
c              ( modified from MINPCP of Stephen Haywood )
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "dneujj.h"
#include "dresjj.h"
#include "minfac.h"
C
      LOGICAL HADRO,GAMMA,GAMEX,GARBA,LCALO
      PARAMETER ( MXNEUT=200 )
      DIMENSION PARLOC(MXNEUT,5)
      DIMENSION IPEPC(MXNEUT),IPCOB(MXNEUT),ITYPE(MXNEUT)
      LOGICAL PARGON(MXNEUT)
C
#include "bmacro.h"
C-----------------------------------------------------------------------
      HADRO(I) = ITYPE(I).GE.13.AND.ITYPE(I).LE.20
      GAMMA(I) = ITYPE(I).LT.13.AND.ITYPE(I).NE.5
      GAMEX(I) = ITYPE(I).EQ.5
      GARBA(I) = ITYPE(I).EQ.21
      LCALO(I) = ITYPE(I).EQ.22
C
      KDNEU = NLINK('DNEU',0)
      IF(KDNEU.GT.0) THEN
         NDNEU = LROWS(KDNEU)
      ELSE
         NDNEU = 0
      ENDIF
C
      KDRES = NLINK('DRES',0)
      IF(KDRES.GT.0) THEN
         NDRES = LROWS(KDRES)
      ELSE
         NDRES = 0
      ENDIF
C
      IF (NDNEU.LE.0 .AND. NDRES.LE.0) RETURN
      MVER = MINGTV(DUM)
C
C++   Fill temporary PARLOC array from DNEU bank.
C
      J = 0
      DO 100 I=1,NDNEU
         IF(J.LT.MXNEUT)J = J + 1
         IY = ITABL(KDNEU,I,JDNENA)
         EN = FLOAT(ITABL(KDNEU,I,JDNEE0))/EFACTM
         TH = FLOAT(ITABL(KDNEU,I,JDNETH))/AFACTM
         PH = FLOAT(ITABL(KDNEU,I,JDNEPH))/AFACTM
         PSUM=0.
         PN = EN
         CP = COS (PH)
         SP = SIN (PH)
         CT = COS (TH)
         ST = SIN (TH)
         PT = PN * ST
         PARLOC(J,1) = PT * CP
         PARLOC(J,2) = PT * SP
         PARLOC(J,3) = PN * CT
         PARLOC(J,4) = EN
         PARLOC(J,5) = PSUM
         ITYPE(J) = ITABL(KDNEU,I,JDNENA)
         IF (MVER.GE.54) THEN
            IPEPC(J) = ITABL(KDNEU,I,JDNEDE)
            IPCOB(J) = ITABL(KDNEU,I,JDNEPC)
         ELSE
            IPEPC(J) = ITABL(KDNEU,I,7)
            IPCOB(J) = 0
         ENDIF
         PARGON(J) = .FALSE.
         IF(GARBA(I)) PARGON(J) = .TRUE.
  100 CONTINUE
      NMXNEU=J
C
C++   Fill temporary array from DRES.
C
      DO 200 I=1,NDRES
         IF(J.LT.MXNEUT) J = J + 1
         IY = ITABL(KDRES,I,JDRENA)
         EN = FLOAT(ITABL(KDRES,I,JDREE0))/EFACTM
         TH = FLOAT(ITABL(KDRES,I,JDRETH))/AFACTM
         PH = FLOAT(ITABL(KDRES,I,JDREPH))/AFACTM
         PSUM = FLOAT(ITABL(KDRES,I,JDREPS))/EFACTM
         PN   = FLOAT(ITABL(KDRES,I,JDREP0))/EFACTM
         CP = COS (PH)
         SP = SIN (PH)
         CT = COS (TH)
         ST = SIN (TH)
         PT = PN * ST
         PARLOC(J,1) = PT * CP
         PARLOC(J,2) = PT * SP
         PARLOC(J,3) = PN * CT
         PARLOC(J,4) = EN
         PARLOC(J,5) = PSUM
         ITYPE(J) = ITABL(KDRES,I,JDRENA)
         IPEPC(J) = 0
         IF (MVER.GE.54) THEN
            IPCOB(J) = ITABL(KDRES,I,JDREPC)
         ELSE
            IPCOB(J) = 0
         ENDIF
         PARGON(J) = .FALSE.
  200 CONTINUE
C
      NMXALL = J
C
C++   Maybe merge and cut particles
C
      CALL PCMECU( NMXALL,MXNEUT,PARLOC,IPEPC,IPCOB,ITYPE,PARGON)
C
      RETURN
      END
#endif
