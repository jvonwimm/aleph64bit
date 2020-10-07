      INTEGER FUNCTION KBESTM(ITK)
CKEY MONTE /INTERNAL
C-----------------------------------------------------------------------
C! Find best match between fitted and MC track
C
C  R. Johnson   10-5-90
C
C  Input:  ITK             /I   ALPHA charged track number, MC or DATA
C  Output: KBESTM          /I   Best match to MC or data track
C-----------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
#include "qmacro.h"
C-----------------------------------------------------------------------
      KBESTM=0
      MXHT=0
      DO 10 K=1,KNMTCH(ITK)
        IF (KSMTCH(ITK,K).GT.MXHT.AND.KCH(KMTCH(ITK,K)).NE.0) THEN
          MXHT=KSMTCH(ITK,K)
          KBESTM=KMTCH(ITK,K)
        ENDIF
   10 CONTINUE
C
C++   For MC, try mothers and daughters also, if they are of
C++   the same type
C
      IF (XMC(ITK)) THEN
        MOM=KMOTH(ITK,1)
        IF (KTPCOD(ITK).EQ.KTPCOD(MOM)) THEN
          DO 20 K=1,KNMTCH(MOM)
            IF (KSMTCH(MOM,K).GT.MXHT) THEN
              MXHT=KSMTCH(MOM,K)
              KBESTM=KMTCH(MOM,K)
            ENDIF
   20     CONTINUE
        ENDIF
        DO 30 J=1,KNDAU(ITK)
          IDAU=KDAU(ITK,J)
          IF (KTPCOD(ITK).EQ.KTPCOD(IDAU)) THEN
            DO 40 K=1,KNMTCH(IDAU)
              IF (KSMTCH(IDAU,K).GT.MXHT) THEN
                MXHT=KSMTCH(IDAU,K)
                KBESTM=KMTCH(IDAU,K)
              ENDIF
   40       CONTINUE
          ENDIF
   30   CONTINUE
      ENDIF
      IF (KBESTM.EQ.0) THEN
        GOTO 999
      ENDIF
      IF (XFRF(ITK)) THEN
        JFIT=ITK
      ELSE
        JFIT=KBESTM
      ENDIF
      NTOT=KFRTNI(JFIT)+KFRTNT(JFIT)
      IF (MXHT.LT.NTOT/2) THEN
        KBESTM=0
        GOTO 999
      ENDIF
      IF (MXHT.LT.4) THEN
        KBESTM=0
        GOTO 999
      ENDIF
C
 999  END
#endif
