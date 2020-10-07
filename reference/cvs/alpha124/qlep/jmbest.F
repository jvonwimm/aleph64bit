      INTEGER FUNCTION JMBEST(ITK)
CKEY  QTRUTH/INTERNAL
C ...............................................................
C! Gives best true track candidate
C  Authors  D. Abbaneo , F. Ligabue 27/07/94  from FINLEP / UPHY
C  Modified 26-Aug-1996 D.Abbaneo ( protections added )         .
C  Called from QTRUTH                                           .
C . Gives the best true track candidate associated to the       .
C . reconstructed track ITK                                     .
C ...............................................................
#ifndef DOC
      IMPLICIT NONE
#include "qdecl.h"
#include "qcde.h"
C
C - variables
      INTEGER ITK,MXHT,MOM,IDAU,NTOT,IMC,I,J,K
C
C - macros
#include "qmacro.h"
C
      JMBEST=0
      MXHT=0
      DO K=1,KNMTCH(ITK)
        IF (KSMTCH(ITK,K).GT.MXHT.and.kch(kmtch(itk,k)).ne.0) THEN
          MXHT=KSMTCH(ITK,K)
          JMBEST=KMTCH(ITK,K)
        ENDIF
      enddo
      IF (JMBEST.EQ.0) RETURN
C
      NTOT=KFRTNI(ITK)+KFRTNT(ITK)
      IF (MXHT.LT.NTOT/2.OR.MXHT.LT.4) THEN
C
C - see whether MC mothers or daughters share this data track and
C   increase MXHT accordingly
        IMC = JMBEST
        DO  I = 1,KNMOTH(IMC)
          MOM = KMOTH(IMC,I)
          DO J = 1,KNMTCH(MOM)
            IF (KMTCH(MOM,J).EQ.ITK) MXHT = MXHT + KSMTCH(MOM,J)
          enddo
        enddo
        DO I = 1,KNDAU(IMC)
          IDAU = KDAU(IMC,I)
          DO J = 1,KNMTCH(IDAU)
            IF (KMTCH(IDAU,J).EQ.ITK) MXHT = MXHT + KSMTCH(IDAU,J)
          enddo
        enddo
        IF (MXHT.LT.NTOT/2.OR.MXHT.LT.4) JMBEST=0
        RETURN
      ENDIF
C
      RETURN
      END
#endif
