      SUBROUTINE QVKINK(IVK,Patkink,ptout,plout,ihypk,ier)
CKEY WRITE VERTEX /USER
C----------------------------------------------------------------------
C! get special values for kink vertex IVX
C
C called from user routines
C                                                   J.Boucrot 29.04.97
C  Input argument :
C     IVK  = ALPHA number for kink vertex
C  Output arguments :
C     IER  = 0 if this is realluy a kink vertex
C          = 1 if not
C   If IER = 0 , the following quantities are defined :
C  Patkink = Momentum of parent at kink position
C  Ptout   = Pt of outgoing charged particle relative to parent
C  Plout   = Pl of outgoing charged particle relative to parent
C  IHYPK   = Hypothesis of mass cuts satisfied by this decay.
C          = 1   pi -> mu nu
C          = 2   K -> mu nu
C          = 3   K -> pi pi0
C          = 4   Sigma+ -> p pi0
C          = 5   Sigma+ -> pi+ n
C          = 6   Sigma- -> pi- n
C          = 7   Xi- -> pi- Lambda
C          = 8   Omega- -> K- Lambda
C          = 9   Omega- -> pi- Xi0
C          = 10  Omega- -> Xi- pi0
C          = 0   if none of the above hypotheses are satisfied
C----------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
      PARAMETER ( IKINK = 5 , NHYPS = 10 )
      LOGICAL BTEST
#include "qmacro.h"
C----------------------------------------------------------------------
      JQVRT(I,N) = KOQVRT + I * KCQVRT + N
C----------------------------------------------------------------------
      IER=1
      PATKINK=0.
      PLOUT=0.
      PTOUT=0.
      IHYPK=0
      IF (IVK.LT.KFREV.OR.IVK.GT.KLREV) GO TO 999
      IF (KVTYPE(IVK).NE.IKINK) GO TO 999
C This is a kink vertex : decode the quantities:
      IER=0
      PATKINK  = RW(JQVRT(IVK,JQVRET+ 9))
      PTOUT    = RW(JQVRT(IVK,JQVRET+10))
      PLOUT    = RW(JQVRT(IVK,JQVRET+11))
      IBIPA    = IW(JQVRT(IVK,JQVRET+12))
C No fit hypothesis:
      IF (IBIPA.EQ.0) GO TO 999
C Find fit hypothesis:
      DO 100 IHY=1,NHYPS
         IF (BTEST(IBIPA,IHY-1)) THEN
            IHYPK=IHY
            GO TO 999
         ENDIF
 100  CONTINUE
 999  RETURN
      END
#endif
