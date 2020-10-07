      SUBROUTINE QFYLV0
CKEY FILL VERTEX /INTERNAL
C----------------------------------------------------------------------
C! Fill ALPHA banks with "Long V0s" from new tracking
C!  Author   : J. Boucrot   23-Apr-1997  mod 15-Oct-1997
C!  Description
C!  ===========
C! QVRT filled with "Long V0" vertices from YLV0 bank
C! QVEC "Long V0 tracks" filled from YLV0
C!  ---->  called from QFILNV
C======================================================================
#ifndef DOC
#include "qcde.h"
#include "ylv0jj.h"
      PARAMETER ( ITYVR = 3 )
      INTEGER ITKD(2)
#include "qmacro.h"
C ----------------------------------------------------------------------
      JQVRT(I,N) = KOQVRT + I * KCQVRT + N
      JQVEC(I,N) = KOQVEC + I * KCQVEC + N
      IQVEC(I,N) = IW (KOQVEC + I * KCQVEC + N)
      IQVRT(I,N) = IW (KOQVRT + I * KCQVRT + N)
      JR(N) = KOQVEC + ITK * KCQVEC + N
C-----------------------------------------------------------------------
C
      NAYLV0=NAMIND('YLV0')
C      IF (KNCHT.EQ.0) GO TO 999
      JYLV0 = IW(NAYLV0)
      IF (JYLV0.LE.0) GO TO 999
      NYLV0=LROWS(JYLV0)
C
C Define pointers into QVRT and QVEC; increase banks if needed:
C
      IF (KFFRV+NYLV0 .GE. KLFRV)  CALL QSBANK ('QVRT', KFFRV+NYLV0+100)
      JNQVRT = KOQVRT + KFFRV*KCQVRT
      IF (KFFRT+NYLV0 .GE. KLFRT)  CALL QSBANK ('QVEC', KFFRT+NYLV0+100)
      JNQVEC = KOQVEC + KFFRT*KCQVEC
      KFF0 = KFFRT
C
C---Loop on all V0's from YLV0:
C
      DO 300 N = 1, NYLV0
        IYLV0 = KROW(JYLV0,N)
        IV0 = KFF0+N-1
        RW(JNQVRT+JQVRVX)=RW(IYLV0+JYLVVX)
        RW(JNQVRT+JQVRVY)=RW(IYLV0+JYLVVY)
        RW(JNQVRT+JQVRVZ)=RW(IYLV0+JYLVVZ)
        IW(JNQVRT+JQVRVN)=N
        IW(JNQVRT+JQVRTY)=ITYVR
        RW(JNQVRT+JQVRCF)=RW(IYLV0+JYLVC2)
        RW(JNQVRT+JQVREM  )=RW(IYLV0+JYLVVM  )
        RW(JNQVRT+JQVREM+1)=RW(IYLV0+JYLVVM+1)
        RW(JNQVRT+JQVREM+2)=RW(IYLV0+JYLVVM+2)
        RW(JNQVRT+JQVREM+3)=RW(IYLV0+JYLVVM+3)
        RW(JNQVRT+JQVREM+4)=RW(IYLV0+JYLVVM+4)
        RW(JNQVRT+JQVREM+5)=RW(IYLV0+JYLVVM+5)
        IW(JNQVRT+JQVRAY)= IYLV0
        IW(JNQVRT+JQVRAF)= 0
        IW(JNQVRT+JQVRIP) = IV0
        IW(JNQVRT+JQVRND)=2
C
C---Fill QVEC with V0s from YLV0 :
C
        RW(JNQVEC+JQVEQX)=RW(IYLV0+JYLVPX)
        RW(JNQVEC+JQVEQY)=RW(IYLV0+JYLVPY)
        RW(JNQVEC+JQVEQZ)=RW(IYLV0+JYLVPZ)
        RW(JNQVEC+JQVEQE)=SQRT(RW(JNQVEC+JQVEQX)**2+
     &                  RW(JNQVEC+JQVEQY)**2+RW(JNQVEC+JQVEQZ)**2)
        RW(JNQVEC+JQVEQM)=0.
        RW(JNQVEC+JQVEQP)=RW(JNQVEC+JQVEQE)
        RW(JNQVEC+JQVECH)=0.
        IW(JNQVEC+JQVETN)=N
        IW(JNQVEC+JQVESC)=-2
        IW(JNQVEC+JQVEKS)=0
        IW(JNQVEC+JQVECL)=1
        IW(JNQVEC+JQVEPA)=0
        IW(JNQVEC+JQVEQD)=KQZER
        IW(JNQVEC+JQVENP)=0
        IW(JNQVEC+JQVESP)= IV0
        IW(JNQVEC+JQVEOV)=0
        IW(JNQVEC+JQVENO)=0
        IW(JNQVEC+JQVEOL)=0
        IW(JNQVEC+JQVENM)=0
        IW(JNQVEC+JQVEML)=0
        IW(JNQVEC+JQVELK)=0
        RW(JNQVEC+JQVEEW)=0.
C
        NOUT = 2
        IW(JNQVEC+JQVEND)= NOUT
        IW(JNQVEC+JQVEEV)= KFFRV
C---Set bit masks for V0s: OR of bit masks of daughters.
        IDA1=KFCHT+IW(IYLV0+JYLVK1)-1
        IDA2=KFCHT+IW(IYLV0+JYLVK2)-1
        ITKD(1)=IDA1
        ITKD(2)=IDA2
        DO 10 IDUM=1,KLOCKM
           IW(JNQVEC+JQVEBM+IDUM-1)
     &     = IOR(IQVEC(IDA1,JQVEBM+IDUM-1),IQVEC(IDA2,JQVEBM+IDUM-1))
10      CONTINUE
C---Compute and store d0 and z0:
        X=RW(IYLV0 + JYLVVX) - QVXNOM
        Y=RW(IYLV0 + JYLVVY) - QVYNOM
        Z=RW(IYLV0 + JYLVVZ) - QVZNOM
        PX =RW(JNQVEC+JQVEQX)
        PY =RW(JNQVEC+JQVEQY)
        PZ =RW(JNQVEC+JQVEQZ)
        PTSQ = PX**2 + PY**2
        IF(PTSQ.NE.0.) THEN
          XLAMB=(-X*PX-Y*PY)/PTSQ
        ELSE
          XLAMB = 0.
        ENDIF
        XDB=X+XLAMB*PX
        YDB=Y+XLAMB*PY
        ZDB=Z+XLAMB*PZ
        DB = SQRT(XDB**2+YDB**2)
        RW(JNQVEC+JQVEDB)=DB
        RW(JNQVEC+JQVEZB)=ZDB
C Set daughter-mother relations for the outgoing tracks :
        CALL QAMOTH(IV0,2,ITKD)
C
C Set daughter-mother relations for the V0 track:
C
        IF (KNQLIN+NOUT.GT.IW(KOQLIN))
     +       CALL QSBANK('QLIN',KNQLIN+NOUT+1000)
        IW(JQVRT(KFFRV,JQVRDL))=KNQLIN - IW(JQVRT(KFFRV,JQVRND))
        IW(JQVEC(IV0,JQVEDL))=KNQLIN
        KNQLIN = KNQLIN + IW(JQVRT(KFFRV,JQVRND))
        DO 200 IIT=1,NOUT
           ITK=ITKD(IIT)
           IW(JQVRT(KFFRV,JQVRDL))=IW(JQVRT(KFFRV,JQVRDL))+1
           IW(KOQLIN+IW(JQVRT(KFFRV,JQVRDL))+IW(JQVRT(KFFRV,JQVRND)))
     +         = ITK
           IW(JR(JQVEOV)) = KFFRV
 200    CONTINUE
C
C go to next V0 : update all pointers
C
        JNQVRT = JNQVRT + KCQVRT
        JNQVEC = JNQVEC + KCQVEC
C Update pointers of vertex families:
        KFFRV = KFFRV + 1
        KLREV = KLREV + 1
        KNREV = KNREV + 1
        KFMCV = KFMCV + 1
        KLMCV = KLMCV + 1
C Update pointers of track  families:
        KFFRT = KFFRT + 1
        KFFRD = KFFRD + 1
C
  300 CONTINUE
C
C Update KLOCUS pointers.
      DO 350 N=KSDCT+1,KSMCT
         KLOCUS(1,N)=KLOCUS(2,KSDCT)+1
         KLOCUS(2,N)=KLOCUS(2,KSDCT)
 350  CONTINUE
C
 999  RETURN
      END
#endif


