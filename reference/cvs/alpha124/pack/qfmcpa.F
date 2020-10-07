      SUBROUTINE QFMCPA
CKEY FILL MC /INTERNAL
C-----------------------------------------------------------------------
C! Fill MC particles
C!                                                  H.Albrecht 18.10.88
C!                                        Modified: E.Lancon   12.07.89
C!                                        Modified: J.Boucrot  26.08.97
C!called from QFILMC
C-----------------------------------------------------------------------
#ifndef DOC
#include "fverjj.h"
#include "fkinjj.h"
#include "ajobjj.h"
#include "qcdesh.h"
      PARAMETER ( NRES = 10 ) 
      DIMENSION PDUM(3),XDUM(3),XHEL(5),XCEN(2)
      DATA EPSIL / 1.E-06 /
#include "bmacro.h"
C-----------------------------------------------------------------------
C
      JFKIN = IW(NAFKIN) + LMHLEN
      LFKIN = IW(JFKIN-1)
      JFVER = IW(NAFVER)
C
C MC particle section in QVEC :
C
      KNMCT = IW(JFKIN)
      KLMCT = KLMCT + KNMCT
      IF (KLMCT .GE. KLFRT)  CALL QSBANK ('QVEC', KLMCT + 200)
      JQVEC = KOQVEC + KFMCT * KCQVEC
      KFFRT = KLMCT + 1
C
      IF (KNQLIN+KNMCT .GE. IW(KOQLIN))
     &  CALL QSBANK ('QLIN',KNQLIN+KNMCT+500)
C
      JAJOB = IW(NAAJOB)
      NAVER = 9999
      IF (JAJOB .NE. 0)  NAVER  = ITABL (JAJOB,1,JAJOAV)
      ITK = KFMCT
      MAINMO = 0
      DO 10 IGA = 1, KNMCT
C
C         protect against weird tracks from KINGAL/GALEPH
C
C         zero momentum tracks
C
        PT=VMOD(RW(JFKIN+JFKIPX),2)
        IF (PT.LT.EPSIL) THEN
           RW(JFKIN+JFKIPY)=EPSIL/1000.
        ENDIF
        PZ=RW(JFKIN+JFKIPZ)
        IF (ABS(PZ).LT.EPSIL) RW(JFKIN+JFKIPZ)=EPSIL
C
C         anomalous particle code
C
        IF (IGA.EQ.KNMCT) THEN
           IF (IW(JFKIN+JFKIPA).LT.0.OR.IW(JFKIN+JFKIPA).GT.1000)
     &         GO TO 20
        ENDIF
C
C         Good tracks : basic track attributes
C
        RW(JQVEC+JQVEQX) = RW(JFKIN+JFKIPX)
        RW(JQVEC+JQVEQY) = RW(JFKIN+JFKIPY)
        RW(JQVEC+JQVEQZ) = RW(JFKIN+JFKIPZ)
        RW(JQVEC+JQVEQP) = SQRT (RW(JQVEC+JQVEQX)**2 +
     &     RW(JQVEC+JQVEQY)**2 + RW(JQVEC+JQVEQZ)**2)
C
C         Particle code and charge
C
        KVTEST=MOD(KEVERT,10000)
        IPC = KMCCOD (IW(JFKIN+JFKIPA))
C        IF (IPC.LE.0) GO TO 10
        RW(JQVEC+JQVECH) = RW(KOQPAR+IPC*KCQPAR+JQPACH)
        IW(JQVEC+JQVEPA) = IPC
        IFP = IW(KOQPLI+IPC)
        IF (IFP .EQ. 0)  IFP = KFPADR (IPC)
        IW(JQVEC+JQVENP) = IW(KOQFPA+IFP*KCQFPA+2)
        IW(KOQFPA+IFP*KCQFPA+2) = ITK
C
C Be careful to deal correctly w/ mass and energy since the banks
C KINE and FKIN store mass, not energy, in ALEPHLIB versions > 9.0
        IF (NAVER .LT. 90)  THEN
C
C Fill in mass. Either from en-mom or from ALEPH part. table
C Future : mass has to be stored in FKIN and taken from there !!!
C The following statements may cause problems (accuracy) !!!
C
          RW(JQVEC+JQVEQE) = RW(JFKIN+JFKIMA)
          RW(JQVEC+JQVEQM) = RW(KOQPAR+IPC*KCQPAR+JQPAMA)
          IF (( RW(JQVEC+JQVEQM) .GE. 0.0005 .AND.
     +       (RW(JQVEC+JQVEQP) / RW(JQVEC+JQVEQM)) .LT. 500.))
     +       RW(JQVEC+JQVEQM) = SQRT (AMAX1
     +       ((RW(JQVEC+JQVEQE) + RW(JQVEC+JQVEQP)) *
     +       (RW(JQVEC+JQVEQE) - RW(JQVEC+JQVEQP)), 0.))
        ELSE
          RW(JQVEC+JQVEQM) = RW(JFKIN+JFKIMA)
          RW(JQVEC+JQVEQE) = SQRT(RW(JQVEC+JQVEQP)**2 +
     &        RW(JQVEC+JQVEQM)**2)
        ENDIF
 
C
        IW(JQVEC+JQVETN) = IGA
        IW(JQVEC+JQVESC) = 0
        IW(JQVEC+JQVECL) = 2
        IW(JQVEC+JQVEQD) = KOQDET
        IW(JQVEC+JQVESP) = ITK
C
C         Origin and end vertex of track
C
        IF (IW(JFKIN+JFKIOV) .EQ. 0)  THEN
          IW(JQVEC+JQVEOV) = 0
        ELSE
          IW(JQVEC+JQVEOV) = IW(JFKIN+JFKIOV) + KFMCV - 1
        ENDIF
        IF (IW(JFKIN+JFKIEV) .EQ. 0)  THEN
          IW(JQVEC+JQVEEV) = 0
        ELSE
          IW(JQVEC+JQVEEV) = IW(JFKIN+JFKIEV) + KFMCV - 1
        ENDIF
C
C         mother - daughter - mother relation
C
        IW(JQVEC+JQVEND) = 0
        IW(JQVEC+JQVENO) = 0
        IW(JQVEC+JQVENM) = 0
        IM2=0
        IF (KNQLIN + NRES .GE. IW(KOQLIN))
     &    CALL QSBANK ('QLIN', KNQLIN + NRES + 500)
C Take account of the different history codes for Herwig events
        IF (KVTEST/100.EQ.51) THEN
          IMCODE = IW(JFKIN+JFKIHC)
          IHIST = IMCODE / 1000000
          IW(JQVEC+JQVEKS) = IHIST
          IHIST =  IHIST - (IHIST/100)*100
          IMCODE = IMCODE - IW(JQVEC+JQVEKS)*1000000
        ELSE
          IW(JQVEC+JQVEKS) = IW(JFKIN+JFKIHC) / 10000
        ENDIF

        IF (IGA .LE. KKEVNT)  THEN
C Herwig events can have particles with 2 mothers
          IF (KVTEST/100.EQ.51) THEN
            IM2 = IMCODE / 1000
            IM = IMCODE - IM2*1000
C Patch to stop initial state gammas being their own mother and
C force Z0 to be mother of initial partons
            IF(IM.EQ.0.AND.MAINMO.EQ.0.AND.IHIST.EQ.20) MAINMO=IGA
            IF(IM.NE.0.AND.MAINMO.EQ.0) IM=0
            IF(IM.EQ.1) IM=IM+MAINMO-1
          ELSE
            IM = MOD (IW(JFKIN+JFKIHC) , 10000)
          ENDIF
          IF (IM .NE. 0)  THEN
            IW(JQVEC+JQVENO) = 1
            IW(JQVEC+JQVEOL) = KNQLIN
            KNQLIN = KNQLIN + 1
            IW(KOQLIN+KNQLIN) = IM + KFMCT - 1
C add in 2nd mother if there
            IF (IM2.NE. 0)  THEN
              IW(JQVEC+JQVENO) = 1 + IW(JQVEC+JQVENO)
              KNQLIN = KNQLIN + 1
              IW(KOQLIN+KNQLIN) = IM2+ KFMCT - 1
            ENDIF
          ENDIF
        ELSE
C We turn to the FVER bank to get the history
          IM = IW(JFKIN+JFKIOV)
          IF (IM .NE. 0)  THEN
            IM = IW(KOQVRT+(IM+KFMCV)*KCQVRT+JQVRIP-KCQVRT)
            IF (IM .NE. 0)  THEN
              IW(JQVEC+JQVENO) = 1
              IW(JQVEC+JQVEOL) = KNQLIN
              KNQLIN = KNQLIN + 1
              IW(KOQLIN+KNQLIN) = IM
            ENDIF
          ENDIF
        ENDIF
C
        RW(JQVEC+JQVEEM) = -1.
        RW(JQVEC+JQVECF) = -1.
        DO 9 IB=1,KLOCKM
          IW(JQVEC+JQVEBM+IB-1) = 0
9       CONTINUE
        IW(JQVEC+JQVELK) = 0
C
        RW(JQVEC+JQVEDB) = 0.
        RW(JQVEC+JQVEZB) = 0.
C Compute D0 and Z0 if MC track has an Origin Vertex
C And if ABS(Q) >= 1
C And if PT .ne. 0.
        IF( IW(JFKIN+JFKIOV).NE.0 .AND. QMFLD.NE.0. .AND.
     &      ABS(RW(JQVEC+JQVECH)).GE.1. ) THEN
          PDUM(1) = RW(JFKIN+JFKIPX)
          PDUM(2) = RW(JFKIN+JFKIPY)
          IF (ABS(PDUM(1)).GT.1.E-10 .OR. ABS(PDUM(2)).GT.1.E-10) THEN
             XDUM(1) = RTABL(JFVER,IW(JFKIN+JFKIOV),JFVEVX)
             XDUM(2) = RTABL(JFVER,IW(JFKIN+JFKIOV),JFVEVY)
             XDUM(3) = RTABL(JFVER,IW(JFKIN+JFKIOV),JFVEVZ)
             PDUM(3) = RW(JFKIN+JFKIPZ)
             CHRG = RW(JQVEC+JQVECH)
             CALL TNRHPA(PDUM,XDUM,CHRG,QMFLD,XHEL,XCEN,DUMMY)
             RW(JQVEC+JQVEDB) = XHEL(4)
             RW(JQVEC+JQVEZB) = XHEL(5)
          ENDIF
        ENDIF
C
        RW(JQVEC+JQVESD) = 0.
        RW(JQVEC+JQVESZ) = 0.
        RW(JQVEC+JQVECB) = 0.
        RW(JQVEC+JQVEEW) = 0.
C
        ITK = ITK + 1
        JFKIN = JFKIN + LFKIN
   10 JQVEC = JQVEC + KCQVEC
      GO TO 999
C Anomalies in tracks : particle code out of range:
 20   WRITE (IW(6),1003) KEVT,KRUN,KNMCT
 1003 FORMAT (/'** QFMCPA : in Event ',I6,'  Run ',I5,
     &          ' : anomalous FKIN track ',I5,' dropped'/)
      KNMCT=KNMCT-1
      KLMCT=KLMCT-1
C
 999  RETURN
      END
#endif




