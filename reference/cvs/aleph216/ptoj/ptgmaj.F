      SUBROUTINE PTGMAJ(LIST,IER)
C
C-------------------------------------------------------------------------
C! Unpack banks TGMA and TMTL from the POT
C
C     Author:   R. Johnson   10-1-89
C
C     Input :    LIST      BOS event list
C                          if LIST(2:2).eq.'-' drop POT banks
C     Output:    IER       = 0  successful
C                          = 1  input bank does not exist or is empty
C                          = 2  not enough space
C                          =-1  OK but garbage collection
C
C    Called by TPTOJ
C
C-------------------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "bcs.h"
#include "frtljj.h"
#include "frftjj.h"
#include "tgmajj.h"
#include "tmtljj.h"
#include "ptmajj.h"
#include "ptmljj.h"
#include "fxjjpar.h"
#include "partjj.h"
C
      CHARACTER LIST*(*),PLIST*8,JLIST*8
      DIMENSION A(5,5),HELIX(5),P(3),X(3),XC(2)
      LOGICAL FIRST
      DATA FIRST/.TRUE./, FIELD/15.0/
C
#include "bmacro.h"
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTGMA=NAMIND('TGMA')
        NTMTL=NAMIND('TMTL')
        NPTMA=NAMIND('PTMA')
        NPTML=NAMIND('PTML')
        NFRTL=NAMIND('FRTL')
        NFRFT=NAMIND('FRFT')
        NFKIN=NAMIND('FKIN')
        NFVER=NAMIND('FVER')
        NPART=NAMIND('PART')
      ENDIF
C
      IER=1
      KPTMA=IW(NPTMA)
      IF (KPTMA.EQ.0) GO TO 888
      KPTML=IW(NPTML)
      IF (KPTML.EQ.0) GO TO 888
C
      IER=0
      IF (IW(NTGMA).NE.0) CALL BDROP(IW,'TGMA')
      IF (IW(NTMTL).NE.0) CALL BDROP(IW,'TMTL')
C
      LEN=LROWS(KPTMA)*LTGMAA+LMHLEN
      IW(1)=1
      CALL AUBOS('TGMA',0,LEN,KTGMA,IER)
      IF (IER.EQ.2) GO TO 999
      IW(KTGMA+LMHCOL)=LTGMAA
      IW(KTGMA+LMHROW)=LROWS(KPTMA)
C
      KPTML=IW(NPTML)
      LEN=LROWS(KPTML)*LTMTLA+LMHLEN
      IW(1)=1
      CALL AUBOS('TMTL',0,LEN,KTMTL,IER)
      IF (IER.EQ.2) GO TO 999
      IW(KTMTL+LMHCOL)=LTMTLA
      IW(KTMTL+LMHROW)=LROWS(KPTML)
C
C++   Copy information from the POT banks
C
      KPTML=IW(NPTML)
      KPTMA=IW(NPTMA)
      KTGMA=IW(NTGMA)
      IOFF=0
      DO 500 ITK=1,LROWS(KTGMA)
        IW(KROW(KTGMA,ITK)+JTGMNM)=ITABL(KPTMA,ITK,JPTMNM)
        IW(KROW(KTGMA,ITK)+JTGMOM)=IOFF
        IW(KROW(KTGMA,ITK)+JTGMNC)=-1
        IW(KROW(KTGMA,ITK)+JTGMNA)=ITABL(KPTMA,ITK,JPTMNA)
        NA=0
        DO 300 II=1,ITABL(KPTMA,ITK,JPTMNM)
          IPT=IOFF+II
          IW(KROW(KTMTL,IPT)+JTMTMT)=ITABL(KPTML,IPT,JPTMMT)
          IW(KROW(KTMTL,IPT)+JTMTNH)=ITABL(KPTML,IPT,JPTMNH)
          RW(KROW(KTMTL,IPT)+JTMTCS)=9999.
  300   CONTINUE
        IOFF=IOFF+ITABL(KPTMA,ITK,JPTMNM)
  500 CONTINUE
C
C++   Calculate the remaining information
C
      KFRTL=IW(NFRTL)
      IF (KFRTL.NE.0) THEN
        DO 600 ITK=1,LROWS(KTGMA)
          IW(KROW(KTGMA,ITK)+JTGMNC)=ITABL(KFRTL,ITK,JFRTNT)
  600   CONTINUE
      ENDIF
C
      KFRFT=IW(NFRFT)
      KFKIN=IW(NFKIN)
      KFVER=IW(NFVER)
      KPART=IW(NPART)
      IF (KFRTL.NE.0 .AND. KFRFT.NE.0 .AND. KFKIN.NE.0
     &                 .AND. KPART.NE.0 .AND. KFVER.NE.0) THEN
        DO 800 ITK=1,LROWS(KTGMA)
          IDX=-1
          DO 180 I=1,6
            DO 178 J=1,I
              IDX=IDX+1
              IF (I.LE.5) THEN
                A(I,J)= RTABL(KFRFT,ITK,JFRFEM+IDX)
              ENDIF
  178       CONTINUE
  180     CONTINUE
C
          DO 700 II=1,ITABL(KTGMA,ITK,JTGMNM)
            IMT= ITABL(KTGMA,ITK,JTGMOM) + II
            ITMC=ITABL(KTMTL,IMT,JTMTMT)
            ITYP=ITABL(KFKIN,ITMC,JFKIPA)
            Q=RTABL(KPART,ITYP,JPARCH)
            IF (Q.EQ.0.) THEN
              CHISQ=9999.
            ELSE
              P(1)=RTABL(KFKIN,ITMC,JFKIPX)
              P(2)=RTABL(KFKIN,ITMC,JFKIPY)
              P(3)=RTABL(KFKIN,ITMC,JFKIPZ)
              IF (P(1).NE.0. .OR. P(2).NE.0.) THEN
                IVT=ITABL(KFKIN,ITMC,JFKIOV)
                X(1)=RTABL(KFVER,IVT,JFVEVX)
                X(2)=RTABL(KFVER,IVT,JFVEVY)
                X(3)=RTABL(KFVER,IVT,JFVEVZ)
                CALL TNRHPA(P,X,Q,FIELD,HELIX,XC,S)
                CHISQ=0.
                DO 185 K=1,5
                  IF (A(K,K).EQ.0.) THEN
                    CHISQ=9999.
                    GO TO 186
                  ENDIF
                  DIF= HELIX(K)-RTABL(KFRFT,ITK,JFRFIR-1+K)
                  IF (ABS(DIF).LT.1.0E10
     &                         .AND. A(K,K).GT.1.0E-10) THEN
                    CHISQ=CHISQ + (DIF)**2/A(K,K)
                  ELSE
                    CHISQ=9999.
                    GO TO 186
                  ENDIF
  185           CONTINUE
  186           CONTINUE
              ELSE
                CHISQ=9999.
              ENDIF
            ENDIF
            RW(KROW(KTMTL,IMT)+JTMTCS)=CHISQ
  700     CONTINUE
  800   CONTINUE
      ENDIF
C
C++   Get the drop flag if any, then drop POT banks if required;
C     add JUL banks to S-list
C     POT banks are on PLIST, JUL banks on JLIST
C
      PLIST = 'PTMAPTML'
      JLIST = 'TGMATMTL'
#include "jplist.h"
C
  888 CONTINUE
      RETURN
C
  999 CONTINUE
      CALL BDROP(IW,'TGMATMTL')
      RETURN
      END
#endif
