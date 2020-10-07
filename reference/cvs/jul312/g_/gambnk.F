      SUBROUTINE GAMBNK
C----------------------------------------------------------------------
C!  - Build EGPC bank (Result of GAMPEC)
C!    M.N. Minard 17/3/92 Built EGPR bank (list of storey of gampec row)
C!
C!   Author   :- E. Lancon              6-NOV-1991
C!               MN Minard        mod  10-FEB-1992
C!
C?
C!======================================================================
#ifndef DOC
#include "egpcjj.h"
#include "egrpjj.h"
#include "egprjj.h"
#include "pecojj.h"
#include "rlunit.h"
#include "bcs.h"
#include "gastin.h"
      PARAMETER ( EMIN0 = 0.25 , NFOMAX = 20 , NGAMVE = 20 )
      DIMENSION GAMVE(NGAMVE,NFOMAX)
      DATA IFIRST / 0 /
#include "bmacro.h"
C
C-  Set name index
C
      IF ( IFIRST.EQ.0) THEN
        NAPEST = NAMIND('PEST')
        NAPECO = NAMIND('PECO')
        IFIRST = 1
        NAEGPC = NAMIND('EGPC')
        NAEGPR = NAMIND('EGPR')
        NAEGRP = NAMIND('EGRP')
        IF ( IW(NAEGRP).EQ.0) THEN
          KEGRP = MDARD(IW,LRCONS,'EGRP',0)
        ENDIF
        IF ( KEGRP.EQ.0) THEN
          EMIN = EMIN0
        ELSE
          EMIN = RTABL(KEGRP,1,JEGRCU)
        ENDIF
      ENDIF
C
C-    Built EGAM
C
      KPECO = IW(NAPECO)
      NPECO = 0
      IF ( KPECO.NE.0 ) NPECO = LROWS(KPECO)
      NGAMX = 3*NPECO
C
C-    Built EGPC bank
C
      KEGPC = NDROP('EGPC',0)
      NLENG = NGAMX*LEGPCA+LMHLEN
      CALL AUBOS('EGPC',0,NLENG,KEGPC,IGARB)
      IF (IGARB.EQ.2) GO TO 997
      IW(KEGPC+LMHCOL) = LEGPCA
      IW(KEGPC+LMHROW) = 0
      IF ( IGARB.NE.0) KPECO = IW(NAPECO)
C
C-    Built EGPR bank
C
      KEGPR = NDROP('EGPR',0)
      KPEST = IW(NAPEST)
      NPEST = 0
      IF (KPEST.NE.0) NPEST = LROWS(KPEST)
      NLENG = NPEST*LEGPRA + LMHLEN
      CALL AUBOS('EGPR',0,NLENG,KEGPR,IGARB)
      IF(IGARB.EQ.2) GO TO 996
      IF ( IGARB.NE.0) THEN
         KPECO = IW(NAPECO)
         KEGPC = IW(NAEGPC)
         KPEST = IW(NAPEST)
      ENDIF
      IW(KEGPR+LMHROW) = 0
      IW(KEGPR+LMHCOL) = LEGPRA
C
C-    Now loop on PECO and fill EGPC
C
      INRES = 0
      NGAM = 0
      DO IPECO=1,NPECO
        IF( RTABL(KPECO,IPECO,JPECEC).GT.EMIN ) THEN
          CALL GAMPEK (IPECO,EMIN,NGAMVE*NFOMAX,NNGA,GAMVE,IRTG)
          IF ( IRTG.GE.0.AND.NNGA.GT.0) THEN
            IF ( NGAM+NNGA.GT.NGAMX) THEN
              NGAMX = NGAMX+NNGA + 10
              NLENG = NGAMX*LEGPCA+LMHLEN
              CALL AUBOS('EGPC',0,NLENG,KEGPC,IGARB)
              IF ( IGARB.EQ.2) GO TO 997
              IF ( IGARB.NE.0) THEN
                KPECO = IW(NAPECO)
                KEGPR = IW(NAEGPR)
                KPEST = IW(NAPEST)
              ENDIF
            ENDIF
            DO IGAM = 1,NNGA
              JEGPC = KROW(KEGPC,IGAM+NGAM)
              RW(JEGPC+JEGPPX) = GAMVE(1,IGAM)
              RW(JEGPC+JEGPPY) = GAMVE(2,IGAM)
              RW(JEGPC+JEGPPZ) = GAMVE(3,IGAM)
              RW(JEGPC+JEGPR1) = GAMVE(9,IGAM)
              RW(JEGPC+JEGPR2) = GAMVE(10,IGAM)
              RW(JEGPC+JEGPF4) = GAMVE(6,IGAM)
              RW(JEGPC+JEGPDM) = GAMVE(16,IGAM)
              IW(JEGPC+JEGPST) = INT(GAMVE(8,IGAM))
              IW(JEGPC+JEGPQU) = INT(GAMVE(7,IGAM))+INT(GAMVE(11,IGAM))
     &          *10
              IW(JEGPC+JEGPPE) = IPECO
              NSTEG = NNSTGA(IGAM)
              DO ISTEG = 1,NSTEG
                JEGPR = KROW(KEGPR,ISTEG+INRES)
                IW(JEGPR+JEGPGR) = IGAM+NGAM
                IW(JEGPR+JEGPPR) = LGASTO(IGAM,ISTEG)
              ENDDO
              INRES = INRES + NSTEG
              IW(KEGPR+LMHROW) = IW(KEGPR+LMHROW)+NSTEG
            ENDDO
            NGAM = NGAM + NNGA
            IW(KEGPC+LMHROW) = IW(KEGPC+LMHROW)+NNGA
          ENDIF
        ENDIF
      ENDDO
      CALL AUBPRS('EGPC')
      CALL AUBPRS('EGPR')
      GOTO 999
C
  997 IER = 1
      CALL RERROR('GAMBNK',IER,' not enough space to book EGPC')
      GOTO 999
C
  996 IER = 2
      CALL RERROR('GAMBNK',IER,' not enough space to book EGPR')
C
  999 RETURN
      END
#endif
