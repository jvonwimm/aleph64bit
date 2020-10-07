      SUBROUTINE LCLUST
C----------------------------------------------------------------
C!   Clustering in Lcal
C!
C!   Author   : P. H. Hansen  870401
C!   Modified : P. H. Hansen  880315  890102 910801
C!
C!   Input : 'LSDA'
C!   Ouput : 'LCLU','LMAP'
C!
C!   Description:
C!   ===========
C!     A cluster consists of towers (=3 storeys).
C!     A tower can join a cluster if it contains at least one
C!     storey with energy larger than low threshold and sharing a common
C!     side, edge or corner with another such storey in the cluster.
C!     A cluster must include at least one storey above high threshold.
C!     Finally the clusters below a cluster energy threshold
C!     are suppressed.
C!     No clustering across module boundaries.
C?
C?     Create a tower map LMAP:
C?        IW(KLMAP + LMHLEN + tower_address) = LSDA row number
C?
C?     DO for all storey_data
C?        IF the tower is not already used
C?        THEN apply high threshold condition on tower
C?             Initialise attributes of a cluster
C?             Fill storey_data,cluster relations
C?             DO for all neighbour towers
C?                IF the tower may join the cluster
C?                THEN update cluster attributes
C?                     Fill storey_data,cluster relations
C?                     Start a new loop over neighbours
C?                ENDIF
C?             ENDDO
C?             Resume parent loop, if not finished
C?        ENDIF
C?     ENDDO
C?     Create LCLU
C?     Apply an energy cut on each cluster
C?     Rewrite storey_data,cluster relation
C?     Normalise cluster attributes
C?     Copy cluster attributes into LCLU
C?     Shrink LCLU to actual size
C?
C!======================================================================
#ifndef DOC
#include "lwbank.h"
#include "lmapjj.h"
      PARAMETER(LDIMC = 4, LMAXC = 20, LMAXL = 50)
C LDIMC : Dimension of vector (Storey1,2,3,Sum)
C LMAXC : Maximum number of clusters
C LMAXL : Maximum depth of recursive loop over adjacent towers
C
      DIMENSION MASK(3),IMSK(LMAXL),NXTN(LMAXL),LCUR(LMAXL)
      DIMENSION ENER(LDIMC,LMAXC)
      DIMENSION XCL(LDIMC,LMAXC),YCL(LDIMC,LMAXC)
      DIMENSION DXCL(LDIMC,LMAXC),DYCL(LDIMC,LMAXC)
      DIMENSION E(LDIMC),XYZ(3),ZST(4)
      DIMENSION NTOWC(LMAXC),ITOWC(LMAXC),SH(LMAXC)
      LOGICAL FIRST
C
#include "alcons.h"
#include "lcnamc.h"
#include "lsdajj.h"
#include "lclujj.h"
#include "lcrejj.h"
#include "rparac.h"
#include "bcs.h"
      DATA MASK/1,2,4/
      DATA FIRST/.TRUE./
#include "bmacro.h"
#include "lcmacr.h"
C-----------------------------------------------------------------------
C
C Constants
         KLCRE = IW(NALCRE)
         ZST(1) = RTABL(KLCRE,1,JLCRZ1)
         ZST(2) = RTABL(KLCRE,1,JLCRZ2)
         ZST(3) = RTABL(KLCRE,1,JLCRZ3)
         ZST(4) = RTABL(KLCRE,1,JLCRZC)
         ECHI   = RTABL(KLCRE,1,JLCRES)
         ECLO   = RTABL(KLCRE,1,JLCRET)
         ECCL   = RTABL(KLCRE,1,JLCREC)
         FIRST = .FALSE.
C
C Initialise cluster attributes
      NCLUS = 0
      CALL VZERO(NTOWC(1),LMAXC)
      CALL VZERO(ITOWC(1),LMAXC)
      CALL VZERO(ENER(1,1),LDIMC*LMAXC)
      CALL VZERO(XCL(1,1),LDIMC*LMAXC)
      CALL VZERO(YCL(1,1),LDIMC*LMAXC)
      CALL VZERO(DXCL(1,1),LDIMC*LMAXC)
      CALL VZERO(DYCL(1,1),LDIMC*LMAXC)
      CALL VZERO(SH(1),LMAXC)
      KLMAPW = 0
C
C Storey data
      KLSDA = IW(NALSDA)
      IF(KLSDA.LE.0)                               GOTO 999
      NTOW = LROWS(KLSDA)
      IF(NTOW.LE.0)                                GOTO 999
C
C Create LMAP
      LEN = LMHLEN + LLMAP
      CALL AUBOS('LMAP',0,LEN,KLMAP,IGARB)
      IF(IGARB.EQ.2)                               GOTO 997
      IW(KLMAP+1) = 1
      IW(KLMAP+2) = LLMAP
C put on T list
      CALL BLIST(IW,'T+','LMAP')
C restore KLSDA
      KLSDA = IW(NALSDA)
C fill LMAP
      DO 100 ITOW = 1,NTOW
        IADDR = ITABL(KLSDA,ITOW,JLSDAD)
        IW(KLMAP+LMHLEN+IADDR) = ITOW
  100 CONTINUE
C
C Loop over Storey Data
      DO 400 ITOW = 1,NTOW
         KLL  = KROW(KLSDA,ITOW)
         LMAP = IW(KLL+JLSDAD)
C Tower already used ?
         IF(IW(KLL+JLSDLC).NE.0)                   GOTO 400
C Mask the storeys exceeding threshold
         ILOW=0
         IHIG=0
         DO 10 IST=1,3
            E(IST) = RW(KLL+JLSDEN+IST-1)
            IF(E(IST).GT.ECLO) ILOW=ILOW+MASK(IST)
            IF(E(IST).GT.ECHI) IHIG=IHIG+MASK(IST)
   10    CONTINUE
C Apply high threshold condition
         IF(IHIG.EQ.0)                             GOTO 400
C Energy and position of tower
         E(4) = E(1)+E(2)+E(3)
         CALL LCPAXY(LMAP,XYZ)
C
C Initialise a cluster
         NCLUS=NCLUS+1
         CALL UCOPY(E(1),ENER(1,NCLUS),LDIMC)
         CALL VSCALE(E(1),XYZ(1),XCL(1,NCLUS),LDIMC)
         CALL VSCALE(E(1),XYZ(2),YCL(1,NCLUS),LDIMC)
         DO 11 I=1,LDIMC
            DXCL(I,NCLUS) = E(I)*XYZ(1)*XYZ(1)
            DYCL(I,NCLUS) = E(I)*XYZ(2)*XYZ(2)
   11    CONTINUE
         SH(NCLUS)   = E(4)*XYZ(1)*XYZ(2)
C Fill tower to cluster relation
         IW(KLL+JLSDLC)=NCLUS
         NTOWC(NCLUS) = NTOWC(NCLUS) + 1
         ITOWC(NCLUS) = ITOW
         LASTW = ITOW
C
C Start recursive algorithm
         LEVEL = 1
C next neighbour for current tower
         NXTN(LEVEL) = 1
C Storey mask of current tower
         IMSK(LEVEL) = ILOW
C Address of current tower
         LCUR(LEVEL) = LMAP
  200    CONTINUE
C
C Check if neighbour list is exhausted at this level
         IF(NXTN(LEVEL).GT.8)                      GOTO 310
         J1=NXTN(LEVEL)
C Loop over all neighbour towers
         DO 300 IADJ=J1,8
            JADJ = JLCRNE+IADJ-1
            LMAP = LCUR(LEVEL) + ITABL(KLCRE,1,JADJ)
            IF(LMAP.GT.LLMAP.OR.LMAP.LE.0)         GOTO 300
C Is this tower in the Storey Data?
            ITADJ = IW(KLMAP+LMHLEN+LMAP)
            IF(ITADJ.EQ.0)                         GOTO 300
C Is it already used?
            KADJ = KROW(KLSDA,ITADJ)
            IF(IW(KADJ+JLSDLC).NE.0)               GOTO 300
C Apply low energy threshold
            ILOW=0
            DO 40 IST=1,3
               E(IST) = RW(KADJ+JLSDEN+IST-1)
               IF(E(IST).GT.ECLO) ILOW=ILOW+MASK(IST)
   40       CONTINUE
            IF(ILOW.EQ.0)                          GOTO 300
C Two storeys over threshold must be connected
            IF(IMSK(LEVEL).EQ.1.AND.ILOW.EQ.4)     GOTO 300
            IF(IMSK(LEVEL).EQ.4.AND.ILOW.EQ.1)     GOTO 300
C Energy and position
            E(4) = E(1)+E(2)+E(3)
            CALL LCPAXY(LMAP,XYZ)
C Update cluster attributes
            DO 50 I=1,LDIMC
               ENER(I,NCLUS) = ENER(I,NCLUS) + E(I)
               XCL(I,NCLUS)  = XCL(I,NCLUS)  + E(I)*XYZ(1)
               YCL(I,NCLUS)  = YCL(I,NCLUS)  + E(I)*XYZ(2)
               DXCL(I,NCLUS) = DXCL(I,NCLUS) + E(I)*XYZ(1)*XYZ(1)
               DYCL(I,NCLUS) = DYCL(I,NCLUS) + E(I)*XYZ(2)*XYZ(2)
   50       CONTINUE
            SH(NCLUS) = SH(NCLUS) + E(4)*XYZ(1)*XYZ(2)
C Fill tower to cluster relation
            IW(KADJ+JLSDLC)=NCLUS
            NTOWC(NCLUS) = NTOWC(NCLUS) + 1
            KLAS = KROW(KLSDA,LASTW)
            IW(KLAS+JLSDLS)=ITADJ
C Remember current Storey Data row
            LASTW = ITADJ
C Remember next neighbour
            NXTN(LEVEL) = IADJ+1
C Start a new loop around the current tower
            IF(LEVEL.LT.LMAXL) THEN
               LEVEL = LEVEL+1
               NXTN(LEVEL) = 1
               IMSK(LEVEL) = ILOW
               LCUR(LEVEL) = LMAP
               GOTO 200
            ENDIF
  300    CONTINUE
C If any loop was not finished, then resume it.
  310    IF(LEVEL.NE.1) THEN
            LEVEL=LEVEL-1
            GOTO 200
         ENDIF
         IF(NCLUS.GE.LMAXC)                        GOTO 410
  400 CONTINUE
  410 CONTINUE
C
C Clustering fini.
      IF (NCLUS .EQ. 0)                            GOTO 999
C
C Make room for the cluster bank
      LEN = NCLUS*LLCLUA + LMHLEN
      CALL AUBOS('LCLU',0,LEN,KLCLU,IGARB)
      IF(IGARB .EQ. 2)                             GOTO 998
      IF(IGARB .NE. 0) KLSDA = IW(NALSDA)
      IW(KLCLU + LMHCOL) = LLCLUA
      CALL BLIST(IW,'T+','LCLU')
C
C For each cluster
      NCL = 0
      DO 520 JCL = 1,NCLUS
C
C If the cluster is below threshold
        IF(ENER(4,JCL).LT.ECCL) THEN
C
C Reset cluster pointer and next_storey in LSDA
           ITOW = ITOWC(JCL)
           DO 501 IT=1,NTOWC(JCL)
              KLSD = KROW(KLSDA,ITOW)
              IW(KLSD+JLSDLC) = 0
              ITOW = IW(KLSD+JLSDLS)
              IW(KLSD+JLSDLS) = 0
  501      CONTINUE
           GOTO 520
        ENDIF
C
C Else reorder cluster pointer and next_storey in LSDA
        NCL = NCL+1
        ITOW = ITOWC(JCL)
        DO 502 IT=1,NTOWC(JCL)
           KLSD = KROW(KLSDA,ITOW)
           IW(KLSD+JLSDLC) = NCL
           ITOW = IW(KLSD+JLSDLS)
  502   CONTINUE
C
C Normalise attributes
        DO 500 K=1,LDIMC
           IF(ENER(K,JCL).GT.ECLO) THEN
              EINV = 1./ENER(K,JCL)
              XCL(K,JCL) = EINV*XCL(K,JCL)
              YCL(K,JCL) = EINV*YCL(K,JCL)
              DXCL(K,JCL) = EINV*DXCL(K,JCL) - XCL(K,JCL)**2
              DYCL(K,JCL) = EINV*DYCL(K,JCL) - YCL(K,JCL)**2
              DXCL(K,JCL) = AMAX1(DXCL(K,JCL),0.01)
              DYCL(K,JCL) = AMAX1(DYCL(K,JCL),0.01)
           ENDIF
  500   CONTINUE
        SH(JCL)   = SH(JCL)/ENER(4,JCL) - XCL(4,JCL)*YCL(4,JCL)
C
C Find module number
        IADDR = ITABL(KLSDA,ITOWC(JCL),JLSDAD)
        MODU  = LCMOD(IADDR)
        SIGNZ = -1.
        IF(MODU.GE.3) SIGNZ = 1.
C
C Fill LCLU data
C
      KCLU = KROW(KLCLU,NCL)
      DO 510 K=1,LDIMC
        IF(ENER(K,JCL).LE.ECLO)  GOTO 510
        KK = K-1
        Z  = ZST(K)*SIGNZ
        RW(KCLU+JLCLE1+KK) = ENER(K,JCL)
        RW(KCLU+JLCLR1+KK) = SQRT(XCL(K,JCL)**2+YCL(K,JCL)**2+Z**2)
        RW(KCLU+JLCLT1+KK) = ACOS(Z/RW(KCLU+JLCLR1+KK))
        RW(KCLU+JLCLF1+KK) = ATAN2(YCL(K,JCL),XCL(K,JCL))
        RW(KCLU+JLCLW1+KK) = SQRT(DXCL(K,JCL)+DYCL(K,JCL))
        IF(RW(KCLU+JLCLF1+KK).LT.0.)
     &     RW(KCLU+JLCLF1+KK) = RW(KCLU+JLCLF1+KK) + TWOPI
  510 CONTINUE
C
        RW(KCLU+JLCLXX) = SQRT(DXCL(4,JCL))
        RW(KCLU+JLCLYY) = SQRT(DYCL(4,JCL))
        RW(KCLU+JLCLXY) = SH(JCL)/SQRT(DXCL(4,JCL)*DYCL(4,JCL))
        IF(RW(KCLU+JLCLXY).LT.-1.) RW(KCLU+JLCLXY) = -1.
        IF(RW(KCLU+JLCLXY).GT.1.) RW(KCLU+JLCLXY) = 1.
        IW(KCLU+JLCLMO) = MODU
        IW(KCLU+JLCLNT) = NTOWC(JCL)
        IW(KCLU+JLCLLS) = ITOWC(JCL)
  520 CONTINUE
C
C Shrink LCLU to real size
      IW(KLCLU + LMHROW) = NCL
      LEN = NCL*LLCLUA + LMHLEN
      CALL AUBOS('LCLU',0,LEN,KLCLU,IGARB)
      IF(IGARB .EQ. 2)                              GOTO 998
C
      GOTO 999
C
C - Error handling
  997 CALL REPORT('LCLUST','no room for LMAP',1)
      GOTO 999
  998 CALL REPORT('LCLUST','no room for LCLU',1)
C
  999 CONTINUE
C
      END
#endif
