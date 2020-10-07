*DK mugetc
      SUBROUTINE MUGETC
C***********************************************************************
C
C T.Wang -851216
C
C             modified for DAF and new bank layout
C             by F.Bossi,D.Kuhn,R.Xu 87-09-19
C           - modified for final dbase
C             by A. Antonelli, F. Bossi 1 July 89
C
C! get the constants needed by the process "Create MU signals"
C        from BOS bank 'M*RD' and to fill them in common
C       block /MUGNCN/ and /MUSGCN/.
C       Common block /MUGNCN/ keeps the general information of muon
C       detector and is filled only once, common block /MUSGCN/
C       keeps the information of the volume in which the current
C       track element is.
C
C       Called by MUHIT
C       Calls     MUNSTA              in this .HLB
C
C***********************************************************************
#ifndef DOC
      SAVE
#include "bcs.h"
#include "munamc.h"
#include "trkcom.h"
#include "mugncn.h"
#include "musgcn.h"
#include "mug1pr.h"
#include "gcvolu.h"
#include "mjjpar.h"
      EXTERNAL MUNSTA
      CHARACTER*4 CHAHOL
#include "bmacro.h"
#include "muiwna.h"
C
C       Now, the constants dependent on the volume
C       The function MUNSTA finds the address in BOS bank by means
C       of the slot number etc.
C
      NSLOT = ITRKEL(10)
      TMUCVO = TRKVOL
      TMU3VO = CHAHOL (NGAMES(3))
      NUMV = MUNSTA(NSLOT,TMUCVO,TMU3VO)
      IF( TMUCVO(1:3).EQ.'MUB' )THEN
C                     Barrel
         DO 7 IJ=1,34
         NM=ITABL(JMBSG,IJ,JMBSVO)
         IF(IJ.GT.17)NM=NM+12
         IF(NM.NE.NUMV) GO TO 7
         IDXMD = ITABL(JMBSG,IJ,JMBSK2)
         XSOFMU(1) = RTABL(JMBSG,IJ,JMBST1)
         XSOFMU(2) = RTABL(JMBSG,IJ,JMBST1+1)
         IJ1=IJ
         IF(IJ.GT.17)IJ1=IJ-17
         IF(IJ1.GE.2.AND.IJ1.LE.7) THEN
              XSOFMU(1) = RTABL(JMBSG,IJ,JMBST1+1)
              XSOFMU(2) = RTABL(JMBSG,IJ,JMBST1)
         ENDIF
         R2 = RTABL(JMBSG,IJ,JMBSRC)
         NUEMMU = ITABL(JMBSG,IJ,JMBSID)
         GO TO 8
 7       CONTINUE
 8       WDMDMU = 0.5*RTABL(JMBTG,IDXMD,JMBTRB)
         XLMDMU = 0.5*RTABL(JMBTG,IDXMD,JMBTZB)
         YSOFMU(1) = RTABL(JMBAG,1,JMBAY1)
         YSOFMU(2) = RTABL(JMBAG,1,JMBAY2)
         DL = RTABL(JMBAG,1,JMBAPD)
         IF(NUMV.LE.12) THEN
            R1 = RTABL(JMBAG,1,JMBAR1)
         ELSE
            R1 = RTABL(JMBAG,1,JMBAR2)
         END IF
         IF(NSLOT.GE.7.OR.NSLOT.EQ.1) THEN
              ZPSWMU(2) = R2-R1
              ZPSWMU(1) = R2-R1+DL
         ELSE
              ZPSWMU(1) = R2-R1
              ZPSWMU(2) = R2-R1+DL
         ENDIF
         YCUTSB(1) = 0.
         YCUTSB(2) = 0.
         YCUTSL(1) = 0.
         YCUTSL(2) = 0.
         NYSTMU = ITABL(JMBTG,IDXMD,JMBTY1)
         NXSTMU = ITABL(JMBTG,IDXMD,JMBTNX)
         WD16MU = RTABL(JMBTG,IDXMD,JMBTW1)
         XLEIMU = FLOAT(NYSTMU)*PTYSMU
         NMEIMU = NXSTMU/8
         GO TO 99
C
      ELSE IF( TMUCVO(1:3).EQ.'MUM'  )THEN
C                   Middle Angle
         IDXMD = ITABL(JMMSG,NUMV,JMMSK2)
         WDMDMU = 0.5*RTABL(JMMTG,IDXMD,JMMTZB)
         XLMDMU = 0.5*RTABL(JMMTG,IDXMD,JMMTRB)
         THCKM = RTABL(JMMAG,1,JMMATB)
         Z0PLAN = RTABL(JMMAG,1,JMMAZ0)
         IF(NSLOT.EQ.10) THEN
            Z0PLAN = RTABL(JMMAG,1,JMMAZ1)
            THCKM = RTABL(JMMAG,1,JMMATH)
         ENDIF
         DL = RTABL(JMMAG,1,JMMAPD)
         IF(NSLOT.GE.2.AND.NSLOT.LE.6) THEN
             ZPSWMU(1) = Z0PLAN - 0.5*THCKM
             ZPSWMU(2) = ZPSWMU(1) + DL
         ELSE
             ZPSWMU(1) = -Z0PLAN + 0.5*THCKM
             ZPSWMU(2) = ZPSWMU(1) - DL
         ENDIF
         DEAZ = RTABL(JMMAG,1,JMMADS)
         NYSTMU = ITABL(JMMSG,NUMV,JMMSNY)
         TL = RTABL(JMMSG,NUMV,JMMSTL)
         TR = RTABL(JMMSG,NUMV,JMMSRL)
C                B side
      IF(NUMV.GE.20) THEN
          YSOFMU(1) = RTABL(JMMSG,NUMV,JMMSL1)
          YSOFMU(2) = RTABL(JMMSG,NUMV,JMMSL2)
          YCUTSB(1) = DEAZ + TL - YSOFMU(1)
          YCUTSB(2) = DEAZ + TL - YSOFMU(2)
          YCUTSL(1) = FLOAT(NYSTMU) *PTYSMU - DEAZ + TR
     +                + RTABL(JMMSG,NUMV,JMMSR1)
          YCUTSL(2) = FLOAT(NYSTMU) *PTYSMU - DEAZ + TR
     +                + RTABL(JMMSG,NUMV,JMMSR2)
          IF(NSLOT.GE.2.AND.NSLOT.LE.6) THEN
             YSOFMU(1) = RTABL(JMMSG,NUMV,JMMSR1)
             YSOFMU(2) = RTABL(JMMSG,NUMV,JMMSR2)
             YCUTSB(1) = DEAZ + TR - YSOFMU(1)
             YCUTSB(2) = DEAZ + TR - YSOFMU(2)
             YCUTSL(1) = FLOAT(NYSTMU) *PTYSMU - DEAZ + TL
     +                   + RTABL(JMMSG,NUMV,JMMSL1)
             YCUTSL(2) = FLOAT(NYSTMU) *PTYSMU - DEAZ + TL
     +                   + RTABL(JMMSG,NUMV,JMMSL2)
          ENDIF
      ELSE
C                 A side
          YSOFMU(1) = RTABL(JMMSG,NUMV,JMMSR1)
          YSOFMU(2) = RTABL(JMMSG,NUMV,JMMSR2)
          YCUTSB(1) = DEAZ + TR - YSOFMU(1)
          YCUTSB(2) = DEAZ + TR - YSOFMU(2)
          YCUTSL(1) = FLOAT(NYSTMU) *PTYSMU - DEAZ + TL
     +                + RTABL(JMMSG,NUMV,JMMSL1)
          YCUTSL(2) = FLOAT(NYSTMU) *PTYSMU - DEAZ + TL
     +                + RTABL(JMMSG,NUMV,JMMSL2)
          IF(NSLOT.GE.2.AND.NSLOT.LE.6) THEN
             YSOFMU(1) = RTABL(JMMSG,NUMV,JMMSL1)
             YSOFMU(2) = RTABL(JMMSG,NUMV,JMMSL2)
             YCUTSB(1) = DEAZ + TL - YSOFMU(1)
             YCUTSB(2) = DEAZ + TL - YSOFMU(2)
             YCUTSL(1) = FLOAT(NYSTMU) *PTYSMU - DEAZ + TR
     +                   + RTABL(JMMSG,NUMV,JMMSR1)
             YCUTSL(2) = FLOAT(NYSTMU) *PTYSMU - DEAZ + TR
     +                   + RTABL(JMMSG,NUMV,JMMSR2)
           ENDIF
      ENDIF
      XSOFMU(1) = RTABL(JMMSG,NUMV,JMMSX1)
      XSOFMU(2) = RTABL(JMMSG,NUMV,JMMSX2)
      WD16MU = RTABL(JMMAG,1,JMMAPI)
      NXSTMU = ITABL(JMMTG,IDXMD,JMMTNX)
      NMEIMU = NXSTMU/8
      NUEMMU = NUMV +200
C
      ELSE IF( TMUCVO(1:3).EQ.'MUC' )THEN
C                   End Caps
         IDXMD = ITABL(JMESG,NUMV,JMESK2)
         XLMDMU = 0.5*RTABL(JMETG,IDXMD,JMETXB)
         WDMDMU = 0.5*RTABL(JMETG,IDXMD,JMETYB)
         NXSTMU = ITABL(JMETG,IDXMD,JMETNX)
         NYSTMU = ITABL(JMETG,IDXMD,JMETNY)
         NMEIMU = NXSTMU/8
         HTCK = 0.5*RTABL(JMECG,1,JMECTH)
         R1 = RTABL(JMECG,1,JMECZI)
         IF(NUMV.GT.4.AND.NUMV.LT.9)  R1 = RTABL(JMECG,1,JMECZE)
         IF(NUMV.GT.12) R1 = RTABL(JMECG,1,JMECZE)
         DL = RTABL(JMECG,1,JMECPD)
         ZPSWMU(1) = R1 - HTCK
         ZPSWMU(2) = R1 - HTCK + DL
         XSOFMU(1) = RTABL(JMETG,IDXMD,JMETX1)
         XSOFMU(2) = RTABL(JMETG,IDXMD,JMETX2)
         WD16MU = RTABL(JMETG,IDXMD,JMETPI)
         IMDM = MOD(NUMV,2)
      IF(NUMV.LT.9.AND.IMDM.NE.0.OR.NUMV.GE.9.AND.IMDM.EQ.0) THEN
        XSOFMU(1) = 2.*WDMDMU-(FLOAT(NXSTMU)/16.)*WD16MU-XSOFMU(1)
        XSOFMU(2) = 2.*WDMDMU-(FLOAT(NXSTMU)/16.)*WD16MU-XSOFMU(2)
      ENDIF
         YSOFMU(1) = RTABL(JMETG,IDXMD,JMETYS)
         YSOFMU(2) = RTABL(JMETG,IDXMD,JMETYS)
         DEAZ = RTABL(JMECG,1,JMECDZ)
         X0 = RTABL(JMECG,1,JMECXO)
         YCUTSB(1) = DEAZ + X0 - YSOFMU(1)
         YCUTSB(2) = DEAZ + X0 - YSOFMU(2)
         YCUTSL(1) = X0 - DEAZ - YSOFMU(1)
         YCUTSL(2) = X0 - DEAZ - YSOFMU(2)
         NUEMMU = NUMV + 100
      ENDIF
C
 99   CONTINUE
C
      RETURN
      END
#endif
