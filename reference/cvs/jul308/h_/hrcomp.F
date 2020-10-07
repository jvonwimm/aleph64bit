      SUBROUTINE HRCOMP
C*************************************************************
C! Compact the HPCO bank                                     *
C!                                                           *
C! Author  : G. BAGLIESI  880415                             *
C!                                                           *
C! VERSION 1.0                                               *
C!                                                           *
C! Input   HPCO BANK                                         *
C! Output  HPCO compacted bank                               *
C!                                                           *
C!                                                           *
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "hwbnkw.h"
#include "rlunit.h"
#include "hdpcut.h"
#include "alcons.h"
#include "hpcojj.h"
#include "htubjj.h"
#include "htpajj.h"
      LOGICAL FIRST
      DATA FIRST/.TRUE./
#include "bmacro.h"
      IHTUB=NLINK('HTUB',0)
      IF(IHTUB.EQ.0) THEN
         GOTO 100
      ENDIF
      NHTUB=LROWS(IHTUB)
C
C We set to 0 the pattern pointer in HTUB
C
      DO 107 IJ=1,NHTUB
        IW(KROW(IHTUB,IJ) + JHTUPP) = 0
 107  CONTINUE
      IHPCO=NLINK('HPCO',0)
      IF(IHPCO.EQ.0) THEN
         GOTO 100
      ENDIF
      NPATT = LROWS(IHPCO)
      IF(NPATT.EQ.0) GOTO 100
      IF(FIRST) THEN
         FIRST=.FALSE.
         IPCOHW=0
         ITE1HW=0
         ITE2HW=0
         NCUT  = NSACU1
         NCUT0 = NSACU2
      ENDIF
      CALL WBANK(IW,IPCOHW,IW(IHPCO),*110)
      IW(IPCOHW + LMHCOL) = IW(IHPCO + LMHCOL)
      IW(IPCOHW + LMHROW) = 0
      CALL WBANK(IW,ITE1HW,NPATT,*110)
      CALL WBANK(IW,ITE2HW,NPATT,*110)
C
C  TWO WORKING BANKS :    ITE1HW (NHIT <= 10,IPL>1), ITE2HW (NHIT>10)
C
         NWTE1=0
         NWTE2=0
         DO 10 I=1,NPATT
           NHIT = ITABL(IHPCO,I,JHPCND)
           JHTPA = ITABL(IHPCO,I,JHPCHT)
           IHTPA=NLINK('HTPA',JHTPA)
           IF(IHTPA.EQ.0) THEN
              CALL RERROR('HRCOMP',1,' BANK HTPA NOT FOUND')
              GOTO 10
           ENDIF
           NHTPA = LROWS(IHTPA)
C
C We put the pointer in HTUB = to HTPA pointer for the moment
C
           DO 11 KK=1,NHTPA
              JTUB=ITABL(IHTPA,KK,JHTPLR)
              IW(KROW(IHTUB,JTUB) + JHTUPP) = JHTPA
  11       CONTINUE
C
C We test the number of hits in the current pattern
C
           IF(NHIT.LE.NCUT) THEN
             NWTE1 = NWTE1 + 1
             IW(ITE1HW+NWTE1) = I
           ELSE IF(NHIT.GT.NCUT) THEN
             NWTE2 = NWTE2 + 1
             IW(ITE2HW+NWTE2) = I
         ENDIF
   10 CONTINUE
C
      DO 30 II=1,NWTE1
         IK = IW(ITE1HW+II)
         IDIR1 = ITABL(IHPCO,IK,JHPCDI)
         EDIG1 = RTABL(IHPCO,IK,JHPCDE)
         COR11 = RTABL(IHPCO,IK,JHPCC1)
         COR21 = RTABL(IHPCO,IK,JHPCC2)
         NHIT1 = ITABL(IHPCO,IK,JHPCND)
         IPL1 = ITABL(IHPCO,IK,JHPCFP)
         LPL1 = ITABL(IHPCO,IK,JHPCLP)
         CENX1 = RTABL(IHPCO,IK,JHPCCX)
         CENY1 = RTABL(IHPCO,IK,JHPCCY)
         JHTP1 = ITABL(IHPCO,IK,JHPCHT)
         DMINX   = 50000.
         JCUR = IK
         IF(IDIR1.EQ.-1) GOTO 30
         DO 20 JJ=1,NWTE2
            JK = IW(ITE2HW+JJ)
            IDIR2 = ITABL(IHPCO,JK,JHPCDI)
            EDIG2 = RTABL(IHPCO,JK,JHPCDE)
            COR12 = RTABL(IHPCO,JK,JHPCC1)
            COR22 = RTABL(IHPCO,JK,JHPCC2)
            NHIT2 = ITABL(IHPCO,JK,JHPCND)
            IPL2 = ITABL(IHPCO,JK,JHPCFP)
            LPL2 = ITABL(IHPCO,JK,JHPCLP)
            CENX2 = RTABL(IHPCO,JK,JHPCCX)
            CENY2 = RTABL(IHPCO,JK,JHPCCY)
            JHTP2 = ITABL(IHPCO,JK,JHPCHT)
            IF(IDIR2.EQ.-1) GOTO 20
            IF(IDIR1.NE.IDIR2.OR.(COR11*COR12).LT.0.) THEN
               DIST = 100000.
            ELSE IF(IDIR1.EQ.1) THEN
               XX1=COR11*COS(COR21)
               YY1=COR11*SIN(COR21)
               XX2=COR12*COS(COR22)
               YY2=COR12*SIN(COR22)
               DIST = SQRT((XX1-XX2)** 2+ (YY1-YY2)**2)
            ELSE IF(IDIR1.GT.1) THEN
               DIST = SQRT((CENX1-CENX2)** 2+ (CENY1-CENY2)**2)
            ENDIF
            IF(DIST.LT.DMINX) THEN
               JCUR = JK
               DMINX = DIST
            ENDIF
   20    CONTINUE
         IF(JCUR.NE.IK.AND.DMINX.LT.DMI0HD) THEN
C
C  We update the pointer in HTUB for the compressed pattern
C
            JHTP2 = ITABL(IHPCO,JCUR,JHPCHT)
            DO 33 IPA=1,NHTUB
               IF(IW(KROW(IHTUB,IPA)+JHTUPP).EQ.JHTP1)
     +            IW(KROW(IHTUB,IPA)+JHTUPP) = JHTP2
   33       CONTINUE
            KRCUR = KROW(IHPCO,JCUR)
            EDIG2 = RTABL(IHPCO,JCUR,JHPCDE) + EDIG1
            RW(KRCUR+JHPCDE) = EDIG2
            NHIT2 = ITABL(IHPCO,JCUR,JHPCND) + NHIT1
            IW(KRCUR+JHPCND) = NHIT2
            IW(ITE1HW+II) = -IW(ITE1HW+II)
          ELSE IF (NHIT1.LE.NCUT0) THEN
C
C  We update the pointer in HTUB for the cancelled  pattern
C
            DO 34 IPA=1,NHTUB
               IF(IW(KROW(IHTUB,IPA)+JHTUPP).EQ.JHTP1)
     +            IW(KROW(IHTUB,IPA)+JHTUPP) = 0
   34       CONTINUE
            IW(ITE1HW+II) = -IW(ITE1HW+II)
         ENDIF
   30 CONTINUE
C
      DO 50 III=1,NWTE2
         JCUR0 = IW(ITE2HW + III)
         DO 40 II0=1,LCOLS(IHPCO)
            IW(KNEXT(IPCOHW)+II0) = ITABL(IHPCO,JCUR0,II0)
   40    CONTINUE
         IW(IPCOHW+LMHROW) = IW(IPCOHW+LMHROW) + 1
   50 CONTINUE
C
      DO 70 III=1,NWTE1
         JCUR0 = IW(ITE1HW + III)
         IF(JCUR0.GT.0) THEN
            DO 60 II0=1,LCOLS(IHPCO)
               IW(KNEXT(IPCOHW)+II0) = ITABL(IHPCO,JCUR0,II0)
   60       CONTINUE
            IW(IPCOHW+LMHROW) = IW(IPCOHW+LMHROW) + 1
         ENDIF
   70 CONTINUE
C
C
C  IPCOHW is reduced to is proper size and then copied to HPCO
C
            CALL WBANK(IW,IPCOHW,KNEXT(IPCOHW)-IPCOHW,*110)
            CALL BKFRW(IW,'HPCO',0,IW,IPCOHW,*110)
C
C  Now it's time to redefine the HTUB pointer to HPCO
C  equal to the HPCO row number
C
            IHPCO=NLINK('HPCO',0)
            NHPCO=LROWS(IHPCO)
            DO 104 JM=1,NHPCO
               JHTP0=ITABL(IHPCO,JM,JHPCHT)
               DO 105 JMM=1,NHTUB
                  IF(IW(KROW(IHTUB,JMM) + JHTUPP).EQ.JHTP0)
     +               IW(KROW(IHTUB,JMM) + JHTUPP) = JM*10000
  105          CONTINUE
  104       CONTINUE
            DO 106 JMN=1,NHTUB
               IW(KROW(IHTUB,JMN) + JHTUPP) =
     +         IW(KROW(IHTUB,JMN) + JHTUPP)/10000
  106       CONTINUE
C
C  We drop the work banks
C
            CALL WDROP(IW,IPCOHW)
            CALL WDROP(IW,ITE1HW)
            CALL WDROP(IW,ITE2HW)
  100       RETURN
  110       CALL RERROR('HRCOMP',2,'INSUFF. SPACE TO CREATE WBANK')
            CALL WDROP(IW,IPCOHW)
            CALL WDROP(IW,ITE1HW)
            CALL WDROP(IW,ITE2HW)
      RETURN
      END
#endif
