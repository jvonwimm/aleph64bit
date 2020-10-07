      SUBROUTINE HFLNOI(NSPN0)
C*************************************************************
C! Steering routine for noise flagging (analog readout)      *
C!                                                           *
C! Author  : R. Tenchini 040190                              *
C!                                                           *
C! Version 1.2                                               *
C!                                                           *
C?
C!======================================================================
#ifndef DOC
#include "hnoicd.h"
#include "hclujj.h"
#include "hsdajj.h"
#include "hsphjj.h"
#include "hpcojj.h"
#include "rflags.h"
#include "rparac.h"
#include "bcs.h"
      INTEGER HNOISY
      LOGICAL HPHNOI,NOITO,TROVAT
      PARAMETER (ELIM1=0.0,ELIM2=3.0)
#include "bmacro.h"
C
C Initializations
C
      NNOISE = 0
      NODIGT = 0
      CALL HNOISE
      KHCLU=NLINK('HCLU',0)
      IF(KHCLU.EQ.0) RETURN
      NHCLU=LROWS(KHCLU)
      IF(NHCLU.EQ.0) RETURN
      KHSDA=NLINK('HSDA',0)
      IF(KHSDA.EQ.0) RETURN
      KHPCO=NLINK('HPCO',0)
      IF(KHPCO.EQ.0) RETURN
      ETOHC=0.
      DO 100 I=1,NHCLU
        IW(KROW(KHCLU,I)+JHCLNF)=0
        NOITO=HPHNOI(I,.5)
        IF(NOITO) IW(KROW(KHCLU,I)+JHCLNF)=64
        TROVAT=.FALSE.
        DO 20 K=1,NSPN0
           IF(TROVAT) GO TO 20
           IHSPH=NLINK('HSPH',K)
           NELEM=LROWS(IHSPH)
           NPATT=0
           PATENE=0.
           DO 10 J=1,NELEM
              IDCOD=IW(KROW(IHSPH,J)+JHSPHR)
              ICHEK=IDCOD/10000
              ICLUS=MOD(IDCOD,10000)
              IF(ICHEK.EQ.3) THEN
                 IF(ICLUS.EQ.I) TROVAT=.TRUE.
              ELSEIF(ICHEK.EQ.1) THEN
                 NPATT=NPATT+1
                 PATENE=PATENE+RTABL(KHPCO,ICLUS,JHPCDE)
              ENDIF
  10       CONTINUE
  20    CONTINUE
        IF(.NOT.TROVAT) THEN
           NODIGT = NODIGT + 1
           IW(KROW(KHCLU,I)+JHCLNF)=IOR(IW(KROW(KHCLU,I)+JHCLNF),32)
        ELSE
           CLUENE=RTABL(KHCLU,I,JHCLEC)
           RCLUPA=CLUENE/PATENE
           IF(RCLUPA.GT.ELIM1.AND.RCLUPA.LT.ELIM2) GO TO 102
        ENDIF
        NSTOC=0
        IFSTO=ITABL(KHCLU,I,JHCLFS)
        INXST=IFSTO
 101    ITHE =ITABL(KHSDA,INXST,JHSDTI)
        IPHI =ITABL(KHSDA,INXST,JHSDPI)
        ITEST=HNOISY(ITHE,IPHI)
        IW(KROW(KHCLU,I)+JHCLNF)=IOR(IW(KROW(KHCLU,I)+JHCLNF),ITEST)
        INXST=ITABL(KHSDA,INXST,JHSDNS)
        NSTOC=NSTOC+1
        IF(INXST.NE.0) GO TO 101
        ETOHC=ETOHC+RTABL(KHCLU,I,JHCLEC)
        FSTOC=NSTOC
        IF(ITABL(KHCLU,I,JHCLNF).EQ.0) THEN
           IF(JHISRF(JULHC).GE.1) CALL HFILL(7015,FSTOC,0.,1.)
        ENDIF
 102    CONTINUE
        IF(JHISRF(JULHC).GE.1) THEN
           FLAG=IW(KROW(KHCLU,I)+JHCLNF)
           CALL HFILL(7010,FLAG,0.,1.)
        ENDIF
 100  CONTINUE
      IF(NNOISE.EQ.0) THEN
         IF(JHISRF(JULHC).GE.1) CALL HFILL(7013,ETOHC,0.,1.)
      ENDIF
      RETURN
      END
#endif
