      SUBROUTINE VMKR90
C----------------------------------------------------------------------
C!  - PREPARE VMRE banks used in hit reconstruction
C!    For Vdet version '90
C!
C
C   A. Bonissent, June 1995
C
C?
C!======================================================================
#ifndef DOC
C      IMPLICIT NONE
C
      SAVE NAVMRE
C
#include "vmrejj.h"
#include "vregjj.h"
#include "bcs.h"
      LOGICAL FIRST
      DATA FIRST /.TRUE./
#include "bmacro.h"
      IF(FIRST)THEN
         FIRST=.FALSE.
         NAVMRE = NAMIND('VMRE')
      ENDIF
C
C  Find the bank
C
      KVMRE = IW(NAVMRE)
      IF( KVMRE .EQ. 0)THEN
        CALL RERROR('VMKR90',1,'Cant find database banks')
        GOTO 999
      END IF
C
C  Loop over the big bank and break it down into
C  separate banks for each wafer.
C
      NVMRE = LROWS(KVMRE)
      IF(NVMRE.NE.0)THEN
C
C  Decode the wafer address of the first region
C
        IADDR = ITABL(KVMRE,1,JVREFC)
        CALL VUNADD(IADDR,NCHAN,ILAY,IWFF,IFAC,IVIEW,ICHAN)
        CALL VAENWA(IROMD,ILAY,IWFF,IFAC,IVIEW)
      ENDIF
C Say that we start a new module
      NEWMD=1
      DO IVMRE=1,NVMRE
C
C  If we start a new module, make the bank - maximum possible size
C
        IF(NEWMD.EQ.1)THEN
          KVREG = NBANK('VREG',IROMD,LMHLEN+NVMRE*LVREGA)
          IF(KVREG .EQ. 0)THEN
            CALL RERROR('VMKR90',2,'Out of BOS space')
            GOTO 999
          END IF
          IW(KVREG+LMHCOL) = LVREGA
          IW(KVREG+LMHROW) = 0
          NEWMD=0
        END IF
C
C  Pack in the new region, unpacking the channel numbers where needed
C
        IW(KNEXT(KVREG)+JVREFC) = ICHAN+1
        IADDR = ITABL(KVMRE,IVMRE,JVMRLC)
        CALL VUNADD(IADDR,NCHAN,ILAY,IWFF,IFAC,IVIEW,ICHAN)
        IW(KNEXT(KVREG)+JVRELC) = ICHAN+1
        IW(KNEXT(KVREG)+JVRECS) = ITABL(KVMRE,IVMRE,JVMRCS)
        IW(KNEXT(KVREG)+JVRECS+1) = 0
        IW(KNEXT(KVREG)+JVRECS+2) = 0
        IW(KNEXT(KVREG)+JVRERF) = ITABL(KVMRE,IVMRE,JVMRRF)
        IW(KVREG+LMHROW) = LROWS(KVREG) + 1
C
C  Decode the wafer address of next region
C
        IADDR = ITABL(KVMRE,IVMRE+1,JVMRFC)
        CALL VUNADD(IADDR,NCHAN,ILAY,IWFF,IFAC,IVIEW,ICHAN)
        CALL VAENWA(IRONX,ILAY,IWFF,IFAC,IVIEW)
C
C  If new module or last row, remake bank to size
C
        IF(IRONX.NE.IROMD.OR.IVMRE.EQ.NVMRE)THEN
          NVREG = LROWS(KVREG)
          KVREG = NBANK('VREG',IROMD,LMHLEN+NVREG*LVREGA)
          IROMD=IRONX
          NEWMD=1
        END IF
      END DO
  999 CONTINUE
      RETURN
      END
#endif
