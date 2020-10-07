*DK vfndth
      SUBROUTINE VFNDTH(IMOD,THRES)
C----------------------------------------------------------------------
C!  Get clustering thresholds for one module
CKEY VDET DBASE
C!
C!  Author         A. Bonissent 15-Jan-1994
C!
C!  ===========
C! Input : IMOD module number
C! Output : THRES thresholds in 2 views
C-----------------------------------------------------------------------
C
#ifndef DOC
      SAVE VELGV,VDLCO,IVDTH
C
      DIMENSION THRES(*)
      INTEGER GTSTUP
      DIMENSION NBCLN(2),NVDCR(2),VDSLP(2),VDPAL(2),VDELC(2)
      DIMENSION MXCNO(2),MXCSI(2),VELGV(2),VDLCO(2),IOFSET(2),NBITSH(2)
      DIMENSION IVDTH(2)
#include "bcs.h"
#include "vdepjj.h"
      LOGICAL FIRST
      DATA FIRST /.TRUE./
#include "bmacro.h"
      IF(FIRST)THEN
        FIRST=.FALSE.
        CALL VFNDEL
     $     (IMOD,NBCLN,NVDCR,VDSLP,VDPAL,VDELC,
     $      MXCSI,MXCNO,VELGV,VDLCO,IOFSET,NBITSH)
        LDBAS = JUNIDB(0)
        KVDEP = MDARD (IW,LDBAS,'VDEL',0)
        KVDEP=IW(NAMIND('VDEP'))
C         acquisition threshold in counts
        IVDTH(1) = ITABL(KVDEP,1,JVDEST)
        IVDTH(2) = ITABL(KVDEP,1,JVDEST+1)
      ENDIF
      THRES(1) = VELGV(1) * VDLCO(1) * IVDTH(1)
      THRES(2) = VELGV(2) * VDLCO(2) * IVDTH(2)
      RETURN
      END
#endif
