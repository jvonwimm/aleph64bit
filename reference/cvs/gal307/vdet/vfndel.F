*DK vfndel
      SUBROUTINE VFNDEL (IMOD,NBCLN,NVDCR,VDSLP,VDPAL,VDELC,
     $                   MXCSI,MXCNO,VELGV,VDLCO,IOFSET,NBITSH)
C----------------------------------------------------------------------
C! Get electronic parameters for one module
CKEY VDET DBASE
C!
C!
C!  Author         A. Bonissent 15-Jan-1994
C!  modif :        A. Bonissent 09-Oct-1996: access properly 
C!                                           // noise in VDEP bank
C!
C!
C!  Description
C!  ===========
C! Input : IMOD module number
C! Output : NBCLN  number of noisy strips added to a cluster
C           NVDCR  number of strips contributing to correlated noise
C           VDSLP  noise slope in electron/pf
C           VDPAL  parallel noise in electrons
C           VDELC  electronic input capacitance in pf
C           VELGV  Gev equivalent to an electron
C           VDLCO  conversion factor from electron to counts
C           NBITSH Offset added when converting digitizings into integers
C           MXCNO  number of strips contributing to correlated noise
C           MXCSI  number of strips contributing to signal
C-----------------------------------------------------------------------
C
#ifndef DOC
      SAVE NAVDEP,NAVPHO,IVSTP
C
      DIMENSION NBCLN(2),NVDCR(2),VDSLP(2),VDPAL(2),VDELC(2)
      DIMENSION MXCNO(2),MXCSI(2),VELGV(2),VDLCO(2),IOFSET(2),NBITSH(2)
#include "vdepjj.h"
#include "bcs.h"
      LOGICAL FIRST
      DATA FIRST /.TRUE./
#include "bmacro.h"
      IF (FIRST) THEN
        FIRST = .FALSE.
        NAVDEP = NAMIND('VDEP')
        NAVPHO = NAMIND('VPHO')
      ENDIF
      KVPHO = IW(NAVPHO)
      IOFSET(1)=1024
      IF (KVPHO.NE.0) IOFSET(1) = ITABL(KVPHO,1,1)
      IOFSET(2)=IOFSET(1)
      KVDEP=IW(NAVDEP)
C         number of noisy strips added to a cluster
      NBCLN(1) = ITABL(KVDEP,1,JVDENL)
      NBCLN(2) = ITABL(KVDEP,1,JVDENL)
C         number of strips contributing to correlated noise
      NVDCR(1) = ITABL(KVDEP,1,JVDENC)
      NVDCR(2) = ITABL(KVDEP,1,JVDENC)
C         noise slope in electron/pf
      VDSLP(1) = RTABL(KVDEP,1,JVDENE)
      VDSLP(2) = RTABL(KVDEP,1,JVDENE)
C         parallel noise in electrons
      VDPAL(1) = RTABL(KVDEP,1,JVDEPN)
      VDPAL(2) = RTABL(KVDEP,1,JVDEPN+1)
C         electronic input capacitance in pf
      VDELC(1) = RTABL(KVDEP,1,JVDEIC)
      VDELC(2) = RTABL(KVDEP,1,JVDEIC)
C         Gev equivalent to an electron
      VELGV(1) = RTABL(KVDEP,1,JVDEEG)
      VELGV(2) = VELGV(1)
C         conversion factor from electron to counts
      VDLCO(1) = RTABL(KVDEP,1,JVDEEC)
      VDLCO(2) = RTABL(KVDEP,1,JVDEEC+1)
C
      NBITSH(1) = ITABL(KVDEP,1,JVDEMF)
      NBITSH(2) = NBITSH(1)
C         number of strips contributing to correlated noise
      MXCNO(1) = ITABL(KVDEP,1,JVDENC)
      MXCNO(2) = ITABL(KVDEP,1,JVDENC)
C
C         number of strips contributing to signal
      MXCSI(1) = ITABL(KVDEP,1,JVDESN)
      MXCSI(2) = ITABL(KVDEP,1,JVDESN+1)
      RETURN
      END
#endif
