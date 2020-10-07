*DK vgwcns
      SUBROUTINE VGWCNS(IMOD,IWAF,VDEPL,VMU,VTE,VDTHL,VFPOS,VDAPP)
C----------------------------------------------------------------------
C! Get wafer constants from DAF
CKEY VDET DBASE
C!
C!  Author         A. Bonissent 15-Jan-1994
C!
C!  Description
C!  ===========
C! Get Vdet constants for one wafer
C! Input : IMOD module number
C!         IWAF wafer number
C! Output : VDEPL Depletion voltage
C!          VMU   Electron or holes mobility
C!          VTE   Electron or holes temperature
C!          VDTHL Electron or holes mobility in silicon
C!          VFPOS Face position in local coordinates
C!          VDAPP Applied voltage
C
C-----------------------------------------------------------------------
C
#ifndef DOC
C
      DIMENSION VMU(*),VTE(*),VDTHL(*),VFPOS(2)
#include "vdepjj.h"
#include "bcs.h"
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C
      SAVE NAVDEP
C
#include "bmacro.h"
      IF (FIRST) THEN
         FIRST = .FALSE.
         NAVDEP = NAMIND('VDEP')
      ENDIF
      KVDEP=IW(NAVDEP)
      VMU(1) = RTABL(KVDEP,1,JVDEEM)
      VMU(2) = RTABL(KVDEP,1,JVDEHM)
      VTE(1) = RTABL(KVDEP,1,JVDEET)
      VTE(2) = RTABL(KVDEP,1,JVDEHT)
      VDTHL(1) = RTABL(KVDEP,1,JVDEHE) * ALFIEL(DUMM) *1.E-5
      VDTHL(2) = RTABL(KVDEP,1,JVDEHH) * ALFIEL(DUMM) *1.E-5
      VFPOS(1) = -VWTHCK()*0.5
      VFPOS(2) = +VWTHCK()*0.5
      VDEPL = RTABL(KVDEP,1,JVDEDV)
      VDAPP = RTABL(KVDEP,1,JVDEAV)
      RETURN
      END
#endif
