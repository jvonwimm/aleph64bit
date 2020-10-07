      SUBROUTINE HPRSUM
C----------------------------------------------------------------------
C!Print run statistics for HCAL
C!  G.Capon     25-aug-87
C?
C!======================================================================
#ifndef DOC
#include "hcjosu.h"
#include "rlunit.h"
#include "rstatr.h"
      CHARACTER*1 BAR(80)
      DATA BAR/80*'-'/
C--------------------------------------------------------------------
      WRITE(LOUTRL,'(''0'',80A)') BAR
C
      FNEVT=NRUNRS(4)
      IF (FNEVT.EQ.0) RETURN
      AVNSF=FLOAT(NSTOHC(1))/FNEVT
      AVNSB=FLOAT(NSTOHC(2))/FNEVT
      AVNCL=FLOAT(NCLUHC)/FNEVT
      AVNTU=FLOAT(NTUBHC)/FNEVT
      AVEHC=(ETOTHC(1)+ETOTHC(2))/FNEVT
      AVPAT=FLOAT(NPATHC)/FNEVT
      AVENF = 0.
      IF (NSTOHC(1).NE.0) AVENF=ETOTHC(1)/FLOAT(NSTOHC(1))
      AVENB = 0.
      IF (NSTOHC(2).NE.0) AVENB=ETOTHC(2)/FLOAT(NSTOHC(2))
      AVECL = 0.
      IF (NCLUHC.NE.0) AVECL=(ETOTHC(1)+ETOTHC(2))/FLOAT(NCLUHC)
      AVEHT = 0.
      IF (NPATHC.NE.0) AVEHT=FLOAT(NHITHC)/FLOAT(NPATHC)
C
      WRITE(LOUTRL,77)AVEHC,AVNSF,AVENF,AVNSB,AVENB,AVNCL,AVECL,AVNTU,
     &                AVPAT,AVEHT
 77   FORMAT(' Hcal Job Summary :',/,
     +' Energy in Hcal/event....................',F8.1,' Gev',/,
     +' Front storeys/event.....................',F8.1,/,
     +' Energy/front-storey.....................',F8.1,' Gev',/,
     +' Back storeys/event......................',F8.1,/,
     +' Energy/back-storey......................',F8.1,' Gev',/,
     +' Clusters/event..........................',F8.1,/,
     +' Energy/cluster..........................',F8.1,' Gev',/,
     +' Digital readout : wire clusters/event...',F8.1,/,
     +' Digital patterns/event..................',F8.1,/,
     +' Fired tubes/Digital pattern.............',F8.1,/)
      RETURN
      END
#endif
