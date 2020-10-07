*DK   vdhit
      SUBROUTINE VDHIT
C-----------------------------------------------------------------------
C! Compute hits for VDET
CKEY VDET DIGITIZE
C!
C!
C!  Author         F.Forti        12/6/86
C!  Modified       G.Triggiani    02/02/88
C!                 F.Ranjard      30/05/90
C!                 A. Bonissent   15/02/94
C!                                Suppress usage of work bank
C!                 G.Taylor       1/08/95
C!                                use GEANT to treat landau
C!                                fluctuations inside the VDET
C!
C!  Description
C!  ===========
C!  When control is given to this routine, data from the
C!  common block TRKCOM are use to accumulate hit information in
C!  VDSS bank
C-----------------------------------------------------------------------
#ifndef DOC
C
      SAVE NAVDSS
      SAVE ENER
C
      PARAMETER (NVDSS = 1200)
C
#include "jobcom.h"
#include "bcs.h"
#include "vdssjj.h"
#include "trkcom.h"
#include "iocom.h"
      INTEGER VIFACI
      EXTERNAL CHAINT
      CHARACTER*4 CHAINT
      LOGICAL FIRST
      DATA FIRST /.TRUE./
#include "bmacro.h"
      IF(FIRST)THEN
        FIRST=.FALSE.
        NAVDSS = NAMIND('VDSS')
      ENDIF
C
C? If it is a new event drop VDSS and create new one
C
      IF (FBEGJO(1)) THEN
        FBEGJO(1) = .FALSE.
C           Create bank
        NDATA = LMHLEN + LVDSSA*NVDSS
        CALL ALBOS('VDSS',0,NDATA,KVDSS,IGARB)
        IW(KVDSS+LMHCOL) = LVDSSA
        IW(KVDSS+LMHROW) = 0
C add bank to T list so that it is removed between events
        CALL BLIST(IW,'T+','VDSS')
        IF (FDEBJO.AND.ICVDJO(6).NE.0) WRITE(LOUTIO,8000)
 8000   FORMAT(/' +++VDHIT +++ New event. VDSS bank created')
      ENDIF
C
C? Process track element.
C
C        Check for first,intermediate and last steps inside volume.
C
C? If enter volume for the first time
C           Check that there is enough space in bank
C           If not increase bank size
C
      KVDSS = IW(NAVDSS)
      IF (ITRKEL(8).EQ.1) THEN
        IF (LFRROW(KVDSS).LT.1) THEN
          NDATA = IW(KVDSS) + NVDSS*LVDSSA
          CALL ALBOS('VDSS',0,NDATA,KVDSS,IGARB)
        ENDIF
C
        ENER = 0.
C
C           Calculate layer number, phi coord, zed coord.
C
        ISLOT = ITRKEL(10)
        IBID = VIFACI(ISLOT,NLAY,NPHI)
C
C              Store ENTRY point
C
        KLINE = KNEXT(KVDSS)
        IW(KLINE + JVDSTN) = ITRKEL(1)
C
C this particle is a delta ray or other non primary track
C store its track number as 0 to indicate this
C
        IF(ITRKEL(2).NE.0) IW(KLINE + JVDSTN) = 0
        IW(KLINE + JVDSLN) = NLAY
        IW(KLINE + JVDSPN) = NPHI
        IW(KLINE + JVDSRN) = 0
        IW(KLINE + JVDSES) = 0
        RW(KLINE + JVDSXE) = TRKELE(1)
        RW(KLINE + JVDSYE) = TRKELE(2)
        RW(KLINE + JVDSZE) = TRKELE(3)
        IF(FDEBJO.AND.ICVDJO(6).NE.0) WRITE(LOUTIO,810) LROWS(KVDSS)+1
  810   FORMAT(' +++VDHIT +++ First step. Hit number= ',I5 )
C
C? IF intermediate step accumulate energy release
C
      ELSEIF (ITRKEL(8).EQ.0 .AND. ITRKEL(9).EQ.0) THEN
        ENER = ENER + TRKELE(12)
        IF (FDEBJO.AND.ICVDJO(6).NE.0) WRITE(LOUTIO,8100) ENER
 8100   FORMAT(' +++VDHIT +++ Intermediate step. ENERGY= ',G8.3)
C
C? IF enter volume for the last time store the exit point
C
      ELSEIF (ITRKEL(8).EQ.2 .OR. ITRKEL(9).GT.0) THEN
        KLINE = KNEXT(KVDSS)
        RW(KLINE + JVDSXL) = TRKNXT(1)
        RW(KLINE + JVDSYL) = TRKNXT(2)
        RW(KLINE + JVDSZL) = TRKNXT(3)
        RW(KLINE + JVDSER) = ENER + TRKELE(12)
C
C if the track ends in this volume set the end segment flag
C
        IF(ITRKEL(9).GT.0) IW(KLINE + JVDSES) = 1
        IF (RW(KLINE+JVDSER).GT.0.) IW(KVDSS+LMHROW) = LROWS(KVDSS) + 1
        IF(FDEBJO.AND.ICVDJO(6).NE.0)
     &    WRITE(LOUTIO,8120) RW(KVDSS+JVDSER)
 8120   FORMAT(' +++VDHIT +++ Last step in volume. ENER= ',G8.3)
      ENDIF
      RETURN
      END

#endif
