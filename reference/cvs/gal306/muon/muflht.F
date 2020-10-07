*DK muflht
      SUBROUTINE MUFLHT (JPLAN,IS,NS)
C
C*****************************************************************************
C
C T.Wang -860108
C
C           - modified for final dbase
C             by A. Antonelli, F. Bossi 1 July 89
C
C       Routine to fill a induced strip signal into BOS bank 'MUHT'
C
C       Input:
C         JPLAN -- the tube plane where the signal is ( 1 or 2 );
C         IS    -- the index of X- or Y- strip;
C                1 -- X
C                2 -- Y
C         NS    -- the strip number in the current strip plane;
C
C       Output:
C         Elements in BOS bank 'MUHT'
C         RETURN 1 if not enough space to enlarge MUHT
C
C       Called by MUSGNL
C       Calls ALBOS               in this .HLB
C
C*************************************************************************
C
#ifndef DOC
      SAVE
#include "muhtjj.h"
#include "musgcn.h"
#include "munamc.h"
#include "bcs.h"
#include "jobcom.h"
#include "iocom.h"
#include "trkcom.h"
#include "bmacro.h"
C
      IF(IS.EQ.1) NSTCU = NXSTMU
      IF(IS.EQ.2) NSTCU = NYSTMU
      IF(NS.GT.NSTCU) RETURN
      IF(NS.LE.0) RETURN
C
C       Get index of BOS bank 'MUHT'
C
      JMUHT = IW(NAMUHT)
      IF (JMUHT.EQ.0) RETURN
C
      IF (LFRROW(JMUHT) .LE. 0) THEN
         ND = IW(JMUHT) + IW(JMUHT)/2
         CALL ALBOS ('MUHT',0,ND,JMUHT,IGARB)
      ENDIF
C
      KMUHT = KNEXT(JMUHT)
      IW(KMUHT + JMUHTN) = ITRKEL(1)
C
C       In BOS bank 'MUHT', index of strip plane from 1-4
C         Strip # 1 -- X1
C                 2 -- Y1
C                 3 -- Y2
C                 4 -- X2
C
      IW(KMUHT + JMUHSP) = (JPLAN-1)*(5-2*IS) + IS
      IW(KMUHT + JMUHEL) = NUEMMU
      IW(KMUHT + JMUHSA) = NS - 1
C
      IW(JMUHT + LMHROW) = LROWS(JMUHT) + 1
C
      END
#endif
