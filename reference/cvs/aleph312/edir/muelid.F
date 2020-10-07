      SUBROUTINE MUELID(NMUON,NELEC)
C----------------------------------------------------------------------
CKEY EDIR DEF CLASS9->10
C! Muons and electrons identification.
C-
C   Input  : None
C   Output : NMUON  = Number of muons (HMAD) with energy >= 3 GeV
C            NELEC  = Number of electrons with energy >= 2 GeV
C-
C   Called by   : SELEVT
C   Calls  : None
C   Input banks : HMAD,EIDT,PEID
C-
C                                        Author: M. Talby September 89
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
C --
#include "bcs.h"
#include "hmadjj.h"
#include "eidtjj.h"
#include "peidjj.h"
#include "bmacro.h"
C --
      NMUON = 0
      NELEC = 0
C --
      KHMAD=IW(NAMIND('HMAD'))
      IF(KHMAD.LE.0) GOTO 888
C --
      NMATRK = LROWS(KHMAD)
C --
C   Loop on charged tracks associated with Hcal
C   and count those with muon flag and energy deposit >= 3 GeV
C --
      DO 10 NMT = 1,NMATRK
        IFMU = ITABL(KHMAD,NMT,JHMAIF)
        IF(IFMU.NE.1) GOTO 10
        IF(RTABL(KHMAD,NMT,JHMAED).LT.3.) GOTO 10
        NMUON = NMUON+1
   10 CONTINUE
C --
  888 CONTINUE
C --
      KPEID=IW(NAMIND('PEID'))
      KEIDT=IW(NAMIND('EIDT'))
      IF(KPEID.GT.0) THEN
        NEATRK = LROWS(KPEID)
      ELSEIF(KEIDT.GT.0) THEN
        NEATRK = LROWS(KEIDT)
      ELSE
        GOTO 999
      ENDIF
C --
C   Loop on charged tracks associated with Ecal and count those
C   with P >= 2.GeV and satisfying the standard R2 and R3 cuts
C --
      DO 20 NET = 1,NEATRK
        R2 = -1000.
        R3 = -1000.
        IF(KPEID.GT.0) THEN
          IR2 = ITABL(KPEID,NET,JPEIR2)
          IR3 = ITABL(KPEID,NET,JPEIR3)
          IF(IR2.EQ.128) R2 = 1000.
          IF(ABS(IR2).EQ.127) R2 = 999.*SIGN(1.,FLOAT(IR2))
          IF(R2.EQ.-1000.) R2 = FLOAT(IR2)/10.
          IF(IR3.EQ.128) R3 = 1000.
          IF(ABS(IR3).EQ.127) R3 = 999.*SIGN(1.,FLOAT(IR3))
          IF(R3.EQ.-1000.) R3 = FLOAT(IR3)/10.
          NT = ITABL(KPEID,NET,JPEIPF)
        ELSE
          R2 = RTABL(KEIDT,NET,JEIDR2)
          R3 = RTABL(KEIDT,NET,JEIDR3)
          NT = ITABL(KEIDT,NET,JEIDFR)
        ENDIF
        IF(R2.LE.-3.5) GOTO 20
        IF(R3.LE.-3.5 .OR. R3.GE.4.) GOTO 20
        CALL QPTRCK(NT,PZ,PTOT)
        IF(PTOT.LT.2.) GOTO 20
        NELEC = NELEC+1
   20 CONTINUE
C --
  999 RETURN
      END
#endif
