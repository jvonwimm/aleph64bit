*DK tpibos
      SUBROUTINE TPIBOS
C -------------------------------------------------
C - F.RANJARD - 860414
C - called at the 1st entry into TPC of an event
C - drop banks if they exist ( read from disk)
C - Called from   TPHIT                    from this .HLB
C - Calls         NDROP                    from BOS77.hlb
C ---------------------------------------------------------
#ifndef DOC
      SAVE
#include "jqcom.h"
#include "jobcom.h"
#include "tpnamc.h"
C ---------------------------------------------------------
C - track element bank
      IF (IW(NATPTE).NE.0) IDRP = NDROP('TPTE',0)
C
C - padrow level 1 (ICTPJO(2)=1)
      IF (ICTPJO(2).EQ.1) THEN
         IF (IW(NATPHT).NE.0) IDRP = NDROP('TPHT',0)
         IF (IW(NATPHE).NE.0) IDRP = NDROP('TPHE',0)
      ENDIF
C - trigger pad level 1 (ICTPJO(3)=1)
      IF (ICTPJO(3).EQ.1) THEN
         IF (IW(NATTHT).NE.0) IDRP = NDROP('TTHT',0)
         IF (IW(NATTHE).NE.0) IDRP = NDROP('TTHE',0)
      ENDIF
C
      IF (IW(NATPCO).NE.0) IDRP = NDROP('TPCO',0)
      IF (IW(NATPCH).NE.0) IDRP = NDROP('TPCH',0)
      IF (IW(NATCRL).NE.0) IDRP = NDROP('TCRL',0)
C
      RETURN
      END
#endif
