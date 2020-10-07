      SUBROUTINE TBPRNT
C
C-----------------------------------------------------------------------
C! Print TPC sector banks
C!
C!     Author:    D. Schlatter
C!     Modified:  R. Johnson     6-05-87
C!
C!     Called by TPCREC
C!
C!     Description
C!     ===========
C!     This routine will dump all TPC raw and reconstructed data
C!     at the sector level (pulses and clusters).
C!     By providing the data card TSECtor, the user can limit the
C!     printout to only specified sectors.  The TPC dump level must be
C!     greater than 4 in order for this routine to be called
C!     automatically by TPCREC.
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "rparac.h"
#include "tparac.h"
#include "tpgpar.h"
#include "rlunit.h"
#include "bcs.h"
#include "rcurnt.h"
#include "tpgeom.h"
C
      IUSAV=IW(6)
      IW(6)=LDEBRL
      KTSEC=IW(NAMIND('TSEC'))
      CALL BPRNT(IW,'EVEH')
      IF (KTSEC.NE.0) THEN
        ILST=IW(KTSEC)
      ELSE
        ILST=NTSECT
      ENDIF
      DO 400 MAL=1,ILST
        IF (KTSEC.NE.0) THEN
          ISECT=IW(KTSEC+MAL)
        ELSE
          ISECT=MAL
        ENDIF
        IND=NPRNT('TPAD',ISECT)
        IND=NPRNT('TPDI',ISECT)
        IND=NPRNT('TPRL',ISECT)
        CALL TPULDP(ISECT,LDEBRL)
        CALL TCLUDP(ISECT,LDEBRL)
        IND=NPRNT('TWIR',ISECT)
        IND=NPRNT('TWDI',ISECT)
        IND=NPRNT('TWPU',ISECT)
        CALL TSPUDP(ISECT,LDEBRL)
        CALL TSCLDP(ISECT,LDEBRL)
  400 CONTINUE
      IW(6)=IUSAV
      RETURN
      END
#endif
