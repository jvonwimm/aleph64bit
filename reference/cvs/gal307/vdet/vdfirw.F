*DK vdfirw
      SUBROUTINE VDFIRW(IFIRS,ILAST,IMOD,IWAF)
C!----------------------------------------------------------------------
C! Compute signal in fired strips for one wafer
CKEY VDET DIGITIZE
C!
C!  Author         A. Bonissent 15-Jan-1994
C!
C!  Description
C!  ===========
C!  For each row in VDLH bank,
C!        build the list of all track segments (routine VDMKTE)
C!        Then, call VDPRTE to process them.
C!        Then, go to next hit
C!
C! Input :  IFIRS : first row in bank VDLH for this wafer             I
C!          ILAST : last  row in bank VDLH for this wafer             I
C!          IMOD  : module number                                     I
C!          IWAF  : Wafer number                                      I
C!
C! Input :  VDLH bank
C!
C! Output : VWS1, VWS2 banks, created by VDPRTE
C!
C-----------------------------------------------------------------------
C
#ifndef DOC
      SAVE NAVDLH,NAVWSX
C
      DIMENSION NAVWSX(2),MAXS(2)
#include "vws1jj.h"
#include "bcs.h"
      LOGICAL FIRST
      DATA FIRST /.TRUE./
#include "bmacro.h"
      IF(FIRST)THEN
        FIRST=.FALSE.
        NAVDLH=NAMIND('VDLH')
        NAVWSX(1)=NAMIND('VWS1')
        NAVWSX(2)=NAMIND('VWS2')
      ENDIF
      KVDLH=IW(NAVDLH)
C
C Reset strips array
C
      DO 30 IV=1,2
      KBNK = IW(NAVWSX(IV))
      NDATA = IW(KBNK)-LMHLEN
      CALL VZERO(IW(KBNK+LMHLEN+1),NDATA)
 30   CONTINUE
C
C Loop over track elements for this wafer
C
      DO 10 IVDLH=IFIRS,ILAST
        NHITW = ILAST-IFIRS+1
C
C build list of small segments for this track element
C
        CALL VDMKTE(IVDLH,IVDHT)
C
C compute signal on strips caused by the track element
C
        CALL VDPRTE(IMOD,IWAF,IVDHT,NHITW)
 10   CONTINUE
      RETURN
      END
#endif
