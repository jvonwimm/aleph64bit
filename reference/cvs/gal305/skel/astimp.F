*DK astimp
      SUBROUTINE ASTIMP
C---------------------------------------------------------------------
C      O.Callot    11-jun-86
C                                    modified by F.Ranjard - 861216
C! Fill 'IMPA' banks
C   Store in bank IMPA, NR=Geant-track-number, the track's parameters
C   at the detector entrance, as defined by AGDIMP
C
C.  Called by GUSTEP                                   this .HLB
C.  Calls     BLIST,NLINK                              BOS77
C.            ALBOS                                    this .HLB
C---------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "agcons.h"
#include "gcvolu.h"
#include "gctrak.h"
#include "gckine.h"
#include "jqcom.h"
#include "jobcom.h"
      PARAMETER (LIMPA=8, LBKIM=5)
      LOGICAL FIMPA(LIMVOL),FECAL
      DATA IECAL /4HECAL/
      FECAL(ICAL) = LAGIMP(3,ICAL) .EQ. IECAL
#include "bmacro.h"
C ---------------------------------------------------------------
C
      DO 10 IM=1,NAGIMP
        IF(NGLEVE.LT.LAGIMP(2,IM)) GO TO 10
        IF(NGAMES(LAGIMP(2,IM)).EQ.LAGIMP(1,IM)) GO TO 20
   10 CONTINUE
      RETURN
C
   20 CONTINUE
      IF (FECAL(IM) .AND. GCHARG.NE.0.) RETURN
C
      KIMPA = NLINK('IMPA',IGTRA)
      IF(KIMPA.EQ.0) THEN
        LON = LMHLEN + LBKIM * LIMPA
         CALL ALBOS ('IMPA',IGTRA,LON,KIMPA,IGARB)
        IW(KIMPA+1) = LIMPA
        IW(KIMPA+2) = 0
        DO 30 M=1,LIMVOL
 30     FIMPA(M)=.FALSE.
      ELSE
        IF (LFRWRD(KIMPA) .LT. LIMPA) THEN
            CALL ALBOS ('IMPA',IGTRA,IW(KIMPA)+LIMPA,KIMPA,IGARB)
        ENDIF
      ENDIF
C
C - IF the impact is already stored for this detector RETURN
      IF (FIMPA(IM)) GOTO 100
      FIMPA(IM) = .TRUE.
C
      KPT = KNEXT (KIMPA)
      IW(KPT+1) = LAGIMP(3,IM)
      CALL UCOPY (GVECT(1),RW(KPT+2),6)
      RW(KPT+8) = GETOT
      IW(KIMPA+2) = IW(KIMPA+2) + 1
  100 CONTINUE
      RETURN
C
      END
#endif
