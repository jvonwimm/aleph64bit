*DK asevst
      SUBROUTINE ASEVST
C ---------------------------------------------------------------------
C - F.Ranjard - 851021
C! Build the event headers
C - Add this block to list 'E'.
C - Called by    ASCEVE                                from this .HLB
C - Calls        NBANK, BLIST, BKFMT, BPRNT, NAMIND    from BOS77.hlb
C                ALBOS                                 from this .HLB
C ----------------------------------------------------
#ifndef DOC
      SAVE
#include "jqcom.h"
#include "iocom.h"
#include "jobcom.h"
#include "kincom.h"
#include "ajjpar.h"
      EXTERNAL NAMIND, NBANK
      INTEGER ALEVEH,ALKEVH
C ----------------------------------------------------------------------
C
C - Build the 'EVEH' if not there
      IF (IW(NAEVEH) .EQ. 0) THEN
         ISTAT = 1
         IF (KERRJO.NE.0) ISTAT = LOFFMC + KERRJO
         JEVEH = ALEVEH (NEVTJO,IEXPJO,IRUNJO,ECMSKI,IPROKI,ISTAT)
         IF (JEVEH.EQ.0) THEN
            CALL ALTELL ('ASEVST: not enough space to book EVEH ',1,
     &                   'NEXT')
         ENDIF
      ENDIF
C
C - Build  the kine event header KEVH if not there
      IF (IPROJO(1) .NE. 0) THEN
         JKEVH = ALKEVH (IRNDJO(1,1),NITRKI,NIVXKI,IPROKI,WEITKI)
          IF (JKEVH.EQ.0) THEN
             CALL ALTELL ('ASEVST: not enough space to book KEVH',1,
     &                    'NEXT')
          ENDIF
          CALL BLIST (IW,'E+','KEVH')
      ENDIF
C
C - Build event header 'ASEV'
C
      CALL AUBOS ('ASEV',0,LMHLEN+LPRO*LASEVA,JASEV,IGARB)
      IF (JASEV.EQ.0) THEN
         CALL ALTELL ('ASEVST: not enough space to book ASEV ',1,
     &                'NEXT')
      ENDIF
      CALL BLIST (IW,'E+','ASEV')
      IW(JASEV+LMHCOL) = LASEVA
      IW(JASEV+LMHROW) = LPRO
      KASEV = JASEV+LMHLEN
      CALL UCOPY (IRNDJO(1,1),IW(KASEV+JASERG),LASEVA*LPRO)
C
 999  CONTINUE
      RETURN
      END
#endif
