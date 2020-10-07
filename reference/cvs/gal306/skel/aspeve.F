*DK aspeve
      SUBROUTINE ASPEVE
C --------------------------------------------------------------------
C. - F.RANJARD - 850328                modified - 890210
C! Process current event # NEVTJO
C. - Execute each processor in turn if required :
C.     KINE, TRAC(incl. hits), ASIG, DIGI, TRIG, RDST
C. - Drop the 'T'list at the end : it contents banks which do not go
C.   onto the output.
C. - called by    ASPRUN                                  from this .HLB
C. - Calls        ASKINE, ASTRAC, ASASIG, ASDIGI          from this .HLB
C.                ASTRIG
C.                BDROP                                   from BOS77.lib
C.
C -----------------------------------------------------
#ifndef DOC
      SAVE
#include "jqcom.h"
#include "iocom.h"
#include "jobcom.h"
#include "kincom.h"
#include "kvoljj.h"
      DATA MKVOL /200/
#include "bmacro.h"
C ----------------------------------------------------------------------
C
C - Execute modules in turn if required
C     set origin vertex volume name
      JKVOL = IW(NAKVOL)
      IF (JKVOL.EQ.0) THEN
          CALL ALBOS ('KVOL',0,LMHLEN+MKVOL*LKVOLA,JKVOL,IGARB)
          IW(JKVOL+LMHCOL) = LKVOLA
          IW(JKVOL+LMHROW) = 0
      ENDIF
C
C     event generator
      IF (IPROJO(1).NE.0) THEN
         CALL ASKINE
      ENDIF
C
C     fill origin vertex volume name if it is not done
         JKVOL = IW(NAKVOL)
         IF (LROWS(JKVOL) .EQ. 0) THEN
            KKVOL = KNEXT(JKVOL)
            DO 1 I=1,NIVXKI
            IW(KKVOL+JKVOVN) = INTCHA ('    ')
            IW(KKVOL+JKVOVM) = INTCHA ('    ')
 1          KKVOL = KKVOL + LCOLS(JKVOL)
            IW(JKVOL+LMHROW) = NIVXKI
         ENDIF
C
C     tracking + hits ===> analog signals in tracking devices
      IF (IPROJO(2).NE.0) THEN
C     transfert KINE BOS banks to KINE GEANT banks
         CALL ASTGEA
         CALL ASTRAC
      ENDIF
C
C     saturation effects in calorimeters ===> analog signals
      IF (IPROJO(3).NE.0) THEN
         CALL ASASIG
      ENDIF

C
C     digitizations in all devices ===> digits
      IF (IPROJO(4).NE.0) CALL ASDIGI
C
C     apply trigger conditions ===> trigger output
      IF (IPROJO(5).NE.0) CALL ASTRIG
C
 999  CONTINUE
      CALL BDROP (IW,'T')
      RETURN
      END
#endif
