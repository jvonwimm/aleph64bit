      SUBROUTINE CALREC
C---------------------------------------------------
C! Perform ECAL and HCAL Analysis
C!   Modified :- E. Lancon             17-MAR-1992
C!        add call to EGAEST
C?
C!======================================================================
#ifndef DOC
#include "rlunit.h"
#include "rparac.h"
#include "bcs.h"
#include "ecnamc.h"
#include "rflags.h"
      LOGICAL RMOD
C
      KESDA = IW(NAESDA)
      IERTK=1
      IF (KESDA .EQ. 0) THEN
        IER=10
        GOTO 10
      ENDIF
      IER=0
      CALL RNXMOD(MODECL,RMOD)
      IF (FDETRF(JULEC)) THEN
        CALL ECFCLU(IER)
        IF (IER .NE. 0) GOTO 10
C? MIP RESEARCH
        CALL RNXMOD(MODMIP,RMOD)
        CALL ECRMIP
C? ELECTRON RESEARCH
        CALL RNXMOD(MODEID,RMOD)
        CALL ELECID
C? SEARCH FOR EMBEDDED GAMMAS WITH EBNEUT
        CALL RNXMOD(MODGMX,RMOD)
        CALL ECGMIX
C? FILL CLUSTERS BANKS
        CALL RNXMOD(MODFEO,RMOD)
        CALL ECFOBJ
C? STORE ECAL STATISTICS
        CALL RNXMOD(MODFEP,RMOD)
        CALL ECPRIN
        IERTK=0
      ENDIF
C
   10 CONTINUE
C      CALL RNXMOD(MODHCL,RMOD)
C      IF (FDETRF(JULHC).AND.RMOD) CALL HCFCLU(NHCLU)
      CALL RNXMOD(MODHDP,RMOD)
      IF (FDETRF(JULHC)) CALL HDPREC
C
C Global calorimeter analysis
C
      IF(IER.NE.0)GO TO 998
      IF(IERTK.NE.0)GO TO 998
      CALL RNXMOD(MODEHG,RMOD)
      CALL ECHCGL
      CALL RNXMOD(MODEND,RMOD)
C
      RETURN
  998 CONTINUE
      CALL RERROR(
     *  'CALREC',1,' Reconstruction not complete for this event')
      END
#endif
