      SUBROUTINE MUNEWR
C----------------------------------------------------------------------
C!  - Initialises muon id part, for use outside of the julia context
C!    Should be called at the start of each run.
C!
C!
C!   Author   :- G.Taylor       15-May-1992
C!
C?
C!======================================================================
#ifndef DOC
#include "rconds.h"
#include "hdebug.h"
#include "mdebug.h"
#include "rflags.h"
#include "rparac.h"
#include "bcs.h"
#include "rlunit.h"
#include "rcurnt.h"
      SAVE
      DATA INIHR/0/
      LOUTRL = IW(6)
      IF (INIHR.EQ.0) THEN
        CALL RERROR('RINERR',1,' INIT RERROR       ')
        INIHR = 1
      ENDIF
      JDBDRF(JULHC)=1
      JHISRF(JULHC)=0
      FIELRC= ALFIEL(DUMMY)
      LRGEOM=JUNIDB(IDUM)
      LDEBRL = LOUTRL
C
C?   Get run number
C
      CALL ABRUEV (IRUNRC, IEVT)
      IHPRIN=0
      JHISRF(JULMU)=2
      IMPRIN=0
C HQOF exists if this identification is running outside julia
      IND = NBANK('HQOF',0,1)
      CALL HINIRU
      CALL MGINIG(LRGEOM,IRUNRC,IER,NEWR)
      CALL MINIJO
      RETURN
      END
#endif
