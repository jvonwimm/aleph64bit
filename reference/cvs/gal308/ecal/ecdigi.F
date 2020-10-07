*DK ecdigi
      SUBROUTINE ECDIGI
C.-----------------------------------------------------------------
C! ECAL Digitisation steering
C      O.CALLOT    29-OCT-85
C  modified by M.Bernard /D.Pallin 05/89
C. - calls EDTONO,EDTDIG,EDTZSU,EDTCOR                    this .HLB
C.         EDWBRU,EDWDGT,ECTRIG,EDFILH                    this .HLB
C.         ECPRTO,ECPRWI,ECPRHI,ECPRTR                    this .HLB
C          WDROP                                         BOS77
C. - called by ASDIGI
C-------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "edcond.h"
#include "bcs.h"
#include "ecnamc.h"
#include "jobcom.h"
C ------------------------------------------------------------
#include "ecstat.h"
#include "bmacro.h"
C
C  ===  Unpack Tower signals, add noise , and mix with real event
C
      CALL EDTONO
C
C  ===  Create storey Digits
C
      CALL EDTDIG
C
C  ===  Suppress Zeroes in storeys, build Tower information
C
      CALL EDTZSU
C
C  ===  Correct tower digits for final format
C
      CALL EDTCOR
      CALL WDROP (IW,IDETTM)
C
C  ===  Wire plane processing. Add noise and mix real event
C
      CALL EDWBRU
C
C  ===  Now, generate trigger signals
C
      CALL ECTRIG
C
C  ===  Create wires digits ( and Zero sup.)
C
      CALL EDWDGT
C
C    if FHISJO(4)=.TRUE., then fill histograms with digital signals
C
       IF(FHISJO(4)) CALL EDFILH
C
      CALL WDROP (IW,IDEWTM)
      IF(FDEBJO.AND.IPRIJO(4).NE.0) THEN
        CALL ECPRTO
        CALL ECPRWI
        CALL ECPRHI
        CALL ECPRTR
      ENDIF
      RETURN
      END
#endif
