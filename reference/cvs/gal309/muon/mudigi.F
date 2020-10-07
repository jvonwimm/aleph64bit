*DK mudigi
      SUBROUTINE MUDIGI
C
C*******************************************************************************
C
C T.Wang -860112
C
C       Routine to generate MU digitizing information.
C       Track uncorreleted noises are generated in subroutine MUUNOI
C       that is called once per event.
C
C       Called by ASDIGI
C       CALLS     MUUNOI, MUDGTZ      in this .HLB
C                 AUBPRS              in this .HLB
C
C*******************************************************************************
C
#ifndef DOC
      SAVE
#include "jobcom.h"
#include "iocom.h"
#include "bcs.h"
#include "munamc.h"
#include "mustat.h"
#include "bmacro.h"
C
C       Initialize BOS banks
C
      IDRP = IW(NAMUDG)
C
      IF( IDRP .NE. 0 )CALL BDROP(IW,'MUDTMUTDMUDG')
C
C       Generate track uncorrelated noise
C
      IF( ICMUJO(3) .NE. 0 )CALL MUUNOI
C
C       Digitization of all signal in 'MUHT'
C
      CALL MUDGTZ
C
C       Reduce bank size to real one
C
C
      CALL AUBPRS ('MUDGMUDTMUTD')
C
C       Fill histograms for MUON
C
      IF( FHISJO(8) )CALL MUFILL
C
C        Fill statistics for MUON
C
      JMUDG = IW(NAMUDG)
      NQ = LROWS (JMUDG)
      IF (NQ.GT.0) THEN
         NEVMUH = NEVMUH + 1
         NMXCLU = MAX (NMXCLU,NQ)
         NTMCLU = NTMCLU + NQ
      ENDIF
C
C - Debug
C
      IF (FDEBJO .AND. IPRIJO(8).EQ.1) THEN
         CALL MUPRDI
      ENDIF
      RETURN
      END
#endif
