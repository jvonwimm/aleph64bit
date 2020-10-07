      SUBROUTINE SNGRND(CLAS4,CLAS29)
C----------------------------------------------------------------------
CKEY EDIR DEF CLASS4-29
C! Classes 4 and 29 code.
C-
C   Input  : None
C   Output : CLAS4   = Clas 4 logical flag
C            CLAS29  = Clas 29 logical code
C-
C   Called by   : SELEVT
C   Calls  : SNGMTR,HCALEN
C   Input banks : None
C-
C                                            Author: M. Talby March 91
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
C --
      LOGICAL TRIGMU(2),CLAS4,CLAS29
C --
      CLAS4 = .FALSE.
      CLAS29 = .FALSE.
      CALL SNGMTR(TRIGMU)
      IF(TRIGMU(1)) CLAS29 = .TRUE.
      CALL HCALEN(EHCAL)
      IF(EHCAL.GT.3 .AND. TRIGMU(2)) CLAS4 = .TRUE.
C --
      RETURN
      END
#endif
