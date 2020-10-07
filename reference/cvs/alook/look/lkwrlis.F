      SUBROUTINE LKWRLIS(COMAND)
C ----------------------------------------------------------
C! write a list on unit 13
C ----------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "lkfil.h"
      LOGICAL LKOPTI
      CHARACTER*(*) COMAND
      CHARACTER*200 TEMP,LISTS*80
C ----------------------------------------------------------
      LISTS=' '
      TEMP=' '
      CALL LKGTPAR(COMAND,1,'_List) to be written out: ',LISTS,LL)
      IF (.NOT. FLGFIL(3)) THEN
          CALL LKOPWR(COMAND,'   ',32040)
          IF (.NOT.FLGFIL(3)) RETURN
      ENDIF
      IF (INDEX(LISTS,'C').GT.0) CALL BWRITE(IW,LFILE(3),'C')
      IF (INDEX(LISTS,'E').GT.0) CALL BWRITE(IW,LFILE(3),'E')
      IF (INDEX(LISTS,'S').GT.0) CALL BWRITE(IW,LFILE(3),'S')
      IF (INDEX(LISTS,'T').GT.0) CALL BWRITE(IW,LFILE(3),'T')
      RETURN
      END
#endif
