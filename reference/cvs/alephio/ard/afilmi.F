      SUBROUTINE AFILMI(FNAME,FTYPE,FDEVI,IRET)
C-----------------------------------------------------------------------
C - Author :    D.Schlatter         AUG-88
C   modified by M.Talby :           MAY-89
C
C! get filename and alephtype from ALEPH input bank ( FILM )
CKEY ALREAD RUNCARDS FILM
C
C  'filename' means 'fn ft fm' on IBM ,'fname.ext' on VAX and APOLLO
C  'filetype' means 'ft' on IBM ,'ext' on VAX and APOLLO.
C
C     input:    runcard     FILM  'filename '
C                         if 'filetype' is not a standard alephtype then
C                           FILM  'filename | alephtype'
C                     e.g.  FILM 'ABC.NATIVE'
C                           FILM 'XYZ.DAT|NATIVE'
C
C     output:   FNAME      full filename
C               FTYPE      alephtype
C               FDEVI      device ('DISK'/ 'TAPE')
C               IRET       RETURN code from ACDARG
C                          = 0  bank found with correct filename
C
C   Calls:   ACDARG,AINQUI
C
C-----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "alrcom.h"
      CHARACTER*(*) FTYPE,FNAME,FDEVI
      INTEGER ACARD0
      LOGICAL LEX
      DATA IFI /0/
C------------------------------------------------------------
C
      IF (IFI.EQ.0) THEN
         IFI = 1
         NRFILM = 0
         IRFILM = 0
      ENDIF
C
C                   get filename/alephtype from FILM card
        CALL ACDARG ('FILM','EPIO','*',FNAME,FTYPE,FDEVI,IRC )
        IRET = IRC
        IRFILM = IRC
        IF (IRC .NE. 0)  GO TO 999
        NAMI = ACARD0 (NR)
        NRFILM = NR
C
  999 RETURN
      END
#endif
