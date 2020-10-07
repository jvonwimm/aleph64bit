      SUBROUTINE AFILIN(FNAME,FTYPE,FDEVI,IRET)
C-----------------------------------------------------------------------
CKEY ALREAD RUNCARDS FILI
C - Author :    D.Schlatter                     AUG-88
C   modified by H.Albrecht             :        NOV-88
C   modified by D.Shlatter and M.Talby :        APR-89
C
C! get filename and alephtype from ALEPH input banks ( FILI )
C  as defined by AIOCAR
C  'filename' means 'fn ft fm' on IBM ,'fname.ext' on
C             VAX, APOLLO, and DECS
C  'filetype' means 'ft' on IBM ,'ext' on VAX, APOLLO, and DECS.
C
C     input:    runcard     FILI  'filename '
C                         if 'filetype' is not a standard alephtype then
C                           FILI  'filename | alephtype'
C                     e.g.  FILI 'ABC.NATIVE'
C                           FILI 'XYZ.DAT|NATIVE'
C
C     output:   FNAME      full filename
C               FTYPE      alephtype
C               FDEVI      device ('DISK'/ 'TAPE')
C               IRET       RETURN code from ACDARG
C                          = 0  bank found with correct filename
C                          = 6  file does not exist
C
C   Calls:   AIOCAR,ACDARG,AINQUI
C
C   ENTRY    AFILI0(ISTR,IND,NR)      RETURNs namind+nr for given stream
C-----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "alrcom.h"
      CHARACTER*(*) FTYPE(1),FNAME(1),FDEVI(1)
      CHARACTER*80  FT,FN,FD
      CHARACTER*4   CARD,FD4,CARINP(MINPUT),CAROUT(MOUTPT)
      CHARACTER*50  CPARA
      INTEGER       ACARD0
      INTEGER       ACARD3
      INTEGER       NAMFI(MINPUT),NRFI(MINPUT)
      LOGICAL LEX,FIRST
      SAVE
      DATA NAMFI/MINPUT*0/,NRFI/MINPUT*0/
      DATA FIRST/.TRUE./

C------------------------------------------------------------
C
      IF(FIRST) THEN
        FIRST=.FALSE.
C                   get ALEPH conventions for cards
        CALL AIOCAR(CARINP,MINPUT,CAROUT,MOUTPT)
        NPAR=2
      ENDIF
C                   loop over all posible input cards
      ISTRM = 1
      IRET = -1
      DO 10 IC=1,MINPUT
C                   get filename/alephtype from FILI,,,  card
        CALL ACDARG (CARINP(IC),'NATIVE','*',FN,FT,FD,IRC )
        IRET = IRC
        IF (IRC .EQ. -1) GOTO 10
        IF (IRC .NE. 0)  GO TO 9
        FNAME(ISTRM)=FN
        FTYPE(ISTRM)=FT
        FDEVI(ISTRM)=FD
        NAMFI(ISTRM) = ACARD0( NR )
        NRFI(ISTRM)  = NR
C                   does file exist ?
        FD4 = FDEVI( ISTRM )
        IF (FD4 .EQ. 'DISK')  THEN
          CALL AINQUI (0,FNAME(ISTRM),LEX)
          IF (.NOT.LEX) THEN
            IRET = 6
            GOTO 999
          ENDIF
        ENDIF
    9   ISTRM = ISTRM + 1
   10 CONTINUE

  999 RETURN
      ENTRY AFILI0( JSTRM, NAID,NUM )
      NAID = NAMFI(JSTRM)
      NUM  = NRFI(JSTRM)
      END
#endif
