      SUBROUTINE AFILOU(FNAME,FTYPE,FDEVI,IRET)
C-----------------------------------------------------------------------
CKEY ALREAD RUNCARDS FILO
C - Author:       D.Schlatter                      - 880811
C   Modified by : H.Albrecht                       - 881108
C   Modified by : D.Schlatter and M.Talby          - APR 89
C
C!  get filename and alephtype from output banks 'FILO','POT ','DST '
C   as defined in AIOCAR
C
C  'filename' means 'fn ft fm' on IBM ,'fname.ext' on
C             VAX, APOLLO and DECS
C  'filetype' means 'ft' on IBM ,'ext' on VAX, APOLLO and DECS.
C
C     input:    runcard  FILO  'filename '
C                         if filetype is not an alephtype then:
C                        FILO  'filename | alephtype'
C                         other allowed cards are: POT, DST, MINI
C                  e.g:  FILO 'ABC.NATIVE'
C                        FILO 'XYZ.DAT | NATIVE'
C
C     output:   FNAME      filename
C               FTYPE      alephtype (NATIVE,EPIO,EDIR )
C               FDEVI      device ('DISK'/ 'TAPE')
C               IRET       ACDARG RETURN code
C                          = 0  bank FILO found with correct filename
C
C   Calls:   AIOCAR,ACDARG
C
C   ENTRY    AFILO0(ISTR,IND,NR)      RETURNs namind+nr for given stream
C-----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "alrcom.h"
      CHARACTER*(*) FNAME(1),FTYPE(1),FDEVI(1)
      CHARACTER*80  FN,FT,FD
      CHARACTER*4   CARINP(MINPUT),CAROUT(MOUTPT)
      CHARACTER*50  CPARA
      INTEGER       NAMFO(MOUTPT),NRFO(MOUTPT)
      INTEGER       ACARD0
      INTEGER       ACARD3
      LOGICAL FIRST
      SAVE
      DATA NAMFO/MOUTPT*0/,NRFO/MOUTPT*0/
      DATA FIRST/.TRUE./

      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL AIOCAR(CARINP,MINPUT,CAROUT,MOUTPT)
        NPAR=2
      ENDIF
C-----------------------------------------------------------------------
C
C                    loop over all legal output cards
C
      ISTRM = 1
      IRET = -1
      DO 10 IC=1, MOUTPT
C                    get filename/type from next card FILO,POT,,,
        CALL ACDARG(CAROUT(IC),'NATIVE','A',FN,FT,FD,IRC)
        IF (IRC .NE. -1) IRET=IRC
        IF (IRC .EQ. 1)  IRET = 5
        IF (IRC .NE. 0) GO TO 9
        FNAME(ISTRM)=FN
        FTYPE(ISTRM)=FT
        FDEVI(ISTRM)=FD
        ILEN=ACARD3(NPAR,'WITH',CPARA)
        IF(ILEN.NE.0) FDEVI(ISTRM)=CPARA(6:ILEN)
        NAMFO(ISTRM) = ACARD0( NR )
        NRFO(ISTRM) = NR
    9   IF(IRC .NE. -1) ISTRM = ISTRM + 1
   10 CONTINUE
C
  999 RETURN
C                   RETURN namind + number of card-bank
      ENTRY AFILO0( JSTRM, NAID,NUM )
      NAID = NAMFO(JSTRM)
      NUM  = NRFO(JSTRM)
      END
#endif
