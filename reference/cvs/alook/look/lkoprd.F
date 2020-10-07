      SUBROUTINE LKOPRD (FNAME,FORMA,IER)
C ------------------------------------------------------
C ------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "lkfil.h"
#include "lkunpk.h"
      CHARACTER*(*) FNAME,FORMA
      CHARACTER*80 FNAM
C -----------------------------------------------------
      IER = 0
C - if FORMA = CART or MOUNT then
C      make a FILI card for CARTridge in EPIO w/o MOUNT
C - elseif FNAME is given then
C      make the FILI card
C   endif
      IF (FORMA.EQ.'CART' .OR. FORMA.EQ.'MOUNT') THEN
         FNAM = FNAME
         LF = LNBLNK(FNAM)
         IF (FORMA.EQ.'MOUNT') FNAM = FNAM(1:LF)//'MOUNT'
         CALL ALK7FIL (FNAM(1:LNBLNK(FNAM)),IRK7)
         IF (IRK7.EQ.0) THEN
            FORMA = 'EPIO'
         ELSE
            RETURN
         ENDIF
      ELSEIF (FNAME.NE.' ') THEN
        IF (FORMA.EQ.'    ') THEN
          IF ((INDEX(FNAME,'.EPIO').GT.0).OR.
     +         (INDEX(FNAME,' EPIO').GT.0)) THEN
              FORMA='EPIO'
          ELSE IF ((INDEX(FNAME,'.EDI').GT.0).OR.
     +         (INDEX(FNAME,' EDI').GT.0)) THEN
              FORMA='EDIR'
          ELSE
              FORMA='NATIVE'
          ENDIF
        ENDIF
        CALL LKFILI(FNAME,FORMA,IER)
      ENDIF
C
C - close previous files if any
C   reset input units to 0
      IF (LINSEL.NE.0) THEN
         CALL ACLOSE (LINSEL,IER)
         LINSEL = 0
      ENDIF
      IF (LINDAT.NE.0) THEN
         CALL ACLOSE (LINDAT,IER)
         LINDAT = 0
      ENDIF
      CALL ABSTUN (LINDAT,LINSEL,LUTDAT,LUTSEL)
C
C - The following entry point is called by LCHKRU or LKRREC
C   when an End of Data has been found when reading an EDIR
C   Try to read a new FILM card on the same EDIR.
C   ==> skip building of FILI card and resetting of input units.
C
      ENTRY LKOPRD1 (FNAME,FORMA,IER)
C - open new input file
      CALL ABOPEN ('FILI',JRE)
      IF (JRE.NE.5) THEN
          WRITE(LOUT,*) '[File does not exist]'
          IER = JRE
          SEQFIL = .FALSE.
          RETURN
      ENDIF
      SEQNAM = FNAME
      CALL ABUNIT(LINDAT,LINSEL,LINSE2,LUTDAT,LUTSEL)
      LNAM = LNBLNK(FNAME)
      IF (LNAM.GT.0) THEN
         WRITE(LOUT,*) '[File ',FNAME(1:LNAM),' opened for read only]'
      ENDIF
C - read it till the 1st event
      CALL LKREAC
C - file has been opened and the 1st event reached
      SEQFIL = .TRUE.
      END
#endif
