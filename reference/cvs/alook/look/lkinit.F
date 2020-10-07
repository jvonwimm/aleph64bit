      SUBROUTINE LKINIT
C ----------------------------------------------------------
C! read data cards, format file  and open data base
C ----------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "lkevt.h"
#include "lkfil.h"
      CHARACTER FNAME*200
      CHARACTER*120 FMT
      INTEGER ALFMT, ALGTENV
      LOGICAL LEX
C ---------------------------------------------------------
C
C     init ULIST
      CALL LKSTUNP('    ')
C     open data card file if any
      IST = ALGTENV ('LOOKCARDS',FNAME)
      LF = LENOCC(FNAME)
      CALL AINQUI(0,FNAME(1:LF),LEX)
#if defined(DEBUG)
      WRITE(LOUT,*) ' LKINIT LEX= ',LEX,' FNAME= ',FNAME(1:LF)
#endif
      IF(.NOT.LEX) THEN
        WRITE(IW(6),*) ' File ',FNAME(1:LF),' not found'
        IER = -1
      ELSE
        IER = 0
        CALL AOPEN (IW(5),FNAME,'CARD','DISK',IER)
      ENDIF
      IF (IER.EQ.0) CALL BREADC
C
C - get all formats
      IER = ALFMT (LUFMT,'ALL ',FMT)
      CALL BKFMT ('FILI','A')
C
C - default card, print, output files
      DO I=1,3
        FLGFIL(I)=.FALSE.
      ENDDO
      FILNAM(1)='LOOK_OUT.CARDS'
      FILNAM(2)='LOOK_OUT.PRINT'
      FILNAM(3)='LOOK_OUT.EPIO'
C
C - open default DAF on unit 4 by default
C
      CALL AOPDBS('    ',IER)
      IF (IER.NE.0) THEN
          DIRFIL=.FALSE.
      ELSE
          DIRFIL=.TRUE.
          CALL ADBVER(IVERS,IDAT)
      ENDIF
C
C - open input file if any given by data card
C
      FNAME = ' '
      SEQFIL=.FALSE.
      CALL ABMODE ('INTE')
      CALL ABRSEL ('E','     ',JRET)
      IRUNTR = 0
      IVOULU=0
      LINSEL = 0
      LINDAT = 0
      CALL LKOPRD(FNAME,FORMA,IER)
      IF(IER.NE.0) GOTO 100
 100  CONTINUE
C
      RETURN
      END
#endif
