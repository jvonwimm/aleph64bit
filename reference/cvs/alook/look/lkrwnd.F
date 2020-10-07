      SUBROUTINE LKRWND(COMAND)
C-----------------------------------------------------------------
C! rewind file specified in the comand
C
C - Input: COMAND / A   :command
C------------------------------------------------------------------------
#ifndef DOC
      CHARACTER*(*) COMAND
#include "bcs.h"
#include "lkfil.h"
      LOGICAL LKOPTI
C ---------------------------------------------------------------------
C - which file should be rewound ?
        IF(LKOPTI(COMAND,'DATA')) THEN
           LUN = LINDAT
        ELSEIF(LKOPTI(COMAND,'EDIR')) THEN
           LUN = LINSEL
        ELSE
           WRITE(LOUT,*) '[no file to rewind,bad command]'
           RETURN
        ENDIF
        IF (LUN.EQ.0) THEN
           WRITE(LOUT,*) '[no input file to rewind]'
           RETURN
        ENDIF
C            EDIR  ?
        IF(LUN.EQ.LINSEL) THEN
C        rewind EDIR, drop current $PTS bank
C        reopen data file
          REWIND LINSEL
          CALL BDROP (IW,'$PTSFILM')
          CALL AOPERD (LINDAT,LINSEL,LINSE2,IER)
          IF (IER.NE.0) GOTO 999
        ELSEIF(LUN.EQ.LINDAT) THEN
          CALL BRWND(LUN)
        ENDIF
        SEQFIL = .TRUE.
C
   99 CALL BDROP(IW,'E')
      CALL BDROP(IW,'R')
      CALL BDROP(IW,'C')
      CALL BDROP(IW,'T')
      CALL BLIST(IW,'E=','0')
      CALL BLIST(IW,'R=','0')
      CALL BLIST(IW,'C=','0')
      CALL BLIST(IW,'T=','0')
C
C     if EDIR read till the 1st event
      IF (LUN.EQ.LINSEL) CALL LKREAC
 999  RETURN
      END
#endif
