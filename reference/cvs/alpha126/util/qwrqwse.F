      SUBROUTINE QWRQWSE(lun,nurun,iers)
CKEY FILL /INTERNAL
C-----------------------------------------------------------------------
C! Write SEVT cards for run nurun
C
C     Author: J. Boucrot      27-Jan-2000
C
C     Input  : QWSE bank
C              lun = Fortran output unit
C             nurun= run number
C
C     Called by QWRFSEVT
C-----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
      PARAMETER ( NRL = 11 )
      CHARACTER*4 SEVV
      DATA SEVV / 'SEVT' /
#include "bmacro.h"
C-----------------------------------------------------------------------
      IERS=0
C First check that 'QWSE' , NR=NURUN bank exists:
      IF (NURUN.LE.0) GO TO 900
      JQWSE=NLINK('QWSE',NURUN) 
      IF (JQWSE.LE.0) GO TO 900
      NQWSE=LROWS(JQWSE)
      IF (NQWSE.LE.0) GO TO 900
C Number of lines to be printed:
      nl1=nrl-1   
      if (nqwse.le.nl1) then
         iline=1 
      else
         nn=nqwse-nl1
         iline=2+nn/nrl
      endif
C Print 1st line with SEVT:
      JQW=JQWSE+LMHLEN
      IF (ILINE.EQ.1) THEN
         nnqq=min0(nqwse,nl1)
         WRITE (LUN,1234) SEVV,NURUN,(IW(JQW+JJ),JJ=1,NNQQ)
      ELSE
         WRITE (LUN,1234) SEVV,NURUN,(IW(JQW+JJ),JJ=1,NL1)
      ENDIF
      IF (ILINE.EQ.1) GO TO 999
C More lines :
      JQW=JQW+NL1
      npt=nqwse-nl1
      DO 100 IK=2,ILINE
         nnmx=min0(npt,nrl)
         WRITE (LUN,1236) (IW(JQW+JJ),JJ=1,NNMX)
         JQW=JQW+NRL 
         NPT=NPT-NRL
 100  CONTINUE   
C
      GO TO 999
 900  IERS=1
 999  RETURN
 1234 FORMAT (A4,1X,11I6) 
 1236 FORMAT (5X,11I6)
      END
#endif
