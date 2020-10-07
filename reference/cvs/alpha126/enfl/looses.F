      SUBROUTINE LOOSES(ROUTIN,NCUT)
C----------------------------------------------------------------
C! Event counter
C
C  Francois Le Diberder -- 1985
C  Rewritten in Fortran 77 . Patrick Janot -- 18 Apr 1990
C----------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
      PARAMETER ( maxrou = 50 , maxcut = 20 )
      CHARACTER*8 routin, rprint, subrou(maxrou)
      PARAMETER ( rprint = 'summary ')
      DIMENSION icheck(maxrou,maxcut)
      LOGICAL first
      DATA nrout/0/, first/.TRUE./
C----------------------------------------------------------------
      IF ( first ) THEN
        CALL vzero(icheck(1,1),maxrou*maxcut)
        first = .FALSE.
      ENDIF
C
C  Last call : cut summary
C
      IF ( routin .EQ. rprint ) THEN
C
        IF ( nrout .NE. 0) THEN
          WRITE (IW(6),2000)
          WRITE (IW(6),2001) ( subrou(irout),
     .                  ( icheck(irout,j),j=1,20), irout = 1 , nrout )
        ELSE
        ENDIF
C
C  Identifies the subroutine :
C
      ELSEIF ( ncut .GT. 0 .AND. ncut .LE. maxcut ) THEN
C
        krout = 0
        IF     ( nrout .LE. maxrou ) THEN
          DO 1 irout = 1 , nrout
            krout = irout
            IF ( routin .EQ. subrou(irout) ) GOTO 2
    1     CONTINUE
          nrout = nrout + 1
          IF ( nrout .GT. maxrou ) RETURN
          krout = nrout
          subrou(krout) = routin
        ELSE
          RETURN
        ENDIF
    2   CONTINUE
C
C  Update counters  :
C
        icheck(krout,ncut) = icheck(krout,ncut) + 1
C
      ENDIF
C
      RETURN
C---------------------------------------------------------------------
 2000 FORMAT(1x,'***** From LOOSES ***** : Cuts effects'/)
 2001 FORMAT(1x,'Subroutine: ',a8/
     * 1X,' !  1:',I6,' !  2:',I6,' !  3:',I6,' !  4:',I6,' !  5:',I6/
     * 1X,' !  6:',I6,' !  7:',I6,' !  8:',I6,' !  9:',I6,' ! 10:',I6/
     * 1X,' ! 11:',I6,' ! 12:',I6,' ! 13:',I6,' ! 14:',I6,' ! 15:',I6/
     * 1X,' ! 16:',I6,' ! 17:',I6,' ! 18:',I6,' ! 19:',I6,' ! 20:',I6)
      END
#endif
