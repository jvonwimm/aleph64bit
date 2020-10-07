      SUBROUTINE ALTELL (TEXT,IERR,TAXION)
C ------------------------------------------------------------------
CKEY IO / USER
C - F.Ranjard - 870105
C! Print text and take action
C - print text 'TEXT' if not blank
C - increment error counter# IERR if in the range [1,LERR]
C   if IERR=0 : no error set
C - take action 'TAXION':
C      TAXION = RETURN  : continue
C               NEXT    : go to end of event, then next event
C               STOP    : stop the program without run summary
C               END     : end of job
C               FATAL   : fatal error , trace back , end of job
C               INIT    : 1st call to initialize /JOBERR/
C -----------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "joberr.h"
      CHARACTER*(*) TEXT,TAXION
C ------------------------------------------------------------------
C - initialize /JOBERR/ at 1st entry
      IF (TAXION.EQ.'INIT') THEN
         TACTJO = 'RETURN'
         DO 1 I=1,LERR
 1       NERRJO(I) = 0
         ITELJO = 0
         KERRJO = 0
         RETURN
      ENDIF
C
C - check that the routine is not called a 2nd times after 'STOP' or
C   'END' or 'FATAL'
      IF (ITELJO .EQ. 1) CALL EXIT
      IF (TAXION.EQ.'STOP' .OR. TAXION.EQ.'END' .OR. TAXION.EQ.'FATAL')
     &    ITELJO = 1
C
C - print text
      LUN =  IW(6)
      IF (LUN .LE. 0) THEN
         TAXION = 'FATAL'
      ELSE
         IF (TEXT .NE. ' ') WRITE (LUN,800) TEXT,IERR,TAXION
 800     FORMAT (1X,'+++ALTELL+++ ',1X,A,2X,'error # ',I3,3X,
     &              'action taken ',1X,A)
      ENDIF
C
C - increment error counter
      IF (IERR .GT. 0) THEN
         JERR = MIN (IERR,LERR)
         NERRJO(JERR) = NERRJO(JERR) + 1
      ENDIF
C
C - take action TAXION
      TACTJO = TAXION
      IF (TAXION .EQ. 'RETURN') THEN
         RETURN
      ELSEIF (TAXION.EQ.'NEXT' .OR. TAXION.EQ.'END') THEN
#ifndef FORTRAN
         CALL QNEXTE
#else
         WRITE (LUN,'(3X,''QNEXTE is not implemented : call EXIT'')')
         CALL EXIT
#endif
      ELSEIF (TAXION.EQ.'STOP') THEN
         STOP
      ELSEIF (TAXION.EQ.'FATAL') THEN
#include "tracbck.h"
      ENDIF
C
      END
#endif
