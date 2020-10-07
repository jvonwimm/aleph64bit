      Subroutine VPRSUM
C----------------------------------------------------------------------
C! Print end of job statistics for VGLOB code
CKEY VGLOB
C
C   Author:   Paul Rensing   21/11/95
C   Modified: Marco Cattaneo 08/07/98
C             Improve test on VGLOB execution
C
C----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C
#include "bcs.h"
#include "vglbcm.h"
#include "vglbst.h"
C
      integer namind, vdyear
      external namind, vdyear
C
      IF( IW(NAMIND('NVGL')) .NE. 0 ) return
      IF( VDYEAR() .GE. 95 .AND. IW(NAMIND('VGLO')) .EQ. 0 ) return
C
      WRITE(iw(6),10) 
      WRITE(iw(6),20) NEvtVD
      IF (NevtVD .EQ. 0) RETURN
C
      WRITE(iw(6),30) TimTot, TimTot / FLOAT(NEvtVD)
      WRITE(iw(6),40) TimHit, TimHit / FLOAT(NEvtVD)
      WRITE(iw(6),50) TimKal, TimKal / FLOAT(NEvtVD)
      WRITE(iw(6),60) TimSol, TimSol / FLOAT(NEvtVD)
C
      WRITE(iw(6),70) 
      WRITE(iw(6),80) 
      WRITE(iw(6),90) NHitVD(1), NHitVD(2)
      IF (NHitVD(1)*NHitVD(2) .NE. 0) THEN
        WRITE(iw(6),100) FLOAT(NHitAs(1)) / FLOAT(NHitVD(1)),
     $       FLOAT(NHitAs(2)) / FLOAT(NHitVD(2))
        WRITE(iw(6),110) FLOAT(NHitDb(1)) / FLOAT(NHitVD(1)),
     $       FLOAT(NHitDb(2)) / FLOAT(NHitVD(2))
      ENDIF  
C
      WRITE(iw(6),120)
C
 10   FORMAT(/,' VGLOB End of Job Statistics',/,1X,70('='))
 20   FORMAT(' Number of Events Processed            ', I9)
 30   FORMAT(' Total CPU Time Used             (sec) ',
     $     F9.1,2X,F8.3,'/event')
 40   FORMAT(' CPU Time to Generate Hit Combinations ',
     $     F9.1,2X,F8.3,'/event')
 50   FORMAT(' CPU Time used by Full Kalman          ',
     $     F9.1,2X,F8.3,'/event')
 60   FORMAT(' CPU Time to Solve Combinatorics       ',
     $     F9.2,2X,F8.3,'/event')
 70   FORMAT(2X)
 80   FORMAT('                                   Z View  R-phi View')
 90   FORMAT(' Total Number of VDET Hits        ', I8, I8)
 100  FORMAT(' Fraction Assigned to Tracks      ', F8.2, F8.2)
 110  FORMAT(' Fraction Assigned to Two Tracks  ', F8.2, F8.2)
 120  FORMAT(1X,70('='))
C
      RETURN
      END
#endif
