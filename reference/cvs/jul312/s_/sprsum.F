      SUBROUTINE SPRSUM
C----------------------------------------------------------------------
C! Print SATR job statistics
C!
C!    Author:     H. Meinhard       09-Oct-1987
C!    Modified:   H. Meinhard       06-Mar-1990  (2)
C!
C?
C!======================================================================
#ifndef DOC
#include "rlunit.h"
#include "sstatc.h"
      REAL RAWD(2),COOR(2),TRCK(2,0:3)
C----------------------------------------------------------------------
      WRITE (LOUTRL,500)
  500 FORMAT (/1X,'Job summary of SATR reconstruction:'/1X,80('-'))
      WRITE (LOUTRL,510) NERDSS
C
C?   normalisation
C
      IF (NERDSS .NE. 0) THEN
        DO 310 ISIDE = 1, 2
          RAWD(ISIDE) = REAL(NRAWSS(ISIDE)) / REAL(NERDSS)
          COOR(ISIDE) = REAL(NCOOSS(ISIDE)) / REAL(NERDSS)
          TRCK(ISIDE,3) = 0
          DO 300 IQF = 0, 2
            TRCK(ISIDE,IQF) = REAL(NTRKSS(ISIDE,IQF)) / REAL(NERDSS)
            TRCK(ISIDE,3) = TRCK(ISIDE,3) + TRCK(ISIDE,IQF)
  300     CONTINUE
  310   CONTINUE
        WRITE (LOUTRL,520) NETRSS,RAWD,COOR,TRCK
      ENDIF
C
  510 FORMAT (
     +  ' Number of events with raw data ....................... ',I8)
  520 FORMAT (
     +  ' Number of events with tracks, qual. 0, on side A ..... ',I8/
     +  ' Number of events with tracks, qual. 0, on side B ..... ',I8/
     +  ' Number of events with tracks, qual. 1, on side A ..... ',I8/
     +  ' Number of events with tracks, qual. 1, on side B ..... ',I8/
     +  ' Number of events with tracks, qual. 2, on side A ..... ',I8/
     +  ' Number of events with tracks, qual. 2, on side B ..... ',I8/
     +  ' Raw data on side A per event with raw data ........... ',F8.3/
     +  ' Raw data on side B per event with raw data ........... ',F8.3/
     +  ' Coordinates on side A per event with raw data ........ ',F8.3/
     +  ' Coordinates on side B per event with raw data ........ ',F8.3/
     +  ' Tracks, qual. 0, on side A per event with raw data ... ',F8.3/
     +  ' Tracks, qual. 0, on side B per event with raw data ... ',F8.3/
     +  ' Tracks, qual. 1, on side A per event with raw data ... ',F8.3/
     +  ' Tracks, qual. 1, on side B per event with raw data ... ',F8.3/
     +  ' Tracks, qual. 2, on side A per event with raw data ... ',F8.3/
     +  ' Tracks, qual. 2, on side B per event with raw data ... ',F8.3/
     +  ' Total tracks on side A per event with raw data ....... ',F8.3/
     +  ' Total tracks on side B per event with raw data ....... ',F8.3)
      END
#endif
