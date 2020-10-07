      SUBROUTINE TRKDMP(IUNIT,ICTRL)
C
C----------------------------------------------------------------------
C! Dump all fitted TPC tracks
C!
C!   Author:    Z. Feng    10-03-87
C!   Modified:  R. Johnson  6-05-87
C!
C!   Called by TPCREC
C!
C!   Input:
C!         - IUNIT   /I    Fortran output logical unit number
C!         - ICTRL   /I    Control parameter
C!                         0:  print only track information
C!                         1:  include coordinates
C!                         2:  include wire hits
C!                         3:  include coordinates and wire hits
C!
C!---------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "rcurnt.h"
C
      KTGFT=IW(NAMIND('TGFT'))
      IF (KTGFT.EQ.0) RETURN
C
      NTRK=IW(KTGFT+LMHROW)
      WRITE(IUNIT,100) NTRK,IRUNRC,IEVTRC
  100 FORMAT(/' Dump of ',I3,' fitted tracks for ','  Run ',I5,
     &            '  Event ',I6)
C
      DO 50 ITK=1,NTRK
        CALL TKSGDP(KTGFT,ITK,IUNIT,ICTRL)
   50 CONTINUE
C
      WRITE(IUNIT,101) IRUNRC,IEVTRC
  101 FORMAT('------ End of TRACK dump for ','  Run ',I5,
     &            '  Event ',I6,'------'/)
      RETURN
      END
#endif
