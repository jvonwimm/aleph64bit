      SUBROUTINE FTKDMP(IUNIT,ICTRL)
C
C----------------------------------------------------------------------
C! Dump all fitted tracks from FRFT
CKEY PRINT TRACKS TPC ITC VDET
C!
C!   Author:    R. Johnson  29-06-87
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
C!                         NOTE that wire hits generally are not
C!                              available from the POT
C!
C!---------------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "bcs.h"
#include "evehjj.h"
C
#include "bmacro.h"
C
      KFRFT=IW(NAMIND('FRFT'))
      IF (KFRFT.EQ.0) RETURN
C
      KEVEH=IW(NAMIND('EVEH'))
      IF (KEVEH.NE.0) THEN
        IRUN=IW(KEVEH+JEVERN)
        IEVT=IW(KEVEH+JEVEEV)
      ELSE
        IRUN=0
        IEVT=0
      ENDIF
      NTRK=LROWS(KFRFT)
      WRITE(IUNIT,100) NTRK,IRUN,IEVT
  100 FORMAT(/' Dump of ',I3,' fitted tracks for Run ',I5,
     &            '  Event ',I6,':')
C
      DO 50 ITK=1,NTRK
        CALL FRFTDP(ITK,IUNIT,ICTRL)
   50 CONTINUE
C
      WRITE(IUNIT,101) IRUNRC,IEVTRC
  101 FORMAT('------ End of TRACK dump for Run ',I5,
     &            '  Event ',I6,'------'//)
C
      RETURN
      END
#endif
