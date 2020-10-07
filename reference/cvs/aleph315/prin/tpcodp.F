      SUBROUTINE TPCODP(IUNIT)
C----------------------------------------------------------------------
C!    Dump BOS bank TPCO to IUNIT
CKEY PRINT TPC
C!
C!    Author:    R. Johnson  23-10-86
C!    Modified:  R. Johnson  18-06-90
C!
C!    Input:
C!         - IUNIT   /I    Fortran output logical unit number
C!
C!---------------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "tpcojj.h"
#include "bcs.h"
#include "evehjj.h"
C
C----------------------------------------------------------------------
C
#include "bmacro.h"
C
C----------------------------------------------------------------------
C
      KTPCO=IW(NAMIND('TPCO'))
      IF (KTPCO.EQ.0) RETURN
      NC=LROWS(KTPCO)
      KEVEH=IW(NAMIND('EVEH'))
      IF (KEVEH.NE.0) THEN
        IRUN=IW(KEVEH+JEVERN)
        IEVT=IW(KEVEH+JEVEEV)
      ELSE
        IRUN=0
        IEVT=0
      ENDIF
      WRITE(IUNIT,103) NC,IRUN,IEVT
  103 FORMAT(//' Dump of ',I4,' TPC coordinates for run ',I5,' event ',
     &         I6)
      WRITE(IUNIT,102)
  102 FORMAT(/2X,' IC',2X,'sect',2X,' row',2X,' pad',5X,
     &       'radius',5X,' phi',6X,
     &       '   z',8X,'SigRPhi',5X,' SigZ ',3X,' Origin',
     &       2X,'track',1X,'clus',1X,'twin',2X,'sect R*Phi',4X,
     &       'sect Z')
      DO 21 IC=1,NC
        IROW=IW(KROW(KTPCO,IC)+JTPCIN)/100000
        IPAD=MOD(IW(KROW(KTPCO,IC)+JTPCIN),1000)
        ISEC=MOD(IW(KROW(KTPCO,IC)+JTPCIN),100000)/1000
        WRITE(IUNIT,101) IC,ISEC,IROW,IPAD,(RTABL(KTPCO,IC,J),J=2,6),
     &                   (ITABL(KTPCO,IC,J),J=7,10),
     &                   (RTABL(KTPCO,IC,J),J=11,12)
  101   FORMAT(1X,I3,2X,I4,2X,I4,2X,I4,1X,F10.4,1X,F10.6,
     &         3(1X,F10.4),4X,I2,5X,3(2X,I3),2(1X,F10.4))
   21 CONTINUE
      WRITE(IUNIT,104)
  104 FORMAT('-------------------------- End of TPCO dump',
     &      ' ---------------------------------------------------'/)
      RETURN
      END
#endif
