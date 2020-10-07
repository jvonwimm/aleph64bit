      SUBROUTINE ESLCT(XXX,NVERT,KKINE,IFLAG)
C***********************************************************************
C!SELECT TRACKS                                                        *
C!                                                                     *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  861128                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : KINE,VRTX                                              *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!    INPUT  : NVERT = NUMBER OF VERTICES ON TRACK                     *
C!             KKINE = POINTER TO KINE BANK                            *
C!                                                                     *
C!    OUTPUT : XXX(3,2) = COORDINATES OF FIRST AND LAST VERTEX         *
C!             IFLAG : SEE BELOW                                       *
C!                                                                     *
C!      IF ONLY ONE VERTEX THEN                                        *
C!          IF IT IS IN TPC VOLUME THEN                                *
C!             IFLAG=1                                                 *
C!          ELSE                                                       *
C!             IFLAG=0                                                 *
C!      ELSE                                                           *
C!          IF ALL VERTICES IN TPC VOLUME THEN                        *
C!              IFLAG=0                                               *
C!          ELSE                                                       *
C!              IFLAG=1                                               *
C!          ENDIF                                                      *
C!      ENDIF                                                          *
C!                                                                     *
C***********************************************************************
#ifndef DOC
#include "bcs.h"
C
      DIMENSION XXX(3,*)
#include "etpgo.h"
      LHEVT3=3
      LKINE=5
      IV=IW(KKINE+LHEVT3+LKINE+1)
      KVERT=NLINK('VERT',IV)
      DO 30 J=1,3
   30 XXX(J,1)=RW(KVERT+LHEVT3+J)
      R=SQRT(XXX(1,1)**2+XXX(2,1)**2)
      RLIM=RTPMX-10.
      ZLIM=ZTPMX-10.
      IF(R.GT.RLIM.OR.ABS(XXX(3,1)).GT.ZLIM)THEN
         IFLAG=0
      ELSEIF(NVERT.EQ.1)THEN
         IFLAG=1
      ELSE
         RLIM=RTPMX
         ZLIM=ZTPMX
         DO 1 I=1,NVERT-1
         IV=IW(KKINE+LHEVT3+LKINE+1+I)
         KVERT=NLINK('VERT',IV)
         DO 10 J=1,3
   10    XXX(J,2)=RW(KVERT+LHEVT3+J)
         R=SQRT(XXX(1,2)**2+XXX(2,2)**2)
         IF(R.GT.RLIM.OR.ABS(XXX(3,2)).GT.ZLIM)THEN
            IFLAG=1
            GO TO 2
         ENDIF
    1    CONTINUE
         IFLAG=0
    2    CONTINUE
      ENDIF
      RETURN
      END
#endif
