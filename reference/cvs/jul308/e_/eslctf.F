      SUBROUTINE ESLCTF (XXX,NOTRAC,IFLAG)
C***********************************************************************
C!SELECT TRACKS                                                        *
C!                                                                     *
C!                                                                     *
C!  AUTHOR   : ALAIN BONISSENT/DENIS COURVOISIER                       *
C!  MODIFIED : DENIS COURVOISIER   89/03/24                            *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : FKIN,FVER                                              *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!    INPUT  :  NOTRAC= FKIN TRACK NUMBER                              *
C!                                                                     *
C!    OUTPUT : XXX(3,2) = COORDINATES OF FIRST AND LAST VERTEX         *
C!             IFLAG : SEE BELOW                                       *
C!                                                                     *
C!     IF FIRST VERTEX IN TPC (ECAL-10 CM) AND LAST VERTEX IN ECAL THEN*
C!         IFLAG= 1                                                    *
C!     ELSE                                                            *
C!         IFLAG= 0                                                    *
C!     ENDIF                                                           *
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "fkinjj.h"
#include "fverjj.h"
C
      DIMENSION XXX(3,2)
      DIMENSION R(2)
#include "etpgo.h"
#include "bmacro.h"
C
      CALL VZERO(R,2)
      CALL VZERO(XXX,6)
      IFLAG= 0
      KFKIN= NLINK('FKIN',0)
      KFVER= NLINK('FVER',0)
      IF (KFKIN.LE.0.OR.KFVER.LE.0) GO TO 999
C        first vertex number of track NOTRAC
      IV = ITABL(KFKIN,NOTRAC,JFKIOV)
C        last vertex number of track NOTRAC
      IV2= ITABL(KFKIN,NOTRAC,JFKIEV)
C        components of the vertices
      DO 30 J=1,3
      IF (IV2.GT.0) XXX(J,2)= RTABL(KFVER,IV2,JFVEVX+J-1)
   30 XXX(J,1)= RTABL(KFVER,IV,JFVEVX+J-1)
      R(1)= SQRT(XXX(1,1)**2+XXX(2,1)**2)
      IF (IV2.GT.0) R(2)= SQRT(XXX(1,2)**2+XXX(2,2)**2)
      RLIM=RTPMX-10.
      ZLIM=ZTPMX-10.
      IF(R(1).GT.RLIM.OR.ABS(XXX(3,1)).GT.ZLIM)THEN
         IFLAG= 0
      ELSEIF (IV2.LE.0) THEN
         IFLAG= 1
      ELSEIF (IV2.GT.0) THEN
         IF (R(2).GT.RTPMX.OR.ABS(XXX(3,2)).GT.ZTPMX) THEN
             IFLAG= 1
         ELSE
             IFLAG= 0
         ENDIF
      ENDIF
      RETURN
 999  CALL RERROR ('ESLCTF',1,' NO FKIN or FVER banks')
      RETURN
      END
#endif
