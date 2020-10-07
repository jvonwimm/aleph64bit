      SUBROUTINE CTKCHG(ITRK,ITRKL,ICHG,IER)
C
C***********************************************************************
C!Find charge of primary particle connected with track itrk            *
C!                                                                     *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870615                                    *
C!                                                                     *
C!  Itrk  = Track number in geant (primary or secondary)               *
C!  Itrkl = Track number of the primary track                          *
C!  Ichg  = charge of the primary                                      *
C!  IER  = return code  0 : normal                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : KINE,VERT,PART                                         *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!                                                                     *
C***********************************************************************
#ifndef DOC
#include "rlunit.h"
C
#include "bcs.h"
      DIMENSION XXX(3,2)
#include "bmacro.h"
      KPART=NLINK('PART',0)
      LHEV3=3
      ITRKL=ITRK
   10 CONTINUE
      KKINE=NLINK('KINE',ITRKL)
C?
C?  GET NUMBER OF VERTICES ON THE TRACK
C?
      NVERT=IW(KKINE+LHEV3)
C?
C?  ESLCT WILL TELL IF TRACK IS PRIMARY (1) OR SECONDARY(0)
C?
      CALL ESLCT(XXX,NVERT,KKINE,IFLAG)
      IF(IFLAG.EQ.1)GO TO 30
C
C?  Get index of creation vertex
C
      LKINE=IW(KKINE+2)
      IVC=IW(KKINE+LHEV3+LKINE+1)
      IF(IVC.LE.1)GO TO 998
C
C?  get incoming track number
C
      KVERT=NLINK('VERT',IVC)
      LPV=IW(KVERT+2)
      ITRKL=IW(KVERT+LHEV3+LPV)
      GO TO 10
   30 CONTINUE
C
C   GET PARTICLE NUMBER AND CHARGE
C
      IPART=IW(KKINE+LHEV3+5)
      ICHG=RTABL(KPART,IPART,7)
      IER=0
      RETURN
  998 CONTINUE
      CALL RERROR(' CTKCHG',1,' track ITRKL DOES NOT EXIT TPC')
C      WRITE(LDEBRL,*)' NVERT = ',NVERT
C      WRITE(LDEBRL,*)' XXX(3,2) :',XXX
      IER=1
      IFLAG = 4
      RETURN
      END
#endif
