*DK tpexmi
      SUBROUTINE TPEXMI(ZMAX,XEX)
C
C! Fast sim : Determine super-broken segment middle and extremities coor
C
C  Called from:    T2TRAN
C
C  Calls:          none
C
C  Inputs:   PASSED       --JSEGT,  num of current segment
C                         --NSEGT,  total number of segments
C
C  Outputs:  PASSED       --XEX,    useful segment coords.
C                          .XEX(1,J) :  beginning  coords (J=1,2,3,4)
C                          .XEX(2,J) :  middle coords     (J=1,2,3,4)
C                          .XEX(3,J) :  end coords        (J=1,2,3,4)
C                                 ( 1,2,3,4  <--> X,Y,Z,T )
C                          .XEX(4,J) :  num of electrons in 1st
C                                       (2nd) part : J=1(2).
C
C  P. Janot   11/15/87
C
#include "bcs.h"
#include "tpcbos.h"
#include "trakel.h"
#include "faster.h"
C
C  Segment bank index
C
      INDEX = 2 + IDCLUS + (JSEGT-1)*10
C
C  Now calculate the XEX(I,J), I,J = 1,2,3
C
 1    CONTINUE
      IF(JSEGT.EQ.1.OR.JSEGT.EQ.2) THEN
        XEX(1,1) = X(1)
        XEX(1,2) = X(2)
        XEX(1,3) = X(3)
        IF(XEX(1,3).GT. ZMAX) XEX(1,3) = ZMAX
        IF(XEX(1,3).LT.-ZMAX) XEX(1,3) =-ZMAX
        XEX(1,4) = TOF
        XEX(3,1) = RW(INDEX+1)
        XEX(3,2) = RW(INDEX+2)
        XEX(3,3) = RW(INDEX+3)
        XEX(3,4) = RW(INDEX+4)
        IF(JSEGT.EQ.1) THEN
           XEX(2,1) = XEX(1,1)
           XEX(2,2) = XEX(1,2)
           XEX(2,3) = XEX(1,3)
           XEX(2,4) = XEX(1,4)
           XEX(4,1) = 0.
           XEX(4,2) = IW(INDEX+9)
           GOTO 60
        ELSE
           XEX(2,1) = RW(INDEX-9)
           XEX(2,2) = RW(INDEX-8)
           XEX(2,3) = RW(INDEX-7)
           XEX(2,4) = RW(INDEX-6)
           XEX(4,1) = IW(INDEX-1)
           XEX(4,2) = IW(INDEX+9)
           GOTO 60
        ENDIF
      ELSEIF (JSEGT.EQ.NSEGT.AND.IW(INDEX+10).EQ.1) THEN
        XEX(1,1) =  RW(INDEX-9)
        XEX(1,2) =  RW(INDEX-8)
        XEX(1,3) =  RW(INDEX-7)
        XEX(1,4) =  RW(INDEX-6)
        XEX(3,1) =  RW(INDEX+1)
        XEX(3,2) =  RW(INDEX+2)
        XEX(3,3) =  RW(INDEX+3)
        XEX(3,4) =  RW(INDEX+4)
        XEX(2,1) =  XEX(3,1)
        XEX(2,2) =  XEX(3,2)
        XEX(2,3) =  XEX(3,3)
        XEX(2,4) =  XEX(3,4)
        XEX(4,1) = IW(INDEX+9)
        XEX(4,2) = 0.
        GOTO 60
      ELSE
        XEX(1,1) = RW(INDEX-19)
        XEX(1,2) = RW(INDEX-18)
        XEX(1,3) = RW(INDEX-17)
        XEX(1,4) = RW(INDEX-16)
        XEX(2,1) = RW(INDEX-9)
        XEX(2,2) = RW(INDEX-8)
        XEX(2,3) = RW(INDEX-7)
        XEX(2,4) = RW(INDEX-6)
        XEX(3,1) = RW(INDEX+1)
        XEX(3,2) = RW(INDEX+2)
        XEX(3,3) = RW(INDEX+3)
        XEX(3,4) = RW(INDEX+4)
        XEX(4,1) = IW(INDEX-1)
        XEX(4,2) = IW(INDEX+9)
      ENDIF
60    CONTINUE
      IF (IW(INDEX+10).EQ.0 .AND. IW(INDEX).EQ.0) XEX(4,1)=0.
      RETURN
      END
