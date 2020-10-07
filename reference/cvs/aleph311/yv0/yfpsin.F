        SUBROUTINE YFPSIN(KAPOI,KBPOI,VERT,
     $  PSIA,PSIB,COOR1,COOR2,IND,CHIT1)
C-------------------------------------------------
C! Find the psi angles of two track at the point of closest approach
CKEY YV0 ANGLE DISTANCE / INTERNAL
C
C      AUTHOR  : M.A.CIOCCI,L.ROLANDI 23/2/88
C      MODIFIED: M.A.CIOCCI 23/3/89
C
C      INPUT:
C           KAPOI/I   :Number of the first current track
C                      in the frft bank.
C           KBPOI/I   :Number of the second current track
C                      in the frft bank.
C           VERT/R    : beam-beam crossing point
C
C
C
C      OUTPUT:
C           PSIA/R    :Angle psi for the first current track
C                      where the two helices have intersection
C                      on the X-Y plane.
C           PSIB/R    :Angle psi for the second current track
C                      where the two helices have intersection
C                      on the X-Y plane.
C
C           COOR1/R   :The point coordinates on the first helix,
C                      when the distance on the X-Y plane is
C                      minimum.
C
C           COOR2/R   :the point coordinate on the second helix,
C                      when the distance on the X-Y plane is
C                      minimum.
C
C           IND/I     :type of intersection
C                      if ind=6 the two circles are one inside the
C                      other with the same center (problem without
C                      solution)
C                      if ind=7 no solution with psi>0
C           CHIT1/R   :Minimum of distance on space between helices
C
C      BANKS:
C            FRFT
C
C
C                  DESCRIPTION
C                  ===========
C        Finds the coordinate minimizing the distance
C        on x-y plane between helices (YDISMC), define the respective
C        psi angles (mod twopi) calling Yfixyp, define psi phases (and so psi)
C        minimizing also the z distance (see also the aleph note about v0)
C        When the projected helices have two points of intersection
C        there are  three solutions -> six psi angles (see aleph notes).
C        In this case: calculates the psi phases for any possible solution
C        respect to the beam beam crossing (vert) calling ypsive.
C        The solution will be (if ther'is..) that one with both psi>0
C        and minimum distance on the space
C
C
C       CALLED: YMFV0V
C
C       CALLS:  YDISMC,YZPHAS,YFIXYP,YPSIVE
C
C-------------------------------------------------------------
#ifndef DOC
      SAVE
      EXTERNAL YFIXYP
#include "bcs.h"
#include "frftjj.h"
C
C
      EXTERNAL NLINK,NAMIND,NBANK,CHAINT,INTCHA,NDROP
      CHARACTER*4 CHAINT
      INTEGER NLINK,NAMIND,NBANK,INTCHA,NDROP
C
        REAL ELIP1(5),ELIP2(5),PSS(3,2),DSS(3),XDIS(3)
        REAL COOR1(3),COOR2(3),VERT(3)
        INTEGER ISOL(3)
      INTEGER NFRFT
      DATA NFRFT / 0 /
#include "bmacro.h"
C
C+      ELIP1,ELIP2: THE FIVE PARAMETERS OF THE TWO HELICES
C+      AS READ  FROM THE FRFT BANK
C
      IF ( NFRFT .EQ. 0 ) THEN
        NFRFT = NAMIND( 'FRFT' )
      ENDIF
      KFRFT = IW( NFRFT )
           DO 10 IPAR=1,5
           ELIP1(IPAR)=RTABL(KFRFT,KAPOI,JFRFIR-1+IPAR)
           ELIP2(IPAR)=RTABL(KFRFT,KBPOI,JFRFIR-1+IPAR)
  10   CONTINUE
C
C+       HERE DEFINE THE SIX NEW PARAMETERS OF
C+       OF THE TWO HELICES
C
       CF=-ELIP1(1)/ABS(ELIP1(1))
       CS=-ELIP2(1)/ABS(ELIP2(1))
       RHOF=ABS(1./ELIP1(1))
       RHOS=ABS(1./ELIP2(1))
       D0F=-CF*ELIP1(4)
       D0S=-CS*ELIP2(4)
       XKF=-CF/(RHOF-D0F)
       XKS=-CS/(RHOS-D0S)
       XLAMF=ELIP1(2)
       XLAMS=ELIP2(2)
       FI0F=ELIP1(3)
       FI0S=ELIP2(3)
       Z0F=ELIP1(5)
       Z0S=ELIP2(5)
C
C
C+        GIVEN THE TWO TRACKS, FIRST AND SECOND,
C+        FINDS THE X AND Y COORDINATES AT THE
C+        INTERSECTION POINT IN THE X-Y PLANE
C
         CALL YDISMC(XKF,D0F,FI0F,Z0F,XLAMF,CF,
     $                    XKS,D0S,FI0S,Z0S,XLAMS,CS,
     $                    XM1,YM1,XM2,YM2,XM3,YM3,XM4,YM4,IND)
C
C
C+        IF THE TWO CIRCLES DO INTERSECT,IND>0,
C+        AND (XM1,YM1),(XM2,YM2) ARE THE COORDINATES
C+        OF THE TWO INTERSECTION POINTS, ON THE X-Y
C+        PLANE
C
C+        IF IND<0 THE TWO CIRCLES DO NOT INTERSECT
C+        AND (XM1,YM1) ARE  THE COORDINATES OF THE FIRST
C+        TRACK AND (XM2,YM2) ARE THE COORDINATES OF THE
C+        SECOND TRACK AT MINIMUM DISTANCE
C+        ON THE X-Y PLANE
C
C+        IF IND =0 THE TWO CIRCLES DO INTERSECT
C+        IN ONE POINT THAT IS STORED IN BOTH THE
C+        ADRESSES
C
C+      IF(IND.EQ.6) THE TWO CIRCLES ARE ONE INSIDE THE OTHER
C+      WITH THE SAME CENTER (PROBLEM WITHOUT SOLUTION)
C
        IF(IND.EQ.6)GO TO 30
C
        IF (IND.LE.0) THEN
C+      THIS IS THE CASE OF TWO NOT INTERSECTING
C+      CIRCLES
C
C
C+      FINDS THE RESPECTIVES Z COORDINATES
C
        PSIA=YFIXYP(CF,XM1,YM1,RHOF,FI0F,D0F)
        PSIB=YFIXYP(CS,XM2,YM2,RHOS,FI0S,D0S)
        CALL YZPHAS(Z0F,Z0S,PSIA,PSIB,RHOF*XLAMF,
     $              RHOS*XLAMS,DISM)
C
C+      FINDS THE TWO POINTS COORDINATES IN THE SPACE
C
        COOR1(1)=XM1
        COOR1(2)=YM1
        COOR1(3)=Z0F+RHOF*PSIA*XLAMF
        COOR2(1)=XM2
        COOR2(2)=YM2
        COOR2(3)=Z0S+RHOS*PSIB*XLAMS
        CHIT1=VDIST(COOR1,COOR2,3)
        GOTO 30
        ENDIF
C
C++     THIS IS THE CASE OF TWO INTERSECTING CIRCLES
C
C  Calculates the psi phases for any possible solution respect
C  to the beam beam crossing (vert).
C  In this case we have three solutions -> six phases
C  (see aleph notes).
C  The solution will be (if ther'is..) that one with both psi>0
C  and minimum distance on the space
C
             IF (IND.EQ.1) THEN
         PSI11=YFIXYP(CF,XM1,YM1,RHOF,FI0F,D0F)
         PSI12=YFIXYP(CS,XM1,YM1,RHOS,FI0S,D0S)
         PSI21=YFIXYP(CF,XM2,YM2,RHOF,FI0F,D0F)
         PSI22=YFIXYP(CS,XM2,YM2,RHOS,FI0S,D0S)
         CALL YZPHAS(Z0F,Z0S,PSI11,PSI12,RHOF*XLAMF,
     $               RHOS*XLAMS,DISM1)
        CALL YPSIVE(VERT,KAPOI,PSI1N)
        PS1NEW=PSI11-PSI1N
        CALL YPSIVE(VERT,KBPOI,PSI2N)
        PS2NEW=PSI12-PSI2N
         CALL YZPHAS(Z0F,Z0S,PSI21,PSI22,RHOF*XLAMF,
     $               RHOS*XLAMS,DISM2)
        CALL YPSIVE(VERT,KAPOI,PSI3N)
        PS3NEW=PSI21-PSI3N
        CALL YPSIVE(VERT,KBPOI,PSI4N)
        PS4NEW=PSI22-PSI4N
        PSIC=YFIXYP(CF,XM3,YM3,RHOF,FI0F,D0F)
        PSID=YFIXYP(CS,XM4,YM4,RHOS,FI0S,D0S)
        CALL YZPHAS(Z0F,Z0S,PSIC,PSID,RHOF*XLAMF,
     $              RHOS*XLAMS,DISM)
        CALL YPSIVE(VERT,KAPOI,PSI5N)
        PS5NEW=PSIC-PSI5N
        CALL YPSIVE(VERT,KBPOI,PSI6N)
        PS6NEW=PSID-PSI6N
        DISM3=SQRT((XM3-XM4)**2+(YM3-YM4)**2+DISM**2)
           IF((PS1NEW.GE.0..AND.PS2NEW.GE.0.).OR.
     $        (PS3NEW.GE.0..AND.PS4NEW.GE.0.).OR.
     $        (PS5NEW.GE.0..AND.PS6NEW.GE.0.))THEN
           PSS(1,1)=PS1NEW
           PSS(1,2)=PS2NEW
           PSS(2,1)=PS3NEW
           PSS(2,2)=PS4NEW
           PSS(3,1)=PS5NEW
           PSS(3,2)=PS6NEW
           DSS(1)=DISM1
           DSS(2)=DISM2
           DSS(3)=DISM3
              NSOL=0
            DO 31 I=1,3
              IF(PSS(I,1).GE.0..AND.PSS(I,2).GE.0.)THEN
                NSOL=NSOL+1
              ISOL(NSOL)=I
              XDIS(NSOL)=DSS(I)
              ENDIF
   31       CONTINUE
            ITSOL=LVMINA(XDIS,NSOL)
            NTSOL=ISOL(ITSOL)
             IF(NTSOL.EQ.1)THEN
         PSIA=PSI11
         PSIB=PSI12
         COOR1(1)=XM1
         COOR1(2)=YM1
         COOR1(3)=Z0F+RHOF*XLAMF*PSIA
         COOR2(1)=XM1
         COOR2(2)=YM1
         COOR2(3)=Z0S+RHOS*XLAMS*PSIB
         CHIT1=VDIST(COOR1,COOR2,3)
          GO TO 30
          ENDIF
             IF(NTSOL.EQ.2)THEN
         PSIA=PSI21
         PSIB=PSI22
         COOR1(1)=XM2
         COOR1(2)=YM2
         COOR1(3)=Z0F+RHOF*XLAMF*PSIA
         COOR2(1)=XM2
         COOR2(2)=YM2
         COOR2(3)=Z0S+RHOS*XLAMS*PSIB
         CHIT1=VDIST(COOR1,COOR2,3)
          GO TO 30
         ENDIF
             IF(NTSOL.EQ.3)THEN
        PSIA=PSIC
        PSIB=PSID
        COOR1(1)=XM3
        COOR1(2)=YM3
        COOR1(3)=Z0F+RHOF*PSIC*XLAMF
        COOR2(1)=XM4
        COOR2(2)=YM4
        COOR2(3)=Z0S+RHOS*PSID*XLAMS
        CHIT1=DISM3
        IND=2
         GO TO 30
          ENDIF
            ELSE
            IND=7
             GO TO 30
           ENDIF
         ENDIF
C
30      CONTINUE
         RETURN
         END
#endif
