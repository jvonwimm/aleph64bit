      SUBROUTINE TCRTOF(FIELD,R,Z,ITK,BNAME,ZC)
C
C-----------------------------------------------------------------------
C! Correct TPC time for time-of-flight
C
CKEY TPC,TOF
C
C  Author:  R. Johnson     18-01-90
C  Modified by: F.Ranjard  24-02-92
C               call TPDVEL to get drift velocity
C
C  Input:   FIELD   /F       Magnitude of B-field
C           R       /F       Coordinate radius
C           Z       /F       Coordinate z
C           ITK     /I       Associated track (0 if none yet)
C           BNAME   /C       Name of track bank (TGFT or FRFT)
C  Output:  ZC      /F       Corrected z position
C
C-----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "tgftjj.h"
#include "alcons.h"
C
      SAVE
      CHARACTER BNAME*4
      DIMENSION S(2),PHE(2),ZE(2),DVA(3),DVB(3)
      DATA RMPI2/0.019477/
C
#include "bmacro.h"
C----------------------------------------------------------------
C
C++   Link to the drift velocity, if not found return without
C     correction
C
      CALL TPDVEL ('POT',DVA,DVB,IRET)
      IF (IRET.NE.0) GOTO 998
C
C++   If there is no associated track, assume a straight flight path
C++   from the origin and a velocity equal to speed of light
C
      IF (ITK.EQ.0) THEN
        D=SQRT(R**2+Z**2)
        V=CLGHT
      ELSE
        KTGFT=IW(NAMIND(BNAME))
        IF (KTGFT.EQ.0) THEN
          D=SQRT(R**2+Z**2)
          V=CLGHT
        ELSE
C
C++       Don't correct if the track is not coming from near the origin
C
          D0=RTABL(KTGFT,ITK,JTGFD0)
          IF (ABS(D0).GT.8.) goto 998
C
C++       Calculate the particle velocity, assuming it is a pion
C
          TL= RTABL(KTGFT,ITK,JTGFTL)
          RI= RTABL(KTGFT,ITK,JTGFIR)
          IF (RI.NE.0.) THEN
            RADC=1./RI
          ELSE
            RADC=1.E20
          ENDIF
          PT=RADC*CLGHT*FIELD/100000.
          SECL=SQRT(1.+TL**2)
          P=ABS(PT)*SECL
          V= (P/SQRT(P**2+RMPI2))*CLGHT
C
C++       Find the path length from the origin to the coordinate
C++       If the particle is not completely in the x-y plane, then
C++       this calculation is trivial.  Otherwise, we must be careful
C++       not to divide by a zero dip angle.
C
          IF (ABS(TL).LT.0.003) THEN
            CALL THLCIR(RW(KROW(KTGFT,ITK)+JTGFIR),R,S,PHE,ZE,IER)
            IF (IER.NE.0) THEN
              D=SQRT(R**2+Z**2)
            ELSE
              D=S(1)
            ENDIF
          ELSE
            Z0= RTABL(KTGFT,ITK,JTGFZ0)
            DZT= Z-Z0
            D= DZT*SECL/TL
          ENDIF
        ENDIF
      ENDIF
      TIME=D/V
      IF (Z.LT.0.) THEN
        ZC = Z - TIME*DVB(3)/1000.
      ELSE
        ZC = Z + TIME*DVA(3)/1000.
      ENDIF
C
      RETURN
C - some information is missing or track is not coming from
C   near the origin, return without correction
 998  CONTINUE
      ZC = Z
      END
#endif
