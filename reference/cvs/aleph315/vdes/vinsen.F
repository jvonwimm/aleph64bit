      LOGICAL FUNCTION VINSEN (IVIEW,A,B)
C ----------------------------------------------------------------------
CKEY VDETDES WAFER / USER
C!  Checks whether a point (a,b) is within sensitive region of wafer
C - Steve Wasserbaech, October 1994
C
C - Input:
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C   A      / R  a coordinate (cm)
C   B      / R  b coordinate (cm)
C
C - Output:
C   VINSEN / L  = .TRUE. if (a,b) lies within sensitive region
C                 for view IVIEW;
C               = .FALSE. if IVIEW is invalid, or if point lies
C                 outside senstive region
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vwgeco.h"
#include "vdetge.h"
C
C     Arguments:
      INTEGER IVIEW
      REAL A, B
C
C ----------------------------------------------------------------------
C
      IF (IVIEW .EQ. VVIEWZ) THEN
C
C     z view:
        VINSEN = ((A .GT. AMNSRZ) .AND. (A .LT. AMXSRZ) .AND.
     >            (B .GT. BMNSRZ) .AND. (B .LT. BMXSRZ))
C
      ELSEIF (IVIEW .EQ. VVIEWP) THEN
C
C     r-phi view:
        VINSEN = ((A .GT. AMNSRP) .AND. (A .LT. AMXSRP) .AND.
     >            (B .GT. BMNSRP) .AND. (B .LT. BMXSRP))
C
      ELSE
C
C     invalid view:
        VINSEN = .FALSE.
C
      ENDIF
C
      RETURN
      END
#endif
