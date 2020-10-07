      SUBROUTINE ERWIDT(LONG,LARGE)
C-----------------------------------------------------------------------
CKEY ECALDES SECTION AREA  / USER
C     H.Videau      creation   Juin 90
C! Computes the height and width (at mid height) of the 45 storeys
C of stack 1, column 1, rows 1 to 45.
C   Input : none
C   Output:
C           LONG(45) :  height    REAL
C           LARGE(45):  width     REAL
C   Calls: ESRBC, ESRPL, ACOUPE
C   Called by USER
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
C    Locales
      INTEGER ROW,COL,ST,LEPLAN,NCOINS,I
      REAL POINT(4),PLAN(4),PLANES(4,6),COINS(3,4),MILIEU(3,4)
      REAL LO,LA
C     Output
      REAL LONG(*),LARGE(*)
C
      ST=1
      COL = 1
      DO 1 ROW=1,45
        CALL ESRBC('ALEPH',ROW,COL,ST,POINT)
        PLAN(1)=POINT(1)
        PLAN(2)=POINT(2)
        PLAN(3)=POINT(3)
        PLAN(4)=-(POINT(1)**2+POINT(2)**2+POINT(3)**2)
        CALL ESRPL('ALEPH',ROW,COL,ST,LEPLAN,PLANES)
        CALL ACOUPE(LEPLAN,PLANES,PLAN,NCOINS,COINS)
        LO=0.
        LA=0.
        DO 2 I=1,3
          MILIEU(I,1)=(COINS(I,1)+COINS(I,2))/2.
          MILIEU(I,2)=(COINS(I,2)+COINS(I,3))/2.
          MILIEU(I,3)=(COINS(I,3)+COINS(I,4))/2.
          MILIEU(I,4)=(COINS(I,4)+COINS(I,1))/2.
          LO=LO+(MILIEU(I,3)-MILIEU(I,1))**2
          LA=LA+(MILIEU(I,4)-MILIEU(I,2))**2
    2   CONTINUE
        LONG(ROW)=SQRT(LO)
        LARGE(ROW)=SQRT(LA)
    1 CONTINUE
      END
#endif
