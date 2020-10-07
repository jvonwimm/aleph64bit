      SUBROUTINE ACOUPE(NPL,PL,SECTION,NCOINS,COINS)
C-----------------------------------------------------------------------
CKEY ECALDES SECTION AREA /USER
C     H.Videau      creation   Juin 90
C! Computes the contour of the section of a volume by a a plane
C  This routine computes the section of a convex volume
C  given by its NPL limiting planes PL by a plane SECTION
C   Input :
C           NPL         number of planes  <= 10       INTEGER
C           PL(4,NPL)   planes coefficients           REAL
C           SECTION(4)  section plane coefficients    REAL
C   Output:
C           NCOINS,COINS(3,NCOINS)
C   Calls: VECT4
C   Called by USER, ERWIDT
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
C   Input
      INTEGER NPL
      REAL PL(4,*),SECTION(4)
C  Output
      INTEGER NCOINS
      REAL COINS(3,*)
C    Locales
      INTEGER I,J,L,K
      INTEGER NPOINTS,PTPL(2,10),ORD(10)
      REAL COIN(4),POINTS(3,10)
C  Execution
      NPOINTS =0
      DO 1 I=1,NPL-1
        DO 2 J=I+1,NPL
          CALL VECT4(COIN,PL(1,I),PL(1,J),SECTION)
          IF(COIN(4).NE.0.) THEN
            COIN(1)=COIN(1)/COIN(4)
            COIN(2)=COIN(2)/COIN(4)
            COIN(3)=COIN(3)/COIN(4)
C             Test de positivite
            DO 4 L=1,NPL
              IF(L.NE.I.AND.L.NE.J)THEN
                IF(COIN(1)*PL(1,L)+COIN(2)*PL(2,L)+
     &               COIN(3)*PL(3,L)+PL(4,L).LT.0.) GO TO 2
              END IF
    4       CONTINUE
C                 C est un sommet
            NPOINTS=NPOINTS+1
            POINTS(1,NPOINTS)=COIN(1)
            POINTS(2,NPOINTS)=COIN(2)
            POINTS(3,NPOINTS)=COIN(3)
            PTPL(1,NPOINTS)=I
            PTPL(2,NPOINTS)=J
          END IF
    2   CONTINUE
    1 CONTINUE
C  Ordonnancement des points de la face
      ORD(1)=1
      K=PTPL(2,1)
      DO 12 I=2,NPOINTS
        DO 10 L=1,NPOINTS
          IF(L.EQ.ORD(I-1)) GO TO 10
          IF(PTPL(1,L).EQ.K) THEN
            ORD(I)=L
            K=PTPL(2,L)
            GO TO 12
          END IF
          IF(PTPL(2,L).EQ.K) THEN
            ORD(I)=L
            K=PTPL(1,L)
            GO TO 12
          END IF
   10   CONTINUE
   12 CONTINUE
C   mise en place des points
      NCOINS=NPOINTS
      DO 11 L=1,NPOINTS
        I=ORD(L)
        COINS(1,L)=POINTS(1,I)
        COINS(2,L)=POINTS(2,I)
        COINS(3,L)=POINTS(3,I)
   11 CONTINUE
      END
#endif
