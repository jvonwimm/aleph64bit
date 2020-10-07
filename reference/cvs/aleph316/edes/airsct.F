        REAL FUNCTION AIRSCT(NPL,PL,SECTION)
C-----------------------------------------------------------------------
CKEY ECALDES SECTION AREA / USER
C     H.Videau      creation   Juin 89
C! Area of a polygon section
C  This function computes the area of the section of a convex volume
C  given by its NPL limiting planes PL by a plane SECTION
C   Input :
C           NPL         number of planes  <= 10       INTEGER
C           PL(4,NPL)   planes coefficients           REAL
C           SECTION(4)  section plane coefficients    REAL
C   Output:
C           AIRSCT
C   Calls: VECT4
C   Called by USER, ERPADS
C----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
C   Input
         INTEGER NPL
         REAL PL(4,*),SECTION(4)
C    Locales
        INTEGER I,J,L,K
        INTEGER NPOINTS,PTPL(2,10),ORD(10)
        REAL COIN(4),POINTS(3,10),VECT(3)
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
     &               COIN(3)*PL(3,L)+PL(4,L).GT.0.) GO TO 2
                END IF
 4            CONTINUE
C                 C est un sommet
                  NPOINTS=NPOINTS+1
                  POINTS(1,NPOINTS)=COIN(1)
                  POINTS(2,NPOINTS)=COIN(2)
                  POINTS(3,NPOINTS)=COIN(3)
                  PTPL(1,NPOINTS)=I
                  PTPL(2,NPOINTS)=J
              END IF
 2        CONTINUE
 1      CONTINUE
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
 10     CONTINUE
 12     CONTINUE
        ORD(NPOINTS+1)=1
C   calcul de l'aire.
        VECT(1)=0.
        VECT(2)=0.
        VECT(3)=0.
        DO 11 L=1,NPOINTS
        I=ORD(L)
        J=ORD(L+1)
        VECT(1)=VECT(1)+POINTS(2,I)*POINTS(3,J)-POINTS(2,J)*POINTS(3,I)
        VECT(2)=VECT(2)+POINTS(3,I)*POINTS(1,J)-POINTS(3,J)*POINTS(1,I)
        VECT(3)=VECT(3)+POINTS(1,I)*POINTS(2,J)-POINTS(1,J)*POINTS(2,I)
 11     CONTINUE
        AIRSCT=ABS(.5*(VECT(1)*SECTION(1)+VECT(2)*SECTION(2)+
     &               VECT(3)*SECTION(3)))
        END
#endif
