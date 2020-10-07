      SUBROUTINE GEMFAK (NNGA,TABEM,TABOUT)
C----------------------------------------------------------------
C  M.Verderi                                            2-10-94
C! Main routine for the electromagnetic fake photon likelihood
C  computation.
C  Perfoms a double loops on the list of photons
C  tabem (1-17,i) are the inputs, tabout(1-6,i) the outputs
C  Input : TABEM ( 1,i) : Energy with gamma hyp. (Not used)
C                ( 2,i) : F4
C                ( 3,i) : Raw energy
C                ( 4,i) : Theta barycenter
C                ( 5,i) : Phi   barycenter
C                ( 6,i) : Germination stack
C                ( 7,i) : Peak storey stack 1 energy
C                ( 8,i) : Peak storey stack 2 energy
C                ( 9,i) : Prfak stack 1
c                (10,i) : Prfak stack 2
c                (11,i) : Raw energy of peak storey stack 1 cluster
c                         neighbour (<0 if not in gampex photons list)
c                (12,i) : Raw energy of peak storey stack 2 cluster
c                         neighbour (<0 if not in gampex photons list)
c                (13,i) : Raw energy of peak storey stack 3 cluster
c                         neighbour (<0 if not in gampex photons list)
c                (14,i) : Border End Cap flag
c                (15,i) : Energy of storeys in the four central towers
c                         sharing a face with storeys inside the four
c                         central towers of an other photon
c                (16,i) : Energy of storey in the four central towers
c                         having a face on a Ecal crack
c                (17,i) : gampex photon number
c
c  Output: TABOUT( 1,i) : warning flag = 100. outside acceptance
c                                             nothing done (cos>.95)
c                                      =  10. Overlap region
c                                      =   2. Crack in EnDCap
c                                      =   1. Crack in Barrel
c                                      =   0. o.k.
c              " ( 2,i) : raw energie of the parent
c                                      =   0.0 for "very unlikely
c                                              fake photons" that is
c                                              photons which don't
c                                              satisfy a prelimary
c                                              criterion.
c                                              In this case no
c                                              computation is done
c              " ( 3,i) : electromagnetic fake photon likelihood
c
c              " (> 3,i): not given in the GAMFAK output
c              " ( 4,i): angular distance expressed at pi/2 between
c                        the two photons barycenters
c              " ( 5,i): relative angle between the two photons
c              " ( 6,i): flag = 1. if at least one peak storey
c                        has an neigbhour not in cluster
c                             = 11. if the other cluster verifies also
c                        this condition with the photon
c
c  Called by GAMPEX
c----------------------------------------------------------------
#ifndef DOC
      INTEGER NNGA,NVARF,NOUT
      PARAMETER (NVARF=16,NOUT=6)
      REAL    TABFAK (NVARF+1+NOUT,100), FAKTAB (NVARF,2)
      REAL    TABEM  (NVARF+1,*), TABOUT (NOUT,*)
      REAL    PFAKE, WARN, ERAW
      REAL    DISTANG, ANGLE12, ECLFL
      INTEGER I,J

      DO I = 1,NNGA
         DO J = 1,NVARF+1
            TABFAK(J,I) = TABEM(J,I)
         ENDDO
         TABFAK(18,I) = 0.0
         TABFAK(19,I) = 0.0
         TABFAK(20,I) = 0.0
         TABFAK(21,I) = 0.0
         TABFAK(22,I) = 0.0
         TABFAK(23,I) = 0.0
      ENDDO
C Sort photons according to their raw energy:
      CALL SORTRQ (TABFAK,NVARF+1+NOUT,NNGA,-3)
C Double loops: index i is for the parent, j for the fake
      DO I = 1,NNGA
         CALL UCOPY (TABFAK(1,I),FAKTAB(1,1),NVARF)
         DO 10 J = I+1,NNGA
            CALL UCOPY (TABFAK(1,J),FAKTAB(1,2),NVARF)
            CALL GFAKE (FAKTAB,PFAKE,WARN,ERAW,DISTANG,ANGLE12,ECLFL)
C Outside acceptance: warn = 100., pfake = -1.0
            IF (WARN.GT.99.) THEN
               TABFAK(18,J) = WARN
               TABFAK(20,J) = PFAKE
               GOTO 10
            ELSE
C Takes highest proba.:
               IF (PFAKE.GT.TABFAK(20,J)) THEN
                  TABFAK(18,J) = WARN
                  TABFAK(19,J) = ERAW
                  TABFAK(20,J) = PFAKE
                  TABFAK(21,J) = DISTANG
                  TABFAK(22,J) = ANGLE12
                  TABFAK(23,J) = ECLFL
               ENDIF
            ENDIF
10       CONTINUE
      ENDDO

C Reorder the array:
      CALL SORTRQ (TABFAK,NVARF+1+NOUT,NNGA,+17)
      DO I = 1,NNGA
          DO J = 1,NOUT
             TABOUT(J,I) = TABFAK(NVARF+1+J,I)
          ENDDO
      ENDDO
      END
#endif
