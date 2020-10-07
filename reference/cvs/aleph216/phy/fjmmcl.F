      SUBROUTINE FJMMCL( YCUT, ENORM, SCHEME, VERSN, MASMAT, PP,
     &                   NPAR, PPARX, PPARY,  PPARZ, PPARE,  NJETMX,
     &                   NJET, PJETX, PJETY,  PJETZ, PJETE,  JNOFP  )
C----------------------------------------------------------------------
CKEY PHYSICS JET JADE
C   Author   : C. Bowdery      9-OCT-1990  Faster algorithm than SJMMCL
C   Modified : M.N. Minard    20-FEB-1991  COMMON replaced by arguments
C   Modified : C. Bowdery     18-APR-1991  SCHEME argument added
C   Modified : C. Bowdery     28-APR-1991  VERSN argument added
C   Modified : C. Bowdery     18-OCT-1991  Name change for ALEPHLIB
C   Modified : C. Bowdery     21-OCT-1991  More argument changes
C   Modified : O. Callot      20-DEC-1995  Performance by linear search
C
C   Description
C   ===========
C!   Find jets using the JADE scaled invariant mass
C!   squared algorithm using BOS work bank.
C!   Uses E, E0 or P scheme.
C!   Uses standard ('NORMAL') or improved ('BETTER') JADE algorithm
C!   according to VERSN argument.
C
C
C  Input   : YCUT            ycut value ( (M/ENORM)**2 )
C            ENORM           normalising energy (if 0.0, use visible E)
C            SCHEME          CHAR*2, either 'E ', 'E0', 'P '
C            VERSN           CHAR*6, either 'NORMAL' or 'BETTER'
C            MASMAT          (empty) invariant mass**2 square matrix
C            PP              (empty) matrix for temporary particle usage
C            NPAR            number of particles
C            PPARX,Y,Z,E(i)  four momentum of input particles (i=1,NPAR)
C            NJETMX          maximum number of jets there is space for
C
C  Output  : NJET       +ve  number of jets (normal return)
C                        -1  input  error (NPAR/ENORM)
C                        -2  error from individ. track
C                        -3  too many jets
C                        -4  unknown particle combination scheme wanted
C                        -5  neither NORMAL nor BETTER algorithm wanted!
C            PJETX,Y,Z,E(j)  four  momentum of jet j (j=1,NJET)
C            JNOFP(i)        jet no. of particle i   (i=1,NPAR)
C======================================================================
#ifndef DOC
C
C     IMPLICIT   NONE
      SAVE
C
      INTEGER    NJETMX, NPAR, NJET, JNOFP(*)
      INTEGER    MINPA1, MINPA2, I, J, K, LOOP
C
      REAL       YCUT, ENORM, ETOT2, ETOT, EVIS
      REAL       PPARX(*), PPARY(*), PPARZ(*), PPARE(*)
      REAL       PJETX(*), PJETY(*), PJETZ(*), PJETE(*)
C
      REAL       MASMAT(NPAR,*), PP(5,*), MASS, MINMAS, YIJMIN
      REAL       DEAD, ZERO, ONE, TWO, START, EIJ
C
      LOGICAL    NORMAL
C
      CHARACTER  SCHEME*2, VERSN*6
C
      PARAMETER  ( DEAD  =  1.1E20, ZERO = 0.0E0 )
      PARAMETER  ( START =  1.0E20, ONE  = 1.0E0, TWO = 2.0E0 )
C
      INTEGER     MXPART
      PARAMETER  (MXPART = 1000)
      REAL        MINMLI(MXPART), WRKMLI
      INTEGER     MININD(MXPART), MA1, MA2
C-----------------------------------------------------------------------
C
C                      M = 2 * EIJ * ( 1 - COS THETA(I,J) )
C
C                  where EIJ = E(I) * E(J)        normal JADE algorithm
C                            = MIN(E(I),E(J))**2  better algorithm
C
      REAL       MASSF
      INTEGER    IA, JA
C
      MASSF( IA, JA ) =   TWO *  EIJ  *
     &                    (ONE - ( PP(1,IA) * PP(1,JA) +
     &                             PP(2,IA) * PP(2,JA) +
     &                             PP(3,IA) * PP(3,JA)   ) /
     &                            (PP(5,IA) * PP(5,JA))      )
C
C-----------------------------------------------------------------------
C
C                            Check for allowed input numbers
C
      IF( NPAR .LT. 1 ) THEN
        NJET = -1
C
      ELSEIF( SCHEME .NE. 'E'  .AND.  SCHEME .NE. 'E0'  .AND.
     &        SCHEME .NE. 'P' ) THEN
        NJET = -4
C
      ELSEIF( VERSN  .NE. 'NORMAL'  .AND.  VERSN  .NE. 'BETTER'  .AND.
     &        VERSN  .NE. 'normal'  .AND.  VERSN  .NE. 'better' ) THEN
        NJET = -5
      ELSEIF ( NPAR .GT. MXPART ) THEN
        NJET = -1
      ELSE
C
C                            Ensure MINPA1 and MINPA2 defined
C                            just in case NPAR=1 and YCUT is huge
C
        MINPA1 = 1
        MINPA2 = 1
        MA1 = 0
        MA2 = 0
        LOOP   = 0
C
        NORMAL = ( VERSN .EQ. 'NORMAL'  .OR.  VERSN .EQ. 'normal' )
C
        DO  10  I = 1, NJETMX
          PJETX(I) = 0.0
          PJETY(I) = 0.0
          PJETZ(I) = 0.0
          PJETE(I) = 0.0
   10   CONTINUE
C
        EVIS = ZERO
C
C                            Copy input momentum components to working
C                            space. Set jet assignment to point at self
C
        DO  20  I = 1, NPAR
          JNOFP(I) = I
          PP(1,I)  = PPARX(I)
          PP(2,I)  = PPARY(I)
          PP(3,I)  = PPARZ(I)
          PP(4,I)  = PPARE(I)
          PP(5,I)  = SQRT( PP(1,I)**2 + PP(2,I)**2 + PP(3,I)**2 )
          EVIS     = EVIS + PP(4,I)
   20   CONTINUE
C
        IF( ENORM .EQ. ZERO ) THEN
          ETOT = EVIS
        ELSE
          ETOT = ENORM
        ENDIF
        ETOT2 = ETOT**2
C
        IF( ETOT2 .LE. ZERO ) THEN
          NJET = -1
          GOTO 999
        ENDIF
C
C                            Detect bad input (divide checks!)
C
        DO  30  I = 1,NPAR
C
C                            WRITE(6,8880) I,(PP(KKK,I),KKK=1,5)
C8880                        FORMAT(' PARTICLE:',I3,5G13.5)
C
          IF( PP(5,I) .LE. ZERO ) THEN
            NJET = -2
            GOTO 999
          ENDIF
   30   CONTINUE
C
C                            Compute a table of invariant mass**2
C                            combinations of all particle pairs.
C                            Diagonal elements and repeats not needed!
C
        DO  50  I = 2,NPAR
          WRKMLI = START
          MININD(I) = 1
          DO  40  J = 1,I-1
C
            IF( NORMAL ) THEN
              EIJ = PP(4,I) * PP(4,J)
            ELSE
              EIJ = MIN( PP(4,I), PP(4,J) )**2
            ENDIF
C
            MASMAT(J,I) =  MASSF( I, J )
            IF( MASMAT(J,I) .LT. WRKMLI ) THEN
              WRKMLI = MASMAT(J,I)
              MININD(I) = J
            ENDIF
C
   40     CONTINUE
          MINMLI(I) = WRKMLI
   50   CONTINUE
C
C                            Set up starting minimum invariant mass**2
C
  100   MINMAS = START
        LOOP   = LOOP + 1
C
C                            Find the lowest invariant mass**2 but
C                            ignore killed combinations (DEAD).
C                            Note a temporary copy of MASMAT(I,J) is
C                            not used since the optimisation makes
C                            use of registers and is faster.
C
        DO  2000  I = 2,NPAR
          IF( PP(5,I) .GT. ZERO ) THEN
            IF ( MINMLI(I) .LT. MINMAS ) THEN
              MINMAS = MINMLI(I)
              MINPA1 = MININD(I)
              MINPA2 = I
            ENDIF
          ENDIF
 2000   CONTINUE
C
C                            Compute YMIN for the found pair
C
        YIJMIN = MINMAS / ETOT2
C
C                     WRITE(6,8881) LOOP, MINMAS, YIJMIN,MINPA1, MINPA2
C8881                 FORMAT(3X,I5,2X,2G13.5,2(1X,I3))
C
        IF( YIJMIN .LE. YCUT  .AND.  LOOP .LT. NPAR ) THEN
C
C                            The particle with the higher position
C                            index is killed and its JNOFP element is
C                            set to the index of the merged particle.
C
          JNOFP(MINPA2) = MINPA1
C
C                            Add the momentum components together of
C                            the pair to make the merged particle.
C                            Kill the other particle of the pair.
C
          CALL JPCOMB( SCHEME, PP, MINPA1, MINPA2 )
C
C                            Divide check protection and JPCOMB check
C
          IF( PP(5,MINPA1) .LE. ZERO ) THEN
            NJET = -2
            GO TO 999
          ENDIF
C
C                            Recompute the invariant mass**2's for the
C                            new merged particle with all the others.
C                            Combinations with killed particles would
C                            give zero mass but this is set to DEAD
C
          DO  4000  J = 1,NPAR
C
            IF( PP(5,J) .LT. ZERO ) THEN
C
              MASS = DEAD
C
            ELSE
C
              IF( NORMAL ) THEN
                EIJ = PP(4,MINPA1) * PP(4,J)
              ELSE
                EIJ = MIN( PP(4,MINPA1), PP(4,J) )**2
              ENDIF
C
              MASS  =   MASSF( MINPA1, J )
C
            ENDIF
C
C                            Only replace those entries that lie in
C                            the used half of the table.
C
            MA1 = 0
            IF ( J .LT. MINPA1 ) THEN
              MA1 = MINPA1
              MA2 = J
            ELSEIF( J .GT. MINPA1 ) THEN
              MA1 = J
              MA2 = MINPA1
            ENDIF
C
C?   Need to recompute min/index if new mass < old min or
C?   if prev mass = current min
C
            IF ( MA1 .NE. 0 ) THEN
              MASMAT( MA2, MA1 ) = MASS
              IF ( MASS .LT. MINMLI(MA1) ) THEN
                MINMLI(MA1) = MASS
                MININD(MA1) = MA2
              ELSEIF( MA2 .EQ. MININD( MA1 ) .AND.
     &                MINMLI(MA1) .LT. START ) THEN
                MINMLI(MA1) = MASS
                DO 4010 I = 1, MA1-1
                  IF ( MASMAT(I,MA1) .LT. MINMLI(MA1) ) THEN
                    MINMLI(MA1) = MASMAT(I,MA1)
                    MININD(MA1) = I
                  ENDIF
 4010           CONTINUE
              ENDIF
            ENDIF
C
C?   Now for MINPA2, which becomes dead : Change only if was mini
C
            IF ( J .LT. MINPA2 ) THEN
              MASMAT( J, MINPA2 ) = DEAD
              MINMLI( MINPA2)     = DEAD
            ELSEIF( J .GT. MINPA2 ) THEN
              MASMAT( MINPA2, J ) = DEAD
              IF( MINPA2 .EQ. MININD( J ) .AND.
     &            MINMLI(J) .LT. START       ) THEN
                MINMLI(J) = START
                DO 4020 I = 1, J-1
                  IF ( MASMAT(I,J) .LT. MINMLI(J) ) THEN
                    MINMLI(J) = MASMAT(I,J)
                    MININD(J) = I
                  ENDIF
 4020           CONTINUE
              ENDIF
            ENDIF
 4000     CONTINUE
C
C                            Go back and find the next lowest mass pair
C
          GOTO 100
C
        ELSE
C
C                            No more jets, fill PJETX/Y/Z/E and JNOFP
C                            JNOFP(ipart) = associated jet #,
C                            (even when only 1 part. in "jet")
C
C
          NJET = 0
          DO  8010  J = 1,NPAR
C
            IF( JNOFP(J) .EQ. J ) THEN
              NJET = NJET + 1
              IF( NJET .GT. NJETMX ) THEN
                NJET = -3
                GOTO 999
              ELSE
                PJETX(NJET) = PP(1,J)
                PJETY(NJET) = PP(2,J)
                PJETZ(NJET) = PP(3,J)
                PJETE(NJET) = PP(4,J)
C
C                            Set the jet number negative for now
C
                JNOFP(J)    = -NJET
              ENDIF
            ELSE
C
C                            Find the jet that this particle belongs to
C                            by recursion. Pointers are positive while
C                            jet numbers are negative.
C
              K = J
 8005         IF( JNOFP(K) .LT. 0 ) THEN
                JNOFP(J) = JNOFP(K)
              ELSE
                K = JNOFP(K)
                GO TO 8005
              ENDIF
            ENDIF
 8010     CONTINUE
C
C                            Reverse the jet number
C
          DO  8020  J = 1, NPAR
            JNOFP(J) = -JNOFP(J)
 8020     CONTINUE
C
C                            WRITE(6,8025) (JNOFP(L),L=1,NPAR)
C8025                        FORMAT(60(1X,I1))
C
        ENDIF
C
      ENDIF
C
  999 RETURN
      END
*A,,GETY
#endif
