      SUBROUTINE QFMMCL( YCUT, EVIST, MASMAT, PP, LENM )
C----------------------------------------------------------------------
CKEY ALPHA JETS /INTERNAL
C----------------------------------------------------------------------
C
C   New      : C. Bowdery      9-OCT-1990  Faster algortihm than QGMMCL
C
C   Description
C   ===========
C!   Evaluate jet multiplicities based on the scaled invariant mass
C!   squared algorithm using BOS work bank
C    from momentum vectors stored in QCTBUF.INC
C
C  input   : YCUT            ycut value ( (M/EVIST)**2 )
C            EVIST           visible energy
C                            (if EVIST=0., it is computed from the
C                             input particle energies)
C            MASMAT          (empty) invariant mass**2 square matrix
C            PP              (empty) matrix for temporary particle usage
C            LENM            dimension of MASMAT and PP matrices
C
C            KTBI            # of particles           (from QCTBUF)
C            QTBIX,Y,Z,E(i)  four momentum (i=1,KTBI) ( "     "   )
C  output  :
C            QTBOX,Y,Z,E(j)  four  momentum of jet j (j=1,NJET)
C            KTBOF(i)        jet # of particle i     (i=1,KTBI)
C            KTBO        -1  input  error (KTBI/EVIST) (in   QCTBUF)
C                        -2  error from individ. track
C                        -3  too many jets
C                       NJET # of jets (normal return)
C======================================================================
C-------------------- /QCTBUF/ --- Buffer for topological routines -----
      PARAMETER (KTBIMX = 2000,KTBOMX = 20)
      COMMON /QCTBUF/ KTBI,QTBIX(KTBIMX),QTBIY(KTBIMX),QTBIZ(KTBIMX),
     &  QTBIE(KTBIMX),KTBIT(KTBIMX),KTBOF(KTBIMX),KTBO,QTBOX(KTBOMX),
     &  QTBOY(KTBOMX),QTBOZ(KTBOMX),QTBOE(KTBOMX),QTBOR(KTBOMX)
C     KTBI : Number of input vectors (max : KTBIMX).
C     QTBIX/Y/Z/E : Input vectors (filled contiguously without unused ve
C                   The vectors 1 to KTBI must NOT be modified.
C     KTBIT : Input vector ident. used for bookkeeping in the calling ro
C     KTBO  : Number of output vectors (max : KBTOMX).
C     QTBOX/Y/Z/E : Output vector(s).
C     QTBOR : Scalar output result(s).
C     KTBOF : If several output vectors are calculated and every input v
C             associated to exactly one of them : Output vector number w
C             the input vector is associated to. Otherwise : Dont't care
C If a severe error condition is detected : Set KTBO to a non-positive n
C which may serve as error flag. Set QTBOR to a non-physical value (or v
C Fill zeros into the appropriate number of output vectors. Do not write
C messages.
C--------------------- end of QCTBUF ---------------------------------
C
C-----------------------------------------------------------------------
C
      REAL              YCUT, EVIST
C
      INTEGER           LENM, MINPA1, MINPA2, I, J, K, IRET, NJET, LOOP
C
      REAL              MASMAT(LENM,LENM), PP(5,LENM)
C
      REAL              MASS, MINMAS, YIJMIN, EVIS2, EVIS, EVISI
      REAL              DEAD, ZERO, ONE, TWO, START
      PARAMETER         ( DEAD  = -1.0E10, ZERO = 0.0E0 )
      PARAMETER         ( START =  1.0E20, ONE  = 1.0E0, TWO = 2.0E0 )
C
C-----------------------------------------------------------------------
C
C                            Check for allowed input numbers
C
      IF( KTBI .GT. LENM  .OR.  KTBI .LT. 1 ) THEN
        IRET = -1
        GO TO 999
      ENDIF
C
C                            Ensure MINPA1 and MINPA2 defined
C                            just in case KTBI=1 and YCUT is huge
C
      MINPA1 = 1
      MINPA2 = 1
      LOOP   = 0
      IRET   = 1
      EVIS   = EVIST
C
      DO  10  I = 1, KTBOMX
        QTBOX(I) = 0.0
        QTBOY(I) = 0.0
        QTBOZ(I) = 0.0
        QTBOE(I) = 0.0
   10 CONTINUE
C
      EVISI = ZERO
C
C                            Copy input momentum components to working
C                            space. Set jet assignment to point at self
C
      DO  20  I = 1, KTBI
        KTBOF(I) = I
        PP(1,I)  = QTBIX(I)
        PP(2,I)  = QTBIY(I)
        PP(3,I)  = QTBIZ(I)
        PP(4,I)  = QTBIE(I)
        PP(5,I)  = SQRT( PP(1,I)**2 + PP(2,I)**2 + PP(3,I)**2 )
        EVISI    = EVISI + PP(4,I)
   20 CONTINUE
C
      DO  25  I = KTBI+1,KTBIMX
        KTBOF(I) = 0
   25 CONTINUE
C
      IF( EVIS .EQ. ZERO ) EVIS = EVISI
      EVIS2 = EVIS**2
C
      IF( EVIS2 .LE. ZERO ) THEN
        IRET = -1
        GOTO 999
      ENDIF
C
C                            Detect bad input (divide checks!)
C
      DO  30  I = 1,KTBI
C
C                            WRITE(6,8880) I,(PP(KKK,I),KKK=1,5)
C8880                        FORMAT(' PARTICLE:',I3,5G13.5)
C
        IF( PP(5,I) .LE. ZERO ) THEN
          IRET = -2
          GOTO 999
        ENDIF
   30 CONTINUE
C
C                            Compute a table of invariant mass**2
C                            combinations of all particle pairs.
C                            Diagonal elements and repeats not needed!
C
      DO  50  I = 2,KTBI
        DO  40  J = 1,I-1
C
C                              COSANG = ( PP(1,I)*PP(1,J) +
C                                         PP(2,I)*PP(2,J) +
C                                         PP(3,I)*PP(3,J)   ) /
C                                        (PP(5,I)*PP(5,J))
C
C                            It does not matter if COSANG lies just
C                            outside the range  -1 to +1.
C
          MASMAT(J,I) =   TWO * PP(4,I) * PP(4,J) *
     &                 (ONE - ( PP(1,I) * PP(1,J) +
     &                          PP(2,I) * PP(2,J) +
     &                          PP(3,I) * PP(3,J)   ) /
     &                         (PP(5,I) * PP(5,J))      )
C
   40   CONTINUE
   50 CONTINUE
C
C                            Set up starting minimum invariant mass**2
C
  100 MINMAS = START
      LOOP   = LOOP + 1
C
C                            Find the lowest invariant mass**2 but
C                            ignore killed combinations (DEAD).
C                            Note a temporary copy of MASMAT(I,J) is
C                            not used since the optimisation makes
C                            use of registers and is faster.
C
      DO  2000  I = 2,KTBI
        IF( PP(5,I) .GT. ZERO ) THEN
          DO  1000  J = 1,I-1
C
            IF( MASMAT(J,I) .GT. DEAD ) THEN
              IF( MASMAT(J,I) .LT. MINMAS ) THEN
                MINMAS  = MASMAT(J,I)
                MINPA1 = J
                MINPA2 = I
              ENDIF
            ENDIF
C
 1000     CONTINUE
        ENDIF
 2000 CONTINUE
C
C                            Compute YMIN for the found pair
C
      YIJMIN = MINMAS / EVIS2
C
C                     WRITE(6,8881) LOOP, MINMAS, YIJMIN,MINPA1, MINPA2
C8881                 FORMAT(3X,I5,2X,2G13.5,2(1X,I3))
C
      IF( YIJMIN .LE. YCUT  .AND.  LOOP .LT. KTBI ) THEN
C
C                            The particle with the higher position
C                            index is killed and its KTBOF element is
C                            set to the index of the merged particle.
C
        KTBOF(MINPA2) = MINPA1
C
C                            Add the momentum components together of
C                            the pair to make the merged particle
C
        PP(1,MINPA1) = PP(1,MINPA1) + PP(1,MINPA2)
        PP(2,MINPA1) = PP(2,MINPA1) + PP(2,MINPA2)
        PP(3,MINPA1) = PP(3,MINPA1) + PP(3,MINPA2)
        PP(4,MINPA1) = PP(4,MINPA1) + PP(4,MINPA2)
C
        PP(5,MINPA1) =          SQRT( PP(1,MINPA1)**2 +
     &                                PP(2,MINPA1)**2 +
     &                                PP(3,MINPA1)**2   )
C
C                            Divide check protection
C
        IF( PP(5,MINPA1) .LE. ZERO ) THEN
          IRET = -2
          GO TO 999
        ENDIF
C
C                            Kill the other particle of the pair
C
        PP(1,MINPA2) =  ZERO
        PP(2,MINPA2) =  ZERO
        PP(3,MINPA2) =  ZERO
        PP(4,MINPA2) =  ZERO
        PP(5,MINPA2) = -ONE
C
C                            Recompute the invariant mass**2's for the
C                            new merged particle with all the others.
C                            Combinations with killed particles would
C                            give zero mass but this is set to DEAD
C
        DO  4000  J = 1,KTBI
C
          IF( PP(5,J) .LT. ZERO ) THEN
C
            MASS = DEAD
C
          ELSE
C
            MASS  =     TWO * PP(4,MINPA1) * PP(4,J) *
     &               (ONE - ( PP(1,MINPA1) * PP(1,J) +
     &                        PP(2,MINPA1) * PP(2,J) +
     &                        PP(3,MINPA1) * PP(3,J)   ) /
     &                       (PP(5,MINPA1) * PP(5,J)))
          ENDIF
C
C                            Only replace those entries that lie in
C                            the used half of the table.
C
          IF( J .LT. MINPA1 ) THEN
            MASMAT(J,MINPA1) = MASS
          ELSE
            MASMAT(MINPA1,J) = MASS
          ENDIF
C
C                            Set the invariant mass to  DEAD  for
C                            the killed particle with all others
C
          IF( J .LT. MINPA2 )THEN
            MASMAT(J,MINPA2) = DEAD
          ELSE
            MASMAT(MINPA2,J) = DEAD
          ENDIF
 4000   CONTINUE
C
C                            Go back and find the next lowest mass pair
C
        GOTO 100
C
      ELSE
C
C                            No more jets, fill QTBOX/Y/Z/E and KTBOF
C                            KTBOF(ipart) = associated jet #,
C                            (even when only 1 part. in "jet")
C
C
        NJET = 0
        DO  8010  J = 1,KTBI
C
          IF( KTBOF(J) .EQ. J ) THEN
            NJET = NJET + 1
            IF( NJET .GT. KTBOMX ) THEN
              IRET = -3
              GOTO 999
            ELSE
              QTBOX(NJET) = PP(1,J)
              QTBOY(NJET) = PP(2,J)
              QTBOZ(NJET) = PP(3,J)
              QTBOE(NJET) = PP(4,J)
C
C                            Set the jet number negative for now
C
              KTBOF(J)    = -NJET
            ENDIF
          ELSE
C
C                            Find the jet that this particle belongs to
C                            by recursion. Pointers are positive while
C                            jet numbers are negative.
C
            K = J
 8005       IF( KTBOF(K) .LT. 0 ) THEN
              KTBOF(J) = KTBOF(K)
            ELSE
              K = KTBOF(K)
              GO TO 8005
            ENDIF
          ENDIF
 8010   CONTINUE
C
C                            Reverse the jet number
C
        DO  8020  J = 1, KTBI
          KTBOF(J) = -KTBOF(J)
 8020   CONTINUE
C
C                            WRITE(6,8025) (KTBOF(L),L=1,KTBI)
C8025                        FORMAT(60(1X,I1))
C
      ENDIF
C
  999 IF( IRET .GE. 0 ) THEN
        KTBO = NJET
      ELSE
        KTBO = IRET
      ENDIF
C
      RETURN
      END
