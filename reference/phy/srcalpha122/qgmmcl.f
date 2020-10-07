      SUBROUTINE QGMMCL(YCUT,EVIST)
CKEY JETS /INTERNAL
C----------------------------------------------------------------------
C   Author   : P. Perez       28-MAR-1989
C   Modified : C. Bowdery      4-DEC-1990   Prevent infinite loops
C
C   Description
C   ===========
C!   Evaluate jet multiplicities based on the scaled invariant mass
C!   squared algorithm
C    from momentum vectors stored iN QCTBUF.INC
C
C  input   : YCUT            ycut value ( (M/EVIST)**2 )
C            EVIST           visible energy
C                            (if EVIST=0., it is computed from the
C                             input particle energies)
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
      DIMENSION  PP(5,KTBIMX)
      REAL MASS , MINMAS
      INTEGER    LOOP
C
C------------------------------------------------------------------
C   set initial values
C------------------------------------------------------------------
      LOOP = 0
      EVIS=EVIST
      IF(KTBI.GT.KTBIMX) THEN
C        WRITE(LOUTIO,*) ' ***MMCLUS ktbi=',KTBI,'>ktbimx'
        IRET = -1
        GOTO 999
      ENDIF
C
      IRET = 1
      DO 10 I = 1, KTBOMX
      QTBOX(I) = 0.
      QTBOY(I) = 0.
      QTBOZ(I) = 0.
      QTBOE(I) = 0.
   10 CONTINUE
      DO 11 I = 1, KTBIMX
      KTBOF(I) = 0
   11 CONTINUE
      EVISI = 0.
      DO 1 I = 1, KTBI
        KTBOF(I) = 0
        PP(1,I)  = QTBIX(I)
        PP(2,I)  = QTBIY(I)
        PP(3,I)  = QTBIZ(I)
        PP(4,I)  = QTBIE(I)
        PP(5,I)  = SQRT(PP(1,I)**2+PP(2,I)**2+PP(3,I)**2)
        EVISI    = EVISI + QTBIE(I)
    1 CONTINUE
C
      IF(EVIS.EQ.0.) EVIS = EVISI
      EVIS2 = EVIS**2
C
      IF(EVIS .LE. 0.) THEN
C        WRITE(LOUTIO,*) ' ***MMCLUS visible energy is 0.'
        IRET = -1
        GOTO 999
      ENDIF
C
C                          Increase starting value from 5E8 to 1E20
C
  100 MINMAS = 1.0E20
      LOOP   = LOOP + 1
C
      DO 2000 I = 1, KTBI-1
C
C skip if particle is already merged in a jet
        IF(PP(4,I) .LE. 0.) GO TO 2000
        PI = PP(5,I)
C
        DO 2100 J = I+1, KTBI
C skip if particle is already merged in a jet
          IF(PP(4,J) .LE. 0.) GO TO 2100
C
          PJ = PP(5,J)
          IF (PI .LE. 0. .OR. PJ .LE. 0.) THEN
C            WRITE(LOUTIO,*) ' ***MMCLUS  : momentum is 0'
            IRET = -2
            GO TO 2100
          ENDIF
          COSANG = (PP(1,I)*PP(1,J)+PP(2,I)*PP(2,J)+PP(3,I)*PP(3,J))
          COSANG = COSANG / (PI*PJ)
C
          ACOSAN = ABS(COSANG)
          IF(ACOSAN .GT. 1.0001) THEN
C            WRITE(LOUTIO,*) '***MMCLUS cosang= ',COSANG
            IRET = -2
            GOTO 999
          ELSEIF(ACOSAN.GT.1..AND.ACOSAN.LE.1.0001) THEN
            COSANG = SIGN(1.,COSANG)
          ENDIF
C
          MASS = 2. * PP(4,I) * PP(4,J) * (1.-COSANG)
C
          IF(MASS.LT.MINMAS) THEN
            MINMAS = MASS
            MINPA1 = I
            MINPA2 = J
          ENDIF
C
 2100   CONTINUE
 2000 CONTINUE
C
      YIJMIN = MINMAS / EVIS2
C
C                            Prevent infinite loops (max = KTBI-1)
C
      IF( YIJMIN .GT. YCUT  .OR.  LOOP .GE. KTBI ) THEN
C no more jets, fill QTBOX,Y,Z,E and KTBOF arrays
        NJET = 0
        DO 3000 I = 1, KTBI
          IF(PP(4,I).GT.0.) THEN
            NJET = NJET + 1
            IF(NJET.GT.KTBOMX) THEN
              IRET = -3
              GOTO 999
            ENDIF
            QTBOX(NJET) = PP(1,I)
            QTBOY(NJET) = PP(2,I)
            QTBOZ(NJET) = PP(3,I)
            QTBOE(NJET) = PP(4,I)
C KTBOF(ipart) = associated jet #, (even when only 1 part. in "jet")
            KTBOF(I) = NJET
            DO 3002 J = 1, KTBI
              IF(KTBOF(J).EQ.I) KTBOF(J) = NJET
 3002       CONTINUE
          ENDIF
 3000   CONTINUE
C
        GOTO 999
C
      ELSE
C update pairing,
C merge particles of the pair and "zero" the 2nd part. of the pair
        KTBOF(MINPA1) = MINPA1
        KTBOF(MINPA2) = MINPA1
        DO 5000 I = 1, KTBI
          IF(KTBOF(I).EQ.MINPA2) KTBOF(I) = MINPA1
 5000   CONTINUE
C
        PP(1,MINPA1) = PP(1,MINPA1) + PP(1,MINPA2)
        PP(2,MINPA1) = PP(2,MINPA1) + PP(2,MINPA2)
        PP(3,MINPA1) = PP(3,MINPA1) + PP(3,MINPA2)
        PP(4,MINPA1) = PP(4,MINPA1) + PP(4,MINPA2)
        PP(5,MINPA1) = SQRT(PP(1,MINPA1)**2+PP(2,MINPA1)**2+
     &                      PP(3,MINPA1)**2)
C
        PP(1,MINPA2) = -1.
        PP(2,MINPA2) = -1.
        PP(3,MINPA2) = -1.
        PP(4,MINPA2) = -1.
        PP(5,MINPA2) = -1.
      ENDIF
      GOTO 100
C
  999 KTBO = IRET
      IF(IRET.GE.0) KTBO = NJET
C
      RETURN
      END
