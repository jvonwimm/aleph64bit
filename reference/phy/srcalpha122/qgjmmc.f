      SUBROUTINE QGJMMC( NJETS, CNAM, ICLASS, YCUT, EVIS, SCHEME, VERSN)
C----------------------------------------------------------------------
CKEY ALPHA JETS /INTERNAL
C
C   Author   : C. Bowdery     18-APR-1991 based on QJMMCL by P. Perez
C   Modified : C. Bowdery     28-APR-1991 add VERSN argument
C   Modified : C. Bowdery     18-OCT-1991 new name
C   Modified : C. Bowdery     24-OCT-1991 work bank index corrected
C   Modified : C. Bowdery     24-OCT-1991 remove call to SJMMCL
C   Modified : C. Bowdery      9-JUN-1994 allow 'JADE  ' and 'DURHAM'
C
C   Description
C   ===========
C!   Set up and call FJMMCL jet finding routine.
C!   Generalised version of QJMMCL with SCHEME and VERSN argument.
C
C  Input   : CNAM            name of jet particles to be created
C            ICLASS          KRECO or KMONTE
C            YCUT            YCUT value ( (M/EVIS)**2 )
C            EVIS            visible energy
C                            (if EVIS=0., it is computed from the
C                             input particle energies)
C            SCHEME          CHAR*2, combination scheme 'E ', 'E0', 'P '
C            VERSN           CHAR*6, 'NORMAL' aka 'JADE  ' or
C                                    'BETTER' aka 'DURHAM'
C
C  Output  : NJETS           number of jets found or error code if -ve
C
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
      REAL       RW
C
      INTEGER    K, LEN, LENTM, LENTP
C
      CHARACTER  CNAM*(*), SCHEME*2, VERSN*6
C
      COMMON / BCS / RW(1)
C
C-----------------------------------------------------------------------
      CALL CLTOU(VERSN)
      IF (INDEX(VERSN,'JADE').GT.0) THEN
        VERSN = 'NORMAL'
      ELSEIF (INDEX(VERSN,'DURHAM').GT.0) THEN
        VERSN = 'BETTER'
      ENDIF
C
C                           Set up /QCTBUF/ common
C
      CALL QJSETU( ICLASS )
C
C                           Protect FJMMCL against a zero length array
C
      IF( KTBI .LE. 0 ) THEN
        KTBO = -1
        GOTO 200
      ENDIF
C
C                           LENTM is the length in words of MASMAT array
C                           LENTP is the length in words of PP array
C
      K     = 0
      LENTM = KTBI*KTBI
      LENTP = KTBI*5
      LEN   = LENTM + LENTP
C
      CALL WBANK( RW, K, LEN, *100 )
C
C                           RW(K+1) is to be used as two REAL*4 arrays
C                                                   known as
C                                                MASMAT      PP
C
        CALL FJMMCL( YCUT, EVIS,  SCHEME, VERSN, RW(K+1), RW(K+1+LENTM),
     &               KTBI, QTBIX, QTBIY,  QTBIZ, QTBIE, KTBOMX,
     &               KTBO, QTBOX, QTBOY,  QTBOZ, QTBOE, KTBOF   )
C
        CALL WDROP( RW, K )
        GOTO 200
C
C                           Insufficient BOS storage
C
  100   KTBO = -99
        CALL QWMESE('_QGJMMC_ Insufficient BOS space for jet finding')
C
  200 NJETS = KTBO
      IF( CNAM .NE. ' ' )  THEN
        IDR = 0
        DO  1  IJET = NJETS, 1, -1
          CALL QJSAVE( CNAM, IJET, ICLASS, IDR, 0, 1 )
          IDR = 1
    1   CONTINUE
      ENDIF
C
      END
