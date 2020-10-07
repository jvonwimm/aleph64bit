      SUBROUTINE QJMDCL (NJETS,CNAM,ICLASS,ALPHA,DELTA,ETA,EVIS)
CKEY JETS /USER
C----------------------------------------------------------------------
C   Author   : P. Perez       28-MAR-1989
C
C   Description
C   ===========
C!   Set up and call QGMDCL Jet finding routine.
C  input   : ALPHA           weight
C            DELTA           half opening angle (degrees)
C            ETA             energy cut-off for jets(in units of Evis)
C            EVIS            visible energy
C           (if EVIS=0., it is computed from the input part. energies)
C   output: NJETS and QCTBUF.INC
C======================================================================
      CHARACTER *(*) CNAM
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
C-----------------------------------------------------------------------
      CALL QJSETU(ICLASS)
C
      CALL QGMDCL(ALPHA,DELTA,ETA,EVIS)
C
      NJETS = KTBO
      IF (CNAM.NE.' ')  THEN
        IDR=0
        DO 1 IJET = NJETS, 1, -1
          CALL QJSAVE (CNAM,IJET,ICLASS,IDR,0,1)
          IDR=1
    1   CONTINUE
      ENDIF
C
      END
