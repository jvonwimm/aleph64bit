      SUBROUTINE QJPTCL  (NJETS, CNAM, ICLASS ,NJTLIM,YJTLIM , EVIS)
CKEY JETS /USER
C----------------------------------------------------------------------
C! Calls QGPTCL: PTCLUS jet algorithm (see ALEPH 89-150).
C!
C!   Author   :- MIKE SCARR Glasgow University
C!
C!   Modified :- John Hearns           31-Aug-1989
C!             - Ingrid ten Have        8-Oct-1989
C!
C!   Inputs:
C!        -   ICLASS  =  class of ALPHA particles to work on
C!        -   NJTLIM  =  the least number of jets to search for.
C!                       If NJTLIM = 0 the algorithm finds the number
C!                       of jets determined by YJTLIM.
C!                       but is you want to force say 3-jet events
C!                       set NJTLIM to 3 and YJTLIM to a very high value
C!        -   YJTLIM =   maximum allowed distance between two clusters
C!                       in (invariant mass divided by the
C!                       visible energy)**2 space
C!                       a  range for YJTLIM is 0.01 - 0.1
C!                       Recommended values are 0.01-0.02 yielding a
C!                       high resolution
C!        -   EVIS   =   can be set to the required value by the user.
C!                       if it is set to zero on input then
C!                       EVIS is replaced by the visible energy in the
C!                       event
C!                    *! EVIS is  IGNORED if LSCALE is set to FALSE
C!                           in the routine QGPTCL *!
C!
C!   Outputs:
C!        -   NJETS - number of jets found
C!                     set to negative number if error condition
C!        -   THRUST  =  GENERALISED THRUST.
C!
C!   Libraries required:
C!
C!   Description
C!   ===========
C!   SET UP AND  CALL THE JET FINDING ROUTINE QGPTCL (PTCLUS)
C!   written by Mike Scarr of Glasgow University
C!   uses QCTBUF.INC and  the common variables therein
C!
C!   ps---
C!     ALSO COULD OUTPUT IF WANTED :
C!        -   THRUST  =  GENERALISED THRUST.
C!     but I want to keep calling sequence same as other ALPHA jetfinder
C!======================================================================
      INTEGER NJETS
      INTEGER NJTLIM
      REAL YVALUT(3)
C!      YVALUT(1) =   SEMS  OF LAST MERGED CLUSTERS.
C!      YVALUT(2) =   LOWEST  SEMS  OF TWO CLUSTERS.
C!                 SEMS = SCALED EFFECTIV MASS SQUARED, M*M/S.
C!      YVALUT(3)=   FRACTIONAL ENERGY REASSIGNED IN LAST MERGE.
C!                 SEMS = SCALED EFFECTIV MASS SQUARED, M*M/S.
C
C      NJTLIM is the least number of jets to search for
      REAL YJTLIM
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
      CALL QJSETU (ICLASS)
C
      EVIST=EVIS
      CALL QGPTCL (YVALUT, THRUST, NJTLIM, YJTLIM, EVIST)
C     if error, KTBO is set to a negative number
      NJETS = KTBO
      IF ( KTBO .LT. 0 ) GOTO 999
      IF (CNAM.NE.' ')  THEN
        IDR = 0
        DO 10  IJET = NJETS, 1, -1
          CALL QJSAVE (CNAM,IJET,ICLASS,IDR,0,1)
          IDR=1
   10   CONTINUE
      ENDIF
C
  999 RETURN
      END
