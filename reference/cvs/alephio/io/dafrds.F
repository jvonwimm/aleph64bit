      SUBROUTINE DAFRDS(IUN,IREC,ARR,NDIM,*)
C----------------------------------------------------------------------
C! direct access read of a single bank
CKEY DAF BOS DBASE READ / INTERNAL
C!   Author   :- Manuel Delfino        10-JUL-1989
C!
C!   Inputs:
C!        - IUN = Unit number to be read
C!        - IREC= Record number to be read
C!
C!   Outputs:
C!        - ARR = Array to receive the data
C!        - NDIM= Length of array ARR
C!        - *   = Alternate return in case of error
C!
C!   Libraries required: NONE
C!
C!   Description
C!   ===========
C!   Direct replacement for BOS77 routine for direct access reads
C!   of single banks. Used to read DAF files. Introduces a cache
C!   in the database stream in order to avoid hundreds of reads
C!   of the same record when initializing ALEPH data base.
C!
C!   on VAX, the LINK command must include the routine explicitely:
C!        ALE:ALEPHLIB/LIB/INCLUDE=(BABEND,DAFRDS),ALE:BOS77/LIB
C
C    ENTRY DAFRST (IUN)
C    reset cache before reading a new data base
C!======================================================================
#ifndef DOC
      INTEGER I
      INTEGER IUN,IREC,NDIM
      INTEGER ARR(NDIM)
C
C Cache depth and width in words
C
      INTEGER CACHDP,CACHWD
      PARAMETER (CACHDP=40,CACHWD=1024)
      INTEGER CACHE(CACHWD,CACHDP)
C
C Cache index, stores unit number, record number and size in words
C
      INTEGER CACHID(3,CACHDP)
      INTEGER CIDIUN,CIDIRE,CIDDIM
      PARAMETER (CIDIUN=1,CIDIRE=2,CIDDIM=3)
C
C Counter to make the cache circular
C
      INTEGER CACHNX
C
      SAVE CACHE
      DATA CACHNX /1/
      DATA CACHID /CACHDP*-1,CACHDP*-1,CACHDP*-1/
C
C Check for valid unit and record
C
      IF(IUN.GT.0.AND.IREC.GT.0) THEN
C
C On read request, check if data is in the cache
C
        DO 1 I=1,CACHDP
          IF(IUN.EQ.CACHID(CIDIUN,I).AND.IREC.EQ.CACHID(CIDIRE,I)) THEN
            IF(NDIM.EQ.CACHID(CIDDIM,I)) THEN
C
C Found record in cache, copy it and return
C
              CALL UCOPY(CACHE(1,I),ARR,NDIM)
              RETURN
            END IF
          END IF
    1   CONTINUE
C
C Did not find record, read it from disk
C
        READ(IUN,REC=IREC,ERR=999)ARR
C
C Store record in cache if possible
C
        IF(NDIM.LE.CACHWD) THEN
C
C Get the next slot in a circular buffer
C
              CACHNX = CACHNX + 1
              IF(CACHNX.GT.CACHDP) CACHNX = 1
              CALL UCOPY(ARR,CACHE(1,CACHNX),NDIM)
              CACHID(CIDIUN,CACHNX) = IUN
              CACHID(CIDIRE,CACHNX) = IREC
              CACHID(CIDDIM,CACHNX) = NDIM
        END IF
        RETURN
      ELSE
C
C Error returns
C
        RETURN 1
      END IF
  999 RETURN 1
C =======================================================
      ENTRY DAFRST (IUN)
      CHCHNX = 1
      DO 1000 I=1,CACHDP
         IF (CACHID(CIDIUN,I).EQ.IUN) THEN
            CACHID(2,I) = -1
            CACHID(3,I) = -1
         ENDIF
 1000 CONTINUE
      END
#endif
