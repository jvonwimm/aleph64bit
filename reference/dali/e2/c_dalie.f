CH
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*DK DEVNAM
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DEV
CH
      SUBROUTINE DEVNAM(T8)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C   Author    Christoph Grab
C              31-Aug-89
C
C   Purpose : Extract the actual filename (max. of 8 char), stripping
C             device, uic and extension.
C
C
*CA DALLCO
      INCLUDE 'DALI_CF.INC'
      CHARACTER *8 T8
C
C LP : position of "."     L1 : Position of "]" or ":" or "/"
C
      DO K=LNINDE(1),1,-1
        IF(TFINDE(1)(K:K).EQ.'.') LP=K
        IF(TFINDE(1)(K:K).EQ.':'.OR.
     &     TFINDE(1)(K:K).EQ.']'.OR.
     &     TFINDE(1)(K:K).EQ.'/') THEN
          L1=K
          GO TO 200
        END IF
      END DO
C
      IF(LP.EQ.0) THEN
        T8='None    '
        RETURN
      END IF
C
      L1=0
  200 L1=L1+1
      LP=LP-1
      T8=TFINDE(1)(L1:MIN(L1+7,LP))
      RETURN
      END
