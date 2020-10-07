      SUBROUTINE LKGTNAM(STR,NAM,*)
C -----------------------------------------------------------
C -----------------------------------------------------------
#ifndef DOC
      CHARACTER STR*80,NAM*4,C*80
C ----------------------------------------------------------
      NAM='    '
      CALL LKGTPAR(STR,1,'_Bank name: ',C,I)
      IF (I.EQ.4) THEN
          NAM=C(1:4)
      ELSEIF (I.EQ.1.AND.C(1:1).EQ.'*') THEN
          NAM='*'
      ELSE
          RETURN 1
      ENDIF
      RETURN
      END
#endif
