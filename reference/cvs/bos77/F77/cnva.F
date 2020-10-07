      SUBROUTINE CNVA(AT,NT,STRUT)
      EXTERNAL STRUT
      CHARACTER STR*256,CHAINT*4,STP*12
      COMMON/FMTSTR/STR
      COMMON/STRSTR/JS,IAST,IBST
      INTEGER AT(*)
      IT=0
   39 IF(JS.EQ.0) THEN
         STR=' '
         JS=IAST-1
      ELSE IF(JS+7.LE.IBST) THEN
         JS=JS+1
      ELSE
         CALL STRUT
         GOTO 39
      END IF
      JS=JS+1
      STR(JS:JS)=''''

   40 IF(IT.LT.NT) THEN
         IF(JS+5.LE.IBST) THEN
            IT=IT+1
            STR(JS+1:JS+4)=CHAINT(AT(IT))
            JS=JS+4
            GOTO 40
         END IF
      END IF
      JS=JS+1
      STR(JS:JS)=''''
      IF(IT.NE.NT) THEN
         CALL STRUT
         GOTO 39
      END IF
      RETURN
      END
