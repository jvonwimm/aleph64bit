      SUBROUTINE CNVZ(NUM,NT,STRUT)
      EXTERNAL STRUT
      CHARACTER STR*256, STP*12
      COMMON/FMTSTR/STR
      COMMON/STRSTR/JS,IAST,IBST
      INTEGER NUM(*)
      DO 50 L=1,NT
      CALL PNVZ(NUM(L),STP,JSP)
   54 IF(JS.EQ.0) THEN
         STR=' '
         JS=IAST-1
      ELSE IF(JS+JSP+3.LE.IBST) THEN
         JS=JS+1
      ELSE
         CALL STRUT
         GOTO 54
      END IF
      STR(JS+1:JS+1)='Z'
      JS=JS+1
      IF(STP(1:4).EQ.'0000') THEN
         STR(JS+1:JS+JSP-4)=STP(5:JSP)
         JS=JS+JSP-4
      ELSE
         STR(JS+1:JS+JSP)=STP(1:JSP)
         JS=JS+JSP
      END IF
   50 CONTINUE
      RETURN
      END
