      SUBROUTINE CNVL(IA,IB)

*     Data conversion to an internal character string
*
*     EXTERNAL STRUT
*
*     CALL CNVL(IA,IB)        reset and define byte range  IA...IB
*
*     CALL CNVI(NUM,N,STRUT)  convert integer and store
*
*     CALL CNVF(FLT,N,STRUT)  convert real and store
*
*     CALL CNVA(ATE,N,STRUT)  convert hollerith data and store (with '')
*
*     CALL CNVT(TEXT, STRUT)  store text (without ' ')
*
*     CALL STRUT   as final call, to write last string
*
*     STRUT is a subroutine (which the user has to supply), which
*     has to write or store one complete string. A model is:
*
*
*     SUBROUTINE STRUT
*     CHARACTER*256 STR
*     COMMON/FMTSTR/STR
*     COMMON/STRSTR/JS,IAST,IBST
*     IF(JS.EQ.0) GOTO 100
*     WRITE(6,101)   STR(1:IBST)
*     JS=0
* 100 RETURN
* 101 FORMAT(A)
*     END
*
      COMMON/STRSTR/JS,IAST,IBST
      JS  = 0
      IAST=IA
      IBST=IB
      RETURN
      END
