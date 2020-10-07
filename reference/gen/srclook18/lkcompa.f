C*EI
C*DK LKCOMPA
C*DF UNIX
      SUBROUTINE LKCOMPA(STR,PAR,COUT)
C --------------------------------------------------------
C --------------------------------------------------------
C*IF .NOT.DOC
      CHARACTER STR*80,PAR*(*),PAR10*10,PARE10*10,COUT*80
      LOGICAL LKEQUAL
      EXTERNAL LKEQUAL
C --------------------------------------------------------
      PAR10=PAR
      COUT=' '
      IP=0
 10   IB=INDEX(STR(IP+1:80),'/')
      IP=IP+IB
      IF (IB.NE.0.AND.IP.LT.80.AND.STR(IP+1:IP+1).NE.' ') THEN
          IF (LKEQUAL(STR(IP+1:IP+10),PAR10)) THEN
              LEN=LKLWRD(STR(IP+1:IP+10))
              L2=INDEX(STR(IP+1:IP+10),'=')
              IF (L2.NE.0.AND.L2.LT.LEN) THEN
                  ISS=LKLWRD(STR(IP+L2+1:80))
                  COUT=STR(IP+L2+1:IP+L2+ISS)
                  IF (COUT(1:1).EQ.'"') COUT=COUT(2:ISS-1)
              ENDIF
          ELSE
              GOTO 10
          ENDIF
      ENDIF
      RETURN
      END
