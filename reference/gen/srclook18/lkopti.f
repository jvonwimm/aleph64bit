C*EI
C*DK LKOPTI
C*DF UNIX
      LOGICAL FUNCTION LKOPTI(STR,PAR)
C ----------------------------------------------------
C ----------------------------------------------------
C*IF .NOT.DOC
      CHARACTER STR*(*),PAR*(*)
      LOGICAL LKEQUAL
      EXTERNAL LKEQUAL
C ----------------------------------------------------
      LP = LNBLNK(PAR)
      LS = LNBLNK(STR)
      IF (LS .EQ. 0) THEN
         LKOPTI = .FALSE.
         RETURN
      ENDIF
      IP=0
 10   IB=INDEX(STR(IP+1:LS),'/')
      IP=IP+IB
      IF (IB.EQ.0.OR.IP.GE.LS.OR.(IP+LP).GT.LS) THEN
          LKOPTI=.FALSE.
      ELSE
          IF (.NOT.LKEQUAL(STR(IP+1:IP+LP),PAR(1:LP))) GOTO 10
          LKOPTI=.TRUE.
      ENDIF
      RETURN
      END
