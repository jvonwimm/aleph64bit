C!- get medium type, tape no. and options
      CHMED  = FDEVI(1:4)
      LDEVI  = LNBLNK(FDEVI)
      JTAPS  = ICNEXT (FDEVI,5,LDEVI)
      CHTAPE = FDEVI(JTAPS:NE432-1)
      IF (LNBLNK(CHTAPE).LE.6) CHTAPE=CHTAPE(1:LNBLNK(CHTAPE))//'.1.SL'
      IF (NE432.GE.LDEVI) THEN
         CHOPT = ' '
      ELSE
         CHOPT  = FDEVI(NE432+1:LDEVI)
      ENDIF
#if defined(ARDEB)
      WRITE(IW(6),*) ' GTVSN ',SUBR,' CHMED = ',CHMED
      WRITE(IW(6),*) ' GTVSN ',SUBR,' CHTAPE= ',CHTAPE
      IF (LNBLNK(CHOPT).GT.0) WRITE(IW(6),*) ' GTVSN ',SUBR,
     &                        ' CHOPT= ',CHOPT
#endif
C
