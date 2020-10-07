      SUBROUTINE TJTOP(FLAGS,IERR)
C
C--------------------------------------------------------------------
C! Change TPC JULIA banks to POT format for packing
C!
C!    Author:  R. Johnson   16-06-88
C!    Modified: W. Wiedenmann 3-10-91
C!              suppres call to TFILTU, call TFILTJ instead in TINIRU
C!              D. Casper   13-05-97
C!              reverse the order of calls to TPAKCO and TFPCOI, since
C!              information in PCOI is useful in TPAKCO and TFPCOI does
C!              not reference the output bank of TPAKCO (PTNC)
C!
C!    Input:    FLAGS(3)  /L    Flag for each of
C!                                   1= coordinates
C!                                   2= bad coordinates
C!                                   3= dE/dx
C!                              Set true for packing of the item.
C!    Output:   IERR      /I    Error return=0 if operation successful
C!                              10s digit:  error from TPAKCO
C!                             100s digit:  error from TPAKBC
C!                            1000s digit:  error from TPAKTX
C!                           10000s digit:  error from TPAKTA
C!                          100000s digit:  error from TFPCOI
C!
C--------------------------------------------------------------------
#ifndef DOC
C
      LOGICAL FLAGS(*)
C
      IERR=0
C
      IF (FLAGS(1)) THEN
        CALL TFPCOI(IER)
        IF (IER.NE.0) THEN
          IERR=IERR+100000*IER
        ENDIF
        CALL TPAKCO(IER)
        IF (IER.NE.0) THEN
          IERR=IERR+10*IER
        ENDIF
      ENDIF
C
      IF (FLAGS(2)) THEN
        CALL TPAKBC(IER)
        IF (IER.NE.0) THEN
          IERR=IERR+100*IER
        ENDIF
      ENDIF
C
      IF (FLAGS(3)) THEN
        CALL TPAKTX(IER)
        IF (IER.NE.0) THEN
          IERR=IERR+1000*IER
        ENDIF
      ENDIF
C
      CALL TPAKTA(IER)
      IF (IER.NE.0) THEN
        IERR=IERR+10000*IER
      ENDIF
C
  999 CONTINUE
      RETURN
      END
#endif
