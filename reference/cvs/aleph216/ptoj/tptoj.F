      SUBROUTINE TPTOJ(FLAGS,FSORT,LIST,IER)
C
C--------------------------------------------------------------------
C! Change TPC POT banks back to the JULIA format
C!
C!    Author:  R. Johnson   16-06-88
C!    Modified: W. Wiedenmann  3-10-91
C!              allow reading of bank PTUN from datastream
C!
C!    Input:    FLAGS(3)  /L    Flag for each of
C!                                   1= coordinates
C!                                   2= bad coordinates
C!                                   3= dE/dx
C!                              Set true for unpacking of the item.
C!              FSORT     /L    Set true to sort the coordinates by
C!                              row and sector and to produce the
C!                              TCRL bank.  This is necessary if the
C!                              coordinates are to be input to the
C!                              pattern recognition.
C!              LIST      /C    BOS event list
C!                              if LIST(2:2) .eq. '-' then
C!                                 drop POT banks
C   Output:      IER   = 0  successful unpacking
C                        -1 OK but garbage collection
C                        1  at least 1 POT bank does not exist
C                        2  not enough space
C                        >2 TPC internal error
C!
C!    If it is desired to have the coordinate bank sorted (FSORT=.T.),
C!    then the tracks (FRFT) should be unpacked first, so that the
C!    pointers from track to coordinate may be updated.
C!
C--------------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "bcs.h"
C
      CHARACTER*(*) LIST
      LOGICAL FIRST,FLAGS(*),FSORT
      DATA FIRST/.TRUE./
C
      IF (FIRST) THEN
        NPTUN=NAMIND('PTUN')
        FIRST=.FALSE.
      ENDIF
C
C++   Check if bank PTUN exists on data
C++   if not: make bank and fill it with (old) values
C
      KPTUN=IW(NPTUN)
      IF (KPTUN.EQ.0) THEN
        CALL TFILTU(IER)
        IF (IER.EQ.2) GOTO 999
      ENDIF
C
      IF (FLAGS(1)) THEN
        CALL PTPCOJ(LIST,IER)
        IF (IER.GE.2) GOTO 999
        IF (FSORT) THEN
          CALL TSRTCO(LIST,IER)
          IF (IER.GE.2) GOTO 999
        ENDIF
      ENDIF
C
      IF (FLAGS(2)) THEN
        CALL PTBCOJ(LIST,IER)
        IF (IER.GE.2) GOTO 999
      ENDIF
C
      IF (FLAGS(3)) THEN
        CALL PTEXSJ(LIST,IER)
        IF (IER.GE.2) GOTO 999
      ENDIF
C
      CALL PTGMAJ(LIST,IER)
      IF (IER.GE.2) GOTO 999
C
  999 CONTINUE
      RETURN
      END
#endif
