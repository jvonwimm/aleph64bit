      SUBROUTINE SPTOJ(LIST,IER)
C----------------------------------------------------------------------
C! Steering for SATR unpacking of POT banks
C!
C!    Author:     H. Meinhard       26-May-1989
C!
C!    Input:      - LIST      /C    BOS event list
C!                                  If LIST(2:2) .eq. '-' drop POT bk
C!    Output:     - IER       /I    = 0  successful
C!                                  = 1  input bank does not exist or
C!                                       is empty
C!                                  = 2  not enough space
C!                                  = -1 ok, but garbage collection
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
      CHARACTER LIST*(*)
C----------------------------------------------------------------------
      IER = 0
C
C PSCO -> SCOO
      CALL PSCOOJ(LIST,IER)
      IF (IER .EQ. 2)                                       GOTO 999
C
C PSPO -> STCP
      CALL PSTCPJ(LIST,IER)
      IF (IER .EQ. 2)                                       GOTO 999
C
  999 CONTINUE
      RETURN
      END
#endif
