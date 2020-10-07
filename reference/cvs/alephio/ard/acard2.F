      INTEGER FUNCTION ACARD2 (NPRM, COUT)
C----------------------------------------------------------------------
C!  Returns NPRM th parameter on Bos card
C!
C!   Author   :H.Albrecht               4-NOV-1988
C!   modified :F.Ranjard                5-AUG-1993
C!             rewrite it to remove ASSIGN statments
C!   Inputs: NPRM : Nth parameter on the card
C!
C!   Outputs: ACARD2 : 0 : less than NPRM parameters given
C!                     I (positive integer) : length of string COUT
C!        -   COUT : output string = NPRMth parameter on card NCRD
C!                   = string of blank spaces if ACARD2 .le. 0 .
C!                   Must be declared with proper length in the calling
C!                   routine. (If too short : ACARD2 is set to the
C!                   length of COUT without any notification).
C!
C!   Description
C!   ===========
C!
C?   ACARD2 returns the NPRMth parameter on AN BOS input card which
C?   has been defined by a previous CALL ACARD1 (...) (q.v).
C?
C?   Input cards may have no parameter at all or all parameters must
C?   be included in one pair of apostrophes.
C?
C?   The separator character | may be surrounded with blanks. Leading
C?   and trailing but no imbedded blank spaces of the parameters are
C?   removed. Two subsequent separators : the parameter inbetween
C?   corresponds to COUT = blank spaces and ACARD2 = 1 .
C?
C!======================================================================
C
#ifndef DOC
      SAVE
      COMMON /BCS/ IW(1000)
      CHARACTER *(*) COUT
      CHARACTER * 4 CBNK,CHAINT
      CHARACTER *120 STR
      PARAMETER (MXWORD=20)
      CHARACTER *80 CWORD(MXWORD)
      CHARACTER * 1 CSEP(2), C1
      DATA CSEP /'|',','/, LBNK /0/
C
      IF (NPRM .LE. 0)  THEN
        LBNK = - NPRM
        CALL ALSTIN (IW(LBNK+1),IW(LBNK),STR)
        CALL ALSTRI (STR,CSEP(1),MXWORD,CWORD,LWORD)
        GO TO 999
      ENDIF
C
      ACARD2 = 0
      COUT = ' '
      IF (NPRM.LE.LWORD) THEN
         LW = LNBLNK(CWORD(NPRM))
         ACARD2 = MIN (LEN(COUT),LW)
         IF (LW.GT.0) COUT = CWORD(NPRM)(1:ACARD2)
      ENDIF
C
  999 RETURN
      END
#endif
