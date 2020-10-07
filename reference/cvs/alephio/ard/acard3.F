      INTEGER FUNCTION ACARD3 (NPRM, CKEY, COUT)
C----------------------------------------------------------------------
C! Tests the parameters on a card for a key CKEY and returns the full
C! parameter in COUT. ACARD3 = 0 if CKEY not found.
C!
C!   Author   : H.Albrecht                     7-MAR-1989
C!
C!   Inputs: Character string CKEY.
C!           NPRM : Start with parameter NPRM.
C!   Outputs: Character string COUT.
C!        -   Integer function value ACARD3 = length of COUT.
C!            If ACARD3 = 0 then COUT = ' '.
C!   Libraries required: None.
C!
C!   Description
C!   ===========
C!
C?   ACARD3 goes through the NPRMth to the last parameter on a data
C?   card and tests whether it starts with the string CKEY. If so,
C?   the full parameter including CKEY is returned in COUT.
C?   The data card is specified by a previous call to the function
C?   ACARD1 or to the subroutine ACDARG.
C?   NPRM = 1 : All parameters.
C?   NPRM = 2 : Ignore the first parameter (e.g. if it contains a data
C?              set name).
C!======================================================================
#ifndef DOC
      INTEGER ACARD2
      CHARACTER *(*) CKEY, COUT
C
      L = LEN (CKEY)
      N = NPRM
   10 ACARD3 = ACARD2 (N, COUT)
      IF (ACARD3 .EQ. 0)  GO TO 90
      IF (CKEY .EQ. COUT(1:L))  GO TO 90
      N = N + 1
      GO TO 10
C
   90 RETURN
      END
#endif
