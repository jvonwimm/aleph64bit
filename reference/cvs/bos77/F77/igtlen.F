      INTEGER FUNCTION IGTLEN(JW,LIST)
C -------------------------------------------------------------
C - F.Ranjard - 910801
C! get the length of LIST
C  returns the number of banks in the LIST list
C - Input    : - JW      / I      = BOS array
C                LIST    / CHAR*1 = one of the list C,E,R,S,T
C - Output   : - IGTLEN  / I      = number of banks in th LIST
C                                   -1 :the LIST-list does not exist
C -------------------------------------------------------------
#ifndef DOC
      INTEGER JW(*)
      CHARACTER LIST*1
#include "param.h"
C -----------------------------------------------------------------
      IGTLEN = -1
      IL = INDEX ('CERST',LIST)
      IF (IL.EQ.0) GOTO 999
      IL = ILT+IL
      IF (JW(IL) .EQ. 0) GOTO 999
      ID = JW(IL)
      IGTLEN = JW(ID)
 999  RETURN
      END
#endif
