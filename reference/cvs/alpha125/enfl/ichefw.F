      FUNCTION ichefw(ich)
C-----------------------------------------------------------------
C! Return the ENFLW track index for any copy of it
C
C  Input:  -- ich:   The track index
C
C  Output  -- ichefw: the corresponding ENFLW track index
C
C  Patrick Janot - 24 Sep 1991
C-----------------------------------------------------------------
#ifndef DOC
#include "parcut.h"
#include "qcde.h"
#include "qmacro.h"
C
      jch = ich
      jchsam = KSAME(ich)
    1 IF ( jchsam .EQ. ich ) GOTO 2
      IF ( jchsam .GE. kfrefw .AND. jchsam .LE. klsefw ) THEN
        jch = jchsam
        GOTO 2
      ENDIF
      jchsam = KSAME(jchsam)
      GOTO 1
    2 ichefw = jch
C
      RETURN
      END
#endif
