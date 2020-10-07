      LOGICAL FUNCTION XVDEOK(DUMMY)
C----------------------------------------------------------------------
C!   Determine HV state of VDET.
CKEY VDET /USER
C!
C!   Author   : HCJ Seywerd            2-AUG-1991
C!   Modified : S Haywood              9-NOV-1992
C!   Modified : H. Seywerd             2-OCT-1997
C!
C!   Outputs: .TRUE. if the HV state of the VDET is ON.
C!            For MC always .TRUE.
C!======================================================================
#ifndef DOC
#include "qcdesh.h"
      LOGICAL XLOCK,XLOCKN
#include "revhjj.h"
#include "bmacro.h"
C----------------------------------------------------------------------
      XVDEOK = .FALSE.
C
C++   For MC, always .TRUE.
C
      IF (KRUN.LT.2000) THEN
         XVDEOK = .TRUE.
         RETURN
      ENDIF
C
C++   Is this in a bad period when readout was blocked ?
C++   If so, check noise to determine HV state.
C
      KGOOD = KVGOOD(DUMMY)
      IF (KGOOD.NE.0) THEN
         IF (KGOOD.EQ.+1) XVDEOK = .TRUE.
         RETURN
      ENDIF
C
C++   Check HV bit in REVH directly.
C++   Note the bit definition changed mid-1991.
C++   Changed again for LEPII (fix inserted 2-10-1997)
      KREVH = IW(NAREVH)
      IF (KREVH.LE.0) RETURN
C
      JHVST = ITABL(KREVH,1,JREVDS)
      IF (KRUN.LT.12089) THEN
         IBVD = 12
      ELSEIF (KRUN.GT.41000) THEN
         IBVD = 12
      ELSE
         IBVD = 21
      ENDIF
      XVDEOK = BTEST(JHVST,IBVD)
C
      RETURN
      END
#endif
