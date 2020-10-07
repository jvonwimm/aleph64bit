      SUBROUTINE MINFMT
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Define bank formats for Mini-DST.
C
C     Author: Stephen Haywood      22-Jan-90
C     Modify: Stephen Haywood      22-Feb-93
C
C     On official Mini's the bank formats are absent to save space.
C     This is a delicate operation and is not in general encouraged.
C     It seems to work for data written on IBM and read on any machine.
C     provided that the format does not contain an 'A'.
C     For this reason, the format for DVMC is explicitly written out by
C     MINVMC.
C
C     MINFMT should be called at initialisation, before banks are
C     written or read.
C
C     On writing a Mini:
C     If 'NOFM' is added to COMP card then
C        formats for Mini banks are not invoked and formats for POT
C        banks may be dropped by MINNOF (when MINOUT is called rather
C        than QWRITE). This is communicated by NOFORM.
C     Else
C        formats for Mini banks are supplied by ALFMT, while those
C        for the POT banks are already present.
C
C     On reading a Mini:
C     Formats are supplied by ALFMT (or implicitly by CMPINI).
C
C     New banks which have not made it to BANKAL FMT can be handled
C     temporarily by explicit calls to BKFMT.
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "mincom.h"
C
      LOGICAL FIRST
      INTEGER ALFMT
      CHARACTER*60 FORM
      SAVE FIRST,LUNIT
      DATA LUNIT / 77 /
      DATA FIRST / .TRUE. /
C-----------------------------------------------------------------------
C
C++   Only need to call MINFMT once.
C
      IF (.NOT.FIRST) RETURN
      FIRST = .FALSE.
C
C++   Determine whether formats should be written.
C
      NOFORM = .FALSE.
      KCOMP = IW(NAMIND('COMP'))
      IF (KCOMP.GT.0) THEN
         DO 10 I=1,IW(KCOMP)
   10    IF (IW(KCOMP+I).EQ.INTCHA('NOFM')) NOFORM = .TRUE.
      ENDIF
C
C++   If formats are not required, return.
C
      IF (NOFORM) RETURN
C
C++   Set up formats for new Mini banks (writing/reading).
C
C     CALL BKFMT('DNEW','I')
C
C++   Supply formats for POT banks which may be without (reading) and
C++   Mini banks already stored in BANKAL FMT (writing/reading).
C
      IER = ALFMT(LUNIT,'ALL ',FORM)
C
      RETURN
      END
#endif
