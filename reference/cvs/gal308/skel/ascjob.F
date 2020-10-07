*DK ascjob
      SUBROUTINE ASCJOB
C ----------------------------------------------------------------------
C. - F.RANJARD - 850328
C! Close the job
C.   print job summary
C.   print histograms
C.   close files
C. - Called by    : QNEXT                            from this .HLB
C. - Calls        : ACLOSE                           from ALEPHLIB
C.                  ASWSUM, xxWSUM, USCJOB           from this .HLB
C.                  HPHST                            from GENLIB
C.
C ----------------------------------------------------------
#ifndef DOC
      SAVE
#include "iocom.h"
#include "jobcom.h"
#include "bcs.h"
C ----------------------------------------------------------------------
C
C - write the last record
      IF(MSAVJO.GT.0) THEN
        IF (MGETJO.GT.0) THEN
           CALL ABWEND
        ELSE
           CALL BWRITE (IW,LSAVIO,'0')
        ENDIF
      ENDIF
C
C - Print run summary
      CALL ASWSUM
C
C - Print detector summaries
      IF (IDETJO(1).GT.0) CALL VDWSUM
      IF (IDETJO(2).GT.0) CALL ITWSUM
      IF (IDETJO(3).GT.0) CALL TPWSUM
      IF (IDETJO(4).GT.0) CALL ECWSUM
      IF (IDETJO(7).GT.0) CALL HCWSUM
      IF (IDETJO(5).GT.0) CALL LCWSUM
      IF (IDETJO(6).GT.0) CALL SAWSUM
      IF (IDETJO(8).GT.0) CALL MUWSUM
      IF (IDETJO(9).GT.0) CALL SIWSUM
C
C - Print trigger summary
      IF (IPROJO(5).NE.0) THEN
         CALL X1WSUM
         CALL X2WSUM
      ENDIF
C
C - Call USER routine
      CALL USCJOB
C
C - Print histograms
C
      CALL HPHST(0)
C
C - Close all files
C
      CALL ACLOSE (0,IER)
C
      RETURN
      END
#endif
