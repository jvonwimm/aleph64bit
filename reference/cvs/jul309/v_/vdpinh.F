      SUBROUTINE VDPINH(IROMD)
C----------------------------------------------------------------------
C! This takes the list of peculiar readout channels for a given module
C! and sets the flags of the corresponding strips in the Monte Carlo
C!                    G. Taylor 20/8/95
C!
C  INPUT  :
C      IROMD   :  Readout module address
C?
C!======================================================================
#ifndef DOC
C
C  Inputs
C
      INTEGER IROMD
C
#include "bcs.h"
#include "vcsgjj.h"
#include "vdflgs.h"
#include "vpecjj.h"
C
C  Reconstruction parameters
C
      SAVE NAVCSG
      INTEGER VDCHST
      LOGICAL FIRST
      DATA FIRST /.TRUE./
#include "bmacro.h"
C--------------------------------------------------------------
C
      IF(FIRST)THEN
       FIRST=.FALSE.
       NAVCSG=NAMIND('VCSG')
      ENDIF
      KVCSG=IW(NAVCSG)
      NVCSG=LROWS(KVCSG)
C
C  Link peculiar channel bank
C
      KVPEC = NLINK('VPEC',IROMD)
C
C  Peculiar channels are present
C
      IF(KVPEC .NE. 0)THEN
       NVPEC = LROWS(KVPEC)
       DO IVPEC=1,NVPEC
        ICHAN = ITABL(KVPEC,IVPEC,JVPECA)
        JFLAG = ITABL(KVPEC,IVPEC,JVPESF)
C
C Compute flag for the peculiar strip
C
        ISTRIP=VDCHST(IROMD,ICHAN)
        IF(ISTRIP.GT.0) THEN
         IOFL=ITABL(KVCSG,ISTRIP,JVCSMF)
C
C only set the flags if the strip was "readout"
C
         IF(IOFL.NE.0) THEN
          ICOMP=IAND(IOFL,JFLAG)
          JCOMP=IAND(ICOMP,VBSUPP)
          IW(KROW(KVCSG,ISTRIP)+JVCSMF)=JCOMP
         ENDIF
        ENDIF
       ENDDO
      ENDIF
      RETURN
      END
#endif
