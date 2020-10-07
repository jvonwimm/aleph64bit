      INTEGER FUNCTION MINI_KPECPC(IECAL)
C
C
C     Input: IECAL (I) : ALPHA track number of the ECAL (PECO) object
C
C     Output: (I): PCOB object number
C
C     error code : -1 = invalid IECAL
C                  -2 = IECAL is NOT a PECO object
C                  -3 = cannot find PECO Julia number
C                  -4 = cannot find PCRL
C-------------------------------------------------------------------------
      IMPLICIT NONE
C
#include "qdecl.h"
#include "qcde.h"
#include "qhac.h"
C
      INTEGER IECAL,IPECO,KPCRL
      INTEGER ICOB
C
#include "qmacro.h"
C
      MINI_KPECPC=0
C
C...check if this is an ECAL object
C
      IF(IECAL.LE.0) THEN
        MINI_KPECPC=-1
        GOTO 999
      ELSEIF(.NOT.(XPEC(IECAL))) THEN
        MINI_KPECPC=-2
        GOTO 999      
      ENDIF
C
C...Look for the PECO row number
C
      IPECO=KTN(IECAL)
      IF(IPECO.LE.0) THEN
        MINI_KPECPC=-3
        GOTO 999
      ENDIF
C
C...Loop on PCRL to find the corresponding calobject number
C
      KPCRL=IW(NAPCRL)
      IF(KPCRL.LE.0) THEN
        MINI_KPECPC=-4
        GOTO 999
      ENDIF
      DO ICOB=1,LROWS(KPCRL)
        IF(IPECO.EQ.ITABL(KPCRL,ICOB,JPCRPE)) THEN
          MINI_KPECPC=ITABL(KPCRL,ICOB,JPCRPC)
          GOTO 999
        ENDIF
      ENDDO
C
 999  CONTINUE
      RETURN
      END
C 
