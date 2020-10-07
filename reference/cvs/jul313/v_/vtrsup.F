      SUBROUTINE VTRSUP(IROMD)
C----------------------------------------------------------------------
C!  - Update the VTRS bank
C!
C
C  Bank VTRS contains the relations between VDET electronic channels
C  and monte carlo tracks. For VDET95, we demultiplex the strips
C  before clustering. VTRS now needs to contain the readout strip
C  address. Has to be done now that we have the relation between
C  el. chann. and R/O strips in bank VCSG
C
C  A. Bonissent June 1995
C
C  INPUT  :
C      IROMD   :  Readout module address
C
C?
C!======================================================================
#ifndef DOC
C
      SAVE NAVCSG,NAVTRS
C
      INTEGER VIWFFW,VFWAFF
C
C  Inputs
C
      INTEGER IROMD
      LOGICAL GOOD
C
C  Global includes
C
#include "bcs.h"
#include "vrecon.h"
#include "vcsgjj.h"
#include "vtrsjj.h"
C
C  Function calls
C
C
      LOGICAL FIRST
      DATA FIRST /.TRUE./
#include "bmacro.h"
      IF(FIRST)THEN
        FIRST=.FALSE.
        NAVCSG=NAMIND('VCSG')
        NAVTRS=NAMIND('VTRS')
      ENDIF
      KVTRSN=NLINK('VTRS',100)
C
C Decode address,
C get the view and the number of readout channels
C
      CALL VADEWA(IROMD,ILAY,IZ,IPH,IVIEW)
C Find the module (1 or 2)
      IRET = VFWAFF(IZ,IMOD,IWAF)
C
      KVTRS=IW(NAVTRS)
      NVTRS=LROWS(KVTRS)
      KVCSG=IW(NAVCSG)
      NVCSG=LROWS(KVCSG)
      DO 20 IVTRS=1,NVTRS
         IAVT=ITABL(KVTRS,IVTRS,JVTRAD)
         CALL VUNADD(IAVT,NC,IL,IWF,IFA,IV,ICH)
         IF(IL.NE.ILAY.OR.
     >      IWF.NE.IZ.OR.
     >      IFA.NE.IPH.OR.
     >      IV.NE.IVIEW)GO TO 20
            CH = RTABL(KVTRS,IVTRS,JVTRCH)
            IVDHT = ITABL(KVTRS,IVTRS,JVTRHT)
            INX = ITABL(KVTRS,IVTRS,JVTRVT)
C
C Transform the el. channel nb. from Galeph into data channel
C
            IVCSG = ICH + CHNOFF(IV)
C
            DO IWAF=1,3
              JSTRP = ITABL(KVCSG,IVCSG,JVCSSA+IWAF-1)
              IF(JSTRP.NE.0)THEN
                NL=LROWS(KVTRSN)+1
                IW(KVTRSN+LMHROW)=NL
                IRET = VIWFFW(IMOD,IWAF,IWFF)
                CALL VPKADD(IAVTN,NC,IL,IWFF,IFA,IV,JSTRP)
                KLINE=KROW(KVTRSN,NL)
                IW(KLINE+JVTRAD)=IAVTN
                RW(KLINE+JVTRCH)=CH
                IW(KLINE+JVTRHT)=IVDHT
C In Julia, we do not use the relation to next for same track;
C and VTRS is not output on POT
C Keeping it up to date would be too complicated
                IW(KLINE+JVTRVT)=0
C
              ENDIF
            ENDDO
  20  CONTINUE
      RETURN
      END
#endif
