       SUBROUTINE VDFIXZ
C ----------------------------------------------------------------------
C!  Correct for cos theta dependent zshift of Vdet hits
C!
C-  Author         Alain Bonissent 10 July 2000
CKEY V_DECK VDET
C!
C -----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
#include "vdizjj.h"
#include "bcs.h"
#include "vdztjj.h"
c      real xdum1,xdum2,xdum3,xdum4
      INTEGER KVDZT, IZ
      INTEGER IV, NR, NH, IROW
      REAL VUW(3), XYZ(3),RAD,ZHIT,corr,CSQ,CNST
      INTEGER NVDZT,ILAY,IWFF,IFAC,IVIEW,IRET,JWAF
      INTEGER LDBAS,KVDIZ,NAVDIZ
C     INTEGER ISIGN            done in bank
      INTEGER ID,NRBOS,L
      INTEGER LROWS,LCOLS,KNEXT,KROW,LFRROW,LFRWRD,ITABL
      REAL RTABL
      INTEGER IMOD,IWAF

      INTEGER NAMIND,VVUWXY,VJWFFW,VFWAFF,VDYEAR,MDARD,JUNIDB

      LOGICAL FIRST
      DATA FIRST /.TRUE./
#include "bmacro.h"
C 
C  Do not apply the correction for LEP1
C
      IF(VDYEAR() .NE.95)GO TO 999
C
C  Do not apply on monte carlo events
C
      IF(IW(NAMIND('AJOB')).NE.0) go to 999

      IF(FIRST)THEN
        FIRST = .FALSE.
        LDBAS = JUNIDB(0)
        IRET = MDARD(IW,LDBAS,'VDIZ',0)
        IF(IRET .LE.0) GO TO 998
        NAVDIZ = NAMIND('VDIZ')
      ENDIF
      
      KVDIZ = IW(NAVDIZ)
      IF(KVDIZ .LE. 0)GO TO 998
      IV = 1
      NVDZT=NAMIND('VDZT')
      KVDZT=NVDZT+1
 30   KVDZT=IW(KVDZT-1)
      IF(KVDZT.EQ.0)GO TO 31
      NR=IW(KVDZT-2)
      NH=LROWS(KVDZT)
      DO 60 IZ=1,NH
C
C     Decode the readout module address. We only want the layer
C
      CALL VADEWA(NR,ILAY,IWFF,IFAC,IVIEW)
C
C     Find the wafer in module, IWAF 
C
      IRET = VFWAFF(IWFF,IMOD,IWAF)
C
C     Make global wafer index JWAF
C
      IRET = VJWFFW(ILAY,IFAC,IWFF,JWAF)
C
C     Make approximate radius, and costheta
C
      VUW(1) = 0.
      VUW(2) = 0.
      VUW(3) = RTABL(KVDZT,IZ,JVDZWC)
      IRET = VVUWXY(VUW,JWAF,XYZ)
      RAD = SQRT(XYZ(1)**2+XYZ(2)**2+XYZ(3)**2)
      ZHIT = RTABL(KVDZT,IZ,JVDZZC)
C
C Access constant and quadratic terms 
C
      CSQ = RTABL(KVDIZ,ILAY,JVDIQT)
      CNST = RTABL(KVDIZ,ILAY,JVDICT+IWAF-1)
C
C     Compute the correction 
C
      CORR = (CSQ*ZHIT**2 + CNST) * SIGN(1.,ZHIT)      

      ZHIT = ZHIT + CORR      
      VUW(3) = VUW(3) + CORR      
C
C-- put the corrected position back into VDZT
C
      IROW = KROW(KVDZT,IZ)
      RW(IROW+JVDZZC) = ZHIT
      RW(IROW+JVDZWC) = VUW(3)
 60   CONTINUE
      GO TO 30
 31   CONTINUE
 999  RETURN
 998  CALL RERROR('VDFIXZ', -1, 'VDIZ could not be loaded from DAF')
      RETURN
      END
#endif



























