      SUBROUTINE LCPAXY(IADDR,XYZ)
C-----------------------------------------------------------------------
C!   Find coordinates of Lcal tower
C!
C!   Author   : P. H. Hansen  870801
C!
C!     Input  :
C!            - IADDR      : Tower address
C!              'LLAY','LMTY','LSLO'
C!
C!     Output :
C!            - XYZ(3)   : ALEPH coordinates assuming no misalignment
C!
C!     Description :
C!     ===========
C!     Find coordinates of an LCAL tower at plane number NDEPLC
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "lcnamc.h"
#include "lcaljj.h"
#include "lslojj.h"
#include "lcrejj.h"
      DIMENSION XYZ(*)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
#include "bmacro.h"
#include "lcmacr.h"
C-----------------------------------------------------------------------
C
C Intialise global constants
      IF(FIRST) THEN
         KLCAL = IW(NALCAL)
         KLCRE = IW(NALCRE)
         ZSTEP = RTABL(KLCRE,1,JLCRZC) - RTABL(KLCAL,1,JLCAZD)
         SIZE  = RTABL(KLCRE,1,JLCRSR)
      ENDIF
      FIRST = .FALSE.
C
C Decoding
      MODU=LCMOD(IADDR)
      IROW=LCROW(IADDR)
      ICOL=LCCOL(IADDR)
C
C Get coordinates of module
      KLSLO = IW(NALSLO)
      KSLO=KROW(KLSLO,MODU)
      CALL UCOPY(RW(KSLO+JLSLXS),XYZ(1),3)
C
C Step in z through front plate and the first NDEP planes
      ZST = ZSTEP*SIGN(1.,XYZ(3))
C
C Step in transverse plane out to ICOL,IROW
      XSTEP = FLOAT(ICOL)*SIZE
      IF(MODU.EQ.1.OR.MODU.EQ.3) XSTEP=-XSTEP
      YSTEP = (FLOAT(IROW-15)-0.5)*SIZE
C
C Position
      XYZ(1)=XYZ(1) + XSTEP
      XYZ(2)=XYZ(2) + YSTEP
      XYZ(3)=XYZ(3) + ZST
C
  999 CONTINUE
      END
#endif
