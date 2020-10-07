      FUNCTION SIEVST (X)
C----------------------------------------------------------------------
C!   - SICAL Returns the Energy at a given time along the Hold timing
C!           curve
C!
C!   Author   : E.Lancon   October 1995
C!   Modified B.Bloch , November 1995 , to get curve from SHLD bank
C!
C!   Description : Compute the energy (GeV) according to the
C!   ===========   parametrized curve as a function of time x ( ns )
C?
C!=====================================================================
#ifndef DOC
      DIMENSION A(7)
      DATA A/    1.8064, 0.10885    ,0.20924E-02,-0.12570E-04,
     $      0.26716E-07,-0.25113E-10,0.87717E-14/
      PARAMETER ( NPAR=7)
#include "bcs.h"
#include "shldjj.h"
      DATA IFIRST/0/
C
#include "bmacro.h"
      IF ( IFIRST.EQ.0) THEN
         IFIRST = 1
         NASHLD = NAMIND('SHLD')
      ENDIF
      KSHLD = IW(NASHLD)
      IF ( KSHLD.GT.0) THEN
         NPARA = LROWS(KSHLD)
         AA = RTABL(KSHLD,1,JSHLPM)
         DO  10  I= 2,NPARA
            AA = AA + X**(I-1)*RTABL(KSHLD,I,JSHLPM)
  10     CONTINUE
         SIEVST = AA
      ELSE
         AA = A(1)
         DO  20 I= 2,NPAR
            AA = AA + X**(I-1)*A(I)
  20     CONTINUE
         SIEVST = AA
      ENDIF
C
      IF (X.LT.0. .OR. X.GT.750.) SIEVST = 0.
      RETURN
      END
#endif
