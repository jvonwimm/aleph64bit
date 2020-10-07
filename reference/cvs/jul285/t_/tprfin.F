      SUBROUTINE TPRFIN
C
C---------------------------------------------------------------------
C! Initialize pad response function
C!
C!     Author:    D. Schlatter
C!     Modified:  T. Lohse      12-03-87
C!     Modified:  R. Johnson    06-06-89
C!
C! Called by TINIRU at start of each run
C!
C! Description
C! -----------
C! The data statements contain measurements of the squared pad response
C! width measured in TPC90 from laser tracks at three drift lengths
C! and several values of magnetic field.  An interpolation is made
C! to find values appropriate for the present field strength, giving
C! constants to be used by TPRF2 for interpolation in z.
C!
C---------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "tparac.h"
#include "tpgpar.h"
#include "tprzdp.h"
#include "tprfjj.h"
#include "tpccon.h"
#include "rcurnt.h"
#include "rlunit.h"
C
      PARAMETER (MXBFLD=17)
      REAL  BB(MXBFLD,MXZVAL)
      REAL  SS(MXBFLD,MXZVAL),SIG(MXZVAL)
      INTEGER NB(MXZVAL),ALGTDB
      LOGICAL FIRST
      DATA FIRST/.TRUE./,EPS/1./
C
C------------------ Statement Function Definitions ----------------
C
#include "bmacro.h"
C
C-------------------------------------------------------------------
C
C++   Look for the calibration data
C
      IRET=ALGTDB(LRCONS,'TPRF',IRUNRC)
      IF (IRET.EQ.0) THEN
        CALL REPORT('TPRFIN','Constants bank TPRF is missing'//
     &                               ' from D.A. file.',1)
      ENDIF
      IF (FIRST .OR. IRET.LT.0) THEN
        KTPRF=IW(NAMIND('TPRF'))
C
C++     Unfold the database bank into arrays in common.  This code
C++     assumes the data in TPRF is arranged such that all entries
C++     with the same drift length are contiguous, and for a given
C++     drift length, that the entries are in order of increasing
C++     B-field.
C
        NZVLDP=0
        ZOLD=-999.
        DO 34 I=1,LROWS(KTPRF)
          ZV=RTABL(KTPRF,I,JTPRDL)
          IF (ABS(ZV-ZOLD).GT.EPS) THEN
            ZOLD=ZV
            IF (NZVLDP.GE.MXZVAL) GO TO 35
            NZVLDP=NZVLDP+1
            ZZVLDP(NZVLDP)=ZV
            NB(NZVLDP)=0
          ENDIF
          IF (NB(NZVLDP).GE.MXBFLD) GO TO 34
          NB(NZVLDP)=NB(NZVLDP)+1
          BB(NB(NZVLDP),NZVLDP)=RTABL(KTPRF,I,JTPRBF)
          SS(NB(NZVLDP),NZVLDP)=RTABL(KTPRF,I,JTPRRW)
   34   CONTINUE
   35   CONTINUE
C
C++     For each drift length, interpolate the measurements with
C++     an interpolating polynomial of degree 2.
C
        BFF=ABS(BFIELD)
C
        DO 52 I=1,NZVLDP
          SIG(I)=DIVDIF(SS(1,I),BB(1,I),NB(I),BFF,2)
   52   CONTINUE
C
C++     Calculate a slope and intercept for each z region
C
        DO 53 I=2,NZVLDP
          TPRFA1(I)= (SIG(I)-SIG(I-1))/(ZZVLDP(I)-ZZVLDP(I-1))
          TPRFQ1(I)=  SIG(I-1) - ZZVLDP(I-1)*TPRFA1(I)
   53   CONTINUE
C
        FIRST=.FALSE.
      ENDIF
C
      RETURN
      END
#endif
