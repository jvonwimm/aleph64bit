      SUBROUTINE AINJET
C
C----------------------------------------------------------------------
C! initialize parts of the LUND commons for ALCLUS avoiding BLOCK DATA
C!
C!    Author:    T.Sjostrand
C!    Modified:  S.Haywood
C!
C! Description
C! -----------
C!    See LUND Monte-Carlo write-up.
C!
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "lundcom.h"
      INTEGER MSTL(L1MST),MSTEL(LEMSTE)
      REAL PARL(L1PAR),PAREL(LEPARE)
C
C++   LUDATA:
C
C...LUDAT1, CONTAINING STATUS CODES AND MOST PARAMETERS
      DATA MSTL/
     1    0,    0,    0,    1,    1,    0,    2,    0,    0,    1,
     2    0,    1,   10,    0,    0,    0,    0,    0,    1,    6,
     3    1,    0,    1,    0,    0,    0,    0,    0,    0, 2000,
     4    0,    0,    0,    0,    0,    0,    0,    0,    0,    0/
      DATA PARL/
     1 0.10, 0.30, 0.40, 0.05, 0.50, 0.50, 0.50, 0.50, 0.60, 0.75,
     2  1.0, 0.35,  1.0,  1.0,   0.,  1.0,  1.0,  2.0,   0.,   0.,
     3 0.10,  1.0,  0.8,  1.5,  0.8,  2.0,  0.2,  2.5,  0.6,  2.5,
     4  0.5,  0.9,  0.5,  0.9,  0.5,  0.9,  1.0,   0.,   0.,   0.,
     5 0.77, 0.77, 0.77,   0.,   0.,   0.,   0.,   0.,  1.0, 0.77,
     6  1.0,  1.0,   0.,  1.0,   0.,   0.,   0.,   0., 0.09, 0.01,
     7   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     8  3.1415927,  6.2831854,   0.,0.001,   0., 5*0./
C
C++   LUDATE:
C
      DATA MSTEL/
     1    3,    2,    7,    5,    1,    1,    0,    2,    1,    0,
     2    2,    4,    2,    2,    5,    0,    0,    0,    0,    0,
     3   42,    1,    1,   25,   24,    1,    0,    0,    0,    1,
     4    1,    1,    0,    0,    0,    0,    0,    0,    0,    0/
      DATA PAREL/
     1  1.5,  0.5, 0.20,0.0072974,0.229,94.,2.8, 0.02,  2.0,  1.0,
     2   0.,   0.,   0.,   0., 0.01, 0.99,  0.2,   0.,   0.,   0.,
     3 0.40,  1.0,   0.,   0.,   0.,   0.,   0.,   0.,   0.,  2.0,
     4  1.0, 0.25,  2.5,0.0001, 2.5,  1.5,  7.0,  1.0,  0.5,  2.0,
     5  40*0./
      CALL UCOPY(MSTL,MSTLU1,L1MST)
      CALL UCOPY(MSTEL,MSTELE,LEMSTE)
      CALL UCOPY(PARL,PARLU1,L1PAR)
      CALL UCOPY(PAREL,PARELE,LEPARE)
      END
#endif
