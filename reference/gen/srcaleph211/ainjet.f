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
      SAVE
C
      PARAMETER (L1MST=40, L1PAR=80)
      PARAMETER (L2KTYP=120, L2PMAS=120, L2PWID=60, L2KFR=80, L2CFR=40)
      PARAMETER (L3DPAR=20, L3IDB=120, L3CBR=400, L3KDP=1600)
      PARAMETER (LEMSTE=40, LEPARE=80)
      PARAMETER (LJNPAR=2000)
      COMMON /LUDAT1/   MSTLU1(L1MST),PARLU1(L1PAR)
      COMMON /LUDAT2/   KTYPL2(L2KTYP),PMASL2(L2PMAS),PWIDL2(L2PWID)
     &                , KFRLU2(L2KFR),CFRLU2(L2CFR)
      COMMON /LUDAT3/   DPARL3(L3DPAR),IDBLU3(L3IDB),CBRLU3(L3CBR)
     &                , KDPLU3(L3KDP)
      COMMON /LUDATE/   MSTELE(LEMSTE),PARELE(LEPARE)
      COMMON /LUJETS/   NPARLU,KODELU(LJNPAR,2),PARTLU(LJNPAR,5)
C
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
