#if defined(IBM)
C     ------MPARAM
C     MACHINE DEPENDENT PARAMETER
C
      CHARACTER*6 MACHIN
      PARAMETER ( NBITWD =   32, NBYTWD = 4, NPR = 6, NCA = 5,
     1            NBLKD  = 1610, NBLD16 = 1800, MACHIN = 'IBM   ')
C     ------
#else
#if defined(ALEPH_DEC)
C     ------MPARAM
C     MACHINE DEPENDENT PARAMETER
C
      CHARACTER*6 MACHIN
      PARAMETER ( NBITWD =   32, NBYTWD = 4, NPR = 6, NCA = 5,
     1            NBLKD  = 1610, NBLD16 = 1800, MACHIN = 'DEC   ')
C     ------
#else
#if defined(GOULD)
C     ------MPARAM
C     MACHINE DEPENDENT PARAMETER
C
      CHARACTER*6 MACHIN
      PARAMETER ( NBITWD =   32, NBYTWD = 4, NPR = 6, NCA = 5,
     1            NBLKD  = 1610, NBLD16 = 1800, MACHIN = 'GOULD ')
C     ------
#else
#if defined(APOLLO)
C     ------MPARAM
C     MACHINE DEPENDENT PARAMETER
C
      CHARACTER*6 MACHIN
      PARAMETER ( NBITWD =   32, NBYTWD = 4, NPR = 6, NCA = 5,
     1            NBLKD  = 1610, NBLD16 = 1800, MACHIN = 'APOLLO')
C     ------
#else
#if defined(CRAY)
C     ------MPARAM
C     MACHINE DEPENDENT PARAMETER
C
      CHARACTER*6 MACHIN
      PARAMETER ( NBITWD =   64, NBYTWD = 8, NPR = 6, NCA = 5,
     1            NBLKD  = 1610, NBLD16 = 1800, MACHIN = 'CRAY  ')
C     ------
#else
#if defined(ALEPH_HP)
C     ------MPARAM
C     MACHINE DEPENDENT PARAMETER
C
      CHARACTER*6 MACHIN
      PARAMETER ( NBITWD =   32, NBYTWD = 4, NPR = 6, NCA = 5,
     1            NBLKD  = 1610, NBLD16 = 1800, MACHIN = 'HP    ')
C     ------
#else
#if defined(ALEPH_SGI)
C     ------MPARAM
C     MACHINE DEPENDENT PARAMETER
C
      CHARACTER*6 MACHIN
      PARAMETER ( NBITWD =   32, NBYTWD = 4, NPR = 6, NCA = 5,
     1            NBLKD  = 1610, NBLD16 = 1800, MACHIN = 'SGI   ')
C     ------
#else
#if defined(ALEPH_LINUX)
C     ------MPARAM
C     MACHINE DEPENDENT PARAMETER
C
      CHARACTER*6 MACHIN
      PARAMETER ( NBITWD =   32, NBYTWD = 4, NPR = 6, NCA = 5,
     1            NBLKD  = 1610, NBLD16 = 1800, MACHIN = 'LINUX ')
C     ------
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
