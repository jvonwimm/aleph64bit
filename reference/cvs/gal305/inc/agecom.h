*CD agecom
      PARAMETER ( NLIMR = 9 , NLIMZ = 6)
      COMMON / AGECOM / AGLIMR(NLIMR),AGLIMZ(NLIMZ),IAGFLD,IAGFLI,IAGFLO
#if defined(DOC)
     Limits in R and Z to define fisrt level of geometry tree
     used in AGXXXXroutines
      AGLIMR = R Limits for geometry volumes
      AGLIMZ = Z Limits for geometry volumes
      IAGFLD = Tracking flag for uniform Bz field (default 3)
      IAGFLI = tracking flag for non uniform B    (default 1)
      IAGFLO = Tracking flag for no field region  (default 0)
#endif
