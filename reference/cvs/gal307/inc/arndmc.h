*CD arndmc
      PARAMETER (LRVEC=100)
      COMMON /ARNDMC/ RVEC(LRVEC),NRVEC,ISEED1,ISEED2
#if defined(DOC)
C! RANECU server common block / INTERNAL
   RVEC         list of RANECU random numbers
   LRVEC        length of the RANECU list
   NRVEC        pointer in the RANECU list
   ISEED1       1st RANECU seed
   ISEED2       2nd RANECU seed
#endif
