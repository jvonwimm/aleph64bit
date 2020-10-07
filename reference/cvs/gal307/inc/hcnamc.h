*CD hcnamc
      PARAMETER(NHTTR=192,NHWTR=36)
      PARAMETER (MHCSE=200)
      PARAMETER(MHCTH=300)
      COMMON /HCNAMC/NAHTHT,NAHWHT,NAHTTR,NAHWTR,NAHTDI,NAHWDI, NAHTDT,
     &               NAHTTD,NAHWDT,NAHWTD,NAHPDI,NAHPHT,NAHLWD,
     &               JDHCSE,JDHCHI,JDHCTH
#if defined(DOC)
C!       Definition of various quantities concerning Bos banks
         NAXXXX points to named bank XXXX
         IDXXXX points to work bank XXXX
         NXXXX is the number of rows (if known beforehand)
         MXXXX is the maximum number of rows to be allocated
             for a bank (if no estimate is possible)
        JDHCHI    pointer to the work bank HCHI
        JDHCSE    pointer to the work bank HCSE
        JDHCTH    pointer to the work bank HCTH
        JDHWHT    pointer to the temporary work bank HWHT
C
        NHTTR  # of trigger segments
        NHWTR  # of modules
#endif
