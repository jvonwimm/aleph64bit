*CD iocom
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
#if defined(DOC)
                     Logical unit definitions

      LFIL               number of logical units
      LGETIO             input data file logical unit   (def=1)
      LSAVIO             output data file logical unit  (def=2)
      LGRAIO             graphics logical unit          (def=3)
      LRDBIO             relationnal data base logical unit  (def=4)
      LINPIO             input data card logical unit   (def=5)
      LOUTIO             print out logical unit         (def=6)
      LUNIIO (1-lfil)    equivalent to LGETIO --- LOUTIO
      TFILIO (1-lfil)    name of the file assigned to LUNIIO (1-lfil)
      TFORIO (1-lfil)    file format : 'EPIO' or 'DAIO' or ' '
                                        ' ' means nativ format

#endif
