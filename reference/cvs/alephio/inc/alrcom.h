      CHARACTER*16 TAPPAR
      PARAMETER (TAPPAR='TAPECARTSMCF8MM ')
      PARAMETER (MINPUT=1,MOUTPT=4)
      COMMON /ALRCOM/ DIRECT
     &               ,NRFILI(MINPUT),IRFILI,IFTHRU
     &               ,NRFILO(MOUTPT),IRFILO,IFWITH
     &               ,NRFILM,IRFILM
      LOGICAL DIRECT
      COMMON /ALRCHA/ NAFILI(MINPUT),NAFILO(MOUTPT)
     &               ,DKMODR(4),DKMODW(5),GMODEN
      CHARACTER*1 DKMODR,DKMODW
      CHARACTER*4 NAFILI, NAFILO
      CHARACTER*10 GMODEN
#if defined(DOC)
      DIRECT          is TRUE if current input file openned in direct
                      access
                      after closure
      NRFILI          bank number of FILI card
      IRFILI          Return code from ACDARG for FILI
      IFTHRU          .NE.0 if THRU parameter present on FILI card
      NRFILO          bank number of FILO/POT/DST/MDST  card
      IRFILO          return code from ACDARG for FILO/POT/DST/...
      IFWITH          .NE.0 if WITH parameter present on FILO/POT/...card
      NRFILM          bank number of FILM card
      IRFILM          return code from ACDARG for FILM
      DKMODR          filemode buffer in read access
      DKMODW          filemode buffer in write access
      NAFILI          name of FILI card
      NAFILO          name of FILO/POT/DST/MDST card
      GMODEN          filemode for Edir files with options "THRU" and "WITH"
#endif
