        PARAMETER (NSLOM=15, NSLOI=12)
        PARAMETER (NMODUL=162)
#if defined(DOC)
C! Detector geometry constants
C Nslom  = num. of phi faces in outer layer
C Nsloi  = max (90/91) number of phi faces in inner layer
C Nmodul = (NSLOM+NSLOI)*4+(NSLOM+NSLOI)*2
#endif
        INTEGER  NHOTEV, NHOTMN
        INTEGER IWADDR(NMODUL), IINDEX(2,2,NSLOM,4)
        COMMON/VHOTPM/  NHOTEV, NHOTMN,IWADDR, IINDEX
