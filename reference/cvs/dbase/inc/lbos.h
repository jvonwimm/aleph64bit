      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)  
C - NAMAX = number of ALEPH banks + number of data cards
C   do not forget to update NBNK in SBKPAR  
      PARAMETER (LBOS=900000,NAMAX=1520)
      COMMON /BCS/   IW(LBOS)   
      INTEGER IW
      REAL RW(1000) 
      EQUIVALENCE (RW(1),IW(1)) 
C   
