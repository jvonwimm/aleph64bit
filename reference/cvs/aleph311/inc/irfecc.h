      INTEGER JLAYIR,IBN0IR,ITLOIR,ITHIIR
      REAL BWIDIR
      PARAMETER (JLAYIR=8)
      COMMON/IRFECC/BWIDIR,IBN0IR,ITLOIR(JLAYIR),ITHIIR(JLAYIR)
#if defined(DOC)
C ITC R-Phi Front end Parameters
C
C BWIDIR    =  Bin Width of R-phi TDCs for ITC  (ns.)
C IBN0IR    =  TDC bin corresponding to sense wire (Drift dist = 0)
C ITLOIR(i) =  Minimum allowed TDC value on layer i
C ITHIIR(i) =  Maximum allowed TDC value on layer i
#endif
