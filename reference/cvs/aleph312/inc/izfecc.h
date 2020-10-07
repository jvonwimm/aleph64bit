      INTEGER JSPAIZ,JLAYIZ,IBN0IZ,ITLOIZ,ITHIIZ,IBNDIZ
      REAL BWIDIZ,EXP8IZ,SBNDIZ
      LOGICAL FZCOIZ
      PARAMETER (JSPAIZ=3,JLAYIZ=8)
      COMMON/IZFECC/BWIDIZ,IBN0IZ,EXP8IZ,SBNDIZ(JSPAIZ),IBNDIZ,
     +              ITLOIZ(JLAYIZ),ITHIIZ(JLAYIZ),FZCOIZ
#if defined(DOC)
C ITC Z Front end Parameters (from DB banks IZFE and ISFE)
C
C BWIDIZ    =  Bin Width of Z TDCs for ITC  (ns.)
C IBN0IZ    =  TDC Bin number corresponding to Z=0
C EXP8IZ    =  Time expansion factor for outermost layer.
C SBNDIZ(i) =  Parameters to decribe the non-linearity in the Z to time
C               relation.
C IBNDIZ    =  S-Bend correction flag: = 0 correction to be done offline
C                                      = 1 correction done online.
C ITLOIZ(i) =  Minimum allowed TDC value on layer i
C ITHIIZ(i) =  Maximum allowed TDC value on layer i
C FZCOIZ    =  .TRUE. if Z TDC data is O.K., i.e. Z coords can be made.
#endif
