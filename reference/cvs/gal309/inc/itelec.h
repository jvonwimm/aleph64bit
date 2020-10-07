*CD itelec
      COMMON /ITELEC/TDCRIT,TOFFIT,TFINIT(8),ITTDCL(8),ITTDCH(8),
     +               TDCZIT,ITZTZB,ZEXPIT,ZPARIT(3),ITZTDL(8),
     +               ITZTDH(8),ZLOFIT(8),ZRESIT(2,8)
C
#if defined(DOC)
   ITC R-phi TDC and Z TDC parameters.
        TDCRIT = R-Phi TDC bin width (ns).
        TOFFIT = TDC timing offset, T0 (ns).
        TFINIT = Fine tune corrections to T0 per layer (ns).
        ITTDCL = Minimum allowed TDC count in layer.
        ITTDCH = Maximum allowed TDC count in layer.
        TDCZIT = Z TDC bin width (ns).
        ITZTZB = Z TDC bin corresponding to Z=0.
        ZEXPIT = Time expansion factor for outer layer.
        ZPARIT = Z non-linearity parameters "S-bend".
        ITZTDL = Minimum allowed Z TDC count on layer.
        ITZTDH = Maximum allowed Z TDC count on layer.
        ZLOFIT = Layer offsets to measured Z from IZNL.
        ZRESIT = Z resolution parameters from IZRS.
#endif
