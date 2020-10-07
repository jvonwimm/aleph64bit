      PARAMETER( NPHMAX= 200 )
      PARAMETER( LCLU= 200, LNEA= 4)
      COMMON /CUPHCO/ NHCO,IOKPH(NPHMAX),
     &                ECOPH(NPHMAX),ENOPH(NPHMAX),
     &                IDPPH(NPHMAX),NPLPH(NPHMAX),
     &                N10PH(NPHMAX),EDPPH(NPHMAX),
     &                ELUPH(NPHMAX)
      COMMON /CUMOST/ JST(LNEA,LCLU),ETW(LNEA,LCLU),
     .                TWH(LNEA,LCLU),PWH(LNEA,LCLU)
C
