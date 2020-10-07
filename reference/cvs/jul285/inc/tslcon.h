      COMMON/TSLCON/ TSLPRS,TSLPDF,TSLTMP(6),
     &               TSLVTG(LTSECT),TSLCRN(LTSECT),
     &               TSLGTV(3,LTSECT),TSLSTP(5,LTSECT),
     &               TSLPAV(2,LTSECT),TSLGGN,TSLDAT,TSLTIM
      COMMON/TSLCHA/ TSCSTT
      CHARACTER*4 TSCSTT
      INTEGER TSLDAT,TSLTIM
      REAL TSLPRS,TSLPDF,TSLTMP,TSLVTG,TSLCRN,TSLGTV,TSLSTP,TSLPAV
      REAL TSLGGN
#if defined(DOC)
C! TPC slow control information unpacked from bank TSCI
C
C TSCSTT = status word='TSCI' if the bank is filled
C TSLPRS = gas pressure in the TPC
C TSLPDF = difference in pressure between TPC gas and atmosphere
C TSLTMP = gas temperatures
C TSLVTG = high voltage for each sector
C TSLCRN = high voltage current for each sector
C TSLGTV = gating voltages for each sector
C TSLSTP = sector temperatures
C TSLPAV = sector preamp voltages
C TSLGGN = FE55 peak position from monitoring chamber
C TSLDAT = date
C TSLTIM = time
C----------------------------------------------------------------------
#endif
