C! Aleph High Voltage bit definitions
      INTEGER NHVBIT
      PARAMETER(NHVBIT=24)
      INTEGER JEEA,JEEB,JEBR,JLCAL,JTEX,JITC,JSATR,JHEA,JHEB,JHBR,
     &  JSCAL,JBCAL,JVDET,JTGA,JTGC,JTTR,JSP1,JSP2,JSP3,JSP4,JSP5,
     &  JSP6,JSP7,JBEAM
      PARAMETER (JEEA=1,JEEB=2,JEBR=3,JLCAL=4,JTEX=5,JITC=6,JSATR=7,
     &  JHEA=8,JHEB=9,JHBR=10,JSCAL=11,JBCAL=12,JVDET=13,JTGA=14,
     &  JTGC=15,JTTR=16,JSP1=17,JSP2=18,JSP3=19,JSP4=20,JSP5=21,
     &  JSP6=22,JSP7=23,JBEAM=24)
#if defined(DOC)
C    NHVBIT is number of highest used bit (used for array dimensions)
C    Jxxx   are the HV bits for each subdetector. Lowest bit is bit 1
C     EEA   ECAL end A
C     EEB   ECAL end B
C     EBR   ECAL barrel
C     LCAL  LCAL
C     TEX   TPC dEdx
C     ITC   ITC
C     SATR  SATR or SAMBA
C     HEA   HCAL end A
C     HEB   HCAL end B
C     HBR   HCAL barrel
C     SCAL  SiCAL
C     BCAL  BCAL
C     VDET  VDET
C     TGA   Trigger analogue voltage
C     TGC   Trigger CAMAC voltage
C     TTR   TPC
C     SP1   Not used by XLUMOK (Actually: VDET not off)
C     SP2   Not used by XLUMOK (Actually: spare)
C     SP3   Not used by XLUMOK (Actually: spare)
C     SP4   Not used by XLUMOK (Actually: spare)
C     SP5   Not used by XLUMOK (Actually: spare)
C     SP6   Not used by XLUMOK (Actually: spare)
C     SP7   Not used by XLUMOK (Actually: T0 synchronised)
C     BEAM  Beam pickup
#endif
