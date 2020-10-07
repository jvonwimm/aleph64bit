C!    Common for lookup tables for index manipulation
C ----------------------------------------------------------------------
      INTEGER IVSTUP, NMODUL, NWAFER, NFACEL, NWAFEM, JJFACM, JJMODW
      INTEGER JIFACF, JIMODM, JIWAFW, IJFACE, IJMODU, IJWAFR, JIWFFW
      INTEGER IJWFFR
      CHARACTER*4 TXMODU
C
      COMMON / VGINDX / IVSTUP, NMODUL, NWAFER, NFACEL(NVLAYR),
     >                  NWAFEM, JJFACM(NVMMAX), JJMODW(NVWMAX),
     >                  JIFACF(NVFMAX), JIMODM(NVMMAX),
     >                  JIWAFW(NVWMAX), IJFACE(NVLAYR,NVFLMX),
     >                  IJMODU(NVLAYR,NVFLMX,NVMODF),
     >                  IJWAFR(NVLAYR,NVFLMX,NVMODF,NVWMMX),
     >                  JIWFFW(NVWMAX), IJWFFR(NVLAYR,NVFLMX,NVWFMX),
     >                  TXMODU(NVLAYR,NVFMAX,NVMODF)
C
#if defined(DOC)
      IVSTUP                       Setup code for the setup in commons
      NMODUL                       Total number of modules
      NWAFER                       Total number of wafers
      NFACEL(JLAY)                 Number of slots in layer JLAY
      NWAFEM                       Number of wafers per module
      JJFACM(JMOD)                 Global face index
      JJMODW(JWAF)                 Global module index
      JIFACF(JFAC)                 Local face index
      JIMODM(JMOD)                 Local module index
      JIWAFW(JWAF)                 Local wafer index
      IJFACE(ILAY,IFAC)            Global face index
      IJMODU(ILAY,IFAC,IMOD)       Global module index
      IJWAFR(ILAY,IFAC,IMOD,IWAF)  Global wafer index
      JIWFFW(JWAF)                 Local wafer-in-face index
      IJWFFR(ILAY,IFAC,IWFF)       Global wafer index
      TXMODU(ILAY,IFAC,IMOD)       CHARACTER*4 module text name
#endif
