      INTEGER FUNCTION IGTFMT(NAMJ)
C --------------------------------------------------
C - F.Ranjard - 900830
C! get bank format index
C   returns the work bank format index for this bank
C - Input   : - NAMJ / I    = bank name-index
C --------------------------------------------------
*MACRO BOSCOM
C
      COMMON /BCS/IW(1000)
      COMMON /SYSBOS/NSYST,NAMES,NPRIM,IDNAM,IDPTR,
     1               IDFMT,NDUMM,NRESR,NLPLM, NARR,
     2               IARR(10),
     3               IEFMT,TLEFT,
     4               LEPIO,NAMI,INDI,INDJ,IBC,DUMMI(73),
     5               INTA(200), NPTR,NRUN,NEVT,
     6               LUNDAT,LUNSEL,LUNSE2,LUTDAT,MASKR,LMASK,
     7               NRE,NAMERE(3),NUMMRE(3),IRUNRE(3),IEVTRE(3)
C
C -----------------------------------------------------------------
C     get the format
      IGTFMT=IW(IDFMT+NAMJ-NSYST)
      END
