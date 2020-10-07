      SUBROUTINE ABRUEV (KRUN, KEVT)
C ------------------------------------------------------------------
C - F.Ranjard - 920226  from ABRREC/ENTRY ABRUEV
CKEY ALPHARD RUN EVENT / USER
C!  Returns current run and event number.
C   For event records : KRUN > 0 ; KEVT > 0.
C   For run records :   KRUN > 0 ; KEVT = 0.
C   For other records : KRUN = 0 ; KEVT = 0.
C --------------------------------------------------------------------
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
C ---------------------------------------------------------------
      KRUN = NRUN
      KEVT = NEVT
      END
