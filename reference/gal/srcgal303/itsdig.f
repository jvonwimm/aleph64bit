      SUBROUTINE ITSDIG(NWIRS,IWIR,TAB,IDAB)
C.
C...ITSDIG  2.10  900517  15:38                        R.Beuselinck.
C.
C!  Calculate the digitised values of the ITC data.
C.  Store the results in the work banks (indices are stored in ITNAMC)
C.  The BOS structure must not be modified by this routine or its
C.  descendants.
C.
C.  Called by: ITDAQ                               from this .HLB
C.      Calls: ITTEXP                              from this .HLB
C.
C-----------------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON /ITELEC/TDCRIT,TOFFIT,TFINIT(8),ITTDCL(8),ITTDCH(8),
     +               TDCZIT,ITZTZB,ZEXPIT,ZPARIT(3),ITZTDL(8),
     +               ITZTDH(8),ZLOFIT(8),ZRESIT(2,8)
C
      PARAMETER (LIHIT=8, MIHIT=500, LITWP=3)
      PARAMETER (LITFN=6, LITFP=2)
      COMMON /ITNAMC/NAIHIT,NAIDIG,NAITTR,NAIDHR,NAIXBW,
     + JDITWP,JDITFN,JDITFP,JDITNW,JDITAB,JDITDC,JDIZSC
C
      PARAMETER (LITWBK = 7)
      INTEGER JDITWB(LITWBK)
      EQUIVALENCE (JDITWB(1),JDITWP)
C
      REAL TAB(2)
      INTEGER IDAB(2)
C
C--  Check whether wire is dead or read out from B-end etc.
C--       (not implemented yet)
      IEN = 1
C
C--  Compute the drift time digitisation and reject any bad values.
C--
      LAY = - IW(JDITNW+IWIR)
      ITIM = (TOFFIT + TFINIT(LAY) - TAB(IEN))/TDCRIT
      IF (ITIM.LT.0) ITIM = 0
      IF (ITIM.LT.ITTDCL(LAY)) ITIM = 0
      IF (ITIM.GT.ITTDCH(LAY)) ITIM = 0
C
C--  Fill the drift-time digitisation.
C--
      NWIRS  = NWIRS + 1
      IW(JDITDC+NWIRS) = ITIM
C
C--  Perform the time-expansion calculation.
C--
      CALL ITTEXP(LAY,TAB,TPULS,IZDIG)
      IW(JDIZSC+NWIRS) = IZDIG
C
C--  Fill remaining work bank data for use in trigger processors and
C--  creation of correlation banks (tracks <=> digits).
C--
      IW(JDITNW+IWIR) = LAY
      II = JDITAB + (NWIRS-1)*3
      RW(II+1) = TPULS
      IW(II+2) = IDAB(1)
      IW(II+3) = IDAB(2)
      END