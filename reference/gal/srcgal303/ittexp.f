      SUBROUTINE ITTEXP(LAY,TAB,TPULS,IZDIG)
C.
C...ITTEXP  2.10  920213  19:18                       R.Beuselinck.
C.
C!  Create time-expanded pulses.  Return the output pulse time and
C.  the digitised value (Z-scalar).
C.
C.  Called by: ITSDIG                            from this .HLB
C.
C-----------------------------------------------------------------------
      SAVE
      COMMON /ITELEC/TDCRIT,TOFFIT,TFINIT(8),ITTDCL(8),ITTDCH(8),
     +               TDCZIT,ITZTZB,ZEXPIT,ZPARIT(3),ITZTDL(8),
     +               ITZTDH(8),ZLOFIT(8),ZRESIT(2,8)
C
      COMMON/ITEXPC/CLOKIT,CLGOIT,TSTRIT,TBINIT,PLENIT(8),EXPFIT(8)
C
      REAL TAB(2)
C
C--  Compute Z TDC bin number.
C--  The resolution smearing is now done on the true Z instead.
C--
      ZT0 = TDCZIT*ITZTZB
      TPULS = ZT0 - (TAB(2) - TAB(1))*EXPFIT(LAY)
      IZDIG = TPULS/TDCZIT
C
C--  Exclude Z digitisings outside the allowed range for the layer.
C--
      IF (IZDIG.LT.ITZTDL(LAY).OR.IZDIG.GT.ITZTDH(LAY)) IZDIG = 0
      END
