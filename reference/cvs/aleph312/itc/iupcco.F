      SUBROUTINE IUPCCO(ITK,IFLMX,IER)
C-----------------------------------------------------------------------
C! Update ICCO bank for coords. on one track.
C!
CKEY ITC
C!  Author  :- J. Sedgbeer   20/04/90
C!  Modified:- J.Sedgbeer 24/10/91 Use IRESRP to set rphi res. in ICCO
C!  Modified:- J.Sedgbeer 12/11/91 Protect against v. small DOCAs
C!
C!    Input:
C!      ITK     /I  :  Track number (number of bank IQXT)
C!      IFLMX   /I  :  Only coords (in IQXT) with Flag less than or
C!                     equal to IFLMX are stored in ICCO
C!      commons:       /BCS/ for banks IQXT and ITCO
C!      parameters:    ITCOJJ
C!                     ICCOJJ
C!                     IQXTJJ
C!                     ALCONS
C!
C!    Output:
C!      IER   /I    : Error flag:
C!                       IER = 0 if all O.K.
C!                       IER = 1 if no ITCO or IQXT banks
C!                       IER = 2 if no room to create ICCO
C!                       IER = -1 O.K. but garbage collection done.
C!      ICCO   bank of ITC corrected coords. updated.
C!
C!    calls     : AUBOS  (Alephlib)
C!
C!    Libraries required: BOS
C!
C? Get ITCO bank - if none (or empty) set error flag IER=1 and return
C? Get IQXT bank number ITK if none (or empty) set IER=1 and return
C? Create/locate ICCO bank
C? If no room for ICCO set IER=2 and return
C? If garbage collection done then set IER=-1
C? Loop over layers
C?   Get flag from IQXT, skip if flag > IFLMX
C?   Get coord from IQXT, skip if no coord on this layer
C?   Use hit and wire posit. etc. to calc coord. position
C?   Store coord in ICCO
C? End Loop
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C I/O commons and parameters
#include "bcs.h"
#include "iqxtjj.h"
#include "itcojj.h"
#include "iccojj.h"
#include "alcons.h"
C-----------------------------------------------------------------------
      EXTERNAL NAMIND,NLINK,IRESRP,AUARCM
      REAL IRESRP
      INTEGER ITK,IER
      LOGICAL FIRST,DEB
      DATA FIRST/.TRUE./,DEB/.FALSE./
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
C Initialise
      IF(FIRST) THEN
        FIRST = .FALSE.
        LPR = IW(6)
        LITCO = NAMIND('ITCO')
        LICCO = NAMIND('ICCO')
        CALL BKFMT('ICCO','2I,(5F)')
      ENDIF
C
C Check for banks ITCO and IQXT (number ITK)
C
      IER = 1
      JITCO = IW(LITCO)
      IF(JITCO.LE.0) GOTO 999
      NCO = LROWS(JITCO)
      IF(NCO.LE.0) GOTO 999
      JIQXT = NLINK('IQXT',ITK)
      IF(JIQXT.LE.0) GOTO 999
      IER = 0
C
C Create/Locate ICCO bank
C
      LEN = NCO*LICCOA + LMHLEN
      CALL AUBOS('ICCO',0,LEN,JICCO,IGARB)
      IF(IGARB.EQ.2) THEN
        IER = 2
        GOTO 999
      ENDIF
      IF(IGARB.EQ.1) IER = -1
C
      IW(JICCO+LMHCOL) = LICCOA
      IW(JICCO+LMHROW) = NCO
C
      IF(DEB) WRITE(LPR,1000) IGARB
 1000 FORMAT(' IUPCCO: ICCO located. IGARB=',I2)
C-----------------------------------------------------------------------
C Loop over layers.
C
      DO 100 LAY=1,8
        JJ = KROW(JIQXT,LAY)
C
C Skip layer if Flag value too big or no coord.
        IFLG = IW(JJ+JIQXFL)
        IF(IFLG.GT.IFLMX) GOTO 100
        IC = IW(JJ+JIQXCO)
        IF(IC.EQ.0) GOTO 100
C
        JC = KROW(JICCO,IC)
        JT = KROW(JITCO,IC)
C
C  Compute the x,y Point of the measurement...
        HTX = RW(JJ+JIQXHX)
        HTY = RW(JJ+JIQXHX+1)
        WRX = RW(JJ+JIQXWX)
        WRY = RW(JJ+JIQXWX+1)
        DDS = RW(JJ+JIQXDD)
        DCA = RW(JJ+JIQXDC)
        DXVEC = HTX - WRX
        DYVEC = HTY - WRY
C Protect against very small DOCA (divide check)
        IF(ABS(DCA).LT.1.E-5) THEN
          DXS = 0.
          DYS = 0.
        ELSE
          DXS = DXVEC*ABS(DDS/DCA)
          DYS = DYVEC*ABS(DDS/DCA)
        ENDIF
        XHIT  = WRX + DXS
        YHIT  = WRY + DYS
C
        IF(DEB) WRITE(LPR,1001) LAY,IC,DXVEC,DXS,XHIT,DYVEC,DYS,YHIT
 1001   FORMAT(' lay-co',2I4,' dx-dxs-xhit',3F7.3,' dy-dys-yhit',3F7.3)
C
C Store the results away in the Bank ICCO
        RW(JC+JICCRV) = SQRT(XHIT**2 + YHIT**2)
        PHI           = ATAN2(YHIT,XHIT)
        RW(JC+JICCPH) = AMOD(PHI + TWOPI, TWOPI)
        PHTR = ATAN2(HTY,HTX)
        IF(ABS(DCA).LT.1.E-5) THEN
          PHEN = PHTR
        ELSE
          PHEN = ATAN2(-DXVEC,DYVEC)
        ENDIF
        CALP = COS( AUARCM(PHEN-PHTR) )
        RW(JC+JICCSR) = ( IRESRP(LAY,DDS,CALP) )**2
C Z of track = RW(JC+JICCZV) = RW(JJ+JIQXHX+2)
C Use Z as calculated from Z digitising
        RW(JC+JICCZV) = RW(JT+JITCZH)
        RW(JC+JICCSZ) = RW(JT+JITCSZ)
C
        IF(DEB) WRITE(LPR,1002) (RW(JC+I),I=1,5)
 1002   FORMAT('   ICCO: R-Phi-z-sigr-sigz',F8.3,F8.4,F8.1,F8.5,F10.1)
  100 CONTINUE
C
  999 CONTINUE
      END
#endif
