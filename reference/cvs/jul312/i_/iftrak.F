      SUBROUTINE IFTRAK
C-----------------------------------------------------------------------
C! Finalise ITC tracking
C!
C!    Author      :- J. Sedgbeer 87/04/22
C!    Modified    :- I. Tomalin  88/11/17
C!
C!   Input:
C!     need common: /BCS/,/ITRKCC/,/IWBNKT/
C!          params: IGTLJJ,ITFTJJ
C!
C!   Output:
C!      finalised banks ITFT#2, IGCL#2, IGTL#2
C!
C!   called by : ITCTRK
C!   calls     : None
C!
C!   Libraries required: BOS,CERNLIB
C!
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
#include "rlunit.h"
C-----------------------------------------------------------------------
C I/O commons
#include "bcs.h"
#include "bosext.h"
#include "itrkcc.h"
#include "iwbnkt.h"
#include "igtljj.h"
#include "itftjj.h"
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
C Compress ITFT bank # 2
C
      KITFT = NLINK('ITFT',2)
      IF(KITFT.GT.0) THEN
        NWTK  = LCOLS(KITFT)
        NTK   = LROWS(KITFT)
        LITFT = NWTK*NTK + LMHLEN
        CALL AUBOS('ITFT',2,LITFT,KITFT,IGARB)
        IF (IGARB.EQ.2) KITFT = NLINK('ITFT',2)
C
C*********************************************************************
C ***** The following transformation has been commented out, as *****
C ***** it was decided to leave the error matrix uncorrected.   *****
C Extend the error matrix to include the effect of scattering between
C the ITC and the TPC.
C        DO 25 J=1,NTK
C          JITFT = KROW(KITFT,J)
C          R0 = 1.0/RW(JITFT+JITFIR)
C          TLAM = RW(JITFT+JITFTL)
C          D0 = RW(JITFT+JITFD0)
C Calculate the angle of the track at the scattering radius.
C          SINB = (0.5*(RSCAIT**2-D0**2)+R0*D0)/(RSCAIT*R0)
C          IF(ABS(SINB).GT.1.0) GOTO 25
C          TPENA = ASIN(SINB)
C Put the r.m.s. scattering angle of the track into the ITFT bank.
C          PT = FRHOIT*ABS(R0)
C          THSCT = FSCAIT/(PT*SQRT(COS(TPENA)*SQRT(1.0+TLAM**2)))
C          RW(JITFT+(JITFCM-1)+21) = THSCT**2
C   25   CONTINUE
C**********************************************************************
C
        IF(FDEBRF.AND.JDBDRF(JULIT).GE.3) THEN
          WRITE(LDEBRL,1001)
 1001     FORMAT('0IFTRAK: ITFT bank number 2',/'  Track',4X,'sig/r0',
     +      2X,'TanLam',4X,'Phi0',5X,'d0',5X,'z0',2X,'ScattAng',
     +      2X,'Chisq',3X,'NDF',2X,'IOPT')
          DO 35 I=1,NTK
            KK = KROW(KITFT,I)
            WRITE(LDEBRL,1002) I,(RW(KK+J),J=1,6),RW(KK+JITFCD),
     +                          IW(KK+JITFND),IW(KK+JITFFT)
 1002       FORMAT(I7,F10.4,2F8.4,F7.3,F7.1,F10.5,F7.1,2I6)
   35     CONTINUE
        END IF
      ENDIF
C
C Compress IGTL bank
C
      KIGTL = NLINK('IGTL',2)
      IF(KIGTL.NE.0) THEN
        NWTK  = LCOLS(KIGTL)
        NTK   = LROWS(KIGTL)
        LIGTL = NWTK*NTK + LMHLEN
        CALL AUBOS('IGTL',2,LIGTL,KIGTL,IGAR1)
      ENDIF
C
C Compress IGCL bank
C
      KIGCL = NLINK('IGCL',2)
      IF(KIGCL.NE.0) THEN
        NWTK  = LCOLS(KIGCL)
        NTK   = LROWS(KIGCL)
        LIGCL = NWTK*NTK + LMHLEN
        CALL AUBOS('IGCL',2,LIGCL,KIGCL,IGAR2)
      ENDIF
C
C Print
C
      IF(FDEBRF.AND.JDBDRF(JULIT).GE.4) THEN
        KIGTL = NLINK('IGTL',2)
        KIGCL = NLINK('IGCL',2)
        LL = IW(KIGTL)
        WRITE(LDEBRL,1006) (IW(KIGTL+K),K=1,LL)
 1006   FORMAT('    IGTL:',2I5,/(9X,5(2X,2I4)))
        LL = IW(KIGCL)
        WRITE(LDEBRL,1007) (IW(KIGCL+K),K=1,LL)
 1007   FORMAT('    IGCL:',2I5,/(9X,20I5))
      ENDIF
C
C Add banks to 'T' list
C
      CALL BLIST(IW,'T+','ITFTIGCLIGTL')
C
C Drop work banks
C
      IF (IUCOIW.NE.0) CALL WDROP(IW,IUCOIW)
      IF (IWHTIW.NE.0) CALL WDROP(IW,IWHTIW)
      IF (ILNKIW.NE.0) CALL WDROP(IW,ILNKIW)
      IF (INNXIW.NE.0) CALL WDROP(IW,INNXIW)
      IF (IOFNIW.NE.0) CALL WDROP(IW,IOFNIW)
      IF (INXLIW.NE.0) CALL WDROP(IW,INXLIW)
      END
#endif
