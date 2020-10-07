      SUBROUTINE RFJCON
C----------------------------------------------------------------------
C!  - Fill JCON (Julia CONstants) bank
C!
C!   Author   :- E. Lancon             16-OCT-1991
C!
C!   Modified :- E. Lancon              1-OCT-1993
C!        Add TPC clock frequency and pressure
C!   Modified :- E. Lancon              2-NOV-1993
C!        Add dE/dx constants
C!               F.Ranjard             30-MAY-1994
C!        remove call to TDEDXV
C!        fill JCON with /TMONIT/GAINTM
C!   Modified :- I.Tomalin              7-NOV-1994
C!        Get error on Vz from TLAS if TDPV missing.
C!   Modified :- W.Wiedenmann           12.05.99  
C!        Use clock frequency in Hz from TSOR bank (since 1999)
C!
C!   Called from RINRUN
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "jconjj.h"
#include "tdpvjj.h"
#include "tlasjj.h"
#include "tsorjj.h"
#include "rcurnt.h"
#include "tpgpar.h"
#include "tmonit.h"
      EXTERNAL GTT0GL
      DIMENSION DVSA (3), DVSB (3)
#include "bmacro.h"
C-----------------------------------------------------------------------
      CALL AUBOS ('JCON', IRUNRC, LJCONA+LMHLEN, KJCON, IGARB)
      IF ( IGARB.EQ.2 ) THEN
        CALL RERROR ('RFJCON', -1, 'No Space to book JCON !!')
        GOTO 999
      ELSE
        IW(KJCON+LMHCOL)=LJCONA
        IW(KJCON+LMHROW)=1
        KS=KJCON+LMHLEN
C
C?   Get Drift Velocity
C
        CALL TPDVEL ( ' ', DVSA, DVSB, IER)
        IF ( IER.EQ.0 ) THEN
          RW(KS+JJCODX) = DVSA (1)
          RW(KS+JJCODY) = DVSA (2)
          RW(KS+JJCODZ) = DVSA (3)
          RW(KS+JJCODB) = DVSB (1)
          RW(KS+JJCODB+1) = DVSB (2)
          RW(KS+JJCODB+2) = DVSB (3)
        ENDIF
C
C?   Is there a TDPV/TLAS ? get error on Vz
C
        KTDPV = IW(NAMIND('TDPV'))
        KTLAS = IW(NAMIND('TLAS'))
        IF ( KTDPV.GT.0 ) THEN
          RW(KS+JJCOEZ) = RTABL (KTDPV,1,JTDPEZ)
        ELSE IF ( KTLAS.GT.0 ) THEN
          IF (ABS(RTABL(KTLAS,1,JTLANV)).GT.1.0E-3) THEN
            RW(KS+JJCOEZ) = RTABL (KTLAS,1,JTLANE)
          ELSE
            RW(KS+JJCOEZ) = RTABL (KTLAS,1,JTLALE)
          END IF
        END IF
        RW(KS+JJCOEZ+1) = RW(KS+JJCOEZ)
C
C?   Get TPC T0
C
        RW(KS+JJCOGT) = GTT0GL (IRUNRC)
C
C?   Get the TPC Clock Frequency
C
        KTSOR = IW(NAMIND('TSOR'))
        IF (KTSOR.GT.0) THEN
           IW(KS+JJCOCF) = IW(KTSOR+JTSOCL)
           IF (IW(KTSOR+JTSOCH).NE.0) THEN
              IFREQH=IW(KTSOR+JTSOCH)
              IF (IABS(IW(KTSOR+JTSOCL)-(IFREQH/1000)).LE.1) 
     +           IW(KS+JJCOCF) = NINT(FLOAT(IFREQH)/1000.) 
           ENDIF
        ENDIF
        KTCLK = IW(NAMIND('TCLK'))
        IF (KTCLK.GT.0) THEN
          SL = RW(KTCLK+1)
          IFREQ = NINT(1.0E6/SL)
          IW(KS+JJCOCF) = IFREQ
        ENDIF
C
C?   Get the TPC Pressure
C
        KTSSR = IW(NAMIND('TSSR'))
        IF (KTSSR.GT.0) IW(KS+JJCOTP) = IW(KTSSR+263)
C
C?   Get the dE/dx constants
C
        RW (KS+JJCONR) = 1.
        DO IS =  1, LTSECT
          RW (KS+JJCOSN+IS-1) = GAINTM(IS)
        ENDDO

C
      ENDIF
  999 RETURN
      END
#endif
