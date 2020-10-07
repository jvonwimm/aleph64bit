      SUBROUTINE XLSLUM(XLUM, SLUM, LLUM)
CKEY XLUMOK TRIG /INTERNAL
C----------------------------------------------------------------------
C! Checks HV status, enabled triggers, and t0 synchronization
C! Called from LLUMOK,SLUMOK,XLUMOK
C!    Author:     H. Meinhard   26-Apr-1993
C!    Modified    B. Bloch      November 1997, back to Lcal Lumi> oct 95
C!    Modified    B. Bloch      April    1998, require VDET ok  > apr 98 
C!    Modified    B. Bloch      July     1998, keep trying to get Trigger
C!                Enable mask if not present for first event of run
C!
C!    Input:      - trigger banks, REVH bank, LOLE bank
C!    Output:     - XLUM    /L      both LCAL and SICAL okay
C!                - SLUM    /L      SICAL okay
C!                - LLUM    /L      LCAL okay
C!
C!    Description
C!    ===========
C!    Merges former XLUMOK (based on LCAL) and SLUMOK flags. Based on
C!    programs by John Harton, Jim Wear, Brigitte Bloch.
C!    Purpose: Checks whether this event is viable on the basis of the
C!    minimal criteria set by general analyses. It checks to see if the
C!    ECAL, LCAL/SICAL, TPC (tracking), ITC, HCAL HV bits are set and
C!    whether the Hadronic Z, LCAL/SICAL luminosity, and single charged
C!    EM triggers are enabled.
C!    For MC data:   XLUM always .TRUE.
C!                   SLUM .TRUE. if Sical simulated
C!                   LLUM always .TRUE.
C!    For real data: before run 16'500:
C!                   LLUM .TRUE. if all required HV except SiCAL on, all
C!                        required triggers except SICAL enabled
C!                   SLUM .FALSE.
C!                   XLUM = LLUM
C!    For real data: after run 16'500:
C!                   LLUM .TRUE. if all required HV except SiCAL on, all
C!                        required triggers except SICAL enabled, t0
C!                        synchronization okay
C!                   SLUM .TRUE. if all required HV except LCAL on, all
C!                        required triggers except LCAL enabled, t0
C!                        synchronization okay
C!                   XLUM = SLUM .AND. LLUM
C!    For real data: after run 40000 ( 1995 Lep 1.5 and Lep 2 back to LCAL
C!                   for Lumi, Sical used as calorimeter)
C!                   LLUM .TRUE. if all required HV including SiCAL on, all
C!                        required triggers except SICAL enabled, t0
C!                        synchronization okay
C!                   SLUM .TRUE. if all required HV except LCAL on, all
C!                        required triggers except LCAL enabled, t0
C!                        synchronization okay
C!                   XLUM = SLUM .AND. LLUM
C!    For real data: after run 45000 ( 1998 Lep 2 , require VDET ok)
C!                   XLUM = SLUM .AND. LLUM .AND. XVDEOK
C----------------------------------------------------------------------
#ifndef DOC
      SAVE LRUN, LEVT, XL, SL, LL, ISITP, XTGQOK, XTGLOK, XTGSOK
#include "xhvbit.h"
      INTEGER GTSTUP
      LOGICAL XTGLOK,XHVOK,XHV(NHVBIT),XHVE,XHVL,XHVTR,XHVH,XHVTG
      LOGICAL XLUM,SLUM,LLUM,XL,SL,LL,XHVS,XT0,XT0SYN,XTLOK,XTGSOK
      LOGICAL XTGQOK,SL1,LL1,XL133,XRE133,XL94,XRE94,XVDEOK
      EXTERNAL XT0SYN,GTSTUP,XRE133,XRE94,XVDEOK
      DATA LRUN /-9999/, LEVT/-9999/, IFIR/0/
C----------------------------------------------------------------------
C get run and event number
      CALL abruev(irun,ievt)
      krun = irun
      kevt = ievt
C do all that only if XLSLUM has not yet been called for current event
      IF (KRUN .NE. LRUN .OR. KEVT .NE. LEVT) THEN
C initialize the logicals
        XL = .FALSE.
        SL = .FALSE.
        LL = .FALSE.
        SL1 = .FALSE.
        LL1 = .FALSE.
C get SICAL setup code
        IF (KRUN .NE. LRUN) ISITP = GTSTUP('SI',KRUN)
C Monte Carlo data?
        IF (KRUN .LT. 2000) THEN
          LL = .TRUE.
          SL = ISITP .GT. 0
          SL1 = SL
          LL1 = LL
        ELSE
C real data
C Get enabled triggers ( only if run changed)
          if(krun .NE. lrun) CALL XTGENB(KRUN,XTGQOK,XTGLOK,XTGSOK)
C In case first event is a reject ....info is not kept so keep trying
          if((krun .EQ. lrun).and.(.not.XTGQOK)) 
     &        CALL XTGENB(KRUN,XTGQOK,XTGLOK,XTGSOK) 
C Get and combine HV information
          CALL XHVSTA(XHV)
          XHVTR = XHV(JTTR).AND.XHV(JITC)
          XHVE  = XHV(JEEA).AND.XHV(JEEB).AND.XHV(JEBR)
          XHVH  = XHV(JHEA).AND.XHV(JHEB).AND.XHV(JHBR)
          XHVTG = XHV(JTGA).AND.XHV(JTGC)
          XHVOK = XHVE.AND.XHVTR.AND.XHVH.AND.XHVTG
          XHVL  = XHV(JLCAL)
          XHVS  = XHV(JSCAL)
C get t0 synchronization information
          XT0   = XT0SYN(KRUN)
C combine to LLUMOK and SLUMOK flag
          LL    = XTGQOK .AND. XTGLOK .AND. XHVOK .AND. XHVL .AND. XT0
          SL    = XTGQOK .AND. XTGSOK .AND. XHVOK .AND. XHVS .AND. XT0
          LL1   = LL
          SL1   = SL
C Starting with 1993, don't require LCAL triggers enabled for XLUMOK
          IF (KRUN .GT. 20000 .and. KRUN.lt.40000)
     +      LL1 = XTGQOK .AND. XHVOK .AND. XHVL .AND. XT0
C For LEP 1.5 running, check if not hidden TPC laser events:
          IF (KRUN.GT.40000.AND.KRUN.LT.40550) THEN
             XL133= XRE133(KRUN,KEVT)
             IF (.NOT.XL133) LL1=.FALSE.
          ENDIF
C For some runs at the beginning of 1994, reject unwanted laser events:
          IF (KRUN.GE.25520.AND.KRUN.LE.25530) THEN
             XL94 =  XRE94(KRUN,KEVT)
             IF (.NOT.XL94) LL1=.FALSE.
          ENDIF
C Starting with 1995/LEP1.5, don't require SCAL triggers enabled for XLUMOK
          IF (KRUN .GT. 40000)
     +      SL1 = XTGQOK .AND. XHVOK .AND. XHVS .AND. XT0 
        END IF
C combine LCAL and SICAL information
        XL = LL1 .AND. (SL1 .OR. ISITP .EQ. 0)
C add Vdet information from 1998 onwards
        IF (KRUN .GT. 45000) XL = XL .AND. XVDEOK(DUM)
C save run and event number
        LRUN = KRUN
        LEVT = KEVT
      END IF
C set the output variables
      XLUM = XL
      SLUM = SL
      LLUM = LL
      END
#endif
