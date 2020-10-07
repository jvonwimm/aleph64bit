      SUBROUTINE TPDVEL ( OPTION, DVA, DVB, IER )
C----------------------------------------------------------------------
C!  Get Tpc Drift Velocity
CKEY TPC DRIFT / INTERNAL
C   Author   :- E. Lancon             20-FEB-1992
C   Modified :- I. Tomalin             3-AUG-1995
C               'RAW' option now also looks for V_d from laser.
C               'ONL' option made completely equivalent to 'RAW'.
C                Allow user to offset Vz using TVOF card.
C   Modified :- P. Comas               9-AUG-1995
C                For the option RAW/ONL, check TDFV from Daf
C                before TLAS to avoid problems with the data
C                before LEP1.5 when PASS0 is not run.
C   Modified :- D. Casper              6-MAR-1997
C                Look for bank TNFV to tune drift velocity for 1997-style
C                alignment.  Correction is v_new = v_old * (1 + delta),
C                only if TDFV bank is used for drift velocity.
C                A TOAL card diasbles the correction.
C   Modified :- M. Cattaneo            5-JUN-1998
C                Print TNFV/TDFV correction message once only
C
C   Inputs:
C        - OPTION      / CH  = type of input data
C                              'ONL' for PASS0
C                              'RAW' for RAW data
C                              'POT' for POT ot DST data
C   Outputs:
C        - DVA (3)     / R     Drift velocity side A (cm/musec)
C                              (Vx, Vy, Vz)
C
C        - DVB (3)     / R     Drift velocity side B (cm/musec)
C                              (Vx, Vy, Vz)
C
C        - IER         / I     Error Code
C                               0 : OK
C                               1 : no drift velocity found
C                               2 : wrong option
C
C   Called by: AUNPCK, JULIA TPC prepare data
C
C   Description
C   ===========
C        - TPC Drift Velocity values are returned according to OPTION.
C        - Routine must be called at each run initialisation with one
C          type of OPTION
C        - for each subsequent call to this routine within a given run
C          tpc drift velocity values loaded at run initialisation are
C          returned in that case option is ignored.
C        - in case of wrong option or missing drift velocity (IER>0)
C          the STATUS is wrong for the full run and no drift velocity
C          is return.
C
C        - 'RAW' or 'ONL' options (equivalent):
C            Look for TDPV  (V_d from PASS0)
C            If that doesn't exist then use TDFV from Daf
C            If that doesn't exist then use TLAS (V_d from laser)
C               and transverse drift velocity from TVXY Daf bank.
C            If that doesn't exist (very old data) use TDVV from Daf.
C
C        - 'POT' option :
C            Look for TDFV on Daf
C            If that doesn't exist then use JCON (V_d used by JULIA)
C            If that doesn't exist (very old data) use TDVV from Daf.
C
C======================================================================
#ifndef DOC
      SAVE
#include "bcs.h"
#include "tdvvjj.h"
#include "tdpvjj.h"
#include "tdfvjj.h"
#include "tsimjj.h"
#include "tlasjj.h"
#include "tvxyjj.h"
#include "tnfvjj.h"
#include "jconjj.h"
      REAL DVA (3), DVB (3)
      REAL DVAD (3), DVBD (3)
      CHARACTER*(*) OPTION
      INTEGER AGETDB, TDFVRU, TNFVRU
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA NATSIM /0/
#include "bmacro.h"
C======================================================================
C
C - 1st entry
      IF (NATSIM.EQ.0) THEN
        NATDVV = NAMIND('TDVV')
        NATSIM = NAMIND('TSIM')
        NAJCON = NAMIND('JCON')
        NATDPV = NAMIND('TDPV')
        NATDFV = NAMIND('TDFV')
        NATLAS = NAMIND('TLAS')
        NATVXY = NAMIND('TVXY')
        NATVOF = NAMIND('TVOF')
        NATOAL = NAMIND('TOAL')
        NATNLO = NAMIND('TNLO')
        NATSLO = NAMIND('TSLO')
        ILAST = 0
        ISTATUS = 1
      ENDIF
C
C    Get current run number
C
      CALL ABRUEV (IRUN, IEVT)
C
      IF (IRUN.NE.ILAST ) THEN
C
C --- new run -------------------------------------------------------
C
        JTDFV = 0
C
        IF ( IRUN.LE.2000 ) THEN
C
C ======= MC data  : use TSIM   (drift velocity is in cm/ns)  =======
C
          JTSIM = IW(NATSIM)
          IF (JTSIM.GT.0) THEN
            IF (ITABL(JTSIM,1,JTSITV) .LT. 206) THEN
              VZ = RW(JTSIM+5)
            ELSE
              VZ = RTABL(JTSIM,1,JTSIDV)
            ENDIF
            DVAD(1) = 0.
            DVAD(2) = 0.
            DVAD(3) = VZ * 1000.
            DVBD(1) = DVAD(1)
            DVBD(2) = DVAD(2)
            DVBD(3) = DVAD(3)
          ELSE
C No TSIM, try to get TDVV NR=1
             IRET = AGETDB('TDVV',-IRUN)
             JTDVV = IW(NATDVV)
             IF ( JTDVV.GT.0 ) THEN
                DVAD (1) = RTABL(JTDVV,3,JTDVVX)
                DVAD (2) = RTABL(JTDVV,3,JTDVVY)
                DVAD (3) = RTABL(JTDVV,3,JTDVVZ)
                DVBD (1) = RTABL(JTDVV,6,JTDVVX)
                DVBD (2) = RTABL(JTDVV,6,JTDVVY)
                DVBD (3) = RTABL(JTDVV,6,JTDVVZ)
             ELSE
C No TSIM, no TDVV NR=1 return IER=1
                ISTATUS = 1
                GOTO 999
             ENDIF
          ENDIF
        ELSE
C
C ======= Real data : check type of input file =====================
C
          IF ( OPTION(1:3).EQ.'POT' ) THEN
C
C ++++++++  input file is a POT or DST ++++++++++++++++++++++++++
C Look for TDFV on Daf
C If that doesn't exist then use JCON (V_d used by JULIA)
C If that doesn't exist (very old data) use TDVV from Daf.
C
C Get TDFV row # for this run from daf
            JTDFV = TDFVRU (IRUN, IROW)
            JTDFV = ABS(JTDFV)
            IF ( JTDFV.GT.0 .AND.IROW.GT.0 ) THEN
C Run exists in TDFV, take it
              DO I=1,3
                 DVAD (I) = RTABL (JTDFV,IROW,JTDFVA-1+I)
                 DVBD (I) = RTABL (JTDFV,IROW,JTDFVB-1+I)
              ENDDO
            ELSE
C Run does not exist in TDFV try JCON
              JJCON = IW(NAJCON)
              IF ( JJCON.GT.0 ) THEN
                DVAD (1) = RTABL (JJCON,1,JJCODX)
                DVAD (2) = RTABL (JJCON,1,JJCODY)
                DVAD (3) = RTABL (JJCON,1,JJCODZ)
                IF (LCOLS(JJCON).GT.JJCOGT) THEN
C New JCON format
                  DVBD (1) = RTABL (JJCON,1,JJCODB)
                  DVBD (2) = RTABL (JJCON,1,JJCODB+1)
                  DVBD (3) = RTABL (JJCON,1,JJCODB+2)
                ELSE
C Old JCON format
                  DVBD(1) = -DVAD(1)
                  DVBD(2) = -DVAD(2)
                  DVBD(3) =  DVAD(3)
                ENDIF
              ELSE
C No TDFV, no JCON try old TDVV from Daf.
C
                IRET = AGETDB('TDVV',-IRUN)
                JTDVV = IW(NATDVV)
                IF ( JTDVV.GT.0 ) THEN
                  DVAD (1) = RTABL(JTDVV,3,JTDVVX)
                  DVAD (2) = RTABL(JTDVV,3,JTDVVY)
                  DVAD (3) = RTABL(JTDVV,3,JTDVVZ)
                  DVBD (1) = RTABL(JTDVV,6,JTDVVX)
                  DVBD (2) = RTABL(JTDVV,6,JTDVVY)
                  DVBD (3) = RTABL(JTDVV,6,JTDVVZ)
                ELSE
C No TDFV, no JCON, no TDVV return IER=1
                  ISTATUS = 1
                  GOTO 999
                ENDIF
              ENDIF
            ENDIF
C
          ELSEIF ( OPTION(1:3).EQ.'RAW' .OR. OPTION(1:3).EQ.'ONL' ) THEN
C
C +++++++++ Read RAW data:  +++++++++++++++++++++++++++++++++++++++
C
C Look for TDPV  (V_d from PASS0)
C If that doesn't exist then use TDFV from Daf
C If that doesn't exist then use TLAS (V_d from laser).
C If that doesn't exist (very old data) use TDVV from Daf.
C
            JTDPV = IW(NATDPV)
            IF ( JTDPV.GT.0 ) THEN
C Bank from PASS0 exists take it
C            Bank from PASS0 exists take it
              DVAD (1) = RTABL (JTDPV,1,JTDPDV)
              DVAD (2) = RTABL (JTDPV,1,JTDPDV+1)
              DVAD (3) = RTABL (JTDPV,1,JTDPDV+2)
              IF (LROWS(JTDPV).EQ.2) THEN
                DVBD (1) = RTABL (JTDPV,2,JTDPDV)
                DVBD (2) = RTABL (JTDPV,2,JTDPDV+1)
                DVBD (3) = RTABL (JTDPV,2,JTDPDV+2)
              ELSE
                DVBD(1) = -DVAD(1)
                DVBD(2) = -DVAD(2)
                DVBD(3) =  DVAD(3)
              ENDIF
            ELSE
C
C No TDPV, search for TDFV on Daf.
              JTDFV = TDFVRU (IRUN, IROW)
              JTDFV = ABS(JTDFV)
              IF ( JTDFV.GT.0 .AND. IROW.GT.0) THEN
C              TDFV info. found on Daf
                DO I=1,3
                  DVAD (I) = RTABL (JTDFV,IROW,JTDFVA-1+I)
                  DVBD (I) = RTABL (JTDFV,IROW,JTDFVB-1+I)
                ENDDO
               GOTO 25
              ENDIF
C
C No TDPV, no TDFV, try TLAS,
C (and take transverse drift velocity from TVXY Daf bank)
              JTLAS = IW(NATLAS)
              IRET = AGETDB ('TVXY',-IRUN)
              JTVXY = IW(NATVXY)
              IF (JTLAS.GT.0.AND.JTVXY.GT.0) THEN
C
C Loop over rows of TLAS (either one for each TPC end or one global).
                VSUM = 0.0
                WSUM = 0.0
                DO IEND = 1,LROWS(JTLAS)
                  ITLAS = KROW(JTLAS,IEND)
                  IF (ABS(RW(ITLAS + JTLANV)).GT.1.0E-3) THEN
                    ERRVEL = MAX(RW(ITLAS + JTLANE),1.0E-6)
                    WW = 1.0/ERRVEL**2
                    VSUM = VSUM + RW(ITLAS + JTLANV)*WW
                    WSUM = WSUM + WW
                  END IF
                ENDDO

C If no drift velocity this run (too few laser events) then use last one.
                IF (ABS(VSUM).LE.1.0E-3) THEN
                  DO IEND = 1,LROWS(JTLAS)
                    ITLAS = KROW(JTLAS,IEND)
                    IF (ABS(RW(ITLAS + JTLALV)).GT.1.0E-3) THEN
                      ERRVEL = MAX(RW(ITLAS + JTLALE),1.0E-6)
                      WW = 1.0/ERRVEL**2
                      VSUM = VSUM + RW(ITLAS + JTLALV)*WW
                      WSUM = WSUM + WW
                    END IF
                  ENDDO
                END IF

C Check that last drift velocity was OK.
                IF (ABS(VSUM).LE.1.0E-3) GOTO 10
C
C Average V_d from each TPC end (or just take global V_d).
                DVAD(3) = VSUM/WSUM
                DVBD(3) = DVAD(3)
C
C Transverse V_d.
                DVAD(1) = RTABL(JTVXY,1,JTVXDX)
                DVAD(2) = RTABL(JTVXY,1,JTVXDY)
                IF (LROWS(JTVXY).EQ.2) THEN
                  DVBD(1) = RTABL(JTVXY,2,JTVXDX)
                  DVBD(2) = RTABL(JTVXY,2,JTVXDY)
                ELSE
                  DVBD(1) = -DVAD(1)
                  DVBD(2) = -DVAD(2)
                ENDIF
C
C Drift velocity from TLAS is OK, so used.
                GOTO 25
C
              END IF
C
   10         CONTINUE
C
C No TDPV, no TDFV, no TLAS, try TDVV from Daf.
                IRET = AGETDB('TDVV',-IRUN)
                JTDVV = IW(NATDVV)
                IF ( JTDVV.GT.0 ) THEN
                  DVAD (1) = RTABL(JTDVV,3,JTDVVX)
                  DVAD (2) = RTABL(JTDVV,3,JTDVVY)
                  DVAD (3) = RTABL(JTDVV,3,JTDVVZ)
                  DVBD (1) = RTABL(JTDVV,6,JTDVVX)
                  DVBD (2) = RTABL(JTDVV,6,JTDVVY)
                  DVBD (3) = RTABL(JTDVV,6,JTDVVZ)
                ELSE
C
C No TDPV, no TDFV, no TLAS, no TDVV, return IER=1
                  ISTATUS = 1
                  GOTO 999
                ENDIF

            ENDIF
C
          ELSE
C
C ++++++++ Unknown option ++++++++++++++++++++++++++++++++++++++
C
            ISTATUS = 2
            GOTO 999
          ENDIF
C +++++++++++++++ end of all options of real data +++++++++++++++++
        ENDIF
C
   25   CONTINUE
        ISTATUS = 0
C
C ================ end of all data types ===========================
      ENDIF
C ---------------- end of new run ----------------------------------
C
C -  same run as before or drift velocity has been found :
C    fill output arguments if the status is ok
      IF (ISTATUS.NE.0) GOTO 999
      DO I=1,3
         DVA(I) = DVAD(I)
         DVB(I) = DVBD(I)
      ENDDO
C
C+ Check for availability of TNFV bank
C
      IF (JTDFV.GT.0 .AND. IW(NATOAL).EQ.0) THEN
        JTNFV = TNFVRU(IRUN,IROW)
        JTNFV = ABS(JTNFV)
        IF (JTNFV.GT.0 .AND. IROW.GT.0) THEN
            DVCORR = RTABL(JTNFV,IROW,JTNFDV)
            DVA(3) = (1.+DVCORR)*DVA(3)
            DVB(3) = (1.+DVCORR)*DVB(3)
            IF (FIRST .AND. IW(6).GT.0) WRITE(IW(6),34)
  34        FORMAT(/,' TPDVEL:  Applying drift velocity correction ',
     &         'from TNFV to TDFV')
            IF (IW(NATSLO).GT.0) CALL ALTELL(
     &        'TPDVEL: WARNING - TSLO found; v_d may be inconsistent!',
     &        0,'RETURN')
        ELSE
            CALL BDROP(IW,'TNFV')
            IF (IW(NATNLO).GT.0) CALL ALTELL(
     &        'TPDVEL: WARNING - TNLO found without TNFV; v_d may '//
     &        'be inconsistent!',0,'RETURN')
        ENDIF
      ELSE
        CALL BDROP(IW,'TNFV')
      ENDIF

C Offset V_drift if required.
      KTVOF = IW(NATVOF)
      IF (KTVOF.GT.0) THEN
        VOFF = RW(KTVOF + 1)
        DVA(3) = DVA(3) + VOFF
        DVB(3) = DVB(3) + VOFF
        IF (FIRST .AND. IW(6).GT.0) THEN
          WRITE(IW(6),35) VOFF
   35     FORMAT(/,' ***** TPC drift velocity offset by ',F9.5,
     +    ' cm/us using TVOF card *****')
        END IF
      END IF
      FIRST = .FALSE.
C
  999 IER = ISTATUS
      ILAST = IRUN
      END
#endif
