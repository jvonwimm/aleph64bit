      SUBROUTINE SRCONS
C----------------------------------------------------------------------
C! Fill SATR CONStants common block
C!
C!    Author:     H. Meinhard       19-Aug-1987
C!    Modified:   H. Meinhard       07-Mar-1991  (6)
C!    Modified:   H. Meinhard       20-Jun-1991  (7)
C!                      Get SPED bank from data base, if necessary
C!
C!    Description
C!    ===========
C!    Constants describing the SATR are filled into common SCALBC.
C!    The constants are read from bank SCLB. If a data card SCAU is
C!    found, try to find the time histogram STHI and calculate a new
C!    x vs t from that histogram, otherwise take it from the SCLB bank.
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rcurnt.h"
#include "rlunit.h"
#include "scalbc.h"
#include "sclbjj.h"
#include "sthijj.h"
      INTEGER ALGTDB,MAXIZE,MINIZE
      EXTERNAL ALGTDB,MAXIZE,MINIZE
C
C parameters for the calculation of x versus t from the histogram, will
C after test period go to a data base bank
      PARAMETER (MCMAX=100,MGMIN=5000,
     +  WIDTH=20.E-9,TMNUS=30.E-9,TPLUS=30.E-9)
      PARAMETER (DRMIN=0.,DRMAX=.47)
      PARAMETER (R1MI=.15,R1MA=.65,R1REF=.50,
     +           R2MI=.15,R2MA=.85,R2REF=.50)
      INTEGER ISORT(MCMAX)
      REAL    ABSCI(MCMAX),THIST(MCMAX),TSUM(MCMAX)
C MCMAX : maximum number of histogram channels
C MGMIN : minimum number of good entries for calc of x vers t from hist
C WIDTH : width of time window being used to determine the max contents
C TMNUS : time in x vs t before T0
C TPLUS : time in x vs t after T47
C DRMIN : minimal drift distance
C DRMAX : maximal drift distance
C RnMI  : percentage of channel contents where straight line fit starts
C RnMA  : percentage of channel contents where straight line fit stops
C RnREF : percentage of channel contents where abszissa of straight line
C         fit will be calculated
C ISORT : field for indices of sorted histogram channels
C ABSCI : field for abscissas of channel centers
C THIST : field for real histogram contents
C TSUM  : field for histogram contents summed up in reverse order
#include "bosext.h"
#include "bmacro.h"
C----------------------------------------------------------------------
      IER = 0
C
C get SCLB bank and SWTU bank
      ISTAT = ALGTDB(LRCONS,'SCLB',IRUNRC)
      IF (ISTAT .EQ. 0)                                     GOTO 901
      ISTAT = ALGTDB(LRCONS,'SWTU',IRUNRC)
C
C get SPED bank from data base, if necessary
      IF (IRUNRC .GE. 11494 .AND. IRUNRC .LE. 11592) THEN
        ISTAT = ALGTDB(LRCONS,'SPED',IRUNRC)
        IF (ISTAT .EQ. 0)                                   GOTO 901
      ENDIF
C
C link to SCLB bank
      KSCLB = IW(NAMIND('SCLB'))
      IF (KSCLB .EQ. 0)                                     GOTO 901
C
C copy channel on/off bit pattern from SCLB bank to SCALBC common
      DO 300 I = 1, MJWSC
  300 JWIRSC(I) = ITABL(KSCLB,1,JSCLCB+I-1)
C
C if there are data cards with kaputt channel information, fed the infor
C mation into the JWIRSC field
      CALL SRDEAD
C
C copy TDC time corrections from SCLB bank to SCALBC common
      DO 301 I = 1, MCRSC
  301 TCRASC(I) = RTABL(KSCLB,1,JSCLCR+I-1)
      TCHASC = RTABL(KSCLB,1,JSCLCH)
C
C Do we want to try the automatic calibration?
C *** Eventually the automatic calibration should become the default ***
      JSCAU = IW(NAMIND('SCAU'))
      IF (JSCAU .EQ. 0)                                     GOTO 340
C
C link to TDC time histogram
      KSTHI = IW(NAMIND('STHI'))
C
C if histogram doesn't exist, take x versus t from SCLB bank
      IF (KSTHI .EQ. 0)                                     GOTO 340
C
C get histogram data and fill ABSCI, THIST, and TSUM vector
      NCHAN = ITABL(KSTHI,1,JSTHNC)
      ISTAR = ITABL(KSTHI,1,JSTHAS)
      ISTEP = ITABL(KSTHI,1,JSTHAD)
      ASTAR = REAL(ISTAR) * 1.E-12
      ASTEP = REAL(ISTEP) * 1.E-12
      NENTR = ITABL(KSTHI,1,JSTHNE)
      NUNDR = ITABL(KSTHI,1,JSTHNU)
      NOVER = ITABL(KSTHI,1,JSTHNO)
      ABSCI(NCHAN) = REAL(ISTAR + (2*NCHAN-1)*ISTEP/2) * 1.E-12
      THIST(NCHAN) = REAL(ITABL(KSTHI,1,JSTHCC+NCHAN-1))
      TSUM(NCHAN)  = THIST(NCHAN)
      DO 310 I = NCHAN-1, 1, -1
        ABSCI(I) = REAL(ISTAR + (2*I-1)*ISTEP/2) * 1.E-12
        THIST(I) = REAL(ITABL(KSTHI,1,JSTHCC+I-1))
        TSUM(I)  = TSUM(I+1) + THIST(I)
  310 CONTINUE
C
C if there aren't enough good histogram entries, take x versus t from
C SCLB bank
      NGOOD = NENTR - NUNDR - NOVER
      IF (NGOOD .LT. MGMIN)                                 GOTO 340
C
C we use N channels to determine the 100 % level of the histogram
      N = INT(WIDTH/ASTEP + .5)
      IF (N .LE. 0)                                         GOTO 340
C
C find the n channels with the biggest contents
      CALL SORTZV(IW(KSTHI+LMHLEN+JSTHCC),ISORT,NCHAN,-1,-1,0)
C
C check that the n biggest channels are well inside the histo borders;
C if not, take x versus t from SCLB bank
C calculate mean value of N biggest channels
C find leftmost and rightmost channel used to determine the 100 % level
      R100 = 0.
      DO 320 I = 1, N
        IF (ISORT(I) .EQ. 1 .OR. ISORT(I) .EQ. NCHAN)       GOTO 340
        R100 = R100 + REAL(ITABL(KSTHI,1,JSTHCC-1+ISORT(I)))
  320 CONTINUE
      R100 = R100 / REAL(N)
      ILEFT = ISORT(MINIZE(ISORT,N))
      IRGHT = ISORT(MAXIZE(ISORT,N))
      IF (ILEFT .GE. IRGHT)                                 GOTO 340
C
C fit to raising edge of histogram in order to find time corres-
C ponding to maximal drift distance
      CALL SRFHIS(ABSCI,THIST,ILEFT,R100*R1MI,R100*R1MA,R100*R1REF,
     +  0,T47,IER)
      IF (IER .NE. 0)                                       GOTO 340
C
C fit to falling edge of histogram in order to find time corres-
C ponding to minimal drift distance
      CALL SRFHIS(ABSCI(IRGHT),THIST(IRGHT),NCHAN-IRGHT+1,
     +  R100*R2MI,R100*R2MA,R100*R2REF,1,T0,IER)
      IF (IER .NE. 0)                                       GOTO 340
C
C check times
      IF (T0 .LE. T47)                                      GOTO 340
C
C get contents of integrated histogram for t0 and t47
      CALL SRIHIS(ASTAR,ASTEP,NCHAN,TSUM,T0,C0,IER)
      IF (IER .NE. 0)                                       GOTO 340
      CALL SRIHIS(ASTAR,ASTEP,NCHAN,TSUM,T47,C47,IER)
      IF (IER .NE. 0)                                       GOTO 340
C
C get number of points of x vs t from database bank
      MPOISC = ITABL(KSCLB,1,JSCLNP)
C
C calculate start and step values for t in x vs t
      TOFFSC = T0 + TMNUS
      TSTASC = 0.
      TDELSC = (TOFFSC - T47 + TPLUS) / REAL(MPOISC-1)
C
C calculate new x versus t
      DO 330 I = 1, MPOISC
        TIME = TOFFSC - TSTASC - REAL(I-1)*TDELSC
        IF (TIME .GT. T0) THEN
          TSPASC(I) = DRMIN
        ELSE IF (TIME .LT. T47) THEN
          TSPASC(I) = DRMAX
        ELSE
          CALL SRIHIS(ASTAR,ASTEP,NCHAN,TSUM,TIME,C,IER)
          IF (IER .NE. 0)                                   GOTO 340
          TSPASC(I) = (DRMAX-DRMIN) * (C-C0) / (C47-C0)
        ENDIF
  330 CONTINUE
      GOTO 360
C
C----------------------------------------------------------------------
C For any reason, it was not possible to derive a new x vs t from the
C histogram, that's why we copy the old x vs t
C----------------------------------------------------------------------
  340 CONTINUE
      MPOISC = ITABL(KSCLB,1,JSCLNP)
      TOFFSC = RTABL(KSCLB,1,JSCLTO)
      TSTASC = RTABL(KSCLB,1,JSCLTB)
      TDELSC = RTABL(KSCLB,1,JSCLTD)
      DO 350 I = 1, MPOISC
  350 TSPASC(I) = RTABL(KSCLB,1,JSCLTS+I-1)
      GOTO 360
C
C----------------------------------------------------------------------
C Now, we have a valid x vs t, create bank that can be written to data
C base
C----------------------------------------------------------------------
  360 CONTINUE
      MSCLB = IW(KSCLB-2)
      IF (MSCLB .NE. IRUNRC) THEN
        CALL AUBOS('SCLB',IRUNRC,LMHLEN+LSCLBA,JSCLB,IGARB)
        IF (IGARB .EQ. 2)                                   GOTO 902
        IW(JSCLB+LMHCOL) = LSCLBA
        IW(JSCLB+LMHROW) = 1
      ELSE
        JSCLB = KSCLB
      ENDIF
C
      IOFF = KROW(JSCLB,1)
      IW(IOFF+JSCLID) = 1
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C preliminary definition of the validity range
      IW(IOFF+JSCLVR) = 1
      IW(IOFF+JSCLVR+1) = 999999
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
      IW(IOFF+JSCLNW) = MJWSC
      DO 370 I = 1, MJWSC
  370 IW(IOFF+JSCLCB+I-1) = JWIRSC(I)
      RW(IOFF+JSCLTO) = TOFFSC
      IW(IOFF+JSCLNP) = MPOISC
      RW(IOFF+JSCLTB) = TSTASC
      RW(IOFF+JSCLTD) = TDELSC
      DO 380 I = 1, MPOISC
  380 RW(IOFF+JSCLTS+I-1) = TSPASC(I)
C
      GOTO 999
C----------------------------------------------------------------------
  901 IER = 1
      CALL RERROR('SRCONS',-IER,'No SCLB or SPED bank from database')
      GOTO 999
  902 IER = 2
      CALL RERROR('SRCONS',IER,'No space for new bank SCLB')
      GOTO 999
  999 CONTINUE
      RETURN
      END
#endif
