      SUBROUTINE OBSPOS( XY, XYERR, IRET )
C----------------------------------------------------------------------
C!  - Correct the BOM reconstruction for the systematic drift in Y
C!    beam position.  Only the Y position is corrected.
C!
C!    *** A temporary feature:
C!    For now, the BOM beam positions are reconstructed before the
C!    correction is applied to ensure that the reconstruction is done
C!    with the new code and the new beam optics.
C!
C!   Author   :- Hwi Y. Kim             2-APR-1993
C!
C!   Outputs:
C!   XY(i)    : Corrected BOM x,y positions(i=1,2) in cm.
C!              At the moment, x is not corrected yet.
C!   XYERR(i) : The error on the corrected x,y positions(i=1,2) in cm.
C!              CAUTION!  These errors are not independent event to
C!              event.  They are completely correlated within a fill,
C!              but are independent fill to fill.
C!   IRET  : This status word is set to zero if everything is OK.
C!           Otherwise, the reasons for failures are,
C!           1 - 27 = Bad BOM data.  Returned from XBOMOK.
C!                    See XBOMOK header for detail.
C!               31 = Correction parameter banks missing.
C!               32 = Correction not available for the fill.
C!               33 = Correction not available for the event.
C!               34 = The event's DATE and TIME is unreasonable.
C!
C?
C!======================================================================
      INCLUDE '/aleph/phy/qcde.inc'
      INCLUDE '/aleph/phy/qhac.inc'
C
      REAL XY(2), XYERR(2)
      INTEGER IRET
C
C  TCUT:  Cut value on the extent of a fill in days.  Allows throwing
C         away events with strange time values.
C
      REAL TCUT
      DATA TCUT/ 2.0 /
C
C  BOMCAL COMMON
C
      REAL DSCALE, CSCALE, PCALMM, XIPOFF, GAINA0, GAINA1, GAINA2,
     &     CURCOR, CALCOR, DEFCOR
      INTEGER IADCOR, IXYCOR
      COMMON /BOMCAL/ DSCALE, CSCALE, PCALMM, XIPOFF(4),
     &                GAINA0(4,2), GAINA1(4,2), GAINA2(4,2),
     &                CURCOR(29), CALCOR(4,2), DEFCOR(4,2),
     &                IADCOR(4,2), IXYCOR(4)
C
C  BOMOK Common
C
      INTEGER BOMERR
      COMMON/BOMOK/ BOMERR
C
C  Bank offsets for BOMV and BOMW banks.
C
      PARAMETER (JBMVFI=1,JBMVDA=2,JBMVOF=3,JBMVDR=4,JBMVLA=5,
     &           JBMVOV=6,JBMVDV=7,JBMVCO=8,LBMVAA=8)
      PARAMETER (JBMWR1=1,JBMWE1=2,JBMWR2=3,JBMWE2=4,JBMWLA=4)
C
C  Local variables
C
      REAL xbom, ybom, day, dtime, dummy,
     &     day0, offset, drift, lambda, ofersq, drersq, covar
      INTEGER IFIL, NFIL, IBLOCK, iyear, prun
      LOGICAL FIRST, XBOMOK, XLOW, XHIGH
      DATA FIRST/.TRUE./
C
      INCLUDE '/aleph/phy/qmacro.inc'
C
C  Statement functions to check acceptable regions of KRUN, KEVT
C  for correction.
C
      XLOW(IND)  = ( KRUN.LT.ITABL(KBOMW,IND,JBMWR1) .OR.
     &               (KRUN.EQ.ITABL(KBOMW,IND,JBMWR1).AND.
     &                 KEVT.LT.ITABL(KBOMW,IND,JBMWE1)) )
      XHIGH(IND) = ( KRUN.GT.ITABL(KBOMW,IND,JBMWR2) .OR.
     &               (KRUN.EQ.ITABL(KBOMW,IND,JBMWR2).AND.
     &                KEVT.GT.ITABL(KBOMW,IND,JBMWE2)) )
C.......................................................................
C
C  Do once:  Initialize BOM code, and correction table indices.
C
      IF (FIRST) THEN
         CALL OINIJO
c
         prun = 0
         IFIL = 0
         NFIL = -999
         IBLOCK = 1
c
         FIRST = .FALSE.
      ENDIF
C
C  If the run changes read in the constants database and calculate
C  the optics matrices.
C
      IF (KRUN.NE.prun) THEN
         CALL OINIRU
         prun = KRUN
      ENDIF
C
C  See if the BOM correction banks BOMV and BOMW exist.
C
      KBOMV = IW( NAMIND('BOMV') )
      KBOMW = IW( NAMIND('BOMW') )
      IF ( KBOMV.EQ.0 .OR. KBOMW.EQ.0 ) THEN
         IRET = 31
         GOTO 999
      ENDIF
      MAXFIL  = LROWS(KBOMV)
      MAXBLCK = LROWS(KBOMW)
C
C  Check to see if BOM data is OK.
C
      IF ( .NOT.XBOMOK(dummy) ) THEN
         IRET = BOMERR
         GOTO 999
      ENDIF
C
C  If the fill number changed since the previous call, search the
C  BOMV bank for the new fill number.
C
      IF (KRINLF.GT.NFIL) THEN
         DO if = IFIL+1, MAXFIL
            IFIL = if
            NFIL = ITABL(KBOMV,IFIL,JBMVFI)
            IF (KRINLF.EQ.NFIL) THEN
               GOTO 100
            ELSEIF (KRINLF.LT.NFIL) THEN
               IRET = 32
               GOTO 999
            ENDIF
         ENDDO
         IRET = 32
         GOTO 999
      ELSEIF (KRINLF.LT.NFIL) THEN
         DO if = IFIL-1, 1, -1
            IFIL = if
            NFIL = ITABL(KBOMV,IFIL,JBMVFI)
            IF (KRINLF.EQ.NFIL) THEN
               GOTO 100
            ELSEIF (KRINLF.GT.NFIL) THEN
               IRET = 32
               GOTO 999
            ENDIF
         ENDDO
         IRET = 32
         GOTO 999
      ENDIF
C
C  The correction parameters for this fill exists.  Now, check whether
C  the correction for this event was acceptable.
C
  100 CONTINUE
      IF (XHIGH(IBLOCK)) THEN
         DO ib = IBLOCK+1, MAXBLCK
            IBLOCK = ib
            IF (.NOT.XHIGH(IBLOCK)) THEN
               IF (.NOT.XLOW(IBLOCK)) GOTO 200
               IRET = 33
               GOTO 999
            ENDIF
         ENDDO
      ELSEIF (XLOW(IBLOCK)) THEN
         DO ib = IBLOCK-1, 1, -1
            IBLOCK = ib
            IF (.NOT.XLOW(IBLOCK)) THEN
               IF (.NOT.XHIGH(IBLOCK)) GOTO 200
               IRET = 33
               GOTO 999
            ENDIF
         ENDDO
      ENDIF
C
C  The event passes all the checks.
C
  200 CONTINUE
C
C  Reconstruct BOM data.
C
      CALL OMBRE2
C
C  Get the reconstructed position and convert the unit from (mm) to
C  (cm).
C
      KBOMB = IW( NAMIND('BOMB') )
      xbom  = RTABL(KBOMB,1,JBOMXX) * .1
      ybom  = ( RTABL(KBOMB,1,JBOMYY) + XIPOFF(3) ) * .1
C
C  Calculate the days in the year.
C

      CALL GETDAYS(KEVEDA,KEVETI,iyear,day)
C
C  Get the parameters from BOMV bank for this fill.  All the parameters
C  that has length unit, are saved in microns, except the COVARIANCE
C  which has micron**2 unit.  The constants multiplying the parameters
C  below reflect the conversion of these units to cm, and cm**2.
C
      day0   = RTABL(KBOMV,IFIL,JBMVDA)
      offset = RTABL(KBOMV,IFIL,JBMVOF) * 1.E-4
      drift  = RTABL(KBOMV,IFIL,JBMVDR) * 1.E-4
      lambda = RTABL(KBOMV,IFIL,JBMVLA)
      ofersq = RTABL(KBOMV,IFIL,JBMVOV) **2 * 1.E-8
      drersq = RTABL(KBOMV,IFIL,JBMVDV) **2 * 1.E-8
      covar  = RTABL(KBOMV,IFIL,JBMVCO) * 1.E-8
C
C  Calculate the corrected beam position, and the error on the
C  correction.
C
      dtime = day - day0
      IF ( ABS(dtime).GT.TCUT ) THEN
         IRET = 34
         GOTO 999
      ENDIF
C
      drcor = EXP( - lambda * dtime )
      XY(1) = xbom
      XY(2) = ybom + offset + drift * drcor
      XYERR(1) = 0.
      XYERR(2) = SQRT( ofersq + drcor**2*drersq + 2.*drcor*covar )
C
C  Return SUCCESS.
C
      IRET = 0
C
  999 CONTINUE
      RETURN
      END
C
      LOGICAL FUNCTION XBOMOK(DUMMY)
C----------------------------------------------------------------------
C!  - Determine status of BOM for each event.
C!
C!   Author   :- Hwi Y. Kim             6-MAR-1993
C!
C!   Outputs: Returns true if BOM data quality is OK.
C!            If not OK, error code is returned in BOMERR word in the
C!            BOMOK Common.  If anyone care to know why the BOM data
C!            was not good, this word can be used.
C!
C!   BOMERR  Comments
C!   -----   --------
C!     0    Everything OK.
C!     1    No BOMR bank.
C!     3    Bad pedestal value.
C!     5    Signal saturation for e-.
C!     7    Signal below pedestal for e-.
C!     9    Signal saturation for e+.
C!    11    Signal below pedestal for e+.
C!    13    No BOMP bank.
C!    15    Bad beam bunch number.
C!    17    No BOME bank.
C!    19    Likely bad Corrector magnet current values.
C!    21    No RUNR bank(used to get the run number for LEP energy).
C!    23    Bad LEP beam energy.
C!    25    Bad BOM current ratio, side B / side A.
C!    27    No BOMB bank.
C!
C?
C!======================================================================
      INCLUDE '/aleph/phy/qcde.inc'
      INCLUDE '/aleph/phy/qhac.inc'
C
C  BOMOK Common
C
      INTEGER BOMERR
      COMMON/BOMOK/ BOMERR
C
C  parameters
C
C  MINDAT,MAXDAT: Minimum and maxmum of the BOM ADC counts multiplied
C                 by 100, as is stored in the rawdata bank BOMR.
C  CRATL(i),CRATH(i): Low and high bounds of the BOM current ratios,
C                     side B / side A, within which the data is
C                     acceptable.  (i=1,2 for e+,e-).
C
      INTEGER MINDAT, MAXDAT
      PARAMETER (MINDAT=0, MAXDAT=409500 )
C
      REAL CRATL(2), CRATH(2)
      DATA CRATL /  0.95 , 0.95  /
      DATA CRATH /  1.00 , 1.00  /
C
C  local variables
C
      INTEGER IPEDS(8), ICUR
C
      INCLUDE '/aleph/phy/qmacro.inc'
C.......................................................................
C
      XBOMOK = .FALSE.
      BOMERR = 0
C
C See if the rawdata exists.
C
      KBOMR = IW( NAMIND('BOMR') )
      IF (KBOMR.EQ.0) THEN
         BOMERR = 1
         GOTO 999
      ENDIF
C
C Check the pedestal.
C
      DO I = 1, 8
         IPEDS(I) = ITABL(KBOMR,I,JBOMPE)
         IF (IPEDS(I).LT.1) THEN
            BOMERR = 3
            GOTO 999
         ENDIF
      ENDDO
C
C Check for saturation and compare with the pedestal.
C
      DO I = 1, 8
         IDAT = ITABL(KBOMR,I,JBOMMN)
         IF (IDAT.GE.MAXDAT) THEN
            BOMERR = 5
            GOTO 999
         ENDIF
         IF (IDAT.LE.IPEDS(I)) THEN
            BOMERR = 7
            GOTO 999
         ENDIF
      ENDDO
C
      DO I = 9, 16
         IDAT = ITABL(KBOMR,I,JBOMMN)
         IF (IDAT.LE.MINDAT) THEN
            BOMERR = 9
            GOTO 999
         ENDIF
         IF (IDAT.GE.IPEDS(I)) THEN
            BOMERR = 11
            GOTO 999
         ENDIF
      ENDDO
C
C Check the Beam Bunch number.
C
      KBOMP = IW( NAMIND('BOMP') )
      IF (KBOMP.EQ.0) THEN
         BOMERR = 13
         GOTO 999
      ENDIF
C
      DO I = 1, 2
         IBUN = ITABL(KBOMP,I,JBOMBI)
         IF ( IBUN.LT.1 .OR. IBUN.GT.4 ) THEN
            BOMERR = 15
            GOTO 999
         ENDIF
      ENDDO
C
C Check the Corrector Magnet currents.
C
C
      KBOME = IW( NAMIND('BOME') )
      IF (KBOME.EQ.0) THEN
         BOMERR = 17
         GOTO 999
      ENDIF
C
      ICUR = 0
      DO IAB = 1, 2
         DO IC = 1, 4
            ICUR = ICUR + INT( 1.E4 * RTABL(KBOME,IAB,JBOMR1+IC-1) )
         ENDDO
      ENDDO
C
      IF (ICUR.EQ.0) THEN
         BOMERR = 19
         GOTO 999
      ENDIF
C
C Check the run number and the beam energy.
C
      KRUNR = IW( NAMIND('RUNR') )
      IF (KRUNR.EQ.0) THEN
         BOMERR = 21
         GOTO 999
      ENDIF
C
      IRUN = IW(KRUNR+2)
      ELEP = ALELEP(IRUN) / 2
      IF (ELEP.LT.40. .OR. ELEP.GT.50.) THEN
         BOMERR = 23
         GOTO 999
      ENDIF
C
C Check the BOM current ratios of sides B/A.
C
      DO I = 1, 2
         RATIO = RTABL(KBOME,I,JBOMIB) / RTABL(KBOME,I,JBOMIA)
         IF ( RATIO.LT.CRATL(I) .OR. RATIO.GT.CRATH(I) ) THEN
            BOMERR = 25
            GOTO 999
         ENDIF
      ENDDO
C
C Lastly, See if BOMB bank exists.
C
      KBOMB = IW( NAMIND('BOMB') )
      IF (KBOMB.EQ.0) THEN
         BOMERR = 27
         GOTO 999
      ENDIF
C
C Passed all the criteria for good BOM data.
C
      XBOMOK = .TRUE.
C
  999 CONTINUE
      RETURN
      END
C
      SUBROUTINE GETDAYS( IDATE, ITIME, IYEAR, DAYS )
C-----------------------------------------------------------------------
C
C  GETDAYS:  Calculate the day in the year given the date and time.
C
C  ARGUMENTS:
C  ---------
C
C  IDATE (I):  Date in the integer format (YYMMDD).
C  ITIME (I):  Time in the integer format (hhmmssss).  The seconds are
C              given in ss.ss seconds.
C  IYEAR (O):  The Year in the integer format (YY).
C  DAYS  (O):  The day in the year in decimal format.  For example, DAYS
C              for 23:30:30 Hour on Feb. 1 will be 32.9795.
C
C  If the code fails to calculate the day, DAYS is set to -1.
C
C  Author: Hwi Kim
C
C  28 Nov 1992;  Program creation date.
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IDATE, ITIME, IYEAR
      REAL DAYS
c
      INTEGER ihour, imin, isec, ibnrep(8), ierr, INDEX
      PARAMETER (INDEX=101)
      REAL time
      CHARACTER*119 chrep
c
c..... end of cde ......................................................
c
c-----------------------------------------------------------------------
c  Caculate the time in hours.
c-----------------------------------------------------------------------
c
      ihour = ITIME / 1E6
      imin  = ITIME / 1E4 - ihour * 1E2
      isec  = ITIME / 1E2 - ihour * 1E4 - imin *1E2
      time  = Float(ihour) + Float(imin) / 60. + Float(isec) / 3600.
c
c-----------------------------------------------------------------------
c  Caculate the day in the year.
c-----------------------------------------------------------------------
c
      IYEAR     = IDATE / 1E4
      ibnrep(3) = IYEAR
      ibnrep(2) = IDATE / 1E2 - ibnrep(3) * 1E2
      ibnrep(1) = IDATE       - ibnrep(3) * 1E4 - ibnrep(2) * 1E2
      Call CALDAT( INDEX, chrep, ibnrep, ierr )
c
c-----------------------------------------------------------------------
c  Combine the day and time to get days in decimal format.
c-----------------------------------------------------------------------
c
      If ( IERR.EQ.0 ) then
         DAYS = Float( ibnrep(4) ) + time / 24.
      else
         DAYS = -1.
      endif
c
      Return
      End
      SUBROUTINE OMBRE2
C----------------------------------------------------------------------
C!  - Calculate beam parameters using BOM data
C!
C!   Author:  R.W.Forty  23-May-91
C?
C!======================================================================
C*IF .NOT.DOC
C*CA BOMFLG
      LOGICAL XINIFL, XCURFL
      INTEGER IMCURR
      COMMON /BOMFLG/ XINIFL, XCURFL, IMCURR
C*CC BOMFLG
C*CA BOMCAL
      REAL DSCALE, CSCALE, PCALMM, XIPOFF, GAINA0, GAINA1, GAINA2,
     &     CURCOR, CALCOR, DEFCOR
      INTEGER IADCOR, IXYCOR
      COMMON /BOMCAL/ DSCALE, CSCALE, PCALMM, XIPOFF(4),
     &                GAINA0(4,2), GAINA1(4,2), GAINA2(4,2),
     &                CURCOR(29), CALCOR(4,2), DEFCOR(4,2),
     &                IADCOR(4,2), IXYCOR(4)
C*CC BOMCAL
C
      REAL DAT(8), SIG(8), PEDS(8), CALI(8),GAIN(4),XYBOM(4),XY(4,2),
     &     XIP(4), XIPB(4,2), BCUR(2), BCURB(2,2), GRAT(4), GRATB(4,2)
      INTEGER I, IDPA, IDBU, IRET, IER, IBUNB(2)
C
      DATA XIPB, BCURB, GRATB /8*0., 4*0., 8*0./
      DATA IBUNB /2*0/
C.......................................................................
C
      IF (.NOT. XINIFL) CALL OINIRU
      IF (.NOT. XINIFL) THEN
        IER = -1
        GOTO 500
      ENDIF
C
      CALL OMCOR2
C
      IF (.NOT. XCURFL) THEN
        IER = -2
        GOTO 500
      ENDIF
C
C  Calculate corrector deflections
C
C***  CALL OMCORR
C
C  Loop over e+ and e- beams
C
      DO 400 IDPA = 1, 2
C
C  Read data from banks
C
        CALL OMREAD (IDPA, IDBU, DAT, SIG, PEDS, CALI, IRET)
        IF (IRET .NE. 0) THEN
          IER = -3
          GOTO 500
        ENDIF
C
C  Use beam calibration
C
        CALL OMGAIN (IDPA, DAT, PEDS, CALI, GAIN, GRAT, IRET)
        IF (IRET .NE. 0) THEN
          IER = -4
          GOTO 500
        ENDIF
C
C  Calculate beam position at BOMs
C
        CALL OMBPOS (DAT, PEDS, GAIN, XYBOM, BCUR, IRET)
        IF (IRET .NE. 0) THEN
          IER = -5
          GOTO 500
        ENDIF
        CALL UCOPY (XYBOM(1), XY(1,IDPA), 4)
C
C  Calculate beam position and angle at IP
C
        CALL OMIPOS (IDPA, XYBOM, XIP, IRET)
        IF (IRET .NE. 0) THEN
          IER = -6
          GOTO 500
        ENDIF
C
        IBUNB(IDPA) = IDBU
C
        DO 200 I = 1, 4
          XIPB (I, IDPA) =  XIP(I) - XIPOFF(I)
          GRATB(I, IDPA) = GRAT(I)
  200   CONTINUE
C
        DO 300 I = 1, 2
          BCURB(I, IDPA) = BCUR(I)
  300   CONTINUE
C
  400 CONTINUE
C
      IER = 0
C
  500 CALL OMBOUT (XY, XIPB, IBUNB, BCURB, DEFCOR, IER)
C
      RETURN
      END
      SUBROUTINE OMCOR2
C----------------------------------------------------------------------
C!  - Read BOM corrector magnet deflections from BOME bank
C!
C!   Author:  R.W.Forty  19-Nov-91
C?
C!======================================================================
C*CA BCS
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C*CC BCS
C*CA BOMFLG
      LOGICAL XINIFL, XCURFL
      INTEGER IMCURR
      COMMON /BOMFLG/ XINIFL, XCURFL, IMCURR
C*CC BOMFLG
C*CA BOMCAL
      REAL DSCALE, CSCALE, PCALMM, XIPOFF, GAINA0, GAINA1, GAINA2,
     &     CURCOR, CALCOR, DEFCOR
      INTEGER IADCOR, IXYCOR
      COMMON /BOMCAL/ DSCALE, CSCALE, PCALMM, XIPOFF(4),
     &                GAINA0(4,2), GAINA1(4,2), GAINA2(4,2),
     &                CURCOR(29), CALCOR(4,2), DEFCOR(4,2),
     &                IADCOR(4,2), IXYCOR(4)
C*CC BOMCAL
C*CA BOMEJJ
      PARAMETER(JBOMXI=1,JBOMXP=2,JBOMYI=3,JBOMYP=4,JBOMBU=5,JBOMIA=6,
     +          JBOMIB=7,JBOMR1=8,JBOMR2=9,JBOMR3=10,JBOMR4=11,
     +          JBOMER=12,LBOMEA=12)
C*CC BOMEJJ
C
C*CA BMACRO
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C*CC BMACRO
C.......................................................................
C
      XCURFL = .FALSE.
      ICHECK = 0

      KBOME = IW( NAMIND('BOME') )
      IF (KBOME .EQ. 0) GOTO 999
C
      DO I = 1, 2
        DEFCOR(1,I) = RTABL (KBOME, I, JBOMR1)
        DEFCOR(2,I) = RTABL (KBOME, I, JBOMR2)
        DEFCOR(3,I) = RTABL (KBOME, I, JBOMR3)
        DEFCOR(4,I) = RTABL (KBOME, I, JBOMR4)
      ENDDO
C
      IMCURR = ITABL (KBOME, 2, JBOMER)
C
      DO I = 1, 2
        DO J = 1, 4
          ICHECK = ICHECK + INT (10000. * DEFCOR(J,I))
        ENDDO
      ENDDO
C
      IF (ICHECK .EQ. IMCURR .AND. ICHECK .NE. 0) XCURFL = .TRUE.
C
  999 RETURN
      END
      SUBROUTINE OMDATA
C----------------------------------------------------------------------
C!  - Fill COMMON from data base for BOM analysis
C!
C!   Author:  R.W.Forty  23-May-91
C!   Modified: Hwi Y. Kim  19-Apr-93
C!             Get the data base using KRUN instead of the RUNR bank.
C?
C!======================================================================
      INCLUDE '/aleph/phy/qcde.inc'
C
C*CA BOMFLG
      LOGICAL XINIFL, XCURFL
      INTEGER IMCURR
      COMMON /BOMFLG/ XINIFL, XCURFL, IMCURR
C*CC BOMFLG
C*CA BOMOPT
      INTEGER NUMCOR
      PARAMETER (NUMCOR = 4)
      REAL RIBOPT, RBIOPT, PIBOPT, RCBOPT, RCIOPT, AMCIOP, ZMOPTI
      COMMON /BOMOPT/ RIBOPT(4,4,2), RBIOPT(4,4,2), PIBOPT(4,4,2),
     &                RCBOPT(4,4,2,NUMCOR), RCIOPT(4,4,2,NUMCOR),
     &                AMCIOP(4,4,2,NUMCOR), ZMOPTI(4,4,2)
C*CC BOMOPT
C*CA BOMCAL
      REAL DSCALE, CSCALE, PCALMM, XIPOFF, GAINA0, GAINA1, GAINA2,
     &     CURCOR, CALCOR, DEFCOR
      INTEGER IADCOR, IXYCOR
      COMMON /BOMCAL/ DSCALE, CSCALE, PCALMM, XIPOFF(4),
     &                GAINA0(4,2), GAINA1(4,2), GAINA2(4,2),
     &                CURCOR(29), CALCOR(4,2), DEFCOR(4,2),
     &                IADCOR(4,2), IXYCOR(4)
C*CC BOMCAL
C*CA BOMOJJ
      PARAMETER(JBOMIR=1,JBOMRI=2,JBOMB1=3,JBOMB2=4,JBOMB3=5,JBOMB4=6,
     +          JBOMI1=7,JBOMI2=8,JBOMI3=9,JBOMI4=10,LBOMOA=10)
C*CC BOMOJJ
C*CA BOMDJJ
      PARAMETER(JBOMDS=1,JBOMCS=2,JBOMPC=3,JBOMO1=4,JBOMO2=5,JBOMO3=6,
     +          JBOMO4=7,LBOMDA=7)
C*CC BOMDJJ
C*CA BOMGJJ
      PARAMETER(JBOMG0=1,JBOMG1=2,JBOMG2=3,LBOMGA=3)
C*CC BOMGJJ
C*CA BOMMJJ
      PARAMETER(JBOMMC=1,JBOMMA=2,JBOMMX=3,LBOMMA=3)
C*CC BOMMJJ
C
      INTEGER I, J, KAB, IROW, AGETDB, IRET, KBOMD, KBOMG,
     &        KBOMM, KBOMO
C*CA BMACRO
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C*CC BMACRO
C.......................................................................
C
C  Find run number
C
      KRUNR = IW( NAMIND('RUNR') )
      IF (KRUNR .EQ. 0) GOTO 998
      IRUN = IW(KRUNR + 2)
C
C  Read banks from data base
C
      IRET = AGETDB ('BOMDBOMGBOMMBOMO', IRUN)
      IF (IRET .EQ. 0) GOTO 998
C
      KBOMD = IW( NAMIND('BOMD') )
      KBOMG = IW( NAMIND('BOMG') )
      KBOMM = IW( NAMIND('BOMM') )
      KBOMO = IW( NAMIND('BOMO') )
C
      IF (KBOMD .EQ. 0 .OR.
     &    KBOMG .EQ. 0 .OR.
     &    KBOMM .EQ. 0 .OR.
     &    KBOMO .EQ. 0     ) GOTO 998
C
C  Fill COMMON with calibration constants from banks
C
      DSCALE = RTABL (KBOMD, 1, JBOMDS)
      CSCALE = RTABL (KBOMD, 1, JBOMCS)
      PCALMM = RTABL (KBOMD, 1, JBOMPC)
      XIPOFF(1) = RTABL (KBOMD, 1, JBOMO1)
      XIPOFF(2) = RTABL (KBOMD, 1, JBOMO2)
      XIPOFF(3) = RTABL (KBOMD, 1, JBOMO3)
      XIPOFF(4) = RTABL (KBOMD, 1, JBOMO4)
C
      DO 200 J = 1, 2
        DO 100 I = 1, 4
          IROW = J*4 + I - 4
          GAINA0(I,J) = RTABL (KBOMG, IROW, JBOMG0)
          GAINA1(I,J) = RTABL (KBOMG, IROW, JBOMG1)
          GAINA2(I,J) = RTABL (KBOMG, IROW, JBOMG2)
          CALCOR(I,J) = RTABL (KBOMM, IROW, JBOMMC)
          IADCOR(I,J) = ITABL (KBOMM, IROW, JBOMMA)
          IXYCOR(I)   = ITABL (KBOMM, IROW, JBOMMX)
  100   CONTINUE
  200 CONTINUE
C
C  Fill COMMON with beam optics from banks
C
      DO 500 KAB = 1, 2
        DO 400 J = 1, 4
          DO 300 I = 1, 4
            IROW = KAB*16 + J*4 + I - 20
            RIBOPT(I,J,KAB)   = RTABL (KBOMO, IROW, JBOMIR)
            RBIOPT(I,J,KAB)   = RTABL (KBOMO, IROW, JBOMRI)
            RCBOPT(I,J,KAB,1) = RTABL (KBOMO, IROW, JBOMB1)
            RCBOPT(I,J,KAB,2) = RTABL (KBOMO, IROW, JBOMB2)
            RCBOPT(I,J,KAB,3) = RTABL (KBOMO, IROW, JBOMB3)
            RCBOPT(I,J,KAB,4) = RTABL (KBOMO, IROW, JBOMB4)
            RCIOPT(I,J,KAB,1) = RTABL (KBOMO, IROW, JBOMI1)
            RCIOPT(I,J,KAB,2) = RTABL (KBOMO, IROW, JBOMI2)
            RCIOPT(I,J,KAB,3) = RTABL (KBOMO, IROW, JBOMI3)
            RCIOPT(I,J,KAB,4) = RTABL (KBOMO, IROW, JBOMI4)
  300     CONTINUE
  400   CONTINUE
  500 CONTINUE
C
      XINIFL = .TRUE.
      GOTO 999
C
  998 CALL RERROR ('OMDATA', 1, ' Error reading database')
      XINIFL = .FALSE.
C
  999 RETURN
      END
C*DK OINIJO
      SUBROUTINE OINIJO
C----------------------------------------------------------------------
C!  - BOM : Job Initialisation
C!
C!   Author   :- E. Lancon             12-AUG-1991
C!
C!      Set Init. Flags to .FALSE.
C?
C!======================================================================
C*IF .NOT.DOC
C*CA BOMFLG
      LOGICAL XINIFL, XCURFL
      INTEGER IMCURR
      COMMON /BOMFLG/ XINIFL, XCURFL, IMCURR
C*IF DOC
C*CC BOMFLG
C.......................................................................
C
      XCURFL = .FALSE.
      XINIFL = .FALSE.
      IMCURR = 0
C
  999 RETURN
      END
C*DK OINIRU
      SUBROUTINE OINIRU
C----------------------------------------------------------------------
C!  - Initialisation for BOM analysis
C!
C!   Author:  R.W.Forty  23-May-91
C?
C!======================================================================
C*IF .NOT.DOC
C
C*CA BOMFLG
      LOGICAL XINIFL, XCURFL
      INTEGER IMCURR
      COMMON /BOMFLG/ XINIFL, XCURFL, IMCURR
C*CC BOMFLG
C.......................................................................
C
C  Read banks from Data Base for this run
C
      CALL OMDATA
C
C  Calculate optics if Banks have been read
C
      IF (XINIFL) CALL OMBOPT
C
      RETURN
      END
C*DK OMBOOK
      SUBROUTINE OMBOOK
C----------------------------------------------------------------------
C!  - BOM : Book Histograms
C!
C!   Author   :- E. Lancon             12-AUG-1991
C!
C?
C!======================================================================
C*IF .NOT.DOC
  999 RETURN
      END
C*DK OMBOPT
      SUBROUTINE OMBOPT
C----------------------------------------------------------------------
C!  - Calculate optics matrices
C!
C!   Author:  R.W.Forty  23-May-91
C?
C!======================================================================
C*IF .NOT.DOC
C*CA BOMOPT
      INTEGER NUMCOR
      PARAMETER (NUMCOR = 4)
      REAL RIBOPT, RBIOPT, PIBOPT, RCBOPT, RCIOPT, AMCIOP, ZMOPTI
      COMMON /BOMOPT/ RIBOPT(4,4,2), RBIOPT(4,4,2), PIBOPT(4,4,2),
     &                RCBOPT(4,4,2,NUMCOR), RCIOPT(4,4,2,NUMCOR),
     &                AMCIOP(4,4,2,NUMCOR), ZMOPTI(4,4,2)
C*IF DOC
C*CC BOMOPT
C
      INTEGER KC, KAB, IFAIL, IZ, I, J, KA, KB, IDPA
      REAL VA(16)
C.......................................................................
C
C  Build P and M matrices
C
      DO 200 KAB = 1, 2
        CALL UCOPY (RBIOPT(1,1,KAB), PIBOPT(1,1,KAB), 16)
        CALL RINV (4, PIBOPT(1,1,KAB), 4, VA, IFAIL)

        DO 100 KC = 1, NUMCOR
          CALL RMMLT (4, 4, 4, PIBOPT(1,1,KAB), PIBOPT(1,2,KAB),
     &                PIBOPT(2,1,KAB), RCIOPT(1,1,KAB,KC),
     &                RCIOPT(1,2,KAB,KC),
     &                RCIOPT(2,1,KAB,KC), AMCIOP(1,1,KAB,KC),
     &                AMCIOP(1,2,KAB,KC), AMCIOP(2,1,KAB,KC), VA)
  100   CONTINUE
  200 CONTINUE
C
      DO 500 IDPA = 1, 2
C
C  IDPA = 1 analyze e+
C         2         e-
C
        IF (IDPA .EQ. 1) THEN
          KA = 2
          KB = 1
        ELSE
          KA = 1
          KB = 2
        ENDIF
C
        DO 400 IZ = 1, 2
          DO 300 J = 1, 4
            ZMOPTI(IZ,J,IDPA)   = PIBOPT((IZ-1)*2+1,J,KB)
            ZMOPTI(IZ+2,J,IDPA) = RIBOPT((IZ-1)*2+1,J,KA)
  300     CONTINUE
  400   CONTINUE
  500 CONTINUE
C
      RETURN
      END
C*DK OMBOUT
      SUBROUTINE OMBOUT (XY, XIPB, IBUNB, BCURB, GRATB, IER)
C----------------------------------------------------------------------
C!  - Output results of BOM analysis
C!
C!   Author:  R.W.Forty  23-May-91
C!   Modified by : J. Nash 8-May-91 Changed IW to RW in appropriate plac
C!   Input:  XIPB  = Array containing the positions and angles at the IP
C!                   (x, x', y, y') for each beam
C!           IBUNB = Bunch id for each beam
C!           BCURB = Beam current at each BOM for each beam
C!           GRATB = Gain ratio of each pair of electrodes for each beam
C!           IER   = 0  OK
C!                 < 0  Fatal error in BOM analysis
C!                 > 0  Non-fatal data quality error flag
C?
C!======================================================================
C*IF .NOT.DOC
C*CA BCS
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C*CC BCS
C*CA BOMFLG
      LOGICAL XINIFL, XCURFL
      INTEGER IMCURR
      COMMON /BOMFLG/ XINIFL, XCURFL, IMCURR
C*CC BOMFLG
C*CA BOMPJJ
      PARAMETER(JBOMXA=1,JBOMYA=2,JBOMSA=3,JBOMCA=4,JBOMXB=5,JBOMYB=6,
     +          JBOMSB=7,JBOMCB=8,JBOMBI=9,JBOMPU=10,LBOMPA=10)
C*CC BOMPJJ
C*CA BOMBJJ
      PARAMETER(JBOMXX=1,JBOMYY=2,JBOMEE=3,LBOMBA=3)
C*CC BOMBJJ
C*CA BOMEJJ
      PARAMETER(JBOMXI=1,JBOMXP=2,JBOMYI=3,JBOMYP=4,JBOMBU=5,JBOMIA=6,
     +          JBOMIB=7,JBOMR1=8,JBOMR2=9,JBOMR3=10,JBOMR4=11,
     +          JBOMER=12,LBOMEA=12)
C*CC BOMEJJ
C
      REAL XIPB(4,*), BCURB(2,*), GRATB(4,*), XY(4,2)
      INTEGER IER, NCOL, NROW, KBOMB, KBOME, JBOME, IROW, IERFL(2),
     &        IGARB, IBUNB(*)
C
      DATA NCOL, NROW /12, 2/
C*CA BMACRO
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C*CC BMACRO
C.......................................................................
C
      KBOMP = IW( NAMIND('BOMP') )
      IF (KBOMP .EQ. 0) GOTO 999
      DO i = 1, 2
         j = 3 - i
         JBOMP = KBOMP + LMHLEN + (i-1) * LBOMPA
         IW (JBOMP + JBOMXA) = INT (XY(1,j) * 1.E5 + 0.5)
         IW (JBOMP + JBOMYA) = INT (XY(3,j) * 1.E5 + 0.5)
         IW (JBOMP + JBOMXB) = INT (XY(2,j) * 1.E5 + 0.5)
         IW (JBOMP + JBOMYB) = INT (XY(4,j) * 1.E5 + 0.5)
      ENDDO
C
      CALL AUBOS ('BOMB', 0, 3+LMHLEN, KBOMB, IGARB)
C
      IF (KBOMB .EQ. 0) GOTO 999
C
      IW (KBOMB + LMHCOL) = 3
      IW (KBOMB + LMHROW) = 1
      RW (KBOMB + LMHLEN + JBOMXX) = (XIPB(1,1) + XIPB(1,2)) / 2.
      RW (KBOMB + LMHLEN + JBOMYY) = (XIPB(3,1) + XIPB(3,2)) / 2.
      IW (KBOMB + LMHLEN + JBOMEE) = IER
C
      CALL AUBOS ('BOME', 0, NCOL*NROW+LMHLEN, KBOME, IGARB)
C
      IF (KBOME .EQ. 0) GOTO 999
C
      IW (KBOME + LMHCOL) = NCOL
      IW (KBOME + LMHROW) = NROW
C
      IERFL(1) = IER
      IERFL(2) = IMCURR
C
      DO 100 IROW = 1, NROW
        JBOME = KBOME + LMHLEN + (IROW-1)*NCOL
C
        RW (JBOME + JBOMXI) =  XIPB(1,IROW)
        RW (JBOME + JBOMXP) =  XIPB(2,IROW)
        RW (JBOME + JBOMYI) =  XIPB(3,IROW)
        RW (JBOME + JBOMYP) =  XIPB(4,IROW)
        IW (JBOME + JBOMBU) = IBUNB(IROW)
        RW (JBOME + JBOMIA) = BCURB(1,IROW)
        RW (JBOME + JBOMIB) = BCURB(2,IROW)
        RW (JBOME + JBOMR1) = GRATB(1,IROW)
        RW (JBOME + JBOMR2) = GRATB(2,IROW)
        RW (JBOME + JBOMR3) = GRATB(3,IROW)
        RW (JBOME + JBOMR4) = GRATB(4,IROW)
        IW (JBOME + JBOMER) = IERFL(IROW)
C
  100 CONTINUE
C
  999 RETURN
      END
C*DK OMBPOS
      SUBROUTINE OMBPOS (DAT, PEDS, GAIN, XYBOM, BCUR, IRET)
C----------------------------------------------------------------------
C!  - Calculate beam position and current at BOM pickup
C!
C!   Author:  R.W.Forty  23-May-91
C!
C!   Input:  DAT   = Raw data for each electrode
C!           PEDS  = Pedestals for each electrode
C!           GAIN  = Relative gain for electrode pairs
C!   Output: XYBOM = 4-component vector containing the positions
C!                   at the BOM pickups: (xA, xB, yA, yB)
C!           BCUR  = Beam current at each BOM (in ADC counts)
C!           IRET  = 0  OK
C!                >< 0  Error
C!
C!  Revised: 11-Apr-1992; Calculate x position in the Aleph det. coord.
C?
C!======================================================================
C*IF .NOT.DOC
C*CA BOMCAL
      REAL DSCALE, CSCALE, PCALMM, XIPOFF, GAINA0, GAINA1, GAINA2,
     &     CURCOR, CALCOR, DEFCOR
      INTEGER IADCOR, IXYCOR
      COMMON /BOMCAL/ DSCALE, CSCALE, PCALMM, XIPOFF(4),
     &                GAINA0(4,2), GAINA1(4,2), GAINA2(4,2),
     &                CURCOR(29), CALCOR(4,2), DEFCOR(4,2),
     &                IADCOR(4,2), IXYCOR(4)
C*IF DOC
C*CC BOMCAL
C
      REAL DAT(*), PEDS(*), GAIN(*), XYBOM(*), BCUR(*), PH(4),
     &     RG, P21, P43, Q21, Q43
      INTEGER IBOM, IRET, IOFF, IE
C.......................................................................
C
      DO 200 IBOM = 1, 2
        IOFF = 4 * (IBOM - 1)
        BCUR(IBOM) = 0.
C
        DO 100 IE = 1, 4
          RG = 1.
          IF (MOD(IE, 2) .EQ. 0) RG = GAIN( (IE + IOFF)/2 )
          PH(IE) = ABS( DAT(IE + IOFF) - PEDS(IE + IOFF) ) * RG
          BCUR(IBOM) = BCUR(IBOM) + PH(IE)
  100   CONTINUE
C
        P21 = PH(2) + PH(1)
        P43 = PH(4) + PH(3)
        IF (P21 .LE. 0.01 .OR. P43 .LE. 0.01) GOTO 998
C
        Q21 = (PH(2) - PH(1)) / P21
        Q43 = (PH(4) - PH(3)) / P43
C
        XYBOM(IBOM)     = PCALMM * (Q43 - Q21)
        XYBOM(IBOM + 2) = PCALMM * (Q43 + Q21)
C
  200 CONTINUE
C
      IRET = 0
      GOTO 999
C
  998 CALL RERROR ('OMBPOS', 1, ' Error calculating position at BOM')
      IRET = 1
C
  999 RETURN
      END
C*DK OMBREC
      SUBROUTINE OMBREC
C----------------------------------------------------------------------
C!  - Calculate beam parameters using BOM data
C!
C!   Author:  R.W.Forty  23-May-91
C?
C!======================================================================
C*IF .NOT.DOC
C*CA BOMFLG
      LOGICAL XINIFL, XCURFL
      INTEGER IMCURR
      COMMON /BOMFLG/ XINIFL, XCURFL, IMCURR
C*CC BOMFLG
C*CA BOMCAL
      REAL DSCALE, CSCALE, PCALMM, XIPOFF, GAINA0, GAINA1, GAINA2,
     &     CURCOR, CALCOR, DEFCOR
      INTEGER IADCOR, IXYCOR
      COMMON /BOMCAL/ DSCALE, CSCALE, PCALMM, XIPOFF(4),
     &                GAINA0(4,2), GAINA1(4,2), GAINA2(4,2),
     &                CURCOR(29), CALCOR(4,2), DEFCOR(4,2),
     &                IADCOR(4,2), IXYCOR(4)
C*CC BOMCAL
C
      REAL DAT(8), SIG(8), PEDS(8), CALI(8), GAIN(4), XYBOM(4),
     &     XIP(4), XIPB(4,2), BCUR(2), BCURB(2,2), GRAT(4), GRATB(4,2)
      INTEGER I, IDPA, IDBU, IRET, IER, IBUNB(2)
C
      DATA XIPB, BCURB, GRATB /8*0., 4*0., 8*0./
      DATA IBUNB /2*0/
C.......................................................................
C
      IF (.NOT. XINIFL) CALL OINIRU
      IF (.NOT. XINIFL) THEN
        IER = -1
        GOTO 500
      ENDIF
C
      IF (.NOT. XCURFL) THEN
        IER = -2
        GOTO 500
      ENDIF
C
C  Calculate corrector deflections
C
      CALL OMCORR
C
C  Loop over e+ and e- beams
C
      DO 400 IDPA = 1, 2
C
C  Read data from banks
C
        CALL OMREAD (IDPA, IDBU, DAT, SIG, PEDS, CALI, IRET)
        IF (IRET .NE. 0) THEN
          IER = -3
          GOTO 500
        ENDIF
C
C  Use beam calibration
C
        CALL OMGAIN (IDPA, DAT, PEDS, CALI, GAIN, GRAT, IRET)
        IF (IRET .NE. 0) THEN
          IER = -4
          GOTO 500
        ENDIF
C
C  Calculate beam position at BOMs
C
        CALL OMBPOS (DAT, PEDS, GAIN, XYBOM, BCUR, IRET)
        IF (IRET .NE. 0) THEN
          IER = -5
          GOTO 500
        ENDIF
C
C  Calculate beam position and angle at IP
C
        CALL OMIPOS (IDPA, XYBOM, XIP, IRET)
        IF (IRET .NE. 0) THEN
          IER = -6
          GOTO 500
        ENDIF
C
        IBUNB(IDPA) = IDBU
C
        DO 200 I = 1, 4
          XIPB (I, IDPA) =  XIP(I) - XIPOFF(I)
          GRATB(I, IDPA) = GRAT(I)
  200   CONTINUE
C
        DO 300 I = 1, 2
          BCURB(I, IDPA) = BCUR(I)
  300   CONTINUE
C
  400 CONTINUE
C
      IER = 0
C
  500 CALL OMBOUT (XIPB, IBUNB, BCURB, DEFCOR, IER)
C
      RETURN
      END
C*DK OMCORR
      SUBROUTINE OMCORR
C----------------------------------------------------------------------
C!  - Calculate BOM corrector magnet deflections
C!
C!   Author:  R.W.Forty  16-Oct-91
C?
C!======================================================================
C*IF .NOT.DOC
C*CA BCS
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C*CC BCS
C*CA BOMFLG
      LOGICAL XINIFL, XCURFL
      INTEGER IMCURR
      COMMON /BOMFLG/ XINIFL, XCURFL, IMCURR
C*CC BOMFLG
C*CA BOMOPT
      INTEGER NUMCOR
      PARAMETER (NUMCOR = 4)
      REAL RIBOPT, RBIOPT, PIBOPT, RCBOPT, RCIOPT, AMCIOP, ZMOPTI
      COMMON /BOMOPT/ RIBOPT(4,4,2), RBIOPT(4,4,2), PIBOPT(4,4,2),
     &                RCBOPT(4,4,2,NUMCOR), RCIOPT(4,4,2,NUMCOR),
     &                AMCIOP(4,4,2,NUMCOR), ZMOPTI(4,4,2)
C*CC BOMOPT
C*CA BOMCAL
      REAL DSCALE, CSCALE, PCALMM, XIPOFF, GAINA0, GAINA1, GAINA2,
     &     CURCOR, CALCOR, DEFCOR
      INTEGER IADCOR, IXYCOR
      COMMON /BOMCAL/ DSCALE, CSCALE, PCALMM, XIPOFF(4),
     &                GAINA0(4,2), GAINA1(4,2), GAINA2(4,2),
     &                CURCOR(29), CALCOR(4,2), DEFCOR(4,2),
     &                IADCOR(4,2), IXYCOR(4)
C*CC BOMCAL
C
      INTEGER I, KAB, KC
      REAL ELEP, ELEP0
C
C  LEP beam energy on peak
      DATA ELEP0 /45.625/
C*CA BMACRO
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C*CC BMACRO
C.......................................................................
C
C  Find run number
C
      KRUNR = IW( NAMIND('RUNR') )
      IF (KRUNR .EQ. 0) THEN
        CALL RERROR ('OMCORR', 1, ' Could not find run number')
        ELEP = ELEP0
      ELSE
        IRUN = IW(KRUNR + 2)
C
C  Find LEP beam energy
C
        ELEP = ALELEP (IRUN) / 2.
C
        IF (ELEP .LT. 40. .OR. ELEP .GT. 50.) THEN
          CALL RERROR ('OMCORR', 2, ' Bad LEP beam energy')
          ELEP = ELEP0
        ENDIF
      ENDIF
C
      IMCURR = 0
      DO 200 KAB = 1, 2
        DO 100 KC = 1, NUMCOR
          I = IADCOR(KC,KAB)
          DEFCOR(KC,KAB) = CURCOR(I) * CALCOR(KC,KAB) * 20. /
     &                     (ELEP * CSCALE)
          IMCURR = IMCURR + INT (10000. * DEFCOR(KC,KAB))
  100   CONTINUE
  200 CONTINUE
C
      RETURN
      END
C*DK OMCRUN
      SUBROUTINE OMCRUN
C----------------------------------------------------------------------
C!  - BOM : Close Run
C!
C!   Author   :- E. Lancon             12-AUG-1991
C!
C!      Set Init. Flags to .FALSE.
C?
C!======================================================================
C*IF .NOT.DOC
C*CA BOMFLG
      LOGICAL XINIFL, XCURFL
      INTEGER IMCURR
      COMMON /BOMFLG/ XINIFL, XCURFL, IMCURR
C*CC BOMFLG
C.......................................................................
C
      XCURFL = .FALSE.
      XINIFL = .FALSE.
      IMCURR = 0
C
C.......................................................................
  999 RETURN
      END
C*DK OMGAIN
      SUBROUTINE OMGAIN (IDPA, DAT, PEDS, CALI, GAIN, GRAT, IRET)
C----------------------------------------------------------------------
C!  - Update relative gains for each BOM electrode pair
C!
C!   Author:  R.W.Forty  23-May-91
C!
C!   Input:  DAT   = Raw signals for each electrode
C!           PEDS  = Pedestals for each electrode
C!           CALI  = Calibration signal for each electrode
C!           IDPA  = 1  e+
C!                   2  e-
C!   Output: GAIN  = Relative gain for the 4 electrode pairs
C!           GRAT  = Gain ratio from calibration pulses for each
C!                   electrode pair
C!           IRET  = 0  OK
C!                >< 0  Error
C?
C!======================================================================
C*IF .NOT.DOC
C*CA BOMCAL
      REAL DSCALE, CSCALE, PCALMM, XIPOFF, GAINA0, GAINA1, GAINA2,
     &     CURCOR, CALCOR, DEFCOR
      INTEGER IADCOR, IXYCOR
      COMMON /BOMCAL/ DSCALE, CSCALE, PCALMM, XIPOFF(4),
     &                GAINA0(4,2), GAINA1(4,2), GAINA2(4,2),
     &                CURCOR(29), CALCOR(4,2), DEFCOR(4,2),
     &                IADCOR(4,2), IXYCOR(4)
C*CC BOMCAL
C
      REAL CALI(*), GAIN(*), GRAT(*), DAT(*), PEDS(*), PHEP
      INTEGER IEP, IEL, IDPA, IRET
C.......................................................................
C
      DO 100 IEP = 1, 4
        IEL = 2 * (IEP-1) + 1
        PHEP = DAT(IEL)-PEDS(IEL) + DAT(IEL+1)-PEDS(IEL+1)
        PHEP = ABS(PHEP)
        IF (ABS(PHEP) .LT. 0.01) GOTO 998
C
        GAIN(IEP) = GAINA0(IEP,IDPA) +
     &              GAINA1(IEP,IDPA) * PHEP +
     &              GAINA2(IEP,IDPA) * PHEP**2
C
C  Calculate gain ratio from calibration pulses
C
        IF ( ABS( CALI(2*IEP) - PEDS(2*IEP) ) .LT. 0.01 ) THEN
          GRAT(IEP) = 999.
        ELSE
          GRAT(IEP) = ( CALI(2*IEP-1) - PEDS(2*IEP-1) ) /
     &                ( CALI(2*IEP)   - PEDS(2*IEP)   )
        ENDIF
C
c       GAIN(IEP) = GAIN(IEP) * GRAT(IEP)
C
  100 CONTINUE
C
      IRET = 0
      GOTO 999
C
  998 CALL RERROR ('OMGAIN', 1, ' Bad pulse height for gain calc')
      IRET = 1
C
  999 RETURN
      END
C*DK OMHIST
      SUBROUTINE OMHIST
C----------------------------------------------------------------------
C!  - BOM : Histogram Event
C!
C!   Author   :- E. Lancon             12-AUG-1991
C!
C?
C!======================================================================
C*IF .NOT.DOC
  999 RETURN
      END
C*DK OMIPOS
      SUBROUTINE OMIPOS (IDPA, XYBOM, XIP, IRET)
C----------------------------------------------------------------------
C!  - Calculate beam position and angle at the IP using the BOM position
C!
C!   Author:  R.W.Forty  23-May-91
C!            based on code from W.Kozanecki
C!
C!   Input:  XYBOM = 4-component vector of x and y positions
C!                   read by BOMs: (xA, xB, yA, yB)
C!           IDPA  = 1  e+
C!                   2  e-
C!   Output: XIP   = 4-component vector containing the positions
C!                   and angles at the IP: (x, x', y, y')
C!           IRET  = 0  OK
C!                >< 0  Error
C?
C!======================================================================
C*IF .NOT.DOC
C*CA BOMOPT
      INTEGER NUMCOR
      PARAMETER (NUMCOR = 4)
      REAL RIBOPT, RBIOPT, PIBOPT, RCBOPT, RCIOPT, AMCIOP, ZMOPTI
      COMMON /BOMOPT/ RIBOPT(4,4,2), RBIOPT(4,4,2), PIBOPT(4,4,2),
     &                RCBOPT(4,4,2,NUMCOR), RCIOPT(4,4,2,NUMCOR),
     &                AMCIOP(4,4,2,NUMCOR), ZMOPTI(4,4,2)
C*CC BOMOPT
C*CA BOMCAL
      REAL DSCALE, CSCALE, PCALMM, XIPOFF, GAINA0, GAINA1, GAINA2,
     &     CURCOR, CALCOR, DEFCOR
      INTEGER IADCOR, IXYCOR
      COMMON /BOMCAL/ DSCALE, CSCALE, PCALMM, XIPOFF(4),
     &                GAINA0(4,2), GAINA1(4,2), GAINA2(4,2),
     &                CURCOR(29), CALCOR(4,2), DEFCOR(4,2),
     &                IADCOR(4,2), IXYCOR(4)
C*CC BOMCAL
C
      INTEGER IDPA, KA, KB, KC, I, IBA, KCBA, IXY, IFAIL, IRET
      REAL XYBOM(*), XIP(*)
C
C  XCV Corrector kick vector of the form (0,x' ,0,0) or (0,0,0,y')
C      depending of whether it is an X- or a Y- corrector
C  XC  Corrector kick (x' or y'   as defined for XCV)
C  VA,VB,RW4  work area vectors and matrix
C  DV  D vector
C
      REAL XCV(4), XC, VA(4), VB(4), RW4(4,4), DV(4)
C.......................................................................
C
C  Inverse beam transport equations
C
      IF (IDPA .EQ. 1) THEN
        KA = 2
        KB = 1
      ELSE
        KA = 1
        KB = 2
      ENDIF
C
      CALL VZERO (DV,4)
C
      DO 300 IBA = 1, 2
C
C  IBA = 1 refers to top half of D vector, IBA = 2 to bottom half
C
        DO 200 KC = 1, NUMCOR
          CALL VZERO (XCV,4)
          IF (IBA. EQ. 1) THEN
            KCBA = KB
          ELSE
            KCBA = KA
          ENDIF
C
          XC = DEFCOR(KC,KCBA)
          IXY = IXYCOR(KC)
c
C
C  Flip sign of corrector deflections for horizontal corrector on e+
C
          IF (IDPA .EQ. 1 .AND. IXY .EQ. 1) XC = -XC
c         IF (IXY .EQ. 2) XC = -XC
          XCV(2*IXY) = XC
          IF (IBA .EQ. 1) THEN
            CALL RMMLT (4, 4, 1, AMCIOP(1,1,KB,KC), AMCIOP(1,2,KB,KC),
     &                  AMCIOP(2,1,KB,KC), XCV(1), XCV(1), XCV(2),
     &                  VA(1), VA(1), VA(2), VB)
          ELSE
            CALL RMMLT (4, 4, 1, RCBOPT(1,1,KA,KC), RCBOPT(1,2,KA,KC),
     &                  RCBOPT(2,1,KA,KC), XCV(1), XCV(1), XCV(2),
     &                  VA(1), VA(1), VA(2), VB)
            VA(1) = -VA(1)
            VA(3) = -VA(3)
          ENDIF
C
          DO 100 I = 1, 2
            DV((IBA-1)*2+I) = DV((IBA-1)*2+I) + VA(2*I-1)
  100     CONTINUE
  200   CONTINUE
  300 CONTINUE
C
C  Add measured BOM positions
C
      VA(1) = XYBOM(KB)
      VA(2) = XYBOM(KB+2)
      VA(3) = XYBOM(KA)
      VA(4) = XYBOM(KA+2)
C
C  VA  vector of input BOM data, in the order (if analyzing e-):
C      (x at BOM B, yB, xA, yA).  For positrons, remember x+ = - x-
C
      IF (IDPA .EQ. 1) THEN
        DO 400 I = 1, 3, 2
          VA(I) = -VA(I)
  400   CONTINUE
      ENDIF
C
      DO 500 I = 1, 4
        DV(I) = DV(I) + VA(I)
  500 CONTINUE
C
C  Ready to solve the linear system ZMOPTI * XIP = DV  for XIP
C
      CALL UCOPY (ZMOPTI(1,1,IDPA), RW4, 16)
      CALL REQN (4, RW4, 4, VA, IFAIL, 1, DV)
      IF (IFAIL .EQ. 0) THEN
        CALL UCOPY (DV, XIP, 4)
      ELSE
        GOTO 998
      ENDIF
C
C  If positrons, convert to e- coordinate system
C
      IF (IDPA .EQ. 1) THEN
        XIP(1) = -XIP(1)
        XIP(4) = -XIP(4)
      ENDIF
C
      IRET = 0
      GOTO 999
C
  998 CALL RERROR ('OMIPOS', 1, ' Error calculating position at IP')
      IRET = 1
C
  999 RETURN
      END
C*DK OMREAD
      SUBROUTINE OMREAD (IDPA, IDBU, DAT, SIG, PEDS, CALI, IRET)
C----------------------------------------------------------------------
C!  - Read BOM data from banks
C!
C!   Author:  R.W.Forty  23-May-91
C!   Modified by : J. Nash 8-2-91  Fixed e- and e+ (e- is first in BOMR)
C!   Input:  IDPA  = 1  e+
C!                   2  e-
C!   Output: IDBU  = Bunch id (1-4)
C!           DAT   = Raw signals for each electrode
C!           SIG   = Sigma (rms) of raw data measurement
C!           PEDS  = Pedestals for each electrode
C!           CALI  = Calibration signal for each electrode
C!           IRET  = 0  OK
C!                >< 0  Error
C?
C!======================================================================
C*IF .NOT.DOC
C*CA BCS
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C*CC BCS
C*CA BOMCAL
      REAL DSCALE, CSCALE, PCALMM, XIPOFF, GAINA0, GAINA1, GAINA2,
     &     CURCOR, CALCOR, DEFCOR
      INTEGER IADCOR, IXYCOR
      COMMON /BOMCAL/ DSCALE, CSCALE, PCALMM, XIPOFF(4),
     &                GAINA0(4,2), GAINA1(4,2), GAINA2(4,2),
     &                CURCOR(29), CALCOR(4,2), DEFCOR(4,2),
     &                IADCOR(4,2), IXYCOR(4)
C*CC BOMCAL
C*CA BOMRJJ
      PARAMETER(JBOMMN=1,JBOMSI=2,JBOMPE=3,JBOMCL=4,LBOMRA=4)
C*CC BOMRJJ
C*CA BOMPJJ
      PARAMETER(JBOMXA=1,JBOMYA=2,JBOMSA=3,JBOMCA=4,JBOMXB=5,JBOMYB=6,
     +          JBOMSB=7,JBOMCB=8,JBOMBI=9,JBOMPU=10,LBOMPA=10)
C*CC BOMPJJ
C
      REAL DAT(*), SIG(*), PEDS(*), CALI(*)
      INTEGER I, IDPA, IDBU, IRET, KBOMR, KBOMP
C*CA BMACRO
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C*CC BMACRO
C.......................................................................
      IRET = 3
C
      KBOMR = IW( NAMIND('BOMR') )
      KBOMP = IW( NAMIND('BOMP') )
C
      IF (KBOMR .EQ. 0 .OR. KBOMP .EQ. 0) GOTO 997
C
C  Read pedestals
C
      IOFF = 8
      IF (IDPA .EQ. 2) IOFF = 0
      DO 100 I = 1, 8
        PEDS(I) = FLOAT (ITABL (KBOMR, I, JBOMPE)) / DSCALE
        IF (PEDS(I) .LT. 1.) GOTO 998
  100 CONTINUE
C
C  Read data
C
      IF (IDPA .EQ. 2) THEN
        IDBU = ITABL (KBOMP, 1, JBOMBI)
        IF (IDBU .LT. 1 .OR. IDBU .GT. 4) GOTO 998
      ELSE
        IDBU = ITABL (KBOMP, 2, JBOMBI)
        IF (IDBU .LT. 1 .OR. IDBU .GT. 4) GOTO 998
      ENDIF
C
      DO 200 I = 1, 8
        CALI(I) = FLOAT (ITABL (KBOMR, I+IOFF, JBOMCL)) / DSCALE
  200 CONTINUE
C
      DO 300 I = 1, 8
        DAT(I) = FLOAT (ITABL (KBOMR, I+IOFF, JBOMMN)) / DSCALE
        SIG(I) = FLOAT (ITABL (KBOMR, I+IOFF, JBOMSI)) / DSCALE
c  Check for no data or saturation.
        IF ( (DAT(I) .LT. 1.0) .OR. (DAT(I) .GT. 4000.0) ) THEN
           GOTO 998
        ENDIF
c  Check for validity of data.
        IF (IDPA .EQ. 2) THEN
          IF (DAT(I) .LT. PEDS(I)+0.01) GOTO 998
        ELSE
          IF (DAT(I) .GT. PEDS(I)-0.01 .OR.
     &        DAT(I) .LT.         0.01) GOTO 998
        ENDIF
  300 CONTINUE
c
      IRET = 0
      GOTO 999
C
  997 CALL RERROR ('OMREAD', 1, ' Error reading BOM raw data banks')
      IRET = 1
      GOTO 999
  998 CALL RERROR ('OMREAD', 2, ' Bad BOM raw data')
      IRET = 2
C
  999 RETURN
      END
C*DK OPRSUM
      SUBROUTINE OPRSUM
C----------------------------------------------------------------------
C!  - BOM : Summarize the runs that have been processed
C!
C!   Author   :- E. Lancon             12-AUG-1991
C!
C?
C!======================================================================
C*IF .NOT.DOC
  999 RETURN
      END
C*DK OSLOWC
      SUBROUTINE OSLOWC
C----------------------------------------------------------------------
C!  - Read BOM slow control record
C!
C!   Author:  R.W.Forty  16-Oct-91
C?
C!======================================================================
C*IF .NOT.DOC
C*CA BCS
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C*CC BCS
C*CA BOMFLG
      LOGICAL XINIFL, XCURFL
      INTEGER IMCURR
      COMMON /BOMFLG/ XINIFL, XCURFL, IMCURR
C*CC BOMFLG
C*CA BOMCAL
      REAL DSCALE, CSCALE, PCALMM, XIPOFF, GAINA0, GAINA1, GAINA2,
     &     CURCOR, CALCOR, DEFCOR
      INTEGER IADCOR, IXYCOR
      COMMON /BOMCAL/ DSCALE, CSCALE, PCALMM, XIPOFF(4),
     &                GAINA0(4,2), GAINA1(4,2), GAINA2(4,2),
     &                CURCOR(29), CALCOR(4,2), DEFCOR(4,2),
     &                IADCOR(4,2), IXYCOR(4)
C*CC BOMCAL
C*CA BOMCJJ
      PARAMETER(JBOMCU=1,LBOMCA=29)
C*CC BOMCJJ
C
      INTEGER I, KBOMC
C*CA BMACRO
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C*CC BMACRO
C.......................................................................
C
      KBOMC = IW( NAMIND('BOMC') )
      IF (KBOMC .EQ. 0) GOTO 999
C
      DO 100 I = 1, 29
        CURCOR(I) = FLOAT ( ITABL (KBOMC, 1, JBOMCU+I-1) )
  100 CONTINUE
C
      XCURFL = .TRUE.
C
  999 RETURN
      END
      SUBROUTINE RERROR (CALID, MNI, MSG)
C----------------------------------------------------------------------
C  Simplified JULIA error reporting routine
C----------------------------------------------------------------------
C
      INCLUDE '/aleph/phy/qcde.inc'
C
      CHARACTER CALID*(*), MSG*(*)
C......................................................................
C
      WRITE(KUPRNT,*) CALID, MSG
C
      RETURN
      END
