      SUBROUTINE QFGET_BP (IRUN, IEVT, ICHUNK, VTX, VTX_ERR, VTX_SIZE)
C ----------------------------------------------------------------------
CKEY FILL BEAMSPOT / INTERNAL
C!  Chunk-by-chunk beam position and size
C - Original version: Dave Brown  9-1-92
C   Adapted to ALPHA and to 1993 data                J. Boucrot 14-6-93
C   Modified MC code to run off database             C. Gay     23-6-93
C   Fix up a few details                         S. Wasserbaech  8-7-93
C   Set up to read BPBB for 1993, ALPB for 1994/95; add Entry QFGET_CH
C                                                S. Wasserbaech  2-5-95
C
C   Major changes (S. Wasserbaech, August 1995):
C     Use BE setup code (see ADBR) to identify run periods.
C     Decide on use of BPBB according to JULIA version, not year.
C     Replaced ALLR with ALRP.
C     Made the routine work without annual interventions.
C     Added control by means of BPER, BPWT, and BSIZ cards.
C     Switched to RANMAR and RNORML for MC calculations.
C     Moved code for new MC setup to routine XFMCBP.
C     Added Entry QFMCSX.
C     Moved HAC parameters to comdecks.
C     Cleaned up!
C
C   Add protection against failed fit (zeros in ALPB), SW, 30 Oct 95
C
C
C - Input:
C   IRUN         / I  Run number
C   IEVT         / I  Event number
C
C - Input and output:
C   ICHUNK       / I  Previous chunk number; set this to 0 for the
C                       first call.  It is updated by the subroutine,
C                       and the subsequent call to QFGET_BP should be
C                       made with the returned value.  If ICHUNK is
C                       returned -1, no BP information exists for this
C                       run/event.
C
C - Output:
C   VTX(3)       / R  (x,y,z) position of luminous region centroid (cm)
C                       Notice: VTX(3) is always set equal to zero.
C
C   VTX_ERR(3)   / R  Estimated uncertainties on VTX (cm)
C                       Notice: VTX_ERR(3) is always set equal to 1.
C
C   VTX_SIZE(3)  / R  (x,y,z) rms size of luminous region (cm),
C                       averaged over the year/run period
C
C
C   Real data:
C   The beam position x and y and their uncertainties returned in VTX
C   and VTX_ERR are taken from the ALPB bank in the run record, or
C   from the beam.position file if JULIA version < 275.03.  The sizes
C   of the luminous region returned in VTX_SIZE are taken from the
C   ALRP database bank and are average values for the year (or run
C   period).
C
C   Monte Carlo:
C   The beam position x and y returned in VTX are generated so as to
C   simulate the desired beam size distribution.  The primary vertex
C   position of the event is extracted from truth information and an
C   offset is added according to the size of the luminous region.
C   The smearing corresponding to the average chunk-by-chunk beam
C   position uncertainty (VTX_ERR) is included.  The "desired" beam
C   size distribution is by default taken from measurements of the
C   actual LEP conditions for the geometry year of the Monte Carlo
C   file.  This default may be changed by means of data cards as
C   explained below.  The horizontal size of the luminous region,
C   sigma_x, is thrown at random from the measured distribution, which
C   is parametrized as the sum of two Gaussians (not a delta function).
C   The uncertainties in x and y returned in VTX_ERR, as well as the
C   three sizes returned in VTX_SIZE, are taken from the ALRP database
C   bank and are average values for the year.
C
C   Customizing the luminous region size for Monte Carlo:
C   As mentioned above, the default behavior is to use the luminous
C   region size parameters for the geometry year of the Monte Carlo
C   file.  The BPER card allows the user to override the default.  The
C   parameters from the year(s) specified on this card are then used;
C   each event is assigned at random to a particular year.  The years
C   (in the LEP 1 era) are weighted according to the numbers of qqbar
C   events in the MAYB/PERF runs in the VD run selection.  Different
C   weights may be specified by means of the BPWT card.  (This card is
C   mandatory for specifying the relative weights of LEP 2 periods,
C   if two or more periods are selected with BPER.)  Finally, arbitrary
C   beam sizes may be simulated by means of the BSIZ card.
C
C   The BPER, BPWT, and BSIZ cards have no effect when real data is
C   analyzed.
C
C   Syntax:
C
C   BPER [per1 [per2 [per3...]]]
C
C     where per1,per2,per3... are run periods as specified in the
C     first column of ADBR.  Years may be given as "1992" or "92"
C     or "9200".  Other run periods have the form "YYMM", such as
C     "9510".  A list of valid run periods is printed (and
C     execution terminates) if the BPER card is given with invalid
C     or no run periods.
C
C   BPWT [iwt1 [iwt2 [iwt3...]]]
C
C     where iwt1,iwt2,iwt3... are *integer* weights for the periods
C     listed on the BPER card.  An error results if there is no
C     accompanying BPER or if it has a different number of tokens.
C     The weights may have an arbitrary normalization.
C
C   BSIZ sigma_x sigma_y [sigma_z]
C
C     where sigma_x and sigma_y are floating point numbers giving the
C     desired sizes of the luminous region in cm.  The BSIZ card
C     overrides all values in ALRP.  The value of sigma_z, if given,
C     is returned in VTX_SIZE(3) but has no other effect.  If sigma_z
C     is not given, the value 1 is returned in VTX_SIZE(3).
C
C     Notes: the beam position uncertainty is set to zero if BSIZ is
C     given.  BPER/BPWT cards are ignored if BSIZ is given.
C
C   Examples:
C
C   1. To simulate 1993 and 1994 beam size conditions:
C      BPER 1993 1994
C
C   2. To simulate 1993 and 1994 with equal weights:
C      BPER 1993 1994
C      BPWT   50   50
C
C   3. To simulate a luminous region size of 120 x 7 microns:
C      BSIZ 0.0120 0.0007
C
C-----------------------------------------------------------------------
C   ENTRY QFGET_CH (IFRUN, IFEVT, ILRUN, ILEVT)
C
C   Returns first and last event numbers for the chunk containing the
C   event IRUN/IEVT specified in the previous call to QFGET_BP.  The
C   outputs are zero if the previous call to QFGET_BP was unsuccessful,
C   or if Monte Carlo is being analyzed.
C
C - Input:
C   None.
C
C - Output:
C   IFRUN  / I  Run number for first event in chunk
C   IFEVT  / I  Event number for first event in chunk
C   ILRUN  / I  Run number for last event in chunk
C   ILEVT  / I  Event number for last event in chunk.  Notice that this
C                 does not necessarily correspond to a real event: an
C                 arbitrary large number may be given if the chunk
C                 extends to the end of a run.
C-----------------------------------------------------------------------
C   ENTRY QFMCSX (ISETUP, SIGMAX)
C
C   Returns the BE setup code (row index in ALRP) used in the previous
C   call to QFGET_BP, and the generated sigma_x (width of the luminous
C   region) for that call.  The outputs are -1 if the previous call to
C   QFGET_BP was unsuccessful, or if real data is being analyzed, or
C   if a BSIZ card is in use.
C
C - Input:
C   None.
C
C - Output:
C   ISETUP  / I  Selected/generated BE setup code
C   SIGMAX  / R  Generated sigma_x (cm)
C-----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE FIRST, IGNSET, ITYPE_OLD, ISET_OLD, IRUN_OLD, LDBOK,
     >     NAALRP, NAFVER, NARHAH, NLR90,
     >     IJCOR, NSET, JSET, SETWT, XMC_PAR, VTX_SIZ, VTX_RES,
     >     IFRUN0, IFEVT0, ILRUN0, ILEVT0, ISETUP0, SIGMAX0
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
      INTEGER IW
      REAL RW(10000)
      COMMON /BCS/ IW(10000)
      EQUIVALENCE (RW(1),IW(1))
      INTEGER JALPFE,JALPXP,JALPXE,JALPYP,JALPYE,LALPBA
      PARAMETER(JALPFE=1,JALPXP=2,JALPXE=3,JALPYP=4,JALPYE=5,LALPBA=5)
      INTEGER JALRPN,JALRWE,JALRER,JALRXS,JALRYS,JALRZS,JALRXR,JALRYR,
     +          JALRF1,JALRM1,JALRS1,JALRM2,JALRS2,LALRPA
      PARAMETER(JALRPN=1,JALRWE=2,JALRER=3,JALRXS=4,JALRYS=5,JALRZS=6,
     +          JALRXR=7,JALRYR=8,JALRF1=9,JALRM1=10,JALRS1=11,
     +          JALRM2=12,JALRS2=13,LALRPA=13)
      INTEGER JBPBFM,JBPBLM,JBPBXW,JBPBYW,LBPBBA
      PARAMETER(JBPBFM=1,JBPBLM=2,JBPBXW=3,JBPBYW=4,LBPBBA=4)
      INTEGER JRHAPN,JRHAPD,JRHAPH,JRHAPV,JRHAAV,JRHADV,JRHADD,JRHANI,
     +          JRHANO,JRHACV,JRHANU,LRHAHA
      PARAMETER(JRHAPN=1,JRHAPD=3,JRHAPH=4,JRHAPV=5,JRHAAV=6,JRHADV=7,
     +          JRHADD=8,JRHANI=9,JRHANO=10,JRHACV=11,JRHANU=12,
     +          LRHAHA=12)
      INTEGER LCOLS, LROWS, KROW, KNEXT, ITABL,LFRWRD, LFRROW
      REAL RTABL
      INTEGER ID, NRBOS, L
C
C     Arguments:
      INTEGER IRUN, IEVT, ICHUNK
      REAL VTX(3), VTX_ERR(3), VTX_SIZE(3)
C
C     Arguments for Entry QFGET_CH:
      INTEGER IFRUN, IFEVT, ILRUN, ILEVT
C
C     Arguments for Entry QFMCSX:
      INTEGER ISETUP
      REAL SIGMAX
C
C     Beam position encoding parameters.  The beam positions and
C     uncertainties are stored as integers; the units are PFAC
C     and EFAC centimeters, respectively.
      REAL PFAC, EFAC
      PARAMETER (PFAC=0.00005)
      PARAMETER (EFAC=0.00001)
C
C     Other parameters:
C     NRMC   = highest possible MC run number
C     NSETMX = maximum number of run periods to simulate
      INTEGER NRMC, NSETMX
      PARAMETER (NRMC=2000)
      PARAMETER (NSETMX=25)
C
C     Local variables declaration:
      INTEGER IMARK
      INTEGER LUNDB, ITYPE_OLD, ITYPE, ISET_OLD, ISET, IISET
      INTEGER ISEED, NTOTIN, NTO2IN
      INTEGER I, NSET, JSET(NSETMX), ICOR, IOFF
      INTEGER NAALRP, KALRP, NALRP, IALRP
      INTEGER NAFVER, KFVER
      INTEGER NARHAH, KRHAH, NRHAH, IRHAH
      INTEGER IRUN_OLD, IRET, IROW, IPERIOD, JSETUP
      INTEGER IJCOR, KALPB, KBPBB
      INTEGER NCHUNK, JCHUNK, MINEV, MAXEV, MAXRO
      INTEGER IFRUN0, IFEVT0, ILRUN0, ILEVT0
      INTEGER ISETUP0,NLR90
      REAL XMC_PAR(5,NSETMX), VTX_SIZ(3,NSETMX)
      REAL VTX_RES(3,NSETMX), SETWT(NSETMX)
      REAL URAN(2), GRAN(5)
      REAL VMC(3), XMC_SIZ, YMC_SIZ
      REAL SIGMAX0
      LOGICAL FIRST, LMC, IGNSET, LDBOK, USEALPB
      CHARACTER*4 CNAM
      CHARACTER*8 PRNAM
C
C     Function types:
      INTEGER JUNIDB, MDARD, NAMIND, GTSTUP, ADBRUN, NLINK
      LOGICAL WITHIN, XFMCBP
      CHARACTER*4 CHAINT
C
C     Data statements:
      DATA FIRST / .TRUE. /
      DATA NLR90 / 9114 /
C
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+LMHCOL)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+LMHROW)
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
C-----------------------------------------------------------------------
C     An inline function to test whether an event is in a BPBB chunk:
      WITHIN(IMARK,ICHUNK) =
     &   IMARK .GE. ITABL(KBPBB,ICHUNK,JBPBFM) .AND.
     &   IMARK .LE. ITABL(KBPBB,ICHUNK,JBPBLM)
C-----------------------------------------------------------------------
C
C     First time through:
C
      IF (FIRST) THEN
        FIRST = .FALSE.
C
C     Read ALRP from DAF:
        LUNDB = JUNIDB(0)
        IRET = MDARD(IW,LUNDB,'ALRP',0)
C
C     Get name indices:
        NAALRP = NAMIND('ALRP')
        NAFVER = NAMIND('FVER')
        NARHAH = NAMIND('RHAH')
C
C     Data type (real=1, MC=2) from last call:
        ITYPE_OLD = 0
C
C Warning  for 1989/1990 data
         IF (IRUN.LE.NLR90) CALL QWMESE('_QFGET_BP_ Not '//
     +                            'executed for 1989/1990 data')
      ENDIF
C
C     Initialize chunk boundaries:
      IFRUN0 = 0
      IFEVT0 = 0
      ILRUN0 = 0
      ILEVT0 = 0
C
C     Initialize MC information:
      ISETUP0 = -1
      SIGMAX0 = -1.
C
C     Check for a change in data type since last call:
      LMC = (IRUN .LE. NRMC)
      IF (LMC) THEN
        ITYPE = 2
      ELSE
        ITYPE = 1
      ENDIF
      IF (ITYPE .NE. ITYPE_OLD) THEN
        ITYPE_OLD = ITYPE
C
C     If BPER/BPWT/BSIZ cards were given, we ignore the MC geometry
C     in setting the setup code.  IGNSET is the flag for this:
        IGNSET = .FALSE.
C
C     Setup code from last call:
        ISET_OLD = -999
C
      ENDIF
C
C ----------------------------------------------------------------------
C   ----------------------------
C     Handle Monte Carlo here:
C   ----------------------------
C
      IF (LMC) THEN
C
C     Load the parameters for a new setup.
C     We only need to do this once if BPER/BPWT/BSIZ are given:
        IF (.NOT. IGNSET) THEN
C
C     Get the BE setup code:
          ISET = GTSTUP('BE',IRUN)
C
C     Has the setup code changed?
          IF (ISET .NE. ISET_OLD) THEN
            ISET_OLD = ISET
            LDBOK = XFMCBP(NSETMX,IGNSET,NSET,JSET,SETWT,
     >                                    XMC_PAR,VTX_RES,VTX_SIZ)
            IF (.NOT. LDBOK) GO TO 999
C
          ELSE
C
C     This is not a new setup.
C     If we failed last time, don't do anything this time:
            IF (.NOT. LDBOK) GO TO 999
C
          ENDIF
        ENDIF
C
C     Calculate a "beam position" for this event by adding a random
C     offset to the true event primary vertex.
C
C     Get true primary vertex position:
        KFVER = IW(NAFVER)
        IF (KFVER .GT. 0) THEN
          DO ICOR=1,3
            VMC(ICOR) = RW(KROW(KFVER,1)+ICOR)
          ENDDO
        ELSE
          CALL VZERO(VMC,3)
        ENDIF
C
C     Set the random seed using the run and event number to
C     ensure reproducibility:
        ISEED = 1000000*(IRUN/10) + IEVT
        NTOTIN = MOD(IRUN,10)
        NTO2IN = 0
        CALL RMARIN(ISEED,NTOTIN,NTO2IN)
        CALL RANMAR(URAN,2)
        CALL RNORML(GRAN,5)
C
C     If we are considering more than one setup, throw the setup
C     number according to the desired weights:
        IISET = 1
        IF (NSET .GT. 1) THEN
          DO I=1,NSET
            IISET = I
            IF (URAN(1) .LT. SETWT(I)) GO TO 110
          ENDDO
 110      CONTINUE
        ENDIF
C
C     Set the width of the luminous region, sigma_x.  The sigma_x
C     distribution is parametrized as the sum of two Gaussians:
C       XMC_PAR(1,I) = fraction of total weight in first Gaussian,
C       XMC_PAR(2,I) = mean of first Gaussian,
C       XMC_PAR(3,I) = sigma of first Gaussian,
C       XMC_PAR(4,I) = mean of second Gaussian,
C       XMC_PAR(5,I) = sigma of second Gaussian.
C     First decide which Gaussian to use, then decide on the actual
C     width to use for this event:
        IF (URAN(2) .LE. XMC_PAR(1,IISET)) THEN
          IOFF = 1
        ELSE
          IOFF = 3
        ENDIF
        XMC_SIZ = XMC_PAR(IOFF+1,IISET) +
     >            XMC_PAR(IOFF+2,IISET)*GRAN(1)
        YMC_SIZ = VTX_SIZ(2,IISET)
C
C     Now generate the "beam position":
        VTX(1) = VMC(1) + XMC_SIZ*GRAN(2) + VTX_RES(1,IISET)*GRAN(3)
        VTX(2) = VMC(2) + YMC_SIZ*GRAN(4) + VTX_RES(2,IISET)*GRAN(5)
        VTX(3) = 0.
C
C     Beam position uncertainties:
        VTX_ERR(1) = VTX_RES(1,IISET)
        VTX_ERR(2) = VTX_RES(2,IISET)
        VTX_ERR(3) = 1.
C
C     Size of luminous region:
        VTX_SIZE(1) = VTX_SIZ(1,IISET)
        VTX_SIZE(2) = VTX_SIZ(2,IISET)
        VTX_SIZE(3) = VTX_SIZ(3,IISET)
C
C     Save the selected BE setup code and the generated sigma_x:
        IF (JSET(IISET) .GT. 0) THEN
          ISETUP0 = JSET(IISET)
          SIGMAX0 = XMC_SIZ
        ENDIF
C
        ICHUNK = 1
        GO TO 1000
C
C     End MC block
C
C-----------------------------------------------------------------------
C   --------------------------
C     Handle real data here:
C   --------------------------
C
      ELSE
C Do nothing for 1989/1990 data
         IF (IRUN.LE.NLR90) GO TO 999
C
C     Get the BE setup code:
        ISET = GTSTUP('BE',IRUN)
C
C     Link to ALRP:
        KALRP = IW(NAALRP)
        IF (KALRP .LE. 0) THEN
          CALL QWMESE('_QFGET_BP_ ALRP bank not found.')
          GO TO 999
        ENDIF
        NALRP = LROWS(KALRP)
C
C     Has the setup code changed?  If so, get the new luminous region
C     parameters:
        IF (ISET .NE. ISET_OLD) THEN
          ISET_OLD = ISET
          IRUN_OLD = 0
          ICHUNK = 1
C
C     Is there a row in ALRP for this setup?
          IF ((ISET .LT. 1) .OR. (ISET .GT. NALRP)) THEN
C
C     Get the name of the run period so we can print a useful
C     error message:
            IRET = ADBRUN('BE',IROW,IPERIOD,IRUN,JSETUP)
            IF (MOD(IPERIOD,100) .EQ. 0) IPERIOD = 1900 + IPERIOD/100
            WRITE (CNAM,'(I4)') IPERIOD
            CALL QWMESE('_QFGET_BP_ Luminous region parameters'//
     >                ' not available for run period '//CNAM)
            LDBOK = .FALSE.
            GO TO 999
          ENDIF
C
C     Report the new run period:
          IPERIOD = ITABL(KALRP,ISET,JALRPN)
          WRITE (CNAM,'(I4)') IPERIOD
          CALL QWMESS('_QFGET_BP_ Using luminous region parameters'//
     >                ' for run period '//CNAM)
          LDBOK = .TRUE.
C
        ELSE
C
C     This is not a new setup.
C     If we failed last time, don't do anything this time:
          IF (.NOT. LDBOK) GO TO 999
C
        ENDIF
C
C     Decide where the beam position and uncertainty will come from.
C     JULIA version 275.03 or later: from run header bank ALPB;
C     otherwise from 'BPBB' bank, read from beam.position file.
C
C     Get the JULIA version/correction number:
        IF (IRUN .NE. IRUN_OLD) THEN
          IJCOR = 0
          IF (NARHAH .EQ. 0) NARHAH = NAMIND('RHAH')
          KRHAH = IW(NARHAH)
          NRHAH = 0
          IF (KRHAH .GT. 0) NRHAH = LROWS(KRHAH)
          DO IRHAH=NRHAH,1,-1
            PRNAM(1:4) = CHAINT(ITABL(KRHAH,IRHAH,JRHAPN))
            PRNAM(5:8) = CHAINT(ITABL(KRHAH,IRHAH,JRHAPN+1))
            IF (PRNAM .EQ. 'JULIA') THEN
              IJCOR = ITABL(KRHAH,IRHAH,JRHACV)
              GO TO 210
            ENDIF
          ENDDO
C
C     Can't get JULIA version?  Don't worry, this might be RAW data.
          GO TO 999
C
 210      CONTINUE
        ENDIF
        USEALPB = (IJCOR .GE. 27503)
C
C     Get information from ALPB bank, NR=IRUN:
C
        IF (USEALPB) THEN
          KALPB = NLINK('ALPB',IRUN)
          IF (KALPB .LE. 0) GO TO 999
          NCHUNK = LROWS(KALPB)
C
C     Check whether the run number changed:
          IF (IRUN .NE. IRUN_OLD) ICHUNK = 1
C
C     Loop over rows of ALPB, starting where we left off in the
C     previous call:
          DO JCHUNK=MAX0(ICHUNK,1),NCHUNK
            MINEV = ITABL(KALPB,JCHUNK,JALPFE)
            MAXRO = MIN0(JCHUNK+1,NCHUNK)
            MAXEV = ITABL(KALPB,MAXRO,JALPFE)
            IF (((IEVT .GE. MINEV) .AND. (IEVT .LT. MAXEV)) .OR.
     >          ((JCHUNK .EQ. NCHUNK) .AND. (IEVT.GE.MAXEV))) THEN
              ICHUNK = JCHUNK
              GO TO 310
            ENDIF
          ENDDO
          GO TO 999
C
C     If the beam position uncertainty is zero, the beam position
C     fit failed:
 310      CONTINUE
          IF (ITABL(KALPB,ICHUNK,JALPXE) .EQ. 0) GO TO 999
C
C     Beam position:
          VTX(1) = FLOAT(ITABL(KALPB,ICHUNK,JALPXP))*PFAC
          VTX(2) = FLOAT(ITABL(KALPB,ICHUNK,JALPYP))*PFAC
          VTX(3) = 0.
C
C     Beam position uncertainties:
          VTX_ERR(1) = FLOAT(ITABL(KALPB,ICHUNK,JALPXE))*EFAC
          VTX_ERR(2) = FLOAT(ITABL(KALPB,ICHUNK,JALPYE))*EFAC
          VTX_ERR(3) = 1.
C
C     Size of luminous region:
          VTX_SIZE(1) = RTABL(KALRP,ISET,JALRXS)
          VTX_SIZE(2) = RTABL(KALRP,ISET,JALRYS)
          VTX_SIZE(3) = RTABL(KALRP,ISET,JALRZS)
C
C     First and last events of chunk:
          IFRUN0 = IRUN
          IFEVT0 = ITABL(KALPB,ICHUNK,JALPFE)
          ILRUN0 = IRUN
          IF (ICHUNK .LT. NCHUNK) THEN
            ILEVT0 = ITABL(KALPB,ICHUNK+1,JALPFE) - 1
          ELSE
            ILEVT0 = 999999
          ENDIF
C
C     Get information from BPBB bank, NR=ISET:
C
        ELSE
          KBPBB = NLINK('BPBB',ISET)
          IF (KBPBB .LE. 0) GO TO 999
          NCHUNK = LROWS(KBPBB)
C
C     Loop over rows of BPBB, starting where we left off in the
C     previous call:
          IMARK = ISHFT(IRUN,16) + IEVT
          DO JCHUNK=MAX(1,ICHUNK),NCHUNK
            IF (WITHIN(IMARK,JCHUNK)) THEN
               ICHUNK = JCHUNK
               GO TO 410
            ENDIF
          ENDDO
C
C     Maybe the runs were delivered out of order; search from the
C     beginning just to be sure:
          DO JCHUNK=1,MIN(ICHUNK,NCHUNK)
            IF (WITHIN(IMARK,JCHUNK)) THEN
               ICHUNK = JCHUNK
               GO TO 410
            ENDIF
          ENDDO
C
C     There is no information for this run/event:
          GO TO 999
C
C     Beam position:
 410      CONTINUE
          VTX(1) = FLOAT(ISHFT(ITABL(KBPBB,ICHUNK,JBPBXW),-16))*PFAC
     >                                      - 1.
          VTX(2) = FLOAT(ISHFT(ITABL(KBPBB,ICHUNK,JBPBYW),-16))*PFAC
     >                                      - 1.
          VTX(3) = 0.
C
C     Beam position uncertainties:
          VTX_ERR(1) = FLOAT(IAND(ITABL(KBPBB,ICHUNK,JBPBXW),65535))
     >                                      * EFAC
          VTX_ERR(2) = FLOAT(IAND(ITABL(KBPBB,ICHUNK,JBPBYW),65535))
     >                                      * EFAC
          VTX_ERR(3) = 1.
C
C     Size of luminous region:
          VTX_SIZE(1) = RTABL(KALRP,ISET,JALRXS)
          VTX_SIZE(2) = RTABL(KALRP,ISET,JALRYS)
          VTX_SIZE(3) = RTABL(KALRP,ISET,JALRZS)
C
C     First and last events of chunk:
          IFRUN0 = ISHFT(ITABL(KBPBB,ICHUNK,JBPBFM),-16)
          IFEVT0 = IAND(ITABL(KBPBB,ICHUNK,JBPBFM),65535)
          ILRUN0 = ISHFT(ITABL(KBPBB,ICHUNK,JBPBLM),-16)
          ILEVT0 = IAND(ITABL(KBPBB,ICHUNK,JBPBLM),65535)
C
        ENDIF
        GO TO 1000
C
      ENDIF
C
 999  CONTINUE
      ICHUNK = -1
C
 1000 CONTINUE
      IRUN_OLD = IRUN
      RETURN
C
C ----------------------------------------------------------------------
C
      ENTRY QFGET_CH (IFRUN, IFEVT, ILRUN, ILEVT)
C
      IFRUN = IFRUN0
      IFEVT = IFEVT0
      ILRUN = ILRUN0
      ILEVT = ILEVT0
C
      RETURN
C
C ----------------------------------------------------------------------
C
      ENTRY QFMCSX (ISETUP, SIGMAX)
C
      ISETUP = ISETUP0
      SIGMAX = SIGMAX0
C
      RETURN
      END
