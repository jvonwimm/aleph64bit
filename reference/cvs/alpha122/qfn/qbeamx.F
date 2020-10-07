      SUBROUTINE QBEAMX (IRUN, IEVT, IRET, SIGX)
C-----------------------------------------------------------------------
CKEY BEAMSPOT / USER
C!  Metachunk size of luminous region (x direction)
C - Steve Wasserbaech, 12 September 1995
C
C   The Alpha array QVTSBP (filled by Qfget_bp) provides an average
C   horizontal beam size sigma_x for each year. QBEAMX is designed
C   to provide run/event-dependent sigma_x values for the real data
C   and to provide corresponding information for Monte Carlo.
C
C   For real data:
C   The measured value of SIGX is taken from the WIDE bank.  A bias
C   correction (from VBWP) is applied.
C
C   For Monte Carlo:
C   The value of sigma_x thrown in Qfget_bp is recovered by means of
C   the entry QFMCSX.  NEVE, the number of events in the "metachunk,"
C   is thrown according to the distribution in the real data, as found
C   in the WIDN bank.  The value of sigma_x is smeared according to
C   the resolution (as a function of NEVE, with parameters in VBWP)
C   and the result is returned in SIGX.
C
C - Input:
C   IRUN  / I  Run number
C   IEVT  / I  Event number
C
C - Output:
C   IRET  / I  Return code
C               =0  OK
C               =1  Error reading ALRP or VBWP from database
C               =2  GET_BP information not available
C                     for this event (XGETBP = .FALSE.)
C               =3  BSIZ in use or 1989/1990 MC file
C               =4  Setup not found in ALRP
C               =5  Invalid LEP era number in ALRP
C               =6  WIDN bank not found
C               =7  WIDE bank not found
C               =8  No WIDE information for this event
C
C   SIGX  / R  sigma_x of luminous region (cm)
C-----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE FIRST, MESS, ICHUNK, NAALRP, NAVBWP
#include "qdecl.h"
#include "qcde.h"
#include "alrpjj.h"
#include "vbwpjj.h"
#include "widejj.h"
#include "widnjj.h"
C
C     Arguments:
      INTEGER IRUN, IEVT, IRET
      REAL SIGX
C
C     Parameters:
      INTEGER NRMC, M9, M13, M18, M22
      REAL SXUNIT
      PARAMETER (NRMC=2000)
      PARAMETER (M9=511)
      PARAMETER (M13=8191)
      PARAMETER (M18=262143)
      PARAMETER (M22=4194303)
      PARAMETER (SXUNIT=0.00001)
C
C     Local variable declarations:
      INTEGER JRUN, JEVT, JCHUNK, IRUN1, IEVT1, IRUN2, IEVT2
      INTEGER ISET, ICHUNK, LUNDB, IRC, IERA, NB, NEVEMX
      INTEGER ISEED, NTOTIN, NTO2IN, I, INDEX
      INTEGER NAALRP, KALRP, NALRP
      INTEGER NAVBWP, KVBWP, NVBWP
      INTEGER KWIDN, KWIDE, NWIDE
      REAL SXGEN, C1, C2, URAN(2), GRAN(5), XNEVE, RESOL, BIAS
      LOGICAL FIRST, MESS
      DATA FIRST / .TRUE. /
      DATA MESS / .FALSE. /
C
C     Functions:
      INTEGER JUNIDB, MDARD, NAMIND, GTSTUP, NLINK, ISHFT
      INTEGER LRUN1, LEVT1, LRUN2, LEVT2
      REAL SIGMAX
      LOGICAL WITHIN, EVGEQ
C
#include "qmacro.h"
C-------------------------------------------------------------------------------
C     An inline function to test whether IRUN1/IEVT1 is greater than
C     or equal to IRUN2/IEVT2:
      EVGEQ(IRUN1,IEVT1,IRUN2,IEVT2) = ((IRUN1 .GT. IRUN2) .OR.
     >          ((IRUN1 .EQ. IRUN2) .AND. (IEVT1 .GE. IEVT2)))
C
C     Inline functions to return the run/event number of the first
C     and last event in a metachunk:
      LRUN1(JCHUNK) = IAND(ISHFT(ITABL(KWIDE,JCHUNK,JWIDW1),-13),M18)
      LEVT1(JCHUNK) = IAND(ISHFT(ITABL(KWIDE,JCHUNK,JWIDW2),-9),M22)
      LRUN2(JCHUNK) = ISHFT(IAND(ITABL(KWIDE,JCHUNK,JWIDW2),M9),9) +
     >                IAND(ISHFT(ITABL(KWIDE,JCHUNK,JWIDW3),-22),M9)
      LEVT2(JCHUNK) = IAND(ITABL(KWIDE,JCHUNK,JWIDW3),M22)
C
C     An inline function to test whether an event is in a
C     WIDE metachunk:
      WITHIN(JRUN,JEVT,JCHUNK) =
     >          EVGEQ(JRUN,JEVT,LRUN1(JCHUNK),LEVT1(JCHUNK)) .AND.
     >          EVGEQ(LRUN2(JCHUNK),LEVT2(JCHUNK),JRUN,JEVT)
C
C     An inline function to return sigma_x for a metachunk:
      SIGMAX(JCHUNK) = SXUNIT *
     >                 FLOAT(IAND(ITABL(KWIDE,JCHUNK,JWIDW1),M13))
C
C ----------------------------------------------------------------------
C
C     Initialization:
      SIGX = -1.
      IF (FIRST) THEN
        FIRST = .FALSE.
        ICHUNK = 0
C
C     Read ALRP and VBWP from the database:
        LUNDB = JUNIDB(0)
        IRC = MDARD(IW,LUNDB,'ALRP',0)
        IRC = MDARD(IW,LUNDB,'VBWP',0)
        NAALRP = NAMIND('ALRP')
        NAVBWP = NAMIND('VBWP')
      ENDIF
C
C     Link to ALRP and VBWP:
      KALRP = IW(NAALRP)
      KVBWP = IW(NAVBWP)
      IF ((KALRP .LE. 0) .OR. (KVBWP .LE. 0)) THEN
        IRET = 1
        GO TO 1000
      ENDIF
      NALRP = LROWS(KALRP)
      NVBWP = LROWS(KVBWP)
C
C     We can't do anything if QFGET_BP didn't work:
      IF (.NOT. XGETBP) THEN
        IRET = 2
        GO TO 1000
      ENDIF
C
C ----------------------------------------------------------------------
C
C     Monte Carlo:
C
      IF (IRUN .LE. NRMC) THEN
C
C     Find out which setup was used for this event:
        CALL QFMCSX(ISET,SXGEN)
        IF (ISET .LT. 1) THEN
          IRET = 3
          GO TO 1000
        ENDIF
C
C     Get the LEP era number (= row number in VBWP):
        IF (ISET .LE. NALRP) THEN
          IERA = ITABL(KALRP,ISET,JALRER)
        ELSE
          IRET = 4
          GO TO 1000
        ENDIF
        IF ((IERA .LT. 1) .OR. (IERA .GT. NVBWP)) THEN
          IRET = 5
          GO TO 1000
        ENDIF
C
C     Get the resolution parameters from VBWP:
        C1 = RTABL(KVBWP,IERA,JVBWC1)
        C2 = RTABL(KVBWP,IERA,JVBWC2)
C
C     Link to WIDN (the NEVE distribution):
        KWIDN = NLINK('WIDN',ISET)
        IF (KWIDN .LE. 0) THEN
          IF (.NOT. MESS) THEN
            MESS = .TRUE.
            WRITE (IW(6),'(A/A,I3,A)')
     >      ' _QBEAMX_ Luminous region size information not available',
     >      '          for this run period (BE setup =', ISET, ')'
          ENDIF
          IRET = 6
          GO TO 1000
        ENDIF
        NB = ITABL(KWIDN,1,JWIDNB)
        NEVEMX = ITABL(KWIDN,1,JWIDNM)
C
C     Set the random seed as in QFGET_BP, using the run and event
C     number to ensure reproducibility:
        ISEED = 1000000*(IRUN/10) + IEVT
        NTOTIN = MOD(IRUN,10)
        NTO2IN = 0
        CALL RMARIN(ISEED,NTOTIN,NTO2IN)
C
C     Generate the same random numbers as in QFGET_BP:
        CALL RANMAR(URAN,2)
        CALL RNORML(GRAN,5)
C
C     Now we can get some fresh random numbers:
        CALL RANMAR(URAN,1)
        CALL RNORML(GRAN,1)
C
C     Now throw NEVE, based on the integral distribution in WIDN:
        DO I=1,NB
          INDEX = I
          IF (RTABL(KWIDN,1,JWIDIN-1+I) .GE. URAN(1)) GO TO 100
        ENDDO
 100    CONTINUE
C
C     If INDEX = 1, we pretend that the metachunk was too small for
C     the beam size measurement to be made, and we return the average
C     sigma_x for the year:
        IF (INDEX .EQ. 1) THEN
          SIGX = QVTSBP(1)
        ELSE
          XNEVE = (FLOAT(INDEX)-0.5)*FLOAT(NEVEMX)/FLOAT(NB) + 0.5
          RESOL = C1 * SQRT((SXGEN**2 + C2**2) / XNEVE)
          SIGX = AMAX1(0., SXGEN + RESOL*GRAN(1))
        ENDIF
C
C ----------------------------------------------------------------------
C
C     Real data:
C
      ELSE
C
C     Get setup code and link to WIDE (the measured SIGX values):
        ISET = GTSTUP('BE',IRUN)
        KWIDE = NLINK('WIDE',ISET)
        IF (KWIDE .LE. 0) THEN
          IF (.NOT. MESS) THEN
            MESS = .TRUE.
            WRITE (IW(6),'(A,I3,A)') ' _QBEAMX_ WIDE NR=', ISET,
     >                     ' not found.  Have you read wide.cards?'
          ENDIF
          IRET = 7
          GO TO 1000
        ENDIF
        NWIDE = LROWS(KWIDE)
C
C     Now look up this event in WIDE:
        IF ((ICHUNK .GT. 0) .AND. WITHIN(IRUN,IEVT,ICHUNK)) THEN
C
C     We've got the right chunk.
          GO TO 200
C
        ELSE
C
C     Search forwards; the list is ordered, so if the events/runs
C     are in order, what we want is just the next chunk.
          DO JCHUNK=MAX(1,ICHUNK),NWIDE
            IF (WITHIN(IRUN,IEVT,JCHUNK)) THEN
              ICHUNK = JCHUNK
              GO TO 200
            ENDIF
          ENDDO
C
C     Maybe the runs were delivered out of order;
C     search from the begining just to be sure:
          DO JCHUNK=1,MIN(ICHUNK,NWIDE)
            IF (WITHIN(IRUN,IEVT,JCHUNK)) THEN
              ICHUNK = JCHUNK
              GO TO 200
            ENDIF
          ENDDO
        ENDIF
C
C     If we get to here, there's no information for this run/event:
        IRET = 8
        GO TO 1000
C
C     Get the measured sigma_x:
 200    CONTINUE
        SIGX = SIGMAX(ICHUNK)
C
C     If SIGX = 0 then no measurement is available; we return
C     the average sigma_x for the year:
        IF (SIGX .EQ. 0.) THEN
C
          SIGX = QVTSBP(1)
C
        ELSE
C
C     Get the LEP era number (= row number in VBWP):
          IF (ISET .LE. NALRP) THEN
            IERA = ITABL(KALRP,ISET,JALRER)
          ELSE
            IRET = 4
            GO TO 1000
          ENDIF
          IF ((IERA .LT. 1) .OR. (IERA .GT. NVBWP)) THEN
            IRET = 5
            GO TO 1000
          ENDIF
C
C     Get the bias from VBWP and apply a correction to SIGX:
          BIAS = RTABL(KVBWP,IERA,JVBWBI)
          SIGX = SIGX - BIAS
C
        ENDIF
C
      ENDIF
      IRET = 0
C
 1000 CONTINUE
      RETURN
      END
#endif
