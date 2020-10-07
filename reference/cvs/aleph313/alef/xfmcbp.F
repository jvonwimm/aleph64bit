      LOGICAL FUNCTION XFMCBP (NSETMX, IGNSET, NSET, JSET, SETWT,
     >                                  XMC_PAR, VTX_RES, VTX_SIZ)
C ----------------------------------------------------------------------
CKEY ALEF BEAMSPOT              
C!  Handle new MC setup for chunk-by-chunk beam position and size
C - Steve Wasserbaech, 28 August 1995
C   Original version (GET_BP): Dave Brown  9-1-92
C   Adapted for the ALEPHLIB : J. Boucrot 23-9-98
C
C - Input:
C   NSETMX        / I  Dimension of arrays JSET, SETWT, XMC_PAR,
C                       VTX_RES, and VTX_SIZ in calling program
C
C   Output:
C   XFMCBP        / L  =.TRUE. if successful, =.FALSE. if error
C   IGNSET        / L  =.TRUE. if we are ignoring the MC geometry setup
C                       because BPER/BPWT/BSIZ card(s) were given
C   NSET          / I  Number of setups specified on BPER card
C   JSET(*)       / I  Row numbers in ALRP of selected setups
C   SETWT(*)      / R  Integrated weights for generating setups;
C                       the fraction of events in which setup JSET(I)
C                       is generated is SETWT(I) - SETWT(I-1).
C
C   XMC_PAR(5,*)  / R  Set of parameters for generating sigma_x.
C                       The distribution of sigma_x is parametrized
C                       as the sum of two Gaussians.  First index =
C                        1 for fraction of total area in first Gaussian,
C                        2 for mean of first Gaussian (cm),
C                        3 for sigma of first Gaussian (cm),
C                        4 for mean of second Gaussian (cm),
C                        5 for sigma of second Gaussian (cm);
C                       the second index runs from 1 to NSET.
C
C   VTX_RES(3,*)  / R  Beam position uncertainty (cm).  First index =
C                       1,2,3 for x,y,z; second index runs from 1 to
C                       NSET.  Note: VTX_RES(3,I) = 1 always.
C
C   VTX_SIZ(3,*)  / R  Average rms size of luminous region (cm).
C                       First index = 1,2,3 for x,y,z; second index
C                       runs from 1 to NSET.
C-----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE FIRST, NAALRP, NAASIM, NABPER, NABPWT, NABSIZ, NARUNH
#include "bcs.h"
#include "alrpjj.h"
#include "asimjj.h"
#include "runhjj.h"
#include "bmacrod.h"
C
C     Arguments:
      INTEGER NSETMX, NSET, JSET(*)
      REAL SETWT(*), XMC_PAR(5,*), VTX_RES(3,*), VTX_SIZ(3,*)
      LOGICAL IGNSET
C
C     Parameters:
      INTEGER NMX
      PARAMETER (NMX=25)
C
C     Local variables declaration:
      INTEGER NMAX, JSETOK(NMX), ISETUP, IPERIOD
      INTEGER I, J, IPER, IWT(NMX), ISET, ITOT ,LOU
      INTEGER NAALRP, KALRP, NALRP, IALRP
      INTEGER NAASIM, KASIM
      INTEGER NABPER, KBPER, NBPER, IBPER
      INTEGER NABPWT, KBPWT, NBPWT
      INTEGER NABSIZ, KBSIZ, NBSIZ
      INTEGER NARUNH, KRUNH
      REAL SETWT0(NMX)
      LOGICAL FIRST, LBPER, LBPWT, LBSIZ, BPEROK, LUSED(NMX), LEP2
      LOGICAL ERROR
      CHARACTER*4 CNAM
C
C     Function types:
      INTEGER NAMIND, GTSTUP
C
C     Data statements:
      DATA FIRST / .TRUE. /
C
#include "bmacro.h"
C
C-----------------------------------------------------------------------
C
      XFMCBP = .FALSE.
      NMAX = MIN0(NSETMX,NMX)
      LOU = IW(6)
C
C     First time through--get name indices:
      IF (FIRST) THEN
        FIRST = .FALSE.
        NAALRP = NAMIND('ALRP')
        NAASIM = NAMIND('ASIM')
        NABPER = NAMIND('BPER')
        NABPWT = NAMIND('BPWT')
        NABSIZ = NAMIND('BSIZ')
        NARUNH = NAMIND('RUNH')
      ENDIF
C
C     Initialize outputs:
      IGNSET = .FALSE.
      NSET = 0
      CALL VZERO(JSET,NSETMX)
      CALL VZERO(SETWT,NSETMX)
      CALL VZERO(XMC_PAR,5*NSETMX)
      CALL VZERO(VTX_RES,3*NSETMX)
      CALL VZERO(VTX_SIZ,3*NSETMX)
C
C     Link to ALRP:
      KALRP = IW(NAALRP)
      IF (KALRP .LE. 0) THEN
         WRITE (LOU,'(/1X,A)')               
     >   '_XFMCBP_ ALRP bank not found.'
         GO TO 1000
      ENDIF
      NALRP = LROWS(KALRP)
C
C     Look for control cards:
      KBPER = IW(NABPER)
      KBPWT = IW(NABPWT)
      KBSIZ = IW(NABSIZ)
      LBPER = (KBPER .GT. 0)
      LBPWT = (KBPWT .GT. 0)
      LBSIZ = (KBSIZ .GT. 0)
      NBPER = 0
      NBPWT = 0
      NBSIZ = 0
      IF (LBPER) NBPER = IW(KBPER)
      IF (LBPWT) NBPWT = IW(KBPWT)
      IF (LBSIZ) NBSIZ = IW(KBSIZ)
C
C ----------------------------------------------------------------------
C
C     Now decide which setups will be simulated.
C
C     If (LBSIZ) Then
C       Use parameters from BSIZ
C     Elseif (LBPER .OR. LBPWT) Then
C       Use setups listed on BPER
C     Else
C       Get the setup from the MC geometry
C     Endif
C
C
C     BSIZ card:
C
      IF (LBSIZ) THEN
        IF (LBPER) WRITE (LOU,'(/1X,A)')
     >     '_XFMCBP_ BSIZ card found; BPER card ignored.'
        IF (LBPWT) WRITE (LOU,'(/1X,A)')
     >     '_XFMCBP_ BSIZ card found; BPWT card ignored.'
        IF ((NBSIZ .NE. 2) .AND. (NBSIZ .NE. 3)) THEN
          WRITE (LOU,'(/1X,A)')
     >    '_XFMCBP_ **FATAL** Invalid syntax on BSIZ card.'
          GO TO 2000
        ENDIF
C
        IGNSET = .TRUE.
        NSET = 1
        JSET(NSET) = -1
        SETWT(NSET) = 1.
        XMC_PAR(1,NSET) = 1.
        XMC_PAR(2,NSET) = RW(KBSIZ+1)
        XMC_PAR(3,NSET) = 0.
        XMC_PAR(4,NSET) = 0.
        XMC_PAR(5,NSET) = 0.
        VTX_RES(1,NSET) = 0.
        VTX_RES(2,NSET) = 0.
        VTX_RES(3,NSET) = 1.
        VTX_SIZ(1,NSET) = RW(KBSIZ+1)
        VTX_SIZ(2,NSET) = RW(KBSIZ+2)
        IF (NBSIZ .EQ. 3) THEN
          VTX_SIZ(3,NSET) = RW(KBSIZ+3)
        ELSE
          VTX_SIZ(3,NSET) = 1.
        ENDIF
C
        WRITE (LOU  ,'(1X,A,2F8.4)')
     >      '_XFMCBP_ Generating beam positions with sigma_x,y =',
     >      VTX_SIZ(1,NSET), VTX_SIZ(2,NSET)
C
C ----------------------------------------------------------------------
C
C     BPER/BPWT cards:
C
      ELSEIF (LBPER .OR. LBPWT) THEN
C
C     Build a list of valid run periods.
C     ISETUP = BE setup code from ADBR = row index in ALRP
C     JSETOK(ISETUP) = first run period YYMM for BE setup ISETUP
C     (The period names in ALRP can be of the form YYMM or 19YY.)
        IF (NALRP .GT. NMAX) THEN
          WRITE (LOU,'(/1X,A)')
     >    '_XFMCBP_ **FATAL** Too many periods--increase NSETMX, NMX.'
          GO TO 2000
        ENDIF
        DO IALRP=1,NALRP
          JSETOK(IALRP) = ITABL(KALRP,IALRP,JALRPN)
          IF (JSETOK(IALRP)/100 .EQ. 19)
     >                    JSETOK(IALRP) = 100*MOD(JSETOK(IALRP),100)
          LUSED(IALRP) = .FALSE.
        ENDDO
C
C     Loop over tokens on card.  BPEROK will be .FALSE. if there are
C     invalid tokens or no tokens at all:
        IF (LBPER) THEN
          BPEROK = (NBPER .GT. 0)
          DO IBPER=1,NBPER
            IPERIOD = IW(KBPER+IBPER)
C
C     Convert to the format used in ADBR:
            IF (IPERIOD .LT. 100) THEN
              IPER = 100*IPERIOD
            ELSEIF (IPERIOD/100 .EQ. 19) THEN
              IPER = 100*MOD(IPERIOD,100)
            ELSE
              IPER = IPERIOD
            ENDIF
C
C     Is it a valid period number?  Make a list of the good ones.
C     JSET(IBPER) = row index in ALRP of the IBPERth selected setup;
C     IBPER = 1,2,...,NBPER.  Duplicated run periods are not allowed.
C
            DO I=1,NALRP
              IF ((IPER .GT. 0) .AND. (IPER .EQ. JSETOK(I))) THEN
                IF (.NOT. LUSED(I)) THEN
                  LUSED(I) = .TRUE.
                  JSET(IBPER) = I
                  GO TO 120
                ELSE
                  GO TO 110
                ENDIF
              ENDIF
            ENDDO
 110        CONTINUE
            BPEROK = .FALSE.
            WRITE (LOU  ,'(1X,2A,I8)')
     >           '_XFMCBP_ Invalid/duplicate run period request ',
     >           'on BPER card:', IPERIOD
 120        CONTINUE
          ENDDO
C
C     Print the valid run periods if appropriate:
          IF (.NOT. BPEROK) THEN
            WRITE (LOU  ,'(/1X,A)') 'Valid run periods for BPER card:'
            WRITE (LOU  ,'(1X,20I5)') (JSETOK(I), I=1,NALRP)
            GO TO 2000
          ENDIF
C
C     BPWT with no BPER is invalid:
        ELSE
          WRITE (LOU,'(/1X,A)')
     >    '_XFMCBP_ **FATAL** BPWT card must be accompanied'//
     >    ' by a BPER card.'
          GO TO 2000
        ENDIF
C
C     BPER and BPWT must have equal numbers of tokens:
        IF (LBPWT) THEN
          IF (NBPER .NE. NBPWT) THEN
            WRITE (LOU,'(/1X,A)')
     >      '_XFMCBP_ **FATAL** BPER and BPWT cards '//
     >      'must have equal numbers of tokens.'
            GO TO 2000
          ENDIF
        ENDIF
C
C     Get the weights of the run periods:
        LEP2 = .FALSE.
        ERROR = .FALSE.
        ITOT = 0
        DO I=1,NBPER
          IF (LBPWT) THEN
            IWT(I) = IW(KBPWT+I)
          ELSE
            IWT(I) = ITABL(KALRP,JSET(I),JALRWE)
          ENDIF
          ERROR = (ERROR .OR. (IWT(I) .LT. 0))
          LEP2 = (LEP2 .OR. (ITABL(KALRP,JSET(I),JALRER) .NE. 1))
          ITOT = ITOT + IWT(I)
        ENDDO
C
C     If LEP2 periods are involved and NBPER > 1, BPWT must be given:
        IF (LEP2 .AND. (NBPER .GT. 1) .AND. (.NOT. LBPWT)) THEN
          WRITE (LOU,'(/1X,A)')
     >    '_XFMCBP_ **FATAL** Give a BPWT card to set '//
     >    'the weights of the run periods.'
          GO TO 2000
        ENDIF
C
C     Watch out for crazy weights:
        ERROR = (ERROR .OR. ((NBPER .GT. 1) .AND. (ITOT .LE. 0)))
        IF (ERROR) THEN
          WRITE (LOU,'(/1X,A)')
     >    '_XFMCBP_ **FATAL** Invalid run period weights.'
          GO TO 2000
        ENDIF
C
C     Valid BPER/BPWT cards!
        IGNSET = .TRUE.
        NSET = NBPER
C
C     Normalize and integrate the weights:
        IF (NSET .EQ. 1) THEN
          SETWT(NSET) = 1.
        ELSE
          DO I=1,NSET
            SETWT0(I) = FLOAT(IWT(I)) / FLOAT(ITOT)
          ENDDO
          SETWT(1) = SETWT0(1)
          DO I=2,NSET
            SETWT(I) = SETWT0(I-1) + SETWT0(I)
          ENDDO
        ENDIF
C
C     Get the other parameters:
        DO I=1,NSET
          XMC_PAR(1,I) = RTABL(KALRP,JSET(I),JALRF1)
          XMC_PAR(2,I) = RTABL(KALRP,JSET(I),JALRM1)
          XMC_PAR(3,I) = RTABL(KALRP,JSET(I),JALRS1)
          XMC_PAR(4,I) = RTABL(KALRP,JSET(I),JALRM2)
          XMC_PAR(5,I) = RTABL(KALRP,JSET(I),JALRS2)
          VTX_RES(1,I) = RTABL(KALRP,JSET(I),JALRXR)
          VTX_RES(2,I) = RTABL(KALRP,JSET(I),JALRYR)
          VTX_RES(3,I) = 1.
          VTX_SIZ(1,I) = RTABL(KALRP,JSET(I),JALRXS)
          VTX_SIZ(2,I) = RTABL(KALRP,JSET(I),JALRYS)
          VTX_SIZ(3,I) = RTABL(KALRP,JSET(I),JALRZS)
        ENDDO
C
C     Print the parameters:
        IF (NSET .EQ. 1) THEN
          WRITE (CNAM,'(I4)') ITABL(KALRP,JSET(NSET),JALRPN)
          WRITE (LOU,'(/1X,A)')
     >    '_XFMCBP_ Using luminous region parameters'//
     >    ' for run period '//CNAM
          WRITE (LOU,'(1X,A,3F8.4)') '_XFMCBP_ Average sigma_x,y,z =',
     >                              (VTX_SIZ(J,NSET), J=1,3)
C
        ELSE
          WRITE (LOU,'(/1X,A)')
     >    '_XFMCBP_ Luminous region parameters:'
          WRITE (LOU,'(1X,A)')
     >    '         Period   Weight   Average sigma_x,y,z (cm)'
          DO I=1,NSET
            WRITE (LOU  ,'(1X,I13,F10.3,F10.4,F9.4,F7.2)')
     >                    ITABL(KALRP,JSET(I),JALRPN), SETWT0(I),
     >                              (VTX_SIZ(J,I), J=1,3)
          ENDDO
        ENDIF
C
C ----------------------------------------------------------------------
C
C     No BPER/BPWT/BSIZ cards; use the MC geometry run period and
C     the corresponding BE setup code to determine the luminous
C     region parameters:
C
      ELSE
C
C     Is there a row in ALRP for this setup?
        ISET = GTSTUP('BE',1)
        IF ((ISET .LT. 1) .OR. (ISET .GT. NALRP)) THEN
C
C     Get the name of the run period so we can print a useful
C     error message:
          IPERIOD = 0
          KASIM = IW(NAASIM)
          IF (KASIM .GT. 0) THEN
            IPERIOD = ITABL(KASIM,1,JASIYM)
          ELSE
            KRUNH = IW(NARUNH)
            IF (KRUNH .GT. 0) IPERIOD = IW(KRUNH+JRUNSD)/100
          ENDIF
          WRITE (CNAM,'(I4)') IPERIOD
          WRITE (LOU,'(/1X,A)')
     >    '_XFMCBP_ Luminous region parameters'//
     >    ' not available for run period '//CNAM
          GO TO 1000
        ENDIF
C
C     Get the parameters for this one period:
        IGNSET = .FALSE.
        NSET = 1
        JSET(NSET) = ISET
        SETWT(NSET) = 1.
        XMC_PAR(1,NSET) = RTABL(KALRP,ISET,JALRF1)
        XMC_PAR(2,NSET) = RTABL(KALRP,ISET,JALRM1)
        XMC_PAR(3,NSET) = RTABL(KALRP,ISET,JALRS1)
        XMC_PAR(4,NSET) = RTABL(KALRP,ISET,JALRM2)
        XMC_PAR(5,NSET) = RTABL(KALRP,ISET,JALRS2)
        VTX_RES(1,NSET) = RTABL(KALRP,ISET,JALRXR)
        VTX_RES(2,NSET) = RTABL(KALRP,ISET,JALRYR)
        VTX_RES(3,NSET) = 1.
        VTX_SIZ(1,NSET) = RTABL(KALRP,ISET,JALRXS)
        VTX_SIZ(2,NSET) = RTABL(KALRP,ISET,JALRYS)
        VTX_SIZ(3,NSET) = RTABL(KALRP,ISET,JALRZS)
C
C     Get the name of the first run period having this setup:
        WRITE (CNAM,'(I4)') ITABL(KALRP,ISET,JALRPN)
        WRITE (LOU,'(/1X,A)')
     >  '_XFMCBP_ Using luminous region parameters'//
     >  ' for run period '//CNAM
        WRITE (LOU  ,'(1X,A,3F8.4)') '_XFMCBP_ Average sigma_x,y,z =',
     >                              (VTX_SIZ(J,NSET), J=1,3)
C
      ENDIF
C
      XFMCBP = .TRUE.
C
 1000 CONTINUE
      GO TO 999
C Fatal error in data cards, job cannot run :
 2000 STOP
  999 RETURN
      END
#endif



