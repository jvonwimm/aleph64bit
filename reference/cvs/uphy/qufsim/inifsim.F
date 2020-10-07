      SUBROUTINE INIFSIM(IER)
C-----------------------------------------------------------------------
C! Initialize FSIM
C
C  G.Ganis  21 Mar 1991
C
C Modification : o P. Janot -- 10 Oct 1993. Cards RESO/ACCE/CRAK for
C                  the detector acceptance and performances
C                o P. Janot -- 12 Mar 1994. Add optional cards to
C                  work with no endcaps (NOEN) and no HCAL (NOHC)
C                  together with the number of interaction lengths
C                  of the ECAL only.
C                o M. Kado  -- 13 May 1998. Add cards for neutral 
C                  hadronic energy deposition (NEUT)
C-----------------------------------------------------------------------
#include "qcde.h"
#include "qhac.h"
#include "resolu.h"
#include "fkclstr.h"
#include "calib.h"
#include "parcut.h"
#include "lundcom.h"
      COMMON / vernfs / version
#include "qmacro.h"
C
C  Welcome !
C
      ier = 0
      version = 2.01
      WRITE(6,1000) version
C
C Format banks
C
      CALL bkfmt('PSFR','2I,(5F,I,9F,I)')
      CALL bkfmt('EFOL','2I,(5F,6I)')
      CALL bkfmt('EJET','2I,(4F)')
C
C Read cards
C
      ifsim= NLINK('FSIM',0)
      IF ( ifsim .NE. 0 ) THEN
       ntrack = IW(ifsim+1)
       mtrack = IW(ifsim+2)
       echmin = RW(ifsim+3)
       echmax = RW(ifsim+4)
       n0     = IW(ifsim+5)
       d0     = RW(ifsim+6)
       z0     = RW(ifsim+7)
       idbg   = IW(ifsim+8)
      ELSE
       ntrack = 0
       mtrack = 200
       echmin = 0.
       echmax = 1000.
       n0     = 4
       d0     = 2.0
       z0     = 10.0
       IDBG   = 0
      END IF
C
      ireso = NLINK('RESO',0)
      IF ( ireso .GT. 0 ) THEN
        fluche = RW(ireso+ 1)
        flucon = RW(ireso+ 2)
        eblind = RW(ireso+ 3)
        ecacut = RW(ireso+ 4)
        flucha = RW(ireso+ 5)
        flucut = RW(ireso+ 6)
        hcacut = RW(ireso+ 7)
        flutpc = RW(ireso+ 8)
        tpccut = RW(ireso+ 9)
      ELSE
        fluche = 0.25
        flucon = 0.02
        eblind = 0.00
        ecacut = 0.35
        flucha = 1.00
        flucut = 1.00
        hcacut = 0.35
        flutpc = 0.0006
        tpccut = 0.00
      END IF
C
      iacce = NLINK('ACCE',0)
      IF ( iacce .GT. 0 ) THEN
        ecaacc = RW(iacce+1)
        hcaacc = RW(iacce+2)
        tpcacc = RW(iacce+3)
      ELSE
        ecaacc =  3.0
        hcaacc =  8.5
        tpcacc =  0.0
      END IF
C
      icrak = NLINK('CRAK',0)
      IF ( icrak .GT. 0 ) THEN
        polcor(1) = RW(icrak+ 1)
        polcor(2) = RW(icrak+ 2)
        polcor(3) = RW(icrak+ 3)
        polcor(4) = RW(icrak+ 4)
        azicra    = RW(icrak+ 5)
        azicrl    = RW(icrak+ 6)
        flucor(1) = RW(icrak+ 7)
        flucor(2) = RW(icrak+ 8)
        flulum    = RW(icrak+ 9)
        flucra    = RW(icrak+10)
        flucrl    = RW(icrak+11)
      ELSE
        polcor(1) = 37.0
        polcor(2) = 45.0
        polcor(3) =  9.5
        polcor(4) = 12.0
        azicra    =  0.5
        azicrl    =  3.0
        flucor(1) =  3.0
        flucor(2) =  3.0
        flulum    =  2.0
        flucra    =  3.0
        flucrl    =  3.0
      END IF
C
      idime = NLINK('DIME',0)
      IF ( idime .GT. 0 ) THEN
        DO i = 1, 13
          zdime(i) = RW(idime+0+i)
          rdime(i) = RW(idime+13+i)
          x0rad(i) = RW(idime+26+i)
          atome(i) = RW(idime+39+i)
        ENDDO
      ELSE
C -- Beam pipe
        zdime( 1) = 460.
        rdime( 1) = 5.35
        x0rad( 1) = 0.0031
        atome( 1) = 4.000
C -- VDET first layer
        zdime( 2) = 20.         !  new vdet
        rdime( 2) = 6.4
        x0rad( 2) = 0.0220/3.   !  new vdet
        atome( 2) = 7.422
C -- VDET 2nd layer
        zdime( 3) = 20.         !  new vdet
        rdime( 3) = 11.2
        x0rad( 3) = 0.0220/3.   !  new vdet
        atome( 3) = 7.422
C -- ITC entrance
        zdime( 4) = 100.
        rdime( 4) = 16.1
        x0rad( 4) = 0.0057
        atome( 4) = 6.327
C -- ITC end
        zdime( 5) = 100.
        rdime( 5) = 26.0
        x0rad( 5) = 0.0097
        atome( 5) = 18.779
C -- TPC entrance
        zdime( 6) = 220.
        rdime( 6) = 31.5
        x0rad( 6) = 0.0195
        atome( 6) = 8.870
C -- TPC end
        zdime( 7) = 220.
        rdime( 7) = 180.
        x0rad( 7) = 0.0123
        atome( 7) = 17.49
C -- ECAL entrance
        zdime( 8) = 245.
        rdime( 8) = 185.
        x0rad( 8) = 0.0499 + 0.0000
        atome( 8) = 50
C -- ECAL stack 1 --> 2
        zdime( 9) = 256.5
        rdime( 9) = 196.
        x0rad( 9) = 0.
        atome( 9) = 50
C -- ECAL stack 2 --> 3
        zdime(10) = 283.
        rdime(10) = 221.
        x0rad(10) = 0.
        atome(10) = 50
C -- ECAL end
        zdime(11) = 297.
        rdime(11) = 234.
        x0rad(11) = 0.
        atome(11) = 50
C -- HCAL entrance
        zdime(12) = 320.
        rdime(12) = 305.
        x0rad(12) = 0.
        atome(12) = 50
C -- HCAL end
        zdime(13) = 460.
        rdime(13) = 445.
        x0rad(13) = 0.
        atome(13) = 50
      END IF
C
C Angular distance for clusterization for photons and hadrons
C
      iclus = NLINK('CLUS',0)
      IF ( iclus .GT. 0 ) THEN
        agcut(1) = COS(RW(iclus+1)*pi/180.)
        agcut(2) = COS(RW(iclus+2)*pi/180.)
      ELSE
        agcut(1) = COS(3.*pi/180.)
        agcut(2) = COS(5.*pi/180.)
      ENDIF
C
C Objects leaving no energy in the calorimeters
C
      inocl = NLINK('NOCL',0)
      IF ( inocl .GT. 0 ) THEN
        pnoclb = RW(inocl+1)
        pnocle = RW(inocl+2)
        pnoclo = RW(inocl+3)
      ELSE
        pnoclb = 1.0
        pnocle = 1.0
        pnoclo = 1.0
      ENDIF
C
C Calibrations for neutral hadrons and photons
C
      icali = NLINK('CALI',0)
      IF ( icali .GT. 0 ) THEN
        calibp    = RW(icali+1)
        calibnnl  = RW(icali+2)
        calibnb   = RW(icali+3)
        calibne   = RW(icali+4)
        calibno   = RW(icali+5)
        calibpf   = RW(icali+6)
        calibnnlf = RW(icali+7)
        calibnbf  = RW(icali+8)
        calibnef  = RW(icali+9)
        calibnof  = RW(icali+10)
      ELSE
        calibp    = 0.0
        calibnnl  = 0.0
        calibnb   = 0.0
        calibne   = 0.0
        calibno   = 0.0
        calibpf   = 1.0
        calibnnlf = 1.0
        calibnbf  = 1.0
        calibnef  = 1.0
        calibnof  = 1.0
      ENDIF
C
C Coefficients for fake neutral hadronic energy deposition
C
      ineut = NLINK('NEUT',0)
      IF ( ineut .GT. 0 ) THEN
        pnlk  = RW(ineut+1)
        pstlf = RW(ineut+2)
        phstl = RW(ineut+3)
      ELSE
        pnlk  = 1.
        pstlf = 1.
        phstl = .2
      ENDIF
C
C Magnetic Field (In KGauss)
C
      ifiel = NLINK('FIEL',0)
      IF ( ifiel .GT. 0 ) THEN
        bfield = RW(ifiel+1)
      ELSE
        bfield = 15.0
      ENDIF
C
C Possibility of switching off the central detector
C
      inotp = NLINK('NOTP',0)
C
C Possibility of no endcaps
C
      inoen = NLINK('NOEN',0)
C
C Possibility of no separate HCAL, number of interaction lengths
c of the ECAL, at 90 degrees, and e/pi ratio for the ecal
C
      inohc = NLINK('NOHC',0)
      IF ( inohc .GT. 0 ) THEN
        xinter = RW(inohc+1)
        xepira = RW(inohc+2)
        xminio = rw(inohc+3)
      ELSE
        xinter = 2.0
        xepira = 1.0
        xminio = 1.0
      ENDIF
C
C Switch on charged pi/K decay modes
C
      kcp = LUCOMP(211)
      kck = LUCOMP(321)
      mdcy(kcp,1) = 1
      mdcy(kck,1) = 1
C
C Initialize muon Id
C
      CALL munewr

C
C Initialization cards for the 97 geometry
C     
      igeom = NLINK('FSGE',0)
      IF ( igeom .GT. 0 ) THEN
        ngeom = IW(igeom+1)

C Geometry 98 data base 232 (Galeph and Julia)

        IF (ngeom .eq. 97) THEN
        calibp    = 0.0
        calibnnl  = 0.0
        calibnb   = 0.0
        calibne   = 0.0
        calibno   = 0.0
        calibpf   = 1.05
        calibnnlf = 1.0
        calibnbf  = 1.0
        calibnef  = 1.0
        calibnof  = 1.0
       ENDIF

C Geometry 98 data base 232 (Galeph and Julia)

        IF (ngeom .eq. 98) THEN
          calibp    = 0.00
          calibnnl  = 0.025
          calibnb   = 0.028
          calibne   = 0.05
          calibno   = -.05
          calibpf   = 1.063
          calibnnlf = 1.00
          calibnbf  = 1.03
          calibnef  = 1.005
          calibnof  = 1.02
          pnoclb    = 0.92
          pnocle    = 0.90
          pnoclo    = 1.50
          pnlk      = 1.00
          pstlf     = 1.40
          phstl     = 0.30
        ENDIF

      ENDIF

      RETURN

C----------------------------------------------------------------
 1000 FORMAT(//10X,'+---------------------------------------------+'/
     .       10X,'|     Welcome to the ALEPH Fast Simulation    |'/
     .       10X,'|                                             |'/
     .       10X,'|                   QUFSIM                    |'/
     .       10X,'|                                             |'/
     .       10X,'|                Version ',F4.2,'                 |'/
     .       10X,'|                                             |'/
     .       10X,'|  Last date of change : 12 June 1998         |'/
     .       10X,'|                                             |'/
     .       10X,'| .Original code (1990):                      |'/
     .       10X,'|          o Gerardo Ganis         - MPI      |'/
     .       10X,'|          o Jean-Francois Grivaz  - LAL      |'/
     .       10X,'|                                             |'/
     .       10X,'| .Taken over in 1991 for new developments:   |'/
     .       10X,'|          o Patrick Janot         - CERN     |'/
     .       10X,'|                                             |'/
     .       10X,'| . ... and in 1998:                          |'/     
     .       10X,'|          o Marumi Kado           - LAL      |'/
     .       10X,'|                                             |'/
     .       10X,'| .Trigger simulation:                        |'/
     .       10X,'|          o Laurent Duflot        - LAL      |'/
     .       10X,'|                                             |'/     
     .       10X,'+---------------------------------------------+'//)
      END





