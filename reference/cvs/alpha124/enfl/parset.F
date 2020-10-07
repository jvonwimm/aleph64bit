      SUBROUTINE PARSET
C----------------------------------------------------------------
C! Read the ENFLW datacards
C
C  Patrick Janot -- 21 june 1990
C----------------------------------------------------------------
#ifndef DOC
#include "parcut.h"
      COMMON / vernvw / vernfw
#include "qcde.h"
C
C  Welcome !
C
      vernfw = 2.09
      WRITE (IW(6),1000) vernfw
C
C  Card for q qbar gamma search
C
      nngama = 0
      kkgama = namind('GAMA')
      IF ( kkgama .GT. 0 ) nngama = IW(kkgama)
C
C  Card for pad/wire ratio
C
      rpsf = 1.
      irpsf = 0
      nnrpsf = namind('RPSF')
      IF ( nnrpsf .GT. 0 ) irpsf = IW(nnrpsf)
      IF ( irpsf .GT. 0 ) rpsf = RW(irpsf+1)
C
C  Calorimeter cleaning card
C
      nclea = 1
      iclea = 0
      nnclea = namind('CLEA')
      IF ( nnclea .GT. 0 ) iclea = IW(nnclea)
      IF ( iclea .GT. 0 ) nclea = IW(iclea+1)
C
C  Card for ENFLW parameters
C
      nnnflw = namind('NFLW')
      IF ( nnnflw .GT. 0 ) inflw = IW(nnnflw)
      IF ( inflw .GT. 0 ) THEN
        ntrack = IW(inflw+1)
        mtrack = IW(inflw+2)
        echmin = RW(inflw+3)
        echmax = RW(inflw+4)
        n0     = IW(inflw+5)
        d0     = RW(inflw+6)
        z0     = RW(inflw+7)
        idbg   = IW(inflw+8)
      ELSE
        ntrack = 0
        mtrack = 200
        echmin = 0.
        echmax = 1000.
        n0 = 4
        d0 = 2.0
        z0 = 10.0
        idbg = 0
      ENDIF
      IF ( idbg .GE. 1 ) WRITE (IW(6),1001) ntrack, mtrack, echmin,
     &                                 echmax,n0, d0, z0, idbg
C
C  Tuning of e/pi ratio, xi value
C
      nnnepi = namind('NEPI')
      IF ( nnnepi .GT. 0 ) inepi = IW(nnnepi)
      IF ( inepi .GT. 0 ) THEN
        sigec  = RW(inepi+1)
        sighc  = RW(inepi+2)
        epilca = RW(inepi+3)
        epiend = RW(inepi+4)
        epibar = RW(inepi+5)
      ELSE
        sigec  = 0.5
        sighc  = 1.0
        epilca = 1.5
        epiend = 1.0
        epibar = 1.0
      ENDIF
      IF ( idbg .GE. 1 ) WRITE (IW(6),1002) sigec,sighc,epilca,
     .                                 epiend,epibar
C
      RETURN
C----------------------------------------------------------------
 1000 FORMAT(//1X,'+---------------------------------------------+'/
     .       1X,'|     Welcome to the Energy Flow Package      |'/
     .       1X,'|                                             |'/
     .       1X,'|                   ENFLW                     |'/
     .       1X,'|                                             |'/
     .       1X,'|                Version ',F4.2,'                 |'/
     .       1X,'|                                             |'/
     .       1X,'|  Last date of change : 27 October 1999      |'/
     .       1X,'|  P. Janot      CERN                         |'/
     .       1X,'+---------------------------------------------+'//)
 1001 FORMAT(/' The program will deal with events such that : '/,
     .       '     * Number of charged tracks : '/
     .       '          Minimum : ',I6/
     .       '          Maximum : ',I6/
     .       '     * Total charged energy : '/
     .       '          Minimum : ',F8.1,' GeV'/
     .       '          Maximum : ',F8.1,' GeV'/
     .       '     * Good track definition : '/
     .       '          TPC coords : >= ',I6/
     .       '          |d0|       : <= ',F8.2,' cm'/
     .       '          |z0|       : <= ',F8.2,' cm'//
     .       '     * Print level : ',I2//)
 1002 FORMAT('     * XI values to determine',
     .       ' the neutral hadronic energy : '/
     .       '          In ECAL    : ',F8.2,' GeV**(1/2)'/
     .       '          In HCAL    : ',F8.2,' GeV**(1/2)'/
     .       '     * e/pi ratios :'/
     .       '          In LCAL    : ',F8.2/
     .       '          In Endcaps : ',F8.2,' (scaling factor)'/
     .       '          In Barrel  : ',F8.2,' (scaling factor)'//)
      END
#endif
