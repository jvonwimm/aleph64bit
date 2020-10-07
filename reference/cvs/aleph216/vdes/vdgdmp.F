      INTEGER FUNCTION VDGDMP ()
C ----------------------------------------------------------------------
CKEY VDETDES PRINT DUMP / USER
C!  Dump VDET geometry commons
C - Steve Wasserbaech, January 1994
C
C - Input:
C   (none)
C
C - Output:
C   VDGDMP / I   = VDOK if successful
C                = VDERR if an error occurred
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vsltco.h"
#include "vdlaco.h"
#include "vzpwco.h"
#include "vwgeco.h"
#include "vrdoco.h"
#include "vgindx.h"
#include "vdetge.h"
#include "bcs.h"
C
C     Local variables
      INTEGER LOUT, JLAY, JFAC, JMOD, JWAF, IWFF
C
C ----------------------------------------------------------------------
C
      VDGDMP = VDERR
      LOUT = IW(6)
C
      IF (IVSTUP .LE. 0) THEN
        WRITE (LOUT,'(1X,A)')
     >          '+++VDGDMP+++  VDET geometry commons are not filled.'
        GO TO 1000
      ENDIF
C
      WRITE (LOUT,'(/1X,A)')
     >          '+++VDGDMP+++  Dump of VDET geometry commons'
      WRITE (LOUT,'(1X,A,I3)') '+++VDGDMP+++  Setup', IVSTUP
C
C ----------------------------------------------------------------------
C
C     Parameters:
C
      WRITE (LOUT,'(/1X,A)') 'Parameters:'
      WRITE (LOUT,'((1X,4(A,I4,5X)))')
     >         'VDERR  =', VDERR,  'VDOK   =', VDOK,
     >         'VVIEWZ =', VVIEWZ, 'VVIEWP =', VVIEWP,
     >         'NVLAYR =', NVLAYR, 'NVMODF =', NVMODF,
     >         'NVVIEW =', NVVIEW, 'NPROMM =', NPROMM,
     >         'IROMAX =', IROMAX, 'NVWMMX =', NVWMMX,
     >         'NVWFMX =', NVWFMX, 'NVFLMX =', NVFLMX,
     >         'NVFMAX =', NVFMAX, 'NVMMAX =', NVMMAX,
     >         'NVWMAX =', NVWMAX, 'NVZRMX =', NVZRMX,
     >         'NVPRMX =', NVPRMX
C
C ----------------------------------------------------------------------
C
C     VSLTCO:
C
      WRITE (LOUT,'(/1X,A)') 'VSLTCO common:'
      WRITE (LOUT,'(1X,A,I4)') 'NSLOTS =', NSLOTS
      WRITE (LOUT,'(1X,A)') ' JFAC    JJLAYF    PHIOFF    ISSFLG'
      DO JFAC=1,NSLOTS
        WRITE (LOUT,'(I6,I10,F10.5,I10)') JFAC, JJLAYF(JFAC),
     >                                 PHIOFF(JFAC), ISSFLG(JFAC)
      ENDDO
C
C ----------------------------------------------------------------------
C
C     VDLACO:
C
      WRITE (LOUT,'(/1X,A)') 'VDLACO common:'
      WRITE (LOUT,'(1X,A)') ' JLAY    RWVDLA    WATILT    IORIEN'
      DO JLAY=1,NVLAYR
        WRITE (LOUT,'(I6,F10.5,F10.6,I10)') JLAY, RWVDLA(JLAY),
     >                                 WATILT(JLAY), IORIEN(JLAY)
      ENDDO
C
C ----------------------------------------------------------------------
C
C     VZPWCO:
C
      WRITE (LOUT,'(/1X,A)') 'VZPWCO common:'
      WRITE (LOUT,'(1X,A,I4)') 'NWAFEF =', NWAFEF
      WRITE (LOUT,'(1X,A)') ' IWFF    WAFERZ'
      DO IWFF=1,NWAFEF
        WRITE (LOUT,'(I6,F10.5)') IWFF, WAFERZ(IWFF)
      ENDDO
C
C ----------------------------------------------------------------------
C
C     VWGECO:
C
      WRITE (LOUT,'(/1X,A)') 'VWGECO common:'
      WRITE (LOUT,'(1X,2(A,F10.5,4X))')
     >            'WSIZEA =', WSIZEA, 'WSIZEB =', WSIZEB
      WRITE (LOUT,'(1X,2(A,I10,4X))')
     >            'NZSTRP =', NZSTRP, 'NPSTRP =', NPSTRP
      WRITE (LOUT,'(1X,2(A,F10.5,4X))')
     >            'STPITZ =', STPITZ, 'STPITP =', STPITP
      WRITE (LOUT,'(1X,2(A,F10.5,4X))')
     >            'STLENZ =', STLENZ, 'STLENP =', STLENP
      WRITE (LOUT,'(1X,2(A,F10.5,4X))')
     >            'AMNSRZ =', AMNSRZ, 'AMNSRP =', AMNSRP
      WRITE (LOUT,'(1X,2(A,F10.5,4X))')
     >            'BMNSRZ =', BMNSRZ, 'BMNSRP =', BMNSRP
      WRITE (LOUT,'(1X,A,F10.5)')
     >            'WTHICK =', WTHICK
C
C ----------------------------------------------------------------------
C
C     VRDOCO:
C
      WRITE (LOUT,'(/1X,A)') 'VRDOCO common:'
      WRITE (LOUT,'(1X,2(A,I10,4X))')
     >            'NRDSTZ =', NRDSTZ, 'NRDSTP =', NRDSTP
      WRITE (LOUT,'(1X,2(A,I10,4X))')
     >            'NREFRZ =', NREFRZ, 'NREFRP =', NREFRP
      WRITE (LOUT,'(1X,2(A,I10,4X))')
     >            'NOFRDZ =', NOFRDZ, 'NOFRDP =', NOFRDP
      WRITE (LOUT,'(1X,A,I10)')
     >            'NZRSSC =', NZRSSC
      WRITE (LOUT,'(1X,2(A,I10,4X))')
     >            'IECORZ =', IECORZ, 'IECORP =', IECORP
      WRITE (LOUT,'(1X,2(A,I10,4X))')
     >            'NZEROM =', NZEROM, 'NPEROM =', NPEROM
      WRITE (LOUT,'(1X,A,I10)')
     >            'NWFBIT =', NWFBIT
C
C ----------------------------------------------------------------------
C
C     VGINDX:
C
      WRITE (LOUT,'(/1X,A)') 'VGINDX common:'
      WRITE (LOUT,'(1X,A,I4,2X,2(3X,A,I1,A,I3))') 'IVSTUP =', IVSTUP,
     >         ('NFACEL(', JLAY, ') =', NFACEL(JLAY), JLAY=1,NVLAYR)
      WRITE (LOUT,'(1X,3(A,I4,5X))')
     >         'NMODUL =', NMODUL, 'NWAFEM =', NWAFEM,
     >         'NWAFER =', NWAFER
      WRITE (LOUT,'(1X,A,12I4/(8X,12I4))')
     >         'JJFACM:', (JJFACM(JMOD), JMOD=1,NMODUL)
      WRITE (LOUT,'(1X,A,12I4/(8X,12I4))')
     >         'JJMODW:', (JJMODW(JWAF), JWAF=1,NWAFER)
      WRITE (LOUT,'(1X,A,12I4/(8X,12I4))')
     >         'JIFACF:', (JIFACF(JFAC), JFAC=1,NSLOTS)
      WRITE (LOUT,'(1X,A,12I4/(8X,12I4))')
     >         'JIMODM:', (JIMODM(JMOD), JMOD=1,NMODUL)
      WRITE (LOUT,'(1X,A,12I4/(8X,12I4))')
     >         'JIWAFW:', (JIWAFW(JWAF), JWAF=1,NWAFER)
      WRITE (LOUT,'(1X,A,12I4/(8X,12I4))') 'IJFACE:', IJFACE
      WRITE (LOUT,'(1X,A,12I4/(8X,12I4))') 'IJMODU:', IJMODU
      WRITE (LOUT,'(1X,A,12I4/(8X,12I4))') 'IJWAFR:', IJWAFR
      WRITE (LOUT,'(1X,A,12I4/(8X,12I4))')
     >         'JIWFFW:', (JIWFFW(JWAF), JWAF=1,NWAFER)
      WRITE (LOUT,'(1X,A,12I4/(8X,12I4))') 'IJWFFR:', IJWFFR
      WRITE (LOUT,'(1X,A,12(1X,A4)/(8X,12(1X,A4)))') 'TXMODU:', TXMODU
C
C ----------------------------------------------------------------------
C
C     VDETGE:
C
      WRITE (LOUT,'(/1X,A)') 'VDETGE common:'
      WRITE (LOUT,'(1X,3(A,F10.5,4X))') 'RVDMIN =', RVDMIN, 'RVDMAX =',
     >                 RVDMAX, 'ZVDMAX =', ZVDMAX
      WRITE (LOUT,'(1X,A)') ' JWAF    WAXCEN    WAYCEN    WAZCEN'
      DO JWAF=1,NWAFER
        WRITE (LOUT,'(I6,3F10.5)') JWAF, WAXCEN(JWAF), WAYCEN(JWAF),
     >                            WAZCEN(JWAF)
      ENDDO
      WRITE (LOUT,'(1X,A)')
     >               ' JFAC    WARHOC    WAPHIC    CPHIOF    SPHIOF'
      DO JFAC=1,NSLOTS
        WRITE (LOUT,'(I6,4F10.5)') JFAC, WARHOC(JFAC), WAPHIC(JFAC),
     >                            CPHIOF(JFAC), SPHIOF(JFAC)
      ENDDO
      WRITE (LOUT,'(1X,A,2F10.6)') 'TNWTLT:', TNWTLT
      WRITE (LOUT,'(1X,2(A,F10.5,4X))')
     >            'AMXSRZ =', AMXSRZ, 'AMXSRP =', AMXSRP
      WRITE (LOUT,'(1X,2(A,F10.5,4X))')
     >            'BMXSRZ =', BMXSRZ, 'BMXSRP =', BMXSRP
      WRITE (LOUT,'(1X,2(A,I10,4X))')
     >            'NZSROM =', NZSROM, 'NPSROM =', NPSROM
      WRITE (LOUT,'(1X,2(A,I10,4X))')
     >            'NZSMOD =', NZSMOD, 'NPSMOD =', NPSMOD
      WRITE (LOUT,'(1X,2(A,I10,4X))')
     >            'NPRSSC =', NPRSSC, 'NZROMM =', NZROMM
      WRITE (LOUT,'(1X,A,L10)')
     >            'LZMULT =', LZMULT
      WRITE (LOUT,'(1X,2(A,I10,4X))')
     >            'MSVWAF =', MSVWAF, 'MSVNST =', MSVNST
      WRITE (LOUT,'(1X,2(A,I10,4X))')
     >            'ISSLAY =', ISSLAY, 'ISSNST =', ISSNST
C
C ----------------------------------------------------------------------
C
C     Finished!
C
      VDGDMP = VDOK
C
 1000 CONTINUE
      RETURN
      END
#endif
