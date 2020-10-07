      SUBROUTINE IPRDAF
C-----------------------------------------------------------------------
C! Print current values of ITC prepare data consts. from DAF
C!
CKEY ITCDES ITC PRINT
C!   Author          :-  J. Sedgbeer  89/03/03
C!   Modified        :-  J. Sedgbeer  91/01/07 Tidy Formats.
C!   Modified        :-  J. Sedgbeer  92/01/30 Remove obsolete code
C!   Modified        :-  J. Sedgbeer  92/02/07 Print /IZNLCC/
C!   Modified        :-  J. Sedgbeer  93/07/01 Print /IDTCCC/ mods.
C!                                             Add print /IDRPCC/
C!   Modified        :-  J. Sedgbeer  98/08/16 Add print of new
C!                                             variables in /IDRPCC/
C!   Input:
C!    commons:    /ITWICC/  ITC wire geometry
C!                /ISWPHI/  Sense wire phi values in ITC frame.
C!                /IALIGC/  ITC alignment constants.
C!                /IALIGG/  Global alignment constants.
C!                /IRFECC/  ITC R-phi Front-end constants
C!                /IZFECC/  ITC Z Front-end constants
C!                /IZNLCC/  ITC Z non linearity constants
C!                /IRESOL/  DC R-phi resolution common (nominal).
C!                /IRESCC/  DC R-Phi resolution common -parametrisation.
C!                /IZRSCC/  DC Z resolution common -parametrisation.
C!                /IDTCCC/  Drift-time relation common - old.
C!                /IDRPCC/  Drift-time relation common.
C!                /BCS/     for print stream IW(6)
C!
C!   Output: none
C!
C!   Description:
C!   Print current values of ITC Geometry and Alignment constants
C!   contained in commons /ITWICC/ and /IALIGC/ and /IALIGG/
C!   Print current values of ITC R-phi and Z Front-End constants
C!   contained in commons /IRFECC/ and /IZFECC/
C!   Print current values of Z non-linearity params. in /IZNLCC/
C!   Print current values of ITC Drift-time and resolution parameters
C!   contained in commons /IRESCC/, /IZRSCC/, /IRESOL/, /IDTCCC/ and
C!                                                     /IDRPCC/.
C!   Note: Must fill commons first by calling IRDDAF.
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C I/O commons etc.
#include "bcs.h"
#include "itwicc.h"
#include "iswphi.h"
#include "ialigc.h"
#include "ialigg.h"
#include "irfecc.h"
#include "izfecc.h"
#include "iznlcc.h"
#include "irescc.h"
#include "iresol.h"
#include "idtccc.h"
#include "idrpcc.h"
#include "izrscc.h"
C-----------------------------------------------------------------------
      LPR = IW(6)
C
C Print Geometry constants
C
      WRITE(LPR,1000) WZMXIT,SGMXIT,MWIRIT
 1000 FORMAT('0IPRDAF: ITC Wire Geometry Constants:',/
     +      9X,'Wire half-length(cm.)=',F6.1,
     +      '    Max. Wire Sag (cm.)=',F7.4,/
     +      9X,'Total number of sense wires=',I4)

      WRITE(LPR,1001)
 1001 FORMAT(8X,' Layer',' Iwire Nwire Radius','  Cell-width',
     +        '    PhiOffset',/28X,'(cm.)',5X,'(cm.)',8X,'(rads.)')
      DO 100 I=1,JWIRIT
        WRITE(LPR,1002) I,IWIRIT(I),NWIRIT(I),RWIRIT(I),CELWIT(I),
     +                 PHWRIT(I)
 1002   FORMAT(8X,I5,2I6,F8.2,F9.2,F15.4)
  100 CONTINUE
C
      WRITE(LPR,1003)
 1003 FORMAT('0IPRDAF: ITC Sense Wire Phi coords. (ITC frame):')
      DO 210 IL = 1,JWIRIT
        NW = NWIRIT(IL)
        WRITE(LPR,1004) IL,NW,RWIRIT(IL)
 1004   FORMAT('0   Layer',I2,5X,'Number of Wires',I4,7X,
     +   'Radius (cm.)',F7.2/1X,'   Wires',7X,'Phi values (rads.)')
        DO 200 J=1,NW/8
          MW2 = 8*J
          MW1 = MW2-7
          KW1 = MW1+IWIRIT(IL)
          KW2 = KW1 + 7
          WRITE(LPR,1005) MW1,MW2,(PHSWIS(KK),KK=KW1,KW2)
 1005     FORMAT(2X,I3,' to ',I3,3X,8F7.4)
  200   CONTINUE
  210 CONTINUE
C
C-----------------------------------------------------------------------
C Print Alignment consts.
C
      WRITE(LPR,1006) (DXYZIA(I),I=1,3),
     +               ((ROTNIA(I,J),J=1,3),I=1,3)
 1006 FORMAT('0IPRDAF:   ITC Alignment Constants (wrt TPC)',/
     +        19X,'dx,dy,dz (cm.) ',3F10.4,/
     +        19X,'Rotation Matrix',3F10.4,/(34X,3F10.4))
C
      WRITE(LPR,1007) (GTRNIA(I),I=1,3),
     +               ((GROTIA(I,J),J=1,3),I=1,3)
 1007 FORMAT('0IPRDAF:   ITC-TPC Alignment Constants (wrt ALEPH)',/
     +        19X,'dx,dy,dz (cm.) ',3F10.4,/
     +        19X,'Rotation Matrix',3F10.4,/(34X,3F10.4))
C
C-----------------------------------------------------------------------
C Print Front-end consts.
C
      WRITE(LPR,2000) BWIDIR,IBN0IR
 2000 FORMAT('0IPRDAF: ITC front-end constants:',/
     +       5X,'R-phi TDC bin width (ns.)      ',F10.2,/
     +       5X,'TDC bin at sense wire          ',I10)
      WRITE(LPR,2001) (I,ITLOIR(I),ITHIIR(I),I=1,JLAYIR)
 2001 FORMAT((5X,'Layer',I3,'     Min. allowed R-phi TDC count',I7,
     +       '   Max. allowed',I7))
C
      WRITE(LPR,2002) BWIDIZ,IBN0IZ,EXP8IZ,
     +                  IBNDIZ,FZCOIZ
 2002 FORMAT(5X,'Z TDC bin width (ns.)            ',F10.2,/
     +         5X,'Z TDC bin number at Z=0          ',I10,/
     +         5X,'Time expansion factor for layer 8',F10.1,/
     +         5X,'S-Bend correction on/off flag     ',I10,/
     +         5X,'Z TDC data O.K.                   ',L2)
      WRITE(LPR,2003) (I,ITLOIZ(I),ITHIIZ(I),I=1,JLAYIZ)
 2003 FORMAT((5X,'Layer',I3,'     Min. allowed Z TDC count',I7,
     +       '   Max. allowed',I7))
      WRITE(LPR,2004) (OFSLIZ(I),I=1,JOFSLN),(CZNLIZ(I),I=1,JCZNLN)
 2004 FORMAT(5X,'Non Linearity: layer offsets 1-4:',4F8.2,/
     +                      20X,'             5-8:',4F8.2,/
     +                      20X,'Coeff - Linear    ',F8.3,/
     +                      20X,'Coeff - Amplitude ',F8.3,/
     +                      20X,'Coeff - Period    ',F8.3)
C
C-----------------------------------------------------------------------
C Print current values of ITC Drift-time corrections and resolutions.
C
      WRITE(LPR,4000) (SIGRIR(I),I=1,8),(SMAXIR(I),I=1,8),
     +                (SIGZIR(I),I=1,8)
 4000 FORMAT('0IPRDAF: ITC Drift-time and Resolution Constants:',/4X,
     +    'Nominal r-phi resolution layers 1-8 (cm.):',/4X,8F7.4,/4X,
     +    'Maximum r-phi resolution layers 1-8 (cm.):',/4X,8F7.4,/4X,
     +    'Nominal  Z    resolution layers 1-8 (cm.):',/4X,8F7.1)
C
      WRITE(LPR,4001)
 4001 FORMAT('0          R-Phi Resolution:  Layer','  Coefficients')
      DO 300 J=1,8
        WRITE(LPR,4002)  J,(RESCOF(I,J),I=1,3)
 4002   FORMAT(31X,I4,3F10.4)
  300 CONTINUE
C
      WRITE(LPR,4003)
 4003 FORMAT('0              Z Resolution:  Layer','  Coefficients')
      DO 400 J=1,8
        WRITE(LPR,4004) J,(ZRESIZ(I,J),I=1,2)
 4004   FORMAT(31X,I4,2F10.4)
  400 CONTINUE
C
C Print for polynomial drift-time parametrisation
C
      IF (IFLGID.EQ.0) THEN
C
        WRITE(LPR,4005)
 4005   FORMAT('0    Drift-Time Constants:',/6X,'/IDTCCC/',' Layer',
     +         ' Time-Offset',' Coefficients')
        DO 500 I=1,8
          WRITE(LPR,4006) I,TOFFID(I),(DCOFID(J,I),J=1,JCOFID)
 4006     FORMAT(13X,I5,F7.2,3E11.3)
  500   CONTINUE
C
        WRITE(LPR,4007)
 4007   FORMAT('0    Drift-Time Constants:',/6X,'/IDRPCC/',' Layer',
     +         ' Time-Offset',' Poly Coeffs ')
        DO 510 I=1,8
          WRITE(LPR,4008) I,TTOFID(I),(DTCOID(J,I),J=1,MCOFID)
 4008     FORMAT(13X,I5,F7.2,5E11.3)
  510   CONTINUE
C
C Print for Spline  drift-time parametrisation
C
      ELSE
        WRITE(LPR,4009)
 4009   FORMAT('0    Drift-Time Constants:',/6X,'/IDRPCC/',' Layer',
     +         ' Time-Offset',' Spline Coeffs ')
        DO 520 I=1,8
          WRITE(LPR,4010) I,TTOFID(I),(DSPLID(J,I),J=1,MSPLID)
 4010     FORMAT(13X,I5,F7.2,5E11.3)
  520   CONTINUE
C
      ENDIF
C
      END
#endif
