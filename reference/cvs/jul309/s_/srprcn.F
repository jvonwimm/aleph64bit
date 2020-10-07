      SUBROUTINE SRPRCN
C----------------------------------------------------------------------
C! Print the constants for this run
C!
C!    Author:     H. Meinhard       30-Jan-1989
C!    Modified:   H. Meinhard       02-Mar-1990  (1)
C!
C?
C!======================================================================
#ifndef DOC
#include "rflags.h"
#include "rlunit.h"
#include "rparac.h"
#include "saligc.h"
#include "scalbc.h"
#include "sgeomc.h"
#include "srecpc.h"
C----------------------------------------------------------------------
      IF (JDBDRF(JULSA) .GE. 1) THEN
C
        WRITE (LOUTRL,'(//'' ------ SATR Geometry'')')
        WRITE (LOUTRL,'(  '' ===================='')')
        WRITE (LOUTRL,100)
     +    'Distance between beam axis and inner acceptance:',RZERSG
        WRITE (LOUTRL,100)
     +    'Distance between two neighboured wires:',RDELSG
        WRITE (LOUTRL,100)
     +  'Dist between interaction point and surface of Al support:',
     +    ZZERSG
        WRITE (LOUTRL,100)
     +    'Distance between two neighboured layers:',ZDELSG
        WRITE (LOUTRL,100)
     +    'Dist between wire in 9th layer and surface of Al support:',
     +    ZOFFSG
        WRITE (LOUTRL,110)
     +    'Azimuth start angle of layers:',PHIBSG
        WRITE (LOUTRL,100)
     +    'Thickness of brass tube material:',TUBESG
        WRITE (LOUTRL,100)
     +    'Dead zone between inner sectors, per sector:',SECDSG
        WRITE (LOUTRL,100)
     +    'Dead zone at the edges of a half layer:',GASDSG
        WRITE (LOUTRL,120)
     +    'Number of SATR layers:',MLAYSG
        WRITE (LOUTRL,120)
     +    'Number of sectors per layer:',MSECSG
        WRITE (LOUTRL,120)
     +    'Number of wires per sector:',MWIRSG
        WRITE (LOUTRL,130)
     +    'Oring modes in layers:',MDORSG
        WRITE (LOUTRL,120)
     +    'Number of TDC crates:',MCRASG
        WRITE (LOUTRL,120)
     +    'Number of TDC cards per crate:',MCRDSG
        WRITE (LOUTRL,120)
     +    'Number of TDCs per card:',MTDCSG
        WRITE (LOUTRL,140)
     +    'Sector oring map:',KSECSG
C
        WRITE (LOUTRL,'(//'' ------ SATR Alignment'')')
        WRITE (LOUTRL,'(  '' ====================='')')
        WRITE (LOUTRL,150)
     +    'Shift of modules in x-direction:',XSHISA
        WRITE (LOUTRL,150)
     +    'Shift of modules in y-direction:',YSHISA
        WRITE (LOUTRL,150)
     +    'Shift of modules in z-direction:',ZSHISA
        WRITE (LOUTRL,150)
     +    'Angle of modules between ideal and real yz-plane:',YZANSA
        WRITE (LOUTRL,150)
     +    'Angle of modules between ideal and real zx-plane:',ZXANSA
        WRITE (LOUTRL,150)
     +    'Angle of modules between ideal and real xy-plane:',XYANSA
C
        WRITE (LOUTRL,'(//'' ------ SATR Calibration'')')
        WRITE (LOUTRL,'(  '' ======================='')')
        WRITE (LOUTRL,160)
     +    'Channel on/off bit pattern:',JWIRSC
        WRITE (LOUTRL,180)
     +    'Time offset (drift time conversion):',TOFFSC
        WRITE (LOUTRL,120)
     +    'Number of points for x-t-relation:',MPOISC
        WRITE (LOUTRL,180)
     +    'Abscissa of first t-x-point:',TSTASC
        WRITE (LOUTRL,180)
     +    'Difference of abscissas of neighboured t-x-points:',TDELSC
        WRITE (LOUTRL,170)
     +    'Points of x-t-relation:',(TSPASC(I),I=1,MPOISC)
C
        WRITE (LOUTRL,'(//'' ------ SATR Reconstruction Cuts'')')
        WRITE (LOUTRL,'(  '' ==============================='')')
        WRITE (LOUTRL,180)
     +    'Lower drift time cut:',TLOWSR
        WRITE (LOUTRL,180)
     +    'Upper drift time cut:',TUPPSR
        WRITE (LOUTRL,100)
     +    'Upper drift distance cut:',RUPPSR
        WRITE (LOUTRL,100)
     +    'Resolution of wire tubes:',SIGMSR
        WRITE (LOUTRL,100)
     +    'Theta window width:',DTHCSR
        WRITE (LOUTRL,100)
     +    'Effectively dead zone between two inner sectors:',DSECSR
        WRITE (LOUTRL,100)
     +    'Effectively dead zone at gas channel:',DGASSR
        WRITE (LOUTRL,120)
     +    'Minimum number of wire paralleles in track fit:',NPARSR
        WRITE (LOUTRL,100)
     +    'Minimum chisquare probability of track fit:',CHPRSR
        WRITE (LOUTRL,100)
     +    'Cut on distance measure of neighboured track candidates:',
     +    ACTRSR
        WRITE (LOUTRL,100)
     +    'Cut on distance measure of neighboured patches:',ACPASR
        WRITE (LOUTRL,100)
     +    'Zone width for cluster and patch finding:',ZONWSR
        WRITE (LOUTRL,100)
     +    'Assumed resolution of wire tubes for 2-arm fit:',SIG2SR
        WRITE (LOUTRL,120)
     +    'Min number of parallels per side for 2-arm fit:',NPA2SR
        WRITE (LOUTRL,120)
     +    'Max number of parallels to be dropped for 2-arm fit:',NDR2SR
C
        WRITE (LOUTRL,'(//)')
      ENDIF
C
  999 CONTINUE
      RETURN
C
C printout formats
  100 FORMAT (11X,A60,F10.5)
  110 FORMAT (11X,A60,3F10.5,2(/71X,3F10.5))
  120 FORMAT (11X,A60,I4)
  130 FORMAT (11X,A60,9(I4,2X))
  140 FORMAT (11X,A60,4(1X,I1,1X,I1,2X),2(/71X,4(1X,I1,1X,I1,2X)))
  150 FORMAT (11X,A60,4(F10.4,3X))
  160 FORMAT (11X,A60,12(I3,1X),5(/71X,12(I3,1X)))
  170 FORMAT (11X,A60,F10.5/(71X,F10.5))
  180 FORMAT (11X,A60,E12.5)
      END
#endif
