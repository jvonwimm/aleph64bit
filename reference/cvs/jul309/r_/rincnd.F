      SUBROUTINE RINCND
C----------------------------------------------------------------------
C! Initialise conditions for all the detector  (Magnetic field)
C
C  Author      S. Orteu   9-FEB-1987
C!   Modified :- E. Lancon              6-MAR-1991
C               Add KAFID = IW(NAMIND('AFID')) undefined before
C!   Modified :- M. Cattaneo            8-JUL-1997
C               Protect against zero magnetic field:
C               Set FIELRC to 0.0001 if ALFIEL returns it as 0.0
C               Produce a "fatal" error
C
C  For the moment, only the magnetic field. The data is taken from the
C  bank from GALEPH, but in the future it will be read from the DataBas
C
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "bosext.h"
#include "rconds.h"
#include "rflags.h"
#include "rlunit.h"
#include "rparac.h"
#include "afidjj.h"
#include "revent.h"
#include "rcurnt.h"
#include "ralejj.h"
#include "ajobjj.h"
#include "runhjj.h"
#include "tsimjj.h"
#include "asimjj.h"
#include "bmacro.h"
C This has to be changed for the acces to the database information. May
C place of the routine has to be changed. (You don't need to specify the
C magnetic field for every event.
C
      IEX=0
      ITYP=0
      IDAT=0
      ITIM=0
      FIELRC=15.
      KRUNH=IW(NAMIND('RUNH'))
      IF(KRUNH.NE.0)THEN
        IEX=IW(KRUNH+JRUNEN)
        ITYP=IW(KRUNH+JRUNRT)
        IDAT=IW(KRUNH+JRUNSD)
        ITIM=IW(KRUNH+JRUNST)
        WRITE (LOUTRL,'(''0+'',78(''-''),''+'')')
        WRITE (LOUTRL,'('' |'',78X,''|'')')
        WRITE (LOUTRL,776) IRUNRC,IEX,ITYP,IDAT,ITIM
 776    FORMAT(' | Run number =',I5,'. Exp. number = '
     +    ,I5,' Type=',I6,' Date/Time',I7,I10,'|')
      ENDIF
      FIELRC=ALFIEL(DUMMY)
      KAJOB=IW(NAMIND('AJOB'))
      KASIM=IW(NAMIND('ASIM'))
      KTSIM=IW(NAMIND('TSIM'))
      KAFID=IW(NAMIND('AFID'))
      IVSIM=0
      IF(KTSIM.NE.0)IVSIM=IW(KTSIM+LMHLEN+JTSITV)
      IF(KAJOB.NE.0)THEN
        GVER=REAL(IW(KAJOB+LMHLEN+JAJOGV))/10.
        AVER=REAL(IW(KAJOB+LMHLEN+JAJOAV))/10.
        IGCOR = -1
        IF ( LCOLS(KAJOB).GE.JAJOCV ) IGCOR = IW(KAJOB+LMHLEN+JAJOCV)/1
        IGEAN = -1
        IF ( LCOLS(KAJOB).GE.JAJOGN ) IGEAN = IW(KAJOB+LMHLEN+JAJOGN)
        IGEO  = -1
        IF ( KASIM.GT.0 ) THEN
          IGEO = IW(KASIM+LMHLEN+JASIYM)
        ENDIF
        WRITE(LOUTRL,777) GVER,AVER,IVSIM
 777    FORMAT(' | Reading MC    ',
     +    'Produced with GALEPH-Version =',F6.1,
     +    '. ALEPHLIB',F6.1,' TPCSIM',I4,' |')
        WRITE(LOUTRL,778) IGCOR,IGEAN,IGEO
 778    FORMAT(' |               ',
     +    '              Correction-File ',I6,
     +    '  Geant  :',I5,  ' Geo. :',I4,' |')
      ENDIF
      ECMS=0.
      IF(KAFID.NE.0)ECMS=RTABL(KAFID,1,JAFIBE)
      ENERRC = ALELEP(IRUNRC)
      WRITE(LOUTRL,779)FIELRC,ENERRC
 779  FORMAT(' | Magnetic field is ',F7.3,
     +  ' KGauss. c.m. energy = ',F8.3,21X,'|')
      KFIEL=IW(NAMIND('FIEL'))
      IF(KFIEL.NE.0)THEN
        FIELRC=RW(KFIEL+1)
        WRITE(LOUTRL,780)FIELRC
 780    FORMAT(' | Magnetic field is ',F7.3,
     +    ' KGauss <------ SET BY DATA CARD!!!!!!!!'
     +    ,12X,'|')
      ENDIF
      WRITE (LOUTRL,'('' |'',78X,''|'')')
      WRITE (LOUTRL,'('' +'',78(''-''),''+''/)')
C
C Protect against zero field
      IF( FIELRC .EQ. 0. ) THEN
        FIELRC = 0.0001
        CALL RERROR( 'RINCND', -1,'Magnetic field is zero!' )
      ENDIF
C
      END
#endif
