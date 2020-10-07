*DK gaxeph
#if defined(INTER)
      SUBROUTINE GTRIG
C ----------------------------------------------------------------------
C. -  B. BLOCH - 850910
C
C  Event   tracking   control routine to replace Geant default
C   Calls   ASPEVE ,ASCEVE from this .HLB
C   Called from <GXINT>GXCONT     from GEANT3
C ---------------------------------------------------------------------
#ifndef DOC
      CALL ASPEVE
      END
#endif
      SUBROUTINE GTRIGC
C ----------------------------------------------------------------------
C. -  B. BLOCH - 850910
C
C   event closing routine to replace Geant default
C
C   Calls   ASCEVE        from this .HLB
C   Called from <GXINT>GXCONT     from GEANT3
C ---------------------------------------------------------------------
#ifndef DOC
      CALL ASCEVE
      END
#endif
      SUBROUTINE GTRIGI
C ----------------------------------------------------------------------
C. -  B. BLOCH - 850910
C
C   event initialisation routine to replace Geant default
C
C   Calls   ASIEVE        from this .HLB
C   Called from <GXINT>GXCONT     from GEANT3
C ---------------------------------------------------------------------
#ifndef DOC
      CALL ASIEVE
      END
#endif
      SUBROUTINE GUKINE
C ----------------------------------------------------------------------
C. -  B. BLOCH - 850910
C
C  Event   generation control routine to replace Geant default
C   Calls   ASKINE        from this .HLB
C   Called from <GXINT>GXCONT     from GEANT3
C ---------------------------------------------------------------------
#ifndef DOC
C
C  Generate kinematics
C
      CALL ASKINE
      END
#endif
      SUBROUTINE UGINIT
C ----------------------------------------------------------------------
C. -  F.Ranjard - 950425
C
C  job initialization to replace Geant default
C   Calls   AIGAL        from this .HLB
C   Called from <GXINT>GXCONT     from GEANT3
C ---------------------------------------------------------------------
#ifndef DOC
C
C  GALEPH initialization
C
      CALL ASIGAL
      END
#endif
      SUBROUTINE UGLAST
C ----------------------------------------------------------------------
C. -  B. BLOCH - 850910
C
C  Job termination  routine to replace Geant default
C   Calls   GLAST       from this .HLB
C   Called from <GXINT>GXCONT     from GEANT3
C -----------------------------------------------------------------------
#ifndef DOC
C
C Termination routine
C
      CALL GLAST
C
      RETURN
      END
#endif
      SUBROUTINE GUVIEW ( NAME ,TXT1 ,TXT2 ,IV )
C------------------------------------------------------------------
C B. BLOCH-DEVAUX  - 850910
C
C   Draw some typical cut view of ALEF  with header and text
C   If option 'STAN' selected then draw standards plots (X-Y and R-Z views)
C   If not,draw plot of detector 'NAME' in cut view  at cutval value
C   determined by answers.A comment is added at the bottom
C   Replaces default GEANT GUVIEW routine
C   Called by <GXINT>GINC1
C   Calls UCTOH,UHTOC                 from cernlib
C   Calls GDOPT,GDRAWT,GDRAWC,GDHEAD  from GEANT3
C   Calls TVNEXT,TVSHOW               from GD3
C   Calls ZCGETI,ZCGETA,ZCGETR        from ZCEDEX
C   Calls ADXY,ADRZ,ADVIEW         from this .HLB
C -------------------------------------------------------------------
#ifndef DOC
#include "iocom.h"
#include "gcdraw.h"
      CHARACTER*4 ISTAC
      CHARACTER*20 TEXT
      CALL UCTOH(TXT1,ISTA,4,4)
      CALL UHTOC(ISTA,4,ISTAC,4)
      IF (ISTAC.NE.'STAN') THEN
      CALL GDOPT('THRZ','OFF ')
      IAXIS=IV
      CALL KUPROI(' Give axis number   ',IAXIS)
      CALL KUPROR(' Give cut value  ',CUTVA)
      CALL KUPROC(', Give text comment with quotes   ',TEXT,NCHI)
      CALL ICLRWK(0,0)
      XTEXT=1.*GPLTRX/20.
      YTEXT=1.*GPLTRY/20.
      CSIZE=0.4*GPLTRY/20.
      IF (IAXIS.EQ.1)
     1  CALL GDRAWT(XTEXT,YTEXT,'SIDE  VIEW $',CSIZE,0.,2,-1)
      IF (IAXIS.EQ.2)
     1  CALL GDRAWT(XTEXT,YTEXT,'TOP   VIEW $',CSIZE,0.,2,-1)
      IF (IAXIS.EQ.3)
     1  CALL GDRAWT(XTEXT,YTEXT,'FRONT VIEW $',CSIZE,0.,2,-1)
      IF (IAXIS.EQ.4)     THEN
       CALL GDRAWT(XTEXT,YTEXT,'R-Z  VIEW $',CSIZE,0.,2,-1)
       CALL GDOPT('THRZ','ON  ')
       IAXIS=1
      ENDIF
CC
      XTEXT=10.*GPLTRX/20.
      IOPT=0
      CALL GDRAWT(XTEXT,YTEXT,TEXT ,CSIZE,0.,2,IOPT)
      CALL GDRAWC(NAME ,IAXIS,CUTVA,GU0,GV0,GSCU,GSCV)
      CALL GDXYZ(0)
      ISEL=111110
      CALL GDHEAD(ISEL,'$',CSIZE)
      ELSE
      CALL ADXY(1)
      CALL ADRZ(2)
      CALL ADVIEW
      ENDIF
      RETURN
      END
#endif
#endif
