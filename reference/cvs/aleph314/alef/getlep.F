      SUBROUTINE GETLEP(IRUN,IFOUN,IFILL,NV,ELEP,XYZ,DXYZ)
C----------------------------------------------------------------------
C! Gets the LEP energy and beam position for run IRUN
C  Author  J.Boucrot  15-June-1990
C  Completely rewritten 22-Sep-1998 to give always the best possible
C             available LEP energy and beam position for LEP1 and LEP2
CKEY ALEF LFIL
C Input argument :
C  IRUN = run number to be searched in above bank
C Output arguments :
C   IFOUN = 0 if no information found for run IRUN
C         = 1 if information found in 'LFIL' bank of ADBSCONS DAF
C             (from the so-called "Toby Burnett" method 
C              for old LEP1 processings)
C         = 2 if information found in the RUN Header banks RLEP,JSUM
C             ( WARNING ! in this case , the quantities described
C               below have PROVISIONAL  values ! )
C         = 3 all other cases 
C   ELEP  = LEP center-of-mass energy for this run , in GEV/C2
C
C The following arguments are defined only if IFOUN.GT.0  :
C   IFILL = LEP fill number  for this run :
C     NV  = number of hadronic events used to compute XYZ , DXYZ
C           for the LFIL bank (meaningful only for old LEP1 
C           processings, kept for backward-compatibility)
C    XYZ  = X,Y,Z values of beam crossing for this run , in cm
C   DXYZ  = Errors on XYZ , in cm
C
C----------------------------------------------------------------------
C        ENTRY GETOFS :
C        SUBROUTINE GETOFS(IRUN,OFSET)
C
C  Must be called after GETLEP - meaningful only if IFLAG = 1
C  Kept only for backward-compatibility for old LEP1 processings
C       
C  Output argument :
C    OFSET = Average D0 for this run ( Last word of bank LFIL )
C            used for systematic effects on vertex position
C            with the "Toby Burnett" method for old LEP1 processings
C          = 0. in all other cases
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
      PARAMETER ( NRUMC = 2000 , NRL11 = 17891 , NRL12 = 37853 ) 
      PARAMETER ( NRLP2 = 41400 , EMIN = 80. )
      REAL XYZ(*),DXYZ(*)
      REAL XLYZ(3),DXLYZ(3),DXYS(3) 
      COMMON / GETFLGS /  IGETEN , IGETXY
      COMMON / GETLVOU /  ICHNNK , VT_SIZE(3)
#include "bcs.h"
#include "jsumjj.h"
#include "lfiljj.h"
#include "rlepjj.h"
      LOGICAL FIRST
      DATA    FIRST / .TRUE. /
#include "bmacro.h"
C----------------------------------------------------------------------
      IFOUN=0
      ELEP=0.
      ELFIL=0.
      IFILL=0
      JLFIL=0 
      IGETEN=0
      IGETXY=0
      DO 1 I=1,3
         XYZ(I)=0. 
         DXYZ(I)=0.
         VT_SIZE(I)=0.
 1    CONTINUE
      OFST=0.
C Find run IRUN in bank 'LFIL' :
      ELFIL = ALEFIL (IRUN,JLFIL,IROW)
      JLFIL = IABS (JLFIL)
      IF (JLFIL.GT.0 .AND. IROW.GT.0) THEN
C
C    'LFIL' bank found : get the output arguments
         IFOUN=1
         KLFIL=KROW(JLFIL,IROW)
         IFILL=IW(KLFIL+JLFILF)
         NV   = IW(KLFIL+JLFINV)
         DO 10 IX=1,3
             XLYZ(IX) = RW(KLFIL+JLFIBX+IX-1)
            DXLYZ(IX) = RW(KLFIL+JLFIEX+IX-1)
 10      CONTINUE
         OFST=RW(KLFIL+JLFIOF)
      ENDIF
      JRLEP=IW(NAMIND('RLEP'))
      IF (IFILL.EQ.0) THEN
         IF (JRLEP.GT.0) IFILL=ITABL(JRLEP,1,JRLELF)
      ENDIF
C----------------------------------------------------------------------
C
C Get the best possible available value of the LEP energy .
C
C The way this value is found is described by IGETEN :
C IGETEN = 0  no energy found
C        = 1  from the 'RLEP' bank (always the case for MCarlo)
C        = 2  from the 'LFIL' bank                             
C        = 3  from the run-averaged values of the LEP Working Group
C        = 4  from the 15' time-slices     of the LEP Working Group
C ---------------------------------------------------------------------
C MCARLO runs : energy always taken from the 'RLEP' bank :
      IF (IRUN.LT.NRUMC) GO TO 80
C
C LEP1 runs from 1989 to 1992 included (run # < 17891 ):
C The only possibility is from the 'LFIL' bank :
C
      IF (IRUN.LT.NRL11) GO TO 50
C
C LEP1 runs from 1993 to 1995 included :
C the run-averaged values of the LEP Working Group should exist
C with a reasonable value; otherwise take the value from 'LFIL' :
C
      IF (IRUN.GT.NRL12) GO TO 20                        
      CALL GETLE1(ELEP,IFLE1)
      IF (IFLE1.EQ.2) THEN
         IF (ELEP.LT.EMIN) GO TO 50 
         IFOUN=3
         IGETEN=3
         GO TO 200
      ELSE
         GO TO 50   
      ENDIF
C
C LEP 1.5 Runs : no run-averaged energy, only 'LFIL' :
C
 20   IF (IRUN.LT.NRLP2) GO TO 50
C
C LEP 2 runs : one has time-chunk energy values for 
C data taken in previous years ;
C if no time-chunk available, one should have a run-averaged value;
C for the current year one has only 'LFIL' or 'RLEP' :
C
      CALL GETLE2(ELEP,IFLE2)
      IF (IFLE2.EQ.1) THEN
         IF (ELEP.LT.EMIN) GO TO 50
         IFOUN=3
         IGETEN=4
         GO TO 200
      ELSE IF (IFLE2.EQ.2) THEN
         IFOUN=3
         IGETEN=3
         GO TO 200
      ELSE
         GO TO 50
      ENDIF
C
C get LEP energy from the 'LFIL' bank for run # IRUN
C
 50   IF (IFOUN.EQ.0) GO TO 80        
      ELEP=ELFIL
      IGETEN=2 
      GO TO 200
C
C No 'LFIL' bank : use 'RLEP' as last try (always the case for MCarlo)
C
 80   IF (JRLEP.LE.0) GO TO 999
      IFOUN=2
      ELEP=0.002*FLOAT(ITABL(JRLEP,1,JRLELE))
      IGETEN=1
 200  IF (IFOUN.EQ.0) GO TO 999
C -------------------------------------------------------------------
C Beam position and errors - 
C
C Try to get the best possible one from subroutine getbp
C which uses event-chunk positions for real data
C and a simulation of it for Monte-Carlo data
C
C If getbp fails, use old-fashioned 'RXYZ' run-by-run positions 
C for old LEP1 processings
C
C If 'RXYZ' fails, try 'LFIL' for old LEP1 processings  
C
C If nothing from 'LFIL' , try the run-header bank 'JSUM' 
C
C The way this value is found is described by IGETXY :
C IGETXY = 0  no beam position found
C        = 1  from the 'JSUM' bank
C        = 2  from the 'LFIL' bank
C        = 3  from the 'RXYZ' bank
C        = 4  from getbp (event-chunk beam position)
C
C -------------------------------------------------------------------
C On first pass, initialize getbp :
      IF (FIRST) THEN
         FIRST = .FALSE.
         ICHUNK = 0
         ICHNNK = 0
      ENDIF
C Call the event-chunk routine to get the beam position:
      CALL ABRUEV(KRUN,KEVT)
      IF (KEVT.LE.0) GO TO 300
      CALL GETBP(IRUN,KEVT,ICHUNK,XYZ,DXYZ,DXYS)
      IF (ICHUNK.GT.0) THEN 
         ICHNNK=ICHUNK
         CALL UCOPY(DXYS(1),VT_SIZE(1),3)  
         IGETXY=4
         GO TO 999
      ENDIF
C No beam-chunk position. If one exists from 'RXYZ', get it :
 300  CALL GETXYB(IRUN,IFXYB,IFL,XYZ,DXYZ,OFST,VL)
      IF (IFXYB.GT.0) THEN
         IGETXY=3
         GO TO 999
      ENDIF
C No position from 'RXYZ'. If one exists from 'LFIL', get it :
      IF (JLFIL.GT.0) THEN
         CALL UCOPY(XLYZ(1),XYZ(1),3)
         CALL UCOPY(DXLYZ(1),DXYZ(1),3)
         IGETXY=2
         GO TO 999
      ENDIF 
C
C     No 'LFIL' bank . Try to find the run header bank 'JSUM' :
C
      JJSUM=NLINK('JSUM',IRUN)
      IF (JJSUM.EQ.0) GO TO 999
      NV= ITABL (JJSUM,1,JJSUNV)
      IF (NV .GE. 2)  THEN
         IGETXY=1
C        compute XYZ and errors :
         V = REAL (NV)
         XYZ(1) = RTABL(JJSUM,1,JJSUXV) / V
         XYZ(2) = RTABL(JJSUM,1,JJSUYV) / V
         XYZ(3) = RTABL(JJSUM,1,JJSUZV) / V
         DXYZ(1)= (RTABL(JJSUM,1,JJSUXS) / V - XYZ(1)**2) / (V - 1.)
         DXYZ(2)= (RTABL(JJSUM,1,JJSUYS) / V - XYZ(2)**2) / (V - 1.)
         DXYZ(3)= (RTABL(JJSUM,1,JJSUZS) / V - XYZ(3)**2) / (V - 1.)
         DXYZ(1)= SQRT( MAX(DXYZ(1),0.0) )
         DXYZ(2)= SQRT( MAX(DXYZ(2),0.0) )
         DXYZ(3)= SQRT( MAX(DXYZ(3),0.0) )
         OFST=0.
      ENDIF
      GO TO 999
C-----------------------------------------------------------------------
      ENTRY GETOFS(IRUN,OFSET)
      OFSET=OFST
C
 999  RETURN
      END
#endif
