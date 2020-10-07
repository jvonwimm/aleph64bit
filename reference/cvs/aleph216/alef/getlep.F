      SUBROUTINE GETLEP(IRUN,IFOUN,IFILL,NV,ELEP,XYZ,DXYZ)
C----------------------------------------------------------------------
C! Gets the LEP energy and beam position for run IRUN
C  Author  J.Boucrot  15-June-1990
CKEY ALEF LFIL
C Input argument :
C  IRUN = run number to be searched in above bank
C Output arguments :
C   IFOUN = 0 if no information found for run IRUN
C         = 1 if information found in 'LFIL' bank of ADBSCONS DAF
C         = 2 if information found in the RUN Header banks
C             ( WARNING ! in this case , the quantities described
C               below have PROVISIONAL  values ! )
C   ELEP  = LEP center-of-mass energy for this run , in GEV/C2
C
C The following arguments are defined only if IFOUN.GT.0  :
C   IFILL = LEP fill number  for this run :
C     NV  = number of hadronic events used to compute XYZ , DXYZ
C    XYZ  = X,Y,Z values of beam crossing for this run , in cm
C   DXYZ  = Errors on XYZ , in cm
C
C Description:
C get the information from the LFIL data base bank.
C if unsuccessful , get it from the SOR bank : JSUM.
C
C        ENTRY GETOFS :
C        SUBROUTINE GETOFS(IRUN,OFSET)
C
C  Must be called after GETLEP
C  Output argument :
C    OFSET = Average D0 for this run ( Last word of bank LFIL )
C            used for systematic effects on vertex position
C----------------------------------------------------------------------
#ifndef DOC
      REAL XYZ(*),DXYZ(*)
#include "bcs.h"
#include "jsumjj.h"
#include "lfiljj.h"
      SAVE OFST
#include "bmacro.h"
C----------------------------------------------------------------------
      IFOUN = 0
C
C get LEP energy and the row # in LFIL bank for run # IRUN
C
      ELEP = ALEFIL (IRUN,JLFIL,IROW)
      JLFIL = IABS (JLFIL)
      IF (JLFIL.GT.0 .AND. IROW.GT.0) THEN
C
C    'LFIL' bank found : get the output arguments
         IFOUN=1
         KLFIL=KROW(JLFIL,IROW)
         IFILL=IW(KLFIL+JLFILF)
         NV   = IW(KLFIL+JLFINV)
         DO 100 IX=1,3
            XYZ(IX) = RW(KLFIL+JLFIBX+IX-1)
            DXYZ(IX) =RW(KLFIL+JLFIEX+IX-1)
 100     CONTINUE
         OFST=RW(KLFIL+JLFIOF)
      ELSE
C
C     No 'LFIL' bank . Try to find the run header bank 'JSUM' :
C
         JJSUM=NLINK('JSUM',IRUN)
         IF (JJSUM.EQ.0) GO TO 999
         IFOUN=2
         NV= ITABL (JJSUM,1,JJSUNV)
         IF (NV .GE. 2)  THEN
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
      ENDIF
      GO TO 999
C-----------------------------------------------------------------------
      ENTRY GETOFS(IRUN,OFSET)
      OFSET=OFST
C
 999  RETURN
      END
#endif
