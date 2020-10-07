      SUBROUTINE YTOIRU(IRUNRC,FIELD)
C
C----------------------------------------------------------*
C!    Initialize the run for TOPOLOGY reconstruction
CKEY YTOP RUN
C!    Author :     M. Bosman      30/11/88
C!    Modified:  G.Lutz,M.Bosman  15/07/91
C!    Modified  :  G. Lutz   30/03/92
C!
C!    Description
C!    ===========
C!    This routine is called once at each RUN beginning to
C!    initialize the TOPOLOGY part of the reconstruction
C!    program.
C!    If the beam crossing constraint is required, it does not
C!    use anymore the PBCR bank to initialize the beam position
C!    but rather calls the routine GETLEP for real data
C!    or the bank KHVF for monte carlo data.
C!    Default values are used if no beam constraint is required
C!    or with a warning message if no proper values where found
C!    by GETLEP or in KHVF
C!
C!---------------------------------------------------------*
#ifndef DOC
#include "bcs.h"
#include "ybcrto.h"
#include "yparto.h"
#include "ysumto.h"
C!---------------------------------------------------------*
      EXTERNAL NAMIND
      EXTERNAL ALGTDB
      INTEGER ALGTDB
      DIMENSION DBCR(3)
      DIMENSION XYZ(3),DXYZ(3),OFS(3)
#include "bmacro.h"
C!---------------------------------------------------------*
C
C-- Define the logical unit for printout and data base
C
      LOUT = IW(6)
      LBASE = JUNIDB(0)
C
C-- Refresh the beam crossing parameters for this run
C-- First case, the beam profile constraint is required
      LBCRFD=.FALSE.
      IF(LVBCR0) THEN
        LBCRFD=.TRUE.
C-- intialize the beam crossing region
        IF(IRUNRC.GT.2000) THEN
C     real data
C       CALL GETXYB(IRUNRC,JFOUN,JFL,XYZ,DXYZ,OFS,VLUM)
C       WRITE(LOUT,*)' GETXYB CALLED: IRUNRC=',IRUNRC,' JFOUN=',JFOUN,
C    &  ' JFL=',JFL,' XYZ',(XYZ(II),II=1,3),' DXYZ',(DXYZ(II),II=1,3),
C    &  ' OFS',(OFS(II),II=1,3),' VLUM=',VLUM
          CALL GETLEP(IRUNRC,IFOUN,IFILL,NV,ELEP,BCROSS,DBCR)
          IF(IFOUN.EQ.0.OR.NV.LT.10) THEN
           WRITE(LOUT,*)' YTOIRU: RUN',IRUNRC,' IFOUN=',IFOUN,' NV=',NV,
     &         ' no LEP information use default values'
            GOTO 1
          ENDIF
#if defined(YDEBUG)
          IF(IFOUN.EQ.2) THEN
            WRITE(LOUT,*) ' YTOIRU: RUN',IRUNRC,' IFOUN=',IFOUN,
     &         ' LEP information from header record'
          ENDIF
#endif
          DO II=1,6
            VBCROS(II)=0
          ENDDO
CHANGED BY WM
          VBCROS(1)=DBCR(1)**2  + 0.03**2
          VBCROS(3)=DBCR(2)**2  + 0.002**2
C THE FOLLOWING LEFT UNCHANGED, I DID NOT KNOW WHAT TO DO
          VBCROS(6)=DBCR(3)**2*FLOAT(NV-1)
          VBCROS(6)=AMAX1(VBCROS(6),1.**2)
#if defined(YDEBUG)
          WRITE(LOUT,9895)IFOUN,(BCROSS(I),I=1,3),(VBCROS(I),I=1,6)
     &      ,(DBCR(I),I=1,3)
 9895     FORMAT (' IFOUN=',I8,' BCROSS',3F10.3/
     &      ' VBCROS',6E10.3/,'  DBCR  ',3E10.3)
#endif
        ELSE
C     MONTE CARLO DATA
          KKHVF = IW(NAMIND('KHVF'))
          IF(KKHVF.EQ.0) THEN
#if defined(YDEBUG)
            WRITE(LOUT,8737) IRUN
 8737       FORMAT(' YTOPOL : BANK KHVF MISSING FOR M.C. RUN ',I5,
     +        '  USE DEFAULT VALUES')
#endif
            GOTO 1
          ENDIF
          BCROSS(1)=0.
          BCROSS(2)=0.
          BCROSS(3)=0.
          VBCROS(1)=RW(KKHVF+LMHLEN+7)**2
          VBCROS(2)=.0
          VBCROS(3)=RW(KKHVF+LMHLEN+8)**2
          VBCROS(4)=.0
          VBCROS(5)=.0
          VBCROS(6)=RW(KKHVF+LMHLEN+9)**2
#if defined(YDEBUG)
          WRITE(LOUT,9897)IFILL,(BCROSS(I),I=1,3),(VBCROS(I),I=1,6)
 9897     FORMAT (' IFILL=',I8,' BCROSS',3F10.3/
     &      ' VBCROS',6E10.3)
#endif
        ENDIF
        GOTO 100
      ENDIF
C
C ----  beam crossing point has not been found, use defaults
    1 CONTINUE
      LBCRFD=.FALSE.
C ----  mark the error in case the beam crossing point
C ----  was requested and not found
      IF(LVBCR0) KYFLAG(1) = KYFLAG(1) + 1
C ----  Default values for the beam crossing point
C ----  are (0.,0.,0.) for x,y,z
C ----  The profile of the beam crossing region
C ----  is (350mus,12mus,1.2cm) for x,y,z
C ----  but set larger errors as default for first real data
C ----  1 cm in R , 5 cm in Z
      DO 20 I=1,3
        BCROSS(I) = 0.
   20 CONTINUE

      VBCROS(1) = 1.**2
      VBCROS(2) = 0.
      VBCROS(3) = 1.**2
      VBCROS(4) = 0.
      VBCROS(5) = 0.
      VBCROS(6) = 5.**2
C
C ----  Set the current field value
C
  100 BFIELD = FIELD
      RETURN
      END
#endif
