      SUBROUTINE PRDTMC
C ----------------------------------------------------------------
C! Print DTMC and DVMC banks in Readable Format
CKEY PRINT MINI KINE / USER
C  G. Ganis                   - 950325 -
C  Modification of PRFKIN
C -----------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
      PARAMETER(JDTMPX=1,JDTMPY=2,JDTMPZ=3,JDTMMA=4,JDTMPA=5,JDTMOV=6,
     +          JDTMEV=7,JDTMHC=8)
      PARAMETER(JDVMVX=1,JDVMVY=2,JDVMVZ=3,JDVMIP=4,JDVMVN=5,JDVMVM=6)
      CHARACTER*4 TVOL, TMEC, CHAINT, NAME(3)
      PARAMETER (NVMEC=31)
      DIMENSION VMEC(NVMEC)
      CHARACTER*4 VMEC
      DATA VMEC /
     &  'NEXT','MULS','LOSS','FIEL','DCAY','PAIR','COMP','PHOT',
     &  'BREM','DRAY','ANNI','HADR','ECOH','EVAP','FISS','ABSO',
     &  'ANNH','CAPT','EINC','INHE','MUNU','TOFM','PFIS','SCUT',
     &  'RAYL','NONE','PRED','LOOP','NULL','STOP','SHOW' /
      DATA NDTMC /0/
#include "bmacro.h"
C ----------------------------------------------------------------
C Initialization
      IF (NDTMC.EQ.0) THEN
        LOUT = IW(6)
        NDTMC = NAMIND('DTMC')
        NDVMC = NAMIND('DVMC')
        NPART = NAMIND('PART')
        CALL KIAVER (AVER,IPROG)
      ENDIF
      IF (IW(NDTMC).EQ.0.AND.IW(NDVMC).EQ.0)THEN
        WRITE (LOUT,'(/1X,''+++PRDTMC+++ NO DTMC/DVMC bank - RETURN'')')
        RETURN
      ENDIF
C
C - get 'PART' bank index , The name of the part# ITYP is in
C                           ITABL(JPART,ITYP,2):ITABL(JPART,ITYP,4)
      JPART = IW(NPART)
      IF (JPART .EQ. 0) THEN
        WRITE (LOUT,'(/1X,''+++PRDTMC+++ NO PART bank so no name '')')
      ENDIF
C
C - Printing of bank DVMC  ------------------------------------------
C
      JDVMC=IW(NDVMC)
      IF (JDVMC.EQ.0) GO TO 999
      NUMV=LROWS(JDVMC)
      JDTMC=IW(NDTMC)
      NUMK=LROWS(JDTMC)
      WRITE (LOUT,1020)
 1020 FORMAT(/1X,'+++ PRDTMC +++   DVMC  Vertex bank' //
     &  T2,'Number',19X,'Vx',9X,'Vy',9X,'Vz',7X,'input track #',/)
      IUNREC= 0
      DO 200 IV=1,NUMV
        KDVMC=KROW(JDVMC,IV)
        TVOL=CHAINT(ITABL(JDVMC,IV,JDVMVN))
        VX =  IW(KDVMC+1)/10000.
        VY =  IW(KDVMC+2)/10000.
        VZ =  IW(KDVMC+3)/10000.
        JIP=  IW(KDVMC+JDVMIP)
        IF (JDVMVM.LE.LCOLS(JDVMC)) THEN
          IMEC = ITABL(JDVMC,IV,JDVMVM)
          IF( IMEC.LE.0 .OR. IMEC.GT.NVMEC ) THEN
            IUNREC= 1
            TMEC= '    '
          ELSE
            TMEC = VMEC(IMEC)
          END IF
        ENDIF
        WRITE (LOUT,1001) IV,TVOL,TMEC,VX,VY,VZ,JIP
  200 CONTINUE
 1001   FORMAT (1X,I4,1X,A4,1X,A4,6X,3(F8.2,3X),5X,I4)
        IF( IUNREC.EQ.1 ) THEN
          WRITE(LOUT,1002)
 1002     FORMAT(/1X,
     &'PRDTMC - DVMC contains some UNKNOWN vertex mechanism index',/)
        END IF
C
C Printing of bank DTMC -------------------------------------------
C
      IF (AVER .GE. 9.0) THEN
        WRITE (LOUT,1030)
 1030   FORMAT(/1X,'+++PRDTMC+++  DTMC  kinematics bank '//T3,
     &    'Number',2X,'Particle',9X,'Px',9X,'Py',9X,'Pz',7X,'Mass',
     &    T69,'Origin',T77,'End',T85,'Input',T93,'History',/
     &    T69,'Vertex',T77,'Vertex',T85,'track#',T93,' Code')
      ELSE
        WRITE (LOUT,1031)
 1031   FORMAT(/1X,'+++PRDTMC+++  DTMC  kinematics bank '//T3,
     &    'Number',2X,'Particle',9X,'Px',9X,'Py',9X,'Pz',5X,'Energy',
     &    T69,'Origin',T77,'End',T85,'Input',T93,'History',/
     &    T69,'Vertex',T77,'Vertex',T85,'track#',T93,' Code')
      ENDIF
C
      DO 300 ITK=1,NUMK
        KDTMC=KROW(JDTMC,ITK)
        ITYP=IW(KDTMC+JDTMPA)
        IF (JPART .GT. 0) THEN
          DO 30 J=1,3
            NAME(J) = CHAINT(ITABL(JPART,ITYP,J+1))
   30     CONTINUE
        ENDIF
        IVPAR=IW(KDTMC+JDTMOV)
        IVSEC=IW(KDTMC+JDTMEV)
        IF (IVPAR.EQ.0) GO TO 300
        IF (JPART.EQ.0) THEN
          WRITE (LOUT,1013 ) ITK
     &       ,(IW(KDTMC+II)/1000.,II=1,4),IVPAR,IVSEC
     &       ,ITABL(JDVMC,IVPAR,JDVMIP),IW(KDTMC+JDTMHC)
        ELSE
          WRITE (LOUT,1014 ) ITK,NAME
     &       ,(IW(KDTMC+II)/1000.,II=1,4),IVPAR,IVSEC
     &       ,ITABL(JDVMC,IVPAR,JDVMIP),IW(KDTMC+JDTMHC)
        ENDIF
  300 CONTINUE
C
  999 RETURN
 1013 FORMAT(T3,I4,16X,4(F10.3),T69,I4,T77,I4,T85,I4,T93,I6)
 1014 FORMAT(T3,I4,4X,3A4,4(F10.3),T69,I4,T77,I4,T85,I4,T93,I6)
      END
#endif
