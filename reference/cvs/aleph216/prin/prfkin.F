      SUBROUTINE PRFKIN
C ----------------------------------------------------------------
C! Print FKIN and FVER banks in Readable Format
CKEY FYXX PRINT / USER
C  J. Boucrot  and J. Hilgart - 880529 -
#ifndef DOC
      SAVE
#include "bcs.h"
#include "fxjjpar.h"
      PARAMETER (MXDAU=10)
      INTEGER LUDAU(MXDAU)
      INTEGER FYLUHC
      CHARACTER*4 TVOL, TMEC, CHAINT, NAME(3)
      DATA NFKIN /0/
#include "bmacro.h"
C ----------------------------------------------------------------
C Initialization
      IF (NFKIN.EQ.0) THEN
         LOUT = IW(6)
         NFKIN = NAMIND('FKIN')
         NFVER = NAMIND('FVER')
         NPART = NAMIND('PART')
         CALL KIAVER (AVER,IPROG)
      ENDIF
      IF (IW(NFKIN).EQ.0.AND.IW(NFVER).EQ.0)THEN
        WRITE (LOUT,'(/1X,''+++PRFKIN+++ NO FKIN/FVER bank - RETURN'')')
        RETURN
      ENDIF
C
C - get 'PART' bank index , The name of the part# ITYP is in
C                           ITABL(JPART,ITYP,2):ITABL(JPART,ITYP,4)
      JPART = IW(NPART)
      IF (JPART .EQ. 0) THEN
         WRITE (LOUT,'(/1X,''+++PRFKIN+++ NO PART bank so no name '')')
      ENDIF
C
C - Printing of bank FVER  ---------------------------------------------
C
        JFVER=IW(NFVER)
        IF (JFVER.EQ.0) GO TO 999
        NUMV=LROWS(JFVER)
        JFKIN=IW(NFKIN)
        NUMK=LROWS(JFKIN)
        WRITE (LOUT,1020)
 1020 FORMAT(/1X,'+++ PRFKIN +++   FVER  Vertex bank' //
     &  T2,'Number',14X,'Vx',11X,'Vy',9X,'Vz',3X,'TOF nsec',
     & 3X,'input track #',T80,'first secondary',T100,'# of secondary'
     & /T80,'  track',T100,' tracks'/)
        DO 200 IV=1,NUMV
           KFVER=KROW(JFVER,IV)
           TVOL=CHAINT(ITABL(JFVER,IV,JFVEVN))
           IF (JFVEVM.LE.LCOLS(JFVER)) THEN
              TMEC = CHAINT(ITABL(JFVER,IV,JFVEVM))
           ELSE
              TMEC = '    '
           ENDIF
           IFTRK=IW(KFVER+JFVEIS)
           LTRK=IW(KFVER+JFVENS)
           IF (LTRK.LE.0) THEN
              IFTRK=0
           ELSE
              IFTRK = IFTRK + 1
           ENDIF
           WRITE (LOUT,1001) IV,TVOL,TMEC,(RW(KFVER+I),I=1,3)
     +     , RW(KFVER+JFVETO)*1.E9,IW(KFVER+JFVEIP),IFTRK,LTRK
 1001    FORMAT (T2,I4,1X,A4,1X,A4,4F11.4,4X,I4,T80,I4,T100,I4)
 200    CONTINUE
C
C Printing of bank FKIN -------------------------------------------
C
      IF (AVER .GE. 9.0) THEN
        WRITE (LOUT,1030)
1030    FORMAT(/1X,'+++PRFKIN+++  FKIN  kinematics bank '//T3,
     & 'Number',2X,'Particle',9X,'Px',9X,'Py',9X,'Pz',7X,'Mass',
     &  T69,'Origin',T77,'End',T85,'Input',T93,'History',
     &  T101,'# of',T112,'list of (or 1st)'/
     &  T69,'Vertex',T77,'Vertex',T85,'track#',T93,' Code',
     &  T101,'daughters',T112,'daughters'/)
      ELSE
        WRITE (LOUT,1031)
1031    FORMAT(/1X,'+++PRFKIN+++  FKIN  kinematics bank '//T3,
     & 'Number',2X,'Particle',9X,'Px',9X,'Py',9X,'Pz',5X,'Energy',
     &  T69,'Origin',T77,'End',T85,'Input',T93,'History',
     &  T101,'# of',T112,'list of (or 1st)'/
     &  T69,'Vertex',T77,'Vertex',T85,'track#',T93,' Code',
     &  T101,'daughters',T112,'daughters'/)
      ENDIF
C
        DO 300 ITK=1,NUMK
           KFKIN=KROW(JFKIN,ITK)
           ITYP=IW(KFKIN+JFKIPA)
           IF (JPART .GT. 0) THEN
              DO 30 J=1,3
                 NAME(J) = CHAINT(ITABL(JPART,ITYP,J+1))
 30           CONTINUE
           ENDIF
           IVPAR=IW(KFKIN+JFKIOV)
           IVSEC=IW(KFKIN+JFKIEV)
           IF (IVPAR.EQ.0) GO TO 300
           LDAU = 0
           NDAU = 0
           IFDAU = 0
           IF (IVSEC.GT.IVPAR) THEN
              NDAU=ITABL(JFVER,IVSEC,JFVENS)
              IFDAU=ITABL(JFVER,IVSEC,JFVEIS)
              IF (NDAU.GT.0) IFDAU = IFDAU+1
           ELSE
              IF (IW(KFKIN+JFKIHC).GT.0) NDAU=FYLUHC(ITK,MXDAU,LUDAU)
              LDAU = MIN (MXDAU,NDAU)
           ENDIF
           IF (LDAU.EQ.0) THEN
              IF (JPART.EQ.0) THEN
                 WRITE (LOUT,1013 ) ITK
     &          ,(RW(KFKIN+II),II=1,4),IVPAR,IVSEC
     &          ,ITABL(JFVER,IVPAR,JFVEIP),IW(KFKIN+JFKIHC),NDAU,IFDAU
              ELSE
                 WRITE (LOUT,1014 ) ITK,NAME
     &          ,(RW(KFKIN+II),II=1,4),IVPAR,IVSEC
     &          ,ITABL(JFVER,IVPAR,JFVEIP),IW(KFKIN+JFKIHC),NDAU,IFDAU
              ENDIF
           ELSE
              IF (JPART.EQ.0) THEN
                 WRITE (LOUT,1015 ) ITK
     &          ,(RW(KFKIN+II),II=1,4),IVPAR,IVSEC
     &          ,ITABL(JFVER,IVPAR,JFVEIP),IW(KFKIN+JFKIHC),NDAU
     &          ,(LUDAU(M),M=1,LDAU)
              ELSE
                 WRITE (LOUT,1016 ) ITK,NAME
     &          ,(RW(KFKIN+II),II=1,4),IVPAR,IVSEC
     &          ,ITABL(JFVER,IVPAR,JFVEIP),IW(KFKIN+JFKIHC),NDAU
     &          ,(LUDAU(M),M=1,LDAU)
              ENDIF
           ENDIF
 300   CONTINUE
C
 999  RETURN
1013  FORMAT(T3,I4,16X,4(F10.3),T69,I4,T77,I4,T85,I4,T93,I6,T101,
     &       I4,T112,I4)
1014  FORMAT(T3,I4,4X,3A4,4(F10.3),T69,I4,T77,I4,T85,I4,T93,I6,T101,I4
     &      ,T112,I4)
1015  FORMAT(T3,I4,16X,4(F10.3),T69,I4,T77,I4,T85,I4,T93,I6,T101,
     &       I4,T112,3I4:/(T112,3I4):/)
1016  FORMAT(T3,I4,4X,3A4,4(F10.3),T69,I4,T77,I4,T85,I4,T93,I6,T101,I4
     &      ,T112,3I4:/(T112,3I4):/)
      END
#endif
