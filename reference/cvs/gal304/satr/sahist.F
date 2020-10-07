*DK sahist
      SUBROUTINE SAHIST
C-----------------------------------------------------------------------
C!    Fill histograms for the SATR test run
C                                   H.Burkhardt   October 1986
C     called from SADIGI if SATR hist flag is set
C     fill track info scattered over three banks temporarily
C     in PP vector to avoid a lot of forward-backword pointing
C     the histogramming is of interest for the following questions:
C        in small angle BHABHA scattering
C           how many secondaries are produced by the beam pipe
C           what is their impact on the SATR Monitor
C           and by how much do they change the measurement of
C           cos theta
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "jqcom.h"
#include "jobcom.h"
#include "sanamc.h"
#include "satrco.h"
#include "wrkspc.h"
C     using the working space NPARM could be maximal 8000
      PARAMETER (NPARM=100)
      DIMENSION PP(10,NPARM)
      EQUIVALENCE (WSPACE(1),PP(1,1))
      CHARACTER NAME*4,CHAINT*4
      EXTERNAL NAMIND
      EXTERNAL CHAINT
      DATA ID/600/
#include "bmacro.h"
#include "kmacro.h"
      LOUT=IW(6)
      JVER = NLINK ('VERT',1)
      DO 1 I=1,NPARM
C       loop over vertex bank
        IF(JVER.EQ.0) GOTO 2
        NVX=IW(JVER-2)
        LHDR=IW(JVER+1)
        LPVER=IW(JVER+2)
        MXTRK=IW(JVER+3)
        KVER=JVER+LHDR
C       store the vertex info
C       one vertex can belong to a sequence of tracks, so loop
        DO 9 ITRK=IW(KVER+LPVER+1),IW(KVER+LPVER+MXTRK)
          IF(ITRK.GT.NPARM) GOTO 9
          PP(7,ITRK)=RW(KVER+1)
          PP(8,ITRK)=RW(KVER+2)
          PP(9,ITRK)=RW(KVER+3)
          RORG=SQRT(RW(KVER+1)**2+RW(KVER+2)**2)
          PP(10,ITRK)=RORG
    9   CONTINUE
        JVER=IW(JVER-1)
    1 CONTINUE
    2 CONTINUE
      JKIN = NLINK ('KINE',1)
C     count total number of charged and neutral tracks
      NCHAR=0
      NNEUT=0
      COSTSA=0.
      DO 3 I=1,NPARM
C       loop over kine bank
        IF(JKIN.EQ.0) GOTO 4
        NTRK=IW(JKIN-2)
        LHKIN=IW(JKIN+1)
        LPKIN=IW(JKIN+2)
        MXVX=IW(JKIN+3)
        KIN=JKIN+LHKIN
C       now store also the momentum/energy
        PP(1,NTRK)=RW(KIN+1)
        PP(2,NTRK)=RW(KIN+2)
        PP(3,NTRK)=RW(KIN+3)
        PP(4,NTRK)=ENERVK(JKIN)
        IF(NTRK.EQ.1) THEN
C         store cos theta of the original particle
          PTOT=SQRT(PP(1,1)**2+PP(2,1)**2+PP(3,1)**2)
          IF(PTOT.GT.0.) COSTSA=PP(3,1)/PTOT
        ENDIF
        ITYP=IW(KIN+5)
        IF(ITYP.EQ.1) THEN
          PP(5,NTRK)=0.0
          PP(6,NTRK)=0.
          NNEUT=NNEUT+1
        ELSEIF(ITYP.EQ.2) THEN
          PP(5,NTRK)=0.00051
          PP(6,NTRK)=1.
          NCHAR=NCHAR+1
        ELSE
          PP(5,NTRK)=0.00051
          PP(6,NTRK)=-1.
          NCHAR=NCHAR+1
        ENDIF
        JKIN=IW(JKIN-1)
    3 CONTINUE
    4 CONTINUE
      CALL HFILL(ID+1,FLOAT(NCHAR),0.,1.)
      CALL HFILL(ID+2,FLOAT(NNEUT),0.,1.)
C     point to IMPA bank (since NAIMPA seems wrong)
      JIMPA=NAMIND('IMPA')+1
C     count number of charged and neutral tracks in SATR acceptance
      NCHAA=0
      NNEUA=0
      DO 5 I=1,NPARM
C       loop over impact bank
        JIMPA=IW(JIMPA-1)
        IF(JIMPA.EQ.0) GOTO 6
        KIMPA=JIMPA+LMHLEN
        DO 7 J=1,LROWS(JIMPA)
          NAME=CHAINT(IW(KIMPA+1))
          IF(NAME.EQ.'SATR') THEN
C           point to info in KINE and PART bank
C           overwrite momentum and vertex for tracks hitting SATR
C           in the PP vector
            NR=IW(JIMPA-2)
            JKIN=NLINK('KINE',NR)
            LHKIN=IW(JKIN+1)
            LPKIN=IW(JKIN+2)
            MXVX=IW(JKIN+3)
            KIN=JKIN+LHKIN
            ITYP=IW(KIN+5)
            KPART=IW(NAPART)
            CHARG=RTABL(KPART,ITYP,7)
C           use position and momentum/energy when hitting SATR
            PP(1,NR)=RW(KIMPA+5)*RW(KIMPA+8)
            PP(2,NR)=RW(KIMPA+6)*RW(KIMPA+8)
            PP(3,NR)=RW(KIMPA+7)*RW(KIMPA+8)
            PP(4,NR)=RW(KIMPA+8)
            PP(7,NR)=RW(KIMPA+2)
            PP(8,NR)=RW(KIMPA+3)
            PP(9,NR)=RW(KIMPA+4)
            PP(10,NR)=SQRT(PP(7,NR)**2+PP(8,NR)**2)
C           reverse sign of pp(10 to signal track as accepted
            IF(PP(4,NR).GT.0.1) THEN
              PP(10,NR)=-PP(10,NR)
              IF(PP(6,NR).EQ.0) THEN
                NNEUA=NNEUA+1
              ELSE
                NCHAA=NCHAA+1
              ENDIF
            ENDIF
            IF(FDEBJO.AND.IPRIJO(6).EQ.1)
     &        WRITE(LOUT,*)' +++SAHIST PVEC FOR TRACK',
     &                     I,(PP(IT,I),IT=1,10)
          ENDIF
          KIMPA=KIMPA+LCOLS(JIMPA)
    7   CONTINUE
    5 CONTINUE
    6 CONTINUE
      IF(NCHAA.GT.0) NCHAA=NCHAA-1
      CALL HFILL(ID+3,FLOAT(NCHAA),0.,1.)
      IF(NCHAA.GT.0) CALL HFILL(ID+16,ABS(COSTSA),0.,FLOAT(NCHAA))
      CALL HFILL(ID+4,FLOAT(NNEUA),0.,1.)
      DO 10 I=1,9
        CALL HFILL(ID+30+I,FLOAT(NHLASA(I)),0.,1.)
        NHLASA(I)=0
   10 CONTINUE
      DO 20 I=1,9
        CALL HFILL(ID+40+I,FLOAT(NHLDSA(I)),0.,1.)
        NHLDSA(I)=0
   20 CONTINUE
C     get number of remaining wires after oring from SADI bank
      KSADI=IW(NASADI)
      IF(KSADI.EQ.0) GOTO 900
      NWIR=LROWS(KSADI)
      NORE=NWIRSA-NWIR
      CALL HFILL(ID+26,FLOAT(NHITSA),0.,1.)
      CALL HFILL(ID+28,FLOAT(NLOSSA),0.,1.)
      CALL HFILL(ID+27,FLOAT(NWIRSA),0.,1.)
      CALL HFILL(ID+29,FLOAT(NORE)  ,0.,1.)
      IF (IPRIJO(6) .EQ. 1) THEN
         IF(FDEBJO .OR. XEVTSA.LE.100. .OR. NORE.GE.10) THEN
           WRITE(LOUTIO,777) NEVTJO,COSTA,NCHAR,NCHAA,NNEUT,NNEUA
           WRITE(LOUTIO,778) NHITSA,NLOSSA,NWIRSA,NORE
         ENDIF 
 777     FORMAT(' +++SAHIST #Evt=',I5,' CosTh=',F8.6,
     &   '    #Part. from Bpipe: #El(total/sec. accepted)=',2I3,
     &   '  #Gam(total/acc)=',2I3)
 778     FORMAT(' +++SAHIST',
     &   ' #Hits  total=',I3,' #lost in dead regions=',I2,4X,
     &   ' #Wires total=',I3,' #ored=',I2)
      ENDIF
      NHITSA=0
      NLOSSA=0
      IEMAX=0
      EMAX=0.
      DO 100 I=1,NTRK
        IF(PP(6,I).NE.0.AND.PP(4,I).GT.EMAX.AND.PP(10,I).LT.0.) THEN
          EMAX=PP(4,I)
          IEMAX=I
        ENDIF
 100  CONTINUE
      CALL HFILL(ID+10,ABS(COSTSA),0.,1.)
      COSTM=0.
      IF(IEMAX.GT.0) THEN
        CALL HFILL(ID+5,EMAX,0.,1.)
        RAMI=SQRT(PP(7,IEMAX)**2+PP(8,IEMAX)**2+PP(9,IEMAX)**2)
        IF(RAMI.GT.0.) COSTM=PP(9,IEMAX)/RAMI
        CALL HFILL(ID+13,ACOS(COSTM)-ACOS(COSTSA),0.,1.)
      ENDIF
      DO 110 I=1,NTRK
        RADI=SQRT(PP(7,I)**2+PP(8,I)**2+PP(9,I)**2)
        IF(RADI.EQ.0) GOTO 110
        COSTR=PP(9,I)/RADI
        DIST=SQRT( (PP(7,IEMAX)-PP(7,I))**2+(PP(8,IEMAX)-PP(8,I))**2+
     .             (PP(9,IEMAX)-PP(9,I))**2 )
        DISTT=ABS(SQRT(PP(7,IEMAX)**2+PP(8,IEMAX)**2)-
     .        SQRT(PP(7,I)**2+PP(8,I)**2))
        IF(PP(6,I).EQ.0) THEN
          CALL HFILL(ID+8,PP(4,I),0.,1.)
          IF(PP(10,I).LT.0.) THEN
            CALL HFILL(ID+9,PP(4,I),0.,1.)
            CALL HFILL(ID+14,DIST,PP(4,I),1.)
            CALL HFILL(ID+15,DISTT,PP(4,I),1.)
          ENDIF
        ELSEIF(I.NE.IEMAX) THEN
          CALL HFILL(ID+6,PP(4,I),0.,1.)
          IF(PP(10,I).LT.0.) THEN
            CALL HFILL(ID+7,PP(4,I),0.,1.)
            CALL HFILL(ID+11,DIST,PP(4,I),1.)
            CALL HFILL(ID+12,DISTT,PP(4,I),1.)
          ENDIF
        ENDIF
  110 CONTINUE
  900 CONTINUE
      END
#endif
