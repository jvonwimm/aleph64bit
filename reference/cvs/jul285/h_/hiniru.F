      SUBROUTINE HINIRU
C--------------------------------------------------------------------
C! Initialise the HCAL run
C! G.Capon
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "bcs.h"
#include "rflags.h"
#include "rlunit.h"
#include "rcurnt.h"
#include "hcalg1.h"
#include "hdeach.h"
#include "hcinjj.h"
#include "hcuejj.h"
#include "hlayjj.h"
#include "hntojj.h"
#include "hwnojj.h"
#include "hdigch.h"
      DIMENSION JCORA(12),JCORB(12),IPOW(23)
C
      INTEGER ALGTDB
      EXTERNAL ALGTDB
      DIMENSION ARRAY(14)
      EQUIVALENCE (HT0LAY,ARRAY(1))
      PARAMETER (TG15=0.267949)
      PARAMETER (IEDEF1=1000,IEDEF2=200)
      CHARACTER*20 LISTN
C
      DATA IWARN/0/
      DATA LISTN /'HCUEHLAYHCIN'/
      DATA JCORA/1,7,2,8,9,3,4,10,5,11,12,6/
      DATA JCORB/1,7,8,2,9,3,4,10,11,5,12,6/
      DATA IPOW/1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,
     +          16384,32768,65536,131072,262144,524288,1048576,
     +          2097152,4194304/
#include "bmacro.h"
C--------------------------------------------------------------------
C
C     read Hcal geometry from DataBase
C
      CALL HRDDAF(LRGEOM,IRUNRC,IFLAG)
      IF (IFLAG.EQ.0) THEN
         CALL RERROR('HINIRU',1,'error reading DataBase')
         GO TO 999
      ENDIF
C
C             print Hcal geometry data, if requested
C
      IF(JDBDRF(JULHC).GE.2 ) CALL HCPRGE
C
C HCAL towers calibration constants are read from the DATABASE
C
      KHOFF=NLINK('HOFF',0)
C HQOF exists if this identification is running outside julia
      KHQOF=NLINK('HQOF',0)
      IF(KHOFF.NE.0.OR.KHQOF.NE.0) THEN
      IF(IWARN.LT.3) THEN
       IWARN=IWARN+1
         WRITE(LOUTRL,*) ' *** HCAL CALIBRATION CONSTANTS NOT USED ***'
      ENDIF
      ELSE
         CALL HCALRD
      ENDIF
C
C    read HCAL interm. and time dep. calib. from Database
C
      CALL HRDCAL
C
C            fill common Hcalg1 for routines Ftrack, Htrack
C
      CALL HGETJU(ARRAY)
      HTSAMP=HCTGAP+HTSLAB
      HXBMIN=HXBMIN+HT0LAY+HCTGAP
      HTHCLE=HXBMAX-HXBMIN
      HYBMIN=HXBMIN*TG15
      HYBMAX=HXBMAX*TG15
      HZEMAX=HZEMIN+HTHCLE
      HZBMAX=HZEMIN+7.*HTSAMP
C
C Read NOISY channel from the Run Header or database
C
      JHNTO=IW(NAMIND('HNTO'))
      IF(JHNTO.EQ.0) THEN
         IFLAT = ALGTDB(LRGEOM,'HNTO',-IRUNRC)
      ENDIF
      JHWNO=IW(NAMIND('HWNO'))
      IF(JHWNO.EQ.0) THEN
         IFLAW = ALGTDB(LRGEOM,'HWNO',-IRUNRC)
      ENDIF
      IFLAG = ALGTDB(LRGEOM,LISTN,-IRUNRC)
      IF(JDBDRF(JULHC).GE.2 ) THEN
         WRITE(LDEBRL,*) ' Flag from DBASE ',IFLAT,IFLAW,IFLAG
         JHCIN=IW(NAMIND('HCIN'))
         IF(JHCIN.EQ.0) THEN
            NHCIN=0
         ELSE
            NHCIN=LROWS(JHCIN)
         ENDIF
         WRITE(LDEBRL,*) ' Number of inversions',NHCIN
         DO 101 I=1,NHCIN
            WRITE(LDEBRL,*) ITABL(JHCIN,I,JHCIWA),ITABL(JHCIN,I,JHCICA)
  101    CONTINUE
         JHNTO=IW(NAMIND('HNTO'))
         IF(JHNTO.EQ.0) THEN
            NHNTO=0
         ELSE
            NHNTO=LROWS(JHNTO)
         ENDIF
         WRITE(LDEBRL,*) ' Number of noisy towers',NHNTO
         DO 102 I=1,NHNTO
            WRITE(LDEBRL,*) ITABL(JHNTO,I,JHNTAD),ITABL(JHNTO,I,JHNTME)
  102    CONTINUE
         JHWNO=IW(NAMIND('HWNO'))
         IF(JHWNO.EQ.0) THEN
            NHWNO=0
         ELSE
            NHWNO=LROWS(JHWNO)
         ENDIF
         WRITE(LDEBRL,*) ' Number of noisy tube clusters',NHWNO
         DO 103 I=1,NHWNO
            WRITE(LDEBRL,*) ITABL(JHWNO,I,JHWNAD)
  103    CONTINUE
         JHLAY=IW(NAMIND('HLAY'))
         IF(JHLAY.EQ.0) THEN
            NHLAY=0
         ELSE
            NHLAY=LROWS(JHLAY)
         ENDIF
         WRITE(LDEBRL,*) ' Number of noisy tube layers',NHLAY
         DO 104 I=1,NHLAY
            WRITE(LDEBRL,*) ITABL(JHLAY,I,JHLAAM),ITABL(JHLAY,I,JHLADL)
  104    CONTINUE
         JHCUE=IW(NAMIND('HCUE'))
         IF(JHCUE.EQ.0) THEN
            NHCUE=0
         ELSE
            NHCUE=LROWS(JHCUE)
         ENDIF
         WRITE(LDEBRL,*) ' Energy cuts for noisy towers',NHCUE
         WRITE(LDEBRL,*) IW(JHCUE+LMHLEN+1),IW(JHCUE+LMHLEN+2)
      ENDIF
C
C               3 - missing buses (SGS boards) Iastr=101/171
C
      DO 14 N=1,36
 14   IHCBUS(N)=0
      JHLAY=IW(NAMIND('HLAY'))
      IF (JHLAY.NE.0) THEN
         DO 15 N=1,LROWS(JHLAY)
         IASTR=ITABL(JHLAY,N,1)
         NASTR=(IASTR-101)/2+1
         IBUS=ITABL(JHLAY,N,2)
         IHCBUS(NASTR)=IBSET(IHCBUS(NASTR),IBUS)
 15      CONTINUE
      ENDIF
C
C Read the cuts for noisy towers or set the defaults
C
      NAHCUE=NAMIND('HCUE')
      JHCUE=IW(NAHCUE)
      IF(JHCUE.EQ.0) THEN
         IECUT1=IEDEF1
         IECUT2=IEDEF2
      ELSE
         IECUT1=IW(JHCUE+LMHLEN+1)
         IECUT2=IW(JHCUE+LMHLEN+2)
      ENDIF
C
C            Prepares the dig. chains masks.
      DO 666 I = 1,23
      DO 665 J = 1,36
      MASK(J,I) = 1
 665  CONTINUE
 666  CONTINUE
C    get the HCNF bank from the data base if 89 data (IRUNRC<6000)
C    and from the run record if 90... data (IRUNRC>6000)
      IF (IRUNRC.LT.6000) THEN
         IFLAG = ALGTDB(LRGEOM,'HCNF',IRUNRC)
         IF(IFLAG.EQ.0) GO TO 999
      ENDIF
      NAHCNF = NAMIND('HCNF')
      JHCNF = IW(NAHCNF)
      IF(JHCNF.EQ.0) GO TO 999
      KHCBA = JHCNF + 7
      KHCEA = KHCBA + 13
      KHCEB = KHCEA + 13
C
      NUMB = IW(KHCBA)
      DO 700 I = 12,1,-1
      IBCH = NUMB/IPOW(I)
      IRES = IBCH*IPOW(I)
      IF(IBCH.EQ.0) THEN
          DO 680 J=1,23
          MASK(I+12,J) = 0
 680      CONTINUE
      ELSE
          NUMB1 = IW(KHCBA+I)
          DO 690 J = 23,1,-1
          IBCH1 = NUMB1/IPOW(J)
          IRES1 = IBCH1*IPOW(J)
          MASK(I+12,J) = IBCH1
          NUMB1 = NUMB1 - IRES1
 690      CONTINUE
      END IF
      NUMB = NUMB - IRES
 700  CONTINUE
C
      NUMB = IW(KHCEA)
      DO 800 I = 12,1,-1
      IBCH = NUMB/IPOW(I)
      IRES = IBCH*IPOW(I)
      IF(IBCH.EQ.0) THEN
          DO 780 J=1,23
          MASK(JCORA(I),J) = 0
 780      CONTINUE
      ELSE
          NUMB1 = IW(KHCEA+I)
          DO 790 J = 22,1,-1
          IBCH1 = NUMB1/IPOW(J)
          IRES1 = IBCH1*IPOW(J)
          MASK(JCORA(I),J+1) = IBCH1
          NUMB1 = NUMB1 - IRES1
 790      CONTINUE
          MASK(JCORA(I),1) = 0
      END IF
      NUMB = NUMB - IRES
 800  CONTINUE
C
      NUMB = IW(KHCEB)
      DO 900 I = 12,1,-1
      IBCH = NUMB/IPOW(I)
      IRES = IBCH*IPOW(I)
      IF(IBCH.EQ.0) THEN
          DO 880 J=1,23
          MASK(JCORB(I)+24,J) = 0
 880      CONTINUE
      ELSE
          NUMB1 = IW(KHCEB+I)
          DO 890 J = 22,1,-1
          IBCH1 = NUMB1/IPOW(J)
          IRES1 = IBCH1*IPOW(J)
          MASK(JCORB(I)+24,J+1) = IBCH1
          NUMB1 = NUMB1 - IRES1
 890      CONTINUE
          MASK(JCORB(I)+24,1) = 0
      END IF
      NUMB = NUMB - IRES
 900  CONTINUE
C
 999  CONTINUE
      RETURN
      END
#endif
