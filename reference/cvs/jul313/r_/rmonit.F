      SUBROUTINE RMONIT
C--------------------------------------------------
C!    Write file for monitoring
C
C     Modified:  S. Wasserbaech  17 Apr 1991
C     1. Now uses PYER and PYFR instead of
C     DMVE to get event vertex information.
C     2. The numbers of + and - tracks are
C     taken from DHEA rather than counting
C     them from PFRF.  (The numbers had been
C     interchanged.)
C     3. The summed charged track energies
C     are computed from PFRF rather than DMFT.
C
C!   Modified :- E. Lancon             29-MAY-1993
C!      Add protection against crazy PYFR bank
C!             - F.Ranjard             10-May-1994
C!      remove call to TDXMON
C!             - M.Cattaneo            27-Oct-1999
C!      Increase max PMOM to 150 GeV
C?
C!======================================================================
#ifndef DOC
#include "alcons.h"
#include "pfrfjj.h"
#include "rmonco.h"
#include "revent.h"
#include "hcjosu.h"
#include "rlunit.h"
#include "bcs.h"
#include "monejj.h"
#include "dheajj.h"
#include "pyerjj.h"
#include "pyfrjj.h"
#include "evehjj.h"
#include "pfrtjj.h"
#include "mcadjj.h"
#include "pestjj.h"
#include "esdajj.h"
#include "phstjj.h"
#include "ecobjj.h"
#include "plsdjj.h"
#include "pecojj.h"
#include "phcojj.h"
#include "rtimes.h"
#include "etdijj.h"
#include "revhjj.h"
#include "fridjj.h"
      REAL ELAY(3)
      DATA LATUB/0/,LAPAT/0/,LAHIT/0/
#include "bmacro.h"
      CALL AUBOS('MONE',0,LMONEA+LMHLEN,KMONE,IGARB)
      IF(IGARB.EQ.2)GOTO 999
      IW(KMONE+1)=LMONEA
      IW(KMONE+2)=1
      IMONE=KMONE+LMHLEN
      KEVEH=IW(NAMIND('EVEH'))
      IF(KEVEH.NE.0)THEN
        IW(IMONE+JMONRN)=IW(KEVEH+JEVERN)
        IW(IMONE+JMONEV)=IW(KEVEH+JEVEEV)
        IW(IMONE+JMONM1)=IW(KEVEH+JEVEM1)
        IW(IMONE+JMONM2)=IW(KEVEH+JEVEM2)
        IW(IMONE+JMONM3)=IW(KEVEH+JEVEM3)
        IW(IMONE+JMONM4)=IW(KEVEH+JEVEM4)
        IW(IMONE+JMONER)=IW(KEVEH+JEVEES)
      ENDIF
      KDHEA=IW(NAMIND('DHEA'))
      IF(KDHEA.NE.0)THEN
        IDHEA=KDHEA+LMHLEN
        IW(IMONE+JMONNX)=IW(IDHEA+JDHENX)
        IW(IMONE+JMONNP)=IW(IDHEA+JDHENP)
        IW(IMONE+JMONNN)=IW(IDHEA+JDHENM)
        IW(IMONE+JMONNJ)=IW(IDHEA+JDHENJ)
      ENDIF
C
      NVPO = 0
      NVNE = 0
      KPYFR=IW(NAMIND('PYFR'))
      KPFRF=IW(NAMIND('PFRF'))
      IF ((KPYFR .NE. 0) .AND. (KPFRF .NE. 0)) THEN
        IF(IW(KPYFR).GT.2)THEN
          NREL=IW(KPYFR+LMHROW)
          DO 5 I=1,NREL
            IPFRF=ITABL(KPYFR,I,JPYFTN)
            IF ( IPFRF.GT.LROWS(KPFRF) ) THEN
              CALL RERROR ('RMONIT', -1,
     &          ' PFRF TRACK NUMBER OUT OF RANGE IN PYFR')
              GOTO 5
            ELSE
              IPYER=ITABL(KPYFR,I,JPYFVN)
              IF (IPYER .EQ. 1) THEN
                R=RTABL(KPFRF,IPFRF,JPFRIR)
                IF(R.LT.0.)NVPO=NVPO+1
                IF(R.GT.0.)NVNE=NVNE+1
              ENDIF
            ENDIF
    5     CONTINUE
        ENDIF
      ENDIF
      IW(IMONE+JMONVP)=NVPO
      IW(IMONE+JMONVM)=NVNE
C
      NTPC=0
      KPTCO=IW(NAMIND('PTCO'))
      IF(KPTCO.NE.0)NTPC=IW(KPTCO+LMHROW)
      IW(IMONE+JMONNT)=NTPC
      NTPB=0
      KPTBC=IW(NAMIND('PTBC'))
      IF(KPTBC.NE.0)NTPB=IW(KPTBC+LMHROW)
      IW(IMONE+JMONNB)=NTPB
      NITC=0
      KPIDI=IW(NAMIND('PIDI'))
      IF(KPIDI.NE.0)NITC=IW(KPIDI+LMHROW)
      IW(IMONE+JMONNI)=NITC
      NVDET=0
      KPVCO=IW(NAMIND('PVCO'))
      IF(KPVCO.NE.0)NVDET=IW(KPVCO+LMHROW)
      IW(IMONE+JMONNV)=NVDET
      NMHIT=0
      KMHIT=IW(NAMIND('MHIT'))
      IF(KMHIT.NE.0)NMHIT=IW(KMHIT+LMHROW)
      IW(IMONE+JMONNM)=NMHIT
C
C        used hits
C
      NUT=0
      NUI=0
      NUV=0
      NUM=0
      NELC=0
      NMUC=0
      NGOT=0
      NTI=0
      NTT=0
      NTB=0
      KPFRT=IW(NAMIND('PFRT'))
      KFRID=IW(NAMIND('FRID'))
      KPFRF=IW(NAMIND('PFRF'))
      IF(KPFRT.NE.0)THEN
        NTRAC=IW(KPFRT+LMHROW)
        DO 10 I=1,NTRAC
          NUT=NUT+ITABL(KPFRT,I,JPFRNT)+ITABL(KPFRT,I,JPFRNR)
          NUI=NUI+ITABL(KPFRT,I,JPFRNI)+ITABL(KPFRT,I,JPFRNE)
          NUV=NUV+ITABL(KPFRT,I,JPFRNV)
C     Electron candidates
C
          IF(KFRID.NE.0)THEN
            IF(RTABL(KFRID,I,JFRIPE).GT..9.AND.
     +             RTABL(KFRID,I,JFRIPI).LT..5)NELC=NELC+1
            IF(RTABL(KFRID,I,JFRIPM).GT..01.AND.
     +             RTABL(KFRID,I,JFRIPI).LT..01)NMUC=NMUC+1
          ENDIF
C
C      Good tracks
C
          IF(KPFRF.NE.0.)THEN
            IF(ABS(RTABL(KPFRF,I,JPFRD0)).LT.1..AND.
     +        ABS(RTABL(KPFRF,I,JPFRZ0)).LT.5.)THEN
              NGOT=NGOT+1
C
C       TPC,ITC, or both?
C
              IF(KPFRT.NE.0.)THEN
                IF(ITABL(KPFRT,I,JPFRNI).GT.0.AND.
     +            ITABL(KPFRT,I,JPFRNT).GT.0)NTB=NTB+1
                IF(ITABL(KPFRT,I,JPFRNI).GT.0)NTI=NTI+1
                IF(ITABL(KPFRT,I,JPFRNT).GT.0)NTT=NTT+1
              ENDIF
            ENDIF
          ENDIF
C
   10   CONTINUE
      ENDIF
      NMUO = 0
      KMCAD=IW(NAMIND('MCAD'))
      IF(KMCAD.NE.0)THEN
        NMUO=IW(KMCAD+LMHROW)
        DO 20 I=1,NMUO
          NUM=NUM+ITABL(KMCAD,I,JMCANH)+ITABL(KMCAD,I,JMCANH+1)
   20   CONTINUE
      ENDIF
      IW(IMONE+JMONUT)=NUT
      IW(IMONE+JMONUI)=NUI
      IW(IMONE+JMONUV)=NUV
      IW(IMONE+JMONUM)=NMUO
      IW(IMONE+JMONEE)=NELC
      IW(IMONE+JMONMU)=NMUC
      IW(IMONE+JMONGT)=NGOT
      IW(IMONE+JMONGT+1)=NTB
      IW(IMONE+JMONGT+2)=NTI
      IW(IMONE+JMONGT+3)=NTT
C
C     ECAL storeys
C
      EEST=0.
      NEST=0
      NESTR=0
      ELAY(1)=0.
      ELAY(2)=0.
      ELAY(3)=0.
      EEA=0.
      EEB=0.
      KPEST=IW(NAMIND('PEST'))
      KETDI=IW(NAMIND('ETDI'))
      IF(KPEST.NE.0.AND.KETDI.NE.0)THEN
        NEST=IW(KPEST+LMHROW)
        E=0.
        DO 30 I=1,NEST
          E=RTABL(KPEST,I,JPESER)
          KSTO=ITABL(KPEST,I,JPESKS)
          ITOW=ITABL(KPEST,I,JPESET)
          ISTA=ITABL(KPEST,I,JPESKS)
          IF(ITOW.NE.0)THEN
            IF(ISTA.GE.1.AND.ISTA.LE.3)THEN
              EEST=EEST+E
              ELAY(ISTA)=ELAY(ISTA)+E
              IADD=ITABL(KETDI,ITOW,JETDTL)
              ITHE=IBITS(IADD,16,8)
              NESTR=NESTR+1
              IF(ITHE.LT.51)EEA=EEA+E
              IF(ITHE.GT.178)EEB=EEB+E
            ELSE
              CALL RERROR('RMONIT',1,
     1          ' PROBLEM IN PEST BANK, STOREY LEVEL OUTSIDE RANGE')
            ENDIF
          ENDIF
   30   CONTINUE
      ENDIF
      IW(IMONE+JMONNE)=NESTOR
      RW(IMONE+JMONES)=EEST
      RW(IMONE+JMONE1)=ELAY(1)
      RW(IMONE+JMONE2)=ELAY(2)
      RW(IMONE+JMONEA)=EEA
      RW(IMONE+JMONEB)=EEB
C
C     HCAL storeys
C
      EHST=0.
      NHST=0
      HEA=0.
      HEB=0.
      KPHST=IW(NAMIND('PHST'))
      IF(KPHST.NE.0)THEN
        NHST=IW(KPHST+LMHROW)
        E=0.
        DO 40 I=1,NHST
          E=RTABL(KPHST,I,JPHSCE)
          EHST=EHST+E
          ITHE=ITABL(KPHST,I,JPHSTI)
          IF(ITHE.LE.13)HEA=HEA+E
          IF(ITHE.GE.50)HEB=HEB+E
   40   CONTINUE
      ENDIF
      IW(IMONE+JMONNH)=NHST
      RW(IMONE+JMONHS)=EHST
      RW(IMONE+JMONHA)=HEA
      RW(IMONE+JMONHB)=HEB
C
C     LCAL storeys
C
      IL=0
      NLST=0
      ILA=0
      ILB=0
      I1=0
      I2=0
      I3=0
      IL=0
      KPLSD=IW(NAMIND('PLSD'))
      IF(KPLSD.NE.0)THEN
        NLST=IW(KPLSD+LMHROW)
        DO 45 I=1,NLST
          I1=ITABL(KPLSD,I,JPLSE1)
          I2=ITABL(KPLSD,I,JPLSE2)
          I3=ITABL(KPLSD,I,JPLSE3)
          IL=IL+I1+I2+I3
          IPLI=ITABL(KPLSD,I,JPLSAD)
          IF(IPLI.LT.1024)ILA=ILA+I1+I2+I3
          IF(IPLI.GE.1024)ILB=ILB+I1+I2+I3
   45   CONTINUE
      ENDIF
      IW(IMONE+JMONNL)=NLST
      RW(IMONE+JMONLS)=FLOAT(IL)*.005
      RW(IMONE+JMONLA)=FLOAT(ILA)*.005
      RW(IMONE+JMONLB)=FLOAT(ILB)*.005
C
C     ECAL object
C
      KPECO=IW(NAMIND('PECO'))
      EPECO=0.
      EPLCO=0.
      EPECC=0.
      ENEUT=0.
      NECO=0
      NLCO=0
      IF(KPECO.NE.0)THEN
        E=0.
        NPECO=IW(KPECO+LMHROW)
        DO 50 I=1,NPECO
          E=RTABL(KPECO,I,JPECER)
          IF(ITABL(KPECO,I,JPECKD).EQ.192)THEN
            EPLCO=EPLCO+E
            NLCO=NLCO+1
          ELSE
            EPECO=EPECO+E
            NECO=NECO+1
            IBI=ITABL(KPECO,I,JPECRB)
            IF(IBI.EQ.1.OR.IBI.EQ.3)EPECC=EPECC+E
          ENDIF
   50   CONTINUE
      ENDIF
      IW(IMONE+JMONEL)=NECO
      RW(IMONE+JMONCE)=EPECO
      RW(IMONE+JMONQE)=EPECC
      IW(IMONE+JMONLL)=NLCO
      RW(IMONE+JMONCL)=EPLCO
C
C     HCAL object
C
      KPHCO=IW(NAMIND('PHCO'))
      EPHCO=0.
      EPHCC=0.
      NHCO=0
      IF(KPHCO.NE.0)THEN
        NHCO=IW(KPHCO+LMHROW)
        E=0.
        DO 60 I=1,NHCO
          E=RTABL(KPHCO,I,JPHCER)
          EPHCO=EPHCO+E
          IBI=ITABL(KPHCO,I,JPHCRB)
          IF(IBI.EQ.1.OR.IBI.EQ.3)EPHCC=EPHCC+E
   60   CONTINUE
      ENDIF
      IW(IMONE+JMONHL)=NHCO
      RW(IMONE+JMONCH)=EPHCO
      RW(IMONE+JMONQH)=EPHCC
      RW(IMONE+JMONEO)=EPHCO-EPHCC+EPECO-EPECC
C
C     charged tracks
C
      KDHEA=IW(NAMIND('DHEA'))
      IF(KDHEA.NE.0)THEN
        IDHEA=KDHEA+LMHLEN
        RW(IMONE+JMONEF)=RW(IDHEA+JDHEEF)
      ENDIF
C
C     Vertex
C
      RW(IMONE+JMONX0)=0.
      RW(IMONE+JMONY0)=0.
      RW(IMONE+JMONZ0)=0.
      KPYER=IW(NAMIND('PYER'))
      IF(KPYER.NE.0)THEN
        IF(IW(KPYER).GT.2)THEN
          IPYER=KPYER+LMHLEN
          RW(IMONE+JMONX0)=RW(IPYER+JPYEVX)
          RW(IMONE+JMONY0)=RW(IPYER+JPYEVY)
          RW(IMONE+JMONZ0)=RW(IPYER+JPYEVZ)
        ENDIF
      ENDIF
C
C     V0
C
      NYV0V=0
      KYV0V=IW(NAMIND('YV0V'))
      IF(KYV0V.NE.0)THEN
        NYV0V=IW(KYV0V+LMHROW)
        CHMIN=99999.
        KY=KYV0V+LMHLEN
        DO 63 IV=1,NYV0V
          CH=RW(KY+26)
          IF(CH.LT.CHMIN)THEN
            CHMIN=CH
            RW(IMONE+JMONPF)=RW(KY+21)
            RW(IMONE+JMONTH)=RW(KY+22)
          ENDIF
          KY=KY+IW(KYV0V+LMHCOL)
   63   CONTINUE
      ENDIF
      IW(IMONE+JMONN0)=NYV0V
C
      RW(IMONE+JMONNS)=NSUPEB
C
      RW(IMONE+JMONSN)=NSUPEC
      RW(IMONE+JMONFL)=SIZERE/4.
C
C     HCAL digital readout
C     watch out - they are comulative
C
      RW(IMONE+JMONWC)=NTUBHC-LATUB
      RW(IMONE+JMONDP)=NPATHC-LAPAT
      RW(IMONE+JMONHP)=NHITHC-LAHIT
      LATUB=NTUBHC
      LAPAT=NPATHC
      LAHIT=NHITHC
      KPEWI=IW(NAMIND('PEWI'))
      IF ( KPEWI.EQ.0) KPEWI = IW(NAMIND('PWEI'))
      EEWIRE=0.
      EWA=0.
      EWB=0.
      EWBA=0.
      IF(KPEWI.NE.0)THEN
        IF(IW(KPEWI).GT.2)THEN
          NMOD=IW(KPEWI+LMHROW)
          DO 85 I=1,NMOD
            DO 83 M=1,45
              MODU=ITABL(KPEWI,I,1)
              E=ITABL(KPEWI,I,1+M)/1000000.
              EEWIRE=EEWIRE+E
              IF(MODU.LT.13)EWA=EWA+E
              IF(MODU.GT.24)EWB=EWB+E
              IF(MODU.GT.12.AND.MODU.LT.25)EWBA=EWBA+E
   83       CONTINUE
   85     CONTINUE
        ENDIF
      ENDIF
      RW(IMONE+JMONEW)=EEWIRE
      RW(IMONE+JMONW1)=EWA
      RW(IMONE+JMONW2)=EWBA
      RW(IMONE+JMONW3)=EWB
C     Tracks
C
      KPFRF=IW(NAMIND('PFRF'))
      EPOS=0.
      ENEG=0.
      IF(KPFRF.NE.0)THEN
        NTR=IW(KPFRF+LMHROW)
        DO 70 I=1,NTR
          R = RTABL(KPFRF,I,JPFRIR)
          IF (ABS(R) .GT. 1.E-6) THEN
            PXY = -ALFIEL(DUM)*CLGHT*1.E-5/R
            PMOM = ABS(PXY)*SQRT(1.+RTABL(KPFRF,I,JPFRTL)**2)
            ICH = INT(SIGN(1.,PXY))
            IF(ICH.EQ.1.AND.PMOM.LT.150.)EPOS=EPOS+PMOM
            IF(ICH.EQ.-1.AND.PMOM.LT.150.)ENEG=ENEG+PMOM
          ENDIF
   70   CONTINUE
      ENDIF
      RW(IMONE+JMONEP)=EPOS
      RW(IMONE+JMONEM)=ENEG
      RW(IMONE+JMONEC)=EPOS+ENEG
C  PCPA energy
C
      CALL CMONI(EPPOS,EPTOT)
      EPPOS=EPPOS+EPOS+ENEG
      EPTOT=EPTOT+EPOS+ENEG
      RW(IMONE+JMONPH)=EPPOS
      RW(IMONE+JMONET)=EPTOT
C
C        T0 FROM ECAL
C
      CALL TIZERO ( TMEAN ,IGOO )
      RW(IMONE+JMONTE)=TMEAN
      CALL ALTIME(TIM)
      RW(IMONE+JMONTI)=TIM-AEVSRT
C       Detector and JULIA status bits
C
      KREVH=IW(NAMIND('REVH'))
      IF(KREVH.NE.0)THEN
        IW(IMONE+JMONDS)=IW(KREVH+LMHLEN+JREVDS)
        IW(IMONE+JMONF1)=IW(KREVH+LMHLEN+JREVFE)
        IW(IMONE+JMONF2)=IW(KREVH+LMHLEN+JREVFE+1)
        IW(IMONE+JMONN1)=IW(KREVH+LMHLEN+JREVNE)
        IW(IMONE+JMONN2)=IW(KREVH+LMHLEN+JREVNE+1)
      ENDIF
C
C    dE/dx monitoring
C
      RW(IMONE+JMONDE)=1.
C
C     T0 MONITORING
C
      CALL TPCT0(IER,DT)
      IF(IER.NE.0)DT=999999.
      RW(IMONE+JMONTD)=DT
  999 CONTINUE
      RETURN
      END
#endif
