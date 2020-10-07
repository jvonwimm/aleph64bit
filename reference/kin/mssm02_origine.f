      subroutine mssmsusy
C ---------------------------------------------------------------------
C     This is the April 96 release of SUSYGEN - call it SUSYGEN_V2
C     Routines which have been modified are now in the mssm02.input file
C              and have been commented out with "CYG"
C ---------------------------------------------------------------------
C
CYG*CMZ :  1.00/00 14/04/95  18.46.23  by  Stavros Katsanevas
CYG*-- Author :    Stavros Katsanevas   14/04/95
CYG      PROGRAM MSSMSUSY
CYG
CYGCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CYGC  Version 1.00
CYGC
CYGC                                 Authors S.Katsanevas- S. Melachroinos
CYGC
CYGC Based essentially on the formulas of A.Bartl et al to be found in
CYGC
CYGC 1) Signatures for chargino production Z. Phys C. 30, 441-449 (1986)
CYGC 2) Production and decay of neutralinos Nuclear Physics B278 (1986) 1-25
CYGC 3) Gaugino-Higsino mixing in selectron-sneutrino pair production
CYGC                                       Z. Phys. C 34, 411-417 (1987)
CYGC 4) Production and decay of SUSY particles   HEPHYPUB-1991-566
CYGC 5) Chargino production at LEP200           Z. Phys C55, 257 (1992)
CYGC
CYGC
CYGCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CYG
CYG      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
CYG
CYG      real*4  rgmaum,rgmaur,rgm0,rgtanb,rgatri,rgma, 
CYG     +        rfmsq,rfmstopl,rfmstopr,
CYG     +        rfmsell,rfmselr,rfmsnu,rfmglu,recm,rflum,rvscan(6)
CYG      common /rkey/rgmaum,rgmaur,rgm0,rgtanb,rgatri,rgma, 
CYG     +        rfmsq,rfmstopl,rfmstopr,
CYG     +        rfmsell,rfmselr,rfmsnu,rfmglu,recm,rflum,rvscan 
CYG
CYG
CYG      common/steer/gmaum,gmaur,gm0,gtanb,gatri,
CYG     +fmsq,fmstopl,fmstopr,fmsell,fmselr,fmsnu,fmglu
CYG      common/mds/modes,MIX
CYG
CYG      logical zino,wino,sele,smuo,stau,snu,squa,stopa,sbota,higgs
CYG      common/sparc/ zino,wino,sele,smuo,stau,snu,squa,stopa,sbota,higgs
CYG      logical wrt,scan,lepi
CYG      common/str/wrt,scan,lepi
CYG      COMMON/KINEM/flum,ecm,s,roots,T,Q,Q2,EN(2)
CYG      COMMON/ISR/ QK(4)
CYG      COMMON /CONST/ idbg,igener,irad
CYG
CYG      COMMON/INDEXX/index,index1,index2,nevt
CYG      COMMON/FINDEX/fmpr1,fmpr2,XCROST,APRO
CYG      common/mixings/cosphimix,facqcd,fgama,spartmas,ratq
CYG
CYG      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
CYG     +FLC(12),FRC(12),gms(12),echar(12)
CYG
CYG      common/spartcl/fmal(12),fmar(12),ratqa(12),fgamc(12),fgamcr(12)
CYG     +,cosmi(12)
CYG
CYG
CYG      COMMON/XCROS/xgaug(8),xeta(8)
CYG      common/reorder/ispa(12,2),kl(2,18),klap(2,18),idecs(12,2)
CYG
CYG
CYG      DOUBLE PRECISION gM(50),gmu(50)
CYG      real*4 step1,step2,sma,scros(13),scrost,scrost1,scosa,amp
CYG      real*4 sela(6)
CYG
CYG
CYG      dimension iffl(12)
CYG      CHARACTER*5 LBLIN,LBLIN1,SSID
CYG
CYG      external gensel,gensmu,gensnue,gensnu,photi
CYG      external sigma
CYG      
CYG      common/brsum/ brsum(5,6,6),brsuma(6)
CYG
CYG      common/variables/fms,fmi,fmk,fml1,fml2,etai,etak,brspa(6,48),
CYG     +lind(6,6,6),brgaug(23,6,6),fmelt,fmert,fmelu,fmeru
CYG
CYG        logical rpar
CYG        common/rpari/rpar
CYG
CYG      common/higsel/ihigsel(5)
CYG
CYG
CYGc******************************************
CYGc STAGE 1        Read data cards          *
CYGc******************************************
CYGc
CYGc open data file
CYGc
CYG      icount=0
CYG      OPEN(1,FILE='susygen.dat',STATUS='NEW',FORM='FORMATTED')
CYG
CYG      call scards
CYG      call sbook
CYG
CYG
CYG
CYG
CYG
CYG
CYG
CYGc
CYGC scan
CYGC
CYG      icmu=rvscan(1)
CYG      step1=(rvscan(3)-rvscan(2))/float(icmu-1)
CYG      icM=rvscan(4)
CYG      step2=(rvscan(6)-rvscan(5))/float(icM-1)
CYG
CYG      if(icmu*icM.gt.0)then
CYG        do imu = 1,icmu
CYG        gmu(imu)=rvscan(2)+(imu-1)*step1
CYG        enddo
CYG        do iM = 1,icM
CYG        gM(iM)=rvscan(5)+(iM-1)*step2
CYG        enddo
CYG      endif
CYG
CYG
CYG      igeneralM=0
CYG   10 continue
CYG
CYG      igeneralM=igeneralM+1
CYG
CYG      if(scan)then
CYG        gmaum=gM(igeneralM)
CYG        rgmaum=real(gmaum)
CYG      endif
CYG
CYG      igeneralmu=0
CYG
CYG   20 continue
CYG
CYG      igeneralmu=igeneralmu+1
CYG
CYG      if(scan)then
CYG        gmaur=gmu(igeneralmu)
CYG        rgmaur=real(gmaur)
CYG      endif
CYG
CYG
CYGc******************************************
CYGc STAGE 2   Define masses and BR          *
CYGc******************************************
CYGc
CYG      S=ECM**2
CYG      ROOTS=DSQRT(S)
CYG      ebeam=ecm/2.
CYG
CYG      icount=icount+1
CYG
CYG      if(scan) print *,' M  ',gmaum,' mu ',gmaur,icount
CYG
CYG
CYG      CALL SUSANA(GMAUM,GMAUR,GTANB,Gm0,Gatri,mfail)
CYG
CYG
CYG      if(.not.rpar)call rparini
CYG
CYGc
CYGc plot masses....
CYGc
CYG
CYG      if(scan)then
CYG        do 30  k=1,3
CYG          ki=k
CYG          if(k.eq.3)ki=5
CYG          sma=xgaug(ki)
CYG   30   call hf2(400+k,rgmaur,rgmaum,sma)
CYG      endif
CYG
CYG      if(mfail.eq.1)go to 270
CYG
CYGc******************************************
CYGc STAGE 3   Calculate cross sections      *
CYGc           and generate                  *
CYGc******************************************
CYG
CYGc
CYGc choose the specific process
CYGc
CYG
CYGC
CYGC NEUTRALINOS
CYGC
CYG      if(zino)then
CYG        index=1
CYG        ihi=0
CYG        do 50 i=1,4
CYG          do 50 j=1,i
CYG            ihi=ihi+1
CYG            scros(ihi)=0.
CYG            index1=i+70
CYG            index2=j+70
CYG            fmpr1=ssmass(index1)
CYG            fmpr2=ssmass(index2)
CYG            xcrost=0.
CYG            if(fmpr1+fmpr2.gt.ECM)go to 50
CYG            if(irad.eq.0)xcrost=photi(index1,index2)
CYG            IF(IRAD.EQ.1) CALL REMT1(EBEAM,SIGMA)
CYG            nevt=xcrost*flum
CYG            if(xcrost.eq.0.)go to 50
CYG            lblin=ssid(index1)
CYG            lblin1=ssid(index2)
CYG            print *,LBLIN,LBLIN1,' sigma ',XCROST,' events ',NEVT
CYG            WRITE(1,10000) LBLIN,LBLIN1,XCROST,NEVT
CYG            scros(ihi)=xcrost*1000.
CYG
CYG            apro=2.d0
CYG            do 40 kap=1,20
CYG              cosa=-1.d0+(kap-1)*.1d0
CYG              AMP = GENPHO(index1,index2,cosa)/xcrost
CYG              if(amp.gt.apro)apro=amp
CYG              scosa=cosa
CYG   40       call hf1(1000+ihi,scosa,amp)
CYG
CYG            if(igener.eq.1) call mssm_gene
CYG
CYG   50   continue
CYG      endif
CYGC
CYGC CHARGINOS
CYGC
CYG      if(wino)then
CYG        index=2
CYG        ihi=10
CYG        do 70 i=1,2
CYG          do 70 j=1,i
CYG            ihi=ihi+1
CYG            scros(ihi)=0.
CYG            index1=(i+74)
CYG            index2=-(j+74)
CYG            fmpr1=ssmass(index1)
CYG            fmpr2=ssmass(index2)
CYG            xcrost=0.
CYG            if(fmpr1+fmpr2.gt.ECM)go to 70
CYG            if(irad.eq.0)xcrost=chargi(index1,index2)
CYG            IF(IRAD.EQ.1) CALL REMT1(EBEAM,SIGMA)
CYG            if(xcrost.eq.0.)go to 70
CYG            nevt=xcrost*flum
CYG            lblin=ssid(index1)
CYG            lblin1=ssid(index2)
CYG            print *,LBLIN,LBLIN1,' sigma ',XCROST,' events ',NEVT
CYG            WRITE(1,10000) LBLIN,LBLIN1,XCROST,NEVT
CYG            scros(ihi)=xcrost*1000.
CYG
CYG            apro=2.d0
CYG            do 60 kap=1,20
CYG              cosa=-1.d0+(kap-1)*.1d0
CYG              AMP = GENCHAR(index1,index2,cosa)/xcrost
CYG              if(amp.gt.apro)apro=amp
CYG              scosa=cosa
CYG   60       call hf1(1000+ihi,scosa,amp)
CYG
CYG            if(igener.eq.1) call mssm_gene
CYG
CYG   70   continue
CYG      endif
CYG
CYG      if(scan)then
CYGc
CYGc cross section scan x01x01, x01x02, x+1x-1
CYGc
CYG
CYG        do 80  ihi=1,3
CYG          khi=ihi
CYG          if(ihi.eq.3)khi=11
CYG   80   call hf2(100+ihi,rgmaur,rgmaum,scros(khi))
CYG
CYGc
CYGc BR scan of x01xo2
CYGc
CYG        sela(1)=brsum(1,1,2)*100.
CYG        sela(2)=brsum(2,1,2)*100.
CYG        sela(3)=brsum(3,1,2)*100.
CYG        sela(4)=brsum(4,5,2)*100.
CYG        sela(5)=brsum(5,5,2)*100.
CYG
CYG        do 90  loi=1,5
CYG   90   call hf2(500+loi,rgmaur,rgmaum,sela(loi))
CYG
CYGc
CYGc BR scan of x+1x-1
CYGc
CYG
CYG        sela(1)=brsum(4,1,5)*100.
CYG        sela(2)=brsum(5,1,5)*100.
CYG        sela(3)=brsum(4,2,5)*100.
CYG        sela(4)=brsum(5,2,5)*100.
CYG
CYG        do 100 loi=1,4
CYG  100   call hf2(600+loi,rgmaur,rgmaum,sela(loi))
CYG
CYG      endif
CYG
CYGC
CYGC SPARTICLES
CYGC
CYG      index=3
CYG
CYGC*******************************************************************************
CYG
CYG      if(snu)then
CYG        nfla=3
CYG        iffl(1)=3
CYG        iffl(2)=7
CYG        iffl(3)=11
CYG        do 120 ka=1,nfla
CYG          k=iffl(ka)
CYG          k1=mod(k-1,4)+1
CYG
CYG          do 120 l=1,1
CYG            scrost=0.
CYG            index1=ispa(k,l)
CYG            index2=-index1
CYG            if(index1.eq.0)go to 120
CYG
CYG            fmpr1=ssmass(index1)
CYG            fmpr2=ssmass(index2)
CYG            if(fmpr1+fmpr2.gt.ECM)go to 120
CYG
CYG            fgama=fgamc(k)
CYG            spartmas=fmpr1
CYG            ratq=ratqa(k)
CYG            cosphimix=1.
CYG            facqcd=0.
CYG
CYG            if(k.eq.3.and.irad.eq.0)xCROSt=ssdint(-1.d0,gensnue,1.D0)
CYG            if(k.ne.3.and.irad.eq.0)xCROSt=ssdint(-1.D0,gensnu,1.D0)
CYG            IF(IRAD.EQ.1) CALL REMT1(EBEAM,SIGMA)
CYG
CYG            scrost=xcrost*1000.
CYG            if(xcrost.eq.0.)go to 120
CYG
CYG            nevt=xcrost*flum
CYG            lblin=ssid(index1)
CYG            lblin1=ssid(index2)
CYG            print *,LBLIN,LBLIN1,' sigma ',XCROST,' events ',NEVT
CYG            WRITE(1,10000) LBLIN,LBLIN1,XCROST,NEVT
CYG
CYG            apro=1.
CYG            do 110 kap=1,20
CYG              cosa=-1.d0+(kap-1)*.1d0
CYG              scosa=cosa
CYG
CYG              if(k.eq.3)then
CYG                AMP = GENSNUE(cosa)/xcrost
CYG                if(amp.gt.apro)apro=amp
CYG              else
CYG                AMP = GENSNU(cosa)/xcrost
CYG                if(amp.gt.apro)apro=amp
CYG              endif
CYG
CYG              if(k.eq.3)then
CYG                call hf1(2001,scosa,amp)
CYG              endif
CYG
CYG              if(k.eq.7)then
CYG                call hf1(2002,scosa,amp)
CYG              endif
CYG
CYG  110       continue
CYG
CYG            if(scan)then
CYG              call hf2(200,rgmaur,rgmaum,scrost)
CYG              if(k.eq.3)then
CYG                scrost1=fmal(3)
CYG                call hf2(111,rgmaur,rgmaum,scrost1)
CYG                scrost= brspa(1,3)+brspa(2,3)+brspa(3,3)+brspa(4,3)+
CYG     +          brspa(5,3)+brspa(6,3)
CYG                scrost1=0.
CYG                if(scrost.ne.0.)scrost1=brspa(1,3)/scrost*100.
CYG                call hf2(301,rgmaur,rgmaum,scrost1)
CYG                if(scrost.ne.0.)scrost1=brspa(2,3)/scrost*100.
CYG                call hf2(302,rgmaur,rgmaum,scrost1)
CYG                if(scrost.ne.0.)scrost1=brspa(5,3)/scrost*100.
CYG                call hf2(303,rgmaur,rgmaum,scrost1)
CYG              endif
CYG            endif
CYG
CYG            if(igener.eq.1) call mssm_gene
CYG  120   continue
CYG      endif
CYG
CYGC*******************************************************************************
CYG      if(sele)then
CYG        nfla=1
CYG        iffl(1)=4
CYG
CYG        do 140 ka=1,nfla
CYG          k=iffl(ka)
CYG          k1=mod(k-1,4)+1
CYG          do 140 l=1,2
CYG            scrost=0.
CYG            index1=ispa(k,l)
CYG            index2=-index1
CYG            if(index1.eq.0)go to 140
CYG            fmpr1=ssmass(index1)
CYG            fmpr2=ssmass(index2)
CYG            if(fmpr1+fmpr2.gt.ECM)go to 140
CYG
CYG            spartmas=fmpr1
CYG            ratq=ratqa(k)
CYG            facqcd=0.
CYG
CYG            if(l.eq.1)then
CYG              fgama=fgamc(k)
CYG              cosphimix=1.d0
CYG            else
CYG              fgama=fgamcr(k)
CYG              cosphimix=0.
CYG            endif
CYG            if(irad.eq.0)xCROSt=ssdint(-1.d0,gensel,1.d0)
CYG            IF(IRAD.EQ.1) CALL REMT1(EBEAM,SIGMA)
CYG
CYG            scrost=xcrost*1000.
CYG            if(scrost.eq.0.)go to 140
CYG
CYG            nevt=xcrost*flum
CYG            lblin=ssid(index1)
CYG            lblin1=ssid(index2)
CYG            print *,LBLIN,LBLIN1,' sigma ',XCROST,' events ',NEVT
CYG            WRITE(1,10000) LBLIN,LBLIN1,XCROST,NEVT
CYG
CYG            apro=2.d0
CYG            do 130 kap=1,20
CYG              cosa=-1.d0+(kap-1)*.1d0
CYG              scosa=cosa
CYG              AMP = GENSEL(cosa)/xcrost
CYG              if(amp.gt.apro)apro=amp
CYG              call hf1(2002+l,scosa,amp)
CYG  130       continue
CYG
CYG            if(scan)then
CYG              IF(L.EQ.1)THEN
CYG              call hf2(201,rgmaur,rgmaum,scrost)
CYG                scrost= brspa(1,4)+brspa(2,4)+brspa(3,4)+brspa(4,4)+
CYG     +          brspa(5,4)+brspa(6,4)
CYG                scrost1=0.
CYG                if(scrost.ne.0.)scrost1=brspa(1,4)/scrost*100.
CYG                call hf2(311,rgmaur,rgmaum,scrost1)
CYG                if(scrost.ne.0.)scrost1=brspa(2,4)/scrost*100.
CYG                call hf2(312,rgmaur,rgmaum,scrost1)
CYG                if(scrost.ne.0.)scrost1=brspa(5,4)/scrost*100.
CYG                call hf2(313,rgmaur,rgmaum,scrost1)
CYG              scrost1=fmal(4)
CYG              call hf2(112,rgmaur,rgmaum,scrost1)
CYG              ELSE
CYG              call hf2(202,rgmaur,rgmaum,scrost)
CYG                scrost= brspa(1,16)+brspa(2,16)+brspa(3,16) +brspa(4,
CYG     +          16)+brspa(5,16)+brspa(6,16)
CYG                scrost1=0.
CYG                if(scrost.ne.0.)scrost1=brspa(1,16)/scrost*100.
CYG                call hf2(321,rgmaur,rgmaum,scrost1)
CYG                if(scrost.ne.0.)scrost1=brspa(2,16)/scrost*100.
CYG                call hf2(322,rgmaur,rgmaum,scrost1)
CYG                if(scrost.ne.0.)scrost1=brspa(5,16)/scrost*100.
CYG                call hf2(323,rgmaur,rgmaum,scrost1)
CYG              scrost1=fmar(4)
CYG              call hf2(113,rgmaur,rgmaum,scrost1)
CYG              ENDIF
CYG            endif
CYG
CYG            if(igener.eq.1) call mssm_gene
CYG
CYG  140   continue
CYG
CYG        index=3
CYG
CYG        index1=51
CYG        index2=-57
CYG        fmpr1=ssmass(index1)
CYG        fmpr2=ssmass(index2)
CYG        if(fmpr1+fmpr2.gt.ECM)go to 160
CYG        spartmas=fmpr1
CYG        ratq=ratqa(4)
CYG        cosphimix=1.
CYG        facqcd=0.
CYG        if(irad.eq.0)xCROSt=genselrs(dummy)
CYG        IF(IRAD.EQ.1) CALL REMT1(EBEAM,SIGMA)
CYG
CYG        scrost=xcrost*1000.
CYG        if(xcrost.eq.0.)go to 160
CYG
CYG        nevt=xcrost*flum
CYG        lblin=ssid(index1)
CYG        lblin1=ssid(index2)
CYG        print *,LBLIN,LBLIN1,' sigma ',XCROST,' events ',NEVT
CYG        WRITE(1,10000) LBLIN,LBLIN1,XCROST,NEVT
CYG
CYG        apro=2.d0
CYG        do 150 kap=1,20
CYG          cosa=-1.d0+(kap-1)*.1d0
CYG          scosa=cosa
CYG          AMP = GENSELR(cosa)/xcrost
CYG          if(amp.gt.apro)apro=amp
CYG          call hf1(2005,scosa,amp)
CYG  150   continue
CYG
CYG        if(scan)then
CYG          call hf2(203,rgmaur,rgmaum,scrost)
CYG        endif
CYG
CYG        if(igener.eq.1) call mssm_gene
CYG
CYG  160   continue
CYG
CYG      endif
CYG
CYGC*******************************************************************************
CYG
CYG      if(smuo)then
CYG        nfla=1
CYG        iffl(1)=8
CYG        do 180 ka=1,nfla
CYG
CYG          k=iffl(ka)
CYG          k1=mod(k-1,4)+1
CYG
CYG          do 180 l=1,2
CYG            scrost=0.
CYG            index1=ispa(k,l)
CYG            index2=-index1
CYG            if(index1.eq.0)go to 180
CYG
CYG            fmpr1=ssmass(index1)
CYG            fmpr2=ssmass(index2)
CYG
CYG            if(fmpr1+fmpr2.gt.ECM)go to 180
CYG
CYG            spartmas=fmpr1
CYG            ratq=ratqa(k)
CYG            facqcd=0.
CYG
CYG            if(l.eq.1)then
CYG              fgama=fgamc(k)
CYG              cosphimix=1.d0
CYG            else
CYG              fgama=fgamcr(k)
CYG              cosphimix=0.
CYG            endif
CYG
CYG
CYG            if(irad.eq.0)xCROSt=gensmus(dummy)
CYG            IF(IRAD.EQ.1) CALL REMT1(EBEAM,SIGMA)
CYG
CYG            scrost=xcrost*1000.
CYG            if(xcrost.eq.0.)go to 180
CYG
CYG            nevt=xcrost*flum
CYG            lblin=ssid(index1)
CYG            lblin1=ssid(index2)
CYG            print *,LBLIN,LBLIN1,' sigma ',XCROST,' events ',NEVT
CYG            WRITE(1,10000) LBLIN,LBLIN1,XCROST,NEVT
CYG
CYG            apro=2.d0
CYG            do 170 kap=1,20
CYG              cosa=-1.d0+(kap-1)*.1d0
CYG              scosa=cosa
CYG              AMP = GENSMU(cosa)/xcrost
CYG              if(amp.gt.apro)apro=amp
CYG              call hf1(2005+l,scosa,amp)
CYG  170       continue
CYG
CYG            if(igener.eq.1) call mssm_gene
CYG  180   continue
CYG      endif
CYG
CYGC*******************************************************************************
CYG
CYG
CYG
CYG      if(stau)then
CYG        nfla=1
CYG        iffl(1)=12
CYG        do 200 ka=1,nfla
CYG
CYG          k=iffl(ka)
CYG          k1=mod(k-1,4)+1
CYG
CYG          do 200 l=1,2
CYG            index1=ispa(k,l)
CYG            index2=-index1
CYG            if(index1.eq.0)go to 200
CYG
CYG            fmpr1=ssmass(index1)
CYG            fmpr2=ssmass(index2)
CYG            if(fmpr1+fmpr2.gt.ECM)go to 200
CYG
CYG            spartmas=fmpr1
CYG            ratq=ratqa(k)
CYG            facqcd=0.
CYG
CYG            if(l.eq.1)then
CYG              fgama=fgamc(k)
CYG            else
CYG              fgama=fgamcr(k)
CYG            endif
CYG
CYG
CYG            if(irad.eq.0)xCROSt=gensmus(dummy)
CYG
CYG            IF(IRAD.EQ.1) CALL REMT1(EBEAM,SIGMA)
CYG            nevt=xcrost*flum
CYG
CYG            scrost=xcrost*1000.
CYG            if(xcrost.eq.0.)go to 200
CYG
CYG            lblin=ssid(index1)
CYG            lblin1=ssid(index2)
CYG            print *,LBLIN,LBLIN1,' sigma ',XCROST,' events ',NEVT
CYG            WRITE(1,10000) LBLIN,LBLIN1,XCROST,NEVT
CYG
CYG            apro=2.d0
CYG            do 190 kap=1,20
CYG              cosa=-1.d0+(kap-1)*.1d0
CYG              scosa=cosa
CYG              AMP = GENSMU(cosa)/xcrost
CYG              if(amp.gt.apro)apro=amp
CYG              call hf1(2007+l,scosa,amp)
CYG  190       continue
CYG
CYG            if(igener.eq.1) call mssm_gene
CYG
CYG  200   continue
CYG      endif
CYG
CYG      if(sbota)then
CYG        nfla=1
CYG        iffl(1)=10
CYG        do 220 ka=1,nfla
CYG
CYG          k=iffl(ka)
CYG          k1=mod(k-1,4)+1
CYG
CYG          do 220 l=1,2
CYG            index1=ispa(k,l)
CYG            index2=-index1
CYG            if(index1.eq.0)go to 220
CYG
CYG            fmpr1=ssmass(index1)
CYG            fmpr2=ssmass(index2)
CYG            if(fmpr1+fmpr2.gt.ECM)go to 220
CYG
CYG
CYG
CYG            spartmas=fmpr1
CYG            ratq=ratqa(k)
CYG            facqcd=1.d0
CYG
CYG            if(l.eq.1)then
CYG              fgama=fgamc(k)
CYG            else
CYG              fgama=fgamcr(k)
CYG            endif
CYG
CYG
CYG            if(irad.eq.0)xCROSt=gensmus(dummy)
CYG
CYG            IF(IRAD.EQ.1) CALL REMT1(EBEAM,SIGMA)
CYG
CYG            scrost=xcrost*1000.
CYG            if(xcrost.eq.0.)go to 220
CYG
CYG            nevt=xcrost*flum
CYG            lblin=ssid(index1)
CYG            lblin1=ssid(index2)
CYG            print *,LBLIN,LBLIN1,' sigma ',XCROST,' events ',NEVT
CYG            WRITE(1,10000) LBLIN,LBLIN1,XCROST,NEVT
CYG
CYG            apro=2.d0
CYG            do 210 kap=1,20
CYG              cosa=-1.d0+(kap-1)*.1d0
CYG              scosa=cosa
CYG              AMP = GENSMU(cosa)/xcrost
CYG              if(amp.gt.apro)apro=amp
CYG              call hf1(2009+l,scosa,amp)
CYG  210       continue
CYG
CYG            if(scan)then
CYG              IF(L.EQ.1)THEN
CYG                call hf2(121,rgmaur,rgmaum,scrost)
CYG                scrost= brspa(1,10)+brspa(2,10)+brspa(3,10)+ brspa(4,
CYG     +          10)+brspa(5,10)+brspa(6,10)
CYG                scrost1=0.
CYG                if(scrost.ne.0.)scrost1=brspa(1,10)/scrost*100.
CYG                call hf2(331,rgmaur,rgmaum,scrost1)
CYG                if(scrost.ne.0.)scrost1=brspa(2,10)/scrost*100.
CYG                call hf2(332,rgmaur,rgmaum,scrost1)
CYG                if(scrost.ne.0.)scrost1=brspa(5,10)/scrost*100.
CYG                call hf2(333,rgmaur,rgmaum,scrost1)
CYG              ELSE
CYG                call hf2(122,rgmaur,rgmaum,scrost)
CYG                scrost= brspa(1,22)+brspa(2,22)+brspa(3,22)+ brspa(4,
CYG     +          22)+brspa(5,22)+brspa(6,22)
CYG                scrost1=0.
CYG                if(scrost.ne.0.)scrost1=brspa(1,22)/scrost*100.
CYG                call hf2(341,rgmaur,rgmaum,scrost1)
CYG                if(scrost.ne.0.)scrost1=brspa(2,22)/scrost*100.
CYG                call hf2(342,rgmaur,rgmaum,scrost1)
CYG                if(scrost.ne.0.)scrost1=brspa(5,22)/scrost*100.
CYG                call hf2(343,rgmaur,rgmaum,scrost1)
CYG              ENDIF
CYG
CYG              scrost1=fmal(10)
CYG              call hf2(114,rgmaur,rgmaum,scrost1)
CYG              scrost1=fmar(10)
CYG              call hf2(115,rgmaur,rgmaum,scrost1)
CYG
CYG            endif
CYG
CYG            if(igener.eq.1) call mssm_gene
CYG  220   continue
CYG      endif
CYG
CYG
CYG      if(stopa)then
CYG        nfla=1
CYG        iffl(1)=9
CYG
CYG        do 240 ka=1,nfla
CYG
CYG          k=iffl(ka)
CYG          k1=mod(k-1,4)+1
CYG
CYG          do 240 l=1,2
CYG            index1=ispa(k,l)
CYG            index2=-index1
CYG            if(index1.eq.0)go to 240
CYG            fmpr1=ssmass(index1)
CYG            fmpr2=ssmass(index2)
CYG
CYG
CYG            if(fmpr1+fmpr2.gt.ECM)go to 240
CYG
CYG            spartmas=fmpr1
CYG            ratq=ratqa(k)
CYG            facqcd=1.d0
CYG
CYG            if(l.eq.1)then
CYG              fgama=fgamc(k)
CYG            else
CYG              fgama=fgamcr(k)
CYG            endif
CYG
CYG            if(irad.eq.0)xCROSt=gensmus(dummy)
CYG            IF(IRAD.EQ.1) CALL REMT1(EBEAM,SIGMA)
CYG
CYG            scrost=xcrost*1000.
CYG            if(xcrost.eq.0.)go to 240
CYG            nevt=xcrost*flum
CYG            lblin=ssid(index1)
CYG            lblin1=ssid(index2)
CYG            print *,LBLIN,LBLIN1,' sigma ',XCROST,' events ',NEVT
CYG            WRITE(1,10000) LBLIN,LBLIN1,XCROST,NEVT
CYG
CYG            apro=2.d0
CYG            do 230 kap=1,20
CYG              cosa=-1.d0+(kap-1)*.1d0
CYG              scosa=cosa
CYG              AMP = GENSMU(cosa)/xcrost
CYG              if(amp.gt.apro)apro=amp
CYG              call hf1(2011+l,scosa,amp)
CYG  230       continue
CYG
CYG            if(scan)then
CYG              IF(L.EQ.1)THEN
CYG                call hf2(123,rgmaur,rgmaum,scrost)
CYG                scrost= brspa(1,9)+brspa(2,9)+brspa(3,9)+ brspa(4,9)+
CYG     +          brspa(5,9)+brspa(6,9)
CYG                scrost1=0.
CYG                if(scrost.ne.0.)scrost1=brspa(1,9)/scrost*100.
CYG                call hf2(351,rgmaur,rgmaum,scrost1)
CYG                if(scrost.ne.0.)scrost1=brspa(2,9)/scrost*100.
CYG                call hf2(352,rgmaur,rgmaum,scrost1)
CYG                if(scrost.ne.0.)scrost1=brspa(5,9)/scrost*100.
CYG                call hf2(353,rgmaur,rgmaum,scrost1)
CYG              ELSE
CYG                call hf2(124,rgmaur,rgmaum,scrost)
CYG                scrost= brspa(1,21)+brspa(2,21)+brspa(3,21)+ brspa(4,
CYG     +          21)+brspa(5,21)+brspa(6,21)
CYG                scrost1=0.
CYG                if(scrost.ne.0.)scrost1=brspa(1,21)/scrost*100.
CYG                call hf2(361,rgmaur,rgmaum,scrost1)
CYG                if(scrost.ne.0.)scrost1=brspa(2,21)/scrost*100.
CYG                call hf2(362,rgmaur,rgmaum,scrost1)
CYG                if(scrost.ne.0.)scrost1=brspa(5,21)/scrost*100.
CYG                call hf2(363,rgmaur,rgmaum,scrost1)
CYG              ENDIF
CYG              scrost1=fmal(9)
CYG              call hf2(116,rgmaur,rgmaum,scrost1)
CYG              scrost1=fmar(9)
CYG              call hf2(117,rgmaur,rgmaum,scrost1)
CYG            endif
CYG
CYG            if(igener.eq.1) call mssm_gene
CYG  240   continue
CYG      endif
CYG
CYG
CYGC**********************************************************
CYG      if(squa)then
CYG        nfla=4
CYG        iffl(1)=1
CYG        iffl(2)=2
CYG        iffl(3)=5
CYG        iffl(4)=6
CYG        do 260 ka=1,nfla
CYG
CYG          k=iffl(ka)
CYG          k1=mod(k-1,4)+1
CYG
CYG          do 260 l=1,2
CYG            index1=ispa(k,l)
CYG            index2=-index1
CYG            if(index1.eq.0)go to 260
CYG
CYG            fmpr1=ssmass(index1)
CYG            fmpr2=ssmass(index2)
CYG            if(fmpr1+fmpr2.gt.ECM)go to 260
CYG
CYG            spartmas=fmpr1
CYG            ratq=ratqa(k)
CYG            facqcd=1.d0
CYG
CYG            if(l.eq.1)then
CYG              fgama=fgamc(k)
CYG              cosphimix=1.d0
CYG            else
CYG              fgama=fgamcr(k)
CYG              cosphimix=0.
CYG            endif
CYG
CYG            if(irad.eq.0)xCROSt=gensmus(dummy)
CYG
CYG            IF(IRAD.EQ.1) CALL REMT1(EBEAM,SIGMA)
CYG
CYG            scrost=xcrost*1000.
CYG            if(xcrost.eq.0.)go to 260
CYG
CYG            nevt=xcrost*flum
CYG            lblin=ssid(index1)
CYG            lblin1=ssid(index2)
CYG            print *,LBLIN,LBLIN1,' sigma ',XCROST,' events ',NEVT
CYG            WRITE(1,10000) LBLIN,LBLIN1,XCROST,NEVT
CYG
CYG            apro=2.d0
CYG            do 250 kap=1,20
CYG              cosa=-1.d0+(kap-1)*.1d0
CYG              scosa=cosa
CYG              AMP = GENSMU(cosa)/xcrost
CYG              if(amp.gt.apro)apro=amp
CYG              call hf1(2013+l,scosa,amp)
CYG  250       continue
CYG
CYG            if(igener.eq.1) call mssm_gene
CYG  260   continue
CYG      endif
CYG
CYG  270 continue
CYG
CYG      if(higgs)then
CYG
CYG        do 300 kla=1,5
CYG
CYGc
CYGc use pythia to produce 
CYGc
CYGc XZ, X=h,H,A  1,2  
CYGc XX' x=h,H x'=A 3,4
CYGc H+H-  5
CYGc
CYG         if(ihigsel(kla).eq.0)go to 300
CYG
CYG            if(kla.eq.1.or.kla.eq.3)lblin=' h '
CYG            if(kla.le.2)lblin1=' Z '
CYG            if(kla.eq.2.or.kla.eq.4)lblin=' H '
CYG            if(kla.eq.3.or.kla.eq.4)lblin1=' A '
CYG            if(kla.eq.5)lblin='H+ '
CYG            if(kla.eq.5)lblin1='H- '
CYG
CYG        call higinit(kla,xcrost)
CYG
CYG            if(xcrost.eq.0.)go to 300
CYG            nevt=xcrost*flum
CYG            print *,LBLIN,LBLIN1,' sigma ',XCROST,' events ',NEVT
CYG            WRITE(1,10000) LBLIN,LBLIN1,XCROST,NEVT
CYG
CYG            if(igener.eq.1) then
CYG      DO L=1,NEVT
CYG       CALL PYEVNT
CYG      if(idbg.eq.1)call lulist(1)
CYG      ENDDO
CYG            endif
CYG
CYG  300   continue
CYG
CYG      endif
CYG
CYG
CYG      if(scan)then
CYG
CYG        if(igeneralM.lt.icM.or.igeneralmu.lt.icmu)then
CYG          if(igeneralmu.lt.icmu)goto 20
CYG          if(igeneralM.lt.icM )goto 10
CYG        endif
CYG
CYG      endif
CYG
CYG  280 continue
CYG
CYG
CYG      call susend
CYG
CYG10000   FORMAT(1X,' Cross section in pb ',2(A5,2X),E15.5,' events ',i10)
CYG
CYG      STOP
CYG      END
CYG*CMZ :  1.00/00 14/04/95  18.39.54  by  Stavros Katsanevas
CYG*-- Author :    Stavros Katsanevas   14/04/95
CYG      subroutine susend
CYG      logical wrt,scan,lepi
CYG      common/str/wrt,scan,lepi
CYG      call hrput(0,'susygen.hist','n')
CYG      return
CYG      end
CYG*CMZ :  1.00/00 14/04/95  18.39.54  by  Stavros Katsanevas
CYG*-- Author :    Stavros Katsanevas   14/04/95
CYG      subroutine scards
CYG
CYGccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
CYGc  READ STEERING CARDS                                        c
CYGccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
CYG
CYG      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
CYG
CYGc
CYGc parameters
CYGc
CYG      real*4  rgmaum,rgmaur,rgm0,rgtanb,rgatri,rgma, 
CYG     +        rfmsq,rfmstopl,rfmstopr,
CYG     +        rfmsell,rfmselr,rfmsnu,rfmglu,recm,rflum,rvscan(6)
CYG      common /rkey/rgmaum,rgmaur,rgm0,rgtanb,rgatri,rgma, 
CYG     +        rfmsq,rfmstopl,rfmstopr,
CYG     +        rfmsell,rfmselr,rfmsnu,rfmglu,recm,rflum,rvscan 
CYG
CYG
CYG      common/higgses/sfmh,sfma,sfmhp,sfmhpc,sina,cosa
CYG
CYG      common/steer/gmaum,gmaur,gm0,gtanb,gatri,
CYG     +fmsq,fmstopl,fmstopr,fmsell,fmselr,fmsnu,fmglu
CYG      common/mds/modes,mix
CYGc
CYGc  produce what ?
CYGc
CYG      logical zino,wino,sele,smuo,stau,snu,squa,stopa,sbota,higgs
CYG      common/sparc/ zino,wino,sele,smuo,stau,snu,squa,stopa,sbota,higgs
CYGc
CYGc
CYGc
CYG      COMMON/KINEM/flum,ecm,s,roots,T,Q,Q2,EN(2)
CYG      COMMON/ISR/ QK(4)
CYG      COMMON /CONST/ idbg,igener,irad
CYG
CYG      logical wrt,scan,lepi
CYG      common/str/wrt,scan,lepi
CYG
CYG
CYG      common/decsel/idecsel(23)
CYG      common/higsel/ihigsel(5)
CYG
CYG      real*4 gmas,phimx
CYG      common/stopmix/gmas(3),phimx(3)
CYG      real*4 rscale
CYG      common/sscale/rscale
CYG
CYG	INTEGER I(4),IJ(3,2)
CYG	COMMON/INDICES/I,IJ
CYG
CYG        real*4 xlama
CYG        common/lamda/xlama
CYG
CYG        logical rpar
CYG        common/rpari/rpar
CYG        logical imsg                      !Added by S.A. for use in the
CYG        common/imessage/imsg              !NTONPH routine
CYG                                                                            
CYG      call vzero(rvscan,6)
CYG
CYG      CALL FFINIT(0)
CYGc
CYGc 3 modes of running
CYGC modes=1  5 input values
CYGC      =2  sfermion masses given
CYGC      =3  M is the LSP neutralino mass
CYG
CYG      MODES=1
CYG      CALL FFKEY('MODES',modes ,1,'INTE')
CYG
CYGc
CYGc first mode
CYGc
CYG      rgmaum=90.
CYG      CALL FFKEY('M',rgmaum,  1,'REAL')
CYG      rscale=1.
CYG      CALL FFKEY('RS',rscale,  1,'REAL')
CYG      rgmaur=90.
CYG      CALL FFKEY('mu',rgmaur,  1,'REAL')
CYG      rgm0=90.
CYG      CALL FFKEY('m0',rgm0,  1,'REAL')
CYG      rgtanb=4.
CYG      CALL FFKEY('tanb',rgtanb,  1,'REAL')
CYG      rgatri=0.
CYG      CALL FFKEY('A',rgatri,  1,'REAL')
CYG      rgma=300.
CYG      CALL FFKEY('mA',rgma,  1,'REAL')
CYGc
CYGc second mode
CYGc
CYG      rfmsq=1000.
CYG      CALL FFKEY('MSQUARK',rfmsq,  1,'REAL')
CYG      rfmstopl=1000.
CYG      CALL FFKEY('MLSTOP',rfmstopl,  1,'REAL')
CYG      rfmstopr=1000.
CYG      CALL FFKEY('MRSTOP',rfmstopr,  1,'REAL')
CYG      rfmsell=1000.
CYG      CALL FFKEY('MLSEL',rfmsell,  1,'REAL')
CYG      rfmselr=1000.
CYG      CALL FFKEY('MRSEL',rfmselr,  1,'REAL')
CYG      rfmsnu=1000.
CYG      CALL FFKEY('MSNU',rfmsnu,  1,'REAL')
CYG
CYG      mix=0
CYG      CALL FFKEY('MIX',MIX,1,'INTE')
CYG      phimx(1)=0.
CYG      phimx(2)=0.
CYG      phimx(3)=0.
CYG      CALL FFKEY('PHIMX',phimx,  3,'REAL')
CYG      gmas(1)=0.
CYG      gmas(2)=0.
CYG      gmas(3)=0.
CYG      CALL FFKEY('MAS1',gmas,  3,'REAL')
CYG
CYGc
CYGc Generate what ?
CYGc
CYG      CALL FFKEY('ZINO',zino,  1,'LOGIC')
CYG      CALL FFKEY('WINO',wino,  1,'LOGIC')
CYG      CALL FFKEY('SELECTRON',sele,  1,'LOGIC')
CYG      CALL FFKEY('SMUON',smuo,  1,'LOGIC')
CYG      CALL FFKEY('STAU',stau,  1,'LOGIC')
CYG      CALL FFKEY('SNU',snu,  1,'LOGIC')
CYG      CALL FFKEY('SQUARK',squa,  1,'LOGIC')
CYG      CALL FFKEY('SSTOP',stopa,  1,'LOGIC')
CYG      CALL FFKEY('SBOTTOM',sbota,  1,'LOGIC')
CYG      CALL FFKEY('HIGGS',higgs,  1,'LOGIC')
CYGc
CYGc R Parity control
CYGc
CYG      CALL FFKEY('RPARITY',rpar,  1,'LOGIC')
CYG      CALL FFKEY('INDIC',I ,4,'INTE')
CYG      CALL FFKEY('LAMDA',xlama ,1,'REAL')
CYGc
CYGc Running conditions
CYGc
CYG      CALL FFKEY('ECM',recm,  1,'REAL')
CYG      CALL FFKEY('LUMINOSITY',rflum,  1,'REAL')
CYG      CALL FFKEY('ISR',irad,1,'INTE')
CYG      CALL FFKEY('GENER',igener,1,'INTE')
CYG
CYG      do 96 lik=1,23
CYG 96   idecsel(lik)=1
CYG
CYG      CALL FFKEY('DECSEL',idecsel,  23,'INTE')
CYG
CYG      do 97 lik=1,5
CYG 97   ihigsel(lik)=1
CYG
CYG      CALL FFKEY('HIGSEL',ihigsel,  5,'INTE')
CYG
CYG      CALL FFKEY('DEBUG',idbg ,1,'INTE')
CYG      call ffkey('LUWRIT',wrt,1,'LOGIC')
CYG      call ffkey('SCAN',scan,1,'LOGIC')
CYG      call ffkey('VSCAN',rvscan,6,'REAL')
CYG      call ffkey('LEPI',lepi,1,'LOGIC')
CYG
CYG      CALL FFGO
CYG
CYG      gmaum=dble(rgmaum)
CYG      gmaur=dble(rgmaur)
CYG      gm0=dble(rgm0)
CYG      gtanb=dble(rgtanb)
CYG      gatri=dble(rgatri)
CYG
CYG      fmsq=dble(rfmsq)
CYG      fmstopl=dble(rfmstopl)
CYG      fmstopr=dble(rfmstopr)
CYG      fmsell=dble(rfmsell)
CYG      fmselr=dble(rfmselr)
CYG      fmsnu=dble(rfmsnu)
CYG      fmglu=dble(rfmglu)
CYG
CYG      ecm=dble(recm)
CYG      flum=dble(rflum)
CYG      if(idbg.eq.3)imsg=.TRUE.
CYG      sfma=dble(rgma)
CYGc
CYGc s.k change
CYGc
CYGc      if(modes.eq.3)gmaum=0.3*fmglu
CYG
CYG      return
CYG      end
CYG*CMZ :  1.00/00 14/04/95  18.46.24  by  Stavros Katsanevas
CYG*-- Author :    Stavros Katsanevas   14/04/95
CYG      SUBROUTINE SUSANA(AMGAUG,AMR,TANA,am0,aatri,mfail)
CYG
CYGC********************************************************************
CYGC initialization routine of susy generator
CYGC                                         author S.Katsanevas
CYGC********************************************************************
CYG
CYG      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
CYG
CYG      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
CYG     +FLC(12),FRC(12),gms(12),echar(12)
CYG
CYG      COMMON/MSSM/TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI
CYG
CYG      common/spartcl/fmal(12),fmar(12),ratqa(12),fgamc(12),fgamcr(12)
CYG     +,cosmi(12)
CYG
CYG
CYG      COMMON /CONST/ idbg,igener,irad
CYG
CYG      COMMON/KINEM/flum,ecm,s,roots,T,Q,Q2,EN(2)
CYG
CYG      common/higgses/sfmh,sfma,sfmhp,sfmhpc,sina,cosa
CYG
CYG
CYG      logical wrt,scan,lepi
CYG      common/str/wrt,scan,lepi
CYGc
CYGc change by s.k 9 aug 1995
CYGc
CYG      common/mds/modes,mix
CYG
CYG
CYG      DIMENSION T3(12)
CYGc
CYGc reorder sparticles and particles
CYGc
CYG      common/reorder/ispa(12,2),kl(2,18),klap(2,18),idecs(12,2)
CYG
CYG      mfail=0
CYGC
CYGC initialize Euclidian constants
CYGC
CYG      PI = 3.1415926535D0
CYG      twopi=2.*pi
CYGC
CYGC initialize SM constants
CYGC
CYG
CYG      ALPHA=1.D0/127.9D0
CYG      FMZ=91.19D0
CYG      GAMMAZ=2.497D0
CYG      GAMMAW=2.01D0
CYG      FMW=80.2
CYG      SIN2W=.231243D0
CYG      SINW=DSQRT(SIN2W)
CYG      COSW=DSQRT(1.-SIN2W)
CYG      E2=4.D0*PI*ALPHA
CYG      G2=E2/SIN2W
CYG
CYG      DO 10 I=1,12
CYG
CYG        IF(MOD(I-1,4).EQ.0)ECHAR(I)=2./3.
CYG        IF(MOD(I-1,4).EQ.1)ECHAR(I)=-1./3.
CYG
CYG        IF(MOD(I-1,4).EQ.2)ECHAR(I)=0
CYG        IF(MOD(I-1,4).EQ.3)ECHAR(I)=-1.
CYG
CYG        IF(MOD(I-1,2).EQ.0)T3(I)=1./2.
CYG        IF(MOD(I-1,2).EQ.1)T3(I)=-1./2.
CYG
CYG        FLC(I)=T3(I)-ECHAR(I)*SINW**2
CYG        FRC(I)=-ECHAR(I)*SINW**2
CYG   10 CONTINUE
CYG
CYGC
CYGC initialize  MSSM constants
CYGC
CYGC gaugino mass
CYG      FMGAUG=AMGAUG
CYG      FMR=AMR
CYG      FM0=am0
CYG      TANB=TANA
CYG      COSB=1./DSQRT(1.D0+TANB**2)
CYG      SINB=TANB*COSB
CYG      ATRI = aatri
CYG
CYGc
CYGc Third mode
CYGc
CYGc s.k change 9 Aug 1995
CYGc
CYG      if(modes.eq.3)then
CYG      call sucalc(mfail)
CYG      if(mfail.eq.1)return
CYG      endif
CYG
CYG      WRITE(1,10000) FMGAUG,FMR,FM0,TANB,atri,gms(9) ,ecm,flum,irad
CYG
CYG10000 FORMAT(' INPUTS:'/
CYG     +' M       =',F10.3,'   mu       =',F10.3/
CYG     +' m0      =',F10.3,'   TANB     =',F10.3,/
CYG     +' A       =',F10.3,'   mtop     =',F10.3,/
CYG     +' Ecm     =',F10.3,'  Luminosity=',F10.3,
CYG     +' RAD CORR=',i3/)
CYG
CYGC
CYGC calculate the mass and mixings of the sparticles
CYGC
CYG
CYG
CYG
CYG      CALL sfermion(mfail)
CYG
CYG      if(mfail.eq.1)return
CYG
CYGC
CYGC calculate the mass and mixings of gauginos
CYGC
CYG      CALL gaugino(mfail)
CYG      if(mfail.eq.1)return
CYG
CYGc
CYGc calculate higgs masses
CYGc
CYG      au=atri
CYG      ad=atri
CYG      CALL SUBH (sfmA,tanb,fm0,fm0,gms(9),Au,Ad,fm0,
CYG     *           sfmh,sfmhp,sfmhpc,sina,cosa)
CYGc
CYGc pythia interface
CYGc
CYG      call pinterf
CYGc
CYGc
CYGc LEP limits
CYGc
CYG      if(LEPI)call leplim(mfail)
CYG
CYG      if(mfail.eq.1)return
CYG
CYGc
CYGc calculate decay BR
CYGc
CYG      CALL branch
CYG
CYG
CYG      RETURN
CYG      END
CYG*CMZ :  1.00/00 14/04/95  18.46.24  by  Stavros Katsanevas
CYG*-- Author :    Stavros Katsanevas   14/04/95
CYG      SUBROUTINE SFERMION(mfail)
CYG
CYGC********************************************************************
CYGC calculates sparticle masses
CYGC                                         author S.Katsanevas
CYGC********************************************************************
CYG
CYG      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
CYG
CYG* Keys
CYG      real*4  rgmaum,rgmaur,rgm0,rgtanb,rgatri,rgma,
CYG     +        rfmsq,rfmstopl,rfmstopr,
CYG     +        rfmsell,rfmselr,rfmsnu,rfmglu,recm,rflum,rvscan(6)
CYG      common /rkey/rgmaum,rgmaur,rgm0,rgtanb,rgatri,rgma,
CYG     +        rfmsq,rfmstopl,rfmstopr,
CYG     +        rfmsell,rfmselr,rfmsnu,rfmglu,recm,rflum,rvscan
CYG
CYG      logical wrt,scan,lepi
CYG      common/str/wrt,scan,lepi
CYG
CYG
CYG      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
CYG     +FLC(12),FRC(12),gms(12),echar(12)
CYG
CYG      COMMON/MSSM/TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI
CYG
CYG      common/spartcl/fmal(12),fmar(12),ratq(12),fgamc(12),fgamcr(12)
CYG     +,cosmi(12)
CYG      dimension fm1(12),fm2(12)
CYG
CYG
CYG      common/steer/gmaum,gmaur,gm0,gtanb,gatri,
CYG     +fmsq,fmstopl,fmstopr,fmsell,fmselr,fmsnu,fmglu
CYG      common/mds/modes,MIX
CYG
CYG      real*4 phimix1,stop1,phimix2,sbot1,phimix3,stau1
CYG      common/stopmix/stop1,sbot1,stau1,phimix1,phimix2,phimix3
CYG
CYG      common/reorder/ispa(12,2),kl(2,18),klap(2,18),idecs(12,2)
CYG      data fmal/12*1./
CYG      data fmar/12*1./
CYG
CYG      call smgut
CYGc
CYGc inexistent snu right
CYGc
CYG      fmar(3) =10000.
CYG      fmar(7) =10000.
CYG      fmar(11)=10000.
CYG
CYG      COS2B=ABS(COSB**2-SINB**2)
CYG
CYG      do 10 k=1,12
CYG
CYG        IF(FMAL(K).LE.0.)then
CYG          write(1,*) ' sfermionL negative ',k,fmal(k)
CYG          if(scan)then
CYG          else
CYG            STOP 99
CYG          endif
CYG        endif
CYG
CYG        IF(FMAR(K).LE.0.)then
CYG          write(1,*) ' sfermionR negative ',k,fmar(k)
CYG          if(scan)then
CYG          else
CYG            STOP 99
CYG          endif
CYG        endif
CYG
CYG        if(fmal(k).lt.1.)fmal(k)=1.
CYG        if(fmar(k).lt.1.)fmar(k)=1.
CYG   10 continue
CYG
CYG
CYG      if(modes.eq.1)go to 30
CYG
CYG      do 20 i=1,3
CYGc
CYGc up
CYGc
CYG        fmar(1+(i-1)*4)=fmsq
CYG        fmal(1+(i-1)*4)=fmsq
CYGc
CYGc down
CYGc
CYG        fmar(2+(i-1)*4)=fmsq
CYG        fmal(2+(i-1)*4)=fmsq
CYGc
CYGc neutrino
CYGc
CYG        fmal(3+(i-1)*4)=fmsnu
CYGc
CYGc electron
CYGc
CYG        fmar(4+(i-1)*4)=fmselr
CYG        fmal(4+(i-1)*4)=fmsell
CYG
CYG   20 continue
CYG
CYG      fmal(9)=fmstopl
CYG      fmar(9)=fmstopr
CYG
CYG   30 continue
CYG
CYGc
CYGc mixings
CYGc
CYG      DO 40 i=1,12
CYG        fm1(i)=fmal(i)**2
CYG        fm2(i)=fmar(i)**2
CYG
CYG        costh=1.
CYG
CYG        if(i.ge.9.and.MIX.ne.0)then
CYG
CYG          ctanb=tanb
CYG
CYG          if(mod(i,2).eq.1)ctanb=1./tanb
CYG
CYG          ALT=((ATRI+FMR*CTANB))
CYG
CYGc
CYGc mixing for the 3d family
CYGc
CYG          sums=fmal(i)**2+fmar(i)**2
CYG          difs=fmal(i)**2-fmar(i)**2
CYG          delta=difs**2+4.d0*alt**2*gms(i)**2
CYG          if(delta.ne.0.)then
CYG            FM1(I)=0.5D0*(sums-dsqrt(delta))
CYG            FM2(I)=0.5D0*(sums+dsqrt(delta))
CYG            cos2th=difs/dsqrt(delta)
CYG            costh=(1+COS2TH)/2.
CYG            if(costh.ne.0.)costh=dsqrt(costh)
CYG          endif
CYG
CYGc
CYGc overwrite stop mixing angle
CYGc
CYGc provision to read in stop mass and mixing
CYGc
CYG          if(stop1.gt.0..and.i.eq.9)then
CYG            phimix1=phimix1*3.1415926535/180.
CYG            phamix=dble(phimix1)
CYG            costh=cos(phamix)
CYG            tanth=tan(phamix)
CYG            fm1(i)=stop1**2
CYG            fm2(i)=stop1**2+4.d0*alt*gms(i)*tanth
CYG          endif
CYG
CYG
CYG          if(sbot1.gt.0..and.i.eq.10)then
CYG            phimix2=phimix2*3.1415926535/180.
CYG            phamix=dble(phimix2)
CYG            costh=cos(phamix)
CYG            tanth=tan(phamix)
CYG            fm1(i)=sbot1**2
CYG            fm2(i)=sbot1**2+4.d0*alt*gms(i)*tanth
CYG          endif
CYG
CYG
CYG          if(stau1.gt.0..and.i.eq.12)then
CYG            phimix3=phimix3*3.1415926535/180.
CYG            phamix=dble(phimix3)
CYG            costh=cos(phamix)
CYG            tanth=tan(phamix)
CYG            fm1(i)=stau1**2
CYG            fm2(i)=stau1**2+4.d0*alt*gms(i)*tanth
CYG          endif
CYG
CYG        endif
CYG
CYG
CYG        COSMI(I)=costh
CYG        ratq(i)=-(ECHAR(I))
CYG        FGAMC(i) =(FLC(I)-FRC(I))*COSMI(I)**2+FRC(I)
CYG        FGAMCR(i)=(FRC(I)-FLC(I))*COSMI(I)**2+FLC(I)
CYG
CYG
CYG        IF(FM1(I).LE.0.)then
CYG          write(1,*) ' sfermion1 negative ',I,fm1(I)
CYG          mfail=1
CYG        else
CYG          FM1(I)=DSQRT(FM1(I))
CYG        endif
CYG
CYG        IF(FM2(I).LE.0.)then
CYG          write(1,*) ' sfermion2 negative ',I,fm2(I)
CYG          mfail=1
CYG        else
CYG          FM2(I)=DSQRT(FM2(I))
CYG        endif
CYG
CYG        if(I.lt.9)go to 40
CYG
CYG        fmal(I)=fm1(I)
CYG        fmar(I)=fm2(I)
CYG
CYG   40 CONTINUE
CYG
CYG
CYG      write (1,*) ' Sparticle masses '
CYG      write (1,10000) ' SUPR ',fmar(1),' SUPL ',fmal(1)
CYG      write (1,10000) ' SDNR ',fmar(2),' SDNL ',fmal(2)
CYG      write (1,10000) ' SELR ',fmar(4),' SELL ',fmal(4)
CYG      write (1,10000) ' SNU ',fmal(3)
CYG      write (1,10000) ' STP1 ',fmal(9) ,' STP2 ',fmar(9)
CYG      write (1,10000) ' SBT1 ',fmal(10),' SBT2 ',fmar(10)
CYG      write (1,10000) ' STA1 ',fmal(12),' STA2 ',fmar(12)
CYG
CYG10000 FORMAT(/a5,f10.0,a7,f10.0)
CYG
CYG
CYG      RETURN
CYG      END
      end
      SUBROUTINE smgut
*CMZ :  1.00/00 14/04/95  18.46.24  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
C********************************************************************
C Computes mass of leptons and squarks
C from a given set of SUSY parameters
C********************************************************************
C by S.Ambrosanio (1994)
C********************************************************************

      IMPLICIT DOUBLE PRECISION (A-H,K-Z)

      COMMON/SM/MW,MZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
     +     FLC(12),FRC(12),
     +     MUP,MDO,MVE,ME,MCH,MST,MVMU,MMU,MTOP,MBO,MVTAU,MTAU,
     +     echar(12)

      common/steer/m2,mu,m0,tgb,gatri,
     +     fmsq,fmstopl,fmstopr,fmsell,fmselr,fmsnu,fmglu

      PARAMETER(
     +     ALFGUT = .041, MGUT = 2.D+16,
     +     ALFEM = 1./127.9, ALFSTR = 0.118,
     +     ALF1 = 1.6981D-2, ALF2 = 3.3613D-2,
     +     SIN2WI=.2324)

      PARAMETER (T3V = 1./2., T3LL = -1./2., T3LR = 0.,
     +     T3UL = 1./2., T3DL = -1./2., T3QR = 0.,
     +     QV = 0., QL = -1., QU = 2./3., QD = -1./3.,
     +     B1 = 33./5., B2 = 1., B3 = -3.)
      PARAMETER (DELALF = 1)

      common/spartcl/
     +     msupl,msdol,msve,msel,mschl,msstl,msvmu,msmul,mstopl,
     +     msbol,msvtau,mstaul,
     +     msupr,msdor,msver,mser,mschr,msstr,msvmur,msmur,mstopr,
     +     msbor,msvtaur,mstaur
     +     ,ratq(12),fgamc(12),fgamcr(12),cosmi(12)

      dimension fspart(24)
      equivalence(fspart(1),msupl)

      COMMON/SFERMIX/MSTAU1,MSTAU2,MSTOP1,MSTOP2,MSBO1,MSBO2

      COMMON/SFLOOP/ILOOP



      ALFA1(Q)= ALF1/(1.-B1*TD(Q)*ALF1/2./PI)
      ALFA2(Q)= ALF2/(1.-B2*TD(Q)*ALF2/2./PI)
      ALFA3(Q)= ALFSTR/(1.-B3*TD(Q)*ALFSTR/2./PI)

      C1(Q) = (2./11.)*(M2/ALF2)**2*
     +     (ALFGUT**2-ALFA1(Q)**2)
      C2(Q) = (3./2.)*(M2/ALF2)**2*
     +     (ALFGUT**2-ALFA2(Q)**2)
      C3(Q) =  (8./9.)*(M2/ALF2)**2*
     +     (ALFA3(Q)**2-ALFGUT**2)

      do 10 k=1,24
   10 fspart(k)=MZ

      SIN2W = SIN2WI-1.03D-7*(MTOP**2-138.**2)
      COS2W = 1.-SIN2W

      SINB  = TGB/SQRT(1. + TGB**2)
      COSB  =  1./SQRT(1. + TGB**2)
      SIN2B =  2.*TGB/(1. + TGB**2)
      COS2B = (1. - TGB**2)/(1. + TGB**2)

c       M12 = (ALFGUT/ALF2)*M2
c       M3  = (ALFSTR/ALF2)*M2
c       M1  = (5./3.)*(SIN2W/COS2W)*M2

      ZV  = T3V -QV*SIN2W
      ZLL = T3LL-QL*SIN2W
      ZLR = -(T3LR-QL*SIN2W)
      ZUL =   T3UL-QU*SIN2W
      ZUR = -(T3QR-QU*SIN2W)
      ZDL =   T3DL-QD*SIN2W
      ZDR = -(T3QR-QD*SIN2W)

      IORDER  = 0
   20 IORDER  = IORDER + 1

      MSVE2 = MVE**2+M0**2
     +     +C2(MSVE)+(1./4.)*C1(MSVE)
     +     +ZV*MZ**2*COS2B
      IF (MSVE2.LE.0.0) THEN
        IF (IORDER.GT.ILOOP) THEN
          MSVE = 1.D-08
        ELSE
          MSVE = MZ
        ENDIF
      ELSE
        MSVE = SQRT(MSVE2)
      ENDIF
      MSVMU  = MSVE
      MSVTAU = MSVE

      MSEL = SQRT(ME**2+M0**2
     +     +C2(MSEL)+(1./4.)*C1(MSEL)
     +     +ZLL*MZ**2*COS2B)
      MSMUL  = MSEL
      MSTAUL = SQRT(MTAU**2+M0**2
     +     +C2(MSTAUL)+(1./4.)*C1(MSTAUL)
     +     +ZLL*MZ**2*COS2B)

      MSER = SQRT(ME**2+M0**2
     +     +C1(MSER)
     +     +ZLR*MZ**2*COS2B)
      MSMUR   = MSER
      MSTAUR  = SQRT(MTAU**2+M0**2
     +     +C1(MSTAUR)
     +     +ZLR*MZ**2*COS2B)

      MSUPL2 = MUP**2+M0**2
     +     +C3(MSUPL)+C2(MSUPL)+(1./36.)*C1(MSUPL)
     +     +ZUL*MZ**2*COS2B
      IF (MSUPL2.LE.0.0) THEN
        IF (IORDER.GT.ILOOP) THEN
          MSUPL = 1.D-08
        ELSE
          MSUPL = MZ
        ENDIF
      ELSE
        MSUPL = SQRT(MSUPL2)
      ENDIF
      MSCHL   = MSUPL

C     MSTOPL2  = MSUPL**2+(MH20Q-MU**2
C     .      -MSEL**2-ZLL*MZ**2*COS2B)/3.+MTOP**2
C     IF (MSTOPL2.LE.0.0) THEN
C      IF (IORDER.GT.ILOOP) THEN
C       MSTOPL = 1.D-08
C      ELSE
C       MSTOPL = MZ
C      ENDIF
C     ELSE
C      MSTOPL  = SQRT(MSTOPL2)
C     ENDIF

      MSUPR2 = MUP**2+M0**2
     +     +C3(MSUPR)+(4./9.)*C1(MSUPR)
     +     +ZUR*MZ**2*COS2B
      IF (MSUPR2.LE.0.0) THEN
        IF (IORDER.GT.ILOOP) THEN
          MSUPR = 1.D-08
        ELSE
          MSUPR = MZ
        ENDIF
      ELSE
        MSUPR = SQRT(MSUPR2)
      ENDIF
      MSCHR   = MSUPR
C     MSTOPR2  = MSUPR**2+2.*(MH20Q-MU**2
C     .      -MSEL**2-ZLL*MZ**2*COS2B)/3.+MTOP**2
C     IF (MSTOPR2.LE.0.0) THEN
C      IF (IORDER.GT.ILOOP) THEN
C       MSTOPR = 1.D-08
C      ELSE
C       MSTOPR = MZ
C      ENDIF
C     ELSE
C      MSTOPR  = SQRT(MSTOPR2)
C     ENDIF

      MSDOL = SQRT(MDO**2+M0**2
     +     +C3(MSDOL)+C2(MSDOL)+(1./36.)*C1(MSDOL)
     +     +ZDL*MZ**2*COS2B)
      MSSTL   = MSDOL

C     MSBOL2   = MSDOL**2+(MH20Q-MU**2
C     .      -MSEL**2-ZLL*MZ**2*COS2B)/3.
C     IF (MSBOL2.LE.0.0) THEN
C      MSBOL   = MSDOL
C     ELSE
C      MSBOL   = SQRT(MSBOL2)
C     ENDIF

      MSDOR = SQRT(MDO**2+M0**2
     +     +C3(MSDOR)+(1./9.)*C1(MSDOR)
     +     +ZDR*MZ**2*COS2B)
      MSSTR   = MSDOR

      MSBOR2 = MBO**2+M0**2
     +     +C3(MSBOR)+(1./9.)*C1(MSBOR)
     +     +ZDR*MZ**2*COS2B
      IF (MSBOR2.LE.0.0) THEN
        MSBOR = MSDOR
      ELSE
        MSBOR = SQRT(MSBOR2)
      ENDIF

      IF (IORDER.LE.ILOOP) GOTO 20

      MSBOL=MSDOL
      MSTOPL=MSUPL
      MSTOPR=MSUPR

      RETURN

      END
*CMZ :  1.00/00 14/04/95  18.39.55  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      function td(q)

      IMPLICIT DOUBLE PRECISION (A-H,K-Z)

      COMMON/SM/MW,MZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
     +     FLC(12),FRC(12),
     +     MUP,MDO,MVE,ME,MCH,MST,MVMU,MMU,MTOP,MBO,MVTAU,MTAU,
     +     echar(12)

      TD = LOG(Q/MZ)

c     TU(Q) = 2.*LOG(MGUT/Q)

      return
      end

*CMZ :  1.00/00 14/04/95  18.46.24  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      SUBROUTINE gaugino(mfail)


C********************************************************************
C calculates gaugino masses and mixings
C                                         author S.Katsanevas
C********************************************************************


      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

C Keys
      real*4  rgmaum,rgmaur,rgm0,rgtanb,rgatri,rgma,
     +        rfmsq,rfmstopl,rfmstopr,
     +        rfmsell,rfmselr,rfmsnu,rfmglu,recm,rflum,rvscan(6)
      common /rkey/rgmaum,rgmaur,rgm0,rgtanb,rgatri,rgma,
     +        rfmsq,rfmstopl,rfmstopr,
     +        rfmsell,rfmselr,rfmsnu,rfmglu,recm,rflum,rvscan
      logical wrt,scan,lepi
      common/str/wrt,scan,lepi



      COMMON/NMSSM/FLAMDA,FKAPPA
      COMMON/MSSM/TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI

      COMMON/XCROS/xgaug(8),xeta(8)
      COMMON/NEUMIX/ZR(4,4),was(4),ESA(4),
     +VOIJL(4,4),VOIJR(4,4),gfir(4,4),gfil(4,4)
      COMMON/CHAMIX/ OIJL(2,2),OIJR(2,2),V(2,2),U(2,2),FM(2),ETA(2)
      COMMON/CHANEU/oijlp(4,2),oijrp(4,2)

      common/spartcl/fmal(12),fmar(12),ratqa(12),fgamc(12),fgamcr(12)
     +,cosmi(12)


      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
     +FLC(12),FRC(12),gms(12),echar(12)

      COMMON /CONST/ idbg,igener,irad

      REAL*4 FMA(4,4),WR(4),vtemp(4),work(16),dr(4,4)
      real*4 rscale
      common/sscale/rscale

	REAL RBOD_G,rbod2_g,rbod4_m
	LOGICAL RBOD,rbod2,rbod3,rbod4
	common /rbod1/ RBOD , RBOD2 
	common /rbod2/ RBOD_G(4),RBOD2_G(4)
	common /rbod3/ rbod3
	common /rbod4/ rbod4
	common /rbod4m/ rbod4_m(2)
      
      
c      real*4 weight

      sin2w=sinw**2
      sin2beta=2.*sinb*cosb
      cos2beta=cosb**2-sinb**2


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C neutralino part
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      FMGAUG1=FMGAUG*5./3.*SINW**2/COSW**2*rscale

CPM      write(1,10000) FMGAUG1
10000 format(//' GAUGINO U(1) = ',F10.3/)

      FMA(1,1)= FMGAUG1*COSW**2+FMGAUG*SINW**2
      FMA(2,1)= (FMGAUG-FMGAUG1)*SINW*COSW
      FMA(3,1)= 0.
      FMA(4,1)= 0.
      FMA(1,2)= (FMGAUG-FMGAUG1)*SINW*COSW
      FMA(2,2)= FMGAUG1*SINW**2+FMGAUG*COSW**2
      FMA(3,2)= FMw/cosw
      FMA(4,2)= 0.
      FMA(1,3)= 0.
      FMA(2,3)= FMw/cosw
      FMA(3,3)=   FMR*SIN2BETA
      FMA(4,3)= - FMR*COS2BETA
      FMA(1,4)= 0.
      FMA(2,4)= 0.
      FMA(3,4)= - FMR*COS2BETA
      FMA(4,4)= - FMR*SIN2BETA

      CALL EISRS1(4,4,FMA,WR,DR,IERR,WORK)
      IF(IERR.NE.0)RETURN

      DO 10 K=1,4
        WAS(K)=dble(ABS(WR(K)))
        ESA(K)=dble(SIGN(1.,WR(K)))
   10 CONTINUE

C
C       Sort eigenvectors and eigenvalues according to masses
C
      DO 30 I=1,3
        DO 20 J=I+1,4
          IF (was(i).GT.was(j)) THEN
            call ucopy(dr(1,j),vtemp,4)
            TEMP=abs(was(J))
            temp1=esa(j)
            call ucopy(dr(1,i),dr(1,j),4)
            Was(J)=Was(I)
            esa(j)=esa(i)
            call ucopy(vtemp,dr(1,i),4)
            Was(I)=TEMP
            esa(i)=temp1
          END IF
   20   CONTINUE
   30 CONTINUE

C     ... bodge - set chi1 mass to the rbo3 value
      if (rbod4) then
         was(1)=rbod4_m(1)
         was(2)=100.0
         was(3)=200.0
         was(4)=300.0
      endif   
      
      do 999 k=1,4
      do 999 l=1,4
       zr(l,k)=dble(dr(l,k))
 999  continue 

c
c
c voijl,voijr are Bartl's o-double-prime ij left + right
c
      do 40 i=1,4
        do 40 j=1,4


          A1=-(ZR(3,i)*ZR(3,j)-ZR(4,i)*ZR(4,j))*COS2BETA
          A2=-(ZR(3,i)*ZR(4,j)+ZR(4,i)*ZR(3,j))*SIN2BETA

          VOIJL(i,j)=(A1+A2)/2.
          VOIJR(i,j)=-VOIJL(i,j)
   40 CONTINUE

CPM      if(idbg.eq.1)write (1,10100) VOIJL
10100 FORMAT(2X,' Neutralino OIJL matrix ',4F10.3)


      DO 50  i=1,4
        do 50  l=1,4

          GFIL(i,l)= -DSQRT(2.D0)*(FLC(l)*ZR(2,i)/COSW-FRC(l)*ZR(1,i)/
     +    SINW)
          GFIR(i,l)= DSQRT(2.D0)*FRC(l)*(ZR(2,i)/COSW-ZR(1,i)/SINW)

   50 CONTINUE

CPM      if(idbg.eq.1)write (1,10200) GFIL
CPM      if(idbg.eq.1)write (1,10300) GFIR
10200 FORMAT(/2X,' Neutralino GFIL ',4F10.3)
10300 FORMAT(/2X,' Neutralino GFIR ',4F10.3)


CPM      WRITE(1,10400) was,esa
10400 FORMAT(/' NEUTRALINO MASSES  =',4F10.3,
     +       /' NEUTRALINO ETA     =',4F10.3)

      DO 60  J=1,4
CPM        WRITE(1,10500) J,(zr(k,j),K=1,4)
10500   FORMAT(' EIGENVECTOR  ',I1,'     =',4F10.3)
   60 CONTINUE


C********************************************************
C
C chargino part
C
C********************************************************

      DELTA1=dsqrt((FMGAUG-FMR)**2+2.D0*FMW**2*(1.+sin2beta))
      DELTA2=dsqrt((FMGAUG+FMR)**2+2.D0*FMW**2*(1.-sin2beta))

      FMM1=0.5*(delta1-delta2)
      FMM2=0.5*(delta1+delta2)
      FM(1)=ABS(FMM1)
      FM(2)=ABS(FMM2)
      ETA(1)=dsign(1.d0,fmm1)
      ETA(2)=dsign(1.d0,fmm2)


      w1=fmgaug**2-fmr**2-2.d0*fmw**2*cos2beta
      w2=fmgaug**2-fmr**2+2.d0*fmw**2*cos2beta
      welta=(fmgaug**2+fmr**2+2.d0*fmw**2)**2
     +-4.d0*(fmgaug*fmr-fmw**2*sin2beta)**2
      if(welta.le.0.)welta=0.0001
      welta=dsqrt(welta)
      ea=fmgaug*sinb+fmr*cosb
      eb=fmgaug*cosb+fmr*sinb

      if(tanb.ge.1)then
        th1=1.
        th2=dsign(1.d0,eb)
        th3=dsign(1.d0,ea)
        th4=1.
      else
        th1=dsign(1.d0,eb)
        th2=1.
        th3=1.
        th4=dsign(1.d0,ea)
      endif

      if(dabs(w1).gt.welta)then
        asign=dsign(1.d0,w1)
        w1=welta*asign
        if(idbg.eq.1)write (6,10600) w1,welta
10600 FORMAT(1X,' ERROR in Gaugino w1 ',2f10.2)
        w1=welta
      endif
      if(dabs(w2).gt.welta)then
        asign=dsign(1.d0,w2)
        w2=welta*asign
        if(idbg.eq.1)write (6,10700) w2,welta
10700 FORMAT(1X,' ERROR in Gaugino w2 ',2f10.2)
        w2=welta
      endif

      u(1,2)= th1*dsqrt(1.d0+w1/welta)/dsqrt(2.d0)
      u(2,1)=u(1,2)
      u(2,2)= th2*dsqrt(1.d0-w1/welta)/dsqrt(2.d0)
      u(1,1)=-u(2,2)

      v(2,1)=th3*dsqrt(1.d0+w2/welta)/dsqrt(2.d0)
      v(1,2)=-v(2,1)
      v(2,2)=th4*dsqrt(1.d0-w2/welta)/dsqrt(2.d0)
      v(1,1)=v(2,2)


      OIJL(1,1)=0.25d0-cosw**2+w2/4.d0/welta
      OIJL(2,2)=0.25d0-cosw**2-w2/4.d0/welta
      OIJL(1,2)=-fmw*ea/dsqrt(2.d0)/welta
      OIJL(2,1)=OIJL(1,2)

      OIJR(1,1)=0.25d0-cosw**2+w1/4.d0/welta
      OIJR(2,2)=0.25d0-cosw**2-w1/4.d0/welta
      OIJR(1,2)=fmw*eb/dsqrt(2.d0)/welta
      OIJR(2,1)=OIJR(1,2)

C     ... bodge - set chi+ mass to the rbo3 value
      if (rbod4) then
         fm(1)=rbod4_m(2)
         fm(2)=100.0
      endif
      
CPM      WRITE(1,10800) fm,eta
10800 FORMAT(/' CHARGINO MASSES    =',2F10.3,
     +        /' CHARGINO ETA      =',2F10.3/)

CPM      Write (1,*) ' U matrix ','  WINO   ',' HIGGSINO '
C wrong order      Write (1,10900) U
CPM      Write (1,10900) U(1,1),U(1,2),U(2,1),U(2,2)
CPM      Write (1,*) ' V matrix ','  WINO   ',' HIGGSINO '
C wrong order      Write (1,11000) V
CPM      Write (1,11000) V(1,1),V(1,2),V(2,1),V(2,2)

CPM      if(idbg.eq.1)Write (1,11100) OIJL
CPM      if(idbg.eq.1)Write (1,11200) OIJR

10900 FORMAT(' W1SS+    ',2F10.3,/' W2SS+    ',2F10.3/)
11000 FORMAT(' W1SS-    ',2F10.3,/' W2SS-    ',2F10.3/)

11100 FORMAT(/2X,' Chargino OIJL matrix ',4F10.3)
11200 FORMAT(/2X,' Chargino OIJR matrix ',4F10.3)


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C neutralino/chargino part
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      do 70 I=1,4
        do 70 J=1,2


          A1=-(ZR(4,i)*COSB-ZR(3,i)*SINB)*V(J,2)/SQRT(2.)
          B1= (ZR(4,i)*SINB+ZR(3,i)*COSB)*U(J,2)/SQRT(2.)
          A2=(ZR(1,i)*SINW+ZR(2,i)*COSW)*V(J,1)
          B2=(ZR(1,i)*SINW+ZR(2,i)*COSW)*U(J,1)

          OIJLP(I,J)=A1+A2
          OIJRP(I,J)=B1+B2
   70 CONTINUE

CPM      if(idbg.eq.1)write (1,11300) oijlp,oijrp
11300 FORMAT(2X,' Neutralino/Chargino OIJL matrix ',4F10.3)

c
c will be changed
c
      do 80 k=1,4
        xgaug(k)=was(k)
        xeta(k)=esa(k)
   80 continue

      do 90 k=1,2
        xgaug(k+4)=fm(k)
        xeta(k+4)=eta(k)
        xgaug(k+6)=fm(k)
        xeta(k+6)=eta(k)
   90 continue

      if(xgaug(1).gt.xgaug(5))then
        write(*,11400) xgaug(1),xgaug(5)
11400 FORMAT(1X,' Warning: neutralino ',f5.1,' > chargino ',f5.1)
        mfail=1
      endif

      do 100 ko=1,12
        if(xgaug(1).gt.fmal(ko))then
          write(*,11500) xgaug(1),ko,fmal(ko)
11500 FORMAT(1X,' Warning: neutralino ',f5.1,' > spart ',i5,f5.1)
          mfail=1
        endif

  100 continue

      do 110 ko=1,12
        if(xgaug(1).gt.fmar(ko))then

          write(*,11600) xgaug(1),ko,fmar(ko)
11600 FORMAT(1X,' Warning: neutralino ',f5.1,' > spart ',i5,f5.1)
          mfail=1
        endif

  110 continue

      RETURN
      END


      SUBROUTINE gaug_nmssm(zr,wasa,eta)

C********************************************************************
C calculates neutralino masses and mixings in NMSSM
C                                         author S.Katsanevas
C********************************************************************

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      COMMON/NMSSM/FLAMDA,FKAPPA
      COMMON/MSSM/TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI

      dimension wasa(4),ETA(4),ZR(4,4)

      dimension was(5),ESA(5)

      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
     +FLC(12),FRC(12),gms(12),echar(12)

      REAL*4 FMA(5,5),WR(5),vtemp(5),work(25),dr(5,5)

      real*4 rscale
      common/sscale/rscale

      data uvev/245./


      sin2w=sinw**2
      sin2beta=2.*sinb*cosb
      cos2beta=cosb**2-sinb**2


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C neutralino part
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      FMGAUG1=FMGAUG*5./3.*SINW**2/COSW**2*rscale

CPM      write(1,10000) FMGAUG1
10000 format(//' GAUGINO U(1) = ',F10.3/)
c
c x(-1) the MSSM
c 

      FMA(1,1)= -FMGAUG1*COSW**2+FMGAUG*SINW**2
      FMA(2,1)= -(FMGAUG-FMGAUG1)*SINW*COSW
      FMA(3,1)= 0.
      FMA(4,1)= 0.
      FMA(1,2)= -(FMGAUG-FMGAUG1)*SINW*COSW
      FMA(2,2)= -FMGAUG1*SINW**2+FMGAUG*COSW**2
      FMA(3,2)= FMw/cosw
      FMA(4,2)= 0.
      FMA(1,3)= 0.
      FMA(2,3)= FMw/cosw
      FMA(3,3)=   -FMR*SIN2BETA
      FMA(4,3)=   +FMR*COS2BETA
      FMA(1,4)= 0.
      FMA(2,4)= 0.
      FMA(3,4)=   FMR*COS2BETA
      FMA(4,4)=   FMR*SIN2BETA

      FMA(1,5)= 0
      FMA(2,5)= 0
      FMA(3,5)= 0
      FMA(4,5)= flamda*uvev
      FMA(5,5)= -2.*fkappa*fmr/flamda
      FMA(5,1)= flamda*uvev
      FMA(5,2)= 0
      FMA(5,3)= 0
      FMA(5,4)= 0

      CALL EISRS1(5,5,FMA,WR,DR,IERR,WORK)
      IF(IERR.NE.0)RETURN

      DO 10 K=1,5
        WAS(K)=dble(ABS(WR(K)))
        ESA(K)=dble(SIGN(1.,WR(K)))
   10 CONTINUE

C
C       Sort eigenvectors and eigenvalues according to masses
C
      DO 30 I=1,4
        DO 20 J=I+1,5
          IF (was(i).GT.was(j)) THEN
            call ucopy(dr(1,j),vtemp,4)
            TEMP=abs(was(J))
            temp1=esa(j)
            call ucopy(dr(1,i),dr(1,j),4)
            Was(J)=Was(I)
            esa(j)=esa(i)
            call ucopy(vtemp,dr(1,i),4)
            Was(I)=TEMP
            esa(i)=temp1
          END IF
   20   CONTINUE
   30 CONTINUE

      do 999 k=1,4
      wasa(k)=was(k)
      eta(k)=esa(k)
      do 999 l=1,4
       zr(l,k)=dble(dr(l,k))
 999  continue 

  110 continue

      RETURN
      END

*CMZ :  1.00/00 14/04/95  18.46.25  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      subroutine baer
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
     +FLC(12),FRC(12),gms(12),echar(12)
      COMMON/MSSM/TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI
      COMMON/NEUMIX/ZR(4,4),was(4),ESA(4),
     +VOIJL(4,4),VOIJR(4,4),gfir(4,4),gfil(4,4)
      COMMON/CHAMIX/ OIJL(2,2),OIJR(2,2),V(2,2),U(2,2),FM(2),ETA(2)
      COMMON/CHANEU/oijlp(4,2),oijrp(4,2)
      dimension uo(4,4),vo(4,4),junk(4,4),z(4,4)
      common/jnk/junk
c
c matrix for change of base ( Baer et al base )
c
      uo(1,1)=cosw
      uo(1,2)=sinw
      uo(1,3)=0
      uo(1,4)=0
      uo(2,1)=-sinw
      uo(2,2)=cosw
      uo(2,3)=0
      uo(2,4)=0
      uo(3,1)=0
      uo(3,2)=0
      uo(3,3)=cosb
      uo(3,4)=-sinb
      uo(4,1)=0
      uo(4,2)=0
      uo(4,3)=sinb
      uo(4,4)=cosb
      vo(1,1)=cosw
      vo(1,2)=-sinw
      vo(1,3)=0
      vo(1,4)=0
      vo(2,1)=sinw
      vo(2,2)=cosw
      vo(2,3)=0
      vo(2,4)=0
      vo(3,1)=0
      vo(3,2)=0
      vo(3,3)=cosb
      vo(3,4)=sinb
      vo(4,1)=0
      vo(4,2)=0
      vo(4,3)=-sinb
      vo(4,4)=cosb

      do 10  i=1,4
        do 10  j=1,4
          z(i,j)=0.
          do 10  k=1,4
            z(i,j)=z(i,j)+junk(i,k)*uo(k,j)
   10 continue
      do 20  i=1,4
        do 20  j=1,4
          junk(i,j)=0.
          do 20  k=1,4
            junk(i,j)=junk(i,j)+vo(i,k)*zr(k,j)
   20 continue

      DO 30  J=1,4
CPM        WRITE(1,10000) J,(junk(k,j),K=1,4)
10000   FORMAT(' Baers EIGENVECTOR ',I1,'     =',4F10.5)
   30 CONTINUE
c
c phiplus=gammal of Baer
c phiminus=gammar of Baer
c But the definition of U and V as a
c function of phi+,phi-  inverted .
c
      PHIM=PI/2.
      PHIP=PI/2.
      IF(U(1,1).NE.0.)phim=atan(u(1,2)/u(1,1))
      IF(V(1,1).NE.0.)phip=atan(v(1,2)/v(1,1))
      if(phim.lt.0.)phim=phim+pi
      if(phip.lt.0.)phip=phip+pi

CPM      WRITE(1,10100) phip,phim
10100 FORMAT(/' CHARGINO PHI+,-   =',2F10.3)

      return
      end
*CMZ :  1.00/00 14/04/95  18.39.55  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      subroutine leplim(mfail)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/XCROS/xgaug(8),xeta(8)
      COMMON/MSSM/TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI

      common/spartcl/fmal(12),fmar(12),ratqa(12),fgamc(12),fgamcr(12)
     +,cosmi(12)

c
c only neutralinos for the time being
c
c FNAL limit
      if(tanb.ge.2.d0.and.fmgaug.le.30.d0)mfail=1
c LEPI limit
      if(tanb.ge.2.d0.and.xgaug(1).le.20.d0)mfail=1
      return
      end

CYG*CMZ :  1.00/00 14/04/95  18.46.25  by  Stavros Katsanevas
CYG*-- Author :    Stavros Katsanevas   14/04/95
CYG      subroutine branch
CYG
CYG      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
CYG
CYG      common/widths/gw,widfl(12),widfr(12),fmeltw,fmertw,fmeluw,fmeruw,
CYG     +gent(50,2,168),gentl(2,2,168),linda(18,6,6)
CYG
CYG      COMMON/OUTMAP/ND,NDIM0,XI(50,10)
CYG
CYG      COMMON/MSSM/TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI
CYG
CYG      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
CYG     +FLC(12),FRC(12),gms(12),echar(12)
CYG
CYG      common/spartcl/fmal(12),fmar(12),ratqa(12),fgamc(12),fgamcr(12)
CYG     +,cosmi(12)
CYG
CYG
CYG      COMMON/NEUMIX/ ZR(4,4),was(4),ESA(4),
CYG     +VOIJL(4,4),VOIJR(4,4),gfir(4,4),gfil(4,4)
CYG      COMMON/CHAMIX/ OIJL(2,2),OIJR(2,2),V(2,2),U(2,2),FM(2),ETA(2)
CYG
CYG      COMMON/KINEM/flum,ecm,s,roots,T,Q,Q2,EN(2)
CYG
CYG      common/variables/fms,fmi,fmk,fml1,fml2,etai,etak,brspa(6,48),
CYG     +lind(6,6,6),brgaug(23,6,6),fmelt,fmert,fmelu,fmeru
CYGc
CYGC index1 =1,6 daughter particles
CYGc index2 =1,6 parent   particles
CYGc i1 =1,6 for 6 kinds of couplings uu,dd,vv,ll,ud,lv
CYG
CYG
CYG      COMMON/SDECAY/DAS,DBS,DCS,DATL,DAUL,DATUL,DASTL,DBSTL,DASUL,
CYG     +DBSUL,DATR,DAUR,DATUR,DASTR,DBSTR,DASUR,DBSUR,xdec(17,64)
CYG      DIMENSION CURRENT(17)
CYG      EQUIVALENCE (CURRENT(1),DAS)
CYG
CYG      COMMON /CONST/ idbg,igener,irad
CYG
CYG
CYG      COMMON/XCROS/xgaug(8),xeta(8)
CYG
CYG      common/reorder/ispa(12,2),kl(2,18),klap(2,18),idecs(12,2)
CYG      common/curind/index1,index2,i1
CYG       
CYG      dimension hfil(4,12),hfir(4,12),yuk(12)
CYG
CYG      logical logvar
CYG      logical hexist
CYG      external hexist
CYG      external sbrdec
CYG      real hbfun2,sbrdec
CYG      external wsc,brodec1,brodec2,brodec3
CYG
CYGc
CYGc variables for vegas
CYGc
CYGc      dimension x(2)
CYG
CYGc
CYGc  sparticle widths
CYGc
CYG
CYG        do 51  l=1,12
CYG          yuk(l)= gms(l)/dsqrt(2.d0)/fmw/sinb
CYG      DO 51  i=1,4
CYG          HFIL(i,l)= -yuk(l)*(-zr(3,i)*sinb+zr(4,i)*cosb)
CYG          HFIR(i,l)= HFIL(i,l)
CYG   51 CONTINUE
CYG
CYG      do 50 k=1,12
CYG
CYG        cos2=cosmi(k)**2
CYG        sin2=1.d0-cos2
CYG      
CYG        widfr(k)=0.
CYG        widfl(k)=0.
CYG
CYG        do 10 l=1,6
CYG
CYG          index=70+l
CYG          fmii=ssmass(index)
CYG
CYG          ik1=idecs(k,1)
CYG          if(l.gt.4)ik1=idecs(k,2)
CYG          fmpart=ssmass(ik1)
CYG
CYG          brspa(l,k)=0
CYG          brspa(l,k+12)=0
CYG          k1=mod(k-1,4)+1
CYG          k2=mod(k-1,2)+1
CYG
CYG          if(fmal(k).gt.(fmii+fmpart))then
CYGc
CYGc         zero quark mass approximation 
CYGc
CYG          fla=xlamda(fmal(k)**2,fmpart**2,fmii**2)
CYG      const=g2*fla/16.d0/pi/fmal(k)**3*(fmal(k)**2-fmii**2-fmpart**2)
CYG          if(const.eq.0.)go to 10
CYG          rat=4.*xeta(l)*fmpart*fmii/(fmal(k)**2-fmii**2-fmpart**2)
CYG
CYG          if(l.lt.5)then
CYG          a1=hfil(l,k)**2+gfil(l,k1)**2
CYG          a2=-rat*gfil(l,k1)*hfil(l,k)
CYG          else
CYG          if(k2.eq.1)then
CYG          a1=v(l-4,1)**2+u(l-4,2)**2*yuk(k)**2
CYG          a2=+rat*v(l-4,1)*u(l-4,2)*yuk(k)
CYG          else
CYG          a1=u(l-4,1)**2+v(l-4,2)**2*yuk(k)**2
CYG          a2=+rat*u(l-4,1)*v(l-4,2)*yuk(k)
CYG          endif
CYG          endif
CYG
CYG          brspa(l,k)=const*(a1+a2*rat)
CYG
CYG          endif
CYG
CYG          if(fmar(k).gt.(fmii+fmpart))then
CYGc
CYGc         zero quark mass approximation 
CYGc
CYG          fla=xlamda(fmar(k)**2,fmpart**2,fmii**2)
CYG      const=g2*fla/16.d0/pi/fmar(k)**3*(fmar(k)**2-fmii**2-fmpart**2)
CYG          if(const.eq.0.)go to 10
CYG          rat=4.*xeta(l)*fmpart*fmii/(fmar(k)**2-fmii**2-fmpart**2)
CYG
CYG          if(l.lt.5)then
CYG          a1=hfir(l,k)**2+gfir(l,k1)**2
CYG          a2=-rat*gfir(l,k1)*hfir(l,k)
CYG          else
CYG          if(k2.eq.1)then
CYG          a1=v(l-4,2)**2
CYG          a2=0.
CYG          else
CYG          a1=u(l-4,2)**2
CYG          a2=0.
CYG          endif
CYG          endif
CYG
CYG          brspa(l,k+12)=const*(a1+a2*rat)
CYG
CYG          endif
CYG
CYG
CYGc stop below top
CYG
CYG        if(k.eq.9.and.l.lt.5)then
CYG        if((fmal(k).lt.gms(9)+fmii).and.fmal(k).gt.fmii)then
CYG      brspa(l,k)=0.0000000003d0/fmal(k)**3*(fmal(k)**2-fmii**2)**2
CYG        endif
CYG        if((fmar(k).lt.gms(9)+fmii).and.fmar(k).gt.fmii)then
CYG      brspa(l,k+12)=0.0000000003d0/fmar(k)**3*(fmar(k)**2-fmii**2)**2
CYG        endif
CYG        endif
CYG
CYG        widfl(k)=widfl(k)+brspa(l,k)
CYG        widfr(k)=widfr(k)+brspa(l,k+12)
CYG
CYGc
CYGc take into accout the mixing
CYGc
CYG        br1=brspa(l,k)*cos2+brspa(l,k+12)*sin2
CYG        br2=brspa(l,k)*sin2+brspa(l,k+12)*cos2
CYG        brspa(l,k)=br1
CYG        brspa(l,k+12)=br2
CYG
CYG   10   continue
CYG   50 continue
CYG
CYG
CYG      write (1,*) ' Sparticle widths  (GeV) '
CYG      write (1,10000) ' SUPR ',widfl(1),' SUPL ',widfr(1)
CYG      write (1,10000) ' SDNR ',widfl(2),' SDNL ',widfr(2)
CYG      write (1,10000) ' SELR ',widfl(4),' SELL ',widfr(4)
CYG      write (1,10000) ' SNU ',widfl(3)
CYG      write (1,10000) ' STPL ',widfl(9) ,' STPR ',widfr(9)
CYG      write (1,10000) ' SBTL ',widfl(10),' SBTR ',widfr(10)
CYG      write (1,10000) ' STAL ',widfl(12),' STAR ',widfr(12)
CYG
CYG10000 FORMAT(/a5,f20.10,a7,f20.10)
CYG
CYG      call wiconst
CYG
CYG      fma=ssmass(71)
CYG      fmb=ssmass(75)
CYG
CYG      indx=0
CYG
CYG      do 80 index1=1,6
CYG        do 80 index2=1,6
CYG          do 80 i1=1,23
CYG
CYG            brgaug(i1,index2,index1)=0.
CYG
CYGc
CYGc products
CYGc
CYG            li1=mod(i1-1,6)+1
CYG
CYG            if(index1.gt.4)call absalom
CYG
CYG            nc=lind(li1,index2,index1)
CYG            if(nc.eq.0)go to 80
CYGc
CYG            fmi=ssmass(70+index1)
CYG            fmk=ssmass(70+index2)
CYG
CYG            if(index1.le.4.and.(fmi+fma).gt.ecm)go to 80
CYG            if(index1.gt.4.and.(fmi+fmb).gt.ecm)go to 80
CYG
CYG      if(i1.eq.19)go to 44
CYG      if(i1.gt.19.and.i1.le.22)go to 444
CYG      if(i1.eq.23)go to 4444
CYG                                 
CYGc
CYGc find mass of 2 fermion products
CYGc
CYG            imk1=kl(1,i1)
CYG            imk2=kl(2,i1)
CYG
CYG
CYG            fml1=ssmass(imk1)
CYG            fml2=ssmass(imk2)
CYGc
CYGc permit the creation of true particles, so at least 2 hadrons
CYGc must be formed
CYGc
CYG            if(iabs(imk1).eq.1)fml1=ulmass(211)
CYG            if(iabs(imk1).eq.2)fml1=ulmass(211)
CYG            if(iabs(imk1).eq.3)fml1=ulmass(321)
CYG            if(iabs(imk1).eq.4)fml1=ulmass(411)
CYG            if(iabs(imk1).eq.5)fml1=ulmass(521)
CYG
CYG            if(iabs(imk2).eq.1)fml2=ulmass(211)
CYG            if(iabs(imk2).eq.2)fml2=ulmass(211)
CYG            if(iabs(imk2).eq.3)fml2=ulmass(321)
CYG            if(iabs(imk2).eq.4)fml2=ulmass(411)
CYG            if(iabs(imk2).eq.5)fml2=ulmass(521)
CYG
CYGc
CYGc check whether I have enough energy to produce them
CYGc
CYG
CYG            q=fmi-fmk-fml1-fml2
CYG            if(q.lt.0.)go to 80
CYG
CYG            smin=(fml1+fml2)**2
CYG            smax=(fmi-fmk)**2
CYG
CYG            umin=(fmk+fml2)**2
CYG            umax=(fmi-fml1)**2
CYG
CYG            tmin=(fmk+fml1)**2
CYG            tmax=(fmi-fml2)**2
CYG
CYG            if(smin.ge.smax)go to 80
CYG            if(umin.ge.umax)go to 80
CYG            if(tmin.ge.tmax)go to 80
CYG
CYGc
CYGc intermediate particles
CYGc
CYG
CYG            fms=fmz
CYG            gw=fmz*gammaz
CYG
CYG            if(index2.gt.4.and.index1.le.4)then
CYG              fms=fmw
CYG              gw=fmw*gammaw
CYG            endif
CYG
CYG            if(index1.gt.4.and.index2.le.4)then
CYG              fms=fmw
CYG              gw=fmw*gammaw
CYG            endif
CYG
CYG
CYG            lk1= klap(1,i1)
CYG            lk2= klap(2,i1)
CYG
CYG
CYG            do 60 k=1,12
CYG              if(ispa(k,1).eq.lk1)then
CYG
CYG                fmelt=fmal(k)
CYG                fmert=fmar(k)
CYG                fmeltw=widfl(k)*fmelt
CYG                fmertw=widfr(k)*fmert
CYGc
CYGc different for 3d generation because of mixing.
CYGc
CYG                if(k.ge.9)then
CYG                  cos2=cosmi(k)**2
CYG                  sin2=1.d0-cos2
CYG                  fmelt=dsqrt(fmal(k)**2*cos2+fmar(k)*sin2)
CYG                  fmert=dsqrt(fmar(k)**2*cos2+fmal(k)*sin2)
CYG                  fmeltw=(widfl(k)*cos2+widfr(k)*sin2)*fmelt
CYG                  fmertw=(widfr(k)*cos2+widfl(k)*sin2)*fmert
CYG                endif
CYG
CYG              endif
CYG              if(ispa(k,1).eq.lk2)then
CYG                fmelu=fmal(k)
CYG                fmeru=fmar(k)
CYG                fmeluw=widfl(k)*fmelu
CYG                fmeruw=widfr(k)*fmeru
CYGc
CYGc different for 3d generation because of mixing.
CYGc
CYG                if(k.ge.9)then
CYG                  cos2=cosmi(k)**2
CYG                  sin2=1.d0-cos2
CYG                  fmelu=dsqrt(fmal(k)**2*cos2+fmar(k)*sin2)
CYG                  fmeru=dsqrt(fmar(k)**2*cos2+fmal(k)*sin2)
CYG                  fmeluw=(widfl(k)*cos2+widfr(k)*sin2)*fmelu
CYG                  fmeruw=(widfr(k)*cos2+widfl(k)*sin2)*fmeru
CYG                endif
CYG              endif
CYG
CYG   60       continue
CYG
CYG
CYG            etai=xeta(index1)
CYG            etak=xeta(index2)
CYGc
CYGc decay constants
CYGc
CYG            do 70 j=1,17
CYG   70       current(j)=xdec(j,nc)
CYG       
CYGc            write(1,*) index1,index2,i1
CYG
CYG            if((smax-smin).gt.1.)then
CYG            fa1=ssdint(smin,wsc,smax)
CYG            if(fa1.lt.0.)fa1=0.
CYG            else
CYG            sb=(smax-smin)/2.
CYG            fa1=wsc(sb)*(smax-smin)
CYG            if(fa1.lt.0.)fa1=0.
CYG            write (6,801) index2, index1, i1
CYG            write (1,801) index2,index1, i1
CYG 801      format(' small q value for ',3i5,' approximation taken')
CYG            endif
CYG
CYGc      call VEGAS(brodec1,0.005d0,2,5000,20,0,0,avgi1,SD1,IT1)
CYGc      call VEGAS(brodec2,0.005d0,2,5000,20,0,0,avgi2,SD2,IT2)
CYGc      call VEGAS(brodec3,0.005d0,2,5000,20,0,0,avgi3,SD3,IT3)
CYGc      avgi=avgi1+avgi2+avgi3
CYGc      if(avgi.eq.0.)go to 4
CYGc      diff=(avgi-fa1)/fa1*100.
CYGc      idiff=diff
CYGc      if(dabs(diff).gt.2.d0)then
CYGc      print *,' WARNING '
CYGc      print *,' vegas differs from analytical by ',idiff,'%'
CYGc      print *,' vegas ',avgi,it1,it2,it3
CYGc      print *,' analytical ',fa1
CYGc      print *,' decay type  ',index1,index2,i1
CYGc      endif
CYG
CYG            brgaug(i1,index2,index1)=fa1
CYG
CYG            if(igener.eq.0)go to 80
CYGc
CYGc prepare throwing matrix
CYGc
CYG            indx=indx+1
CYG            if(indx.gt.168)then
CYG              print *,' error more than 168 BR '
CYG              stop 99
CYG            endif
CYG
CYG            linda(i1,index2,index1)=indx
CYG
CYGc      gentl(1,1,indx)=smin
CYGc      gentl(2,1,indx)=smax
CYGc      gentl(1,2,indx)=umin
CYGc      gentl(2,2,indx)=umax
CYGc      do 441 li=1,50
CYGc      sb=xi(li,1)*(smax-smin)+smin
CYGc      ub=xi(li,2)*(umax-umin)+umin
CYGc      gent(li,1,indx)=sb
CYGc      gent(li,2,indx)=ub
CYGc 441  continue
CYG
CYG            nbin1=30
CYG            nbin2=30
CYG            logvar=hexist(indx)
CYG            if(logvar)call hdelet(indx)
CYG            call hbfun2(indx,' ',nbin1,sngl(smin),sngl(smax) ,nbin2,
CYG     +      sngl(umin),sngl(umax),sbrdec)
CYG
CYG
CYG  44  continue 
CYG
CYG      if(index1.gt.4.or.index2.gt.3)go to 4 
CYG      if(index1.eq.1)go to 4
CYG      if(index1.le.index2)go to 4
CYG
CYG      ogntonp = 0.0d0 
CYG      call  NTONPH(index1,index2,FM0,FMGAUG,FMR,TANB,GMS(9),
CYG     + oGNTONP)
CYG
CYG      brgaug(19,index2,index1)=ogntonp
CYG      go to 4 
CYG
CYG 444  continue
CYG
CYG      if(index1.gt.4.or.index2.gt.3)go to 4 
CYG      if(index1.eq.1)go to 4
CYG      if(index1.le.index2)go to 4
CYG
CYG      ogntonlh  = 0.0d0 
CYG      ogntonhh  = 0.0d0
CYG      ogntona   = 0.0d0 
CYG      ogntochpm = 0.0d0
CYG      call  NTOXH(index1,index2,FM0,FMGAUG,FMR,TANB,GMS(9),
CYG     + oGNTONLH, oGNTONHH, oGNTONA, oGNTOCHPM)
CYG
CYG      if(i1.eq.20)then 
CYG       brgaug(i1,index2,index1)=ogntonlh  
CYG      else if(i1.eq.21)then 
CYG       brgaug(i1,index2,index1)=ogntonhh
CYG      else if(i1.eq.22)then 
CYG       brgaug(i1,index2,index1)=ogntona 
CYG      endif 
CYG      go to 4 
CYG
CYG 4444 continue 
CYG
CYG      if(index1.eq.1.or.index1.gt.4)go to 4 
CYG      if(index2.lt.5)go to 4 
CYG
CYG      ogntonlh  = 0.0d0 
CYG      ogntonhh  = 0.0d0
CYG      ogntona   = 0.0d0 
CYG      ogntochpm = 0.0d0
CYG      call  NTOXH(index1,index2,FM0,FMGAUG,FMR,TANB,GMS(9),
CYG     + oGNTONLH, oGNTONHH, oGNTONA, oGNTOCHPM)
CYG
CYG      if(i1.eq.23) then 
CYG       brgaug(23,index2,index1)=ogntochpm 
CYG      endif 
CYG
CYG   4  continue 
CYG
CYG
CYG   80 continue
CYG
CYG
CYG      CALL INTERF
CYG
CYG      return
CYG      end
*CMZ :  1.00/00 14/04/95  18.39.55  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      real function sbrdec(sb,ub)
      real sb,ub
      double precision Dsb,Dub,brdec
      external brdec
      Dsb = DBLE(sb)
      Dub = DBLE(ub)
      Sbrdec = SNGL(brdec(Dsb,Dub))
      end
*CMZ :  1.00/00 14/04/95  18.39.55  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      double precision function brdec(sb,ub)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/SDECAY/DAS,DBS,DCS,DATL,DAUL,DATUL,DASTL,DBSTL,DASUL,
     +DBSUL,DATR,DAUR,DATUR,DASTR,DBSTR,DASUR,DBSUR,xdec(17,64)

      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
     +FLC(12),FRC(12),gms(12),echar(12)

      common/variables/fms,fmi,fmk,fml1,fml2,etai,etak,brspa(6,48),
CPM     +lind(6,6,6),brgaug(23,6,6),fmelt,fmert,fmelu,fmeru
     +lind(6,6,6),brgaug(6,7,11,3),fmelt,fmert,fmelu,fmeru
C 04/96 P.Morawitz & M.Williams  changed brgaug to allow for r-parity decays 

      common/widths/gw,widfl(12),widfr(12),fmeltw,fmertw,fmeluw,fmeruw,
     +gent(50,2,168),gentl(2,2,168),linda(18,7,6)
CPM     +gent(50,2,168),gentl(2,2,168),linda(18,6,6)
C 04/96 P.Morawitz & M.Williams  changed linda to allow for r-parity decays 
c
c      dimension x(2)
c

      brdec=0.
c
c  u= charged slepton
c
      GW1 = ((SB+FML2**2-FML1**2)/(2.*DSQRT(SB))+
     +        (FMI**2-FMK**2-SB) /(2.*DSQRT(SB)))**2
      GW2 = ((SB+FML2**2-FML1**2)**2/(4.*SB)-FML2**2)
      GW3 = ((FMI**2-SB-FMK**2)**2/(4.*SB)-FMK**2)
      if(GW2.le.0..or.GW3.le.0.)return
      GW2 = DSQRT(GW2)
      GW3 = DSQRT(GW3)
      UMIN = GW1-(GW2+GW3)**2
      UMAX = GW1-(GW2-GW3)**2
      if(umin.ge.umax)return
      if(ub.le.umin.or.ub.ge.umax)return
      TB = FMI**2+FMK**2+FML1**2+FML2**2-SB-UB


c      GW1 = ((SB+FML1**2-FML2**2)/(2.*DSQRT(SB))+
c     +        (FMI**2-FMK**2-SB) /(2.*DSQRT(SB)))**2
c      GW2 = ((SB+FML1**2-FML2**2)**2/(4.*SB)-FML1**2)
c      GW3 = ((FMI**2-SB-FMK**2)**2/(4.*SB)-FMK**2)
c      if(GW2.le.0..or.GW3.le.0.)return
C      GW2 = DSQRT(GW2)
C      GW3 = DSQRT(GW3)
c      TMIN = GW1-(GW2+GW3)**2
c      TMAX = GW1-(GW2-GW3)**2
c      if(tmin.ge.tmax)return
c      if(tb.lt.tmin.or.tb.gt.tmax)return
c      UB = FMI**2+FMK**2+FML1**2+FML2**2-SB-TB

      fmi2=fmi**2
      fmk2=fmk**2
      sba=2.*etai*etak*fmi*fmk*sb
      tba=(fmi2-tb)*(tb-fmk2)
      uba=(fmi2-ub)*(ub-fmk2)

      tubal1=(tb-fmelt**2)*(ub-fmelu**2)+fmeluw*fmeltw
      subal1=(sb-fms**2)*(ub-fmelu**2)+fmeluw*gw
      tsbal1=(tb-fmelt**2)*(sb-fms**2)+fmeltw*gw
      tubar1=(tb-fmert**2)*(ub-fmeru**2)+fmertw*fmeruw
      tsbar1=(tb-fmert**2)*(sb-fms**2)+fmertw*gw
      subar1=(sb-fms**2)*(ub-fmeru**2)+fmeruw*gw

      tbmert=(tb-fmert**2)**2+fmertw**2
      tbmelt=(tb-fmelt**2)**2+fmeltw**2
      ubmeru=(ub-fmeru**2)**2+fmeruw**2
      ubmelu=(ub-fmelu**2)**2+fmeluw**2
      sbfms= (sb-fms**2)**2  +gw**2

      tubal=tbmelt*ubmelu
      subal=sbfms*ubmelu
      tsbal=sbfms*tbmelt

      tubar=tbmert*ubmeru
      subar=sbfms*ubmeru
      tsbar=sbfms*tbmert

      ws=(das*tba+dbs*uba+dcs*sba)/sbfms
      wt=datl*tba/tbmelt+datr*tba/tbmert
      wu=daul*uba/ubmelu+daur*uba/ubmeru

      wtu=datul*sba*tubal1/tubal
     +   +datur*sba*tubar1/tubar
      wst=(2.*dastl*tba+dbstl*sba)*tsbal1/tsbal
     +   +(2.*dastr*tba+dbstr*sba)*tsbar1/tsbar
      wsu=(2.*dasul*uba+dbsul*sba)*subal1/subal
     +   +(2.*dasur*uba+dbsur*sba)*subar1/subar

      brdec=ws+wt+wu+wtu+wst+wsu

      brdec=brdec*alpha**2/32./pi/sinw**4/fmi**3
      return
      end
*CMZ :  1.00/00 14/04/95  18.44.37  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      double precision function brodec1(x)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      common/variables/fms,fmi,fmk,fml1,fml2,etai,etak,brspa(6,48),
CPM     +lind(6,6,6),brgaug(23,6,6),fmelt,fmert,fmelu,fmeru
     +lind(6,6,6),brgaug(6,7,11,3),fmelt,fmert,fmelu,fmeru
C 04/96 P.Morawitz & M.Williams  changed brgaug to allow for r-parity decays 

      common/widths/gw,widfl(12),widfr(12),fmeltw,fmertw,fmeluw,fmeruw,
     +gent(50,2,168),gentl(2,2,168),linda(18,7,6)
CPM     +gent(50,2,168),gentl(2,2,168),linda(18,6,6)
C 04/96 P.Morawitz & M.Williams  changed linda to allow for r-parity decays 
      dimension x(2)

      brodec1=0.

      smin=(fml1+fml2)**2
      smax=(fmi-fmk)**2

      smin1=datan((smin-fms**2)/gw)
      smax1=datan((smax-fms**2)/gw)
      s=smin1+(smax1-smin1)*x(1)
      sb=fms**2+gw*dtan(s)

      tmin=(fmk+fml1)**2
      tmax=(fmi-fml2)**2
      if(fmert.le.fmelt)then
        fmt=fmert
        fmtw=fmertw
      else
        fmt=fmelt
        fmtw=fmeltw
      endif

      tmin1=datan((tmin-fmt**2)/fmtw)
      tmax1=datan((tmax-fmt**2)/fmtw)
      t=tmin1+(tmax1-tmin1)*x(2)
      tb=fmt**2+fmtw*dtan(t)

      BRODEC1=brdec1(sb,tb)
      sa=(smax1-smin1)*(tmax1-tmin1)
      sa=sa*(gw**2+(sb-fms**2)**2)/gw*(fmtw**2+(tb-fmt**2)**2)/fmtw
      BRODEC1=BRODEC1*sa


      return
      end
*CMZ :  1.00/00 14/04/95  18.44.37  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      double precision function brodec2(x)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      common/variables/fms,fmi,fmk,fml1,fml2,etai,etak,brspa(6,48),
CPM     +lind(6,6,6),brgaug(23,6,6),fmelt,fmert,fmelu,fmeru
     +lind(6,6,6),brgaug(6,7,11,3),fmelt,fmert,fmelu,fmeru
C 04/96 P.Morawitz & M.Williams  changed brgaug to allow for r-parity decays 

      common/widths/gw,widfl(12),widfr(12),fmeltw,fmertw,fmeluw,fmeruw,
     +gent(50,2,168),gentl(2,2,168),linda(18,7,6)
CPM     +gent(50,2,168),gentl(2,2,168),linda(18,6,6)
C 04/96 P.Morawitz & M.Williams  changed linda to allow for r-parity decays 
      dimension x(2)
      brodec2=0.

      smin=(fml1+fml2)**2
      smax=(fmi-fmk)**2

      smin1=datan((smin-fms**2)/gw)
      smax1=datan((smax-fms**2)/gw)
      s=smin1+(smax1-smin1)*x(1)
      sb=fms**2+gw*dtan(s)

      umin=(fmk+fml2)**2
      umax=(fmi-fml1)**2
      if(fmeru.le.fmelu)then
        fmu=fmeru
        fmuw=fmeruw
      else
        fmu=fmelu
        fmuw=fmeluw
      endif
      umin1=datan((umin-fmu**2)/fmuw)
      umax1=datan((umax-fmu**2)/fmuw)
      u=umin1+(umax1-umin1)*x(2)
      ub=fmu**2+fmuw*dtan(u)

      BRODEC2=brdec2(sb,ub)
      sa=(smax1-smin1)*(umax1-umin1)
      sa=sa*(gw**2+(sb-fms**2)**2)/gw*(fmuw**2+(ub-fmu**2)**2)/fmuw
      BRODEC2=BRODEC2*sa

      return
      end
*CMZ :  1.00/00 14/04/95  18.44.37  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      double precision function brodec3(x)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      common/variables/fms,fmi,fmk,fml1,fml2,etai,etak,brspa(6,48),
CPM     +lind(6,6,6),brgaug(23,6,6),fmelt,fmert,fmelu,fmeru
     +lind(6,6,6),brgaug(6,7,11,3),fmelt,fmert,fmelu,fmeru
C 04/96 P.Morawitz & M.Williams  changed brgaug to allow for r-parity decays 

      common/widths/gw,widfl(12),widfr(12),fmeltw,fmertw,fmeluw,fmeruw,
     +gent(50,2,168),gentl(2,2,168),linda(18,7,6)
CPM     +gent(50,2,168),gentl(2,2,168),linda(18,6,6)
C 04/96 P.Morawitz & M.Williams  changed linda to allow for r-parity decays 
      dimension x(2)
      brodec3=0.

      umin=(fmk+fml2)**2
      umax=(fmi-fml1)**2
      if(fmeru.le.fmelu)then
        fmu=fmeru
        fmuw=fmeruw
      else
        fmu=fmelu
        fmuw=fmeluw
      endif
      umin1=datan((umin-fmu**2)/fmuw)
      umax1=datan((umax-fmu**2)/fmuw)
      u=umin1+(umax1-umin1)*x(2)
      ub=fmu**2+fmuw*dtan(u)

      tmin=(fmk+fml1)**2
      tmax=(fmi-fml2)**2
      if(fmert.le.fmelt)then
        fmt=fmert
        fmtw=fmertw
      else
        fmt=fmelt
        fmtw=fmeltw
      endif
      tmin1=datan((tmin-fmt**2)/fmtw)
      tmax1=datan((tmax-fmt**2)/fmtw)
      t=tmin1+(tmax1-tmin1)*x(1)
      tb=fmt**2+fmtw*dtan(t)

      BRODEC3=brdec3(tb,ub)
      sa=(tmax1-tmin1)*(umax1-umin1)
      sa=sa*(fmtw**2+(tb-fmt**2)**2)/fmtw*(fmuw**2+(ub-fmu**2)**2)/fmuw
      BRODEC3=BRODEC3*sa

      return
      end
*CMZ :  1.00/00 14/04/95  18.39.56  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      double precision function brdec1(sb,tb)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/SDECAY/DAS,DBS,DCS,DATL,DAUL,DATUL,DASTL,DBSTL,DASUL,
     +DBSUL,DATR,DAUR,DATUR,DASTR,DBSTR,DASUR,DBSUR,xdec(17,64)

      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
     +FLC(12),FRC(12),gms(12),echar(12)

      common/variables/fms,fmi,fmk,fml1,fml2,etai,etak,brspa(6,48),
CPM     +lind(6,6,6),brgaug(23,6,6),fmelt,fmert,fmelu,fmeru
     +lind(6,6,6),brgaug(6,7,11,3),fmelt,fmert,fmelu,fmeru
C 04/96 P.Morawitz & M.Williams  changed brgaug to allow for r-parity decays 

      common/widths/gw,widfl(12),widfr(12),fmeltw,fmertw,fmeluw,fmeruw,
     +gent(50,2,168),gentl(2,2,168),linda(18,7,6)
CPM     +gent(50,2,168),gentl(2,2,168),linda(18,6,6)
C 04/96 P.Morawitz & M.Williams  changed linda to allow for r-parity decays 

c      dimension x(2)

      brdec1=0.

      GW1 = ((SB+FML1**2-FML2**2)/(2.*DSQRT(SB))+
     +        (FMI**2-FMK**2-SB) /(2.*DSQRT(SB)))**2
      GW2 = ((SB+FML1**2-FML2**2)**2/(4.*SB)-FML1**2)
      GW3 = ((FMI**2-SB-FMK**2)**2/(4.*SB)-FMK**2)
      if(GW2.le.0..or.GW3.le.0.)return
      GW2 = DSQRT(GW2)
      GW3 = DSQRT(GW3)
      TMIN = GW1-(GW2+GW3)**2
      TMAX = GW1-(GW2-GW3)**2
      if(tmin.ge.tmax)return
      if(tb.le.tmin.or.tb.ge.tmax)return
      UB = FMI**2+FMK**2+FML1**2+FML2**2-SB-TB

      fmi2=fmi**2
      fmk2=fmk**2
      sba=2.*etai*etak*fmi*fmk*sb
      uba=(fmi2-ub)*(ub-fmk2)
      tba=(fmi2-tb)*(tb-fmk2)

      tsbal1=(tb-fmelt**2)*(sb-fms**2)+fmeltw*gw
      tsbar1=(tb-fmert**2)*(sb-fms**2)+fmertw*gw
      tbmert=(tb-fmert**2)**2+fmertw**2
      tbmelt=(tb-fmelt**2)**2+fmeltw**2
      sbfms= (sb-fms**2)**2  +gw**2
      tsbal=sbfms*tbmelt
      tsbar=sbfms*tbmert

      ws=(das*tba+dbs*uba+dcs*sba)/sbfms
      wt=datl*tba/tbmelt+datr*tba/tbmert
      wst=(2.*dastl*tba+dbstl*sba)*tsbal1/tsbal
     +   +(2.*dastr*tba+dbstr*sba)*tsbar1/tsbar

      brdec1=ws+wt+wst
      brdec1=brdec1*alpha**2/32./pi/sinw**4/fmi**3
      return
      end
*CMZ :  1.00/00 14/04/95  18.39.56  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      double precision function brdec2(sb,ub)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/SDECAY/DAS,DBS,DCS,DATL,DAUL,DATUL,DASTL,DBSTL,DASUL,
     +DBSUL,DATR,DAUR,DATUR,DASTR,DBSTR,DASUR,DBSUR,xdec(17,64)

      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
     +FLC(12),FRC(12),gms(12),echar(12)

      common/variables/fms,fmi,fmk,fml1,fml2,etai,etak,brspa(6,48),
CPM     +lind(6,6,6),brgaug(23,6,6),fmelt,fmert,fmelu,fmeru
     +lind(6,6,6),brgaug(6,7,11,3),fmelt,fmert,fmelu,fmeru
C 04/96 P.Morawitz & M.Williams  changed brgaug to allow for r-parity decays 

      common/widths/gw,widfl(12),widfr(12),fmeltw,fmertw,fmeluw,fmeruw,
     +gent(50,2,168),gentl(2,2,168),linda(18,7,6)
CPM     +gent(50,2,168),gentl(2,2,168),linda(18,6,6)
C 04/96 P.Morawitz & M.Williams  changed linda to allow for r-parity decays 
      dimension x(2)

      brdec2=0.
c
c  u= charged slepton

      GW1 = ((SB+FML2**2-FML1**2)/(2.*DSQRT(SB))+
     +        (FMI**2-FMK**2-SB) /(2.*DSQRT(SB)))**2
      GW2 = ((SB+FML2**2-FML1**2)**2/(4.*SB)-FML2**2)
      GW3 = ((FMI**2-SB-FMK**2)**2/(4.*SB)-FMK**2)
      if(GW2.le.0..or.GW3.le.0.)return
      GW2 = DSQRT(GW2)
      GW3 = DSQRT(GW3)
      UMIN = GW1-(GW2+GW3)**2
      UMAX = GW1-(GW2-GW3)**2
      if(umin.ge.umax)return
      if(ub.le.umin.or.ub.ge.umax)return
      TB = FMI**2+FMK**2+FML1**2+FML2**2-SB-UB

      fmi2=fmi**2
      fmk2=fmk**2
      sba=2.*etai*etak*fmi*fmk*sb
      tba=(fmi2-tb)*(tb-fmk2)
      uba=(fmi2-ub)*(ub-fmk2)

      subal1=(sb-fms**2)*(ub-fmelu**2)+fmeluw*gw
      subar1=(sb-fms**2)*(ub-fmeru**2)+fmeruw*gw
      ubmeru=(ub-fmeru**2)**2+fmeruw**2
      ubmelu=(ub-fmelu**2)**2+fmeluw**2
      sbfms= (sb-fms**2)**2  +gw**2
      subal=sbfms*ubmelu
      subar=sbfms*ubmeru

      wu=daul*uba/ubmelu+daur*uba/ubmeru
      wsu=(2.*dasul*uba+dbsul*sba)*subal1/subal
     +   +(2.*dasur*uba+dbsur*sba)*subar1/subar

      brdec2=wu+wsu
      brdec2=brdec2*alpha**2/32./pi/sinw**4/fmi**3
      return
      end
*CMZ :  1.00/00 14/04/95  18.39.56  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      double precision function brdec3(tb,ub)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/SDECAY/DAS,DBS,DCS,DATL,DAUL,DATUL,DASTL,DBSTL,DASUL,
     +DBSUL,DATR,DAUR,DATUR,DASTR,DBSTR,DASUR,DBSUR,xdec(17,64)

      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
     +FLC(12),FRC(12),gms(12),echar(12)

      common/variables/fms,fmi,fmk,fml1,fml2,etai,etak,brspa(6,48),
CPM     +lind(6,6,6),brgaug(23,6,6),fmelt,fmert,fmelu,fmeru
     +lind(6,6,6),brgaug(6,7,11,3),fmelt,fmert,fmelu,fmeru
C 04/96 P.Morawitz & M.Williams  changed brgaug to allow for r-parity decays 

      common/widths/gw,widfl(12),widfr(12),fmeltw,fmertw,fmeluw,fmeruw,
     +gent(50,2,168),gentl(2,2,168),linda(18,7,6)
CPM     +gent(50,2,168),gentl(2,2,168),linda(18,6,6)
C 04/96 P.Morawitz & M.Williams  changed linda to allow for r-parity decays 
      dimension x(2)

      brdec3=0.
c
c  u= charged slepton
c
      smin=(fml1+fml2)**2
      smax=(fmi-fmk)**2
      SB = FMI**2+FMK**2+FML1**2+FML2**2-TB-UB
      if(sb.le.smin.or.sb.ge.smax)return

      GW1 = ((SB+FML2**2-FML1**2)/(2.*DSQRT(SB))+
     +        (FMI**2-FMK**2-SB) /(2.*DSQRT(SB)))**2
      GW2 = ((SB+FML2**2-FML1**2)**2/(4.*SB)-FML2**2)
      GW3 = ((FMI**2-SB-FMK**2)**2/(4.*SB)-FMK**2)
      if(GW2.le.0..or.GW3.le.0.)return
      GW2 = DSQRT(GW2)
      GW3 = DSQRT(GW3)
      UMIN = GW1-(GW2+GW3)**2
      UMAX = GW1-(GW2-GW3)**2
      if(umin.ge.umax)return
      if(ub.le.umin.or.ub.ge.umax)return

      GW1 = ((SB+FML1**2-FML2**2)/(2.*DSQRT(SB))+
     +        (FMI**2-FMK**2-SB) /(2.*DSQRT(SB)))**2
      GW2 = ((SB+FML1**2-FML2**2)**2/(4.*SB)-FML1**2)
      GW3 = ((FMI**2-SB-FMK**2)**2/(4.*SB)-FMK**2)
      if(GW2.le.0..or.GW3.le.0.)return
      GW2 = DSQRT(GW2)
      GW3 = DSQRT(GW3)
      TMIN = GW1-(GW2+GW3)**2
      TMAX = GW1-(GW2-GW3)**2
      if(tmin.ge.tmax)return
      if(tb.le.tmin.or.tb.ge.tmax)return

      fmi2=fmi**2
      fmk2=fmk**2
      sba=2.*etai*etak*fmi*fmk*sb
      tba=(fmi2-tb)*(tb-fmk2)
      uba=(fmi2-ub)*(ub-fmk2)

      tubal1=(tb-fmelt**2)*(ub-fmelu**2)+fmeluw*fmeltw
      tubar1=(tb-fmert**2)*(ub-fmeru**2)+fmertw*fmeruw
      tbmert=(tb-fmert**2)**2+fmertw**2
      tbmelt=(tb-fmelt**2)**2+fmeltw**2
      ubmeru=(ub-fmeru**2)**2+fmeruw**2
      ubmelu=(ub-fmelu**2)**2+fmeluw**2

      tubal=tbmelt*ubmelu
      tubar=tbmert*ubmeru

      wtu=datul*sba*tubal1/tubal+datur*sba*tubar1/tubar

      brdec3=wtu
      brdec3=brdec3*alpha**2/32./pi/sinw**4/fmi**3

      return
      end
*CMZ :  1.00/00 14/04/95  18.46.25  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      SUBROUTINE WICONST

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
     +FLC(12),FRC(12),gms(12),echar(12)

      COMMON/MSSM/TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI

      common/spartcl/fmal(12),fmar(12),ratqa(12),fgamc(12),fgamcr(12)
     +,cosmi(12)



      COMMON/NEUMIX/ ZR(4,4),was(4),ESA(4),
     +VOIJL(4,4),VOIJR(4,4),gfir(4,4),gfil(4,4)
      COMMON/CHAMIX/ OIJL(2,2),OIJR(2,2),V(2,2),U(2,2),FM(2),ETA(2)
      COMMON/CHANEU/oijlp(4,2),oijrp(4,2)

      COMMON/KINEM/flum,ecm,s,roots,T,Q,Q2,EN(2)

      COMMON/SDECAY/DAS,DBS,DCS,DATL,DAUL,DATUL,DASTL,DBSTL,DASUL,
     +DBSUL,DATR,DAUR,DATUR,DASTR,DBSTR,DASUR,DBSUR,XDEC(17,64)
      DIMENSION CURRENT(17)
      EQUIVALENCE (CURRENT(1),DAS)

      common/variables/fms,fmi,fmk,fml1,fml2,etai,etak,brspa(6,48),
CPM     +lind(6,6,6),brgaug(23,6,6),fmelt,fmert,fmelu,fmeru
     +lind(6,6,6),brgaug(6,7,11,3),fmelt,fmert,fmelu,fmeru
C 04/96 P.Morawitz & M.Williams  changed brgaug to allow for r-parity decays 

      COMMON/XCROS/xgaug(8),xeta(8)

      COMMON /CONST/ idbg,igener,irad


C 64 entries
C first   24 concern decays of neutralinos to neutralinos 6*4
C second  32 concern neutralinos to charginos
C last     8 concern decays of charginos to charginos
C
c
C  X0i --- > X0k Z0
C
      do 10   i=1,6
        do 10   j=1,6
          do 10   k=1,6
   10 lind(i,j,k)=0

      nc=0
c loop over father
      do 30 i=2,4
        k1=i-1
c loop over son
        do 30 k=1,k1
c loop over lepton kind
          do 30 l=1,4

            col=3.
            if(l.gt.2)col=1.

            nc=nc+1
            lind(l,i,k)=0
            lind(l,k,i)=nc

            das=col*4.*voijl(k,i)**2*(flc(l)**2+frc(l)**2)/cosw**4
            dbs=das
            dcs=das

            datl=col*(gfil(k,l)*gfil(i,l))**2
            daul=datl
            datul=datl

            datr=col*(gfir(k,l)*gfir(i,l))**2
            daur=datr
            datur=datr

            dastl=col*2.*gfil(k,l)*gfil(i,l)*voijl(k,i)*flc(l)/cosw**2
            dbstl=dastl
            dasul=dastl
            dbsul=dastl

            dastr=col*2.*gfir(k,l)*gfir(i,l)*voijr(k,i)*frc(l)/cosw**2
            dbstr=dastr
            dasur=dastr
            dbsur=dastr

            do 20 j=1,17
   20       xdec(j,nc)=current(j)

   30 continue
c
c
C  X0i --- > X+k W-
c

c loop over neutralinos
      do 50 i=1,4
c loop over charginos
        do 50 k=1,2
c loop over quarks/leptons
          do 50 lin=5,6

            fmi=xgaug(i)
            fmk=xgaug(k+4)

            l=1
            if(lin.eq.6)l=3

            col=3.
            if(l.gt.2)col=1.

            nc=nc+1

            if(xgaug(i).le.xgaug(k+4))lind(lin,i,k+4)=nc
            if(xgaug(i).gt.xgaug(k+4))lind(lin,k+4,i)=nc

            das=col*2.*oijlp(i,k)**2
            dbs=col*2.*oijrp(i,k)**2
            dcs=-col*2.*oijrp(i,k)*oijlp(i,k)

            datl=col*(gfil(i,l)*v(k,1))**2
            daul=col*(gfil(i,l+1)*u(k,1))**2
            datul=col*gfil(i,l)*gfil(i,l+1)*v(k,1)*u(k,1)

            datr=0.
            daur=0.
            datur=0.

            dastl=-dsqrt(2.d0)*col*gfil(i,l)*v(k,1)*oijlp(i,k)
            dbstl= dsqrt(2.d0)*col*gfil(i,l)*v(k,1)*oijrp(i,k)
            dasul= dsqrt(2.d0)*col*gfil(i,l+1)*u(k,1)*oijrp(i,k)
            dbsul=-dsqrt(2.d0)*col*gfil(i,l+1)*u(k,1)*oijlp(i,k)

            dastr=0.
            dbstr=0.
            dasur=0.
            dbsur=0.
            do 40 j=1,17
   40       xdec(j,nc)=current(j)
   50 continue

C
c
c  x+i --- > X+k z0
c
      i=2
      k=1
      do 70 l=1,4
        col=3.
        if(l.gt.2)col=1.

        up=float(mod(l,2))
        nc=nc+1
        lind(l,k+4,i+4)=nc

        das=col*4.*((oijl(i,k)*flc(l))**2+(oijr(i,k)*frc(l))**2)
     +  /cosw**4
        dbs=col*4.*((oijr(i,k)*flc(l))**2+(oijl(i,k)*frc(l))**2)
     +  /cosw**4
        dcs=-col*4.*(flc(l)**2+frc(l)**2)*oijl(i,k)*oijr(i,k)/cosw**4

        datl =col*(1.-up)*(v(k,1)*v(i,1))**2
        daul =col*up*(u(k,1)*u(i,1))**2
        datul=0.
        dastl= 2.*col*(1.-up)*v(k,1)*v(i,1)*oijl(i,k)*flc(l)/cosw**2
        dbstl=-2.*col*(1.-up)*v(k,1)*v(i,1)*oijr(i,k)*flc(l)/cosw**2
        dasul=-2.*col*up*u(k,1)*u(i,1)*oijr(i,k)*flc(l)/cosw**2
        dbsul= 2.*col*up*u(k,1)*u(i,1)*oijl(i,k)*flc(l)/cosw**2
        datr=0.
        daur=0.
        datur=0.
        dastr=0.
        dbstr=0.
        dasur=0.
        dbsur=0.
        do 60 j=1,17
   60   xdec(j,nc)=current(j)
   70 continue
c


      return
      end
CYG*CMZ :  1.00/00 14/04/95  18.46.25  by  Stavros Katsanevas
CYG*-- Author :    Stavros Katsanevas   14/04/95
CYG      subroutine interf
CYG
CYG      PARAMETER (NOUT=33)
CYG      INTEGER IDOUT(NOUT)
CYG
CYG      SAVE /SSMODE/
CYGC          SM ( JETSET 7.03 ) ident code definitions.
CYG      INTEGER IDUP,IDDN,IDST,IDCH,IDBT,IDTP
CYG      INTEGER IDNE,IDE,IDNM,IDMU,IDNT,IDTAU
CYG      INTEGER IDGL,IDGM,IDW,IDZ
CYG
CYG      PARAMETER (IDUP=2,IDDN=1,IDST=3,IDCH=4,IDBT=5,IDTP=6)
CYG      PARAMETER (IDNE=12,IDE=11,IDNM=14,IDMU=13,IDNT=16,IDTAU=15)
CYG      PARAMETER (IDGL=21,IDGM=22,IDW=24,IDZ=23)
CYG
CYG      PARAMETER (ISUPL=42,ISDNL=41,ISSTL=43,ISCHL=44,ISBTL=45,ISTPL=46)
CYG      PARAMETER (ISNEL=52,ISEL=51,ISNML=54,ISMUL=53,ISNTL=56,ISTAUL=55)
CYG      PARAMETER (ISUPR=48,ISDNR=47,ISSTR=49,ISCHR=50,ISBTR=61,ISTPR=62)
CYG      PARAMETER (ISER=57,ISMUR=58,ISTAUR=59)
CYG      PARAMETER (ISGL=70)
CYG      PARAMETER (ISZ1=71,ISZ2=72,ISZ3=73,ISZ4=74,ISW1=75,ISW2=76)
CYG      PARAMETER (ISWN1=77,ISWN2=78,ISHL=25,ISHH=35,ISHA=36,ISHC=37)
CYG
CYG      DATA IDOUT/
CYG     +ISZ1,ISZ2,ISZ3,ISZ4,ISW1,ISW2,
CYG     +ISGL,ISUPL,ISDNL,ISSTL,ISCHL,ISBTL,ISTPL,ISUPR,ISDNR,
CYG     +ISSTR,ISCHR,ISBTR,ISTPR,ISEL,ISMUL,ISTAUL,ISNEL,ISNML,ISNTL,
CYG     +ISER,ISMUR,ISTAUR,ISHL,ISHH,ISHA,ISHC,IDTP/
CYG
CYG
CYGC          MXSS                 = maximum number of modes
CYGC          NSSMOD               = number of modes
CYGC          ISSMOD               = initial particle
CYGC          JSSMOD               = final particles
CYGC          GSSMOD               = width
CYGC          BSSMOD               = branching ratio
CYG      INTEGER MXSS
CYG      PARAMETER (MXSS=2000)
CYG      COMMON/SSMODE/NSSMOD,ISSMOD(MXSS),JSSMOD(5,MXSS)
CYG      COMMON/SSMOD1/GSSMOD(MXSS),BSSMOD(MXSS)
CYG      INTEGER NSSMOD,ISSMOD,JSSMOD
CYG      DOUBLE PRECISION GSSMOD,BSSMOD
CYG
CYG      double precision fms,fmi,fmk,fml1,fml2,etai,etak,brspa
CYG     +,brgaug,fmelt,fmert,fmelu,fmeru
CYG
CYG      common/variables/fms,fmi,fmk,fml1,fml2,etai,etak,brspa(6,48),
CYG     +lind(6,6,6),brgaug(23,6,6),fmelt,fmert,fmelu,fmeru
CYG
CYG      double precision fmal,fmar,cosmi,ratqa,fgamc,fgamcr
CYG
CYG      common/spartcl/fmal(12),fmar(12),ratqa(12),fgamc(12),fgamcr(12)
CYG     +,cosmi(12)
CYG
CYG      double precision FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,
CYG     +E2,G2,PI,TWOPI,FLC,FRC,gms,echar
CYG
CYG      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
CYG     +FLC(12),FRC(12),gms(12),echar(12)
CYG
CYG
CYG      DOUBLE PRECISION flum,ECM,s,roots,T,Q,Q2,EN
CYG      COMMON/KINEM/flum,ecm,s,roots,T,Q,Q2,EN(2)
CYG
CYG      DOUBLE PRECISION XGAUG,XETA,brtot
CYG      COMMON/XCROS/xgaug(8),xeta(8)
CYG
CYG      common/ubra/ndeca(-80:80)
CYG      common/ubra1/brtot(2,100,-80:80)
CYGC
CYGC      brgaug(i,j,k) is the integrated branching ratios for gauginos
CYGC      i is one of the 18 patterns uu,dd,vv,ll,ud,lv x3 generations
CYGC      j is one of the son gauginos
CYGC      k is one of the father gauginos
CYGC
CYG      common/reorder/ispa(12,2),kl(2,18),klap(2,18),idecs(12,2)
CYG      common/decsel/idecsel(23)
CYG
CYG      n=0
CYG      do 30 k=1,6
CYG        do 20 j=1,6
CYG          do 10 i=1,23
CYG            if(brgaug(i,j,k).eq.0)go to 10
CYG            if(i.le.18)then
CYG              if(idecsel(i).eq.0)go to 10
CYG              n=n+1
CYG              issmod(n)=70+k
CYG              jssmod(1,n)=70+j
CYG              jssmod(2,n)=-kl(2,i)
CYG              jssmod(3,n)=-kl(1,i)
CYG              if(k.eq.5.or.k.eq.6)jssmod(2,n)=kl(1,i)
CYG              if(k.eq.5.or.k.eq.6)jssmod(3,n)=kl(2,i)
CYG              gssmod(n)=brgaug(i,j,k)
CYG            endif
CYG
CYG      if(i.eq.19)then
CYG      n=n+1
CYG      issmod(n)=70+k
CYG      jssmod(1,n)=70+j
CYG      jssmod(2,n)=22
CYG      gssmod(n)=brgaug(i,j,k)
CYG      endif
CYG
CYG      if(i.eq.20)then
CYG      n=n+1
CYG      issmod(n)=70+k
CYG      jssmod(1,n)=70+j
CYG      jssmod(2,n)=25
CYG      gssmod(n)=brgaug(i,j,k)
CYG      endif
CYG
CYG      if(i.eq.21)then
CYG      n=n+1
CYG      issmod(n)=70+k
CYG      jssmod(1,n)=70+j
CYG      jssmod(2,n)=35
CYG      gssmod(n)=brgaug(i,j,k)
CYG      endif
CYG
CYG      if(i.eq.22)then
CYG      n=n+1
CYG      issmod(n)=70+k
CYG      jssmod(1,n)=70+j
CYG      jssmod(2,n)=36
CYG      gssmod(n)=brgaug(i,j,k)
CYG      endif
CYG
CYG      if(i.eq.23)then
CYG      n=n+1
CYG      issmod(n)=70+k
CYG      jssmod(1,n)=70+j
CYG      jssmod(2,n)=37
CYG      gssmod(n)=brgaug(i,j,k)
CYG      endif
CYG
CYG
CYG   10     continue
CYG   20   continue
CYG   30 continue
CYG
CYGC
CYGC      brspa(i,j) is the integrated branching ratios for sparticles
CYGC      i is one of the 8 gaugino decays
CYGC      j are the sparticle codes 12 left + 12 right
CYG
CYG      do 50 j=1,24
CYG        do 40 i=1,6
CYG          if(brspa(i,j).eq.0.)go to 40
CYG          n=n+1
CYG          il=mod(j-1,12)+1
CYG          kla=(j-1)/12+1
CYG          issmod(n) =ispa(il,kla)
CYG          jssmod(1,n)=70+i
CYG          jssmod(2,n)=idecs(il,1)
CYG
CYGc
CYGc jongling with stop below top
CYGc
CYG          if(i.le.4)then
CYG
CYG          if(j.eq.9.and.(fmal(il).lt.gms(9)+xgaug(i)))then
CYG          jssmod(2,n)=4
CYG          endif
CYG
CYG          if(j.eq.21.and.(fmar(il).lt.gms(9)+xgaug(i)))then
CYG          jssmod(2,n)=4
CYG          endif
CYG
CYG          endif
CYG
CYG          if(i.gt.4)jssmod(2,n)=idecs(il,2)
CYG
CYG        
CYG          jssmod(3,n)=0
CYG          gssmod(n)=brspa(i,j)
CYG
CYG   40   continue
CYG   50 continue
CYG
CYG      nssmod=n
CYG
CYG
CYG      WRITE(1,10000)
CYG10000 FORMAT(/' PARENT -->     DAUGHTERS',14X,'WIDTH (KeV) ',7X,
CYG     +'BRANCHING RATIO'/)
CYG
CYGC          Write all modes
CYG
CYG      DO 60  J=1,NOUT
CYG
CYG        call ssnorm(idout(j))
CYG
CYG        CALL SSPRT(IDOUT(J))
CYG
CYG   60 CONTINUE
CYG
CYG      CALL NEWSUM
CYG
CYGc
CYGc integrate the branching ratios
CYGc
CYG      do 70  k=41,76
CYG   70 call sstot(k)
CYG
CYG      return
CYG      end
CYG*CMZ :  1.00/00 14/04/95  18.44.38  by  Stavros Katsanevas
CYG*-- Author :    Stavros Katsanevas   14/04/95
CYG      CHARACTER*5 FUNCTION SSID(ID)
CYGC-----------------------------------------------------------------------
CYGC
CYGC     Return character name for ID, assuming the default IDENT codes
CYGC     are used in /SSTYPE/.
CYGC                                      modified ISAJET
CYGC-----------------------------------------------------------------------
CYG
CYG      common/ludat4/chaf(500)
CYG      character chaf*8
CYG
CYG      CHARACTER*5 LABEL(-80:80)
CYG      SAVE LABEL
CYG
CYG
CYG      DATA LABEL(0)/'     '/
CYG
CYG      DATA (LABEL(J),J=1,10)
CYG     +/'DN   ','UP   ','ST   ','CH   ','BT   ','TP   '
CYG     +,'ERROR','ERROR','ERROR','ERROR'/
CYG      DATA (LABEL(J),J=-1,-10,-1)
CYG     +/'DB   ','UB   ','SB   ','CB   ','BB   ','TB   '
CYG     +,'ERROR','ERROR','ERROR','ERROR'/
CYG
CYG      DATA (LABEL(J),J=11,20)
CYG     +/'E-   ','NUE  ','MU-  ','NUM  ','TAU- ','NUT  '
CYG     +,'ERROR','ERROR','ERROR','ERROR'/
CYG      DATA (LABEL(J),J=-11,-20,-1)
CYG     +/'E+   ','ANUE ','MU+  ','ANUM ','TAU+ ','ANUT '
CYG     +,'ERROR','ERROR','ERROR','ERROR'/
CYG
CYG      DATA (LABEL(J),J=21,30)
CYG     +/'GLUON','GAMMA','Z0   ','W+   ','H0L  ','ERROR'
CYG     +,'ERROR','ERROR','ERROR','ERROR'/
CYG      DATA (LABEL(J),J=-21,-30,-1)
CYG     +/'GLUON','GAMMA','Z0   ','W-   ','H0L  ','ERROR'
CYG     +,'ERROR','ERROR','ERROR','ERROR'/
CYG
CYG      DATA (LABEL(J),J=31,40)
CYG     +/'ERROR','ERROR','ERROR','ERROR','H0H  ','A0   '
CYG     +,'H+   ','ERROR','ERROR','ERROR'/
CYG      DATA (LABEL(J),J=-31,-40,-1)
CYG     +/'ERROR','ERROR','ERROR','ERROR','H0H  ','A0   '
CYG     +,'H-   ','ERROR','ERROR','ERROR'/
CYG
CYG      DATA (LABEL(J),J=41,50)
CYG     +/'DNL  ','UPL  ','STL ','CHL  ','BT1  ','TP1  '
CYG     +,'DNR  ','UPR  ','STR  ','CHR  '/
CYG      DATA (LABEL(J),J=-41,-50,-1)
CYG     +/'DNLb ','UPLb ','STLb ','CHLb ','BT1b ','TP1b '
CYG     +,'DNRb ','UPRb ','STRb ','CHRb '/
CYG
CYG      DATA (LABEL(J),J=51,60)
CYG     +     /'EL-  ','NUEL ','MUL- ','NUML ','TAU1-','NUTL '
CYG     +     ,'ER-  ','MUR- ','TAU2-','ERROR'/
CYG      DATA (LABEL(J),J=-51,-60,-1)
CYG     +     /'EL+  ','ANUEL','MUL+ ','ANUML','TAU1+','ANUTL'
CYG     +     ,'ER+  ','MUR+ ','TAU2+','ERROR'/
CYG
CYG      DATA (LABEL(J),J=61,70)
CYG     +/'BT2  ','TP2  ','BTL  ','TPL  ','TAUL ','BTR  '
CYG     +,'TPR  ','TAUR ','ERROR','GLSS '/
CYG      DATA (LABEL(J),J=-61,-70,-1)
CYG     +/'BT2b ','TP2b ','BTLb ','TPLb ','TAULb','BTRb '
CYG     +,'TPRb ','TAURb','ERROR','GLSS '/
CYG
CYG      DATA (LABEL(J),J=71,80)
CYG     +/'Z1SS ','Z2SS ','Z3SS ','Z4SS ','W1SS+','W2SS+'
CYG     +,'W1SS-','W2SS-','ERROR','ERROR'/
CYG      DATA (LABEL(J),J=-71,-80,-1)
CYG     +/'Z1SS ','Z2SS ','Z3SS ','Z4SS ','W1SS-','W2SS-'
CYG     +,'W1SS+','W2SS+','ERROR','ERROR'/
CYG
CYG      IF(IABS(ID).GT.80) THEN
CYG        WRITE(1,*) 'SSID: ID = ',ID
CYG        STOP99
CYG      ENDIF
CYG
CYG      if(id.ne.0)then
CYG        id1=iabs(id)
CYG        chaf(id1)=label(id1)
CYG      endif
CYG
CYG      SSID=LABEL(ID)
CYG      RETURN
CYG      END
*CMZ :  1.00/00 14/04/95  18.46.25  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      SUBROUTINE SSPRT(ID)

C-----------------------------------------------------------------------
C
C     Print decay modes for ID. Note these need not be contiguous,
C     so the loop is over all modes in /SSMODE/.
C
C-----------------------------------------------------------------------

      COMMON/SSLUN/LOUT
      INTEGER LOUT
      SAVE /SSLUN/
C          MXSS                 = maximum number of modes
C          NSSMOD               = number of modes
C          ISSMOD               = initial particle
C          JSSMOD               = final particles
C          GSSMOD               = width
C          BSSMOD               = branching ratio
      INTEGER MXSS
      PARAMETER (MXSS=2000)
      COMMON/SSMODE/NSSMOD,ISSMOD(MXSS),JSSMOD(5,MXSS)
      COMMON/SSMOD1/GSSMOD(MXSS),BSSMOD(MXSS)
      INTEGER NSSMOD,ISSMOD,JSSMOD
      DOUBLE PRECISION GSSMOD,BSSMOD

      DOUBLE PRECISION flum,ecm,s,roots,t,q,q2,en
      COMMON/KINEM/flum,ecm,s,roots,T,Q,Q2,EN(2)

      SAVE /SSMODE/
C
      INTEGER ID,I,K,NOUT
      CHARACTER*5 SSID,LBLIN,LBLOUT(3)
C

      widthqq=0.
      brqq=0.
      widthll=0.
      brll=0.
      widthvv=0.
      brvv=0.
      widthlv=0.
      brlv=0.

      NOUT=0
      DO 20  I=1,NSSMOD
        IF(iabs(ISSMOD(I)).NE.ID) GO TO 20
        NOUT=NOUT+1
        LBLIN=SSID(ISSMOD(I))
        DO 10  K=1,3
   10   LBLOUT(K)=SSID(JSSMOD(K,I))

c
c Gamma in KeV
c
        gssmod(i)=gssmod(i)*1000000.d0

CPM        WRITE(1,10000) LBLIN,(LBLOUT(K),K=1,3), GSSMOD(I),BSSMOD(I)

10000   FORMAT(1X,A5,'  -->  ',3(A5,2X),f20.3,f10.3)

   20 CONTINUE
C
CPM      IF(NOUT.GT.0) WRITE(1,*) ' '
C
      RETURN
      END
CYG*CMZ :  1.00/00 14/04/95  18.46.25  by  Stavros Katsanevas
CYG*-- Author :    Stavros Katsanevas   14/04/95
CYG      subroutine newsum
CYG
CYG      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
CYG
CYG      common/variables/fms,fmi,fmk,fml1,fml2,etai,etak,brspa(6,48),
CYG     +lind(6,6,6),brgaug(23,6,6),fmelt,fmert,fmelu,fmeru
CYG
CYG      character*5 gaugi(6),signa(5)
CYG
CYG      data gaugi/'Z1SS ','Z2SS ','Z3SS ','Z4SS ','W1SS ','W2SS '/
CYG      DATA SIGNA/'QQBAR','L+L- ','VVBAR','L+-V ',' QQP '/
CYG
CYG      common/brsum/ brsum(5,6,6),brsuma(6)
CYG
CYG      do 10 k1=1,6
CYG        do 10 k2=1,6
CYG          do 10 k3=1,5
CYG   10 brsum(k3,k2,k1)=0.
CYG
CYG
CYG      do 60 l1=1,6
CYG        brsuma(l1)=0.
CYG
CYG        do 30 l2=1,6
CYG          do 20 l3=1,18
CYG            l4=mod(l3-1,6)+1
CYG            if(l4.eq.1.or.l4.eq.2)l5=3
CYG            if(l4.eq.3)l5=2
CYG            if(l4.eq.4)l5=1
CYG            if(l4.eq.6)l5=4
CYG            if(l4.eq.5)l5=5
CYG
CYG            fact=1.
CYG            if(l2.gt.4.and.l4.gt.4)fact=2.
CYG
CYG            brsum(l5,l2,l1)=brsum(l5,l2,l1)+brgaug(l3,l2,l1)*fact
CYG            brsuma(l1)=brsuma(l1)+brgaug(l3,l2,l1)*fact
CYG
CYG   20     continue
CYG   30   continue
CYG
CYG        if(brsuma(l1).gt.0.)then
CYG          do 50 l2=1,6
CYG            do 40 l5=1,5
CYG              brsum(l5,l2,l1)=brsum(l5,l2,l1)/brsuma(l1)
CYG   40       continue
CYG   50     continue
CYG        endif
CYG
CYG   60 continue
CYG
CYG      WRITE (1,10200)' SIGNA ',signa
CYG
CYG      DO 70 L1=2,6
CYG
CYG        WRITE (1,10100) GAUGI(L1)
CYG
CYG        DO 70 L2=1,6
CYG          stum= BRSUM(1,l2,L1)+BRSUM(2,l2,L1)+BRSUM(3,l2,L1)+BRSUM(4,
CYG     +    l2,L1) +BRSUM(5,L2,L1)
CYG          if(stum.ne.0.)then
CYG            WRITE(1,10000) GAUGI(l2), BRSUM(1,l2,L1),BRSUM(2,l2,L1),
CYG     +      BRSUM(3,l2,L1),BRSUM(4,l2,L1), BRSUM(5,L2,L1)
CYG10000 FORMAT(A7,5F7.3)
CYG          endif
CYG   70 continue
CYG
CYG10100 FORMAT('DECAYS OF ',A7)
CYG10200 FORMAT(6A7)
CYG
CYG      return
CYG      end
*CMZ :  1.00/00 14/04/95  18.46.25  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      SUBROUTINE SSTOT(ID)

C-----------------------------------------------------------------------
C
C     Integrate decay modes for ID.
C     Fill array ndeca(80),brtot(2,50,80) with integrated decay modes
C     ndeca(j) is the number of possible decays for sparticle j
C     brtot(1,i,j) indicates the pointer to ssmode
C     brtot(2,i,j) indicates the integrated branching fraction
C     i loops over possible decays
C     j loops over sparticles
C-----------------------------------------------------------------------

c      SAVE /SSLUN/
C          MXSS                 = maximum number of modes
C          NSSMOD               = number of modes
C          ISSMOD               = initial particle
C          JSSMOD               = final particles
C          GSSMOD               = width
C          BSSMOD               = branching ratio
      INTEGER MXSS
      PARAMETER (MXSS=2000)
      COMMON/SSMODE/NSSMOD,ISSMOD(MXSS),JSSMOD(5,MXSS)
      COMMON/SSMOD1/GSSMOD(MXSS),BSSMOD(MXSS)
      INTEGER NSSMOD,ISSMOD,JSSMOD
      DOUBLE PRECISION GSSMOD,BSSMOD
      SAVE /SSMODE/
C
      DOUBLE PRECISION brtot
      common/ubra/ndeca(-80:80)
      common/ubra1/brtot(2,100,-80:80)
C
C  carefull we assume that particles are contained within  80
C
      kd=iabs(id)
      if(kd.lt.1.or.kd.gt.80) write(*,*) ' error in SSTOT '

      brtot(1,1,id)=0
      brtot(2,1,id)=0

      NOUT=0
      DO 10  I=1,NSSMOD
        IF(iabs(ISSMOD(I)).NE.ID) GO TO 10

        nout=nout+1
        brtot(1,nout,id)=i

        if(nout.eq.1)then
          brtot(2,1,id)=bssmod(i)
        else
          brtot(2,nout,id)=bssmod(i)+brtot(2,nout-1,id)
        endif

   10 CONTINUE

      ndeca(id)=nout

      RETURN
      END
*CMZ :  1.00/00 14/04/95  18.44.38  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      double precision function SSMASS(IDA)
C-----------------------------------------------------------------------
C          Give mass  for ID
C-----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      common/spartcl/fmal(12),fmar(12),ratqa(12),fgamc(12),fgamcr(12)
     +,cosmi(12)



      COMMON/NEUMIX/ ZR(4,4),was(4),ESA(4),
     +VOIJL(4,4),VOIJR(4,4),gfir(4,4),gfil(4,4)

      COMMON/CHAMIX/ OIJL(2,2),OIJR(2,2),V(2,2),U(2,2),FM(2),ETA(2)
      COMMON/CHANEU/oijlp(4,2),oijrp(4,2)

      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
     +FLC(12),FRC(12),gms(12),echar(12)

      common/higgses/sfmh,sfma,sfmhp,sfmhpc,sina,cosa


      common/reorder/ispa(12,2),kl(2,18),klap(2,18),idecs(12,2)

      ssmass=-1.

      id=iabs(ida)

      if(id.lt.40)then
c      PARAMETER (IDUP=2,IDDN=1,IDST=3,IDCH=4,IDBT=5,IDTP=6)
        if(id.eq.1)ssmass=gms(2)
        if(id.eq.2)ssmass=gms(1)
        if(id.eq.3)ssmass=gms(6)
        if(id.eq.4)ssmass=gms(5)
        if(id.eq.5)ssmass=gms(10)
        if(id.eq.6)ssmass=gms(9)
c      PARAMETER (IDNE=12,IDE=11,IDNM=14,IDMU=13,IDNT=16,IDTAU=15)
        if(id.eq.11)ssmass=gms(4)
        if(id.eq.12)ssmass=gms(3)
        if(id.eq.13)ssmass=gms(8)
        if(id.eq.14)ssmass=gms(7)
        if(id.eq.15)ssmass=gms(12)
        if(id.eq.16)ssmass=gms(11)
c      PARAMETER (IDGL=21,IDGM=22,IDW=24,IDZ=23)
        if(id.eq.21)ssmass=0.
        if(id.eq.22)ssmass=0.
        if(id.eq.23)ssmass=fmz
        if(id.eq.24)ssmass=fmw
c
c higgses
c
c      PARAMETER (ISHL=25,ISHH=35,ISHA=36,ISHC=37)
        if(id.eq.25)ssmass=sfmh
        if(id.eq.35)ssmass=sfmhp
        if(id.eq.36)ssmass=sfma
        if(id.eq.37)ssmass=sfmhpc
      else

c      PARAMETER (ISGL=70)
        if(id.eq.70)ssmass=was(1)/0.303
c      PARAMETER (ISZ1=71,ISZ2=72,ISZ3=73,ISZ4=74,ISW1=75,ISW2=76)
        if(id.ge.71.and.id.le.74)then
          ssmass=was(id-70)
          return
        endif

c      PARAMETER (ISW1=75,ISW2=76,ISWN1=77,ISWN2=78)
        if(id.eq.77)id=75
        if(id.eq.78)id=76
        if(id.ge.75.and.id.le.76)then
          ssmass=FM(id-74)
          return
        endif

        if(id.gt.40.and.id.lt.70)then

c      PARAMETER (ISUPL=42,ISDNL=41,ISSTL=43,ISCHL=44,ISBTL=45,ISTPL=46)
          if(id.eq.41)ssmass=fmal(2)
          if(id.eq.42)ssmass=fmal(1)
          if(id.eq.43)ssmass=fmal(6)
          if(id.eq.44)ssmass=fmal(5)
          if(id.eq.45)ssmass=fmal(10)
          if(id.eq.46)ssmass=fmal(9)
c      PARAMETER (ISUPR=48,ISDNR=47,ISSTR=49,ISCHR=50)
          if(id.eq.47)ssmass=fmar(2)
          if(id.eq.48)ssmass=fmar(1)
          if(id.eq.49)ssmass=fmar(6)
          if(id.eq.50)ssmass=fmar(5)

c      PARAMETER (ISNEL=52,ISEL=51,ISNML=54,ISMUL=53,ISNTL=56,ISTAUL=55)
          if(id.eq.51)ssmass=fmal(4)
          if(id.eq.52)ssmass=fmal(3)
          if(id.eq.53)ssmass=fmal(8)
          if(id.eq.54)ssmass=fmal(7)
          if(id.eq.55)ssmass=fmal(12)
          if(id.eq.56)ssmass=fmal(11)

c      PARAMETER (ISER=57,ISMUR=58,ISTAUR=59)
          if(id.eq.57)ssmass=fmar(4)
          if(id.eq.58)ssmass=fmar(8)
          if(id.eq.59)ssmass=fmar(12)

c      PARAMETER (ISBTR=61,ISTPR=62)
          if(id.eq.61)ssmass=fmar(10)
          if(id.eq.62)ssmass=fmar(9)


        endif

      endif


      if(ssmass.lt.0.)write(*,*) ' error in ssmass  unknown code ',ida

      RETURN
      END
*CMZ :  1.00/00 14/04/95  18.46.26  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      SUBROUTINE SSNORM(ID)
C-----------------------------------------------------------------------
C          Normalize branching ratios for ID
C                                      modified ISAJET
C-----------------------------------------------------------------------
      IMPLICIT NONE
      COMMON/SSLUN/LOUT
      INTEGER LOUT
      SAVE /SSLUN/
C          MXSS                 = maximum number of modes
C          NSSMOD               = number of modes
C          ISSMOD               = initial particle
C          JSSMOD               = final particles
C          GSSMOD               = width
C          BSSMOD               = branching ratio
      INTEGER MXSS
      PARAMETER (MXSS=2000)
      COMMON/SSMODE/NSSMOD,ISSMOD(MXSS),JSSMOD(5,MXSS)
      COMMON/SSMOD1/GSSMOD(MXSS),BSSMOD(MXSS)
      INTEGER NSSMOD,ISSMOD,JSSMOD
      DOUBLE PRECISION GSSMOD,BSSMOD
      SAVE /SSMODE/
C
      INTEGER ID,I
      REAL GAMSUM
C
      GAMSUM=0
      DO 10  I=1,NSSMOD
        IF(ISSMOD(I).EQ.ID) GAMSUM=GAMSUM+GSSMOD(I)
   10 CONTINUE
      IF(GAMSUM.EQ.0) RETURN
      DO 20  I=1,NSSMOD
        IF(ISSMOD(I).EQ.ID) BSSMOD(I)=GSSMOD(I)/GAMSUM
   20 CONTINUE
      RETURN
      END
CYG*CMZ :  1.00/00 14/04/95  18.46.26  by  Stavros Katsanevas
CYG*-- Author :    Stavros Katsanevas   14/04/95
CYG      SUBROUTINE mssm_gene
CYG
CYG      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
CYG
CYG
CYG      COMMON/KINEM/flum,ecm,s,roots,T,Q,Q2,EN(2)
CYG      COMMON/INDEXX/index,index1,index2,nevt
CYG      COMMON/FINDEX/fmpr1,fmpr2,XCROST,APRO
CYG
CYG
CYG      COMMON/ISR/ QK(4)
CYG      COMMON /CONST/ idbg,igener,irad
CYG
CYG      real erad,srad
CYG      common/srada/erad(100),srad(100)
CYG      logical wrt,scan,lepi
CYG      common/str/wrt,scan,lepi
CYG
CYG      external gensel,gensmu,gensnue,gensnu,photi
CYG
CYG      common/kev/jev
CYGC
CYGC       MAXIMUM OF  PRODUCTION, UNIFORM STEP SAMPLING
CYGC
CYG      if(nevt.lt.1)return
CYG
CYG      if(irad.eq.1)then
CYG        STEP = 2.
CYG        NSORT =ecm/step
CYG        ZSLEPT = 0.
CYG        DO 10 J=1,NSORT
CYG          ZSLEPT = ZSLEPT+STEP
CYG          s=zslept**2
CYG          roots=dsqrt(s)
CYG          if(index.eq.1)amplit=photi(index1,index2)
CYG          if(index.eq.2)amplit=chargi(index1,index2)
CYG          if(index.eq.3)then
CYG            IF(INDEX1.EQ.51.or.index1.eq.57)then
CYG              if(index1.eq.-index2)amplit=ssdint(-1.d0,gensel,1.d0)
CYG              if(index1.ne.-index2)amplit=genselrs(dummy)
CYG            elseif(index1.eq.52)then
CYG              amplit=ssdint(-1.d0,gensnue,1.d0)
CYG            elseif(index1.eq.54.or.index1.eq.56)then
CYG              amplit=ssdint(-1.d0,gensnu,1.d0)
CYG            else
CYG              amplit=gensmus(dummy)
CYG            endif
CYG          endif
CYG          erad(j)=real(roots)
CYG          srad(j)=real(amplit)
CYG          if(srad(j).ne.0.)write(1,10000) erad(j),srad(j)
CYG10000 format('  ECM =  ',f10.3,'  CROSS SECTION =  ',f10.3)
CYG   10   CONTINUE
CYG      endif
CYG
CYG
CYG      s=ecm**2
CYG
CYG      nfail=0
CYG
CYG      DO 30 JEV=1,NEVT
CYG
CYG   20   continue
CYG
CYG        CALL suseve(ifail)
CYG
CYG        if(ifail.eq.1)then
CYG          nfail=nfail+1
CYG          if(nfail.gt.1000)then
CYG            print *,' warning nfail = ',nfail,' event = ',jev
CYG            print *,' ********  Event skipped  ******** '
CYG            nfail=0
CYG            goto 30
CYG          endif
CYG          go to 20
CYG        endif
CYG
CYG
CYG        if(wrt) CALL SXWRLU(12)
CYG
CYG        call user
CYG
CYG   30 CONTINUE
CYG
CYG
CYG      RETURN
CYG      END
CYG*CMZ :  1.00/00 14/04/95  18.39.57  by  Stavros Katsanevas
CYG*-- Author :    Stavros Katsanevas   14/04/95
CYG      subroutine user
CYG      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
CYG      COMMON/INDEXX/index,index1,index2,nevt
CYG      data icount,ncount/0,0/
CYG      dimension sum(4)
CYG      call luedit(2)
CYG      ptmax=0.
CYG      cosmax=1.
CYG      ncount=ncount+1
CYG      do 1 l=1,n
CYG      if(k(l,2).ne.22.or.p(l,4).eq.0.)go to 1
CYG      costh=abs(p(l,3)/p(l,4))
CYG      if(costh.gt.0.9999)costh=0.9999
CYG      ang=acos(costh)*180./3.14159
CYG      if(ang.lt.10.)go to 1
CYG      pt=p(l,1)**2+p(l,2)**2
CYG      if(pt.gt.ptmax)ptmax=pt
CYG 1    continue
CYG
CYG      if(ptmax.gt.10.**2)then
CYG      icount=icount+1
CYG      print *,' event ',icount,ncount
CYG      endif
CYG
CYG      return
CYG      end
*CMZ :  1.00/00 14/04/95  18.46.26  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      DOUBLE PRECISION FUNCTION PHOTI(IN1,IN2)
C********************************************************************
C production cross section for neutralinos
C                                         author S.Katsanevas
C********************************************************************

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
     +FLC(12),FRC(12),gms(12),echar(12)

      COMMON/MSSM/TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI

      common/spartcl/fmal(12),fmar(12),ratqa(12),fgamc(12),fgamcr(12)
     +,cosmi(12)



      COMMON/NEUMIX/ ZR(4,4),was(4),ESA(4),
     +VOIJL(4,4),VOIJR(4,4),gfir(4,4),gfil(4,4)

      COMMON/CHAMIX/ OIJL(2,2),OIJR(2,2),V(2,2),U(2,2),FM(2),ETA(2)
      COMMON/KINEM/flum,ecm,s,roots,T,Q,Q2,EN(2)

      i1=iabs(in1)-70
      i2=iabs(in2)-70

      PHOTI=0.
      DIJ=1.
      FMEL=FMAL(4)
      FMER=FMAR(4)
      IF(I1.NE.I2)DIJ=0.
      Q2=(S-(WAS(I1)+WAS(I2))**2)
      IF(Q2.LE.0)RETURN
      Q2=(S-(WAS(I1)+WAS(I2))**2)*(S-(WAS(I1)-WAS(I2))**2)/4.D0/S
      Q=DSQRT(Q2)
      EN(1)=DSQRT(Q2+WAS(I1)**2)
      EN(2)=DSQRT(Q2+WAS(I2)**2)
C
C phase space
C
      DL=(S+2.D0*FMEL**2-WAS(I1)**2-WAS(I2)**2)/2.D0/S
      DR=(S+2.D0*FMER**2-WAS(I1)**2-WAS(I2)**2)/2.D0/S

      DLA=DL-Q/ROOTS
      DRA=DR-Q/ROOTS
      IF(DLA.eq.0.)DLA=0.00001
      IF(DRA.eq.0.)DRA=0.00001
      AL=DLOG(ABS((DL+Q/ROOTS)/DLA))
      AR=DLOG(ABS((DR+Q/ROOTS)/DRA))

      GAM=(EN(1)*EN(2)+Q2/3.D0-esa(i1)*esa(i2)*WAS(I1)*WAS(I2))
      GAML=1.D0-2.D0*DL-esa(i1)*esa(i2)*WAS(I1)*WAS(I2)/S/DL
      GAMR=1.D0-2.D0*DR-esa(i1)*esa(i2)*WAS(I1)*WAS(I2)/S/DR
C
C propagators
C
      DZ2=1.D0/((S-FMZ**2)**2+FMZ**2*GAMMAZ**2)
      REDZ=(S-FMZ**2)*DZ2
C
C couplings
C
      FLE=FLC(4)
      FRE=FRC(4)
      CLR=FLE**2+FRE**2
C
C z propagator
C
      SIZ=G2**2*DZ2/4.D0/PI/COSW**4*Q/ROOTS
      SZ=SIZ*VOIJL(I1,I2)**2*CLR*GAM*0.389D+9
C
C sel propagator
C
      SDLA=S*DL**2-Q2
      SDRA=S*DR**2-Q2
      if(SDLA.EQ.0)SDLA=0.0001
      if(SDRA.EQ.0)SDRA=0.0001

      SEL=G2**2/16.D0/PI*Q/S/ROOTS
      A1=(EN(1)*EN(2)-S*DL+Q2)/SDLA+2.D0+ROOTS/2.D0/Q*GAML*AL
      A2=(EN(1)*EN(2)-S*DR+Q2)/SDRA+2.D0+ROOTS/2.D0/Q*GAMR*AR
      S1=(A1*gFIL(I1,4)**2*gFIL(I2,4)**2+A2*gFIR(I1,4)**2*gFIR(I2,4)**2)
      SEL=SEL*s1*0.389D+9
C
C z-sel interference
C
      SZSEL=-G2**2/8.D0/PI/COSW**2*Q/ROOTS*REDZ*VOIJL(I1,I2)
      A1=((EN(1)*EN(2)-S*DL*(1.D0-DL)-esa(i1)*esa(i2)*WAS(I1)*WAS(I2))
     +*AL/Q/ROOTS+2.D0*(1.-DL))*FLE*gFIL(I1,4)*gFIL(I2,4)
      A2=((EN(1)*EN(2)-S*DR*(1.D0-DR)-esa(i1)*esa(i2)*WAS(I1)*WAS(I2))
     +*AR/Q/ROOTS+2.D0*(1.-DR))*FRE*gFIR(I1,4)*gFIR(I2,4)
      SZSEL=SZSEL*(A1-A2)*0.389D+9
C
C total
C
c      write (6,*) ' z      neutralino ',sz
c      write (6,*) ' snu    neutralino ',sel
c      write (6,*) ' z-snu  neutralino ',szsel

      PHOTI=(SZ+SEL+SZSEL)/2.D0*(2.D0-DIJ)

      if(photi.lt.0.)then
        write (6,10000) photi
10000 FORMAT(1X,' ERROR in photi cross section ',f8.2)
        photi=0.
      endif

      RETURN
      END
*CMZ :  1.00/00 14/04/95  18.39.57  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      DOUBLE PRECISION FUNCTION GENPHO(IN1,IN2,COSTHE)
C********************************************************************
C production cross section ds/dcostheta for neutralinos
C                                         author S.Katsanevas
C********************************************************************
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
     +FLC(12),FRC(12),gms(12),echar(12)

      COMMON/MSSM/TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI

      common/spartcl/fmal(12),fmar(12),ratqa(12),fgamc(12),fgamcr(12)
     +,cosmi(12)



      COMMON/NEUMIX/ ZR(4,4),was(4),ESA(4),
     +VOIJL(4,4),VOIJR(4,4),gfir(4,4),gfil(4,4)

      COMMON/CHAMIX/ OIJL(2,2),OIJR(2,2),V(2,2),U(2,2),FM(2),ETA(2)

      COMMON/KINEM/flum,ecm,s,roots,T,Q,Q2,EN(2)

      i1=iabs(in1)-70
      i2=iabs(in2)-70

      GENPHO=0.
      DIJ=1.
      FMEL=FMAL(4)
      FMER=FMAR(4)
      IF(I1.NE.I2)DIJ=0.
      Q2=(S-(WAS(I1)+WAS(I2))**2)
      IF(Q2.LE.0)RETURN
      Q2=(S-(WAS(I1)+WAS(I2))**2)*(S-(WAS(I1)-WAS(I2))**2)/4.D0/S
      Q=DSQRT(Q2)
      EN(1)=DSQRT(Q2+WAS(I1)**2)
      EN(2)=DSQRT(Q2+WAS(I2)**2)
C
C calculation of t,u

      T=WAS(I1)**2-ROOTS*(EN(1)-Q*COSTHE)
      UMAN=WAS(I1)**2+WAS(I2)**2-S-T
C
C phase space
C
      SGU=(WAS(I1)**2-UMAN)*(WAS(I2)**2-UMAN)
      SGT=(WAS(I1)**2-T)*(WAS(I2)**2-T)
      SGS=ESA(I1)*ESA(I2)*was(I1)*was(I2)*S
C
C propagators
C
      DZ2=1.D0/((S-FMZ**2)**2+FMZ**2*GAMMAZ**2)
      REDZ=(S-FMZ**2)*DZ2

      DSELT1=(T-FMEL**2)
      DSELU1=(UMAN-FMEL**2)
      DSERT1=(T-FMER**2)
      DSERU1=(UMAN-FMER**2)

      DSELT=1.D0/DSELT1
      DSELU=1.D0/DSELU1
      DSERT=1.D0/DSERT1
      DSERU=1.D0/DSERU1
C
C couplings
C
      FLE=FLC(4)
      FRE=FRC(4)
      CLR=FLE**2+FRE**2

C
C z propagator
C
C
      CONZ=G2**2*DZ2/16.D0/PI/COSW**4/S**2
      SZ=CONZ*VOIJL(I1,I2)**2*CLR*0.389D+9*(SGT+SGU-2.D0*SGS)
C
C sel propagator
C
      CONSEL=G2**2/64.D0/PI/S**2
      A1=gFIL(I1,4)**2*gFIL(I2,4)**2*(DSELT**2*SGT+DSELU**2*SGU-
     +2.D0*DSELT*DSELU*SGS)
      A2=gFIR(I1,4)**2*gFIR(I2,4)**2*(DSERT**2*SGT+DSERU**2*SGU-
     +2.D0*DSERT*DSERU*SGS)
      SEL=CONSEL*0.389D+9*(A1+A2)
C
C z-sel interference
C
      CONSZ=G2**2/16.D0/PI/COSW**2/S**2*REDZ*VOIJL(I1,I2)
      A1=FLE*gFIL(I1,4)*gFIL(I2,4)*(DSELT*(SGT-SGS)+DSELU*(SGU-SGS))
      A2=FRE*gFIR(I1,4)*gFIR(I2,4)*(DSERT*(SGT-SGS)+DSERU*(SGU-SGS))
      SZSEL=CONSZ*(A1-A2)*0.389D+9
C
C total
C
      GENPHO=(SZ+SEL+SZSEL)/2.D0*(2.D0-DIJ)*ROOTS*Q

      IF(GENPHO.LT.0.)GENPHO=0.

      RETURN
      END
*CMZ :  1.00/00 14/04/95  18.46.26  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      DOUBLE PRECISION FUNCTION CHARGI(IN1,IN2)
C********************************************************************
C production cross section for charginos
C                                         author S.Katsanevas
C********************************************************************
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
     +FLC(12),FRC(12),gms(12),echar(12)

      COMMON/MSSM/TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI

      common/spartcl/fmal(12),fmar(12),ratqa(12),fgamc(12),fgamcr(12)
     +,cosmi(12)



      COMMON/NEUMIX/ ZR(4,4),was(4),ESA(4),
     +VOIJL(4,4),VOIJR(4,4),gfir(4,4),gfil(4,4)

      COMMON/CHAMIX/ OIJL(2,2),OIJR(2,2),V(2,2),U(2,2),FM(2),ETA(2)
      COMMON/KINEM/flum,ecm,s,roots,T,Q,Q2,EN(2)

      i1=iabs(in1)-74
      i2=iabs(in2)-74

      CHARGI=0.
      DIJ=1.

      FMNU=FMAL(3)

      IF(I1.NE.I2)DIJ=0.
      Q2=(S-(FM(I1)+FM(I2))**2)
      if(Q2.LE.0.)return
      Q2=(S-(FM(I1)+FM(I2))**2)*(S-(FM(I1)-FM(I2))**2)/4.D0/S
      Q=DSQRT(Q2)
      EN(1)=DSQRT(Q2+FM(I1)**2)
      EN(2)=DSQRT(Q2+FM(I2)**2)
C
C phase space
C
      AL=(2.D0*FMNU**2+S-FM(I1)**2-FM(I2)**2)/2.D0/FMNU**2
      BL=Q*ROOTS/FMNU**2
      ALBL=Al-BL
      IF(ALBL.EQ.0)ALBL=0.00001
      AR=DLOG(ABS((AL+BL)/ALBL))
      H=2.D0*Q*ROOTS-2.D0*Q2*AL/BL+
     +  (EN(1)*EN(2)+Q2*AL**2/BL**2-Q*ROOTS*AL/BL)*AR
      GAM=(EN(1)*EN(2)+Q2/3.D0+FM(I1)*FM(I2))
C
C propagators
C
      DZ2=1.D0/((S-FMZ**2)**2+FMZ**2*GAMMAZ**2)
      REDZ=(S-FMZ**2)*DZ2

C
C z propagator
C
C
      CONZ=G2**2*DZ2/8.D0/PI/COSW**4*Q/ROOTS
      FLE=FLC(4)
      FRE=FRC(4)
      CLR=FLE**2+FRE**2
      A1=(OIJL(I1,I2)**2+OIJR(I1,I2)**2)*CLR*(EN(1)*EN(2)+Q2/3.D0)
      A2=2.D0*OIJL(I1,I2)*OIJR(I1,I2)*CLR*ETA(I1)*ETA(I2)*FM(I1)*FM(I2)
      SZ=CONZ*(A1+A2)*0.389D+9
C
C gamma propagator
C
      SG=E2**2/2.D0/PI*Q*ROOTS/S**3*DIJ*GAM*0.389D+9
C
C snu propagator
C
      CONU=G2**2*V(I1,1)**2*V(I2,1)**2/32.D0/PI/FMNU**4*Q/ROOTS
      A1=(EN(1)*EN(2)+Q2-Q*ROOTS*AL/BL)/(AL**2-BL**2)+2.D0*Q2/BL**2
      A2=(Q*ROOTS-2.D0*Q2*AL/BL)*AR/2.D0/BL**2
      SNU=CONU*(A1+A2)*0.389D+9
C
C gamma-z interference
C
      SGZ=E2*G2/4.D0/PI/COSW**2*Q*ROOTS/S**2*REDZ*DIJ*(FLE+FRE)*GAM*
     +(OIJL(I1,I2)+OIJR(I1,I2))*0.389D+9
C
C gamma-snu interference
C
      COSG=-E2*G2*V(I1,1)**2/16.D0/PI*DIJ/S**2*(H+FM(I1)*FM(I2)*AR)
      SGSNU=COSG*0.389D+9
C
C z-snu interference
C
      COSZS=-G2**2*V(I1,1)*V(I2,1)/16.D0/PI/COSW**2*REDZ/S*FLE
      A1=OIJL(I1,I2)*H+OIJR(I1,I2)*ETA(I1)*ETA(I2)*FM(I1)*FM(I2)*AR
      SZSNU=COSZS*A1*0.389D+9
C
C total
C
C      write (6,*) ' gamma    chargino ',sg
C      write (6,*) ' z         chargino ',sz
C      write (6,*) ' snu       chargino ',snu
C      write (6,*) ' gamma-z   chargino ',sGZ
C      write (6,*) ' gamma-snu chargino ',sgsnu
C      write (6,*) ' z-snu      chargino ',szsnu

      CHARGI=(SG+SZ+SNU+SGZ+SGSNU+SZSNU)

      if(chargi.lt.0.)then
        write (6,10000) chargi
10000 FORMAT(1X,' ERROR in chargi cross section ',f8.2)
        chargi=0.
      endif

      RETURN
      END
*CMZ :  1.00/00 14/04/95  18.39.57  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      DOUBLE PRECISION FUNCTION GENCHAR(IL1,IL2,COSTHE)
C********************************************************************
C production cross section ds/dcostheta for charginos
C                                         author S.Katsanevas
C********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
     +FLC(12),FRC(12),gms(12),echar(12)

      COMMON/MSSM/TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI

      common/spartcl/fmal(12),fmar(12),ratqa(12),fgamc(12),fgamcr(12)
     +,cosmi(12)



      COMMON/NEUMIX/ ZR(4,4),was(4),ESA(4),
     +VOIJL(4,4),VOIJR(4,4),gfir(4,4),gfil(4,4)

      COMMON/CHAMIX/ OIJL(2,2),OIJR(2,2),V(2,2),U(2,2),FM(2),ETA(2)

      COMMON/KINEM/flum,ecm,s,roots,T,Q,Q2,EN(2)


      I1=iabs(IL1)-74
      I2=iabs(IL2)-74

      GENCHAR=0.
      DIJ=1.
      FMNU=FMAL(3)

      IF(I1.NE.I2)DIJ=0.
      Q2=(S-(FM(I1)+FM(I2))**2)
      if(Q2.LE.0.)return
      Q2=(S-(FM(I1)+FM(I2))**2)*(S-(FM(I1)-FM(I2))**2)/4.D0/S
      IF(Q2.LE.0.)RETURN
      Q=DSQRT(Q2)
      EN(1)=DSQRT(Q2+FM(I1)**2)
      EN(2)=DSQRT(Q2+FM(I2)**2)

C
C calculation of t,u

      T=FM(I1)**2-ROOTS*(EN(1)-Q*COSTHE)
      UMAN=FM(I1)**2+FM(I2)**2-S-T
C
C phase space
C
      SGU=(FM(I1)**2-UMAN)*(FM(I2)**2-UMAN)
      SGT=(FM(I1)**2-T)*(FM(I2)**2-T)
      SGS=ETA(I1)*ETA(I2)*FM(I1)*FM(I2)*S

C
C propagators
C
      DZ2=1.D0/((S-FMZ**2)**2+FMZ**2*GAMMAZ**2)
      DNU1=T-FMNU**2
      IF(DNU1.EQ.0)DNU1=0.00001D0
      DNU=1./DNU1
      REDZ=(S-FMZ**2)*DZ2

C
C couplings
C
      FLE=FLC(4)
      FRE=FRC(4)
      CLR =FLE**2+FRE**2
      CLR1=FLE**2-FRE**2

C
C
C gamma propagator
C


      SG=E2**2/8.D0/PI/S**4*DIJ*0.389D+9*(SGU+SGT+2.D0*SGS)

C
C
C z propagator
C
C
      CONZ=G2**2*DZ2/32.D0/PI/COSW**4/S**2
      A1=(OIJL(I1,I2)**2+OIJR(I1,I2)**2)*CLR*(SGU+SGT)
      A2=4.D0*OIJL(I1,I2)*OIJR(I1,I2)*CLR*SGS
      A3=-(OIJL(I1,I2)**2-OIJR(I1,I2)**2)*CLR1*(SGU-SGT)
      SZ=CONZ*(A1+A2+A3)*0.389D+9

C snu propagator
C
      SNU=G2**2*V(I1,1)**2*V(I2,1)**2*DNU**2/64.D0/PI/S**2*SGT
     +*0.389D+9
C
C gamma-z interference
C
      A1=(FLE+FRE)*(OIJL(I1,I2)+OIJR(I1,I2))*(SGU+SGT+2.D0*SGS)
      A2=-(FLE-FRE)*(OIJL(I1,I2)-OIJR(I1,I2))*(SGU-SGT)
      SGZ=E2*G2/16.D0/PI/COSW**2/S**3*REDZ*DIJ*0.389D+9*(A1+A2)
C
C gamma-snu interference
C
      SGSNU=E2*G2*DNU*V(I1,1)**2/16.D0/PI*DIJ/S**3*0.389D+9*
     +(SGT+SGS)
C
C z-snu interference
C
      COZNU=G2**2*V(I1,1)*V(I2,1)/16.D0/PI/COSW**2*REDZ*DNU/S**2
      A1=OIJL(I1,I2)*SGT+OIJR(I1,I2)*SGS
      SZSNU=COZNU*A1*0.389D+9*FLE

C
C total
C
      GENCHAR=(SG+SZ+SNU+SGZ+SGSNU+SZSNU)*ROOTS*Q

      if(GENCHAR.lt.0.)GENCHAR=0.

      RETURN
      END
*CMZ :  1.00/00 14/04/95  18.46.26  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      DOUBLE PRECISION FUNCTION GENSEL(COSTHE)
C********************************************************************
C production cross section for selectrons
C                                         author S.Katsanevas
C********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
     +FLC(12),FRC(12),gms(12),echar(12)

      COMMON/MSSM/TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI

      common/spartcl/fmal(12),fmar(12),ratqa(12),fgamc(12),fgamcr(12)
     +,cosmi(12)


      COMMON/NEUMIX/ ZR(4,4),was(4),ESA(4),
     +VOIJL(4,4),VOIJR(4,4),gfir(4,4),gfil(4,4)

      COMMON/CHAMIX/ OIJL(2,2),OIJR(2,2),V(2,2),U(2,2),FM(2),ETA(2)

      COMMON/KINEM/flum,ecm,s,roots,T,Q,Q2,EN(2)

      DIMENSION DK(4),dk1(4)

      common/mixings/cosphi,facqcd,fgam,selm,ratq
C
C cosphimix = 1  left
C cosphimix = 0  right
C

      GENSEL=0.
      FLE=FLC(4)
      FRE=FRC(4)

      Q2=(S-4.D0*SELM**2)/4.D0
      IF(Q2.LE.0.)RETURN
      Q=DSQRT(Q2)
      EN(1)=DSQRT(Q2+SELM**2)
      EN(2)=DSQRT(Q2+SELM**2)

      sinphi=dsqrt(1.d0-cosphi**2)
C
C calculation of t,u

      T=SELM**2-ROOTS*(EN(1)-Q*COSTHE)
      UMAN=2.D0*SELM**2-S-T
C
C phase space
C
      SGUT=(UMAN*T-SELM**4)
C
C propagators
C
      DZ2=1.D0/((S-FMZ**2)**2+FMZ**2*GAMMAZ**2)
      REDZ=(S-FMZ**2)*DZ2


      DSUMR=0.
      DSUML=0.
      DSUMLR=0.
      DMONSTR=0.
      DSUMLRA=0.
      DO 20 K=1,4

        DK1(K)=(T-WAS(K)**2)

        if(dk1(k).eq.0.)THEN
          dk1(k)=0.001
        ENDIF

        DK(K)=1.D0/dk1(k)
        DSUMR=DSUMR+DK(K)*gFIR(k,4)**2
        DSUML=DSUML+DK(K)*gFIL(k,4)**2
        DSUMLR=DSUMLR+
     +    DK(K)*(COSPHI**2*gFIL(k,4)**2+SINPHI**2*gFIR(k,4)**2)
        DSUMLRA=DSUMLRA+DK(K)*
     +  (FLE*COSPHI**2*gFIL(k,4)**2+FRE*SINPHI**2*gFIR(k,4)**2)

        DO 10 L=1,4
          DMONSTR=DMONSTR+Was(K)*Was(L)*esa(k)*esa(l)
     + *DK(K)*DK(L)*gFIR(k,4)*gFIL(k,4)*gFIR(l,4)*gFIL(l,4)
   10   CONTINUE

   20 CONTINUE


C
C couplings
C

      CLR =FLE**2+FRE**2

C
C
C gamma propagator
C
C
      SG=E2**2/8.D0/PI/S**4*SGUT*0.389D+9
C
C z propagator
C
      SZ=G2**2/16.D0/PI/S**2/COSW**4*SGUT*DZ2*CLR*FGAM**2*0.389D+9
C
C gamma-z interference
C
      SGZ=E2*G2/8.D0/PI/S**3/COSW**2*SGUT*FGAM*(FLE+FRE)*
     +REDZ*0.389D+9
C
C neutralino propagator
C
      SCHI=G2**2/64.D0/PI/S**2*
     +(SGUT*(COSPHI**4*DSUML**2+SINPHI**4*DSUMR**2)
     ++2.D0*SINPHI**2*COSPHI**2*S*DMONSTR)*0.389D+9
C
C gamma-neutralino
C
      SGCHI=E2*G2/16.D0/PI/S**3*SGUT*DSUMLR*0.389D+9
C
C  z - neutralino
C
      SZCHI=G2**2/16.D0/PI/S**2/COSW**2*SGUT*FGAM*REDZ*DSUMLRA*
     +0.389D+9
C
C total
C
      SS=SZ+SG+SGZ

C      write (6,*) ' s channel    selec     ',ss
C      write (6,*) ' chargino     selec     ',schi
C      write (6,*) ' charg-gam    selec     ',sgchi
C      write (6,*) ' charg-z      selec     ',szchi

      GENSEL=(SG+SZ+SCHI+SGZ+SGCHI+SZCHI)*ROOTS*Q
      iF(GENSEL.LT.0.)GENSEL=0.

      RETURN
      END
*CMZ :  1.00/00 14/04/95  18.46.26  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      DOUBLE PRECISION FUNCTION GENSELR(COSTHE)

C********************************************************************
C production cross section for selectrons (left +right)
C                                         author S.Katsanevas
C********************************************************************

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
     +FLC(12),FRC(12),gms(12),echar(12)

      COMMON/MSSM/TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI

      common/spartcl/fmal(12),fmar(12),ratqa(12),fgamc(12),fgamcr(12)
     +,cosmi(12)


      COMMON/NEUMIX/ ZR(4,4),was(4),ESA(4),
     +VOIJL(4,4),VOIJR(4,4),gfir(4,4),gfil(4,4)

      COMMON/CHAMIX/ OIJL(2,2),OIJR(2,2),V(2,2),U(2,2),FM(2),ETA(2)

      COMMON/KINEM/flum,ecm,s,roots,T,Q,Q2,EN(2)

      DIMENSION DK(4),dk1(4)

      common/mixings/cosphi,facqcd,fgam,selm,ratq
C
C cosphimix = 1  left
C cosphimix = 0  right
C

      GENSELR=0.
      XPCM   = XLAMDA (s,fmal(4)**2,fmar(4)**2)/ (2. * roots)
      IF(XPCM.LE.0.)RETURN

      Q=XPCM

      EN(1)=DSQRT(XPCM**2+fmal(4)**2)
      EN(2)=DSQRT(XPCM**2+fmar(4)**2)

C
C calculation of t
C
      T=fmal(4)**2-ROOTS*(EN(1)-xpcm*COSTHE)
      DMONSTR1=0.
      DO 20 K=1,4
        DK1(K)=(T-WAS(K)**2)
        if(dk1(k).eq.0.)THEN
          dk1(k)=0.001
        ENDIF
        DK(K)=1.D0/dk1(k)
        DO 10 L=1,4
          DMONSTR1=DMONSTR1+Was(K)*Was(L)*esa(k)*esa(l)
     + *DK(K)*DK(L)*gFIR(k,4)*gFIL(k,4)*gFIR(l,4)*gFIL(l,4)
   10   CONTINUE
   20 CONTINUE

      T=fmar(4)**2-ROOTS*(EN(2)-xpcm*COSTHE)
      DMONSTR2=0.
      DO 40 K=1,4
        DK1(K)=(T-WAS(K)**2)
        if(dk1(k).eq.0.)THEN
          dk1(k)=0.001
        ENDIF
        DK(K)=1.D0/dk1(k)
        DO 30 L=1,4
          DMONSTR2=DMONSTR2+Was(K)*Was(L)*esa(k)*esa(l)
     + *DK(K)*DK(L)*gFIR(k,4)*gFIL(k,4)*gFIR(l,4)*gFIL(l,4)
   30   CONTINUE
   40 CONTINUE

      dmonstr=dmonstr1+dmonstr2

      genselr=G2**2/64.D0/PI/S**2*S*DMONSTR*0.389D+9
      GENSELR=genselr*ROOTS*Q
      iF(GENSELR.LT.0.)GENSELR=0.

      RETURN
      END
*CMZ :  1.00/00 14/04/95  18.46.26  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      DOUBLE PRECISION FUNCTION GENSELRS(dummy)

C********************************************************************
C production cross section for selectrons (left +right)
C                                         author S.Katsanevas
C********************************************************************

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
     +FLC(12),FRC(12),gms(12),echar(12)

      COMMON/MSSM/TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI

      common/spartcl/fmal(12),fmar(12),ratqa(12),fgamc(12),fgamcr(12)
     +,cosmi(12)


      COMMON/NEUMIX/ ZR(4,4),was(4),ESA(4),
     +VOIJL(4,4),VOIJR(4,4),gfir(4,4),gfil(4,4)

      COMMON/CHAMIX/ OIJL(2,2),OIJR(2,2),V(2,2),U(2,2),FM(2),ETA(2)

      COMMON/KINEM/flum,ecm,s,roots,T,Q,Q2,EN(2)

      DIMENSION DK(4),dk1(4)

      common/mixings/cosphi,facqcd,fgam,selm,ratq
C
C cosphimix = 1  left
C cosphimix = 0  right
C

      GENSELRS=0.
      XPCM   = XLAMDA (s,fmal(4)**2,fmar(4)**2)/ (2. * roots)
      IF(XPCM.LE.0.)RETURN

      EN(1)=DSQRT(XPCM**2+fmal(4)**2)
      EN(2)=DSQRT(XPCM**2+fmar(4)**2)

C
C calculation of t
C

      TMIN1=fmal(4)**2-ROOTS*(EN(1)+xpcm)
      TMIN2=fmar(4)**2-ROOTS*(EN(2)+xpcm)

      TMax1=fmal(4)**2-ROOTS*(EN(1)-xpcm)
      TMax2=fmar(4)**2-ROOTS*(EN(2)-xpcm)

      DMONSTR1=0.
      DO 20 K=1,4
        dmonstr1=dmonstr1+Was(K)**2*gFIR(k,4)**2*gFIL(k,4)**2 *(tmax1-
     +  tmin1)/(tmin1-was(k)**2)/(tmax1-was(k)**2)
        DO 10 L=1,4
          if(k.eq.l)go to 10
          dm=was(k)**2-was(l)**2
          if(dm.eq.0.)go to 10
          dmonstr1=dmonstr1+Was(K)*Was(L)*esa(k)*esa(l) *gFIR(k,4)*
     +    gFIL(k,4)*gFIR(l,4)*gFIL(l,4) *dlog( (tmax1-was(k)**2)*
     +    (tmin1-was(l)**2) /(tmin1-was(k)**2)/(tmax1-was(l)**2) )/dm
   10   CONTINUE
   20 CONTINUE

      DMONSTR2=0.
      DO 40 K=1,4
        dmonstr2=dmonstr2+Was(K)**2*gFIR(k,4)**2*gFIL(k,4)**2 *(tmax2-
     +  tmin2)/(tmin2-was(k)**2)/(tmax2-was(k)**2)
        DO 30 L=1,4
          if(k.eq.l)go to 30
          dm=was(k)**2-was(l)**2
          if(dm.eq.0.)go to 30
          dmonstr2=dmonstr2+Was(K)*Was(L)*esa(k)*esa(l) *gFIR(k,4)*
     +    gFIL(k,4)*gFIR(l,4)*gFIL(l,4) *dlog( (tmax2-was(k)**2)*
     +    (tmin2-was(l)**2) /(tmin2-was(k)**2)/(tmax2-was(l)**2) )/dm
   30   CONTINUE
   40 CONTINUE

      dmonstr=dmonstr1+dmonstr2

      genselrs=G2**2/64.D0/PI/S**2*S*DMONSTR*0.389D+9
      iF(GENSELRs.LT.0.)GENSELRs=0.

      RETURN
      END
*CMZ :  1.00/00 14/04/95  18.44.39  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      DOUBLE PRECISION FUNCTION GENSMU(COSTHE)
C********************************************************************
C production cross section for smuons
C                                         author S.Katsanevas
C********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
     +FLC(12),FRC(12),gms(12),echar(12)

      COMMON/MSSM/TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI

      common/spartcl/fmal(12),fmar(12),ratqa(12),fgamc(12),fgamcr(12)
     +,cosmi(12)



      COMMON/NEUMIX/ ZR(4,4),was(4),ESA(4),
     +VOIJL(4,4),VOIJR(4,4),gfir(4,4),gfil(4,4)

      COMMON/CHAMIX/ OIJL(2,2),OIJR(2,2),V(2,2),U(2,2),FM(2),ETA(2)

      COMMON/KINEM/flum,ecm,s,roots,T,Q,Q2,EN(2)


      common/mixings/cosphi,facqcd,fgam,selm,ratq

      xlam=.270d0
      alphas = alfas(s,xlam,5)
      alphaem= alfaem(s)

      E2=4.D0*PI*ALPHAEM
      G2=E2/SINW**2


      GENSMU=0.


      BETA = 1.d0 - (4.d0*selm**2 /s)
      IF( BETA .LE. 0. ) THEN
        RETURN
      ELSE
        BETA = DSQRT( BETA )
      ENDIF


      Q2=(S-4.D0*SELM**2)/4.D0
      IF(Q2.LE.0.)RETURN
      Q=DSQRT(Q2)

      EN(1)=DSQRT(Q2+SELM**2)
      EN(2)=DSQRT(Q2+SELM**2)

C calculation of t,u

      T=SELM**2-ROOTS*(EN(1)-Q*COSTHE)

      UMAN=2.D0*SELM**2-S-T

C
C phase space
C
      SGUT=(UMAN*T-SELM**4)
C
C propagators
C
      DZ2=1.D0/((S-FMZ**2)**2+FMZ**2*GAMMAZ**2)
      REDZ=(S-FMZ**2)*DZ2
C
C couplings
C
      FLE=FLC(4)
      FRE=FRC(4)
      CLR =FLE**2+FRE**2

C
C gamma propagator
C
      SG=ratq**2*E2**2/8.D0/PI/S**4*SGUT*0.389D+9
C
C z propagator
C
      SZ=G2**2/16.D0/PI/S**2/COSW**4*SGUT*DZ2*CLR*
     +FGAM**2*0.389D+9
C
C
C gamma-z interference
C

      SGZ=ratq*E2*G2/8.D0/PI/S**3/COSW**2*SGUT*FGAM*(FLE+FRE)*
     +REDZ*0.389D+9
C
C total
C
C      write (6,*) ' z      smuon     ',sz
C      write (6,*) ' gam    smuon     ',sg
C      write (6,*) ' z-gam  smuon     ',sGZ

      if(facqcd.eq.1.d0)then
        F = PI**2 / (2.d0*BETA) - (1.d0+BETA )*(PI**2/2.-3.d0)/2.d0
        FQCD = 1.d0 + (4.d0*ALPHAS/(3.d0*PI))*F
        phase = 3.d0 * FQCD
        GENSMU=(SG+SZ+SGZ)*phase
      else
        GENSMU=SG+SZ+SGZ
      endif

      GENSMU=(SG+SZ+SGZ)*ROOTS*Q*2.

      iF(GENSMU.LT.0.)GENSMU=0.

      RETURN
      END
*CMZ :  1.00/00 14/04/95  18.44.39  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      DOUBLE PRECISION FUNCTION GENSMUS(dummy)
C********************************************************************
C production cross section for smuons
C                                         author S.Katsanevas
C********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
     +FLC(12),FRC(12),gms(12),echar(12)

      COMMON/MSSM/TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI

      common/spartcl/fmal(12),fmar(12),ratqa(12),fgamc(12),fgamcr(12)
     +,cosmi(12)



      COMMON/NEUMIX/ ZR(4,4),was(4),ESA(4),
     +VOIJL(4,4),VOIJR(4,4),gfir(4,4),gfil(4,4)

      COMMON/CHAMIX/ OIJL(2,2),OIJR(2,2),V(2,2),U(2,2),FM(2),ETA(2)

      COMMON/KINEM/flum,ecm,s,roots,T,Q,Q2,EN(2)


      common/mixings/cosphi,facqcd,fgam,selm,ratq


      dummy=1.

      xlam=.270d0
      alphas = alfas(s,xlam,5)
      alphaem= alfaem(s)

      E2=4.D0*PI*ALPHAEM
      G2=E2/SINW**2

      GENSMUS=0.

      BETA = 1.d0 - (4.d0*selm**2 /s)
      IF( BETA .LE. 0. ) THEN
        RETURN
      ELSE
        BETA = DSQRT( BETA )
      ENDIF

      SGUT=BETA**3*S**3/6.D0

C
C propagators
C
      DZ2=1.D0/((S-FMZ**2)**2+FMZ**2*GAMMAZ**2)
      REDZ=(S-FMZ**2)*DZ2
C
C couplings
C
      FLE=FLC(4)
      FRE=FRC(4)
      CLR =FLE**2+FRE**2

      SCON=E2**2/8.D0/PI/S**4*SGUT*0.389D+9
C
C gamma propagator
C
      SG=SCON*ratq**2
C
C z propagator
C
      SZ=SCON*S**2*DZ2*CLR*FGAM**2/2.D0/SINW**4/COSW**4
C
C
C gamma-z interference
C

      SGZ=SCON*ratq*S*REDZ*FGAM*(FLE+FRE)/SINW**2/COSW**2

C
C total
C
C      write (6,*) ' z      smuon     ',sz
C      write (6,*) ' gam    smuon     ',sg
C      write (6,*) ' z-gam  smuon     ',sGZ

      if(facqcd.eq.1.d0)then
        F = PI**2 / (2.d0*BETA) - (1.d0+BETA )*(PI**2/2.-3.d0)/2.d0
        FQCD = 1.d0 + (4.d0*ALPHAS/(3.d0*PI))*F
        phase = 3.d0 * FQCD
        GENSMUS=(SG+SZ+SGZ)*phase
      else
        GENSMUS=SG+SZ+SGZ
      endif

      iF(GENSMUS.LT.0.)GENSMUS=0.

      RETURN
      END
*CMZ :  1.00/00 14/04/95  18.39.58  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      function alfas(q2,xlam,inf)
      implicit real*8 (a-h,o-z)
      data olam/0.d0/,pi/3.14159d0/
      data xmb/5.d0/,xmc/1.5d0/
      if(xlam.ne.olam) then
        olam = xlam
        b5  = (33-2*5)/pi/12
        bp5 = (153 - 19*5) / pi / 2 / (33 - 2*5)
        b4  = (33-2*4)/pi/12
        bp4 = (153 - 19*4) / pi / 2 / (33 - 2*4)
        b3  = (33-2*3)/pi/12
        bp3 = (153 - 19*3) / pi / 2 / (33 - 2*3)
        xlc = 2 * log(xmc/xlam)
        xlb = 2 * log(xmb/xlam)
        xllc = log(xlc)
        xllb = log(xlb)
        c45  =  1/( 1/(b5 * xlb) - xllb*bp5/(b5 * xlb)**2 )
     #        - 1/( 1/(b4 * xlb) - xllb*bp4/(b4 * xlb)**2 )
        c35  =  1/( 1/(b4 * xlc) - xllc*bp4/(b4 * xlc)**2 )
     #        - 1/( 1/(b3 * xlc) - xllc*bp3/(b3 * xlc)**2 ) + c45
      endif
      q   = sqrt(q2)
      xlq = 2 * log( q/xlam )
      xllq = log( xlq )
      nf = inf
      if( nf .lt. 0) then
        if( q .gt. xmb ) then
          nf = 5
        elseif( q .gt. xmc ) then
          nf = 4
        else
          nf = 3
        endif
      endif
      if    ( nf .eq. 5 ) then
        alfas = 1/(b5 * xlq) -  bp5/(b5 * xlq)**2 * xllq
      elseif( nf .eq. 4 ) then
        alfas = 1/( 1/(1/(b4 * xlq) - bp4/(b4 * xlq)**2 * xllq) + c45 )
      elseif( nf .eq. 3 ) then
        alfas = 1/( 1/(1/(b3 * xlq) - bp3/(b3 * xlq)**2 * xllq) + c35 )
      else
        print *,'error in alfa: unimplemented # of light flavours',nf
        stop
      endif
      return
      end
*CMZ :  1.00/00 14/04/95  18.39.58  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      function alfaem(q2)
      implicit real*8 (a-z)
      data zm/91.19d0/,ooaz/128.87d0/,pi/3.14159d0/
      nc=3
      xlq = log(q2/zm**2)
      b = 3 + 3*nc*(1d0/3d0)**2 + 2*nc*(2d0/3d0)**2
      ooa = ooaz - 2d0/3d0/pi * b * xlq
      alfaem = 1/ooa
      end
*CMZ :  1.00/00 14/04/95  18.46.26  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      DOUBLE PRECISION FUNCTION GENSNUE(COSTHE)
C********************************************************************
C
C production cross section for snues
C
C                                         author S.Katsanevas
C********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
     +FLC(12),FRC(12),gms(12),echar(12)

      COMMON/MSSM/TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI

      common/spartcl/fmal(12),fmar(12),ratqa(12),fgamc(12),fgamcr(12)
     +,cosmi(12)


      COMMON/NEUMIX/ ZR(4,4),was(4),ESA(4),
     +VOIJL(4,4),VOIJR(4,4),gfir(4,4),gfil(4,4)

      COMMON/CHAMIX/ OIJL(2,2),OIJR(2,2),V(2,2),U(2,2),FM(2),ETA(2)
      COMMON/KINEM/flum,ecm,s,roots,T,Q,Q2,EN(2)
      DIMENSION DK(2),dk1(2)

C
      GENSNUE=0

      FMNU=FMAL(3)

      Q2=(S-4.D0*FMNU**2)/4.D0

      IF(Q2.LE.0.)RETURN
      Q=DSQRT(Q2)
      EN(1)=DSQRT(Q2+FMNU**2)
      EN(2)=DSQRT(Q2+FMNU**2)

C
C calculation of t,u

      T=FMNU**2-ROOTS*(EN(1)-Q*COSTHE)

      UMAN=2.D0*FMNU**2-S-T
C
C phase space
C
      SGUT=(UMAN*T-FMNU**4)
C
C propagators
C
      DZ2=1.D0/((S-FMZ**2)**2+FMZ**2*GAMMAZ**2)
      REDZ=(S-FMZ**2)*DZ2
      DSUM=0.
      DO 10 K=1,2
        DK1(K)=(T-FM(K)**2)
        if(dk1(k).eq.0.)dk1(k)=0.001
        DK(K)=1.D0/dk1(k)
        DSUM=DSUM+DK(K)*V(K,1)**2
   10 CONTINUE

C
C z propagator
C
      FLE=FLC(4)
      FRE=FRC(4)
      CLR =FLE**2+FRE**2

      CON=G2**2/64.D0/PI/S**2*SGUT*0.389D+9

      SZ=CON*DZ2*CLR/COSW**4
C
C chargino propagator
C
      SCHI=CON*DSUM**2
C
C chargino-z interference
C
      SCHZ=CON*REDZ*DSUM*2.d0*FLE/COSW**2
C
C total
C

C      write (6,*) ' z           snu ',sz
C      write (6,*) ' chargino    snu ',schi
C      write (6,*) ' z-chargino  snu ',schz

      GENSNUE=(SZ+SCHI+SCHZ)*ROOTS*Q

      iF(GENSNUE.LT.0.)GENSNUE=0.

      RETURN
      END
*CMZ :  1.00/00 14/04/95  18.39.58  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      DOUBLE PRECISION FUNCTION GENSNU(COSTHE)
C********************************************************************
C
C production cross section for snus
C
C                                         author S.Katsanevas
C********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
     +FLC(12),FRC(12),gms(12),echar(12)

      COMMON/MSSM/TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI
      common/spartcl/fmal(12),fmar(12),ratqa(12),fgamc(12),fgamcr(12)
     +,cosmi(12)


      COMMON/NEUMIX/ ZR(4,4),was(4),ESA(4),
     +VOIJL(4,4),VOIJR(4,4),gfir(4,4),gfil(4,4)
      COMMON/CHAMIX/ OIJL(2,2),OIJR(2,2),V(2,2),U(2,2),FM(2),ETA(2)
      COMMON/KINEM/flum,ecm,s,roots,T,Q,Q2,EN(2)


C
C attention force mass of nu
C

      GENSNU=0.

      FMNU=FMAL(3)

      Q2=(S-4.D0*FMNU**2)/4.D0
      IF(Q2.LE.0.)RETURN
      Q=DSQRT(Q2)
      EN(1)=DSQRT(Q2+FMNU**2)
      EN(2)=DSQRT(Q2+FMNU**2)

C
C calculation of t,u

      T=FMNU**2-ROOTS*(EN(1)-Q*COSTHE)
      UMAN=2.D0*FMNU**2-S-T
C
C phase space
C
      SGUT=(UMAN*T-FMNU**4)
C
C propagators
C
      DZ2=1.D0/((S-FMZ**2)**2+FMZ**2*GAMMAZ**2)
      REDZ=(S-FMZ**2)*DZ2

C
C z propagator
C
      FLE=FLC(4)
      FRE=FRC(4)
      CLR =FLE**2+FRE**2

      CON=G2**2/64.D0/PI/S**2*SGUT*0.389D+9

      SZ=CON*DZ2*CLR/COSW**4

      GENSNU=SZ*ROOTS*Q
      iF(GENSNU.LT.0.)GENSNU=0.

      RETURN
      END
*CMZ :  1.00/00 14/04/95  18.46.26  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
CMW      SUBROUTINE suseve(ifail)
CMW
CMWC----------------------------------------------------------------------
CMWC-
CMWC-   GENERATE AN EVENT
CMWC-
CMW
CMW      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
CMW      COMMON/INDEXX/index,index1,index2,nevt
CMW      COMMON/FINDEX/fmpr1,fmpr2,XCROST,APRO
CMW
CMW
CMW      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
CMW     +FLC(12),FRC(12),gms(12),echar(12)
CMW
CMW
CMW      COMMON/KINEM/flum,ecm,s,roots,T,Q,Q2,EN(2)
CMW
CMW      COMMON/XCROS/xgaug(8),xeta(8)
CMW
CMW      REAL*4 RNDM
CMW
CMW
CMW      integer*4 lindex(3)
CMW
CMW      COMMON /PARTC / pv(5,30)
CMW      COMMON /PARTC1 / iflav(30,2),nflav
CMW
CMW      COMMON/ISR/ QK(4)
CMW
CMW      CHARACTER*5 LBLIN,SSID
CMW
CMW      COMMON /CONST/ idbg,igener,irad
CMW
CMW      real as,ampl,hrndm1
CMW      external ampl
CMW
CMW        logical rpar
CMW        common/rpari/rpar
CMW
CMW      ifail=1
CMW
CMW      ntry=0
CMW
CMW   10 continue
CMW
CMWC...Generate event with radiative photon.
CMW      IF(IRAD.EQ.1) THEN
CMW        CALL REMT2(QK)
CMW        E=ecm/2.D0
CMW        s=ecm**2*(1.-QK(4)/E)
CMW        ROOTS=DSQRT(S)
CMW      ELSE
CMWC...Generate event without radiative photon.
CMW        s=ECM**2
CMW        ROOTS=ECM
CMW        QK(1) = 0.0000D0
CMW        QK(2) = 0.0000D0
CMW        QK(3) = 0.0001D0
CMW        QK(4) = 0.0001D0
CMW      ENDIF
CMW
CMW      IFLAG=0
CMW      nflav=0
CMW
CMW      XPCM   = XLAMDA (roots**2,fmpr1**2,fmpr2**2)/ (2. * roots)
CMW
CMW      ntry=ntry+1
CMW      if(ntry.gt.10000)then
CMW        write(1,*) ' more than 10000 tries in REMT2 loop '
CMW        return
CMW      endif
CMW      if(xpcm.eq.0..and.irad.eq.1)go to 10
CMWc
CMWc generate ds/dcostheta
CMWc
CMW      call hbfun1(9999,' ',50,-1.,1.,ampl)
CMW      cthcm=hrndm1(9999)
CMW      call hdelet(9999)
CMW
CMW      PHCM  = TWOPI * dble(RNDM(0))
CMW      STHCM = DSQRT( 1. - CTHCM**2 )
CMW      CPHCM = DCOS( PHCM )
CMW      SPHCM = DSIN( PHCM )
CMW
CMW      CALL sdecay2 ( XPCM,CTHCM,PHCM,fmpr1,PV(1,1))
CMW
CMW      lblin=ssid(index1)
CMW
CMW      IFLAV(1,1)=INDEX1
CMW      IFLAV(1,2)=0
CMW
CMW      nflav=nflav+1
CMWC
CMWC     Here the three body decay of the FIRST product is generated.
CMWC
CMW
CMW      NC1=1
CMW
CMW      LINDEX1=INDEX1
CMW
CMW
CMW   20 continue
CMW
CMW      call decabr(lindex1,nbod,lindex,adeca)
CMW
CMW      if(nbod.ne.1)then
CMW
CMW        if(nbod.eq.3)then
CMW
CMW          CALL smbod3(PV(1,nc1),lindex1,PV(1,nc1+1),PV(1,nc1+2),PV(1,
CMW     +    nc1+3),lindex,adeca,kfail)
CMW          if(kfail.eq.1)return
CMW        endif
CMW
CMW        if(nbod.eq.2)then
CMW          CALL smbod2(PV(1,nc1),lindex1,PV(1,nc1+1),PV(1,nc1+2),
CMW     +    lindex,kfail)
CMW          if(kfail.eq.1)return
CMW        endif
CMW        nflav=nflav+nbod
CMW        do 30 k=1,nbod
CMW          lblin=ssid(LINDEX(K))
CMW          IFLAV(nc1+k,1)=lindex(k)
CMW          IFLAV(nc1+k,2)=nc1
CMW   30   continue
CMW        nc1=nc1+nbod
CMW        lindex1=lindex(nbod)
CMWc
CMWc cascade the decays
CMWc
CMW        go to 20
CMW
CMW      endif
CMW
CMW      nc1=nc1+1
CMW      nflav=nflav+1
CMW      CALL sdecay2 (-XPCM,CTHCM,PHCM,fmpr2,PV(1,nc1))
CMW
CMW      lblin=ssid(index2)
CMW
CMW      IFLAV(nc1,1)=INDEX2
CMW      IFLAV(nc1,2)=0
CMW
CMW      LINDEX2=INDEX2
CMW
CMW   40 continue
CMW
CMW      call decabr(lindex2,nbod,lindex,adeca)
CMW
CMW
CMW      if(nbod.ne.1)then
CMW
CMW
CMW        if(nbod.eq.3)then
CMW          CALL smbod3(PV(1,nc1),lindex2,PV(1,nc1+1),PV(1,nc1+2),PV(1,
CMW     +    nc1+3),lindex,adeca,kfail)
CMW          if(kfail.eq.1)return
CMW        endif
CMW
CMW        if(nbod.eq.2)then
CMW          CALL smbod2(PV(1,nc1),lindex2,PV(1,nc1+1),PV(1,nc1+2),
CMW     +    lindex,kfail)
CMW          if(kfail.eq.1)return
CMW        endif
CMW
CMW        nflav=nflav+nbod
CMW        do 50 k=1,nbod
CMW          lblin=ssid(LINDEX(K))
CMW          IFLAV(nc1+k,1)=lindex(k)
CMW          IFLAV(nc1+k,2)=nc1
CMW   50   continue
CMW
CMW        nc1=nc1+nbod
CMW        lindex2=lindex(nbod)
CMWc
CMWc cascade the decays
CMWc
CMW        go to 40
CMW
CMW      endif
CMW
CMW      IF(IRAD.EQ.1.and.qk(4).gt.0.001)THEN
CMW        do 60  lp=1,3
CMW   60   qk(lp)=-qk(lp)
CMW
CMW        do 70  k=1,nflav
CMW          CALL REMT3(QK,PV(1,k),PV(1,k))
CMW   70   continue
CMW
CMW        nflav=nflav+1
CMW
CMW        do 80 k=1,4
CMW   80   pv(k,nflav)=qk(k)
CMW
CMW        pv(5,nflav)=0.
CMW
CMW        iflav(nflav,1)=22
CMW        iflav(nflav,2)=0
CMW
CMW      ENDIF
CMW
CMW      if(.not.rpar)then
CMW      do 99 k=1,nflav
CMW      if(iabs(iflav(k,1)).ne.71)go to 99
CMW      nc1=nflav
CMW      call lspdecay(PV(1,k),
CMW     +Pv(1,nc1+1),PV(1,nc1+2),PV(1,nc1+3),IFLAV(nc1+1,1),ifail)
CMW      if(ifail.eq.1)go to 99
CMW       do 98 l=1,3
CMW          IFLAV(nc1+l,2)=k
CMW   98 continue
CMW      nflav=nflav+3
CMW  99  continue    
CMW      endif
CMW      
CMW
CMW      call sfragment(ifail)
CMW
CMW
CMW      END
*CMZ :  1.00/00 14/04/95  18.44.39  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      real function ampl(cthcm)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL CTHCM
      COMMON/INDEXX/index,index1,index2,nevt
      DCTHCM=CTHCM
      if(index.eq.1)AMP   = GENPHO(index1,index2,DCTHCM)
      if(index.eq.2)AMP   = GENCHAR(index1,index2,DCTHCM)
      if(index.eq.3)then
        IF(INDEX1.EQ.51.or.index1.eq.57)then
          if(index1.eq.-index2)AMP = GENSEL(DCTHCM)
          if(index1.ne.-index2)AMP = GENSELR(DCTHCM)
        elseif(index1.eq.52)then
          AMP = GENSNUE(DCTHCM)
        elseif(index1.eq.54.or.index1.eq.56)then
          AMP = GENSNU(DCTHCM)
        else
          AMP = GENSMU(DCTHCM)
        endif
      endif
      ampl=amp
      return
      end
*CMZ :  1.00/00 14/04/95  18.39.58  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      function radfun(s)
      common/srada/erad(100),srad(100)
      radfun=divdif(srad,erad,100,s,1)
      if(radfun.lt.0.)radfun=0.
      return
      end
CYG*CMZ :  1.00/00 14/04/95  18.46.27  by  Stavros Katsanevas
CYG*-- Author :    Stavros Katsanevas   14/04/95
CYG      SUBROUTINE DECABR(lindex1,nbod,lindex2,adeca)
CYGC
CYGC randomly generates a gaugino or spaticle decay to a nbod=1,2,3 mode
CYGC nbod=1 means no decay
CYGC lindex2 contains the decay particles list
CYGC
CYG
CYGC          MXSS                 = maximum number of modes
CYGC          NSSMOD               = number of modes
CYGC          ISSMOD               = initial particle
CYGC          JSSMOD               = final particles
CYGC          GSSMOD               = width
CYGC          BSSMOD               = branching ratio
CYG      INTEGER MXSS
CYG      PARAMETER (MXSS=2000)
CYG
CYG      COMMON/SSMODE/NSSMOD,ISSMOD(MXSS),JSSMOD(5,MXSS)
CYG      COMMON/SSMOD1/GSSMOD(MXSS),BSSMOD(MXSS)
CYG      INTEGER NSSMOD,ISSMOD,JSSMOD
CYG      DOUBLE PRECISION GSSMOD,BSSMOD
CYG      integer*4 kindex(3),lindex2(3)
CYG      real fm(3)
CYG
CYG      DOUBLE PRECISION brtot,adeca
CYG      common/ubra/ndeca(-80:80)
CYG      common/ubra1/brtot(2,100,-80:80)
CYG
CYG      call vzero(lindex2,3)
CYG      kin=0
CYG
CYG      mindex1=iabs(lindex1)
CYG
CYG      if(mindex1.lt.1.or.mindex1.gt.80)then
CYG        write(1,*)' error in lindex1 in decabr ',mindex1
CYG        stop99
CYG      endif
CYG
CYG      nbod=1
CYG      nm=ndeca(mindex1)
CYG      lindex2(1)=lindex1
CYG      if(nm.lt.1)return
CYG
CYG      CHAN=RNDM(0)*brtot(2,nm,mindex1)
CYG
CYG      do 10 j=1,nm
CYG        if(chan.lt.brtot(2,j,mindex1))go to 20
CYG   10 continue
CYG   20 continue
CYG
CYG      index=brtot(1,j,mindex1)
CYG
CYG      if(issmod(index).ne.mindex1)
CYG     +write(1,*)' error lindex1/issmod in decabr ',issmod(index),mindex1
CYG
CYG
CYG      adeca=brtot(2,j,mindex1)
CYG
CYG      nbod=3
CYG      call ucopy(jssmod(1,index),kindex,3)
CYG      if(kindex(3).eq.0)nbod=2
CYG
CYG      kferm=0
CYG      do 40 k=1,nbod
CYG        if(iabs(kindex(k)).gt.40)go to 30
CYG        kferm=kferm+1
CYG        fm(kferm)=ssmass(kindex(k))
CYG        lindex2(kferm)=kindex(k)
CYG        if(lindex1.lt.0)lindex2(kferm)=-kindex(k)
CYG        go to 40
CYG   30   kin=k
CYG   40 continue
CYG
CYG      fm(kferm+1)=ssmass(kindex(kin))
CYG      lindex2(kferm+1)=kindex(kin)
CYG      if(lindex1.lt.0)lindex2(kferm+1)=-kindex(kin)
CYG
CYG      RETURN
CYG      END
*CMZ :  1.00/00 14/04/95  18.39.59  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      SUBROUTINE smbod2(pin,indin,p1,p2,lindex2,ifail)

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
     +FLC(12),FRC(12),gms(12),echar(12)

      DIMENSION BETA(3)
      DOUBLE PRECISION pin(5),p1(5),p2(5)
      real rndm

      integer lindex2(3)

C
C attention top decays to s
C
      ifail=1

      selema=ssmass(indin)
      FMASS  =ssMASS(lindex2(1))
      fmk=ssmass(lindex2(2))

      XPS1 = XLAMDA (SELEMA**2,FMASS**2,fmk**2) / (2. * SELEMA)

      if(xps1.le.0)return

      PHCM    = TWOPI*dble(RNDM(0))
      CTHCM   =-1.+2.*dble(RNDM(0))
      STHCM   = dsqrt(1.-cthcm**2)

      p1(1)=xps1*sthcm*cos(phcm)
      p1(2)=xps1*sthcm*sin(phcm)
      p1(3)=xps1*cthcm
      p1(4)=dsqrt(xps1**2+fmass**2)
      p1(5)=fmass
      p2(1)=-p1(1)
      p2(2)=-p1(2)
      p2(3)=-p1(3)
      p2(4)=dsqrt(xps1**2+fmk**2)
      p2(5)=fmk

C
C         BOOST THE ELECTRONS 4-MOMENTA
C
      BETA(1) = Pin(1) / Pin(4)
      BETA(2) = Pin(2) / Pin(4)
      BETA(3) = Pin(3) / Pin(4)
      CALL BOOSTSUSY (P1,BETA)
      CALL BOOSTSUSY (P2,BETA)

      ifail=0

      return
      END
CYG*CMZ :  1.00/00 14/04/95  18.46.27  by  Stavros Katsanevas
CYG*-- Author :    Stavros Katsanevas   14/04/95
CYG      SUBROUTINE smbod3(PIN,indin,PP1,PP2,PP3,IND,adeca,ifail)
CYG*
CYGC****************   IT GENERATES THREE BODY DECAY
CYG*
CYG      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
CYG
CYG      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
CYG     +FLC(12),FRC(12),gms(12),echar(12)
CYG
CYG      integer ind(3)
CYG      DOUBLE PRECISION BETA(3)
CYG      DOUBLE PRECISION PIN(5),P1(5),P2(5),P3(5)
CYG      DOUBLE PRECISION        PP1(5),PP2(5),PP3(5)
CYG
CYG      common/widths/gw,widfl(12),widfr(12),fmeltw,fmertw,fmeluw,fmeruw,
CYG     +gent(50,2,168),gentl(2,2,168),linda(18,6,6)
CYG      real*4 rndm,rsb,rub
CYG
CYG
CYG      common/reorder/ispa(12,2),kl(2,18),klap(2,18),idecs(12,2)
CYG
CYGc
CYGc s = the W*,Z* combination
CYGc t = the up combination
CYGc u = the down combination
CYGc
CYG      ifail=1
CYG
CYG      fmi=ssmass(indin)
CYG      fml1=ssmass(ind(1))
CYG      fml2=ssmass(ind(2))
CYG      fmk=ssmass(ind(3))
CYGc
CYGc translete back codes to kfcur
CYGc
CYG      index1=iabs(indin)-70
CYG      k1=iabs(ind(1))
CYG      k2=iabs(ind(2))
CYG      index2=iabs(ind(3))-70
CYG
CYG
CYG      do 10 kfcur=1,18
CYG        ik1=iabs(kl(1,kfcur))
CYG        ik2=iabs(kl(2,kfcur))
CYG        if((k1.eq.ik1.and.k2.eq.ik2).or. (k1.eq.ik2.and.k2.eq.ik1))go
CYG     +  to 20
CYG   10 continue
CYG   20 continue
CYG
CYG
CYG
CYG      ntry=0
CYG   30 continue
CYG
CYG      ntry=ntry+1
CYG      if(ntry.gt.1000)then
CYG        write (1,*) ' Warning > 1000 in smbod3 '
CYG        return
CYG      endif
CYG
CYG      if(kfcur.gt.18)then
CYG        write(1,*)' error in branch logic kf ',kfcur,indin,ind(3)
CYG        write(1,*)' error in branch logic kf ',ind(1),ind(2)
CYG        stop99
CYG      endif
CYG
CYG      indx=linda(kfcur,index2,index1)
CYG
CYGc      call  gveg(z,gent(1,1,indx),gentl(1,1,indx))
CYGc      sb=z(1)
CYGc      ub=z(2)
CYG
CYG
CYG      call hrndm2(indx,rsb,rub)
CYG      sb=dble(rsb)
CYG      ub=dble(rub)
CYG
CYGc
CYGc  u= charged slepton
CYGc
CYG      GW1 = ((SB+FML2**2-FML1**2)/(2.*DSQRT(SB))+
CYG     +        (FMI**2-FMK**2-SB) /(2.*DSQRT(SB)))**2
CYG      GW2 = ((SB+FML2**2-FML1**2)**2/(4.*SB)-FML2**2)
CYG      GW3 = ((FMI**2-SB-FMK**2)**2/(4.*SB)-FMK**2)
CYG      if(GW2.le.0..or.GW3.le.0)go to 30
CYG      GW2 = DSQRT(GW2)
CYG      GW3 = DSQRT(GW3)
CYG      UMIN = GW1-(GW2+GW3)**2
CYG      UMAX = GW1-(GW2-GW3)**2
CYG      if(umin.ge.umax)go to 30
CYG      if(ub.lt.umin.or.ub.gt.umax)go to 30
CYG
CYG*
CYG      TB=FMI**2+FMK**2+fml1**2+fml2**2-SB-UB
CYG
CYG      P1(4)=(FMI**2+fml1**2-UB)/2./fmi
CYG      P2(4)=(FMI**2+fml2**2-TB)/2./fmi
CYG*
CYG      C12    = fmi**2-fmk**2-2.*fmi*(P1(4)+P2(4))
CYG      C12    = .5*C12/(P1(4)*P2(4))+1.
CYG      IF(ABS(C12).GT.1.)GO TO 30
CYG      CT1    = -1.+2.*dble(RNDM(0))
CYG      FI2    = TWOPI*dble(RNDM(0))
CYG
CYG      CF2    = COS(FI2)
CYG      SF2    = SIN(FI2)
CYG      ST1    = SQRT(1.D0-CT1**2)
CYG      S12    = SQRT(1.D0-C12**2)
CYG      PP=(P1(4)**2-fml1**2)
CYG
CYG      IF(PP.LT.0.)GO TO 30
CYG      PP=SQRT(PP)
CYG      P1(1)  = PP*ST1
CYG      P1(2)  = 0.0
CYG      P1(3)  = PP*CT1
CYG      P1(5)  = fml1
CYGC
CYG      PP=(P2(4)**2-fml2**2)
CYG      IF(PP.LT.0.)GO TO 30
CYG      PP=SQRT(PP)
CYG      P2(1)  = PP*(S12*CF2*CT1+C12*ST1)
CYG      P2(2)  = PP*S12*SF2
CYG      P2(3)  = PP*(C12*CT1-S12*CF2*ST1)
CYG      P2(5)  = fml2
CYGC
CYG      P3(4)  = fmi-P1(4)-P2(4)
CYG      P3(1)  = -P1(1)-P2(1)
CYG      P3(2)  = -P2(2)
CYG      P3(3)  = -P1(3)-P2(3)
CYG      P3(5)  = fmk
CYG
CYGC
CYGC      Here we rotate of phi1 the particles from the gaugino decay.
CYGC
CYG
CYG      FI    = TWOPI*dble(RNDM(0))
CYG      CF    = COS( FI )
CYG      SF    = SIN( FI )
CYG      CALL TRASQU(P1,PP1,CF,SF)
CYG      CALL TRASQU(P2,PP2,CF,SF)
CYG      CALL TRASQU(P3,PP3,CF,SF)
CYGC
CYGC      Here we rotate the GAUGINO vector of theta and phi
CYGC
CYG      pmom=sqrt(pin(1)**2+pin(2)**2+pin(3)**2)
CYG      px=pin(1)/pmom
CYG      py=pin(2)/pmom
CYG      CTHCM=pin(3)/pmom
CYG      PHCM  = atan2(py,px)
CYG
CYG      STHCM = SQRT( 1. - CTHCM**2 )
CYG      CPHCM = COS( PHCM )
CYG      SPHCM = SIN( PHCM )
CYG
CYG      CALL TRASLA( PP1,CTHCM,STHCM,CPHCM,SPHCM )
CYG      CALL TRASLA( PP2,CTHCM,STHCM,CPHCM,SPHCM )
CYG      CALL TRASLA( PP3,CTHCM,STHCM,CPHCM,SPHCM )
CYGC
CYGC      Here we boost the particles from the wino to the c. of m. frame.
CYGC
CYG      BETA(1) = Pin(1) / Pin(4)
CYG      BETA(2) = Pin(2) / Pin(4)
CYG      BETA(3) = Pin(3) / Pin(4)
CYG
CYG      CALL BOOSTSUSY (PP1,BETA)
CYG      CALL BOOSTSUSY (PP2,BETA)
CYG      CALL BOOSTSUSY (PP3,BETA)
CYG
CYG      ifail=0
CYG
CYG      return
CYG
CYG
CYG      END
CYG*CMZ :  1.00/00 14/04/95  18.46.27  by  Stavros Katsanevas
CYG*-- Author :    Stavros Katsanevas   14/04/95
CYG      SUBROUTINE sfragment(IFLAG)
CYGC*
CYGC*
CYG      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
CYG      DOUBLE PRECISION PV
CYG      COMMON /PARTC / pv(5,30)
CYG      COMMON /PARTC1 / iflav(30,2),nflav
CYG      COMMON /CONST/ idbg,igener,irad
CYG	INTEGER Ipar(4),IJ(3,2)
CYG	COMMON/INDICES/Ipar,IJ
CYG
CYG        logical rpar
CYG        common/rpari/rpar
CYG        
CYG      DOUBLE PRECISION  DBE(3)
CYG
CYG      dimension qp(4),qm(4),qa(4)
CYG      dimension ijon(3,10),erd(10)
CYG    
CYG
CYG   10 CONTINUE
CYG
CYG      
CYG      if(nflav.eq.0)return
CYG
CYGC...Reset commonblock LUJETS.
CYG
CYG      DO 20  I=1,20
CYG        DO 20  J=1,5
CYG          K(I,J)=0
CYG          P(I,J)=0.
CYG   20 V(I,J)=0.
CYG
CYGC...Store PARTICLES
CYG
CYG      ISTR=0
CYG
CYG      njon=0
CYG
CYG      DO 60 NC=1,NFLAV
CYG      
CYG        K(NC,1)=1
CYG        K(NC,2)=IFLAV(NC,1)
CYG        K(NC,3)=IFLAV(NC,2)
CYG
CYG        if(iabs(iflav(nc,1)).ge.40)k(nc,1)=21
CYG
CYG        DO 30  J=1,5
CYG   30   P(NC,j)=PV(J,NC)
CYG
CYG        N=NC
CYG
CYG
CYG        if(ipar(1).eq.3.and.(.not.rpar).and.nc.gt.2)then
CYG      IF(iabs(K(NC-2,2)).LT.10.AND.iabs(K(NC-1,2)).LT.10.and.
CYG     +iabs(K(NC,2)).LT.10)then
CYG      IF((K(NC-2,3).eq.K(NC-1,3)).and.(K(NC-1,3).eq.K(NC,3)))then
CYG
CYG         k1=k(nc-2,2)
CYG         k2=k(nc-1,2)
CYG         isign=1
CYG         if(k1.lt.0)isign=-1
CYG         ik=k1*1000+k2*100+isign
CYG         if(iabs(k2).gt.iabs(k1))ik=k2*1000+k1*100+isign
CYG
CYG          k(nc-2,1)=2
CYG          k(nc-2,2)=ik
CYG
CYG          DO 141  J=1,4
CYG  141     p(nc-2,j)=p(nc-2,j)+p(nc-1,j)
CYG
CYG          DO 140  J=1,4
CYG  140     qa(J)=P(NC-2,j)+P(nc-1,j)
CYG          p(nc-2,5)=sqrt(qa(4)**2-qa(1)**2-qa(2)**2-qa(3)**2)
CYG       
CYG          k(nc-1,1)=1
CYG          k(nc-1,2)=k(nc,2)
CYG
CYG          DO 142  J=1,5
CYG  142     p(nc-1,j)=p(nc,j)
CYG
CYG        DO 143  J=1,5
CYG          K(nc,J)=0
CYG          P(nc,J)=0.
CYG  143     V(nc,J)=0.
CYG
CYG
CYG          njon=njon+1
CYG          ijon(1,njon)=nc-2
CYG          ijon(2,njon)=nc-1
CYG          ijon(3,njon)=0
CYG
CYG
CYG          DO 144  J=1,4
CYG          QP(J)=p(nc-2,j)
CYG  144     QM(J)=P(nc-1,j)
CYG
CYG          DO 145  J=1,3
CYG  145     DBE(J)=(QP(J)+QM(J))/(QP(4)+QM(4))
CYG
CYG          CALL LUDBRB(NC-2,NC-1,0.,0.,-DBE(1),-DBE(2),-DBE(3))
CYG
CYG          ERED=P(NC-2,4)+P(NC-1,4)
CYG
CYG          K1=K(NC-1,2)
CYG          K2=K(NC-2,2)
CYG
CYG          IF(ERED.LT.ULMASS(k1)+ulmass(k2)) write(1,*)' ER ',ERED
CYG
CYG          THE=ULANGL(P(NC-2,3),SQRT(P(NC-2,1)**2+P(NC-2,2)**2))
CYG          PHI=ULANGL(P(NC-2,1),P(NC-2,2))
CYG
CYG          CALL LU2ENT(-(NC-2),k1,k2,ERED)
CYG
CYG          CALL LUDBRB(NC-2,NC-1,THE,PHI,DBE(1),DBE(2),DBE(3))
CYG
CYG      endif
CYG      endif
CYG
CYG        else
CYG
CYG        IF(iabs(K(NC-1,2)).LT.10.AND.iabs(K(NC,2)).LT.10)THEN
CYG
CYG          DO 40  J=1,4
CYG            QP(J)=p(nc,j)
CYG   40     QM(J)=P(nc-1,j)
CYG
CYG          DO 50  J=1,3
CYG   50     DBE(J)=(QP(J)+QM(J))/(QP(4)+QM(4))
CYG
CYG          CALL LUDBRB(NC-1,NC,0.,0.,-DBE(1),-DBE(2),-DBE(3))
CYG
CYG          ERED=P(NC-1,4)+P(NC,4)
CYG
CYG          K1=IFLAV(NC,1)
CYG          K2=IFLAV(NC-1,1)
CYG
CYG          njon=njon+1
CYG          ijon(1,njon)=nc-1
CYG          ijon(2,njon)=nc
CYG          ijon(3,njon)=0
CYG          erd(njon)=ered
CYG
CYG          IF(ERED.LT.ULMASS(k1)+ulmass(k2)) write(1,*)' ER ',ERED
CYG
CYG          THE=ULANGL(P(NC-1,3),SQRT(P(NC-1,1)**2+P(NC-1,2)**2))
CYG          PHI=ULANGL(P(NC-1,1),P(NC-1,2))
CYG
CYG          CALL LU2ENT(-(NC-1),k1,k2,ERED)
CYG
CYG          CALL LUDBRB(NC-1,NC,THE,PHI,DBE(1),DBE(2),DBE(3))
CYG
CYG
CYG
CYG        ENDIF
CYG        endif
CYG
CYG
CYG
CYG   60 CONTINUE
CYG
CYG      if(idbg.eq.2)call lulist(1)
CYG
CYGC...Shower, fragment and decay.
CYG
CYG      if(njon.gt.0)then
CYG      do 70 jj=1,njon
CYG      if(ijon(3,jj).eq.0)then
CYG      CALL LUSHOW(ijon(1,jj),ijon(2,jj),ERD(jj))
CYG      else
CYG      CALL LUSHOW(ijon(1,jj),-3,ERD(jj))
CYG      endif
CYG 70   continue
CYG      endif
CYG      CALL LUEXEC
CYG
CYG      if(idbg.eq.1)call lulist(1)
CYG
CYG      iflag=0
CYG
CYG      RETURN
CYG      END

      subroutine absalom
      return
      end

*CMZ :  1.00/00 14/04/95  18.39.59  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      SUBROUTINE ROTSUSY(PIN,COSTH,PHI,POUT)

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
      DIMENSION PIN(5),POUT(5)
C
C
C
      SINTH = DSQRT (1. - COSTH**2)
      SINPH = DSIN (PHI)
      COSPH = DCOS (PHI)
      POUT(1)   = (PIN(3)  *SINTH + PIN(1)  *COSTH)*COSPH
     +-PIN(2)  *SINPH
      POUT(2)   = (PIN(3)  *SINTH + PIN(1)  *COSTH)*SINPH
     ++PIN(2)  *COSPH
      POUT(3)   = (PIN(3)  *COSTH - PIN(1)  *SINTH)
      POUT(4) = PIN(4)
      POUT(5) = PIN(5)
      RETURN
      END
*CMZ :  1.00/00 14/04/95  18.44.40  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      SUBROUTINE BOOSTSUSY (PV,BST)
C----------------------------------------------------------------------
C-
C-   PV --> PV' (BOOSTED BY BST)
C-
C-   NB.  PV   (PX,PY,PZ,E,M)
C-        BST  (BX,BY,BZ)
C-
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION PV(5),BST(3)
C

      BB2 = BST(1)**2 + BST(2)**2 + BST(3)**2

      if(BB2.ge.1.d0)then
        write(*,*)' error in boostsusy ',BB2
        bb2=0.99999d0
      endif

      GAM = 1. / DSQRT (1. - BB2)
      TM1 = BST(1) * PV(1) + BST(2) * PV(2) + BST(3) * PV(3)
      TM2 = GAM * TM1 / (GAM + 1.)  + PV(4)
      PV(4) = GAM * (PV(4) + TM1)
      DO 10 J=1,3
   10 PV(J) = PV(J) + GAM * BST(J) * TM2
      END
*CMZ :  1.00/00 14/04/95  18.39.59  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      SUBROUTINE sdecay2 (PM,COST,PHI,XM,PV)
C----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION PV(5)
      SOST = DSQRT (1. - COST**2)
      PV(3) = PM * COST
      PV(1) = PM * SOST * DCOS (PHI)
      PV(2) = PM * SOST * DSIN (PHI)
      PV(4) = DSQRT (PM**2 + XM**2)
      PV(5) = XM
      END
*CMZ :  1.00/00 14/04/95  18.44.40  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      DOUBLE PRECISION FUNCTION XLAMDA (A,B,C)
C----------------------------------------------------------------------
C-
C-   X = SQRT (AA + BB + CC - 2AB - 2AC - 2AC)
C-
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      xlamda=0.d0

      X = A * A + B * B + C * C
      Y = A * B + A * C + B * C
      if(x-2.d0*y.lt.0.)return

      aa=dsqrt(a)
      bb=dsqrt(b)
      cc=dsqrt(c)
      if(aa.lt.bb+cc)return

      XLAMDA = DSQRT (X - 2. d0* Y)
      END
*CMZ :  1.00/00 14/04/95  18.39.59  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      SUBROUTINE TRASQU(P1,P2,CF,SF)
C******** IT ROTATES WITH RESPECT TO THE Z AXIS

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

C
      DIMENSION P1(5),P2(5)
C
      P2(1)=P1(1)*CF+P1(2)*SF
      P2(2)=P1(2)*CF-P1(1)*SF
      P2(3)=P1(3)
      P2(4)=P1(4)
      P2(5)=P1(5)
      END
*CMZ :  1.00/00 14/04/95  18.46.27  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      SUBROUTINE TRASLA(P2,CT,ST,CF,SF)
C*****
C***** IT ROTATES THE VECTORS
C***** OF THE ANGLES THETA AND PHI
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      DIMENSION P1(5),P2(5)
      do 10 k=1,3
   10 p1(k)=p2(k)
C
      P2(3)=P1(3)*CT-P1(1)*ST
      PP=P1(1)*CT+P1(3)*ST
C
      P2(1)=PP*CF-P1(2)*SF
      P2(2)=P1(2)*CF+PP*SF
C
      END
*CMZ :  1.00/00 14/04/95  18.46.27  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
CDECK  ID>, SSDINT.
      DOUBLE PRECISION FUNCTION SSDINT(XL,F,XR)
C-----------------------------------------------------------------------
C     Integrate double precision F over double precision (XL,XR)
C     Note quadrature constants R and W have been converted to explicit
C     double precision (.xxxxxDxx) form.
C
C     Bisset's XINTH
C-----------------------------------------------------------------------
      IMPLICIT NONE
      COMMON/SSLUN/LOUT
      INTEGER LOUT
      SAVE /SSLUN/
      EXTERNAL F
      INTEGER NMAX
      DOUBLE PRECISION TOLABS,TOLREL,XLIMS(200)
      DOUBLE PRECISION R(93),W(93)
      INTEGER PTR(4),NORD(4)
      INTEGER ICOUNT
      DOUBLE PRECISION XL,XR,F
      DOUBLE PRECISION AA,BB,TVAL,VAL,TOL
      INTEGER NLIMS,I,J,KKK
C
      DATA PTR,NORD/4,10,22,46,  6,12,24,48/
      DATA (R(KKK),KKK=1,48)/
     + .2386191860D0,.6612093865D0,.9324695142D0,.1252334085D0,
     + .3678314990D0,.5873179543D0,.7699026742D0,.9041172563D0,
     + .9815606342D0,.0640568929D0,.1911188675D0,.3150426797D0,
     + .4337935076D0,.5454214714D0,.6480936519D0,.7401241916D0,
     + .8200019860D0,.8864155270D0,.9382745520D0,.9747285560D0,
     + .9951872200D0,.0323801710D0,.0970046992D0,.1612223561D0,
     + .2247637903D0,.2873624873D0,.3487558863D0,.4086864820D0,
     + .4669029048D0,.5231609747D0,.5772247261D0,.6288673968D0,
     + .6778723796D0,.7240341309D0,.7671590325D0,.8070662040D0,
     + .8435882616D0,.8765720203D0,.9058791367D0,.9313866907D0,
     + .9529877032D0,.9705915925D0,.9841245837D0,.9935301723D0,
     + .9987710073D0,.0162767488D0,.0488129851D0,.0812974955D0/
      DATA (R(KKK),KKK=49,93)/
     + .1136958501D0,.1459737146D0,.1780968824D0,.2100313105D0,
     + .2417431561D0,.2731988126D0,.3043649444D0,.3352085229D0,
     + .3656968614D0,.3957976498D0,.4254789884D0,.4547094222D0,
     + .4834579739D0,.5116941772D0,.5393881083D0,.5665104186D0,
     + .5930323648D0,.6189258401D0,.6441634037D0,.6687183100D0,
     + .6925645366D0,.7156768123D0,.7380306437D0,.7596023411D0,
     + .7803690438D0,.8003087441D0,.8194003107D0,.8376235112D0,
     + .8549590334D0,.8713885059D0,.8868945174D0,.9014606353D0,
     + .9150714231D0,.9277124567D0,.9393703398D0,.9500327178D0,
     + .9596882914D0,.9683268285D0,.9759391746D0,.9825172636D0,
     + .9880541263D0,.9925439003D0,.9959818430D0,.9983643759D0,
     + .9996895039/
      DATA (W(KKK),KKK=1,48)/ .4679139346D0,.3607615730D0,
     +.1713244924D0,.2491470458D0, .2334925365D0,.2031674267D0,
     +.1600783285D0,.1069393260D0, .0471753364D0,.1279381953D0,
     +.1258374563D0,.1216704729D0, .1155056681D0,.1074442701D0,
     +.0976186521D0,.0861901615D0, .0733464814D0,.0592985849D0,
     +.0442774388D0,.0285313886D0, .0123412298D0,.0647376968D0,
     +.0644661644D0,.0639242386D0, .0631141923D0,.0620394232D0,
     +.0607044392D0,.0591148397D0, .0572772921D0,.0551995037D0,
     +.0528901894D0,.0503590356D0, .0476166585D0,.0446745609D0,
     +.0415450829D0,.0382413511D0, .0347772226D0,.0311672278D0,
     +.0274265097D0,.0235707608D0, .0196161605D0,.0155793157D0,
     +.0114772346D0,.0073275539D0, .0031533461D0,.0325506145D0,
     +.0325161187D0,.0324471637D0/
      DATA (W(KKK),KKK=49,93)/
     + .0323438226D0,.0322062048D0,.0320344562D0,.0318287589D0,
     + .0315893308D0,.0313164256D0,.0310103326D0,.0306713761D0,
     + .0302999154D0,.0298963441D0,.0294610900D0,.0289946142D0,
     + .0284974111D0,.0279700076D0,.0274129627D0,.0268268667D0,
     + .0262123407D0,.0255700360D0,.0249006332D0,.0242048418D0,
     + .0234833991D0,.0227370697D0,.0219666444D0,.0211729399D0,
     + .0203567972D0,.0195190811D0,.0186606796D0,.0177825023D0,
     + .0168854799D0,.0159705629D0,.0150387210D0,.0140909418D0,
     + .0131282296D0,.0121516047D0,.0111621020D0,.0101607705D0,
     + .0091486712D0,.0081268769D0,.0070964708D0,.0060585455D0,
     + .0050142027D0,.0039645543D0,.0029107318D0,.0018539608D0,
     + .0007967921/
C
C      DATA TOLABS,TOLREL,NMAX/1.D-35,5.D-5,100/
      DATA TOLABS,TOLREL,NMAX/1.D-30,5.D-4,200/
C
      SSDINT=0
      NLIMS=2
      XLIMS(1)=XL
      XLIMS(2)=XR
      ICOUNT=0
C
   10 AA=(XLIMS(NLIMS)-XLIMS(NLIMS-1))/2
      BB=(XLIMS(NLIMS)+XLIMS(NLIMS-1))/2
      TVAL=0
      DO 20 I=1,3
   20 TVAL=TVAL+W(I)*(F(BB+AA*R(I))+F(BB-AA*R(I)))
      TVAL=TVAL*AA
      DO 40 J=1,4
        VAL=0
        DO 30 I=PTR(J),PTR(J)-1+NORD(J)
          ICOUNT=ICOUNT+1
          IF(ICOUNT.GT.1E5) THEN
            WRITE(*,*) 'ERROR IN SSDINT: SET SSDINT TO ZERO'
CPM            WRITE(6,*) 'ERROR IN SSDINT: SET SSDINT TO ZERO'
            SSDINT=0.
            RETURN
          ENDIF
   30   VAL=VAL+W(I)*(F(BB+AA*R(I))+F(BB-AA*R(I)))
        VAL=VAL*AA
        TOL=MAX(TOLABS,TOLREL*ABS(VAL))
        IF(ABS(TVAL-VAL).LT.TOL) THEN
          SSDINT=SSDINT+VAL
          NLIMS=NLIMS-2
          IF (NLIMS.NE.0) GO TO 10
          RETURN
        ENDIF
   40 TVAL=VAL
      IF(NMAX.EQ.2) THEN
        SSDINT=VAL
        RETURN
      END IF
      IF(NLIMS.GT.(NMAX-2)) THEN
        WRITE(*,10000) SSDINT,NMAX,BB-AA,BB+AA
CPM        WRITE(6,10000) SSDINT,NMAX,BB-AA,BB+AA
        RETURN
      ENDIF
      XLIMS(NLIMS+1)=BB
      XLIMS(NLIMS+2)=BB+AA
      XLIMS(NLIMS)=BB
      NLIMS=NLIMS+2
      GO TO 10
C
10000 FORMAT (' SSDINT FAILS, SSDINT,NMAX,XL,XR=',G15.7,I5,2G15.7)
      END
*CMZ :  1.00/00 14/04/95  18.39.59  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      DOUBLE PRECISION FUNCTION SPECTR(XK,S,FAC)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      EXTERNAL SIGMA
C DEFINITION OF BREMSSTRAHLUNG SPECTRUM
      SPECTR=FAC*(1.+(1.-XK)**2)/XK*SIGMA(S*(1.-XK))
      RETURN
      END
*CMZ :  1.00/00 14/04/95  18.46.27  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      SUBROUTINE REMT1(EBEAMX,CROSS)
C THIS PART INITIALIZES THE INITIAL-STATE RADIATOR.
C IT CALCULATES SOME QUANTITIES, AND PERFORMS THE
C NUMERICAL INTEGRATION OVER THE PHOTON SPECTRUM.
C EBEAMX=BEAM ENERGY (IN GEV)
C CROSS=NONRADIATIVE CROSS SECTION, TO BE DEFINED
C       WITH ONE VARIABLE: CROSS(S),
C       WHERE S IS THE INVARIANT MASS OF THE E+E- PAIR.

      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
c      DIMENSION X(5000),F(5000),A(5000),Y(5000),Z(5000),XNEW(5000)
      DIMENSION X(1000),F(1000),A(1000),Y(1000),Z(1000),XNEW(1000)
      DIMENSION QK(4),QIN(4),QOUT(4)
      real*4 rndm
      EXTERNAL SPECTR,CROSS
      SAVE EBEAM

      COMMON/INDEX/index,index1,index2,nevt
      COMMON/FINDEX/fmpr1,fmpr2,XCROST,APRO


C
C INITIALIZE A FEW QUANTITIES AND CONSTANTS
      EBEAM = EBEAMX
      S  = 4.D0*EBEAM**2

      XCROST=0.
      SIG0 = CROSS(S)
      SIG1 = CROSS(S)
      xcrost=sig1
      IF(SIG0.LT.0.000001)RETURN

C
C CHECK WHETHER THE CROSS SECTION IS 
C
      XPS= (.511D-03/EBEAM)**2/2.
      XPT= (2.+XPS)/XPS
      XPL= DLOG(XPT)
      PI = 4.*DATAN(1.D0)
      TPI= 2.*PI
c      ALF= 1./137.036D0

      ALF= 1./137.D0

      alf=alfaem(s)

      FAC= ALF/PI*(XPL-1.)
      XKC= DEXP( - ( PI/(2.*ALF) + 3./4.*XPL + PI**2/6. - 1. )
     +            /( XPL - 1. )                           )
c      WRITE(6,1) EBEAM,S,XPS,XPT,XPL,PI,TPI,ALF,FAC,XKC
10000 FORMAT(1H0,80(1H=),/,
     +  '0INITIALIZATION OF ROUTINE PACKAGE REMT:',/,
     +  '0BEAM ENERGY  : ',F7.3,' GEV',/,
     +2('0CONSTANTS    : ',4D15.6,/),
     +  '0MINIMAL BREMSSTRAHLUNG ENERGY : ',D10.4,' * EBEAM')
C
C PARAMETERS OF NUMERICAL INTEGRATION STEP
      X1   = XKC
      XN   = 1.-400./S
      N    = 500
      ITER = 6
c      WRITE(6,10100) X1,XN,N,ITER
10100 FORMAT('0PARAMETERS OF SPECTRUM INTEGRATION:',/,
     +       '0LOWEST  K VALUE   : ',D10.3,/,
     +       '0HIGHEST K VALUE   : ',D10.3,/,
     +       '0NO. OF POINTS     : ',I5,/,
     +       '0NO. OF ITERATIONS : ',I3)
C
C INITIALIZE BY CHOOSING EQUIDISTANT X VALUES
      IT=0
      M=N-1
      DX=(XN-X1)/DFLOAT(M)
      X(1)=X1
      DO 10  I=2,N
   10 X(I)=X(I-1)+DX
C
C STARTING POINT FOR ITERATIONS

   20 CONTINUE
C
C CALCULATE FUNCTION VALUES
      DO 30  I=1,N
   30 F(I)=SPECTR(X(I),S,FAC)
C
C CALCULATE BIN AREAS
      DO 40  I=1,M
   40 A(I)=(X(I+1)-X(I))*(F(I+1)+F(I))/2.d0
C
C CALCULATE CUMULATIVE SPECTRUM Y VALUES
      Y(1)=0.D0
      DO 50  I=2,N
   50 Y(I)=Y(I-1)+A(I-1)
C
C PUT EQUIDISTANT POINTS ON Y SCALE

      if(Y(N).eq.0)return

      DZ=Y(N)/DFLOAT(M)
      Z(1)=0.D0
      DO 60  I=2,N
   60 Z(I)=Z(I-1)+DZ
C
C DETERMINE SPACING OF Z POINTS IN BETWEEN Y POINTS
C FROM THIS, DETERMINE NEW X VALUES AND FINALLY REPLACE OLD VALUES
      XNEW(1)=X(1)
      XNEW(N)=X(N)
      K=1
      DO 90  I=2,M
   70   IF( Y(K+1) .GT. Z(I) ) GOTO 80
        K=K+1
        GOTO 70
   80   R= ( Z(I) - Y(K) ) / ( Y(K+1) - Y(K) )
   90 XNEW(I) = X(K) + ( X(K+1)-X(K) )*R
      DO 100 I=1,N
  100 X(I)=XNEW(I)
C
C CHECK ON END OF ITERATIONS AND RETURN
      IT=IT+1
c      WRITE(6,3) IT,Y(M)
10200 FORMAT('0ITERATION NO.=',I3,'  INTEGRAL =',D15.6)
      IF(IT.LT.ITER) GOTO 20
C
C PRESENT RESULTS IN FORM OF CORRECTION

      SIG0 = CROSS(S)

      SIG1 = Y(M)

      xcrost=sig1

      DELT = (SIG1/SIG0-1.)*100.
CPM      WRITE(1,10300) SIG0,SIG1,DELT
10300 FORMAT(
     +       ' NONRADIATIVE CROSS SECTION :',D15.6,/,
     +       '    RADIATIVE CROSS SECTION :',D15.6,/,
     +       '    RADIATIVE CORRECTION    :',F10.3,' %')
      RETURN


      ENTRY REMT2(QK)
C THIS PART GENERATES A BREMSSTRAHLUNG PHOTON
C AND CALCULATES WHICH BEAM AXIS TO CHOOSE FOR
C THE GENERATION OF THE 'NONRADIATIVE' CROSS SECTION.
C THE PHOTON ENERGY SPECTRUM MUST HAVE BEEN EXAMINED
C BY CALLING ENTRY 'REMT1' BEFORE THE FIRST CALL TO
C THIS ENTRY.
C
C INITIALIZE FLAG FOR REMT3
      IR=0
C
C GENERATE PHOTON ENERGY FROM CUMULATIVE SPECTRUM BINS
      R=dfloat(M)*dble(rndm(0))
      I=IDINT(R)
      S=R-dfloat(I)
      XK = X(I+1) + S*( X(I+2)-X(I+1) )
C
C GENERATE AZIMUTHAL SCATTERING ANGLE OF THE PHOTON
      FG=TPI*dble(rndm(0))
C
C GENERATE COSINE OF POLAR SCATTERING ANGLE OF THE PHOTON
  110 IT=IT+1
      V= XPS * ( XPT**dble(rndm(0)) - 1.d0 )
      W= XPS + V*(1.d0-.5d0*V)
      W= dble(rndm(0))/(1.d0-(XK*XK*W+2.*XPS*(1.d0-XK)/W)/(1.d0+
     +(1.d0-XK)**2))
      IF(W.GT.1.D0) GOTO 110
      W= -1.d0 + 2.d0*W
      CG=DSIGN(1.d0-V,W)
C
C CHOOSE WHICH OF THE TWO Z AXES SHOULD BE CONSIDERED
      CH=-1.
      IF(DABS(W).LT.(1./(1.+(1.-2./(1.+XK*CG/(2.-XK)))**2))) CH=+1.
C
C CONSTRUCT PHOTON FOUR-MOMENTUM
      SG=DSQRT(V*(2.d0-V))
      QK(4)=XK*EBEAM
      if(qk(4).lt.0.0001)qk(4)=0.0001
      QK(1)=QK(4)*SG*DCOS(FG)
      QK(2)=QK(4)*SG*DSIN(FG)
      QK(3)=QK(4)*CG
C
      RETURN
C
      ENTRY REMT3(QK,QIN,QOUT)
C THIS PART PERFORMS THE ROTATIONS AND BOOSTS OF THE I.S.R.
C FORMALISM AFTER THE USER'S BLACK BOX HAS RUN AN EVENT.
C THE INPUT VECTOR (FROM USERS BLACK BOX) IS QIN;
C THE RESULTING VECTOR IN THE LAB FRAME IS QOUT.
C
C INITIALIZATION PART: ONCE FOR EVERY GENERATED PHOTON MOMENTUM
      IF(IR.NE.0) GOTO 120
      IR=1

      ads=qk(1)**2+qk(2)**2
      if(ads.le.0.d0)ads=0.0000000001d0

C
C CALCULATE ROTATTION PARAMETERS FOR BEAM DIRECTION IN C.M.S.
      XKP = DSQRT( ads )
      XKM = 2.d0* DSQRT( EBEAM*(EBEAM-QK(4)) )
      XKD = 2.d0*EBEAM - QK(4) + XKM
      XKA = ( CH + QK(3)/XKD )/XKM
      XKB = DSQRT( (1.+XKA*QK(3))**2 + (XKA*XKP)**2 )
      S1  = XKA*XKP/XKB
      C1  = (1.d0+XKA*QK(3))/XKB
      S2  = QK(1)/XKP
      C2  = QK(2)/XKP
      YK=QK(4)**2- ads -QK(3)**2
      Y1=C1**2+S1**2-1.d0
      Y2=C2**2+S2**2-1.d0
C
C ROTATE INPUT VECTOR QIN(I) TO CORRESPOND WITH CHOZEN Z-AXIS
  120 QQ =  C1*QIN(2) + S1*QIN(3)
      QZ = -S1*QIN(2) + C1*QIN(3)
      QX =  C2*QIN(1) + S2*QQ
      QY = -S2*QIN(1) + C2*QQ
C
C BOOST ROTATED VECTOR TO LAB FRAME VECTOR QOUT
      QOUT4   =((XKD-XKM)*QIN(4)-QK(1)*QX-QK(2)*QY-QK(3)*QZ)/XKM
      QQ     =(QIN(4)+QOUT4)/XKD
      QOUT(1)= QX - QK(1)*QQ
      QOUT(2)= QY - QK(2)*QQ
      QOUT(3)= QZ - QK(3)*QQ
      QOUT(4)= QOUT4

      RETURN
      END
*CMZ :  1.00/00 14/04/95  18.46.27  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      DOUBLE PRECISION FUNCTION SIGMA(SHAT)

      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON/INDEXX/index,index1,index2,nevt
      COMMON/FINDEX/fmpr1,fmpr2,XCROST,APRO

      COMMON/KINEM/flum,ecm,s,roots,T,Q,Q2,EN(2)

      common/reorder/ispa(12,2),kl(2,18),klap(2,18),idecs(12,2)
      external gensel,genselrs,gensmu,gensnue,gensnu,photi,chargi

      roots=dsqrt(shat)
      S=shat

      if(index.eq.1)then
        xcrost=photi(index1,index2)
      endif

      if(index.eq.2)then
        xcrost=chargi(index1,index2)
      endif

      if(index.eq.3)then
        mindex1=iabs(index1)
        mindex2=iabs(index2)

        if(mindex1.eq.51.and.mindex2.ne.mindex1)then
          xCROSt=genselrs(dummy)
        else

          do 10 k=1,12
            do 10 l=1,2
              k1=mod(k-1,4)+1

              if(index1.ne.ispa(k,l))go to 10

              if(k.eq.3)xCROSt=ssdint(-1.d0,gensnue,1.D0)
              if(k.eq.4)xCROSt=ssdint(-1.d0,gensel,1.d0)
              if(k.ne.4.and.k1.ne.3)xCROSt=gensmus(dummy)
              if(k.ne.3.and.k1.eq.3)xCROSt=ssdint(-1.D0,gensnu,1.D0)

   10     continue
        endif
      endif

      sigma=xcrost
      if(sigma.lt.0)write(*,*)' negative cros section ? ',xcrost
      if(sigma.lt.0)sigma=0.

      RETURN
      END
*CMZ :  1.00/00 14/04/95  18.40.00  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      SUBROUTINE SXWRLU (LUNIT)
C
C...SXWRLU  FOR WRITING EVENTS FROM LUND COMMON BLOCK OUT ON EXTERNAL FILE
C.
C.  ARGUMENTS    CALL SXWRLU (LUNIT)
C.          LUNIT     LOGICAL UNIT FOR WRITING EVENTS
C.
C.  FORMATS: (NON-PORTABLE!!!)
C.        EVENT RECORDS, IN THE FOLLOWING
C.        FORMAT (ONE RECORD PER EVENT):
C.
C.    WORD 0 : N                    (I)
C.         1 : K(1,1)               (I) \
C.         2 : K(1,2)               (I)  \
C.         3 : K(1,3)               (I)   \
C.         4 : K(1,4)               (I)    \
C.         5 : K(1,5)               (I)     \
C.         6 : P(1,1)               (F)      \
C.         7 : P(1,2)               (F)       \
C.         8 : P(1,3)               (F)        >  REPEATED N TIMES
C.         9 : P(1,4)               (F)       /
C.        10 : P(1,5)               (F)      /
C.        11 : V(1,1)               (F)     /
C.        12 : V(1,2)               (F)    /
C.        13 : V(1,3)               (F)   /
C.        14 : V(1,4)               (F)  /
C.        15 : V(1,5)               (F) /
C.        16 : K(2,1)
C.       ...
C.     N*7-1 : P(N,4)
C.       N*7 : P(N,5)
C.
C.
C.----------------------------------------------------------------------
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      SAVE /LUJETS/
C
C
C   WRITE EVENT DATA
C
   10 CONTINUE

      if(N.eq.0)return

      WRITE (LUNIT)
     +     N,((K(I,J),J=1,5),(P(I,J),J=1,5),(V(I,J),J=1,5),I=1,N)
      RETURN
      END
CYG*CMZ :  1.00/00 14/04/95  18.44.40  by  Stavros Katsanevas
CYG*-- Author :    Stavros Katsanevas   14/04/95
CYG      subroutine sbook
CYG
CYGC Keys
CYG      logical wrt,scan,lepi
CYG      common/str/wrt,scan,lepi
CYG
CYG      real*4  rgmaum,rgmaur,rgm0,rgtanb,rgatri,rgma,
CYG     +        rfmsq,rfmstopl,rfmstopr,
CYG     +        rfmsell,rfmselr,rfmsnu,rfmglu,recm,rflum
CYG     +        ,vscan(6) ! only in histobook rvscan --> vscan, sorry
CYG      common /rkey/rgmaum,rgmaur,rgm0,rgtanb,rgatri,rgma,
CYG     +        rfmsq,rfmstopl,rfmstopr,
CYG     +        rfmsell,rfmselr,rfmsnu,rfmglu,recm,rflum
CYG     +        ,vscan    ! only in histobook rvscan --> vscan, sorry
CYG
CYGC ntuple
CYG      common/pawc/hmemor(300000)
CYG
CYG      logical zino,wino,sele,smuo,stau,snu,squa,stopa,sbota,higgs
CYG      common/sparc/ zino,wino,sele,smuo,stau,snu,squa,stopa,sbota,higgs
CYG
CYG      call hlimit(300000)
CYG
CYG
CYG      if(scan)then
CYGc
CYGc scan histos
CYGc
CYGc
CYGc gauginos
CYGc
CYGc
CYGc masses
CYGc
CYG        call hbook2(401,' mass x01 ', int(vscan(1)),vscan(2),vscan(3),
CYG     +  int(vscan(4)),vscan(5),vscan(6),0.)
CYG        call hbook2(402,' mass x02  ', int(vscan(1)),vscan(2),vscan(3),
CYG     +  int(vscan(4)),vscan(5),vscan(6),0.)
CYG        call hbook2(403,' mass x+1  ', int(vscan(1)),vscan(2),vscan(3),
CYG     +  int(vscan(4)),vscan(5),vscan(6),0.)
CYG
CYG        if(zino.or.wino)then
CYG
CYGc
CYGc cross sections
CYGc
CYG          call hbook2(101,' cs x01x01 ', int(vscan(1)),vscan(2),
CYG     +    vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG          call hbook2(102,' cs x01x02 ', int(vscan(1)),vscan(2),
CYG     +    vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG          call hbook2(103,' cs x+1x-1 ', int(vscan(1)),vscan(2),
CYG     +    vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG
CYGc
CYGc BR
CYGc
CYGc x02 BR
CYG
CYG          call hbook2(501,' x02 BR to x01qqbar ', int(vscan(1)),
CYG     +    vscan(2),vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG          call hbook2(502,' x02 BR to x01l+l-  ', int(vscan(1)),
CYG     +    vscan(2),vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG          call hbook2(503,' x02 BR to x01vv  ', int(vscan(1)),vscan(2),
CYG     +    vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG          call hbook2(504,' x02 BR to x-1x-l+v ', int(vscan(1)),
CYG     +    vscan(2),vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG          call hbook2(505,' x02 BR to x-1x-qqp  ', int(vscan(1)),
CYG     +    vscan(2),vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG
CYGc
CYGc x+1 BR
CYGc
CYG          call hbook2(601,' x+1 BR to x01x-l+v ', int(vscan(1)),
CYG     +    vscan(2),vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG          call hbook2(602,' x+1 BR to x01x-qqp  ', int(vscan(1)),
CYG     +    vscan(2),vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG          call hbook2(603,' x+1 BR to x02x-l+v ', int(vscan(1)),
CYG     +    vscan(2),vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG          call hbook2(604,' x+1 BR to x02x-qqp  ', int(vscan(1)),
CYG     +    vscan(2),vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG
CYG        endif
CYG
CYGc
CYGc sfermions
CYGc
CYG        if(snu)then
CYG          call hbook2(111,' snu MASS ', int(vscan(1)),vscan(2),vscan(3)
CYG     +    , int(vscan(4)),vscan(5),vscan(6),0.)
CYG
CYGc
CYGc CR
CYGc
CYG          call hbook2(200,' cs snu ', int(vscan(1)),vscan(2),vscan(3),
CYG     +    int(vscan(4)),vscan(5),vscan(6),0.)
CYG
CYGc
CYGc BR
CYGc
CYG          call hbook2(301,' snu BR to x01 ', int(vscan(1)),vscan(2),
CYG     +    vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG          call hbook2(302,' snu BR to x02  ', int(vscan(1)),vscan(2),
CYG     +    vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG          call hbook2(303,' snu BR to X+1  ', int(vscan(1)),vscan(2),
CYG     +    vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG
CYG        endif
CYG
CYG        if(sele)then
CYG
CYG          call hbook2(112,' SEL MASS ', int(vscan(1)),vscan(2),vscan(3)
CYG     +    , int(vscan(4)),vscan(5),vscan(6),0.)
CYG
CYG          call hbook2(113,' SER MASS ', int(vscan(1)),vscan(2),vscan(3)
CYG     +    , int(vscan(4)),vscan(5),vscan(6),0.)
CYG
CYG          call hbook2(201,' cs sel ', int(vscan(1)),vscan(2),vscan(3),
CYG     +    int(vscan(4)),vscan(5),vscan(6),0.)
CYG          call hbook2(202,' cs ser ', int(vscan(1)),vscan(2),vscan(3),
CYG     +    int(vscan(4)),vscan(5),vscan(6),0.)
CYG          call hbook2(203,'cs sel-er', int(vscan(1)),vscan(2),vscan(3),
CYG     +    int(vscan(4)),vscan(5),vscan(6),0.)
CYG
CYG          call hbook2(311,' sel BR to x01 ', int(vscan(1)),vscan(2),
CYG     +    vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG          call hbook2(312,' sel BR to x02  ', int(vscan(1)),vscan(2),
CYG     +    vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG          call hbook2(313,' sel BR to X+1  ', int(vscan(1)),vscan(2),
CYG     +    vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG
CYG          call hbook2(321,' ser BR to x01 ', int(vscan(1)),vscan(2),
CYG     +    vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG          call hbook2(322,' ser BR to x02  ', int(vscan(1)),vscan(2),
CYG     +    vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG          call hbook2(323,' ser BR to X+1  ', int(vscan(1)),vscan(2),
CYG     +    vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG
CYG        endif
CYG
CYG        if(sbota)then
CYG          call hbook2(121,' cs botl ', int(vscan(1)),vscan(2),vscan(3),
CYG     +    int(vscan(4)),vscan(5),vscan(6),0.)
CYG          call hbook2(122,' cs botr ', int(vscan(1)),vscan(2),vscan(3),
CYG     +    int(vscan(4)),vscan(5),vscan(6),0.)
CYG
CYG          call hbook2(114,' SBOTL MASS ', int(vscan(1)),vscan(2),
CYG     +    vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG
CYG          call hbook2(115,' SBOTR MASS ', int(vscan(1)),vscan(2),
CYG     +    vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG
CYG          call hbook2(331,' sbotl BR to x01 ', int(vscan(1)),vscan(2),
CYG     +    vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG          call hbook2(332,' sbotl BR to x02  ', int(vscan(1)),vscan(2),
CYG     +    vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG          call hbook2(333,' sbotl BR to X+1  ', int(vscan(1)),vscan(2),
CYG     +    vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG
CYG          call hbook2(341,' sbotr BR to x01 ', int(vscan(1)),vscan(2),
CYG     +    vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG          call hbook2(342,' sbotr BR to x02  ', int(vscan(1)),vscan(2),
CYG     +    vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG          call hbook2(343,' sbotr BR to X+1  ', int(vscan(1)),vscan(2),
CYG     +    vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG
CYG        endif
CYG
CYG        if(stopa)then
CYG
CYG          call hbook2(123,' cs stopl ', int(vscan(1)),vscan(2),vscan(3)
CYG     +    , int(vscan(4)),vscan(5),vscan(6),0.)
CYG          call hbook2(124,' cs stopr ', int(vscan(1)),vscan(2),vscan(3)
CYG     +    , int(vscan(4)),vscan(5),vscan(6),0.)
CYG
CYG          call hbook2(116,' STOPL MASS ', int(vscan(1)),vscan(2),
CYG     +    vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG
CYG          call hbook2(117,' STOPL MASS ', int(vscan(1)),vscan(2),
CYG     +    vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG
CYG
CYG          call hbook2(351,' stopl BR to x01 ', int(vscan(1)),vscan(2),
CYG     +    vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG          call hbook2(352,' stopl BR to x02  ', int(vscan(1)),vscan(2),
CYG     +    vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG          call hbook2(353,' stopl BR to X+1  ', int(vscan(1)),vscan(2),
CYG     +    vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG
CYG          call hbook2(361,' sbtopr BR to x01 ', int(vscan(1)),vscan(2),
CYG     +    vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG          call hbook2(362,' sbtor BR to x02  ', int(vscan(1)),vscan(2),
CYG     +    vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG          call hbook2(363,' stopr BR to X+1  ', int(vscan(1)),vscan(2),
CYG     +    vscan(3), int(vscan(4)),vscan(5),vscan(6),0.)
CYG
CYG        endif
CYG
CYG      else
CYGc
CYGc no scan histos
CYGc
CYG        if(zino)then
CYG          call hbook1(1001,' costh x01x01 ',20,-1.,1.,0.)
CYG          call hbook1(1002,' costh x01x02 ',20,-1.,1.,0.)
CYG          call hbook1(1003,' costh x01x03 ',20,-1.,1.,0.)
CYG          call hbook1(1004,' costh x01x04 ',20,-1.,1.,0.)
CYG          call hbook1(1005,' costh x02x02 ',20,-1.,1.,0.)
CYG          call hbook1(1006,' costh x02x03 ',20,-1.,1.,0.)
CYG          call hbook1(1007,' costh x02x04 ',20,-1.,1.,0.)
CYG          call hbook1(1008,' costh x03x03 ',20,-1.,1.,0.)
CYG          call hbook1(1009,' costh x03x04 ',20,-1.,1.,0.)
CYG          call hbook1(1010,' costh x04x04 ',20,-1.,1.,0.)
CYG
CYG        endif
CYG
CYG        if(wino)then
CYG          call hbook1(1011,' costh x+1x-1 ',20,-1.,1.,0.)
CYG          call hbook1(1012,' costh x+1x-2 ',20,-1.,1.,0.)
CYG          call hbook1(1013,' costh x+2x-2 ',20,-1.,1.,0.)
CYG
CYG        endif
CYG
CYG        if(snu)then
CYG          call hbook1(2001,' costh snue ',20,-1.,1.,0.)
CYG          call hbook1(2002,' costh snu ',20,-1.,1.,0.)
CYG
CYG        endif
CYG
CYG        if(sele)then
CYG          call hbook1(2003,' costh sel ',20,-1.,1.,0.)
CYG          call hbook1(2004,' costh ser ',20,-1.,1.,0.)
CYG          call hbook1(2005,' costh selr ',20,-1.,1.,0.)
CYG
CYG        endif
CYG
CYG        if(smuo)then
CYG          call hbook1(2006,' costh smul ',20,-1.,1.,0.)
CYG          call hbook1(2007,' costh smur ',20,-1.,1.,0.)
CYG
CYG        endif
CYG
CYG        if(stau)then
CYG          call hbook1(2008,' costh staul ',20,-1.,1.,0.)
CYG          call hbook1(2009,' costh staur ',20,-1.,1.,0.)
CYG
CYG        endif
CYG
CYG        if(sbota)then
CYG          call hbook1(2010,' costh sbotl ',20,-1.,1.,0.)
CYG          call hbook1(2011,' costh sbotr ',20,-1.,1.,0.)
CYG
CYG        endif
CYG
CYG        if(stopa)then
CYG          call hbook1(2012,' costh stopl ',20,-1.,1.,0.)
CYG          call hbook1(2013,' costh stopr ',20,-1.,1.,0.)
CYG
CYG        endif
CYG
CYG        if(squa)then
CYG          call hbook1(2014,' costh squa ',20,-1.,1.,0.)
CYG          call hbook1(2015,' costh squa ',20,-1.,1.,0.)
CYG
CYG        endif
CYG
CYG        CALL hbook1(1101,' amp/apro ',100,0.,2.,0.)
CYG        CALL hbook1(1111,' ntries ',100,0.,1000.,0.)
CYG
CYG      endif
CYG      return
CYG      end
CPM*CMZ :  1.00/00 14/04/95  18.40.00  by  Stavros Katsanevas
CPM*-- Author :    Stavros Katsanevas   14/04/95
CPM      double precision FUNCTION wsc(sb)
CPM      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
CPM
CPM      COMMON/SDECAY/DAS,DBS,DCS,DATL,DAUL,DATUL,DASTL,DBSTL,DASUL,
CPM     +DBSUL,DATR,DAUR,DATUR,DASTR,DBSTR,DASUR,DBSUR,xdec(17,64)
CPM
CPM      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
CPM     +FLC(12),FRC(12),gms(12),echar(12)
CPM
CPM      common/variables/fms,fmi,fmk,fml1,fml2,etai,etak,brspa(6,48),
CPM     +lind(6,6,6),brgaug(23,6,6),fmelt,fmert,fmelu,fmeru
CPM
CPM      common/widths/gw,widfl(12),widfr(12),fmeltw,fmertw,fmeluw,fmeruw,
CPM     +gent(50,2,168),gentl(2,2,168),linda(18,6,6)
CPM      common/mass/sfmi,sfmk,sfms,sgw,sfmelt,sfmelu,alt,alu,c0
CPM
CPM      wsc=0.
CPM
CPM      sfmi=fmi
CPM      sfmk=fmk
CPM      sfms=fms
CPM      sgw=gw
CPM
CPM      c0=fmi**2+fmk**2+fml1**2+fml2**2
CPMc
CPMc t sneutrino
CPMc
CPM      GW1 = ((SB+FML1**2-FML2**2)/(2.*DSQRT(SB))+
CPM     +        (FMI**2-FMK**2-SB) /(2.*DSQRT(SB)))**2
CPM      GW2 = ((SB+FML1**2-FML2**2)**2/(4.*SB)-FML1**2)
CPM      GW3 = ((FMI**2-SB-FMK**2)**2/(4.*SB)-FMK**2)
CPM      if(GW2.le.0..or.GW3.le.0.)return
CPM      GW2 = DSQRT(GW2)
CPM      GW3 = DSQRT(GW3)
CPM      TMIN = GW1-(GW2+GW3)**2
CPM      TMAX = GW1-(GW2-GW3)**2
CPMc
CPMc  u= charged slepton
CPMc
CPM      GW1 = ((SB+FML2**2-FML1**2)/(2.*DSQRT(SB))+
CPM     +        (FMI**2-FMK**2-SB) /(2.*DSQRT(SB)))**2
CPM      GW2 = ((SB+FML2**2-FML1**2)**2/(4.*SB)-FML2**2)
CPM      GW3 = ((FMI**2-SB-FMK**2)**2/(4.*SB)-FMK**2)
CPM      if(GW2.le.0..or.GW3.le.0.)return
CPM      GW2 = DSQRT(GW2)
CPM      GW3 = DSQRT(GW3)
CPM      UMIN = GW1-(GW2+GW3)**2
CPM      UMAX = GW1-(GW2-GW3)**2
CPM
CPMc
CPMc s channel integrals
CPMc
CPM
CPM      f1=ff1(sb,tmax)-ff1(sb,tmin)
CPM      f2=ff1(sb,umax)-ff1(sb,umin)
CPM      f3=ff2(sb,tmax)-ff2(sb,tmin)
CPM
CPM      wsc1=das*f1+dbs*f2+2.*dcs*etai*etak*fmi*fmk*f3
CPMc
CPMc t channel
CPMc
CPM
CPM      sfmelt=fmelt
CPM      alt=fmeltw
CPM      f4l=ff3(sb,tmax)-ff3(sb,tmin)
CPM
CPM      sfmelt=fmert
CPM      alt=fmertw
CPM      f4r=ff3(sb,tmax)-ff3(sb,tmin)
CPM
CPM      wsc2=datl*f4l+datr*f4r
CPMc
CPMc u channel
CPMc
CPM      sfmelt=fmelu
CPM      alt=fmeluw
CPM      f5l=ff3(sb,umax)-ff3(sb,umin)
CPM
CPM      sfmelt=fmeru
CPM      alt=fmeruw
CPM      f5r=ff3(sb,umax)-ff3(sb,umin)
CPM
CPM      wsc3=daul*f5l+daur*f5r
CPM
CPMc
CPMc st channel
CPMc
CPM
CPM      sfmelt=fmelt
CPM      alt=fmeltw
CPM      f4l=ff3(sb,tmax)-ff3(sb,tmin)
CPM      f6l=ff4(sb,tmax)-ff4(sb,tmin)
CPM      f7l=ff5(sb,tmax)-ff5(sb,tmin)
CPM
CPM      sfmelt=fmert
CPM      alt=fmertw
CPM      f4r=ff3(sb,tmax)-ff3(sb,tmin)
CPM      f6r=ff4(sb,tmax)-ff4(sb,tmin)
CPM      f7r=ff5(sb,tmax)-ff5(sb,tmin)
CPM
CPM      wsc4=2.*dastl*f7l+dastl*gw*f4l/((sb-fms**2)**2+gw**2)
CPM     +    +2.*dbstl*etai*etak*fmi*fmk*sb*f6l
CPM     +    +2.*dastr*f7r+dastr*gw*f4r/((sb-fms**2)**2+gw**2)
CPM     +    +2.*dbstr*etai*etak*fmi*fmk*sb*f6r
CPMc
CPMc su channel
CPMc
CPM
CPM      sfmelt=fmelu
CPM      alt=fmeluw
CPM      f5l=ff3(sb,umax)-ff3(sb,umin)
CPM      f8l=ff4(sb,umax)-ff4(sb,umin)
CPM      f9l=ff5(sb,umax)-ff5(sb,umin)
CPM
CPM      sfmelt=fmeru
CPM      alt=fmeruw
CPM      f5r=ff3(sb,umax)-ff3(sb,umin)
CPM      f8r=ff4(sb,umax)-ff4(sb,umin)
CPM      f9r=ff5(sb,umax)-ff5(sb,umin)
CPM
CPM      wsc5=2.*dasul*f9l+dasul*gw*f5l/((sb-fms**2)**2+gw**2)
CPM     +    +2.*dbsul*etai*etak*fmi*fmk*sb*f8l
CPM     +    +2.*dasur*f9r+dasur*gw*f5r/((sb-fms**2)**2+gw**2)
CPM     +    +2.*dbsur*etai*etak*fmi*fmk*sb*f8r
CPM
CPM
CPMc
CPMc tu channel integrals
CPMc
CPM      sfmelt=fmelt
CPM      alt=fmeltw
CPM      sfmelu=fmelu
CPM      alu=fmeluw
CPM      altl=alt
CPM      alul=alu
CPM      f01l=ff6(sb,tmax)-ff6(sb,tmin)
CPM      f02l=ff7(sb,tmax)-ff7(sb,tmin)
CPM
CPM      sfmelt=fmert
CPM      alt=fmertw
CPM      sfmelu=fmeru
CPM      alu=fmeruw
CPM      altr=alt
CPM      alur=alu
CPM      f01r=ff6(sb,tmax)-ff6(sb,tmin)
CPM      f02r=ff7(sb,tmax)-ff7(sb,tmin)
CPM
CPM      wsc6=2.*datul*etai*etak*fmi*fmk*sb*(altl*alul*f01l+f02l)
CPM     +    +2.*datur*etai*etak*fmi*fmk*sb*(altr*alur*f01r+f02r)
CPM
CPM      wsc=wsc1+wsc2+wsc3+wsc4+wsc5+wsc6
CPM      wsc=wsc*alpha**2/32./pi/sinw**4/fmi**3
CPM
CPM      RETURN
CPM      END
*CMZ :  1.00/00 14/04/95  18.40.00  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      double precision FUNCTION FF1(s,t)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      common/mass/fmi,fmk,fms,gw,fmelt,fmelu,alt,alu,c0
c------corec 11-5-95--------------------------------------------------
      if(alt.eq.0.)alt=0.000001
c-------------------------------------------------------------------
      fmikt=(2*t**2+6*fmi**2*fmk**2-3*(fmi**2+fmk**2)*t)
      alfms=(s-fms**2)**2+gw**2
      ff1=-t*fmikt/6./alfms
      return
      end
*CMZ :  1.00/00 14/04/95  18.40.00  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      double precision FUNCTION FF2(s,t)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      common/mass/fmi,fmk,fms,gw,fmelt,fmelu,alt,alu,c0
c------corec 11-5-95--------------------------------------------------
      if(alt.eq.0.)alt=0.000001
c-------------------------------------------------------------------
      alfms=(s-fms**2)**2+gw**2
      ff2=s*t/alfms
      return
      end
*CMZ :  1.00/00 14/04/95  18.40.00  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95

      double precision FUNCTION FF3(s,t)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      common/mass/fmi,fmk,fms,gw,fmelt,fmelu,alt,alu,c0
c------corec 11-5-95--------------------------------------------------
      if(alt.eq.0.)alt=0.000001
c-------------------------------------------------------------------
      att=datan((t-fmelt**2)/alt)
      algt=dlog((t-fmelt**2)**2+alt**2)
      dikl=(fmelt**2-fmk**2)*(fmi**2-fmelt**2)
      eikl=(fmk**2+fmi**2-2.*fmelt**2)
      ff3=(-2.*alt*t+2.*att*(alt**2+dikl)+alt*algt*eikl)/(2.*alt)
      return
      end
*CMZ :  1.00/00 14/04/95  18.40.00  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      double precision FUNCTION FF4(s,t)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      common/mass/fmi,fmk,fms,gw,fmelt,fmelu,alt,alu,c0
c------corec 11-5-95--------------------------------------------------
      if(alt.eq.0.)alt=0.000001
c-------------------------------------------------------------------
      attr=datan(alt/(t-fmelt**2))
      algt=dlog((t-fmelt**2)**2+alt**2)
      alfms=(s-fms**2)**2+gw**2
      bb1=-2.*gw*attr+(s-fms**2)*algt
      ff4=bb1/2./alfms
      return
      end
*CMZ :  1.00/00 14/04/95  18.40.00  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      double precision FUNCTION FF5(s,t)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      common/mass/fmi,fmk,fms,gw,fmelt,fmelu,alt,alu,c0
c------corec 11-5-95--------------------------------------------------
      if(alt.eq.0.)alt=0.000001
c-----------------------------------------------------------------
      att=datan((t-fmelt**2)/alt)
      algt=dlog((t-fmelt**2)**2+alt**2)
      dikl=(fmelt**2-fmk**2)*(fmi**2-fmelt**2)
      eikl=(fmk**2+fmi**2-fmelt**2)
      alfms=(s-fms**2)**2+gw**2
      bb2=2.*eikl*t-t**2-2.*alt*(fmk**2+fmi**2-2*fmelt**2)*att
     +    +(alt**2+dikl)*algt
      ff5=(s-fms**2)*bb2/2./alfms
      return
      end
*CMZ :  1.00/00 14/04/95  18.40.00  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      double precision FUNCTION FF6(s,t)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      common/mass/fmi,fmk,fms,gw,fmelt,fmelu,alt,alu,c0
c------corec 11-5-95--------------------------------------------------
      if(alt.eq.0.)alt=0.000001
      if(alu.eq.0.)alu=0.000001
c--------------------------------------------------------------------
      att=datan((t-fmelt**2)/alt)
      atu=datan((t+fmelu**2-c0+s)/alu)
      algt=dlog((t-fmelt**2)**2+alt**2)
      algu=dlog((t-(c0-s-fmelu**2))**2+alu**2)
      dtu=c0-s-fmelt**2-fmelu**2
      etu1=(fmelt**2+fmelu**2)**2-2*(c0-s)*(fmelt**2+fmelu**2)
     +    +((c0-s)**2+alu**2-alt**2)
      etu2=(fmelt**2+fmelu**2)**2-2*(c0-s)*(fmelt**2+fmelu**2)
     +    +((c0-s)**2+alt**2-alu**2)
      ftu1=(fmelt**2+fmelu**2)**2+(alt-alu)**2
     +    +(c0-s)*(c0-s-2*(fmelt**2+fmelu**2))
      ftu2=(fmelt**2+fmelu**2)**2+(alt+alu)**2
     +    +(c0-s)*(c0-s-2*(fmelt**2+fmelu**2))
      ff6=(alu*att*etu1+alt*atu*etu2+alu*alt*dtu
     +   *(algt-algu))/alu/alt/ftu1/ftu2
      return
      end
*CMZ :  1.00/00 14/04/95  18.40.00  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      double precision FUNCTION FF7(s,t)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      common/mass/fmi,fmk,fms,gw,fmelt,fmelu,alt,alu,c0
c------corec 11-5-95--------------------------------------------------
      if(alt.eq.0.)alt=0.000001
      if(alu.eq.0.)alu=0.000001
c----------------------------------------------------------------------
      att=datan((t-fmelt**2)/alt)
      atu=datan((t+fmelu**2-c0+s)/alu)
      algt=dlog((t-fmelt**2)**2+alt**2)
      algu=dlog((t-(c0-s-fmelu**2))**2+alu**2)
      etu1=(fmelt**2+fmelu**2)**2-2*(c0-s)*(fmelt**2+fmelu**2)
     +    +((c0-s)**2+alt**2-alu**2)
      etu2=(fmelt**2+fmelu**2)**2-2*(c0-s)*(fmelt**2+fmelu**2)
     +    +((c0-s)**2+alu**2-alt**2)
      ftu1=(fmelt**2+fmelu**2)**2+(alt-alu)**2
     +    +(c0-s)*(c0-s-2*(fmelt**2+fmelu**2))
      ftu2=(fmelt**2+fmelu**2)**2+(alt+alu)**2
     +    +(c0-s)*(c0-s-2*(fmelt**2+fmelu**2))
      gtu=(alt**2+alu**2)*(c0-s-fmelt**2-fmelu**2)
     +   -3.*(fmelt**2+fmelu**2)*((c0-s)**2+fmelt**2*fmelu**2)
     +   +3.*(c0-s)*(fmelt**2+fmelu**2)**2-(fmelt**6+fmelu**6)+(c0-s)**3
      ff7=(-2.*alt*etu1*att-2.*alu*etu2*atu
     +   +gtu*(algt-algu))/2./ftu1/ftu2
      return
      end
*CMZ :  1.00/00 14/04/95  19.00.47  by  Unknown
*-- Author :    Stavros Katsanevas   14/04/95
      SUBROUTINE INBOOK(NOW,FF,PDX,IGRAPH)
C   08/02/85 602032337  MEMBER NAME  INBOOK   (S)           FORTRAN
C ===================================================================
C
C     INTERFACE ROUTINE BETWEEN VEGAS AND HBOOK.
C                                M.MARTINEZ  DESY-85
C
C
C ===================================================================
C
C A)  NOW   =/=2 NORMAL BOOKKEPING
C           =  2 STOP  (TO PRINT THE HISTOGRAMS YOU MUST CALL HISTDO
C                  IN THE MAIN PROGRAM)
C
C     FF    = FUNCTION VALUE (CORRECTED BY THE SIZE OF THE
C                  INTEGRATION INTERVAL)
C
C     PDX   = SIZE OF THE INTEGRATION INTERVAL
C
C     IGRAPH=1  ONLY STATISTICS ABOUT THE INTEGRATION
C           =10 ONLY HISTOGRAMS BUT NOT STATISTICS
C
C
C
C B)  READING FORMAT FOR THE HISTOGRAM INFORMATION:
C
C   /         READ 550,NLS
C   /   550   FORMAT(I2)
C   /         READ 570,SMI(I),SMA(I),NLP(I),LDUM,LL,(LEXT(J),J=1,8)
C   /   570   FORMAT(2E12.4,3I2,8A4)
C
C
C         NLS  = # OF HISTOGRAMS  (<=10)
C
C         SMI  = MINIMUM VALUE
C
C         SMA  = MAXIMUM VALUE
C
C         NLP  = # BINS
C
C         LDUM = HISTOGRAM INFORMATION CHOICES
C              IF( | LDUMM | > 10 ) --> USUAL INFORMATION
C              IF( | LDUMM | < 10 AND ...
C                    LDUMM  > -1  --> INTEGRATED INFORMATION
C                    LDUMM  <  1  --> ERROR INFORMATION
C
C         LL   = HISTOGRAM SCALE CHOICES
C              IF( LL > 0 ) --> LINEAL SCALE
C              IF( LL < 0 ) --> LOGARITMIC SCALE
C
C         LEXT = TEXT
C
C =================================================================
C
C.... A) INBOOK ---> BOOKING THE HISTOGRAMS  =========================
C
      IMPLICIT double precision(A-G,O-Z)
      REAL*4 XX,YY,ZSIG,EIT,EIA
      COMMON/PAWC/HMEMOR(30000)
      COMMON/RESULT1/CHI2A,Y,SI,U,V
      COMMON/INPARM1/ALPH
      COMMON/RESULT2/IT0
      COMMON/INPARM/ITMX0

      COMMON /LPLOT/XL(10)
      DIMENSION NLP(10),SMA(10),SMI(10),DEL(10),ZSIG(50)
      DIMENSION NLSN(50,10),MLSN(50,10),XLS(50,10),YLS(50,10)
      DIMENSION XLSS(50,10),YLSS(50,10),EIT(100),EIA(100)
CC
      CALL HLIMIT(30000)

      IF(IGRAPH.EQ.10)GO TO 10
      ZIT=ITMX0+1.
      CALL HBOOK1(777,'PARTIAL INTEGRAL EVOLUTION',ITMX0,1.,ZIT,0.)
      CALL HBOOK1(778,'ACCUMULATED INTEGRAL EVOLUTION',ITMX0,1.,ZIT,0.)
      CALL HBOOK1(779,'CHI**2 EVOLUTION',ITMX0,1.,ZIT,0.)
      IF(IGRAPH.EQ.1)RETURN
   10 CONTINUE

      NLS=10
      DO 20  I=1,NLS
        II=10*I
        NLP(I)=40
        SMI(I)=0.
        SMA(I)=1.
        CALL HBOOK1(II,' ',NLP(I),SMI(I),SMA(I),0.)
        DEL(I)=(SMA(I)-SMI(I))/NLP(I)
   20 CONTINUE
      RETURN
C
C.... B) REBOOK --> RESETING STORAGE MATRICES AFTER EACH ITERATION ===
C
      ENTRY REBOOK(NOW,FF,PDX,IGRAPH)
      IF(IGRAPH.EQ.1)RETURN
      DO 30  I=1,NLS
        NLPS=NLP(I)+2
        DO 30  J=1,NLPS
          XLS(J,I)=0.
          XLSS(J,I)=0.
          NLSN(J,I)=0
   30 CONTINUE
      RETURN
C
C.... C) XBOOK ---> FILLING STORAGE MATRICES =========================
C
      ENTRY XBOOK(NOW,FF,PDX,IGRAPH)
      IF(IGRAPH.EQ.1)RETURN
      IF(FF.LT.0.D0)PRINT 10000
10000 FORMAT('    WARNING !!!!  ======  FUN < 0 ')
      DO 50  I=1,NLS
        NLPS=(XL(I)-SMI(I))/DEL(I)+1.
        IF(NLPS.LT.0)NLPS=0
        IF(NLPS.GT.NLP(I))NLPS=NLP(I)+1
        NLPS=NLPS+1
        FUN=FF/DEL(I)
        XLS(NLPS,I)=XLS(NLPS,I)+FUN
        IF(PDX.EQ.0.D0)GO TO 40
        XLSS(NLPS,I)=XLSS(NLPS,I)+FUN*FUN/PDX
   40   NLSN(NLPS,I)=NLSN(NLPS,I)+1
   50 CONTINUE
      RETURN
C
C.... D1) BOOKIT ---> ACCUMULATING ITERATIONS INFORMATION ============
C
      ENTRY BOOKIT(NOW,FF,PDX,IGRAPH)
      IF(IGRAPH.EQ.10)GO TO 60
      ZIT0=1.*IT0
      EIT(IT0)=V
      CALL HF1(777,ZIT0,U)
      EIA(IT0)=SI
      CALL HF1(778,ZIT0,Y)
      CALL HF1(779,ZIT0,CHI2A)
      IF(IGRAPH.EQ.1.AND.NOW.NE.2)RETURN
      IF(IGRAPH.EQ.1.AND.NOW.EQ.2)GOTO 160
C
   60 CONTINUE
      DO 70  I=1,NLS
        NLPS=NLP(I)+2
        DO 70  J=1,NLPS
          DEN=XLSS(J,I)-XLS(J,I)*XLS(J,I)
          IF(DEN.GT.1.D20)DEN=1.D20
          IF(DEN.LT.1.D-20)DEN=1.D-20
          XLSS(J,I)=0.
          IF(DEN.NE.0.D0)XLSS(J,I)=1./DEN
   70 CONTINUE
C
      IF(KK.GT.0)GO TO 90
      DO 80  I=1,NLS
        NLPS=NLP(I)+2
        DO 80  J=1,NLPS
          MLSN(J,I)=NLSN(J,I)
          YLS(J,I)=XLS(J,I)
          YLSS(J,I)=XLSS(J,I)
   80 CONTINUE
      GO TO 120
   90 VBEF=VTOT
      VU=(V/U)**2
      BE1=U*U
      BE2=Y*Y
      DO 110 I=1,NLS
        NLPS=NLP(I)+2
        DO 110 J=1,NLPS
          IF(NLSN(J,I).EQ.0) GO TO 110
          IF(MLSN(J,I).EQ.0) GO TO 100
          AL1=VU/NLSN(J,I)
          AL2=VBEF/MLSN(J,I)
          MLSN(J,I)=MLSN(J,I)+NLSN(J,I)
          YLS(J,I)=(AL2*XLS(J,I)+AL1*YLS(J,I))/(AL1+AL2)
C     YLSS(J,I)=(BE2*YLSS(J,I)+BE1*XLSS(J,I))/(BE2+BE1)
          YLSS(J,I)=YLSS(J,I)+XLSS(J,I)
          GO TO 110
  100     MLSN(J,I)=NLSN(J,I)
          YLS(J,I)=XLS(J,I)
          YLSS(J,I)=XLSS(J,I)
  110 CONTINUE
  120 CONTINUE
      VTOT=(SI/Y)**2
CC
      KK=KK+1
      IF(NOW.NE.2)RETURN
CC
C
C.... D2) BOOKIT ---> FILLING AND PRINTING HISTOGRAMS ================
C
      DO 150 I=1,NLS
        NLPS=NLP(I)+2
        NHI=10*I
        DO 140 J=1,NLPS
          XX=SNGL(SMI(I)+DEL(I)*(J-1.5))
          YY=SNGL(YLS(J,I))
          CALL HF1(NHI,XX,YY)
          JJ=J-1
          IF(JJ.EQ.0.OR.JJ.GT.NLP(I))GO TO 130
          ZSIG(JJ)=0.
          IF(MLSN(J,I).NE.0.AND.YLSS(J,I).NE.0.D0) ZSIG(JJ)=SNGL(1./
     +    DSQRT(YLSS(J,I)*MLSN(J,I)))
  130     CONTINUE
  140   CONTINUE
        CALL HPAKE(NHI,ZSIG)
  150 CONTINUE
      IF(IGRAPH.EQ.10)GOTO 170
C
  160 CALL HPAKE(777,EIT)
      CALL HPAKE(778,EIA)
  170 CONTINUE

      RETURN
      END
*CMZ :  1.00/00 14/04/95  18.46.28  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      SUBROUTINE VEGAS(FXN,ACC,NDIM,NCALL,ITMX,NPRN,IGRAPH,sgv,sdd,itt)
C   01/12/84 602042308  MEMBER NAME  VEGAS    (S)           FORTRAN
C ===================================================================
C
C     VEGAS VERSION INCLUDING HBOOK CALLS POSSIBILITY,
C     SIMPLIFIED BUT MORE CLEAR AND EQUALLY EFFICIENT.
C
C                              M.MARTINEZ  DESY-85
C ===================================================================
C
C     FXN   = FUNCTION TO BE INTEGRATED/MAPPED
C     ACC   = RELATIVE ACCURACY REQUESTED
C     NDIM  = # DIMENSIONS
C     NCALL = MAXIMUM TOTAL # OF CALLS TO THE FUNCTION PER ITERATION
C     ITMX  = MAXIMUM # OF ITERATIONS ALLOWED
C     NPRN  = PRINTOUT LEVEL:
C             =0  ONLY FINAL RESULTS
C            >=1  ADDITIONNALLY INF. ABOUT INPUT PARAMETERS
C            >=2  ADDITIONNALLY INF. ABOUT ACCUMULATED VALUES
C                  PER ITERATION.
C            >=3  ADDITIONNALLY INF. ABOUT PARTIAL VALUES
C                  PER ITERATION.
C            >=5  ADDITIONNALLY INF. ABOUT FINAL BIN DISTRIBUTION
C                  (NUMERICAL MAPPING).
C     IGRAPH= HISTOGRAMMATION LEVEL:
C             =0  NO HISTOGRAMS AT ALL
C             =1  ONLY STATISTICS ABOUT THE INTEGRATION
C             =10 ONLY HISTOGRAMS DEFINED BUT NOT STATISTICS
C
C
C ===================================================================

      IMPLICIT double precision(A-H,O-Z)
      COMMON/RESULT1/CHI2A,Y,SI,U,V
      COMMON/INPARM1/ALPH
      COMMON/RESULT2/IT0
      COMMON/INPARM/ITMX0

      COMMON/OUTMAP/ND,NDIM0,XI(50,10)

      DIMENSION X(10),XIN(50),R(50),IA(10),D(50,10)
      DATA ALPH0/1.5/,INIT/0/

      dimension xio(50,10)
      equivalence(xi(1,1),xio(1,1))
      external FXN
      ND=50
C===========================================================
C A))  INITIALIZING SOME VARIABLES
C===========================================================
      ITMX0=ITMX
      NDIM0=NDIM
      IF(ALPH.EQ.0.)ALPH=ALPH0

      CALLS=dble(NCALL)

      XND=ND
      NDM=ND-1
      IF(IGRAPH.NE.0)CALL INBOOK(0,FUN,WEIGHT,IGRAPH)
C.............................................
C  INITIALIZING CUMMULATIVE VARIABLES
C.............................................
      IT=0
      SI=0.
      SI2=0.
      SWGT=0.
      SCHI=0.
      SCALLS=0.
      NZEROT=0
      FMAXT=0.
      FMINT=1.D20
C.............................................
C  DEFINING THE INITIAL INTERVALS DISTRIBUTION
C.............................................
      RC=1./XND
      DO 10 J=1,NDIM
        XI(ND,J)=1.
        DR=0.
        DO 10 I=1,NDM
          DR=DR+RC
          XI(I,J)=DR
   10 CONTINUE
c      IF(INIT.EQ.0)PRINT 407
      INIT=1
      IF(NPRN.GE.1)PRINT 10100,NDIM,NCALL,ITMX,ND
C===========================================================
C B))  ITERATIONS LOOP
C===========================================================
   20 IT=IT+1
C.............................................
C  INITIALIZING ITERATION VARIABLES
C.............................................
      TI=0.
      SFUN2=0.
      NZERO=0
      FMAX=0.
      FMIN=1.D20
      DO 30 J=1,NDIM
        DO 30 I=1,ND
          D(I,J)=0.
   30 CONTINUE
      IF(IGRAPH.NE.0)CALL REBOOK(0,FUN,WEIGHT,IGRAPH)
      DO 60 JJ=1,NCALL
        WGT=1.
C.............................................
C  COMPUTING THE POINT POSITION
C.............................................
        DO 40 J=1,NDIM
          XN=rndm(0)*XND+1.
          IA(J)=XN
          XIM1=0.
          IF(IA(J).GT.1)XIM1=XI(IA(J)-1,J)
          XO=XI(IA(J),J)-XIM1
          X(J)=XIM1+(XN-IA(J))*XO
          WGT=WGT*XO*XND
   40   CONTINUE
C.............................................
C  COMPUTING THE FUNCTION VALUE
C.............................................
        FUN=FXN(X)
c      if(fun.le.1.E-11)fun=0.
        IF(FMAX.LT.FUN)FMAX=FUN
        IF(FMIN.GT.FUN)FMIN=FUN
        FUN=FUN*WGT/CALLS
        IF(FUN.NE.0.)NZERO=NZERO+1
        FUN2=FUN*FUN
        WEIGHT=WGT/CALLS
        IF(IGRAPH.NE.0)CALL XBOOK(0,FUN,WEIGHT,IGRAPH)
        TI=TI+FUN
        SFUN2=SFUN2+FUN2
        DO 50 J=1,NDIM
          IAJ=IA(J)
          D(IAJ,J)=D(IAJ,J)+FUN2
   50   CONTINUE
   60 CONTINUE
C.............................................
C  COMPUTING THE INTEGRAL AND ERROR VALUES
C.............................................
      IF (SFUN2.NE.0.)GO TO 70
      PRINT 10800
      sgv=0.
      return
   70 CONTINUE
      TI2=TI*TI
      TICAL=(SFUN2*CALLS-TI2)/(CALLS-1.)
c melachroinos corrections
      TSI=0.
      IF(TICAL.GT.0)TSI=DSQRT((SFUN2*CALLS-TI2)/(CALLS-1.))
c
      WGT=TI2/TSI**2
      SI=SI+TI*WGT
      SI2=SI2+TI2
      SWGT=SWGT+WGT
      SCHI=SCHI+TI2*WGT
      SCALLS=SCALLS+CALLS
      AVGI=SI/SWGT
      SD=SWGT*IT/SI2
      CHI2A=0.
      IF(IT.GT.1)CHI2A=SD*(SCHI/SWGT-AVGI*AVGI)/(IT-1)
      SD=1./DSQRT(SD)
      ERR=SD*100./AVGI
      NZEROT=NZEROT+NZERO
      IF(FMAXT.LT.FMAX)FMAXT=FMAX
      IF(FMINT.GT.FMIN)FMINT=FMIN
      IT0=IT
C.............................................
C  PRINTING AND HISTOGRAMMING
C.............................................
      sgv=avgi

      IF(NPRN.GE.2)PRINT 10200,IT,AVGI,SD,ERR
      IF(NPRN.GE.3)PRINT 10300,TI,TSI,NZERO,FMIN,FMAX,CHI2A
      IF(NPRN.LT.5) GO TO 100
      DO 90 J=1,NDIM
        PRINT 10400,J
        XIN(1)=XI(1,J)
        DO 80   L=2,ND
   80   XIN(L)=XI(L,J)-XI(L-1,J)
   90 PRINT 10500,(XI(I,J),XIN(I),D(I,J),I=1,ND)
  100 CONTINUE
      IF(DABS(SD/AVGI).GT.DABS(ACC).AND.IT.LT.ITMX)GO TO 110
      NPIT=IT*NCALL
      EFF=NZEROT*100./NPIT
      IF(NPRN.GE.2)PRINT 10600,NPIT,NZEROT,EFF,FMINT,FMAXT
c      PRINT 777,AVGI,SD,CHI2A
      SDD=SD
      ITT=IT
      IF(IGRAPH.NE.0)CALL BOOKIT(2,FUN,WEIGHT,IGRAPH)
      RETURN
  110 CONTINUE
      IF(IGRAPH.NE.0)CALL BOOKIT(0,FUN,WEIGHT,IGRAPH)
C===========================================================
C C))  REDEFINING THE GRID
C===========================================================
C.............................................
C  SMOOTHING THE F**2 VALUED STORED FOR EACH INTERVAL
C.............................................
      DO 130 J=1,NDIM
        XO=D(1,J)
        XN=D(2,J)
        D(1,J)=(XO+XN)/2.
        X(J)=D(1,J)
        DO 120 I=2,NDM
          D(I,J)=XO+XN
          XO=XN
          XN=D(I+1,J)
          D(I,J)=(D(I,J)+XN)/3.
          X(J)=X(J)+D(I,J)
  120   CONTINUE
        D(ND,J)=(XN+XO)/2.
        X(J)=X(J)+D(ND,J)
  130 CONTINUE
C.............................................
C  COMPUTING THE 'IMPORTANCE FUNCTION' OF EACH INTERVAL
C.............................................
      DO 190 J=1,NDIM
        RC=0.
        DO 150 I=1,ND
          R(I)=0.
          IF(D(I,J).LE.0.) GO TO 140
          XO=X(J)/D(I,J)
          R(I)=((XO-1.)/XO/DLOG(XO))**ALPH
  140     RC=RC+R(I)
  150   CONTINUE
C.............................................
C  REDEFINING THE SIZE OF EACH INTERVAL
C.............................................
        RC=RC/XND
        K=0
        XN=0.
        DR=0.
        I=0
  160   K=K+1
        DR=DR+R(K)
        XO=XN
        XN=XI(K,J)
  170   IF(RC.GT.DR) GO TO 160
        I=I+1
        DR=DR-RC
        XIN(I)=XN-(XN-XO)*DR/R(K)
        IF(I.LT.NDM) GO TO 170
        DO 180 I=1,NDM
          XI(I,J)=XIN(I)
  180   CONTINUE
        XI(ND,J)=1.
  190 CONTINUE
C
      GO TO 20
C===========================================================
C D))  FORMATS FOR THE PRINTOUTS
C===========================================================
10000 FORMAT('1  %%%%  ROUTINE "VEGAS" V.1-85 FROM P.G.LEPAGE',
     + '    (MOD.BY M.MARTINEZ)'/)
10100 FORMAT('0  %%%%  INTEGRATION PARAMETERS :'/
     + '   # DIMENSIONS                =',I8/,
     + '   # CALLS TO F PER ITERATION  =',I8/,
     + '   # ITERATIONS MAXIMUM        =',I8/,
     + '   # BINS IN EACH DIMENSION    =',I8)
10200 FORMAT(//' ITER. NO',I3,' ACC.RESULTS==> INT =',G14.5,'+/-',G10.4,
     + '   % ERROR=',G10.2)
10300 FORMAT(/20X,'ITER.RESULTS=',G14.5,'+/-',G10.4,
     + '   (F=/=0)=',I6/20X,'   FMIN=',G10.4,'   FMAX=',G10.4,
     + '   CHI**2=',G10.2)
10400 FORMAT(14H0DATA FOR AXIS,I2 /
     + 7X,'X',9X,'DELT X',6X,'SIG(F2)',
     + 13X,'X',9X,'DELT X',6X,'SIG(F2)'/)
10500 FORMAT(1X,3G12.4,5X,3G12.4)
10600 FORMAT(//'  %%%% FINAL INFORMATION : IN TOTAL '/
     + '  #FUNCTION CALLS =',I6,'  #(F=/=0) =',I6,'  % =',G10.4/
     + 5X,'  FMIN=',G10.4,'   FMAX=',G10.4)
10700 FORMAT(' '//' ',30('+'),' FINAL RESULT ',30('+')//
     + ' INTEGRAL VALUE =',G14.5,'+/-',G10.4,6X,
     + ' ( CHI**2=',G10.4,')'//' ',74('+'))
10800 FORMAT(//
     + '  ## WARNING: IN "VEGAS" THE VALUE OF THE INTEGRAL'/
     + '  IS EXACTLY ZERO, SO NO EVOLUTION OF THE DENSITY '/
     + '  DISTRIBUTION CAN BE EXPECTED ---> PROGRAM STOP ')
      END
*CMZ :  1.00/00 14/04/95  18.46.28  by  Stavros Katsanevas
*-- Author :    Stavros Katsanevas   14/04/95
      subroutine GVEG(Z,xi,xlim)
C===========================================================
C   SUBROUTINE TO USE THE VEGAS-COMPUTED GRID TO
C   Generate partial distributions
C
C                                S. Katsanevas
C===========================================================
      IMPLICIT double precision(A-H,O-Z)
      DIMENSION Z(10),XI(50,2),xlim(2,2)
      real rndm
      data nd,ndim/50,2/

      DO 10 I=1,NDIM
        XJ=dble(rndm(0))*ND+1
        J=XJ
        if(j.gt.nd+1)j=nd+1
        XI0=1.0001*xlim(1,I)
        XIM=0.9999*xlim(2,i)
        IF(j.gt.1)XI0=XI(J-1,I)
        if(j.le.50)XIM=XI(j,i)
        DD=XIM-XI0
        Z(I)=XI0+DD*(XJ-J)
   10 CONTINUE
      RETURN
      END
      subroutine dummyss
CYG*CMZ :  1.00/00 14/04/95  18.46.28  by  Stavros Katsanevas
CYG*-- Author :    Stavros Katsanevas   14/04/95
CYGC************************************************************************
CYG      block data susygdat
CYG
CYG      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
CYG
CYGc
CYGc reorder sparticles and particles
CYGc
CYG
CYG      common/reorder/ispa(12,2),kl(2,18),klap(2,18),idecs(12,2)
CYG
CYG
CYG      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
CYG     +FLC(12),FRC(12),gms(12),echar(12)
CYG
CYG
CYG
CYG       common/spartcl/fmal(12),fmar(12),ratq(12),fgamc(12),fgamcr(12)
CYG     +,cosmi(12)
CYG
CYG
CYG       COMMON/SFLOOP/ILOOP
CYGc
CYGc reorder sparticles and particles
CYGc
CYGc      PARAMETER (IDUP=2,IDDN=1,IDST=3,IDCH=4,IDBT=5,IDTP=6)
CYGc      PARAMETER (IDNE=12,IDE=11,IDNM=14,IDMU=13,IDNT=16,IDTAU=15)
CYG
CYG      data kl/2,-2,1,-1,12,-12,11,-11,2,-1,12,-11,
CYG     +        4,-4,3,-3,14,-14,13,-13,4,-3,14,-13,
CYG     +        6,-6,5,-5,16,-16,15,-15,6,-5,16,-15/
CYG
CYGc      PARAMETER (ISUPL=42,ISDNL=41,ISSTL=43,ISCHL=44,ISBTL=45,ISTPL=46)
CYGc      PARAMETER (ISNEL=52,ISEL=51,ISNML=54,ISMUL=53,ISNTL=56,ISTAUL=55)
CYGc      PARAMETER (ISUPR=48,ISDNR=47,ISSTR=49,ISCHR=50,ISBTR=61,ISTPR=62)
CYGc      PARAMETER (ISER=57,ISMUR=58,ISTAUR=59)
CYG
CYG      data ispa/42,41,52,51,44,43,54,53,46,45,56,55,
CYG     +          48,47, 0,57,50,49, 0,58,62,61, 0,59/
CYG
CYG
CYG
CYG      data klap/42,42,41,41,52,52,51,51,42,41,52,51,
CYG     +          44,44,43,43,54,54,53,53,44,43,54,53,
CYG     +          46,46,45,45,56,56,55,55,46,45,56,55/
CYG
CYG      data idecs/2,1,12,11,4,3,14,13,6,5,16,15,
CYG     +           1,2,11,12,3,4,13,14,5,6,15,16/
CYG
CYGC
CYGC STANDARD PARTICLE MASSES
CYGC
CYG
CYG      DATA GMS/.0099d0,.0056d0,0.d0,.0005d0,1.35d0,
CYG     +         .199d0,0.d0,.105d0,174.d0,5.d0,0.d0,1.777d0/
CYG
CYG      data fmal/12*1.d0/
CYG      data fmar/12*1.d0/
CYG      data iloop/1/
CYG
CYG
      end
      subroutine sucalc(ifail)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/MSSM/TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI
      common/param/fch1
      external sucal
      logical err
      fch1=fmgaug
      ifail=0
      call DMINFC(sucal,0.0d0,1000.d0,0.00000001d0,0.0000001d0,x,y,err)
      write(*,*) 'Best M of approach and corersponding diff ',x
      If(abs(y).gt.1.)then
      print *,' NO such mass possible for this mu/tanb value '
      ifail=1
      endif

      return
      end

      
      double precision function sucal(x)

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
     +FLC(12),FRC(12),gms(12),echar(12)
      COMMON/MSSM/TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI

      real*4 rscale
      common/sscale/rscale

      common/param/fch1

      real*4 zr,was
      dimension ZR(4,4),was(4)
      REAL*4 FMA(4,4),WR(4),work(16)
      dimension fm(2)
      data ifirst/0/
c
c constants
c
      if(ifirst.eq.0)then
      ifirst=1
      sin2beta=2.*sinb*cosb
      cos2beta=cosb**2-sinb**2
      endif

      sucal=9999.
      IN=1

      fmgaug=x

      if(in.eq.1)then
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C neutralino part
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c

      FMGAUG1=FMGAUG*5./3.*SINW**2/COSW**2*rscale

      FMA(1,1)= FMGAUG1*COSW**2+FMGAUG*SINW**2
      FMA(2,1)= (FMGAUG-FMGAUG1)*SINW*COSW
      FMA(3,1)= 0.
      FMA(4,1)= 0.
      FMA(1,2)= (FMGAUG-FMGAUG1)*SINW*COSW
      FMA(2,2)= FMGAUG1*SINW**2+FMGAUG*COSW**2
      FMA(3,2)= FMw/cosw
      FMA(4,2)= 0.
      FMA(1,3)= 0.
      FMA(2,3)= FMw/cosw
      FMA(3,3)=   FMR*SIN2BETA
      FMA(4,3)= - FMR*COS2BETA
      FMA(1,4)= 0.
      FMA(2,4)= 0.
      FMA(3,4)= - FMR*COS2BETA
      FMA(4,4)= - FMR*SIN2BETA

      CALL EISRS1(4,4,FMA,WR,ZR,IERR,WORK)
      IF(IERR.NE.0)RETURN

      DO 10 K=1,4
        WAS(K)=ABS(WR(K))
   10 CONTINUE

C
C       Sort eigenvectors and eigenvalues according to masses
C
      DO 30 I=1,3
        DO 20 J=I+1,4
          IF (was(i).GT.was(j)) THEN
            TEMP=was(J)
            Was(J)=Was(I)
            Was(I)=TEMP
          END IF
   20   CONTINUE
   30 CONTINUE

      sucal=abs(fch1-was(1))

      else
C********************************************************
C
C chargino part
C
C********************************************************

      DELTA1=dsqrt((FMGAUG-FMR)**2+2.D0*FMW**2*(1.+sin2beta))
      DELTA2=dsqrt((FMGAUG+FMR)**2+2.D0*FMW**2*(1.-sin2beta))

      FMM1=0.5*(delta1-delta2)
      FMM2=0.5*(delta1+delta2)
      FM(1)=ABS(FMM1)
      FM(2)=ABS(FMM2)

      sucal=abs(fm(1)-was(1))

      endif

      RETURN
      END
      subroutine dummyss2
CYGcccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
CYGc
CYGc  R PARITY VIOLATION
CYGcccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
CYGC LAST UPDATE: APRIL 28TH, 1994, EXPAND COMMENTS
CYGC This program calculates the decay of a neutralino lsp decay via
CYGC the R-parity violating operators LLE, LQD, and UDD. It is based
CYGc on a tree-level calculation of the decay, E.G. L_1 L_2 E_3-OPERATOR
CYGC
CYGc (LSP) --> ELECTRON + (S-ELECTRON)* --> ELECTRON + TAU + NU-MU
CYGC
CYGC SUSY-INPUT:  TANBETA
CYGC	       XMIX(1),...,XMIX(4) THE NEUTRALINO MIXING PARAMETERS
CYGC              XMLSP               THE LSP MASS IN GeV
CYGC              XMSCAL....          THE S-FERMION MASSES
CYGC
CYGC I have followed the notation of Gunion and haber Nucl Phys B 272
CYGc (1986)1. In particular I have chosen the basis Eq. (A.15) not
CYGc (A.17), so the bino wino basis not the photino, zino basis.
CYGC For the scalar fermions I have assumed left-right degeneracy.
CYGC
CYGc R-PARITY VIOLATION
CYGC The choice of the indices I(1),...,I(4) completely determines the
CYGc R-parity violating operator structure. It is assumed that only one
CYGc operator contributes to the decay.
CYGc I(1) determines which is the dominant operator:
CYGc I(1)=1 --> LLE
CYGc     =2 --> LQD
CYGc     =3 --> UDD
CYGc
CYGc I(2)-I(4) determine the generation indices. So for I=(2,1,2,3)
CYGc the dominant operator is [L_1 Q_2 D_3].
CYGc
CYGc MASSES
CYGc This program includes all fermion masses. They enter (a) in the final state
CYGc phase space, (b) in the matrix element via (i) the higgsino couplings,
CYGc (ii) dirac spinors. These are all included. By choosing the indices
CYGc I(1),...,I(4) the proper masses are determined in the subroutine XMASS.
CYGc
CYGc COUPLINGS
CYGC The couplings are calculated in the subroutine COUPL. The full
CYGc neutralino couplings are determined, including the higgsino couplings 
CYGc proportional to mass (this was included mainly for cosmological
CYGc calculations).
CYGc
CYGc
CYGc As output this program gives the total decay rate of of the LSP: RES.
CYGc Since it is a 3 body decay this program includes a 2-dim phase space integration, 
CYGc which is done with 2 nested 1-dim intergrations TINT, one in the main
CYGc program, the other in F1 or F2. The matrix elements squared have been
CYGc calculated analytically and are given in FM1--FM5. Before using this for
CYGc a differential cross section computation several points must be noted.
CYGc 
CYGc     The matrix element squared is given in the notation of the text book
CYGc     BARGER AND PHILLIPS: COLLIDER PHYSICS. In particular the notation
CYGc     of the 3-particle phase space is that of Appendix B.2.2, and here
CYGc     it is the second formulation, at the bottom of page 567. The important 
CYGc     point is that all the final-state masses squared as well as the
CYGc     energy are normalized to the LSP-mass. The differential decay rate is then
CYGc     given towards the bottom of page 568. (Note a typo pi**3 instead of
CYGc     pi**2.) In the program the prefactor is denoted FAC. Thus the simple
CYGc     matrix element squared is given by e.g. FM1* 8. * XLAM**2 * G**2, but
CYGc     now in terms of the normalized masses and energies of the final 
CYGc     state particles. (mass-norm)**2 = mass**2/mass(lsp)**2
CYGc     (energy-norm)=2*energy/mass(lsp).
CYGc
CYGC
CYG        subroutine rparini
CYG
CYG	IMPLICIT REAL*8 (A-H,O-Z)
CYG	REAL*8 XL1,XU1,XL2,XU2,RES1,RES2,RES,XMLSP,XMSCALDOWNL(3) 
CYG	REAL*8 TANTHW,XMIX(4),COSB,SINB,SIN2THW,XMU(6)
CYG	REAL*8 XM(6),XMDOWN(3),XMUP(3),XMELEC(3),XMW,XMSCALUPL(3) 
CYG	REAL*8 XMTILDE(6),XMSCALELECL(3),XMSCALNEUT(3)
CYG        REAL*8 XMSCALELECR(3),XMSCALDOWNR(3),XMSCALUPR(3)
CYG	REAL*8 A(6),B(6),XMUTILDE(6),F1,F2
CYG	COMMON/SUSY/TANTHW,XMIX,COSBc,SINBc
CYG	COMMON/MASS1/XMELEC,XMUP,XMDOWN
CYG	COMMON/MASS2/XMW
CYG	COMMON/MASS3/XMU
CYG	COMMON/MASS4/XMSCALELECL,XMSCALNEUT,XMSCALDOWNL,XMSCALUPL,
CYG     +  XMSCALELECR,XMSCALDOWNR,XMSCALUPR
CYG	COMMON/MASS5/XMUTILDE
CYG	COMMON/COUPLS/A,B
CYG
CYG	INTEGER I(4),IJ(3,2)
CYG	COMMON/INDICES/I,IJ
CYGc
CYGc susygen interface
CYGc
CYG      COMMON/MSSM/TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI
CYG      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
CYG     +FLC(12),FRC(12),gms(12),echar(12)
CYG      common/spartcl/fmal(12),fmar(12),ratqa(12),fgamc(12),fgamcr(12)
CYG     +,cosmi(12)
CYG      COMMON/XCROS/xgaug(8),xeta(8)
CYG      COMMON/NEUMIX/ZR(4,4),was(4),ESA(4),
CYG     +VOIJL(4,4),VOIJR(4,4),gfir(4,4),gfil(4,4)
CYG      real*4 xlama
CYG      common/lamda/xlama
CYG      common/resa/res,res1a,res2a
CYG
CYG      common/variables/fms,fmi,fmk,fml1,fml2,etai,etak,brspa(6,48),
CYG     +lind(6,6,6),brgaug(23,6,6),fmelt,fmert,fmelu,fmeru
CYGc
CYGc
CYGc
CYG	EXTERNAL F1,F2
CYG	EXTERNAL EG1,EG2
CYG
CYGC*****************************************************************
CYGC I(4) are indices, I(1) determines the choice of dominant R-parity
CYGC violating operators 1=LLE,2=LQD,3=UDD. I(2)-I(4) are generation
CYGC indices. (I(1),I(2),I(3),I(4))=(2,1,2,3)  ~~ L_1Q_2D_3.
CYGC XLAM is the R-parity violating coupling
CYGC***************************************************************** 
CYGC       e.g
CYGc	I(1)=2
CYGc	I(2)=2
CYGc	I(3)=1
CYGc	I(4)=1
CYG
CYG	XLAM=xlama
CYGC*******************************************************************
CYGC	*** SUSY PARAMETERS FROM NEUTRALINO MIXING PROGRAM ***
CYGc                       (LSP mass in GeV)
CYGC*******************************************************************
CYG	SIN2THW = sinw**2
CYG	TANTHW = DSQRT(SIN2THW)/(DSQRT(1-SIN2THW))
CYG        call chbase(sinw,cosw,sinb,cosb,zr,xmix)
CYG	XMLSP= xgaug(1)
CYG	SINBc=SINB 
CYG	COSBc=COSB 
CYGc
CYGC   *******************************
CYGC
CYGC       FERMION MASSES IN GEV 
CYGC
CYGC   **********************************
CYG        do 1 k=1,3
CYG	XMUP(k)  =gms(1+(k-1)*4)
CYG	XMDOWN(k)=gms(2+(k-1)*4)
CYG	XMELEC(k)=gms(4+(k-1)*4)
CYGC***************************************
CYGC
CYGC       SFERMION MASSES IN GEV 
CYGC
CYGC**************************************
CYG	XMscalUPL(k)  =fmal(1+(k-1)*4)
CYG	XMscalDOWNL(k)=fmal(2+(k-1)*4)
CYG	XMscalneut(k) =fmal(3+(k-1)*4)
CYG	XMscalELECL(k)=fmal(4+(k-1)*4)
CYG	XMscalUPR(k)  =fmar(1+(k-1)*4)
CYG	XMscalDOWNR(k)=fmar(2+(k-1)*4)
CYG	XMscalELECR(k)=fmar(4+(k-1)*4)
CYG 1      continue
CYG
CYGC******************************************
CYGC   W-BOSON MASS IN GeV, EW-COUPLING
CYGC****************************************
CYG	XMW=fmw
CYG	E= dsqrt(e2)
CYG	G= dsqrt(g2)
CYG
CYG
CYGC************************************************************
CYGC The subroutine XMAS designates the masses according to the 
CYGC indices I(1),I(2),I(3),I(4). For the index choice: (2,1,2,3) XM(1) 
CYGC corresponds to the mass of the charged lepton of the lepton
CYGC doublet, XM(2) corresponds to the up-like quark mass in the
CYGC Quark doublet and XM(3) corresponds to the mass of the down-like
CYGC quark singlet. XM4,XM5,XM6 are the corresponding masses for the
CYGC other term, XM(4)=mneutrino_{I(2)}, XM(5)=mdown_{I(3)}, and 
CYGC XM(6)=mdown_{I(4)}=XM3. For I(1)=3, there is no doublet structure
CYGC and XM(4)=XM(5)=XM(6)=0.0. XMTILDE are the corresponding susy
CYGC masses. The XMU, XMUTILDE are normalized mass squares, which are
CYGC convenient for doing the integrals. The subroutine COUPL
CYGC determines the coupling constants corresponding to a choice
CYGC of indices. Again for L_iL_jE_k=e_i(1)nu_j(2)e_k(3)-
CYGC nu_i(4)e_j(5)e_k(6). So 6=3 always. 
CYGC
CYGC************************************************************
CYGC   Here the program starts
CYGC
CYG	CALL COUPL(I,A,B)
CYG	CALL XMASS(I,XM,XMTILDE)
CYGc
CYGc normalization of the masses
CYGc
CYG	DO 10 J=1,6
CYG		XMU(J)= (XM(J)/XMLSP)
CYG   10	CONTINUE
CYG	DO 70 K2=1,6
CYG		XMUTILDE(K2)= (XMTILDE(K2)/XMLSP)**2
CYG   70	CONTINUE
CYG
CYGC**************************************************************
CYGC Below 2 integrals are done: RES1 and RES2, because there are 2 decay 
CYGC modes for the operators LLe, LQD; for UDD, F2=0. XU are the upper integration 
CYGC bounds according to B&Ph and XL are the lower bounds. Integration
CYGC is set up as follows:(for I(1)=1)
CYGC RES1=\Int_{XL1}^{XU1} dX(1) 
CYGC and F1(X(1)) =\int_{XL3}^{XU3} dX(2) FM1
CYGC***************************************************************
CYG
CYG        
CYG        XL1= 2* ( XMU(1) )
CYG	XU1= 1+ XMU(1)**2- XMU(2)**2- XMU(3)**2 -2*(XMU(2)*XMU(3))
CYG        XL2= 2* ( XMU(4) )
CYG	XU2= 1+ XMU(4)**2- XMU(5)**2- XMU(6)**2 -2*(XMU(5)*XMU(6))
CYG
CYG        res1=0.
CYG        res2=0.
CYG	CALL TINT(XL1,XU1,F1,RES1)
CYG	if(i(1).ne.3)CALL TINT(XL2,XU2,F2,RES2)
CYG	FAC= XMLSP / (256. * PI**3)
CYG	RES1A= ( 8. * XLAM**2 * G**2 * RES1 ) * FAC
CYG	RES2A= ( 8. * XLAM**2 * G**2 * RES2 ) * FAC
CYGC
CYGC WE PUT IN AN EXTRA FACTOR OF TWO HERE, BECAUSE THE LSP DECAYS BOTH TO
CYGC PARTICLES AND ANTI-PARTICLES.
CYGC
CYG	RES = 2*(RES1A + RES2A)
CYGc
CYGc res1 eud res2 nuddbar X2 for charge conjugate
CYGc
CYG
CYG	write(1,*) ' LSP decay G1,G2 ',res1a,res2a
CYG        
CYG        call itran(I,ij)
CYG
CYG            do 80 klp=1,2
CYG            if(i(1).eq.3.and.klp.eq.2)go to 80
CYG            fmi =xmlsp
CYG            fmk =ssmass(ij(1,klp))
CYG            fml1=ssmass(ij(2,klp))
CYG            fml2=ssmass(ij(3,klp))
CYG            q=fmi-fmk-fml1-fml2
CYG            if(q.le.0.)go to 80
CYG
CYG            smin=(fml1+fml2)**2
CYG            smax=(fmi-fmk)**2
CYGc            umin=(fmk+fml2)**2
CYGc            umax=(fmi-fml1)**2
CYG            tmin=(fmk+fml1)**2
CYG            tmax=(fmi-fml2)**2
CYG
CYG            if(smin.ge.smax)go to 80
CYGc            if(umin.ge.umax)go to 80
CYG            if(tmin.ge.tmax)go to 80
CYG
CYG            nbin1=50
CYG            nbin2=50
CYG      if(klp.eq.1)then 
CYG      call hbfun2(7000+klp,' ',nbin1,sngl(smin),sngl(smax) ,nbin2,
CYG     +      sngl(tmin),sngl(tmax),eg1)
CYG      else  
CYG
CYG      if(i(1).ne.3)then
CYG      call hbfun2(7000+klp,' ',nbin1,sngl(smin),sngl(smax) ,nbin2,
CYG     +      sngl(tmin),sngl(tmax),eg2)
CYG      endif
CYG
CYG      endif
CYG   80 continue
CYG
CYG
CYG   40   RETURN
CYG	END
CYG
CYG
CYG        subroutine itran(i,ij)
CYG        integer i(4),ij(3,2)
CYG      common/reorder/ispa(12,2),kl(2,18),klap(2,18),idecs(12,2)
CYG
CYGc
CYGc LLE
CYGc
CYG        if(i(1).eq.1)then
CYG        ij(1,1)=idecs(4+(i(2)-1)*4,1)
CYG        ij(2,1)=-idecs(3+(i(3)-1)*4,1)
CYG        ij(3,1)=-idecs(4+(i(4)-1)*4,1)
CYG        ij(1,2)=-idecs(3+(i(2)-1)*4,1)
CYG        ij(2,2)=idecs(4+(i(3)-1)*4,1)
CYG        ij(3,2)=-idecs(4+(I(4)-1)*4,1)
CYG
CYG        endif
CYGc
CYGc LQD
CYGc
CYG        if(i(1).eq.2)then
CYG        ij(1,1)=idecs(4+(i(2)-1)*4,1)
CYG        ij(2,1)=idecs(1+(i(3)-1)*4,1)
CYG        ij(3,1)=-idecs(2+(i(4)-1)*4,1)
CYG
CYG        ij(1,2)=idecs(3+(i(2)-1)*4,1)
CYG        ij(2,2)=idecs(2+(i(3)-1)*4,1)
CYG        ij(3,2)=-idecs(2+(I(4)-1)*4,1)
CYG        endif
CYGc
CYGc UDD
CYGc
CYG        if(i(1).eq.3)then
CYG        ij(1,1)=idecs(1+(i(2)-1)*4,1)
CYG        ij(2,1)=idecs(2+(i(3)-1)*4,1)
CYG        ij(3,1)=idecs(2+(i(4)-1)*4,1)
CYG        ij(1,2)=0
CYG        ij(2,2)=0
CYG        ij(3,2)=0
CYG        endif
CYG
CYG        return
CYG        end
CYG
CYG
CYG	SUBROUTINE XMASS(I,XM,XMTILDE) 
CYGC
CYGC DETERMINE THE FINAL AND INTERMEDIATE STATE MASSES FROM THE GIVEN INDICES
CYGC
CYG	REAL*8 XM(6),XMDOWN(3),XMUP(3),XMELEC(3)
CYG	REAL*8 XMSCALELECR(3),XMSCALNEUT(3),XMSCALDOWNR(3)
CYG	REAL*8 XMSCALUPR(3),XMTILDE(6),XMSCALELECL(3),XMSCALDOWNL(3)
CYG	REAL*8 XMSCALUPL(3)
CYG	COMMON/MASS1/XMELEC,XMUP,XMDOWN
CYG	COMMON/MASS4/XMSCALELECL,XMSCALNEUT,XMSCALDOWNL,XMSCALUPL,
CYG     +  XMSCALELECR,XMSCALDOWNR,XMSCALUPR
CYG	INTEGER I(4)
CYG
CYG          IF (I(1).EQ.1) THEN	
CYG			XM(1)=XMELEC(I(2))
CYG			XM(2)=0.0
CYG			XM(3)=XMELEC(I(4))
CYG			XM(4)=0.0
CYG			XM(5)=XMELEC(I(3))
CYG			XM(6)=XM(3)
CYG			XMTILDE(1)= XMSCALELECL(I(2))
CYG			XMTILDE(2)= XMSCALNEUT(I(3))
CYG			XMTILDE(3)= XMSCALELECR(I(4))
CYG			XMTILDE(4)= XMSCALNEUT(I(3))
CYG			XMTILDE(5)= XMSCALELECL(I(3))
CYG			XMTILDE(6)= XMTILDE(3)
CYG		ELSE IF (I(1).EQ.2) THEN	
CYG			XM(1)=XMELEC(I(2))
CYG			XM(2)=XMUP(I(3))
CYG			XM(3)=XMDOWN(I(4))
CYG			XM(4)=0.0
CYG			XM(5)=XMDOWN(I(3))
CYG			XM(6)=XM(3)
CYG			XMTILDE(1)= XMSCALELECL(I(2))
CYG			XMTILDE(2)= XMSCALUPL(I(3))
CYG			XMTILDE(3)= XMSCALDOWNR(I(4))
CYG			XMTILDE(4)= XMSCALNEUT(I(3))
CYG			XMTILDE(5)= XMSCALDOWNL(I(3))
CYG			XMTILDE(6)= XMTILDE(3)
CYG	 	ELSE IF (I(1).EQ.3) THEN
CYG			XM(1)=XMUP(I(2))
CYG			XM(2)=XMDOWN(I(3))
CYG			XM(3)=XMDOWN(I(4))
CYG			XM(4)=0.0
CYG			XM(5)=0.0
CYG			XM(6)=0.0
CYG			XMTILDE(1)= XMSCALUPR(I(2))
CYG			XMTILDE(2)= XMSCALDOWNR(I(3))
CYG			XMTILDE(3)= XMSCALDOWNR(I(4))
CYG			XMTILDE(4)= 0.0 
CYG			XMTILDE(5)= 0.0 
CYG			XMTILDE(6)= 0.0 
CYG
CYG	  ENDIF
CYG	END
CYGC
CYGC*************************************************************
CYGC
CYG	SUBROUTINE COUPL(I,A,B)
CYG
CYGC
CYGC DETERMINE THE COUPLINGS AS A FUNCTION OF THE INDICES
CYGC
CYG	REAL*8 A(6),B(6),TANTHW,XMIX(4),SINB,COSB
CYG	REAL*8 	XMELEC(3), XMUP(3), XMDOWN(3), XMW
CYG	INTEGER I(4)
CYG	COMMON/SUSY/TANTHW,XMIX,COSB,SINB
CYG	COMMON/MASS1/XMELEC,XMUP,XMDOWN
CYG	COMMON/MASS2/XMW
CYG
CYG        IF (I(1).EQ.1) THEN	
CYG        	A(1)= XMIX(3)/(2*XMW*COSB) * XMELEC(I(2))
CYG	        A(2)= 0.0 
CYG		A(3)= XMIX(3)/(2*XMW*COSB) * XMELEC(I(4))
CYG		A(4)= 0.0
CYG		A(5)= XMIX(3)/(2*XMW*COSB) * XMELEC(I(3))
CYG		B(1)= -0.5*(XMIX(2) + TANTHW* XMIX(1))
CYG		B(2)= 0.5*(XMIX(2) - TANTHW* XMIX(1))
CYG		B(3)= -TANTHW * XMIX(1)
CYG		B(4)= B(2)
CYG		B(5)= B(1)
CYG           ELSE IF (I(1).EQ.2) THEN	
CYG        	A(1)= XMIX(3)/(2*XMW*COSB) * XMELEC(I(2))
CYG	        A(2)= XMIX(4)/(2*XMW*SINB) * XMUP(I(3))
CYG		A(3)= XMIX(3)/(2*XMW*COSB) * XMDOWN(I(4))
CYG		A(4)= 0.0
CYG		A(5)= XMIX(3)/(2*XMW*COSB) * XMDOWN(I(3))
CYG		B(1)= -0.5 * (XMIX(2) + TANTHW* XMIX(1))
CYG		B(2)= 0.5  * (XMIX(2) + 1./3.*TANTHW* XMIX(1))
CYG		B(3)= -1./3.* TANTHW * XMIX(1)
CYG		B(4)= 0.5*(XMIX(2) - TANTHW* XMIX(1))
CYG		B(5)= -0.5*(XMIX(2) - 1./3.*TANTHW* XMIX(1))
CYG	   ELSE IF (I(1).EQ.3) THEN
CYG        	A(1)= XMIX(4)/(2*XMW*SINB) * XMUP(I(2))
CYG	        A(2)= XMIX(3)/(2*XMW*COSB) * XMDOWN(I(3))
CYG		A(3)= XMIX(3)/(2*XMW*COSB) * XMDOWN(I(4))
CYG		A(4)= A(1)
CYG		A(5)= A(2)
CYG		B(1)= 2./3.* TANTHW * XMIX(1)
CYG		B(2)= -1./3.* TANTHW * XMIX(1)
CYG		B(3)= -1./3.* TANTHW * XMIX(1)
CYG		B(4)= B(1)
CYG		B(5)= B(2)
CYG	  ENDIF
CYG		A(6)= A(3)
CYG		B(6)= B(3)
CYGC
CYG        RETURN
CYG        END
CYGC
CYGC*****************************************************************
CYGC
CYG        SUBROUTINE TINT(XL, XU, FCT, Y) 
CYG
CYG        REAL*8  XL, XU, AI, BI, C, FCT, Y
CYG
CYG        AI = 0.5D0*(XU+XL)
CYG        BI = XU-XL
CYG        IF(BI.LE.0.0)Y = 0
CYGC 
CYGC Put a flag here for BI < 0.0
CYGC 
CYG        IF(BI.LE.0.0)RETURN
CYG        C = 0.49863193092474078D0 * BI
CYG        Y  =    0.35093050047350483D-2 * (FCT(AI+C) +FCT(AI-C))
CYG        C = 0.49280575577263417D0 * BI
CYG        Y =  Y+ 0.81371973654528350D-2 * (FCT(AI+C) +FCT(AI-C))
CYG        C = 0.48238112779375322D0 * BI
CYG        Y  = Y +0.12696032654631030D-1 * (FCT(AI+C) +FCT(AI-C))
CYG        C = 0.46745303796886984D0 * BI
CYG        Y  = Y +0.17136931456510717D-1 * (FCT(AI+C) +FCT(AI-C))
CYG        C = 0.44816057788302606D0 * BI
CYG        Y  = Y +0.21417949011113340D-1 * (FCT(AI+C) +FCT(AI-C))
CYG        C = 0.42468380686628499D0 * BI
CYG        Y  = Y +0.25499029631188088D-1 * (FCT(AI+C) +FCT(AI-C))
CYG        C = 0.39724189798397120D0 * BI
CYG        Y  = Y +0.29342046739267774D-1 * (FCT(AI+C) +FCT(AI-C))
CYG        C = 0.36609105937014484D0 * BI
CYG        Y  = Y+ 0.32911111388180923D-1 * (FCT(AI+C) +FCT(AI-C))
CYG        C = 0.33152213346510760D0 * BI
CYG        Y  = Y +0.36172897054424253D-1 * (FCT(AI+C) +FCT(AI-C))
CYG        C = 0.29385787862038116D0 * BI
CYG        Y = Y + 0.39096947893535153D-1 * (FCT(AI+C) +FCT(AI-C))
CYG        C = 0.25344995446611470D0 * BI
CYG        Y  = Y +0.41655962113473378D-1 * (FCT(AI+C) +FCT(AI-C))
CYG        C = 0.21067563806531767D0 * BI
CYG        Y  = Y +0.43826046502201906D-1 * (FCT(AI+C) +FCT(AI-C))
CYG        C = 0.16593430114106382D0 * BI
CYG        Y  = Y +0.45586939347881942D-1 * (FCT(AI+C) +FCT(AI-C))
CYG        C = 0.11964368112606854D0 * BI
CYG        Y  = Y +0.46922199540402283D-1 * (FCT(AI+C) +FCT(AI-C))
CYG        C = 0.7223598079139825D-1 * BI
CYG        Y  =  Y+0.47819360039637430D-1 * (FCT(AI+C) +FCT(AI-C))
CYG        C = 0.24153832843869158D-1 * BI
CYG        Y =BI * ( Y + 0.48270044257363900D-1 * (FCT(AI+C)+FCT(AI-C)))
CYG
CYG        RETURN
CYG        END
CYGC
CYGC****************************************************************
CYG
CYG	SUBROUTINE DOTPRODS(PP)
CYGC**************************************************************
CYGC This subroutine calculates all the 4-dotproducts which can
CYGC appear in the matrix element. For I(1)=1,  
CYGC     LSP(0)--> e_i(1)+nu_j(2)+e_k(3), nu_i(4)+e_j(5)+e_k(6), 
CYGC and PP(0,4)= (LSP.nu_i); PP(4,6)=(nu_i.e_k), etc
CYGC The dotproducts are given as in Barger and Phillips B2.2
CYGC second part.
CYGC*************************************************************
CYG
CYG	REAL*8 PP(0:6,0:6),X(6),XMU(6)
CYG	INTEGER J,K,L1,L2
CYG	COMMON/INTVAR/X
CYG	COMMON/MASS3/XMU
CYG	X(3)= 2 - X(1) - X(2)
CYG	X(6)= 2 - X(4) - X(5)
CYG	PP(0,0)=1.0
CYG	DO 20 J=1,6
CYG		PP(J,J) = (XMU(J)*XMU(J))**2
CYG   20	CONTINUE
CYG
CYG	PP(1,2) = 0.5* (1+ XMU(3)**2 - XMU(1)**2 - XMU(2)**2  - X(3) )
CYG	PP(1,3) = 0.5* (1+ XMU(2)**2 - XMU(1)**2 - XMU(3)**2  - X(2) )
CYG	PP(2,3) = 0.5* (1+ XMU(1)**2 - XMU(2)**2 - XMU(3)**2  - X(1) )
CYG	PP(4,5) = 0.5* (1+ XMU(6)**2 - XMU(4)**2 - XMU(5)**2  - X(6) )
CYG	PP(4,6) = 0.5* (1+ XMU(5)**2 - XMU(4)**2 - XMU(6)**2  - X(5) )
CYG	PP(5,6) = 0.5* (1+ XMU(4)**2 - XMU(5)**2 - XMU(6)**2  - X(4) )
CYG
CYG	DO 30 J=1,3
CYG		DO 40 K=4,6
CYG			PP(J,K)=0.0
CYG   40 		CONTINUE		
CYG   30	CONTINUE
CYG
CYG	DO 50 J=1,6
CYG		PP(0,J)= 0.5 * X(J)
CYG   50	CONTINUE
CYG	
CYG
CYG	DO 41 L1=0,5
CYG		DO 42 L2=L1+1,6
CYG			PP(L2,L1)=PP(L1,L2)
CYG   42 		CONTINUE		
CYG   41	CONTINUE
CYG
CYG        RETURN
CYG        END
CYGC
CYGC***********************************************************
CYGC F1 and F2 are the results of the first integration over
CYGC G1 or G2. 
CYGC*********************************************************
CYG	REAL*8 FUNCTION F1(Z1)
CYG
CYG	REAL*8 R1,Z1,XL3,XU3,X(6)
CYG	REAL*8 XMU(6),C1,C2,C3,G1,PS
CYG	COMMON/INTVAR/X
CYG	COMMON/MASS3/XMU
CYG	EXTERNAL G1
CYG
CYG	X(1)=Z1
CYG	C1= 1-X(1)+XMU(1)**2
CYG	C2= (2-X(1)) * (1 + XMU(1)**2 + XMU(2)**2 -XMU(3)**2 -X(1)) 
CYG
CYG	C3= DSQRT((Z1**2-4*XMU(1)**2 ) * PS( C1,XMU(2)**2 ,XMU(3)**2 ))
CYG
CYG	XU3= 0.5/ C1* ( C2 + C3 )
CYG	XL3= 0.5/ C1* ( C2 - C3 )
CYG	CALL TINT(XL3,XU3,G1,R1)
CYG	F1=R1
CYG
CYG	RETURN
CYG	END
CYGC
CYGC**********************************************************************
CYGC
CYG        real*4 function eg1(sb,tb)
CYG	IMPLICIT REAL*8 (A-H,O-Z)
CYG        real*4 sb,tb
CYG        real*8 xgaug,xeta
CYG        COMMON/XCROS/xgaug(8),xeta(8)
CYG	REAL*8 XMU(6),X(6)
CYG	COMMON/INTVAR/X
CYG	COMMON/MASS3/XMU
CYG      common/variables/fms,fmi,fmk,fml1,fml2,etai,etak,brspa(6,48),
CYG     +lind(6,6,6),brgaug(23,6,6),fmelt,fmert,fmelu,fmeru
CYG
CYG      eg1=0
CYG      GW1 = ((SB+FML1**2-FML2**2)/(2.*SQRT(SB))+
CYG     +        (FMI**2-FMK**2-SB) /(2.*SQRT(SB)))**2
CYG      GW2 = ((SB+FML1**2-FML2**2)**2/(4.*SB)-FML1**2)
CYG      GW3 = ((FMI**2-SB-FMK**2)**2/(4.*SB)-FMK**2)
CYG      if(GW2.le.0..or.GW3.le.0.)return
CYG      GW2 = SQRT(GW2)
CYG      GW3 = SQRT(GW3)
CYG      TMIN = GW1-(GW2+GW3)**2
CYG      TMAX = GW1-(GW2-GW3)**2
CYG      if(tmin.ge.tmax)return
CYG      if(tb.lt.tmin.or.tb.gt.tmax)return
CYG
CYG        x(1)=1.d0+xmu(1)**2 -sb/xgaug(1)**2
CYG        x(2)=1.d0+xmu(2)**2 -tb/xgaug(1)**2
CYG        eg1=g1(x(2))
CYG        eg1=eg1/xgaug(1)**4
CYG        return
CYG        end
CYG
CYG        real*4 function eg2(sb,tb)
CYG	IMPLICIT REAL*8 (A-H,O-Z)
CYG        real*4 sb,tb
CYG        real*8 xgaug,xeta
CYG        COMMON/XCROS/xgaug(8),xeta(8)
CYG	REAL*8 XMU(6),X(6)
CYG	COMMON/INTVAR/X
CYG	COMMON/MASS3/XMU
CYG      common/variables/fms,fmi,fmk,fml1,fml2,etai,etak,brspa(6,48),
CYG     +lind(6,6,6),brgaug(23,6,6),fmelt,fmert,fmelu,fmeru
CYG       eg2=0
CYG      GW1 = ((SB+FML1**2-FML2**2)/(2.*SQRT(SB))+
CYG     +        (FMI**2-FMK**2-SB) /(2.*SQRT(SB)))**2
CYG      GW2 = ((SB+FML1**2-FML2**2)**2/(4.*SB)-FML1**2)
CYG      GW3 = ((FMI**2-SB-FMK**2)**2/(4.*SB)-FMK**2)
CYG      if(GW2.le.0..or.GW3.le.0.)return
CYG      GW2 = SQRT(GW2)
CYG      GW3 = SQRT(GW3)
CYG      TMIN = GW1-(GW2+GW3)**2
CYG      TMAX = GW1-(GW2-GW3)**2
CYG      if(tmin.ge.tmax)return
CYG      if(tb.lt.tmin.or.tb.gt.tmax)return
CYG
CYG        x(4)=1.d0+xmu(4)**2 -sb/xgaug(1)**2
CYG        x(5)=1.d0+xmu(5)**2 -tb/xgaug(1)**2
CYG        eg2=g2(x(5))
CYG        eg2=eg2/xgaug(1)**4
CYG        return
CYG        end
CYG
CYG	REAL*8 FUNCTION G1(Z2)
CYG	REAL*8 Z2,FM1,FM2,FM3
CYG	INTEGER I(4),IJ(3,2)
CYG	COMMON/INDICES/I,IJ
CYGC
CYGC CF IS THE COLOUR FACTOR
CYGc
CYG          IF (I(1).EQ.1) THEN
CYG	      G1=FM1(Z2)
CYG          ELSE IF (I(1).EQ.2) THEN
CYG	      CF=3
CYG	      G1=CF*FM2(Z2)
CYG	  ELSE IF (I(1).EQ.3) THEN
CYG	      CF=6
CYG	      G1=CF*FM3(Z2)
CYG	  END IF
CYG	RETURN
CYG	END
CYGC
CYGC**********************************************************************
CYGC FM1--FM5 give the explicit matrix elements in terms of normalized
CYGC masses squared and the dotproducts.
CYGC*********************************************************************
CYG	REAL*8 FUNCTION FM1(Z2)
CYG
CYG	REAL*8 Z2,XMU(6),X(6),PP(0:6,0:6),XMUTILDE(6)
CYG	REAL*8 A(6),B(6),D(6),GFUNC
CYG	COMMON/INTVAR/X
CYG	INTEGER I(4),IJ(3,2)
CYG	COMMON/INDICES/I,IJ
CYG	COMMON/MASS3/XMU
CYG	COMMON/MASS5/XMUTILDE
CYG	COMMON/DOTPROD/PP
CYG	COMMON/COUPLS/A,B
CYG
CYG	X(2)=Z2
CYG	CALL DOTPRODS(PP)
CYG	DO 64 K=1,3
CYG		D(K)=1./(1.+XMU(K)**2-XMUTILDE(K)-2*PP(0,K))
CYG   64	CONTINUE
CYG	FM1=D(2)**2 * PP(1,3) * ( (A(2)**2 + B(2)**2 ) * PP(0,2)+ 
CYG     *               2* A(2)* B(2) * (XMU(2)) ) 
CYG     *     + D(3)**2 * PP(1,2) * ( (A(3)**2 + B(3)**2) * PP(0,3)+
CYG     *               2* A(3)*(-B(3))* (XMU(3)) )
CYG     *     + D(1)**2 * PP(2,3) * ((A(1)**2 + B(1)**2) * PP(0,1) + 
CYG     *               2* A(1)*B(1) * (XMU(1)) )
CYG     *  -D(1)*D(2) * ( A(2)*A(1) * (XMU(1) * XMU(2)) * PP(0,3)
CYG     *     + A(2) * B(1) *( XMU(2) ) * PP(1,3)
CYG     *     + A(1) * B(2) *( XMU(1) ) * PP(2,3)
CYG     *     + B(1) * B(2) * GFUNC(2,0,1,3) )
CYG     *  -D(2)* D(3)* (A(2)* A(3) * (XMU(2)*XMU(3))* PP(0,1)
CYG     *     + A(2) * (-B(3)) * (XMU(2)) * PP(1,3)
CYG     *     + A(3) * B(2) * (XMU(3)) * PP(1,2)
CYG     *     + B(2) * (-B(3)) * GFUNC(2,0,3,1))
CYG     *  -D(1)*D(3)* (A(1) * (-B(3))* (XMU(1)) * PP(2,3)
CYG     *     + A(1) * A(3) * (XMU(1)*XMU(3)) * PP(0,2)
CYG     *     + A(3) * B(1) * (XMU(3)) * PP(1,2)
CYG     *     + B(1) * (-B(3)) * GFUNC(0,1,2,3) )
CYG
CYG	RETURN
CYG	END
CYGC
CYGC**********************************************************************
CYGC
CYG	REAL*8 FUNCTION FM2(Z2)
CYG 	REAL*8 XMU(6),Z2,X(6),PP(0:6,0:6),XMUTILDE(6),D(6)
CYG	REAL*8 A(6),B(6),GFUNC
CYG	COMMON/INTVAR/X
CYG	INTEGER I(4),IJ(3,2)
CYG	COMMON/INDICES/I,IJ
CYG	COMMON/MASS3/XMU
CYG	COMMON/MASS5/XMUTILDE
CYG	COMMON/DOTPROD/PP
CYG	COMMON/COUPLS/A,B
CYG
CYG	X(2)=Z2
CYG	CALL DOTPRODS(PP)
CYG	DO 63 K=1,3
CYG		D(K)=1./(1.+XMU(K)**2-XMUTILDE(K)-2*PP(0,K))
CYG   63	CONTINUE
CYG	FM2= (D(2)**2 * PP(1,3)* ( (A(2)**2 + B(2)**2 ) * PP(0,2) 
CYG     *            +   2* A(2)* B(2) * (XMU(2)) ) 
CYG     *     + D(3)**2 * PP(1,2) * ( (A(3)**2 + B(3)**2) * PP(0,3)+
CYG     *               2* A(3)*(-B(3))* (XMU(3)) )
CYG     *     + D(1)**2 * PP(2,3) * ((A(1)**2 + B(1)**2) * PP(0,1) + 
CYG     *               2* A(1)*B(1) * (XMU(1)) )
CYG     *  -D(1)*D(2)* ( A(2)*A(1) * (XMU(1) * XMU(2)) * PP(0,3)
CYG     *     + A(2) * B(1) *( XMU(2) ) * PP(1,3)
CYG     *     + A(1) * B(2) *( XMU(1) ) * PP(2,3)
CYG     *     + B(1) * B(2) * GFUNC(2,0,1,3) )
CYG     *  -D(2)* D(3)* (A(2)* A(3) * (XMU(2)*XMU(3))* PP(0,1)
CYG     *     + A(2) * (-B(3)) * (XMU(2)) * PP(1,3)
CYG     *     + A(3) * B(2) * (XMU(3)) * PP(1,2)
CYG     *     + B(2) * (-B(3)) * GFUNC(2,0,3,1))
CYG     *  -D(1)*D(3)* (A(1) * (-B(3))* (XMU(1)) * PP(2,3)
CYG     *     + A(1) * A(3) * (XMU(1)*XMU(3)) * PP(0,2)
CYG     *     + A(3) * B(1) * (XMU(3)) * PP(1,2)
CYG     *     + B(1) * (-B(3)) * GFUNC(0,1,2,3) ))
CYG
CYG	RETURN
CYG	END
CYGC
CYGC**********************************************************************
CYGC
CYG	REAL*8 FUNCTION FM3(Z2)
CYG	REAL*8 XMU(6),Z2,X(6),PP(0:6,0:6)
CYG	REAL*8 A(6),B(6),XMUTILDE(6),D(6),GFUNC
CYG	COMMON/INTVAR/X
CYG	INTEGER I(4),IJ(3,2)
CYG	COMMON/INDICES/I,IJ
CYG	COMMON/MASS3/XMU 
CYG	COMMON/MASS5/XMUTILDE
CYG	COMMON/DOTPROD/PP
CYG	COMMON/COUPLS/A,B
CYG
CYG	X(2)=Z2
CYG	CALL DOTPRODS(PP)
CYG	DO 62 K=1,3
CYG		D(K)=1./(1.+XMU(K)**2-XMUTILDE(K)-2*PP(0,K))
CYG   62	CONTINUE
CYG								
CYG	FM3=  
CYG     *   D(1) * D(1) * PP(2,3) * ( ( B(1)**2 + A(1)**2 ) *PP(0,1)
CYG     *      +2 * (-B(1)) * A(1) *( XMU(1) ) )
CYG     *  +D(2) * D(2) * PP(1,3) * ( (A(2)**2 + B(2)**2) * PP(0,2)
CYG     *      +2 * (-B(2)) * A(2) *( XMU(2) ) )
CYG     *  +D(3) * D(3) * PP(1,2) * ( ( A(3)**2 + B(3)**2 ) * PP(0,3)
CYG     *      +2 * (-B(3)) * A(3) *( XMU(3) )  )
CYG     *  + D(1)*D(2)* ( A(1)*A(2)*GFUNC(1,0,2,3) 
CYG     *    + B(1) * B(2) *( XMU(1)* XMU(3) ) * PP(0,3)
CYG     *    + (-B(1)) * A(2) *( XMU(1) ) * PP(2,3) 
CYG     *    + (-B(2)) * A(1) *( XMU(2) ) * PP(1,3)  ) 
CYG     *  + D(1) * D(3) * ( A(1)*A(3)*GFUNC(1,0,3,2)
CYG     *    + B(1) * B(3) *( XMU(1) * XMU(3)) * PP(0,2)   
CYG     *    + (-B(1)) * A(3) *( XMU(1) ) * PP(2,3)      
CYG     *    + (-B(3)) * A(1) *( XMU(3) ) * PP(1,2)  )
CYG     *  + D(2) * D(3) * ( A(2) * A(3) * GFUNC(2,0,3,1)
CYG     *    + B(2) * B(3) * (XMU(2) * XMU(3)) * PP(0,1)
CYG     *    + (-B(2)) * A(3) * (XMU(2)) * PP(1,3)
CYG     *    + (-B(3)) * A(2) * (XMU(3)) * PP(1,2)  )
CYG
CYG
CYG	RETURN
CYG	END
CYGC
CYGC*********************************************************************
CYGC
CYG	REAL*8 FUNCTION F2(Y1)
CYG
CYG	REAL*8 R2,XL4,XU4,XMU(6),X(6),Y1
CYG	REAL*8 C1,C2,C3,PS,G2
CYG	COMMON/INTVAR/X
CYG	COMMON/MASS3/XMU 
CYG	EXTERNAL G2
CYG	X(4)=Y1
CYG	C1= 1- X(4)+ XMU(4)**2
CYG	C2=  (2-Y1) * (1 + XMU(4)**2 + XMU(5)**2-XMU(6)**2-Y1) 
CYG	C3= DSQRT((Y1**2-4*XMU(4)**2 ) * PS( C1,XMU(5)**2,XMU(6)**2))
CYG
CYG	XU4= 0.5/ C1* ( C2 + C3 )
CYG	XL4= 0.5/ C1* ( C2 - C3 )
CYG	CALL TINT(XL4,XU4,G2,R2)
CYG	F2=R2
CYG	RETURN
CYG	END
CYGC
CYGC**********************************************************************
CYGC
CYG	REAL*8 FUNCTION G2(Y2)
CYG
CYG	INTEGER I(4),IJ(3,2)
CYG	COMMON/INDICES/I,IJ
CYG	REAL*8 Y2,FM4,FM5
CYG
CYG          IF (I(1).EQ.1) THEN
CYG	      G2= FM4(Y2)
CYG          ELSE IF (I(1).EQ.2) THEN
CYG	      G2= 3*FM5(Y2)
CYG	  ELSE IF (I(1).EQ.3) THEN
CYG	      G2= 0.0 
CYG	  END IF
CYG
CYG	RETURN
CYG	END
CYGC
CYGC**********************************************************************
CYGC
CYG	REAL*8 FUNCTION FM4(Y2)
CYG
CYG	REAL*8 XMU(6),X(6),PP(0:6,0:6),Y2
CYG	REAL*8 A(6),B(6),XMUTILDE(6),D(6),GFUNC
CYG	INTEGER I(4),IJ(3,2)
CYG	COMMON/INDICES/I,IJ
CYG	COMMON/INTVAR/X
CYG	COMMON/MASS3/XMU 
CYG	COMMON/MASS5/XMUTILDE
CYG	COMMON/DOTPROD/PP
CYG	COMMON/COUPLS/A,B
CYG
CYG	X(5)=Y2
CYG	CALL DOTPRODS(PP)
CYG
CYG	DO 60 K=4,6
CYG		D(K)=1./(1.+XMU(K)**2-XMUTILDE(K)-2*PP(0,K))
CYG   60	CONTINUE
CYG
CYG	FM4= D(5)**2 * PP(4,6) * ( (A(5)**2 + B(5)**2 ) * PP(0,5)+ 
CYG     *               2* A(5)* B(5) * (XMU(5)) ) 
CYG     *     + D(6)**2 * PP(4,5) * ( (A(6)**2 + B(6)**2) * PP(0,6)+
CYG     *               2* A(6)*(-B(6))* (XMU(6)) )
CYG     *     + D(4)**2 * PP(5,6) * ((A(4)**2 + B(4)**2) * PP(0,4) + 
CYG     *               2* A(4)*B(4) * (XMU(4)) )
CYG     *  -D(4)*D(5) * ( A(5)*A(4) * (XMU(4) * XMU(5)) * PP(0,6)
CYG     *     + A(5) * B(4) *( XMU(5) ) * PP(4,6)
CYG     *     + A(4) * B(5) *( XMU(4) ) * PP(5,6)
CYG     *     + B(4) * B(5) * GFUNC(5,0,4,6) )
CYG     *  -D(5)* D(6)* (A(5)* A(6) * (XMU(5)*XMU(6))* PP(0,4)
CYG     *     + A(5) * (-B(6)) * (XMU(5)) * PP(4,6)
CYG     *     + A(6) * B(5) * (XMU(6)) * PP(4,5)
CYG     *     + B(5) * (-B(6)) * GFUNC(5,0,6,4))
CYG     *  -D(4)*D(6)* (A(4) * (-B(6))* (XMU(4)) * PP(5,6)
CYG     *     + A(4) * A(6) * (XMU(4)*XMU(6)) * PP(0,5)
CYG     *     + A(6) * B(4) * (XMU(6)) * PP(4,5)
CYG     *     + B(4) * (-B(6)) * GFUNC(0,4,5,6) )
CYG	RETURN
CYG	END
CYGC
CYGC**********************************************************************
CYGC
CYG	REAL*8 FUNCTION FM5(Y2)
CYG
CYG	REAL*8 Y2,XMU(6),X(6),PP(0:6,0:6),XMUTILDE(6),D(6)
CYG	REAL*8 A(6),B(6),GFUNC
CYG	INTEGER I(4),IJ(3,2)
CYG	COMMON/INDICES/I,IJ
CYG	COMMON/INTVAR/X
CYG	COMMON/MASS3/XMU
CYG	COMMON/MASS5/XMUTILDE
CYG	COMMON/DOTPROD/PP
CYG	COMMON/COUPLS/A,B
CYG
CYG	X(5)=Y2
CYG	CALL DOTPRODS(PP)
CYG	DO 61 K=4,6
CYG		D(K)=1./(1.+XMU(K)**2-XMUTILDE(K)-2*PP(0,K))
CYG   61	CONTINUE
CYG	FM5=(D(5)**2* PP(4,6)* ( (A(5)**2 + B(5)**2) * PP(0,5)+ 
CYG     *               2* A(5)* B(5) * (XMU(5)) ) 
CYG     *     + D(6)**2 * PP(4,5) * ( (A(6)**2 + B(6)**2) * PP(0,6)+
CYG     *               2* A(6)*(-B(6))* (XMU(6)) )
CYG     *     + D(4)**2 * PP(5,6) * ((A(4)**2 + B(4)**2) * PP(0,4) + 
CYG     *               2* A(4)*B(4) * (XMU(4)) )
CYG     *  -D(4)*D(5) * ( A(5)*A(4) * (XMU(4) * XMU(5)) * PP(0,6)
CYG     *     + A(5) * B(4) *( XMU(5) ) * PP(4,6)
CYG     *     + A(4) * B(5) *( XMU(4) ) * PP(5,6)
CYG     *     + B(4) * B(5) * GFUNC(5,0,4,6) )
CYG     *  -D(5)* D(6)* (A(5)* A(6) * (XMU(5)*XMU(6))* PP(0,4)
CYG     *     + A(5) * (-B(6)) * (XMU(5)) * PP(4,6)
CYG     *     + A(6) * B(5) * (XMU(6)) * PP(4,5)
CYG     *     + B(5) * (-B(6)) * GFUNC(5,0,6,4))
CYG     *  -D(4)*D(6)* (A(4) * (-B(6))* (XMU(4)) * PP(5,6)
CYG     *     + A(4) * A(6) * (XMU(4)*XMU(6)) * PP(0,5)
CYG     *     + A(6) * B(4) * (XMU(6)) * PP(4,5)
CYG     *     + B(4) * (-B(6)) * GFUNC(0,4,5,6) ) )
CYG
CYG	RETURN
CYG	END
CYGC
CYGC**************************************************************
CYGC PS is the Phase space function given at the beginning of appendix
CYGC B in Barger and Phillips as \lambda(x,y,z).
CYGC****************************************************************
CYG	REAL*8 FUNCTION PS(W1,W2,W3)
CYG
CYG	REAL*8 W1,W2,W3
CYG
CYG	PS= W1**2+ W2**2+ W3**2- 2*(W1*W2+ W1*W3+ W2*W3)
CYG
CYG        if(ps.le.0.)ps=0.
CYG
CYG	RETURN
CYG	END
CYGC
CYGC************************************************************
CYGC GFUNC is a recurring expression in the matrix elements for the trace 
CYGC of four gamma matrices divided by 4.
CYGC***************************************************************
CYG	REAL*8 FUNCTION GFUNC(J1,J2,J3,J4)
CYG
CYG	REAL*8 PP(0:6,0:6)
CYG	INTEGER J1,J2,J3,J4
CYG	COMMON/DOTPROD/PP
CYG
CYG	GFUNC= PP(J1,J2)*PP(J3,J4) - PP(J1,J3)*PP(J2,J4) 
CYG     *        + PP(J1,J4) * PP(J2,J3)
CYG
CYG	RETURN
CYG	END
CYG
CYG      subroutine chbase(sinw,cosw,sinb,cosb,zr,xm)
CYG      real*8 sinw,cosw,sinb,cosb,xm(4)
CYG      real*8 zr(4,4),vo(4,4)
CYG
CYG      vo(1,1)=cosw
CYG      vo(1,2)=-sinw
CYG      vo(1,3)=0
CYG      vo(1,4)=0
CYG
CYG      vo(2,1)=sinw
CYG      vo(2,2)=cosw
CYG      vo(2,3)=0
CYG      vo(2,4)=0
CYG
CYG      vo(3,1)=0
CYG      vo(3,2)=0
CYG      vo(3,3)=cosb
CYG      vo(3,4)=sinb
CYG
CYG      vo(4,1)=0
CYG      vo(4,2)=0
CYG      vo(4,3)=-sinb
CYG      vo(4,4)=cosb
CYG
CYG      do 21  i=1,4
CYG      xm(i)=
CYG     +vo(i,1)*zr(1,1)+vo(i,2)*zr(2,1)+vo(i,3)*zr(3,1)+vo(i,4)*zr(4,1)
CYG   21 continue
CYG
CYG
CYG      do 20  i=1,4
CYG      xm(i)=
CYG     +vo(i,1)*zr(1,1)+vo(i,2)*zr(1,2)+vo(i,3)*zr(1,3)+vo(i,4)*zr(1,4)
CYG   20 continue
CYG
CYG      r=dsqrt(xm(1)**2+xm(2)**2+xm(3)**2+xm(4)**2)
CYG      print *,r
CYG
CYG      print *,xmix
CYG
CYG  
CYG      return
CYG      end
CYG
CYG
CYG*CMZ :  1.00/00 14/04/95  18.46.27  by  Stavros Katsanevas
CYG*-- Author :    Stavros Katsanevas   14/04/95
CYG
CYG      SUBROUTINE lspdecay(PIN,PP1,PP2,PP3,ind,ifail)
CYG*
CYGC****************   IT GENERATES THREE BODY DECAY
CYG*
CYG      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
CYG
CYG      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
CYG     +FLC(12),FRC(12),gms(12),echar(12)
CYG
CYG	INTEGER I(4),IJ(3,2)
CYG	COMMON/INDICES/I,IJ
CYG
CYG      common/reorder/ispa(12,2),kl(2,18),klap(2,18),idecs(12,2)
CYG
CYG
CYG      integer ind(3)
CYG      DOUBLE PRECISION BETA(3)
CYG      DOUBLE PRECISION PIN(5),P1(5),P2(5),P3(5)
CYG      DOUBLE PRECISION        PP1(5),PP2(5),PP3(5)
CYG
CYG      real*4 rndm,rsb,rtb
CYG
CYG
CYG        real*4 xlama
CYG        common/lamda/xlama
CYG        common/resa/res,res1,res2
CYG
CYGc
CYGc s = the W*,Z* combination
CYGc t = the up combination
CYGc u = the down combination
CYGc
CYG      ifail=1
CYG
CYG      ntry=0
CYG  30  continue
CYG      ntry=ntry+1
CYG      if(ntry.gt.100)return
CYG
CYG      fmi=ssmass(71)
CYG
CYG      res=res1+res2
CYG      ares=res*rndm(0)
CYG      if(ares.lt.res1)then
CYG      indx=7001
CYG      do 81 lp=1,3
CYG 81   ind(lp)=ij(lp,1)
CYG      else
CYG      indx=7002
CYG      do 82 lp=1,3
CYG 82   ind(lp)=ij(lp,2)
CYG      endif
CYG
CYG            fmk =ssmass(ind(1))
CYG            fml1=ssmass(ind(2))
CYG            fml2=ssmass(ind(3))
CYG
CYG      isign=1
CYG      if(rndm(0).lt.0.5)isign=-1
CYG      do 83 lp=1,3
CYG 83   ind(lp)=ind(lp)*isign
CYG
CYG      call hrndm2(indx,rsb,rtb)
CYG      sb=dble(rsb)
CYG      tb=dble(rtb)
CYG
CYGc
CYGc  u= charged slepton
CYGc
CYG      GW1 = ((SB+FML1**2-FML2**2)/(2.*DSQRT(SB))+
CYG     +        (FMI**2-FMK**2-SB) /(2.*DSQRT(SB)))**2
CYG      GW2 = ((SB+FML1**2-FML2**2)**2/(4.*SB)-FML1**2)
CYG      GW3 = ((FMI**2-SB-FMK**2)**2/(4.*SB)-FMK**2)
CYG      if(GW2.le.0..or.GW3.le.0.)go to 30
CYG      GW2 = DSQRT(GW2)
CYG      GW3 = DSQRT(GW3)
CYG      TMIN = GW1-(GW2+GW3)**2
CYG      TMAX = GW1-(GW2-GW3)**2
CYG      if(tmin.ge.tmax)go to 30
CYG      if(tb.lt.tmin.or.tb.gt.tmax)go to 30
CYG      UB = FMI**2+FMK**2+FML1**2+FML2**2-SB-TB
CYG
CYGc      GW1 = ((SB+FML2**2-FML1**2)/(2.*DSQRT(SB))+
CYGc     +        (FMI**2-FMK**2-SB) /(2.*DSQRT(SB)))**2
CYGc      GW2 = ((SB+FML2**2-FML1**2)**2/(4.*SB)-FML2**2)
CYGc      GW3 = ((FMI**2-SB-FMK**2)**2/(4.*SB)-FMK**2)
CYGc      if(GW2.le.0..or.GW3.le.0)go to 30
CYGc      GW2 = DSQRT(GW2)
CYGc      GW3 = DSQRT(GW3)
CYGc      UMIN = GW1-(GW2+GW3)**2
CYGc      UMAX = GW1-(GW2-GW3)**2
CYGc      if(umin.ge.umax)go to 30
CYGc      if(ub.lt.umin.or.ub.gt.umax)go to 30
CYG*
CYGc      TB=FMI**2+FMK**2+fml1**2+fml2**2-SB-UB
CYG
CYG      P1(4)=(FMI**2+fml1**2-UB)/2./fmi
CYG      P2(4)=(FMI**2+fml2**2-TB)/2./fmi
CYG*
CYG      C12    = fmi**2-fmk**2-2.*fmi*(P1(4)+P2(4))
CYG      C12    = .5*C12/(P1(4)*P2(4))+1.
CYG      IF(ABS(C12).GT.1.)GO TO 30
CYG      CT1    = -1.+2.*dble(RNDM(0))
CYG      FI2    = TWOPI*dble(RNDM(0))
CYG
CYG      CF2    = COS(FI2)
CYG      SF2    = SIN(FI2)
CYG      ST1    = SQRT(1.D0-CT1**2)
CYG      S12    = SQRT(1.D0-C12**2)
CYG      PP=(P1(4)**2-fmk**2)
CYG
CYG      IF(PP.LT.0.)GO TO 30
CYG      PP=SQRT(PP)
CYG      P1(1)  = PP*ST1
CYG      P1(2)  = 0.0
CYG      P1(3)  = PP*CT1
CYG      P1(5)  = fmk
CYGC
CYG      PP=(P2(4)**2-fml1**2)
CYG      IF(PP.LT.0.)GO TO 30
CYG      PP=SQRT(PP)
CYG      P2(1)  = PP*(S12*CF2*CT1+C12*ST1)
CYG      P2(2)  = PP*S12*SF2
CYG      P2(3)  = PP*(C12*CT1-S12*CF2*ST1)
CYG      P2(5)  = fml1
CYGC
CYG      P3(4)  = fmi-P1(4)-P2(4)
CYG      P3(1)  = -P1(1)-P2(1)
CYG      P3(2)  = -P2(2)
CYG      P3(3)  = -P1(3)-P2(3)
CYG      P3(5)  = fml2
CYG
CYGC
CYGC      Here we rotate of phi1 the particles from the gaugino decay.
CYGC
CYG
CYG      FI    = TWOPI*dble(RNDM(0))
CYG      CF    = COS( FI )
CYG      SF    = SIN( FI )
CYG      CALL TRASQU(P1,PP1,CF,SF)
CYG      CALL TRASQU(P2,PP2,CF,SF)
CYG      CALL TRASQU(P3,PP3,CF,SF)
CYGC
CYGC      Here we rotate the GAUGINO vector of theta and phi
CYGC
CYG      pmom=sqrt(pin(1)**2+pin(2)**2+pin(3)**2)
CYG      px=pin(1)/pmom
CYG      py=pin(2)/pmom
CYG      CTHCM=pin(3)/pmom
CYG      PHCM  = atan2(py,px)
CYG
CYG      STHCM = SQRT( 1. - CTHCM**2 )
CYG      CPHCM = COS( PHCM )
CYG      SPHCM = SIN( PHCM )
CYG
CYG      CALL TRASLA( PP1,CTHCM,STHCM,CPHCM,SPHCM )
CYG      CALL TRASLA( PP2,CTHCM,STHCM,CPHCM,SPHCM )
CYG      CALL TRASLA( PP3,CTHCM,STHCM,CPHCM,SPHCM )
CYGC
CYGC      Here we boost the particles from the wino to the c. of m. frame.
CYGC
CYG      BETA(1) = Pin(1) / Pin(4)
CYG      BETA(2) = Pin(2) / Pin(4)
CYG      BETA(3) = Pin(3) / Pin(4)
CYG
CYG      CALL BOOSTSUSY (PP1,BETA)
CYG      CALL BOOSTSUSY (PP2,BETA)
CYG      CALL BOOSTSUSY (PP3,BETA)
CYG
CYG      ifail=0
CYG
CYG      return
         END
*********************************************************************
	SUBROUTINE NTONPH(idx,ifx,fM0,fM2,fMU,fTGB,fMTOP,oGNTONP)
*********************************************************************
C Computes the total width for the 1-loop radiative decay 
C X°(i) --> X°(j<i) gamma 
C with exact formula and approximate formulae 
C in the 2 special cases in which it is very important for i=2, j=1: 
C 1) M1, M2, mu << Mz --> X°(1) & X°(2) ca. ~gamma & ~Hb 
C 2) M1, M2 >> |mu|, Mz --> X°(1) & X°(2) ca. ~H1 & ~H2 
**********************************************************************
C Written by Sandro Ambrosanio (1994) 
**********************************************************************

	IMPLICIT DOUBLE PRECISION (A-H,L-Z)

	DOUBLE PRECISION INN, INC 
C S.A. note:  
C according to the declaration of these variables in all   
C other SUSYGEN routines 

	LOGICAL IMSG
C S.A. note: to allow the user for decide if the WARNING messages 
C	     have to be displayed or not 

        PARAMETER (PI=3.141592653589793238, 
     .	  MZ=91.19, WZ=2.49, 
     .    MW=80.22, WW=2.08, 
     .	  MVE=0.0, MVMU=0.0, MVTAU=0.0, 
     .    ME=0.511D-3, MMU=105.66D-3, MTAU=1.784, 
     .	  MUP=3.D-3, MDO=8.D-3, MST=.2, MCH=1.5, MBO=5., 
     .    ALFEM = 1./127.9, SIN2WI=.2324, 
     .    ALFAS = 0.12) 
 
	LOGICAL FLAGMFHB, FLAGMHBF, FLAGNFHB, FLAGNHBF, 
     .		FLAGEFHB, FLAGEHBF,   
     .		FLAGMHH, FLAGNHH, FLAGEHH 

	DIMENSION AMNEUT(4), NMX(4,4), ZR(4,4), INN(4), 
     .		  Z(4,4), ZP(4), ZM(4),  
     .		  MCHAR(2), AMCHAR(2), U(2,2), V(2,2), INC(2), 
     .		  GNTONP(4,4), GNTONPA(4,4)

C	COMMON/PARMS/M0,M2,MU,TGB,MTOP,DELALF,MA 
C	COMMON/GAUMASS/M12,M1,M3
C S.A. note: The commons above now not needed 
C	COMMON/NEUTS/NEUT,ANEUT,MNEUT,AMNEUT,INN,NMX
C S.A. note: the above common is substituted by the below one  
C S.A. WARNING: here amneut and inn are WAS and ESA, respectively 
C S.A. WARNING: BUT ZR HAVE REVERSED INDICES WITH RESPECT TO NMX!!! 
	common/neumix/ZR,amneut,inn,waste(64)
	COMMON/ZEUTS/Z,NMX    	!Z is the neutralino mixing matrix 
				!in another basis, calculated below 
				!NMX is ZR after reversing indices 
				!They are also used in the NTOXH routine 
C	COMMON/CHARS/CHAR,ACHAR,MCHAR,AMCHAR,INC,U,V 
C S.A. note: the above common is substituted by the below one  
	common/chamix/wwaste(8),v,u,amchar,inc 
C S.A. WARNING: here amneut and inc are FM and ETA respectively 
C	COMMON/SFERMS/MSVE,MSVMU,MSVTAU,MSEL,MSER,MSMUL,MSMUR,MSTAUL,
C     .		     MSTAUR,MSUPL,MSUPR,MSDOL,MSDOR,MSSTL,MSSTR,
C     .		     MSCHL,MSCHR,MSBOL,MSBOR,MSTOPL,MSTOPR
C S.A. note: the above common is substituted by the below one  
      common/spartcl/
     +     msupl,msdol,msve,msel,mschl,msstl,msvmu,msmul,mstopl,
     +     msbol,msvtau,mstaul,
     +     msupr,msdor,msver,mser,mschr,msstr,msvmur,msmur,mstopr,
     +     msbor,msvtaur,mstaur,
     +     ratq(12),fgamc(12),fgamcr(12),cosmi(12)
C	COMMON/SFERMIX/MSTAU1,MSTAU2,MSTOP1,MSTOP2,MSBO1,MSBO2	

      common/higgses/mlh,ma,mhh,mhpm,sina,cosa

C  S.A. note: only MHPM (mass of the charged higgses) is really needed 
C             and is re-calculated also below. 
C             The other higgs masses are calculated 
C             and used in the HIGMAS routine 
C	COMMON/NWIDTHS/GNTONLL,GNTONVV,GNTONUU,GNTONDD, 
C     .	        GNTOGUU,GNTOGDD, 
C     .	        GNTOCLV,GNTOCUD, 
C     .         GNTOLLL,GNTOLLR,GNTOVSV,
C     .         GNTOUUL,GNTOUUR,GNTODDL,GNTODDR, 
C     .		GNTONP,
C     .		GNTONLH, GNTONHH, 
C     .		GNTONA, GNTOCHPM, 
C     .		WNEUT 
C  S.A. note: all the above is not needed here 
	COMMON/XMASSES/IEPSI,IEPSJ,MI,MJ    	! Commons 
	COMMON/LOOPMS/MF,MB 			! with the 
	COMMON/LOOPBSN/ISPIN 			! the radmatel 
	COMMON/LOOPCPLS/LLRR,LRRL,EF,CF 	! subroutine:  
	COMMON/RADRES/ELM   			! internal use 
                                                ! S.A.                 
C S.A. note: the common below is for the approx. calculation 
C            here not needed 
C	COMMON/RADEC/BNTONPA,GNTONPA,RT,ICASE   
	COMMON/IMESSAGE/IMSG !S.A. note: 
			     !This is read via steer card 
c
c to satisfy the compiler S.K 
c
        data rt/0/
    
	F(X)   = (1.+X*LOG(X)/(1.-X))/(1.-X) 

C
C       renaming input parameters
C
        m0=fm0
        m2=fm2
        mu=fmu
        tgb=ftgb
        mtop=fmtop

	RAD2	= SQRT(2.) 

	SIN2W   = SIN2WI-1.03D-7*(MTOP**2-138.**2) 
	COS2W	= 1. - SIN2W 	
	SINW 	= SQRT(SIN2W) 
	COSW    = SQRT(COS2W) 
	TGW	= SINW/COSW  

	E	= SQRT(4.*PI*ALFEM) 
	G	= SQRT(4.*PI*ALFEM/SIN2W)

	SINB    = TGB/SQRT(1.+TGB**2)    !Assuming pi/4<beta<pi/2
	COSB	= 1./SQRT(1.+TGB**2)     !Assuming pi/4<beta<pi/2
	COTB    = 1./TGB 
	SIN2B   = 2.*TGB/(1. + TGB**2)
	COS2B   = (1. - TGB**2)/(1. + TGB**2)

C
C	m1 not really needed.    
C

        M1 = M2*5./3.*TGW**2

**************************************************
C Because of the different convention for the 
C order of indices in ZR and in NMX matrices 
C the following operation is needed in order 
C to get correct results in my routines by using 
C the ZR matrix from SUSYGEN. S.A. 
**************************************************

	DO I = 1,4 
	 DO J = 1,4 
	  NMX(I,J) = ZR(J,I) 
	 ENDDO 
	ENDDO 

**************************************************
C Neutralino matrix in a convenient basis for 
C radiative decay purpose   S.A. 
**************************************************
C	NMX are in the -i~g -i~Z  ~Ha ~Hb basis 
C		where ~Ha = ~H1*cosb - ~H2*sinb
C		      ~Hb = ~H1*sinb + ~H2*cosb 
C	Z   are in the -i~B -i~W3 ~H1 ~H2 basis
************************************************** 

	DO JJ=1,4 
	 Z(JJ,1) =  COSW*NMX(JJ,1)-SINW*NMX(JJ,2) 
	 Z(JJ,2) =  SINW*NMX(JJ,1)+COSW*NMX(JJ,2) 
	 Z(JJ,3) =  COSB*NMX(JJ,3)+SINB*NMX(JJ,4) 
	 Z(JJ,4) = -SINB*NMX(JJ,3)+COSB*NMX(JJ,4) 
	ENDDO  

C These below are other useful definitions S.A. 

	DO JJ = 1,4 
	 ZP(JJ)	= Z(JJ,2)+Z(JJ,1)*TGW
	 ZM(JJ)	= Z(JJ,2)-Z(JJ,1)*TGW
	ENDDO 

C S.A. note: it is useful to introduce chargino mass with sign 
C            (see below) 
        do kk = 1,2 
         mchar(kk) = inc(kk)*amchar(kk) 
	enddo 

	ELMTOT 	= 0.0D0 

****************************************
*******	 EXACT CALCULATION  ************
****************************************

	IEPSI	= INN(IFX)
	IEPSJ	= INN(IDX)
	MI	= AMNEUT(IFX)
	MJ	= AMNEUT(IDX)

	IF ((IDX.GT.IFX).AND.(AMNEUT(IDX).GT.0.0D0)) THEN 
	
***************************************************
*	"Vector triangles" with W's & charginos 
***************************************************

	ISPIN	= 1 
	MB	= MW 

**Light chargino

	MF 	= MCHAR(1)  	!Here a different convention is used 
				!for U, V matrices with respect 
				!to NPB 323 (1989) 267. There 
				!Haber and Wyler use U, V such that 
				!charginos have positive masses
				!IN ORDER TO OBTAIN THE SAME RESULTS 
				!HERE THE MASS WITH SIGN HAS TO BE USED  
				!CHECK ALSO FOR NEUTRALINO MATRIX!! 
 
	LLRR	= Z(IFX,2)*Z(IDX,2)*(V(1,1)**2-U(1,1)**2)
     .    +(Z(IFX,4)*Z(IDX,4)*V(1,2)**2-Z(IFX,3)*Z(IDX,3)*U(1,2)**2)/2. 
     .    -V(1,1)*V(1,2)*(Z(IFX,2)*Z(IDX,4)+Z(IFX,4)*Z(IDX,2))/RAD2 
     .	  -U(1,1)*U(1,2)*(Z(IFX,2)*Z(IDX,3)+Z(IFX,3)*Z(IDX,2))/RAD2 
	LRRL	= U(1,2)*V(1,2)*(Z(IFX,3)*Z(IDX,4)-Z(IFX,4)*Z(IDX,3))/2.
     .	  +U(1,1)*V(1,2)*(Z(IFX,2)*Z(IDX,4)-Z(IFX,4)*Z(IDX,2))/RAD2 
     .	  +U(1,2)*V(1,1)*(Z(IFX,2)*Z(IDX,3)-Z(IFX,3)*Z(IDX,2))/RAD2 

	IF (MJ.GE.(MB+ABS(MF))) THEN 
         IF (IMSG) THEN  
	  PRINT *, 'WARNING: on-shell particles in radiative decay loop'
	  PRINT *, '  Decaying Neutralino(',IDX,') Mass (GeV) = ', MJ 
	  PRINT *, '  Final    Neutralino(',IFX,') Mass (GeV) = ', MI 
	  PRINT *, '  Light Chargino Mass (GeV) = ', MF,' & W' 
	  PRINT *, 'SUSY POINT: ', M2, MU  	 
	 ENDIF 
	 GNTONP(IDX,IFX) = 0.0D0 
	 ogntonp = 0.0d0 
	 RETURN 
	ENDIF 
	CALL RADMATEL 

	ELMTOT = ELMTOT + ELM		

**Heavy chargino

	MF 	= MCHAR(2)

	LLRR	= Z(IFX,2)*Z(IDX,2)*(V(2,1)**2-U(2,1)**2)
     .    +(Z(IFX,4)*Z(IDX,4)*V(2,2)**2-Z(IFX,3)*Z(IDX,3)*U(2,2)**2)/2. 
     .    -V(2,1)*V(2,2)*(Z(IFX,2)*Z(IDX,4)+Z(IFX,4)*Z(IDX,2))/RAD2 
     .	  -U(2,1)*U(2,2)*(Z(IFX,2)*Z(IDX,3)+Z(IFX,3)*Z(IDX,2))/RAD2 
	LRRL	= U(2,2)*V(2,2)*(Z(IFX,3)*Z(IDX,4)-Z(IFX,4)*Z(IDX,3))/2.
     .	  +U(2,1)*V(2,2)*(Z(IFX,2)*Z(IDX,4)-Z(IFX,4)*Z(IDX,2))/RAD2 
     .	  +U(2,2)*V(2,1)*(Z(IFX,2)*Z(IDX,3)-Z(IFX,3)*Z(IDX,2))/RAD2 

	IF (MJ.GE.(MB+ABS(MF))) THEN 
         IF (IMSG) THEN  
	  PRINT *, 'WARNING: on-shell particles in radiative decay loop'
	  PRINT *, '  Decaying Neutralino',(IDX),'  = ', MJ 
	  PRINT *, '  Final    Neutralino',(IFX),'  = ', MI 
	  PRINT *, '  Heavy Chargino = ', MF,' & W' 
	  PRINT *, 'SUSY POINT: ', M2, MU  	 
	 ENDIF 
	 GNTONP(IDX,IFX) = 0.0D0 
	 ogntonp = 0.0d0 
	 RETURN 
	ENDIF 
	CALL RADMATEL 

	ELMTOT = ELMTOT + ELM		

***********************************************************
*	"Scalar triangles" with charged Higgses & charginos 
***********************************************************

	ISPIN	= 0  
	MB	= MHPM 

	EF	= 1. 
	CF	= 1. 

**Light chargino

	MF 	= MCHAR(1)

	LRRL	= 2.*COSB**2*(Z(IFX,4)*V(1,1)+ZP(IFX)*V(1,2)/RAD2)
     .    *(Z(IDX,4)*V(1,1)+ZP(IDX)*V(1,2)/RAD2) 
     .		 -2.*SINB**2*(Z(IFX,3)*U(1,1)-ZP(IFX)*U(1,2)/RAD2) 
     .    *(Z(IDX,3)*U(1,1)-ZP(IDX)*U(1,2)/RAD2) 		
	LLRR 	= SIN2B*(U(1,1)*V(1,1)
     .    *(Z(IFX,4)*Z(IDX,3)-Z(IFX,3)*Z(IDX,4)) 
     .   	 -ZP(IDX)*(Z(IFX,3)*U(1,1)*V(1,2)
     .    +Z(IFX,4)*U(1,2)*V(1,1))/RAD2 
     .           +ZP(IFX)*(Z(IDX,3)*U(1,1)*V(1,2) 
     .    +Z(IDX,4)*U(1,2)*V(1,1))/RAD2) 


	IF (MJ.GE.(MB+ABS(MF))) THEN 
         IF (IMSG) THEN 
	  PRINT *, 'WARNING: on-shell particles in radiative decay loop'
	  PRINT *, '  Decaying Neutralino(',IDX,') Mass (GeV) = ', MJ 
	  PRINT *, '  Final    Neutralino(',IFX,') Mass (GeV) = ', MI 
	  PRINT *, '  Light Chargino Mass (GeV) = ', MF 
	  PRINT *, ' & Charged Higgs Mass (GeV) = ', MB   
	  PRINT *, 'SUSY POINT: ', M2, MU  	 
	 ENDIF 
	 GNTONP(IDX,IFX) = 0.0D0 
	 ogntonp = 0.0d0 
	 RETURN 
	ENDIF 
	CALL RADMATEL 

	ELMTOT = ELMTOT + ELM		

**Heavy chargino

	MF 	= MCHAR(2)

	LRRL	= 2.*COSB**2*(Z(IFX,4)*V(2,1)+ZP(IFX)*V(2,2)/RAD2)
     .    *(Z(IDX,4)*V(2,1)+ZP(IDX)*V(2,2)/RAD2) 
     .		 -2.*SINB**2*(Z(IFX,3)*U(2,1)-ZP(IFX)*U(2,2)/RAD2) 
     .    *(Z(IDX,3)*U(2,1)-ZP(IDX)*U(2,2)/RAD2) 		
	LLRR 	= SIN2B*(U(2,1)*V(2,1)
     .    *(Z(IFX,4)*Z(IDX,3)-Z(IFX,3)*Z(IDX,4)) 
     .   	 -ZP(IDX)*(Z(IFX,3)*U(2,1)*V(2,2)
     .    +Z(IFX,4)*U(2,2)*V(2,1))/RAD2 
     .           +ZP(IFX)*(Z(IDX,3)*U(2,1)*V(2,2) 
     .    +Z(IDX,4)*U(2,2)*V(2,1))/RAD2) 

	IF (MJ.GE.(MB+ABS(MF))) THEN 
         IF (IMSG) THEN 
	  PRINT *, 'WARNING: on-shell particles in radiative decay loop'
	  PRINT *, '  Decaying Neutralino(',IDX,') Mass (GeV) = ', MJ 
	  PRINT *, '  Final    Neutralino(',IFX,') Mass (GeV) = ', MI 
	  PRINT *, '  Heavy Chargino Mass (GeV) = ', MF
	  PRINT *, ' & Charged Higgs Mass (GeV) = ', MB   
	  PRINT *, 'SUSY POINT: ', M2, MU  	 
	 ENDIF 
	 GNTONP(IDX,IFX) = 0.0D0 
	 ogntonp = 0.0d0 
	 RETURN 
	ENDIF 
	CALL RADMATEL 

	ELMTOT = ELMTOT + ELM		

***************************************************************
*  "Scalar triangles" with charged Goldstone bosons & charginos 
*************************************************************** 

	ISPIN	= 0  
	MB	= MW  

	EF 	= 1. 
	CF	= 1. 

**Light chargino

	MF 	= MCHAR(1)

	LRRL	= 2.*SINB**2*(Z(IFX,4)*V(1,1)+ZP(IFX)*V(1,2)/RAD2)
     .    *(Z(IDX,4)*V(1,1)+ZP(IDX)*V(1,2)/RAD2) 
     .		 -2.*COSB**2*(Z(IFX,3)*U(1,1)-ZP(IFX)*U(1,2)/RAD2) 
     .    *(Z(IDX,3)*U(1,1)-ZP(IDX)*U(1,2)/RAD2) 		
	LLRR 	= -SIN2B*(U(1,1)*V(1,1)
     .    *(Z(IFX,4)*Z(IDX,3)-Z(IFX,3)*Z(IDX,4)) 
     .   	 -ZP(IDX)*(Z(IFX,3)*U(1,1)*V(1,2)
     .    +Z(IFX,4)*U(1,2)*V(1,1))/RAD2 
     .           +ZP(IFX)*(Z(IDX,3)*U(1,1)*V(1,2) 
     .    +Z(IDX,4)*U(1,2)*V(1,1))/RAD2) 

	IF (MJ.GE.(MB+ABS(MF))) THEN 
         IF (IMSG) THEN 
	  PRINT *, 'WARNING: on-shell particles in radiative decay loop'
	 ENDIF 
	 GNTONP(IDX,IFX) = 0.0D0 
	 ogntonp = 0.0d0 
	 RETURN 
	ENDIF 
	CALL RADMATEL 

	ELMTOT = ELMTOT + ELM		

**Heavy chargino

	MF 	= MCHAR(2)

	LRRL	= 2.*SINB**2*(Z(IFX,4)*V(2,1)+ZP(IFX)*V(2,2)/RAD2)
     .    *(Z(IDX,4)*V(2,1)+ZP(IDX)*V(2,2)/RAD2) 
     .		 -2.*COSB**2*(Z(IFX,3)*U(2,1)-ZP(IFX)*U(2,2)/RAD2) 
     .    *(Z(IDX,3)*U(2,1)-ZP(IDX)*U(2,2)/RAD2) 		
	LLRR 	= -SIN2B*(U(2,1)*V(2,1)
     .    *(Z(IFX,4)*Z(IDX,3)-Z(IFX,3)*Z(IDX,4)) 
     .   	 -ZP(IDX)*(Z(IFX,3)*U(2,1)*V(2,2)
     .    +Z(IFX,4)*U(2,2)*V(2,1))/RAD2 
     .           +ZP(IFX)*(Z(IDX,3)*U(2,1)*V(2,2) 
     .    +Z(IDX,4)*U(2,2)*V(2,1))/RAD2) 

	IF (MJ.GE.(MB+ABS(MF))) THEN 
         IF (IMSG) THEN 
	  PRINT *, 'WARNING: on-shell particles in radiative decay loop'
	 ENDIF 
	 GNTONP(IDX,IFX) = 0.0D0 
	 ogntonp = 0.0d0 
	 RETURN 
	ENDIF 
	CALL RADMATEL 

	ELMTOT = ELMTOT + ELM		
	
****************************************************************
*  "Scalar triangles" with fermions & sfermions (L-R degenerate)
**************************************************************** 

	ISPIN	= 0  

**Up-type quark-squark 

	EF 	= 2./3.  
	CF	= 3. 

*Up 
	MB	= (MSUPL+MSUPR)/2. 
	MF 	= MUP  

 	LLRR	= (-2.*MF/MW/SINB)*
     .		    (Z(IFX,4)*((1./2.)*ZM(IDX)+(2./3.)*TGW*Z(IDX,1))
     .		    -Z(IDX,4)*((1./2.)*ZM(IFX)+(2./3.)*TGW*Z(IFX,1)) 
     .		    -(2./3.)*TGW*(Z(IDX,4)*Z(IFX,1)-Z(IFX,4)*Z(IDX,1))) 
	LRRL	= ZM(IFX)*ZM(IDX)+4.*(1./2.)*(2./3.)*TGW*
     .		  (Z(IFX,1)*ZM(IDX)+Z(IDX,1)*ZM(IFX)) 

	IF (MJ.GE.(MB+ABS(MF))) THEN 
         IF (IMSG) THEN 
	  PRINT *, 'WARNING: on-shell particles in radiative decay loop'
	  PRINT *, '  Decaying Neutralino(',IDX,') Mass (GeV) = ', MJ 
	  PRINT *, '  Final    Neutralino(',IFX,') Mass (GeV) = ', MI 
	  PRINT *, '  Up-squark Mass (GeV) = ', MB, ' & Up-quark' 
	  PRINT *, 'SUSY POINT: ', M2, MU  	 
	 ENDIF 
	 GNTONP(IDX,IFX) = 0.0D0 
	 ogntonp = 0.0d0 
	 RETURN 
	ENDIF 
	CALL RADMATEL 

	ELMTOT = ELMTOT + ELM		

*Charm 

	MB	= (MSCHL+MSCHR)/2. 
	MF 	= MCH 

 	LLRR	= (-2.*MF/MW/SINB)*
     .		    (Z(IFX,4)*((1./2.)*ZM(IDX)+(2./3.)*TGW*Z(IDX,1))
     .		    -Z(IDX,4)*((1./2.)*ZM(IFX)+(2./3.)*TGW*Z(IFX,1)) 
     .		    -(2./3.)*TGW*(Z(IDX,4)*Z(IFX,1)-Z(IFX,4)*Z(IDX,1))) 

	IF (MJ.GE.(MB+ABS(MF))) THEN 
	 IF (IMSG) THEN 
	  PRINT *, 'WARNING: on-shell particles in radiative decay loop'
	 ENDIF 
	 GNTONP(IDX,IFX) = 0.0D0 
	 ogntonp = 0.0d0 
	 RETURN
	ENDIF 
	CALL RADMATEL 

	ELMTOT = ELMTOT + ELM 

*Top 

	MB	= (MSTOPL+MSTOPR)/2. 
	MF 	= MTOP 

 	LLRR	= (-2.*MF/MW/SINB)*
     .		    (Z(IFX,4)*((1./2.)*ZM(IDX)+(2./3.)*TGW*Z(IDX,1))
     .		    -Z(IDX,4)*((1./2.)*ZM(IFX)+(2./3.)*TGW*Z(IFX,1)) 
     .		    -(2./3.)*TGW*(Z(IDX,4)*Z(IFX,1)-Z(IFX,4)*Z(IDX,1))) 

	IF (MJ.GE.(MB+ABS(MF))) THEN 
	 IF (IMSG) THEN 
	  PRINT *, 'WARNING: on-shell particles in radiative decay loop'
	 ENDIF  
	 GNTONP(IDX,IFX) = 0.0D0 
	 ogntonp = 0.0d0 
	 RETURN
	ENDIF 
	CALL RADMATEL 
	
	ELMTOT = ELMTOT + ELM 
	
**Down-type quark-squark 

	EF 	= -1./3.  
	CF	= 3. 

*Down

	MB	= (MSDOL+MSDOR)/2. 
	MF 	= MDO 

	LLRR	= (-2.*MF/MW/COSB)*
     .		   (Z(IFX,3)*((-1./2.)*ZM(IDX)+(-1./3.)*TGW*Z(IDX,1))
     .		   -Z(IDX,3)*((-1./2.)*ZM(IFX)+(-1./3.)*TGW*Z(IFX,1)) 
     .		   -(-1./3.)*TGW*(Z(IDX,3)*Z(IFX,1)-Z(IFX,3)*Z(IDX,1)))
	LRRL	= ZM(IFX)*ZM(IDX)+4.*(-1./2.)*(-1./3.)*TGW*
     .		  (Z(IFX,1)*ZM(IDX)+Z(IDX,1)*ZM(IFX)) 

	IF (MJ.GE.(MB+ABS(MF))) THEN 
	 IF (IMSG) THEN 
	  PRINT *, 'WARNING: on-shell particles in radiative decay loop'
	  PRINT *, '  Decaying Neutralino(',IDX,') Mass (GeV) = ', MJ 
	  PRINT *, '  Final    Neutralino(',IFX,') Mass (GeV) = ', MI 
	  PRINT *, '  Down-squark Mass (GeV) = ', MB, ' & Down-quark' 
	  PRINT *, 'SUSY POINT: ', M2, MU  	 
	 ENDIF  
	 GNTONP(IDX,IFX) = 0.0D0 
	 ogntonp = 0.0d0 
	 RETURN
	ENDIF 
	CALL RADMATEL 

	ELMTOT = ELMTOT + ELM		

*Strange  

	MB	= (MSSTL+MSSTR)/2. 
	MF 	= MST

	LLRR	= (-2.*MF/MW/COSB)*
     .		   (Z(IFX,3)*((-1./2.)*ZM(IDX)+(-1./3.)*TGW*Z(IDX,1))
     .		   -Z(IDX,3)*((-1./2.)*ZM(IFX)+(-1./3.)*TGW*Z(IFX,1)) 
     .		   -(-1./3.)*TGW*(Z(IDX,3)*Z(IFX,1)-Z(IFX,3)*Z(IDX,1)))

	IF (MJ.GE.(MB+ABS(MF))) THEN 
	 IF (IMSG) THEN 
	  PRINT *, 'WARNING: on-shell particles in radiative decay loop'
 	 ENDIF  
	 GNTONP(IDX,IFX) = 0.0D0 
	 ogntonp = 0.0d0 
	 RETURN 
	ENDIF 
	CALL RADMATEL 

	ELMTOT = ELMTOT + ELM 

*Bottom

	MB	= (MSBOL+MSBOR)/2. 
	MF 	= MBO 

	LLRR	= (-2.*MF/MW/COSB)*
     .		   (Z(IFX,3)*((-1./2.)*ZM(IDX)+(-1./3.)*TGW*Z(IDX,1))
     .		   -Z(IDX,3)*((-1./2.)*ZM(IFX)+(-1./3.)*TGW*Z(IFX,1)) 
     .		   -(-1./3.)*TGW*(Z(IDX,3)*Z(IFX,1)-Z(IFX,3)*Z(IDX,1)))

	IF (MJ.GE.(MB+ABS(MF))) THEN 
	 IF (IMSG) THEN 
	  PRINT *, 'WARNING: on-shell particles in radiative decay loop'
	 ENDIF 
	 GNTONP(IDX,IFX) = 0.0D0 
	 ogntonp = 0.0d0 
	 RETURN 
	ENDIF 
	CALL RADMATEL 
	
	ELMTOT = ELMTOT + ELM 

**Charged leptons-sleptons 

	EF 	= -1.  
	CF	= 1. 

*Electron 

	MB	= (MSEL+MSER)/2. 
	MF 	= ME 

	LLRR	= (-2.*MF/MW/COSB)*
     .		   (Z(IFX,3)*((-1./2.)*ZM(IDX)+(-1.)*TGW*Z(IDX,1))
     .		   -Z(IDX,3)*((-1./2.)*ZM(IFX)+(-1.)*TGW*Z(IFX,1)) 
     .		   -(-1.)*TGW*(Z(IDX,3)*Z(IFX,1)-Z(IFX,3)*Z(IDX,1)))
	LRRL	= ZM(IFX)*ZM(IDX)+4.*(-1./2.)*(-1.)*TGW*
     .		  (Z(IFX,1)*ZM(IDX)+Z(IDX,1)*ZM(IFX)) 

	IF (MJ.GE.(MB+ABS(MF))) THEN 
	 IF (IMSG) THEN 
	  PRINT *, 'WARNING: on-shell particles in radiative decay loop'
	  PRINT *, '  Decaying Neutralino(',IDX,') Mass (GeV) = ', MJ 
	  PRINT *, '  Final    Neutralino(',IFX,') Mass (GeV) = ', MI 
	  PRINT *, '  Ch.slepton Mass (GeV) = ', MB, ' & Ch.lepton' 
	  PRINT *, 'SUSY POINT: ', M2, MU  	 
	 ENDIF 
	 GNTONP(IDX,IFX) = 0.0D0 
	 ogntonp = 0.0d0 
	 RETURN 
	ENDIF 
	CALL RADMATEL 

	ELMTOT	= ELMTOT+ELM		
	
*Muon

	MB	= (MSMUL+MSMUR)/2. 
	MF 	= MMU 

	LLRR	= (-2.*MF/MW/COSB)*
     .		   (Z(IFX,3)*((-1./2.)*ZM(IDX)+(-1.)*TGW*Z(IDX,1))
     .		   -Z(IDX,3)*((-1./2.)*ZM(IFX)+(-1.)*TGW*Z(IFX,1)) 
     .		   -(-1.)*TGW*(Z(IDX,3)*Z(IFX,1)-Z(IFX,3)*Z(IDX,1)))

	IF (MJ.GE.(MB+ABS(MF))) THEN 
	 IF (IMSG) THEN 
	  PRINT *, 'WARNING: on-shell particles in radiative decay loop'
	 ENDIF 
	 GNTONP(IDX,IFX) = 0.0D0 
	 ogntonp = 0.0d0 
	 RETURN 
	ENDIF 
	CALL RADMATEL 
	
	ELMTOT = ELMTOT + ELM 

*Tau

	MB	= (MSTAUL+MSTAUR)/2. 
	MF 	= MTAU 

	LLRR	= (-2.*MF/MW/COSB)*
     .		   (Z(IFX,3)*((-1./2.)*ZM(IDX)+(-1.)*TGW*Z(IDX,1))
     .		   -Z(IDX,3)*((-1./2.)*ZM(IFX)+(-1.)*TGW*Z(IFX,1)) 
     .		   -(-1.)*TGW*(Z(IDX,3)*Z(IFX,1)-Z(IFX,3)*Z(IDX,1)))

	IF (MJ.GE.(MB+ABS(MF))) THEN 
	 IF (IMSG) THEN 
	  PRINT *, 'WARNING: on-shell particles in radiative decay loop'
	 ENDIF 
	 GNTONP(IDX,IFX) = 0.0D0 
	 ogntonp = 0.0d0 
	 RETURN 
	ENDIF 
	CALL RADMATEL 

	ELMTOT = ELMTOT + ELM 

*******************************************************
* Total width for the radiative decay 
*******************************************************

	 GNTONP(IDX,IFX) = E**2*G**4*(MJ**2-MI**2)**3*ELMTOT**2
     .                     /8**3/PI**5/MJ**5 
	ELSE 
	 GNTONP(IDX,IFX) = 0.0D0 
	ENDIF 
	
C Here is where the subroutine interacts with susygen 
	 ogntonp = gntonp(idx,ifx)
	
**************************************************************************
*	APPROXIMATE CALCULATION IN SPECIAL CASES FOR LIGHT NEUTRALINOS 
******* Case  +-1 recognition ******************************************** 

	IF (IDX.EQ.2) THEN 
	 
	 IFX = 1 

	 MFOT 	= M2*SIN2W + M1*COS2W 
	 MHB    = ABS(MU)*SIN2B 

	 IF (MFOT.EQ.0.0) MFOT = 1.D-8 
	 IF (MHB.EQ.0.0)  MHB  = 1.D-8 

	 IF ((ABS((AMNEUT(2)-MFOT)/MFOT).LE.RT)   
     &	    .AND.(ABS((AMNEUT(1)-MHB)/MHB).LE.RT)) THEN 
	  FLAGMFHB = .TRUE. 
	 ELSE 
	  FLAGMFHB = .FALSE. 
	 ENDIF 
	 IF ((ABS((AMNEUT(2)-MHB)/MHB).LE.RT) 
     &      .AND.(ABS((AMNEUT(1)-MFOT)/MFOT).LE.RT)) THEN 
	  FLAGMHBF = .TRUE. 
	 ELSE 
	  FLAGMHBF = .FALSE. 
	 ENDIF 
	 IF (((NMX(2,1)**2).GE.(1.-RT))
     &      .AND.((NMX(1,4)**2).GE.(1.-RT))) THEN 
	  FLAGNFHB = .TRUE. 
	 ELSE 
	  FLAGNFHB = .FALSE. 
	 ENDIF 
	 IF (((NMX(1,1)**2).GE.(1.-RT))
     &      .AND.((NMX(2,4)**2).GE.(1.-RT))) THEN 
	  FLAGNHBF = .TRUE. 
	 ELSE 
	  FLAGNHBF = .FALSE. 
	 ENDIF 
	 IF ((INN(2).EQ.1).AND.(INN(1).EQ.(-SIGN(1.D0,MU)))) THEN 
	  FLAGEFHB = .TRUE. 
	 ELSE 
	  FLAGEFHB = .FALSE. 
	 ENDIF 
	 IF ((INN(1).EQ.1).AND.(INN(2).EQ.(-SIGN(1.D0,MU)))) THEN 
	  FLAGEHBF = .TRUE. 
	 ELSE 
	  FLAGEHBF = .FALSE. 
	 ENDIF 

******* Case 2 recognition **************************************	

	 IF ((M1.EQ.0.0).OR.(M2.EQ.0.0)) THEN 
	  ICASE = 0 
	  GOTO 300 
	 ENDIF 

	 MH12	= MZ**2*(SIN2W/M1+COS2W/M2) 

	 IF (MH12.EQ.0.0) MH12 = 1.D-8 

	 IF (ABS((AMNEUT(2)-AMNEUT(1)-MH12)/MH12).LE.RT) THEN 
	  	 ELSE 
	  FLAGMHH = .FALSE. 
	 ENDIF 
	 IF ((ABS((Z(2,3)**2-0.5)/0.5).LE.RT)  
     &	    .AND.(ABS((Z(2,4)**2-0.5)/0.5).LE.RT) 
     &      .AND.(ABS((Z(1,3)**2-0.5)/0.5).LE.RT)  
     &	    .AND.(ABS((Z(1,4)**2-0.5)/0.5).LE.RT)) THEN 
	  FLAGNHH = .TRUE. 	 
	 ELSE 
	  FLAGNHH = .FALSE. 
	 ENDIF 
	 IF ((INN(2).EQ.-1).AND.(INN(1).EQ.1)) THEN 
	  FLAGEHH = .TRUE. 
	 ELSE 
	  FLAGEHH = .FALSE. 
	 ENDIF 

	 IF ((FLAGMFHB).AND.(FLAGNFHB).AND.(FLAGEFHB)) THEN 
	  ICASE = 1 
     	 ELSE IF ((FLAGMHBF).AND.(FLAGNHBF).AND.(FLAGEHBF)) THEN 
	  ICASE = -1 
	 ELSE IF ((FLAGMHH).AND.(FLAGNHH).AND.(FLAGEHH)) THEN 
	  ICASE = 2
	 ELSE 
	  ICASE = 0 
	 ENDIF 	 

	 IF (ICASE.EQ.0) THEN 
	  GNTONPA(IDX,IFX) = 0.0D0
	  GOTO 300  
	 ELSE IF (ABS(ICASE).EQ.1) THEN 
          GOTO 100 
	 ELSE IF (ICASE.EQ.2) THEN 
          GOTO 200 
	 ENDIF 

******* Case  ±1 radiative width computation *************************

 100     C = 3.*(2./3.)**2*COTB
     .        *(F(MSTOPL**2/MTOP**2)+F(MSTOPR**2/MTOP**2)) 
     .	   +(SIN2B/2.) 
     .		*(F(MHPM**2/MCHAR(1)**2)+F(MHPM**2/MCHAR(2)**2) 
     .           +3.*F(MW**2/MCHAR(1)**2)+3.*F(MW**2/MCHAR(2)**2) 
     .		 -8.) 

*	 Approximate width in case ±1  

	 GNTONPA(IDX,IFX) = 
     .		(C**2*E**4*G**2/256./8./PI**5/MW**2/AMNEUT(2)**3) 
     .		      *(AMNEUT(2)**2-AMNEUT(1)**2)**3 		

	 GOTO 300 

******* Case 2 radiative width computation *************************
	
 200 	 R	= (MW/MU)**2
	 IF (R.LE.4.0D0) THEN 
	  C	= 1. + (1.-R)*LOG(R)/2. + 
     .		  (R-3.)*ATAN(SQRT(4./R-1.))/SQRT(4./R-1.)
	  GNTONPA(IDX,IFX) = (C**2*E**2*G**4/256./PI**5/MU**2) 
     .		      *(AMNEUT(2)-AMNEUT(1))**3 
	 ELSE 
	  C 	= 0.0 
	  GNTONPA(IDX,IFX) = 0.0
	 ENDIF 

	ENDIF 


  300 	RETURN 


 	END 	

*****************************************************************
	SUBROUTINE RADMATEL 

	IMPLICIT DOUBLE PRECISION (A-H,L-Z)

	EXTERNAL AII, AJI, AI2I, AKI 

	COMMON/XMASSES/IEPSI,IEPSJ,MI,MJ
	COMMON/LOOPMS/MF,MB 
	COMMON/LOOPBSN/ISPIN 
	COMMON/LOOPCPLS/LLRR,LRRL,EF,CF 
	COMMON/RADRES/ELM  

	LAMI 	= (MF**2+MB**2-MI**2)**2-4.*MF**2*MB**2
	LAMJ 	= (MF**2+MB**2-MJ**2)**2-4.*MF**2*MB**2 

	AI 	=  DGAUSS(AII, 0.0D0,1.0D0,1.D-3)/(MJ**2-MI**2)  
	AJ 	=  DGAUSS(AJI, 0.0D0,1.0D0,1.D-3)/(MJ**2-MI**2)  
	AI2 	=  DGAUSS(AI2I,0.0D0,1.0D0,1.D-3)/(MJ**2-MI**2)  
	AK 	= -DGAUSS(AKI, 0.0D0,1.0D0,1.D-3)/(MJ**2-MI**2)  
	
	AILIM	= (1.-MB**2*LOG(MB**2/MF**2)/(MB**2-MF**2))/
     .		  (MB**2-MF**2) 
	AJLIM   = LOG(MF**2/MB**2)/(MB**2-MF**2)-AILIM 
	AI2LIM  = -((MF**2+MB**2)/2.-MF**2*MB**2*LOG(MB**2/MF**2)/  
     .            (MB**2-MF**2))/(MB**2-MF**2)**2 
	AKLIM   = AI2/2.

	AKCALC  = -(1.+MF**2*AI+MB**2*AJ-MJ**2*AI2)/(MJ**2-MI**2)

	IF (ISPIN.EQ.1) THEN 
	 ELM 	= IEPSI*MJ*
     .		  (LLRR*(IEPSJ*MJ*(AI2-AJ-AK)+IEPSI*MI*(AJ-AK)) 
     .		  +2.*MF*LRRL*AJ) 
	ELSE IF (ISPIN.EQ.0) THEN 
	 ELM 	= -IEPSI*MJ*EF*CF*
     .		   (LRRL*(IEPSJ*MJ*(AI2-AK)-IEPSI*MI*AK)
     .		   +MF*LLRR*AI)/4. 
	ELSE 
	 ELM = 0.0D0 
	ENDIF

	RETURN 

	END 

**************************************************************
	DOUBLE PRECISION FUNCTION AII(X) 

	IMPLICIT DOUBLE PRECISION (A-H,L-Z)

	COMMON/XMASSES/IEPSI,IEPSJ,MI,MJ
	COMMON/LOOPMS/MF,MB 

	LGNUM   = MF**2*X+MB**2*(1.-X)-MJ**2*X*(1.-X)
	LGDEN   = MF**2*X+MB**2*(1.-X)-MI**2*X*(1.-X)

	AII  	= LOG(LGNUM/LGDEN)/(1.-X)

	RETURN 

	END 

***************************************************************
	DOUBLE PRECISION FUNCTION AJI(X) 

	IMPLICIT DOUBLE PRECISION (A-H,L-Z)

	COMMON/XMASSES/IEPSI,IEPSJ,MI,MJ
	COMMON/LOOPMS/MF,MB 

	LGNUM   = MF**2*X+MB**2*(1.-X)-MJ**2*X*(1.-X)
	LGDEN   = MF**2*X+MB**2*(1.-X)-MI**2*X*(1.-X)

	AJI  	= LOG(LGNUM/LGDEN)/X  

	RETURN 

	END 

***************************************************************
	DOUBLE PRECISION FUNCTION AI2I(X) 

	IMPLICIT DOUBLE PRECISION (A-H,L-Z)

	COMMON/XMASSES/IEPSI,IEPSJ,MI,MJ
	COMMON/LOOPMS/MF,MB 

	LGNUM   = MF**2*X+MB**2*(1.-X)-MJ**2*X*(1.-X)
	LGDEN   = MF**2*X+MB**2*(1.-X)-MI**2*X*(1.-X)

	AI2I  	= LOG(LGNUM/LGDEN)  

	RETURN 

	END 

***************************************************************
	DOUBLE PRECISION FUNCTION AKI(X) 

	IMPLICIT DOUBLE PRECISION (A-H,L-Z)

	COMMON/XMASSES/IEPSI,IEPSJ,MI,MJ
	COMMON/LOOPMS/MF,MB 

	LGNUM   = MF**2*X+MB**2*(1.-X)-MJ**2*X*(1.-X)
	LGDEN   = MF**2*X+MB**2*(1.-X)-MI**2*X*(1.-X)

	NUM	= MF**2*X+MB**2*(1.-X)-MJ**2*X*(1.-X)
	DEN	= X*(1.-X)*(MJ**2-MI**2)

	AKI  	= 1.+NUM*LOG(LGNUM/LGDEN)/DEN   

	RETURN 

	END 



	SUBROUTINE NTOXH(INDEX1,INDEX2,fM0,fM2,fMU,fTGB,fMTOP,
     .		oGNTONLH,oGNTONHH,oGNTONA,oGNTOCHPM) 
**********************************************************************
C Computes widths for the two-body decays:  
C X°(i) --> X°±(j) h° or H° or A° or H±
C The Higgs masses are now computed in the HIGMAS routine (changed for 
C SUSYGEN)
**********************************************************************    
C by Sandro Ambrosanio (1994) 
**********************************************************************

	IMPLICIT DOUBLE PRECISION (A-H,L-Z)

	DOUBLE PRECISION INN, INC 
C S.A. note: 
C according to the declaration of these variables in all 
C other SUSYGEN routines 

        PARAMETER (PI=3.141592653589793238, 
     .	  MZ=91.19, WZ=2.49, 
     .    MW=80.22, WW=2.08, 
     .	  MVE=0.0, MVMU=0.0, MVTAU=0.0, 
     .    ME=0.511D-3, MMU=105.66D-3, MTAU=1.784, 
     .	  MUP=3.D-3, MDO=8.D-3, MST=.2, MCH=1.5, MBO=5., 
     .    ALFEM = 1./127.9, SIN2WI=.2324, 
     .    ALFAS = 0.12) 


	DIMENSION AMNEUT(4), NMX(4,4), ZR(4,4), INN(4), ZMX(4,4), 
     .		  AMCHAR(2), UMX(2,2), VMX(2,2), INC(2), 
     .		  GNTONLH(4,4), GNTONHH(4,4), 
     .		  GNTONA(4,4), GNTOCHPM(4,2) 

	DIMENSION CC(3), DD(3), IETA(3), F(4,4,3) 

C	COMMON/PARMS/M0,M2,MU,TGB,MTOP,DELALF,MA 
C	COMMON/GAUMASS/M12,M1,M3 
C S.A. note: the commons above are not needed 
C	COMMON/NEUTS/NEUT,ANEUT,MNEUT,AMNEUT,INN,NMX
C S.A. note: the above common is substituted by the below one 
C S.A. WARNING: here amneut and inn are WAS and ESA respectively 
C S.A. WARNING: BUT ZR HAVE REVERSED INDICES WITH RESPECT TO NMX!!! 
	common/neumix/ZR,amneut,inn,waste(64) 
	COMMON/ZEUTS/ZMX,NMX    !ZMX is the neutralino mixing matrix 
				!in another basis, calculated below 
				!NMX is ZR after reversing indices 
				!They are also used in the NTOXH routine 
C	COMMON/CHARS/CHAR,ACHAR,MCHAR,AMCHAR,INC,UMX,VMX 
C S.A. note: the above common is substituted by the below one  
	common/chamix/wwaste(8),vmx,umx,amchar,inc 
C S.A. WARNING: here amneut and inc are FM and ETA respectively 
C	COMMON/SFERMS/MSVE,MSVMU,MSVTAU,MSEL,MSER,MSMUL,MSMUR,MSTAUL,
C     .		     MSTAUR,MSUPL,MSUPR,MSDOL,MSDOR,MSSTL,MSSTR,
C     .		     MSCHL,MSCHR,MSBOL,MSBOR,MSTOPL,MSTOPR
C S.A. note: the above common is substituted by the below one  
      common/spartcl/
     +     msupl,msdol,msve,msel,mschl,msstl,msvmu,msmul,mstopl,
     +     msbol,msvtau,mstaul,
     +     msupr,msdor,msver,mser,mschr,msstr,msvmur,msmur,mstopr,
     +     msbor,msvtaur,mstaur,
     +     ratq(12),fgamc(12),fgamcr(12),cosmi(12)
C	COMMON/SFERMIX/MSTAU1,MSTAU2,MSTOP1,MSTOP2,MSBO1,MSBO2	
C S.A. note: the Higgs masses and mixing are now calculated in the 
C            HIGMAS routine  
      common/higgses/mlh,ma,mhh,mhpm,sina,cosa
C	COMMON/NWIDTHS/GNTONLL,GNTONVV,GNTONUU,GNTONDD, 
C     .	        GNTOGUU,GNTOGDD, 
C     .	        GNTOCLV,GNTOCUD, 
C     .         GNTOLLL,GNTOLLR,GNTOVSV,
C     .         GNTOUUL,GNTOUUR,GNTODDL,GNTODDR, 
C     .		GNTONP,
C     .		GNTONLH, GNTONHH, 
C     .		GNTONA, GNTOCHPM, 
C     .		WNEUT 
C	COMMON/HWIDTHS/GLHTOEE,GLHTOMM,GLHTOTT,
C     .		GLHTOUU,GLHTODD,GLHTOCC,GLHTOSS,
C     .		GLHTOBB,WLH,
C     .		GHHTOEE,GHHTOMM,GHHTOTT,
C     .		GHHTOUU,GHHTODD,GHHTOCC,GHHTOSS,
C     .		GHHTOBB,WHH,
C     .		GATOEE,GATOMM,GATOTT,
C     .		GATOUU,GATODD,GATOCC,GATOSS,
C     .		GATOBB,WA C
C	COMMON/HBRATIOS/BLHTOEE,BLHTOMM,BLHTOTT,
C     .		BLHTOUU,BLHTODD,BLHTOCC,BLHTOSS,
C     .		BLHTOBB,
C     .		BHHTOEE,BHHTOMM,BHHTOTT,
C     .		BHHTOUU,BHHTODD,BHHTOCC,BHHTOSS,
C     .		BHHTOBB, 
C     .		BATOEE,BATOMM,BATOTT,
C     .		BATOUU,BATODD,BATOCC,BATOSS,
C     .		BATOBB  
C  S.A. note: all the above is needed only if one wants to include 
C             the higgs decays 

C
C       renaming input parameters
C
        m0=fm0
        m2=fm2
        mu=fmu
        tgb=ftgb
        mtop=fmtop

	RAD2	= SQRT(2.)

	SIN2W   = SIN2WI-1.03D-7*(MTOP**2-138.**2) 
	COS2W	= 1. - SIN2W 	
	SINW	= SQRT(SIN2W) 
	COSW    = SQRT(COS2W)
	TGW	= SINW/COSW  

	G	= SQRT(4.*PI*ALFEM/SIN2W)

	SINB    = TGB/SQRT(1+TGB**2)    !Assuming pi/4<beta<pi/2
	COSB	= 1/SQRT(1+TGB**2)      !Assuming pi/4<beta<pi/2
	SINV	= COSB			!Assuming 0<thetav<pi/4
	COSV	= SINB			!Assuming 0<thetav<pi/4 
	COTB	= 1./TGB 
	COS2B   = (1. - TGB**2)/(1. + TGB**2) 
	SIN2B   = 2.*TGB/(1. + TGB**2) 

C
C	M1 IS NEEDED HERE!!!     
C

        M1 = M2*5./3.*TGW**2


*******  Computation of Higgs masses and parameters ***********
*******  S.A. NOTE: now this is performed in HIGMAS routine ***
C        V	= 2.*MZ*COSW/G 
C	V2      = SQRT(TGB**2/(1.+TGB**2))*V
C	HT	= RAD2*MTOP/V2 

C	MSTOP	= MSTOP1 
C	DELTA1  = 3.*LOG((MSTOP**2+MTOP**2)/MTOP**2)*(HT**2/4./PI)**2
C	MSTOP	= MSTOP2 
C	DELTA2  = 3.*LOG((MSTOP**2+MTOP**2)/MTOP**2)*(HT**2/4./PI)**2
C       DELTA   = (DELTA1 + DELTA2)/2. 
C	A 	= MA**2+MZ**2*SIN2B**2+(1./2.)*DELTA*SIN2B**2*V**2
C	B	= MZ**2*COS2B**2+2.*DELTA*SINB**4*V**2
C	C	= -MZ**2*SIN2B*COS2B+DELTA*SINB**2*SIN2B*V**2

C	MHPM	= SQRT(MA**2+MW**2) 
C	MLH	= SQRT(A+B-SQRT((A+B)**2-4.*(A*B-C**2)))/RAD2
C	MHH	= SQRT(A+B+SQRT((A+B)**2-4.*(A*B-C**2)))/RAD2
	 
C	COS2A   = -COS2B*(MA**2-MZ**2)/(MHH**2-MLH**2)
C	SIN2A	= -SIN2B*(MHH**2+MLH**2)/(MHH**2-MLH**2)

C	SINA	= SQRT(1-COS2A)/RAD2 
C	COSA 	= SQRT(1+COS2A)/RAD2 

**********************************************************************
*** Computation of widths for Neutralino --> Neutralino + Higgs decays
**********************************************************************

	CC(1)	= COSA*SINB-SINA*COSB 
	CC(2)	= COSA*COSB+SINA*SINB 
	CC(3)	= COS2B 

	DD(1)	= -SINA 
	DD(2)	=  COSA 
	DD(3) 	=  COSB 

	IETA(1)	= 1 
	IETA(2) = 1
	IETA(3) = -1

	DO I = 1,4 
	 DO J = 1,4 
	  DO K = 1,3  
 	   F(I,J,K)	= (CC(K)/2./SINB)* 
     .   	 (ZMX(I,3)*ZMX(J,2)+ZMX(J,3)*ZMX(I,2) 
     .	    -TGW*(ZMX(I,3)*ZMX(J,1)+ZMX(J,3)*ZMX(I,1))) 
     .			 +(DD(K)/2./MW/SINB)* 
     .           (M2*ZMX(I,2)*ZMX(J,2)+M1*ZMX(I,1)*ZMX(J,1)
     .	     -MU*(ZMX(I,3)*ZMX(J,4)+ZMX(I,4)*ZMX(J,3))) 
	  ENDDO 
	 ENDDO 
	ENDDO 

C This has been added 
C This is because in the main routine charginos have indices=5,6 
	IDX = INDEX1        
	IFX = INDEX2        
	IF ((IFX.GT.4).AND.(IDX.LE.4).AND.(IDX.GT.1)) THEN 
	 IFX = IFX-4 
	 GOTO 111 
	ENDIF 
*** Heavy CP-even Higgs H°
 
 	MH 	= MHH 
	IFH     = 1 	
 
C Here the loop is performed by the main routine 
C	DO IFX = 1,4,1 
	 IF ((AMNEUT(IDX)-AMNEUT(IFX)-MH).GT.0.0D0) THEN 
	  LMB = (AMNEUT(IDX)**2+AMNEUT(IFX)**2-MH**2)**2-
     .			4.*AMNEUT(IDX)**2*AMNEUT(IFX)**2 	
	  GNTONHH(IDX,IFX) = (G**2*SQRT(LMB)/32./PI/AMNEUT(IDX)**3)* 
     .	     ((F(IDX,IFX,IFH)**2+F(IFX,IDX,IFH)**2)* 
     .	     (AMNEUT(IDX)**2+AMNEUT(IFX)**2-MH**2) 
     .	      +4.*F(IDX,IFX,IFH)*F(IFX,IDX,IFH)*
     .	      INN(IDX)*INN(IFX)*IETA(IFH)*AMNEUT(IDX)*AMNEUT(IFX))
	 ELSE 
	  GNTONHH(IDX,IFX) = 0.0D0 
	 ENDIF 
C	ENDDO 

*** Light CP-even Higgs h°
 
 	MH 	= MLH 
	IFH     = 2 	 
C Here the loop is performed by the main routine 
C	DO IFX = 1,4,1 
	 IF ((AMNEUT(IDX)-AMNEUT(IFX)-MH).GT.0.0D0) THEN 
	  LMB = (AMNEUT(IDX)**2+AMNEUT(IFX)**2-MH**2)**2-
     .			4.*AMNEUT(IDX)**2*AMNEUT(IFX)**2 	
	  GNTONLH(IDX,IFX) = (G**2*SQRT(LMB)/32./PI/AMNEUT(IDX)**3)* 
     .	     ((F(IDX,IFX,IFH)**2+F(IFX,IDX,IFH)**2)* 
     .	     (AMNEUT(IDX)**2+AMNEUT(IFX)**2-MH**2) 
     .	      +4.*F(IDX,IFX,IFH)*F(IFX,IDX,IFH)*
     .	      INN(IDX)*INN(IFX)*IETA(IFH)*AMNEUT(IDX)*AMNEUT(IFX))
	 ELSE 
	  GNTONLH(IDX,IFX) = 0.0D0 
	 ENDIF 
C	ENDDO 

*** CP-odd Higgs A°
 
 	MH 	= MA 
	IFH     = 3	 
C Here the loop is performed by the main routine 
C	DO IFX = 1,4,1 
	 IF ((AMNEUT(IDX)-AMNEUT(IFX)-MH).GT.0.0D0) THEN 
	  LMB = (AMNEUT(IDX)**2+AMNEUT(IFX)**2-MH**2)**2-
     .			4.*AMNEUT(IDX)**2*AMNEUT(IFX)**2 	
	  GNTONA(IDX,IFX) = (G**2*SQRT(LMB)/32./PI/AMNEUT(IDX)**3)* 
     .	     ((F(IDX,IFX,IFH)**2+F(IFX,IDX,IFH)**2)* 
     .	     (AMNEUT(IDX)**2+AMNEUT(IFX)**2-MH**2) 
     .	      +4.*F(IDX,IFX,IFH)*F(IFX,IDX,IFH)*
     .	      INN(IDX)*INN(IFX)*IETA(IFH)*AMNEUT(IDX)*AMNEUT(IFX))
	 ELSE 
	  GNTONA(IDX,IFX) = 0.0D0 
	 ENDIF 
C	ENDDO 

 111    CONTINUE 
*** Charged decays X°(i) --> X±(j) H±

C This is to avoid IFX > 2 in decays to charginos 
	IF ((IFX.GT.2).OR.(IDX.GT.4).OR.(IDX.EQ.1)) RETURN 

C Here the loop is performed by the main routine 
C	DO IFX = 1,2,1
	 IF ((AMNEUT(IDX)-AMCHAR(IFX)-MHPM).GT.0.0D0) THEN 
	  GNTOCHPM(IDX,IFX) = 0.0D0  !IN PROGRESS.... 
	 ELSE 
	  GNTOCHPM(IDX,IFX) = 0.0D0 
	 ENDIF 
C 	ENDDO 

C Here is where the subroutine interacts with susygen 
        ogntonlh  = gntonlh(idx,ifx) 
        ogntonhh  = gntonhh(idx,ifx) 
        ogntona   = gntona(idx,ifx) 
        ogntochpm = gntochpm(idx,ifx) 

C all the code below is only needed for higgs decays 
*******************************************************
*** Computation of widths for Higgs --> f fbar decays
*******************************************************
* This are the dominant decays for Higgses in the 
* [2mb,2Mw] range as must be in this case 
* because they arise from a neutralino(2) decay 

*** Light CP-even Higgs H°

*Electrons 
	
	MH	= MLH 
	MF 	= ME 

	IF (MH.GT.(2.*MF)) THEN 
	 GLHTOEE = (G**2*MF**2*MH/32./PI/MW**2)*(SINA/COSB)**2 
     .			*(1.-4.*MF**2/MH**2)**(3./2.) 
	ELSE 
	 GLHTOEE = 0.0D0 
	ENDIF 

*Muons 
	
	MH	= MLH 
	MF 	= MMU

	IF (MH.GT.(2.*MF)) THEN 
	 GLHTOMM = (G**2*MF**2*MH/32./PI/MW**2)*(SINA/COSB)**2  
     .			*(1.-4.*MF**2/MH**2)**(3./2.) 
	ELSE 
	 GLHTOMM = 0.0D0 
	ENDIF 

*Tau's 
	
	MH	= MLH 
	MF 	= MTAU

	IF (MH.GT.(2.*MF)) THEN 
	 GLHTOTT = (G**2*MF**2*MH/32./PI/MW**2)*(SINA/COSB)**2  
     .			*(1.-4.*MF**2/MH**2)**(3./2.) 
	ELSE 
	 GLHTOTT = 0.0D0 
	ENDIF 

*Up-quarks 
	
	MH	= MLH 
	MF 	= MUP 

	IF (MH.GT.(2.*MF)) THEN 
	 GLHTOUU = (3.D0)*(G**2*MF**2*MH/32./PI/MW**2)*(COSA/SINB)**2  
     .			 *(1.-4.*MF**2/MH**2)**(3./2.) 
	ELSE 
	 GLHTOUU = 0.0D0 
	ENDIF 

*Charm-quarks 
	
	MH	= MLH 
	MF 	= MCH 

	IF (MH.GT.(2.*MF)) THEN 
	 GLHTOCC = (3.D0)*(G**2*MF**2*MH/32./PI/MW**2)*(COSA/SINB)**2  
     .			 *(1.-4.*MF**2/MH**2)**(3./2.) 
	ELSE 
	 GLHTOCC = 0.0D0 
	ENDIF 

*Down-quarks 
	
	MH	= MLH 
	MF 	= MDO 

	IF (MH.GT.(2.*MF)) THEN 
	 GLHTODD = (3.D0)*(G**2*MF**2*MH/32./PI/MW**2)*(SINA/COSB)**2  
     .			 *(1.-4.*MF**2/MH**2)**(3./2.) 
	ELSE 
	 GLHTODD = 0.0D0 
	ENDIF 

*Strange-quarks 
	
	MH	= MLH 
	MF 	= MST 

	IF (MH.GT.(2.*MF)) THEN 
	 GLHTOSS = (3.D0)*(G**2*MF**2*MH/32./PI/MW**2)*(SINA/COSB)**2  
     .			 *(1.-4.*MF**2/MH**2)**(3./2.) 
	ELSE 
	 GLHTOSS = 0.0D0 
	ENDIF 

*Bottom-quarks 
	
	MH	= MLH 
	MF 	= MBO 

	IF (MH.GT.(2.*MF)) THEN 
	 GLHTOBB = (3.D0)*(G**2*MF**2*MH/32./PI/MW**2)*(SINA/COSB)**2  
     .			 *(1.-4.*MF**2/MH**2)**(3./2.) 
	ELSE 
	 GLHTOBB = 0.0D0 
	ENDIF 

*** Heavy CP-even Higgs H°

*Electrons 
	
	MH	= MHH 
	MF 	= ME 

	IF (MH.GT.(2.*MF)) THEN 
	 GHHTOEE = (G**2*MF**2*MH/32./PI/MW**2)*(COSA/COSB)**2 
     .			*(1.-4.*MF**2/MH**2)**(3./2.) 
	ELSE 
	 GHHTOEE = 0.0D0 
	ENDIF 

*Muons 
	
	MH	= MHH 
	MF 	= MMU

	IF (MH.GT.(2.*MF)) THEN 
	 GHHTOMM = (G**2*MF**2*MH/32./PI/MW**2)*(COSA/COSB)**2  
     .			*(1.-4.*MF**2/MH**2)**(3./2.) 
	ELSE 
	 GHHTOMM = 0.0D0 
	ENDIF 

*Tau's 
	
	MH	= MHH 
	MF 	= MTAU

	IF (MH.GT.(2.*MF)) THEN 
	 GHHTOTT = (G**2*MF**2*MH/32./PI/MW**2)*(COSA/COSB)**2  
     .			*(1.-4.*MF**2/MH**2)**(3./2.) 
	ELSE 
	 GHHTOTT = 0.0D0 
	ENDIF 

*Up-quarks 
	
	MH	= MHH 
	MF 	= MUP 

	IF (MH.GT.(2.*MF)) THEN 
	 GHHTOUU = (3.D0)*(G**2*MF**2*MH/32./PI/MW**2)*(SINA/SINB)**2  
     .			 *(1.-4.*MF**2/MH**2)**(3./2.) 
	ELSE 
	 GHHTOUU = 0.0D0 
	ENDIF 

*Charm-quarks 
	
	MH	= MHH 
	MF 	= MCH 

	IF (MH.GT.(2.*MF)) THEN 
	 GHHTOCC = (3.D0)*(G**2*MF**2*MH/32./PI/MW**2)*(SINA/SINB)**2  
     .			 *(1.-4.*MF**2/MH**2)**(3./2.) 
	ELSE 
	 GHHTOCC = 0.0D0 
	ENDIF 

*Down-quarks 
	
	MH	= MHH 
	MF 	= MDO 

	IF (MH.GT.(2.*MF)) THEN 
	 GHHTODD = (3.D0)*(G**2*MF**2*MH/32./PI/MW**2)*(COSA/COSB)**2  
     .			 *(1.-4.*MF**2/MH**2)**(3./2.) 
	ELSE 
	 GHHTODD = 0.0D0 
	ENDIF 

*Strange-quarks 
	
	MH	= MHH 
	MF 	= MST 

	IF (MH.GT.(2.*MF)) THEN 
	 GHHTOSS = (3.D0)*(G**2*MF**2*MH/32./PI/MW**2)*(COSA/COSB)**2  
     .			 *(1.-4.*MF**2/MH**2)**(3./2.) 
	ELSE 
	 GHHTOSS = 0.0D0 
	ENDIF 

*Bottom-quarks 
	
	MH	= MHH 
	MF 	= MBO 

	IF (MH.GT.(2.*MF)) THEN 
	 GHHTOBB = (3.D0)*(G**2*MF**2*MH/32./PI/MW**2)*(COSA/COSB)**2  
     .			 *(1.-4.*MF**2/MH**2)**(3./2.) 
	ELSE 
	 GHHTOBB = 0.0D0 
	ENDIF 

*** CP-odd Higgs A°

	COST 	= 1. !IN PROGRESS...(not relevant for B.R.'s)  

*Electrons 
	
	MH	= MA 
	MF 	= ME 

	IF (MH.GT.(2.*MF)) THEN 
	 GATOEE = COST*(G**2*MF**5/4./PI/MW**2/MH**2)*TGB**2 
	ELSE 
	 GATOEE = 0.0D0 
	ENDIF 

*Muons 
	
	MH	= MA 
	MF 	= MMU

	IF (MH.GT.(2.*MF)) THEN 
	 GATOMM = COST*(G**2*MF**5/4./PI/MW**2/MH**2)*TGB**2 
	ELSE 
	 GATOMM = 0.0D0 
	ENDIF 

*Tau's 
	
	MH	= MA 
	MF 	= MTAU

	IF (MH.GT.(2.*MF)) THEN 
	 GATOTT = COST*(G**2*MF**5/4./PI/MW**2/MH**2)*TGB**2 
	ELSE 
	 GATOTT = 0.0D0 
	ENDIF 

*Up-quarks 
	
	MH	= MA  
	MF 	= MUP 

	IF (MH.GT.(2.*MF)) THEN 
	 GATOUU = COST*3.D0*(G**2*MF**5/4./PI/MW**2/MH**2)*COTB**2 
	ELSE 
	 GATOUU = 0.0D0 
	ENDIF 

*Charm-quarks 
	
	MH	= MA 
	MF 	= MCH 

	IF (MH.GT.(2.*MF)) THEN 
	 GATOCC = COST*3.D0*(G**2*MF**5/4./PI/MW**2/MH**2)*COTB**2 
	ELSE 
	 GATOCC = 0.0D0 
	ENDIF 

*Down-quarks 
	
	MH	= MA 
	MF 	= MDO 

	IF (MH.GT.(2.*MF)) THEN 
	 GATODD = COST*3.D0*(G**2*MF**5/4./PI/MW**2/MH**2)*TGB**2 
	ELSE 
	 GATODD = 0.0D0 
	ENDIF 

*Strange-quarks 
	
	MH	= MA 
	MF 	= MST 

	IF (MH.GT.(2.*MF)) THEN 
	 GATOSS = COST*3.D0*(G**2*MF**5/4./PI/MW**2/MH**2)*TGB**2 
	ELSE 
	 GATOSS = 0.0D0 
	ENDIF 

*Bottom-quarks 
	
	MH	= MA 
	MF 	= MBO 

	IF (MH.GT.(2.*MF)) THEN 
	 GATOBB = COST*3.D0*(G**2*MF**5/4./PI/MW**2/MH**2)*TGB**2 
	ELSE 
	 GATOBB = 0.0D0 
	ENDIF 

*****************************************************
*** Computation of B.R.'s for Higgs --> f fbar decays
*****************************************************

*** Light CP-even Higgs h°

	WLH	= 0.0D0 
	WLH	= WLH+GLHTOEE+GLHTOMM+GLHTOTT
     .		     +GLHTOUU+GLHTOCC+GLHTODD+GLHTOSS+GLHTOBB 

	IF (WLH.GT.0.0D0) THEN 
	 BLHTOEE = GLHTOEE/WLH 
	 BLHTOMM = GLHTOMM/WLH 
	 BLHTOTT = GLHTOTT/WLH 
	 BLHTOUU = GLHTOUU/WLH 
	 BLHTOCC = GLHTOCC/WLH 
	 BLHTODD = GLHTODD/WLH 
	 BLHTOSS = GLHTOSS/WLH 
	 BLHTOBB = GLHTOBB/WLH 
	ELSE 
	 BLHTOEE = 0.0D0 
	 BLHTOMM = 0.0D0
	 BLHTOTT = 0.0D0  
	 BLHTOUU = 0.0D0 
	 BLHTOCC = 0.0D0 
	 BLHTODD = 0.0D0  
	 BLHTOSS = 0.0D0 
	 BLHTOBB = 0.0D0 
	ENDIF  

*** Heavy CP-even Higgs H°

	WHH	= 0.0D0 
	WHH	= WHH+GHHTOEE+GHHTOMM+GHHTOTT
     .		     +GHHTOUU+GHHTOCC+GHHTODD+GHHTOSS+GHHTOBB 

	IF (WHH.GT.0.0D0) THEN 
	 BHHTOEE = GHHTOEE/WHH 
	 BHHTOMM = GHHTOMM/WHH 
	 BHHTOTT = GHHTOTT/WHH 
	 BHHTOUU = GHHTOUU/WHH 
	 BHHTOCC = GHHTOCC/WHH 
	 BHHTODD = GHHTODD/WHH 
	 BHHTOSS = GHHTOSS/WHH 
	 BHHTOBB = GHHTOBB/WHH 
	ELSE 
	 BHHTOEE = 0.0D0 
	 BHHTOMM = 0.0D0
	 BHHTOTT = 0.0D0  
	 BHHTOUU = 0.0D0 
	 BHHTOCC = 0.0D0 
	 BHHTODD = 0.0D0  
	 BHHTOSS = 0.0D0 
	 BHHTOBB = 0.0D0 	
	ENDIF 

*** CP-odd Higgs A°

	WA	= 0.0D0 
	WA	= WA+GATOEE+GATOMM+GATOTT
     .		    +GATOUU+GATOCC+GATODD+GATOSS+GATOBB 

	IF (WA.GT.0.0D0) THEN 
	 BATOEE	= GATOEE/WA 
	 BATOMM	= GATOMM/WA 
	 BATOTT	= GATOTT/WA 
	 BATOUU	= GATOUU/WA 
	 BATOCC	= GATOCC/WA 
	 BATODD	= GATODD/WA
	 BATOSS	= GATOSS/WA 
	 BATOBB	= GATOBB/WA  
	ELSE 
	 BATOEE = 0.0D0 
	 BATOMM = 0.0D0
	 BATOTT = 0.0D0  
	 BATOUU = 0.0D0 
	 BATOCC = 0.0D0 
	 BATODD = 0.0D0  
	 BATOSS = 0.0D0 
	 BATOBB = 0.0D0 
	ENDIF 

	RETURN 

 	END 	



 

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C
C     THIS PROGRAM COMPUTES THE RENORMALIZATION GROUP IMPROVED
C     VALUES OF HIGGS MASSES AND COUPLINGS IN THE MSSM.
C
C     INPUT: mA,tanb = tan(beta),mq,mur,mtop,Au,Ad,mu.
C
C     All masses in GeV units. mA is the CP-odd Higgs mass,
C     mtop is the physical top mass, mq and mur are the soft
C     supersymmetry breaking mass parameters of left handed
C     and right handed stops respectively, Au and Ad are the
C     stop and sbottom trilinear soft breaking terms,
C     respectively,  and mu is the supersymmetric
C     Higgs mass parameter. We use the  conventions from
C     the Physics Report of Haber and Kane: Left right
C     stop mixing term proportional to (Au - mu/tanb).
C
C     OUTPUT: mh,Hm,mhch, sa = sin(alpha), ca= cos(alpha)
C
C     where mh and HM are the lightest and heaviest CP-even
C     Higgs masses, mhch is the charged Higgs mass and
C     alpha is the Higgs mixing angle.
C
C     RANGE of VALIDITY:
C
C    (stop1**2 - stop2**2)/(stop2**2 + stop1**2) < 0.5
C    (sbot1**2 - sbot2**2)/(sbot2**2 + sbot2**2) < 0.5
C
C     where stop1, stop2, sbot1 and sbot2 are the stop and
C     are the sbottom  mass eigenvalues, respectively. This
C     range automatically excludes the existence of tachyons.
C
C
C     For the charged Higgs mass computation, the method is
C     valid if
C
C     2 * |mb * Ad* tanb|  < M_SUSY**2,  2 * |mtop * Au| < M_SUSY**2
C
C     2 * |mb * mu * tanb| < M_SUSY**2,  2 * |mtop * mu| < M_SUSY**2
C
C     where M_SUSY**2 is the average of the squared stop mass
C     eigenvalues, M_SUSY**2 = (stop1**2 + stop2**2)/2. The sbottom
C     masses have been assumed to be of order of the stop ones.
C
C
C     Program based on the work by M. Carena, J.R. Espinosa,
C     M. Quiros and C.E.M. Wagner, CERN-preprint CERN-TH/95-45.
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      SUBROUTINE SUBH (mA,tanb,mq,mur,mtop,Au,Ad,mu,mh,HM,
     * mhch,sa,ca)
      implicit real*8(a-h,l,m,o-z)

      mz = 91.187
      alpha1 = 0.0101
      alpha2 = 0.0337
      alpha3Z = 0.12
      v = 174.1
      pi = 3.14159
      tanbi = tanb

C     mbottom(mtop) = 3. GeV
      mb = 3.
      alpha3 = alpha3Z/(1. +(11. - 10./3.)/4./pi*alpha3Z*
     *log(mtop**2/mz**2))

C     rmtop= running top quark mass
      rmtop = mtop/(1.+4.*alpha3/3./pi)
      MS = ((mq**2 + mur**2)/2. + mtop**2)**.5



      t = log(MS**2/mtop**2)
      sinb = tanbi/((1. + tanbi**2)**.5)
      if(ma.gt.mtop)
     *tanb = tanbi*(1.+3./32./pi**2*rmtop**2/v**2/sinb**2*
     *log(ma**2/mtop**2))

      sinb = tanb/((1. + tanb**2)**.5)
      cosb = 1./((1. + tanb**2)**.5)
      cos2b = (tanb**2 - 1.)/(tanb**2 + 1.)
      g1 = (alpha1*4.*pi)**.5
      g2 = (alpha2*4.*pi)**.5
      g3 = (alpha3*4.*pi)**.5
      hu = rmtop/v/sinb
      hd =  mb/v/cosb
      mu2=mu*mu

      xau = (2.*Au**2/MS**2)*(1. - Au**2/12./MS**2)
      xad = (2.*Ad**2/MS**2)*(1. - Ad**2/12./MS**2)
      aud = (-6.*mu*mu/MS**2 - ( mu*mu- Ad*Au)*
     &   (mu*mu-Ad*Au)/MS**4
     *+ 3.*(Au + Ad)**2/MS**2)/6.
      lambda1 = ((g1**2 + g2**2)/4.)*(1.-3.*hd**2*t/8./pi**2)
     *+(3.*hd**4/8./pi**2) * (t + xad/2. + (3.*hd**2/2. + hu**2/2.
     *- 8.*g3**2) * (xad*t + t**2)/16./pi**2)
     *-(3.*hu**4* mu2*mu2/96./pi**2/MS**4) * (1+ (9.*hu**2 -5.* hd**2
     *-  16.*g3**2) *t/16./pi**2)
      lambda2 = ((g1**2 + g2**2)/4.)*(1.-3.*hu**2*t/8./pi**2)
     *+(3.*hu**4/8./pi**2) * (t + xau/2. + (3.*hu**2/2. + hd**2/2.
     *- 8.*g3**2) * (xau*t + t**2)/16./pi**2)
     *-(3.*hd**4* mu2*mu2/96./pi**2/MS**4) * (1+ (9.*hd**2 -5.* hu**2
     *-  16.*g3**2) *t/16./pi**2)
      lambda3 = ((g2**2 - g1**2)/4.)*(1.-3.*
     *(hu**2 + hd**2)*t/16./pi**2)
     *+(6.*hu**2*hd**2/16./pi**2) * (t + aud/2. + (hu**2 + hd**2
     *- 8.*g3**2) * (aud*t + t**2)/16./pi**2)
     *+(3.*hu**4/96./pi**2) * (3.*mu2/MS**2 - mu2*Au**2/
     *MS**4)* (1.+ (6.*hu**2 -2.* hd**2/2.
     *-  16.*g3**2) *t/16./pi**2)
     *+(3.*hd**4/96./pi**2) * (3.*mu2/MS**2 - mu2*Ad**2/
     *MS**4)*(1.+ (6.*hd**2 -2.* hu**2/2.
     *-  16.*g3**2) *t/16./pi**2)
      lambda4 = (- g2**2/2.)*(1.-3.*(hu**2 + hd**2)*t/16./pi**2)
     *-(6.*hu**2*hd**2/16./pi**2) * (t + aud/2. + (hu**2 + hd**2
     *- 8.*g3**2) * (aud*t + t**2)/16./pi**2)
     *+(3.*hu**4/96./pi**2) * (3.*mu2/MS**2 - mu2*Au**2/
     *MS**4)*
     *(1+ (6.*hu**2 -2.* hd**2/2.
     *-  16.*g3**2) *t/16./pi**2)
     *+(3.*hd**4/96./pi**2) * (3.*mu2/MS**2 - mu2*Ad**2/
     *MS**4)*
     *(1+ (6.*hd**2 -2.* hu**2/2.
     *-  16.*g3**2) *t/16./pi**2)
      lambda5 = -(3.*hu**4* mu2*Au**2/96./pi**2/MS**4) *
     * (1- (2.*hd**2 -6.* hu**2 + 16.*g3**2) *t/16./pi**2)
     *-(3.*hd**4* mu2*Ad**2/96./pi**2/MS**4) *
     * (1- (2.*hu**2 -6.* hd**2 + 16.*g3**2) *t/16./pi**2)
      lambda6 = (3.*hu**4* mu2*mu*Au/96./pi**2/MS**4) *
     * (1- (7.*hd**2/2. -15.* hu**2/2. + 16.*g3**2) *t/16./pi**2)
     *+(3.*hd**4* mu *(Ad**3/MS**3 - 6.*Ad/MS )/96./pi**2/MS) *
     * (1- (hu**2/2. -9.* hd**2/2. + 16.*g3**2) *t/16./pi**2)
      lambda7 = (3.*hd**4* mu2*mu*Ad/96./pi**2/MS**4) *
     * (1- (7.*hu**2/2. -15.* hd**2/2. + 16.*g3**2) *t/16./pi**2)
     *+(3.*hu**4* mu *(Au**3/MS**3 - 6.*Au/MS )/96./pi**2/MS) *
     * (1- (hd**2/2. -9.* hu**2/2. + 16.*g3**2) *t/16./pi**2)
      Trm2 = mA**2 + 2.*v**2* (lambda1* cosb**2 +
     *2.* lambda6*sinb*cosb
     *+ lambda5*sinb**2 + lambda2* sinb**2 + 2.* lambda7*sinb*cosb
     *+ lambda5*cosb**2)
      detm2 = 4.*v**4*(-(sinb*cosb*(lambda3 + lambda4) +
     *lambda6*cosb**2
     *+ lambda7* sinb**2)**2 + (lambda1* cosb**2 +
     *2.* lambda6* cosb*sinb
     *+ lambda5*sinb**2)*(lambda2* sinb**2 +2.* lambda7* cosb*sinb
     *+ lambda5*cosb**2)) + mA**2*2.*v**2 *
     *((lambda1* cosb**2 +2.*
     *lambda6* cosb*sinb + lambda5*sinb**2)*cosb**2 +
     *(lambda2* sinb**2 +2.* lambda7* cosb*sinb + lambda5*cosb**2)
     **sinb**2
     * +2.*sinb*cosb* (sinb*cosb*(lambda3 + lambda4) + lambda6*cosb**2
     *+ lambda7* sinb**2))

      mh2 = (Trm2 - (Trm2**2 - 4.* detm2)**.5)/2.
      HM2 = (Trm2 + (Trm2**2 - 4.* detm2)**.5)/2.
      HM = Hm2**.5
      mh = mh2**.5
      mhch2 = mA**2 + (lambda5 - lambda4)* v**2
      mhch = mhch2**.5
      mhch = mhch2**.5

      sinalpha = sqrt(((Trm2**2 - 4.* detm2)**.5) -
     * ((2.*v**2*(lambda1* cosb**2 + 2.*
     *lambda6* cosb*sinb
     *+ lambda5*sinb**2) + mA**2*sinb**2)
     *- (2.*v**2*(lambda2* sinb**2 +2.* lambda7* cosb*sinb
     *+ lambda5*cosb**2) + mA**2*cosb**2)))/
     *sqrt(((Trm2**2 - 4.* detm2)**.5))/2.**.5

      cosalpha = (2.*(2.*v**2*(sinb*cosb*(lambda3 + lambda4) +
     *lambda6*cosb**2 + lambda7* sinb**2) -
     *mA**2*sinb*cosb))/2.**.5/
     *sqrt(((Trm2**2 - 4.* detm2)**.5)*
     *(((Trm2**2 - 4.* detm2)**.5) -
     * ((2.*v**2*(lambda1* cosb**2 + 2.*
     *lambda6* cosb*sinb
     *+ lambda5*sinb**2) + mA**2*sinb**2)
     *- (2.*v**2*(lambda2* sinb**2 +2.* lambda7* cosb*sinb
     *+ lambda5*cosb**2) + mA**2*cosb**2))))

      sa = -sinalpha
      ca = -cosalpha

	if(mh.le.0)then 
         print *, 'Warning: negative Higgs mass!' 
	 imfail = 1
	endif 

CPM      write (1,*) ' ' 
CPM      write (1,*) ' ' 
CPM      write (1,*) ' HIGGS masses '
CPM      write (1,*) ' ' 
CPM      WRITE(1,3500) Mh, hM, MA, MHch,sa,ca
3500  FORMAT(/' Light CP-even Higgs =',F10.3,
     +       /' Heavy CP-even Higgs =',F10.3, 
     +       /'       CP-odd  Higgs =',F10.3, 
     +       /'       Charged Higgs =',F10.3,
     +       /'       sin(a-b)      =',F10.3, 
     +       /'       cos(a-b)      =',F10.3)  

 2242 RETURN
      end

      subroutine pinterf

* Pythia commons
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/LUDAT2/kchg(500,3),pmas(500,4),parf(2000),vckm(4,4)
      COMMON/LUDAT3/mdcy(500,3),mdme(2000,2),brat(2000),kfdp(2000,5)

* Susygen commons
      double precision 
     +FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
     +FLC,FRC,gms,echar
      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
     +FLC(12),FRC(12),gms(12),echar(12)
      double precision TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI
      COMMON/MSSM/TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI
      double precision sfmh,sfma,sfmhp,sfmhpc,sina,cosa
      common/higgses/sfmh,sfma,sfmhp,sfmhpc,sina,cosa

* Set Higgs couplings,masses and branching-ratios

      MSTP(4) = 1

* Prepare Z' ******************************

* Set Z' mass etc.
      PMAS(32,1) = fmz
      PMAS(32,2) = gammaz
      PMAS(32,3) = 10.*gammaz
      paru(102)= sinw**2

* MSSM  *****************************

* Top mass
      pmas(6,1)=gms(9)

* tanb
      PARU(141) = tanb
* Set h-mass
      PMAS(25,1) = sfmh
* Set H-mass
      PMAS(35,1) = sfmhp
* Set A-mass
      PMAS(36,1) = sfma
* Set H+-mass
      PMAS(37,1) = sfmhpc

* h,A couplings ****************************

      MDCY(LUCOMP(35),1)=1
      MDCY(LUCOMP(36),1)=1
      MDCY(LUCOMP(37),1)=1

* h couplings. down,up,lepton

      paru(161) = real(SINa/COSb)
      paru(162) = real(COSa/SINb)
      paru(163) = real(SINa/COSb)

* H couplings. down,up,lepton

      paru(171) = real(COSa/COSb)
      paru(172) = real(SINa/SINb)
      paru(173) = real(COSa/COSb)

* A couplings. down,up,lepton
      paru(181) = real(tanb)
      paru(182) = real(1/tanb)
      paru(183) = real(tanb)


      return
      end

CPM      subroutine higinit(kl,xcrost)
CPM
CPM* Pythia commons
CPM      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
CPM      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
CPM      COMMON/PYSUBS/MSEL,MSUB(200),KFIN(2,-40:40),CKIN(200)
CPM      COMMON/PYINT5/ngen(0:200,3),xsec(0:200,3)
CPM      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
CPM      COMMON/LUDAT2/kchg(500,3),pmas(500,4),parf(2000),vckm(4,4)
CPM      COMMON/LUDAT3/mdcy(500,3),mdme(2000,2),brat(2000),kfdp(2000,5)
CPM      COMMON/LUDATR/mrlu(6),rrlu(100)
CPM
CPM      double precision 
CPM     +FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
CPM     +FLC,FRC,gms,echar
CPM      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
CPM     +FLC(12),FRC(12),gms(12),echar(12)
CPM
CPM
CPM      double precision TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI
CPM      COMMON/MSSM/TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI
CPM
CPM      double precision sfmh,sfma,sfmhp,sfmhpc,sina,cosa
CPM      common/higgses/sfmh,sfma,sfmhp,sfmhpc,sina,cosa
CPM
CPM      double precision sba,xcrost,sigha
CPM
CPM      double precision flum,ecm,s,roots,T,Q,Q2,EN
CPM      COMMON/KINEM/flum,ecm,s,roots,T,Q,Q2,EN(2)
CPM
CPM      COMMON /CONST/ idbg,igener,irad
CPM
CPM* Different PYTHIA effects ***********************
CPM
CPM* Initial State Radiation switched on/off
CPM
CPM      MSTP(11) = irad
CPM
CPM* Set Z' mass etc.
CPM
CPM* Choose the process ***********************
CPM
CPM      do kjh=1,200
CPM      msub(kjh)=0
CPM      enddo
CPM
CPM      IF(KL.EQ.1)THEN
CPM      MSEL=0
CPM      msub(24)=1
CPM      msub(123)=1
CPM      msub(124)=1
CPM      ENDIF
CPM
CPM      IF(KL.EQ.2)THEN
CPM      MSEL=0
CPM      msub(171)=1
CPM      msub(173)=1
CPM      msub(174)=1
CPM      ENDIF
CPM      
CPM      IF(KL.EQ.3.or.KL.eq.4.or.kl.eq.5)THEN
CPM
CPM      if(kl.eq.3)then
CPM      xcrost=sigha(1)
CPM      print *,' hA ',xcrost
CPM      endif
CPM
CPM      if(kl.eq.4)then
CPM      xcrost=sigha(2)
CPM      print *,' HA ',xcrost
CPM      endif
CPM
CPM      MSEL=0
CPM
CPM* only Z'
CPM*      MSTP(44) = 3
CPM* only Z' -> hA
CPM      MSUB(141) = 1
CPM
CPM* Remove unwanted Z' decays
CPMc
CPMc carefull this is jetset 7.4
CPMc
CPM      DO I=209,230
CPM      MDME(I,1) = 0
CPM      ENDDO
CPM
CPM* Z'-> hA coupling
CPM      SBA=(sinb*cosa-sina*cosb)
CPM
CPM* Switch on Z'-> hA
CPM
CPM      if(kl.eq.3)then
CPM      MDME(229,1) = 1
CPM      PARU(188)   = dsqrt(1.d0-sba**2)
CPM      PARU(189)   = 0.
CPM      endif
CPM
CPM      if(kl.eq.4)then
CPM      MDME(230,1) = 1
CPM      PARU(188)   = 0.
CPM      PARU(189)   = sba
CPM      endif
CPM
CPM      if(kl.eq.5)then
CPM      MDME(226,1) = 1
CPM      endif
CPM
CPM      ENDIF
CPM
CPM
CPM      CALL PYINIT ('CMS','e-','e+',sngl(ecm))
CPM
CPM      DO L=1,100
CPM      mstj(1)=0
CPM       CALL PYEVNT
CPM      ENDDO
CPM
CPM      mstj(1)=1
CPM
CPM      IF(KL.EQ.1)THEN
CPM      xcrost= xsec(24,3)+xsec(103,3)+xsec(123,3)+xsec(124,3)
CPM      ENDIF
CPM
CPM      IF(KL.EQ.2)THEN
CPM      xcrost= xsec(153,3)+xsec(171,3)+xsec(173,3)+xsec(174,3)
CPM      ENDIF
CPM      
CPM      IF(KL.EQ.3.or.KL.eq.4.or.KL.eq.5)THEN
CPM      xcrost=xsec(141,3)
CPM      ENDIF
CPM
CPM      xcrost=xcrost*1000000000.d0
CPM      print *,' xcrost ',xcrost
CPM
CPM      RETURN
CPM      END


        DOUBLE PRECISION FUNCTION SIGHA(ikind)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/SM/FMW,FMZ,GAMMAZ,GAMMAW,SINW,COSW,ALPHA,E2,G2,PI,TWOPI,
     +FLC(12),FRC(12),gms(12),echar(12)
      COMMON/MSSM/TANB,SINB,COSB,FMGAUG,FMR,FM0,ATRI
      common/higgses/sfmh,sfma,sfmhp,sfmhpc,sina,cosa
      COMMON/KINEM/flum,ecm,s,roots,T,Q,Q2,EN(2)

        SBASQ=(sinb*cosa-sina*cosb)**2

        if(ikind.eq.1)then
        rmh=sfmh
        c1=(1.d0-sbasq)
        else
        c1=sbasq
        rmh=sfmhp
        endif

        S2W=sinw**2
	S=ECM**2

	CLEP=(1./2.-S2W)**2+S2W**2
	PROP=1.D0/((S-fmz**2)**2+fmz**2*GAMMAZ**2)/S**2
	COEFF=PI*ALPHA**2/(24.D0*S2W**2*(1.D0-S2W)**2)


	IF((RMH+SFMA).GT.ECM)THEN
	SIGHA=0.
	ELSE
	RLHA=SQRT(S**2-2.*S*(RMH**2+SFMA**2)+(RMH**2-SFMA**2)**2)
	SIGHA=COEFF*CLEP*PROP*c1*RLHA**3*0.3893857D9
	ENDIF
	return
        end



