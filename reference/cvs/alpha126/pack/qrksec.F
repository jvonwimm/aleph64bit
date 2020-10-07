      SUBROUTINE QRKSEC
C----------------------------------------------------------------------
C get the generated cross-section from bank RKSE
C for the ru presently analyzed, 
C and for the whole dataset
C results are transmitted into the common /XKSECS/
C Author J. Boucrot 26-10-2000
C Mod J. Boucrot 31-01-2002 fix for same Ranmar seeds
C Mod B. Bloch   24-09-2002 fix for banks on the ADBSCONS DAF
C Mod J. Boucrot 11-10-2002 replace Kingal code 5030 by 5300
C----------------------------------------------------------------------
#include "qcde.h"
#include "krunjj.h"
#include "rksejj.h"
#include "asimjj.h"
#include "krsktrg.h"
#include "rhahjj.h" 
C-----------------------------------------------------------------------
      save
      external chaint
      parameter ( nrumm = 2000, ikrco = 10000, irma = 100000)
      parameter ( kinor = 5030 , kinok = 5300)
      common / XKSECS / xsecru,xseceru,nevpro,xsecpr,xsecepr
      character chaint*4,prog*4,title*48,zkey*60,ckey*60,tcp*12,tap*12
      data irun / -1 /
#include "bmacro.h"
C-----------------------------------------------------------------------
      call qfkirev
      IF (KRUN.GT.NRUMM) GO TO 999
      XSEC=-1.
      XSECE=0.
      NEVPR=0
      kinco=0
      XSECP=0.
      XSECPE=0.
      XSECR=0.
      XSECRE=0.
      NEVTP=0
      NSETS=0
      TITLE=' ' 
      ZKEY=' '
      CKEY=' '
      IF (KRUN.NE.IRUN) THEN
         XSECRU=-1.
         XSECERU=-1.
         NEVPRO=0
         XSECPR=-1.
         XSECEPR=-1.
      ENDIF
C Find run number, first/last ranmar seed of dataset:
      IKRU=IW(NAMIND('KRUN'))
      IF (IKRU.GT.0) THEN
         KKRU=KROW(IKRU,1)
         KOD=IW(KKRU+JKRUGI)
         KINCO=MOD(KOD,IKRCO)
C For historical reasons, Kingal code 5030 must be 
C relaced by Kingal code 5300 for RKSE banks:
         if (kinco.eq.kinor) kinco=kinok 
         IRAN1=IW(KKRU+JKRUFS)
         IRAN2=IW(KKRU+JKRUSS)
         CALL ALSTIN(IW(KKRU+JKRURT),JKRUFS-JKRURT,TITLE)
      endif
C Tape name:
      tcp=' '
      lcp=0
      if (nrukin.gt.0) then
         tcp=filrlis(nrukin) 
         call cltou(tcp)
         lcp=lnblnk(tcp) 
      endif      
C KINGAL code found: look for 'RKSE' bank:
      if (kinco.EQ.0) GO TO 999
      JRKSE=NLINK('RKSE',KINCO)
C -------------- banks may be on ADBSCONS.DAF 
      if (JRKSE.EQ.0) then
C try load from dbase if not in dbas.bank
         LDBAS = JUNIDB(0)
         JBRKSE = NDANR (LDBAS,'RKSE','EQ',kinco)
         if(JBRKSE.ne.0) ind = mdard(iw,ldbas,'RKSE',kinco)
         JRKSE=NLINK('RKSE',KINCO)
      endif
      IF (JRKSE.EQ.0) GO TO 999
C Find year of GALEPH geometry:
      IKAP=IW(NAMIND('ASIM'))
      IF (IKAP.NE.0) THEN
C Year/version of GALEPH Geometry in ASIM :
C was previously yyvv, may become yyyyvv
         IDASI=ITABL(IKAP,1,JASIYM)
         IF (IDASI.GT.IRMA) THEN
            IYEOG=IDASI/100
         ELSE                     
            IYY=IDASI/100
            IF (IYY.GT.10) THEN
               IYEOG=1900+IYY
            ELSE
               IYEOG=2000+IYY
            ENDIF
         ENDIF  
      ENDIF
C Find the LEP centre-of-mass energy in MeV:
      IELEP=NINT(1000.*ALELEP(KRUN))
C Find nature of data, Galeph/Julia/MINI versions:
      igver=0
      ijver=0
      imver=0
      JRHAH=IW(NAMIND('RHAH'))
      DO 100 IRHAH=1,LROWS(JRHAH)
         KRHAH=KROW(JRHAH,IRHAH)
         INDAT=IW(KRHAH+JRHANO)
         PROG=CHAINT(IW(KRHAH+JRHAPN))
         IF (PROG(1:1).EQ.'G') IGVER=IW(KRHAH+JRHAPV)
         IF (PROG(1:1).EQ.'J') IJVER=IW(KRHAH+JRHAPV)
         IF (PROG(1:1).EQ.'M') IMVER=IW(KRHAH+JRHAPV)
 100  CONTINUE
C 'RKSE' bank exists: look for the dataset
      DO 200 IRKSE=1,LROWS(JRKSE)
         KRKSE=KROW(JRKSE,IRKSE)
C check the tape name:
         call alstin(iw(krkse+jrksvs),3,tap)
         call cltou(tap)
         ltap=lnblnk(tap)
         if (lcp.gt.0) then
            if (ltap.ne.lcp) go to 200
            if (tap(1:ltap).ne.tcp(1:lcp)) go to 200
         endif
         IF (IYEOG.NE.IW(KRKSE+JRKSYG)) GO TO 200
         IF (INDAT.NE.IW(KRKSE+JRKSND)) GO TO 200
         IF (IELEP.NE.IW(KRKSE+JRKSLE)) GO TO 200
         IF (IGVER.NE.IW(KRKSE+JRKSGV)) GO TO 200
         IF (XMINI.AND.IMVER.NE.IW(KRKSE+JRKSMV)) GO TO 200
         IF (IRAN1.NE.IW(KRKSE+JRKSR1)) GO TO 200
         IF (IRAN2.NE.IW(KRKSE+JRKSR2)) GO TO 200
         IF (KRUN.NE.IW(KRKSE+JRKSRN)) GO TO 200 
         IF (IW(KRKSE+JRKSLT).NE.0) GO TO 200
C Dataset found: give Xsec, Xsece, ZKEY,
C then the prod. cross section and error, and flag this row:
         XSEC=RW(KRKSE+JRKSKX)
         XSECE=RW(KRKSE+JRKSKE)
         NEVPR=IW(KRKSE+JRKSNE)
         CALL ALSTIN(IW(KRKSE+JRKSKW),LRKSEA-JRKSKW+1,ZKEY)
         xsecp=rw(krkse+jrkspx)
         xsecpe=rw(krkse+jrkspe)
         IW(KRKSE+JRKSLT)=1
         go to 250
 200  CONTINUE
C---------------------------------------------------------------------
C get the total number of events from all runs of the production:
C and the corresponding cross-section and error:
C
 250  xsecr=0. 
      xsecre=0.
      CKEY=' '
      DO 300 IRKSE=1,LROWS(JRKSE)
         KRKSE=KROW(JRKSE,IRKSE)
         IF (IYEOG.NE.IW(KRKSE+JRKSYG)) GO TO 300
         IF (INDAT.NE.IW(KRKSE+JRKSND)) GO TO 300
         IF (IELEP.NE.IW(KRKSE+JRKSLE)) GO TO 300
         IF (IGVER.NE.IW(KRKSE+JRKSGV)) GO TO 300
         IF (XMINI.AND.IMVER.NE.IW(KRKSE+JRKSMV)) GO TO 300
         CALL  ALSTIN(IW(KRKSE+JRKSKW),LRKSEA-JRKSKW+1,CKEY)
         IF (CKEY(1:LNBLNK(CKEY)).NE.ZKEY(1:LNBLNK(ZKEY))) GO TO 300
C Store X-section and Nevents for all runs of this prod:
         NSETS=NSETS+1
         NEVTP=NEVTP+IW(KRKSE+JRKSNE)
         XSECR=XSECR+RW(KRKSE+JRKSKX)
         XSECRE=XSECRE+RW(KRKSE+JRKSKE)
 300  CONTINUE
C ---------------------------------------------------------------------
C Now printout the results and store them at the beginning of a new run:
C
 500  CONTINUE
      if (krun.ne.irun) then
         irun=krun
         ltit=lnblnk(title)
         XSECRU=-1.
         XSECERU=-1.
         NEVPRO=0
         XSECPR=-1.
         XSECEPR=-1.
         if (xsec.gt.0.) then 
            xsecru=xsec
            xseceru=xsece
            nevpro=nevtp
            fset=float(nsets)
            xsect=0.
            xsecte=0.
            if (nsets.gt.0) then 
               xsect=xsecr/fset
               xsecte=xsecre/(fset*sqrt(fset))             
            endif
C Choose if one uses the prod cross section from 'RKSE' or the one
C from the average of runs:           
            if (xsecp.gt.0.) then
               xsecpr=xsecp 
               xsecepr=xsecpe
               iprov=1
            else
               xsecpr=xsect
               xsecepr=xsecte
               iprov=2
            endif
C           
            write (iw(6),1234) krun,title(1:ltit),xsecru,xseceru,
     +                         nevpr,nsets,nevpro,xsecpr,xsecepr
 1234       format (/1x,100(1H*)/5x,'==> New run :',i5,
     +          2x,'** Title of production =',2x,(A)/
     +          5x,'==> Cross-section for this run =',
     +          f12.5,' +/- ',f10.5,' picobarns',/5x,
     +          '==> Number of events in this run =',I6/
     +          5x,'--> This is part of a production of ',i5,
     +          ' runs with Nevents =',i8/ 
     +          5x,'==> Cross-section for the whole production =',
     +          f12.5,' +/- ',f10.5,' picobarns',/
     +          1x,100(1H*)/)
         endif
      endif
c  
 999  continue
      RETURN
      END
