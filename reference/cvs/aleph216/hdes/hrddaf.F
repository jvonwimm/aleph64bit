      SUBROUTINE HRDDAF (LDBAS,IRUN,IFLAG)
C------------------------------------------------------------------
CKEY HCALDES HCAL READ DAF BANK /USER
C - G.Catanesi - 871007            modified by - F.Ranjard - 880202
C            Modify: L.Silvestris 09/07/90
C! Get HCAL banks and store the content in the commons
C
C - input arguments:
C           LDBAS  : data base logical unit
C           IRUN   : run number
C - output argument:
C           IFLAG  : return flag  ( = ALGTDB(LDBAS,list,IRUN) )
C
C           Called by : HCIRUN
C           Calls     : ALGTDB,ALTELL,HDEADM from Alephlib
C                       UCOPY from CERNLIB
C
#ifndef DOC
#include "bcs.h"
#include "hcgeom.h"
#include "hjjpar.h"
#include "htusjj.h"
      CHARACTER*48 LISTG
C
      INTEGER ALGTDB
      EXTERNAL ALGTDB
C
      DATA LISTG/'HCCOHETCHCALHBGEHEGEHBARHENDHBDEHEDEHEMTHTXD    '/
C
#include "bmacro.h"
C ----------------------------------------------------------------------
C
C  If the physical constants are required loads the banks
C    from the D.A file ADBSCOMB.DAF (unit 4) to the
C            HCCONS common
C
      LL = LNBLNK(LISTG)
      IF (IRUN.GT.2000) LISTG = LISTG(1:LL)//'HRDT'
      IFLAG=ALGTDB(LDBAS,LISTG,IRUN)
C
C
C      Get index for constant banks
C
      JHCCO = IW(NAMIND( 'HCCO'))
C
      IF(JHCCO.NE.0)THEN
C
C  Store the content of the HCCO bank
C
         HCTHRF = RTABL(JHCCO,1,JHCCTR)
         HCRSIZ = RTABL(JHCCO,1,JHCCRR)
         HCZSIZ = RTABL(JHCCO,1,JHCCZR)
         NHCBAR = ITABL(JHCCO,1,JHCCNB)
         NHCECA = ITABL(JHCCO,1,JHCCNE)
C
      ENDIF
C
C Store the content of the HETC bank
C
      JHETC = IW(NAMIND( 'HETC'))
      IF(JHETC.NE.0)THEN
C
         NHCEIT = ITABL(JHETC,1,JHETNT)
         HCEIWI = RTABL(JHETC,1,JHETEW)
         HCDOWI = RTABL(JHETC,1,JHETDW)
         HCTUGA = RTABL(JHETC,1,JHETTG)
         HCSEPO = RTABL(JHETC,1,JHETE2)
         HCTUEN = RTABL(JHETC,1,JHETIZ)
         HCSABL = HCEIWI/NHCEIT
         HCSAEC = HCDOWI/(2*NHCEIT)
         CALL UCOPY(RW(JHETC+LMHLEN+JHETLT),XLNHCE(1),LHETLT)
C
         CALL UCOPY(RW(JHETC+LMHLEN+JHETLI),HCTLEI(1,1),LHETLI)
         CALL UCOPY(RW(JHETC+LMHLEN+JHETLO),HCTLEO(1,1),LHETLO)
C
         CALL UCOPY(RW(JHETC+LMHLEN+JHETAI),HCTAEI(1),LHETAI)
         CALL UCOPY(RW(JHETC+LMHLEN+JHETAO),HCTAEO(1),LHETAO)
C
C
      ENDIF
C
      IRET = ALGTDB (LDBAS,'HTUS',IRUN)
      JHTUS = IW(NAMIND('HTUS'))
      IF(JHTUS.NE.0) THEN
         HCTUSH = RTABL(JHTUS,1,JHTUTE)
         CALL UCOPY(RW(JHTUS+LMHLEN+JHTUTS),XHCSHI(1),LHCBL)
      ENDIF
C
      JHCAL = IW(NAMIND( 'HCAL'))
      JHBGE = IW(NAMIND( 'HBGE'))
      JHEGE = IW(NAMIND( 'HEGE'))
      JHBAR = IW(NAMIND( 'HBAR'))
      JHEND = IW(NAMIND( 'HEND'))
      JHBDE = IW(NAMIND( 'HBDE'))
      JHEDE = IW(NAMIND( 'HEDE'))
      JHEMT = IW(NAMIND( 'HEMT'))
C
      IF(JHCAL.NE.0)THEN
C
C  Store the content of the HCAL bank in the HCALGE common
C
         NHCSUB = ITABL(JHCAL,1,JHCANS)
         NHCTWR = ITABL(JHCAL,1,JHCANT)
         NHCPHC = ITABL(JHCAL,1,JHCANP)
         NHCREF = ITABL(JHCAL,1,JHCANR)
         IHCTID(1) = ITABL(JHCAL,1,JHCAIT)
         IHCTID(2) = ITABL(JHCAL,1,JHCAIH)
         HCTUTH = RTABL(JHCAL,1,JHCAGA)
         HCIRTH = RTABL(JHCAL,1,JHCATF)
         HCLSLA = RTABL(JHCAL,1,JHCATL)
C
C  sampling thickness (iron + tubes layer)
C
         HCSMTH = HCTUTH + HCIRTH
      ENDIF
C
C Store the content of HBGE bank (Barrel Geometry) in HCALGE
C      and HBAR
C
      IF(JHBGE.NE.0)THEN
C
         HCRMIN(1) = RTABL(JHBGE,1,JHBGYI)
         HCRMAX(1) = RTABL(JHBGE,1,JHBGYX)
         HCZMIN(1) = 0.
         HCZMAX(1) = RTABL(JHBGE,1,JHBGZX)
         HFSPBL = RTABL(JHBGE,1,JHBGIT)
         HFSRBL = RTABL(JHBGE,1,JHBGRA)
         HCPHOF = RTABL(JHBGE,1,JHBGPO)
C
      ENDIF

C Store the content of HEGE bank (End-Cap Geometry) in HCALGE
C
      IF(JHEGE.NE.0)THEN
C
         CALL UCOPY(RW(JHEGE+LMHLEN+JHEGZI),HCZMIN(2),2)
         CALL UCOPY(RW(JHEGE+LMHLEN+JHEGZX),HCZMAX(2),2)
         CALL UCOPY(RW(JHEGE+LMHLEN+JHEGRI),HCRMIN(2),2)
         CALL UCOPY(RW(JHEGE+LMHLEN+JHEGRX),HCRMAX(2),2)
C
      ENDIF
C
C Store the content of HBAR bank (Barrel Geometry) in HBAR common
C
      IF(JHBAR.NE.0)THEN
C
         NHCBMO = ITABL(JHBAR,1,JHBANS)
         NHCBFS = ITABL(JHBAR,1,JHBANF)
         NHCBBS = ITABL(JHBAR,1,JHBANB)
         NHCBLA = ITABL(JHBAR,1,JHBANL)
C
      ENDIF
C
C Store the content of HBDE bank (Barrel Geometry) in HBAR common
C
      IF(JHBDE.NE.0)THEN
C
         KHBDE = JHBDE + LMHLEN + JHBDL1 - 1
         DO 10 J=1,3
            HCLTNO(J) = RW(KHBDE+J*2-1)
            HCWINO(J) = RW(KHBDE+J*2)
   10    CONTINUE
C
         HCDEWI = RTABL(JHBDE,1,JHBDDW)
         NHBLA2 = ITABL(JHBDE,1,JHBDT2)
         NHBLI3 = ITABL(JHBDE,1,JHBDT3)
         NHBLO3 = ITABL(JHBDE,1,JHBDNB)
         CALL UCOPY(IW(JHBDE+LMHLEN+JHBDN8),NEITHC(1),LHBDN8)
         CALL UCOPY(IW(JHBDE+LMHLEN+JHBDT8),NEITSP(1,1),LHBDT8)
         CALL UCOPY(RW(JHBDE+LMHLEN+JHBDSP),HCSPLT(1,1),LHBDSP)
C
      ENDIF
C
      IF(JHEDE.NE.0)THEN
C
C  Store the content of the HEDE bank in the HEND common (End-Cap)
C
         HCDREC = RTABL(JHEDE,1,JHEDWT)
         HCAPSL = RTABL(JHEDE,1,JHEDST)
         HCDSTP = RTABL(JHEDE,1,JHEDDS)
C
C  -  these 3 quantities are modified to take into account the air
C     between tubes in the following way
C     IF TOL is the difference between the distance between spacers
C        HCDSTP and the space occupied by 5 double-eightfolders
C        5*HCDOWI
C     THEN enlarge the iron edge HCDREC by 0.5*TOL and the spacer
C        width HCAPSL by TOL and reduce the distance between spacers
C        HCDSTP by TOL
C
         TOL = HCDSTP - 5.*HCDOWI
         HCDREC = HCDREC + 0.5*TOL
         HCAPSL = HCAPSL + TOL
         HCDSTP = HCDSTP - TOL
C
      ENDIF
C
      IF(JHEND.NE.0)THEN
C
C  Store the content of the HEND bank in the HEND common
C
         NHCSEX = ITABL(JHEND,1,JHENNS)
         NHCEFS = ITABL(JHEND,1,JHENNF)
         NHCEBS = ITABL(JHEND,1,JHENNB)
         NHCTRE = ITABL(JHEND,1,JHENNW)
         NHCINL = ITABL(JHEND,1,JHENNL)
         NHCOUL = ITABL(JHEND,1,JHENNL+1)
C
      ENDIF
C
      IF(JHEMT.NE.0)THEN
C
C  Store the content of the HEMT bank in the HEND common
C
         NHCIND = ITABL(JHEMT,1,JHEMNI)
         NHCOUD = ITABL(JHEMT,1,JHEMNO)
C
      ENDIF
C
      HCTIRF(1) = HCIRTH
      HCTIRF(2) = HFSPBL
      HCTIRF(3) = HCIRTH
C
      NHCPLA(1) = NHCINL + NHCOUL
      NHCPLA(2) = NHCBLA
      NHCPLA(3) = NHCPLA(1)
C
C Create and store look-up table to take in account dead tubes
C
C
C Create and store look-up table to take in account dead tubes
C
      JHTXD = IW(NAMIND( 'HTXD'))
      JHRDT = IW(NAMIND( 'HRDT'))
      IF(JHTXD.NE.0.OR.JHRDT.NE.0)CALL HDEADM
C
C get STATIC banks HTID and HTUE
C
      JHTID = MDARD (IW,LDBAS,'HTID',0)
      IF(JHTID.NE.0)THEN
         HTINBL  = RTABL(JHTID,1,JHTIIB)
         CALL UCOPY(RW(JHTID+LMHLEN+JHTIIE),HTINEC(1),LHTIIE)
         HTPIEC  = RTABL(JHTID,1,JHTICI)
         HTPOEC  = RTABL(JHTID,1,JHTICO)
         HBWREC  = RTABL(JHTID,1,JHTIPC)
         CALL UCOPY(RW(JHTID+LMHLEN+JHTIPE),HBWCEC(1),LHTIPE)
         HBSREC  = RTABL(JHTID,1,JHTIPR)
         HBSCEC  = RTABL(JHTID,1,JHTIPB)
         HBWRBL  = RTABL(JHTID,1,JHTIPL)
         HBSCBL  = RTABL(JHTID,1,JHTIPO)
       ELSE
         IFLAG = 0
         RETURN
       ENDIF
C
       JHTUE = MDARD (IW,LDBAS,'HTUE',0)
       IF(JHTUE.NE.0)THEN
         CALL UCOPY(IW(JHTUE+LMHLEN+JHTUMN),NHMBDF(1),LHTUMN)
         NHTY4D =  ITABL(JHTUE,1,JHTUN4)
         NHTY3D =  ITABL(JHTUE,1,JHTUN3)
         NHTY2D =  ITABL(JHTUE,1,JHTUN2)
         CALL UCOPY(IW(JHTUE+LMHLEN+JHTUMS),NHMBFL(1),LHTUMS)
         NHBDOU  = ITABL(JHTUE,1,JHTUN8)
         NHDLEC  = ITABL(JHTUE,1,JHTULN)
         NHDET0  = ITABL(JHTUE,1,JHTUN0)
         NHDEBS  = ITABL(JHTUE,1,JHTUNF)
      ELSE
         IFLAG = 0
         RETURN
      ENDIF
C
C Secondary variables computation
C
       CALL HCSECO
C
      RETURN
      END
#endif
