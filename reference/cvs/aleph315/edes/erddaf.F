      SUBROUTINE ERDDAF(LUNDAF,IRUN,IGOOD)
C.---------------------------------------------------------------------
CKEY ECALDES READ DAF BANKS / USER
C     M.Rumpf        June  88      Modification H Videau 14/03/90
C! ECALDES interface with DAF
C  Fill ECALDES commons from direct access file.
C   Input :
C          LUNDAF Logical Unit for DAF
C          IRUN   Run number
C   Output:
C          IGOOD (0 = error,  1 = OK)
C   Called by USER program before ECDFRD
C   Calls: ALGTDB                    from ALEPHLIB
C.---------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "alcons.h"
#include "bcs.h"
C! global ECAL data base HAC parameters
#include "ealijj.h"
#include "ebpljj.h"
#include "ecgnjj.h"
#include "ecmtjj.h"
#include "ecncjj.h"
#include "ecoljj.h"
#include "ecrpjj.h"
#include "eecbjj.h"
#include "efacjj.h"
#include "elnfjj.h"
#include "eltyjj.h"
#include "ematjj.h"
#include "emodjj.h"
#include "ephyjj.h"
#include "eplnjj.h"
#include "epscjj.h"
#include "eptyjj.h"
#include "eqtyjj.h"
#include "eregjj.h"
#include "erowjj.h"
#include "escojj.h"
#include "esecjj.h"
#include "eslojj.h"
#include "esscjj.h"
#include "estyjj.h"
#include "etscjj.h"
#include "etyvjj.h"
#include "evoljj.h"
#include "exrgjj.h"
CD EVLFJJ
      PARAMETER(JEVLFI=1,JEVLSG=2,JEVLEA=3,JEVLEL=4,JEVLEV=5)
CD EVLSJJ
      PARAMETER(JEVLSI=1,JEVLNM=2,JEVLVL=6,JEVLZN=7)
#include "ecccom.h"
C- First fill ECAL Bos banks from direct access file
C
      CHARACTER ECBANK * 112
      CHARACTER * 4 CHA4(4),CHAINT
      INTEGER LUNDAF,IRUN,IGOOD
      INTEGER ALGTDB,NAMIND
      INTEGER INDEC
      INTEGER JEBPL,JECGN,JECMT,JECNC,JECOL,JECRP,JEECB
      INTEGER JEFAC,JELNF,JELTY,JEMAT,JEPHY,JEPLN
      INTEGER JEPSC,JEPTY,JEQTY,JEROW,JESCO,JESEC
      INTEGER JESLO,JESSC,JESTY,JETSC,JEVLF
      INTEGER JEVLS,JEVOL,JEXRG,JEXRO
C
      EXTERNAL ALGTDB
C
      INTEGER I,J,NC,NR,NRCUR
C
C- List of ECbanks names from data base
      DATA ECBANK(1:60) /
     &'EBPLECGNECMTECNCECOLECRPEECBEFACELNFELTYEMATEPHYEPLNEPSCEPTY' /
      DATA ECBANK(61:112) /
     &'EQTYEROWESCOESECESLOESSCESTYETSCEVLFEVLSEVOLEXRGEXRO' /
C
C
#include "bmacro.h"
C
C- Read Banks for ECAL from d/a file into memory
C
      IGOOD = 1
C
         INDEC = ALGTDB(LUNDAF,ECBANK,IRUN)
         IF(INDEC .EQ. 0) GO TO 998
C
C Now fill ECAL commons from banks
C
C  ... Bank EBPL
C
      JEBPL = IW(NAMIND('EBPL'))
      NC = LCOLS(JEBPL)
      NR = LROWS(JEBPL)
      IF (NR.GT.NEBPL)                GO TO 998
      DO 10 I = 1,NR
         EBPLID(I) = ITABL(JEBPL,I,JEBPID)
         EBPLVL(1,I) = ITABL(JEBPL,I,JEBPVR)
         EBPLVL(2,I) = ITABL(JEBPL,I,JEBPVR+1)
         DO 11 J = 1,ECALLS
           EBPLLF(J,I) = RTABL(JEBPL,I,JEBPLF+J-1)
   11    CONTINUE
   10 CONTINUE
      EBPLLF(1,6)=0.
      EBPLLF(2,6)=0.
      EBPLLF(3,6)=1.
      EBPLLF(4,6)=-6.4
      EBPLLF(1,7)=0.
      EBPLLF(2,7)=0.
      EBPLLF(3,7)=1.
      EBPLLF(4,7)=+6.4
C
C
C  ... Bank ECGN (ex ECAL)
C
      JECGN = IW(NAMIND('ECGN'))
      NR = LROWS(JECGN)
      IF(NR.NE.1)                  GO TO 998
C
      ECALID =    ITABL(JECGN,1,JECGID)
C Following variables defined by PARAMETER statements
C     ECALLS =    ITABL(JECGN,1,JECGLS)
C     ECALLP =    ITABL(JECGN,1,JECGLP)
C     ECALSC =    ITABL(JECGN,1,JECGSC)
C     ECALSY =    ITABL(JECGN,1,JECGSY)
C     ECALMD =    ITABL(JECGN,1,JECGMD)
C     ECALPL =    ITABL(JECGN,1,JECGPL)
C     ECALST =    ITABL(JECGN,1,JECGST)
C     ECALCL =    ITABL(JECGN,1,JECGCL)
C     ECALRG =    ITABL(JECGN,1,JECGRG)
C     ECALSS =    ITABL(JECGN,1,JECGSS)
C     ECALRW =    ITABL(JECGN,1,JECGRW)
C     ECALXW =    ITABL(JECGN,1,JECGXW)
C     ECALEC =    ITABL(JECGN,1,JECGEC)
C     ECALXG =    ITABL(JECGN,1,JECGXG)
C     ECALNP =    ITABL(JECGN,1,JECGNP)
C
      ECALPR =    ITABL(JECGN,1,JECGPR)
      DO 20 I = 1,ECALLS
        DO 21 J = 1,ECALLS
          ECALDM(J,I) = RTABL(JECGN,1,JECGPR+J+ ECALLS * (I-1))
   21   CONTINUE
   20 CONTINUE
C
      ECALBL =    RTABL(JECGN,1,JECGBL)
      ECALBO =    RTABL(JECGN,1,JECGBO)
      ECALEI =    RTABL(JECGN,1,JECGEI)
      ECALEW =    RTABL(JECGN,1,JECGEW)
      ECALEL =    RTABL(JECGN,1,JECGEL)
      ECALGP =    RTABL(JECGN,1,JECGGP)
      ECALWS =    RTABL(JECGN,1,JECGWS)
      ECALAP =    RTABL(JECGN,1,JECGAP)
      ECALAL =    ITABL(JECGN,1,JECGAL)
C
      ECALTI =    RTABL(JECGN,1,JECGTI)
      ECALC1 =    ITABL(JECGN,1,JECGC1)
      ECALC2 =    ITABL(JECGN,1,JECGC2)
      ECALM1 =    ITABL(JECGN,1,JECGM1)
      ECALM2 =    ITABL(JECGN,1,JECGM2)
C
C  ... Bank ECMT
C
      JECMT = IW(NAMIND('ECMT'))
      NC = LCOLS(JECMT)
      NR = LROWS(JECMT)
      IF(NR.NE.NECMT)                 GO TO 998
      DO 30 I = 1,NR
         ECMTID(I) = ITABL(JECMT,I,JECMID)
         ECMTMP(I) = RTABL(JECMT,I,JECMMP)
         ECMTCP(I) = ITABL(JECMT,I,JECMCP)
         ECMTEL(I) = ITABL(JECMT,I,JECMEL)
   30 CONTINUE
C
C  ... Bank ECNC
C
      JECNC = IW(NAMIND('ECNC'))
      NC = LCOLS(JECNC)
      NR = LROWS(JECNC)
      IF(NR.NE.NECNC)                 GO TO 998
      DO 40 I = 1,NR
         ECNCID(I) = ITABL(JECNC,I,JECNID)
         ECNCEC(I) = ITABL(JECNC,I,JECNEC)
         ECNCEQ(I) = ITABL(JECNC,I,JECNEQ)
         ECNCET(I) = ITABL(JECNC,I,JECNET)
   40 CONTINUE
C
C  ... Bank ECOL
C
      JECOL = IW(NAMIND('ECOL'))
      NC = LCOLS(JECOL)
      NR = LROWS(JECOL)
      IF(NR.NE.ECALCL)                GO TO 998
      DO 50 I = 1,NR
         ECOLID(I) = ITABL(JECOL,I,JECOID)
         ECOLPC(1,I) = RTABL(JECOL,I,JECOPC)
         ECOLPC(2,I) = RTABL(JECOL,I,JECOPC+1)
         ECOLER(I) = ITABL(JECOL,I,JECOER)
   50 CONTINUE
C
C  ... Bank ECRP
C
      JECRP = IW(NAMIND('ECRP'))
      NC = LCOLS(JECRP)
      NR = LROWS(JECRP)
      IF(NR.NE.NECRP)                 GO TO 998
      DO 60 I = 1,NR
         ECRPID(I) = ITABL(JECRP,I,JECRID)
         ECRPEC(I) = ITABL(JECRP,I,JECREC)
         ECRPET(I) = ITABL(JECRP,I,JECRET)
         ECRPP1(I) = ITABL(JECRP,I,JECRP1)
         ECRPP2(I) = ITABL(JECRP,I,JECRP2)
         ECRPP3(I) = ITABL(JECRP,I,JECRP3)
   60 CONTINUE
C
C  ... Bank EECB
C
      JEECB = IW(NAMIND('EECB'))
      NC = LCOLS(JEECB)
      NR = LROWS(JEECB)
      IF(NR.NE.1)                   GO TO 998
      IF(NC.NE.1+ECALLS*ECALLS)     GO TO 998
      EECBID = ITABL(JEECB,1,JEECID)
      DO 70 I = 1,ECALLS
        DO 71 J = 1,ECALLS
         EECBDM(J,I) =RTABL(JEECB,1,JEECDM-1+J + ECALLS*(I-1))
   71   CONTINUE
   70 CONTINUE
C
C  ... Bank EFAC
C
      JEFAC = IW(NAMIND('EFAC'))
      NC = LCOLS(JEFAC)
      NR = LROWS(JEFAC)
      IF(NR.NE.NEFAC)                 GO TO 998
      DO 80 I = 1,NR
         EFACID(I) = ITABL(JEFAC,I,JEFAID)
         EFACEA(I) = ITABL(JEFAC,I,JEFAEA)
         EFACEC(I) = ITABL(JEFAC,I,JEFAEC)
         EFACEF(I) = ITABL(JEFAC,I,JEFAEF)
         EFACET(I) = ITABL(JEFAC,I,JEFAET)
   80 CONTINUE
C
C  ... Bank ELNF
C
      JELNF = IW(NAMIND('ELNF'))
      NC = LCOLS(JELNF)
      NR = LROWS(JELNF)
      IF (NR.NE.NELNF)                GO TO 998
      DO 90 I = 1,NR
         ELNFID(I) = ITABL(JELNF,I,JELNID)
         DO 91 J=1,ECALLS
           ELNFLF(J,I) = RTABL(JELNF,I,JELNLF-1+J)
   91    CONTINUE
         DO 92 K = 1,4
           CHA4(K) = CHAINT(ITABL(JELNF,I,JELNNM-1+K))
   92    CONTINUE
      ELNFNM(I) = CHA4(1)//CHA4(2)//CHA4(3)//CHA4(4)
   90 CONTINUE
C
C  ... Bank ELTY
C
      JELTY = IW(NAMIND('ELTY'))
      NC = LCOLS(JELTY)
      NR = LROWS(JELTY)
      IF(NR.NE.NELTY)                 GO TO 998
      DO 100 I = 1,NR
         ELTYID(I) = ITABL(JELTY,I,JELTID)
         ELTYEA(I) = ITABL(JELTY,I,JELTEA)
         ELTYEF(I) = ITABL(JELTY,I,JELTEF)
         ELTYET(I) = ITABL(JELTY,I,JELTET)
  100 CONTINUE
C
C  ... Bank EMAT
C
      JEMAT = IW(NAMIND('EMAT'))
      NC = LCOLS(JEMAT)
      NR = LROWS(JEMAT)
      IF(NR.NE.NEMAT)                 GO TO 998
      DO 110 I = 1,NR
         EMATID(I) = ITABL(JEMAT,I,JEMAID)
         EMATMI(I) = ITABL(JEMAT,I,JEMAMI)
         EMATAW(I) = RTABL(JEMAT,I,JEMAAW)
         EMATAN(I) = RTABL(JEMAT,I,JEMAAN)
         EMATDE(I) = RTABL(JEMAT,I,JEMADE)
         EMATRL(I) = RTABL(JEMAT,I,JEMARL)
         EMATAL(I) = RTABL(JEMAT,I,JEMAAL)
         DO 109 K = 1,4
           CHA4(K) = CHAINT(ITABL(JEMAT,I,JEMAMN-1+K))
  109    CONTINUE
      EMATMN(I) = CHA4(1)//CHA4(2)//CHA4(3)//CHA4(4)
  110 CONTINUE
C
C  ... Bank EPHY
C
      JEPHY = IW(NAMIND('EPHY'))
      NC = LCOLS(JEPHY)
      NR = LROWS(JEPHY)
      IF(NR.GT.NEPHY)                 GO TO 998
      LEPHY=NR
      DO 120 I = 1,NR
         EPHYID(I) = ITABL(JEPHY,I,JEPHID)
         EPHYEQ(I) = ITABL(JEPHY,I,JEPHEQ)
         EPHYES(I) = ITABL(JEPHY,I,JEPHES)
         EPHYVL(1,I) = ITABL(JEPHY,I,JEPHVR)
         EPHYVL(2,I) = ITABL(JEPHY,I,JEPHVR+1)
         DO 121 J=1,ECALLS-1
           EPHYLE(J,I) = RTABL(JEPHY,I,JEPHLE-1+J)
           EPHYAN(J,I) = RTABL(JEPHY,I,JEPHAN-1+J)
  121    CONTINUE
         DO 123 K = 1,4
           CHA4(K) = CHAINT(ITABL(JEPHY,I,JEPHNM-1+K))
  123    CONTINUE
      EPHYNM(I) = CHA4(1)//CHA4(2)//CHA4(3)//CHA4(4)
  120 CONTINUE
C
C  ... Bank EPLN
C
      JEPLN = IW(NAMIND('EPLN'))
      NC = LCOLS(JEPLN)
      NR = LROWS(JEPLN)
      IF(NR.NE.ECALPL)                GO TO 998
      DO 130 I = 1,NR
         EPLNID(I) = ITABL(JEPLN,I,JEPLID)
         EPLNES(I) = ITABL(JEPLN,I,JEPLES)
         EPLNPI(I) = ITABL(JEPLN,I,JEPLPI)
         EPLNPS(I) = ITABL(JEPLN,I,JEPLPS)
  130 CONTINUE
C
C  ... Bank EPSC
C
      JEPSC = IW(NAMIND('EPSC'))
      NC = LCOLS(JEPSC)
      NR = LROWS(JEPSC)
      IF(NR.NE.(ECALPL+1)*ECALSC)     GO TO 998
      DO 140 I = 1,ECALSC
         DO 141 J=1,ECALPL+1
           NRCUR = (ECALPL+1)*(I-1) + J
           EPSCID(J,I) = ITABL(JEPSC,NRCUR,JEPSID)
           EPSCPC(1,J,I) = RTABL(JEPSC,NRCUR,JEPSPC)
           EPSCPC(2,J,I) = RTABL(JEPSC,NRCUR,JEPSPC+1)
           EPSCEP(J,I) = ITABL(JEPSC,NRCUR,JEPSEP)
           EPSCES(J,I) = ITABL(JEPSC,NRCUR,JEPSES)
  141    CONTINUE
  140 CONTINUE
C
C  ... Bank EPTY
C
      JEPTY = IW(NAMIND('EPTY'))
      NC = LCOLS(JEPTY)
      NR = LROWS(JEPTY)
      IF(NR.NE.ECALPL*ECALSY)         GO TO 998
      DO 150 I = 1,ECALSY
         DO 151 J=1,ECALPL
           NRCUR = ECALPL*(I-1) + J
           EPTYID(J,I) = ITABL(JEPTY,NRCUR,JEPTID)
           EPTYNB(J,I) = ITABL(JEPTY,NRCUR,JEPTNB)
           EPTYFS(J,I) = RTABL(JEPTY,NRCUR,JEPTFS)
           EPTYEP(J,I) = ITABL(JEPTY,NRCUR,JEPTEP)
           EPTYEQ(J,I) = ITABL(JEPTY,NRCUR,JEPTEQ)
  151    CONTINUE
  150 CONTINUE
C
C  ... Bank EQTY
C
      JEQTY = IW(NAMIND('EQTY'))
      NC = LCOLS(JEQTY)
      NR = LROWS(JEQTY)
      IF(NR.NE.NEQTY)                 GO TO 998
      DO 160 I = 1,NR
         EQTYID(I) = ITABL(JEQTY,I,JEQTID)
         EQTYMN(I) = ITABL(JEQTY,I,JEQTMN)
        DO 161 K = 1,4
         DO 162 J=1,ECALLS-1
           EQTYTP(J,K,I) = RTABL(JEQTY,I,JEQTTP-1+J+
     &                       (ECALLS-1)*(K-1))
  162    CONTINUE
  161   CONTINUE
        DO 163 K = 1,4
          CHA4(K) = CHAINT(ITABL(JEQTY,I,JEQTNM-1+K))
  163   CONTINUE
        EQTYNM(I) = CHA4(1)//CHA4(2)//CHA4(3)//CHA4(4)
  160 CONTINUE
C
C  ... Bank EROW
C
      JEROW = IW(NAMIND('EROW'))
      NC = LCOLS(JEROW)
      NR = LROWS(JEROW)
      IF(NR.NE.ECALRW+1)              GO TO 998
      DO 170 I = 1,NR
         EROWID(I) = ITABL(JEROW,I,JEROID)
         EROWEX(I) = ITABL(JEROW,I,JEROEX)
  170 CONTINUE
C
C  ... Bank ESCO
C
      JESCO = IW(NAMIND('ESCO'))
      NC = LCOLS(JESCO)
      NR = LROWS(JESCO)
      IF(NR.NE.ECALSC)                GO TO 998
      DO 180 I = 1,NR
         ESCOID(I) = ITABL(JESCO,I,JESCID)
         DO 181 J=1,ECALLS
           ESCOLI(J,I) = RTABL(JESCO,I,JESCLI-1+J)
           ESCOMD(J,1,I) = RTABL(JESCO,I,JESCMD-1+J)
           ESCOMD(J,2,I) = RTABL(JESCO,I,JESCMD-1+J+ECALLS)
  181    CONTINUE
         ESCOCF(I) = ITABL(JESCO,I,JESCCF)
         ESCORF(I) = ITABL(JESCO,I,JESCRF)
         ESCORL(I) = ITABL(JESCO,I,JESCRL)
         ESCOEC(I) = ITABL(JESCO,I,JESCEC)
         ESCOEQ(I) = ITABL(JESCO,I,JESCEQ)
         ESCOET(I) = ITABL(JESCO,I,JESCET)
C         ESCOFR(I) = ITABL(JESCO,I,JESCFR)
C         ESCOLS(I) = ITABL(JESCO,I,JESCLS)
         ESCOS1(I) = ITABL(JESCO,I,JESCS1)
         ESCOS2(I) = ITABL(JESCO,I,JESCS2)
         DO 183 K = 1,4
           CHA4(K) = CHAINT(ITABL(JESCO,I,JESCNM-1+K))
  183    CONTINUE
         ESCONM(I) = CHA4(1)//CHA4(2)//CHA4(3)//CHA4(4)
  180 CONTINUE
C introduction ad hoc des plans de base pour la definition des lignes.
         ESCORR(1,1)=2
         ESCORR(2,1)=6
         ESCORR(1,2)=2
         ESCORR(2,2)=3
         ESCORR(1,3)=2
         ESCORR(2,3)=7
C
C  ... Bank ESEC
C
      JESEC = IW(NAMIND('ESEC'))
      NC = LCOLS(JESEC)
      NR = LROWS(JESEC)
      IF(NR.NE.ECALSS)                GO TO 998
      DO 190 I = 1,NR
         ESECID(I) = ITABL(JESEC,I,JESEID)
         ESECSR(I) = ITABL(JESEC,I,JESESR)
         ESECEQ(I) = ITABL(JESEC,I,JESEEQ)
         DO 191 J=1,ECALLS
           DO 192 K=1,ECALLS
         ESECDM(K,J,I) =RTABL(JESEC,I,JESEDM-1+K+ECALLS*(J-1))
  192      CONTINUE
  191    CONTINUE
  190 CONTINUE
C
C  ... Bank ESLO
C
      JESLO = IW(NAMIND('ESLO'))
      NC = LCOLS(JESLO)
      NR = LROWS(JESLO)
      IF(NR.NE.ECALMD*ECALSC)         GO TO 998
      DO 200 I = 1,ECALSC
        DO 201 J=1,ECALMD
         NRCUR  = J+ECALMD*(I-1)
         ESLOID(J,I) = ITABL(JESLO,NRCUR,JESLID)
         ESLOEA(J,I) = ITABL(JESLO,NRCUR,JESLEA)
         ESLOEM(J,I) = ITABL(JESLO,NRCUR,JESLEM)
         ESLOEQ(J,I) = ITABL(JESLO,NRCUR,JESLEQ)
         ESLOES(J,I) = ITABL(JESLO,NRCUR,JESLES)
         DO 203 K = 1,4
           CHA4(K) = CHAINT(ITABL(JESLO,NRCUR,JESLNM-1+K))
  203    CONTINUE
         ESLONM(J,I) = CHA4(1)//CHA4(2)//CHA4(3)//CHA4(4)
  201   CONTINUE
  200 CONTINUE
C
C  ... Bank ESSC
C
      JESSC = IW(NAMIND('ESSC'))
      NC = LCOLS(JESSC)
      NR = LROWS(JESSC)
      IF(NR.NE.ECALST*ECALSC)         GO TO 998
      DO 210 I = 1,ECALSC
        DO 211 J=1,ECALST
         NRCUR =J+ECALST*(I-1)
         ESSCID(J,I) = ITABL(JESSC,NRCUR,JESSID)
         DO 212 K=1,ECALLS
           ESSCPP(K,J,I) = RTABL(JESSC,NRCUR,JESSPP-1+K)
  212    CONTINUE
         DO 213 K=1,ECALLP
           ESSCPC(K,J,I) = RTABL(JESSC,NRCUR,JESSPC-1+K)
           ESSCPS(K,J,I) = RTABL(JESSC,NRCUR,JESSPS-1+K)
  213    CONTINUE
         ESSCES(J,I) = ITABL(JESSC,NRCUR,JESSES)
         ESSCST(J,I) = ITABL(JESSC,NRCUR,JESSST)
  211   CONTINUE
  210 CONTINUE
C
C  ... Bank ESTY
C
      JESTY = IW(NAMIND('ESTY'))
      NC = LCOLS(JESTY)
      NR = LROWS(JESTY)
      IF(NR.NE.NEQTY*ECALST)          GO TO 998
      DO 220 I = 1,NEQTY
        DO 221 J=1,ECALST
         NRCUR=J+ECALST*(I-1)
         ESTYID(J,I) = ITABL(JESTY,NRCUR,JESTID)
         DO 222 K=1,ECALLS
           ESTYWF(K,J,I) = RTABL(JESTY,NRCUR,JESTWF-1+K)
           ESTYPF(K,J,I) = RTABL(JESTY,NRCUR,JESTPF-1+K)
  222    CONTINUE
         ESTYEQ(J,I) = ITABL(JESTY,NRCUR,JESTEQ)
         ESTYES(J,I) = ITABL(JESTY,NRCUR,JESTES)
  221   CONTINUE
  220 CONTINUE
C
C  ... Bank ETSC
C
      JETSC = IW(NAMIND('ETSC'))
      NC = LCOLS(JETSC)
      NR = LROWS(JETSC)
      IF(NR.NE.ECALSY)                GO TO 998
      DO 230 I = 1,NR
         ETSCID(I) = ITABL(JETSC,I,JETSID)
         ETSCEB(I) = ITABL(JETSC,I,JETSEB)
         ETSCEC(I) = ITABL(JETSC,I,JETSEC)
         DO 231 J=1,ECALLS
           DO 232 K=1,ECALLS
         ETSCDM(K,J,I) =RTABL(JETSC,I,JETSDM-1+K+ECALLS*(J-1))
  232      CONTINUE
  231    CONTINUE
         ETSCAF(I) = RTABL(JETSC,I,JETSAF)
         DO 233 K = 1,4
           CHA4(K) = CHAINT(ITABL(JETSC,I,JETSNM-1+K))
  233    CONTINUE
         ETSCNM(I) = CHA4(1)//CHA4(2)//CHA4(3)//CHA4(4)
  230 CONTINUE
C
C  ... Bank EVLF
C
      JEVLF = IW(NAMIND('EVLF'))
      NC = LCOLS(JEVLF)
      NR = LROWS(JEVLF)
      IF(NR.NE.NEVLF)                 GO TO 998
      DO 240 I = 1,NR
         EVLFID(I) = ITABL(JEVLF,I,JEVLFI)
         EVLFSG(I) = ITABL(JEVLF,I,JEVLSG)
         EVLFEA(I) = ITABL(JEVLF,I,JEVLEA)
         EVLFEL(I) = ITABL(JEVLF,I,JEVLEL)
         EVLFEV(I) = ITABL(JEVLF,I,JEVLEV)
  240 CONTINUE
C
C  ... Bank EVLS
C
      JEVLS = IW(NAMIND('EVLS'))
      NC = LCOLS(JEVLS)
      NR = LROWS(JEVLS)
      IF(NR.NE.ECALSC*ECALNP)         GO TO 998
      DO 250 I = 1,ECALSC
         DO 251 J=1,ECALNP
           NRCUR = J+ECALNP*(I-1)
           EVLSID(J,I) = ITABL(JEVLS,NRCUR,JEVLSI)
           DO 253 K = 1,4
             CHA4(K) = CHAINT(ITABL(JEVLS,NRCUR,JEVLNM-1+K))
  253      CONTINUE
           EVLSNM(J,I) = CHA4(1)//CHA4(2)//CHA4(3)//CHA4(4)
           EVLSVL(J,I) = ITABL(JEVLS,NRCUR,JEVLVL)
           EVLSZN(J,I) = ITABL(JEVLS,NRCUR,JEVLZN)
  251    CONTINUE
  250 CONTINUE
C
C  ... Bank EVOL
C
      JEVOL = IW(NAMIND('EVOL'))
      NC = LCOLS(JEVOL)
      NR = LROWS(JEVOL)
      IF(NR.NE.NEVOL)                 GO TO 998
      DO 260 I = 1,NR
         EVOLID(I) = ITABL(JEVOL,I,JEVOID)
         DO 263 K = 1,4
           CHA4(K) = CHAINT(ITABL(JEVOL,I,JEVONM-1+K))
  263    CONTINUE
         EVOLNM(I) = CHA4(1)//CHA4(2)//CHA4(3)//CHA4(4)
         EVOLEM(I) = ITABL(JEVOL,I,JEVOEM)
         EVOLEQ(I) = ITABL(JEVOL,I,JEVOEQ)
         EVOLET(I) = ITABL(JEVOL,I,JEVOET)
         EVOLFR(I) = ITABL(JEVOL,I,JEVOFR)
         EVOLLS(I) = ITABL(JEVOL,I,JEVOLS)
  260 CONTINUE
C
C  ... Bank EXRG
C
      JEXRG = IW(NAMIND('EXRG'))
      NC = LCOLS(JEXRG)
      NR = LROWS(JEXRG)
      IF (NR.NE.ECALXG+1)             GO TO 998
      DO 270 I = 1,NR
         EXRGID(I) = ITABL(JEXRG,I,JEXRID)
         EXRGPC(1,I) = RTABL(JEXRG,I,JEXRPC)
         EXRGPC(2,I) = RTABL(JEXRG,I,JEXRPC+1)
         EXRGER(I) = ITABL(JEXRG,I,JEXRER)
  270 CONTINUE
C
C  ... Bank EXRO
C
      JEXRO = IW(NAMIND('EXRO'))
      NC = LCOLS(JEXRO)
      NR = LROWS(JEXRO)
      IF (NR.NE.ECALXW)               GO TO 998
      DO 280 I = 1,NR
         EXROID(I) = ITABL(JEXRO,I,1)
         EXROER(I) = ITABL(JEXRO,I,2)
         EXROES(I) = ITABL(JEXRO,I,3)
  280 CONTINUE
C
      RETURN
  998 IGOOD  = 0
      END
#endif
