      SUBROUTINE EPRCOM (KEY)
C.----------------------------------------------------------------------
CKEY ECALDES PRINT COMMON / INTERNAL
C     M.Rumpf            November 87     Modification H V 14/03/90
C! Print ECAL commons contents for database interface checks and DEBUG
C   Input : KEY character * 4
C Valid KEY values  are :
C      'ALL'   ,'EALI'  ,'EBPL'  ,'ECGN'  ,'ECMT'  ,'ECNC'  ,'ECOL'  ,
C      'ECRP'  ,'EECB'  ,'EFAC'  ,'ELNF'  ,'ELOC'  ,'ELTY'  ,'EMAT'  ,
C      'EMOD'  ,'EPHY'  ,'EPLN'  ,'EPSC'  ,'EPTY'  ,'EQTY'  ,'EREG'  ,
C      'EROW'  ,'ESCO'  ,'ESEC'  ,'ESLO'  ,'ESSC'  ,'ESTK'  ,'ESTY'  ,
C      'ETSC'  ,'ETYV'  ,'EVLF'  ,'EVLS'  ,'EVOL'  ,'EXRG'  ,'EXRO'
C   Called by USER
C.----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "ecccom.h"
#include "erdvrs.h"
C    Locales
      INTEGER I,J,K,L,M,NPKEYS,KEYOK
      CHARACTER * (*) KEY
      PARAMETER (NPKEYS=35)
      CHARACTER * 6 PRKEYS (NPKEYS)
C
      DATA PRKEYS /
     & 'ALL'   ,'EALI'  ,'EBPL'  ,'ECGN'  ,'ECMT'  ,'ECNC'  ,'ECOL'  ,
     & 'ECRP'  ,'EECB'  ,'EFAC'  ,'ELNF'  ,'ELOC'  ,'ELTY'  ,'EMAT'  ,
     & 'EMOD'  ,'EPHY'  ,'EPLN'  ,'EPSC'  ,'EPTY'  ,'EQTY'  ,'EREG'  ,
     & 'EROW'  ,'ESCO'  ,'ESEC'  ,'ESLO'  ,'ESSC'  ,'ESTK'  ,'ESTY'  ,
     & 'ETSC'  ,'ETYV'  ,'EVLF'  ,'EVLS'  ,'EVOL'  ,'EXRG'  ,'EXRO'  /
C
      LOUTIO = IW(6)
C
C Check if KEY is valid
C
      DO 1 I=1,NPKEYS
        IF(KEY.EQ.PRKEYS(I)) GO TO 2
    1 CONTINUE
      GO TO 998
C
    2 CONTINUE
      WRITE(LOUTIO,1000) ERDVRS,DATVRS
 1000 FORMAT(' ++++++ ECAL GEOMETRY PACKAGE COMMONS CONTENT ++++++'/,
     &                12X,' version ',I5,' date ',I8)
C
Comdeck EALI (build)
C
      IF(KEY .EQ. 'EALI'   .OR. KEY .EQ. 'ALL')               THEN
      WRITE(LOUTIO,1100)
 1100 FORMAT(/,' +++ Common EALICC +++')
      DO 101 J=1,ECALSC
      DO 102 I=1,ECALMD
        WRITE(LOUTIO,1101) EALIID(I,J)
 1101   FORMAT(' EALIID(ECALMD,ECALSC) ',I10)
        WRITE(LOUTIO,1102)((EALIAM(K,L,I,J),K=1,ECALLS),L=1,ECALLS)
 1102   FORMAT(' EALIAM(ECALLS,ECALLS,ECALMD,ECALSC)',/,(4(1X,F12.5)))
        WRITE(LOUTIO,1103)((EALITD(K,L,I,J),K=1,ECALLS),L=1,ECALLS)
 1103   FORMAT(' EALITD(ECALLS,ECALLS,ECALMD,ECALSC)',/,(4(1X,F12.5)))
        WRITE(LOUTIO,1104)((EALIMP(K,L,I,J),K=1,ECALLS-1),L=1,4)
 1104   FORMAT(' EALIMP(ECALLS-1, 4,ECALMD,ECALSC)',/,(3(1X,F12.5)))
        WRITE(LOUTIO,1105)((EALIPL(K,L,I,J),K=1,ECALLS),L=1,ECALLP)
 1105   FORMAT(' EALIPL(ECALLS,ECALLP,ECALMD,ECALSC)',/,(4(1X,F12.5)))
        WRITE(LOUTIO,1106)(EALISE(K,I,J),K=1,ECALLS)
 1106   FORMAT(' EALISE(ECALLS,ECALMD,ECALSC)',/,(4(1X,F12.5)))
        WRITE(LOUTIO,1107)((EALICL(K,L,I,J),K=1,ECALLS),L=1,ECALLP)
 1107   FORMAT(' EALICL(ECALLS,ECALLP,ECALMD,ECALSC)',/,(4(1X,F12.5)))
        WRITE(LOUTIO,1108)(((EALIRW(K,L,M,I,J),K=1,ECALLS),L=1,ECALNP),
     &  M=1,ECALSS)
 1108   FORMAT(' EALIRW(ECALLS,ECALLP,ECALSS,ECALMD,ECALSC)',
     &  /,(4(1X,F12.5)))
        WRITE(LOUTIO,1109)((EALILP(K,L,I,J),K=1,ECALLS),L=1,ECALNP)
 1109   FORMAT(' EALILP(ECALLS,ECALNP,ECALMD,ECALSC)',/,(4(1X,F12.5)))
  102 CONTINUE
  101 CONTINUE
                                                              ENDIF
C
Comdeck EBPL
C
      IF(KEY .EQ. 'EBPL'   .OR. KEY .EQ. 'ALL')               THEN
      WRITE(LOUTIO,1110)
 1110 FORMAT(/,' +++ Common EBPLCC +++')
      WRITE(LOUTIO,1111)(EBPLID(I),(EBPLVL(J,I),J=1,2)
     &,(EBPLLF(J,I),J=1,ECALLS)  ,I=1,NEBPL)
 1111 FORMAT(' EBPLID(NEBPL),EBPLVL(NEBPL),EBPLLF(ECALLS,NEBPL)',/,
     & (3I10,4(1X,F12.5)))
                                                              ENDIF
C
Comdeck ECGN
C
      IF(KEY .EQ. 'ECGN' .OR. KEY .EQ. 'ALL')               THEN
      WRITE(LOUTIO,1002)
 1002 FORMAT(/,' +++ Common ECALCC +++')
      WRITE(LOUTIO,20)ECALID,ECALLS,ECALLP,ECALSC,ECALSY,ECALMD,ECALPL,
     & ECALST,ECALCL,ECALRG,ECALSS,ECALRW,ECALXW,ECALEC,ECALXG,ECALNP
 20   FORMAT(1X,'ECALID,ECALLS,ECALLP,',
     &'ECALSC,ECALSY,ECALMD,ECALPL,ECALST',/,8I10,/,
     &'ECALCL,ECALRG,ECALSS,ECALRW,ECALXW,ECALEC,ECALXG,ECALNP'
     &,/,8I10)
      WRITE(LOUTIO,22)ECALPR,ECALAL,ECALC1,ECALC2,ECALM1,ECALM2
 22   FORMAT(1X,'ECALPR,ECALAL,ECALC1,ECALC2,ECALM1,ECALM2',/,6I10)
      WRITE(LOUTIO,23)ECALBL,ECALBO,ECALEI,ECALEW,ECALEL,ECALGP,ECALWS,
     & ECALAP,ECALTI
 23   FORMAT(1X,'ECALBL,ECALBO,ECALEI,ECALEW,ECALEL,ECALGP,ECALWS,',
     &'ECALAP,ECALTI',/,9F10.5)
      WRITE(LOUTIO,21)((ECALDM(J,I),J=1,ECALLS),I=1,ECALLS)
 21   FORMAT(' ECALDM(ECALLS,ECALLS)',/,4(1X,F12.5))
                                                              ENDIF
C
Comdeck ECMT
C
      IF(KEY .EQ. 'ECMT'   .OR. KEY .EQ. 'ALL')               THEN
      WRITE(LOUTIO,1150)
 1150 FORMAT(/,' +++ Common ECMTCC +++')
      WRITE(LOUTIO,1151)(ECMTID(I),ECMTMP(I),ECMTCP(I),
     & ECMTEL(I),I=1,NECMT)
 1151 FORMAT(' ECMTID,ECMTMP,ECMTCP,ECMTEL ...(NECMT)',/,
     &        (I7,F9.5,2I7))
                                                              ENDIF
C
Comdeck ECNC
C
      IF(KEY .EQ. 'ECNC'   .OR. KEY .EQ. 'ALL')               THEN
      WRITE(LOUTIO,1112)
 1112 FORMAT(/,' +++ Common ECNCCC +++')
      WRITE(LOUTIO,1113)(ECNCID(I),ECNCEC(I),ECNCEQ(I),
     &                   ECNCET(I),I=1,NECNC)
 1113 FORMAT(' ECNCID,ECNCEC,ECNCEQ,ECNCET ...(NECNC)',/,(4(1X,I10)))
                                                              ENDIF
C
Comdeck ECOL
C
      IF(KEY .EQ. 'ECOL'   .OR. KEY .EQ. 'ALL')               THEN
      WRITE(LOUTIO,1116)
 1116 FORMAT(/,' +++ Common ECOLCC +++')
      WRITE(LOUTIO,1117)(ECOLID(I),(ECOLPC(J,I),J=1,ECALLP),
     &                   ECOLER(I),I=1,ECALCL)
 1117 FORMAT(' ECOLID(ECALCL),ECOLPC(ECALLP,ECALCL),ECOLER(ECALCL),'
     &    ,/,(I10,2F10.5,I10))
                                                              ENDIF
C
Comdeck ECRP
C
      IF(KEY .EQ. 'ECRP'   .OR. KEY .EQ. 'ALL')               THEN
      WRITE(LOUTIO,1160)
 1160 FORMAT(/,' +++ Common ECRPCC +++')
      WRITE(LOUTIO,1161) (ECRPID(I),ECRPEC(I),ECRPET(I),ECRPP1(I),
     &                    ECRPP2(I),ECRPP3(I),I=1,NECRP)
 1161 FORMAT(' ECRPID,ECRPEC,ECRPET,ECRPP1,ECRPP2,ECRPP3 ...(NECRP)',
     & /,(6I10))
                                                              ENDIF
C
Comdeck EECB
C
      IF(KEY .EQ. 'EECB'   .OR. KEY .EQ. 'ALL')               THEN
      WRITE(LOUTIO,1114)
 1114 FORMAT(/,' +++ Common EECBCC +++')
      WRITE(LOUTIO,1115) EECBID,((EECBDM(J,I),J=1,ECALLS),I=1,ECALLS)
 1115 FORMAT(' EECBID,EECBDM(ECALLS,ECALLS)',/,I4,4(/,4F10.5))
                                                              ENDIF
C
Comdeck EFAC
C
      IF(KEY .EQ. 'EFAC'   .OR. KEY .EQ. 'ALL')               THEN
      WRITE(LOUTIO,1170)
 1170 FORMAT(/,' +++ Common EFACCC +++')
      WRITE(LOUTIO,1171) (EFACID(I),EFACEA(I),EFACEC(I),EFACEF(I),
     &                    EFACET(I),I=1,NEFAC)
 1171 FORMAT(' EFACID,EFACEA,EFACEC,EFACEF,EFACET ...(NEFAC)',
     & /,(5I10))
                                                              ENDIF
C
Comdeck ELNF
C
      IF(KEY .EQ. 'ELNF'   .OR. KEY .EQ. 'ALL')               THEN
      WRITE(LOUTIO,1130)
 1130 FORMAT(/,' +++ Common ELNFCC-ELNFCH +++')
      WRITE(LOUTIO,1131)(ELNFID(I),ELNFNM(I),(ELNFLF(J,I),
     & J=1,ECALLS),I=1,NELNF)
 1131 FORMAT(1X,'ELNFID(NELNF),ELNFNM(NELNF),ELNFLF(ECALLS,NELNF)',/,
     &      (I4,3X,A16,1X,F12.4,1X,F12.4,1X,F5.1,1X,F10.2))
                                                              ENDIF
C
Comdeck ELOC
C
      IF(KEY .EQ. 'ELOC'   .OR. KEY .EQ. 'ALL')               THEN
      WRITE(LOUTIO,1200)
 1200 FORMAT(/,' +++ Common ELOCCC +++')
      WRITE(LOUTIO,1201)ELOCID,((ELOCPL(I,J),I=1,ECALLS),J=1,ECALLP),
     & ((ELOCCL(I,J),I=1,ECALLS),J=1,ECALLP),
     & (((ELOCRW(I,J,K),I=1,ECALLS),J=1,ECALLP),K=1,ECALSS),
     & ((ELOCLP(I,J),I=1,ECALLS),J=1,ECALNP),
     & (ELOCSE(I),I=1,ECALLS),ELOCEM,ELOCES
 1201 FORMAT(' ELOCID,ELOCPL(ECALLS,ECALLP),ELOCCL(ECALLS,ECALLP),',
     & 'ELOCRW(ECALLS,ECALLP,ECALSS),ELOCLP(ECALLS,ECALNP),',
     & 'ELOCSE(ECALLS),ELOCEM,ELOCES',/,
     &      (I10,8(/,8F10.5),/,4F10.5,/,2I10))
                                                              ENDIF
C
Comdeck ELTY
C
      IF(KEY .EQ. 'ELTY'   .OR. KEY .EQ. 'ALL')               THEN
      WRITE(LOUTIO,1180)
 1180 FORMAT(/,' +++ Common ELTYCC +++')
      WRITE(LOUTIO,1181) (ELTYID(I),ELTYEA(I),ELTYEF(I),ELTYET(I),
     &       I=1,NELTY)
 1181 FORMAT(' ELTYID,ELTYEA,ELTYEF,ELTYET ...(NELTY)',/,(4I10))
                                                              ENDIF
C
Comdeck EMAT
C
      IF(KEY .EQ. 'EMAT'   .OR. KEY .EQ. 'ALL')               THEN
      WRITE(LOUTIO,1152)
 1152 FORMAT(/,' +++ Common EMATCC +++')
      WRITE(LOUTIO,1153)(EMATID(I),EMATMI(I),EMATMN(I),EMATAW(I),
     & EMATAN(I),EMATDE(I),EMATRL(I),EMATAL(I),I=1,NEMAT)
 1153 FORMAT(' EMATID,EMATMI,EMATMN,EMATAW,EMATAN,EMATDE,EMATRL,EMATAL'
     &,' ...(NEMAT)',/,(2I4,2X,A16,5F12.4))
                                                              ENDIF
C
Comdeck EMOD
C
      IF(KEY .EQ. 'EMOD'   .OR. KEY .EQ. 'ALL')               THEN
      WRITE(LOUTIO,1154)
 1154 FORMAT(/,' +++ Common EMODCC +++')
      WRITE(LOUTIO,1155)(EMODID(I),((EMODDM(J,K,I),J=1,ECALLS),
     & K=1,ECALLS),(EMODPC(J,I),J=1,ECALLP),I=1,ECALMD)
 1155 FORMAT(' EMODID(ECALMD),EMODDM(ECALLS,ECALLS,ECALMD),',
     &     'EMODPC(ECALLP,ECALMD)',/,
     &(I10,/,4(4F10.5,/),2F10.5))
                                                              ENDIF
C
Comdeck EPHY
C
      IF(KEY .EQ. 'EPHY'   .OR. KEY .EQ. 'ALL')               THEN
      WRITE(LOUTIO,1186)
 1186 FORMAT(/,' +++ Common EPHYCC +++')
      WRITE(LOUTIO,1187) (EPHYID(I),EPHYNM(I),EPHYEQ(I),EPHYES(I),
     &  (EPHYVL(J,I),J=1,2),(EPHYLE(J,I),J=1,ECALLS-1),
     &  (EPHYAN(J,I),J=1,ECALLS-1),
     &      I=1,LEPHY)
 1187 FORMAT(' EPHYID(NEPHY),EPHYNM(NEPHY),EPHYEQ(NEPHY),',
     &'EPHYES(NEPHY),EPHYVL(2,NEPHY)',/,' EPHYLE(ECALLS-1,NEPHY),',
     &'EPHYAN(ECALLS-1,NEPHY)',/,
     & (I10,1X,A16,4I10,2(/3(1X,F12.5))))
                                                              ENDIF
C
Comdeck EPLN
C
      IF(KEY .EQ. 'EPLN' .OR. KEY .EQ. 'ALL')               THEN
      WRITE(LOUTIO,1020)
 1020 FORMAT(/,' +++ Common EPLNCC +++')
      WRITE(LOUTIO,1021) (EPLNID(I),EPLNES(I),EPLNPI(I),EPLNPS(I),
     & I=1,ECALPL)
 1021 FORMAT(' EPLNID,EPLNES,EPLNPI,EPLNPS..(ECALPL)',/,(4I10))
                                                              ENDIF
C
Comdeck EPSC
C
      IF(KEY .EQ. 'EPSC' .OR. KEY .EQ. 'ALL')               THEN
      WRITE(LOUTIO,1022)
 1022 FORMAT(/,' +++ Common EPSCCC +++')
      WRITE(LOUTIO,1023) ((EPSCID(I,J),(EPSCPC(K,I,J),K=1,ECALLP),
     & EPSCEP(I,J),EPSCES(I,J),I=1,ECALPL+1),J=1,ECALSC)
 1023 FORMAT(' EPSCID(ECALPL+1,ECALSC),EPSCPC(ECALLP,ECALPL+1,ECALSC),',
     &'EPSCEP(ECALPL+1,ECALSC),EPSCES(ECALP+1L,ECALSC)',/,
     &(I10,2F10.5,2I10))
                                                              ENDIF
C
Comdeck EPTY
C
      IF(KEY .EQ. 'EPTY' .OR. KEY .EQ. 'ALL')               THEN
      WRITE(LOUTIO,1026)
 1026 FORMAT(/,' +++ Common EPTYCC +++')
      WRITE(LOUTIO,1027) ((EPTYID(J,I),EPTYNB(J,I),EPTYFS(J,I),
     & EPTYEP(J,I),EPTYEQ(J,I),J=1,ECALPL),I=1,ECALSY)
 1027 FORMAT(' EPTYID(ECALPL,ECALSY),EPTYNB(ECALPL,ECALSY),',
     &'EPTYFS(ECALPL,ECALSY),EPTYEP(ECALPL,ECALSY),',
     &'EPTYEQ(ECALPL,ECALSY)',/,
     & (2I10,F10.5,2I10))
                                                              ENDIF
C
Comdeck EREG
C
      IF(KEY .EQ. 'EREG' .OR. KEY .EQ. 'ALL')               THEN
      WRITE(LOUTIO,1028)
 1028 FORMAT(/,' +++ Common EREGCC +++')
      WRITE(LOUTIO,1029) (EREGID(I),EREGCN(I),EREGFR(I),EREGLS(I),
     & I=1,ECALRG)
 1029 FORMAT(' EREGID(ECALRG),EREGCN(ECALRG),EREGFR(ECALRG),',
     &            ' EREGLS(ECALRG)',/
     & ,(4I10))
                                                              ENDIF
C
Comdeck EROW
C
      IF(KEY .EQ. 'EROW' .OR. KEY .EQ. 'ALL')               THEN
      WRITE(LOUTIO,1032)
 1032 FORMAT(/,' +++ Common EROWCC +++')
      WRITE(LOUTIO,1033) (EROWID(I),EROWEX(I),I=1,ECALRW+1)
 1033 FORMAT(' EROWID(ECALRW+1),EROWEX(ECALRW+1)'/,(2I10))
                                                              ENDIF
C
Comdeck EQTY
C
      IF(KEY .EQ. 'EQTY'   .OR. KEY .EQ. 'ALL')               THEN
      WRITE(LOUTIO,1188)
 1188 FORMAT(/,' +++ Common EQTYCC +++')
      WRITE(LOUTIO,1189) (EQTYID(I),EQTYNM(I),EQTYMN(I),
     &((EQTYTP(J,K,I),J=1,ECALLS-1),K=1,4),  I=1,NEQTY)
 1189 FORMAT(' EQTYID(NEQTY),EQTYNM(NEQTY),EQTYMN(NEQTY),',
     &'       EQTYTP(ECALLS-1,4,NEQTY)',/,(I10,1X,A16,I10,4(/,3F12.5)))
                                                              ENDIF
C
Comdeck ESCO
C
      IF(KEY .EQ. 'ESCO'   .OR. KEY .EQ. 'ALL')               THEN
      WRITE(LOUTIO,1190)
 1190 FORMAT(/,' +++ Common ESCOCC ESCOCH+++')
      WRITE(LOUTIO,1191) (ESCOID(I),ESCONM(I),
     &    (ESCOLI(J,I),J=1,ECALLS),
     &    ((ESCOMD(K,J,I),K=1,ECALLS),J=1,ECALLP),
     &    ESCOCF(I),ESCORF(I),ESCORL(I),ESCOEC(I),ESCOEQ(I),
     &    ESCOET(I),ESCOFR(I),ESCOLS(I),ESCOS1(I),ESCOS2(I),
     &    (ESCORR(J,I),J=1,ECALLP),
     &    I=1,ECALSC)
 1191 FORMAT(' ESCOID(ECALSC),ESCONM(ECALSC),ESCOLI(ECALLS,ECALSC)',/,
     &' ESCOMD(ECALLS,ECALLP,ECALSC)'/'ESCOCF, ESCORF, ESCORL, ESCOEC,'
     &,'ESCOEQ, ESCOET, ESCOFR, ESCOLS, ESCOS1, ESCOS2..(ECALSC)',/,
     &'ESCORR(ECALLP,ECALSC)',/,
     &(I4,1X,A16,4F10.5/8F10.5/10I10/2I10))
                                                              ENDIF
C
Comdeck ESEC
C
      IF(KEY .EQ. 'ESEC' .OR. KEY .EQ. 'ALL')               THEN
      WRITE(LOUTIO,1034)
 1034 FORMAT(/,' +++ Common ESECCC +++')
      WRITE(LOUTIO,1035) (ESECID(I),ESECSR(I),ESECEQ(I),
     &((ESECDM(J,K,I),J=1,ECALLS),K=1,ECALLS),I=1,ECALSS)
 1035 FORMAT(' ESECID(ECALSS),ESECSR(ECALSS),ESECEQ(ECALSS),',
     &            'ESECDM(ECALLS,ECALLS,ECALSS)'/,
     & (3I10,4(/,4F10.5)))
                                                              ENDIF
C
Comdeck ESLO
C
      IF(KEY .EQ. 'ESLO' .OR. KEY .EQ. 'ALL')               THEN
      WRITE(LOUTIO,3016)
 3016 FORMAT(/,' +++ Common ESLOCC  ESLOCH +++  ')
      WRITE(LOUTIO,3017)((ESLOID(J,I),ESLONM(J,I),ESLOEA(J,I),
     & ESLOEM(J,I),ESLOEQ(J,I),ESLOES(J,I),J=1,ECALMD),I=1,ECALSC)
 3017 FORMAT (' ESLOID,ESLONM,ESLOEA,ESLOEM,ESLOEQ,ESLOES ..',
     &'(ECALMD,ECALSC)',/ ,
     & (I10,1X,A16,4I10))
                                                              ENDIF
C
Comdeck ESSC
C
      IF(KEY .EQ. 'ESSC' .OR. KEY .EQ. 'ALL')               THEN
      WRITE(LOUTIO,3014)
 3014 FORMAT(/,' +++ Common ESSC +++  ')
      WRITE(LOUTIO,3015)((ESSCID(I,J),(ESSCPP(K,I,J),K=1,ECALLS),
     &(ESSCPC(K,I,J),K=1,ECALLP),(ESSCPS(K,I,J),K=1,ECALLP),
     & ESSCES(I,J),ESSCST(I,J),I=1,ECALST),J=1,ECALSC)
 3015 FORMAT (' ESSCID(ECALST,ECALSC)',/,'ESSCPP(ECALLS,ECALST,ECALSC),'
     &,'ESSCPC(ECALLP,ECALST,ECALSC),ESSCPS(ECALLP,ECALST,ECALSC)',/,
     & 'ESSCES(ECALST,ECALSC),ESSCST(ECALST,ECALSC)',/,
     & (I10,2(/4F10.5),/2I10))
                                                              ENDIF
C
Comdeck ESTK
C
      IF(KEY .EQ. 'ESTK' .OR. KEY .EQ. 'ALL')               THEN
      WRITE(LOUTIO,3004)
 3004 FORMAT(/,' +++ Common ESTK +++  ')
      WRITE(LOUTIO,3005)(ESTKID(I),ESTKFR(I),ESTKLS(I),I=1,ECALST)
 3005 FORMAT (' ESTKID, ESTKFR, ESTKLS...(ECALST)',/,(3I10))
                                                              ENDIF
C
Comdeck ESTY
C
      IF(KEY .EQ. 'ESTY' .OR. KEY .EQ. 'ALL')               THEN
      WRITE(LOUTIO,3001)
 3001 FORMAT(/,' +++ Common ESTY +++  ')
      WRITE(LOUTIO,3002)((ESTYID(I,J),
     & (ESTYWF(K,I,J),ESTYPF(K,I,J),K=1,ECALLS),
     &  ESTYEQ(I,J),ESTYES(I,J),I=1,ECALST),J=1,NEQTY)
 3002 FORMAT (' ESTYID(ECALST,NEQTY)',/,
     &' ESTYWF(ECALLS,ECALST,NEQTY),ESTYPF(ECALLS,ECALST,NEQTY),',/,
     &' ESTYEQ(ECALST,NEQTY),ESTYES(ECALST,NEQTY) ',/,
     & (I10,2(/,4F10.5),/,2I10))
                                                              ENDIF
C
Comdeck ETSC
C
      IF(KEY .EQ. 'ETSC'   .OR. KEY .EQ. 'ALL')               THEN
      WRITE(LOUTIO,3010)
 3010 FORMAT(/,' +++ Common ETSCCC ,ETSCCH+++')
      WRITE(LOUTIO,3011) (ETSCID(I),ETSCNM(I),ETSCEB(I),ETSCEC(I),
     & ((ETSCDM(K,J,I),K=1,ECALLS),J=1,ECALLS),ETSCAF(I),
     &      I=1,ECALSY)
 3011 FORMAT(' ETSCID(ECALSY),ETSCNM(ECALSY),ETSCEC(ECALSY),',
     & 'ETSCDM(ECALLS,ECALLS,ECALSY),ETSCAF(ECALSY)',/,
     &(I10,1X,A16,2I10,4(/,4F10.5),/,F10.5))
                                                              ENDIF
C
Comdeck ETYV
C
      IF(KEY .EQ. 'ETYV'   .OR. KEY .EQ. 'ALL')               THEN
      WRITE(LOUTIO,1182)
 1182 FORMAT(/,' +++ Common ETYVCC +++')
      WRITE(LOUTIO,1183) (ETYVID(I),ETYVFC(I),ETYVCR(I),ETYVNG(I),
     &    ETYVFT(I),ETYVFR(I),ETYVFF(I),ETYVLT(I),ETYVLR(I),
     &    ETYVLF(I),I=1,NETYV)
 1183 FORMAT(' ETYVID,ETYVFC,ETYVCR,ETYVNG,ETYVFT,ETYVFR,ETYVFF,',
     &       'ETYVLT,ETYVLR,ETYVLF ...(NETYV)',/,(10I6))
                                                              ENDIF
C
Comdeck EVLF
C
      IF(KEY .EQ. 'EVLF'   .OR. KEY .EQ. 'ALL')               THEN
      WRITE(LOUTIO,1140)
 1140 FORMAT(/,' +++ Common ELVFCC +++')
      WRITE(LOUTIO,1141)(EVLFID(I),EVLFSG(I),EVLFEA(I),
     & EVLFEL(I),EVLFEV(I),I=1,NEVLF)
 1141 FORMAT(' EVLFID,EVLFSG,EVLFEA,EVLFEL,EVLFEV ...(NEVLF)',/,
     &       (I4,1X,F5.0,3I5))
                                                              ENDIF
C
Comdeck EVLS
C
      IF(KEY .EQ. 'EVLS'   .OR. KEY .EQ. 'ALL')               THEN
      WRITE(LOUTIO,1184)
 1184 FORMAT(/,' +++ Common EVLSCC-EVLSCH +++')
      WRITE(LOUTIO,1185) ((EVLSID(J,I),EVLSNM(J,I),EVLSVL(J,I),
     & EVLSZN(J,I),J=1,ECALNP),I=1,ECALSC)
 1185 FORMAT(' EVLSID,EVLSNM,EVLSVL,EVLSZN ...(ECALNP,ECALSC)',
     & /,(I10,3X,A16,2I10))
                                                              ENDIF
C
Comdeck EVOL
C
      IF(KEY .EQ. 'EVOL'   .OR. KEY .EQ. 'ALL')               THEN
      WRITE(LOUTIO,1120)
 1120 FORMAT(/,' +++ Common EVOLCC-EVOLCH +++')
      WRITE(LOUTIO,1121)(EVOLID(I),EVOLNM(I),EVOLEM(I),EVOLEQ(I),
     &EVOLET(I),EVOLFR(I),EVOLLS(I),I=1,NEVOL)
 1121 FORMAT(1X,'EVOLID,EVOLNM,EVOLEM,EVOLEQ,EVOLET,EVOLFR,EVOLLS.',
     &'..(NEVOL)',/,
     &      (8X,I4,3X,A16,5(2X,I4)))
                                                              ENDIF
C
Comdeck EXRG
C
      IF(KEY .EQ. 'EXRG'   .OR. KEY .EQ. 'ALL')               THEN
      WRITE(LOUTIO,3008)
 3008 FORMAT(/,' +++ Common EXRGCC +++')
      WRITE(LOUTIO,3009)(EXRGID(I),(EXRGPC(J,I),J=1,ECALLP),
     & EXRGER(I),EXRGFR(I),EXRGLS(I),I=1,ECALXG+1)
 3009 FORMAT(' EXRGID(ECALXG),EXRGPC(ECALLP,ECALXG),EXRGER(ECALXG),',
     &'EXRGFR(ECALXG),EXRGLS(ECALXG)',/,
     &      (I10,2F10.5,3I10))
                                                              ENDIF
C
Comdeck EXRO
C
      IF(KEY .EQ. 'EXRO'   .OR. KEY .EQ. 'ALL')               THEN
      WRITE(LOUTIO,3012)
 3012 FORMAT(/,' +++ Common EXROCC +++')
      WRITE(LOUTIO,3013)(EXROID(I),EXROER(I),EXROES(I),
     &   (EXROPC(J,I),J=1,ECALLP),I=1,ECALXW)
 3013 FORMAT(' EXROID(ECALXW),EXROER(ECALXW),EXROES(ECALXW)',
     &',EXROPC(ECALLP,ECALXW)',/,
     &     (3I10,2F10.5))
                                                              ENDIF
C
  999 RETURN
C
  998 WRITE(LOUTIO,997) KEY
  997 FORMAT(1X,'+++ EPRCOM +++ ',A6,' invalid key')
      WRITE(LOUTIO,996) (PRKEYS(I),I=1,NPKEYS)
  996 FORMAT(' List of valid keys ',/,(10A10))
      END
#endif
