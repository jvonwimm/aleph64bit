      SUBROUTINE LELECT
C-----------------------------------------------------------------------
C! Construct luminosity electron candidates
C!
C!   Author   : P. H. Hansen   881201
C!   Modified : H. Meinhard    24-Apr-1991
C!   Modified : P. Hansen      01-Aug-1991
C!   Modified : P. Hansen      01-May-1996 LIDT back on 'T' list
C!   Input    : 'LSDA' 'LCLU' 'LSIN' 'LMAP'
C!   Output   : 'LIDT' 'LONL' 'LOCL'
C!   Created  : 'LIDT' 'LOCL'
C!
C!   Description :
C!   ===========
C?     Create cluster bank LOBJ in parallel with LCLU
C?     Create electron identification bank LIDT and bank LOCL for local
C?     coordinates
C?     For each cluster
C?       find central towers
C?       find R estimators
C?       correct barycenter and energy for geometrical effects
C?       fill attributes of LOBJ
C?       If (Eclus > 3 GeV) fill a new LIDT row
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "bcs.h"
#include "rflags.h"
#include "lcnamc.h"
#include "lsdajj.h"
#include "lclujj.h"
#include "lobjjj.h"
#include "lidtjj.h"
#include "locljj.h"
#include "lcrejj.h"
#include "lwbank.h"
#include "lmapjj.h"
#include "alcons.h"
C
      DIMENSION DIST(4),XOLD(3),XNEW(3),XLOC(3)
      DIMENSION ENE4(3),ENE9(3),R(6)
      EXTERNAL NDROP,VMOD
#include "bmacro.h"
C----------------------------------------------------------------------
C
C Check if lumi clusters exist
      KLCLU = IW(NALCLU)
      NCLU = 0
      IF(KLCLU.GT.0) NCLU = LROWS(KLCLU)
      IF(NCLU .EQ. 0)                                   GOTO 999
C
C Create LOBJ
      LEN = LMHLEN + LLOBJA*NCLU
      CALL AUBOS('LOBJ',0,LEN,KLOBJ,IGARB)
      IF(IGARB.EQ.2)                                    GOTO 997
      IW(KLOBJ+1) = LLOBJA
      IW(KLOBJ+2) = NCLU
      CALL BLIST(IW,'T+','LOBJ')
C
C Create LIDT;
      LEN = LMHLEN + LLIDTA*NCLU
      CALL AUBOS('LIDT',0,LEN,KLIDT,IGARB)
      IF(IGARB.EQ.2)                                    GOTO 997
      IW(KLIDT+1) = LLIDTA
      CALL BLIST(IW,'T+','LIDT')
C
C Create LOCL
      LEN = LMHLEN + LLOCLA*NCLU
      CALL AUBOS('LOCL',0,LEN,KLOCL,IGARB)
      IF (IGARB.EQ.2)                                   GOTO 997
      IW(KLOCL+1) = LLOCLA
      CALL BLIST(IW,'T+','LOCL')
C
C Restore bank pointers
      KLCLU = IW(NALCLU)
      KLMAP = IW(NALMAP)
      KLSDA = IW(NALSDA)
      KLOBJ = IW(NALOBJ)
      KLCRE = IW(NALCRE)
C
C For each cluster
      NIDT = 0
      DO 300 ICLU=1,NCLU
C
C Find the central towers, their barycenter
C and the distance from the barycenter to the acceptance
        THOLD = RTABL(KLCLU,ICLU,JLCLT4)
        PHOLD = RTABL(KLCLU,ICLU,JLCLF4)
        RAOLD = RTABL(KLCLU,ICLU,JLCLR4)
        EOLD = RTABL(KLCLU,ICLU,JLCLE4)
        CALL LCLUTW(THOLD,PHOLD,IADDR,DIST,ENE4,ENE9)
C
C Find the corresponding LSDA row
        IF(IADDR .EQ. 0)                   GOTO 300
        ITOW = IW(KLMAP+LMHLEN+IADDR)
C
C Find the shower shape (R) estimators
        CALL LESTIM(IADDR,EOLD,ENE4,ENE9,R)
C
C Correct the barycenter for geometrical effects
        XOLD(1) = RAOLD*COS(PHOLD)*SIN(THOLD)
        XOLD(2) = RAOLD*SIN(PHOLD)*SIN(THOLD)
        XOLD(3) = RAOLD*COS(THOLD)
        CALL LCORRC(IADDR,DIST,XOLD,EOLD,XNEW,ENEW,XLOC)
        RANEW = VMOD(XNEW(1),3)
        THNEW = ACOS(XNEW(3)/RANEW)
        PHNEW = ATAN2(XNEW(2),XNEW(1))
        IF(PHNEW.LT.0.) PHNEW=PHNEW+TWOPI
C
C Fill LOBJ
        KCL = KROW(KLOBJ,ICLU)
        RW(KCL+JLOBEC) = ENEW
        RW(KCL+JLOBTC) = THNEW
        RW(KCL+JLOBPC) = PHNEW
        IW(KCL+JLOBKD) = 64+128
        IW(KCL+JLOBKT) = 1
        RW(KCL+JLOBR1) = R(1)
        RW(KCL+JLOBR2) = R(2)
        RW(KCL+JLOBR3) = R(3)
        RW(KCL+JLOBR4) = R(4)
C
C If E > energy cut
        ICHA = 0
        IF(ENEW.GT.RTABL(KLCRE,1,JLCREI)) ICHA=1
        IF(ICHA.EQ.1) THEN
C
C .. make a new entry in LIDT
          NIDT = NIDT + 1
C
C .. store the relation in LOBJ
          IW(KCL+JLOBLI) = NIDT
          IW(KCL+JLOBKT) = 2
C
C .. fill LIDT columns
          KID = KROW(KLIDT,NIDT)
          RW(KID+JLIDEC) = ENEW
          RW(KID+JLIDTC) = THNEW
          RW(KID+JLIDPC) = PHNEW
          RW(KID+JLIDE4) = ENE4(1)+ENE4(2)+ENE4(3)
          RW(KID+JLIDE1) = ENE4(1)/AMAX1(RW(KID+JLIDE4),0.1)
          RW(KID+JLIDE2) = ENE4(2)/AMAX1(RW(KID+JLIDE4),0.1)
          RW(KID+JLIDR1) = R(5)
          RW(KID+JLIDR2) = R(6)
          RW(KID+JLIDXX) = RTABL(KLCLU,ICLU,JLCLXX)
          RW(KID+JLIDYY) = RTABL(KLCLU,ICLU,JLCLYY)
          RW(KID+JLIDXY) = RTABL(KLCLU,ICLU,JLCLXY)
          CALL UCOPY(DIST(1),RW(KID+JLIDDA),2)
          IW(KID+JLIDLO) = ICLU
C Central tower
          KLSDA = IW(NALSDA)
          ICEN = ITOW
          EMAX = 0.
          DO 200 IT=1,LROWS(KLSDA)
            IF(ITABL(KLSDA,IT,JLSDLC).NE.ICLU) GOTO 200
            E = RTABL(KLSDA,IT,JLSDEN)+RTABL(KLSDA,IT,JLSDEN+1)
     &                                +RTABL(KLSDA,IT,JLSDEN+2)
            IF(E.LT.EMAX) GOTO 200
            ICEN = IT
            EMAX = E
  200     CONTINUE
          IW(KID+JLIDLS) = ICEN
C
C .. fill LOCL columns
          KLO = KROW(KLOCL,NIDT)
          RW(KLO+JLOCXL) = XLOC(1)
          RW(KLO+JLOCYL) = XLOC(2)
          RW(KLO+JLOCZL) = XLOC(3)
        ENDIF
  300 CONTINUE
C
C Shrink LIDT and LOCL to real size
      IW(KLIDT+2) = NIDT
      IW(KLOCL+2) = NIDT
      IF(NIDT.LT.NCLU) THEN
        LEN = LMHLEN + LLIDTA*NIDT
        CALL AUBOS('LIDT',0,LEN,KLIDT,IGARB)
        IF(IGARB.EQ.2)                                 GOTO 997
        LEN = LMHLEN + LLOCLA*NIDT
        CALL AUBOS('LOCL',0,LEN,KLOCL,IGARB)
        IF(IGARB.EQ.2)                                 GOTO 997
      ENDIF
C
C Dump the output banks on request
      IF(FDEBRF.AND.JDBDRF(JULLC).GE.2) CALL PRTABL('LOBJ',0)
C
C Update general event statistics
      CALL LSTATI
C
      GOTO 999
C
  997 CALL REPORT('LELECT','AUBOS failure',1)
  999 CONTINUE
      END
#endif
