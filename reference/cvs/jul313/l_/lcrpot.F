      SUBROUTINE LCRPOT
C------------------------------------------------------------------
C! Create Lcal POT banks
C!
C!   Author   : P. H. Hansen   880801
C!   Modified : H. Meinhard    24-Apr-1991
C!   Input    : 'LSDA' 'LPDA' 'LCLU' 'LOBJ' 'LIDT'
C!   Output   : 'PLSD' 'PLPD'
C!              'PCOB' 'PCPA' 'PCHY' 'PCRL'
C!              'PEOB' 'PEHY' 'PEOT'
C!   Created  : 'PLSD' 'PLPD'
C!
C!   Description :
C!   ===========
C?   Create or expand packed banks for POT
C?   Zero suppression is applied:
C?    PLSD row filled if any storey with E > 50 MeV
C?    PLPD row filled for planes with E > 10 MeV
C------------------------------------------------------------------
#ifndef DOC
#include "rparac.h"
#include "bcs.h"
#include "rflags.h"
#include "lcnamc.h"
#include "lsdajj.h"
#include "lpdajj.h"
#include "lobjjj.h"
#include "lclujj.h"
#include "lidtjj.h"
#include "plsdjj.h"
#include "plpdjj.h"
#include "pchyjj.h"
#include "pehyjj.h"
#include "pcobjj.h"
#include "pcrljj.h"
#include "peobjj.h"
#include "peotjj.h"
#include "pcpajj.h"
#include "lpotun.h"
#include "lcrejj.h"
#include "alcons.h"
      EXTERNAL NAMIND
#include "bmacro.h"
C-----------------------------------------------------------------
C
C Check if LCalobjects exist
      KLOBJ = IW(NAMIND('LOBJ'))
      IF(KLOBJ.LE.0)                                    GOTO 999
      NLOB = LROWS(KLOBJ)
      IF(NLOB.LE.0)                                     GOTO 999
C
C Get the "charged" Lcalobjects
      KLIDT = IW(NALIDT)
      NLID = 0
      IF(KLIDT.GT.0) NLID = LROWS(KLIDT)
C
C Create or modify Calobject bank
      KPCOB = IW(NAMIND('PCOB'))
      NCOB = 0
      IF(KPCOB.GT.0) NCOB = LROWS(KPCOB)
      LEN = LMHLEN + (NCOB+NLOB)*LPCOBA
      CALL AUBOS('PCOB',0,LEN,KPCOB,IGARB)
      IF(IGARB.EQ.2)                              GOTO 997
      CALL BLIST(IW,'E+','PCOB')
      IW(KPCOB+1) = LPCOBA
      IW(KPCOB+2) = NCOB+NLOB
C
C Create or expand Calobject Hypothesis bank
      KPCHY = IW(NAMIND('PCHY'))
      NCHY = 0
      IF(KPCHY.GT.0) NCHY = LROWS(KPCHY)
      LEN = LMHLEN + (NCHY+NLOB)*LPCHYA
      CALL AUBOS('PCHY',0,LEN,KPCHY,IGARB)
      IF(IGARB.EQ.2)                              GOTO 997
      CALL BLIST(IW,'E+','PCHY')
      IW(KPCHY+1) = LPCHYA
      IW(KPCHY+2) = NCHY+NLOB
C
C Create or expand Calobject Relations bank
      KPCRL = IW(NAMIND('PCRL'))
      NCRL = 0
      IF(KPCRL.GT.0) NCRL = LROWS(KPCRL)
      LEN = LMHLEN + (NCRL+NLOB)*LPCRLA
      CALL AUBOS('PCRL',0,LEN,KPCRL,IGARB)
      IF(IGARB.EQ.2)                              GOTO 997
      CALL BLIST(IW,'E+','PCRL')
      IW(KPCRL+1) = LPCRLA
      IW(KPCRL+2) = NCRL+NLOB
C
C Create or expand Calobject neutral particle bank
      KPCPA = IW(NAMIND('PCPA'))
      NCPA = 0
      IF(KPCPA.GT.0) NCPA = LROWS(KPCPA)
      IF(NLOB.GT.0) THEN
         LEN = LMHLEN + (NCPA+NLOB)*LPCPAA
         CALL AUBOS('PCPA',0,LEN,KPCPA,IGARB)
         IF(IGARB.EQ.2)                           GOTO 997
         CALL BLIST(IW,'E+','PCPA')
         IW(KPCPA+1) = LPCPAA
         IW(KPCPA+2) = NCPA+NLOB
      ENDIF
C
C Create or expand ECalobject bank
      KPEOB = IW(NAMIND('PEOB'))
      NEOB = 0
      IF(KPEOB.GT.0) NEOB = LROWS(KPEOB)
      LEN = LMHLEN + (NEOB+NLOB)*LPEOBA
      CALL AUBOS('PEOB',0,LEN,KPEOB,IGARB)
      IF(IGARB.EQ.2)                              GOTO 997
      CALL BLIST(IW,'E+','PEOB')
      IW(KPEOB+1) = LPEOBA
      IW(KPEOB+2) = NEOB+NLOB
C
C Create or expand ECalobject Hypothesis bank
      KPEHY = IW(NAMIND('PEHY'))
      NEHY = 0
      IF(KPEHY.GT.0) NEHY = LROWS(KPEHY)
      LEN = LMHLEN + (NEHY+NLOB)*LPEHYA
      CALL AUBOS('PEHY',0,LEN,KPEHY,IGARB)
      IF(IGARB.EQ.2)                              GOTO 997
      CALL BLIST(IW,'E+','PEHY')
      IW(KPEHY+1) = LPEHYA
      IW(KPEHY+2) = NEHY+NLOB
C
C Create or expand ECalobject Topology bank
      KPEOT = IW(NAMIND('PEOT'))
      NEOT = 0
      IF(KPEOT.GT.0) NEOT = LROWS(KPEOT)
      LEN = LMHLEN + (NEOT+NLOB)*LPEOTA
      CALL AUBOS('PEOT',0,LEN,KPEOT,IGARB)
      IF(IGARB.EQ.2)                             GOTO 997
      CALL BLIST(IW,'E+','PEOT')
      IW(KPEOT+1) = LPEOTA
      IW(KPEOT+2) = NEOT+NLOB
C
C Create Lcal storey data bank with maximum size
      IF (IW(NAMIND('PLSD')).NE.0) CALL BDROP(IW,'PLSD')
      KLSDA = IW(NALSDA)
      NSDA = LROWS(KLSDA)
      LEN = LMHLEN + LPLSDA*NSDA
      CALL AUBOS('PLSD',0,LEN,KPLSD,IGARB)
      IF(IGARB.EQ.2)                                    GOTO 997
      CALL BLIST(IW,'E+','PLSD')
      IF(IGARB.NE.0) THEN
         KLSDA = IW(NALSDA)
      ENDIF
      NPLS = 0
      IW(KPLSD+1) = LPLSDA
      DO 100 I = 1,NSDA
         KLSD = KROW(KLSDA,I)
         IE1 = NINT(RW(KLSD+JLSDEN)/UNITES)
         IE2 = NINT(RW(KLSD+JLSDEN+1)/UNITES)
         IE3 = NINT(RW(KLSD+JLSDEN+2)/UNITES)
C
C Clear a word in LSDA (which is  not used)
         IW(KLSD+JLSDLS) = 0
C
         IF(IE1.LE.10.AND.IE2.LE.10.AND.IE3.LE.10) GOTO 100
         NPLS = NPLS+1
         KPLS = KROW(KPLSD,NPLS)
         IW(KPLS+JPLSE1) = IE1
         IW(KPLS+JPLSE2) = IE2
         IW(KPLS+JPLSE3) = IE3
         IW(KPLS+JPLSAD) = IW(KLSD+JLSDAD)
         IF(IW(KLSD+JLSDLC).NE.0) THEN
           IW(KPLS+JPLSPE) = NEOB+IW(KLSD+JLSDLC)
         ELSE
           IW(KPLS+JPLSPE) = 0
         ENDIF
C
C Store the PLSD row number in the cleared LSDA word
         IW(KLSD+JLSDLS) = NPLS
  100 CONTINUE
C
C Shrink PLSD
      IW(KPLSD+2) = NPLS
      LEN = LMHLEN + LPLSDA*NPLS
      CALL AUBOS('PLSD',0,LEN,KPLSD,IGARB)
      IF(IGARB.EQ.2)                                    GOTO 997
C
C
C Create Lcal wire data bank with maximum size
      IF (IW(NAMIND('PLPD')).NE.0) CALL BDROP(IW,'PLPD')
      KLPDA = IW(NALPDA)
      IF(KLPDA.LE.0) GOTO 220
      NPDA = LROWS(KLPDA)*(LLPDAA-1)
      LEN = LMHLEN + LPLPDA*NPDA
      CALL AUBOS('PLPD',0,LEN,KPLPD,IGARB)
      IF(IGARB.EQ.2)                                    GOTO 997
      CALL BLIST(IW,'E+','PLPD')
      IF(IGARB.NE.0) THEN
         KLPDA = IW(NALPDA)
      ENDIF
      IW(KPLPD+1) = LPLPDA
      NPLP = 0
      DO 200 I = 1,LROWS(KLPDA)
         KLPD = KROW(KLPDA,I)
         DO 210 J = 1,39
           IENE = NINT(RW(KLPD+JLPDEN+J-1)/UNITEW)
           IF(IENE.GT.10) THEN
              NPLP = NPLP+1
              KPLP = KROW(KPLPD,NPLP)
              IW(KPLP+JPLPAD) = (IW(KLPD+JLPDMO)-1)*64 + J
              IW(KPLP+JPLPEN) = IENE
            ENDIF
  210    CONTINUE
  200 CONTINUE
C Shrink PLPD
      IW(KPLPD+2) = NPLP
      LEN = LMHLEN + LPLPDA*NPLP
      CALL AUBOS('PLPD',0,LEN,KPLPD,IGARB)
      IF(IGARB.EQ.2)                                    GOTO 997
  220 CONTINUE
C
C Restore bank pointers
      KLCRE = IW(NALCRE)
      KLOBJ = IW(NALOBJ)
      KLCLU = IW(NALCLU)
      KLIDT = IW(NALIDT)
      KLSDA = IW(NALSDA)
      KPCOB = IW(NAMIND('PCOB'))
      KPCHY = IW(NAMIND('PCHY'))
      KPCRL = IW(NAMIND('PCRL'))
      KPCPA = IW(NAMIND('PCPA'))
      KPEOB = IW(NAMIND('PEOB'))
      KPEHY = IW(NAMIND('PEHY'))
      KPEOT = IW(NAMIND('PEOT'))
C
C Fill calobject banks
      DO 500 I=1,NLOB
         NCHY = NCHY+1
         NCOB = NCOB+1
         NCRL = NCRL+1
         NEOB = NEOB+1
         NEHY = NEHY+1
         NEOT = NEOT+1
C
C PCHY
         KCHY = KROW(KPCHY,NCHY)
         IW(KCHY+JPCHBH) = 1
         IW(KCHY+JPCHKT) = ITABL(KLOBJ,I,JLOBKT)
         IW(KCHY+JPCHPC) = NCOB
C
C PCRL
         KCRL = KROW(KPCRL,NCRL)
         IW(KCRL+JPCRPC) = NCOB
         IW(KCRL+JPCRPE) = NEOB
C
C PEOB
         KEOB = KROW(KPEOB,NEOB)
         ERAW = RTABL(KLCLU,I,JLCLE4)
         RW(KEOB+JPEOER) = ERAW
         RW(KEOB+JPEOE1) = RTABL(KLCLU,I,JLCLE1)/ERAW
         RW(KEOB+JPEOE2) = RTABL(KLCLU,I,JLCLE2)/ERAW
         RW(KEOB+JPEOEC) = RTABL(KLOBJ,I,JLOBEC)
         RW(KEOB+JPEOKD) = RTABL(KLOBJ,I,JLOBKD)
         RW(KEOB+JPEOR1) = RTABL(KLOBJ,I,JLOBR1)
         RW(KEOB+JPEOR2) = RTABL(KLOBJ,I,JLOBR2)
C
C PEHY
         KEHY = KROW(KPEHY,NEHY)
         IW(KEHY+JPEHBH) = 1
         IW(KEHY+JPEHKT) = ITABL(KLOBJ,I,JLOBKT)
         IW(KEHY+JPEHPE) = NEOB
C
C PEOT
            KEOT = KROW(KPEOT,NEOT)
            RW(KEOT+JPEOT1) = RTABL(KLCLU,I,JLCLT1)
            RW(KEOT+JPEOP1) = RTABL(KLCLU,I,JLCLF1)
            RW(KEOT+JPEOS1) = RTABL(KLCLU,I,JLCLW1)/
     &                        RTABL(KLCRE,1,JLCRZ1)
            RW(KEOT+JPEOT2) = RTABL(KLCLU,I,JLCLT2)
            RW(KEOT+JPEOP2) = RTABL(KLCLU,I,JLCLF2)
            RW(KEOT+JPEOS2) = RTABL(KLCLU,I,JLCLW2)/
     &                        RTABL(KLCRE,1,JLCRZ2)
            RW(KEOT+JPEOT3) = RTABL(KLCLU,I,JLCLT3)
            RW(KEOT+JPEOP3) = RTABL(KLCLU,I,JLCLF3)
            RW(KEOT+JPEOS3) = RTABL(KLCLU,I,JLCLW3)/
     &                        RTABL(KLCRE,1,JLCRZ3)
            IW(KEOT+JPEOPE) = NEOB
C
C If "neutral"
         IF(ITABL(KLOBJ,I,JLOBLI).EQ.0) THEN
         ENDIF
C
C Put any Lcob into PCPA
            NCPA = NCPA + 1
            KPCP = KROW(KPCPA,NCPA)
C
C  Codes are described in ddl of PCPA
C
            KODE=22
            IW(KPCP+JPCPNA) = KODE
            RW(KPCP+JPCPEN) = RTABL(KLOBJ,I,JLOBEC)
            RW(KPCP+JPCPTE) = RTABL(KLOBJ,I,JLOBTC)
            RW(KPCP+JPCPFI) = RTABL(KLOBJ,I,JLOBPC)
            R1 = RTABL(KLOBJ,I,JLOBR3)
            R1 = AMAX1(-10.,R1)
            R1 = AMIN1(10.,R1)
            RW(KPCP+JPCPR1) = R1
            R2 = RTABL(KLOBJ,I,JLOBR4)
            R2 = AMAX1(-10.,R2)
            R2 = AMIN1(10.,R2)
            RW(KPCP+JPCPR2) = R2
            IW(KPCP+JPCPPC) = NCHY
  500 CONTINUE
C
      IF(FDEBRF.AND.JDBDRF(JULLC).GE.4) THEN
         CALL PRTABL('PLSD',0)
C
         CALL PRTABL('PCOB',0)
         CALL PRTABL('PCHY',0)
         CALL PRTABL('PCRL',0)
         CALL PRTABL('PCPA',0)
         CALL PRTABL('PEOB',0)
         CALL PRTABL('PEHY',0)
         CALL PRTABL('PEOT',0)
      ENDIF
C
      GOTO 999
  997 CALL REPORT('LCRPOT','AUBOS failure',1)
  999 CONTINUE
      END
#endif
