      SUBROUTINE ECLAMP
C----------------------------------------------------------------------
C!  - Clean ECAL looking to amplificator noise
C!
C!                       AUTHOR : M.N MINARD 25.11.90
C!
C!    Bank used EWHE , ETDI
C!    Bank of killed channels done  ETKC
C?
C!======================================================================
#ifndef DOC
#include "etkcjj.h"
#include "etdijj.h"
#include "ecnamc.h"
#include "bcs.h"
      PARAMETER (NSTOR=3,LENW1=5,LENW2=2,NMISC=12)
      PARAMETER (ETHRES=0.050 , THDEC = 0.8)
      PARAMETER (NMINS=5 , FRACS=0.6)
      DIMENSION ECATOW(36),EWIRE(36),SIGM(3)
      DIMENSION EITH(3,3)
      COMMON /CECLAM/ JCETDI, JWETDI, JRETDI
      DATA SIGM /0.015,0.015,0.025/
#include "bmacro.h"
C
C-    INITIALISE ARRAY
C
      CALL VZERO (ECATOW,36)
      CALL VZERO (EWIRE,36)
C
C-    Initialise pointers
C
      JWETDI = 0
      JRETDI = 0
      JCETDI = 0
      KETDI = IW(NAETDI)
      IF (KETDI.EQ.0) GO TO 900
      NETDI = LROWS(KETDI)
      NLEN = NSTOR*NETDI*LENW1+LMHLEN
      CALL WBANK(IW,JWETDI,NLEN,*999)
      NLEN1 = NSTOR*NETDI*LENW2+LMHLEN
      CALL WBANK(IW,JRETDI,NLEN1,*999)
      IW(JWETDI+LMHCOL) = LENW1
      IW(JRETDI+LMHCOL) = LENW2
      IRETDI= 0
      IWETDI = 0
C
C-    First calculate energy in module
C
      DO 100 IETDI = 1,NETDI
        JETDI = KROW(KETDI,IETDI)
      ITOW=ITABL(KETDI,IETDI,1)
      ITETA = IBITS(ITOW,16,8)
      IPHI = IBITS(ITOW,2,9)
      CALL ECSCMD(IPHI,ITETA,IMOD,ISC)
      IGMOD = (ISC-1)*NMISC+IMOD
      IF (IGMOD.LT.1.OR.IGMOD.GT.36) GO TO 100
      DO 50 IK =1,3
      ECATOW(IGMOD)=ECATOW(IGMOD)+FLOAT(ITABL(KETDI,IETDI,IK+1))/1.E6
 50   CONTINUE
 100  CONTINUE
C
C-     Calculate wire energy
C
      NAEWHE = NAMIND('EWHE')
      KEWHE = IW(NAEWHE)
      DO 110 IEWHE = 1,LROWS(KEWHE)
      IGMOD = ITABL(KEWHE,IEWHE,2)
      IF (IGMOD.LT.1.OR.IGMOD.GT.36) GO TO 110
      EWIRE (IGMOD) = FLOAT(ITABL(KEWHE,IEWHE,1))/1.E6
 110  CONTINUE
C
C-    Loop on storeys to built structuture
C-    Cleaning performed only for pad/wire descrepency
C
      DO 150  IETDI = 1,NETDI
      JETDI = KROW(KETDI,IETDI)
      ITOW = ITABL(KETDI,IETDI,1)
      ITETA = IBITS(ITOW,16,8)
      IPHI = IBITS(ITOW,2,9)
      CALL ECSCMD(IPHI,ITETA,IMOD,ISC)
      IGMOD = (ISC-1)*NMISC+IMOD
      IF (IGMOD.LT.1.OR.IGMOD.GT.36) GO TO 150
      IF (EWIRE(IGMOD).LT.ETHRES) GO TO 150
      DECAL = ECATOW(IGMOD)-EWIRE(IGMOD)
      IF (DECAL.LT.AMIN1(0.1*EWIRE(IGMOD),THDEC))GO TO 150
      CALL ECDAMP (ITOW,IAMP)
C
C-    Search for amplificator in working bank
C
      DO 130 IK =1,3
      IAMPK  =  IAMP*NSTOR+IK-1
        IFI= 0
      DO 120 IWJ=1,IWETDI
      IF (ITABL(JWETDI,IWJ,1 ).NE.IAMPK) GO TO 120
C
C-    Look in chain
C
      IJETDI = ITABL(JWETDI,IWJ,5)
 115  IF (ITABL(JRETDI,IJETDI,2).EQ.0) THEN
        IFI= IWJ
         IRETDI = IRETDI+1
         IW(KROW(JRETDI,IJETDI)+2) = IRETDI
         IW(KROW(JRETDI,IRETDI)+1) = (IETDI-1)*4+1+IK
         GO TO 121
      ENDIF
      IJETDN = ITABL(JRETDI,IJETDI,2)
      IJETDI = IJETDN
      GO TO 115
 120  CONTINUE
  121   IF ( IFI.EQ.0) THEN
        IWETDI = IWETDI +1
        IW(KROW(JWETDI,IWETDI)+1) = IAMPK
        IW(KROW(JWETDI,IWETDI)+5) = IRETDI+1
        IRETDI = IRETDI+1
        IW(KROW(JRETDI,IRETDI)+1) = (IETDI-1)*4+1+IK
          IFI = IWETDI
      ENDIF
C
C-    Now store quantities to calculate mean & averges
C
      ES = FLOAT(IW(JETDI+1+IK))/1.E6
          JCUR = KROW(JWETDI,IFI)
      RW(JCUR+2) = RW(JCUR+2)+ES
      RW(JCUR+3) = RW(JCUR+3)+1.
      RW(JCUR+4) = RW(JCUR+4)+ES**2
 130  CONTINUE
 150  CONTINUE
C
C-    Now loop over bank to create new structure
C
C     Copy ETDI in a working bank
C
      KETDI = IW(NAETDI)
      NLEN = NETDI*LETDIA+LMHLEN
      CALL WBANK(IW,JCETDI,NLEN,*999)
      CALL BKTOW(IW,'ETDI',0,IW,JCETDI,*999)
C
C-    Calculate Means  & Dispersion
C
      DO 200 IWJ= 1,IWETDI
      JCUR = KROW(JWETDI,IWJ)
      IF (RW(JCUR+3).LE. 1) THEN
          RW(JCUR+4) = 0.
          GO TO 200
      ENDIF
      IAMPK = IW(JCUR+1)
      IK = MOD(IAMPK,NSTOR)+1
      RW(JCUR+2) = RW(JCUR+2)/RW(JCUR+3)
C
C     Calculate dispersion
C
      RW(JCUR+4) = RW(JCUR+4)/RW(JCUR+3)
      N1SIG = 0
      IJETDI = IW(JCUR+5)
230   IETDI = ITABL(JRETDI,IJETDI,1)
      ES = FLOAT(IW(KETDI+LMHLEN+IETDI))/1.E6
      IF (ABS(ES-RW(JCUR+2)).LT.SIGM(IK)) N1SIG = N1SIG +1
      IJETDN = ITABL(JRETDI,IJETDI,2)
      IF ( IJETDN.NE.0) THEN
        IJETDI = IJETDN
        GO TO 230
      ENDIF
C
C-    Decide to remove amplificator
C
      IF(RW(JCUR+3).GT.NMINS.AND.N1SIG.GT.FRACS*FLOAT(NMINS)) THEN
           RW(JCUR+4) = -100.
      ELSE
           RW(JCUR+4) = 0.
      ENDIF
C
C-    Remove energy in working bank
C
      IF (RW(JCUR+4).LT.-50.) THEN
        IJETDI = IW(JCUR+5)
240     IETDI = ITABL(JRETDI,IJETDI,1)
        IW(JCETDI+LMHLEN+IETDI)= 0.
        IJETDN = ITABL(JRETDI,IJETDI,2)
        IF ( IJETDN.NE.0) THEN
          IJETDI = IJETDN
          GO TO 240
        ENDIF
      ENDIF
200   CONTINUE
C
C-    Drop working banks related to ampli
C
      CALL WDROP(IW,JWETDI)
      CALL WDROP(IW,JRETDI)
C
C-    Built bank of killed storeys
C
      NLEN = NETDI*LETKCA+LMHLEN
      CALL AUBOS('ETKC',0,NLEN,KETKC,IGARB)
      IF (IGARB.EQ.2) GO TO 999
      CALL BLIST(IW,'E+','ETKC')
      IF (IGARB.EQ.1) KETDI = IW(NAETDI)
      IW(KETKC+LMHCOL) = LETKCA
      IW(KETKC+LMHROW) = 0
C
C-    Update ETDI & ETKC
C
C     Initialise threshold  EITH (Stack,SubComponent) in MeV !
C
      CALL EBTZTH (EITH,IRC)
C
C-    Update ETDI if needed
C
      IKILL = 0
      ICOPY = 0
      DO 400 IETDI = 1,NETDI
      JETDI = KROW(JCETDI,IETDI)
      ITOW = IW(JETDI+1)
      IGOO = 0
      ITETA = IBITS(ITOW,16,8)
      IPHI = IBITS(ITOW,2,9)
      CALL ECSCMD(IPHI,ITETA,IMOD,ISC)
      DO 370 IK=1,3
      IF (IW(JETDI+1+IK).LT.EITH(IK,ISC )*1000.) GO TO 370
          IGOO = IGOO +1
 370  CONTINUE
      IF(IGOO.NE.0) THEN
        ICOPY = ICOPY +1
        JETDIO = KROW(KETDI,ICOPY)
        CALL UCOPY(IW(JETDI+1),IW(JETDIO+1),4)
      ELSE
        IKILL = IKILL +1
        JETKC = KROW(KETKC,IKILL)
        CALL UCOPY(IW(JETDI+1),IW(JETKC+1),4)
        IW(JETKC+5) = 10
      ENDIF
 400  CONTINUE
C
C-    Delete working banks & Compress new banks
C
      IW(KETDI + LMHROW) = ICOPY
      IW(KETKC + LMHROW) = IKILL
      CALL AUBPRS('ETDIETKC')
      CALL WDROP(IW,JCETDI)
 900  CONTINUE
      RETURN
 999  CONTINUE
      CALL RERROR('ECLAMP',2,' BOS MEMORY FULL NO NOISE CLEANING')
      RETURN
      END
#endif
