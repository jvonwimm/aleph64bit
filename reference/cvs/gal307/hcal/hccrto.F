*DK hccrto
      SUBROUTINE HCCRTO
C --------------------------------------------
C
C!   Builds the tower hit bank
C
C!     Author      :G.Zito      86/05/21
C!     Modified    :G.Catanesi  86/12/03
C!                 :G.Catanesi  87/10/21
C!                 :G.Catanesi  88/08/20
C!                 :A.Venturi   98/07/01
C!        simulation of the signal spread along the tubes
C!        and simulation of the half-towers calibration
C!                 :A.Sciaba    98/08/21 mods in hctwth.h  
C!
C!     Input bank  : JDHCSE   McHcTrachSegment
C!                   JDHCHI   McHcHits
C!     Output bank : 'HTHT'   McHcStoreys
C!
C!         Called by: HCASIG
C!        -Calls : HFNDTR, HCSTEN     from this .HLB
C!                 HFNDTW             from Alephlib
C------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "jobcom.h"
#include "iocom.h"
#include "hcpara.h"
#include "hccoun.h"
#include "hcnamc.h"
#include "hctwth.h"
#include "hcsejj.h"
#include "hchijj.h"
#include "hthtjj.h"
#include "htusjj.h"
C
      PARAMETER(NSTP=101,DS=0.76,XFRAC=.70)
      DIMENSION XS(3),DX(3),XL(3),XP(3)
      DIMENSION ANGL(2), ITOW(3), ITRI(2)
      CHARACTER*6 SYSRF
      DATA NAHTUS /0/
C
#include "bmacro.h"
C
C       Barrel pad shift
C
      IF (NAHTUS.EQ.0) THEN
        NAHTUS = NAMIND('HTUS')
        JHTUS = IW(NAHTUS)
        ZOFF = RTABL(JHTUS,1,JHTUTE)/2. + RTABL(JHTUS,1,JHTUTS)
      ENDIF
C
C       Loop on hits
C
      NHCHI = LROWS (JDHCHI)
      IF(NHCHI.EQ.0) RETURN
      KHCHI = JDHCHI + LMHLEN
      JHTHT = IW(NAHTHT)
      NHTHT = 0
      KHTHT = JHTHT + LMHLEN
C   Initialize half tower vector
      CALL VZERO(ENHFTW,NUHFTH*96*2*2)
C
C       ANGL  = phi, theta
C       ITOW  = nphi, ntheta, nstac
C       ITRI  = nphi trigger, ntheta trigger
C
      DO 50 I=1, NHCHI
         NSEG = IW(KHCHI+1)
         KHCSE = KROW (JDHCSE,NSEG)
         IPOR = ITABL (JDHCSE,NSEG,4)
         JMOD = ITABL (JDHCSE,NSEG,3)
         ILAY = ITABL (JDHCSE,NSEG,2)
         XLOC = RTABL (JDHCSE,NSEG,6)
         YLOC = RTABL (JDHCSE,NSEG,7)
         DYLOC = RTABL (JDHCSE,NSEG,8)
         ITUB = ITABL (JDHCSE,NSEG,1)
         IADD = ITABL (JDHCSE,NSEG,5)
         IHCH = ITABL (JDHCSE,NSEG,13)
C       Streamer energy
         EDEP = HCSTEN(IW(KHCHI+4))
         NSTM = IW(KHCHI+4)
         DHY  = RW(KHCHI+3)
         HY   = RW(KHCHI+2)
         IHCS = IW(KHCHI+1)
C       Streamer central position
         XS(1)=RW(KHCSE+9)
         XS(2)=RW(KHCSE+10)
         XS(3)=RW(KHCSE+11)
C     Loop on signal sample along the tube
         EDEP2=0.
         WID=DS*(NSTP-1)/2
         DO 45 ISMP=1,NSTP          
C     Evaluate the shift along the tube
         DN=(1-2*MOD(ISMP,2))*DS*(ISMP/2)
         IF(IPOR.EQ.LPBAR) THEN
           DX(1)=0
           DX(2)=0
           DX(3)=DN
           DT=DN
           IF(MOD(JMOD,2).NE.0) DT=-DT
         ELSEIF(IPOR.EQ.LPECA) THEN
           IF(JMOD.LE.0.OR.JMOD.GE.7) THEN 
            WRITE(LOUTIO,*) 'HCCRTO: inconsistency in EndCap'
            WRITE(LOUTIO,*) JMOD,IPOR
           ENDIF
           IF(JMOD.EQ.2.OR.JMOD.EQ.5) THEN
             DX(1)=DN/2.
             DX(2)=DN*SQRT(3.)/2.
             DX(3)=0.
             IF(JMOD.EQ.2) DT=DN
             IF(JMOD.EQ.5) DT=-DN
           ELSE
             DX(1)=DN
             DX(2)=0.
             DX(3)=0.
             IF(JMOD.EQ.1.OR.JMOD.EQ.6) DT=DN
             IF(JMOD.EQ.3.OR.JMOD.EQ.4) DT=-DN
           ENDIF
         ELSEIF(IPOR.EQ.LPECB) THEN
           IF(JMOD.LE.0.OR.JMOD.GE.7) THEN
            WRITE(LOUTIO,*) 'HCCRTO: inconsistency in EndCap'
            WRITE(LOUTIO,*) JMOD,IPOR
           ENDIF
           IF(JMOD.EQ.2.OR.JMOD.EQ.5) THEN
             DX(1)=-DN/2.
             DX(2)=DN*SQRT(3.)/2.
             DX(3)=0.
             IF(JMOD.EQ.2) DT=DN
             IF(JMOD.EQ.5) DT=-DN
           ELSE
             DX(1)=DN
             DX(2)=0.
             DX(3)=0.
             IF(JMOD.EQ.1.OR.JMOD.EQ.6) DT=DN
             IF(JMOD.EQ.3.OR.JMOD.EQ.4) DT=-DN
           ENDIF
         ENDIF           
C     Evaluate the coordinate of the sample
         XL(1)=XS(1)+DX(1)
         XL(2)=XS(2)+DX(2)
         XL(3)=XS(3)+DX(3)
C     Evaluate the energy of the sample
C...Central Point Delta-like contribution
         IF(ISMP.EQ.1) THEN
           EDEP1=EDEP*(1.-XFRAC)
         ELSE
C...Side points
           IF(ABS(DN)-DS.NE.0) THEN
             EDEP1=EDEP*XFRAC/2.*
     +          (ABS(DN)/WID*LOG((ABS(DN)-DS)/ABS(DN))+
     +           DS/WID*(1.-LOG((ABS(DN)-DS)/WID)))
           ELSE
             EDEP1=EDEP*XFRAC/2.*
     +          ABS(DS)/WID*(1-LOG(DS/WID))
           ENDIF
         ENDIF
         EDEP2=EDEP2+EDEP1
C
C     Check if the sample is in a dead region (outside a tube)
         YLLC=YLOC+DT
C     Tube length
         IS=SIGN(1.,YLLC)
         XLEN=HTULEN(ITUB,ILAY,JMOD,IPOR,IS)
         CALL HNEIGH(ITUB,ILAY,IPOR,IHEIF,IDHEI)
         IF(IPOR.EQ.LPBAR) THEN
           XMIN=0
           XMAX=XLEN/2.
         ELSE
           XMIN=HTSTEC(ITUB,ILAY,IDHEI)
           XMAX=XMIN+XLEN
         ENDIF
C
         IF(ABS(YLLC).LT.XMIN.OR.ABS(YLLC).GT.XMAX) GOTO 45
C
C find tower adress i,j,k
C
C...if we are in the barrel, shift z by ZOFF to take into account
C...pads misalignment
         XP(1)=XL(1)
         XP(2)=XL(2)
         XP(3)=XL(3)
         IF(IPOR.EQ.2) XP(3)=XP(3)+ZOFF
C
         CALL HFNDTW(XP,SYSRF,ITOW(2),ITOW(1),ITOW(3), ISUBC,
     +   IMOD,IPL,IDEAD)
C
         IF(IDEAD.GT.0) GOTO 45
C   Check consistency
         IF(ISUBC.NE.IPOR.OR.IMOD.NE.JMOD) THEN
           WRITE(LOUTIO,*) 'HCCRTO: Inconsistency in module'
           WRITE(LOUTIO,*) ISUBC,IMOD,IPOR,JMOD,
     +                     (XL(JJ),JJ=1,3),(ITOW(JK),JK=1,3),
     +                     ILAY,ITUB,YLLC,XMIN,XMAX,XLEN,IDEAD
         ENDIF
C
         CALL HFNDTR(XL,IPOR,ITOW,ITRI)
C
         NTOW = ITOW(1)*10000 + ITOW(2)*100 + ITOW(3)
C     Check if we are in a half tower
         IOV=0
         IF(ITOW(2).GE.IFHFTH.AND.ITOW(2).LE.ILHFTH) IOV=1
         IF(63-ITOW(2).GE.IFHFTH.AND.63-ITOW(2).LE.ILHFTH) IOV=2
         IF(IOV.NE.0) THEN
           IF(IOV.EQ.1) LTH=ITOW(2)-IFHFTH+1           
           IF(IOV.EQ.2) LTH=63-ITOW(2)-IFHFTH+1           
           IF(ISUBC.EQ.2) IHF=1
           IF(ISUBC.NE.2) IHF=2
         ENDIF
C
C - IF it is not the 1st entry in 'HTHT' THEN
C      IF this tower# has already been entered into 'HTHT' THEN
C         sum up energy into this tower
C         jump to the next tower
C   ELSE  check dead regions if not Hcdetail
C         fill tower bank
C   ENDIF
C
         IF (NHTHT.EQ.0) GOTO 30
         DO 10 JROW =1,NHTHT
            IF (NTOW .EQ. ITABL(JHTHT,JROW,1)) GOTO 20
   10    CONTINUE
         GOTO 30
C        the tower# has already been entered
 20      CONTINUE
         KJROW = KROW(JHTHT,JROW)
         RW(KJROW+4) = RW(KJROW+4) + EDEP1
C   Half tower ?
         IF(IOV.NE.0) ENHFTW(LTH,ITOW(1),IOV,IHF)=
     +                ENHFTW(LTH,ITOW(1),IOV,IHF)+EDEP1
C
         IF(FHCDB2)THEN
            HCEPOR(IPOR) = HCEPOR(IPOR) + EDEP1
            IROW = LOCTAB (IW(JDHCTH+LMHLEN+1),LCOLS(JDHCTH) ,LROWS
     +      (JDHCTH),1,IW(KHCSE+12))
            KHCTH = KROW(JDHCTH,IROW)
            RW(KHCTH+5) =RW(KHCTH+5)+EDEP1
         ENDIF
         IF(ISMP.EQ.1) IW(KHCSE+JHCSFS) = JROW
         GOTO 45
C
   30    CONTINUE
C
C - Fill 'HTHT' with new tower
         NHTHT = NHTHT + 1
         IF(NHTHT*LHTHTA+LMHLEN.GE.IW(JHTHT))
     +    WRITE(LOUTIO,*) 'HCCRTO:  HTHT IS TOO SMALL !!'
         IW(KHTHT+1) = NTOW
         IW(KHTHT+2) = ITRI(1)
         IW(KHTHT+3) = ITRI(2)
         RW(KHTHT+4) = EDEP1
C   Half tower ?
         IF(IOV.NE.0) ENHFTW(LTH,ITOW(1),IOV,IHF)=
     +                ENHFTW(LTH,ITOW(1),IOV,IHF)+EDEP1
C
         IF(FHCDB2)THEN
            HCEPOR(IPOR) = HCEPOR(IPOR) + RW(KHTHT+4)
            IROW = LOCTAB (IW(JDHCTH+LMHLEN+1),LCOLS(JDHCTH) ,LROWS
     +      (JDHCTH),1,IW(KHCSE+12))
            KHCTH = KROW(JDHCTH,IROW)
            RW(KHCTH+5) = RW(KHCTH+5) + RW(KHTHT+4)
         ENDIF
         KHTHT = KHTHT + LHTHTA
C
   40    IF(ISMP.EQ.1) IW(KHCSE+JHCSFS) = NHTHT
 45   CONTINUE
C...Check energy sum
      IF((EDEP-EDEP2).GT.0.001) THEN
        WRITE(LOUTIO,*) 'HCCRTO: Inconsistency in hit energy'
        WRITE(LOUTIO,*) EDEP,EDEP2 
      ENDIF
C
      KHCHI = KHCHI + LHCHIA
 50   CONTINUE
      IW(JHTHT+LMHROW) = NHTHT
C
      IF (FHCDEB) THEN
         ETOT = 0.
         KHTHT = JHTHT + LMHLEN
         DO 60 I = 1,NHTHT
            ETOT = ETOT + RW(KHTHT+4)
 60      KHTHT = KHTHT + LHTHTA
         KHTH1 = JHTHT + LMHLEN
         WRITE (LOUTIO,500) LROWS(JHTHT),ETOT
         WRITE (LOUTIO,510) ((IW(K+I),I=1,3),RW(K+4),K=KHTH1, KHTH1
     +   +NHTHT*LHTHTA-1,LHTHTA)
      ENDIF
      RETURN

 500  FORMAT (/1X,'+++HCCRTO+++ McHcStoreys - ',5X,I3,' storeys , ',
     &        F5.2,' GeV deposited energy'/
     +3(4X,'PhThSt  Pt  Tt' ,'   Energy', 6X))
  510 FORMAT (3(I10,I4,I4,F10.5,5X))
      END
#endif
