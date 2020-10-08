        PROGRAM REDKIN
C----------------------------------------------------------------------
C! read from KORALW or other generator and rehadronize
C
C  Job to use the KINGAL output from a KORALW/ 2fermions generation and
C  refragment using JETSET(Color Reco)/Ariadne(color reco)/HERWIG 6.2.
C  The program uses the W+W-, the 4 fermion decay products and any
C  ISR or FSR produced. These are used to fill the HEPEVT common
C  and the fermions are colour connected. The daughters of each
C  W are connected, no reconnection is done, this should be controlled
C  with PRECO.
C  
C  The cards file is essentially the standard HERWIG set and is 
C  set by the environment variable KINAGAINCARDS. It should be noted
C  though that some parameters are picked up from KORALW and the
C  corresponding values in the cards file are ignored.
C  NOTE also the Process Id is in principle ignored, obviously
C  200 (ee-->WW) is a good value, but 100 - 199 must not be used.
C  
C  This program is based on CRCKIN written by Anders Waananen in
C  Copenhagen but heavily transformed to get more options
C
C  Features:
C    The output file have new KINE, VERT and RUNE banks
C    The KINGAL code is changed from 50xx to 51xx if HERWIG
C                       kept if JETSET still there
C
C  Bugs(known)/upgrades :
C       - Some events fail (approx 1/10000) with HERWIG error 104 
C                                          Stan Thompson  -  April 1998
C       - This was fixed later and was due to a misrelation of W to daughters
C        in case of radiative leptonic decay   B.Bloch July 1999
C       - version 103 : prevent B'0s from having 2 displaced vertices 
C          in HERwig interface   B.Bloch in KXHEAL sep 2000
C       - version 104 : qqbar from Koralz to Herwig, handle low mass systems
C         (TOHEP8), reset IP vertex(4) to avoid pb with B mixing  (TOHAD) 
C         do not process in HERWIG system with mass below 2mquark
C        - version 105 : better handling of ARIADNE setup when option choosen
C         new routine ARJTMD ( data cards MSTV,PARV,MSTG,PARG)
C         new option for Color reconnection models within Jetset: possibility 
C         to add a boost to the W+W- system before computing overlap ( add 10 
C         to option =1,2,3 )
C        - version 106 : HIST card to store histograms in a file
C                       "copy" version to produce the KINGAL output from any 
C                        POT/DST file: active if ipsho=1, ireco=0, ibe=0
C        - version 200 : switch to HERWIG version 6.2 ( new commons,new code
C                        new interface )
C                        keep KW4F bank if exits
C                        fix filling of KWGT , makes it dynamic
C        - version 201 : fix nb of particle to be transferred to HERWIG when
C                        reading Pot input
C        - version 202 : modify CR scheme in JETSET option : copy input event
C                        record including Parton shower, then branch to CR
C        - version 203 : few fixes for Linux version ....
C                        counters for semilep/fullhad/fulllep types
C                        transmit KSEC bank to output if found.
C                        do not try BE on fully leptonic ( no way it works)
C                        icopy flag on the GAGA card
C                        allow to replay JETSET hadronisation with different
C                        parameters ( ipsho = 1, ireco =0 ibe =0 icopy =0)
C                        new PS and fragmentation is done , then BE is applied
C                        Rathsman GAL model as CR option
C                        possible KKEY card to change the RUN title only in
C                        KRUN bank
C           Aug 29       add ilow counter to allow skipping of too low qqbar
C                        mass events in Herwig ( from WW hadronic events )
C           Sep 25       fix possible replay of qqbar events from HVFL with
C                        Herwig ( Z0 appears there ...)
C           Nov 16       fix interface of Ariadne for HVFL qqbar events
C                        sometimes the qqbar pair appears twice ! keep one ..
C           Nov 22       fix herwig interface for WW events stored as ZZ
C                        in case of Cabbibo suppressed decays
C           Nov 25       new interface of input to Ariadne to transmit 
C                        correctly semileptonic and fully leptonic ww's
C                        in case of Color reco, init hadronics with AR2, semi
C                        leptonic with AR1
C           Dec 6        new interface for Rathsman (GAL) model to be able to
C                        redo the parton shower with the tuned QCD parameters
C     2002  Jan          possibility to just copy semilep and lep events in
C                        case of SKI choice      
C           March 1,2002 a priori call to rndm at event number from card POOR
C                        to change the random sequence and avoid a phase space
C                        problem in Herwig in one job !!!! poor man solution..
C           May 15,2002  allow kinagain to read back Bhabha events in refrag
C                        and copy modes by setting the status code  ( undefined
C                        from the Bhabha generator)
C                        allow kinagain to really copy banks from input in copy
C                        mode while still analysing event content
C           July 23,2002 add more control histos for EP conservation checks
C           Apr 10,2003  introduce the 2-step cascade possibility for Ariadne 
C----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C      
      INTEGER I,IER,MREC
      INTEGER INEVT
      INTEGER IREVT,ISEVT,RAN(3),nakevh,ityp(3)      
      INTEGER JNEVT,JDEBU,JVERT,JEVEH,JRUNH,NEXPE,IRTYP,JGKRW
      INTEGER JKEVH,LBAS,JKRUN,NAKRUN,KKRUN,IGCOD,NOTRK,NAKPAR
      INTEGER JKLIN,LLUGE,LLUAL,KFCOD,KCODE,JKPAR,IGCODO
      INTEGER NMXCDK,nakwgt,jkwgt,nkwgt
      INTEGER NARLEP,JKLEP,kwgtbk,ist,ista
      PARAMETER (LLUGE=52,LLUAL=315)
      CHARACTER FNAME*120,TITLE*48
      CHARACTER*4 CHAINT
      EXTERNAL CHAINT
      REAL*4 VTX(4),PBEAM,prec
      INTEGER NVRT,NTRK,NTRO,NVRO
      INTEGER IMODE,ISNFQ,IKCRB,IFRFR,IRURN,ISAVE
      CHARACTER*1 OLIST
      INTEGER LPDEC,MXDEC,NCLDK,LOCN
      PARAMETER (LPDEC = 48)
      INTEGER NODEC(LPDEC)
      INTEGER IDEBB,IDEBE,IDEBL
      INTEGER iout,ind,ifix
      INTEGER  JUNIDB,MDARD,NLINK,NAMIND,KNODEC,LUCOMP,ALRUNE
      EXTERNAL JUNIDB,MDARD,NLINK,NAMIND,KNODEC,LUCOMP,ALRUNE
      INTEGER KMRUNH,KMKRUN
      EXTERNAL KMRUNH,KMKRUN   
      INTEGER IVERS,IDATE,IREDB,IPART,IKLIN,iver,nz0
      LOGICAL LRUNINIT,CLRECO
CC      COMMON / DTBUG / IDEBB,IDEBE,IDEBL
!   Ariadne setup
      REAL*4 PARA,PHAR
      INTEGER msta,MHAR      
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      COMMON /ARHIDE/ PHAR(400),MHAR(400) 
!   jetset setup
      INTEGER N7LU,K7LU,LJNPAR
      REAL*4  P7LU,V7LU
      PARAMETER (LJNPAR=4000)
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5)
      integer L1MST, L1PAR, L2PAR, L2PARF
      PARAMETER (L1MST=200, L1PAR=200)
      PARAMETER (L2PAR=500, L2PARF=2000 )
      REAL*4 PARU,PARJ,BRAT,PMAS,PARF,VCKM
      INTEGER MSTU,MSTJ,MDCY,MDME,KFDP,KCHG
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),
     &                KFDP(L2PARF,5)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      REAL*4 PARP,PARI
      INTEGER MSTI,MSTP
! This common can be everywhere, contains various switches
      INTEGER KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
      COMMON / KeyKey /  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
CBB
C   Herwig common with reco info
C
C Parameters for cluster decays (NMXCDK = max total no of cluster
C                                         decay channels)
      REAL*8 CLDKWT,CTHRPW,RESN,RMIN,PRECO
      PARAMETER(NMXCDK=4000)
      COMMON/HWUCLU/CLDKWT(NMXCDK),CTHRPW(12,12),PRECO,RESN(12,12),
     & RMIN(12,12),LOCN(12,12),NCLDK(NMXCDK),NRECO,CLRECO

      common/colrec/nptcr
      INTEGER IJKLI2,NTOTI2,NTOTN2,iflav(4),ireco,ihad,idpr,nreco
      INTEGER LNOTRK,NRUN,IGCO,JRUN,icr,ipsho,lapar,ifhadp,ifhadm,ilow
      INTEGER m1,m2,JKZFR,lll,nrow,JKHIS,nseq,jidb,ibe,ierx,icopy,irx
      INTEGER jkmar,JFVER,ij,jgaga,ivec,jver,idum,mreset,ihevt,nptcr
      REAL*4 PCR,weik,ecmi,weit,dum,tlim,para28,xx,dummy,rndm
      REAL*4 am,amarset,plu,vsec(10,4),sump
      external amarset,plu,mreset,rndm,klu
      integer nbep,irbug,idd,klu       
      common/bepairs/nbep(9)
      CHARACTER*10 TSHO(4),treco(5,4),thad(2),tbe(4),breco(2),tcop(2)
C      CHARACTER*10 TSHO(3),treco(5,3),thad(2),tbe(3),breco(2),tcop(2)
      CHARACTER*80 ATYPE,DTYPE,FFNAM
      CHARACTER*4 CNAME,DMODE,FDEVI
      CHARACTER*12 daterel
      DATA CNAME/'HIST'/
      DATA DTYPE / 'HIS       '/, DMODE / 'A   '/
      integer jhist,jtmp,ktmp,jgcod,nkar,intcha,ierr,icoref,iberef
      integer inewbe,mstj51,js,jkey,ico,msta35,msta40,iflagg,irecoref
      DATA iver/ 203/, DATEREL/'April 10,2003'/
      DATA nptcr/100/
      Logical crete
      Logical seml,full,fulh,qqgen,wwgen
CBB
#include "inout.h"
#include "partjj.h"
#include "vertjj.h"
#include "kevhjj.h"            
#include "evehjj.h"            
#include "krunjj.h"            
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
      integer mxbnk,lqbos
      parameter (MXBNK=20000,LQBOS=9000000)
C
      COMMON /BCS/   IW(LQBOS)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      integer lhb
      real hb
      PARAMETER (LHB=3000000)
      COMMON /PAWC/ HB(LHB)

      DATA TSHO /'JETSET 7.4','ARIADNE4.8','HERWIG 6.2','PYTHIA 6.1'/
      DATA TRECO/' None',' SK I ',' SK II ',' SK II prim',' GAL',
     $           ' None',' AR 1 ',' AR 2 ',' AR 3 ',' ',
     $           ' None',' Reconnect','  ','  ',' ',
     $           ' None',' SK I ',' SK II ',' SK II prim',' GAL'/
      DATA BRECO/' No ',' Yes '/ 
      DATA THAD /'all events','hadronics '/
      DATA TBE  /'Not includ','interW onl','intraW onl','all pairs '/
      DATA TCOP /' No ',' Yes '/ 
#include "bmacrod.h"
#include "bmacro.h"
CBB inline functions
      qqgen(ico) = ico.eq.4008 .or. ico.eq.4010 .or. ico.eq.4017
     &        .or. ico.eq.5030 .or. ico.eq.5046  .or. ico.eq.5047 
      wwgen(ico) = ico.eq.5035 .or. ico.eq.5038
      crete = .false.
      TLIM=999999.
      CALL TIMeST(TLIM)
      
C
C Initialise BOS
C
      CALL BNAMES(MXBNK)
      CALL BOS(IW,LQBOS)
      write(6,*) 'BOS initilised with',lqbos,' words'
      CALL HLIMIT(LHB)
C      
      LCARD = 5
      LOUTP = 6
      IW(4) = 1
      IW(5) = LCARD
      IW(6) = LOUTP
#if defined(DEBUG)
      OPEN(UNIT=92,FORM='UNFORMATTED')
#endif
      call hbook1(99,' IER from read',10,0.,10.,0.)
C
C Read CARDS file
C
      FNAME = ' '
      CALL GETENVF ('KINAGAINCARDS',FNAME)
      IF (FNAME.EQ.' ') FNAME = 'kinagain.cards'
      CALL AOPEN (LCARD,FNAME,'CARD','DISK',IER)
      IF (IER.EQ.0)THEN
        IDUM = MRESET(LCARD)
        CALL BREADC
      ELSE
        STOP
      ENDIF
C
C - if READ data cards open card files on unit LCAR+1
C
C     print *,ier,lcard
    1 CALL AGTFIL ('READ','READ',LCARD+2,IERx)
      print *,' open on unit ier',lcard+2,ierx
      IF (IERx.NE.-1) THEN
        IF(IERx.NE.0) THEN
          WRITE (6,*) ' kinagain : cannot open extra data card file'
        ELSE
          IDUM = MRESET(LCARD+2)
          CALL BREADC
        ENDIF
        GOTO 1
      ENDIF
C
      MREC  = 0
C
C Open DATABASE
C
      LBAS = JUNIDB(0)
      CALL AOPDBS ('   ',IER)
      IF (IER.NE.0) THEN
         WRITE(LOUTP,'(1X,''HERKIN: NO data base (AOPDBS) - STOP'')')
         STOP
      ENDIF
C   Get ADBSCONS DAF version # and date of last change
      CALL ADBVER (IVERS,IDATE)
      CALL ADBCHK(IVERS)
C
      CALL RDMIN(12345)
C
C NEVT flag
C
      INEVT = 999999999
      JNEVT = NLINK('NEVT',0)
      IF (JNEVT.NE.0) THEN
        IF (IW(JNEVT).EQ.1) THEN
          INEVT = IW(JNEVT+1)
        ENDIF
      ENDIF      
      IDEBB = 0
      IDEBE = 0
      JNEVT = NLINK('DEBU',0)
      IF (JNEVT.NE.0) THEN
          IDEBB = IW(JNEVT+1)
          IDEBE = IW(JNEVT+2)
      ENDIF   
      LRUNINIT = .TRUE.
      JNEVT = NLINK('CRET',0)
      IF (JNEVT.NE.0) crete = .true.
C  suggested HIST card to store Histos on a disk file
C       HIST  'file name specifications '
      IERx = 0
      JHIST= NLINK('HIST',0)
      IF(JHIST.NE.0 ) THEN
         CALL ACDARG(CNAME,DTYPE,DMODE,FFNAM,ATYPE,FDEVI,IERx)
         print *, 'HIST card found'
      ENDIF
C
C Read input file
C
      IDEBU = 0
      IREVT = 0
      ISEVT = 0
      IHEVT = 0
      ILOW  = 0   ! number of too low mass evets for Herwig
      call vzero(ityp,3)
 10   CALL ABRSEL('E',' ',IER)
      if(ier.eq.1) then
C    in case one reads a POT, drop POT event banks
         call bdrop(iw,'PEPTPFXTPGACPHCOPHHYPIDIPPOBPT2X')
         call bdrop(iw,'REVHRTLSSILHTRIKVCPLVDMRVDXYVDZT')
         call bdrop(iw,'X1RGX2DFXTCNYLV0YV0VECRQEFOLEGPR')
         call bdrop(iw,'ETDIEWHEHLTUHMADHPDIHTUBPASLPCHY')
         call bdrop(iw,'PCOIPCPAPCRLPECOPEHYPEMHPESTPEWI')
         call bdrop(iw,'ASEVDHEAEIDTEJETETKCFRIDMUEXMUID')
         call bdrop(iw,'PPRLPRPWPTBCPTEXPTNCPTPXPTSTPYFR')
         call bdrop(iw,'HROALIDTLUPAPCOBPYERX1IPPFRFPFRT')
         call bdrop(iw,'PHSTPITMPLPDPLSDPPDSTWZZVCOMVDCO')
         call bdrop(iw,'VDFKVDGCVDHTVFHLVFLGVFPHVUFKLCRA')
         call bdrop(iw,'MHITMUDGPGIDSCLSSILUSIXASPDAYNLI')
         call bdrop(iw,'YKNKYNVHYSMOMTHRMCADECTE')
      endif
      CALL BGARB(IW)
      ISAVE = 0
      OLIST='E'
      call hfill(99,float(ier),dum,1.)
      IF ( ier.ge.6) print *,'   <==attention  ier ',ier
      IF (IER.EQ.1) IREVT = IREVT + 1
      IF (IER.eq.3) ISAVE = 1
      IF (IER.EQ.2) THEN
        IF ( IREVT.GT.0 ) go to 10   ! same file, new header banks 
        ISAVE = 1
        OLIST = 'C'
        call aublis('C')
C      drop Run header POT banks if any
        call bdrop(iw,'TSIMLALILCALEZTHVOLUACUTAFIDAJOB')
        call bdrop(iw,'ARUNAPROASIMILIVECHEEHGFSMPDRHAH')
        call bdrop(iw,'ZPFRJCONPTUN') 
C pick up beam energy
        NARLEP=NAMIND('RLEP')
        JKLEP=IW(NARLEP)
        PBEAM=ITABL(JKLEP,1,1)/1000.
        WRITE(6,*) 'NOTE - Initial run Beam energy (GeV) = ',PBEAM
        ecmi = 2.*pbeam
        WRITE(6,*) 'NOTE - Initial run header banks RUNH RUNR KRUN'
        call prtabl('RUNH',0)
        call prtabl('RUNR',0)
        call prtabl('KRUN',0)
C get KINGAL code and modify it
        NAKRUN=NAMIND('KRUN')
        JKRUN=IW(NAKRUN)
        IGCOD=ITABL(JKRUN,1,JKRUGI)
        IGCODO = IGCOD
        WRITE(6,*) 'NOTE - Old Kingal code                         ',
     +   IGCOD
        NOTRK=ITABL(JKRUN,1,JKRUNO)
C get flags at generation
        NAKPAR = NAMIND('KPAR')
        JKPAR = IW(NAKPAR)
        JGCOD = mod(igcod,10000)
        if ( wwgen(JGCOD)) then   ! KRLW02
        KEYRAD= ifix(rtabl(jkpar,1,1))
        KEYPHY= ifix(rtabl(jkpar,1,2))
        KEYTEK= ifix(rtabl(jkpar,1,3))
        KEYMIS= ifix(rtabl(jkpar,1,4))
        if ( JGCOD.eq.5035) then   ! KRLW02
          PMAS(23,1) = rtabl(jkpar,1,24)   ! Z mass and width
          PMAS(23,2) = rtabl(jkpar,1,25)
          PMAS(24,1) = rtabl(jkpar,1,26)   ! W mass and width
          if (rtabl(jkpar,1,27).gt.0.) PMAS(24,2) = rtabl(jkpar,1,27) 
          PMAS(25,1) = rtabl(jkpar,1,81)   ! Higgs mass and width
          PMAS(25,2) = rtabl(jkpar,1,82)
        else if (jgcod.eq.5038) then
          PMAS(23,1) = rtabl(jkpar,1,4)   ! Z mass and width
          PMAS(23,2) = rtabl(jkpar,1,5)
          PMAS(24,1) = rtabl(jkpar,1,6)   ! W mass and width
          if (rtabl(jkpar,1,7).gt.0.) PMAS(24,2) = rtabl(jkpar,1,7)
          PMAS(25,1) = rtabl(jkpar,1,11)   ! Higgs mass and width
          PMAS(25,2) = rtabl(jkpar,1,12)
        endif
        else if (JGCOD.eq.5034) then    !  PYTH04
C           no parameters to get !
        endif
        DO I=1,12
          TITLE(I*4-3:I*4)=CHAINT(ITABL(JKRUN,1,JKRURT+I-1))
        ENDDO
C       WRITE(6,*) 'KRUN-',IGCOD,NOTRK,' ',TITLE
        irbug = 0
        JGAGA = NLINK('POOR',0)
        IF(JGAGA.ne.0) irbug = IW(JGAGA+1) 
        ipsho = 1
        ireco = 0
        ihad = 0
        ibe = 0
        icopy = 0
        JGAGA = NLINK('GAGA',0)
        IF(JGAGA.ne.0) then
          ipsho = IW(JGAGA+1)         ! fragmentation model
          ihad  = IW(JGAGA+2)         ! 1 if only on 4q events
          ireco = IW(JGAGA+3)         ! color reco model
          ibe   = IW(JGAGA+4)         ! Bose Einstein model
          icopy = IW(JGAGA+5)         ! copy option
        ENDIF 
        IF ( ipsho.eq.2 ) then        ! Ariadne selected
          JGKRW = NLINK('MSTA',35)    ! loook for CR requested
          IF(JGKRW.ne.0) ireco = max(0,IW(JGKRW+1))
        ELSEIF ( ipsho.eq.13) then     ! Herwig selected
          JGKRW = NLINK('GHCR',0)    ! loook for CR requested card GHCR
          IF((JGKRW.ne.0).and.(IW(JGKRW).ge.1)
     $         .and.(IW(JGKRW+1).gt.0.)) ireco = 1 
        ELSEIF ( ipsho.eq.1.and.ibe.gt.0) then
C analyse MSTJ(51) value if ibe ON        
          JGKRW = NLINK('MSTJ',51)
          IF(JGKRW.eq.0) JGKRW = NLINK('MSTG',51)
          IF(JGKRW.ne.0) then
           if (IW(JGKRW+1).gt.0) then
COP           if (ibe.gt.0) then 
C              ibe = max(0,IW(JGKRW+1)) ! Bose Einstein model
               ibe = min(1,IW(JGKRW+1))
               Jgaga = NLINK('MSTJ',53)
               if (jgaga.le.0) jgaga = NLINK('MSTG',53)
               if (jgaga.gt.0) ibe = ibe + 1 - iw(jgaga+1)
COP           else         ! ibe=0 but MSTJ(51) not 0
COP             icopy = 0          
COP           endif
           endif
          ENDIF
        ENDIF
C
      WRITE(LOUTP,110)
      WRITE(LOUTP,100)
      WRITE(LOUTP,120) '  K I N A G A I N                  '
      WRITE(LOUTP,130) iver
      WRITE(LOUTP,140) daterel
      if(qqgen(jgcod))WRITE(LOUTP,292) jgcod
      if(wwgen(jgcod))WRITE(LOUTP,293) jgcod  
      WRITE(LOUTP,110)
      WRITE(LOUTP,100)
      WRITE(LOUTP,120) '  R E F R A G M E N T A T I O N    '
      WRITE(LOUTP,100)
      WRITE(LOUTP,120) '     P A R A M E T E R S               '
      WRITE(LOUTP,100)
      WRITE(LOUTP,230) IPSHO,TSHO(mod(IPSHO,10))
      WRITE(LOUTP,250) IRECO,treco(mod(ireco,10)+1,mod(ipsho,10)) 
      if (ireco.gt.0 .and. ipsho.eq.1)
     + WRITE(LOUTP,260) breco(ireco/10 +1),nptcr
      WRITE(LOUTP,270) IHAD,thad(ihad+1)
      WRITE(LOUTP,280) IBE,tbe(ibe+1)
      WRITE(LOUTP,290) icopy,tcop(icopy+1)
      if (icopy+ibe+ireco.eq.0.and.ipsho.eq.1) WRITE(LOUTP,291)
      WRITE(LOUTP,100)
      WRITE(LOUTP,110) 
 130  FORMAT(1X,'|  version              : ',i5,12x,'  |')
 140  FORMAT(1X,'|  released on          : ',a12,5x,'  |')
 230  FORMAT(1X,'|  Hadronisation scheme : ',I5,A12,'  |')
 250  FORMAT(1X,'|  Reconnection scheme  : ',I5,A12,'  |')
 260  FORMAT(1X,'|  Boost to WW system   : ',5x,A12,'  |'
     +     /,1X,'|  Integration points   : ',5x,i12,'  |')
 270  FORMAT(1X,'|  Fully hadronic evts  : ',I5,A12,'  |')
 280  FORMAT(1X,'|  Bose-Einstein scheme : ',I5,A12,'  |')
 290  FORMAT(1X,'|  Copy from input      : ',I5,A12,'  |')
 291  FORMAT(1X,'|  Replay whole fragmentation  ',12x,'  |')
 292  FORMAT(1X,'|  acting on a qqbar generator code ',I7,'  |')
 293  FORMAT(1X,'|  acting on a ww    generator code ',I7,'  |')
C    keep original flag setup
      icoref = icopy
      iberef = ibe
      irecoref = ireco
C    modify KPAR bank if necessary
        if (wwgen(JGCOD)) then
            rw(JKPAR+LMHLEN+8) = float(ipsho)
            rw(JKPAR+LMHLEN+9) = float(ireco)
            rw(JKPAR+LMHLEN+10) = float(ibe)
            rw(JKPAR+LMHLEN+7) = float(ihad)
        endif
        if ( ipsho.eq.13) then
C        IGCOD=(IGCOD/10000)*10000+5100+(IGCOD-(IGCOD/100)*100)
        IGCOD = IGCOD - (IGCOD/10000)*10000 + 100
        WRITE(6,*) 'NOTE - New Kingal code for HERWIG fragmentation',
     +   IGCOD
        endif
C rebuild KRUN bank
CBB        CALL BDROP(IW,'KRUN')
CBB        CALL ALKRUN(IGCOD,NOTRK,TITLE)
        WRITE(6,*) 'NOTE - New Kingal code for this setup',IGCOD
        JKRUN=IW(NAKRUN)
        if ( JKRUN.gt.0 .and. IGCOD.ne.igcodo ) then        
C        JRUN = KMKRUN(IGCOD,NOTRK,TITLE)
         IW(JKRUN+LMHLEN+JKRUGI) = IGCOD   ! update IGCOD not the rest
        endif
CBB       IGCOD = IGCOD - (IGCOD/10000)*10000
CBB
      LNOTRK=100
      NRUN = 100
      JRUNH = NLINK('RUNH',0)
      IF(JRUNH.NE.0) then
         NRUN =  IW(JRUNH+2)
      ENDIF
      IGCO = IGCOD
      JRUN = NLINK('RUN ',0)
      IF(JRUN.NE.0) then   ! get new run number and title if any
         NRUN = IW(JRUN+1) 
         NKAR = IW(JRUN)-1
        DO I=1,12
          if(I.le.NKAR) then
            TITLE(I*4-3:I*4)=CHAINT(IW(JRUN+1+I))
          else
            TITLE(I*4-3:I*4)='    '
          endif
          IW(JKRUN+LMHLEN+JKRURT+I-1)= INTCHA(TITLE(I*4-3:I*4))
        ENDDO
CBB
        WRITE(6,*) 'RUN card - new RUN and title ',NRUN,TITLE
      endif
C  update possibly only the comment with a card KKEY
      JKEY = NLINK('KKEY',0)
      IF(JKEY.NE.0) then   ! get new run number and title if any
        NKAR = IW(JKEY)
        DO I=1,12
          if(I.le.NKAR) then
            TITLE(I*4-3:I*4)=CHAINT(IW(JKEY+I))
          else
            TITLE(I*4-3:I*4)='    '
          endif
          IW(JKRUN+LMHLEN+JKRURT+I-1)= INTCHA(TITLE(I*4-3:I*4))
        ENDDO
        WRITE(6,*) 'KKEY card - RUN and new title ',NRUN,TITLE
      endif
C rebuild RUNR/RUNH bank with updated run#
      JRUNH = NLINK('RUNH',0)
      IF(JRUNH.NE.0) then
         NEXPE = IW(JRUNH+1)
         IRTYP = IW(JRUNH+3)  
         JRUN  = KMRUNH (NRUN,NEXPE,IRTYP)     
      endif
      call hbook1(11,' Vertex position x',50,-0.1,0.1,0.)
      call hbook1(12,' Vertex position y',50,-0.01,0.01,0.)
      call hbook1(13,' Vertex position z',50,-5.,5.,0.)
      call hbook1(21,' px balance (GeV)',50,-0.005,0.005,0.)
      call hbook1(22,' py balance (GeV)',50,-0.005,0.005,0.)
      call hbook1(23,' pz balance (GeV)',50,-0.005,0.005,0.)
      call hbook1(24,' E  balance (GeV)',50,-0.005,0.005,0.)
      call hbook1(25,' E tot (GeV)',50,0.7*ecmi,1.2*ecmi,0.)
      call hbook1(521,' px balance (GeV) end',50,-0.005,0.005,0.)
      call hbook1(522,' py balance (GeV) end',50,-0.005,0.005,0.)
      call hbook1(523,' pz balance (GeV) end',50,-0.005,0.005,0.)
      call hbook1(524,' E  balance (GeV) end',50,-0.005,0.005,0.)
      call hbook1(525,' E tot (GeV) end',50,0.7*ecmi,1.2*ecmi,0.)
      call hbook1(600,' -log(xp) all final part',100,0.,10.,0.)
      call hbook1(601,' -log(xp) all final charged',100,0.,10.,0.)
      call hbook1(602,' -log(xp) all final neutral',100,0.,10.,0.)
      call hbook1(700,' p all final part',100,0.,20.,0.)
      call hbook1(701,' p all final charged',100,0.,20.,0.)
      call hbook1(702,' p all final neutral',100,0.,20.,0.)
      if ( ipsho.eq.13)then
C Read in HERWIG cards and initialize
        if(.not.lruninit)WRITE(6,*)'reinitialise HERWIG at event',irevt
        CALL iniher6(jgcod,PBEAM,prec)
        if ( prec.gt.0.) then
         call hbook1(100,'Herwig : reco per evt',         30,0.,30.,0.)
         call hbook1(201,'Herwig : reco per hadronic evt',30,0.,30.,0.)
         call hbook1(202,'Herwig : reco per semilept evt',30,0.,30.,0.)
        endif
        call hbook1(300,'Herwig : process code ',50,50.,550.,0.)
      elseif ( ipsho.eq.2) then
          msta(7) = iw(6)
          msta(8) = iw(6)
          msta(3) = 0
          mstj(105) = 0
          IFHADM = 0
          IFHADP = 0
C   if qqbar needs JETSET init, if ww needs PYTHIA !!!!
         if (wwgen(jgcod)) then
            msti(1)=25
            CALL ARINIT('PYTHIA')
         elseif (qqgen(jgcod)) then 
            msti(1)= 1
            CALL ARINIT('JETSET')
         endif
         CALL ARTUNE('EMC')
         CALL KXARCO(LAPAR)
         CALL ARPRDA
C   keep cr setup in local variables
         msta35 = msta(35)
         para28 = para(28)
         msta40 = msta(40)
         print *,' for this ARIADNE run '
         print *,' Fully hadronic events will have Reco option',
     &  min(MSTA(35),abs(msta(35)-msta(40)))
         print *,' Semileptonic events will have Reco option',
     &           min(1,MSTA(35)) 
         if ((para(28).gt.0.).and.(msta(40)+msta(35).ge.2)) 
     &          print *,' cascade will be done in 2 steps' 
         call hbook1(100,'Ariadne : reco per evt',30,0.,30.,0.)
         call hbook1(201,'Ariadne : reco per hadronic evt',30,0.,30.,0.)
         call hbook1(202,'Ariadne : reco per semilept evt',30,0.,30.,0.)
         call hbook1(203,'Ariadne : reco same W hadr evt',30,0.,30.,0.)
         call hbook1(204,'Ariadne : reco same W seml evt',30,0.,30.,0.)
         call hbook1(300,'Ariadne : process code ',50,0.,50.,0.)
      elseif ( ipsho.eq.1) then
        if ( ireco.gt.0) then
          call crini(ecmi)
          call hbook1(100,'Jetset : reco per evt',30,0.,30.,0.)
          if(mod(ireco,10).eq.4) then
            call hbook1(201,'Jetset GAL : reco per hadronic evt',30,0.,
     +                                                           30.,0.)
            call hbook1(202,'Jetset GAL : reco per semilept evt',30,0.,
     +                                                           30.,0.) 
          endif 
          if(mod(ireco,10).eq.1) 
     +     call hbook1(101,'Jetset SKI: overlap',100,0.,20.,0.)
C    
        endif
      endif
      inewbe = 0
      JGKRW = NLINK('MSTJ',51)
      IF(JGKRW.eq.0) JGKRW = NLINK('MSTG',51)
      IF(ibe.eq.0.and.JGKRW.ne.0) then
          if (IW(JGKRW+1).gt.0) inewbe = 1
      endif
      if ( ibe.gt.0 .or. inewbe.gt.0) then

         call hbook1(200,'entries to LUBOEI',50,1.,51.,0.)
         call hbook1(201,'Jetset BE : # pi+',50,0.,50.,0.)
         call hbook1(202,'Jetset BE : # pi-',50,0.,50.,0.)
         call hbook1(203,'Jetset BE : # pi0',50,0.,50.,0.)
      endif
      if ( ipsho.le.2) then
        MSTU(17)=1
        CALL KXL74A (IPART,IKLIN)
        IF (IPART.LE.0 .OR. IKLIN.LE.0) THEN
        WRITE (IOUT,
     &    '(1X,''ASKUSI :error in PART or KLIN bank - STOP - ''
     &                 ,2I3)') IPART,IKLIN
        STOP
        ENDIF
        CALL ARJTMD(LAPAR)
        print *,' JETSET setup within this new run'
        print *,LAPAR,' switches / parameters modified'
        mstu(17)=1
        print *,'fragmentation storage switch ', mstu(17)
        print *,' Lambda QCD                  ', parj(81)
        print *,' Q0                          ', parj(82)
        print *,' SIgma                       ', parj(21)
        print *,' a                           ', parj(41)
        print *,' b                           ', parj(42)
C
C  in case new fragmentation and BE required
C save BE option and generate without BE first and apply it later
C
      If ( inewbe.eq.1) then
         MSTJ51 = MSTJ(51)
         MSTJ(51)=0
         print *,' you will apply BE model    ',mstj51
      endif
C
C   Inhibit decays
C
      MXDEC=KNODEC(NODEC,LPDEC)
      MXDEC=MIN(MXDEC,LPDEC)
      IF (MXDEC.GT.0) THEN
        DO 50 I=1,MXDEC
          IF (NODEC(I).GT.0) THEN
            JIDB = NLINK('MDC1',NODEC(I))
            IF (JIDB .EQ. 0) MDCY(LUCOMP(NODEC(I)),1) = 0
          ENDIF
   50   CONTINUE
      ENDIF
C   Make sure masses are input ones
      
       call lutabu(10)
       call lutabu(20)
       print * ,' init finished with Jetset 7.4'
      endif
      if (ipsho.eq.2) then
              rw(JKPAR+9) = float(msta(35))
       print * ,' init finished with Ariadne   '
      else if (ipsho.eq.13) then
               rw(JKPAR+9) = prec
       print * ,' init finished with Herwig    '
      endif
      call prtabl('RUNR',0)
      call prtabl('RUNH',0)
      call prtabl('KRUN',0)
      print *,' after init  ,igco,nrun,lnotrk',igco,nrun,lnotrk
      call prtabl('KPAR',0)
      print * ,'  Now going for ',inevt,' events '
      if (CRETE) then
            call hdrwr(IGCO,lNOTRk,NRUN)
         if (jgcod.eq.5035  .or. jgcod.eq.5038) then
                call kpawr_ww
         else
                call kpawr_qq            
         endif
      endif
      endif      
C
C Examine event
C
        IF ( mod(IREVT,1000).eq.0)
     &   print * ,' Starting with evt',irevt,' for ',inevt,' events'
      IF (IER.LT.6.AND.IREVT.LE.INEVT) THEN
C  reinit of 2nd random numbers for fragmentation at each event
C  only for first event as anyway there is no point in keeping the
C  same if hadronisation has changed. This may be very time consuming ! 
        am = amarset(0)        
        IF ( IREVT.eq.1) then
           JEVEH=NLINK('KMAR',0)
           IF ( JEVEH.gt.0) then
                IJKLI2 = itabl(JEVEH,2,1) 
                NTOTI2 = itabl(JEVEH,2,2)
                NTOTN2 = itabl(JEVEH,2,3)
                call AMARIN(IJKLI2,NTOTI2,NTOTN2)
           ENDIF 
        endif 
        IF ((IREVT.eq.IRBUG).and.(IRBUG.gt.0)) dum = rndm(dummy)  
        IDEBU = 0
        ISTA = 0
        IST = 0
        IF(IREVT.GE.IDEBB.AND.IREVT.LE.IDEBE) IDEBU = 1
        JEVEH=NLINK('EVEH',0)
        IF (IDEBU.GT.0) THEN
          IF (JEVEH.GT.0)
     &       WRITE(LOUTP,*) 'RUN EVT',IW(JEVEH+JEVERN),IW(JEVEH+JEVEEV)
        ENDIF
CB update run number
        IF (JEVEH.GT.0) then
          IW(JEVEH+JEVERN) = NRUN
          IW(JEVEH+JEVERT) = (IW(JEVEH+JEVERT)/10000)*10000+IGCOD
        endif
        IF (IDEBU.GT.1) THEN
          WRITE(LOUTP,*)
     &       '=================================================='
          IF (IER.EQ.3) WRITE(LOUTP,*) 'UNKNOWN RECORD'      
          CALL AUBLIS(OLIST)
          CALL AUBLIS('S')
          CALL AUBLIS('T')
        ENDIF
C   if not an event record , take appropriate action
        IF (IER.GE.4) GOTO 10
        IF (IER.EQ.3) go to 12
        IF (IER.EQ.2) GO TO 11
         nakwgt = namind('KWGT')
         nakevh = namind('KEVH')
         jkevh = iw(nakevh)
         idpr = 0
C        JKEVH = NLINK('KEVH',0)
        if(jkevh.le.0.and. (JEVEH.GT.0)) then
C           write(loutp,*) 'KEVH bank lost !',nakevh,jkevh
            print *, 'KEVH bank lost !',nakevh,jkevh
        else
C           print *, 'KEVH bank read !',nakevh,jkevh
           idpr = IW(JKEVH+2+JKEVPI)
           weit = RW(JKEVH+2+JKEVWT)
           ntro = IW(JKEVH+2+JKEVNT)
           nvro = IW(JKEVH+2+JKEVNV)
        endif
         full = .false.
         fulh = .false.
         seml = .false.
         if (qqgen(jgcod)) then
            seml = .true. 
            iflav(1) = idpr
            iflav(2)= 0
            iflav(3)= 0
            iflav(4)= 0
         else 
         iflav(4)= idpr/1000000
         iflav(3)= (idpr-1000000*iflav(4))/10000
         iflav(2)= (idpr-1000000*iflav(4)-10000*iflav(3))/100
         iflav(1)= (idpr-1000000*iflav(4)-10000*iflav(3))-100*iflav(2)
         full = (iflav(1)+iflav(3)).ge.22
         fulh = (iflav(1)+iflav(3).le.10 .and. iflav(1)+iflav(3).gt.0)
         seml = (.not.fulh).and.(.not.full)
         endif
        IF (IDEBU.GT.0) THEN
          print *,'process code',idpr,' Kingal tracks/vert ',ntro,nvro
          print *,'flavours',iflav,'type had seml lep',fulh,seml,full
        endif
C        if ( ihad.gt.0) then
C           if(iflav(1).gt.10) go to 10  !  1st W into leptons reject
C           if(iflav(3).gt.10) go to 10  !  2nd W into leptons reject
C           ihevt = ihevt +1
C        endif        
        if ( ihad.gt.0 ) then
            if ( fulh ) then
               ihevt = ihevt +1
            else
               go to 10
            endif
        endif
        ISAVE = 1
        nreco =0
        MSTU(90) = 0    ! reset fragmentation info ....if needed
C  check how many rows in KWGT
        jkwgt = iw(nakwgt)
        nkwgt = 0
        if (jkwgt.gt.0) nkwgt = lrows(jkwgt) 
C fill the HEPEVT/LUJET common and fragment
        if (ipsho.eq.13) then
C initialize herwig
           nreco = 0
           call hwuine
           CALL TOHEP8(IDEBU,ipsho,ntro)
           CALL TOHAD6 (ierr)
           if ( ierr.eq.1) ilow = ilow + 1 
           if ( ierr.ne.0) go to 10 ! this event could not go through herwig
           IF (idebu.ge.1) call hwuepr
           if ( prec.gt.0.) then
              weik = float(nreco)  ! # of reconnections in event
              nkwgt = nkwgt + 1
              ind = kwgtbk(nkwgt,2000,weik)
              call hfill(100,weik,dum,1.)
            if (seml) call hfill(202,weik,dum,1.)
            if (fulh) call hfill(201,weik,dum,1.)
           endif
        else if (ipsho.eq.2) then !   transfer only necessary info
           if (fulh) then
              MSTA(35) = msta35
              para(28) = para28
              msta(40) = msta40
           elseif (seml) then
              MSTA(35) = min (1,msta35)
              para(28) = 0.
              if (para28.gt.0) then
                  para(28) = para28
                  MSTA(35) = msta35
                  if(MSTA35.eq.0) then
                     MSTA(40)= 2
                  elseif(MSTA35.eq.2)then
                     msta(40) = 1
                  endif
              endif
           endif
           call totmpj(idebu,ipsho,ntro)
           if ( qqgen(jgcod)) call tojet(1,nz0,idebu)
           if ( wwgen(jgcod)) call tojet(2,nz0,idebu)
           if (nz0.eq.0) msti(1) = 25        !  this is a WW event
           if (nz0.eq.2) msti(1) = 22        !  this is a ZZ event
C           call TOLUJ(IDEBU,ipsho,ntro,ibe,ireco,icopy) 
           do ij = 1,3
             call hfill(20+ij,plu(0,ij),dum,1.)
           enddo
             call hfill(24,ecmi-plu(0,4),dum,1.)
             call hfill(25,plu(0,4),dum,1.)
             call hfill(300,float(msti(1)),dum,1.)
           call AREXEC
           call luexec
           IF (idebu.ge.1) call lulist(1)
           if (MSTA(35).gt.0) then
C   add info for reconnected events in KWGT
            weik = float(mhar(135))  ! # of reconnections in event
            nkwgt = nkwgt + 1
            ind = kwgtbk(nkwgt,2000,weik)
            call hfill(100,weik,dum,1.)
            if (seml) call hfill(202,weik,dum,1.)
            if (fulh) call hfill(201,weik,dum,1.)
            weik = float(mhar(136))  ! # of reconnections in event, same W
            nkwgt = nkwgt + 1
            ind = kwgtbk(nkwgt,2001,weik)
            if (seml) call hfill(204,weik,dum,1.)
            if (fulh) call hfill(203,weik,dum,1.)
C            if (seml .and. mhar(136).ne.mhar(135)) then
C              print *,' semilep with diff reco',mhar(135),mhar(136)
C              call lulist(1)
C             endif
           endif
           if ( plu(0,4).gt.1.2*ecmi)  call lulist(1)
           if ( plu(0,4).lt.0.95*ecmi)  call lulist(1)
        else if (ipsho.eq.1) then
          if  (mod(ireco,10).eq.4) then
           call totmpj(idebu,ipsho,ntro)
           if ( qqgen(jgcod)) iflagg = 1
           if ( wwgen(jgcod)) iflagg = 2
           call tojet(iflagg,nz0,idebu)
C   now do parton shower with the retuned QCD parameters
           call redops(iflagg,nz0)
           if (idebu.ge.1) call lulist(1)
           do ij = 1,3
             call hfill(20+ij,plu(0,ij),dum,1.)
           enddo
           call hfill(24,ecmi-plu(0,4),dum,1.)
           call hfill(25,plu(0,4),dum,1.)
           if (idebu.ge.1) print *,' now calling recogal'
           call recogal(nreco)
           weik = float(nreco)
           nkwgt = nkwgt + 1
           ind = kwgtbk(nkwgt,2000,weik)
           call hfill(100,weik,dum,1.)
           if (seml) call hfill(202,weik,dum,1.)
           if (fulh) call hfill(201,weik,dum,1.)
           if (idebu.ge.1) print *,' GAL switches ',nreco
           call luexec
          else
           if (full) then
C   for full leptonic, no way to aplly BE effects or anything ....
C    taus live too long !
              icopy = 1
           endif
           if (mod(ireco,10).eq.1 .and. seml) then
              icopy = 1
              ireco = 0
           endif
C           print *,' calling toluj with ireco,icopy ',ireco,icopy
           call TOLUJ(IDEBU,ipsho,ntro,ibe,ireco,icopy)
           do ij = 1,3
             call hfill(20+ij,plu(0,ij),dum,1.)
           enddo
C           if (plu(0,1).gt.0.002) call lulist(1)
           call hfill(24,ecmi-plu(0,4),dum,1.)
           call hfill(25,plu(0,4),dum,1.)
           if (ibe.eq.0 .and. icopy.eq.0) 
     $             call TOFRAG (ireco,inewbe,mstj51)
C           if (full) then
C              icopy = icoref
C           endif
C           if (seml) then
              icopy = icoref
              ireco = irecoref
C           endif
           IF (idebu.ge.1) call lulist(1)
        endif
       endif
C
C        write(loutp,*) ' ==> after TOHAD'
C
C Get primary vertex from VERT NR=1 or Row 1 of FVER
C
        JVERT = NLINK('VERT',1)
        JFVER = IW(NAMIND('FVER'))
        IF ((JVERT.GT.0).OR.(JFVER.GT.0)) THEN
          IF (JFVER.GT.0) then
            VTX(1) = RW(JFVER+LMHLEN+JVERVX)
            VTX(2) = RW(JFVER+LMHLEN+JVERVY)
            VTX(3) = RW(JFVER+LMHLEN+JVERVZ)
            VTX(4) = RW(JFVER+LMHLEN+4)
             do ivec = 2,nvro
               do ij = 1,4
                 vsec(ivec-1,ij) = rtabl(JFVER,ivec,ij)
               enddo
             enddo
          ELSE IF (JVERT.GT.0) THEN
            VTX(1) = RW(JVERT+3+JVERVX)
            VTX(2) = RW(JVERT+3+JVERVY)
            VTX(3) = RW(JVERT+3+JVERVZ)
            VTX(4) = RW(JVERT+3+JVERTI)
            do ivec = 2,nvro
               JVER = NLINK('VERT',ivec)
               if ( JVER.gt.0) then
                  do ij = 1,4
                    vsec(ivec-1,ij) = rw(jver+3+ij)
                  enddo
               endif
            enddo
          endif           
C           print * ,'vertex', vtx 
C           if ( nvro.gt.1) print * ,((vsec(i,ij),ij=1,4),i=1,nvro-1)
         do m1=1,3
            call hfill(10+m1,vtx(m1),dum,1.)
          enddo
        ELSE
          WRITE(LOUTP,*) 'No primary vertex found'
          STOP
        ENDIF
CBB   HEPEVT: dump it
      
      icr  = 0
      pcr = 0.
CBB
C
C Create new KHIS, KINE and VERT banks
C
C    preserve KZFR/FZFR in case they need to be restored later
        jtmp = nlink('KZFR',0)
        if ( jtmp.gt.0) Call bswap(IW,'KZFR','KTMP')
        ktmp = nlink('FZFR',0)
        if ( ktmp.gt.0) Call bswap(IW,'FZFR','KTMP')
        if (icopy.lt.1) then
          CALL BDROP(IW,'KHISKINEVERTKZFR')
          CALL BDROP(IW,'FKINFVERFPOIFZFR')
        endif
        CALL BKFMT ('VERT','3I,4F,(I)')
        CALL BKFMT ('KINE','3I,4F,(I)')
        CALL BKFMT ('KHIS','(I)')
        CALL BKFMT ('KZFR','2I,(F)')
        CALL BKFMT ('KTMP','2I,(F)')
C
        IERx = 0
        IRX = 0
        if (ipsho.eq.13) then
          CALL KXHEAL(VTX,IRx,NVRT,NTRK)
C        write(loutp,*) ' ==> after KXHEAL'
C        IF (idebu.ge.1) call hwuepr
          IST = IRX
          IF (IRx.NE.0) THEN
            WRITE(LOUTP,*) 'KXHEAL Failed'
            WRITE(LOUTP,*) 'KXHEAL Errorcode:',IRx
            CALL HWUEPR
            GO TO 10
          ENDIF
        else
C   jetset or ariadne
          call lutabu(11)
          call lutabu(21)
          do ij = 1,3
             call hfill(520+ij,plu(0,ij),dum,1.)
          enddo
           call hfill(524,ecmi-plu(0,4),dum,1.)
           call hfill(525,plu(0,4),dum,1.)
          if ( icopy.lt.1) then
            CALL KXL7AL(VTX,IERx,NVRT,NTRK)
            IST = IERx
            IF(IST.NE.0) THEN
              WRITE(LOUTP,*) 'KXl7AL Failed at event', irevt
              WRITE(LOUTP,*) 'KXl7AL Errorcode:',IERx
              call lulist(1)
              GO TO 10
            ENDIF
          else
           NTRK = ntro
           NVRT = nvro
          endif
C - some histograms ....
          do ij =1,n7lu
            if (klu(ij,1).eq.1) then
              call hfill(600,-log(2.*plu(ij,8)/ecmi),dum,1.)
              call hfill(700,plu(ij,8),dum,1.)
              idd = 602
              if(klu(ij,6).ne.0) idd = 601
              call hfill(idd,-log(2.*plu(ij,8)/ecmi),dum,1.)
              call hfill(idd+100,plu(ij,8),dum,1.)
            endif
          enddo
C -   book fragmentation info
         
         if ( ibe.eq.0 .and. icopy.eq.0) then
          CALL KZFRBK (IST)
          IF(IST.NE.0) THEN
              WRITE(LOUTP,*) 'KZFRBK Failed at event', irevt
              WRITE(LOUTP,*) 'KZFRBK Errorcode:',IST
              call lulist(1)
            GO TO 10
          ENDIF 
         else
           jtmp = nlink('KTMP',0)
           if (jtmp.gt.0) then
              Call bswap(IW,'KZFR','KTMP')
           endif
         endif
        endif
        CALL KIBPRS ('VERTKINE')

        if(jeveh.le.0) then
          write(6,*) 'EVEH bank lost !'
        endif     
C
C Refill KEVH bank with new values
C
        JKEVH = NLINK('KEVH',0)
        if(jkevh.le.0) then
          write(6,*) 'KEVH bank lost !'
        else
          IW(JKEVH+2+JKEVNT) = NTRK
          IW(JKEVH+2+JKEVNV) = NVRT
        endif
C  restore secondary vertices if no change 
        if ( ipsho.eq.1 .and. (ibe.gt.0 .or. icopy.gt.0 .or. full)) then
           if ((ntrk.ne.ntro) .or. (nvrt.ne.nvro)) then
            write(iw(6),'(/1x,''Event'',i6,'' pb with track-vertices '',
     &      4i5)') IREVT,ntrk,ntro,nvrt,nvro
            call prkine
            go to 10
           else
C            print *,' here we go for copy of secondary vertices ',nvro
C            if (nvro.ge.2) print *,((vsec(ivec,ij),ij=1,4),ivec=1,nvro-1)
            do ivec = 2,nvro
               JVER = NLINK('VERT',ivec)
               if ( JVER.gt.0) then
                  do ij = 1,4
                    rw(jver+3+ij) = vsec(ivec-1,ij) 
                  enddo
               endif
            enddo
           endif
        endif
        IF (IDEBU.GT.0) then
            WRITE(IW(6),'(/1X,''IEVT,IDPR,ISTA,NTRK,NVRT,ECMS,WEIT'',
     +       5I10,F10.4,3X,E12.5)') IREVT,IDPR,IST,NTRK,NVRT,ECMI,WEIT
           CALL PRKINE
           JKHIS = NLINK ('KHIS',0)
           IF (JKHIS.GT.0) THEN
             WRITE (LOUTP,'(/1X,''+++KINGAL+++ KHIS bank print out'')')
             NROW = IW(JKHIS+LMHROW)
             M1 = 1
 19          M2 = MIN (M1+9,NROW)
             WRITE (LOUTP,'(/1X,10I10)') (LLL,LLL=M1,M2)
            WRITE(LOUTP,'(1X ,10I10)') (IW(JKHIS+LMHLEN+LLL),LLL=M1,M2)
             M1 = M1+10
             IF (M1.LE.NROW) GOTO 19
           ENDIF
           JKZFR = NLINK ('KZFR',0)
           IF (JKZFR.GT.0) THEN
             WRITE (LOUTP,'(/1X,''+++KINGAL+++ KZFR bank print out'')')
             NROW = IW(JKZFR+LMHROW)
             M1 = 1
 21          M2 = MIN (M1+9,NROW)
             WRITE (LOUTP,'(/1X,10I10)') (LLL,LLL=M1,M2)
          WRITE(LOUTP,'(1X ,10F10.3)') (RW(JKZFR+LMHLEN+LLL),LLL=M1,M2)
             M1 = M1+10
             IF (M1.LE.NROW) GOTO 21
           ENDIF
           JKWGT = NLINK ('KWGT',0)
           IF (JKWGT.GT.0) CALL PRTABL('KWGT',0)
         
        endif
      if (CRETE) then
        if ( ipsho.ne.13) call newhepc(1)
        if ( ipsho.eq.13) call herhepc(1)
C         call hwuepr
         icr  = 0
         pcr = 0.
        if ( ipsho.eq.1 ) then

            JKWGT = NLINK ('KWGT',0)
            IF (JKWGT.GT.0) then
               do i = 1,lrows(JKWGT)
                  m1 = krow(JKWGT,i)
                  if ( iw(m1+1).eq.1000) then
                    pcr = rw(m1+2)
                    icr = 1
                    go to 121
                  endif
               enddo
               pcr =0.
            endif
 121        continue
         elseif ( ipsho.eq.13 .or. ipsho.eq.2) then
            JKWGT = NLINK ('KWGT',0)
            IF (JKWGT.GT.0) then
               do i = 1,lrows(JKWGT)
                  m1 = krow(JKWGT,i)
                  if ((iw(m1+1).eq.2000).or.(iw(m1+1).eq.2001)) then
                    if (iw(m1+1).eq.2000) icr = rw(m1+2)
                    if (iw(m1+1).eq.2001) pcr = rw(m1+2)
                    if ( pcr.gt.0.) go to 122
                  endif
               enddo
               pcr =0.
            endif
 122        continue
            if (ipsho.eq.13) pcr = float(min(1 ,icr))
         endif
C         if(ibe.gt.0) call vhepmod(vsec,nvro)
         call hepwr(IREVT,icr,pcr)
      endif
 12       continue
 11     IF (ISAVE.GT.0) THEN
         IF( ier.ne.3) then
          IF (JEVEH.GT.0) then
            ISEVT = ISEVT + 1
            if ( fulh) ityp(1) = ityp(1) + 1
            if ( seml) ityp(2) = ityp(2) + 1
            if ( full) ityp(3) = ityp(3) + 1
          endif
          CALL BGARB(IW)
C   Redefine the output lists to drop info created on POT or Mini
          call blist(iw,'E=','EVEHKEVHKMARVERTKINEKHISKZFRKWGTKW4F')
          call blist(iw,'C=','RUNRRUNHKRUNKRANKJOBPARTKCARRLEPKLUNKPAR') 
          call blist(iw,'C+','KLINMSTUPARUMSTJPARJPMA1PMA2PMA3PMA4MDM1')
          call blist(iw,'C+','MSTAPARAGADMGRPLGMOBKORLMSELMSUBMSTP')
C          call aublis(olist) 
         else
           WRITE(LOUTP,*) '  Banks found on input : '
           call prtabl('KSEC',0)
           call prtabl('RUNE',0)
           if (crete) call ksecwr
         endif
         CALL ABWSEL(OLIST)
        ENDIF
C        print *,' finish with event ', irevt
        js = 1
        call alevnum(js,nrun,irevt)
        GOTO 10
      ENDIF
      CALL BGARB(IW)
C
C Create and write a new RUNE bank updated for run number and events 
C
      JRUNH = IW(NAMIND('RUNH'))
      IERx  = ALRUNE(NRUN,IW(JRUNH+1),IW(JRUNH+3),ISEVT)
      call prtabl('RUNE',0)
      CALL BLIST(IW,'C=','RUNEKSEC')
      CALL ABWSEL('C')
C
C Close output file
C
      CALL ABWEND
      IF (IDEBU.GT.0) CALL BOSIO
      CALL ACLOSE(0,IERx)
C
C Summary
C
      WRITE(LOUTP,110)
      WRITE(LOUTP,100)
      WRITE(LOUTP,120) '  R E F R A G M E N T A T I O N    '
      WRITE(LOUTP,100)
      WRITE(LOUTP,120) '     S U M M A R Y                 '
      WRITE(LOUTP,100)
      WRITE(LOUTP,220) IREVT
      if (wwgen(jgcod))WRITE(LOUTP,221) ityp      
      if ( ihad.gt.0) WRITE(LOUTP,360) ihevt
      WRITE(LOUTP,240) ISEVT
      WRITE(LOUTP,100)
      WRITE(LOUTP,110)
      if (ihad.eq.0) write(loutp,370) IREVT-ILOW,ISEVT
      if (ihad.eq.1) write(loutp,370) IHEVT-ILOW,ISEVT
      WRITE(LOUTP,110)
 100  FORMAT(1X,'|                                            |')
 110  FORMAT(1X,'+--------------------------------------------+')
 120  FORMAT(1X,'|',A44,'|')
 220  FORMAT(1X,'|  Number of events read    : ',I7,'        |')
 221  FORMAT(1x,'|  fully hadronics          : ',I7,'        |',/,
     &       1x,'|  semi leptonics           : ',I7,'        |',/,
     &       1x,'|  fully leptonics          : ',I7,'        |')
 360  FORMAT(1X,'|  Number of events treated : ',I7,'        |')
 370  FORMAT(1X,'|  according to Kinagain flags, Nb of Events |',/,
     &       1x,'|  treated : ',I7,' and written : ',I7,'   |')
 240  FORMAT(1X,'|  Number of events written : ',I7,'        |')
 200  FORMAT(1X,'CRCKIN - Accessing Data Base for KREF NR = ',I5)
      if ( ipsho.le.2) then
        call lutabu(12)
        call lutabu(22)
      endif
C  suggested HIST card to store Histos on a disk file
C       HIST  'file name specifications '
      JHIST= NLINK('HIST',0)
      IF(JHIST.NE.0 ) THEN
         IF (IERx.EQ.0) THEN
            CALL HRPUT(0,FFNAM,'N')
            WRITE(LOUTP,'(/,1X,''******** HISTOGRAMS WRITTEN TO DISK FILE
     $ '',A80)') FFNAM
         ENDIF
       ENDIF
      CALL HPHST(0)
      END
#endif
      SUBROUTINE TOLUJ(IDEBU,ipsho,ntro,ibe,ireco,icopy)
      INTEGER IDEBU,ipsho,icopy,ibe,ntro
#include "bcs.h"
      INTEGER JKIN,JHIS,JKLI,KS,MO,LHHIS,HIS,IDCMS
      INTEGER NAMK,NAMV,NAMH,NAML,LOUT,LHKIN,LPKIN,ALCODE,JTCODE,KIN
C   fills back common LUJETS from KINE banks
      PARAMETER (IDCMS = 94, IDstring=92, IDclus=91)
      real*4 xp(5)
C#include "lun7com.h"
      parameter (lout=6)
      PARAMETER (LJNPAR=4000) 
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5)
      integer L1MST, L1PAR, L2PAR, L2PARF
      PARAMETER (L1MST=200, L1PAR=200)
      PARAMETER (L2PAR=500, L2PARF=2000 )
      REAL*4 PARU,PARJ,BRAT,PMAS,PARF,VCKM
      INTEGER MSTU,MSTJ,MDCY,MDME,KFDP,KCHG
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),
     &                KFDP(L2PARF,5)
C
      logical done
#include "bmacrod.h"
#include "bmacro.h"
C
      N7LU = 0
      iminus = 0
      nbeam = 0
      nqua = 0
      nsav = 4
      done = .false.
      NAML = NAMIND('KLIN')
 2    call getpart(xp,ks,kfa,km,ier)
C    KINE with NR<0 are beam electrons
         if ( ier.lt.0 .and. N7LU.gt.2) go to 3
         if (ks.eq.21) nbeam= nbeam+1
         mo = km
         JKLI = iw(NAML)
         JTCODE = ITABL(JKLI,kfa,1)
         kf = JTCODE
C    if JETSET copy .....copy whole evnt
      if (icopy.gt.0) then
         done = n7lu-nbeam.eq.ntro
C    if JETSET BE .....copy whole evnt
      elseif ( ibe.gt.0) then
         done = n7lu-nbeam.eq.ntro
C    if new fragmentation ...copy to 4 fermions only or first CMShower
C     for 2 fermions
      elseif ((ipsho.ne.1).or.((ipsho.eq.1).and.(ireco.eq.0))) then
C         done = (nqua.eq.4) .or. (kf.eq.IDCMS)
         done = (nqua-iminus.eq.4).or.(kf.eq.IDCMS).or.(kf.eq.IDstring)
     &                                   .or.(kf.eq.IDclus)
         if (abs(kf).lt.10.and.ks.gt.10) then
            ks = 2
            nqua = nqua +1
            if ( mod(nqua,2).eq.0 ) ks =1
         endif
C    if JETSET reco .....copy up to parton shower
      else
         done = (kf.eq.IDstring).or.(kf.eq.IDclus) 
      endif
      if ( done)           go to 3
         IF(IDEBU.GE.1)
     +    WRITE (LOUT,1012) ier,kf,xp,MO,KS
       if (nqua.gt.2) then
C          print *,' extra quark ',nqua,n7lu 
C check if this is not a second copy of same quarks ...
              if ((xp(1).eq.p7lu(n7lu+nqua-4,1)).and.
     &            (xp(2).eq.p7lu(n7lu+nqua-4,2)).and.
     &            (xp(3).eq.p7lu(n7lu+nqua-4,3))) then
C do not copy this quark
C              print *,' skip this quark', iminus
                    iminus = iminus + 1
                     go to 2
              endif
       endif
          N7LU = N7LU +1
C      else
C   copy particles which may decay before Bose Einstein only 
C   otherwise freeze their decay products
            kc = lucomp(kf)
            if ( icopy.gt.0) go to 5
            if ( ibe.eq.0) go to 5
            call BEFROZ(KC,KS,KF,mo,nbeam,KSNEW,nsav)
CZ            if ( KCHG(KC,2).NE.0) go to 4        ! isosinglet particle
CZ            if ( ks.lt.10 ) go to 4              ! not decayed
CZ            if (abs(kf).eq.311)  go to 4     ! K0
CZ            if (pmas(kc,2).ge.parj(91)) go to 4  ! decay before Bose EInstein
CZ            if (ks.gt.20) go to 4                ! doc lines ok
CZ            kfb = abs(kf)
CZ            if ((kfb.ne.15).and.(kfb.lt.100)) go to 4 ! quarks,leptons,
C                                                  string .. but not Tau
C            print *,' frozen ? ',n7lu,kf
CZ             ks = ks-10                     ! particle should not decay now
C         endif
C   do not copy if mother undecayed
CZ 4       continue
C         if ((k7lu(mo+nbeam,1).lt.10).and.((abs(kf).gt.100).or.
C     &          (abs(k7lu(mo+nbeam,2)).eq.15) .or. (kf.eq.22))) then
CZ         if ((k7lu(mo+nbeam,1).lt.10).and.(mo.gt.0)) then
C         print *,n7lu,'mother',mo+nbeam,k7lu(mo+nbeam,2),'status',
C     &     k7lu(mo+nbeam,1),'skip' 
CZ            if (abs(kf).ne.21) 
CZ     $                                   ks = 0
CZ         endif
CZ         if (kf.eq.94) nsav = max(nsav,n7lu)
CZ         if (nsav.eq.4.and.kf.ge.91) nsav = max(nsav,n7lu)
C         if ( ksnew.ne.ks ) print *,' new status at trk ',n7lu,ksnew
         if ( ksnew.ne.ks ) ks = ksnew

 5       continue
C    for copy of Bhabhas which do not have any history
         if (ks.eq.0 .and. kf.eq.22 ) ks = 1
         if (ks.eq.0 .and. abs(kf).eq.11 ) ks = 1
C    end mod 
         K7LU(N7LU,1) = KS
         K7LU(N7LU,2) = kf
         K7LU(N7LU,3) = MO
         if ( mo.gt.0) K7LU(N7LU,3) = MO+nbeam
         K7LU(N7LU,4) = 0
         K7LU(N7LU,5) = 0
         do it=1,5
           P7LU(N7LU,it) = xp(it)
         enddo
C         P7LU(N7LU,5) =  am
         go to 2
 3       continue
C   now restore mother to daughter relation and fix Pi0's status if
C   from tau decays
          do imo = 1,n7lu
            idauf = 10000
            idaul = 0
            itf = min(imo+1,n7lu)
            do it = itf,n7lu
              if ( K7LU(it,3).eq.imo) then
                idauf = min(idauf,it)
                idaul = max(idaul,it)                
              endif
            enddo
            if ( idauf.lt.10000) K7LU(imo,4)= idauf 
            if ( idaul.gt.0)     K7LU(imo,5)= idaul
            if ( ibe.le.0 .and. icopy.eq.0) then
C   particles with no daugters cannot be declared decayed
C   in case of 'copy' just let it be as it is
C   keep status of beam electrons and CMshowers...
              if ((k7lu(imo,1).ne.21).and.(K7LU(imo,2).ne.IDCMS)) then
              if ((k7lu(imo,4).le.0).and.(k7lu(imo,1).gt.10)) 
     $             K7LU(imo,1) = K7LU(imo,1)-10
              endif
            endif 
          enddo
      if (nsav.eq.4) nsav = n7lu   ! there were no quarks to fragment
      if(idebu.ge.1) call lulist(1)
      if (ibe.gt.0) call bewarm(idebu,nsav)
CW      if ((ibe.gt.0)) then
CW         call hfill(200,float(nsav),dum,1.)
CW         if (nsav.gt.40) call lulist(1)
C         print * ,'starting LUBOEI nsav',nsav,n7lu
CW         call luboeiw(nsav)
CW        if(idebu.ge.1) then
CW           print *,' after LUBOEI'
C           call lulist(1)
CW        endif
      if ( ibe.eq.0 ) go to 99
C     restore decays not affected by Bose Einstein
C      print *,'n7lu', n7lu   
CW        nsav2 = n7lu 
CW        do it= 3,nsav2
CW          jtcode = k7lu(it,2)
CW          kc = lucomp(jtcode)
CW          if (mdcy(kc,1).gt.0) then
C if Pi0 may be affected : redecay   ks = 1
C if Pi0 not affected (ks =0) or something else : daughters are ok
CW             if ( abs(jtcode).eq.111) then
CW                if (k7lu(it,1).eq.1) then
C          print *,'Pi0track',it,'was decayed to ',k7lu(it,4),k7lu(it,5)
CW                   iolf = k7lu(it,4)
CW                   ioll = k7lu(it,5)
CW 13       continue
CW                  if ( iolf.gt.0) then
CW                    call ludecy (it)
CW                    ndec = ioll-iolf +1
C          print * ,'now to ',k7lu(it,4),k7lu(it,5)
CW                    ndecn = k7lu(it,5)-k7lu(it,4) +1
CW                    if ( ndecn.eq.ndec) then
C copy new decay products
CW                    do itt = k7lu(it,4),k7lu(it,5)
CW                     inew = iolf+itt-k7lu(it,4)
C                 print *, ' copying new to old', itt,inew
CW                      do itp = 1,5
CW                         k7lu(inew,itp) = k7lu(itt,itp)
CW                         p7lu(inew,itp) = p7lu(itt,itp)
CW                      enddo
CW                    enddo
CW                    n7lu = n7lu-ndecn
C                  print *, 'new length',n7lu
CW                    else
C                  print *,' try again',it,ndec,ndecn
CW                       n7lu = n7lu-ndecn
CW                       go to 13           ! try again
CW                    endif
CW                  endif
CW                endif
CW                if (k7lu(it,1).eq.0) then
C          print *,'Pi0track',it,'restored to ',k7lu(it,4),k7lu(it,5)
CW                   k7lu(it,1) = 11
CW                endif
CW             else 
CW              if (k7lu(it,1).le.1 ) then
C restore if there are daughters
CW                if (k7lu(it,4).gt.0) k7lu(it,1) = 11
C          print *,'track',it,jtcode,'restored to ',k7lu(it,4),k7lu(it,5)
CW              endif
C    B which have mixed may have ks =2 .... restore as well
CW              if (k7lu(it,1).eq.2 .and. abs(k7lu(it,2)).gt.510) then
CW                  k7lu(it,1) = k7lu(it,1) + 10
CW         print *,'track',it,jtcode,'restored to ',k7lu(it,4),k7lu(it,5)
CW              endif 
CW             endif   
CW          else
CW             if (k7lu(it,1).eq.0) then
CW                 k7lu(it,1) = 1
C          print *,'track',it,jtcode,'restored to stable'
CW             endif
CW          endif
CW        enddo
 99     continue
CZ      endif
1012  FORMAT(T3,I4,4X,I8,4X,5(F12.3),4X,I5,4X,I5)
      END
C#endif
      subroutine totmpj(idebu,ipsho,ntro)
      INTEGER IDEBU,ipsho,icopy,ibe,ntro
#include "bcs.h"
      INTEGER JKIN,JHIS,JKLI,KS,MO,LHHIS,HIS,IDCMS
      INTEGER NAMK,NAMV,NAMH,NAML,LOUT,LHKIN,LPKIN,ALCODE,JTCODE,KIN
C   fills back common LUJETS from KINE banks
      PARAMETER (IDCMS = 94, IDstring=92, IDclus=91)
      real*4 xp(5)
      integer tmpPAR 
      PARAMETER (tmpPAR=150) 
      COMMON /tmJETS/ Ntmp,Ktmp(tmpPAR,5),Ptmp(tmpPAR,5) 
      REAL*4 Ptmp
      INTEGER Ntmp,Ktmp
      logical done
#include "bmacrod.h"
#include "bmacro.h"
C
      Ntmp = 0
      nbeam = 0
      nqua = 0
      done = .false.
      NAML = NAMIND('KLIN')
 2    call getpart(xp,ks,kfa,km,ier)
C    KINE with NR<0 are beam electrons
         if ( ier.lt.0 .and. Ntmp.gt.2) go to 3
         if (ks.eq.21) nbeam= nbeam+1
         mo = km
         JKLI = iw(NAML)
         JTCODE = ITABL(JKLI,kfa,1)
         kf = JTCODE
         done = (ntmp-nbeam.eq.ntro).or.(ntmp.eq.tmppar)
C     &                                   .or.(kf.eq.IDclus)
      if ( done)           go to 3
         IF(IDEBU.GE.1)
     +    WRITE (6,1012) ier,kf,xp,MO,KS
         NTMP = NTMP +1
         KTMP(NTMP,1) = KS
         KTMP(NTMP,2) = kf
         KTMP(NTMP,3) = MO
         if ( mo.gt.0) KTMP(NTMP,3) = MO+nbeam
         KTMP(NTMP,4) = 0
         KTMP(NTMP,5) = 0
         do it=1,5
           PTMP(NTMP,it) = xp(it)
         enddo
C         PTMP(NTMP,5) =  am
         go to 2
 3       continue
C   now restore mother to daughter relation 
         IF(IDEBU.GE.1) call tmplist(1) 
         do imo = 1,ntmp
            idauf = 10000
            idaul = 0
            itf = min(imo+1,ntmp)
            do it = itf,ntmp
              if ( Ktmp(it,3).eq.imo) then
                idauf = min(idauf,it)
                idaul = max(idaul,it)                
              endif
            enddo
            if ( idauf.lt.10000) Ktmp(imo,4)= idauf 
            if ( idaul.gt.0)     Ktmp(imo,5)= idaul
          enddo
      if(idebu.ge.1) call tmplist(2)
1012  FORMAT(T3,I4,4X,I8,4X,5(F12.3),4X,I5,4X,I5)
      return
      end
      subroutine tojet(ifl,nz0,idebu)
      integer tmpPAR
      PARAMETER (tmpPAR=150) 
      COMMON /tmJETS/ Ntmp,Ktmp(tmpPAR,5),Ptmp(tmpPAR,5) 
      REAL*4 Ptmp
      INTEGER Ntmp,Ktmp
      PARAMETER (LJNPAR=4000) 
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5)
      dimension imoth(2),imoto(2)
      PARAMETER (IDCMS = 94, IDstring=92, IDclus=91) 
      logical done,kfsho
      n7lu = 0
      nbos = 0
      nqua = 0
      nda = 0
      nisr = 0
      ntau = 0
      nlep = 0
      nz0 = 0
      imoth(1) = 0
      imoth(2) = 0
      done = .false.
      imoto(1) = 0
      imoto(2) = 0
      do i = 1,ntmp
CB         print *,' nbos,nqua,nda,ntau,nisr',nbos,nqua,nda,ntau,nisr
         if ( done ) go to 3
         N7LU = N7LU +1
         do it =1,5
           K7LU(i,it) = Ktmp(i,it) 
           P7LU(i,it) = Ptmp(i,it)            
           V7LU(i,it) = 0.
         enddo
         kf = K7LU(i,2)
         ks = K7LU(i,1)
         km = K7LU(i,3)
         if (abs(kf).lt.10.and.ks.gt.10) then
            ks = 2
            nqua = nqua +1
            if ( mod(nqua,2).eq.0 ) ks =1
            K7LU(i,4) = 0
            K7LU(i,5) = 0
            K7LU(i,1) = ks
         elseif ( kf.eq.22 .and. km.eq.0) then
            nisr = nisr + 1     
         else if (((abs(kf).eq.23).or.(abs(kf).eq.24))
     &                                .and.(ks.gt.10)) then
            if ( km.eq.0) then
              nbos = nbos +1
              imoth(nbos) = n7lu
              if ( kf.eq.23 ) nz0 = nz0+1
            endif
         endif
         if ( ifl.eq.1 ) then
C   we look for qqbar
           if (km.eq.imoth(1).and. imoth(1).gt.0) nda = nda +1
           done = (nbos.eq.1).and.(nqua.eq.2)
         elseif ( ifl.eq.2 ) then
C  we look for WW or ZZ
C   beams ?
           if ( ks.eq.21 ) then
              nbeam = nbeam +1
C daughters of W/Z
           else if ((abs(kf).le.22).and.(nbos.eq.2)) then
              if ( (km.eq.imoth(1)) .or. (km.eq.imoth(2))) then
                  nda = nda +1
                  if(abs(kf).gt.10) nlep = nlep +1
                  if ( abs(kf).eq.15 ) then
                      ntau = ntau +1
                      imoto(ntau) = n7lu
                  endif
               endif
           endif
           kfsho =(kf.eq.idcms).or.(kf.eq.idclus).or.(kf.eq.idstring)  
           done = (nbos.eq.2).and.((nqua.eq.4 .and. ntau.eq.0).or.
     &            (nqua.eq.2 .and. nda.ge.4 .and. ntau.eq.0  .and.
     &             kfsho)) .or.
     &            (nda.ge.4 .and. ntau.gt.0 .and. kfsho)
           if (kfsho.or.(nqua+nlep  .gt. nda)) then
CB              print *,' we stop line ',i
              n7lu = n7lu-1
              go to 4
           endif
         endif
CB         print *,' we keep line ',i
 4    continue
      enddo
 3    continue
C  now get back the Pi0and eta  decay products
      nsav = n7lu
      nsav2 = n7lu
        i = 0
 5      continue
        i = i +1
        
        if ( i.gt.n7lu) go to 6
CC      do i = 1,nsav
         kf = k7lu(i,2)
         ks = k7lu(i,1)
         km = k7lu(i,3)
         if ( (kf.eq.111 .or. kf.eq.221).and. ks.eq.11) then
            ifi =  k7lu(i,4)
            ila =  k7lu(i,5)   
C  ifi,ila already stored ? we already have the full event 
            if ( ifi.le.nsav .and. ila.le.nsav ) go to 5
CB         print *,kf,ks,i,' 1-look for Pi0 daughters in ',ifi,ila        
CC            do j= nsav+1,ntmp
CC              if ( ktmp(j,3).eq.i) then
            do j = ifi,ila
              if ( n7lu.lt.ntmp) then
                 n7lu = n7lu+1
              else
CB                 print *,' +++ we have a pb n7lu,ntmp =',n7lu,ntmp
                 call tmplist(2)
              endif
CB              print *,' 2-we add line ',n7lu,' fill with temp',j
              do it =1,5
                K7LU(n7lu,it) = Ktmp(j,it) 
                P7LU(n7lu,it) = Ptmp(j,it)            
                V7LU(n7lu,it) = 0.
              enddo 
              k7lu(i,4)= min (n7lu, nsav2+1)
              k7lu(i,5)= max (nsav2+1,n7lu)
              k7lu(n7lu,3)= i
CC              endif
            enddo
            nsav2 = n7lu
CB            print *,' 3-new daughters line ',k7lu(i,4),k7lu(i,5)
CB              if (k7lu(i,4).eq. k7lu(i,5)) then
CB                print *,' 4-pb decay pi0 or eta',
CB     &          ' daughters = ',k7lu(i,4),k7lu(i,5),ifi,ila
CB               call tmplist(1)
CB              endif
         endif     
         go to 5
 6       continue
CC      enddo
CC      do i = 1,n7lu
CC         kf = k7lu(i,2)
CC         ks = k7lu(i,1)
CC         if ( (kf.eq.111).and. ks.eq.11) then
CC              if (k7lu(i,4).eq. k7lu(i,5)) then
CC                 k7lu(i,1)= 1
CC                 call ludecy(I) 
CC              ENDIF  
CC         endif
CC      enddo
      if ( idebu.gt.0) then
        print * ,' event restored '
        call lulist(1)
      endif
      return
      end
      subroutine tmplist(ifl)
      integer tmpPAR
      PARAMETER (tmpPAR=150) 
      COMMON /tmJETS/ Ntmp,Ktmp(tmpPAR,5),Ptmp(tmpPAR,5) 
      REAL*4 Ptmp
      INTEGER Ntmp,Ktmp
      print *,'       Temp Event listing summary'
      if ( ifl.eq.1) then
         print 100
 100  format(6x,' I      ks      kf      orig   p_x      p_y  ',
     & 'p_z       E       m')
         do i = 1,ntmp
           print 101,i,Ktmp(i,1),Ktmp(i,2),Ktmp(i,3),ptmp(i,1)
     &              ,ptmp(i,2),ptmp(i,3),ptmp(i,4),ptmp(i,5)
         enddo
 101  format (2x,i6,3i8,5f8.3)
      elseif ( ifl.eq.2 ) then 
      print 200
 200  format(6x,' I      ks      kf      km ki kl     p_x      p_y ',
     &'  p_z    E       m')
         do i = 1,ntmp
           print 201,i,Ktmp(i,1),Ktmp(i,2),Ktmp(i,3),Ktmp(i,4),
     &                 Ktmp(i,5),ptmp(i,1)
     &              ,ptmp(i,2),ptmp(i,3),ptmp(i,4),ptmp(i,5)
 201  format (2x,i6,3i8,2i4,5f8.3)
         enddo
      endif
      return
      end
      subroutine befroz(kc,ksi,kf,mo,nbeam,ks,nsav)
C ------------------------ B.Bloch March 2001
C     "freeze" decay products of particles which may decay after Bose 
C     Einstein only
C      inputs : 
C     kc compressed code of current particule
C     ksi status code of current particule
C     kf  code of current particule
C      output :
C     ks status code of current particule after freeze
C     nsav pointer to LUJETS to start analysing BE
C ----------------------------
      PARAMETER (LJNPAR=4000) 
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5)
      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      integer L1MST, L1PAR
      PARAMETER (L1MST=200, L1PAR=200)
      REAL*4 PARU,PARJ
      INTEGER MSTU,MSTJ
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      data  ientry/0/
      ientry = ientry +1
C      print *,' status ',ksi,' code',kf
      ks = ksi
      if ( KCHG(KC,2).NE.0) go to 4        ! isosinglet particle
      if ( ks.lt.10 ) go to 4              ! not decayed
      if (abs(kf).eq.311)  go to 4     ! K0
      if (pmas(kc,2).ge.parj(91)) go to 4  ! decay before Bose EInstein
      if (ks.gt.20) go to 4                ! doc lines ok
      kfb = abs(kf)
      if ((kfb.ne.15).and.(kfb.lt.100)) go to 4 ! quarks,leptons,
C                                                  string .. but not Tau
        ks = ks-10                     ! particle should not decay now
C       print *,' frozen',kf,' status ',ks
C   do not copy if mother undecayed
 4       continue
C         if ((k7lu(mo+nbeam,1).lt.10).and.((abs(kf).gt.100).or.
C     &          (abs(k7lu(mo+nbeam,2)).eq.15) .or. (kf.eq.22))) then
C       if ( kf.eq.22) print *,' photon mother beam',mo,nbeam
       if ((k7lu(mo+nbeam,1).lt.10).and.(mo.gt.0)) then
C         print *,ntrk,'mother',mo+nbeam,k7lu(mo+nbeam,2),'status',
C     &     k7lu(mo+nbeam,1),'skip' 
           if (abs(kf).ne.21) ks = 0
C         print *,' frozen',kf,' status ',ks
         endif
      if (kf.eq.94) nsav = max(nsav,ientry)
C    warning ..... any real particle has kf>100 ....
      if (nsav.eq.4.and.kf.ge.91.and.kf.lt.100) nsav = max(nsav,ientry)
      if ( ientry.eq.n7lu) ientry = 0
      return
      end

      subroutine bewarm(idebu,nsav)
C ----------------------------B.Bloch March 2001
C     "warm up" decay products of particles which have been affected by  
C     Bose Einstein
C      inputs : 
C     nsav  pointer to first line to be considered
C ----------------------------
      PARAMETER (LJNPAR=4000) 
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5)
      INTEGER MSTU,MSTJ,MDCY,MDME,KFDP,KCHG
      PARAMETER (L1MST=200, L1PAR=200)
      PARAMETER (L2PAR=500, L2PARF=2000 )
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      COMMON /LUDAT2/ KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),
     &                KFDP(L2PARF,5)
      am = amarset(1)
      call hfill(200,float(nsav),dum,1.)
      if (nsav.gt.40) call lulist(1)
C         print * ,'starting LUBOEI nsav',nsav,n7lu
      call luboeiw(nsav)
      if(idebu.ge.1) then
           print *,' after LUBOEI'
C           call lulist(1)
       endif
C     restore decays not affected by Bose Einstein
C      print *,'n7lu', n7lu   
      nsav2 = n7lu 
      do it= 3,nsav2
          jtcode = k7lu(it,2)
          kc = lucomp(jtcode)
          if (mdcy(kc,1).gt.0) then
C if Pi0 may be affected : redecay   ks = 1
C if Pi0 not affected (ks =0) or something else : daughters are ok
             if ( abs(jtcode).eq.111) then
                if (k7lu(it,1).eq.1) then
C          print *,'Pi0track',it,'was decayed to ',k7lu(it,4),k7lu(it,5)
                   iolf = k7lu(it,4)
                   ioll = k7lu(it,5)
 13       continue
                  if ( iolf.gt.0) then
                    call ludecy (it)
                    ndec = ioll-iolf +1
C          print * ,'now to ',k7lu(it,4),k7lu(it,5)
                    ndecn = k7lu(it,5)-k7lu(it,4) +1
                    if ( ndecn.eq.ndec) then
C copy new decay products
                    do itt = k7lu(it,4),k7lu(it,5)
                     inew = iolf+itt-k7lu(it,4)
C                 print *, ' copying new to old', itt,inew
                      do itp = 1,5
                         k7lu(inew,itp) = k7lu(itt,itp)
                         p7lu(inew,itp) = p7lu(itt,itp)
                      enddo
                    enddo
                    n7lu = n7lu-ndecn
C                  print *, 'new length',n7lu
                    else
C                  print *,' try again',it,ndec,ndecn
                       n7lu = n7lu-ndecn
                       go to 13           ! try again
                    endif
                  endif
                endif
                if (k7lu(it,1).eq.0) then
C          print *,'Pi0track',it,'restored to ',k7lu(it,4),k7lu(it,5)
                   k7lu(it,1) = 11
                endif
             else 
              if (k7lu(it,1).le.1 ) then
C restore if there are daughters
                if (k7lu(it,4).gt.0) k7lu(it,1) = 11
C          print *,'track',it,jtcode,'restored to ',k7lu(it,4),k7lu(it,5)
              endif
C    B which have mixed may have ks =2 .... restore as well
              if (k7lu(it,1).eq.2 .and. abs(k7lu(it,2)).gt.510) then
                  k7lu(it,1) = k7lu(it,1) + 10
         print *,'track',it,jtcode,'restored to ',k7lu(it,4),k7lu(it,5)
              endif 
             endif   
          else
             if (k7lu(it,1).eq.0) then
                 k7lu(it,1) = 1
C          print *,'track',it,jtcode,'restored to stable'
             endif
          endif
       enddo
      am = amarset(0)
      return
      end
      subroutine crini(ecm)
C---------------------------
#include "bcs.h"      
      COMMON/PYWWCM/ECMWW,PMW,PGW,HBAR,TFRAG,RHAD
      COMMON/PYGAL/mstp188,parp188
      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
C...Save ECM; set up some default values (modified 19/6 1995).
      ECMWW=ECM
      PMW=PMAS(24,1)
      PGW=PMAS(24,2)
      HBAR=0.2
      TFRAG=1.5
      RHAD=0.5
      write (6,100),ECMWW,tfrag,rhad,pmw,pgw
 100  format (1x,' ==>if using a color reconnection model SK I,II',
     $ ' at cm energy ',f9.3,/,' parameters Tfrag and Rhad are',2F10.3,
     $ /,'  W mass and width are ',2F10.3)
      jggal = nlink('GGAL',0)
      parp188 = 0.
      mstp188 = 0
      if ( jggal.gt.0) then
         parp188 = rw(jggal+1)
         mstp188 = 3
      write (6,101),mstp188,parp188
 101  format (1x,' ==> BUT you are using the GAL model',I3,' with R0=',
     $ f10.3)
      endif
      RETURN
      END
      SUBROUTINE TOFRAG(ireco,inewbe,mstj51)
C----------------------------------------------------------------------
C  Hadronise LUJETS Real *4 version
C----------------------------------------------------------------------
      integer ijoin(2)
      integer iflav(4)
      INTEGER nakwgt,jkwgt,nkwgt
      REAL*8 BEWW(3)
! This common can be everywhere, contains various switches
      COMMON / KeyKey /  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
#include "bcs.h"
C#include "lun7com.h"

      PARAMETER (LJNPAR=4000) 
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5)
C
!   jetset setup
      integer L1MST, L1PAR
      PARAMETER (L1MST=200, L1PAR=200)
      REAL*4 PARU,PARJ
      INTEGER MSTU,MSTJ
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      data ifir /0/
#include "bmacrod.h"
#include "bmacro.h"
C
       ifir = ifir + 1
       idau1 =0
       idau2 = 0
       imo1 =0
       imo2 = 0
       nakwgt = NAMIND('KWGT')
       do ip =1,4
          iflav(ip)= 0
       enddo
       do ip =1,n7lu
         if (abs(iflav(1)*iflav(2)*iflav(3)*iflav(4)).gt.0) go to 100
         if(k7lu(ip,2).eq.24) imo2=ip
         if(k7lu(ip,2).eq.-24) imo1=ip
         if (imo1*imo2.gt.0) then 
           if (k7lu(ip,3).eq.imo1.and.idau1.eq.0) then
               iflav(1)=k7lu(ip,2)
               idau1 = ip
           else if (k7lu(ip,3).eq.imo1.and.idau1.ge.1) then
             iflav(2)=k7lu(ip,2)
             
           else if (k7lu(ip,3).eq.imo2.and.idau2.eq.0) then
             iflav(3)=k7lu(ip,2) 
             idau2 = ip
           else if (k7lu(ip,3).eq.imo2.and.idau2.ge.1) then
             iflav(4)= k7lu(ip,2) 
             
           endif
         endif
       enddo
 100   continue
       if (abs(imo1-imo2).gt.1) call lulist(1)
C Switch to 1st random sequence if needed
       am = amarset(0)
       if (imo1.eq.0 .and.imo2.eq.0) go to 200   ! this was not a W event ..
!--------------------------
        Key4f  = MOD(KeyMis,100)/10
C       if (key4f.ne.0) then
C!--- routine spdetx can make a random choice of two colour recombination
C!--- pattern either WW or ZZ like. It is necessary only for uudd ccss f.s
C        if ( (abs(iflav(1)).eq.1).and.(abs(iflav(4)).eq.1)
C     $  .and.(abs(iflav(2)).eq.2).and.(abs(iflav(3)).eq.2)
C     $      )  call spdetx(ireco)
C        if ( (abs(iflav(1)).eq.3).and.(abs(iflav(4)).eq.3)
C     $  .and.(abs(iflav(2)).eq.4).and.(abs(iflav(3)).eq.4)
C     $      )  call spdetx(ireco)
C       endif
!--------------------
      id2 = idau2
      id1 = idau1

      IF(IRECO.eq.0) then
          if (abs(iflav(1)).lt.10) then
            ijoin(1) = ID1
            ijoin(2) = ID1+1
            call lujoin(2,ijoin)
          endif
         
          if (abs(iflav(3)).lt.10) then
            ijoin(1) = ID2
            ijoin(2) = ID2+1
            call lujoin(2,ijoin)
          endif

C That's it, shower and hadronize now
          SQRS1=P7lu(imo1,5)
          IF (ABS(IFLAV(1)).LT.10) CALL LUSHOW(ID1,ID1+1,SQRS1)
          SQRS2=P7lu(imo2,5)
          IF (ABS(IFLAV(3)).LT.10) CALL LUSHOW(ID2,ID2+1,SQRS2)
C      else
C
      elseif (ireco.gt.0) then
          Iol=MSTJ(14)
          MSTJ(14)=-1
          nold = n7lu
          CALL LUPREP(0)
          MSTJ(14)=Iol
          if (mstu(24).gt.0) call lulist(1)
C        
C compute boost in W+W- system
C
C          print *,'boost for W1 W2',imo1,imo2
          do J=1,3
             BEWW(J)=
     +      (P7lu(Imo1,J)+P7lu(Imo2,J))/(P7lu(Imo1,4)+P7lu(Imo2,4))
          enddo
C          print *,'boost vec',beww
          if (ireco/10.gt.0) then
           CALL LUDBRB(imo1,imo1,0.,0.,-BEWW(1),-BEWW(2),-BEWW(3))
           CALL LUDBRB(Imo2,Imo2,0.,0.,-BEWW(1),-BEWW(2),-BEWW(3))
           CALL LUDBRB(NOLD+1,N7lu,0.,0.,-BEWW(1),-BEWW(2),-BEWW(3))
          endif
C   Offset original partons so that they are not removed by LUEDIT.
C         print *,'original partons before luedit',id1,id1+1,id2,id2+1
C          K7lu(id1,1)=K7lu(id1,1)-20
C          K7lu(id1+1,1)=K7lu(id1+1,1)-20
C          K7lu(id2,1)=K7lu(id2,1)-20
C          K7lu(id2+1,1)=K7lu(id2+1,1)-20
C          call lulist(1)
          CALL LUEDIT(12)
          CALL LUEDIT(14)
C          print *,'after luedit'
C          call lulist(1)
C restore
C          K7lu(id1,1)=K7lu(id1,1)+20
C          K7lu(id1+1,1)=K7lu(id1+1,1)+20
C          K7lu(id2,1)=K7lu(id2,1)+20
C          K7lu(id2+1,1)=K7lu(id2+1,1)+20
C         print *,'after restore'
C          call lulist(1)
C  check how many rows in KWGT
          jkwgt = iw(nakwgt)
          nkwgt = 0
          if (jkwgt.gt.0) nkwgt = lrows(jkwgt) 
          if (mod(ireco,10).eq.1) then 
            mode = 2
            para = 1.E09             
            call reco4(mode,para,res)
C            call reco4_jbh(mode,para,res)
            weik = res
            nkwgt = nkwgt + 1
            ind = kwgtbk(nkwgt,1000,weik)
            if (weik.gt.0.) call hfill(101,weik,dum,1.)
            if (ifir.lt.20) print *,' reco overlap',res
          else if  (mod(ireco,10).eq.2) then
            mode = 1
            call reco5(mode,nreco)
            weik = float(nreco)
            nkwgt = nkwgt + 1
            ind = kwgtbk(nkwgt,2000,weik)
            call hfill(100,weik,dum,1.)
          else if  (mod(ireco,10).eq.3) then
            mode = 2
            call reco5(mode,nreco)
            weik = float(nreco)
            nkwgt = nkwgt + 1
            ind = kwgtbk(nkwgt,2000,weik)
            call hfill(100,weik,dum,1.)
          else if  (mod(ireco,10).eq.4) then
            call recogal(nreco)
            weik = float(nreco)
            nkwgt = nkwgt + 1
            ind = kwgtbk(nkwgt,2000,weik)
            call hfill(100,weik,dum,1.)
            if (ifir.lt.20) print *,' GAL switches ',nreco
          endif
C          call lulist(1)      ! control bb
          if (ireco/10.gt.0) then   ! boost back
           CALL LUDBRB(imo1,imo1,0.,0.,BEWW(1),BEWW(2),BEWW(3))
           CALL LUDBRB(Imo2,Imo2,0.,0.,BEWW(1),BEWW(2),BEWW(3))
           IF(N7lu.GT.NOLD) CALL LUDBRB(NOLD+1,N7lu ,0.,0.,
     +                                  BEWW(1),BEWW(2),BEWW(3))
C          call lulist(1)      ! control bb
          endif
      endif
C      endif
 200   continue
      call luexec
C   now do we have to apply some BE effects ?
      if ( inewbe.gt.0) then
         mstj(51)=mstj51
         nbeam = 0
         nsav = 4
         do it = 1,n7lu
            kf = k7lu(it,2)
            ks = k7lu(it,1)
            kc = lucomp(kf)
            mo = k7lu(it,3)
            if ( ks.eq.21 ) nbeam = nbeam + 1
            call BEFRoZ(KC,KS,KF,mo,nbeam,KSNEW,nsav)
         if ( ksnew.ne.ks ) ks = ksnew
C 5        continue
          k7lu(it,1) = ks
C         K7LU(N7LU,1) = KS
C         K7LU(N7LU,2) = kf
C         K7LU(N7LU,3) = MO
C         if ( mo.gt.0) K7LU(N7LU,3) = MO+nbeam
C         K7LU(N7LU,4) = 0
C         K7LU(N7LU,5) = 0
C         do it=1,5
C           P7LU(N7LU,it) = xp(it)
C         enddo
C         P7LU(N7LU,5) =  am
C         go to 2
C 3       continue
C         print *,' track ',it,' status',ks,' code',kf,'mother', mo
         enddo
C   now restore mother to daughter relation and fix Pi0's status if
C   from tau decays
          do imo = 1,n7lu
            idauf = 10000
            idaul = 0
            itf = min(imo+1,n7lu)
            do it = itf,n7lu
              if ( K7LU(it,3).eq.imo) then
                idauf = min(idauf,it)
                idaul = max(idaul,it)                
              endif
            enddo
            if ( idauf.lt.10000) K7LU(imo,4)= idauf 
            if ( idaul.gt.0)     K7LU(imo,5)= idaul
          enddo
C      print *,' after befroz nsav', nsav
      if (nsav.eq.4) nsav = n7lu   ! there were no quarks to fragment
      if(idebu.ge.1) call lulist(1)
      if (inewbe.gt.0) call bewarm(idebu,nsav)
      mstj(51) = 0
      endif
      RETURN
      END
      SUBROUTINE reco5(MODE,nreco)
C...Subroutine to reconnect strings
C...when they cross (type II superconductor).
C...Input: MODE = 0 : no string reconnection.
C               = 1 : string reconnection.
C               = 2 : string reconnection if string length decreased.
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      COMMON/PYWWCM/ECMWW,PMW,PGW,HBAR,TFRAG,RHAD
      COMMON/PYWWII/IW1,IW2,IQ1,IQ2,IQ3,IQ4,IRR,ALBEF,ALAFT,DCUT1,DCUT2
      DIMENSION XP(3),XM(3),V1P(3),V2P(3),V1M(3),V2M(3)
      DIMENSION INP(50),INM(50),IPC(20),IMC(20),TC(0:20),TPC(20),
     &TMC(20),IJOIN(100),PSUMP(4),PSUMM(4)
C...Function to give four product.
      FOUR(I,J)=P(I,4)*P(J,4)-P(I,1)*P(J,1)-P(I,2)*P(J,2)-P(I,3)*P(J,3)
C     print *,'entering reco5 :', tfrag,hbar,pmw,pgw
      IACC = 0
C...Find position of W's and quarks.
      IW1=0
      IW2=0
      IQ1=0
      IQ2=0
      IQ3=0
      IQ4=0
      DO 100 I=1,n
      IF(K(I,2).EQ.24) IW1=I
      IF(K(I,2).EQ.-24) IW2=I
      IF(K(I,2).GT.0.AND.K(I,2).LT.10.AND.K(K(I,3),2).EQ.24) IQ1=I
      IF(K(I,2).GT.-10.AND.K(I,2).LT.0.AND.K(K(I,3),2).EQ.24) IQ2=I
      IF(K(I,2).GT.0.AND.K(I,2).LT.10.AND.K(K(I,3),2).EQ.-24) IQ3=I
      IF(K(I,2).GT.-10.AND.K(I,2).LT.0.AND.K(K(I,3),2).EQ.-24) IQ4=I
  100 CONTINUE
      IF(MODE.EQ.0) GOTO 250
C...Select decay vertices of W+ and W-.
      TP=HBAR*(-LOG(RLU(0)))*P(IW1,4)/
     &SQRT((P(IW1,5)**2-PMW**2)**2+(P(IW1,5)**2*PGW/PMW)**2)
      TM=HBAR*(-LOG(RLU(0)))*P(IW2,4)/
     &SQRT((P(IW2,5)**2-PMW**2)**2+(P(IW2,5)**2*PGW/PMW)**2)
      DO 110 J=1,3
      XP(J)=TP*P(IW1,J)/P(IW1,4)
      XM(J)=TM*P(IW2,J)/P(IW2,4)
  110 CONTINUE
C...Find partons pointing back to W+ and W-; store them with quark
C...end of string first.
C      print *,'w1,w2,q1-4',iw1,iw2,iq1,iq2,iq3,iq4,tp,tm
      NNP=0
      NNM=0
      ISGP=0
      ISGM=0
      DO 140 I=4,N
      IF(K(I,1).NE.1.AND.K(I,1).NE.2) GOTO 140
C...Do not count photons (added 19/6 1995).
      IF(K(I,2).EQ.22) GOTO 140
      IF(K(I,3).EQ.IW1) THEN
        IF(ISGP.EQ.0) ISGP=ISIGN(1,K(I,2))
        NNP=NNP+1
        IF(ISGP.EQ.1) THEN
          INP(NNP)=I
        ELSE
          DO 120 I1=NNP,2,-1
  120     INP(I1)=INP(I1-1)
          INP(1)=I
        ENDIF
        IF(K(I,1).EQ.1) ISGP=0
      ELSEIF(K(I,3).EQ.IW2) THEN
        IF(ISGM.EQ.0) ISGM=ISIGN(1,K(I,2))
        NNM=NNM+1
        IF(ISGM.EQ.1) THEN
          INM(NNM)=I
        ELSE
          DO 130 I1=NNM,2,-1
  130     INM(I1)=INM(I1-1)
          INM(1)=I
        ENDIF
        IF(K(I,1).EQ.1) ISGM=0
      ENDIF
  140 CONTINUE 
C      print *,'partons from w1',nnp,(inp(iip),iip=1,nnp)
C      print *,'partons from w2',nnm,(inm(iip),iip=1,nnm)
C...Loop through all string pieces, one from W+ and one from W-.
      NPAIR=0
      NCROSS=0
      TC(0)=0.
      DO 200 IIP=1,NNP-1
      IF(K(INP(IIP),2).LT.0) GOTO 200
      I1P=INP(IIP)
      I2P=INP(IIP+1)
      DO 190 IIM=1,NNM-1
      IF(K(INM(IIM),2).LT.0) GOTO 190
      I1M=INM(IIM)
      I2M=INM(IIM+1)
      NPAIR=NPAIR+1
C...Find endpoint velocity vectors and determine crossing.
      DO 150 J=1,3
      V1P(J)=P(I1P,J)/P(I1P,4)
      V2P(J)=P(I2P,J)/P(I2P,4)
      V1M(J)=P(I1M,J)/P(I1M,4)
      V2M(J)=P(I2M,J)/P(I2M,4)
  150 CONTINUE
      CALL PYWWXX(TP,TM,XP,XM,V1P,V2P,V1M,V2M,IANSW,T,TAUP,TAUM)
      
C...Order crossings by time. End loop over crossings.
      IF(IANSW.EQ.1.AND.NCROSS.LT.20) THEN
        NCROSS=NCROSS+1
        DO 160 I1=NCROSS,1,-1
        IF(T.GT.TC(I1-1).OR.I1.EQ.1) THEN
          IPC(I1)=IIP
          IMC(I1)=IIM
          TC(I1)=T
          TPC(I1)=TAUP
          TMC(I1)=TAUM
          GOTO 170
        ELSE
          IPC(I1)=IPC(I1-1)
          IMC(I1)=IMC(I1-1)
          TC(I1)=TC(I1-1)
          TPC(I1)=TPC(I1-1)
          TMC(I1)=TMC(I1-1)
        ENDIF
  160   CONTINUE
  170   CONTINUE
      ENDIF
  190 CONTINUE
  200 CONTINUE
C     print *,' crossins',ncross
C...Loop over crossings; find first (if any) acceptable one.
      IF(NCROSS.GE.1) THEN
        DO 210 IC=1,NCROSS
        PNFRAG=EXP(-(TPC(IC)**2+TMC(IC)**2)/TFRAG**2)
        IF(PNFRAG.GT.RLU(0)) THEN
          IF(MODE.EQ.1) THEN
            IACC=IC
            GOTO 220
          ELSE
            IIP=IPC(IC)
            IIM=IMC(IC)
            I1P=INP(IIP)
            I2P=INP(IIP+1)
            I1M=INM(IIM)
            I2M=INM(IIM+1)
            ELOLD=FOUR(I1P,I2P)*FOUR(I1M,I2M)
            ELNEW=FOUR(I1P,I2M)*FOUR(I1M,I2P)
            IF(ELNEW.LT.ELOLD) THEN
              IACC=IC
              GOTO 220
            ENDIF
          ENDIF
        ENDIF
  210   CONTINUE
  220   CONTINUE
      ENDIF
      CALL PYLAMB(RLEN)
      ALBEF=RLEN
C...Recouple strings.
      IF(IACC.NE.0) THEN
        IIP=IPC(IACC)
        IIM=IMC(IACC)
        NJOIN=0
        DO 230 IS=1,NNP+NNM
        NJOIN=NJOIN+1
        IF(IS.LE.IIP) THEN
          I=INP(IS)
        ELSEIF(IS.LE.IIP+NNM-IIM) THEN
          I=INM(IS-IIP+IIM)
        ELSEIF(IS.LE.IIP+NNM) THEN
          I=INM(IS-IIP-NNM+IIM)
        ELSE
          I=INP(IS-NNM)
        ENDIF
        IJOIN(NJOIN)=I
        IF(K(I,2).LT.0) THEN
C     print *,'partons to join SKII',njoin,(ijoin(j),j=1,njoin)
          CALL LUJOIN(NJOIN,IJOIN)
          NJOIN=0
        ENDIF
  230   CONTINUE
      ENDIF
C...Amount of energy to either side of recoupling.
      DO 330 J=1,4
      PSUMP(J)=0.
  330 PSUMM(J)=0.
      DO 340 IS=1,IIP
      I=INP(IS)
      DO 340 J=1,4
  340 PSUMP(J)=PSUMP(J)+P(I,J)
      DO 350 IS=1,IIM
      I=INM(IS)
      DO 350 J=1,4
  350 PSUMM(J)=PSUMM(J)+P(I,J)
      FRACP=(P(IW1,4)*PSUMP(4)-P(IW1,1)*PSUMP(1)-P(IW1,2)*PSUMP(2)-
     &P(IW1,3)*PSUMP(3))/P(IW1,5)**2
      DCUT1=2.*MAX(FRACP,1.-FRACP)-1.
      FRACM=(P(IW2,4)*PSUMM(4)-P(IW2,1)*PSUMM(1)-P(IW2,2)*PSUMM(2)-
     &P(IW2,3)*PSUMM(3))/P(IW2,5)**2
      DCUT2=2.*MAX(FRACM,1.-FRACM)-1.

C...Do fragmentation.
  250 CALL LUEXEC
      IF(IACC.NE.0) IRR=1
      CALL PYLAMB(RLEN)
      ALAFT=RLEN
      nreco = ncross
      RETURN
      END
      SUBROUTINE PYWWXX(TP,TM,XP,XM,V1P,V2P,V1M,V2M,IANSW,T,TAUP,TAUM)

C...Subroutine to determine if two string pieces cross.
C...P stands for W+ related quantities, M for W- related ones.
C...Input: TP, TM times of creation of string pieces.
C          XP, XM vertices of creation of string pieces.
C          V1P, V2P, V1M, V2M velocity vectors of endpoint partons.
C...Output: IANSW = 0 if not cross; = 1 if do cross; -1 if error.
C           T time of crossing (when they do cross, else 0).
C           TAUP, TAUM invariant times of strings at crossing point.

      DIMENSION XP(3),XM(3),V1P(3),V2P(3),V1M(3),V2M(3)
      DIMENSION Q(4,3),XPP(3),XMM(3)

C...Function to do determinant of three rows of q matrix.
      DETER(I,J,K)=Q(I,1)*Q(J,2)*Q(K,3)-Q(I,1)*Q(K,2)*Q(J,3)+
     &Q(J,1)*Q(K,2)*Q(I,3)-Q(J,1)*Q(I,2)*Q(K,3)+
     &Q(K,1)*Q(I,2)*Q(J,3)-Q(K,1)*Q(J,2)*Q(I,3)

C...Define q matrix and find t.
      DO 100 J=1,3
      Q(1,J)=V2P(J)-V1P(J)
      Q(2,J)=-(V2M(J)-V1M(J))
      Q(3,J)=XP(J)-XM(J)-TP*V1P(J)+TM*V1M(J)
      Q(4,J)=V1P(J)-V1M(J)
  100 CONTINUE
      T=-DETER(1,2,3)/DETER(1,2,4)

C...Find alpha and beta.
      S11=Q(1,1)*(T-TP)
      S12=Q(2,1)*(T-TM)
      S13=Q(3,1)+Q(4,1)*T
      S21=Q(1,2)*(T-TP)
      S22=Q(2,2)*(T-TM)
      S23=Q(3,2)+Q(4,2)*T
      DEN=S11*S22-S12*S21
      ALP=(S12*S23-S22*S13)/DEN
      BET=(S21*S13-S11*S23)/DEN

C...Check if solution acceptable.
      IANSW=1
      IF(T.LT.TP.OR.T.LT.TM) IANSW=0
      IF(ALP.LT.0..OR.ALP.GT.1.) IANSW=0
      IF(BET.LT.0..OR.BET.GT.1.) IANSW=0

C...Find point of crossing and check that not inconsistent.
      DO 110 J=1,3
      XPP(J)=XP(J)+(V1P(J)+ALP*(V2P(J)-V1P(J)))*(T-TP)
      XMM(J)=XM(J)+(V1M(J)+BET*(V2M(J)-V1M(J)))*(T-TM)
  110 CONTINUE
      D2PM=(XPP(1)-XMM(1))**2+(XPP(2)-XMM(2))**2+(XPP(3)-XMM(3))**2
      D2P=XPP(1)**2+XPP(2)**2+XPP(3)**2
      D2M=XMM(1)**2+XMM(2)**2+XMM(3)**2
      IF(D2PM.GT.1E-4*(D2P+D2M)) IANSW=-1

C...Find string eigentimes at crossing.
      IF(IANSW.EQ.1) THEN
        TAUP=SQRT(MAX(0.,(T-TP)**2-(XPP(1)-XP(1))**2-
     &  (XPP(2)-XP(2))**2-(XPP(3)-XP(3))**2))
        TAUM=SQRT(MAX(0.,(T-TM)**2-(XMM(1)-XM(1))**2-
     &  (XMM(2)-XM(2))**2-(XMM(3)-XM(3))**2))
      ELSE
        TAUP=0.
        TAUM=0.
      ENDIF

      RETURN
      END
      subroutine redops(iflag,nz0)
      dimension iflav(4),ijoin(2)
      real*4 p7lu,v7lu
      COMMON/LUJETS/N7lu,K7lu(4000,5),P7lu(4000,5),V7lu(4000,5)
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      idau1 =0
      idau2 = 0
      imo1 =0
      imo2 = 0
      do ip =1,4
         iflav(ip)= 0
      enddo
      do ip =1,n7lu
        if (abs(iflav(1)*iflav(2)*iflav(3)*iflav(4)).gt.0) go to 100
        if (iflag.eq.1) then
C   this is a qqbar event, one look for Z0 daughters
        if(k7lu(ip,2).eq.23) imo1 = ip
        else if (iflag.eq.2) then
C   this is WW or ZZ event , look for two mothers
         if( (k7lu(ip,2).eq.23).and.(imo2.gt.0)) imo1 = ip
         if((k7lu(ip,2).eq.24).or.(k7lu(ip,2).eq.23)) imo2=ip
         if(k7lu(ip,2).eq.-24) imo1=ip
        endif
        if (imo1.gt.0) then 
          if (k7lu(ip,3).eq.imo1.and.idau1.eq.0) then
             iflav(1)=k7lu(ip,2)
             idau1 = ip
          else if (k7lu(ip,3).eq.imo1.and.idau1.ge.1) then
             iflav(2)=k7lu(ip,2)
          endif
        endif
        if (imo2.gt.0) then             
          if (k7lu(ip,3).eq.imo2.and.idau2.eq.0) then
             iflav(3)=k7lu(ip,2) 
             idau2 = ip
          else if (k7lu(ip,3).eq.imo2.and.idau2.ge.1) then
             iflav(4)= k7lu(ip,2)              
          endif
        endif
        enddo
  100   continue
C       print *,' redo ps : flag,imo1,imo2',iflag,imo1,imo2
C       print *,' redo ps : iflav,idau1,idau2 ',iflav,idau1,idau2
C       if (abs(imo1-imo2).gt.1) call lulist(1)
C Switch to 1st random sequence if needed
        am = amarset(0)
!--------------------------

        id2 = idau2
        id1 = idau1
        
      if (abs(iflav(1)).lt.10) then
           ijoin(1) = ID1
           ijoin(2) = ID1+1
           call lujoin(2,ijoin)
           SQRS1=P7lu(imo1,5)
           CALL LUSHOW(ID1,ID1+1,SQRS1)
      endif
      if (imo2.gt.0) then  
         if (abs(iflav(3)).lt.10) then
              ijoin(1) = ID2
              ijoin(2) = ID2+1
              call lujoin(2,ijoin)
              SQRS2=P7lu(imo2,5)
              CALL LUSHOW(ID2,ID2+1,SQRS2)
         endif

        endif
C That's it, shower now
        Iol=MSTJ(14)
        MSTJ(14)=-1
        nold = n7lu
        CALL LUPREP(0)
        MSTJ(14)=Iol
        CALL LUEDIT(12)
        CALL LUEDIT(14)
C          print *,'after luedit'
C          call lulist(1)
      return
      end
      SUBROUTINE recogh(MODE,PARA,res)
      return
      end
      subroutine recogal(nreco)
      nswitch = 0
C      am = amarset(1)
      call pysci(nswitch)
C      am = amarset(0)
      nreco = nswitch
      return
      end
      SUBROUTINE reco4(MODE,PARA,res)
C...Subroutine to generate WW event and reconnect strings
C...according to overlap of cylindrical string piece volumes.
C...Input: MODE = 0 : no reconnection.
C               = 1 : reconnect, instantaneous cylinders.
C               = 2 : reconnect, time-retarded cylinders.
C          PARA : strength of reconnection probability.
      PARAMETER (PI=3.141592)
C   here version with 1000 points instead of 100
CB      PARAMETER (NPT=1000)
      PARAMETER (NPT=100)
      PARAMETER (BLOWR=2.5)
      PARAMETER (BLOWT=2.)
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      COMMON/PYWWCM/ECMWW,PMW,PGW,HBAR,TFRAG,RHAD
      COMMON/PYWWII/IW1,IW2,IQ1,IQ2,IQ3,IQ4,IRR,ALBEF,ALAFT,DCUT1,DCUT2
      DIMENSION XP(3),XM(3),INP(50),INM(50),IJOIN(100),
     &V1(3),V2(3),BETP(50,4),DIRP(50,3),BETM(50,4),DIRM(50,3),
     &XD(4),XB(4),PSUMP(4),PSUMM(4)
      DIMENSION IAP(NPT),IAM(NPT),WTA(NPT)
C      common/colrec/nptcr 
C  event is already in common /LUJETS/
C...Find position of W's and quarks.
C      NPT = nptcr
      IACC = 0
CB      call lulist(1)
      IW1=0
      IW2=0
      IQ1=0
      IQ2=0
      IQ3=0
      IQ4=0
      DO 100 I=1,N
      IF(K(I,2).EQ.24) IW1=I
      IF(K(I,2).EQ.-24) IW2=I
      IF(K(I,2).GT.0.AND.K(I,2).LT.10.AND.K(K(I,3),2).EQ.24) IQ1=I
      IF(K(I,2).GT.-10.AND.K(I,2).LT.0.AND.K(K(I,3),2).EQ.24) IQ2=I
      IF(K(I,2).GT.0.AND.K(I,2).LT.10.AND.K(K(I,3),2).EQ.-24) IQ3=I
      IF(K(I,2).GT.-10.AND.K(I,2).LT.0.AND.K(K(I,3),2).EQ.-24) IQ4=I
  100 CONTINUE
C      print *,'iw1,iw2,iq1,iq2,iq3,iq4',iw1,iw2,iq1,iq2,iq3,iq4
      IF(MODE.EQ.0) GOTO 320
C...Select decay vertices of W+ and W-.
      TP=HBAR*(-LOG(RLU(0)))*P(IW1,4)/
     &SQRT((P(IW1,5)**2-PMW**2)**2+(P(IW1,5)**2*PGW/PMW)**2)
      TM=HBAR*(-LOG(RLU(0)))*P(IW2,4)/
     &SQRT((P(IW2,5)**2-PMW**2)**2+(P(IW2,5)**2*PGW/PMW)**2)
C      print *,'tp tm',iw1,tp,iw2,tm
      DO 110 J=1,3
      XP(J)=TP*P(IW1,J)/P(IW1,4)
      XM(J)=TM*P(IW2,J)/P(IW2,4)
  110 CONTINUE
      GTMAX=MAX(TP,TM)
C...Find partons pointing back to W+ and W-; store them with quark
C...end of string first.
      NNP=0
      NNM=0
      ISGP=0
      ISGM=0
      DO 140 I=1,N
      IF(K(I,1).NE.1.AND.K(I,1).NE.2) GOTO 140
C...Do not count photons (added 19/6 1995).
      IF(K(I,2).EQ.22) GOTO 140
      IF(K(I,3).EQ.IW1) THEN
        IF(ISGP.EQ.0) ISGP=ISIGN(1,K(I,2))
        NNP=NNP+1
        IF(ISGP.EQ.1) THEN
          INP(NNP)=I
        ELSE
          DO 120 I1=NNP,2,-1
  120     INP(I1)=INP(I1-1)
          INP(1)=I
        ENDIF
        IF(K(I,1).EQ.1) ISGP=0
      ELSEIF(K(I,3).EQ.IW2) THEN
        IF(ISGM.EQ.0) ISGM=ISIGN(1,K(I,2))
        NNM=NNM+1
        IF(ISGM.EQ.1) THEN
          INM(NNM)=I
        ELSE
          DO 130 I1=NNM,2,-1
  130     INM(I1)=INM(I1-1)
          INM(1)=I
        ENDIF
        IF(K(I,1).EQ.1) ISGM=0
      ENDIF
  140 CONTINUE
C...Reconstruct velocity and direction of W+ string pieces.
      DO 180 IIP=1,NNP-1
      IF(K(INP(IIP),2).LT.0) GOTO 180
      I1=INP(IIP)
      I2=INP(IIP+1)
      P1A=SQRT(P(I1,1)**2+P(I1,2)**2+P(I1,3)**2)
      P2A=SQRT(P(I2,1)**2+P(I2,2)**2+P(I2,3)**2)
      DO 150 J=1,3
      V1(J)=P(I1,J)/P1A
  150 V2(J)=P(I2,J)/P2A
      DO 160 J=1,3
      BETP(IIP,J)=0.5*(V1(J)+V2(J))
  160 DIRP(IIP,J)=V1(J)-V2(J)
      BETP(IIP,4)=1./SQRT(1.-BETP(IIP,1)**2-BETP(IIP,2)**2-
     &BETP(IIP,3)**2)
      DIRL=SQRT(DIRP(IIP,1)**2+DIRP(IIP,2)**2+DIRP(IIP,3)**2)
      DO 170 J=1,3
  170 DIRP(IIP,J)=DIRP(IIP,J)/DIRL
  180 CONTINUE
C...Reconstruct velocity and direction of W- string pieces.
      DO 230 IIM=1,NNM-1
      IF(K(INM(IIM),2).LT.0) GOTO 230
      I1=INM(IIM)
      I2=INM(IIM+1)
      P1A=SQRT(P(I1,1)**2+P(I1,2)**2+P(I1,3)**2)
      P2A=SQRT(P(I2,1)**2+P(I2,2)**2+P(I2,3)**2)
      DO 190 J=1,3
      V1(J)=P(I1,J)/P1A
  190 V2(J)=P(I2,J)/P2A
      DO 200 J=1,3
      BETM(IIM,J)=0.5*(V1(J)+V2(J))
  200 DIRM(IIM,J)=V1(J)-V2(J)
      BETM(IIM,4)=1./SQRT(1.-BETM(IIM,1)**2-BETM(IIM,2)**2-
     &BETM(IIM,3)**2)
      DIRL=SQRT(DIRM(IIM,1)**2+DIRM(IIM,2)**2+DIRM(IIM,3)**2)
      DO 220 J=1,3
  220 DIRM(IIM,J)=DIRM(IIM,J)/DIRL
  230 CONTINUE
C...Loop over number of space-time points.
      NACC=0
      SUM=0.
      DO 280 IPT=1,NPT

C...Pick x,y,z,t Gaussian (width RHAD and TFRAG, respectively).
      R=SQRT(-LOG(RLU(0)))
      PHI=2.*PI*RLU(0)
      X=BLOWR*RHAD*R*COS(PHI)
      Y=BLOWR*RHAD*R*SIN(PHI)
      R=SQRT(-LOG(RLU(0)))
      PHI=2.*PI*RLU(0)
      Z=BLOWR*RHAD*R*COS(PHI)
      T=GTMAX+BLOWT*SQRT(0.5)*TFRAG*R*ABS(SIN(PHI))
C...Weight for sample distribution.
      WTSMP=EXP(-(X**2+Y**2+Z**2)/(BLOWR*RHAD)**2)*
     &EXP(-2.*(T-GTMAX)**2/(BLOWT*TFRAG)**2)

C...Loop over W+ string pieces and find one with largest weight.
      IMAXP=0
      WTMAXP=1E-10
      XD(1)=X-XP(1)
      XD(2)=Y-XP(2)
      XD(3)=Z-XP(3)
      XD(4)=T-TP
      DO 250 IIP=1,NNP-1
      IF(K(INP(IIP),2).LT.0) GOTO 250
      BED=BETP(IIP,1)*XD(1)+BETP(IIP,2)*XD(2)+BETP(IIP,3)*XD(3)
      BEDG=BETP(IIP,4)*(BETP(IIP,4)*BED/(1.+BETP(IIP,4))-XD(4))
      DO 240 J=1,3
  240 XB(J)=XD(J)+BEDG*BETP(IIP,J)
      XB(4)=BETP(IIP,4)*(XD(4)-BED)
      SR2=XB(1)**2+XB(2)**2+XB(3)**2
      SZ2=(DIRP(IIP,1)*XB(1)+DIRP(IIP,2)*XB(2)+DIRP(IIP,3)*XB(3))**2
      WTP=EXP(-(SR2-SZ2)/(2.*RHAD**2))*EXP(-(XB(4)**2-SZ2)/TFRAG**2)
      IF(MODE.EQ.1.AND.XB(4)-SQRT(SZ2).LT.0.) WTP=0.
      IF(MODE.EQ.2.AND.XB(4)-SQRT(SR2).LT.0.) WTP=0.
      IF(WTP.GT.WTMAXP) THEN
        IMAXP=IIP
        WTMAXP=WTP
      ENDIF
  250 CONTINUE
C...Loop over W- string pieces and find one with largest weight.
      IMAXM=0
      WTMAXM=1E-10
      XD(1)=X-XM(1)
      XD(2)=Y-XM(2)
      XD(3)=Z-XM(3)
      XD(4)=T-TM
      DO 270 IIM=1,NNM-1
      IF(K(INM(IIM),2).LT.0) GOTO 270
      BED=BETM(IIM,1)*XD(1)+BETM(IIM,2)*XD(2)+BETM(IIM,3)*XD(3)
      BEDG=BETM(IIM,4)*(BETM(IIM,4)*BED/(1.+BETM(IIM,4))-XD(4))
      DO 260 J=1,3
  260 XB(J)=XD(J)+BEDG*BETM(IIM,J)
      XB(4)=BETM(IIM,4)*(XD(4)-BED)
      SR2=XB(1)**2+XB(2)**2+XB(3)**2
      SZ2=(DIRM(IIM,1)*XB(1)+DIRM(IIM,2)*XB(2)+DIRM(IIM,3)*XB(3))**2
      WTM=EXP(-(SR2-SZ2)/(2.*RHAD**2))*EXP(-(XB(4)**2-SZ2)/TFRAG**2)
      IF(MODE.EQ.1.AND.XB(4)-SQRT(SZ2).LT.0.) WTM=0.
      IF(MODE.EQ.2.AND.XB(4)-SQRT(SR2).LT.0.) WTM=0.
      IF(WTM.GT.WTMAXM) THEN
        IMAXM=IIM
        WTMAXM=WTM
      ENDIF
  270 CONTINUE

C...Result of integration.
      WT=0.
      IF(IMAXP.NE.0.AND.IMAXM.NE.0) THEN
        WT=WTMAXP*WTMAXM/WTSMP
        SUM=SUM+WT
        NACC=NACC+1
        IAP(NACC)=IMAXP
        IAM(NACC)=IMAXM
        WTA(NACC)=WT
      ENDIF

  280 CONTINUE
      RES=BLOWR**3*BLOWT*SUM/NPT
CBB always reconnect
C...Decide whether to reconnect.
CBB      PREC=1.-EXP(-PARA*RES)
      MREC=0
CBB      IF(PREC.GT.RLU(0)) MREC=1
      if ( res.gt.0.) MREC = 1
      IF(MREC.EQ.0) GOTO 320
C...Decide which pair of strings to reconnect.
      RSUM=RLU(0)*SUM
      DO 290 IA=1,NACC
      IACC=IA
      RSUM=RSUM-WTA(IA)
      IF(RSUM.LE.0.) GOTO 300
  290 CONTINUE
  300 IIP=IAP(IACC)
      IIM=IAM(IACC)
      CALL PYLAMB(RLEN)
      ALBEF=RLEN

C...Recouple strings.
      NJOIN=0
      DO 310 IS=1,NNP+NNM
      NJOIN=NJOIN+1
      IF(IS.LE.IIP) THEN
        I=INP(IS)
      ELSEIF(IS.LE.IIP+NNM-IIM) THEN
        I=INM(IS-IIP+IIM)
      ELSEIF(IS.LE.IIP+NNM) THEN
        I=INM(IS-IIP-NNM+IIM)
      ELSE
        I=INP(IS-NNM)
      ENDIF
      IJOIN(NJOIN)=I
      IF(K(I,2).LT.0) THEN
C     print *,'partons to join SKI',njoin,(ijoin(j),j=1,njoin)
        CALL LUJOIN(NJOIN,IJOIN)
        NJOIN=0
      ENDIF
  310 CONTINUE

C...Amount of energy to either side of recoupling.
      DO 330 J=1,4
      PSUMP(J)=0.
  330 PSUMM(J)=0.
      DO 340 IS=1,IIP
      I=INP(IS)
      DO 340 J=1,4
  340 PSUMP(J)=PSUMP(J)+P(I,J)
      DO 350 IS=1,IIM
      I=INM(IS)
      DO 350 J=1,4
  350 PSUMM(J)=PSUMM(J)+P(I,J)
      FRACP=(P(IW1,4)*PSUMP(4)-P(IW1,1)*PSUMP(1)-P(IW1,2)*PSUMP(2)-
     &P(IW1,3)*PSUMP(3))/P(IW1,5)**2
      DCUT1=2.*MAX(FRACP,1.-FRACP)-1.
      FRACM=(P(IW2,4)*PSUMM(4)-P(IW2,1)*PSUMM(1)-P(IW2,2)*PSUMM(2)-
     &P(IW2,3)*PSUMM(3))/P(IW2,5)**2
      DCUT2=2.*MAX(FRACM,1.-FRACM)-1.
C...Do fragmentation.
  320 CONTINUE
      CALL LUEXEC
      IF(IACC.NE.0) IRR=1
      CALL PYLAMB(RLEN)
      ALAFT=RLEN

      RETURN
      END
      SUBROUTINE PYLAMB(RLEN)

C...Find lambda measure length of string.
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      PARAMETER (PMRHO=0.8)

C...Loop through partons; find where there is a string piece.
      RLEN=0.
      DO 100 I=1,N
      IF(K(I,1).NE.2.AND.K(I,1).NE.12) GOTO 100

C...Calculate string piece mass and add.
      PM2=(P(I,4)+P(I+1,4))**2-(P(I,1)+P(I+1,1))**2-
     &(P(I,2)+P(I+1,2))**2-(P(I,3)+P(I+1,3))**2
      DLEN=LOG(1.+SQRT(MAX(0.,PM2))/PMRHO)
      RLEN=RLEN+DLEN
  100 CONTINUE

      RETURN
      END
      SUBROUTINE TOHEP8(IDEBU,ipsho,ntro)
C----------------------------------------------------------------------
C  fills HEPEVT Real *8 version
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IDEBU,ipsho,ntro
#include "bcs.h"
      REAL*8 EBEAM1,EBEAM2,PBEAM1,PBEAM2
      INTEGER IPROC,MAXEV,IPART1,IPART2
      COMMON/HWPROC/EBEAM1,EBEAM2,PBEAM1,PBEAM2,IPROC,MAXEV
      COMMON/HWBEAM/IPART1,IPART2
C Arrays for particle properties (NMXRES = max no of particles defined)
      REAL*8 RLTIM,RMASS,RSPIN
      INTEGER  NMXRES,ICHRG,IDPDG,IFLAV,NRES
      LOGICAL QORQQB,QBORQQ,VTOCDK,VTORDK
      PARAMETER(NMXRES=500)
      COMMON/HWPROP/RLTIM(0:NMXRES),RMASS(0:NMXRES),RSPIN(0:NMXRES),
     & ICHRG(0:NMXRES),IDPDG(0:NMXRES),IFLAV(0:NMXRES),NRES,
     & VTOCDK(0:NMXRES),VTORDK(0:NMXRES),
     & QORQQB(0:NMXRES),QBORQQ(0:NMXRES)

      INTEGER JKIN,JHIS,KS,MO,LHHIS,HIS
      INTEGER NAMK,NAMV,NAMH,LOUT,LHKIN,LPKIN,ALCODE,PDCODE,KIN
      INTEGER I,J,NBEAM,NPOS,KWP,KWN,NTRK
      integer npos1,npos2,ist
      INTEGER  NAMIND
      EXTERNAL NAMIND
      LOGICAL MAYBE_DONE
C      REAL*4 AM,XP(4)
      REAL*4 AM,XP(5)
      integer km,kf,ier
      REAL*4 BEAM1P(4),BEAM1M,BEAM2P(4),BEAM2M
      INTEGER PDBEAM1,DABEAM1,PDBEAM2
      REAL*4 BEAI1P(4),BEAI2P(4)
      REAL*4 ISRP1(4,10),ISRM1(10)
      REAL*4 ISRP2(4,10),ISRM2(10)
      INTEGER NISR1,NBEAI1,NISR2,NBEAI2,NFSR
      REAL*4 PFSR(4,10),MFSR(10)
      INTEGER MOFSR(10),PDFSR(10)
      REAL*4 CMF(4),dum
      REAL*4 PW1(4),MW1,PW2(4),MW2
      INTEGER PDW1,PDW2,DAW1(2),DAW2(2)
      REAL*4 PFW1(4,2),MFW1(2),PFW2(4,2),MFW2(2),pqq(4,2),mqq(2),pz0(4)
     &      ,mz0
      INTEGER PDFW1(2),MOFW1,NFW1
      INTEGER PDFW2(2),MOFW2,NFW2
      INTEGER PDqq(2),nqq,moqq,pdz0,moz0,iminus,nz0,kz0
      DATA NAMK /0/
C Particle codes for ALEPH to PDG conversion
      INTEGER IPDGAL(367)
      DATA IPDGAL /22,-11,11,0,-13,13,111,211,-211,130,321,-321,2112,
     +           2212,-2212,310,221,3122,3222,3212,3112,             
     +           3322,3312,3334,-2112,-3122,-3222,-3212,-3112,       
     +           -3322,-3312,-3334,-15,15,411,-411,421,-421,         
     +           431,-431,4122,24,-24,23,8*0,
     &            -4122,25,551,311,-311,12,-12,14,-14,16,-16,20213,     
     &-20213,20113,10221,10111,331,10441,20443,445,443,440,441,213,-213,
     &323,-323,313,-313,423,-423,413,-413,433,-433,113,223,333,10551,   
     &20553,555,553,0,661,10661,20663,665,21,2,1,3,4,5,6,-2,-1,-3,-4,-5,
     &-6,663,0,-521,521,511,-511,531,-531,-541,541,-523,523,513,-513,   
     &533,-533,-543,543,621,-621,611,-611,631,-631,641,-641,            
     &651,-651,623,-623,613,-613,                                       
     &633,-633,643,-643,653,-653,2224,-2224,2214,-2214,2114,-2114,1114, 
     &-1114,3224,-3224,3214,-3214,3114,-3114,3324,-3324,3314,-3314,4222,
     &-4222,4212,-4212,4112,-4112,4322,-4322,4312,-4312,4332,-4332,4232,
     &-4232,4132,-4132,4224,-4224,4214,-4214,4114,-4114,4324,-4324,4314,
     &-4314,4334,-4334,4422,-4422,4412,-4412,4432,-4432,4424,-4424,4414,
     &-4414,4434,-4434,4444,-4444,5222,-5222,5212,-5212,5112,-5112,5322,
     &-5322,5312,-5312,5332,-5332,5242,-5242,5142,-5142,5342,-5342,5442,
     &-5442,5522,-5522,5512,-5512,5532,-5532,5542,-5542,6222,-6222,6212,
     &-6212,6112,-6112,6232,-6232,6132,-6132,6332,-6332,6242,-6242,6142,
     &-6142,6342,-6342,6442,-6442,5122,-5122,5232,-5232,5132,-5132,5422,
     &-5422,5412,-5412,5432,-5432,6122,-6122,6322,-6322,6312,-6312,6422,
     &-6422,6412,-6412,6432,-6432,5224,-5224,5214,-5214,5114,-5114,5324,
     &-5324,5314,-5314,5334,-5334,5424,-5424,5414,-5414,5434,-5434,5444,
     &-5444,6224,-6224,6214,-6214,6114,-6114,6324,-6324,6314,-6314,6334,
     &-6334,6424,-6424,6414,-6414,6434,-6434,6444,-6444,6524,-6524,6514,
     &-6514,6534,-6534,6544,-6544,6554,-6554,6252,-6252,6152,-6152,6352,
     &-6352,6452,-6452,6552,-6552,6522,-6522,6512,-6512,6532,-6532,6542,
     &-6542,5524,-5524,5514,-5514,5534,-5534,5544,-5544,5554,-5554,0,   
     &0,5*0/
#include "bmacrod.h"
#include "bmacro.h"
C
C      IF (NAMK .EQ. 0) THEN
         LOUT = IW(6)
C      ENDIF
C      IF (IW(NAMK).EQ.0 .OR. IW(NAMV).EQ.0 .OR. IW(NAMH).EQ.0) THEN
C        WRITE (LOUT,1000)
C 1000   FORMAT(/1X,'+++PRKINE+++ NO KINE/VERT/KHIS bank-RETURN')
C        RETURN
C      ENDIF
      IF(IDEBU.GE.1)
     + WRITE ( LOUT,1010 )
 1010   FORMAT(/1X,'+++PRKINE+++ kinematics banks '/T3,
     & 'Number',2X,'Particle',12X,'Px',10X,'Py',10X,'Pz',10X,'Energy',
     & 'Mass   Mother    Status')
      NBEAM = 0
      NPOS  = 3
      NISR1 = 0
      NISR2 = 0
      NBEAI1= 0
      NBEAI2= 0
      NFW1  = 0
      NFW2  = 0
      NFSR = 0
      nz0 = 0
      nqq =0
      iminus = 0
      KWP = -1
      KWN = -1
      kz0 = -1
      DO I = 1,4
        CMF(I) = 0.
      ENDDO
      NTRK = 0
C  fill beam  particle in case they were not stored on POT for example
C
      do I = 1,2
         beam1p(i)= 0.D0
         BEAM2P(i)= 0.D0
         BEAI1P(i)= 0.D0
         BEAI2P(i)= 0.D0
      enddo
         beam1p(3)= pbeam1 
         beam2p(3)= -pbeam1      
         BEAI1P(3)= pbeam1 
         BEAI2P(3)= -pbeam1
         beam1p(4)= ebeam1 
         beam2p(4)= ebeam2      
         BEAI1P(4)= ebeam1 
         BEAI2P(4)= ebeam2 
         PDBEAM1 = IPDGAL(3)
         PDBEAM2 = IPDGAL(2)
         BEAM1M = RMASS(IPART1)
         BEAM2M = RMASS(IPART2)
 2    call getpart(xp,ks,kf,km,ier)      
C    KINE with NR<0 are beam electrons
C     print *, ier,kf,ks,km
      if ( ier.lt.0 .and. NTRK.gt.2) go to 3
      alcode = kf
      mo = km
      NTRK = NTRK + 1
C     print * ,' done with getpart',ntrk
C extract KINE info
         PDCODE = 99999
         IF(ALCODE.LT.368) PDCODE= IPDGAL(ALCODE)
C clear our the loop once it looks like everything necessary is found
         IF(MAYBE_DONE.AND.ABS(PDCODE).GT.25) GO TO 3
         IF(NTRK-nbeam.gt.ntro) go to 3
         IF(IDEBU.GE.1)
     +    WRITE (LOUT,1012) NTRK,PDCODE,xp,mo,ks
C         IF(NTRK.LT.0) THEN

         if (ks.eq.21) THEN
C beams
           NBEAM = NBEAM + 1
           IF(NBEAM.LE.2) THEN
             IF(PDCODE.EQ.11) THEN
               CALL UCOPY(XP,BEAM1P,4)
               CALL UCOPY(XP,BEAI1P,4)
C               BEAM1M = AM
               BEAM1M = xp(5)
               PDBEAM1=PDCODE
             ELSEIF(PDCODE.EQ.-11) THEN
               CALL UCOPY(XP,BEAM2P,4)
               CALL UCOPY(XP,BEAI2P,4)
C               BEAM2M = AM
               BEAM2M = xp(5)
               PDBEAM2=PDCODE
             ELSE
               WRITE(LOUT,*) 'TOHEP warning, unknown beam',PDCODE
             ENDIF
           ELSE
             WRITE(LOUT,*) 'TOHEP warning, too many beams',nbeam
           ENDIF
         ELSEIF(MO.EQ.0.AND.PDCODE.EQ.22) THEN
C ISR photons
           IF(XP(3).GT.0.) THEN
             NISR1 = NISR1 + 1
             NBEAI1= 1
             CALL UCOPY(XP,ISRP1(1,NISR1),4)
C             ISRM1(NISR1) = AM
             ISRM1(NISR1) = xp(5)
             DO I=1,4
               BEAI1P(I) = BEAI1P(I) - XP(I)
             ENDDO             
           ELSE
             NISR2 = NISR2 + 1
             NBEAI2= 1
             CALL UCOPY(XP,ISRP2(1,NISR2),4)
C             ISRM2(NISR2) = AM
             ISRM2(NISR2) = xp(5)
             DO I=1,4
               BEAI2P(I) = BEAI2P(I) - XP(I)
             ENDDO      
           ENDIF             
         ELSEIF(PDCODE.EQ.24.AND.MO.EQ.0) THEN
C W+
           DO I = 1,4
             CMF(I) = CMF(I) + XP(I)
           ENDDO
           KWP = NTRK
           CALL UCOPY(XP,PW2,4)
C           MW2 = AM
           MW2 = xp(5)
           PDW2= PDCODE
         ELSEIF(PDCODE.EQ.-24.AND.MO.EQ.0) THEN
C W-
           DO I = 1,4
             CMF(I) = CMF(I) + XP(I)
           ENDDO
           CALL UCOPY(XP,PW1,4)
C           MW1 = AM
           MW1 = xp(5)
           PDW1= PDCODE
           KWN = NTRK
C  Z0  in case of some qqbar generators ...
         ELSEIF(PDCODE.EQ.23.AND.MO.EQ.0) THEN
            if ( nz0.eq.0) then
            CALL UCOPY(XP,Pz0(1),4)
            mz0 = xp(5)
            nz0 = nz0 +1
            kz0 = ntrk
            elseif ( nz0.eq.1) then
C  if we are getting a second Z , it must be a ZZ...fill it as a WW
              DO I = 1,4
                CMF(I) = CMF(I) + pz0(I)
              ENDDO
              KWP = kz0
              CALL UCOPY(Pz0,PW2,4)
              MW2 = mz0
              PDW2= PDCODE
              DO I = 1,4
                  CMF(I) = CMF(I) + XP(I)
              ENDDO
              CALL UCOPY(XP,PW1,4)
              MW1 = xp(5)
              PDW1= PDCODE
              KWN = NTRK
              nz0 = nz0 +1
            endif
         ELSEIF(ABS(PDCODE).LE.16) THEN
C fermions
           IF(MO.EQ.KWP-nbeam.and.(nz0.eq.0 .or. nz0.eq.2)) then
             NFW2 = NFW2 + 1
             CALL UCOPY(XP,PFW2(1,NFW2),4)
C             MFW2(NFW2) = AM
             MFW2(NFW2) = xp(5)
             PDFW2(NFW2) = PDCODE
           ELSEIF(MO.EQ.KWN-nbeam.and.(nz0.eq.0 .or. nz0.eq.2)) THEN 
             NFW1 = NFW1 + 1
             CALL UCOPY(XP,PFW1(1,NFW1),4)
C             MFW1(NFW1) = AM
             MFW1(NFW1) = xp(5)
             PDFW1(NFW1) = PDCODE
           ELSEIF(MO.le.nbeam.or.nz0.eq.1) then !    here are qqbar events
             nqq = nqq +1
            if (nqq.le.2) then
             CALL UCOPY(XP,pqq(1,nqq),4)
             mqq(nqq) = xp(5)
             pdqq(nqq) = pdcode
            else if ( nqq.gt.2) then
C check if this is not a second copy of same quarks ...
              if ((xp(1).eq.pqq(1,nqq-2)).and.
     &            (xp(2).eq.pqq(2,nqq-2)).and.
     &            (xp(3).eq.pqq(3,nqq-2))) then
C do not copy this quark
                    iminus = iminus + 1
              endif
            endif
           ENDIF
         ELSEIF(PDCODE.EQ.22.AND.
     &         (MO.EQ.KWP-nbeam.OR.MO.EQ.KWN-nbeam)) then
C FSR primarily off leptons (from PHOTOS)
           nfsr = nfsr + 1
           CALL UCOPY(XP,PFSR(1,NFSR),4)
C           MFSR(NFSR) = AM
           MFSR(NFSR) = xp(5)
           PDFSR(NFSR) = PDCODE
           MOFSR(NFSR) = 1
           if(mo.eq.kwp-nbeam) mofsr(nfsr) = 2
         ENDIF
         if (NFW1.EQ.0.AND.NFW2.EQ.0.and.nqq.eq.4) nqq = nqq -iminus
         MAYBE_DONE = (NFW1.EQ.2.AND.NFW2.EQ.2).or.
     &                (NFW1.EQ.0.AND.NFW2.EQ.0.and.nqq.eq.2)
         GO TO 2
C      ENDIF
C FILL IN /HEPEVT/---------------------------------------------------
 3    CONTINUE
CB      if(NFSR.gt.0) WRITE(LOUT,*)
CB     +       'event analysis Nisr1,nISR2,Nfsr',Nisr1,nISR2,Nfsr
CB     + , (mofsr(i),i=1,nfsr)
C beams
      DABEAM1 = 4 + NISR1 + NISR2 + NBEAI1 + NBEAI2
      CALL FILHEP8(1,101,PDBEAM1,0,0,DABEAM1,DABEAM1+1,
     + BEAM1P,BEAM1M,.TRUE.)
      CALL FILHEP8(2,102,PDBEAM2,0,0,DABEAM1+1,DABEAM1,
     + BEAM2P,BEAM2M,.TRUE.)
C CMF
      if ( nqq.eq.2.or .nz0.eq.0) then 
           DO I = 1,4
             CMF(I) = beam1p(i)+beam2p(i)
             if (nz0.eq.0) pz0(i) = pqq(i,1)+pqq(i,2)
           ENDDO 
           if (nz0.eq.0) mz0 = 
     &         sqrt(pz0(4)**2- pz0(1)**2-  pz0(2)**2-  pz0(3)**2)
      endif
      AM=SQRT(CMF(4)**2-CMF(1)**2-CMF(2)**2-CMF(3)**2)
      CALL FILHEP8(3,103,0,1,2,0,0,CMF,AM,.TRUE.)
      NPOS = 3
C ISR
      IF(NISR1.GT.0) THEN
        NPOS = NPOS + 1
        CALL FILHEP8(NPOS,3,PDBEAM1,1,0,0,0,BEAI1P,BEAM1M,.TRUE.)
        DO I=1,NISR1
          NPOS = NPOS + 1
          CALL FILHEP8(NPOS,1,22,1,0,0,0,ISRP1(1,I),0.,.TRUE.)
        ENDDO
      ENDIF
C      WRITE(LOUT,*) ' ISR1 done'
      IF(NISR2.GT.0) THEN
        NPOS = NPOS + 1
        CALL FILHEP8(NPOS,3,PDBEAM2,2,0,0,0,BEAI2P,BEAM2M,.TRUE.)
        DO I=1,NISR2
          NPOS = NPOS + 1
          CALL FILHEP8(NPOS,1,22,2,0,0,0,ISRP2(1,I),0.,.TRUE.)
        ENDDO
      ENDIF
C      WRITE(LOUT,*) ' ISR2 done'
C Ws
      If( nfw1.eq.2 .and. nfw2.eq.2) then
      NPOS = NPOS + 1
      MOFW1 = NPOS
      CALL FILHEP8(NPOS,110,PDW1,1,2,NPOS+2,NPOS+3,PW1,MW1,.TRUE.)
      NPOS = NPOS + 1
      MOFW2 = NPOS
      CALL FILHEP8(NPOS,110,PDW2,1,2,NPOS+3,NPOS+4,PW2,MW2,.TRUE.)
C      WRITE(LOUT,*) ' Ws done'
C FERMIONS
C colour reconnection done here via the 2nd daughter
      NPOS = NPOS + 1
      npos1 = npos
      CALL FILHEP8(NPOS,113,PDFW1(1),MOFW1,NPOS+1,0,NPOS+1,
     + PFW1(1,1),MFW1(1),.TRUE.)
      NPOS = NPOS + 1
      npos2 = npos
      CALL FILHEP8(NPOS,114,PDFW1(2),MOFW1,NPOS-1,0,NPOS-1,
     + PFW1(1,2),MFW1(2),.TRUE.)
C FSR from W1
      if (nfsr.gt.0) then
      DO I=1,NFSR
        mo = mofw1
         if(mofsr(i).eq.2) mo = mofw2
CB               WRITE(LOUT,*) ' FSR mo', mofsr(i),mo
        if ( mo.ne.mofw1) go to 123 
        NPOS = NPOS + 1
CB        print *, '  FSR found for W1 stored in ',npos
        CALL FILHEP8(NPOS,1,22,MO,0,0,0,PFSR(1,I),0.,.TRUE.)
C  if leptons....make them stable
        ist =1
        if(abs(PDFW1(1)).eq.15) ist = 190
          CALL FILHEP8(NPOS1,IST,PDFW1(1),MOFW1,0,0,0,
     +    PFW1(1,1),MFW1(1),.TRUE.)
        ist =1
        if(abs(PDFW1(2)).Eq.15) ist=190
           CALL FILHEP8(NPOS2,ist,PDFW1(2),MOFW1,0,0,0,
     +     PFW1(1,2),MFW1(2),.TRUE.)
 123    continue 
      ENDDO
      call fildau(mofw1,npos1,npos)
CB      if (idebu.ge.1) call hwuepr
      endif
      NPOS = NPOS + 1
      npos1 = npos
      CALL FILHEP8(NPOS,113,PDFW2(1),MOFW2,NPOS+1,0,NPOS+1,
     + PFW2(1,1),MFW2(1),.TRUE.)
      NPOS = NPOS + 1
      npos2 = npos
      CALL FILHEP8(NPOS,114,PDFW2(2),MOFW2,NPOS-1,0,NPOS-1,
     + PFW2(1,2),MFW2(2),.TRUE.)
C FSR from W2
      if (nfsr.gt.0) then
      DO I=1,NFSR
        mo = mofw1
         if(mofsr(i).eq.2) mo = mofw2
        if ( mo.ne.mofw2) go to 124
        NPOS = NPOS + 1
CB        print *, '  FSR found for W2 stored in ',npos
        CALL FILHEP8(NPOS,1,22,MO,0,0,0,PFSR(1,I),0.,.TRUE.)
C  if leptons....make them stable
        ist =1
        if(abs(PDFW2(1)).eq.15) ist=190
        CALL FILHEP8(NPOS1,ist,PDFW2(1),MOFW2,0,0,0,
     + PFW2(1,1),MFW2(1),.TRUE.)
        ist =1
        if(abs(PDFW2(2)).eq.15) ist=190 
       CALL FILHEP8(NPOS2,ist,PDFW2(2),MOFW2,0,0,0,
     + PFW2(1,2),MFW2(2),.TRUE.)
 124    continue
      ENDDO
      endif
       call fildau(mofw2,npos1,npos)
       if (nz0.eq.0) iproc = 200
       if (nz0.eq.2) iproc = 250
C print /HEPEVT/ if required
CBB       if(nfsr.gt.0) call hwuepr
      else   ! qqbar
        if (nz0.eq.1) iproc = 100
        NPOS = NPOS + 1
        moz0 = npos
        pdz0 = 23
C fill Z0
        CALL FILHEP8(NPOS,110,PDz0,1,2,NPOS+1,NPOS+2,
     +                                  Pz0(1),Mz0,.TRUE.)
        NPOS = NPOS + 1
        CALL FILHEP8(NPOS,113,PDqq(1),MOz0,NPOS+1,0,NPOS+1,
     +                                  Pqq(1,1),Mqq(1),.TRUE.)
        NPOS = NPOS + 1
        CALL FILHEP8(NPOS,114,PDqq(2),MOz0,NPOS-1,0,NPOS-1,
     +                                  Pqq(1,2),Mqq(2),.TRUE.)
   
      endif
      if(idebu.ge.1) call hwuepr
      call hfill(300,float(iproc),dum,1.)
1012  FORMAT(T3,I4,4X,I8,4X,5(F12.3),4X,I5,4X,I5)
      RETURN
      END

      SUBROUTINE TOHAD6(ierr) 
      implicit none
C BBL - modified to interface with herwig 6.2
C
      integer IDEBB,IDEBE,ierr,IDEBL 
      COMMON / DTBUG / IDEBB,IDEBE,IDEBL
C          ****COMMON BLOCK FILE FOR HERWIG VERSION 6.2****
C
C ALTERATIONS: Layout completely overhauled for 5.9
C
C
C New common blocks added for version 6.1:
C              HWCLUS,HWSUSY,HWRPAR,HWMINB
C
C New variables added for version 6.1:
C              OMHMIX,ET2MIX,PH3MIX,IOP4JT,NPRFMT,
C              PRNDEF,PRNTEX,PRNWEB,EFFMIN,GCUTME,
C              IOP4JT,NPRFMT                       see HWPRAM
C              Y4JT,DURHAM                         see HWHARD
C              QORQQB,QBORQQ                       see HWPROP
C              NRECO                               see HWUCLU
C              TXNAME                              see HWUNAM
C              PPCL,NCL,IDCL                       see HWCLUS
C              TANB,ALPHAH,COSBPA,SINBPA,COSBMA,
C              SINBMA,COSA,SINA,COSB,SINB,COTB,
C              ZMIXSS,ZMXNSS,ZSGNSS,LFCH,RFCH,
C              SLFCH,SRFCH,WMXUSS,WMXVSS,WSGNSS,
C              QMIXSS,LMIXSS,THETAT,THETAB,THETAL,
C              ATSS,ABSS,ALSS,MUSS,FACTSS,GHWWSS,
C              GHZZSS,GHDDSS,GHUUSS,GHWHSS,GHSQSS,
C              XLMNSS,RMMNSS,SENHNC,SUSYIN         see HWSUSY
C              LAMDA1,LAMDA2,LAMDA3,HRDCOL,RPARTY,
C              COLUPD                              see HWRPAR
C              PMBN1,PMBN2,PMBN3,PMBK1,PMBK2,
C              PMBM1,PMBM2,PMBP1,PMBP2,PMBP3       see HWMINB
C
C New parameters added for version 6.1:
C              NMXCL
C
C Parameter NMXRES raised to 500
C
C Scalar variables changed to arrays of size 2:
C              CLSMR,PSPLT,CLDIR                   see HWPRAM
C
C
C   NEW for HERWIG6.2
C
C New common blocks added for version 6.2:
C	       HWGRAV 				   see HWHGRV
C	       
C New variables added for version 6.2:
C              FIX4JT                              see HWIGIN
C
C New parameters added for version 6.2:
C	       DXRCYL,DXZMAX,DXRSPH	           see HWIGIN	
C	       GRVLAM,EMGRV,GAMGRV		   see HWHGRV
C
C New control flags added for version 6.2:
C              WZRFR                               see HWBJCO
C              IMSSM,IHIGGS                        see HWUINC
C
C New variable added for version 6.2:
C              PARITY                              see HWUINC
C
      DOUBLE PRECISION ZERO,ONE,TWO,THREE,FOUR,HALF
      PARAMETER (ZERO =0.D0, ONE =1.D0, TWO =2.D0,
     &           THREE=3.D0, FOUR=4.D0, HALF=0.5D0)
C
      DOUBLE PRECISION
     & ACCUR,AFCH,ALPFAC,ALPHEM,ANOMSC,ASFIXD,AVWGT,B1LIM,BETAF,BRFRAC,
     & BRHIG,BTCLM,CAFAC,CFFAC,CLDKWT,CLMAX,CLPOW,CLQ,CLSMR,CMMOM,COSS,
     & COSTH,CSPEED,CTHRPW,CTMAX,DECPAR,DECWT,DISF,DKLTM,EBEAM1,EBEAM2,
     & EMLST,EMMAX,EMMIN,EMPOW,EMSCA,ENHANC,ENSOF,EPOLN,ETAMIX,EVWGT,
     & EXAG,F0MIX,F1MIX,F2MIX,GAMH,GAMMAX,GAMW,GAMWT,GAMZ,GAMZP,GCOEF,
     & GEV2NB,GEV2MM,GPOLN,H1MIX,HBAR,HARDST,OMEGA0,PBEAM1,PBEAM2,PDIQK,
     & PGSMX,PGSPL,PHEP,PHIMIX,PHIPAR,PHOMAS,PIFAC,PLTCUT,PPAR,PPOLN,
     & PRECO,PRSOF,PSPLT,PTINT,PTMAX,PTMIN,PTPOW,PTRMS,PXRMS,PWT,Q2MAX,
     & Q2MIN,Q2POW,Q2WWMN,Q2WWMX,QCDL3,QCDL5,QCDLAM,QDIQK,QEV,QFCH,QG,
     & QLIM,QSPAC,QV,QWT,REPWT,RESN,RHOHEP,RHOPAR,RLTIM,RMASS,RMIN,
     & RSPIN,SCABI,SINS,SNGWT,SWEIN,SWTEF,SUD,THMAX,TLOUT,TMTOP,TMNISR,
     & TQWT,VCKM,VFCH,VGCUT,VHEP,VMIN2,VPAR,VPCUT,VQCUT,VTXPIP,VTXQDK,
     & WBIGST,WGTMAX,WGTSUM,WHMIN,WSQSUM,XFACT,XLMIN,XMIX,XMRCT,XX,
     & XXMIN,YBMAX,YBMIN,YJMAX,YJMIN,YMIX,YMRCT,YWWMAX,YWWMIN,ZBINM,
     & ZJMAX,ZMXISR,Y4JT,EFFMIN,PPCL,
     & TANB,ALPHAH,COSBPA,SINBPA,COSBMA,SINBMA,COSA,SINA,COSB,SINB,COTB,
     & ZMIXSS,ZMXNSS,ZSGNSS,LFCH,RFCH,SLFCH,SRFCH, WMXUSS,WMXVSS,WSGNSS,
     & QMIXSS,LMIXSS,THETAT,THETAB,THETAL,ATSS,ABSS,ALSS,MUSS,FACTSS,
     & GHWWSS,GHZZSS,GHDDSS,GHUUSS,GHWHSS,GHSQSS,
     & XLMNSS,RMMNSS,SENHNC,LAMDA1,LAMDA2,LAMDA3,
     & PMBN1,PMBN2,PMBN3,PMBK1,PMBK2,PMBM1,PMBM2,PMBP1,PMBP2,PMBP3,
     & OMHMIX,ET2MIX,PH3MIX,GCUTME,GRVLAM,EMGRV,GAMGRV,DXRCYL,DXRSPH,
     & DXZMAX,VIPWID
C
      INTEGER
     & CLDIR,IAPHIG,IBRN,IBSH,ICHRG,ICO,IDCMF,IDHEP,IDHW,IDK,IDKPRD,IDN,
     & IDPAR,IDPDG,IERROR,IFLAV,IFLMAX,IFLMIN,IHPRO,IMQDK,INHAD,INTER,
     & IOPDKL,IOPHIG,IOPREM,IPART1,IPART2,IPRINT,IPRO,IPROC,ISLENT,
     & ISPAC,ISTAT,ISTHEP,ISTPAR,JCOPAR,JDAHEP,JDAPAR,JMOHEP,JMOPAR,
     & JNHAD,LNEXT,LOCN,LOCQ,LRSUD,LSTRT,LWEVT,LWSUD,MAPQ,MAXER,MAXEV,
     & MAXFL,MAXPR,MODBOS,MODMAX,MODPDF,NBTRY,NCLDK,NCOLO,NCTRY,NDKYS,
     & NDTRY,NETRY,NEVHEP,NEVPAR,NFLAV,NGSPL,NHEP,NME,NMODES,NMXCDK,
     & NMXDKS,NMXHEP,NMXJET,NMXMOD,NMXPAR,NMXQDK,NMXRES,NMXSUD,NPAR,
     & NPRODS,NQDK,NQEV,NRES,NRN,NSPAC,NSTRU,NSTRY,NSUD,NUMER,NUMERU,
     & NWGTS,NZBIN,SUDORD,IOP4JT,HRDCOL,NMXCL,NCL,IDCL,NPRFMT,NRECO,
     & IMSSM,IHIGGS,PARITY,LRSUSY
C
      LOGICAL
     & AZSOFT,AZSPIN,BGSHAT,BREIT,CLRECO,COLISR,DKPSET,FROST,FSTEVT,
     & FSTWGT,GENEV,GENSOF,HARDME,HVFCEN,MAXDKL,MIXING,NOSPAC,NOWGT,
     & PRNDEC,PIPSMR,PRVTX,RSTAB,SOFTME,TMPAR,TPOL,USECMF,VTOCDK,VTORDK,
     & ZPRIME,RPARTY,COLUPD,PRNDEF,PRNTEX,PRNWEB,DURHAM,SUSYIN,WZRFR,
     & QORQQB,QBORQQ,FIX4JT
C
      CHARACTER*4
     & BDECAY
      CHARACTER*8
     & PART1,PART2,RNAME
      CHARACTER*20
     & AUTPDF
      CHARACTER*37
     & TXNAME
C
C New standard event common
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     & JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
C
C Beams, process and number of events
      COMMON/HWBEAM/IPART1,IPART2
      COMMON/HWBMCH/PART1,PART2
      COMMON/HWPROC/EBEAM1,EBEAM2,PBEAM1,PBEAM2,IPROC,MAXEV
C
C Basic parameters (and quantities derived from them)
      COMMON/HWPRAM/AFCH(16,2),ALPHEM,B1LIM,BETAF,BTCLM,CAFAC,CFFAC,
     & CLMAX,CLPOW,CLSMR(2),CSPEED,ENSOF,ETAMIX,F0MIX,F1MIX,F2MIX,GAMH,
     & GAMW,GAMZ,GAMZP,GEV2NB,H1MIX,PDIQK,PGSMX,PGSPL(4),PHIMIX,PIFAC,
     & PRSOF,PSPLT(2),PTRMS,PXRMS,QCDL3,QCDL5,QCDLAM,QDIQK,QFCH(16),QG,
     & QSPAC,QV,SCABI,SWEIN,TMTOP,VFCH(16,2),VCKM(3,3),VGCUT,VQCUT,
     & VPCUT,ZBINM,EFFMIN,OMHMIX,ET2MIX,PH3MIX,GCUTME,
     & IOPREM,IPRINT,ISPAC,LRSUD,LWSUD,MODPDF(2),NBTRY,NCOLO,NCTRY,
     & NDTRY,NETRY,NFLAV,NGSPL,NSTRU,NSTRY,NZBIN,IOP4JT(2),NPRFMT,
     & AZSOFT,AZSPIN,CLDIR(2),HARDME,NOSPAC,PRNDEC,PRVTX,SOFTME,ZPRIME,
     & PRNDEF,PRNTEX,PRNWEB
C
      COMMON/HWPRCH/AUTPDF(2),BDECAY
C
C Parton shower common (same format as /HEPEVT/)
      PARAMETER (NMXPAR=500)
      COMMON/HWPART/NEVPAR,NPAR,ISTPAR(NMXPAR),IDPAR(NMXPAR),
     & JMOPAR(2,NMXPAR),JDAPAR(2,NMXPAR),PPAR(5,NMXPAR),VPAR(4,NMXPAR)
C
C Parton polarization common
      COMMON/HWPARP/DECPAR(2,NMXPAR),PHIPAR(2,NMXPAR),RHOPAR(2,NMXPAR),
     & TMPAR(NMXPAR)
C
C Electroweak boson common
      PARAMETER (MODMAX=5)
      COMMON/HWBOSC/ALPFAC,BRHIG(12),ENHANC(12),GAMMAX,RHOHEP(3,NMXHEP),
     & IOPHIG,MODBOS(MODMAX)
C
C Parton colour common
      COMMON/HWPARC/JCOPAR(4,NMXPAR)
C
C other HERWIG branching, event and hard subprocess common blocks
      COMMON/HWBRCH/ANOMSC(2,2),HARDST,PTINT(3,2),XFACT,INHAD,JNHAD,
     & NSPAC(7),ISLENT,BREIT,FROST,USECMF,WZRFR
C
      COMMON/HWEVNT/AVWGT,EVWGT,GAMWT,TLOUT,WBIGST,WGTMAX,WGTSUM,WSQSUM,
     & IDHW(NMXHEP),IERROR,ISTAT,LWEVT,MAXER,MAXPR,NOWGT,NRN(2),NUMER,
     & NUMERU,NWGTS,GENSOF
C
      COMMON/HWHARD/ASFIXD,CLQ(7,6),COSS,COSTH,CTMAX,DISF(13,2),EMLST,
     & EMMAX,EMMIN,EMPOW,EMSCA,EPOLN(3),GCOEF(7),GPOLN,OMEGA0,PHOMAS,
     & PPOLN(3),PTMAX,PTMIN,PTPOW,Q2MAX,Q2MIN,Q2POW,Q2WWMN,Q2WWMX,QLIM,
     & SINS,THMAX,Y4JT,TMNISR,TQWT,XX(2),XLMIN,XXMIN,YBMAX,YBMIN,YJMAX,
     & YJMIN,YWWMAX,YWWMIN,WHMIN,ZJMAX,ZMXISR,IAPHIG,IBRN(2),IBSH,
     & ICO(10),IDCMF,IDN(10),IFLMAX,IFLMIN,IHPRO,IPRO,MAPQ(6),MAXFL,
     & BGSHAT,COLISR,FSTEVT,FSTWGT,GENEV,HVFCEN,TPOL,DURHAM,FIX4JT
C
C Arrays for particle properties (NMXRES = max no of particles defined)
      PARAMETER(NMXRES=500)
      COMMON/HWPROP/RLTIM(0:NMXRES),RMASS(0:NMXRES),RSPIN(0:NMXRES),
     & ICHRG(0:NMXRES),IDPDG(0:NMXRES),IFLAV(0:NMXRES),NRES,
     & VTOCDK(0:NMXRES),VTORDK(0:NMXRES),
     & QORQQB(0:NMXRES),QBORQQ(0:NMXRES)
C
      COMMON/HWUNAM/RNAME(0:NMXRES),TXNAME(2,0:NMXRES)
C
C Arrays for particle decays (NMXDKS = max total no of decays,
C                             NMXMOD = max no of modes for a particle)
      PARAMETER(NMXDKS=4000,NMXMOD=200)
      COMMON/HWUPDT/BRFRAC(NMXDKS),CMMOM(NMXDKS),DKLTM(NMXRES),
     & IDK(NMXDKS),IDKPRD(5,NMXDKS),LNEXT(NMXDKS),LSTRT(NMXRES),NDKYS,
     & NME(NMXDKS),NMODES(NMXRES),NPRODS(NMXDKS),DKPSET,RSTAB(0:NMXRES)
C
C Weights used in cluster decays
      COMMON/HWUWTS/REPWT(0:3,0:4,0:4),SNGWT,DECWT,QWT(3),PWT(12),
     & SWTEF(NMXRES)
C
C Parameters for cluster decays (NMXCDK = max total no of cluster
C                                         decay channels)
      PARAMETER(NMXCDK=4000)
      COMMON/HWUCLU/CLDKWT(NMXCDK),CTHRPW(12,12),PRECO,RESN(12,12),
     & RMIN(12,12),LOCN(12,12),NCLDK(NMXCDK),NRECO,CLRECO
C
C Variables controling mixing and vertex information
      COMMON/HWDIST/EXAG,GEV2MM,HBAR,PLTCUT,VMIN2,VTXPIP(4),XMIX(2),
     & XMRCT(2),YMIX(2),YMRCT(2),DXRCYL,DXRSPH,DXZMAX,VIPWID(3),IOPDKL,
     & MAXDKL,MIXING,PIPSMR
C
C Arrays for temporarily storing heavy-b,c-hadrons decaying partonicaly
C (NMXQDK = max no such decays in an event)
      PARAMETER (NMXQDK=20)
      COMMON/HWQDKS/VTXQDK(4,NMXQDK),IMQDK(NMXQDK),LOCQ(NMXQDK),NQDK
C
C Parameters for Sudakov form factors
C (NMXSUD= max no of entries in lookup table)
      PARAMETER (NMXSUD=1024)
      COMMON/HWUSUD/ACCUR,QEV(NMXSUD,6),SUD(NMXSUD,6),INTER,NQEV,NSUD,
     & SUDORD
C
      PARAMETER (NMXJET=200)
C
C
C Cluster common used by soft event routines
      PARAMETER (NMXCL=500)
      COMMON/HWCLUS/PPCL(5,NMXCL),IDCL(NMXCL),NCL

CBB
C
C Arrays for particle properties (NMXRES = max no of particles defined)
                                                                  
C
      INTEGER i,ifl
       EMSCA = PHEP(5,3)
       ierr = 0
C
C OK let herwig do the business
C     for qqbar final states :
C      check that what is left in Z0/gam* is compatible with a 2 quark masses
      do i = 1,nhep
          if (idhep(i).eq.23) then
             ifl = abs(idhep(i+1))
             if (phep(5,i).gt.2.*rmass(ifl)) go to 10
             print *,'low gamma*',phep(5,i),' quark mass',
     &       rmass(ifl),'event',nevhep
             ierr = 1
             go to 20
          endif
      enddo
 10   continue

C                             
C generate parton cascades                                              
C   
       CALL HWBGEN   
C                                                                       
C---DO HEAVY OBJECT DECAYS
       CALL HWDHOB 
C                                                                       
C do cluster formation                                                  
C                                                                       
       CALL HWCFOR   
C                                                                       
C do cluster decays                                                     
C                                                                       
       CALL HWCDEC   
C                                                                       
C do unstable particle decays                                           
C                                                                       
       CALL HWDHAD   
C                                                                       
C do heavy flavour decays                                               
C                                                                       
       CALL HWDHVY   
C                                                                       
C add soft underlying event if required                                 
C                                                                       
       CALL HWMEVT   
C                                                                       
C finish event                                                          
C                                                                       
 20   CALL HWUFNE  
C
      RETURN                                                      
      end
      SUBROUTINE FILHEP8(N,IST,ID,JMO1,JMO2,JDA1,JDA2,P4,PINV,PHFLAG)
C ----------------------------------------------------------------------
C this subroutine fills one entry into the HEPEVT common
C and updates the information for affected mother entries
C
C AST double precision version
C Add VTXPIP common and initialisation for mixing B Bloch October 2000
C
C     called by : TOHEP
C ----------------------------------------------------------------------
C
      PARAMETER (NMXHEP=2000)
      character*8 nwig
       real*8 phep,vhep
       real*8 AVWGT,EVWGT,GAMWT,TLOUT,WBIGST,WGTMAX,WGTSUM,WSQSUM
      LOGICAL GENSOF
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      COMMON/HWEVNT/AVWGT,EVWGT,GAMWT,TLOUT,WBIGST,WGTMAX,WGTSUM,WSQSUM,
     & IDHW(NMXHEP),IERROR,ISTAT,LWEVT,MAXER,MAXPR,NOWGT,NRN(2),NUMER,
     & NUMERU,NWGTS,GENSOF
      SAVE  /HEPEVT/
C Variables controling mixing and vertex information
      DOUBLE PRECISION 
     & EXAG,GEV2MM,HBAR,PLTCUT,VMIN2,VTXPIP,XMIX,XMRCT,YMIX,YMRCT,
     & DXRCYL,DXRSPH,DXZMAX,VIPWID
      INTEGER IOPDKL
      LOGICAL MAXDKL,MIXING,PIPSMR
      COMMON/HWDIST/EXAG,GEV2MM,HBAR,PLTCUT,VMIN2,VTXPIP(4),XMIX(2),
     & XMRCT(2),YMIX(2),YMRCT(2), DXRCYL,DXRSPH,DXZMAX,VIPWID(3),
     & IOPDKL,MAXDKL,MIXING,PIPSMR
C
      COMMON/PHOQED/QEDRAD(NMXHEP)
      LOGICAL QEDRAD
      SAVE /PHOQED/
      LOGICAL PHFLAG
C
      REAL*4  P4(4)
C
C check address mode
      IF (N.EQ.0) THEN
C
C append mode
        IHEP=NHEP+1
      ELSE IF (N.GT.0) THEN
C
C absolute position
        IHEP=N
      ELSE
C
C relative position
        IHEP=NHEP+N
      END IF
C
C check on IHEP
      IF ((IHEP.LE.0).OR.(IHEP.GT.NMXHEP)) RETURN
C
C add entry
      if(nhep.lt.ihep)  NHEP=IHEP
      ISTHEP(IHEP)=IST
      IDHEP(IHEP)=ID
      JMOHEP(1,IHEP)=JMO1
      IF(JMO1.LT.0)JMOHEP(1,IHEP)=JMOHEP(1,IHEP)+IHEP
      JMOHEP(2,IHEP)=JMO2
      IF(JMO2.LT.0)JMOHEP(2,IHEP)=JMOHEP(2,IHEP)+IHEP
      JDAHEP(1,IHEP)=JDA1
      JDAHEP(2,IHEP)=JDA2
C
      DO I=1,4
        PHEP(I,IHEP)=P4(I)
C
C KORAL-B and KORAL-Z do not provide vertex and/or lifetime informations
        VHEP(I,IHEP)=0.0
      END DO
      PHEP(5,IHEP)=PINV
C FLAG FOR PHOTOS...
      QEDRAD(IHEP)=PHFLAG
      CALL HWUIDT(1,ID,IDHW(IHEP),NWIG)
      if(ihep.eq.3) IDHW(IHEP) = 14
C main vertex
      do i =1,4
        vtxpip(i)= 0.D0
      enddo
      RETURN
C entry to fix daughters
      ENTRY FILDAU(N,JDA1,JDA2)
      JDAHEP(1,N) = JDA1
      JDAHEP(2,N) = JDA2
      RETURN
      END
      SUBROUTINE INIHER6(igcod,PBEAM,prec)
C 
C Routine to initialise HERWIG
C
C          ****COMMON BLOCK FILE FOR HERWIG VERSION 6.2****
C
C ALTERATIONS: Layout completely overhauled for 5.9
C
C
C New common blocks added for version 6.1:
C              HWCLUS,HWSUSY,HWRPAR,HWMINB
C
C New variables added for version 6.1:
C              OMHMIX,ET2MIX,PH3MIX,IOP4JT,NPRFMT,
C              PRNDEF,PRNTEX,PRNWEB,EFFMIN,GCUTME,
C              IOP4JT,NPRFMT                       see HWPRAM
C              Y4JT,DURHAM                         see HWHARD
C              QORQQB,QBORQQ                       see HWPROP
C              NRECO                               see HWUCLU
C              TXNAME                              see HWUNAM
C              PPCL,NCL,IDCL                       see HWCLUS
C              TANB,ALPHAH,COSBPA,SINBPA,COSBMA,
C              SINBMA,COSA,SINA,COSB,SINB,COTB,
C              ZMIXSS,ZMXNSS,ZSGNSS,LFCH,RFCH,
C              SLFCH,SRFCH,WMXUSS,WMXVSS,WSGNSS,
C              QMIXSS,LMIXSS,THETAT,THETAB,THETAL,
C              ATSS,ABSS,ALSS,MUSS,FACTSS,GHWWSS,
C              GHZZSS,GHDDSS,GHUUSS,GHWHSS,GHSQSS,
C              XLMNSS,RMMNSS,SENHNC,SUSYIN         see HWSUSY
C              LAMDA1,LAMDA2,LAMDA3,HRDCOL,RPARTY,
C              COLUPD                              see HWRPAR
C              PMBN1,PMBN2,PMBN3,PMBK1,PMBK2,
C              PMBM1,PMBM2,PMBP1,PMBP2,PMBP3       see HWMINB
C
C New parameters added for version 6.1:
C              NMXCL
C
C Parameter NMXRES raised to 500
C
C Scalar variables changed to arrays of size 2:
C              CLSMR,PSPLT,CLDIR                   see HWPRAM
C
C
C   NEW for HERWIG6.2
C
C New common blocks added for version 6.2:
C	       HWGRAV 				   see HWHGRV
C	       
C New variables added for version 6.2:
C              FIX4JT                              see HWIGIN
C
C New parameters added for version 6.2:
C	       DXRCYL,DXZMAX,DXRSPH	           see HWIGIN	
C	       GRVLAM,EMGRV,GAMGRV		   see HWHGRV
C
C New control flags added for version 6.2:
C              WZRFR                               see HWBJCO
C              IMSSM,IHIGGS                        see HWUINC
C
C New variable added for version 6.2:
C              PARITY                              see HWUINC
C
      IMPLICIT NONE
      DOUBLE PRECISION ZERO,ONE,TWO,THREE,FOUR,HALF
      PARAMETER (ZERO =0.D0, ONE =1.D0, TWO =2.D0,
     &           THREE=3.D0, FOUR=4.D0, HALF=0.5D0)
C
      DOUBLE PRECISION
     & ACCUR,AFCH,ALPFAC,ALPHEM,ANOMSC,ASFIXD,AVWGT,B1LIM,BETAF,BRFRAC,
     & BRHIG,BTCLM,CAFAC,CFFAC,CLDKWT,CLMAX,CLPOW,CLQ,CLSMR,CMMOM,COSS,
     & COSTH,CSPEED,CTHRPW,CTMAX,DECPAR,DECWT,DISF,DKLTM,EBEAM1,EBEAM2,
     & EMLST,EMMAX,EMMIN,EMPOW,EMSCA,ENHANC,ENSOF,EPOLN,ETAMIX,EVWGT,
     & EXAG,F0MIX,F1MIX,F2MIX,GAMH,GAMMAX,GAMW,GAMWT,GAMZ,GAMZP,GCOEF,
     & GEV2NB,GEV2MM,GPOLN,H1MIX,HBAR,HARDST,OMEGA0,PBEAM1,PBEAM2,PDIQK,
     & PGSMX,PGSPL,PHEP,PHIMIX,PHIPAR,PHOMAS,PIFAC,PLTCUT,PPAR,PPOLN,
     & PRECO,PRSOF,PSPLT,PTINT,PTMAX,PTMIN,PTPOW,PTRMS,PXRMS,PWT,Q2MAX,
     & Q2MIN,Q2POW,Q2WWMN,Q2WWMX,QCDL3,QCDL5,QCDLAM,QDIQK,QEV,QFCH,QG,
     & QLIM,QSPAC,QV,QWT,REPWT,RESN,RHOHEP,RHOPAR,RLTIM,RMASS,RMIN,
     & RSPIN,SCABI,SINS,SNGWT,SWEIN,SWTEF,SUD,THMAX,TLOUT,TMTOP,TMNISR,
     & TQWT,VCKM,VFCH,VGCUT,VHEP,VMIN2,VPAR,VPCUT,VQCUT,VTXPIP,VTXQDK,
     & WBIGST,WGTMAX,WGTSUM,WHMIN,WSQSUM,XFACT,XLMIN,XMIX,XMRCT,XX,
     & XXMIN,YBMAX,YBMIN,YJMAX,YJMIN,YMIX,YMRCT,YWWMAX,YWWMIN,ZBINM,
     & ZJMAX,ZMXISR,Y4JT,EFFMIN,PPCL,
     & TANB,ALPHAH,COSBPA,SINBPA,COSBMA,SINBMA,COSA,SINA,COSB,SINB,COTB,
     & ZMIXSS,ZMXNSS,ZSGNSS,LFCH,RFCH,SLFCH,SRFCH, WMXUSS,WMXVSS,WSGNSS,
     & QMIXSS,LMIXSS,THETAT,THETAB,THETAL,ATSS,ABSS,ALSS,MUSS,FACTSS,
     & GHWWSS,GHZZSS,GHDDSS,GHUUSS,GHWHSS,GHSQSS,
     & XLMNSS,RMMNSS,SENHNC,LAMDA1,LAMDA2,LAMDA3,
     & PMBN1,PMBN2,PMBN3,PMBK1,PMBK2,PMBM1,PMBM2,PMBP1,PMBP2,PMBP3,
     & OMHMIX,ET2MIX,PH3MIX,GCUTME,GRVLAM,EMGRV,GAMGRV,DXRCYL,DXRSPH,
     & DXZMAX,VIPWID
C
      INTEGER
     & CLDIR,IAPHIG,IBRN,IBSH,ICHRG,ICO,IDCMF,IDHEP,IDHW,IDK,IDKPRD,IDN,
     & IDPAR,IDPDG,IERROR,IFLAV,IFLMAX,IFLMIN,IHPRO,IMQDK,INHAD,INTER,
     & IOPDKL,IOPHIG,IOPREM,IPART1,IPART2,IPRINT,IPRO,IPROC,ISLENT,
     & ISPAC,ISTAT,ISTHEP,ISTPAR,JCOPAR,JDAHEP,JDAPAR,JMOHEP,JMOPAR,
     & JNHAD,LNEXT,LOCN,LOCQ,LRSUD,LSTRT,LWEVT,LWSUD,MAPQ,MAXER,MAXEV,
     & MAXFL,MAXPR,MODBOS,MODMAX,MODPDF,NBTRY,NCLDK,NCOLO,NCTRY,NDKYS,
     & NDTRY,NETRY,NEVHEP,NEVPAR,NFLAV,NGSPL,NHEP,NME,NMODES,NMXCDK,
     & NMXDKS,NMXHEP,NMXJET,NMXMOD,NMXPAR,NMXQDK,NMXRES,NMXSUD,NPAR,
     & NPRODS,NQDK,NQEV,NRES,NRN,NSPAC,NSTRU,NSTRY,NSUD,NUMER,NUMERU,
     & NWGTS,NZBIN,SUDORD,IOP4JT,HRDCOL,NMXCL,NCL,IDCL,NPRFMT,NRECO,
     & IMSSM,IHIGGS,PARITY,LRSUSY
C
      LOGICAL
     & AZSOFT,AZSPIN,BGSHAT,BREIT,CLRECO,COLISR,DKPSET,FROST,FSTEVT,
     & FSTWGT,GENEV,GENSOF,HARDME,HVFCEN,MAXDKL,MIXING,NOSPAC,NOWGT,
     & PRNDEC,PIPSMR,PRVTX,RSTAB,SOFTME,TMPAR,TPOL,USECMF,VTOCDK,VTORDK,
     & ZPRIME,RPARTY,COLUPD,PRNDEF,PRNTEX,PRNWEB,DURHAM,SUSYIN,WZRFR,
     & QORQQB,QBORQQ,FIX4JT
C
      CHARACTER*4
     & BDECAY
      CHARACTER*8
     & PART1,PART2,RNAME
      CHARACTER*20
     & AUTPDF
      CHARACTER*37
     & TXNAME
C
C New standard event common
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     & JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
C
C Beams, process and number of events
      COMMON/HWBEAM/IPART1,IPART2
      COMMON/HWBMCH/PART1,PART2
      COMMON/HWPROC/EBEAM1,EBEAM2,PBEAM1,PBEAM2,IPROC,MAXEV
C
C Basic parameters (and quantities derived from them)
      COMMON/HWPRAM/AFCH(16,2),ALPHEM,B1LIM,BETAF,BTCLM,CAFAC,CFFAC,
     & CLMAX,CLPOW,CLSMR(2),CSPEED,ENSOF,ETAMIX,F0MIX,F1MIX,F2MIX,GAMH,
     & GAMW,GAMZ,GAMZP,GEV2NB,H1MIX,PDIQK,PGSMX,PGSPL(4),PHIMIX,PIFAC,
     & PRSOF,PSPLT(2),PTRMS,PXRMS,QCDL3,QCDL5,QCDLAM,QDIQK,QFCH(16),QG,
     & QSPAC,QV,SCABI,SWEIN,TMTOP,VFCH(16,2),VCKM(3,3),VGCUT,VQCUT,
     & VPCUT,ZBINM,EFFMIN,OMHMIX,ET2MIX,PH3MIX,GCUTME,
     & IOPREM,IPRINT,ISPAC,LRSUD,LWSUD,MODPDF(2),NBTRY,NCOLO,NCTRY,
     & NDTRY,NETRY,NFLAV,NGSPL,NSTRU,NSTRY,NZBIN,IOP4JT(2),NPRFMT,
     & AZSOFT,AZSPIN,CLDIR(2),HARDME,NOSPAC,PRNDEC,PRVTX,SOFTME,ZPRIME,
     & PRNDEF,PRNTEX,PRNWEB
C
      COMMON/HWPRCH/AUTPDF(2),BDECAY
C
C Parton shower common (same format as /HEPEVT/)
      PARAMETER (NMXPAR=500)
      COMMON/HWPART/NEVPAR,NPAR,ISTPAR(NMXPAR),IDPAR(NMXPAR),
     & JMOPAR(2,NMXPAR),JDAPAR(2,NMXPAR),PPAR(5,NMXPAR),VPAR(4,NMXPAR)
C
C Parton polarization common
      COMMON/HWPARP/DECPAR(2,NMXPAR),PHIPAR(2,NMXPAR),RHOPAR(2,NMXPAR),
     & TMPAR(NMXPAR)
C
C Electroweak boson common
      PARAMETER (MODMAX=5)
      COMMON/HWBOSC/ALPFAC,BRHIG(12),ENHANC(12),GAMMAX,RHOHEP(3,NMXHEP),
     & IOPHIG,MODBOS(MODMAX)
C
C Parton colour common
      COMMON/HWPARC/JCOPAR(4,NMXPAR)
C
C other HERWIG branching, event and hard subprocess common blocks
      COMMON/HWBRCH/ANOMSC(2,2),HARDST,PTINT(3,2),XFACT,INHAD,JNHAD,
     & NSPAC(7),ISLENT,BREIT,FROST,USECMF,WZRFR
C
      COMMON/HWEVNT/AVWGT,EVWGT,GAMWT,TLOUT,WBIGST,WGTMAX,WGTSUM,WSQSUM,
     & IDHW(NMXHEP),IERROR,ISTAT,LWEVT,MAXER,MAXPR,NOWGT,NRN(2),NUMER,
     & NUMERU,NWGTS,GENSOF
C
      COMMON/HWHARD/ASFIXD,CLQ(7,6),COSS,COSTH,CTMAX,DISF(13,2),EMLST,
     & EMMAX,EMMIN,EMPOW,EMSCA,EPOLN(3),GCOEF(7),GPOLN,OMEGA0,PHOMAS,
     & PPOLN(3),PTMAX,PTMIN,PTPOW,Q2MAX,Q2MIN,Q2POW,Q2WWMN,Q2WWMX,QLIM,
     & SINS,THMAX,Y4JT,TMNISR,TQWT,XX(2),XLMIN,XXMIN,YBMAX,YBMIN,YJMAX,
     & YJMIN,YWWMAX,YWWMIN,WHMIN,ZJMAX,ZMXISR,IAPHIG,IBRN(2),IBSH,
     & ICO(10),IDCMF,IDN(10),IFLMAX,IFLMIN,IHPRO,IPRO,MAPQ(6),MAXFL,
     & BGSHAT,COLISR,FSTEVT,FSTWGT,GENEV,HVFCEN,TPOL,DURHAM,FIX4JT
C
C Arrays for particle properties (NMXRES = max no of particles defined)
      PARAMETER(NMXRES=500)
      COMMON/HWPROP/RLTIM(0:NMXRES),RMASS(0:NMXRES),RSPIN(0:NMXRES),
     & ICHRG(0:NMXRES),IDPDG(0:NMXRES),IFLAV(0:NMXRES),NRES,
     & VTOCDK(0:NMXRES),VTORDK(0:NMXRES),
     & QORQQB(0:NMXRES),QBORQQ(0:NMXRES)
C
      COMMON/HWUNAM/RNAME(0:NMXRES),TXNAME(2,0:NMXRES)
C
C Arrays for particle decays (NMXDKS = max total no of decays,
C                             NMXMOD = max no of modes for a particle)
      PARAMETER(NMXDKS=4000,NMXMOD=200)
      COMMON/HWUPDT/BRFRAC(NMXDKS),CMMOM(NMXDKS),DKLTM(NMXRES),
     & IDK(NMXDKS),IDKPRD(5,NMXDKS),LNEXT(NMXDKS),LSTRT(NMXRES),NDKYS,
     & NME(NMXDKS),NMODES(NMXRES),NPRODS(NMXDKS),DKPSET,RSTAB(0:NMXRES)
C
C Weights used in cluster decays
      COMMON/HWUWTS/REPWT(0:3,0:4,0:4),SNGWT,DECWT,QWT(3),PWT(12),
     & SWTEF(NMXRES)
C
C Parameters for cluster decays (NMXCDK = max total no of cluster
C                                         decay channels)
      PARAMETER(NMXCDK=4000)
      COMMON/HWUCLU/CLDKWT(NMXCDK),CTHRPW(12,12),PRECO,RESN(12,12),
     & RMIN(12,12),LOCN(12,12),NCLDK(NMXCDK),NRECO,CLRECO
C
C Variables controling mixing and vertex information
      COMMON/HWDIST/EXAG,GEV2MM,HBAR,PLTCUT,VMIN2,VTXPIP(4),XMIX(2),
     & XMRCT(2),YMIX(2),YMRCT(2),DXRCYL,DXRSPH,DXZMAX,VIPWID(3),IOPDKL,
     & MAXDKL,MIXING,PIPSMR
C
C Arrays for temporarily storing heavy-b,c-hadrons decaying partonicaly
C (NMXQDK = max no such decays in an event)
      PARAMETER (NMXQDK=20)
      COMMON/HWQDKS/VTXQDK(4,NMXQDK),IMQDK(NMXQDK),LOCQ(NMXQDK),NQDK
C
C Parameters for Sudakov form factors
C (NMXSUD= max no of entries in lookup table)
      PARAMETER (NMXSUD=1024)
      COMMON/HWUSUD/ACCUR,QEV(NMXSUD,6),SUD(NMXSUD,6),INTER,NQEV,NSUD,
     & SUDORD
C
      PARAMETER (NMXJET=200)
C
C
C Parameters for minimum bias/soft underlying event
      COMMON/HWMINB/
     & PMBN1,PMBN2,PMBN3,PMBK1,PMBK2,PMBM1,PMBM2,PMBP1,PMBP2,PMBP3
C
C Cluster common used by soft event routines
      PARAMETER (NMXCL=500)
      COMMON/HWCLUS/PPCL(5,NMXCL),IDCL(NMXCL),NCL
C
C  Parameters for resonant gravition production
      COMMON/HWGRAV/GRVLAM,EMGRV,GAMGRV

C also the following include files from the interface                   
C                                                                       

      INTEGER LMHLEN, LMHCOL, LMHROW                                    
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          
C                                                                       
      COMMON /BCS/   IW(1000)                                           
      INTEGER IW                                                        
      REAL RW(1000)                                                     
      EQUIVALENCE (RW(1),IW(1))                                         
C                                                                       
      INTEGER LCOLS, LROWS, KROW, KNEXT, ITABL,LFRWRD, LFRROW           
      REAL RTABL                                                        
      INTEGER ID, NRBOS, L                                              
      INTEGER JPARGN,JPARNA,JPARCO,JPARMA,JPARCH,JPARLT,JPARMW,JPARAN,  
     +          LPARTA                                                  
      PARAMETER(JPARGN=1,JPARNA=2,JPARCO=5,JPARMA=6,JPARCH=7,JPARLT=8,  
     +          JPARMW=9,JPARAN=10,LPARTA=10)                           
C some declarations in KMACRO                                           
      INTEGER NAPAR,JPA,NLINK                                           
      REAL PARMAS,TIMLIF                                                
C Flags and counters for the Herwig interface                           
      INTEGER NCOL                                                      
C                                                                       
      PARAMETER (NCOL = 41)                                             
C                                                                       
      INTEGER IOUT,IDEBB,IDEBE,IDEBL
      REAL*4 SDVRT,TABL,prec,pbeam                                                   
C                                                                       
      COMMON / DTMILL / SDVRT(3),TABL(NCOL)                             
      COMMON / INPOUT / IOUT                                            
      COMMON / DTBUG / IDEBB,IDEBE,IDEBL                                
C flag for the initial process and whether DIS process                  
      INTEGER IHARD,IFL,IDIS                                            
C                                                                       
      COMMON /INIPRO/ IHARD,IFL,IDIS                                    
      character*20 datemod                                              
      parameter (datemod=' 1st December 2000 ')                             
C                                                                       
      CHARACTER*4 CHAINT,CHBDEC(3)                                      
C                                                                       
      INTEGER NAMIND                                                    
      INTEGER DUMMY,NOSOFT,iflb,iq 
      REAL*8 HWRGET,pspb,gluma,BOSMASS(4)
C                                                                       
      INTEGER ALTABL,ALRLEP,ndrop 
      EXTERNAL ALTABL,ALRLEP,ndrop 
C                                                                       
      INTEGER HEDE,IGCO                                                 
C                                                                       
      PARAMETER (IGCO = 5144)                                           
      PARAMETER (HEDE = 52)                                             
C                                                                       
      INTEGER HEDEC(HEDE)                                               
C Standard variables                                                    
      INTEGER IGCOD,NDAT,IMAXER,IPROCI                                  
      INTEGER IPART,IKLIN,IPPART,JGHRW,IQUARK,IIBDEC                    
      INTEGER JGSPAR,NAMI,JDEBU,JGHRC                                   
      INTEGER JGPAR,JGMAS,MNUM,INPART,IANTI,JGSTA                       
      INTEGER MXDEC,NROW                                                
      INTEGER KGPART,KNODEC,I,JSVRT,JKPAR,IEBEAM,JRLEP,JANTI            
C                                                                       
C                                                                       
      REAL RAZSOF,RAZSPI,XTOT                                           
      REAL*8 ECMS                                                       
C                                                                       
C!    set of intrinsic functions to handle BOS banks                    
C - # of words/row in bank with index ID                                
      LCOLS(ID) = IW(ID+1)                                              
C - # of rows in bank with index ID                                     
      LROWS(ID) = IW(ID+2)                                              
C - index of next row in the bank with index ID                         
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)                       
C - index of row # NRBOS in the bank with index ID                      
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)                 
C - # of free words in the bank with index ID                           
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)                              
C - # of free rows in the bank with index ID                            
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)                               
C - Lth integer element of the NRBOSth row of the bank with index ID    
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)            
C - Lth real element of the NRBOSth row of the bank with index ID       
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)            
C                                                                       
C - mass of ALEPH particle# JPA                                         
      PARMAS(JPA) = RTABL(IW(NAPAR),JPA,6)                              
C - time of life of ALEPH particle# JPA                                 
      TIMLIF(JPA) = RTABL(IW(NAPAR),JPA,8)                              
C                                                                       
C                                                                       
      DATA CHBDEC /'HERW','EURO','CLEO'/                                
      DATA NAPAR /0/                                                    
C                                                                       
      IF (NAPAR.EQ.0) NAPAR = NAMIND('PART')                            
C                                                                       
C   Return the generator code as defined in the KINGAL library          
C                                                                       
C      IGCOD = IGCO                                                      
      IOUT = IW(6)                                                      
      WRITE(IOUT,101) IGCOD,IGCO,DATEMOD                                     
 101  FORMAT(/,10X,                                                     
     & 'HRWG12 - Code Number = ',I4,' HERWIG 6.2 ',I4,
     & ' Last Modifications ',A20          
     & ,/,10X,'***********************************************',        
     &'**************************',//)                                        
C                                                                       
C initialization                                                        
C                                                                       
      NDAT = 0                                                          
      CALL HWIGIN                                                       
      print * ,' after HWIGIN'
      DUMMY=HWRGET(NRN)                                                 
      IBRN(1)= nrn(1) 
      IBRN(2)= nrn(2)                                                  
C                                                                       
C create the KLIN bank and complete the PART  bank                      
C                                                                       
      DO I=1,4
        BOSMASS(I) = RMASS(197+I)
      ENDDO
      CALL KXHEPA(IPART,IKLIN) 
      DO I=1,4
        RMASS(I+197) = BOSMASS(I)
      ENDDO                                         
C                                                                       
      IF (IPART.LE.0 .OR. IKLIN.LE.0) THEN                              
         WRITE (IOUT,'(1X,''+++ASKUSI+++ IPART IKLIN '',2I5)')          
     &          IPART,IKLIN                                             
         WRITE(IOUT,'('' ASKUSI error filling PART or KLIN -STOP-'')')  
         CALL EXIT                                                      
      ENDIF                                                             
C Adjust masses when decays have become forbidden                       
C K0s K0l                                                               
       rmass(42)  = rmass(60)                                           
       rmass(50)  = rmass(60)                                           
C F0(980)                                                               
       rmass(293) = 0.996 
       rmass(294) = 1.000   ! 294 = FH_00 IDPDG= 10221                                              
C A0(980)                                                               
       rmass(290) = 0.996                                               
C XIb s                                                                 
       rmass(227) = 5.800                                               
       rmass(228) = rmass(227)                                          
       rmass(251) = rmass(227)                                          
       rmass(252) = rmass(227)                                          
C                                                                       
      PART1 = 'E-      '                                                
      PART2 = 'E+      '                                                
C                                                                       
C beam momentum                                                         
C                                                                       
      PBEAM1 = PBEAM
C                                                                       
      IHARD = 0                                                         
C                                                                       
C print PART bank per default                                           
C                                                                       
      IPPART = 1  
      iproc = 100    ! most probably qqbar
C decide on iproc depending on input gene code 
C  w pair = koralw 5035,5038 yfsww 5045
      if ((igcod.eq.5035).or.(igcod.eq.5038).or.(igcod.eq.5045)) 
     &           iproc = 200   ! most probably ww pair 
      modpdf(1)=-1                                                      
      modpdf(2)=-1                                                      
C                                                                       
C the default values can be changed by the DATA CARD GHRW               
C                                                                       
      JGHRW = NLINK('GHRW',0)                                           
C                                                                       
      IF(JGHRW.NE.0) THEN                                               
        NDAT = IW(JGHRW)                                                
C       IF (NDAT.GE.1) PBEAM1 = DBLE(RW(JGHRW+1))                       
C       IF (NDAT.GE.2) IPROC = IW(JGHRW+2)                              
        IF (NDAT.GE.3) IPRINT = IW(JGHRW+3)                             
C       IF (NDAT.GE.4) IHARD = IW(JGHRW+4)                              
        IF (NDAT.GE.5) IPPART = IW(JGHRW+5)                             
        IF (NDAT.GE.6) IMAXER = IW(JGHRW+6)                             
      ENDIF                                                             
C                                                                       
      PBEAM2 = PBEAM1                                                   
      ECMS   = PBEAM1 + PBEAM2                                          
C                                                                       
C here the user can change more standard model parameters
C
      JGPAR = NLINK('GGEW',0)
      IF(JGPAR.NE.0) THEN
        NDAT = IW(JGPAR)
        IF (NDAT.GE.1) NFLAV = IW(JGPAR+1)
        IF (NDAT.GE.2) SWEIN = DBLE(RW(JGPAR+2))
        IF (NDAT.GE.3) SCABI = DBLE(RW(JGPAR+3))
        IF (NDAT.GE.4) GAMZ = DBLE(RW(JGPAR+4))
        IF (NDAT.GE.5) GAMW = DBLE(RW(JGPAR+5))
      ENDIF

C                                                                       
C here the user can change B-decay parameter and choices
C
      JGPAR = NLINK('GBDK',0)
      IIBDEC = 1
C
      IF(JGPAR.NE.0) THEN
        NDAT = IW(JGPAR)
        IF (NDAT.GE.1) B1LIM = DBLE(RW(JGPAR+1))
        IF (NDAT.GE.2) THEN
          BDECAY = CHAINT(IW(JGPAR+2))
          DO I=3,1,-1
            IF(BDECAY.EQ.CHBDEC(I)) IIBDEC=I
          ENDDO
        ENDIF
C choose B mixing and mass and width differences
        IF (NDAT.GE.3) MIXING = IW(JGPAR+3).EQ.1
        IF (NDAT.GE.4) XMIX(1) = DBLE(RW(JGPAR+4))
        IF (NDAT.GE.5) XMIX(2) = DBLE(RW(JGPAR+5))
        IF (NDAT.GE.6) YMIX(1) = DBLE(RW(JGPAR+6))
        IF (NDAT.GE.7) YMIX(2) = DBLE(RW(JGPAR+7))
      ENDIF

C                                                                       
C                                                                       
C Here the user can set the max lifetime for an unstable particle or    
C if it outside a specified volume                                      
C      JGPAR = NLINK('GMLT',0)                                           
C      MAXDKL=.FALSE.                                                    
C      IF(JGPAR.NE.0) THEN                                               
C        NDAT = IW(JGPAR)                                                
C        IF(NDAT.GE.1) PLTCUT = DBLE(RW(JGPAR+1))                        
C        IF(NDAT.GE.2) MAXDKL = IW(JGPAR+2).EQ.1                         
C        IF(NDAT.GE.3) PRVTX  = IW(JGPAR+3).EQ.1                         
C      ENDIF                                                             
C here the user can change the most important                           
C parameters for the cluster fragmentation                              
C                                                                       
      JGPAR = NLINK('GQCD',0)
      IF(JGPAR.NE.0) THEN
        NDAT = IW(JGPAR)
        IF (NDAT.GE.1) QCDLAM = DBLE(RW(JGPAR+1))
        IF (NDAT.GE.2) CLMAX  = DBLE(RW(JGPAR+2))
        IF (NDAT.GE.3) CLPOW  = DBLE(RW(JGPAR+3))
        IF (NDAT.GE.4) VQCUT  = DBLE(RW(JGPAR+4))
        IF (NDAT.GE.5) VGCUT  = DBLE(RW(JGPAR+5))
        IF (NDAT.GE.6) VPCUT  = DBLE(RW(JGPAR+6))
        IF (NDAT.GE.7) QDIQK = DBLE(RW(JGPAR+7))
        IF (NDAT.GE.8) PDIQK = DBLE(RW(JGPAR+8))
      ENDIF
C                                                                       
C Now some further parameters controlling the program                   
C for flavor dependant variables
C
C
      JGPAR = NLINK('GFLP',0)
      IF(JGPAR.NE.0) THEN
        NDAT = IW(JGPAR)
        IF (NDAT.GE.1) PSPLT(1) = DBLE(RW(JGPAR+1))
        IF (NDAT.GE.2) PSPLT(2) = DBLE(RW(JGPAR+2))
        IF (NDAT.GE.3) CLSMR(1) = DBLE(RW(JGPAR+3))
        IF (NDAT.GE.4) CLSMR(2) = DBLE(RW(JGPAR+4))
        IF (NDAT.GE.5) CLDIR(1) = IFIX(RW(JGPAR+5))
        IF (NDAT.GE.6) CLDIR(2) = IFIX(RW(JGPAR+6))
      ENDIF
C
C Allow some relative weight parameters to vary
C
      JGPAR = NLINK('GHWT',0)
      IF(JGPAR.NE.0) THEN
        NDAT = IW(JGPAR)
        IF (NDAT.GE.1) SNGWT = DBLE(RW(JGPAR+1))        ! singlet baryon
        IF (NDAT.GE.2) DECWT = DBLE(RW(JGPAR+2))        ! decuplet baryo
        IF (NDAT.GE.3) REPWT(0,1,0) = DBLE(RW(JGPAR+3)) ! vector
        IF (NDAT.GE.4) REPWT(0,2,0) = DBLE(RW(JGPAR+4)) ! tensor
        IF(NDAT.GT.12) NDAT=12
        IF(NDAT.GT.4) THEN
          DO I=5,NDAT
            PWT(I-4) = DBLE(RW(JGPAR+I))        ! d,u,s,c,b,t,qqb in clu
          ENDDO
        ENDIF
      ENDIF
C                                                                       
C change here the parameters governing final state radiation            
C                                                                       
      JGPAR = NLINK('GFSR',0)                                           
      IF(JGPAR.NE.0) THEN                                               
        NDAT = IW(JGPAR)                                                
        IF (NDAT.GE.1) VPCUT = DBLE(RW(JGPAR+1))                        
        IF (NDAT.GE.2) ALPFAC = DBLE(RW(JGPAR+2))                       
      ENDIF                                                             
C                                                                       
C change here the parameters governing initial state radiation          
C                                                                       
      JGPAR = NLINK('GISR',0)                                           
      IF(JGPAR.NE.0) THEN                                               
        NDAT = IW(JGPAR)                                                
        IF (NDAT.GE.1) TMNISR = DBLE(RW(JGPAR+1))                       
        IF (NDAT.GE.2) ZMXISR = DBLE(RW(JGPAR+2))                       
        IF (NDAT.GE.3) COLISR = IW(JGPAR+3).eq.1                        
      ENDIF                                                             
C                                                                       
C Sudakov form factor options                                           
C                                                                       
      LWSUD = 0                                                         
      LRSUD = 0                                                         
      JGPAR = NLINK('GSUD',0)                                           
C                                                                       
      IF(JGPAR.NE.0) THEN                                               
        NDAT = IW(JGPAR)                                                
        IF (NDAT.GE.1) SUDORD = IW(JGPAR+1)                             
        IF (NDAT.GE.2) LWSUD  = IW(JGPAR+2)                             
        IF (NDAT.GE.3) LRSUD  = IW(JGPAR+3)                             
      ENDIF                                                             
C                                                                       
C W and Z decay options                                                 
C                                                                       
      JGPAR = NLINK('GHMB',0)                                           
      IF(JGPAR.NE.0) THEN                                               
        NDAT = IW(JGPAR)                                                
        IF(NDAT.GE.1) THEN                                              
          DO I=1,NDAT                                                   
            MODBOS(I) = IW(JGPAR+I)                                     
          ENDDO                                                         
        ENDIF                                                           
      ENDIF                                                             
C
C Colour rearrangement model
C
      JGPAR = NLINK('GHCR',0)
      IF(JGPAR.NE.0) THEN
        NDAT = IW(JGPAR)
        IF (NDAT.GE.1) CLRECO = IW(JGPAR+1).EQ.1
        IF (NDAT.GE.2) PRECO  = DBLE(RW(JGPAR+2))
        IF (NDAT.GE.3) EXAG   = DBLE(RW(JGPAR+3))
        IF (NDAT.GE.4) VMIN2  = DBLE(RW(JGPAR+4))
      ENDIF
C finally we redefine some steering parameters of the                   
C HERWIG program:                                                       
C don't print out event listings                                        
C increase number of possible HERWIG errors                             
C                                                                       
      MAXPR = 0                                                         
      MAXER = 100                                                       
C                                                       
C                                                       
C switch off time check in HERWIG                                       
C                                                                       
      TLOUT = 0.D0                                                      
C                                                                       
C the user can debug specific events with the                           
C data card DEBU                                                        
C                                                                       
      NAMI = NAMIND('DEBU')                                             
C                                                                       
      JDEBU = IW(NAMI)                                                  
      IDEBB = 0                                                         
      IDEBE = 0 
C                                                                       
      IF(JDEBU.NE.0) THEN                                               
        IDEBL = 1
C                                                                       
        NDAT = IW(JDEBU)                                                
C                                                                       
        IF (NDAT.GE.1) IDEBB = IW(JDEBU+1)
        IDEBE = IDEBB
        IF (NDAT.GE.2) IDEBE = IW(JDEBU+2)
        IF (NDAT.GE.3) IDEBL = IW(JDEBU+3)
C                                                                       
      ENDIF                                                             
C                                                                       
C user can reset parameters at                                          
C this point by data cards, otherwise values                            
C set in HWIGIN/HWUINC will be used.                                    
C                                                                       
C stop smearing of primary vertex (done in ASKUSE)                      
      PIPSMR = .FALSE.                                                  
      IF (PRECO.gt.0.D0) CLRECO = .TRUE.
      prec = preco
CC   drop any modification to quark masses ( except top )
C        do iq = 1,5
C           JGMAS = NLINK('GMAS',IQ)
C           IF ( JGMAS.gt.0) MNUM = NDROP('GMAS',IQ)
C        enddo

C                                                                       
C the user can define different values with the data cards GMAS         
C the masses of the t-mesons, t-baryons and the diquarks should         
C be redefined after the call to HWUINC                                 
C                                                                       
      NAMI = NAMIND ('GMAS')                                            
C                                                                       
      IF (IW(NAMI).NE.0) THEN                                           
C                                                                       
        JGMAS = NAMI + 1                                                
C                                                                       
C loop over all GMAS banks                                              
C                                                                       
   5    JGMAS = IW(JGMAS-1)                                             
        IF (JGMAS.EQ. 0) GOTO 6                                         
C                                                                       
        MNUM = IW(JGMAS-2)                                              
        IF ((MNUM.LT.109.OR.MNUM.GT.120).AND.                           
     &      (MNUM.LT.232.OR.MNUM.GT.244).AND.                           
     &      (MNUM.LT.255.OR.MNUM.GT.264)) THEN                          
          RMASS(MNUM) = RW(JGMAS+1)                                     
          INPART = IW(NAPAR)                                            
          IPART = KGPART(MNUM)                                          
          IANTI = ITABL(INPART,IPART,JPARAN)                            
          RW(KROW(INPART,IPART)+JPARMA) = RW(JGMAS+1)                   
          IF (IANTI.NE.IPART) RW(KROW(INPART,IANTI)+JPARMA)=RW(JGMAS+1) 
        ENDIF                                                           
        GOTO 5                                                          
C                                                                       
   6    CONTINUE                                                        
C                                                                       
      ENDIF                                                             
C                                                                       
C create the KLIN bank and complete the PART  bank                      
C                                                                       
C compute parameter-dependent constants                                 
C HWUINC overwrites the masses of the t-mesons, t-baryons               
C and the diquarks                                                      
C
      rmass(42)  = rmass(60) + 0.00000
      rmass(50)  = rmass(60) + 0.00000
C mods from Gerald Rudolph switching off the D-wave meson multiplets.
      do  i = 363, 387
        VTOCDK(i) = .true.
      end  do
      do  i = 395, 399
         VTOCDK(i) = .true.
      end  do
      CALL HWUINC                                                       
      WRITE(IOUT,*) 'ASKUSI :You will be using the HERWIG parameters '
      WRITE(IOUT,*) '  QCDLAM,   CLMAX,    PSPLT(1-2),      CLSMR(1-2)'
      WRITE(IOUT,112) QCDLAM,CLMAX,1./PSPLT(1),1./PSPLT(2),CLSMR
      WRITE(IOUT,*) '  DECWT,    GLUMAS,   PRECO,    VMIN2'
      WRITE(IOUT,112) DECWT,RMASS(13),PRECO,VMIN2
 112  Format(10F10.5)
C                                                                       
C reset the overwritten masses back to the ALEPH values                 
C (if available)                                                        
C        
      RMASS(232) = DBLE(PARMAS(140))                                    
      RMASS(233) = DBLE(PARMAS(133))                                    
      RMASS(234) = DBLE(PARMAS(131))                                    
      RMASS(235) = DBLE(PARMAS(135))                                    
      RMASS(236) = DBLE(PARMAS(239))                                    
      RMASS(237) = DBLE(PARMAS(271))                                    
      RMASS(238) = DBLE(PARMAS(243))                                    
C                                                                       
      RMASS(241) = DBLE(PARMAS(249))                                    
      RMASS(242) = DBLE(PARMAS(137))                                    
      RMASS(243) = DBLE(PARMAS(139))                                    
C                                                                       
      RMASS(255) = DBLE(PARMAS(134))                                    
      RMASS(256) = DBLE(PARMAS(132))                                    
      RMASS(257) = DBLE(PARMAS(136))                                    
      RMASS(258) = DBLE(PARMAS(240))                                    
      RMASS(259) = DBLE(PARMAS(272))                                    
      RMASS(260) = DBLE(PARMAS(244))                                    
C                                                                       
      RMASS(263) = DBLE(PARMAS(250))                                    
      RMASS(264) = DBLE(PARMAS(138))                                    
C                                                                       
C now we we have to set the user defined masses of the                  
C t-mesons, t-baryons and diquarks                                      
C                                                                       
      NAMI = NAMIND ('GMAS')                                            
C                                                                       
      IF (IW(NAMI).NE.0) THEN                                           
C                                                                       
        JGMAS = NAMI + 1                                                
C                                                                       
C loop over all GMAS banks                                              
C                                                                       
  15    JGMAS = IW(JGMAS-1)                                             
        IF (JGMAS.EQ. 0) GOTO 16                                        
C                                                                       
        MNUM = IW(JGMAS-2)                                              
        IF ((MNUM.GE.109.AND.MNUM.LE.120).OR.                           
     &      (MNUM.GE.232.AND.MNUM.LE.244).OR.                           
     &      (MNUM.GE.255.AND.MNUM.LE.264)) THEN                         
          RMASS(MNUM) = RW(JGMAS+1)                                     
          INPART = IW(NAPAR)                                            
          IPART = KGPART(MNUM)                                          
          RW(KROW(INPART,IPART)+JPARMA) = RW(JGMAS+1)                   
        ENDIF                                                           
        GOTO 15                                                         
C                                                                       
  16    CONTINUE                                                        
C now rest Z amd W masses
C      rmass(200) = xpar(4)
C      rmass(198) = xpar(6)
C      rmass(199) = xpar(6)
C                                                                       
      ENDIF                                                             
      WRITE(IOUT,103) (RMASS(iq),iq=1,6),RMASS(13)
 103  FORMAT (' Masses to be used : d         u         s         c    '
     &,'      b         t       gluon',/, '                    ',7G10.3)

C                                                                       
C the user can set any particle stable                                  
C with the data cards GSTA                                              
C                                                                       
      NAMI = NAMIND ('GSTA')                                            
C                                                                       
      IF (IW(NAMI).NE.0) THEN                                           
C                                                                       
        JGSTA = NAMI + 1                                                
C                                                                       
C loop over all GSTA banks                                              
C                                                                       
 105    JGSTA = IW(JGSTA-1)                                             
        IF (JGSTA.EQ. 0) GOTO 106                                       
C                                                                       
        MNUM = IW(JGSTA-2)                                              
C                                                                       
        IF (CHAINT(IW(JGSTA+1)).EQ.'OFF'.AND.NMODES(MNUM).NE.0)         
     &    CALL HWUSTA(RNAME(MNUM))                                      
        IF (CHAINT(IW(JGSTA+1)).EQ.'ON'.AND.NMODES(MNUM).EQ.0)          
     &    WRITE(IOUT,1001)MNUM                                          
 1001     FORMAT(1X,'+++ASKUSI+++ Herwig particle # = ',I5,             
     &           ' no decay modes available - left stable')             
C                                                                       
        GOTO 105                                                        
C                                                                       
 106    CONTINUE                                                        
C                                                                       
      ENDIF                                                             
C                                                                       
C get particles which should not be decayed by the generator            
C                                                                       
      MXDEC = KNODEC (HEDEC,HEDE)                                       
      MXDEC = MIN (MXDEC,HEDE)                                          
C                                                                       
C now set them stable, unless the user has provided a data              
C card for them                                                         
C                                                                       
      DO 200 IPART = 1,MXDEC                                            
C                                                                       
C look if data card is present                                          
C                                                                       
         JGSTA = NLINK('GSTA',HEDEC(IPART))                             
C                                                                       
C if not, inhibit decay                                                 
C                                                                       
         IF (JGSTA.LE.0) THEN                                           
           IF (HEDEC(IPART).GT.0.AND.NMODES(HEDEC(IPART)).NE.0)         
     &       CALL HWUSTA(RNAME(HEDEC(IPART)))                           
         ENDIF                                                          
C                                                                       
  200 CONTINUE                                                          
C                                                                       
C user's initial calculations, redefine any other parameter in HERWIG   
C                                                                       
      CALL USTART                                                       
C                                                                       
C  Print PART and KLIN bank                                             
C                                                                       
      IF (IPPART.EQ.1) CALL PRPART                                      
C                                                                       
      RETURN                                                            
      END 
CBB  added to create Crete files
      subroutine ksecwr
      common /myout/ jout
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)
C
      COMMON /BCS/   IW(LBCS )
      INTEGER IW
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
      data  nksec/0/
      jkpar = iw(namind('KSEC'))
      if (jkpar.gt.0) then
        nksec = nksec +1
        igco  = iw(JKPAR+LMHLEN+1)
        nev   =  iw(JKPAR+LMHLEN+4)
        xsec  = rw(JKPAR+LMHLEN+7)
        ersec = rw(JKPAR+LMHLEN+8)
        if ( nksec.eq.2) then
          write (jout,10) igco,nev,xsec,ersec
  10   format('KSEC',2i10,2F20.10)
          CLOSE (jout) 
        endif
      endif
      END
      subroutine hdrwr(igco,inotr,nrun)
      common /myout/ jout
      logical fok
      character*10 atype, fdevi
      character*10 dtype, ddevi
      character*80 fname
      character*4 cname
      save fname,fok
      data cname,dtype,ddevi/'CRET','CARDS','    '/
    
        call acdarg(cname,dtype,ddevi,fname, atype, fdevi, ier)
C   file is ok if given with type (ier=0) or default type (ier=1)
        fok = (ier.eq.0) .or. ((ier.eq.1).and.(fname.ne.' '))
        if (fok) 
     &     print *,' file',fname,' type',atype,' device',fdevi

      jout = 66
      if(fok) OPEN(UNIT=jout,STATUS='UNKNOWN',FILE=fname)
      write (jout,10) nrun,igco,inotr
 10   format('KRUN',3i10)
      return
      end
      subroutine kpawr_qq
      common /myout/ jout
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)
C
      COMMON /BCS/   IW(LBCS )
      INTEGER IW
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
      PARAMETER (nwordi=14,nwordf=101)
      dimension npar(nwordi)
      dimension xpar(nwordf)
      do i= 1,nwordi
        npar(i)=0
      enddo
      do i= 1,nwordf
        xpar(i)=0.
      enddo
      jkpar = iw(namind('KPAR'))
      if (jkpar.gt.0) then
       do i = 1,3
       xpar(92+i) = 10.*RW(JKPAR+LMHLEN+5+i)  !   XVRT
       xpar(95+i) = 10.*RW(JKPAR+LMHLEN+2+i)  !   SVERT
       xpar(98+i) = 10.*RW(JKPAR+LMHLEN+8+i)  !   sXVRT
       enddo
      endif
      write(jout,10) nwordi,nwordf
 10   format('KPAR',2i10)
      WRITE (jOUT,'(10E12.4)') (xpar(ii),ii=1,nwordf)      
      return
      end
      subroutine kpawr_WW
      double precision xpar(100)
      dimension npar(100)
      common /myout/ jout
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)
C
      COMMON /BCS/   IW(LBCS )
      INTEGER IW
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
      PARAMETER (nwordi=14,nwordf=101)
      write(jout,10) nwordi,nwordf
 10   format('KPAR',2i10)

      jkpar = iw(namind('KPAR'))
      if (jkpar.gt.0) then
        ipsho = rw(JKPAR+LMHLEN+8)  ! fragm scheme 
        icr   = rw(JKPAR+LMHLEN+9)  ! color reco model
        do ii=1,6
          npar(ii)=NINT((RW(JKPAR+LMHLEN+ii)))
          npar(20+ii)=NINT((RW(JKPAR+LMHLEN+10+ii)))
        enddo
        do ii=1,10
          xpar(ii)=RW(JKPAR+LMHLEN+20+ii)
        if (ii.le.7) then
           xpar(20+ii)=RW(JKPAR+LMHLEN+40+ii)
           xpar(30+ii)=RW(JKPAR+LMHLEN+50+ii)
           xpar(40+ii)=RW(JKPAR+LMHLEN+60+ii)
           xpar(50+ii)=RW(JKPAR+LMHLEN+70+ii)
        endif
        enddo
        xpar(11)= RW(JKPAR+LMHLEN+81)
        xpar(12)= RW(JKPAR+LMHLEN+82)
        write(jout,11) (npar(ii),ii=1,6),(npar(ii),ii=21,26),ipsho,icr
 11     format(14i9)
        WRITE (jOUT,'(10E12.4)') (xpar(ii),ii=1,12),(xpar(ii),ii=21,27),
     $ (xpar(ii),ii=31,37),(xpar(ii),ii=41,47),(xpar(ii),ii=51,57),
     $ (RW(JKPAR+LMHLEN+LLL),LLL=91,142),
     $ (10.*RW(JKPAR+LMHLEN+LLL),LLL=34,36),
     $ (10.*RW(JKPAR+LMHLEN+LLL),LLL=31,33),
     $ (10.*RW(JKPAR+LMHLEN+LLL),LLL=37,39)
      endif
      return
      end
      SUBROUTINE herHEPC(MCONV)
C...Purpose: to convert HEPEVT event record contents to or from
C...the new newevt event record commonblock.
C...Note that newEVT is  in double precision according to LEP 2 stand
      PARAMETER (NMXHEP=2000)
      COMMON/newEVT/NEVEVT,NEVT,ISTEVT(NMXHEP),IDEVT(NMXHEP),
     &JMOEVT(2,NMXHEP),JDAEVT(2,NMXHEP),PEVT(5,NMXHEP),VEVT(4,NMXHEP)
CCCCC   attention !
      REAL*8 PEVT,VEVT
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
CCCCC   attention !
      REAL*8 PHEP,VHEP
C      print *, ' nevhep',nevhep
      NEVEVT = NEVHEP
      NEVT   = NHEP
      do i = 1,NHEP
       ISTEVT(i) =  ISTHEP(i)
       IDEVT(i) = IDHEP(i)
C       print *, ' i, mother dauter KF',i,jmohep(1,i),jmohep(2,i),
C     &  jDAhep(1,i),jDAhep(2,i),IDHEP(I)
       do j = 1,5
        if ( j.le.2) then
          JMOEVT(j,i) = JMOHEP(j,i)
          JDAEVT(j,i) = JDAHEP(j,i)
        endif
        PEVT(j,i) = PHEP(j,i)
        if (j.le.4) VEVT(j,i) = VHEP(j,i)
       enddo
      enddo
      return
      end
      SUBROUTINE newHEPC(MCONV)

C...Purpose: to convert JETSET event record contents to or from
C...the standard event record commonblock.
C...Note that newEVT is  in double precision according to LEP 2 stand
      PARAMETER (NMXHEP=2000)
      COMMON/newEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
CCCCC   attention !
      REAL*8 PHEP,VHEP
      REAL*4 P,V
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      SAVE /LUJETS/,/LUDAT1/,/LUDAT2/

C...Conversion from JETSET to standard, the easy part.
      IF(MCONV.EQ.1) THEN
        NEVHEP=0
        IF(N.GT.NMXHEP) CALL LUERRM(8,
     &  '(LUHEPC:) no more space in /HEPEVT/')
        NHEP=MIN(N,NMXHEP)
        DO 140 I=1,NHEP
        ISTHEP(I)=0
        IF(K(I,1).GE.1.AND.K(I,1).LE.10) ISTHEP(I)=1
        IF(K(I,1).GE.11.AND.K(I,1).LE.20) ISTHEP(I)=2
        IF(K(I,1).GE.21.AND.K(I,1).LE.30) ISTHEP(I)=3
        IF(K(I,1).GE.31.AND.K(I,1).LE.100) ISTHEP(I)=K(I,1)
        IDHEP(I)=K(I,2)
        JMOHEP(1,I)=K(I,3)
        JMOHEP(2,I)=0
        IF(K(I,1).NE.3.AND.K(I,1).NE.13.AND.K(I,1).NE.14) THEN
          JDAHEP(1,I)=K(I,4)
          JDAHEP(2,I)=K(I,5)
        ELSE
          JDAHEP(1,I)=0
          JDAHEP(2,I)=0
        ENDIF
        DO 100 J=1,5
        PHEP(J,I)=P(I,J)
  100   CONTINUE
        DO 110 J=1,4
        VHEP(J,I)=V(I,J)
  110   CONTINUE

C...Check if new event (from pileup).
        IF(I.EQ.1) THEN
          INEW=1
        ELSE
          IF(K(I,1).EQ.21.AND.K(I-1,1).NE.21) INEW=I
        ENDIF

C...Fill in missing mother information.
        IF(I.GE.INEW+2.AND.K(I,1).EQ.21.AND.K(I,3).EQ.0) THEN
          IMO1=I-2
          IF(I.GE.INEW+3.AND.K(I-1,1).EQ.21.AND.K(I-1,3).EQ.0)
     &    IMO1=IMO1-1
          JMOHEP(1,I)=IMO1
          JMOHEP(2,I)=IMO1+1
        ELSEIF(K(I,2).GE.91.AND.K(I,2).LE.93) THEN
          I1=K(I,3)-1
  120     I1=I1+1
          IF(I1.GE.I) CALL LUERRM(8,
     &    '(LUHEPC:) translation of inconsistent event history')
          IF(I1.LT.I.AND.K(I1,1).NE.1.AND.K(I1,1).NE.11) GOTO 120
          KC=LUCOMP(K(I1,2))
          IF(I1.LT.I.AND.KC.EQ.0) GOTO 120
          IF(I1.LT.I.AND.KCHG(KC,2).EQ.0) GOTO 120
          JMOHEP(2,I)=I1
        ELSEIF(K(I,2).EQ.94) THEN
          NJET=2
          IF(NHEP.GE.I+3.AND.K(I+3,3).LE.I) NJET=3
          IF(NHEP.GE.I+4.AND.K(I+4,3).LE.I) NJET=4
          JMOHEP(2,I)=MOD(K(I+NJET,4)/MSTU(5),MSTU(5))
          IF(JMOHEP(2,I).EQ.JMOHEP(1,I)) JMOHEP(2,I)=
     &    MOD(K(I+1,4)/MSTU(5),MSTU(5))
        ENDIF

C...Fill in missing daughter information.
        IF(K(I,2).EQ.94.AND.MSTU(16).NE.2) THEN
          DO 130 I1=JDAHEP(1,I),JDAHEP(2,I)
          I2=MOD(K(I1,4)/MSTU(5),MSTU(5))
          JDAHEP(1,I2)=I
  130     CONTINUE
        ENDIF
        IF(K(I,2).GE.91.AND.K(I,2).LE.94) GOTO 140
        I1=JMOHEP(1,I)
        IF(I1.LE.0.OR.I1.GT.NHEP) GOTO 140
        IF(K(I1,1).NE.13.AND.K(I1,1).NE.14) GOTO 140
        IF(JDAHEP(1,I1).EQ.0) THEN
          JDAHEP(1,I1)=I
        ELSE
          JDAHEP(2,I1)=I
        ENDIF
  140   CONTINUE
        DO 150 I=1,NHEP
        IF(K(I,1).NE.13.AND.K(I,1).NE.14) GOTO 150
        IF(JDAHEP(2,I).EQ.0) JDAHEP(2,I)=JDAHEP(1,I)
  150   CONTINUE

C...Conversion from standard to JETSET, the easy part.
      ELSE
        IF(NHEP.GT.MSTU(4)) CALL LUERRM(8,
     &  '(LUHEPC:) no more space in /LUJETS/')
        N=MIN(NHEP,MSTU(4))
        NKQ=0
        KQSUM=0
        DO 180 I=1,N
        K(I,1)=0
        IF(ISTHEP(I).EQ.1) K(I,1)=1
        IF(ISTHEP(I).EQ.2) K(I,1)=11
        IF(ISTHEP(I).EQ.3) K(I,1)=21
        K(I,2)=IDHEP(I)
        K(I,3)=JMOHEP(1,I)
        K(I,4)=JDAHEP(1,I)
        K(I,5)=JDAHEP(2,I)
        DO 160 J=1,5
        P(I,J)=PHEP(J,I)
  160   CONTINUE
        DO 170 J=1,4
        V(I,J)=VHEP(J,I)
  170   CONTINUE
        V(I,5)=0.
        IF(ISTHEP(I).EQ.2.AND.PHEP(4,I).GT.PHEP(5,I)) THEN
          I1=JDAHEP(1,I)
          IF(I1.GT.0.AND.I1.LE.NHEP) V(I,5)=(VHEP(4,I1)-VHEP(4,I))*
     &    PHEP(5,I)/PHEP(4,I)
        ENDIF

C...Fill in missing information on colour connection in jet systems.
        IF(ISTHEP(I).EQ.1) THEN
          KC=LUCOMP(K(I,2))
          KQ=0
          IF(KC.NE.0) KQ=KCHG(KC,2)*ISIGN(1,K(I,2))
          IF(KQ.NE.0) NKQ=NKQ+1
          IF(KQ.NE.2) KQSUM=KQSUM+KQ
          IF(KQ.NE.0.AND.KQSUM.NE.0) THEN
            K(I,1)=2
          ELSEIF(KQ.EQ.2.AND.I.LT.N) THEN
            IF(K(I+1,2).EQ.21) K(I,1)=2
          ENDIF
        ENDIF
  180   CONTINUE
        IF(NKQ.EQ.1.OR.KQSUM.NE.0) CALL LUERRM(8,
     &  '(LUHEPC:) input parton configuration not colour singlet')
      ENDIF

      END

      subroutine hepwr(ievt,icr,pcr)
      common /myout/ jout
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)
C
      COMMON /BCS/   IW(LBCS )
      INTEGER IW
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
      PARAMETER (NMXHEP=2000)
      real*8 phep,vhep
      COMMON/newEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
CC    DOUBLE PRECISION PHEP,VHEP
      COMMON/comvert/xsec(4,10)  ! vertices without B field effects....
      real xsec
      PARAMETER (CLGHT = 2.99792458)
C   loop over vertices and attach to particles

      NAMV = NAMIND('VERT')
      JVER = NAMV + 1
 1    JVER = IW(JVER-1)
      IF (JVER .NE. 0) THEN
         NVX = IW(JVER-2)
         LHDR = IW(JVER+1)
         LPVER= IW(JVER+2)
         MXTRK= IW(JVER+3)
         KVER = JVER + LHDR
C
         do itk = 1,MXTRK
C   common contains 2 incoming particles
            itkv = IW(KVER+LPVER+itk)+2
            do ico = 1,3
CBB               VHEP(ico,itkv) = 10.*RW(KVER+ico)
                  VHEP(ico,itkv) = 10.*xsec(ico,nvx)
            enddo
CBB              VHEP(4,itkv) = (RW(KVER+4)*10**12)/CLGHT
                 VHEP(4,itkv) = (xsec(4,nvx)*10.D0**12)/CLGHT
         enddo
         GO TO 1
      ENDIF
         write(jout,10) ievt,nhep,nvx,icr,pcr
 10   format('KEVT',4i10,F20.10)
      do ihep = 1,nhep
         write(jout,20) ISTHEP(ihep),IDHEP(ihep),JMOHEP(1,ihep),
     &     JMOHEP(2,ihep),JDAHEP(1,ihep),JDAHEP(2,ihep)
         write(jout,30) (PHEP(ii,ihep),ii=1,5)
         write(jout,30) (VHEP(ii,ihep),ii=1,4)
      enddo
 20   format(6i10)
 30   format(5F20.10)
      return
      end
      SUBROUTINE KFEVBK (VMAIN,RPARA,IPARA,MTRAK,ISTAT)
C -----------------------------------------------------------
C - J.Boucrot - B.Bloch - F.Ranjard - 870515
C! Fill event banks KINE VERT KHIS
CKEY KINE KINGAL FILL BANK /  INTERNAL
C  first KINE and VERT banks are booked and filled with parameters
C        sent as arguments (all vertices at the same position).
C  then  depending on the decay length of secondary particles , the
C        secondary vertices are displaced from the main vertex . The
C        propagation follows a straight line for neutral generating a
C        secondary vertec, and a simple helix for charged particles.
C        In case of charge particles generating a secondary vertex,
C        swim Px and Py of all secondaries up to decay vertex. Then
C        store the time of flight.
C        The magnetic field is assumed to be 15.0 Kgauss.
C
C - structure: SUBROUTINE subprogram
C              User Entry Name: KFEVBK
C              External References: KBVERT/KBKINE/KGPART/KGDECL(ALEPHLIB)
C              Comdecks referenced: BCS, ALCONS, KIPARA, BMACRO, KMACRO
C
C - Usage   : CALL KFEVBK (VMAIN,RPARA,IPARA,MTRAK,ISTAT)
C - Input   : VMAIN          = vx,vy,vz of the main vertex
C             RPARA (1-4,k)  = px,py,pz,(mass) of track(k)
C                              if RPARA(4,k)=0. then the mass is taken
C                              from the PART bank.
C             IPARA (1,k)    = vertex# of the origin of the track(k)
C                   (2,k)    = vertex# of the decay of the track(k)
C                                0 if there is no decay
C                   (3,k)    = ALEPH particle#
C             MTRAK          = # of tracks
C             ISTAT          = return code  ( 0 means OK)
C                              -1 means too many particles
C                              -2 means wrong KINE/VERT booking
C#ifndef DOC
      SAVE
C#include "bcs.h"
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C#include "alcons.h"
C! define universal constants
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
C#include "kipara.h"
CKEY KINE KINGAL DEFAULT
      PARAMETER (LHKIN=3, LPKIN=5, LKVX=2, LHVER=3, LPVER=5, LVKI=50)
      PARAMETER (LGDCA=32)
      PARAMETER (LRPART=200, LCKLIN=1)
      PARAMETER (LRECL=16020, LRUN=1, LEXP=1001, LRTYP=1000)
      CHARACTER*60 LTITL
      PARAMETER (LUCOD=0, LNOTRK=100, LTITL='KINGAL run')
      PARAMETER (LUTRK=450)
      PARAMETER (BFIEL=15., CFIEL=BFIEL*3.E-4)
      PARAMETER (CLITS = CLGHT * 1.E+9)
      INTEGER IPARA(3,*)
      REAL RPARA(4,*),VMAIN(3)
      REAL KGDECL
      LOGICAL FDECAY,FNEUTR
      DATA NAPAR /0/
      COMMON/comvert/xsec(4,10)  ! vertices without B field effects....
      real xsec
C#include "bmacro.h"
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C#include "kmacro.h"

C - index of the next vertex/track to be stored in KINE/VERT
C   bank known by its index JVK
      KNEXVK(JVK) = JVK + IW(JVK+1)+IW(JVK+2)+IW(JVK+3)
C - # of vertices/tracks which could be stored in KINE/VERT
C   bank known by its index JVK
      LFRVK(JVK)  = IW(JVK) - (IW(JVK+1)+IW(JVK+2)+IW(JVK+3))
C - index of the 1st parameter of KINE/VERT bank known by its
C   index JVK
      KPARVK(JVK) = JVK + IW(JVK+1)
C - index of 1st vertex/track # contained into the list of
C   bank KINE/VERT known by its index JVK
      KLISVK(JVK) = JVK + IW(JVK+1) + IW(JVK+2)
C - charge of ALEPH particle# JPA
      CHARGE(JPA) = RTABL(IW(NAPAR),JPA,7)
C - mass of ALEPH particle# JPA
      PARMAS(JPA) = RTABL(IW(NAPAR),JPA,6)
C - time of life of ALEPH particle# JPA
      TIMLIF(JPA) = RTABL(IW(NAPAR),JPA,8)
C - # of vertices on a track known by its BOS index JVK /
C   # of outgoing tracks of a vertex known by its BOS index JVK
      NOFVK(JVK)  = IW(JVK+3)
C - Particle type of a track known by its BOS index JVK
      KINTYP(JVK) = IW(KPARVK(JVK)+5)
C - incoming track # of a vertex known by its BOS index JVK
      INPTRK(JVK) = IW(KPARVK(JVK)+5)
C - origin vertex # of a track known by its BOS index JVK
      INPVRT(JVK) = IW(KLISVK(JVK)+1)
C - momentum of a track known by its BOS index JVK
      PMODVK(JVK) = SQRT (RW(KPARVK(JVK)+1)**2 + RW(KPARVK(JVK)+2)**2
     &                   +RW(KPARVK(JVK)+3)**2)
C - mass of a track known by its BOS index JVK
      PMASVK(JVK) = RW(KPARVK(JVK)+4)
C - energy of a track known by its BOS index JVK
      ENERVK(JVK) = SQRT (PMODVK(JVK)**2 + PMASVK(JVK)**2)
C - time of flight of the icoming particle to the vertex known by its
C   BOS index JVK
      TOFLIT(JVK) = RW(KPARVK(JVK)+4)
C - radius of the vertex known by its BOS index
      RADVK(JVK)  = SQRT (RW(KPARVK(JVK)+1)**2 + RW(KPARVK(JVK)+2)**2)
C - mother track # of a track known by its BOS index
      MOTHVK(JVK) = INPTRK (NLINK('VERT',INPVRT(JVK)))
C - alephlib version# used in KINGAL (NKJ is the name-index of KJOB)
C   the user should have called *CA KJOBJJ to use this function
      ALEKIN(NKJ) = ITABL(IW(NKJ),1,JKJOAV)
C
C
      FDECAY(JTR) = IPARA(2,JTR).GT.1 .AND. IPARA(2,JTR).NE.IPARA(1,JTR)
      FNEUTR(JPA) = ABS (CHARGE(JPA)) .LT. .1
C -------------------------------------------------------
      ISTAT = 0
C
C - Get 'PART' name-index at the 1st entry
      IF (NAPAR .EQ. 0) NAPAR = NAMIND ('PART')
C
C - Create main vertex bank
      IVMAI = 1
      JVERT = KBVERT (IVMAI,VMAIN,0)
      do ii=1,4
         xsec(ii,1) = vmain(ii)
      enddo
C
C - Fill VERT and KINE banks
      DO 1 NT = 1,MTRAK
         JKINE = KBKINE (NT,RPARA(1,NT),IPARA(3,NT),IPARA(1,NT))
         IF (JKINE.LE.0) GOTO 998
         IF (IPARA(2,NT).GT.0) THEN
            JVERT = KBVERT (IPARA(2,NT),VMAIN,NT)
            IF (JVERT.LE.0) GOTO 998
         ENDIF
 1    CONTINUE
C
C - Propagate secondary vertices if any
C
      DO 100 NT = 1,MTRAK
         IPART = IPARA(3,NT)
         PMOD = SQRT (RPARA(1,NT)**2+RPARA(2,NT)**2+RPARA(3,NT)**2)
         TLIF = TIMLIF (IPART)
         IF (RPARA(4,NT).EQ.0.) THEN
            ZMAS = PARMAS (IPART)
         ELSE
            ZMAS = RPARA(4,NT)
         ENDIF
C
         IF (FDECAY(NT)) THEN
            DCLEN = KGDECL (PMOD,ZMAS,TLIF)
            IF (DCLEN .LE. 0.) GOTO 100
C           get the origin vertex
            IVOR = IPARA(1,NT)
            JVOR = NLINK ('VERT',IVOR)
            KVO  = KPARVK (JVOR)
C           get the decay vertex
            IVOUT = IPARA(2,NT)
            JVERT = NLINK ('VERT',IVOUT)
            KVX   = KPARVK (JVERT)
            KVTR  = KLISVK (JVERT)
C
C           straight line for neutral generating a secondary vx
            IF (FNEUTR(IPART)) THEN
               DO 102 IX = 1,3
                  RW(KVX+IX) = RW(KVO+IX) + RPARA(IX,NT)*DCLEN/PMOD
             xsec(ix,ivout) =  xsec(ix,ivor) + RPARA(IX,NT)*DCLEN/PMOD
 102           CONTINUE
            ELSE
              do ix = 1,3
             xsec(ix,ivout) =  xsec(ix,ivor) + RPARA(IX,NT)*DCLEN/PMOD
              enddo
C
C          propagation according to a simple helix for charged
C
               RAD = PMOD / (CFIEL*CHARGE(IPART))
               DPSI = DCLEN / RAD
               DXDS = RPARA(1,NT) / PMOD
               DYDS = RPARA(2,NT) / PMOD
               DZDS = RPARA(3,NT) / PMOD
               CPSI = COS (DPSI)
               SPSI = SIN (DPSI)
               DX = RAD * (DXDS*SPSI + DYDS*(1.-CPSI))
               DY = RAD * (DYDS*SPSI - DXDS*(1.-CPSI))
               DZ = DCLEN * DZDS
               RW(KVX+1)  = RW(KVO+1) + DX
               RW(KVX+2)  = RW(KVO+2) + DY
               RW(KVX+3)  = RW(KVO+3) + DZ
C           swim Px and Py of all secondaries up to decay vertex
               MTVX = IW(JVERT+3)
               IF (MTVX .GT. 0) THEN
                  DO 103 N=1,MTVX
                     NS = IW (KVTR+N)
                     JKINE = NLINK ('KINE',NS)
                     IF (JKINE.EQ.0) GOTO 998
                     KTR = KPARVK (JKINE)
                     RW(KTR+1) = RPARA(1,NS)*CPSI - RPARA(2,NS)*SPSI
                     RW(KTR+2) = RPARA(1,NS)*SPSI + RPARA(2,NS)*CPSI
103              CONTINUE
               ENDIF
            ENDIF
C           Store the time of flight
            RW(KVX+4)  = RW(KVO+4) + DCLEN/CLITS
            xsec(4,ivout) = xsec(4,ivor) +  DCLEN/CLITS
         ENDIF
C
 100   CONTINUE
C
       GOTO 999
C
C - Error
C      unsuccessfull booking of VERT or KINE
 998   ISTAT = -2
C
C - End
 999   CONTINUE
       END
      SUBROUTINE KXHEAL(VMAIN,ISTATU,MVX,MTRK)
C ---------------------------------------------------------
C - B.Bloch-Devaux - J.Boucrot - F.Ranjard - 870516
C - arranged for HERWIG  E. Lange  891801
C   B.Bloch :modified to allow for change from Energy to mass in
C   KINE banks (April 89)
C - A.S.Thompson :modified February 1992 for quark radiation in HERWIG
C - A.S.Thompson :modified August 1992 to handle partons at the end of
C                                      the shower
C - A.S.Thompson :modified March 1993 for slight change in status codes
C - A.S.Thompson :modified December 1993 for additional processes
C - B.Bloch Sept 2000 : modified to prevent B0's to have two vertices
C - Fill    : PTRAK(ix,n)  = px,py,pz,E( or Mass from Alephlib 9.0) of
C                            track(n)
C                            if E or M=0.it will be filled by the system
C             IPVNU(1,n)   = origin vertex # of track(n)
C                  (2,n)   = decay vertex # of track(n)
C                             0 if no decay
C                  (3,n)   = ALEPH particle #
C             IPCOD(n)     = status and history code of track(n)
C                          = KS*1000000 + KH
C                            KS = HERWIG status code
C                            KH = history code
C                                 for particles = mother track imoth
C                                 for clusters 1000*imoth1+imoth2
C - Book    : KHIS bank filled with IPCOD(n)
C - Call    : KFEVBK (VMAIN,PTRAK,IPVNU,MTRK,JSTAT)
C             to book propagate the decay and fill VERT and KINE
C
C - structure: SUBROUTINE subprogram
C              User Entry Name: KXHEAL
C              External References: ALTABL/ALVERS/KFEVBK(ALEPHLIB)
C              Comdecks referenced: BCS, HETRCK
C
C - usage   : CALL KXHEAL (VMAIN,ISTATU,MVX,MTRK)
C - Input   : VMAIN = vx,vy,vz,tof of the primary vertex
C - Output  : ISTATU = status word ( = 0 means OK)
C                     - 2 means not enough space for VERT or KINE
C                     - 5 means interface error
C                     > 0 means unknown HERWIG particle# ISTATU
C             MVX   = # of vertices
C             MTRK  = # of tracks to be propagated ( no beam electrons )
C
C ------------------------------------------------------
C          ****COMMON BLOCK FILE FOR HERWIG VERSION 6.2****
C
C ALTERATIONS: Layout completely overhauled for 5.9
C
C
C New common blocks added for version 6.1:
C              HWCLUS,HWSUSY,HWRPAR,HWMINB
C
C New variables added for version 6.1:
C              OMHMIX,ET2MIX,PH3MIX,IOP4JT,NPRFMT,
C              PRNDEF,PRNTEX,PRNWEB,EFFMIN,GCUTME,
C              IOP4JT,NPRFMT                       see HWPRAM
C              Y4JT,DURHAM                         see HWHARD
C              QORQQB,QBORQQ                       see HWPROP
C              NRECO                               see HWUCLU
C              TXNAME                              see HWUNAM
C              PPCL,NCL,IDCL                       see HWCLUS
C              TANB,ALPHAH,COSBPA,SINBPA,COSBMA,
C              SINBMA,COSA,SINA,COSB,SINB,COTB,
C              ZMIXSS,ZMXNSS,ZSGNSS,LFCH,RFCH,
C              SLFCH,SRFCH,WMXUSS,WMXVSS,WSGNSS,
C              QMIXSS,LMIXSS,THETAT,THETAB,THETAL,
C              ATSS,ABSS,ALSS,MUSS,FACTSS,GHWWSS,
C              GHZZSS,GHDDSS,GHUUSS,GHWHSS,GHSQSS,
C              XLMNSS,RMMNSS,SENHNC,SUSYIN         see HWSUSY
C              LAMDA1,LAMDA2,LAMDA3,HRDCOL,RPARTY,
C              COLUPD                              see HWRPAR
C              PMBN1,PMBN2,PMBN3,PMBK1,PMBK2,
C              PMBM1,PMBM2,PMBP1,PMBP2,PMBP3       see HWMINB
C
C New parameters added for version 6.1:
C              NMXCL
C
C Parameter NMXRES raised to 500
C
C Scalar variables changed to arrays of size 2:
C              CLSMR,PSPLT,CLDIR                   see HWPRAM
C
C
C   NEW for HERWIG6.2
C
C New common blocks added for version 6.2:
C	       HWGRAV 				   see HWHGRV
C	       
C New variables added for version 6.2:
C              FIX4JT                              see HWIGIN
C
C New parameters added for version 6.2:
C	       DXRCYL,DXZMAX,DXRSPH	           see HWIGIN	
C	       GRVLAM,EMGRV,GAMGRV		   see HWHGRV
C
C New control flags added for version 6.2:
C              WZRFR                               see HWBJCO
C              IMSSM,IHIGGS                        see HWUINC
C
C New variable added for version 6.2:
C              PARITY                              see HWUINC
C
      IMPLICIT NONE
      DOUBLE PRECISION ZERO,ONE,TWO,THREE,FOUR,HALF
      PARAMETER (ZERO =0.D0, ONE =1.D0, TWO =2.D0,
     &           THREE=3.D0, FOUR=4.D0, HALF=0.5D0)
C
      DOUBLE PRECISION
     & ACCUR,AFCH,ALPFAC,ALPHEM,ANOMSC,ASFIXD,AVWGT,B1LIM,BETAF,BRFRAC,
     & BRHIG,BTCLM,CAFAC,CFFAC,CLDKWT,CLMAX,CLPOW,CLQ,CLSMR,CMMOM,COSS,
     & COSTH,CSPEED,CTHRPW,CTMAX,DECPAR,DECWT,DISF,DKLTM,EBEAM1,EBEAM2,
     & EMLST,EMMAX,EMMIN,EMPOW,EMSCA,ENHANC,ENSOF,EPOLN,ETAMIX,EVWGT,
     & EXAG,F0MIX,F1MIX,F2MIX,GAMH,GAMMAX,GAMW,GAMWT,GAMZ,GAMZP,GCOEF,
     & GEV2NB,GEV2MM,GPOLN,H1MIX,HBAR,HARDST,OMEGA0,PBEAM1,PBEAM2,PDIQK,
     & PGSMX,PGSPL,PHEP,PHIMIX,PHIPAR,PHOMAS,PIFAC,PLTCUT,PPAR,PPOLN,
     & PRECO,PRSOF,PSPLT,PTINT,PTMAX,PTMIN,PTPOW,PTRMS,PXRMS,PWT,Q2MAX,
     & Q2MIN,Q2POW,Q2WWMN,Q2WWMX,QCDL3,QCDL5,QCDLAM,QDIQK,QEV,QFCH,QG,
     & QLIM,QSPAC,QV,QWT,REPWT,RESN,RHOHEP,RHOPAR,RLTIM,RMASS,RMIN,
     & RSPIN,SCABI,SINS,SNGWT,SWEIN,SWTEF,SUD,THMAX,TLOUT,TMTOP,TMNISR,
     & TQWT,VCKM,VFCH,VGCUT,VHEP,VMIN2,VPAR,VPCUT,VQCUT,VTXPIP,VTXQDK,
     & WBIGST,WGTMAX,WGTSUM,WHMIN,WSQSUM,XFACT,XLMIN,XMIX,XMRCT,XX,
     & XXMIN,YBMAX,YBMIN,YJMAX,YJMIN,YMIX,YMRCT,YWWMAX,YWWMIN,ZBINM,
     & ZJMAX,ZMXISR,Y4JT,EFFMIN,PPCL,
     & TANB,ALPHAH,COSBPA,SINBPA,COSBMA,SINBMA,COSA,SINA,COSB,SINB,COTB,
     & ZMIXSS,ZMXNSS,ZSGNSS,LFCH,RFCH,SLFCH,SRFCH, WMXUSS,WMXVSS,WSGNSS,
     & QMIXSS,LMIXSS,THETAT,THETAB,THETAL,ATSS,ABSS,ALSS,MUSS,FACTSS,
     & GHWWSS,GHZZSS,GHDDSS,GHUUSS,GHWHSS,GHSQSS,
     & XLMNSS,RMMNSS,SENHNC,LAMDA1,LAMDA2,LAMDA3,
     & PMBN1,PMBN2,PMBN3,PMBK1,PMBK2,PMBM1,PMBM2,PMBP1,PMBP2,PMBP3,
     & OMHMIX,ET2MIX,PH3MIX,GCUTME,GRVLAM,EMGRV,GAMGRV,DXRCYL,DXRSPH,
     & DXZMAX,VIPWID
C
      INTEGER
     & CLDIR,IAPHIG,IBRN,IBSH,ICHRG,ICO,IDCMF,IDHEP,IDHW,IDK,IDKPRD,IDN,
     & IDPAR,IDPDG,IERROR,IFLAV,IFLMAX,IFLMIN,IHPRO,IMQDK,INHAD,INTER,
     & IOPDKL,IOPHIG,IOPREM,IPART1,IPART2,IPRINT,IPRO,IPROC,ISLENT,
     & ISPAC,ISTAT,ISTHEP,ISTPAR,JCOPAR,JDAHEP,JDAPAR,JMOHEP,JMOPAR,
     & JNHAD,LNEXT,LOCN,LOCQ,LRSUD,LSTRT,LWEVT,LWSUD,MAPQ,MAXER,MAXEV,
     & MAXFL,MAXPR,MODBOS,MODMAX,MODPDF,NBTRY,NCLDK,NCOLO,NCTRY,NDKYS,
     & NDTRY,NETRY,NEVHEP,NEVPAR,NFLAV,NGSPL,NHEP,NME,NMODES,NMXCDK,
     & NMXDKS,NMXHEP,NMXJET,NMXMOD,NMXPAR,NMXQDK,NMXRES,NMXSUD,NPAR,
     & NPRODS,NQDK,NQEV,NRES,NRN,NSPAC,NSTRU,NSTRY,NSUD,NUMER,NUMERU,
     & NWGTS,NZBIN,SUDORD,IOP4JT,HRDCOL,NMXCL,NCL,IDCL,NPRFMT,NRECO,
     & IMSSM,IHIGGS,PARITY,LRSUSY
C
      LOGICAL
     & AZSOFT,AZSPIN,BGSHAT,BREIT,CLRECO,COLISR,DKPSET,FROST,FSTEVT,
     & FSTWGT,GENEV,GENSOF,HARDME,HVFCEN,MAXDKL,MIXING,NOSPAC,NOWGT,
     & PRNDEC,PIPSMR,PRVTX,RSTAB,SOFTME,TMPAR,TPOL,USECMF,VTOCDK,VTORDK,
     & ZPRIME,RPARTY,COLUPD,PRNDEF,PRNTEX,PRNWEB,DURHAM,SUSYIN,WZRFR,
     & QORQQB,QBORQQ,FIX4JT
C
      CHARACTER*4
     & BDECAY
      CHARACTER*8
     & PART1,PART2,RNAME
      CHARACTER*20
     & AUTPDF
      CHARACTER*37
     & TXNAME
C
C New standard event common
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     & JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
C
C Beams, process and number of events
      COMMON/HWBEAM/IPART1,IPART2
      COMMON/HWBMCH/PART1,PART2
      COMMON/HWPROC/EBEAM1,EBEAM2,PBEAM1,PBEAM2,IPROC,MAXEV
C
C Basic parameters (and quantities derived from them)
      COMMON/HWPRAM/AFCH(16,2),ALPHEM,B1LIM,BETAF,BTCLM,CAFAC,CFFAC,
     & CLMAX,CLPOW,CLSMR(2),CSPEED,ENSOF,ETAMIX,F0MIX,F1MIX,F2MIX,GAMH,
     & GAMW,GAMZ,GAMZP,GEV2NB,H1MIX,PDIQK,PGSMX,PGSPL(4),PHIMIX,PIFAC,
     & PRSOF,PSPLT(2),PTRMS,PXRMS,QCDL3,QCDL5,QCDLAM,QDIQK,QFCH(16),QG,
     & QSPAC,QV,SCABI,SWEIN,TMTOP,VFCH(16,2),VCKM(3,3),VGCUT,VQCUT,
     & VPCUT,ZBINM,EFFMIN,OMHMIX,ET2MIX,PH3MIX,GCUTME,
     & IOPREM,IPRINT,ISPAC,LRSUD,LWSUD,MODPDF(2),NBTRY,NCOLO,NCTRY,
     & NDTRY,NETRY,NFLAV,NGSPL,NSTRU,NSTRY,NZBIN,IOP4JT(2),NPRFMT,
     & AZSOFT,AZSPIN,CLDIR(2),HARDME,NOSPAC,PRNDEC,PRVTX,SOFTME,ZPRIME,
     & PRNDEF,PRNTEX,PRNWEB
C
      COMMON/HWPRCH/AUTPDF(2),BDECAY
C
C Parton shower common (same format as /HEPEVT/)
      PARAMETER (NMXPAR=500)
      COMMON/HWPART/NEVPAR,NPAR,ISTPAR(NMXPAR),IDPAR(NMXPAR),
     & JMOPAR(2,NMXPAR),JDAPAR(2,NMXPAR),PPAR(5,NMXPAR),VPAR(4,NMXPAR)
C
C Parton polarization common
      COMMON/HWPARP/DECPAR(2,NMXPAR),PHIPAR(2,NMXPAR),RHOPAR(2,NMXPAR),
     & TMPAR(NMXPAR)
C
C Electroweak boson common
      PARAMETER (MODMAX=5)
      COMMON/HWBOSC/ALPFAC,BRHIG(12),ENHANC(12),GAMMAX,RHOHEP(3,NMXHEP),
     & IOPHIG,MODBOS(MODMAX)
C
C Parton colour common
      COMMON/HWPARC/JCOPAR(4,NMXPAR)
C
C other HERWIG branching, event and hard subprocess common blocks
      COMMON/HWBRCH/ANOMSC(2,2),HARDST,PTINT(3,2),XFACT,INHAD,JNHAD,
     & NSPAC(7),ISLENT,BREIT,FROST,USECMF,WZRFR
C
      COMMON/HWEVNT/AVWGT,EVWGT,GAMWT,TLOUT,WBIGST,WGTMAX,WGTSUM,WSQSUM,
     & IDHW(NMXHEP),IERROR,ISTAT,LWEVT,MAXER,MAXPR,NOWGT,NRN(2),NUMER,
     & NUMERU,NWGTS,GENSOF
C
      COMMON/HWHARD/ASFIXD,CLQ(7,6),COSS,COSTH,CTMAX,DISF(13,2),EMLST,
     & EMMAX,EMMIN,EMPOW,EMSCA,EPOLN(3),GCOEF(7),GPOLN,OMEGA0,PHOMAS,
     & PPOLN(3),PTMAX,PTMIN,PTPOW,Q2MAX,Q2MIN,Q2POW,Q2WWMN,Q2WWMX,QLIM,
     & SINS,THMAX,Y4JT,TMNISR,TQWT,XX(2),XLMIN,XXMIN,YBMAX,YBMIN,YJMAX,
     & YJMIN,YWWMAX,YWWMIN,WHMIN,ZJMAX,ZMXISR,IAPHIG,IBRN(2),IBSH,
     & ICO(10),IDCMF,IDN(10),IFLMAX,IFLMIN,IHPRO,IPRO,MAPQ(6),MAXFL,
     & BGSHAT,COLISR,FSTEVT,FSTWGT,GENEV,HVFCEN,TPOL,DURHAM,FIX4JT
C
C Arrays for particle properties (NMXRES = max no of particles defined)
      PARAMETER(NMXRES=500)
      COMMON/HWPROP/RLTIM(0:NMXRES),RMASS(0:NMXRES),RSPIN(0:NMXRES),
     & ICHRG(0:NMXRES),IDPDG(0:NMXRES),IFLAV(0:NMXRES),NRES,
     & VTOCDK(0:NMXRES),VTORDK(0:NMXRES),
     & QORQQB(0:NMXRES),QBORQQ(0:NMXRES)
C
      COMMON/HWUNAM/RNAME(0:NMXRES),TXNAME(2,0:NMXRES)
C
C Arrays for particle decays (NMXDKS = max total no of decays,
C                             NMXMOD = max no of modes for a particle)
      PARAMETER(NMXDKS=4000,NMXMOD=200)
      COMMON/HWUPDT/BRFRAC(NMXDKS),CMMOM(NMXDKS),DKLTM(NMXRES),
     & IDK(NMXDKS),IDKPRD(5,NMXDKS),LNEXT(NMXDKS),LSTRT(NMXRES),NDKYS,
     & NME(NMXDKS),NMODES(NMXRES),NPRODS(NMXDKS),DKPSET,RSTAB(0:NMXRES)
C
C Weights used in cluster decays
      COMMON/HWUWTS/REPWT(0:3,0:4,0:4),SNGWT,DECWT,QWT(3),PWT(12),
     & SWTEF(NMXRES)
C
C Parameters for cluster decays (NMXCDK = max total no of cluster
C                                         decay channels)
      PARAMETER(NMXCDK=4000)
      COMMON/HWUCLU/CLDKWT(NMXCDK),CTHRPW(12,12),PRECO,RESN(12,12),
     & RMIN(12,12),LOCN(12,12),NCLDK(NMXCDK),NRECO,CLRECO
C
C Variables controling mixing and vertex information
      COMMON/HWDIST/EXAG,GEV2MM,HBAR,PLTCUT,VMIN2,VTXPIP(4),XMIX(2),
     & XMRCT(2),YMIX(2),YMRCT(2),DXRCYL,DXRSPH,DXZMAX,VIPWID(3),IOPDKL,
     & MAXDKL,MIXING,PIPSMR
C
C Arrays for temporarily storing heavy-b,c-hadrons decaying partonicaly
C (NMXQDK = max no such decays in an event)
      PARAMETER (NMXQDK=20)
      COMMON/HWQDKS/VTXQDK(4,NMXQDK),IMQDK(NMXQDK),LOCQ(NMXQDK),NQDK
C
C Parameters for Sudakov form factors
C (NMXSUD= max no of entries in lookup table)
      PARAMETER (NMXSUD=1024)
      COMMON/HWUSUD/ACCUR,QEV(NMXSUD,6),SUD(NMXSUD,6),INTER,NQEV,NSUD,
     & SUDORD
C
      PARAMETER (NMXJET=200)
C
C SUSY parameters
      COMMON/HWSUSY/
     & TANB,ALPHAH,COSBPA,SINBPA,COSBMA,SINBMA,COSA,SINA,COSB,SINB,COTB,
     & ZMIXSS(4,4),ZMXNSS(4,4),ZSGNSS(4), LFCH(16),RFCH(16),
     & SLFCH(16,4),SRFCH(16,4), WMXUSS(2,2),WMXVSS(2,2), WSGNSS(2),
     & QMIXSS(6,2,2),LMIXSS(6,2,2),
     & THETAT,THETAB,THETAL,ATSS,ABSS,ALSS,MUSS,FACTSS,
     & GHWWSS(3),GHZZSS(3),GHDDSS(4),GHUUSS(4),GHWHSS(3),
     & GHSQSS(4,6,2,2),XLMNSS,RMMNSS,SENHNC(24),
     & IMSSM,IHIGGS,PARITY,LRSUSY,SUSYIN
C
C R-Parity violating parameters and colours
      COMMON /HWRPAR/ LAMDA1(3,3,3),LAMDA2(3,3,3),
     &                LAMDA3(3,3,3),HRDCOL(2,5),RPARTY,COLUPD
C
C Parameters for minimum bias/soft underlying event
      COMMON/HWMINB/
     & PMBN1,PMBN2,PMBN3,PMBK1,PMBK2,PMBM1,PMBM2,PMBP1,PMBP2,PMBP3
C
C Cluster common used by soft event routines
      PARAMETER (NMXCL=500)
      COMMON/HWCLUS/PPCL(5,NMXCL),IDCL(NMXCL),NCL
C
C  Parameters for resonant gravition production
      COMMON/HWGRAV/GRVLAM,EMGRV,GAMGRV

C
C also the following include files from the interface
      INTEGER LCOLS, LROWS, KROW, KNEXT, ITABL,LFRWRD, LFRROW
      REAL RTABL
      INTEGER ID, NRBOS, L
C some declarations in KMACRO
      INTEGER NAPAR,JPA,NLINK
      REAL PARMAS,TIMLIF
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C
      REAL PTRAK(4,NMXHEP),VMAIN(4)
      REAL TLIF
      REAL TLIMI
      PARAMETER(TLIMI = 1.E-15)
C
      INTEGER IPVNU(3,NMXHEP),IPCOD(NMXHEP)
      INTEGER IPVAL(2,NMXHEP),NEWTR(NMXHEP)
      INTEGER ALTABL
      INTEGER JP1,JP2,I,IT,MVX,MTRK
      INTEGER IST,NPARL,INOTR,NVER,ITRK,ITR,IHER,IPART,KS
      INTEGER ISTATU,IMOTH,IMOTH2,IGRAND,KSMOTH,KSGRAN,IFAIL,JKHIS
      INTEGER KGPART,KBKINE,NAMIND,NDAU
      LOGICAL LHVDEC,LCLUST,LPAJET,LBMIX
C
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C - mass of ALEPH particle# JPA
      PARMAS(JPA) = RTABL(IW(NAPAR),JPA,6)
C - time of life of ALEPH particle# JPA
      TIMLIF(JPA) = RTABL(IW(NAPAR),JPA,8)
C

C
      LPAJET(I) = I.EQ.141.OR.I.EQ.142.OR.I.EQ.143.OR.I.EQ.144
      LHVDEC(I) = I.EQ.196.OR.I.EQ.197.OR.I.EQ.199.OR.I.EQ.200
      LCLUST(I) = I.GE.183.AND.I.LE.186
      LBMIX(I,L) = (I.eq.200).and.(L.eq.1) ! mixing of B0's
C
      DATA NAPAR/0/
C
C initialization
C
      JP1 = 0
      JP2 = 0
      IST = 0
C
      IF (NAPAR.EQ.0) NAPAR = NAMIND('PART')
C
C Build array containing vertex # and particle # of each track
C
      NPARL = 0
      INOTR = 0
      NVER = 1
C
      DO 10 ITRK = 1,NHEP
C
        ITR = ITRK - INOTR
C
C get ALEPH particle#
C
        IHER = IDHW(ITRK)
        IPART = KGPART(IHER)
C
        IF (IPART.LE.0) GOTO 998
C
C store momenta
C
        DO 9 I = 1,3
          PTRAK(I,ITR) = SNGL(PHEP(I,ITRK))
   9    CONTINUE
C
C KBKINE needs the mass of the particles instead of the energy
C
        PTRAK(4,ITR) = SNGL(PHEP(5,ITRK))
C
C store new line number
C
        NEWTR(ITRK) = ITR
C
        IPVNU(3,ITR) = IPART
        ipcod(itr) = 0
        KS = ISTHEP(ITRK)
        IMOTH = JMOHEP(1,ITRK)
        IF(IMOTH.GT.0) THEN
          KSMOTH=ISTHEP(IMOTH)
          if(imoth.lt.itrk) IPCOD(ITR) = NEWTR(IMOTH)
          IGRAND=JMOHEP(1,IMOTH)
          IF(IGRAND.GT.0) KSGRAN=ISTHEP(IGRAND)
        ENDIF
C just in case this is the mother of an initial state track
        IPVAL(2,ITRK)=1
C
C now we have look for different status codes to decide what
C we want to do
C
        IF(KS.EQ.3.OR.KS.EQ.100.OR.KS.EQ.121.OR.KS.EQ.122) THEN
C special HERWIG objects - also virtual photons from 2-photon events
C we just skip them
C
          INOTR = INOTR + 1
C
        ELSEIF (KS.EQ.101.OR.KS.EQ.102) THEN
C
C beam electrons are stored with negative numbers in the KINE bank
C
          IST = KBKINE(-ITRK,PTRAK(1,ITR),IPART,0)
C
          IF (IST.LE.0) THEN
            IHER = -2
            GOTO 998
          ENDIF
C
          INOTR = INOTR + 1
C just in case this is the mother of an initial state track
          IPVAL(2,ITRK)=1
C
        ELSEIF (KS.EQ.103.OR.KS.EQ.110) THEN
C
C Normally just the mass frame, but use it as the main vertex if it has
C daughters
C
          IF(JDAHEP(1,ITRK)*JDAHEP(2,ITRK).NE.0) THEN
            IPVNU(1,ITR) = 1
            IPVNU(2,ITR) = IPVNU(1,ITR)
            IPVAL(1,ITRK) = IPVNU(1,ITR)
            IPVAL(2,ITRK) = IPVNU(2,ITR)
            NEWTR(ITRK) = ITRK - INOTR
            IPCOD(ITR) = 0
            KS = 120
            NPARL = NPARL + 1
          ELSE
            INOTR = INOTR + 1
          ENDIF
        ELSEIF (KS.EQ.120) THEN
C
C this should be the Z0/gamma or W+,W-
C
C could also be the CMF in 2-photon events
          if((iher.lt.198.or.iher.gt.201).and.
     &       (iher.ne.14.and.iher.ne.15)) then
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ found illegal exchange''
     &       ,'' boson : HERWIG particle# = '',I5,'' in event ''
     &       ,2I6)')IHER,NEVHEP,KS
            ISTATU= -5
            GOTO 999
          ENDIF
C
C create primary vertex
C
          IPVNU(1,ITR) = 1
          IPVNU(2,ITR) = IPVNU(1,ITR)
C
          IPVAL(1,ITRK) = IPVNU(1,ITR)
          IPVAL(2,ITRK) = IPVNU(2,ITR)
C
C define new track number
C and store in KINE bank
C
          NEWTR(ITRK) = ITRK - INOTR
          IPCOD(ITR) = 0
C
          NPARL = NPARL + 1
C
        ELSEIF (KS.EQ.170) THEN
C
C could be the CMF in 2-photon events
          if(iher.ne.16) then
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ found illegal exchange''
     &         ,'' boson : HERWIG particle# = '',I5,'' in event ''
     &         ,2I6)')IHER,NEVHEP,KS
            ISTATU= -5
            GOTO 999
          ENDIF
          if(ksgran.eq.3.and.idhw(igrand).eq.59) then
C
C create primary vertex
C
            IPVNU(1,ITR) = 1
            IPVNU(2,ITR) = IPVNU(1,ITR)
C
            IPVAL(1,ITRK) = IPVNU(1,ITR)
            IPVAL(2,ITRK) = IPVNU(2,ITR)
C
C define new track number
C and store as through a HARD object
C
            NEWTR(ITRK) = ITRK - INOTR
            IPCOD(ITR) = 0
            KS = 120
          else
            IPVNU(1,ITR) = IPVAL(2,IMOTH)
            IPVNU(2,ITR) = IPVNU(1,ITR)
C
            IPVAL(1,ITRK) = IPVNU(1,ITR)
            IPVAL(2,ITRK) = IPVNU(2,ITR)
          endif
C
          NPARL = NPARL + 1
C
        ELSEIF (KS.GE.123.AND.KS.LE.124) THEN
C
C outgoing partons will be linked to their mother
C
C they should come from the exchange boson or a heavy quark before decay
C Force a main vertex just in case
          if(ksmoth.eq.120) IPVAL(2,IMOTH) = 1
          IPVNU(1,ITR) = IPVAL(2,IMOTH)
          IPVNU(2,ITR) = IPVNU(1,ITR)
C
          IPVAL(1,ITRK) = IPVNU(1,ITR)
          IPVAL(2,ITRK) = IPVNU(2,ITR)
C
C we don't keep them in the KINE bank
C AST we do now ! 15.01.1998
C         INOTR = INOTR + 1
          IPCOD(ITR) = NEWTR(IMOTH )
          NPARL = NPARL + 1
C
        ELSEIF(KS.EQ.125) THEN
C
C spectator parton after processing
C it should come from a decayed heavy flavour hadron
C
          IF (KSMOTH.NE.199) THEN
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ spectator parton, ''
     &         ,''ISTHEP = ''
     &         ,I3,'' has mother, ISTHEP =  '',I3,'' in event '',I6)')
     &         KS,KSMOTH,NEVHEP
            ISTATU = -5
            GOTO 999
          ENDIF
C
          IPVNU(1,ITR) = IPVAL(2,IMOTH)
          IPVNU(2,ITR) = IPVNU(1,ITR)
C
          IPVAL(1,ITRK) = IPVNU(1,ITR)
          IPVAL(2,ITRK) = IPVNU(2,ITR)
C
C  we don't want to track it
C
          INOTR = INOTR + 1
C
        ELSEIF (LPAJET(KS)) THEN
C
C outgoing parton jets (KS = 141 - 144)
C they should come from their outgoing partons
C
          IF (KSMOTH+20.NE.KS) THEN
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ jet, ISTHEP = ''
     &         ,I3,'' has mother, ISTHEP =  '',I3,'' in event '',I6)')
     &         KS,KSMOTH,NEVHEP
            ISTATU = -5
            GOTO 999
          ENDIF
C
C and the grand should be the Z0/gamma or from a heavy quark
C before decay
C
C basically taus from ZZ,ZH events
          IF(KSGRAN.NE.120.AND.KSGRAN.NE.155.AND.KSGRAN.NE.195.AND.
     &     .NOT.LHVDEC(KSGRAN)) THEN
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ jet, ISTHEP = '',I3,
     &      '' has grandma, ISTHEP =  '',I3,'' in event '',I6)')
     &      KS,KSGRAN,NEVHEP
            ISTATU = -5
            GOTO 999
          ENDIF
C
C this must not be any particle which might be tracked by GEANT
C
          IF (IHER.GT.13) THEN
C could be a virtual photon
            if(iher.eq.59) then
C ignore it, but give a warning if it has daughters, this could compromi
C succeeding mother-daughter relationships
              INOTR=INOTR+1
              if(jdahep(1,i).gt.0.or.jdahep(2,i).gt.0)
     &        WRITE (IW(6),'(/1X,''+++KXHEAL+++ Virtual photon has'',
     &        '' daughters in event '',i6)') nevhep
            else
              WRITE (IW(6),'(/1X,''+++KXHEAL+++ jet, ISTHEP = '',I3,
     &        '' is not a quark in event '',I6)') KS,NEVHEP
              ISTATU = -5
              GOTO 999
            ENDIF
          Else
C
C just possible that the mother had appeared before the grand for
C the hard process, force this to be the primary vertex anyway
C AST 15.01.1998 Now NOT if we keep their mothers
C            if(ksgran.eq.120) IPVAL(2,IMOTH) = 1
            IPVNU(1,ITR) = IPVAL(2,IMOTH)
            IPVNU(2,ITR) = IPVNU(1,ITR)
C
            IPVAL(1,ITRK) = IPVNU(1,ITR)
            IPVAL(2,ITRK) = IPVNU(2,ITR)
C
C store them in the KINE bank
C KS = 123,124 is not stored in KINE, we have to link to the grands
C AST 15.01.1998 But link to their mothers if they ARE kept
            IPCOD(ITR) = NEWTR(IMOTH )
            NPARL = NPARL + 1
          endif
C
        ELSEIF (KS.EQ.161.OR.KS.EQ.162) then
C
C beam or target spectators after gluon splitting
C
          IF (KSMOTH+20.NE.KS) THEN
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ jet, ISTHEP = ''
     &         ,I3,'' has mother, ISTHEP =  '',I3,'' in event '',I6)')
     &         KS,KSMOTH,NEVHEP
            ISTATU = -5
            GOTO 999
          ENDIF
C
          IPVNU(1,ITR) = IPVAL(2,IMOTH)
          IPVNU(2,ITR) = IPVNU(1,ITR)
C
          IPVAL(1,ITRK) = IPVNU(1,ITR)
          IPVAL(2,ITRK) = IPVNU(2,ITR)
C
C store them in the KINE bank
C

          NPARL = NPARL + 1
C
        ELSEIF(KS.EQ.155) THEN
C
C heavy quark or tau before decay
C
          IF (.NOT.(IHER.LE.12.OR.IHER.EQ.125.OR.IHER.EQ.131)) THEN
            WRITE(IW(6),'(/1X,''+++KXHEAL+++ heavy quark or tau ''
     &                 ,'' before decay has IHER = '',I3
     &                 ,'' in event '',I6)')IHER,NEVHEP
            ISTATU = -5
            GOTO 999
          ENDIF
C
          IF (KSMOTH.NE.199.AND.KSMOTH.NE.124.AND.KSMOTH.NE.123) THEN
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ part. ISTHEP = ''
     &         ,I3,'' has mother, ISTHEP =  ''
     &         ,I3,'' in event '',I6)')KS,KSMOTH,NEVHEP
            ISTATU = -5
            GOTO 999
          ENDIF
C
          IPVNU(1,ITR) = IPVAL(2,IMOTH)
          IPVNU(2,ITR) = IPVNU(1,ITR)
C
          IPVAL(1,ITRK) = IPVNU(1,ITR)
          IPVAL(2,ITRK) = IPVNU(2,ITR)
C
          NPARL = NPARL + 1
C
C update history code
C
          IF (KSMOTH.EQ.199) THEN
C
            IPCOD(ITR) = NEWTR(IMOTH)
C
          ELSE
C
C KS = 123,124 is not stored in KINE, we have to link to the grands
C
            IPCOD(ITR) = NEWTR(IGRAND)
C
          ENDIF
C
        elseif((ks.ge.157.and.ks.le.159).or.ks.eq.149.or.
     &                                      ks.eq.162) then
C
          IF (IHER.GT.13.AND.IHER.NE.59) THEN
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ particle from splitting''
     &      ,'' is not a parton IHER= '',I3,'' KS= '',I3
     &      ,'' in event '',I7)') IHER,KS,NEVHEP
            ISTATU = -5
            GOTO 999
          ENDIF
C
          IF (KS.EQ.157.AND..NOT.LPAJET(KSMOTH)) THEN
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ parton from ''
     &         ,'' QCD branching not from parton jet ''
     &         ,'' in event '',I6)')NEVHEP
            ISTATU = -5
            GOTO 999
          ENDIF
C
          IF (KS.EQ.159.AND..NOT.LPAJET(KSMOTH)) THEN
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ parton from ''
     &         ,'' cluster splitting not from parton jet ''
     &         ,'' in event '',4I6)')NEVHEP,itr,ks,ksmoth
            ISTATU = -5
            GOTO 999
          ENDIF
C AST ignore FSR photons appearing here
          IF(IHER.EQ.59) THEN
            INOTR=INOTR+1
          else
            IPVNU(1,ITR) = IPVAL(2,IMOTH)
            IPVNU(2,ITR) = IPVNU(1,ITR)
C
            IPVAL(1,ITRK) = IPVNU(1,ITR)
            IPVAL(2,ITRK) = IPVNU(2,ITR)
C
            NPARL = NPARL + 1
C
C update history code
C
            IPCOD(ITR) = NEWTR(IMOTH)
          ENDIF
C
        ELSEIF (KS.EQ.160) THEN
C
C spectator after heavy decay, just link
C
          IF (.NOT.(IHER.LE.12.OR.(IHER.GE.109.AND.IHER.LE.120))) THEN
            WRITE(IW(6),'(/1X,''+++KXHEAL+++ spectator after ''
     &         ,''heavy decay is not a (di)quark, IHER = '',I3
     &         ,'' in event '',I6)')IHER,NEVHEP
            ISTATU = -5
            GOTO 999
          ENDIF
C
C it should come from a spectator parton
C
          IF (KSMOTH.NE.125) THEN
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ spectator after ''
     &         ,''heavy decay, ISTHEP = ''
     &         ,I3,'' has mother, ISTHEP = '',I3,'' in event '',I6)')
     &         KS,KSMOTH,NEVHEP
            ISTATU = -5
            GOTO 999
          ENDIF
C
          IPVNU(1,ITR) = IPVAL(2,IMOTH)
          IPVNU(2,ITR) = IPVNU(1,ITR)
C
          IPVAL(1,ITRK) = IPVNU(1,ITR)
          IPVAL(2,ITRK) = IPVNU(2,ITR)
C
          NPARL =  NPARL + 1
C
C update history code
C
          IF (KSGRAN.NE.199) THEN
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ quark, ISTHEP = ''
     &         ,I3,'' has mother, ISTHEP =  '',I3,'' in event '',I6)')
     &         KS,KSGRAN,NEVHEP
            ISTATU = -5
            GOTO 999
          ENDIF
C
          IPCOD(ITR) = NEWTR(IGRAND)
C
        ELSEIF (KS.EQ.167.or.ks.eq.168) THEN
C
C clusters from beam or target
C just ignore if from virtual photon
C
          if(ksmoth.eq.3.and.idhw(imoth).eq.59) then
            inotr = inotr + 1
          else
            IPVNU(1,ITR) = IPVAL(2,IMOTH)
            IPVNU(2,ITR) = IPVNU(1,ITR)
C
            IPVAL(1,ITRK) = IPVNU(1,ITR)
            IPVAL(2,ITRK) = IPVNU(2,ITR)
C
            nparl = nparl + 1
          endif
C
        ELSEIF (KS.EQ.183.or.ks.eq.163.or.
     +        ((ks.eq.185.or.ks.eq.184).and.ksmoth.ne.170)) THEN
C
C this is a hard cluster or soft cluster from DIS event
C find it's partons first
C
          IMOTH2 = JMOHEP(2,ITRK)
C
C we check that the partons are coming from the same vertex
C
          IF (IPVAL(2,IMOTH).NE.IPVAL(2,IMOTH2).AND.
     &        INT(IPROC/10).ne.900) THEN
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ cluster particles''
     &         ,'' ITRK = '',I4,I4,'' of cluster ITRK = '',I4
     &         ,'' do not come from the same vertex in event ''
     &         ,I6)')JP1,JP2,ITRK,NEVHEP
            WRITE(IW(6),'(1X,''Vertices are'',2I4)') IPVAL(2,IMOTH),
     &       IPVAL(2,IMOTH2)
            ISTATU = -5
            GOTO 999
          ENDIF
C
          IPVNU(1,ITR) = IPVAL(2,IMOTH)
          IPVNU(2,ITR) = IPVNU(1,ITR)
C
          IPVAL(1,ITRK) = IPVNU(1,ITR)
          IPVAL(2,ITRK) = IPVNU(2,ITR)
C
          NPARL = NPARL + 1
C
C update history, we may have two different mothers
C
          IPCOD(ITR) = NEWTR(IMOTH)*1000 + NEWTR(IMOTH2)
C
        ELSEIF (KS.EQ.184.or.ks.eq.185.OR.KS.EQ.186) THEN
C
C clusters from soft interactions
C
          IF (KSMOTH.NE.170) THEN
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ soft cluster''
     &         ,'' not from soft process, ISTHEP = ''
     &         ,I3,'' has mother, ISTHEP =  '',I3,'' in event '',I6)')
     &         KS,KSMOTH,NEVHEP
            ISTATU = -5
            GOTO 999
          ENDIF
C
          IPVNU(1,ITR) = IPVAL(2,IMOTH)
          IPVNU(2,ITR) = IPVNU(1,ITR)
C
          IPVAL(1,ITRK) = IPVNU(1,ITR)
          IPVAL(2,ITRK) = IPVNU(2,ITR)
C
C  we don't want to track it
C
C          INOTR = INOTR + 1
           nparl = nparl + 1
C
        ELSEIF (KS.GE.195.AND.KS.LE.200) THEN
C
C these are unstable leptons and hadrons
C link them, propagate if necessary
C
C direct unstable hadrons and decayed heavy flavour hadrons
C should come from the cluster
C
C         IF ((KS.EQ.196.OR.KS.EQ.197.OR.KS.EQ.199.OR.KS.EQ.200)
C    &    .AND.(KSMOTH.LT.183.OR.KSMOTH.GT.186).AND.KSMOTH.NE.197)THEN
          IF (LHVDEC(KS).AND.(.NOT.KSMOTH.EQ.198.AND.
     &    .NOT.LHVDEC(KSMOTH).AND..NOT.LCLUST(KSMOTH))) THEN
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ unstab. hadr. or ''
     &         ,''dec. heavy flav. hadr. not from clus. ''
     &         ,'' in event '',2I6)')NEVHEP
            write(6,*) 'track',itrk,' mother,grand ',ksmoth,ksgran
            ISTATU = -5
            GOTO 999
          ENDIF
C
C indirect unstable hadrons or leptons should come from direct
C or indirect unstable hadrons or direct unstable leptons
C
          IF (KS.EQ.198.AND.(KSMOTH.LT.195.OR.KSMOTH.GT.200)) THEN
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ indir unstab. hadr. ''
     &         ,'' or lept. not from dir. unstab. hadr.''
     &         ,'' in event '',I6)')NEVHEP
            write(6,*) 'track',itrk,' mother',ksmoth
            ISTATU = -5
            GOTO 999
          ENDIF
C
C direct unstable leptons should come from outgoing partons and their
C grandma should be a heavy quark before decay
C AST allow Z,H from initial process as well
C
          IF (KS.EQ.195.and.ksmoth.ne.110) THEN
            IF (KSMOTH.NE.123.AND.KSMOTH.NE.124.AND.KSMOTH.NE.120.AND.
     &          KSMOTH.NE.110) THEN
              WRITE (IW(6),'(/1X,''+++KXHEAL+++ dir unstab. lept. '',
     &         '' not from outgoing partons in event '',I6)')NEVHEP
              ISTATU = -5
              GOTO 999
            ENDIF
            IF (KSGRAN.NE.155.AND.KSGRAN.NE.120.AND.KSGRAN.NE.195.AND.
     &          KSGRAN.NE.3.AND.(IHER.LT.198.OR.IHER.GT.201)) THEN
C basically taus from ZZ,ZH AND GAMMA-GAMMA events
              WRITE (IW(6),'(/1X,''+++KXHEAL+++ dir unstab. lept. ''
     &           ,'' not from heavy quark before decay''
     &           ,'' in event '',I6)')NEVHEP
              ISTATU = -5
              GOTO 999
            ENDIF
          ENDIF
C
          IPVNU(1,ITR) = IPVAL(2,IMOTH)
C
          TLIF = TIMLIF(IPART)
          NDAU = JDAHEP(2,ITRK)-JDAHEP(1,ITRK)+1
          
C
          IF ((TLIF.GT.TLIMI).AND.(.not.LBMIX(KS,NDAU))) THEN
C
C these particles will be propagated
C
            NVER = NVER + 1
            IPVNU(2,ITR) = NVER
C
          ELSE
C
C these particles will decay immediately
C link them to the main vertex
C
            IPVNU(2,ITR) = IPVNU(1,ITR)
C
          ENDIF
C
          IPVAL(1,ITRK) = IPVNU(1,ITR)
          IPVAL(2,ITRK) = IPVNU(2,ITR)
C
          NPARL = NPARL + 1
C
C update the history code
C for the direct unstable leptons (tau's) we link directly
C to the heavy quark before decay
C WE HAVE KEPT THESE HEAVY QUARKS NOW
C          IF (KS.EQ.195) THEN
C            IPCOD(ITR) = NEWTR(IGRAND)
C          ELSE
            IPCOD(ITR) = NEWTR(IMOTH)
C          ENDIF
C
        ELSEIF (KS.EQ.1) THEN
C
C final state particles, just link them
C
          IMOTH2= JMOHEP(2,ITRK)
C AST
          IF(IDHW(IMOTH2).LE.12.AND.IHER.EQ.59.AND.
     &     IDHW(IMOTH).EQ.59.AND.KSMOTH.EQ.157) THEN
C AST
C final state photons . drop them since they have appeared in the
C parton shower
            INOTR = INOTR + 1
          else
C spectator e+e- from 2-photon events
            if(isthep(imoth).eq.103.and.iher.eq.59) then
C           if((isthep(imoth).eq.103.and.iher.eq.59).or.
C    &        ((iher.eq.121.or.iher.eq.127.).and.
C    &        (isthep(imoth).eq.101.or.isthep(imoth).eq.102)))then
C AST
C initial state photons are linked to the main vertex
C
              IPVNU(1,ITR) = 1
C
            ELSE
C
C other particles are linked to the decay vertex of their mother
C
              IPVNU(1,ITR) = IPVAL(2,IMOTH)
C
            ENDIF
C
C no decay
C
            IPVNU(2,ITR) = 0
C
            IPVAL(1,ITRK) = IPVNU(1,ITR)
            IPVAL(2,ITRK) = IPVNU(2,ITR)
C
            NPARL = NPARL + 1
C
C update the history code
C
C           IF(KSMOTH.EQ.123.OR.KSMOTH.EQ.124) THEN
C
C leptons from heavy quarks
C WE HAVE KEPT THESE HEAVY QUARK NOW
C              IPCOD(ITR) = NEWTR(IGRAND)
C
            IF (KSMOTH.EQ.103.AND.IHER.EQ.59) THEN
C
C initial state photons are initial particles
C
              IPCOD(ITR) = 0
C
            ELSE
C
              IPCOD(ITR) = NEWTR(IMOTH)
C
            ENDIF
          ENDIF

C
        ELSEIF (KS.EQ.2) THEN
C
C AST parton before hadronization. Needed if work required at parton
C level. This can also apply to partons from heavy quark decay so
C give those partons whose greatgrandmother is the Z a 2nd mother
C
          IF(IHER.LE.13.OR.IHER.EQ.59) THEN
            IMOTH2= JMOHEP(2,ITRK)
            IPCOD(ITR) = NEWTR(IMOTH)
C
            IF (ISTHEP(JMOHEP(1,IGRAND)).EQ.120)
     &       IPCOD(ITR)=IPCOD(ITR)+1000*NEWTR(IGRAND)
C
C we link it to its mother
C
            IPVNU(1,ITR) = IPVAL(2,IMOTH)
            IPVNU(2,ITR) = IPVNU(1,ITR)
            IF(IHER.EQ.59) IPVNU(2,ITR)=0
C
            IPVAL(1,ITRK) = IPVNU(1,ITR)
            IPVAL(2,ITRK) = IPVNU(2,ITR)
C
C  we don't want to track it
C
            NPARL = NPARL + 1
C non-partons
          else
            WRITE (IW(6),'(/1X,''+++KXHEAL+++ particle in parton ''
     &      ,''shower is not a parton IHER= '',I3,'' KS= '',I3
     &      ,'' in event '',I7)') iher,ks,nevhep
            ISTATU = -5
            GOTO 999
          endif
C
        ELSE
C
          WRITE (IW(6),'(/1X,''+++KXHEAL+++ non interfaced''
     &         ,'' HERWIG status code found = '',I3,'' in event ''
     &         ,I6)') KS,NEVHEP
          ISTATU = -5
          GOTO 999
C
        ENDIF
C
C add status code
C
        IPCOD(ITR) = IPCOD(ITR) + KS*1000000
C
 10   CONTINUE
C
C Propagate decays and fill KINE and VERT banks
C
      CALL KFEVBK(VMAIN,PTRAK,IPVNU,NPARL,IFAIL)
C
C Fill history bank KHIS
C
      JKHIS = ALTABL ('KHIS',1,NPARL,IPCOD,'I','E')
C
      MVX = NVER
      MTRK = NPARL
      ISTATU = IFAIL
      if(ifail.ne.0) write(iw(6),*)'+++KXHEAL+++ KFEVBK error',ifail
      GOTO 999
C
C Error
C
 998  ISTATU = IHER
C
 999  RETURN
      END

C...This file contains routines for Bose-Einstein studies.
C...The routines are intended to replace the standard LUBOEI routine
C...found in JETSET 7.4.
C...The physics of the routines is described in
C...Leif Lonnblad and Torbjorn Sjostrand,
C..."Modelling Bose-Einstein correlations at LEP 2"
C...preprint no. LU TP 97-30, NORDITA-97/75 P and hep-ph/9711460, 
C...to appear in European Physical Journal C.
C...Here you also find further information underlying the meaning
C...of the switches below and how to combine them sensibly.

C*********************************************************************
C*********************************************************************
C...MSTJ(53)      In e+e- -> W+W-, apply BE algorithm
C...         0 => on all pion pairs
C...         1 => only on pairs were both pions come from the same W
C...         2 => only on pairs were the pions come from different Ws
C...        -1 => on all pairs except unequal pions coming from
C...              different Ws
C...        -2 => When calculating balancing shifts for pions from same
C...              W, only consider pairs from this W
C...MSTJ(54)      Alternative local energy compensation
C...         0 => Global energy compensation
C...         1 => Compensate with identical pairs by negative BE
C...              enhancement with a third of the radius
C...        -5 => Compansate with pair giving the smallest string length
C...        -2 => Compensate with pair giving the smallest invariant mass
C...MSTJ(55)      Calculation of difference vector
C...         0 => In the lab frame
C...         1 => In the CMS of the given pair.
C...MSTJ(56)      In e+e- -> W+W-, include distance between W's
C...         0 => Radius is the same for all pairs
C...         1 => Radius for pairs from different W's is R+deltaR_WW
C...MSTJ(57)      Penalty for shifting particles with close by identical
C...              neighbors in local energy compensation.
C...         0 => No penalty
C...         1 => Penalty
C...PARJ(95)(R)   Set to the energy imbalance after the BE algorithm,
C...              before rescaling of momenta.
C...PARJ(96)(R)   Set to the alpha needed to retain energy-momentum
C...              conservation in each event for relevant models.
C...PARJ(97)(D=0) If larger than 0 used to get alternative shape for
C...              MSTJ(54)=1
C*********************************************************************

      SUBROUTINE LUBOEIW(NSAV)

C...Purpose: to modify event so as to approximately take into account
C...Bose-Einstein effects according to a simple phenomenological
C...parametrization.
      IMPLICIT DOUBLE PRECISION(D)
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      SAVE /LUJETS/,/LUDAT1/
      COMMON /LUDAT2/ KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      SAVE /LUDAT2/
      DIMENSION DPS(4),KFBE(9),NBE(0:10),BEI(100),BEI3(100),
     $     BEIW(100),BEI3W(100)    
      REAL*4 DUM
      DATA KFBE/211,-211,111,321,-321,130,310,221,331/

      SDIP(I,J)=((P(I,4)+P(J,4))**2-(P(I,3)+P(J,3))**2-
     $     (P(I,2)+P(J,2))**2-(P(I,1)+P(J,1))**2)
C      print *,' entering LUBOEI',nsav,mstj(51)
CBB      call lulist(1)
C...Boost event to overall CM frame. Calculate CM energy.
      IF((MSTJ(51).NE.1.AND.MSTJ(51).NE.2).OR.N-NSAV.LE.1) RETURN
      DO 100 J=1,4
      DPS(J)=0.
  100 CONTINUE
      DO 120 I=1,N
      KFA=IABS(K(I,2))
      IF(K(I,1).LE.10.AND.((KFA.GT.10.AND.KFA.LE.20).OR.KFA.EQ.22).AND.
     &K(I,3).GT.0) THEN
        KFMA=IABS(K(K(I,3),2))
        IF(KFMA.GT.10.AND.KFMA.LE.80) K(I,1)=-K(I,1)
      ENDIF
      IF(K(I,1).LE.0.OR.K(I,1).GT.10) GOTO 120
      DO 110 J=1,4
      DPS(J)=DPS(J)+P(I,J)
  110 CONTINUE
  120 CONTINUE
C   protect against nul dps(4) ( energy of the boost)
C      if ( dps(4).le.0.) return
      CALL LUDBRB(0,0,0.,0.,-DPS(1)/DPS(4),-DPS(2)/DPS(4),
     &-DPS(3)/DPS(4))
      PECM=0.
      DO 130 I=1,N
      IF(K(I,1).GE.1.AND.K(I,1).LE.10) PECM=PECM+P(I,4)
  130 CONTINUE
C       print *,'n PECM',n,pecm
C...Reserve copy of particles by species at end of record.
      IWP=0
      IWN=0
      NBE(0)=N+MSTU(3)
      NMAX=NBE(0)
      SMMIN=PECM
      DO 160 IBE=1,MIN(10,MSTJ(52)+1)
      NBE(IBE)=NBE(IBE-1)
      DO 150 I=NSAV+1,N
        IF (IBE.EQ.MIN(10,MSTJ(52)+1)) THEN
          DO 152 IIBE=1,IBE-1
            IF (K(I,2).EQ.KFBE(IIBE)) GOTO 150
 152      CONTINUE
        ELSE
          IF(K(I,2).NE.KFBE(IBE)) GOTO 150
        ENDIF
      IF(K(I,1).LE.0.OR.K(I,1).GT.10) GOTO 150
      IF(NBE(IBE).GE.MSTU(4)-MSTU(32)-5) THEN
        CALL LUERRM(11,'(LUBOEI:) no more memory left in LUJETS')
        RETURN
      ENDIF
      NBE(IBE)=NBE(IBE)+1
      NMAX=NBE(IBE)
      K(NBE(IBE),1)=I
      K(NBE(IBE),5)=0
      SMMIN=MIN(SMMIN,P(I,5))
      IF (MSTJ(53).NE.0.OR.MSTJ(56).GT.0) THEN
        IM=I
 151    IF (K(IM,3).GT.0) THEN
          IM=K(IM,3)
          IF (ABS(K(IM,2)).NE.24) GOTO 151
          K(NBE(IBE),5)=K(IM,2)
          IF (IWP.EQ.0.AND.K(IM,2).EQ.24) IWP=IM
          IF (IWN.EQ.0.AND.K(IM,2).EQ.-24) IWN=IM
        ENDIF
      ENDIF
      DO 140 J=1,3
      P(NBE(IBE),J)=0.
      V(NBE(IBE),J)=0.
  140 CONTINUE
      P(NBE(IBE),5)=-1.0
  150 CONTINUE
  160 CONTINUE
C        print * ,'NBE',nbe        
      npip = nbe(1)-nbe(0)
      npim = nbe(2)-nbe(1)
      npi0 = nbe(3)-nbe(2)
      call hfill(201,float(npip),dum,1.)
      call hfill(202,float(npim),dum,1.)
      call hfill(203,float(npi0),dum,1.)
      IF(NBE(MIN(9,MSTJ(52)))-NBE(0).LE.1) GOTO 280

C...Calculate separation between W+ and W-
      SIGW=PARJ(93)
      IF (IWP.GT.0.AND.IWN.GT.0.AND.MSTJ(56).GT.0) THEN
        print * ,' entering LUBOEI with rlu calls MSTJ(56)>0'
        DMW=PMAS(24,1)
        DGW=PMAS(24,2)
        DMP=P(IWP,5)
        DMN=P(IWN,5)
        TAUPD=DMP/SQRT((DMP**2-DMW**2)**2+(DGW*(DMP**2)/DMW)**2)
        TAUND=DMN/SQRT((DMN**2-DMW**2)**2+(DGW*(DMN**2)/DMW)**2)
        TAUP=-TAUPD*LOG(RLU(IDUM))
        TAUN=-TAUND*LOG(RLU(IDUM))
        DXP=TAUP*PLU(IWP,8)/DMP
        DXN=TAUN*PLU(IWN,8)/DMN
        DX=DXP+DXN
        SIGW=1.0/(1.0/PARJ(93)+REAL(MSTJ(56))*DX)
C        call gmeasure(21,real(paru(3)*dx))
      ELSE
        SIGW=PARJ(93)
      ENDIF

      IF (MSTJ(57).EQ.1) THEN
        DO 221 IBE=1,MIN(9,MSTJ(52))
          DO 222 I1M=NBE(IBE-1)+1,NBE(IBE)-1
            Q2MIN=PECM**2
            I1=K(I1M,1)
            DO 223 I2M=NBE(IBE-1)+1,NBE(IBE)-1
              IF (I2M.EQ.I1M) GOTO 223
              I2=K(I2M,1)
              Q2=(P(I1,4)+P(I2,4))**2-(P(I1,1)+P(I2,1))**2-
     &           (P(I1,2)+P(I2,2))**2-(P(I1,3)+P(I2,3))**2-
     $           (P(I1,5)+P(I2,5))**2
              IF (Q2.GT.0.0.AND.Q2.LT.Q2MIN) THEN
                Q2MIN=Q2
              ENDIF
 223        CONTINUE
            P(I1M,5)=Q2MIN
 222      CONTINUE
 221    CONTINUE
      ENDIF

C...Tabulate integral for subsequent momentum shift.
      DO 220 IBE=1,MIN(9,MSTJ(52))
      IF(IBE.NE.1.AND.IBE.NE.4.AND.IBE.LE.7) GOTO 180
      IF(IBE.EQ.1.AND.MAX(NBE(1)-NBE(0),NBE(2)-NBE(1),NBE(3)-NBE(2))
     &.LE.1) GOTO 180
      IF(IBE.EQ.4.AND.MAX(NBE(4)-NBE(3),NBE(5)-NBE(4),NBE(6)-NBE(5),
     &NBE(7)-NBE(6)).LE.1) GOTO 180
      IF(IBE.GE.8.AND.NBE(IBE)-NBE(IBE-1).LE.1) GOTO 180
      IF(IBE.EQ.1) PMHQ=2.*ULMASS(211)
      IF(IBE.EQ.4) PMHQ=2.*ULMASS(321)
      IF(IBE.EQ.8) PMHQ=2.*ULMASS(221)
      IF(IBE.EQ.9) PMHQ=2.*ULMASS(331)
      QDEL=0.1*MIN(PMHQ,PARJ(93))
      QDEL3=0.1*MIN(PMHQ,PARJ(93)*3.0)
      QDELW=0.1*MIN(PMHQ,SIGW)
      QDEL3W=0.1*MIN(PMHQ,SIGW*3.0)
      IF(MSTJ(51).EQ.1) THEN
        NBIN=MIN(100,NINT(9.*PARJ(93)/QDEL))
        NBIN3=MIN(100,NINT(27.*PARJ(93)/QDEL3))
        NBINW=MIN(100,NINT(9.*SIGW/QDELW))
        NBIN3W=MIN(100,NINT(27.*SIGW/QDEL3W))
        BEEX=EXP(0.5*QDEL/PARJ(93))
        BEEX3=EXP(0.5*QDEL3/(3.0*PARJ(93)))
        BEEXW=EXP(0.5*QDELW/SIGW)
        BEEX3W=EXP(0.5*QDEL3W/(3.0*SIGW))
        BERT=EXP(-QDEL/PARJ(93))
        BERT3=EXP(-QDEL3/(3.0*PARJ(93)))
        BERTW=EXP(-QDELW/SIGW)
        BERT3W=EXP(-QDEL3W/(3.0*SIGW))
      ELSE
        NBIN=MIN(100,NINT(3.*PARJ(93)/QDEL))
        NBIN3=MIN(100,NINT(9.*PARJ(93)/QDEL3))
        NBINW=MIN(100,NINT(3.*SIGW/QDELW))
        NBIN3W=MIN(100,NINT(9.*SIGW/QDEL3W))
      ENDIF
      DO 170 IBIN=1,NBIN
      QBIN=QDEL*(IBIN-0.5)
      BEI(IBIN)=QDEL*(QBIN**2+QDEL**2/12.)/SQRT(QBIN**2+PMHQ**2)
      IF(MSTJ(51).EQ.1) THEN
        BEEX=BEEX*BERT
        BEI(IBIN)=BEI(IBIN)*BEEX
      ELSE
        BEI(IBIN)=BEI(IBIN)*EXP(-(QBIN/PARJ(93))**2)
      ENDIF
      IF(IBIN.GE.2) BEI(IBIN)=BEI(IBIN)+BEI(IBIN-1)
  170 CONTINUE
      DO 173 IBIN=1,NBIN3
      QBIN=QDEL3*(IBIN-0.5)
      BEI3(IBIN)=QDEL3*(QBIN**2+QDEL3**2/12.)/SQRT(QBIN**2+PMHQ**2)
      IF(MSTJ(51).EQ.1) THEN
        BEEX3=BEEX3*BERT3
        BEI3(IBIN)=BEI3(IBIN)*BEEX3
      ELSE
        BEI3(IBIN)=BEI3(IBIN)*EXP(-(QBIN/(3.0*PARJ(93)))**2)
      ENDIF
      IF(IBIN.GE.2) BEI3(IBIN)=BEI3(IBIN)+BEI3(IBIN-1)
 173  CONTINUE
      DO 171 IBIN=1,NBINW
      QBIN=QDELW*(IBIN-0.5)
      BEIW(IBIN)=QDELW*(QBIN**2+QDELW**2/12.)/SQRT(QBIN**2+PMHQ**2)
      IF(MSTJ(51).EQ.1) THEN
        BEEXW=BEEXW*BERTW
        BEIW(IBIN)=BEIW(IBIN)*BEEXW
      ELSE
        BEIW(IBIN)=BEIW(IBIN)*EXP(-(QBIN/SIGW)**2)
      ENDIF
      IF(IBIN.GE.2) BEIW(IBIN)=BEIW(IBIN)+BEIW(IBIN-1)
 171  CONTINUE
      DO 174 IBIN=1,NBIN3W
      QBIN=QDEL3W*(IBIN-0.5)
      BEI3W(IBIN)=QDEL3W*(QBIN**2+QDEL3W**2/12.)/SQRT(QBIN**2+PMHQ**2)
      IF(MSTJ(51).EQ.1) THEN
        BEEX3W=BEEX3W*BERT3W
        BEI3W(IBIN)=BEI3W(IBIN)*BEEX3W
      ELSE
        BEI3W(IBIN)=BEI3W(IBIN)*EXP(-(QBIN/(3.0*SIGW))**2)
      ENDIF
      IF(IBIN.GE.2) BEI3W(IBIN)=BEI3W(IBIN)+BEI3W(IBIN-1)
 174  CONTINUE
C...Loop through particle pairs and find old relative momentum.
  180 DO 210 I1M=NBE(IBE-1)+1,NBE(IBE)-1
      I1=K(I1M,1)
      DO 200 I2M=I1M+1,NBE(IBE)
      IF (MSTJ(53).EQ.1.AND.K(I1M,5).NE.K(I2M,5)) GOTO 200
      IF (MSTJ(53).EQ.2.AND.K(I1M,5).EQ.K(I2M,5)) GOTO 200
      I2=K(I2M,1)
      Q2OLD=(P(I1,4)+P(I2,4))**2-(P(I1,1)+P(I2,1))**2-(P(I1,2)+
     &P(I2,2))**2-(P(I1,3)+P(I2,3))**2-(P(I1,5)+P(I2,5))**2
      IF (Q2OLD.LE.0.0) GOTO 200
      QOLD=SQRT(Q2OLD)

C...Calculate new relative momentum.
      QMOV=0.0
      QMOV3=0.0
      QMOVW=0.0
      QMOV3W=0.0
      IF(QOLD.LT.1E-3*QDEL) THEN
        GOTO 203
      ELSEIF(QOLD.LE.QDEL) THEN
        QMOV=QOLD/3.
      ELSEIF(QOLD.LT.(NBIN-0.1)*QDEL) THEN
        RBIN=QOLD/QDEL
        IBIN=RBIN
        RINP=(RBIN**3-IBIN**3)/(3*IBIN*(IBIN+1)+1)
        QMOV=(BEI(IBIN)+RINP*(BEI(IBIN+1)-BEI(IBIN)))*
     &  SQRT(Q2OLD+PMHQ**2)/Q2OLD
      ELSE
        QMOV=BEI(NBIN)*SQRT(Q2OLD+PMHQ**2)/Q2OLD
      ENDIF
 203  Q2NEW=Q2OLD*(QOLD/(QOLD+3.*PARJ(92)*QMOV))**(2./3.)
      IF(QOLD.LT.1E-3*QDEL3) THEN
        GOTO 204
      ELSEIF(QOLD.LE.QDEL3) THEN
        QMOV3=QOLD/3.
      ELSEIF(QOLD.LT.(NBIN3-0.1)*QDEL3) THEN
        RBIN3=QOLD/QDEL3
        IBIN3=RBIN3
        RINP3=(RBIN3**3-IBIN3**3)/(3*IBIN3*(IBIN3+1)+1)
        QMOV3=(BEI3(IBIN3)+RINP3*(BEI3(IBIN3+1)-BEI3(IBIN3)))*
     &  SQRT(Q2OLD+PMHQ**2)/Q2OLD
      ELSE
        QMOV3=BEI3(NBIN3)*SQRT(Q2OLD+PMHQ**2)/Q2OLD
      ENDIF
 204  Q2NEW3=Q2OLD*(QOLD/(QOLD+3.*PARJ(92)*QMOV3))**(2./3.)
      RSCALE=1.0
      IF (PARJ(97).GT.0.0)
     $     RSCALE=1.0-EXP(-(QOLD/(PARJ(93)*PARJ(97)))**2)
      IF (MSTJ(56).LE.0.OR.IWP.EQ.0.OR.IWN.EQ.0.OR.
     $     K(I1M,5).EQ.K(I2M,5)) GOTO 207

      IF(QOLD.LT.1E-3*QDELW) THEN
        GOTO 205
      ELSEIF(QOLD.LE.QDELW) THEN
        QMOVW=QOLD/3.
      ELSEIF(QOLD.LT.(NBINW-0.1)*QDELW) THEN
        RBINW=QOLD/QDELW
        IBINW=RBINW
        RINPW=(RBINW**3-IBINW**3)/(3*IBINW*(IBINW+1)+1)
        QMOVW=(BEIW(IBINW)+RINPW*(BEIW(IBINW+1)-BEIW(IBINW)))*
     &  SQRT(Q2OLD+PMHQ**2)/Q2OLD
      ELSE
        QMOVW=BEIW(NBINW)*SQRT(Q2OLD+PMHQ**2)/Q2OLD
      ENDIF
 205  Q2NEW=Q2OLD*(QOLD/(QOLD+3.*PARJ(92)*QMOVW))**(2./3.)
      IF(QOLD.LT.1E-3*QDEL3W) THEN
        GOTO 206
      ELSEIF(QOLD.LE.QDEL3W) THEN
        QMOV3W=QOLD/3.
      ELSEIF(QOLD.LT.(NBIN3W-0.1)*QDEL3W) THEN
        RBIN3W=QOLD/QDEL3W
        IBIN3W=RBIN3W
        RINP3W=(RBIN3W**3-IBIN3W**3)/(3*IBIN3W*(IBIN3W+1)+1)
        QMOV3W=(BEI3W(IBIN3W)+RINP3W*(BEI3W(IBIN3W+1)-BEI3W(IBIN3W)))*
     &  SQRT(Q2OLD+PMHQ**2)/Q2OLD
      ELSE
        QMOV3W=BEI3W(NBIN3W)*SQRT(Q2OLD+PMHQ**2)/Q2OLD
      ENDIF
 206  Q2NEW3=Q2OLD*(QOLD/(QOLD+3.*PARJ(92)*QMOV3W))**(2./3.)
      IF (PARJ(97).GT.0.0)
     $     RSCALE=1.0-EXP(-(QOLD/(SIGW*PARJ(97)))**2)

 207  CALL LUBESQ(I1,I2,NMAX,Q2OLD,Q2NEW)
      DO 190 J=1,3
        P(I1M,J)=P(I1M,J)+P(NMAX+1,J)
        P(I2M,J)=P(I2M,J)+P(NMAX+2,J)
 190  CONTINUE
      IF(MSTJ(54).EQ.1) THEN
        CALL LUBESQ(I1,I2,NMAX,Q2OLD,Q2NEW3)
        DO 193 J=1,3
          V(I1M,J)=V(I1M,J)+P(NMAX+1,J)*RSCALE
          V(I2M,J)=V(I2M,J)+P(NMAX+2,J)*RSCALE
 193    CONTINUE
      ELSEIF(MSTJ(54).LE.-1) THEN
        EDEL=P(I1,4)+P(I2,4)-
     $       SQRT(MAX(Q2NEW-Q2OLD+(P(I1,4)+P(I2,4))**2,0.0))
        A2=(P(I1,1)-P(I2,1))**2+(P(I1,2)-P(I2,2))**2+
     $       (P(I1,3)-P(I2,3))**2
        WMAX=-1.0E20
        MI3=0
        MI4=0
        IF (MSTJ(54).EQ.-10) THEN
        print * ,' entering LUBOEI with rlu calls mstj(54)=-10'
          SMIN=PECM**2
          II1=I1
          II2=I2
          IF (RLU(IDUM).GT.0.5) THEN
            II1=I2
            II2=I1
          ENDIF
          DO 1095 I3M=NBE(0)+1,NBE(MIN(10,MSTJ(52)+1))
            IF (I3M.EQ.I1M.OR.I3M.EQ.I2M) GOTO 1095
            IF (MSTJ(53).EQ.1.AND.K(I3M,5).NE.K(I1M,5)) GOTO 1095
            IF (MSTJ(53).EQ.-2.AND.K(I1M,5).EQ.K(I2M,5).AND.
     $           K(I3M,5).NE.K(I1M,5)) GOTO 1095
            I3=K(I3M,1)
            IF(K(I3,2).EQ.K(I1,2)) GOTO 1095
            S13=(P(II1,4)+P(I3,4))**2-
     $           (P(II1,3)+P(I3,3))**2-
     $           (P(II1,2)+P(I3,2))**2-
     $           (P(II1,1)+P(I3,1))**2
            IF (S13.GE.SMIN) GOTO 1095
            MI3=I3M
            SMIN=S13
 1095     CONTINUE
          IF (MI3.GT.0) THEN
            I3=K(MI3,1)
            SMIN=PECM**2
            DO 1096 I4M=NBE(0)+1,NBE(MIN(10,MSTJ(52)+1))
              IF (I4M.EQ.I1M.OR.I4M.EQ.I2M.OR.I4M.EQ.MI3) GOTO 1096
              IF (MSTJ(53).EQ.1.AND.K(I4M,5).NE.K(I1M,5)) GOTO 1096
              IF (MSTJ(53).EQ.-2.AND.K(I1M,5).EQ.K(I2M,5).AND.
     $             K(I4M,5).NE.K(I1M,5)) GOTO 1096
              I4=K(I4M,1)
              IF(K(I4,2).EQ.K(I1,2).OR.K(I4,2).EQ.K(I3,2)) GOTO 1096
              S24=(P(II2,4)+P(I4,4))**2-
     $             (P(II2,3)+P(I4,3))**2-
     $             (P(II2,2)+P(I4,2))**2-
     $             (P(II2,1)+P(I4,1))**2
              IF (S24.GE.SMIN) GOTO 1096
              IF ((P(I3,4)+P(I4,4)+EDEL)**2.LT.(P(I3,1)+P(I4,1))**2+
     $             (P(I3,2)+P(I4,2))**2+(P(I3,3)+P(I4,3))**2+
     $             (P(I3,5)+P(I4,5))**2) GOTO 1096
              MI4=I4M
              SMIN=S24
 1096       CONTINUE
          ENDIF
        ELSE
          S12=SDIP(I1,I2)
          SM1=(P(I1,5)+SMMIN)**2
          DO 195 I3M=NBE(0)+1,NBE(MIN(10,MSTJ(52)+1))
            IF (I3M.EQ.I1M.OR.I3M.EQ.I2M) GOTO 195
            IF (MSTJ(53).EQ.1.AND.K(I3M,5).NE.K(I1M,5)) GOTO 195
            IF (MSTJ(53).EQ.-2.AND.K(I1M,5).EQ.K(I2M,5).AND.
     $           K(I3M,5).NE.K(I1M,5)) GOTO 195
            I3=K(I3M,1)
            IF(K(I3,2).EQ.K(I1,2))GOTO 195
            S13=SDIP(I1,I3)
            S23=SDIP(I2,I3)
            SM3=(P(I3,5)+SMMIN)**2
            IF (MSTJ(54).EQ.-5) THEN
              WI=1.0/(MIN(S12*SM3,S13*MIN(SM1,SM3),
     $             S23*MIN(SM1,SM3))*SM1)
            ELSE
              WI=1.0/((P(I1,4)+P(I2,4)+P(I3,4))**2-
     $             (P(I1,3)+P(I2,3)+P(I3,3))**2-
     $             (P(I1,2)+P(I2,2)+P(I3,2))**2-
     $             (P(I1,1)+P(I2,1)+P(I3,1))**2)
            ENDIF
            IF (MSTJ(57).EQ.1.AND.P(I3M,5).GT.0)
     $           WI=WI*(1.0-EXP(P(I3M,5)/(PARJ(93)**2)))
            IF (WI.LE.WMAX) GOTO 195
            DO 196 I4M=I3M+1,NBE(MIN(10,MSTJ(52)+1))
              IF (I4M.EQ.I1M.OR.I4M.EQ.I2M) GOTO 196
              IF (MSTJ(53).EQ.1.AND.K(I4M,5).NE.K(I1M,5)) GOTO 196
              IF (MSTJ(53).EQ.-2.AND.K(I1M,5).EQ.K(I2M,5).AND.
     $             K(I4M,5).NE.K(I1M,5)) GOTO 196
              I4=K(I4M,1)
              IF (K(I3,2).EQ.K(I4,2).OR.K(I4,2).EQ.K(I1,2)) GOTO 196       
              IF ((P(I3,4)+P(I4,4)+EDEL)**2.LT.(P(I3,1)+P(I4,1))**2+
     $             (P(I3,2)+P(I4,2))**2+(P(I3,3)+P(I4,3))**2+
     $             (P(I3,5)+P(I4,5))**2) GOTO 196
              IF (MSTJ(54).EQ.-5) THEN
                S14=SDIP(I1,I4)
                S24=SDIP(I2,I4)
                S34=SDIP(I3,I4)
                W=S12*MIN(MIN(S23,S24),MIN(S13,S14))*S34
                W=MIN(W,S13*MIN(MIN(S23,S34),S12)*S24)
                W=MIN(W,S14*MIN(MIN(S24,S34),S12)*S23)
                W=MIN(W,MIN(S23,S24)*S13*S14)
                W=1.0/W
              ELSE
C...weight=1-cos(theta)/mtot2
                S1234=(P(I1,4)+P(I2,4)+P(I3,4)+P(I4,4))**2-
     $               (P(I1,3)+P(I2,3)+P(I3,3)+P(I4,3))**2-
     $               (P(I1,2)+P(I2,2)+P(I3,2)+P(I4,2))**2-
     $               (P(I1,1)+P(I2,1)+P(I3,1)+P(I4,1))**2
                W=1.0/S1234
                IF (W.LE.WMAX) GOTO 196
                IF (MSTJ(54).LE.-2) THEN
                  AB2=(P(I1,1)-P(I2,1)+P(I3,1)-P(I4,1))**2+
     $                 (P(I1,2)-P(I2,2)+P(I3,2)-P(I4,2))**2+
     $                 (P(I1,3)-P(I2,3)+P(I3,3)-P(I4,3))**2
                  B2=(P(I3,1)-P(I4,1))**2+(P(I3,2)-P(I4,2))**2+
     $                 (P(I3,3)-P(I4,3))**2
                  CTH=ABS(0.5*(AB2-A2-B2)/SQRT(A2*B2))
                  W=(1.0-CTH)/S1234
                  IF (MSTJ(54).EQ.-3) W=W*(1.0-CTH)
                  IF (MSTJ(54).EQ.-4) W=W*(1.0-CTH)**3
                ENDIF
              ENDIF
              IF (MSTJ(57).EQ.1.AND.P(I3M,5).GT.0)
     $             W=W*(1.0-EXP(P(I3M,5)/(PARJ(93)**2)))
              IF (MSTJ(57).EQ.1.AND.P(I4M,5).GT.0)
     $             W=W*(1.0-EXP(P(I4M,5)/(PARJ(93)**2)))
              IF (W.LE.WMAX) GOTO 196
              MI3=I3M
              MI4=I4M
              WMAX=W
 196        CONTINUE
 195      CONTINUE
        ENDIF
        IF (MI4.EQ.0) GOTO 200
        I3=K(MI3,1)
        I4=K(MI4,1)
        EOLD=P(I3,4)+P(I4,4)
        ENEW=EOLD+EDEL
        P2=(P(I3,1)+P(I4,1))**2+(P(I3,2)+P(I4,2))**2+
     $       (P(I3,3)+P(I4,3))**2
        Q2NEWP=MAX(0.0,ENEW**2-P2-(P(I3,5)+P(I4,5))**2)
        Q2OLDP=MAX(0.0,EOLD**2-P2-(P(I3,5)+P(I4,5))**2)
        CALL LUBESQ(I3,I4,NMAX,Q2OLDP,Q2NEWP)
        DO 197 J=1,3
          V(MI3,J)=V(MI3,J)+P(NMAX+1,J)
          V(MI4,J)=V(MI4,J)+P(NMAX+2,J)
 197    CONTINUE
      ENDIF
  200 CONTINUE
  210 CONTINUE
  220 CONTINUE

C...Shift momenta and recalculate energies.
      ESUMP=0.0
      ESUM=0.0
      PROD=0.0
      DO 240 IM=NBE(0)+1,NBE(MIN(10,MSTJ(52)+1))
      I=K(IM,1)
      ESUMP=ESUMP+P(I,4)
      DO 230 J=1,3
        P(I,J)=P(I,J)+P(IM,J)
  230 CONTINUE
      P(I,4)=SQRT(P(I,5)**2+P(I,1)**2+P(I,2)**2+P(I,3)**2)
      ESUM=ESUM+P(I,4)
      DO 233 J=1,3
        PROD=PROD+V(IM,J)*P(I,J)/P(I,4)
 233  CONTINUE
  240 CONTINUE

      PARJ(96)=0.0
      IF (MSTJ(54).NE.0.AND.PROD.NE.0.0) THEN
 333    ALPHA=(ESUMP-ESUM)/PROD
        PARJ(96)=PARJ(96)+ALPHA
        PROD=0.0
        ESUM=0.0
        DO 300 IM=NBE(0)+1,NBE(MIN(10,MSTJ(52)+1))
          I=K(IM,1)
          DO 310 J=1,3
            P(I,J)=P(I,J)+ALPHA*V(IM,J)
 310      CONTINUE
          P(I,4)=SQRT(P(I,5)**2+P(I,1)**2+P(I,2)**2+P(I,3)**2)
          ESUM=ESUM+P(I,4)
          DO 313 J=1,3
            PROD=PROD+V(IM,J)*P(I,J)/P(I,4)
 313      CONTINUE
 300    CONTINUE
        IF (PROD.NE.0.0.AND.ABS(ESUMP-ESUM)/PECM.GT.0.00001) GOTO 333
      ENDIF


C...Rescale all momenta for energy conservation.
      PES=0.
      PQS=0.
      DO 250 I=1,N
      IF(K(I,1).LE.0.OR.K(I,1).GT.10) GOTO 250
      PES=PES+P(I,4)
      PQS=PQS+P(I,5)**2/P(I,4)
  250 CONTINUE
      PARJ(95)=PES-PECM
      FAC=(PECM-PQS)/(PES-PQS)
      DO 270 I=1,N
      IF(K(I,1).LE.0.OR.K(I,1).GT.10) GOTO 270
      DO 260 J=1,3
      P(I,J)=FAC*P(I,J)
  260 CONTINUE
      P(I,4)=SQRT(P(I,5)**2+P(I,1)**2+P(I,2)**2+P(I,3)**2)
  270 CONTINUE

C...Boost back to correct reference frame.
  280 CALL LUDBRB(0,0,0.,0.,DPS(1)/DPS(4),DPS(2)/DPS(4),DPS(3)/DPS(4))
      DO 290 I=1,N
      IF(K(I,1).LT.0) K(I,1)=-K(I,1)
  290 CONTINUE

C      call fillbehist(2)
C      print *,'leaving LUBOEI'
CBB      call lulist(1)
      RETURN
      END

C*********************************************************************

      SUBROUTINE LUBESQ(I1,I2,NI,Q2OLD,Q2NEW)

C...Purpose: To calculate the momentum shift i a system of two
C...particles assuming the relative momentum squared
C...should be shifted to Q2NEW. NI is the last position occupied in
C.../LUJETS/

      IMPLICIT DOUBLE PRECISION(D)
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      SAVE /LUJETS/,/LUDAT1/
      DIMENSION DP(5)
      SAVE HC1
      double precision dq2,dp2,dp12,se,de


      IF (MSTJ(55).EQ.-1) THEN
        HC1=(P(I1,4)+P(I2,4))**2-(Q2OLD-Q2NEW)
        HC2=(Q2OLD-Q2NEW)*(P(I1,4)-P(I2,4))**2
        HA=0.5*(1.-SQRT(HC1*Q2NEW/(HC1*Q2OLD-HC2)))
        DO 100 J=1,3
          PD=HA*(P(I2,J)-P(I1,J))
          P(NI+1,J)=PD
          P(NI+2,J)=-PD
 100    CONTINUE
        RETURN
      ENDIF
      IF (MSTJ(55).EQ.0) THEN
        DQ2=Q2NEW-Q2OLD
        DP2=(P(I1,1)-P(I2,1))**2+(P(I1,2)-P(I2,2))**2+
     $       (P(I1,3)-P(I2,3))**2
        DP12=P(I1,1)**2+P(I1,2)**2+P(I1,3)**2
     $       -P(I2,1)**2-P(I2,2)**2-P(I2,3)**2
        SE=P(I1,4)+P(I2,4)
        DE=P(I1,4)-P(I2,4)
        DQ2SE=DQ2+SE**2
        DA=SE*DE*DP12-DP2*DQ2SE
        DB=DP2*DQ2SE-DP12**2
        HA=(DA+SQRT(MAX(DA**2+DQ2*(DQ2+SE**2-DE**2)*DB,0D0)))/(2D0*DB)
        DO 101 J=1,3
          PD=HA*(P(I1,J)-P(I2,J))
          P(NI+1,J)=PD
          P(NI+2,J)=-PD
 101    CONTINUE
        RETURN
      ENDIF

      K(NI+1,1)=1
      K(NI+2,1)=1
      DO 200 J=1,5
        P(NI+1,J)=P(I1,J)
        P(NI+2,J)=P(I2,J)
        DP(J)=DBLE(P(I1,J))+DBLE(P(I2,J))
 200  CONTINUE
      
C...Boost to cms and rotate first particle to z-axis
      CALL LUDBRB(NI+1,NI+2,0.0,0.0,
     $     -DP(1)/DP(4),-DP(2)/DP(4),-DP(3)/DP(4))
      PHI=ULANGL(P(NI+1,1),P(NI+1,2))
      THE=ULANGL(P(NI+1,3),SQRT(P(NI+1,1)**2+P(NI+1,2)**2))
      S=Q2NEW+(P(I1,5)+P(I2,5))**2
      PZ=0.5*SQRT(Q2NEW*(S-(P(I1,5)-P(I2,5))**2)/S)
      P(NI+1,1)=0.0
      P(NI+1,2)=0.0
      P(NI+1,3)=PZ
      P(NI+1,4)=SQRT(PZ**2+P(I1,5)**2)
      P(NI+2,1)=0.0
      P(NI+2,2)=0.0
      P(NI+2,3)=-PZ
      P(NI+2,4)=SQRT(PZ**2+P(I2,5)**2)
      DP(4)=SQRT(DP(1)**2+DP(2)**2+DP(3)**2+DBLE(S))
      CALL LUDBRB(NI+1,NI+2,THE,PHI,
     $     DP(1)/DP(4),DP(2)/DP(4),DP(3)/DP(4))

      DO 300 J=1,3
        P(NI+1,J)=P(NI+1,J)-P(I1,J)
        P(NI+2,J)=P(NI+2,J)-P(I2,J)
 300  CONTINUE

      RETURN
      END     
      subroutine getpart(xp,ks,kf,km,ier)
C ------------------------------------------
C     get next particle ip from current evnt
C     xp(5)= px,py,pz,e,m
C     ks,kf,km = status,Aleph code , mother
C     ier <0   error, ier>0 = particle #
C     KINGAL output from KINE/VERT/KHIS
C     Pot/DST       from FKIN/FVER
C     MIni          from DTMC/DVER
C ------------------------------------------
      real*4 xp
      dimension xp(5)
      integer alcode
#include "bcs.h"
      data ifir,ievold/0,0/
      save ifir,ievold
C#include "bmacrod.h"
#include "bmacro.h"
      ier = -1
      IF (IFIR .EQ. 0) THEN
         LOUT = IW(6)
         NAMK = NAMIND('KINE')
         NAMV = NAMIND('VERT')
         NAMH = NAMIND('KHIS')
         NAML = NAMIND('KLIN')
         NAMF = NAMIND('FKIN')
         NAMR = NAMIND('FVER')
         NAMT = NAMIND('DTMC')
         NAMX = NAMIND('DVMC')
         jinput = 0
         if (iw(NAMK).gt.0) then
            if((iw(NAMV).gt.0).and.(iw(NAMH).gt.0)) jinput =1
         else  if (iw(NAMF).gt.0) then
            if (iw(NAMR).gt.0) jinput =2
         else if (iw(NAMT).gt.0) then
            if (iw(NAMX).gt.0) jinput =3
         endif
         IF (jinput.eq.0 ) THEN
           WRITE (LOUT,1000)
           RETURN   
         ENDIF  
          print *,' entering getpart for format ',jinput
         ifir = 1
      ENDIF
C    get current event number
      jkev = nlink('EVEH',0)
      ievt = 0
      if ( jkev.gt.0) ievt = iw(jkev+6)
      if ( ievt.ne.ievold) then
C        print * , ' starting event',ievt
         if (jinput.eq.1 ) THEN
           JKIN = NAMK + 1
           JHIS = iw(NAMH)
         else if (jinput.eq.3 ) THEN
            call MINFKI
            call MINFVE
C            call prfkin
         endif
         if ( jinput.ge.2) then
           JKIN = iw(NAMF)
           NFMX = LROWS(JKIN)
           itr = 0          
         ENDIF 
         ievold = ievt
         JKLI = iw(NAML)
      endif
      if (jinput.eq.1)then
 2      JKIN = IW(JKIN-1)
        
        IF (JKIN .NE. 0) THEN
C extract KINE info
           LHKIN = IW(JKIN+1)
           LPKIN = IW(JKIN+2)
           KIN  = JKIN + LHKIN
           XP(1) = RW(KIN+1)
           XP(2) = RW(KIN+2)
           XP(3) = RW(KIN+3)
           xp(5) = RW(KIN+4)
           am = xp(5)
           XP(4) = SQRT(AM**2+XP(1)**2+XP(2)**2+XP(3)**2)
           ALCODE= IW(KIN+5)
C          JTCODE = ITABL(JKLI,ALCODE,1)
           NTRK = IW(JKIN-2)
           MO = 0
           KS = 21
           iword = 0
           IF(JHIS.GT.0.and.ntrk.gt.0) THEN
             iword = itabl(jhis,1,ntrk)
             KS = IWORD/10000
             MO = IWORD - KS * 10000
           ENDIF
           ier = ntrk           
        ENDIF
      else if (jinput.ge.2)then
           itr = itr +1
           if (itr.gt.nfmx) then
                 itr = -1
                 return
           endif
           do ic =1,3
             xp(ic)= rtabl(jkin,itr,ic)
           enddo
           xp(5) = rtabl(jkin,itr,4)
           am = xp(5)
           XP(4) = SQRT(AM**2+XP(1)**2+XP(2)**2+XP(3)**2)
           ALCODE= itabl(jkin,itr,5)
C          JTCODE = ITABL(JKLI,ALCODE,1)
           MO = 0
           KS = 21
           iword = itabl(jkin,itr,8)
           KS = IWORD/10000
           MO = IWORD - KS * 10000
           ier = itr
C           print *,'itr,kf,iword,mo,ks',itr,kf,iword,mo,ks
      endif
      kf = alcode
      km = mo
 1000 FORMAT(/1X,'++Getpart++ NO KINE/VERT/KHIS or FKIN/FVER bank -',
     &                                     'RETURN')
C      print *,' return from getpart',ier
      return
      end
      SUBROUTINE MINFKI
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill FKIN from DTMC.
C
C     Author: Stephen Haywood      21-Nov-90
C
C     Input  : DTMC bank
C     Output : FKIN bank
C
C     Called by MINFIL
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "fkinjj.h"
#include "dtmcjj.h"
CC#include "minfac.h"
      PARAMETER (AFACTM=10000.,DFACTM=10000.,EFACTM=1000.)
*     PARAMETER (AFACTM=100000.,DFACTM=100000.,EFACTM=10000.)
#if defined(DOC)
C!    Scale factors to ingerise Mini-DST quantities.
C
C     AFACTM = angular  factor: rad -> 1/10th mrad
C     DFACTM = distance factor: cm -> micron
C     EFACTM = energy   factor: GeV -> MeV
#endif
#include "bmacro.h"
C-----------------------------------------------------------------------
C
C++   Pick up DTMC bank.
C
      KDTMC = NLINK('DTMC',0)
      IF(KDTMC.LE.0) RETURN
      NDTMC = LROWS(KDTMC)
      IF(NDTMC.LE.0) RETURN
C
C++   Create the FKIN bank.
C
      LEN = LMHLEN + LFKINA * NDTMC
      CALL AUBOS('FKIN',0,LEN, KFKIN,IGARB)
      CALL BLIST(IW,'S+','FKIN')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDTMC = NLINK('DTMC',0)
      ENDIF
      IW(KFKIN+LMHCOL) = LFKINA
      IW(KFKIN+LMHROW) = NDTMC
C
C++   Fill FKIN bank.
C++   Note: there is a check to ensure that the pt is not 0;
C++   if it is, it is modified to protect later calculations.
C++   In reality, such tracks will never be used.
C
      I=0
      DO 10 KI=1,NDTMC
         IPX = ITABL(KDTMC,KI,JDTMPX)
         IPY = ITABL(KDTMC,KI,JDTMPY)
         IF(IPX.EQ.0 .AND. IPY.EQ.0) IPX = 1
         IPA = ITABL(KDTMC,KI,JDTMPA)
         IFV = ITABL(KDTMC,KI,JDTMOV)
         ILV = ITABL(KDTMC,KI,JDTMEV)
         IF (IPA.LE.0.OR.(IFV.EQ.0.AND.ILV.EQ.0)) GO TO 10
         I=I+1
         RW(KROW(KFKIN,I)+JFKIPX) = FLOAT(IPX)/EFACTM
         RW(KROW(KFKIN,I)+JFKIPY) = FLOAT(IPY)/EFACTM
         RW(KROW(KFKIN,I)+JFKIPZ) = FLOAT(ITABL(KDTMC,KI,JDTMPZ))/EFACTM
         RW(KROW(KFKIN,I)+JFKIMA) = FLOAT(ITABL(KDTMC,KI,JDTMMA))/EFACTM
         CALL UCOPY(IW(KROW(KDTMC,KI)+JDTMPA),IW(KROW(KFKIN,I)+
     +              JFKIPA),4)
 10   CONTINUE
      IW(KFKIN+LMHROW) = I
C
      RETURN
      END
#endif
      SUBROUTINE MINFVE
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill FVER from DVMC.
C
C     Author: Stephen Haywood      21-Nov-90
C
C     Input  : DVMC bank
C     Output : FVER bank
C
C     Called by MINFIL
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "fverjj.h"
#include "dtmcjj.h"
#include "dvmcjj.h"
#include "alcons.h"
CCC#include "minfac.h"
      PARAMETER (AFACTM=10000.,DFACTM=10000.,EFACTM=1000.)
*     PARAMETER (AFACTM=100000.,DFACTM=100000.,EFACTM=10000.)
#if defined(DOC)
C!    Scale factors to ingerise Mini-DST quantities.
C
C     AFACTM = angular  factor: rad -> 1/10th mrad
C     DFACTM = distance factor: cm -> micron
C     EFACTM = energy   factor: GeV -> MeV
#endif
C
C++   Speed of light in cm/s.
C
      PARAMETER (CCMPS=1.E9*CLGHT)
C
C++   Labels for vertex mechanism.
C
      PARAMETER (NVMEC=31)
      DIMENSION VMEC(NVMEC),IVMEC(NVMEC)
      CHARACTER*4 VMEC
      DATA VMEC /
     &  'NEXT','MULS','LOSS','FIEL','DCAY','PAIR','COMP','PHOT',
     &  'BREM','DRAY','ANNI','HADR','ECOH','EVAP','FISS','ABSO',
     &  'ANNH','CAPT','EINC','INHE','MUNU','TOFM','PFIS','SCUT',
     &  'RAYL','NONE','PRED','LOOP','NULL','STOP','SHOW' /
      LOGICAL FIRST
      SAVE FIRST,IVMEC,NULL
      DATA FIRST,IVMEC,NULL / .TRUE.,NVMEC*0,0 /
C
#include "bmacro.h"
C-----------------------------------------------------------------------
C
C++   Pick up DVMC bank.
C
      KDVMC = NLINK('DVMC',0)
      IF (KDVMC.LE.0) RETURN
      NDVMC = LROWS(KDVMC)
      IF(NDVMC.LE.0) RETURN
C
C++   Initialisation for vertex mechanism labels.
C
      IF(FIRST) THEN
         DO I=1,NVMEC
            IVMEC(I) = INTCHA(VMEC(I))
         ENDDO
         NULL = INTCHA('    ')
         FIRST = .FALSE.
      ENDIF
C
C++   Create the FVER bank.
C
      LEN = LMHLEN + LFVERA * NDVMC
      CALL AUBOS('FVER',0,LEN, KFVER,IGARB)
      CALL BLIST(IW,'S+','FVER')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDVMC = NLINK('DVMC',0)
      ENDIF
      IW(KFVER+LMHCOL) = LFVERA
      IW(KFVER+LMHROW) = NDVMC
C
C++   Fill FVER bank.
C
      DO I=1,NDVMC
         RW(KROW(KFVER,I)+JFVEVX) = FLOAT(ITABL(KDVMC,I,JDVMVX))/DFACTM
         RW(KROW(KFVER,I)+JFVEVY) = FLOAT(ITABL(KDVMC,I,JDVMVY))/DFACTM
         RW(KROW(KFVER,I)+JFVEVZ) = FLOAT(ITABL(KDVMC,I,JDVMVZ))/DFACTM
C        CT = FLOAT(ITABL(KDVMC,I,JDVMTO))/DFACTM
C        RW(KROW(KFVER,I)+JFVETO) = CT / CCMPS
         IW(KROW(KFVER,I)+JFVEIP) = ITABL(KDVMC,I,JDVMIP)
         IW(KROW(KFVER,I)+JFVEVN) = ITABL(KDVMC,I,JDVMVN)
C
C++      Reassign the vertex mechanism.
C
         LABEL = ITABL(KDVMC,I,JDVMVM)
         IF(LABEL.EQ.0) THEN
            JVMEC = NULL
         ELSE
            IF(LABEL.GT.0 .AND. LABEL.LE.NVMEC) THEN
               JVMEC = IVMEC(LABEL)
            ELSE
               JVMEC = LABEL
            ENDIF
         ENDIF
         IW(KROW(KFVER,I)+JFVEVM) = JVMEC
      ENDDO
C
C++   Reconstruct the relationships to the FKIN bank.
C++   The code to identify the mother of the vertex from the DTMC (FKIN)
C++   bank is not used since some tracks end at more than one vertex.
C
      KDTMC = NLINK('DTMC',0)
      IF(KDTMC.LE.0) RETURN
      NDTMC = LROWS(KDTMC)
      IF(NDTMC.LE.0) RETURN
C
      DO 10 I=1,NDTMC
         IORIG = ITABL(KDTMC,I,JDTMOV)
         IF (IORIG.LE.0.OR.IORIG.GT.NDVMC) GO TO 10
         IF(ITABL(KFVER,IORIG,JFVENS).EQ.0) THEN
            IW(KROW(KFVER,IORIG)+JFVEIS) = I - 1
         ENDIF
         IW(KROW(KFVER,IORIG)+JFVENS) = IW(KROW(KFVER,IORIG)+JFVENS) + 1
 10   CONTINUE
C
      RETURN
      END
#endif
      SUBROUTINE RECO4_JBH(mode,para,res)
      IMPLICIT NONE

      INTEGER MODE
      REAL PARA,RES

      REAL ECMWW,PMW,PGW,HBAR,TFRAG,RHAD
      COMMON/PYWWCM/ECMWW,PMW,PGW,HBAR,TFRAG,RHAD

      DOUBLE PRECISION TFRAG_JBH,RHAD_JBH,RPROB,RES_JBH
      COMMON /SJKOCO/ TFRAG_JBH,RHAD_JBH,RPROB,RES_JBH

      INTEGER IREC
CBBL bm1,bm2 are arrays(5)
      DOUBLE PRECISION BM1(5),BM2(5)
* Part of LUND common...
      INTEGER N,K
      REAL    P,V
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)      
C
      INTEGER IDAU1,IDAU2,IMO1,IMO2,IP,ID1,ID2,LUJ1(2),LUJ2(2),MODE_JBH
     &     ,IFLAV(4),idau11,idau22
      DOUBLE PRECISION MW_JBH,WW_JBH

*
      TFRAG_JBH=DBLE(TFRAG)
      RHAD_JBH=DBLE(RHAD)
      MW_JBH=DBLE(PMW)
      WW_JBH=DBLE(PGW)
      MODE_JBH=MODE
      RPROB=DBLE(PARA)
      RES_JBH=0.0D0 
c
c GET THE W DAUGTERS
c
      IDAU1 =0
      IDAU2 = 0
      IMO1 =0
      IMO2 = 0
      DO IP =1,N
        IF(K(IP,2).EQ.24) IMO2=IP
        IF(K(IP,2).EQ.-24) IMO1=IP
        IF (IMO1*IMO2.GT.0) THEN
          IF (K(IP,3).EQ.IMO1.AND.IDAU1.EQ.0) THEN
            IFLAV(1)=K(IP,2)
            IDAU1 = IP
          ELSE IF (K(IP,3).EQ.IMO1.AND.IDAU1.GE.1) THEN
            IFLAV(2)=K(IP,2)
            IDAU11 = IP
          ELSE IF (K(IP,3).EQ.IMO2.AND.IDAU2.EQ.0) THEN
            IFLAV(3)=K(IP,2)
            IDAU2 = IP
          ELSE IF (K(IP,3).EQ.IMO2.AND.IDAU2.GE.1) THEN
            IFLAV(4)= K(IP,2)
            idau22 = ip
          ENDIF
        ENDIF
      ENDDO
      ID2 = IDAU2
      ID1 = IDAU1
C       print *,'mother 1, daughter',imo1,idau1
C       print *,'mother 2, daughter',imo2,idau2
C
C NWO STORE IN THE EXCALIBUR WAY: W-W+: F F~ F F~
C we have W's so no need for that
      LUJ1(1)=ID1
      LUJ1(2)=idau11
      LUJ1(1)=imo1
      LUJ1(2)=imo1
C      IF (K(ID1,2).LT.0) THEN
C        LUJ1(1)=IDau11
C        LUJ1(2)=ID1
C      ENDIF
      LUJ2(1)=ID2
      LUJ2(2)=IDau22
      LUJ2(1)=imo2
      LUJ2(2)=imo2
C      IF (K(ID2,2).LT.0) THEN
C        LUJ2(1)=IDau22
C        LUJ2(2)=ID2
C      ENDIF
CBB   warning BM1,BM2 are arrays(5) !!!!! must be filled
      do 6 ip=1,5
        bm1(ip) = P(imo1,ip)
        bm2(ip) = P(imo2,ip)
 6    continue 
CBB 
C      BM1(5)=DBLE(SQRT(MAX(0.0,(P(LUJ1(1),4)+P(LUJ1(2),4))**2-(P(LUJ1(1)
C     &     ,1)+P(LUJ1(2),1))**2-(P(LUJ1(1),2)+P(LUJ1(2),2))**2-(P(LUJ1(1
C     &     ),3)+P(LUJ1(2),3))**2)))
C      BM2(5)=DBLE(SQRT(MAX(0.0,(P(LUJ2(1),4)+P(LUJ2(2),4))**2-(P(LUJ2(1)
C     &     ,1)+P(LUJ2(2),1))**2-(P(LUJ2(1),2)+P(LUJ2(2),2))**2-(P(LUJ2(1
C     &     ),3)+P(LUJ2(2),3))**2)))
CBB try the reverse : iw1 = W+ (code 24) iw2 = W- (code -24)
      do  ip=1,5
        bm1(ip) = P(imo2,ip)
        bm2(ip) = P(imo1,ip)
      enddo
      LUJ2(1)=imo1
      LUJ2(2)=imo1
      LUJ1(1)=imo2
      LUJ1(2)=imo2
CBB endif swapping
      CALL SJOST2(MODE_JBH,BM1,BM2,MW_JBH,WW_JBH,LUJ1,LUJ2,IREC)
      RES=REAL(RES_JBH)
      
      RETURN
      END

      SUBROUTINE SJOST2(IMODE,B1,B2,WM,WW,LUJ1,LUJ2,MREC)
C========================================================
C! Reconnect strings according to overlap of cylindrical 
C  string piece volumes.
C
C Input:  MODE  = 0 : no reconnection.
C               = 1 : reconnect, instantaneous cylinders.
C               = 2 : reconnect, time-retarded cylinders.
C         B1,B2 = Masses of the strings
C         WM,WW = W mass and width
C         LUJ1,LUJ2 = Pointers to strings in LUND common
C
C Output: MREC  = 0 : no reconnection
C               = 1 : reconnection occurred
C
C  This routine is mainly extracted from the pywwa4 routine
C  of the "color rearrangement in WW events" routines of
C  Sjostrand.
C
C  Created  01-Mar-96 : J.B.Hansen
C  Modified 01-Oct-96 : A.Waananen
C========================================================
      IMPLICIT NONE
      DOUBLE PRECISION TFRAG,RHAD,RPROB
      COMMON /SJKOCO/ TFRAG,RHAD,RPROB,RES
      INTEGER NCREVT
      COMMON /CREVT/ NCREVT
* Part of LUND common...
      INTEGER N,K
      REAL    P,V
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)      
* Arguments
      INTEGER IMODE,LUJ1(2),LUJ2(2)
      DOUBLE PRECISION B1(5),B2(5),WM,WW
* Local variables
      INTEGER NPT
      PARAMETER (NPT=100)
      INTEGER I,J,NNP,NNM,ISGP,ISGM,INP(50),INM(50),IJOIN(100),IIP,I1,I2
     &  ,IIM,NACC,IPT,IMAXP,IMAXM,IAP(NPT),IAM(NPT),MREC,IA,NJOIN,IS
     &  ,IACC
      REAL RLU
      DOUBLE PRECISION TP,TM,XP(3),XM(3),GTMAX,P1A,P2A,V1(3),V2(3)
     &  ,BETP(50,4),DIRP(50,3),DIRL,BETM(50,4),DIRM(50,3),SUM,R,PHI,X,Y
     &  ,Z,T,BLOWR,BLOWT,WTSMP,WTMAXP,XD(4),XB(4),BED,BEDG,SR2,SZ2,WTP
     &  ,WTMAXM,WTM,WT,WTA(NPT),RSUM,RES,PREC
      PARAMETER (BLOWR=2.50D0)
      PARAMETER (BLOWT=2.0D0)
      DOUBLE PRECISION PI,HBAR
C      PARAMETER (PI=3.14159265358979324D0,HBAR=0.197327053D0)
CJBH FOR BRIGITTE
      PARAMETER (PI=3.141592D0,HBAR=0.2D0)
      EXTERNAL RLU      
*
C      print *,'input sjo2',IMODE,B1,B2,WM,WW,LUJ1,LUJ2
      IACC=0
C...Select decay vertices of W+ and W-.
      TP=HBAR*(-LOG(DBLE(RLU(0))))*B1(4)/
     &SQRT((B1(5)*B1(5)-WM*WM)**2+(B1(5)*B1(5)*WW/WM)**2)
      TM=HBAR*(-LOG(DBLE(RLU(0))))*B2(4)/
     &SQRT((B2(5)*B2(5)-WM*WM)**2+(B2(5)**2*WW/WM)**2)
C      print *,'tp,tm',luj1(1),tp,luj2(1),tm
      DO 110 J=1,3
      XP(J)=TP*B1(J)/B1(4)
      XM(J)=TM*B2(J)/B2(4)
  110 CONTINUE
      GTMAX=MAX(TP,TM)
 
C...Find partons pointing back to "W+"-pairing and "W-"-pairing; store them with quark
C...end of string first.
      NNP=0
      NNM=0
      ISGP=0
      ISGM=0
      J=MAX(LUJ1(1),LUJ1(2),LUJ2(1),LUJ2(2))+1
      DO 140 I=J,N
      IF(K(I,1).NE.1.AND.K(I,1).NE.2) GOTO 140
C...Do not count photons (added 19/6 1995).
      IF(K(I,2).EQ.22) GOTO 140
      IF(K(I,3).EQ.LUJ1(1).OR.K(I,3).EQ.LUJ1(2)) THEN
        IF(ISGP.EQ.0) ISGP=ISIGN(1,K(I,2))
        NNP=NNP+1
        IF(ISGP.EQ.1) THEN
          INP(NNP)=I
        ELSE
          DO 120 I1=NNP,2,-1
  120     INP(I1)=INP(I1-1)
          INP(1)=I
        ENDIF
        IF(K(I,1).EQ.1) ISGP=0
      ELSEIF(K(I,3).EQ.LUJ2(1).OR.K(I,3).EQ.LUJ2(2)) THEN
        IF(ISGM.EQ.0) ISGM=ISIGN(1,K(I,2))
        NNM=NNM+1
        IF(ISGM.EQ.1) THEN
          INM(NNM)=I
        ELSE
          DO 130 I1=NNM,2,-1
  130     INM(I1)=INM(I1-1)
          INM(1)=I
        ENDIF
        IF(K(I,1).EQ.1) ISGM=0
      ENDIF
  140 CONTINUE
 
C...Reconstruct velocity and direction of "W+"-paired string pieces.
      DO 180 IIP=1,NNP-1
      IF(K(INP(IIP),2).LT.0) GOTO 180
      I1=INP(IIP)
      I2=INP(IIP+1)
      P1A=DBLE(SQRT(P(I1,1)*P(I1,1)+P(I1,2)*P(I1,2)+P(I1,3)*P(I1,3)))
      P2A=DBLE(SQRT(P(I2,1)*P(I2,1)+P(I2,2)*P(I2,2)+P(I2,3)*P(I2,3)))
      DO 150 J=1,3
      V1(J)=DBLE(P(I1,J))/P1A
  150 V2(J)=DBLE(P(I2,J))/P2A
      DO 160 J=1,3
      BETP(IIP,J)=0.5D0*(V1(J)+V2(J))
  160 DIRP(IIP,J)=V1(J)-V2(J)
      BETP(IIP,4)=1.0D0/SQRT(1.0D0-BETP(IIP,1)*BETP(IIP,1)-BETP(IIP,2)
     &  *BETP(IIP,2)-BETP(IIP,3)*BETP(IIP,3))
      DIRL=SQRT(DIRP(IIP,1)*DIRP(IIP,1)+DIRP(IIP,2)*DIRP(IIP,2)+DIRP(IIP
     &  ,3)*DIRP(IIP,3))
      DO 170 J=1,3
  170 DIRP(IIP,J)=DIRP(IIP,J)/DIRL
  180 CONTINUE
 
C...Reconstruct velocity and direction of "W-"-paired string pieces.
      DO 230 IIM=1,NNM-1
      IF(K(INM(IIM),2).LT.0) GOTO 230
      I1=INM(IIM)
      I2=INM(IIM+1)
      P1A=DBLE(SQRT(P(I1,1)*P(I1,1)+P(I1,2)*P(I1,2)+P(I1,3)*P(I1,3)))
      P2A=DBLE(SQRT(P(I2,1)*P(I2,1)+P(I2,2)*P(I2,2)+P(I2,3)*P(I2,3)))
      DO 190 J=1,3
      V1(J)=DBLE(P(I1,J))/P1A
  190 V2(J)=DBLE(P(I2,J))/P2A
      DO 200 J=1,3
      BETM(IIM,J)=0.5D0*(V1(J)+V2(J))
  200 DIRM(IIM,J)=V1(J)-V2(J)
      BETM(IIM,4)=1.0D0/SQRT(1.0D0-BETM(IIM,1)*BETM(IIM,1)-BETM(IIM,2)
     &  *BETM(IIM,2)-BETM(IIM,3)*BETM(IIM,3))
      DIRL=SQRT(DIRM(IIM,1)*DIRM(IIM,1)+DIRM(IIM,2)*DIRM(IIM,2)+DIRM(IIM
     &  ,3)*DIRM(IIM,3))
      DO 220 J=1,3
  220 DIRM(IIM,J)=DIRM(IIM,J)/DIRL
  230 CONTINUE
 
C...Loop over number of space-time points.
      NACC=0
      SUM=0.0D0
      DO 280 IPT=1,NPT
 
C...Pick x,y,z,t Gaussian (width RHAD and TFRAG, respectively).
      R=SQRT(-LOG(DBLE(RLU(0))))
      PHI=2.0D0*PI*DBLE(RLU(0))
      X=BLOWR*RHAD*R*COS(PHI)
      Y=BLOWR*RHAD*R*SIN(PHI)
      R=SQRT(-LOG(DBLE(RLU(0))))
      PHI=2.0D0*PI*DBLE(RLU(0))
      Z=BLOWR*RHAD*R*COS(PHI)
      T=GTMAX+BLOWT*SQRT(0.5D0)*TFRAG*R*ABS(SIN(PHI))
 
C...Weight for sample distribution.
      WTSMP=EXP(-(X*X+Y*Y+Z*Z)/(BLOWR*BLOWR*RHAD*RHAD))*
     &EXP(-2.0D0*(T-GTMAX)**2/(BLOWT*BLOWT*TFRAG*TFRAG))
 
C...Loop over W+ string pieces and find one with largest weight.
      IMAXP=0
      WTMAXP=1.0D-10
      XD(1)=X-XP(1)
      XD(2)=Y-XP(2)
      XD(3)=Z-XP(3)
      XD(4)=T-TP
      DO 250 IIP=1,NNP-1
      IF(K(INP(IIP),2).LT.0) GOTO 250
      BED=BETP(IIP,1)*XD(1)+BETP(IIP,2)*XD(2)+BETP(IIP,3)*XD(3)
      BEDG=BETP(IIP,4)*(BETP(IIP,4)*BED/(1.0D0+BETP(IIP,4))-XD(4))
      DO 240 J=1,3
  240 XB(J)=XD(J)+BEDG*BETP(IIP,J)
      XB(4)=BETP(IIP,4)*(XD(4)-BED)
      SR2=XB(1)*XB(1)+XB(2)*XB(2)+XB(3)*XB(3)
      SZ2=(DIRP(IIP,1)*XB(1)+DIRP(IIP,2)*XB(2)+DIRP(IIP,3)*XB(3))**2
      WTP=EXP(-(SR2-SZ2)/(2.0D0*RHAD*RHAD))*EXP(-(XB(4)*XB(4)-SZ2)/TFRAG
     &  /TFRAG)
      IF(IMODE.EQ.1.AND.XB(4)-SQRT(SZ2).LT.0.0D0) WTP=0.0D0
      IF(IMODE.EQ.2.AND.XB(4)-SQRT(SR2).LT.0.0D0) WTP=0.0D0
      IF(WTP.GT.WTMAXP) THEN
        IMAXP=IIP
        WTMAXP=WTP
      ENDIF
  250 CONTINUE
 
C...Loop over W- string pieces and find one with largest weight.
      IMAXM=0
      WTMAXM=1.0D-10
      XD(1)=X-XM(1)
      XD(2)=Y-XM(2)
      XD(3)=Z-XM(3)
      XD(4)=T-TM
      DO 270 IIM=1,NNM-1
      IF(K(INM(IIM),2).LT.0) GOTO 270
      BED=BETM(IIM,1)*XD(1)+BETM(IIM,2)*XD(2)+BETM(IIM,3)*XD(3)
      BEDG=BETM(IIM,4)*(BETM(IIM,4)*BED/(1.0D0+BETM(IIM,4))-XD(4))
      DO 260 J=1,3
  260 XB(J)=XD(J)+BEDG*BETM(IIM,J)
      XB(4)=BETM(IIM,4)*(XD(4)-BED)
      SR2=XB(1)*XB(1)+XB(2)*XB(2)+XB(3)*XB(3)
      SZ2=(DIRM(IIM,1)*XB(1)+DIRM(IIM,2)*XB(2)+DIRM(IIM,3)*XB(3))**2
      WTM=EXP(-(SR2-SZ2)/(2.0D0*RHAD*RHAD))*EXP(-(XB(4)*XB(4)-SZ2)/TFRAG
     &  /TFRAG)
      IF(IMODE.EQ.1.AND.XB(4)-SQRT(SZ2).LT.0.0D0) WTM=0.0D0
      IF(IMODE.EQ.2.AND.XB(4)-SQRT(SR2).LT.0.0D0) WTM=0.0D0
      IF(WTM.GT.WTMAXM) THEN
        IMAXM=IIM
        WTMAXM=WTM
      ENDIF
  270 CONTINUE
 
C...Result of integration.
      WT=0.0D0
      IF(IMAXP.NE.0.AND.IMAXM.NE.0) THEN
        WT=WTMAXP*WTMAXM/WTSMP
        SUM=SUM+WT
        NACC=NACC+1
        IAP(NACC)=IMAXP
        IAM(NACC)=IMAXM
        WTA(NACC)=WT
      ENDIF
 
  280 CONTINUE
      RES=BLOWR*BLOWR*BLOWR*BLOWT*SUM/NPT
C      print *,'res',res 
C...Decide whether to reconnect.

CBB      PREC=1.0D0-EXP(-RPROB*RES)
      MREC=0
CBB      IF(PREC.GT.RLU(0)) MREC=1
      if ( res.gt.0.) MREC = 1
      IF(MREC.EQ.0) GOTO 320
 
C...Decide which pair of strings to reconnect.
      RSUM=DBLE(RLU(0))*SUM
      DO 290 IA=1,NACC
      IACC=IA
      RSUM=RSUM-WTA(IA)
      IF(RSUM.LE.0.0D0) GOTO 300
  290 CONTINUE
  300 IIP=IAP(IACC)
      IIM=IAM(IACC)
C...Recouple strings.
      NJOIN=0
      DO 310 IS=1,NNP+NNM
      NJOIN=NJOIN+1
      IF(IS.LE.IIP) THEN
        I=INP(IS)
      ELSEIF(IS.LE.IIP+NNM-IIM) THEN
        I=INM(IS-IIP+IIM)
      ELSEIF(IS.LE.IIP+NNM) THEN
        I=INM(IS-IIP-NNM+IIM)
      ELSE
        I=INP(IS-NNM)
      ENDIF
      IJOIN(NJOIN)=I
      IF(K(I,2).LT.0) THEN
        CALL LUJOIN(NJOIN,IJOIN)
        NJOIN=0
      ENDIF
  310 CONTINUE
  320 CONTINUE
 999  RETURN
      END
      SUBROUTINE ARJTMD(LUPAR)
C ====================================
C    take into account cards like MSTG and PARG to modify mstj and parj
C                                 MSTV and PARV to modify mstu and paru
C    even if several data cards exits (from original processing, then cards)
C    B.Bloch October 200
C ==========================================
#include "bcs.h"
      integer L1MST, L1PAR
      PARAMETER (L1MST=200, L1PAR=200)
      REAL*4 PARU,PARJ
      INTEGER MSTU,MSTJ
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
C
      PARAMETER (LKEYS=4)
      CHARACTER*4 KEY(LKEYS),CHAINT
      CHARACTER*1 FMT(LKEYS)


      DATA KEY / 'MSTV','PARV','MSTG','PARG'/
      DATA FMT /'I','F','I','F'/
      LUPAR=0
      DO 50 I=1,LKEYS
         NAMI=NAMIND(KEY(I))
         IF (IW(NAMI).EQ.0) GOTO 50
         KIND=NAMI+1
   15    KIND=IW(KIND-1)
         IF (KIND.EQ.0) GOTO 49
         LUPAR = LUPAR+1
         J = IW(KIND-2)
         GOTO (21,22,23,24) I
   21    MSTU(J) = IW(KIND+1)
         print *,' MSTU ',j,' = ',MSTU(J)
       GOTO 15
   22    PARU(J) = RW(KIND+1)
         print *,' PARU ',j,' = ',PARU(J)
       GOTO 15
   23    MSTJ(J) = IW(KIND+1)
         print *,' MSTJ ',j,' = ',MSTJ(J)
       GOTO 15
   24    PARJ(J) = RW(KIND+1)
         print *,' PARJ ',j,' = ',PARJ(J)
       GOTO 15
   49    CONTINUE
         CALL BKFMT (KEY(I),FMT(I))
         CALL BLIST (IW,'C+',KEY(I))
       GOTO 50
   50 CONTINUE
      RETURN
      END

      SUBROUTINE PYSCI(NSWITCH)
C--                                                                   --C
C--   Created:        950319                                          --C
C--   Last update:    26 Mar 2001                                     --C
C--   Purpose:        to generate random switches of parton           --C
C--                   colours in the partonic final state             --C
C--   Three versions: MSTP(188)=1 original                            --C
C--                   MSTP(188)=2 no switch between pert. partons     --C
C--                   MSTP(188)=3 exponential damping with area diff. --C
C-- adapted for Aleph by B.Bloch March 2001
C--  mstp(188)/parp(188) are filled in common PYGAL
C--  Mint(51) error flag replaced by MSTU(24)

      IMPLICIT NONE

C--       global variables
      COMMON /LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      INTEGER N,K
      REAL P,V
C      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
C      REAL PARP,PARI
C      INTEGER MSTP,MSTI
C      COMMON/PYINT1/MINT(400),VINT(400)
C      REAL VINT
C      INTEGER MINT
C      COMMON/LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,Q2,U
C      REAL CUT,PARL,X,Y,W2,Q2,U
      integer L1MST, L1PAR
      PARAMETER (L1MST=200, L1PAR=200)
      REAL*4 PARU,PARJ
      INTEGER MSTU,MSTJ
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      INTEGER LST
      integer mstp188
      real*4 parp188
      COMMON/PYGAL/mstp188,parp188
C--       functions
      REAL RLU
C--       local variables
      INTEGER I,J,LUCOMP,NS,NEXT,THIS,INIT,FIRSTGLUON
      LOGICAL QUARK,QUARK1,QUARK2,AQUARK1,AQUARK2,GLUON1,GLUON2
      LOGICAL FIRST,FORWARD
      REAL PROB,SUPP,AREA,ADIFF,MASS,MRHO
      INTEGER NSWITCH
      INTEGER NIN,KIN(4000,5)
      REAL PIN(4000,5),VIN(4000,5)
      LOGICAL ADD
C--       Calculate string area from energy stored in string, i.e.
C--       (p_1+p_2)^2-(m_1+m_2)^2 = 2*(p_1*p_2-m_1*m_2)
      AREA(I,J)=2.*(P(I,4)*P(J,4)-P(I,1)*P(J,1)-
     -              P(I,2)*P(J,2)-P(I,3)*P(J,3)-P(I,5)*P(J,5))

      MASS(I,J)=SQRT(MAX(0.,(P(I,4)+P(J,4))**2-(P(I,1)+P(J,1))**2-
     -                      (P(I,2)+P(J,2))**2-(P(I,3)+P(J,3))**2))

C      print *,' entering pysci with mstp(188).parp(188) ',
C     &                                         mstp188,parp188
      MRHO=0.7
      NSWITCH=0
      PROB = parp188      
C--       Copy incoming state
      NIN=N
      DO 100 I=1,N
         DO 110 J=1,5
            PIN(I,J)=P(I,J)
            VIN(I,J)=V(I,J)
            KIN(I,J)=K(I,J)
110      CONTINUE
100   CONTINUE
C--       Assign colour and anticolour pointers to all partons. Colour
C--       pointers are in K(I,4) and anticolour pointers are in K(I,5).
C--       The pointer points to the row where the respective anticolour
C--       and colour is.

      FIRST=.TRUE.
      DO 10 I=1,N
         IF (K(I,1).LT.10 .AND. K(I,1).GT.0) THEN
C--       check if parton is a quark, antiquark or diquark
            IF (ABS(K(I,2)).LT.10 .OR. LUCOMP(K(I,2)).EQ.90) THEN
               IF (K(I,2).LT.10 .AND. K(I,2).GT.0 .OR.
     &             K(I,2).LT.-1000) THEN
                  QUARK=.TRUE.
               ELSE
                  QUARK=.FALSE.
               ENDIF
C--       reset pointers
              K(I,4)=0
              K(I,5)=0
C--       the first quark, antiquark or diquark in a string points
C--       to the parton in the next line
               IF (FIRST) THEN
                  IF (QUARK) THEN
                     K(I,4)=(I+1)
                  ELSE
                     K(I,5)=(I+1)
                  ENDIF
                  FIRST=.FALSE.
C--       the last quark, antiquark or diquark in a string points
C--       to the parton in the previous line
               ELSE
                  IF (QUARK) THEN
                     K(I,4)=(I-1)
                  ELSE
                     K(I,5)=(I-1)
                  ENDIF
                  FIRST=.TRUE.
               ENDIF
               K(I,1)=3
C--       check if parton gluon
            ELSEIF (K(I,2).EQ.21) THEN
C--       if the previous colour points to this gluon then its anticolour
C--       should point back and its colour should point to the next line
               IF(K(I-1,4).EQ.I) THEN
                  K(I,4)=(I+1)
                  K(I,5)=(I-1)
               ELSEIF(K(I-1,5).EQ.I) THEN
                  K(I,4)=(I-1)
                  K(I,5)=(I+1)
               ELSE
C--       take care of strings with only gluons
                  FIRSTGLUON=I
C--       random choice of direction of colourflow
                  IF (RLU(0).LT.0.5) THEN
                     FORWARD=.TRUE.
                     K(I,4)=(I+1)
                  ELSE
                     FORWARD=.FALSE.
                     K(I,5)=(I+1)
                  ENDIF
               ENDIF
C--       the last gluon in a string points to the first gluon in the
C--       same string
               IF(K(I,1).EQ.1) THEN
                  IF (FORWARD) THEN
                     K(FIRSTGLUON,5)=I
                     K(I,4)=FIRSTGLUON
                  ELSE
                     K(FIRSTGLUON,4)=I
                     K(I,5)=FIRSTGLUON
                  ENDIF
               ENDIF
               K(I,1)=3
            ENDIF
         ENDIF
10    CONTINUE
               
C--       find first parton in colour switch
      DO 20 I=1,N
         QUARK1=.FALSE.
         AQUARK1=.FALSE.
         GLUON1=.FALSE.
         IF (K(I,1).EQ.3) THEN
C--       check if parton quark or antidiquark
            IF (K(I,4).NE.0 .AND. K(I,5).EQ.0) THEN
               QUARK1=.TRUE.
C--       check if parton antiquark or diquark
            ELSEIF (K(I,4).EQ.0 .AND. K(I,5).NE.0) THEN
               AQUARK1=.TRUE.
C--       check if parton gluon
            ELSEIF (K(I,2).EQ.21) THEN
               GLUON1=.TRUE.
            ENDIF
C--       find second parton in colour switch
            DO 30 J=I+1,N
               QUARK2=.FALSE.
               AQUARK2=.FALSE.
               GLUON2=.FALSE.
               IF (K(J,1).EQ.3 .AND.
C--       at least one remnant parton if MSTP(188)=2
     &            (K(J,3).EQ.1 .OR. K(I,3).EQ.1 .OR.
     &             K(J,3).EQ.2 .OR. K(I,3).EQ.2 .OR.
     &            MSTP188.EQ.1 .OR. MSTP188.EQ.3))THEN
C--       check if second parton quark or antidiquark
                  IF (K(J,4).NE.0 .AND. K(J,5).EQ.0) THEN
                     QUARK2=.TRUE.
C--       check if second parton antquark or diquark
                  ELSEIF (K(J,4).EQ.0 .AND. K(J,5).NE.0) THEN
                     AQUARK2=.TRUE.
C--       check if second parton gluon
                  ELSEIF (K(J,2).EQ.21) THEN
                     GLUON2=.TRUE.
                  ENDIF
C--       switch colour pointers
                  IF (QUARK1.AND.QUARK2) THEN
                     IF (MSTP188.EQ.3) THEN
                        ADIFF=AREA(I,K(I,4))+AREA(J,K(J,4))-
     -                        AREA(I,K(J,4))-AREA(J,K(I,4))
                        ADIFF=MAX(0.,ADIFF)
                        SUPP=(1.-EXP(-PARJ(42)*ADIFF))
                     ELSE
                        SUPP=1.
                     ENDIF
                     IF (MASS(I,K(J,4)).LT.MRHO .OR.
     /                   MASS(J,K(I,4)).LT.MRHO) THEN
                        SUPP=0.
                     ENDIF
C                    CALL LULIST(2)
C                    WRITE(*,*) 'I,K(I,4),J,K(J,4),SUPP:',
C     ,                           I,K(I,4),J,K(J,4),SUPP
C     ,               ,AREA(I,K(I,4)),AREA(J,K(J,4))
C     /               ,AREA(I,K(J,4)),AREA(J,K(I,4)),ADIFF
                     IF (RLU(0).LT.PROB*SUPP) THEN
                        CALL LECSWI(I,J)
                        NSWITCH=NSWITCH+1
                     ENDIF
C                    CALL LULIST(2)
                  ELSEIF (K(I,4).NE.J .AND. K(J,4).NE.I .AND.
     &            (QUARK1.AND.GLUON2 .OR. GLUON1.AND.QUARK2)) THEN
                     IF (MSTP188.EQ.3) THEN
                        ADIFF=AREA(I,K(I,4))+AREA(J,K(J,4))-
     -                        AREA(I,K(J,4))-AREA(J,K(I,4))
                        ADIFF=MAX(0.,ADIFF)
                        SUPP=(1.-EXP(-PARJ(42)*ADIFF))
                     ELSE
                        SUPP=1.
                     ENDIF
                     IF (MASS(I,K(J,4)).LT.MRHO .OR.
     /                   MASS(J,K(I,4)).LT.MRHO) THEN
                        SUPP=0.
                     ENDIF
                     IF (RLU(0).LT.PROB*SUPP) THEN
                        CALL LECSWI(I,J)
                        NSWITCH=NSWITCH+1
                     ENDIF
                  ELSEIF (AQUARK1.AND.AQUARK2) THEN
                     IF (MSTP188.EQ.3) THEN
                        ADIFF=AREA(I,K(I,5))+AREA(J,K(J,5))-
     -                        AREA(I,K(J,5))-AREA(J,K(I,5))
                        ADIFF=MAX(0.,ADIFF)
                        SUPP=(1.-EXP(-PARJ(42)*ADIFF))
                     ELSE
                        SUPP=1.
                     ENDIF
                     IF (MASS(I,K(J,5)).LT.MRHO .OR.
     /                   MASS(J,K(I,5)).LT.MRHO) THEN
                        SUPP=0.
                     ENDIF
                     IF (RLU(0).LT.PROB*SUPP) THEN
                        CALL LEASWI(I,J)
                        NSWITCH=NSWITCH+1
                     ENDIF
                  ELSEIF (K(I,5).NE.J .AND. K(J,5).NE.I .AND.
     &            (AQUARK1.AND.GLUON2 .OR. GLUON1.AND.AQUARK2)) THEN
                     IF (MSTP188.EQ.3) THEN
                        ADIFF=AREA(I,K(I,5))+AREA(J,K(J,5))-
     -                        AREA(I,K(J,5))-AREA(J,K(I,5))
                        ADIFF=MAX(0.,ADIFF)
                        SUPP=(1.-EXP(-PARJ(42)*ADIFF))
                     ELSE
                        SUPP=1.
                     ENDIF
                     IF (MASS(I,K(J,5)).LT.MRHO .OR.
     /                   MASS(J,K(I,5)).LT.MRHO) THEN
                        SUPP=0.
                     ENDIF
                     IF (RLU(0).LT.PROB*SUPP) THEN
                        CALL LEASWI(I,J)
                        NSWITCH=NSWITCH+1
                     ENDIF
                  ELSEIF (K(I,4).NE.J .AND. K(J,4).NE.I .AND.
     &                 GLUON1.AND.GLUON2) THEN
                     IF (MSTP188.EQ.3) THEN
                        ADIFF=AREA(I,K(I,4))+AREA(J,K(J,4))-
     -                        AREA(I,K(J,4))-AREA(J,K(I,4))
                        ADIFF=MAX(0.,ADIFF)
                        SUPP=(1.-EXP(-PARJ(42)*ADIFF))
                     ELSE
                        SUPP=1.
                     ENDIF
                     IF (MASS(I,K(J,4)).LT.MRHO .OR.
     /                   MASS(J,K(I,4)).LT.MRHO) THEN
                        SUPP=0.
                     ENDIF
                     IF (RLU(0).LT.PROB*SUPP) THEN
                        CALL LECSWI(I,J)
                        NSWITCH=NSWITCH+1
                     ENDIF
                     IF (MSTP188.EQ.3) THEN
                        ADIFF=AREA(I,K(I,5))+AREA(J,K(J,5))-
     -                        AREA(I,K(J,5))-AREA(J,K(I,5))
                        ADIFF=MAX(0.,ADIFF)
                        SUPP=(1.-EXP(-PARJ(42)*ADIFF))
                     ELSE
                        SUPP=1.
                     ENDIF
                     IF (MASS(I,K(J,5)).LT.MRHO .OR.
     /                   MASS(J,K(I,5)).LT.MRHO) THEN
                        SUPP=0.
                     ENDIF
                     IF (RLU(0).LT.PROB*SUPP) THEN
                        CALL LEASWI(I,J)
                        NSWITCH=NSWITCH+1
                     ENDIF
                  ENDIF
               ENDIF
30          CONTINUE
         ENDIF
20    CONTINUE
                 
      IF (NSWITCH.GT.0) THEN
        NS=N
        DO 40 I=1,NS
C--       find first quark (or anti diquark) string end
         IF (K(I,1).EQ.3 .AND. K(I,4).NE.0 .AND. K(I,5).EQ.0 )THEN
            NEXT=I
50          CONTINUE
            N=N+1
            IF(N.GT.MSTU(4)) THEN
CBB              MINT(51)=1
              MSTU(24)=1
              RETURN
            ENDIF
            THIS=NEXT
C--       copy to last row in event-record and update K-vector
            DO 60 J=1,5
               P(N,J)=P(THIS,J)
               V(N,J)=V(THIS,J)
               K(N,J)=K(THIS,J)
60          CONTINUE
            K(THIS,1)=13
            K(N,1)=2
            K(N,3)=THIS
            K(N,4)=0
            K(N,5)=0
C--       find next parton in string in row K(THIS,4)
            NEXT=K(THIS,4)
            IF (NEXT.NE.0) GOTO 50
C--       this is the last parton in string
            K(N,1)=1
         ENDIF
40      CONTINUE
        DO 70 I=1,NS
C--       find first gluon string end
         IF (K(I,1).EQ.3 .AND. K(I,2).EQ.21) THEN
            INIT=I
            NEXT=I
80          CONTINUE
            N=N+1
            IF(N.GT.MSTU(4)) THEN
CBB              MINT(51)=1
              MSTU(24)=1
              RETURN
            ENDIF
            THIS=NEXT
C--       copy to last row in event-record and update K-vector
            DO 90 J=1,5
               P(N,J)=P(THIS,J)
               V(N,J)=V(THIS,J)
               K(N,J)=K(THIS,J)
90          CONTINUE
            K(THIS,1)=13
            K(N,1)=2
            K(N,3)=THIS
            K(N,4)=0
            K(N,5)=0
C--       find next parton in string in row K(THIS,4)
            NEXT=K(THIS,4)
            IF (NEXT.NE.INIT) GOTO 80
C--       this is the last parton in string
            K(N,1)=1
         ENDIF
70      CONTINUE
      ELSE
C--       add incoming partons if no switches
       ADD=.FALSE.
       IF (ADD) THEN
        DO 200 I=1,NIN
         IF (KIN(I,1).LT.10 .AND. KIN(I,1).GT.0 .AND.
     &       (KIN(I,2).EQ.21 .OR. ABS(KIN(I,2)).LT.10
     %        .OR. LUCOMP(KIN(I,2)).EQ.90) ) THEN
            N=N+1
            DO 210 J=1,5
               P(N,J)=PIN(I,J)
               V(N,J)=VIN(I,J)
               K(N,J)=KIN(I,J)
210         CONTINUE
            K(I,1)=13
            K(N,3)=I
         ENDIF
200     CONTINUE
       ELSE
C--       copy incoming state if no switches
        DO 300 I=1,NIN
           DO 310 J=1,5
              P(I,J)=PIN(I,J)
              V(I,J)=VIN(I,J)
              K(I,J)=KIN(I,J)
310        CONTINUE
300     CONTINUE
        N=NIN
       ENDIF
      ENDIF
      RETURN
      end
      SUBROUTINE LEASWI(I,J)

C--       switch anticolour pointers for partons in rows I and J
C--       and colour pointers for the partons pointing back

      IMPLICIT NONE

C--       global variables
      INTEGER N,K
      REAL P,V
      COMMON /LUJETS/N,K(4000,5),P(4000,5),V(4000,5)

C--       local variables
      INTEGER I,J,KI5,KJ5

      KI5=K(I,5)
      KJ5=K(J,5)
      K(KI5,4)=J
      K(KJ5,4)=I
      K(I,5)=KJ5
      K(J,5)=KI5

      END
      SUBROUTINE LECSWI(I,J)

C--       switch colour pointers for partons in rows I and J
C--       and anticolour pointers for the partons pointing back

      IMPLICIT NONE

C--       global variables
      INTEGER N,K
      REAL P,V
      COMMON /LUJETS/N,K(4000,5),P(4000,5),V(4000,5)

C--       local variables
      INTEGER I,J,KI4,KJ4

      KI4=K(I,4)
      KJ4=K(J,4)
      K(KI4,5)=J
      K(KJ4,5)=I
      K(I,4)=KJ4
      K(J,4)=KI4

      END
