************************************************************************
*                                                                      *
* Kingal interface for grc4f ver2.1 (GR4F02)         Nov. 1998         *
*                            ver 2.2.43              April 2002        *
*                                   B.Bloch from R.Tanaka              *
*  modify include statements to access .h files ( not .f)              *
*                                                                      *
************************************************************************
      SUBROUTINE ASKUSI(IGCOD)
C ------
C ASKUSI     Initialization                  
C ------
C-----------------------------------------------------------------------  
      implicit none
C ----------------------------------------------------------------------   
C                                                                      
C grc4f : variables
C
      integer  maxact,maxdi4,maxdr8,maxdc8,ikyadr,ikytyp,keytch
      include 'inclpi.h'
      integer MPROC,mulprc,ichgcj,mevent
      real*8  xscton, xscter
      include 'inclan.h'
*
      character*8 grcpcn,dumpcn
      external    grcpcn
*    local 
      real*8  totalx,totale
      integer iopt,ierr,m,ic,isrtype
      integer mtotal,wtotal,ngensp,mwrite(MPROC)
      integer ntotal,nevt,ngen
      common /RTEVTC/ntotal,nevt(MPROC),ngen(MPROC)
*    - bases/spring my control
      real*4  energy
      integer set,hist
      character*8 process
      common /RTGRC1/energy,set,hist,process
      integer     lu,bssp,lubsdt,lubsrl,lubscs,lusprl,luspev,ipoint
      common /RTGRC2/bssp,lubsdt,lubsrl,lubscs,lusprl,luspev,ipoint
      character*256  fbsdir,fbsdt,fbsrl,fbscs,fsprl,fspev,fdum
      common /RTGRC3/fbsdir,fbsdt,fbsrl,fbscs,fsprl,fspev,fdum
C
C ----------------------------------------------------------------------   
C
C - Generator code (see KINLIB DOC)                                      
      INTEGER    IGCOD,IGCO,IVER
      PARAMETER (IGCO=5048, IVER=202)
C - BOS                                            
      INTEGER  ALTABL,ALRLEP,NAMIND,NLINK,KGPART,LUCOMP,INTCHA
      EXTERNAL ALTABL,ALRLEP,NAMIND,NLINK,KGPART,LUCOMP,INTCHA
      CHARACTER*4 CHAINT                                 
C - BCS
      INTEGER    LMHLEN,   LMHCOL,   LMHROW  , LBCS                    
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)              
      COMMON /BCS/IW(LBCS)                                             
      INTEGER IW                                                       
      REAL    RW(LBCS)                                                 
      EQUIVALENCE (RW(1),IW(1))                                        
C - LUND jetst74
      INTEGER    L1MST,L1PAR,L2PAR,L2PARF,LJNPAR
      PARAMETER (L1MST=200, L1PAR=200)                                  
      PARAMETER (L2PAR=500, L2PARF=2000 )                               
      PARAMETER (LJNPAR=4000)                                           
      INTEGER MSTU,MSTJ,KCHG,MDCY,MDME,KFDP,N7LU,K7LU
      REAL    PARU,PARJ,PMAS,PARF,VCKM,BRAT,P7LU,V7LU
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)   
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),        
     &                KFDP(L2PARF,5)                                    
      COMMON /LUDAT4/ CHAF(L2PAR)                                       
      CHARACTER*8 CHAF                                                  
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5) 
C - LUND decay variables                 
      INTEGER    LPDEC
      PARAMETER (LPDEC = 48)                                           
      INTEGER  NODEC(LPDEC),KNODEC
      EXTERNAL              KNODEC
C - TAUOLA
c      INTEGER JLIST,NCHAN
c      REAL    GAMPRT 
c      COMMON / TAUBRA / GAMPRT(30),JLIST(30),NCHAN 
c      COMMON / TAUKLE / BRA1,BRK0,BRK0B,BRKS       
c      REAL*4            BRA1,BRK0,BRK0B,BRKS       
C - Local variables
      INTEGER INUT,IOUT
      COMMON /INOUT/ INUT,IOUT
      CHARACTER*4   prcini,prcfin
      CHARACTER*256 ftitle
      integer i,j
      integer nkar,nkmx
      integer jgfbd
      integer ngprc,nghel,ngrad,ngpsm,ngmlp,ngmqk,ngcta,ngcte,ngcti
      integer jgprc,jghel,jgrad,jgpsm,jgmlp,jgmqk,jgcta,jgcte,jgcti
      integer ngbsp,ngtgc
      integer jgbsp,jgtgc
      integer jdebu,jtrig,jrmar,jsvrt,NCOL,NROW,JKPAR,IEBEAM
     &       ,JRLEP,IPART,JPART,IKLIN,KKPART,MXDEC,JIDB,NX
      real    W
      real*8  amw,agw,amz,agz,alp,sin2th
      real*8  ang1a,ankaa,anlma,ang1z,ankaz,anlmz,alpwp,alpw,alpbp

* - parametre table
      INTEGER      NTABL
      PARAMETER   (NTABL=100)
      REAL    TABL(NTABL)
      INTEGER KABL(NTABL)
      EQUIVALENCE (TABL,KABL)
* - event control
      INTEGER seed1,seed2
      REAL    sdvrt,vrtex
      COMMON /RTGRCP/SDVRT(3),VRTEX(4)
C - Event counter
      integer NEVENT
      INTEGER        NGENE,IDB1,IDB2,NISR,NFSR,KF4F,JF4F   
      COMMON /RTGRCE/NGENE,IDB1,IDB2,NISR,NFSR,KF4F(6),JF4F(6)          
      REAL           V4F,V4ISR,V4FSR,HEL4F
      COMMON /RTGRCV/V4F(6,5),V4ISR(20,5),V4FSR(20,5),HEL4F(6)
C - ntuple data -----------------------------------------------------
      CHARACTER*8 ZCVAR(25)
      DATA ZCVAR / 'KEVT','IDP',
     .             'enegy3','enegy4','enegy5','enegy6',
     .             'cos3'  ,'cos4'  ,'cos5'  ,'cos6'  ,
     .             'mass34','mass56','mass36','mass45',
     .             'cos34' ,'cos56' ,'cos36' ,'cos45' ,
     .             'amp4f' ,'ampww' ,'ampzz' ,'amplnw',
     .             'ampllz','ampnnz','ampgz' /
C -------------------------------------------------------------------
C
C BMACRO
C
C      INTEGER ID,NRBOS,L
C - # of words/row in bank with index ID                                
C      LCOLS(ID) = IW(ID+1)                                             
C - # of rows in bank with index ID                                    
C      LROWS(ID) = IW(ID+2)                                             
C - index of next row in the bank with index ID                        
C      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)                      
C - index of row # NRBOS in the bank with index ID                     
C      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)                
C - # of free words in the bank with index ID                          
C      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)                             
C - # of free rows in the bank with index ID                           
C      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)                              
C - Lth integer element of the NRBOSth row of the bank with index ID   
C      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)           
C - Lth real element of the NRBOSth row of the bank with index ID      
C      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)            
C                                                                    
C -------------------------------------------------------------------
C
      INUT = IW(5)                                                      
      IOUT = IW(6)                                                      
C                                                                       
C   Welcome !                                                           
C                                                                       
      IGCOD= IGCO                                                       
      WRITE(IOUT,101) IGCOD ,IVER                                       
  101 FORMAT(//15X,'+---------------------------------------------+'/   
     .      15X,'|                                             |'/     
     .      15X,'|     W E L C O M E   T O  grc4f              |'/     
     .      15X,'|     **********************************      |'/     
     .      15X,'|      Four fermion generator                 |'/     
     .      15X,'|             in e+e- collisions              |'/     
     .      15X,'|     **********************************      |'/     
     .      15X,'|             Code is # ',I6,'                |'/     
     .      15X,'|             version is # ',I6,'             |'/     
     .      15X,'|                                             |'/     
     .      15X,'|                                             |'/     
     .      15X,'|     Last date of change : April  29, 2002   |'/     
     .      15X,'|     Reisaburo TANAKA --- LPNHE              |'/
     .      15X,'|                 okayama university          |'/     
     .      15X,'|            (Tanaka@fphy.hep.okayama-u.ac.jp)|'/     
     .      15X,'|     Brigitte Bloch ----- DAPNIA/SPP         |'/ 
     .      15X,'|            (Brigitte.Bloc@cern.ch)          |'/     
     .      15X,'+---------------------------------------------+'//)   

C ----------------------------------------------------------------------
C --------------------
C grc4f initialization
C --------------------
      write(IOUT,*) '+++ASKUSI+++ : calling GRCEVT(-3)' 
      call GRCEVT(-3,iopt,ierr)
C
C Open output files 
C
C --------------------------------
C grc4f file : directory pass name
C --------------------------------
      ftitle = ' '
      jgfbd = NLINK('GFBD',0)
      IF (jgfbd.ne.0) THEN 
         nkar = IW(jgfbd)
         j = 1
         nkmx = MIN(nkar,64)
         do 11 i = 1,nkmx
            ftitle(j:j+3) = chaint(IW(jgfbd+i))
            j = j+4
 11      continue
         do i = 1,256
            if (ftitle(i:i).eq.' ') then
               ipoint = i-1
               goto 12
            end if
         end do
 12      continue
c         ipoint = ipoint-1
         fbsdir = ftitle(1:ipoint)
      END IF
         write(IOUT,'(a10,5x,a)') ' # of char','bases dir'
         write(IOUT,'(i10,5x,a,a)') ipoint,fbsdir(1:ipoint),'.'
C
C -------------------------
C grc4f file : BASES data
C -------------------------
         lubsdt = 31
         call GRCPAR('lubsdt',lubsdt,1,ierr)
C -------------------------
C grc4f file : BASES result
C -------------------------
         lubsrl = 32
         call GRCPAR('lubsrl',lubsrl,1,ierr)
C -----------------------------
C grc4f file : BASES integ data
C -----------------------------
         lubscs = 33
C --------------------------
C grc4f file : SPRING result
C --------------------------
         lusprl = 34
         call GRCPAR('lusprl',lusprl,1,ierr)
C ----------------------------------
C grc4f file : SPRING temporary data
C ----------------------------------
         luspev = 35
C
C ----------------------------------------------------------------------
C
 100  continue 
C
C -----------------------------------
C grc4f input : Read the process name
C -----------------------------------
      ngprc = NAMIND('GPRC')                 
      jgprc = IW(ngprc)                      
      IF (jgprc.NE.0) THEN
         energy = RW(jgprc+1)          
         set    = IW(jgprc+2)
         process(1:4) = chaint(IW(jgprc+3))
         process(5:8) = chaint(IW(jgprc+4))
         call GRCPAR('energy' ,dble(RW(jgprc+1)),1,ierr)
         call GRCPAR('set'    ,     IW(jgprc+2) ,1,ierr)
         call GRCPAR('process',process          ,1,ierr)
      END IF                       
         prcini = process(1:4)
         prcfin = process(5:8)
C ----------------------
C grc4f input : Helicity
C ----------------------
      nghel = NAMIND('GHEL')                 
      jghel = IW(nghel)                      
      IF (jghel.NE.0) THEN                    
         call GRCPAR('helicity1',IW(jghel+1),1,ierr)
         call GRCPAR('helicity2',IW(jghel+2),1,ierr)
         call GRCPAR('helicity3',IW(jghel+3),1,ierr)
         call GRCPAR('helicity4',IW(jghel+4),1,ierr)
         call GRCPAR('helicity5',IW(jghel+5),1,ierr)
         call GRCPAR('helicity6',IW(jghel+6),1,ierr)
      END IF                       
C ----------------------------------
C grc4f input : Radiative correction
C ----------------------------------
      ngrad = NAMIND('GRAD')                 
      jgrad = IW(ngrad)                      
      IF (jgrad.NE.0) THEN
         call GRCPAR('scheme'  ,IW(jgrad+1),1,ierr)
         call GRCPAR('width'   ,IW(jgrad+2),1,ierr)
         call GRCPAR('isrtype' ,IW(jgrad+3),1,ierr)
         call GRCPAR('coulomb' ,IW(jgrad+4),1,ierr)
         call GRCPAR('qcdcr'   ,IW(jgrad+5),1,ierr)
         call GRCPAR('gluon'   ,IW(jgrad+6),1,ierr)
         call GRCPAR('canon'   ,IW(jgrad+7),1,ierr)
         call GRCPAR('shower'  ,IW(jgrad+8),1,ierr)
         call GRCPAR('chrgconj',IW(jgrad+9),1,ierr)
         call GRCPAR('eisr'    ,dble(RW(jgrad+10)),1,ierr)
      END IF                       
C --------------------------------------
C grc4f input : Standard Model Parametre
C --------------------------------------
      ngpsm = NAMIND('GPSM')
      jgpsm = IW(ngpsm)
      IF (jgpsm.NE.0) THEN
         call GRCPAR('amw'   ,dble(RW(jgpsm+1)),1,ierr)
         call GRCPAR('agw'   ,dble(RW(jgpsm+2)),1,ierr)
         call GRCPAR('amz'   ,dble(RW(jgpsm+3)),1,ierr)
         call GRCPAR('agz'   ,dble(RW(jgpsm+4)),1,ierr)
         call GRCPAR('amh'   ,dble(RW(jgpsm+5)),1,ierr)
         call GRCPAR('agh'   ,dble(RW(jgpsm+6)),1,ierr)
         call GRCPAR('alpha' ,1.d0/dble(RW(jgpsm+7)),1,ierr)
         call GRCPAR('alphas',dble(RW(jgpsm+8)),1,ierr)
      END IF
C -------------------------------------
C grc4f input : particle mass - Leptons
C -------------------------------------
      ngmlp = NAMIND('GMLP')
      jgmlp = IW(ngmlp)
      IF (jgmlp.NE.0) THEN
            call GRCPAR('lmass',IW(jgmlp+1),1,ierr)
         IF (IW(jgmlp+1).eq.2) THEN
            call GRCPAR('lmass',1,1,ierr)
            call GRCPAR('amel',dble(RW(jgmlp+2)),1,ierr)
            call GRCPAR('ammu',dble(RW(jgmlp+3)),1,ierr)
            call GRCPAR('amta',dble(RW(jgmlp+4)),1,ierr)
         END IF
      END IF
C ------------------------------------
C grc4f input : particle mass - Quarks
C ------------------------------------
      ngmqk = NAMIND('GMQK')
      jgmqk = IW(ngmqk)
      IF (jgmqk.NE.0) THEN
            call GRCPAR('qmass',IW(jgmqk+1),1,ierr)
         IF (IW(jgmqk+1).eq.2) THEN
            call GRCPAR('qmass',1,1,ierr)
            call GRCPAR('amuq',dble(RW(jgmqk+2)),1,ierr)
            call GRCPAR('amcq',dble(RW(jgmqk+3)),1,ierr)
            call GRCPAR('amtq',dble(RW(jgmqk+4)),1,ierr)
            call GRCPAR('amdq',dble(RW(jgmqk+5)),1,ierr)
            call GRCPAR('amsq',dble(RW(jgmqk+6)),1,ierr)
            call GRCPAR('ambq',dble(RW(jgmqk+7)),1,ierr)
         END IF
      END IF
C -----------------------
C grc4f input : angle cut
C -----------------------
      ngcta = NAMIND('GCTA')
      jgcta = IW(ngcta)
      IF (jgcta.NE.0) THEN
         call GRCPAR('opncut'  ,dble(RW(jgcta+1)),1,ierr)
         call GRCPAR('thecut31',dble(RW(jgcta+2)),1,ierr)
         call GRCPAR('thecut32',dble(RW(jgcta+3)),1,ierr)
         call GRCPAR('thecut41',dble(RW(jgcta+4)),1,ierr)
         call GRCPAR('thecut42',dble(RW(jgcta+5)),1,ierr)
         call GRCPAR('thecut51',dble(RW(jgcta+6)),1,ierr)
         call GRCPAR('thecut52',dble(RW(jgcta+7)),1,ierr)
         call GRCPAR('thecut61',dble(RW(jgcta+8)),1,ierr)
         call GRCPAR('thecut62',dble(RW(jgcta+9)),1,ierr)
      END IF
C ------------------------
C grc4f input : energy cut
C ------------------------
      ngcte = NAMIND('GCTE')
      jgcte = IW(ngcte)
      IF (jgcte.NE.0) THEN
         call GRCPAR('engcut31',dble(RW(jgcte+1)),1,ierr)
         call GRCPAR('engcut32',dble(RW(jgcte+2)),1,ierr)
         call GRCPAR('engcut41',dble(RW(jgcte+3)),1,ierr)
         call GRCPAR('engcut42',dble(RW(jgcte+4)),1,ierr)
         call GRCPAR('engcut51',dble(RW(jgcte+5)),1,ierr)
         call GRCPAR('engcut52',dble(RW(jgcte+6)),1,ierr)
         call GRCPAR('engcut61',dble(RW(jgcte+7)),1,ierr)
         call GRCPAR('engcut62',dble(RW(jgcte+8)),1,ierr)
      END IF
C --------------------------------
C grc4f input : invariant mass cut
C --------------------------------
      ngcti = NAMIND('GCTI')
      jgcti = IW(ngcti)
      IF (jgcti.NE.0) THEN
         call GRCPAR('invcut341',dble(RW(jgcti+1)),1,ierr)
         call GRCPAR('invcut342',dble(RW(jgcti+2)),1,ierr)
         call GRCPAR('invcut561',dble(RW(jgcti+3)),1,ierr)
         call GRCPAR('invcut562',dble(RW(jgcti+4)),1,ierr)
         call GRCPAR('invcut351',dble(RW(jgcti+5)),1,ierr)
         call GRCPAR('invcut352',dble(RW(jgcti+6)),1,ierr)
         call GRCPAR('invcut461',dble(RW(jgcti+7)),1,ierr)
         call GRCPAR('invcut462',dble(RW(jgcti+8)),1,ierr)
         call GRCPAR('invcut361',dble(RW(jgcti+9)),1,ierr)
         call GRCPAR('invcut362',dble(RW(jgcti+10)),1,ierr)
         call GRCPAR('invcut451',dble(RW(jgcti+11)),1,ierr)
         call GRCPAR('invcut452',dble(RW(jgcti+12)),1,ierr)
      END IF 
C ------------------------------------
C grc4f input : BASES & SPRING control
C ------------------------------------
      ngbsp = NAMIND('GBSP')
      jgbsp = IW(ngbsp)
      IF (jgbsp.NE.0) THEN
         bssp    = IW(jgbsp+1)
         call GRCPAR('itmx1',IW(jgbsp+2),1,ierr)
         call GRCPAR('itmx2',IW(jgbsp+3),1,ierr)
         call GRCPAR('ncall',IW(jgbsp+4),1,ierr)
         call GRCPAR('mxtry',IW(jgbsp+5),1,ierr)
         call GRCPAR('acc1' ,dble(RW(jgbsp+6)),1,ierr)
         call GRCPAR('acc2' ,dble(RW(jgbsp+7)),1,ierr)
         hist    = IW(jgbsp+8)
      END IF 
C ---------------------------------------------------
C grc4f input : Anomalous triple gauge boson coupling
C ---------------------------------------------------
      ngtgc = NAMIND('GTGC')
      jgtgc = IW(ngtgc)
      IF (jgtgc.NE.0) THEN
c        anomal  = IW(jgtgc+1)
         IF (IW(jgtgc+1).eq.1 .or. IW(jgtgc+1).eq.2) then
            ang1a = 1.d0
c           ang1a = dble(RW(jgtgc+2))
            ankaa = dble(RW(jgtgc+3))
            anlma = dble(RW(jgtgc+4))
            ang1z = dble(RW(jgtgc+5))
            ankaz = dble(RW(jgtgc+6))
            anlmz = dble(RW(jgtgc+7))
            IF (IW(jgtgc+1).eq.2) then
               write(IOUT,*)  ' +++ASKUSI+++ : TGC SU(2)xU(1) constr.'
               amw = r8data(39)
               amz = r8data(41)
               alp = r8data(54)
               if (i4data(17).eq.0) then
                  sin2th=1.d0-(amw/amz)**2
               else
                  sin2th=dacos(-1.d0)*alp/dsqrt(2.d0)/1.16639d-5/amw**2
               end if
               ankaz = -(ankaa-1.d0)*sin2th/(1.d0-sin2th) + ang1z
               anlmz = anlma
            END IF   
         ELSE IF (IW(jgtgc+1).eq.3) then
C     - LEP (alpha_Wphi, alpha_W, alpha_Bphi)
            alpwp = dble(RW(jgtgc+2))
            alpw  = dble(RW(jgtgc+3))
            alpbp = dble(RW(jgtgc+4))
            write(IOUT,*)  ' +++ASKUSI+++ : TGC alpha_Wphi',alpwp
            write(IOUT,*)  '                    alpha_W   ',alpw
            write(IOUT,*)  '                    alpha_Bphi',alpbp
C
            write(IOUT,*)  ' +++ASKUSI+++ : TGC SU(2)xU(1) constr.'
            amw = r8data(39)
            amz = r8data(41)
            alp = r8data(54)
            if (i4data(17).eq.0) then
               sin2th=1.d0-(amw/amz)**2
            else
               sin2th=dacos(-1.d0)*alp/dsqrt(2.d0)/1.16639d-5/amw**2
            end if
C
            ang1a = 1.d0
            ankaa = alpbp + alpwp +1.d0
            anlma = alpw
            ang1z = alpwp/(1.d0-sin2th)+1.d0
            ankaz = -(ankaa-1.d0)*sin2th/(1.d0-sin2th) + ang1z
            anlmz = anlma
         END IF   
C
         IF (IW(jgtgc+1).ne.0) then      
            write(IOUT,*)  ' +++ASKUSI+++ :    sin2th',sin2th
            write(IOUT,*)  '                TGC ang1a',ang1a
            write(IOUT,*)  '                TGC ankaa',ankaa
            write(IOUT,*)  '                TGC anlma',anlma
            write(IOUT,*)  '                TGC ang1z',ang1z
            write(IOUT,*)  '                TGC ankaz',ankaz
            write(IOUT,*)  '                TGC anlmz',anlmz
            call GRCPAR('ang1a',ang1a,1,ierr)
            call GRCPAR('ankaa',ankaa,1,ierr)
            call GRCPAR('anlma',anlma,1,ierr)
            call GRCPAR('ang1z',ang1z,1,ierr)
            call GRCPAR('ankaz',ankaz,1,ierr)
            call GRCPAR('anlmz',anlmz,1,ierr)
         END IF   
      END IF
C
C ----------------------------------------------------------------------
C
C Input process name
C
      write(IOUT,'(A,A12,5X,A4,1X,A4)')  ' +++ASKUSI+++ : process name'
     .                                         ,process,prcini,prcfin
C
C ----------------------------------------------------------------------
C
C Set Number of Events
C
C  - Event counter
      NGENE = 0
C  - Debug events
      IDB1  = 1
      IDB2  = 5 
      jdebu = IW(NAMIND('DEBU'))
      IF ( jdebu .GT. 0 ) THEN
         IDB1 = IW(jdebu+1)
         IDB2 = IW(jdebu+2)
      END IF
C  - TRIG events   
      jtrig = NLINK('TRIG',0)
      IF (jtrig.NE.0) THEN
         nevent = IW(jtrig+2)-IW(jtrig+1)+1
         IF (IW(jtrig).GE.3) nevent=IW(jtrig+3)
         write(IOUT,*) '+++ASKUSI+++ : nevent from TRIG card',nevent
      ELSE
         write(IOUT,*) '+++ASKUSI+++ : no TRIG card found !'
         nevent = 1000
         write(IOUT,*) '               default nevent used  ',nevent
      END IF
C ----------------------------------------------------------------------
C
C Check seed for Random Number (RANMAR)
C
      jrmar = NLINK('RMAR',0)
      IF (jrmar.NE.0) THEN
         seed1 = IW(jrmar+1)
         seed2 = IW(jrmar+2)
         write(IOUT,*) '+++ASKUSI+++ : seed from RMAR card',seed1,seed2
      ELSE
         write(IOUT,*) '+++ASKUSI+++ : no RMAR card found'
         write(IOUT,*) '               default seed in RDMIN used  '
      END IF
C    - dummy for grc4f intrinsic random number generator 
C      call GRCPAR('seed' ,seed1,1,ierr)
C      print *,'  seed after GRCPAR call =',i4data(ikyadr(18)) 
C
C ----------------------------------------------------------------------
C
C  Vertex smearing initialization
C
C     defaults MC97 values 
      SDVRT(1) = 0.0150
      SDVRT(2) = 0.0005
      SDVRT(3) = 0.66  
      jsvrt = NLINK('SVRT',0)
      IF (jsvrt.NE.0) THEN
         SDVRT(1) = RW(jsvrt+1)
         SDVRT(2) = RW(jsvrt+2)
         SDVRT(3) = RW(jsvrt+3)
      END IF
      write(IOUT,*) '+++ASKUSI+++ : vertex smearing',(SDVRT(J),J=1,3)
C ----------------------------------------------------------------------
C
C - Parametre Initialization
C
C process name
         TABL(1) = real(intcha(prcini))
         TABL(2) = real(intcha(prcfin))
C steer integer
      do 20 i=1,maxdi4
         j = 2+i 
         TABL(j) = real(i4data(i-1))
 20   continue 
C steer real
      do 21 i=1,maxdr8
         j = 2+maxdi4+i  
         TABL(j) = sngl(r8data(i-1))
 21   continue 
C vertex
      do 22 i=1,3
         j = 2+maxdi4+maxdr8+i
         TABL(j) = sdvrt(i)
 22   continue   
C
C  Fill the KPAR bank with the generator parameters
C             2+  27  +  56  + 3 = 88
      NCOL  = 2+maxdi4+maxdr8+3
      NROW  = 1
      JKPAR = ALTABL('KPAR',NCOL,NROW,TABL,'2I,(F)','C')
      if (JKPAR.LE.0) then
         write(IOUT,*) '+++ASKUSI+++ : error in filling KPAR bank'
      end if   
C
C  Fill RLEP bank
C
      IEBEAM = NINT(energy*500.)
      JRLEP  = ALRLEP(IEBEAM,'    ',0,0,0)
      if (JRLEP.LE.0) then
         write(IOUT,*) '+++ASKUSI+++ : error in filling RLEP bank'
      end if   
C-----------------------------------------------------------------------
C                                                               
C  Create the KLIN bank and complete the PART bank             
C                                                               
      call KXL74A(IPART,IKLIN)                                  
      IF (IPART.LE.0 .OR. IKLIN.LE.0) THEN                      
         WRITE (IOUT,                                            
     &    '(1X,''+++ASKUSI+++ :error in PART or KLIN bank - STOP - '' 
     &                 ,2I3)') IPART,IKLIN                      
        STOP                                                    
      END IF                                                     
C
C  modify Lund masses according to input masses ...need for grc4f ?
C            Lund code
c     a1     20213 
c      PMAS(LUCOMP(20213),1)= 1.251
c      PMAS(LUCOMP(20213),2)= 0.599
C
C Update masses and width in the PART bank for Z, W.
C
      JPART = NLINK('PART',0)
      IF (JPART.GT.0) THEN
         amz = r8data(41)
         agz = r8data(42)
         amw = r8data(39)
         agw = r8data(40)
         WRITE (IOUT,                                            
     &    '(1X,''+++ASKUSI+++ : update PART bank'' 
     &                 ,4F10.5)') amz,agz,amw,agw                      
C  Z0
         KKPART = KGPART(23)
         IF (KKPART.GT.0) THEN
            RW(JPART+LMHLEN+(KKPART-1)*IW(JPART+1)+6) = sngl(amz)
            RW(JPART+LMHLEN+(KKPART-1)*IW(JPART+1)+9) = sngl(agz)
         END IF
C  W
         KKPART = KGPART(24)
         IF (KKPART.GT.0) THEN
            RW(JPART+LMHLEN+(KKPART-1)*IW(JPART+1)+6) = sngl(amw)
            RW(JPART+LMHLEN+(KKPART-1)*IW(JPART+1)+9) = sngl(agw)
         END IF
         KKPART = KGPART(-24)
         IF (KKPART.GT.0) THEN
            RW(JPART+LMHLEN+(KKPART-1)*IW(JPART+1)+6) = sngl(amw)
            RW(JPART+LMHLEN+(KKPART-1)*IW(JPART+1)+9) = sngl(agw)
         END IF

c a1                                                                      
c         IHPART = KGPART(20213)                                          
c         IF (IHPART.GT.0)  THEN                                          
c            ZMAS = PMAS(LUCOMP(20213),1)                                  
c            ZWID = PMAS(LUCOMP(20213),2)                                  
c            KPART = KROW(JPART,IHPART)                                    
c            RW(KPART+6)=ZMAS                                              
c            RW(KPART+9)=ZWID                                              
c            IANTI = ITABL(JPART,IHPART,10)                                
c            IF (IANTI.NE.IHPART) THEN                                     
c               KAPAR = KROW(JPART,IANTI)                               
c               RW(KAPAR+6)=ZMAS                                             
c               RW(KAPAR+9)=ZWID                                              
c            ENDIF                                                           
c         ENDIF                                                             
      ELSE
         write(IOUT,*) '+++ASKUSI+++ : error in filling PART bank'
C   
      END IF
C
C   
C   Inhibit LUND decays (should be done in GALEPH)
C                                                                       
      MXDEC = KNODEC(NODEC,LPDEC)                                         
      MXDEC = MIN(MXDEC,LPDEC)                                            
      IF (MXDEC.GT.0) THEN                                              
        DO 50 I=1,MXDEC                                                 
          IF (NODEC(I).GT.0) THEN                                       
            JIDB = NLINK('MDC1',NODEC(I))                               
            IF (JIDB .EQ. 0) MDCY(LUCOMP(NODEC(I)),1) = 0               
          END IF                                                         
   50   CONTINUE                                                        
      END IF                                                             
C                                                                       
C  Print PART and KPAR banks                                            
C                                                                       
       call LULIST(12)                                                   
       call PRPART                                                       
       call PRTABL('RLEP',0)
       call PRTABL('KPAR',0)
c      call PRTABL('KREF',9600)
c      call PRTABL('KCAR',0)
C
C -----------------------------------------------------------
C                                                                       
C    possibly update branching ratios  with card GKBR                   
C                                                                       
c      NAGKBR = NAMIND('GKBR')                                           
c      JGKBR = IW(NAGKBR)                                                
c      IF(JGKBR.NE.0) THEN                                               
C check consitency of length                                           
c        NLEN = IW(JGKBR)                                                
c        IF ( NLEN .NE.NCHAN+4 ) THEN                                    
c            WRITE (IW(6),'(1X,'' Inconsistent number of Brs should be'',
c     $                    I5,'' is '',I5)') NCHAN,NLEN-4                
CCC         call EXIT                                                   
c        ENDIF                                                           
c        BRA1   = RW(JGKBR+1)                                            
c        BRK0   = RW(JGKBR+2)                                            
c        BRK0B  = RW(JGKBR+3)                                            
c        BRKS   = RW(JGKBR+4)                                            
c        DO 51 I = 1,NCHAN                                               
c           GAMPRT(I) = RW(JGKBR+4+I)                                    
c 51     CONTINUE                                                        
c        IF ( GAMPRT(1).NE.1.) THEN                                      
c         DO 52 I = 1, NCHAN                                             
c           GAMPRT(I) = GAMPRT(I)/GAMPRT(1)                              
c 52      CONTINUE                                                       
c        ENDIF                                                           
c      ENDIF                                                             
C                                                                       
C   Store the version used in the job and the branching ratios in       
C   header bank  KORL                                                   
c      NCOL = NCHAN+5                                                    
c      NROW = 1                                                          
c      KABL(1) = IVER                                                    
c      TABL(2) = BRA1                                                    
c      TABL(3) = BRK0                                                    
c      TABL(4) = BRK0B                                                   
c      TABL(5) = BRKS                                                    
c      DO 57 IBR = 1,NCHAN                                               
c          TABL(5+IBR) = GAMPRT(IBR)                                     
c 57   CONTINUE                                                          
c      JKORL = ALTABL('KORL',NCOL,NROW,TABL,'2I,(F)','C')                
c      call PRTABL('KORL',0)                                             
C
C -------------------------------------------------------------------- 
C HBOOK Initialization
      if (hist.eq.1) then
         write(IOUT,*) '+++ASKUSI+++ : BASES/SPRING+geneated evts hist'
      else if (hist.eq.2) then
         write(IOUT,*) '+++ASKUSI+++ : BASES/SPRING+geneated evts hist'
         write(IOUT,*) '             : also Ntuple data added'   
      else
         write(IOUT,*) '+++ASKUSI+++ : standard BASES/SPRING hist only'
         goto 200
      end if   
*--- 6. Set histograms
      NX = 50
      W  = energy
* - 4f KF code
      call HBOOK1(40001,'Flavour (KF-code) fermion 1',33,-16.5,16.5,0.)
      call HBOOK1(40002,'Flavour (KF-code) fermion 2',33,-16.5,16.5,0.)
      call HBOOK1(40003,'Flavour (KF-code) fermion 3',33,-16.5,16.5,0.)
      call HBOOK1(40004,'Flavour (KF-code) fermion 4',33,-16.5,16.5,0.)
      call HBOOK1(40005,'Flavour (KF-code) fermion 5',33,-16.5,16.5,0.)
      call HBOOK1(40006,'Flavour (KF-code) fermion 6',33,-16.5,16.5,0.)
* - 4f helicity
      call HBOOK1(40007,'helicity fermion 1',5,-1.25,1.25,0.)
      call HBOOK1(40008,'helicity fermion 2',5,-1.25,1.25,0.)
      call HBOOK1(40009,'helicity fermion 3',5,-1.25,1.25,0.)
      call HBOOK1(40010,'helicity fermion 4',5,-1.25,1.25,0.)
      call HBOOK1(40011,'helicity fermion 5',5,-1.25,1.25,0.)
      call HBOOK1(40012,'helicity fermion 6',5,-1.25,1.25,0.)
* - energy 
      CALL HBOOK1(40013,'Energy of Particle 1' , NX, 0., W, 0.0)
      CALL HBOOK1(40014,'Energy of Particle 2' , NX, 0., W, 0.0)
      CALL HBOOK1(40015,'Energy of Particle 3' , NX, 0., W, 0.0)
      CALL HBOOK1(40016,'Energy of Particle 4' , NX, 0., W, 0.0)
      CALL HBOOK1(40017,'Energy of Particle 5' , NX, 0., W, 0.0)
      CALL HBOOK1(40018,'Energy of Particle 6' , NX, 0., W, 0.0)
* - angle
      CALL HBOOK1(40019,'cos(the) of Particle 1',120,-1.2,1.2, 0.0)
      CALL HBOOK1(40020,'cos(the) of Particle 2',120,-1.2,1.2, 0.0)
      CALL HBOOK1(40021,'cos(the) of Particle 3',120,-1.2,1.2, 0.0)
      CALL HBOOK1(40022,'cos(the) of Particle 4',120,-1.2,1.2, 0.0)
      CALL HBOOK1(40023,'cos(the) of Particle 5',120,-1.2,1.2, 0.0)
      CALL HBOOK1(40024,'cos(the) of Particle 6',120,-1.2,1.2, 0.0)
* - invariant mass
      CALL HBOOK1(40025,'Mass 3-4 '            , NX, 0., W, 0.0)
      CALL HBOOK1(40026,'Mass 5-6 '            , NX, 0., W, 0.0)
      CALL HBOOK1(40027,'Mass 3-6 '            , NX, 0., W, 0.0)
      CALL HBOOK1(40028,'Mass 4-5 '            , NX, 0., W, 0.0)
      CALL HBOOK1(40029,'Mass 3-5 '            , NX, 0., W, 0.0)
      CALL HBOOK1(40030,'Mass 4-6 '            , NX, 0., W, 0.0)
* - invariant mass scatter plot
      call HBOOK2(40031,' M34-M36 ', NX, 0., W, NX, 0., W, 0.0) 
      call HBOOK2(40032,' M56-M45 ', NX, 0., W, NX, 0., W, 0.0)  
      call HBOOK2(40033,' M34-M56 ', NX, 0., W, NX, 0., W, 0.0) 
      call HBOOK2(40034,' M36-M45 ', NX, 0., W, NX, 0., W, 0.0)  
* - ISR/FSR
      call HBOOK1(40041,'Number of ISR photons'    ,20,-0.5,19.5,0.)       
      call HBOOK1(40042,'ISR total energy'         ,40,0., 60.,0.)    
      call HBOOK1(40043,'ISR dN/dE(gamma)'         ,40,0., 20.,0.)
      call HBOOK1(40044,'ISR dN/dPt(gamma)'        ,40,0., 20.,0.)
      call HBOOK1(40045,'Number of FSR photons'    ,20,-0.5,19.5,0.)       
      call HBOOK1(40046,'FSR total energy'         ,40,0., 60.,0.)
      call HBOOK1(40047,'FSR dN/dE(gamma)'         ,40,0., 20.,0.)
      call HBOOK1(40048,'FSR dN/dPt(gamma)'        ,40,0., 20.,0.)
      call HBOOK1(40049,'ISR   log10(Etot)'        ,50,-3.,2.,0.)    
* - jetset multiplicity
      call HBOOK1(40051,'grc4f jetset7.4 N',        101,-0.5,100.5,0.)
      call HBOOK1(40052,'grc4f jetset7.4 N_neutral',101,-0.5,100.5,0.)
      call HBOOK1(40053,'grc4f jetset7.4 N_charged',101,-0.5,100.5,0.)
      call HBOOK1(40054,'grc4f jetset7.4 N_chtrack',101,-0.5,100.5,0.)
* - vertex
      call HBOOK1(40055,'Vertex smearing x(cm)',NX,-0.1, 0.1,0.)
      call HBOOK1(40056,'Vertex smearing y(cm)',NX,-0.003,0.003,0.)
      call HBOOK1(40057,'Vertex smearing z(cm)',NX,-5.0,5.0,0.)
* - tauola info
      call HBOOK1(40061,'in UTOPOL tau+ P3',5,-2.5,2.5,0.) 
      call HBOOK1(40062,'in UTOPOL tau- P3',5,-2.5,2.5,0.) 
* - amplitude**2 info ... log10(A2)
      call HBOOK1(40070,'4f'            ,100,-20.,0.,0.) 
      call HBOOK1(40071,'WW-CC03'       ,100,-20.,0.,0.) 
      call HBOOK1(40072,'ZZ-NC02'       ,100,-20.,0.,0.) 
      call HBOOK1(40073,'lnW (non CC03)',100,-20.,0.,0.) 
      call HBOOK1(40074,'llZ (non NC08)',100,-20.,0.,0.) 
      call HBOOK1(40075,'nnZ (non MC08)',100,-20.,0.,0.) 
      call HBOOK1(40076,'gam-gam'       ,100,-20.,0.,0.) 
      call HBOOK1(40077,'s-ch'          ,100,-20.,0.,0.) 
      call HBOOK1(40078,'t-ch'          ,100,-20.,0.,0.) 
      call HBOOK1(40079,'multi&brems(6)',100,-20.,0.,0.) 
      call HBOOK1(40080,'multi only'    ,100,-20.,0.,0.)
      call HBOOK1(40081,'4f but multi'  ,100,-20.,0.,0.)  
      call HBOOK1(40082,'gg/gZ/ZZ-NC08' ,100,-20.,0.,0.)  
      call HBOOK1(40083,'WW+lnW (1+3)'  ,100,-20.,0.,0.)  
      call HBOOK1(40084,'llZ+gg/gZ/ZZ'  ,100,-20.,0.,0.)  
      call HBOOK1(40085,'nnZ+gg/gZ/ZZ'  ,100,-20.,0.,0.)  
*
* - reconstructed WW/ZZ
      call HBOOK1(40091,'cos(the) of Particle 3+4',50,-1.0,1.0,0.0) 
      call HBOOK1(40092,'cos(the) of Particle 5+6',50,-1.0,1.0,0.0) 
      call HBOOK1(40093,'cos(the) of Particle 3+6',50,-1.0,1.0,0.0) 
      call HBOOK1(40094,'cos(the) of Particle 4+5',50,-1.0,1.0,0.0) 
      call HBOOK1(40095,'phi of Particle 3+4' ,50,-180.0,180.0,0.0) 
      call HBOOK1(40096,'phi of Particle 5+6' ,50,-180.0,180.0,0.0) 
      call HBOOK1(40097,'phi of Particle 3+6' ,50,-180.0,180.0,0.0) 
      call HBOOK1(40098,'phi of Particle 4+5' ,50,-180.0,180.0,0.0) 
*
C ... book the Ntuple
      if (hist.eq.2) then
         CALL HBOOKN(5010,'ntuple',25,' ',4000,ZCVAR)
      end if   
*
 200  continue
C ---------------------------------------------------
C grc4f
C ---------------------------------------------------
C Print again parametres
c      write(IOUT,*) '+++ASKUSI+++ : calling grcset(1)'    
c      call GRCSET(1)
C -------------------------------
C grc4f : Process Initialization
C -------------------------------
      write(IOUT,*) '+++ASKUSI+++ : calling GRCEVT(-2)'    
      call GRCEVT(-2,iopt,ierr)
C -------------------------
C grc4f : Integration loop 
C -------------------------    
      write(IOUT,*) '+++ASKUSI+++ : # of process for BASES',mulprc(0)
C
C -----------
C BASES call
C -----------
      if (bssp.eq.0 .or. bssp.eq.1) then
*      
         do 3100 m = 1 , mulprc(0)
C - open bases data file
            fbsdt = fbsdir(1:ipoint)//'/'//grcpcn(m)//'.bases_data'
            fdum  = fbsdt
            lu    = lubsdt
C            open(lubsdt,file=fbsdt,status='unknown',
C     .                  form='unformatted',err=1200)
            open(lubsdt,file=fbsdt,status='unknown')
C - open bases result file
            fbsrl = fbsdir(1:ipoint)//'/'//grcpcn(m)//'.bases_result'
            fdum  = fbsrl
            lu    = lubsrl
            open(lubsrl,file=fbsrl,status='unknown')
C     .                  form='formatted',err=1200)
C - open cross section data file
            fbscs = fbsdir(1:ipoint)//'/'//grcpcn(m)//'.bases_integ'
            fdum  = fbscs
            lu    = lubscs
            open(lubscs,file=fbscs,status='unknown')
C     .               form='formatted',err=1200)
C
C - Minimum Minv protection
            write(IOUT,*) '+++ASKUSI+++ : BS invariant mass protection'
            call grcmct(m)
C - integration
            write(IOUT,*) '+++ASKUSI+++ : calling GRCEVT(-1)'    
            call GRCEVT(-1,m,ierr)
C - output
            write(IOUT,*) '+++ASKUSI+++ : integral result'
            write(IOUT,102) mulprc(m),ichgcj(m),mevent(m)
            write(IOUT,103) grcpcn(m),xscton(m),xscter(m)
            write(lubscs,104) grcpcn(m),mulprc(m),ichgcj(m)
     .                    ,xscton(m),xscter(m)
            close(lubsdt)
            close(lubsrl)
            close(lubscs)
 3100    continue
C
 102  format(' Process:',i12,' chg conj fact =',i12  ,' mevent =',i12)
 103  format(' Process:',a12,' cross section =',d12.5,' +-',d12.5,' pb')
 104  format(a8,i5,i5,2d12.5)
C
C -----------
C SPRING only
C -----------
      else if (bssp.eq.2) then
         do 3200 m = 1 , mulprc(0)
C - open cross section data file
            fbscs = fbsdir(1:ipoint)//'/'//grcpcn(m)//'.bases_integ'
            fdum  = fbscs
            lu    = lubscs
            open(lubscs,file=fbscs,status='old',
     .               form='formatted',err=1200)
C - input
C           read(lubscs,104) grcpcn(m),mulprc(m),ichgcj(m)
            read(lubscs,104) dumpcn   ,mulprc(m),ichgcj(m)
     .                    ,xscton(m),xscter(m)
            write(IOUT,*) '+++ASKUSI+++ : integral result'
            write(IOUT,102) mulprc(m),ichgcj(m),mevent(m)
            write(IOUT,103) grcpcn(m),xscton(m),xscter(m)
            close(lubscs)
 3200    continue
      end if   
C
*-----------------------------------------------------------------------
*     Calculation of Total cross section
*-----------------------------------------------------------------------
      totalx = 0.0d0
      totale = 0.0d0
      do 4100 m = 1 , mulprc(0)
         totalx = totalx + ichgcj(m)*xscton(m)
         totale = totale +(ichgcj(m)*xscter(m))**2
         write(IOUT,105) ' cross section : ',m,grcpcn(m),mulprc(m)
     .          ,ichgcj(m),ichgcj(m)*xscton(m),ichgcj(m)*xscter(m)
 4100 continue
         totale = dsqrt(totale) 
         write(IOUT,'(a,27x,2f12.5)') ' Total         : ',totalx,totale
 105  format(a,i5,a12,i5,i5,2f12.5)
C
*-----------------------------
*     Stop for BASES only
* ---------------------------- 
      if (bssp.eq.1) then
         call bshbok(0)
         
         stop
      end if   
*
C ----------------------------
C Initialization for libraries
C ----------------------------
C call always TAUOLA for tau decay
      call LUGIVE('MSTJ(28)=2')
C Interface to TAUOLA/PHOTOS (May 1997, R.Tanaka)
      call grcpho(-1)
*
C ISR/FSR control
c     PHOTOS ... FSR for promt e/mu/(tau)
c     Jetset ... FSR (gluon/gamma) in parton shower (LUSHOW)
c 
c     isrtype   fsrtype   ISR       FSR
c    ------------------------------------------------------------
c     0         -         none      none
c     1         0/1       SF        PHOTOS+Jetset(gluon+gamma)
c     2         0/1       QEDPS     PHOTOS+Jetset(gluon+gamma)
c     3         -         QEDPS     QEDPS +Jetset(gluon+gamma)
c    ------------------------------------------------------------
c
      isrtype = i4data(ikyadr(7))
      write(IOUT,'(a,2i10)') ' isrtype',isrtype
c     write(IOUT,'(a,2i10)') ' isrtype, fsrtype',isrtype,fsrtype
      if (isrtype.eq.0) then 
            call LUGIVE('MSTJ(41)=0')
      else if (isrtype.eq.1 .or. isrtype.eq.2) then
c         if (fsrtype.eq.0) then
c            call LUGIVE('MSTJ(41)=0')
c         else
c            call LUGIVE('MSTJ(41)=2')
c         end if
          call LUGIVE('MSTJ(41)=2') 
      else if (isrtype.eq.3) then
c           call LUGIVE('MSTJ(41)=1')
            call LUGIVE('MSTJ(41)=2')
      else
         write(IOUT,'(a,i10)') ' irregal isrtype',isrtype
         stop
      end if
c FSR control in Jetset, FSR(photon/gluon) in parton-shower follows

C ISR control in Jetset, no rad. correction for continuum (D)
c     call LUGIVE('MSTJ(107)=0')
*-----------------------------------------------------------------------
C -------------------------
C grc4f : event generation 
C -------------------------    
*       nevt(m): the number of events for each processes.
*-----------------------------------------------------------------------
      do m=1,mulprc(0)
         nevt(m) = nint(ichgcj(m)*xscton(m)/totalx*Nevent)
         write(IOUT,'(a,2i10)') ' required  # of evt nevt: ',m,nevt(m)
c        mevent(m)=0
         mwrite(m)=0
      end do
C
 5100 continue   
C
      ntotal = 0
      mtotal = 0
         write(IOUT,107) '   i ',' process #','   id','   cc'
     .    ,' cross section (pb) ',' requested',' generated'
      do 5200 m = 1 , mulprc(0)
         ntotal = ntotal + nevt(m)
         mtotal = mtotal + mevent(m)
         write(IOUT,106) m,grcpcn(m),mulprc(m)
     .            ,ichgcj(m),ichgcj(m)*xscton(m),ichgcj(m)*xscter(m)
     .            ,nevt(m),mevent(m)
 5200 continue
         write(IOUT,'(25x,2f10.5,2i10//)') totalx,totale,ntotal,mtotal
C - loop until ntotal = Nevent
      if (ntotal.eq.Nevent) goto 5400
 5300 continue 
      do m = 1,mulprc(0)
         nevt(m) = nevt(m)-isign(1,ntotal-Nevent)
         ntotal  = ntotal -isign(1,ntotal-Nevent)
         if (ntotal.eq.Nevent) goto 5100
      end do
      goto 5300
C     stop
C ----------------------------------------------------------------
 5400 continue
C -----------
C SPRING call
C -----------
      do 5500 m=1,mulprc(0)
C - open BASES data
         fbsdt = fbsdir(1:ipoint)//'/'//grcpcn(m)//'.bases_data'
         fdum  = fbsdt
         lu    = lubsdt
         open(lubsdt,file=fbsdt,status='old')
C     .            form='unformatted',err=1200)
C - open SPRING result
         fsprl = fbsdir(1:ipoint)//'/'//grcpcn(m)//'.spring_result'
         fdum  = fsprl
         lu    = lusprl
         open(lusprl,file=fsprl,status='unknown')
C     .            form='formatted',err=1200)
C - open temporary event data
c        fspev = fbsdir(1:ipoint)//'/'//grcpcn(m)//'.event_data'
         fspev = grcpcn(m)//'.event_data'
         fdum  = fspev
         lu    = luspev
         open(luspev,file=fspev,status='unknown')
C     .            form='unformatted',err=1200)
C
C - Minimum Minv protection
         write(IOUT,*) '+++ASKUSI+++ : SP invariant mass protection'
         call grcmct(m)
C
* - loop
C     generate 20% more events
         ngensp = nint(nevt(m)*1.2)
cc       ngensp = nevt(m)
         write(IOUT,'(a,2i10)') ' ..... generating # evts : ',ngensp
         do 5600 i=1,ngensp
C ---------------------------
C - generate event
C ---------------------------
            NGENE = NGENE + 1
C           write(IOUT,*) '+++ASKUSI+++ : calling GRCEVT(0)',i    
            call GRCEVT(0,m,ierr)
            if(ierr.ne.0) then
               write(IOUT,*) '+++ASKUSI+++ : error in GRCEVT(0)',ierr     
               goto 5600
            end if
            if (i.ge.IDB1 .AND. i.le.IDB2) then      
c           if (i.eq.1) then      
               write(IOUT,*) '+++ASKUSI+++ : after GRCEVT(0)'   
               call lulist(1)
C              call grchel
            endif
C ---------------------------
* - write event on file
C --------------------------- 
           if(luspev.ne.0)then
               call wlist(luspev,ic)
               if (ic.ne.0) then
                  write(IOUT,*) '+++ASKUSI+++ : write error in wlist'
               else 
                  mwrite(m) = mwrite(m)+1
               end if   
            endif
 5600    continue
C
         write(IOUT,*) '+++ASKUSI+++ : calling GRCEVT(1)'    
         call GRCEVT(1,m,ierr)
C
         close(lubsdt)
         close(lusprl)
         close(luspev)
C
 5500 continue
*==================
* end of generation
*==================
C   - reset event counter 
      NGENE = 0
*----------------
* user summary
*----------------
      ntotal = 0
      mtotal = 0
      wtotal = 0
         write(IOUT,*) '+++ASKUSI+++ : event summary ++++++++++++++++++'
         write(IOUT,107) '   i ',' process #','   id','   cc'
     .    ,' cross section (pb) ',' requested',' generated',' written  '
      do 5700 m = 1 , mulprc(0)
         ntotal = ntotal + nevt(m)
         mtotal = mtotal + mevent(m)
         wtotal = wtotal + mwrite(m)
         write(IOUT,106) m,grcpcn(m),mulprc(m),ichgcj(m)
     .                  ,ichgcj(m)*xscton(m),ichgcj(m)*xscter(m)
     .                  ,nevt(m),mevent(m),mwrite(m)
 5700 continue
      write(IOUT,'(25x,f10.5,10x,3i10//)') totalx,ntotal,mtotal,wtotal
 106  format(i5,a10,i5,i5,2f10.5,3i10)
 107  format(////a5,a10,a5,a5,a20,3a10//)
*-----------------------------------------------------------------------
C --- Check number of events
c      do 5800 m = 1 , mulprc(0)
c         if (nevt(m).ne.mwrite(m)) then
c            write(IOUT,*) ' Error in requested or written # of events'
c            write(IOUT,*) m,' nevt=',nevt(m),' mwrite=',mwrite(m)
c            STOP
c         end if   
c 5800 continue
C
 1100 write(IOUT,*) '+++ASKUSI+++ : end of this routine'    
      RETURN
C
 1200 write(IOUT,*) '+++ASKUSI+++ : error in opening ',lu,' ',fdum,'.'
      STOP
C
      END



      SUBROUTINE ASKUSE (IDP,IST,NTRK,NVRT,ECM,WEI)                     
C --------------------------------------------------------------------  
C     input     : none                                                  
C                                                                       
C     output    : 6 arguments                                           
C          IDP    : process identification                              
C          IST    : status flag ( 0 means ok)                           
C          NTRK   : number of tracks generated and kept                 
C          NVRT   : number of vertices generated                        
C          ECM    : center of mass energy for the event                 
C          WEI    : event weight always equal to 1                      
C--------------------------------------------------------------------   
      implicit none
C ----------------------------------------------------------------------   
C                                                                      
C grc4f : variables
C
      integer  maxact,maxdi4,maxdr8,maxdc8,ikyadr,ikytyp,keytch
      include 'inclpi.h'
      integer MPROC,mulprc,ichgcj,mevent
      real*8  xscton, xscter
      include 'inclan.h'
*
      character*8 grcpcn
      external    grcpcn
*    local 
      integer ierr,m
      integer ntotal,nevt,ngen,ntab
      common /RTEVTC/ntotal,nevt(MPROC),ngen(MPROC)
      real*4   xxx,tabmin,tabmax
      common /RTEVTT/tabmin(MPROC),tabmax(MPROC)
C     save tabmin,tabmax
      real*4   rndm
      external rndm
*    - bases/spring my control
      real*4  energy
      integer set,hist
      character*8 process
      common /RTGRC1/energy,set,hist,process
      integer     lu,bssp,lubsdt,lubsrl,lubscs,lusprl,luspev,ipoint
      common /RTGRC2/bssp,lubsdt,lubsrl,lubscs,lusprl,luspev,ipoint
      character*256  fbsdir,fbsdt,fbsrl,fbscs,fsprl,fspev,fdum
      common /RTGRC3/fbsdir,fbsdt,fbsrl,fbscs,fsprl,fspev,fdum
C - helicity
      integer lect
      common/helicity/lect(4000)
C - amplitude calc
      integer kproc
      real*8  am2(3,0:15)
C ----------------------------------------------------------------------   
C BCS
      INTEGER    LMHLEN,   LMHCOL,   LMHROW  , LBCS                    
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)              
      COMMON /BCS/IW(LBCS)                                             
      INTEGER IW                                                       
      REAL    RW(LBCS)                                                 
      EQUIVALENCE (RW(1),IW(1))
C - LUND jetst74
      INTEGER LJNPAR
      PARAMETER (LJNPAR=4000)
      INTEGER N7LU,K7LU
      REAL    P7LU,V7LU
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5) 
      INTEGER  KLU
      REAL     PLU
      EXTERNAL KLU,PLU
C
      INTEGER INUT,IOUT
      COMMON /INOUT/ INUT,IOUT                                        
C Input/output 
      INTEGER IDP,IST,NTRK,NVRT,ISTA
      REAL    ECM,WEI
* - parametre table
      INTEGER      NTABL
      PARAMETER   (NTABL=100)
      REAL    TABL(NTABL)
      INTEGER KABL(NTABL)
      EQUIVALENCE (TABL,KABL)
* - event control
      REAL    sdvrt,vrtex
      COMMON /RTGRCP/SDVRT(3),VRTEX(4)
C - Event counter
      INTEGER        NGENE,IDB1,IDB2,NISR,NFSR,KF4F,JF4F   
      COMMON /RTGRCE/NGENE,IDB1,IDB2,NISR,NFSR,KF4F(6),JF4F(6)     
      REAL           V4F,V4ISR,V4FSR,HEL4F
      COMMON /RTGRCV/V4F(6,5),V4ISR(20,5),V4FSR(20,5),HEL4F(6)
C - KINGAL
      INTEGER  ALTABL,KGPART,KBKINE
      EXTERNAL ALTABL,KGPART,KBKINE
      INTEGER  NCOL,NROW,IPART,JKINE,JKPOL,JKXME
      REAL     PTRAK(4)
c - local
      INTEGER i,itr,n74trk,n74neu,n74chg,n74nch,iprint
      REAL rn1,rn2,rn3,dum,cos1,cos2,cos3,cos4,cos5,cos6
      REAL cos34,cos56,cos36,cos45,phi34,phi56,phi36,phi45,raddeg
      REAL mass34,mass56,mass36,mass45,mass35,mass46
c     REAL the34,the56,the36,the45,the35,the46
      REAL pi
      REAL engisr,ptisr,engfsr,ptfsr
      parameter (pi=3.14159265)
*     REAL SM,SP(4),PB(4),PF(4),cos5cm
C - Ntuple
      real ZQVAR(25)
C
      logical initev
      data initev/.false./
C
C - Initialization
      IDP  = 0
      IST  = 1
      ISTA = 1
      NTRK = 0
      NVRT = 0
      ECM  = 0.
      WEI  = 0.
*
      NISR = 0
      NFSR = 0
C
C -------------------------------------------
C - Protect against BASES (integration) only
C -------------------------------------------
      if (bssp.eq.1) then
         ISTA = 0
         goto 100
      end if   
C
C --------------- grc4f event generation  -------------------
C
C - Initialization
      if (.not.initev) then
*     - prepare tabreau ----------   
         ntab = 0
         do 10 m=1,mulprc(0)
            ngen(m) = 0
            tabmin(m) = float(ntab)/float(ntotal) 
            ntab = ntab+nevt(m)
            tabmax(m) = float(ntab)/float(ntotal) 
         write(IOUT,'(a,i10,2f10.5)') 
     .        ' m, tabmin, tabmax =',m,tabmin(m),tabmax(m)
  10     continue    
C     - open event data file -----
         do 20 m=1,mulprc(0)
          lu    = luspev+m  
c         fspev = fbsdir(1:ipoint)//'/'//grcpcn(m)//'.event_data'
          fspev = grcpcn(m)//'.event_data'
          fdum  = fspev
          write(IOUT,'(a,i5,a)') ' --- Opening file:',lu,fspev
          open(lu,file=fspev,status='old')
C     .             form='unformatted',err=1200)
C         rewind(lu)
  20    continue 
         initev = .true.
      end if   
C -------------
C - read event
C -------------
  30  continue 
      xxx = rndm(dum)
      do 40 m=1,mulprc(0)
         if (xxx.gt.tabmin(m) .and. xxx.le.tabmax(m)) then
            if (ngen(m).eq.nevt(m)) goto 30
            lu = luspev+m
C           write(IOUT,*) '+++ASKUSE+++ : xxx, m,lu',xxx,m,lu
            call rlist(lu,0,ierr)
            if (ierr.ne.0) then
               write(IOUT,*) '+++ASKUSE+++ : rlist error ',ierr
               ISTA = ierr
               GO TO 100
            else
               ngen(m)=ngen(m)+1
               goto 200
            end if   
         end if   
  40  continue 
      goto 30
C
 200  continue
C
      NGENE = NGENE+1
C
c     if (NGENE.GE.IDB1 .AND. NGENE.LE.IDB2) then      
c         write(IOUT,*) '+++ASKUSE+++ : after rlist'   
c         call lulist(1)
c         call GRCHEL
c      end if
C ---------------------------
C - first fill 4-vector info
C ---------------------------
      call GRCHEP(ISTA)
*
      IF (ISTA.NE.0) THEN
         write(IOUT,*) '+++ASKUSE+++ : error in GRCHEP',ISTA    
         GO TO 100
      END IF
C --------------------
C - check helisitty 
C --------------------
c      call GRCHCK(ISTA)
*
c      IF (ISTA.NE.0) THEN
c         write(IOUT,*) '+++ASKUSE+++ : error in GRCHCK',ISTA    
cC         GO TO 100
c      END IF
C --------------------
C - Pack process code
C --------------------
      IDP = 1000000*abs(KF4F(3))+10000*abs(KF4F(4))
     &     +    100*abs(KF4F(5))+      abs(KF4F(6))
C
C -----------------------------------------------
C - remove initial e+-(KS=21) (ALEPH convention)
C -----------------------------------------------
C    first update helicity information for TAUOLA call
      do i=3,N7LU+2,1
         lect(i-2)=lect(i)
      end do   
C    then remove e+- (KS>20) (for TAUOLA)
      K7LU(1,1) = 0
      K7LU(2,1) = 0
      call LUEDIT(12)
C     call LUEDIT(15)
*      if (NGENE.GE.IDB1 .AND. NGENE.LE.IDB2) then      
*         write(IOUT,*) '+++ASKUSE+++ : after luedit'   
*         call lulist(1)
*         call GRCHEL
*      end if
C --------------------------------------------
C - exec hadronization (LUTAUD called inside)
C --------------------------------------------
      call LUEXEC
      if (NGENE.GE.IDB1 .AND. NGENE.LE.IDB2) then
         write(IOUT,*) '+++ASKUSE+++ : after luexec'    
         call LULIST(1)
         write(IOUT,*) '+++ASKUSE+++ : calling grchel'    
         call GRCHEL
      end if
C
C --------------- Interface to  ALEPH -----------------------
C - Generate primary vertex
C --------------------------
      call RANNOR(rn1,rn2)
      call RANNOR(rn3,dum)
      VRTEX(1) = rn1*SDVRT(1)
      VRTEX(2) = rn2*SDVRT(2)
      VRTEX(3) = rn3*SDVRT(3)
      VRTEX(4) = 0.
C --------------------------------------------------
C - Fill KINE bank with beam particles -1:e-, -2:e+
C --------------------------------------------------
      do 50 itr = 1,2
c         do 51 i=1,4 
c            ptrak(i)= 0.
c 51      continue
         do i=1,3
            ptrak(i)=V4F(itr,i)
         end do
            ptrak(4)= 0.
         IF ( itr .EQ. 1 ) THEN
            ipart = KGPART(11)
c           ptrak(3) = 0.5*energy
         ELSE IF ( itr .EQ. 2 ) THEN
            ipart = KGPART(-11)
c           ptrak(3) =-0.5*energy
         END IF
         JKINE = KBKINE(-itr,ptrak,ipart,0)
         IF (JKINE .LE. 0 ) THEN
            ISTA = -2
            write(IOUT,*) '+++ASKUSE+++ : error in writing KINE'    
            GO TO 100
         END IF
 50   CONTINUE
C ------------------------------------------------------
C - Book and fill KZFR bank (fragmentation information)
C ------------------------------------------------------
      call KZFRBK(ISTA)
      IF (ISTA.NE.0) THEN
         write(IOUT,*) '+++ASKUSE+++ : error in filling KZFR bank'    
         GO TO 100
      END IF
C -----------------------------------------------
C - Book all banks KINE/VERT/KHIS (after LUEXEC)
C -----------------------------------------------
      call KXL7AL(VRTEX,ISTA,NVRT,NTRK)
      IF (ISTA.NE.0) THEN
         write(IOUT,*) '+++ASKUSE+++ : error in calling KXL7AL',ISTA
         write(IOUT,*) ' VRTEX,NVRT,NTRK',(VRTEX(I),I=1,3),NVRT,NTRK
         call lulist(1)
         GO TO 100
      END IF
C --------------------------------------------
C - Book and fill KPOL : helicity information
C --------------------------------------------
      NCOL = 4
      NROW = 6
      do 60 I=1,NROW
         KABL( (I-1)*NCOL + 1) = JF4F(I)
         TABL( (I-1)*NCOL + 2) = 0.
         TABL( (I-1)*NCOL + 3) = 0.
         TABL( (I-1)*NCOL + 4) = HEL4F(I)
 60   CONTINUE
      JKPOL = ALTABL('KPOL',NCOL,NROW,TABL,'2I,(I,3F)','E')
      IF (JKPOL.LT.0) then
         write(IOUT,*) '+++ASKUSI+++ : error in filling KPOL bank'
       end if   
C
C -------------------------------------------
C - Book and fill KXME : matrix element info
C -------------------------------------------
c      kproc = 1
c      if (NGENE.GE.IDB1 .AND. NGENE.LE.IDB2) then
c         iprint = 1
c      else
c         iprint = 0
c      end if   
C
CCC      call GRCMX2(kproc,iprint,am2)
*
c      NCOL = 3
c      NROW = 1
c      KABL(1) = mulprc(1)
c      KABL(2) = set
c      TABL(3) = sngl(am2(1,0))
c      JKXME = ALTABL('KXME',NCOL,NROW,TABL,'2I,(2I,F)','E')
c      if (JKXME.LT.0) then
c         write(IOUT,*) '+++ASKUSI+++ : error in filling KXME bank'
c      end if
c      if (NGENE.GE.IDB1 .AND. NGENE.LE.IDB2) then      
c         call PRTABL('KXME',0)
c      end if   
C
C -----------------------------------
C - protection for low hadronic mass
C -----------------------------------
c      mass56 = (V4F(5,4)+V4F(6,4))**2-(V4F(5,1)+V4F(6,1))**2
c     &        -(V4F(5,2)+V4F(6,2))**2-(V4F(5,3)+V4F(6,3))**2
c      mass56 = sqrt(MAX(0.0,mass56))
C **************************************************************
C for single-W e- + nu_e_bar + f3 + f4_bar  ONLY !!!!!!!!!
C radiate photons for leptonic W decays (electron, muon, tau)
C **************************************************************
*    e   nu u d / e   nu c s
*    mu  nu u d / mu  nu c s
*    tau nu u d / tau nu c s 
c      if (IDP.eq.11120201 .or. IDP.eq.11120403 .or.
c     &    IDP.eq.13140201 .or. IDP.eq.13140403 .or. 
c     &    IDP.eq.15160201 .or. IDP.eq.15160403) then
c         if (mass56.lt.0.270) then
c            write(IOUT,*) '+++ASKUSE+++ : M56<M(2pi)!',mass56    
c            call LULIST(1)
C            ISTA = 2
C            GO TO 100
c         end if
C **************************************************************
C for two photon like  e- + e+ + + q + q_bar (hadronic only)
C **************************************************************
*    eeuu/eecc/eedd/eess/eebb
c      else if (IDP.eq.11110202 .or. IDP.eq.11110404 .or.
c     &         IDP.eq.11110101 .or. IDP.eq.11110303 .or. 
c     &         IDP.eq.11110505 ) then
c         if (mass56.lt.0.270) then
c            write(IOUT,*) '+++ASKUSE+++ : M56<M(2pi)!',mass56    
c            call LULIST(1)
C            ISTA = 2
C            GO TO 100
c         end if
c      end if
C 
C
C -----------------
C - Event analysis
C -----------------
C - check if analysis and histogram required    
      if (hist.eq.0) goto 100
C - angle
      cos1 = V4F(1,3)/sqrt(V4F(1,4)**2-V4F(1,5)**2)
      cos2 = V4F(2,3)/sqrt(V4F(2,4)**2-V4F(2,5)**2)
      cos3 = V4F(3,3)/sqrt(V4F(3,4)**2-V4F(3,5)**2)
      cos4 = V4F(4,3)/sqrt(V4F(4,4)**2-V4F(4,5)**2)
      cos5 = V4F(5,3)/sqrt(V4F(5,4)**2-V4F(5,5)**2)
      cos6 = V4F(6,3)/sqrt(V4F(6,4)**2-V4F(6,5)**2)
* - i+j
      cos34= (V4F(3,3)+V4F(4,3))/sqrt((V4F(3,1)+V4F(4,1))**2
     .     + (V4F(3,2)+V4F(4,2))**2 + (V4F(3,3)+V4F(4,3))**2)
      cos56= (V4F(5,3)+V4F(6,3))/sqrt((V4F(5,1)+V4F(6,1))**2
     .     + (V4F(5,2)+V4F(6,2))**2 + (V4F(5,3)+V4F(6,3))**2)
      cos36= (V4F(3,3)+V4F(6,3))/sqrt((V4F(3,1)+V4F(6,1))**2
     .     + (V4F(3,2)+V4F(6,2))**2 + (V4F(3,3)+V4F(6,3))**2)
      cos45= (V4F(4,3)+V4F(5,3))/sqrt((V4F(4,1)+V4F(5,1))**2
     .     + (V4F(4,2)+V4F(5,2))**2 + (V4F(4,3)+V4F(5,3))**2)
* - i+j
      raddeg=180.0/acos(-1.0)
      phi34= raddeg*atan2(V4F(3,2)+V4F(4,2),V4F(3,1)+V4F(4,1))
      phi56= raddeg*atan2(V4F(5,2)+V4F(6,2),V4F(5,1)+V4F(6,1))
      phi36= raddeg*atan2(V4F(3,2)+V4F(6,2),V4F(3,1)+V4F(6,1))
      phi45= raddeg*atan2(V4F(4,2)+V4F(5,2),V4F(4,1)+V4F(5,1))
C - angle between partons
c      the34=(V4F(3,1)*V4F(4,1)+V4F(3,2)*V4F(4,2)+V4F(3,3)*V4F(4,3))
c     .     /sqrt(V4F(3,1)**2+V4F(3,2)**2+V4F(3,3)**2)
c     .     /sqrt(V4F(4,1)**2+V4F(4,2)**2+V4F(4,3)**2)
c      the56=(V4F(5,1)*V4F(6,1)+V4F(5,2)*V4F(6,2)+V4F(5,3)*V4F(6,3))
c     .     /sqrt(V4F(5,1)**2+V4F(5,2)**2+V4F(5,3)**2)
c     .     /sqrt(V4F(6,1)**2+V4F(6,2)**2+V4F(6,3)**2)
c      the36=(V4F(3,1)*V4F(6,1)+V4F(3,2)*V4F(6,2)+V4F(3,3)*V4F(6,3))
c     .     /sqrt(V4F(3,1)**2+V4F(3,2)**2+V4F(3,3)**2)
c     .     /sqrt(V4F(6,1)**2+V4F(6,2)**2+V4F(6,3)**2)
c      the45=(V4F(4,1)*V4F(5,1)+V4F(4,2)*V4F(5,2)+V4F(4,3)*V4F(5,3))
c     .     /sqrt(V4F(4,1)**2+V4F(4,2)**2+V4F(4,3)**2)
c     .     /sqrt(V4F(5,1)**2+V4F(5,2)**2+V4F(5,3)**2)
c      the35=(V4F(3,1)*V4F(5,1)+V4F(3,2)*V4F(5,2)+V4F(3,3)*V4F(5,3))
c     .     /sqrt(V4F(3,1)**2+V4F(3,2)**2+V4F(3,3)**2)
c     .     /sqrt(V4F(5,1)**2+V4F(5,2)**2+V4F(5,3)**2)
c      the46=(V4F(4,1)*V4F(6,1)+V4F(4,2)*V4F(6,2)+V4F(4,3)*V4F(6,3))
c     .     /sqrt(V4F(4,1)**2+V4F(4,2)**2+V4F(4,3)**2)
c     .     /sqrt(V4F(6,1)**2+V4F(6,2)**2+V4F(6,3)**2)
c      the34 = raddeg*acos(the34)
c      the56 = raddeg*acos(the56)
c      the36 = raddeg*acos(the36)
c      the45 = raddeg*acos(the45)
c      the35 = raddeg*acos(the35)
c      the46 = raddeg*acos(the46)
c - invariant mass
      mass34 = (V4F(3,4)+V4F(4,4))**2-(V4F(3,1)+V4F(4,1))**2
     &        -(V4F(3,2)+V4F(4,2))**2-(V4F(3,3)+V4F(4,3))**2
      mass56 = (V4F(5,4)+V4F(6,4))**2-(V4F(5,1)+V4F(6,1))**2
     &        -(V4F(5,2)+V4F(6,2))**2-(V4F(5,3)+V4F(6,3))**2
      mass36 = (V4F(3,4)+V4F(6,4))**2-(V4F(3,1)+V4F(6,1))**2
     &        -(V4F(3,2)+V4F(6,2))**2-(V4F(3,3)+V4F(6,3))**2
      mass45 = (V4F(4,4)+V4F(5,4))**2-(V4F(4,1)+V4F(5,1))**2
     &        -(V4F(4,2)+V4F(5,2))**2-(V4F(4,3)+V4F(5,3))**2
      mass35 = (V4F(3,4)+V4F(5,4))**2-(V4F(3,1)+V4F(5,1))**2
     &        -(V4F(3,2)+V4F(5,2))**2-(V4F(3,3)+V4F(5,3))**2
      mass46 = (V4F(4,4)+V4F(6,4))**2-(V4F(4,1)+V4F(6,1))**2
     &        -(V4F(4,2)+V4F(6,2))**2-(V4F(4,3)+V4F(6,3))**2
c
      mass34 = sqrt(MAX(0.0,mass34))
      mass56 = sqrt(MAX(0.0,mass56))
      mass36 = sqrt(MAX(0.0,mass36))
      mass45 = sqrt(MAX(0.0,mass45))
      mass35 = sqrt(MAX(0.0,mass35))
      mass46 = sqrt(MAX(0.0,mass46))
c
C - ISR/FSR
      engisr = 0.0
      do itr=1,NISR
         engisr = engisr+V4ISR(itr,4)
         ptisr  = sqrt(V4ISR(itr,1)**2+V4ISR(itr,2)**2)
         call HFILL(40043,V4ISR(itr,4),0.,1.)
         call HFILL(40044,ptisr,0.,1.)
      end do 
*  
      engfsr = 0.0
      do itr=1,NFSR
         engfsr = engfsr+V4FSR(itr,4)
         ptfsr  = sqrt(V4FSR(itr,1)**2+V4FSR(itr,2)**2)
         call HFILL(40047,V4FSR(itr,4),0.,1.)
         call HFILL(40048,ptfsr,0.,1.)
      end do   
C - multiplicity in final state
      n74trk = 0
      n74neu = 0
      n74chg = 0
      n74nch = 0
      do itr = 1,N7LU
         if (KLU(itr,1).eq.1) then
            n74trk = n74trk + 1
            if (KLU(itr,6).eq.0) then
               n74neu = n74neu + 1
            else
               n74chg = n74chg + 1
               if (abs(cos(plu(itr,13))).lt.0.95
     .             .and. plu(itr,10).gt.0.2 ) then
C     .             .and. mass56.gt.60.0 ) then
                  n74nch = n74nch+1
               end if   
            end if
         end if
      end do      
C
C ------------------
C - Fill histograms
C ------------------
c     KF code
      call HFILL(40001,real(KF4F(1)),0.,1.)
      call HFILL(40002,real(KF4F(2)),0.,1.)
      call HFILL(40003,real(KF4F(3)),0.,1.)
      call HFILL(40004,real(KF4F(4)),0.,1.)
      call HFILL(40005,real(KF4F(5)),0.,1.)
      call HFILL(40006,real(KF4F(6)),0.,1.)
c     helicity
      call HFILL(40007,HEL4F(1),0.,1.)
      call HFILL(40008,HEL4F(2),0.,1.)
      call HFILL(40009,HEL4F(3),0.,1.)
      call HFILL(40010,HEL4F(4),0.,1.)
      call HFILL(40011,HEL4F(5),0.,1.)
      call HFILL(40012,HEL4F(6),0.,1.)
c     energy
      call HFILL(40013,V4F(1,4),0.,1.)
      call HFILL(40014,V4F(2,4),0.,1.)
      call HFILL(40015,V4F(3,4),0.,1.)
      call HFILL(40016,V4F(4,4),0.,1.)
      call HFILL(40017,V4F(5,4),0.,1.)
      call HFILL(40018,V4F(6,4),0.,1.)
c     angle
      call HFILL(40019,cos1,0.,1.)
      call HFILL(40020,cos2,0.,1.)
      call HFILL(40021,cos3,0.,1.)
      call HFILL(40022,cos4,0.,1.)
      call HFILL(40023,cos5,0.,1.)
      call HFILL(40024,cos6,0.,1.)
c     invariant mass
      call HFILL(40025,mass34,0.,1.)
      call HFILL(40026,mass56,0.,1.)
      call HFILL(40027,mass36,0.,1.)
      call HFILL(40028,mass45,0.,1.)
      call HFILL(40029,mass35,0.,1.)
      call HFILL(40030,mass46,0.,1.)
c     invariant mass
      call HFILL(40031,mass34,mass36,1.)
      call HFILL(40032,mass56,mass45,1.)
      call HFILL(40033,mass34,mass56,1.)
      call HFILL(40034,mass36,mass45,1.)
c     ISR/FSR
      call HFILL(40041,real(NISR),0.,1.)      
      call HFILL(40042,   engisr ,0.,1.)
      if (engisr.gt.1.e-6) then
         call HFILL(40049,log10(engisr),0.,1.)
      end if   
      call HFILL(40045,real(NFSR),0.,1.)      
      call HFILL(40046,   engfsr ,0.,1.)
c     multiplicity
      call HFILL(40051,real(n74trk),0.,1.)
      call HFILL(40052,real(n74neu),0.,1.)
      call HFILL(40053,real(n74chg),0.,1.)
      call HFILL(40054,real(n74nch),0.,1.)
c     vertex
      call HFILL(40055,VRTEX(1),0.,1.)
      call HFILL(40056,VRTEX(2),0.,1.)
      call HFILL(40057,VRTEX(3),0.,1.)
*
C     helicity amplitude
      do I=0,15
         am2(1,I)=MAX(1.d-25,am2(1,I))
         call HFILL(40070+I,sngl(dlog10(am2(1,I))),0.,1.)
      end do   
C
c     reconstructed
      call HFILL(40091,cos34,0.,1.)
      call HFILL(40092,cos56,0.,1.)
      call HFILL(40093,cos36,0.,1.)
      call HFILL(40094,cos45,0.,1.)
      call HFILL(40095,phi34,0.,1.)
      call HFILL(40096,phi56,0.,1.)
      call HFILL(40097,phi36,0.,1.)
      call HFILL(40098,phi45,0.,1.)
C
C    - Lorenz boost to W rest frame
c      SM = mass56
c      SP(1) = V4F(5,1)+V4F(6,1)   
c      SP(2) = V4F(5,2)+V4F(6,2)   
c      SP(3) = V4F(5,3)+V4F(6,3)
c      SP(4) = V4F(5,4)+V4F(6,4)
c      PB(1) = V4F(5,1)
c      PB(2) = V4F(5,2)
c      PB(3) = V4F(5,3)
c      PB(4) = V4F(5,4)
c      CALL LORENF(SM,SP,PB,PF)
c      cos5cm = PF(3)/SQRT(PF(1)**2+PF(2)**2+PF(3)**2)
cc     call HFILL(40204,cos5cm,0.,1.)
C
C - Ntuple
      if (hist.eq.2) then
         CALL VZERO(ZQVAR,25)
*
         ZQVAR( 1) = FLOAT(NGENE)
         ZQVAR( 2) = FLOAT(IDP)
*
         ZQVAR( 3) = V4F(3,4)
         ZQVAR( 4) = V4F(4,4)
         ZQVAR( 5) = V4F(5,4)
         ZQVAR( 6) = V4F(6,4)
*
         ZQVAR( 7) = cos3
         ZQVAR( 8) = cos4
         ZQVAR( 9) = cos5
         ZQVAR(10) = cos6
*
         ZQVAR(11) = mass34
         ZQVAR(12) = mass56
         ZQVAR(13) = mass36
         ZQVAR(14) = mass45
*
         ZQVAR(15) = cos34
         ZQVAR(16) = cos56
         ZQVAR(17) = cos36
         ZQVAR(18) = cos45
*
         ZQVAR(19) = sngl(am2(1,0))
         ZQVAR(20) = sngl(am2(1,1))
         ZQVAR(21) = sngl(am2(1,2))
         ZQVAR(22) = sngl(am2(1,3))
         ZQVAR(23) = sngl(am2(1,4))
         ZQVAR(24) = sngl(am2(1,5))
         ZQVAR(25) = sngl(am2(1,12))
*
         CALL HFN(5010,ZQVAR)
      end if   
C
 100  continue
C
C ---------------
C - Return codes
C ---------------
      IST  = ISTA
      ECM  = energy
      WEI  = 1.
C
 999  continue

      RETURN                                                            
C
 1200 write(IOUT,*) '+++ASKUSI+++ : error in opening ',lu,' ',fdum,'.'
      STOP
C
      END





      SUBROUTINE USCJOB
C ------
C USCJOB
C ------
      implicit none
C ----------------------------------------------------------------------   
*    local 
      integer iopt,ierr
C - BCS
      INTEGER    LMHLEN,   LMHCOL,   LMHROW  , LBCS                    
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=1000)              
      COMMON /BCS/IW(LBCS)                                             
      INTEGER IW                                                       
      REAL    RW(LBCS)                                                 
      EQUIVALENCE (RW(1),IW(1))                                        
C
      INTEGER INUT,IOUT
      COMMON /INOUT/ INUT,IOUT
C
      INTEGER  imstj,itau,ndec,nlink
      external nlink
C - Event counter
c      INTEGER        NGENE,IDB1,IDB2,NISR,NFSR,KF4F,JF4F   
c      COMMON /RTGRCE/NGENE,IDB1,IDB2,NISR,NFSR,KF4F(6),JF4F(6)          
c      REAL           V4F,V4ISR,V4FSR,HEL4F
c      COMMON /RTGRCV/V4F(6,5),V4ISR(20,5),V4FSR(20,5),HEL4F(6)
C
C - Ntuple
c      integer icycle,istat      
*-----------------------------------------------------------------------
C Event summary
      write(IOUT,*) '+++USCJOB+++ : calling grcevt(+1)'    
      call GRCEVT(+1,iopt,ierr)
      call grc4fm(IOUT)
C
*----------------
* user summary
*----------------
      call UGTSEC

*-----------------------------------------------------------------------
C Interface to TAUOLA/PHOTOS (May 1997, R.Tanaka)
C   tau decay summary if needed
      imstj = nlink('MSTJ',28)
      if (imstj.gt.0) then
         if (IW(imstj+1).eq.2) call TAUDKAY(itau,ndec,100)
      endif
C Interface to TAUOLA/PHOTOS (May 1997, R.Tanaka)
      call grcpho(+1)
*-----------------------------------------------------------------------
C
C Output Histograms
C
c      if (lsphb.ne.0) then
         call bshbok(0)
         call sphbok(0)
c      end if   
C - Ntuples are written in standard hist
c      if (hist.ne.0) then
c         fdum = fbsdir(1:ipoint)//'/'//process//'.ntuple'
c         CALL HROPEN(59,'WW',fdum,'N',1024,istat)
c         CALL HROUT(5010,icycle,' ')
c         CALL HREND('WW')
c      end if
C
      RETURN
      END



      subroutine UGTSEC
C ------
C UGTSEC     Cross section information             
C ------
      implicit none
C                                                                      
C grc4f : variables
C
      integer  maxact,maxdi4,maxdr8,maxdc8,ikyadr,ikytyp,keytch
      include 'inclpi.h'
      integer MPROC,mulprc,ichgcj,mevent
      real*8  xscton, xscter
      include 'inclan.h'
*
      character*8 grcpcn
      external    grcpcn
*    local 
      integer m
      real*8  totalx,totale
      integer ntotal,nevt,ngen,mtotal
      common /RTEVTC/ntotal,nevt(MPROC),ngen(MPROC)
*    - bases/spring my control
c      real*4  energy
c      integer set,hist
c      character*8 process
c      common /RTGRC1/energy,set,hist,process
c      integer     lu,bssp,lubsdt,lubsrl,lubscs,lusprl,luspev,ipoint
c      common /RTGRC2/bssp,lubsdt,lubsrl,lubscs,lusprl,luspev,ipoint
c      character*256  fbsdir,fbsdt,fbsrl,fbscs,fsprl,fspev,fdum
c      common /RTGRC3/fbsdir,fbsdt,fbsrl,fbscs,fsprl,fspev,fdum
C
C
      INTEGER INUT,IOUT
      COMMON /INOUT/ INUT,IOUT
C - Generator code (see KINLIB DOC)                                      
      INTEGER    IGCO,IVER
      PARAMETER (IGCO=5039, IVER=201)
c
      INTEGER   IS, NGCO, NVER, NTOT, NACC, ISEC, KSECBK
      EXTERNAL KSECBK
      REAL*4  XTOT, RTOT, XACC, RACC
C --------------------------------------------------------------------
C
      ntotal = 0
      mtotal = 0
      totalx = 0.0d0
      totale = 0.0d0
      write(IOUT,*) '+++UGTSEC+++ : event summary ++++++++++++++++++'
      write(IOUT,107) '   i ',' process #','   id','   cc'
     . ,' cross section (pb) ',' requested',' generated'
      do 10 m = 1 , mulprc(0)
         totalx = totalx + ichgcj(m)*xscton(m)
         totale = totale +(ichgcj(m)*xscter(m))**2
         ntotal = ntotal + nevt(m)
         mtotal = mtotal + ngen(m)
         write(IOUT,106) m,grcpcn(m),mulprc(m)
     .        ,ichgcj(m),ichgcj(m)*xscton(m)
     .        ,ichgcj(m)*xscter(m),nevt(m),ngen(m)
  10  continue
         totale = dsqrt(totale) 
      write(IOUT,'(25x,2f10.5,2i10//)') totalx,totale,ntotal,mtotal
 106  format(i5,a10,i5,i5,2f10.5,3i10)
 107  format(////a5,a10,a5,a5,a20,3a10//)
C
      if (ntotal.ne.mtotal) then
         write(IOUT,*) '+++UGTSEC+++ : WARNING in number of events !'
         write(IOUT,*) ' requested =',ntotal,'   generated',mtotal
      end if
* --- Total cross section informatio
      IS   = 1
      NGCO = IGCO
      NVER = IVER
      NTOT = ntotal
      NACC = mtotal
      XTOT = totalx
      RTOT = totale
      XACC = XTOT 
      RACC = RTOT
c
      ISEC = KSECBK(IS,NGCO,NVER,NTOT,NACC,XTOT,RTOT,XACC,RACC)
c
* --- loop over sub-processes      
      do 20 m = 1 , mulprc(0)
         IS = IS + 1
         NGCO = IGCO
         NVER = mulprc(m)
         NTOT = nevt(m)
         NACC = ngen(m)
         XTOT = ichgcj(m)*xscton(m)
         RTOT = ichgcj(m)*xscter(m)
         XACC = XTOT 
         RACC = RTOT         
c
         ISEC = KSECBK(IS,NGCO,NVER,NTOT,NACC,XTOT,RTOT,XACC,RACC)
c      
  20  continue
* --- print KSEC bank
      call PRTABL('KSEC',0)
C
      return
      end



      subroutine GRCHEP(ISTA)
C ------
C GRCHEP     Interface between HEPEVT and grc4f
C ------
C --------------------------------------------------------------------
C - Event analysis from LUJETS information
C     isrtype 0 : Tree          4f (no ISR/FSR)
C             1 : SF/PHOTOS     ISR[0,2],4f,FSR[0,2]
C             2 : QEDPSI/PHOTOS 4f,ISR[0,10],FSR[0,2]
C             3 : QEDPSIF       4f,ISR[0,10],FSR[0,10]
c     count ISR/FSR, Note: isrtype=2 QEDPSI/PHOTOS=ISR/FSR
c                          isrtype=3 QEDPSIF...all counted as ISR!
c     output : ISTA = 0 ... OK, 1 ... NG 
C --------------------------------------------------------------------
      implicit none
C
      integer ISTA
C - GRC4F
c      integer  maxact,maxdi4,maxdr8,maxdc8,ikyadr,ikytyp,keytch
c      include 'inclpi.h'
c      integer isrtype
C   - helicity
      integer lect
      common/helicity/lect(4000)
C - LUND
      integer n,k,klu
      real*4  p,v,plu
      external klu,plu
      common/lujets/n,k(4000,5),p(4000,5),v(4000,5)
C - Standard HEPEVT common block (single precision)
      integer     NMXHEP
      parameter (NMXHEP=2000)
      integer NEVHEP,NHEP,ISTHEP,IDHEP,JMOHEP,JDAHEP
      real*4  PHEP,VHEP
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
C - Event counter
      INTEGER        NGENE,IDB1,IDB2,NISR,NFSR,KF4F,JF4F   
      COMMON /RTGRCE/NGENE,IDB1,IDB2,NISR,NFSR,KF4F(6),JF4F(6)          
      REAL           V4F,V4ISR,V4FSR,HEL4F
      COMMON /RTGRCV/V4F(6,5),V4ISR(20,5),V4FSR(20,5),HEL4F(6)
C - I/O
      INTEGER INUT,IOUT
      COMMON /INOUT/ INUT,IOUT
C - local
      INTEGER i,j,itr,jtr,ktr,i4f
C --------------------------------------------------------------------
C - initialization
      ISTA = 0
      NISR = 0
      NFSR = 0
C - check ISR type
C      isrtype = i4data(ikyadr(7))

C - fill initial and final e+-
      i4f=0
      do 10 itr=1,n
         if (KLU(itr,2).ne.22 .and. KLU(itr,3).le.1) then
            i4f = i4f+1
C         - check number of 4f
            if (i4f.gt.6) then
               print *, '+++GRCHEP+++ : N_4f>6 !',i4f
               ISTA = 10 
               return
            end if   
            do 11 jtr=1,5
               V4F(i4f,jtr) = PLU(itr,jtr)
 11         continue
                KF4F(i4f) = KLU(itr,2)
               HEL4F(i4f) = real(lect(itr))/2.0
               if (i4f.le.2) then
                  JF4F(i4f) = -i4f
               else
C                 - pointer for KINE track number
C                 - adjusted e+- offset due to later luedit(15)
                  JF4F(i4f) = itr-2
               end if   
         else if (KLU(itr,2).eq.22) then
            if (KLU(itr,3).eq.1 .or. KLU(itr,3).eq.2) then
               NISR = NISR+1
               ktr  = NISR
C            - check number of ISR
               if (NISR.gt.10) then
                  print *, '+++GRCHEP+++ : NISR>10 !',NISR
                  ISTA = 11 
                  return
               end if   
               do 12 jtr=1,5
                  V4ISR(ktr,jtr) = PLU(itr,jtr)
 12            continue
C     - FSR SF(2+2+4),QEDPS(2+4+10+10)
C            else if (KLU(itr,3).ge.3 .and. KLU(itr,3).le.8) then
             else if (KLU(itr,3).ge.3) then
               NFSR = NFSR+1
               ktr  = NFSR
C ---debug
c                  print *, '+++GRCHEP+++ : NFSR = ',NFSR
c                  call lulist(1)
C            - check number of FSR
               if (NFSR.gt.10) then
                  print *, '+++GRCHEP+++ : NFSR>10 !',NFSR
                  ISTA = 12
                  return
               end if   
               do 13 jtr=1,5
                  V4FSR(ktr,jtr) = PLU(itr,jtr)
 13            continue               
            end if   
         end if
 10   continue
C
C     - debug
      if (NGENE.GE.IDB1 .AND. NGENE.LE.IDB2) then
         WRITE(IOUT,*) '+++GRCHEP+++ : event summary'
         do 100 i=1,i4f
         WRITE(IOUT,500) ' fermion',i,(V4F(i,j),j=1,5)
     .                   ,KF4F(i),HEL4F(i),JF4F(i)
 100     continue
         do 110 i=1,NISR
         WRITE(IOUT,500) '     isr',i,(V4ISR(i,j),j=1,5)
 110     continue
         do 120 i=1,NFSR
         WRITE(IOUT,500) '     fsr',i,(V4FSR(i,j),j=1,5)
 120     continue
      end if
 500  format(A,I2,5F10.3,I10,F10.1,I10) 
C
      return
      end


      SUBROUTINE HEPLST(UNIT,LVL)
C-----------------------------------------------------------------------
C! List HEPEVT common block, much like LULIST.
C
C  12-MAR-96 A.Waananen
C-----------------------------------------------------------------------
      IMPLICIT NONE
C
C Old Standard HEPEVT common block (single precision)
C
      INTEGER NMXHEP
      PARAMETER (NMXHEP=2000)
      INTEGER NEVHEP,NHEP,ISTHEP,IDHEP,JMOHEP,JDAHEP
      REAL    PHEP,VHEP
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
*
      INTEGER UNIT,LVL,I,J,K
      REAL PSUM(5)
      CHARACTER*16 NAME
      CHARACTER*12 NAM
*      
      DO 3 I=1,5
      PSUM(I) = 0.0
 3    CONTINUE
*      
      WRITE(UNIT,15)
      IF (LVL.LE.1) THEN
        WRITE(UNIT,16)
      ELSEIF (LVL.EQ.2) THEN
        WRITE(UNIT,17)
      ENDIF
*      
      DO 10 I = 1, NHEP
        CALL LUNAME(IDHEP(I),NAME)
        DO 5 K=1,12
          NAM(K:K)=NAME(K:K)
 5      CONTINUE
        IF (LVL.EQ.1) THEN
          WRITE(UNIT,21) I,NAM,ISTHEP(I),IDHEP(I),
     &       JMOHEP(1,I),JMOHEP(2,I),JDAHEP(1,I),JDAHEP(2,I),
     &       (PHEP(J,I),J=1,5)
        ELSEIF (LVL.EQ.2) THEN
          WRITE(UNIT,22) I,NAM,ISTHEP(I),IDHEP(I),
     &       JMOHEP(1,I),JMOHEP(2,I),JDAHEP(1,I),JDAHEP(2,I),
     &       (PHEP(J,I),J=1,5),(VHEP(J,I),J=1,3)
        ENDIF
        DO 7 J = 1,4
          IF (ISTHEP(I).EQ.1) PSUM(J) = PSUM(J) + REAL(PHEP(J,I))
 7      CONTINUE
        PSUM(5) = PSUM(4)*PSUM(4)-
     &    PSUM(3)*PSUM(3)-PSUM(2)*PSUM(2)-PSUM(1)*PSUM(1)
        PSUM(5) = SQRT(MAX(PSUM(5),0.0))
 10   CONTINUE
      WRITE(UNIT,30) (PSUM(J),J=1,5)
*      
 15   FORMAT(//,30X,'HEPEVT Event listing (summary)')
 16   FORMAT(/,1X,3X,'I','  particle/jet',
     &   ' KS','     KF','  Moth1','  Moth2','   Dau1','   Dau2',
     &   '       p_x','       p_y','       p_z','         E',
     &   '         m',/)
 17   FORMAT(/,1X,3X,'I','  particle/jet',
     &   ' KS','     KF','  Moth1','  Moth2','   Dau1','   Dau2',
     &   '       p_x','       p_y','       p_z','         E',
     &   '         m','       V_x','       V_y','       V_z',/)
 21   FORMAT(1X,I4,2X,A12,I3,5I7,5F10.4)
 22   FORMAT(1X,I4,2X,A12,I3,5I7,5F10.4,3F10.4)
 30   FORMAT(1X,18X,'sum:',34X,5F10.4)
*
      RETURN
      END
*

      subroutine rlist(lu,irec,ierr)
c-----------------------------------------------------------------------
c                                                                 LAPP
c-----------------------------------------------------------------------
c...Purpose:
c      To read lujets common to file. Direct access read for improved
c      randomization of the temporary file reading
c...Parameters:
c     lu     : integer
c            : Logical Unit
c     irec   : integer
c            : record number
c            : 0 sequential read (file size optimization)
c            : WARNING the file open should be set accordingly
c              open(lu,file=ftemp,status='new',recl=MSIZEE,
c                   access='direct',form='unformatted',err=99)
c              where MSIZE is the maximun size of the lujet common
c              before hadronization but after parton shower.
c          
c     ierr   : integer
c             0 Ok
c             1 logical unit set to 0 no reading
c             2 read error
c             3 end of file
c-----------------------------------------------------------------------
      integer lu,ierr,irec
      integer n, k
      real*4 p,v
      common/lujets/n,k(4000,5),p(4000,5),v(4000,5)
      integer lect
      common/helicity/lect(4000)
*     ierr=1 error

      if(lu.eq.0) goto 10
      do 1000 i=1,4000
       lect(i)=-999
 1000 continue
      if(irec.eq.0) then
       read (unit=lu,err=11,end=12) 
     &      n,((k(i,j),j=1,5),i=1,n),((p(i,j),j=1,5),i=1,n),
     &       ((v(i,j),j=1,5),i=1,n),(lect(i),i=1,n)
      else
       read (unit=lu,err=11,rec=irec) 
     &    n,((k(i,j),j=1,5),i=1,n),((p(i,j),j=1,5),i=1,n),
     &       ((v(i,j),j=1,5),i=1,n),(lect(i),i=1,n)
      endif
      ierr=0
      return
 10   ierr=1
      return
 11   ierr=2
      return
 12   ierr=3
      return
      end


      subroutine wlist(lu,ierr)
c...Purpose:
c      To write lujets common to file for SEQUETIAL FILE
c...Parameters:
c     lu     : integer
c            : Logical Unit
c     ierr   : integer
c             0 Ok
c             1 error
      integer lu,ierr
      integer n, k
      real*4 p,v
      common/lujets/n,k(4000,5),p(4000,5),v(4000,5)
      integer lect
      common/helicity/lect(4000)      
      if(lu.eq.0) goto 10
      write(lu,err=11) 
     &      n,((k(i,j),j=1,5),i=1,n),((p(i,j),j=1,5),i=1,n),
     &        ((v(i,j),j=1,5),i=1,n),(lect(i),i=1,n)
      ierr=0
      return
 10   ierr=1
      return
 11   ierr=2
      return
      end



      subroutine wdirct(lu,ierr)
c...Purpose:
c      To write lujets common to file for  DIRECT ACCESS FILE
c...Parameters:
c     lu     : integer
c            : Logical Unit
c     ierr   : integer
c             0 Ok
c             1 error
      integer lu,ierr
      integer n, k
      real*4 p,v
      common/lujets/n,k(4000,5),p(4000,5),v(4000,5)
      integer lect
      common/helicity/lect(4000)      
      data irec/0/
      save irec

      if(lu.eq.0) goto 10
      irec = irec + 1
      write(lu,rec=irec,err=11) 
     &      n,((k(i,j),j=1,5),i=1,n),((p(i,j),j=1,5),i=1,n),
     &        ((v(i,j),j=1,5),i=1,n),(lect(i),i=1,n)
      ierr=0
      return
 10   ierr=1
      return
 11   ierr=2
      return
      end




      subroutine GRCHCK(ISTA)
c-----------------------------------------------------------------------
c...Purpose     
c     Check helicity sum                       
c-----------------------------------------------------------------------
      implicit none
      integer i,it,ii,iff,ISTA
c
      integer n,k
      real*4  p,v
      common/lujets/n,k(4000,5),p(4000,5),v(4000,5)
      integer lect
      common/helicity/lect(4000)
c
      ii=0
      it=0
      iff=0
      ISTA=0
c
      do 1000 i=1,n
         if( lect(i).ne.-999)then
            if(lect(i).ne.999)then
               it=it+lect(i)
               if(i.eq.1.or.i.eq.2)then
                  ii=ii+lect(i)
               else
                  iff=iff+lect(i)
               endif
            endif
         endif
 1000 continue

      if (it.ne.0 .or. ii.ne.0 .or. iff.ne.0) then
         ISTA = 1
         write(6,*) ' ++++++++ helicity sum error !!! +++++++++'
         write(6,*) ' helicity :',(lect(i),i=1,n)
         write(6,'(1x,a4,2x,i4,2x,a18,2x,i3,2x,a16,2x,i3)')
     &   'Sum:',it,'Initial state Sum:',ii,'Final state Sum:',iff
      end if   
C
      return
      end





      real function UTOPOL(ITAU,IORIG,KFORIG,TPOL)
C ------
C UTOPOL     interface to TAUOLA
C ------
C        TPOL in this routine overrides that in routine TAUPOL
C        if TPOL=0.0 randomized +-1 in TAUDKAY (DEXAY)
C
      implicit none
C - input/output
      integer ITAU,IORIG,KFORIG
      real    TPOL
C - LUND jetst74
      INTEGER LJNPAR
      PARAMETER (LJNPAR=4000)
      INTEGER N7LU,K7LU
      REAL    P7LU,V7LU
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5) 
      INTEGER  KLU
      REAL     PLU
      EXTERNAL KLU,PLU
C - GRC4F
      integer lect
      common/helicity/lect(4000)      
*    - bases/spring my control
      real*4  energy
      integer set,hist
      character*8 process
      common /RTGRC1/energy,set,hist,process
C
C - search primary tau in 4f
      if (abs(KLU(ITAU,2)).eq.15 .and. KLU(ITAU,3).eq.0) then
         TPOL = -sign(1.,PLU(ITAU,6))*sign(1.,real(lect(ITAU)))
      else
         TPOL = 0.0
      end if
C
      UTOPOL = TPOL
C
C - debug ----------------------------
      if (TPOL.gt.0.5) then
      write(6,*) 'ITAU,PLU6,LECT,TPOL',ITAU,PLU(ITAU,6),lect(ITAU),TPOL
      call lulist(1)
      call grchel
      end if
C ------------------------------------
C
      if (hist.eq.1) then
         if (PLU(ITAU,6).gt.0.) then
            call HFILL(40061,TPOL,0.,1.)
         else
            call HFILL(40062,TPOL,0.,1.)
         end if
      end if   
C 
      end



*981117:RT:mod Interface to HEPVEC/TAUOLA/PHOTOS (May, 1997 R. Tanaka)
*970626:YK:mod the shower
*970509:TI:mod the origin of photon FOR ISR
*970506:TI:mod Charge conjugate for ISR
*970419:TI:del /common/KMCNTL
* File sp2lnd.f generated by GRACE Ver. 2.00(35)        1996/03/24/15:33
*      for llll,llqq,qqqq 
*          Fortran source code generator
*     (c)copyright 1990-1996 Minami-Tateya Group, Japan
*-----------------------------------------------------------------------
      subroutine sp2lnd
*  Inteface of SPRING to JETSET.
*  1996.03.13 YK: put initial e+e- information.
      implicit real*8 (a-h,o-z)
*     include 'incpar.h'
      include 'incl1.h'
      include 'inclk.h'
      dimension ijoin(2)
      integer jp(2)
      real*4 xp(2,5),xbeam,x
      real*4  p,v
      common/lujets/n,k(4000,5),p(4000,5),v(4000,5)
      integer lect
      common/helicity/lect(4000)

      common /sp4vec/ vec(4,nextn)
      real*4  rqmax
      common /kinem1/ s,w,fact
*-----------------------------------------------------------------------
*     For only QEDPS
*-----------------------------------------------------------------------
      common /qplist/plptn(10,1000),nlptn(10,1000),ntop
*-----------------------------------------------------------------------
      include 'inclgi.h'
      common / grcspc / jmxtry, jchrgc
      common / grc4fs / nthprc
*-----------------------------------------------------------------------
      if( jqedps .eq. 0 ) then
* Tree level only no initial state radiation
       kc = 0
       do 10 j = 1 , nextn
        kc  = kc + 1

        p(kc,1) = real(vec(1,j))
        p(kc,2) = real(vec(2,j))
        p(kc,3) = real(vec(3,j))
        p(kc,4) = real(vec(4,j))
        p(kc,5) = real(amass1(j))

        k(kc,1) = 1
        k(kc,2) = kfcode(j)
        k(kc,3) = 1
        k(kc,4) = 0
        k(kc,5) = 0
        if( j .le. 2 ) then
         k(kc,1) = 21
         k(kc,3)=0
        endif

        v(kc,1) = 0.0
        v(kc,2) = 0.0
        v(kc,3) = 0.0
        v(kc,4) = 0.0
        v(kc,5) = 0.0
   10  continue

       n = kc
*-----------------------------------------------------------------------
      else
*-ISRTYPE can be 1 or 2 or 3
       k0 = nextn
* ntop number of QEDPS photons can be 0
       do 11 j = 1 , ntop
        if(nlptn(3,j).ne.0) goto 11
* YK:1996.03.13
*       if(nlptn(1,j).ge.3) then
        if(nlptn(1,j).ge.1) then
         kc     = nlptn(1,j)
         p(kc,1) = real(plptn(4,j))
         p(kc,2) = real(plptn(5,j))
         p(kc,3) = real(plptn(6,j))
         p(kc,4) = real(plptn(7,j))
* YK:1996.03.13
*        p(kc,5) = real(amass1(j))
         p(kc,5) = real(amass1(kc))

         k(kc,1) = 1
         k(kc,2) = kfcode(kc)
         k(kc,3) = 1
         k(kc,4) = 0
         k(kc,5) = 0
c         lect(kc)=999
*           Assume number of initial partile is 2.(Toriaezu)
         if( kc .le. 2 ) then
          k(kc,1) = 21
          k(kc,3) = 0
         endif
         v(kc,1) = 0.0
         v(kc,2) = 0.0
         v(kc,3) = 0.0
         v(kc,4) = 0.0
         v(kc,5) = 0.0
        else
         k0      = k0 + 1
         p(k0,1) = real(plptn(4,j))
         p(k0,2) = real(plptn(5,j))
         p(k0,3) = real(plptn(6,j))
         p(k0,4) = real(plptn(7,j))
         p(k0,5) = 0.0
         k(k0,1) = 1
         k(k0,2) = 22
         k(k0,3) = 1
         k(k0,4) = 0
         k(k0,5) = 0
         v(k0,1) = 0.0
         v(k0,2) = 0.0
         v(k0,3) = 0.0
         v(k0,4) = 0.0
         v(k0,5) = 0.0
         lect(k0)= 999
        endif
   11  continue
       n = k0
      endif

c generate 1 or 2  photon for the isr=1 case
      nisr=0
      if( isr.eq.1) then
         xbeam=sngl(w/2.d0)
         do 1020 i=1,2
            jp(i)=0
            do 1010 ik=1,5
               xp(i,ik)=0.
 1010       continue
            x=xbeam-p(i,4)
*       print *,'xbeam eisr nisr i p(i,4) x ',
*    &             xbeam,eisr,nisr,i,p(i,4),x
            if(x.gt.sngl(eisr)) then
               nisr=nisr+1
               jp(nisr)=i
               xp(i,3)=sign(x,p(i,3))
               xp(i,4)=x
               p(i,1)=0.
               p(i,2)=0.  
               p(i,3)=sign(sqrt(xbeam**2-p(i,5)**2),p(i,3))
               p(i,4)=xbeam  
             endif
 1020     continue
c         do 1030 i=1,20
c            print *,lect(i)
c1030     continue
c         print *,n,nisr

          if(nisr.ne.0) then                          
             do 1050 i=n,3,-1
                l=i+nisr
                lect(l)=lect(i) 
                do 1040 j=1,5
                   k(l,j)=k(i,j)
                   p(l,j)=p(i,j)
                   v(l,j)=v(i,j)
 1040           continue
 1050        continue
             n=n+nisr
             do 1070 i=1,nisr
                do 1060 m=1,5
                   p(2+i,m)=xp(jp(i),m)
                   v(2+i,m)=0.
 1060           continue
                k(2+i,1)=1
                k(2+i,2)=22
                k(2+i,3)=jp(i)
                lect(2+i)=999
 1070        continue
          endif
          j=0
          do 1080 i=3,n
             if( lect(i).ne.999.and.lect(i).ne.-999)then
                 j=j+lect(i)
             endif
 1080     continue
          if(j.ne.-4.and.j.ne.-2.and.j.ne.0.and.j.ne.2.and.j.ne.4)then
             print*,'sp2lnd n,nisr: ',n,nisr
             do 1090 i=1,n
                print *,lect(i)
 1090        continue
          endif
      endif  

*-----------------------------------------------------------------------
C Interface to HEPVEC/TAUOLA/PHOTOS (May, 1997 R. Tanaka)
      call grcpho(0)
C
*-----------------------------------------------------------------------
c Charge conjugate
*     print *,'<<sp2lnd>>'
*     print *,' chgcjpr ',chgcjpr
      if (jchrgc.eq.1) then
*-------------------
* check
*-------------------
c      if( iprc(nthprc-1,2) .gt.1 .and. drn(idum) .gt. 0.5d0 ) then
c --- for single W !!!
       if( drn(idum) .gt. 0.5d0 ) then
*-------------------
*         print *,' chrgconj ',nthprc,iprc(nthprc-1,2)
          do 5010 l=3,n
             if( k(l,2) .eq. 22 ) then
                 if( k(l,3) .eq. 1 ) then
                     k(l,3) = 2
                 else if( k(l,3) .eq. 2 ) then
                     k(l,3) = 1
                 endif
             endif
             if(lucomp(-k(l,2)).ne.0) k(l,2)=-k(l,2)
             p(l,1)=-p(l,1)
             p(l,2)=-p(l,2)
             p(l,3)=-p(l,3)
             if(lect(l).ne.999.and.lect(l).ne.-999)lect(l)=-lect(l)
 5010     continue
        endif 
      endif 
*-----------------------------------------------------------------------
*     String Join
*-----------------------------------------------------------------------
      do 20 i = 1, kmcbmx, 2
         if( kmcstr(i,icolst) .ne. 0 ) then
* YK:1996.03.13
* YK         ijoin(1) = kmcstr( i ,icolst) - 2
             ijoin(1) = kmcstr( i ,icolst) + nisr
* YK         ijoin(2) = kmcstr(i+1,icolst) - 2
             ijoin(2) = kmcstr(i+1,icolst) +nisr
*            call lujoin(2,ijoin)
             k(ijoin(1),1) = 3
             if( k(ijoin(1),2) .gt. 0 ) then
                 k(ijoin(1),4) = ijoin(2)*10000
             else
                 k(ijoin(1),5) = ijoin(2)*10000
             endif
             k(ijoin(2),1) = 3
             if( k(ijoin(2),2) .gt. 0 ) then
                 k(ijoin(2),4) = ijoin(1)*10000
             else
                 k(ijoin(2),5) = ijoin(1)*10000
             endif
         endif
   20 continue

********************************************************************
*      'rqmax '  This q-maximum
*has to be the maximum of q-q-bar system, NOT a initial e+e- system.
********************************************************************
c     rqmax = real(w)

*     print *,'before shower',jshowr
*     call lulist(1)
      if( jshowr .eq.1 ) then
         do 21 i=1,kmcbmx,2
         if(kmcstr(i,icolst) .ne.0) then
          kk1=kmcstr(i  ,icolst)+nisr
          kk2=kmcstr(i+1,icolst)+nisr
          rqmax=sqrt( (p(kk1,4)+p(kk2,4))**2
     .-               (p(kk1,1)+p(kk2,1))**2
     .-               (p(kk1,2)+p(kk2,2))**2
     .-               (p(kk1,3)+p(kk2,3))**2 )
c         call lushow(kmcstr(i,icolst),kmcstr(i+1,icolst),rqmax)
          call lushow(   kk1          ,   kk2            ,rqmax)
         endif
 21      continue
*        print *,'after shower'
*        call lulist(1)
      endif
      return
      end





      subroutine grcpho(flag)
C ------
C grcpho     Interface to PHOTOS
C ------
C     input : ISTATP - -1 init, 0 event, +1 summary
C
      implicit none
C --------------------------------------------------------------------
C GRC4F
      integer  maxact,maxdi4,maxdr8,maxdc8,ikyadr,ikytyp,keytch
      include 'inclpi.h'
      integer lect
      common/helicity/lect(4000)
C LUND
      integer n,k,klu
      real*4  p,v,plu
      external klu,plu
      common/lujets/n,k(4000,5),p(4000,5),v(4000,5)
C Standard HEPEVT common block (single precision)
      integer     NMXHEP
      parameter (NMXHEP=2000)
      integer NEVHEP,NHEP,ISTHEP,IDHEP,JMOHEP,JDAHEP
      real*4  PHEP,VHEP
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
C - Event counter
      INTEGER        NGENE,IDB1,IDB2,NISR,NFSR,KF4F,JF4F   
      COMMON /RTGRCE/NGENE,IDB1,IDB2,NISR,NFSR,KF4F(6),JF4F(6)     
      REAL           V4F,V4ISR,V4FSR,HEL4F
      COMMON /RTGRCV/V4F(6,5),V4ISR(20,5),V4FSR(20,5),HEL4F(6)
C local
      integer isrtype,flag,ipcall,i4f,itr,jtr,ktr,iploop,i1,i2,kc,ichrg
      integer ipoint(4),ipf1(2),ipf2(2)
      integer IDP,NFSR0,NHEP0
      real*4  aamw,aagw,aamz,aagz
      real*4  mass1,mass2,massW,massWP,massWM,massZ
C --------------------------------------------------------------------
C - check ISR type
      isrtype = i4data(ikyadr(7))
c     print *, '+++GRCPHO+++ : isrtype',isrtype,' IDB1 IDB2',IDB1,IDB2
C   - no correction for Tree or QEDPSI  
      if (isrtype.ne.1 .and. isrtype.ne.2) return
C   - set mass
         aamw = sngl(r8data(ikyadr(65)))
         aagw = sngl(r8data(ikyadr(66)))
         aamz = sngl(r8data(ikyadr(67)))
         aagz = sngl(r8data(ikyadr(68)))         
C        print *, '+++GRCPHO+++ : W/Z ',aamw,aagw,aamz,aagz
C -------------------------------------------------------------------
C
      if (flag) 1, 1000, 5000
C
C - Initialization --------------------------------------------------
C
    1 continue   
C
C - PHOTOS Initializaiton
         print *, '+++GRCPHO+++ : PHOTOS initialization'
         call PHOINI 
c        call PHOCIN
c        call PHOINF
      return
C   
C - PHOTOS generation ------------------------------------------
C
 1000 continue
C    - debug 
      if (NGENE.GE.IDB1 .AND. NGENE.LE.IDB2) then
         print *, '+++GRCPHO+++ : calling lulist First'
         call LULIST(1)
      end if
C
C - HEPEVT Initialization
         NEVHEP = 0
         NHEP   = 0
         ipcall = 0
C
C
C - Event analysis from LUJETS information
C     isrtype 1 : SF 1 or 2 photons comes first
C           2,3 : QEDPSIF first 4f, then n photons  
C
      i4f  = 0
      NISR = 0
      NFSR = 0
C     Beam particles
c        if (abs(KLU(itr,2)).eq.11 .and. KLU(itr,3).eq.0) then
c            continue
c        end if
      do 10 itr=3,n
C        ISR photons
         if (KLU(itr,2).eq.22 .and. 
     &           (KLU(itr,3).eq.1) .or. KLU(itr,3).eq.2) then
            NISR = NISR+1
C        4 fermions
         else if (KLU(itr,2).ne.22 .and. KLU(itr,3).eq.1) then
            i4f = i4f+1
            if (i4f.gt.4) then
               print *, '+++GRCPHO+++ : i4f > 4 !!!'
               goto 10
            end if   
            ipoint(i4f)=itr
            do 11 jtr=1,5
               V4F(i4f,jtr)=PLU(itr,jtr)
 11         continue
               KF4F(i4f)=KLU(itr,2)
         else
            print *, '+++GRCPHO+++ : particle not found'
            return
         end if   
 10   continue
C        
      if (i4f.ne.4) then
         print *, '+++GRCPHO+++ : i4f .ne. 4 !!!',i4f
         return
      end if   
C   Pack process code 
      IDP = 1000000*abs(KF4F(1))+10000*abs(KF4F(2))
     &     +    100*abs(KF4F(3))+      abs(KF4F(4))
C
C **************************************************************
C for single-W e- + nu_e_bar + f3 + f4_bar  ONLY !!!!!!!!!
C radiate photons for leptonic W decays (electron, muon, tau)
C **************************************************************
C --- l nu l nu ------------------------------------------------
C --- single-W  ------------------------------------------------
C    - find eenn,enmn,entn process, otherwise return !!!
*    e e nu nu 
      if      (IDP.eq.11111212) then
         massZ  = (V4F(3,4)+V4F(4,4))**2-(V4F(3,1)+V4F(4,1))**2
     .           -(V4F(3,2)+V4F(4,2))**2-(V4F(3,3)+V4F(4,3))**2
         massZ  = sqrt(MAX(0.0,massZ))
         massWP = (V4F(2,4)+V4F(3,4))**2-(V4F(2,1)+V4F(3,1))**2
     .           -(V4F(2,2)+V4F(3,2))**2-(V4F(2,3)+V4F(3,3))**2
         massWP = sqrt(MAX(0.0,massWP))
         massWM = (V4F(1,4)+V4F(4,4))**2-(V4F(1,1)+V4F(4,1))**2
     .           -(V4F(1,2)+V4F(4,2))**2-(V4F(1,3)+V4F(4,3))**2
         massWM = sqrt(MAX(0.0,massWM))
C    - debug
         if (NGENE.GE.IDB1 .AND. NGENE.LE.IDB2) then
            print *, '+++GRCPHO+++ : aamz,aagz,massZ ',aamz,aagz,massZ
            print *, '+++GRCPHO+++ : aamw,aagw,massWP',aamw,aagw,massWP
            print *, '               aamw,aagw,massWM',aamw,aagw,massWM
         end if
* --- Cut against Z->neu+neu events (within 5*width)
         if (abs(massZ-aamz).lt.5.0*aagz) return
* --- WW pair
C     - apply PHOTOS only for W like events !
c            if (abs(massWP-aamw).lt.5.0*aagw .and.
c     .          abs(massWM-aamw).lt.5.0*aagw) then
c               ipcall  = 2
c               ipf1(1) = 2
c               ipf2(1) = 3      
c               ipf1(2) = 1
c               ipf2(2) = 4
c            else
c               return
c            end if
* --- single-W
           if (abs(massWP-aamw).lt.5.0*aagw) then
              ipcall  = 1
              ipf1(1) = 2
              ipf2(1) = 3
           else
              return
           end if 
C
*    e e nu_mu nu_mu / e e nu_tau nu_tau
C      else if (IDP.eq.11111414 .or. IDP.eq.11111616) then
C         return  
C
*    e   nu mu nu, e   nu tau nu  
*    mu  nu mu nu, mu  nu tau nu  
*                  tau nu tau nu  
      else if (IDP.eq.11121314 .or. IDP.eq.11121516
     &    .or. IDP.eq.14131314 .or. IDP.eq.13141516
     &                         .or. IDP.eq.16151516) then
c            ipcall  = 2
c            ipf1(1) = 1
c            ipf2(1) = 2      
c            ipf1(2) = 3
c            ipf2(2) = 4
* --- single-W
            ipcall  = 1
            ipf1(1) = 3
            ipf2(1) = 4      
C
C --- l nu q q -------------------------------------------------
*    e   nu u d / e   nu c s
*    mu  nu u d / mu  nu c s
*    tau nu u d / tau nu c s 
C      else if (IDP.eq.11120201 .or. IDP.eq.11120403
C     &    .or. IDP.eq.13140201 .or. IDP.eq.13140403
C     &    .or. IDP.eq.15160201 .or. IDP.eq.15160403) then
C            ipcall  = 1
C            ipf1(1) = 1
C            ipf2(1) = 2      
c
      else
C    - debug 
         if (NGENE.GE.IDB1 .AND. NGENE.LE.IDB2) then
            print *, '+++GRCPHO+++ : not for PHOTOS process !'
         end if
         return
      end if   
C
C --- reconstruct boson  --------------------------------------
C
      do 100 iploop=1,ipcall
*
         i1 = ipf1(iploop)
         i2 = ipf2(iploop)
         mass1 =  V4F(i1,5)
         mass2 =  V4F(i2,5)
         massW = (V4F(i1,4)+V4F(i2,4))**2-(V4F(i1,1)+V4F(i2,1))**2
     .          -(V4F(i1,2)+V4F(i2,2))**2-(V4F(i1,3)+V4F(i2,3))**2
         massW = sqrt(MAX(0.0,massW))
C    - debug 
         if (NGENE.GE.IDB1 .AND. NGENE.LE.IDB2) then
          print *, '+++GRCPHO+++ : aamw,aagw,massW',aamw,aagw,massW   
         end if
C     - apply PHOTOS only for W like events !
         if (abs(massW-aamw).lt.5.0*aagw) then
            continue
         else
            goto 100
         end if
C --------------------------------------------------

         NEVHEP = NGENE
         NHEP   = 3
         NFSR0  = 0
C
         ISTHEP(1) = 2
C
         ichrg = KF4F(i1)+KF4F(i2)
         if   (ichrg.gt.0) then
            IDHEP(1) =  24
         else if (ichrg.lt.0) then
            IDHEP(1) = -24
         else
            IDHEP(1) =  23
         end if  
         do 20 jtr=1,4
            PHEP(jtr,1)=V4F(i1,jtr)+V4F(i2,jtr)
 20      continue
            PHEP(5,1)=massW
C       No vertex / infinit lifetime 
         do 21 jtr=1,4
            VHEP(jtr,1)=0.0
 21      continue
C
            JMOHEP(1,1) = 0
            JMOHEP(2,1) = 0
            JDAHEP(1,1) = 2
            JDAHEP(2,1) = 3
C
*     - Daughter
            ISTHEP(2)   = 1
            ISTHEP(3)   = 1
            IDHEP(2)    = KF4F(i1)
            IDHEP(3)    = KF4F(i2)
C
            JMOHEP(1,2) = 1
            JMOHEP(1,3) = 1
            JMOHEP(2,2) = 0
            JMOHEP(2,3) = 0
C
            JDAHEP(1,2) = 0
            JDAHEP(1,3) = 0
            JDAHEP(2,2) = 0
            JDAHEP(2,3) = 0
C
            do 22 jtr=1,5
               PHEP(jtr,2)=V4F(i1,jtr)
               PHEP(jtr,3)=V4F(i2,jtr)
 22         continue
c            PHEP(5,2)=mass1
c            PHEP(5,3)=mass2
C       No vertex / infinit lifetime 
            do 23 jtr=1,4
               VHEP(jtr,2)=0.0
               VHEP(jtr,2)=0.0
 23         continue
C     - debug
         if (NGENE.GE.IDB1 .AND. NGENE.LE.IDB2) then
            print *, '+++GRCPHO+++ : call HELLST before PHOTOS'
            call HEPLST(6,2)
         end if
C     - get FSR
         NHEP0 = NHEP
         call PHOTOS(1)
         NFSR0  = NHEP - NHEP0
C     - debug
         if (NGENE.GE.IDB1 .AND. NGENE.LE.IDB2) then
            print *, '+++GRCPHO+++ : call HELLST after  PHOTOS'
            call HEPLST(6,2)
            if (NFSR0.gt.1) then
               print *, '+++GRCPHO+++ : more than 1 FSR !',NFSR0
            end if   
         end if
C
C     - Update LUJETS (2 fermion + fsr)
         if (NFSR0.gt.0) then
*     fermion 1,2 --- p:changed, k:un-changed, v:un-changed
            do 30 ktr=1,5
               p(ipoint(i1),ktr)=PHEP(ktr,2)
               p(ipoint(i2),ktr)=PHEP(ktr,3)
 30         continue
*     FSR --- added at the end
            do 31 jtr=1,NFSR0
               kc = n + jtr
               do 32 ktr=1,4
                  p(kc,ktr)=PHEP(ktr,jtr+3)
 32            continue
                  p(kc,5)=0.0
                  k(kc,1)=  1
                  k(kc,2)= 22
                  if (ichrg.lt.0) then
                     k(kc,3)=  1
                  else if (ichrg.gt.0) then
                     k(kc,3)=  2
                  else
                     k(kc,3)=  1
                  end if  
C                    error in KXL7AL if >2
ccc                  k(kc,3)=  3 
                  k(kc,4)=  0
                  k(kc,5)=  0
               do 33 ktr=1,5
                  v(kc,ktr)=0.0
 33            continue
               lect(kc) = 999
 31         continue   
            n = n + NFSR0
            NFSR = NFSR + NFSR0
C     - debug
            if (NGENE.GE.IDB1 .AND. NGENE.LE.IDB2) then
               print *, '+++GRCPHO+++ : call LULIST after update'
               call LULIST(1)
            end if
         end if
*
 100  continue   
C
      return   
C
C - PHOTOS summary ---------------------------------------------
C
 5000 continue   
         call PHOREP
C
      return
      end




C      SUBROUTINE DRNSET( ISEED )
CC ------
CC DRNSET     dummy routine for DRN (RDMIN is used for RANMAR)
CC ------
CC
C      implicit none
C      integer ISEED
CC
C      print *,'+++DRNSET+++ : dummy ISEED in DRNSET =',ISEED       
CC
C      RETURN
C      END
C
C


      double precision function drn(iseed)
C ------
C  drn       random number used in bases/spring (interfaced to RANMAR)
C ------
c
      implicit none
      integer  iseed
      real*4   rndm,dum
      external rndm
c
      drn = dble(rndm(dum))
c
      RETURN
      END



      subroutine grcmct(m)
C ------
C grcmct     Protect for minimum invarianat mass for u,d quarks
C ------
      implicit none
c
      integer  maxact,maxdi4,maxdr8,maxdc8,ikyadr,ikytyp,keytch
      include 'inclpi.h'
      integer MPROC,mulprc,ichgcj,mevent
      real*8  xscton, xscter
      include 'inclan.h'
c
      integer i,m
      character*8 grcpcn
      external    grcpcn
C - Local variables
      INTEGER INUT,IOUT
      COMMON /INOUT/ INUT,IOUT
      real*8 twopi
      data twopi/0.270d0/
C
C - brcbs/grcsp -> userin -> kinit0 -> kinctu -> amasct=r8data(iky)
c       vct341 = r8data(ikyadr( 47))
c       vct342 = r8data(ikyadr( 48))
c       vct561 = r8data(ikyadr( 49))
c       vct562 = r8data(ikyadr( 50))
c       vct351 = r8data(ikyadr( 51))
c       vct352 = r8data(ikyadr( 52))
c       vct461 = r8data(ikyadr( 53))
c       vct462 = r8data(ikyadr( 54))
c       vct361 = r8data(ikyadr( 55))
c       vct362 = r8data(ikyadr( 56))
c       vct451 = r8data(ikyadr( 57))
c       vct452 = r8data(ikyadr( 58))
C ----------------------------------------------------------------
C - if not single process, first reset all
      if (mulprc(0).gt.1) then
         write(IOUT,*) ' Warning: reset Minv cuts for multi-process'
         do 100 i=47,58
            r8data(ikyadr(i))=-1.d0
 100     continue  
      end if
C
C ------------ minimum invariant mass protection -----------------
c     - enud, mnud, tnud
      if      (grcpcn(m).eq.'eceNuqdQ' .or. grcpcn(m).eq.'mcmNuqdQ'
     .    .or. grcpcn(m).eq.'tctNuqdQ') then
         write(IOUT,*) ' Warning: min Minv changed for 56'
         if (r8data(ikyadr(49)).lt.twopi) r8data(ikyadr(49))=twopi
c     - eeuu, eedd, mmuu, mmdd, ttuu, ttdd
      else if (grcpcn(m).eq.'eceCuquQ' .or. grcpcn(m).eq.'eceCdqdQ'
     .    .or. grcpcn(m).eq.'mcmCuquQ' .or. grcpcn(m).eq.'mcmCdqdQ'
     .    .or. grcpcn(m).eq.'tctCuquQ' .or. grcpcn(m).eq.'tctCdqdQ')
     .   then
         write(IOUT,*) ' Warning: min Minv changed for 56'
         if (r8data(ikyadr(49)).lt.twopi) r8data(ikyadr(49))=twopi
c     - nnuu, nndd, nnuu, nndd, nnuu, nndd
      else if (grcpcn(m).eq.'eneNuquQ' .or. grcpcn(m).eq.'eneNdqdQ'
     .    .or. grcpcn(m).eq.'mnmNuquQ' .or. grcpcn(m).eq.'mnmNdqdQ'
     .    .or. grcpcn(m).eq.'tntNuquQ' .or. grcpcn(m).eq.'tntNdqdQ')
     .   then
         write(IOUT,*) ' Warning: min Minv changed for 56'
         if (r8data(ikyadr(49)).lt.twopi) r8data(ikyadr(49))=twopi
c     - uddu, uuuu, dddd
      else if (grcpcn(m).eq.'uqdQdquQ' .or. grcpcn(m).eq.'uquQuquQ'
     .    .or. grcpcn(m).eq.'dqdQdqdQ') then
         write(IOUT,*) ' Warning: min Minv changed for 34,56,36,45'
         if (r8data(ikyadr(47)).lt.twopi) r8data(ikyadr(47))=twopi
         if (r8data(ikyadr(49)).lt.twopi) r8data(ikyadr(49))=twopi
         if (r8data(ikyadr(55)).lt.twopi) r8data(ikyadr(55))=twopi
         if (r8data(ikyadr(57)).lt.twopi) r8data(ikyadr(57))=twopi
c     - udsc, uucc, uuss, uubb, ddss, ddbb
      else if (grcpcn(m).eq.'uqdQsqcQ' .or. grcpcn(m).eq.'uquQcqcQ'
     .    .or. grcpcn(m).eq.'uquQsqsQ' .or. grcpcn(m).eq.'uquQbqbQ'
     .    .or. grcpcn(m).eq.'dqdQsqsQ' .or. grcpcn(m).eq.'dqdQbqbQ')
     .    then
         write(IOUT,*) ' Warning: min Minv changed for 34'
         if (r8data(ikyadr(47)).lt.twopi) r8data(ikyadr(47))=twopi
c     - ccdd
      else if (grcpcn(m).eq.'cqcQdqdQ') then
         write(IOUT,*) ' Warning: min Minv changed for 56'
         if (r8data(ikyadr(49)).lt.twopi) r8data(ikyadr(49))=twopi
      end if
C
      return
      end


C 1999/10/10: RT change ISR protection in jktype
C 1997/06/15: 1->76, --> 0->75
C 1997/06/08: add kqedps
C     Initilization of kinematics  (1997/03/05)
C     1997/04/02: "kclmb" array , 0(no permition of Coulmb cor.)
C                                 1(permition of Coulmb cor.)
C=======================================================================
C     Initialization of kinematics parameter (depends on energy ...)
C=======================================================================
*
      subroutine knmdbi
      implicit real*8(a-h,o-z)
      include 'inclkd.h'
*
C 1(old 77)  electron,positron,nu-e,nu-e-bar
*   irs[1-4]d(1) ... is dynamically defined in kinem.(04/02)
**    ngd(1)    = 56
*     irs1d(1) = 3
      irs1d(1) = 999
      ars1md(1) = 23
*     irs2d(1) = 4
      irs2d(1) = 999
      ars2md(1) = 23
*     irs3d(1) = 1
      irs3d(1) = 999
      ars3md(1) = 24
*     irs4d(1) = 1
      irs4d(1) = 999
      ars4md(1) = 24
      ics3d(1) = 2
      ics5d(1) = 2
      iph6d(1) = 0
      icq3d(1) = 2
      iswpd(1) = 0
      swpmd(1) = -999.
      idntd(1) = 0
      jtgmd(1) = 1
      kinmt(1) = 5
      iit1 (1) = 7
      iit2 (1) =15
      incal(1) = 80000
*
      kclmb(1) = 0
      kclmb(1) = 1
C 2  electron,nu-e-bar,anti-muon,nu-mu
C 3  electron,nu-e-bar,anti-tau,nu-tau
      do 1002 ip = 2, 3
**          ngd(ip) = 18
          irs1d(ip) = 1
         ars1md(ip) = 24
          irs2d(ip) = 1
         ars2md(ip) = 24
          kclmb(ip) = 1
          irs3d(ip) = -255
         ars3md(ip) = 0
          irs4d(ip) = -255
         ars4md(ip) = 0
          ics3d(ip) = 1
          ics5d(ip) = 0
          iph6d(ip) = 0
          icq3d(ip) = 2
          iswpd(ip) = 0
          swpmd(ip) = -999.
          idntd(ip) = 0
          jtgmd(ip) = 1
          kinmt(ip) = 1
           iit1(ip) = 7
           iit2(ip) = 15
          incal(ip) = 40000
 1002 continue

C 4  nu-mu,anti-muon,muon,nu-mu-bar
C 5  nu-tau,anti-tau,tau,nu-tau-bar
      do 1004 ip = 4, 5
**          ngd(ip) = 20
          irs1d(ip) = 1
         ars1md(ip) = 24
          irs2d(ip) = 1
         ars2md(ip) = 24
          kclmb(ip) = 1
          irs3d(ip) = 1
         ars3md(ip) = 23
          irs4d(ip) = 3
         ars4md(ip) = 23
          ics3d(ip) = 0
          ics5d(ip) = 5
          iph6d(ip) = -999
          icq3d(ip) = 2
          iswpd(ip) = 1
          swpmd(ip) = -999.
          idntd(ip) = 0
          jtgmd(ip) = 0
          kinmt(ip) = 4
           iit1(ip) = 7
           iit2(ip) = 15
          incal(ip) = 40000
 1004 continue

C 6  muon,nu-mu-bar,anti-tau,nu-tau
**    ngd( 6)    = 9
      irs1d( 6) = 1
      ars1md( 6) = 24
      irs2d( 6) = 1
      ars2md( 6) = 24
          kclmb(6) = 1
      irs3d( 6) = -255
      ars3md( 6) = 0
      irs4d( 6) = -255
      ars4md( 6) = 0
      ics3d( 6) = 0
      ics5d( 6) = 0
      iph6d( 6) = 0
      icq3d( 6) = 2
      iswpd( 6) = 0
      swpmd( 6) = -999.
      idntd( 6) = 0
      jtgmd( 6) = 0
      kinmt( 6) = 1
      iit1 ( 6) = 7
      iit2 ( 6) =15
      incal( 6) = 80000

C 7  electron,positron,electron,positron
**    ngd( 7)    = 144
      irs1d( 7) =  2
      ars1md( 7) = 23
      irs2d( 7) = 3
      ars2md( 7) = 23
          kclmb(7) = 0
      irs3d( 7) = -1
*     ars3md( 7) = amtq
      ars3md( 7) = 0
      irs4d( 7) = 0
      ars4md( 7) = 0
      ics3d( 7) = 2
      ics5d( 7) = 2
      iph6d( 7) = 0
      icq3d( 7) = 2
      iswpd( 7) = 0
      swpmd( 7) = -999.
      idntd( 7) = 0
      jtgmd( 7) = 1
      iit1 ( 7) = 7
      iit2 ( 7) =15
      incal( 7) = 80000
*     kinmt( 7) = 2
      kinmt( 7) = 6
*                 ^

C 8  electron,positron,muon,anti-muon
C 9  electron,positron,tau,anti-tau
      do 1008 ip = 8, 9

**    ngd(ip)    = 50
      irs1d(ip) = 2
      ars1md(ip) = 23
      irs2d(ip) = 3
      ars2md(ip) = 23
          kclmb(ip) = 0
      irs3d(ip) = -255
      ars3md(ip) = 0
      irs4d(ip) = -255
      ars4md(ip) = 0
      ics3d(ip) = 2
      ics5d(ip) = 2
      iph6d(ip) = 0
      icq3d(ip) = 2
      iswpd(ip) = 0
      swpmd(ip) = -999.
      idntd(ip) = 0
      jtgmd(ip) = 1
      kinmt(ip) = 2
      iit1 (ip) = 7
      iit2 (ip) =15
      incal(ip) = 40000
 1008 continue


C 10  muon,anti-muon,muon,anti-muon
C 11  tau,anti-tau,tau,anti-tau
      do 1010 ip = 10, 11

**    ngd(ip)    = 68
      irs1d(ip) = 3
      ars1md(ip) = 23
      irs2d(ip) = 3
      ars2md(ip) = 23
          kclmb(ip) = 0
*     irs3d(ip) = 4
      irs3d(ip) = 3
      ars3md(ip) = 23
*     irs4d(ip) = -1
      irs4d(ip) = 3
      ars4md(ip) = 23
      ics3d(ip) = 0
      ics5d(ip) = 3
      iph6d(ip) = -999
      icq3d(ip) = 2
      iswpd(ip) = 1
      swpmd(ip) = 3.
      idntd(ip) = 4
      jtgmd(ip) = 0
      kinmt(ip) = 3
      iit1 (ip) = 7
      iit2 (ip) =15
      incal(ip) = 80000
 1010 continue

C 12  muon,anti-muon,tau,anti-tau
**    ngd(12)    = 34
      irs1d(12) = 3
      ars1md(12) = 23
      irs2d(12) = 3
      ars2md(12) = 23
          kclmb(12) = 0
      irs3d(12) = -255
      ars3md(12) = 0
      irs4d(12) = -255
      ars4md(12) = 0
      ics3d(12) = 0
      ics5d(12) = 0
      iph6d(12) = 0
      icq3d(12) = 2
      iswpd(12) = 0
      swpmd(12) = -999.
      idntd(12) = 0
      jtgmd(12) = 0
      kinmt(12) = 1
      iit1 (12) = 7
      iit2 (12) =15
      incal(12) = 80000

C 13  electron,positron,nu-mu,nu-mu-bar
C 14  electron,positron,nu-tau,nu-tau-bar

      do 1013 ip = 13, 14

**    ngd(ip)    = 20
      irs1d(ip) = 3
      ars1md(ip) = 23
      irs2d(ip) = 1
      ars2md(ip) = 23
          kclmb(ip) = 0
      irs3d(ip) = -255
      ars3md(ip) = 0
      irs4d(ip) = -255
      ars4md(ip) = 0
      ics3d(ip) = 2
      ics5d(ip) = 0
      iph6d(ip) = 0
      icq3d(ip) = 2
      iswpd(ip) = 0
      swpmd(ip) = -999.
      idntd(ip) = 0
      jtgmd(ip) = 1
      kinmt(ip) = 1
      iit1 (ip) = 7
      iit2 (ip) =15
      incal(ip) = 40000

 1013 continue

C 15  nu-e,nu-e-bar,muon,anti-muon
C 16  nu-e,nu-e-bar,tau,anti-tau

      do 1015 ip = 15, 16

**    ngd(ip)    = 21
      irs1d(ip) = 1
      ars1md(ip) = 23
      irs2d(ip) = 3
      ars2md(ip) = 23
          kclmb(ip) = 0
      irs3d(ip) = -255
      ars3md(ip) = 0
      irs4d(ip) = -255
      ars4md(ip) = 0
      ics3d(ip) = 0
      ics5d(ip) = 0
      iph6d(ip) = 0
      icq3d(ip) = 2
      iswpd(ip) = 0
      swpmd(ip) = -999.
      idntd(ip) = 0
      jtgmd(ip) = 0
      kinmt(ip) = 1
       iit1(ip) = 7
       iit2(ip) = 15
      incal(ip) = 80000
 1015 continue

C 17  nu-tau,nu-tau-bar,muon,anti-muon
C 18  nu-mu,nu-mu-bar,tau,anti-tau
      do 1017 ip = 17, 18

**    ngd(ip)    = 11
      irs1d(ip) = 1
      ars1md(ip) = 23
      irs2d(ip) = 3
      ars2md(ip) = 23
          kclmb(ip) = 0
      irs3d(ip) = -255
      ars3md(ip) = 0
      irs4d(ip) = -255
      ars4md(ip) = 0
      ics3d(ip) = 0
      ics5d(ip) = 0
      iph6d(ip) = 0
      icq3d(ip) = 2
      iswpd(ip) = 0
      swpmd(ip) = -999.
      idntd(ip) = 0
      jtgmd(ip) = 0
      kinmt(ip) = 1
      iit1 (ip) = 7
      iit2 (ip) =15
      incal(ip) = 80000
 1017 continue

C 19  nu-e,nu-e-bar,nu-e,nu-e-bar
**    ngd(19)    = 36
      irs1d(19) = 1
      ars1md(19) = 23
      irs2d(19) = 1
      ars2md(19) = 23
          kclmb(19) = 0
      irs3d(19) = 1
      ars3md(19) = 23
      irs4d(19) = 1
      ars4md(19) = 23
      ics3d(19) = 0
      ics5d(19) = 4
      iph6d(19) = 0
      icq3d(19) = 0
      iswpd(19) = 1
      swpmd(19) = -999.
      idntd(19) = 2
      jtgmd(19) = 0
      kinmt(19) = 3
      iit1 (19) = 7
      iit2 (19) =15
      incal(19) = 80000

C 20  nu-e,nu-e-bar,nu-mu,nu-mu-bar
C 21  nu-e,nu-e-bar,nu-tau,nu-tau-bar

      do 1020 ip = 20, 21

**    ngd(ip)    = 12
      irs1d(ip) = 1
      ars1md(ip) = 23
      irs2d(ip) = 1
      ars2md(ip) = 23
          kclmb(ip) = 0
      irs3d(ip) = -255
      ars3md(ip) = 0
      irs4d(ip) = -255
      ars4md(ip) = 0
      ics3d(ip) = 0
      ics5d(ip) = 0
      iph6d(ip) = 0
      icq3d(ip) = 0
      iswpd(ip) = 0
      swpmd(ip) = -999.
      idntd(ip) = 0
      jtgmd(ip) = 0
      kinmt(ip) = 1
      iit1 (ip) = 7
      iit2 (ip) =15
      incal(ip) = 40000
 1020 continue

C 22  nu-mu,nu-mu-bar,nu-mu,nu-mu-bar
C 23  nu-tau,nu-tau-bar,nu-tau,nu-tau-bar
      do 1022 ip = 22, 23

**    ngd(ip)    = 12
      irs1d(ip) = 1
      ars1md(ip) = 23
      irs2d(ip) = 1
      ars2md(ip) = 23
          kclmb(ip) = 0
      irs3d(ip) = 1
      ars3md(ip) = 23
      irs4d(ip) = 1
      ars4md(ip) = 23
      ics3d(ip) = 0
      ics5d(ip) = 4
      iph6d(ip) = 0
      icq3d(ip) = 0
      iswpd(ip) = 1
      swpmd(ip) = -999.
      idntd(ip) = 2
      jtgmd(ip) = 0
      kinmt(ip) = 3
      iit1 (ip) = 7
      iit2 (ip) = 15
      incal(ip) = 40000
 1022 continue

C 24  nu-tau,nu-tau-bar,nu-mu,nu-mu-bar
**    ngd(24)    = 6
      irs1d(24) = 1
      ars1md(24) = 23
      irs2d(24) = 1
      ars2md(24) = 23
          kclmb(24) = 0
      irs3d(24) = -255
      ars3md(24) = 0
      irs4d(24) = -255
      ars4md(24) = 0
      ics3d(24) = 0
      ics5d(24) = 0
      iph6d(24) = 0
      icq3d(24) = 0
      iswpd(24) = 0
      swpmd(24) = -999.
      idntd(24) = 0
      jtgmd(24) = 0
      kinmt(24) = 1
      iit1 (24) = 7
      iit2 (24) =15
      incal(24) = 80000

C 25  electron,nu-e-bar,u,d-bar
C 26  electron,nu-e-bar,c,s-bar
      do 1025 ip = 25, 26

**    ngd(ip)    = 20
      irs1d(ip) = 1
      ars1md(ip) = 24
      irs2d(ip) = 1
      ars2md(ip) = 24
          kclmb(ip) = 1
      irs3d(ip) = -255
      ars3md(ip) = 0
      irs4d(ip) = -255
      ars4md(ip) = 0
      ics3d(ip) = 1
      ics5d(ip) = 2
      iph6d(ip) = 0
      icq3d(ip) = 1
      iswpd(ip) = 0
      swpmd(ip) = -999.
      idntd(ip) = 0
      jtgmd(ip) = 1
      kinmt(ip) = 1
      iit1 (ip) = 7
      iit2 (ip) = 15
      incal(ip) = 40000
 1025 continue

C 27  muon,nu-mu-bar,u,d-bar
C 28  muon,nu-mu-bar,c,s-bar
C 29  tau,nu-tau-bar,u,d-bar
C 30  tau,nu-tau-bar,c,S-bar
      do 1027 ip = 27, 30

**    ngd(ip)    = 10
      irs1d(ip) = 1
      ars1md(ip) = 24
      irs2d(ip) = 1
      ars2md(ip) = 24
          kclmb(ip) = 1
      irs3d(ip) = -255
      ars3md(ip) = 0
      irs4d(ip) = -255
      ars4md(ip) = 0
      ics3d(ip) = 0
      ics5d(ip) = 0
      iph6d(ip) = 0
      icq3d(ip) = 1
      iswpd(ip) = 0
      swpmd(ip) = -999.
      idntd(ip) = 0
      jtgmd(ip) = 0
      kinmt(ip) = 1
      iit1 (ip) = 7
      iit2 (ip) =15
      incal(ip) = 40000
 1027 continue

C 31  electron,positron,u,u-bar
C 32  electron,positron,c,c-bar
C 33  electron,positron,d,d-bar
C 34  electron,positron,s,s-bar
C 35  electron,positron,b,b-bar
      do 1031 ip = 31, 35

**    ngd(ip)    = 50
      irs1d(ip) = 2
      ars1md(ip) = 23
      irs2d(ip) = 3
      ars2md(ip) = 23
          kclmb(ip) = 0
      irs3d(ip) = -255
      ars3md(ip) = 0
      irs4d(ip) = -255
      ars4md(ip) = 0
      ics3d(ip) = 2
      ics5d(ip) = 2
      iph6d(ip) = 0
      icq3d(ip) = 2
      iswpd(ip) = 0
      swpmd(ip) = -999.
      idntd(ip) = 0
      jtgmd(ip) = 1
      kinmt(ip) = 2
       iit1(ip) = 7
       iit2(ip) = 15
      incal(ip) = 40000

 1031 continue

C 36  muon,anti-muon,u,u-bar
C 37  muon,anti-muon,c,c-bar
C 38  tau,anti-tau,u,u-bar
C 39  tau,anti-tau,c,c-bar
C 40  muon,anti-muon,d,d-bar
C 41  muon,anti-muon,s,s-bar
C 42  muon,anti-muon,b,b-bar
C 43  tau,anti-tau,d,d-bar
C 44  tau,anti-tau,s,s-bar
C 45  tau,anti-tau,b,b-bar

      do 1036 ip = 36, 45

**    ngd(ip)    = 34
*     for CANON-CUT
*     irs1d(ip) = 1
*     for NO-CUT
*YK   irs1d(ip) = 3
      irs1d(ip) = 2
      ars1md(ip) = 23

*YK   irs2d(ip) = 3
      irs2d(ip) = 2
      ars2md(ip) = 23
          kclmb(ip) = 0
*YK 97/03/08
      if( ip .eq. 36 ) irs2d(ip) = 3
      irs3d(ip) = -255
      ars3md(ip) = 0
      irs4d(ip) = -255
      ars4md(ip) = 0
      ics3d(ip) = 0
      ics5d(ip) = 0
      iph6d(ip) = 0
      icq3d(ip) = 2
      iswpd(ip) = 0
      swpmd(ip) = -999.
      idntd(ip) = 0
      jtgmd(ip) = 0
      kinmt(ip) = 1
      iit1 (ip) = 7
      iit2 (ip) =15
      incal(ip) = 40000

 1036 continue

C 46  nu-e,nu-e-bar,u,u-bar
C 47  nu-e,nu-e-bar,c,c-bar
C 48  nu-e,nu-e-bar,d,d-bar
C 49  nu-e,nu-e-bar,s,s-bar
C 50  nu-e,nu-e-bar,b,b-bar

      do 1046 ip = 46, 50

**    ngd(ip)    = 21
      irs1d(ip) = 1
      ars1md(ip) = 23
      if( ip .le. 47 ) then
*         u,c
          irs2d(ip) = 3
      else
*         d,s,b
          irs2d(ip) = 2
      endif
      ars2md(ip) = 23
          kclmb(ip) = 0
      irs3d(ip) = -255
      ars3md(ip) = 0
      irs4d(ip) = -255
      ars4md(ip) = 0
      ics3d(ip) = 0
      ics5d(ip) = 0
      iph6d(ip) = 0
      icq3d(ip) = 2
      iswpd(ip) = 0
      swpmd(ip) = -999.
      idntd(ip) = 0
      jtgmd(ip) = 0
      kinmt(ip) = 1
      iit1 (ip) = 7
      iit2 (ip) =15
      incal(ip) = 40000

 1046 continue

C 51  nu-mu,nu-mu-bar,u,u-bar
C 52  nu-mu,nu-mu-bar,c,c-bar
C 53  nu-tau,nu-tau-bar,u,u-bar
C 54  nu-tau,nu-tau-bar,c,c-bar
C 55  nu-mu,nu-mu-bar,d,d-bar
C 56  nu-mu,nu-mu-bar,s,s-bar
C 57  nu-mu,nu-mu-bar,b,b-bar
C 58  nu-tau,nu-tau-bar,d,d-bar
C 59  nu-tau,nu-tau-bar,s,s-bar
C 60  nu-tau,nu-tau-bar,b,b-bar

      do 1051 ip = 51, 60

**    ngd(ip)    = 11
      irs1d(ip) = 1
      ars1md(ip) = 23
      irs2d(ip) = 3
      ars2md(ip) = 23
          kclmb(ip) = 0
      irs3d(ip) = -255
      ars3md(ip) = 0
      irs4d(ip) = -255
      ars4md(ip) = 0
      ics3d(ip) = 0
      ics5d(ip) = 0
      iph6d(ip) = 0
*     icq3d(ip) = 0
      icq3d(ip) = 2
      iswpd(ip) = 0
      swpmd(ip) = -999.
      idntd(ip) = 0
      jtgmd(ip) = 0
      kinmt(ip) = 1
      iit1 (ip) = 7
      iit2 (ip) =15
      incal(ip) = 40000

 1051 continue


C 61  u,d-bar,d,u-bar
C 62  c,s-bar,s,c-bar
      do 1061 ip = 61, 62
*
**    ngd(ip)    = 53
      irs1d(ip) = 1
      ars1md(ip) = 24
      irs2d(ip) = 1
      ars2md(ip) = 24
          kclmb(ip) = 1
      irs3d(ip) = 3
      ars3md(ip) = 23
      irs4d(ip) = 3
      ars4md(ip) = 23
      ics3d(ip) = 0
      ics5d(ip) = 999
      iph6d(ip) = 0
      icq3d(ip) = 2
      iswpd(ip) = 1
      swpmd(ip) = -999.
      idntd(ip) = 0
      jtgmd(ip) = 0
      kinmt(ip) = 4
      iit1 (ip) = 7
      iit2 (ip) =15
      incal(ip) = 40000
 1061 continue

C 63  u,d-bar,s,c-bar
**    ngd(63)    = 11
      irs1d(63) = 1
      ars1md(63) = 24
      irs2d(63) = 1
      ars2md(63) = 24
          kclmb(63) = 1
      irs3d(63) = -255
      ars3md(63) = 0
      irs4d(63) = -255
      ars4md(63) = 0
      ics3d(63) = 0
      ics5d(63) = 0
      iph6d(63) = 0
      icq3d(63) = 2
      iswpd(63) = 0
      swpmd(63) = -999.
      idntd(63) = 0
      jtgmd(63) = 0
      kinmt(63) = 1
      iit1 (63) = 7
      iit2 (63) =15
      incal(63) = 80000

C 64  u,u-bar,u,u-bar
C 65  c,c-bar,c,c-bar
C 66  d,d-bar,d,d-bar
C 67  s,s-bar,s,s-bar
C 68  b,b-bar,b,b-bar
      do 1064 ip = 64, 68
*
**    ngd(ip)    = 84
      irs1d(ip) = 3
      ars1md(ip) = 23
      irs2d(ip) = 3
      ars2md(ip) = 23
          kclmb(ip) = 0
*     irs3d(ip) = 4
      irs3d(ip) = 3
      ars3md(ip) = 23
*     irs4d(ip) = -1
      irs4d(ip) = 3
      ars4md(ip) = 23
      ics3d(ip) = 0
      ics5d(ip) = 3
      iph6d(ip) = -999
      icq3d(ip) = 2
      iswpd(ip) = 1
      swpmd(ip) = 3.
      idntd(ip) = 4
      jtgmd(ip) = 0
      kinmt(ip) = 3
      iit1 (ip) = 7
      iit2 (ip) =15
      incal(ip) = 80000

 1064 continue

C 69  u,u-bar,c,c-bar
C 70  u,u-bar,s,s-bar
C 71  u,u-bar,b,b-bar
C 72  c,c-bar,d,d-bar
C 73  c,c-bar,b,b-bar
C 74  d,d-bar,s,s-bar
C 75  d,d-bar,b,b-bar
C 76  s,s-bar,b,b-bar

      do 1069 ip = 69, 76
*
**    ngd(ip)    = 42
      irs1d(ip) = 3
      ars1md(ip) = 23
      irs2d(ip) = 3
      ars2md(ip) = 23
          kclmb(ip) = 0
      irs3d(ip) = -255
      ars3md(ip) = 0
      irs4d(ip) = -255
      ars4md(ip) = 0
      ics3d(ip) = 0
      ics5d(ip) = 0
      iph6d(ip) = 0
      icq3d(ip) = 2
      iswpd(ip) = 0
      swpmd(ip) = -999.
      idntd(ip) = 0
      jtgmd(ip) = 0
      kinmt(ip) = 1
      iit1 (ip) = 7
      iit2 (ip) =15
      incal(ip) = 80000

 1069 continue
*=======================================================================
*===========================
* Default NCALL
*===========================
          do 1100 j = 1, MPROC
             incal(j) = 40000
 1100     continue
          incal(35) = 80000
          incal(36) = 80000
          incal(46) = 80000
          incal(51) = 80000
          incal(53) = incal(51)
          incal(51) = 80000
          incal(58) = incal(55)
          incal(64) = 80000

*==========================================================
* Protection of QEDPS  [3]{Tree,ISR,QEDPS,QEDPSif}
*                      [2]{Tree,ISR,QEDPS},[1]={Tree,ISR}
*==========================================================
          do 1200 j = 1, MPROC
             kqedps(j) = 3
 1200     continue
          kqedps( 1) = 1
          kqedps( 2) = 1
          kqedps( 3) = 1
          kqedps( 7) = 1
          kqedps( 8) = 1
          kqedps( 9) = 1
          kqedps(31) = 1
          kqedps(32) = 1
          kqedps(33) = 1
          kqedps(34) = 1
          kqedps(35) = 1
C --- RT added/modified -----------------
          kqedps( 1) = 2
          kqedps( 2) = 2
          kqedps( 3) = 2
          kqedps( 7) = 2
          kqedps( 8) = 2
          kqedps( 9) = 2
c          kqedps(13) = 1
c          kqedps(14) = 1
          kqedps(25) = 2
          kqedps(26) = 2
          kqedps(31) = 2
          kqedps(32) = 2
          kqedps(33) = 2
          kqedps(34) = 2
          kqedps(35) = 2
*----------------------------------------
C======== end of k172db.f ==============================================
* move

      do 8000 i = 1 , MPROC
         irs1d(i-1) = irs1d(i)
        ars1md(i-1) = ars1md(i)
         irs2d(i-1) = irs2d(i)
        ars2md(i-1) = ars2md(i)
         irs3d(i-1) = irs3d(i)
        ars3md(i-1) = ars3md(i)
         irs4d(i-1) = irs4d(i)
        ars4md(i-1) = ars4md(i)
         ics3d(i-1) = ics3d(i)
         ics5d(i-1) = ics5d(i)
         iph6d(i-1) = iph6d(i)
         icq3d(i-1) = icq3d(i)
         iswpd(i-1) = iswpd(i)
         idntd(i-1) = idntd(i)
         jtgmd(i-1) = jtgmd(i)
         kinmt(i-1) = kinmt(i)
          iit1(i-1) = iit1(i)
          iit2(i-1) = iit2(i)
         incal(i-1) = incal(i)
         kclmb(i-1) = kclmb(i)
        kqedps(i-1) = kqedps(i)
         swpmd(i-1) = swpmd(i)
 8000 continue
      return
      end

*990408:RT: add set 16-19 for two-photon process 7-9,31-35
*970715:TI: add set 7(S-chanel) and set 8(T-chanel)
*970615:TI: 1->76 --> 0->75
*970608:TI: move kqedps to kxxx.f
*970506:TI: define ngd in prcdb
*970506:TI: add set 10 and 11 for 7-9,31-35
*------------------------------------------------------------------
      subroutine prcdbi
      implicit real*8 (a-h,o-z)
*------------------------------------------------------------------
*     Initilization of data base of processes
*------------------------------------------------------------------
      include 'incl1.h'
      include 'inclpd.h'
* number of graphs
C========  ngd.f ====================================================
      ngd(1)    = 56
      do 3002 ip = 2, 3
            ngd(ip) = 18
 3002 continue
      do 3004 ip = 4, 5
            ngd(ip) = 20
 3004 continue
      ngd( 6)    =   9
      ngd( 7)    = 144
      do 3008 ip = 8, 9
         ngd(ip)    = 50
 3008 continue
      do 3010 ip = 10, 11
         ngd(ip)    = 68
 3010 continue
      ngd(12)    = 34
      do 3013 ip = 13, 14
         ngd(ip)    = 20
 3013 continue
      do 3015 ip = 15, 16
         ngd(ip)    = 21
 3015 continue
      do 3017 ip = 17, 18
         ngd(ip)    = 11
 3017 continue
      ngd(19)    = 36
      do 3020 ip = 20, 21
         ngd(ip)    = 12
 3020 continue
      do 3022 ip = 22, 23
         ngd(ip)    = 12
 3022 continue
      ngd(24)    = 6
      do 3025 ip = 25, 26
         ngd(ip)    = 20
 3025 continue
      do 3027 ip = 27, 30
         ngd(ip)    = 10
 3027 continue
      do 3031 ip = 31, 35
         ngd(ip)    = 50
 3031 continue
      do 3036 ip = 36, 45
         ngd(ip)    = 34
 3036 continue
      do 3046 ip = 46, 50
         ngd(ip)    = 21
 3046 continue
      do 3051 ip = 51, 60
         ngd(ip)    = 11
 3051 continue
      do 3061 ip = 61, 62
         ngd(ip)    = 53
 3061 continue
      ngd(63)    = 11
      do 3064 ip = 64, 68
         ngd(ip)    = 84
 3064 continue
      do 3069 ip = 69, 76
         ngd(ip)    = 42
 3069 continue
C=======================================================================
* subset of graph for the category
* type = 1    (WW)
*     kdiagr(1,1,13)=1
*     kdiagr(1,1,14)=1
*     kdiagr(1,1,51)=1
* type = 2    (ZZ)
*     kdiagr(1,2,48)=1
*     kdiagr(1,2,50)=1
*
*     --> define sigmdb.f
*
*========= removed (see below 76)
* particle name
*     kmpr_c(1,1) = 'electron'
*     kmpr_l(1,1) = 8
*     kmpr_c(1,2) = 'positron'
*     kmpr_l(1,2) = 8
*     kmpr_c(1,3) = 'nu-e'
*     kmpr_l(1,3) = 4
*     kmpr_c(1,4) = 'positron'
*     kmpr_l(1,4) = 8
*     kmpr_c(1,5) = 'electron'
*     kmpr_l(1,5) = 8
*     kmpr_c(1,6) = 'nu-e-bar'
*     kmpr_l(1,6) = 8
* masses of external particles
*     amas_1(1,1) = 11
*     amas_1(1,2) = 11
*     amas_1(1,3) = 12
*     amas_1(1,4) = 11
*     amas_1(1,5) = 11
*     amas_1(1,6) = 12
*
* Charge*3
*     kcha_g(1,1) =  -3
*     kcha_g(1,2) =   3
*     kcha_g(1,3) =   0
*     kcha_g(1,4) =   3
*     kcha_g(1,5) =  -3
*     kcha_g(1,6) =   0
*
* KFcode
*     kfco_e(1,1) =  11
*     kfco_e(1,2) = -11
*     kfco_e(1,3) =  12
*     kfco_e(1,4) = -11
*     kfco_e(1,5) =  11
*     kfco_e(1,6) = -12
* weight for the color base (llll)
*     cf_mtx(1,0,0) = 1.d0
*
*     kmcb_s(1)    = 1
*     kmcb_x(1)    = 0

* particle name
      kmpr_c(2,1) = 'electron'
      kmpr_l(2,1) = 8
      kmpr_c(2,2) = 'positron'
      kmpr_l(2,2) = 8
      kmpr_c(2,3) = 'electron'
      kmpr_l(2,3) = 8
      kmpr_c(2,4) = 'nu-e-bar'
      kmpr_l(2,4) = 8
      kmpr_c(2,5) = 'anti-muon'
      kmpr_l(2,5) = 9
      kmpr_c(2,6) = 'nu-mu'
      kmpr_l(2,6) = 5
* masses of external particles
      amas_1(2,1) = 11
      amas_1(2,2) = 11
      amas_1(2,3) = 11
      amas_1(2,4) = 12
      amas_1(2,5) = 13
      amas_1(2,6) = 14

* Charge*3
      kcha_g(2,1) =  -3
      kcha_g(2,2) =   3
      kcha_g(2,3) =  -3
      kcha_g(2,4) =   0
      kcha_g(2,5) =   3
      kcha_g(2,6) =   0

* KFcode
      kfco_e(2,1) =  11
      kfco_e(2,2) = -11
      kfco_e(2,3) =  11
      kfco_e(2,4) = -12
      kfco_e(2,5) = -13
      kfco_e(2,6) =  14
* weight for the color base (llll)
      cf_mtx(2,0,0) = 1.d0

      kmcb_s(2)    = 1
      kmcb_x(2)    = 0

* particle name
      kmpr_c(3,1) = 'electron'
      kmpr_l(3,1) = 8
      kmpr_c(3,2) = 'positron'
      kmpr_l(3,2) = 8
      kmpr_c(3,3) = 'electron'
      kmpr_l(3,3) = 8
      kmpr_c(3,4) = 'nu-e-bar'
      kmpr_l(3,4) = 8
      kmpr_c(3,5) = 'anti-tau'
      kmpr_l(3,5) = 8
      kmpr_c(3,6) = 'nu-tau'
      kmpr_l(3,6) = 6
* masses of external particles
      amas_1(3,1) = 11
      amas_1(3,2) = 11
      amas_1(3,3) = 11
      amas_1(3,4) = 12
      amas_1(3,5) = 15
      amas_1(3,6) = 16

* Charge*3
      kcha_g(3,1) =  -3
      kcha_g(3,2) =   3
      kcha_g(3,3) =  -3
      kcha_g(3,4) =   0
      kcha_g(3,5) =   3
      kcha_g(3,6) =   0

* KFcode
      kfco_e(3,1) =  11
      kfco_e(3,2) = -11
      kfco_e(3,3) =  11
      kfco_e(3,4) = -12
      kfco_e(3,5) = -15
      kfco_e(3,6) =  16
* weight for the color base (llll)
      cf_mtx(3,0,0) = 1.d0

      kmcb_s(3)    = 1
      kmcb_x(3)    = 0

* particle name
      kmpr_c(4,1) = 'electron'
      kmpr_l(4,1) = 8
      kmpr_c(4,2) = 'positron'
      kmpr_l(4,2) = 8
      kmpr_c(4,3) = 'nu-mu'
      kmpr_l(4,3) = 5
      kmpr_c(4,4) = 'anti-muon'
      kmpr_l(4,4) = 9
      kmpr_c(4,5) = 'muon'
      kmpr_l(4,5) = 4
      kmpr_c(4,6) = 'nu-mu-bar'
      kmpr_l(4,6) = 9
* masses of external particles
      amas_1(4,1) = 11
      amas_1(4,2) = 11
      amas_1(4,3) = 14
      amas_1(4,4) = 13
      amas_1(4,5) = 13
      amas_1(4,6) = 14

* Charge*3
      kcha_g(4,1) =  -3
      kcha_g(4,2) =   3
      kcha_g(4,3) =   0
      kcha_g(4,4) =   3
      kcha_g(4,5) =  -3
      kcha_g(4,6) =   0

* KFcode
      kfco_e(4,1) =  11
      kfco_e(4,2) = -11
      kfco_e(4,3) =  14
      kfco_e(4,4) = -13
      kfco_e(4,5) =  13
      kfco_e(4,6) = -14
* weight for the color base (llll)
      cf_mtx(4,0,0) = 1.d0

      kmcb_s(4)    = 1
      kmcb_x(4)    = 0

* particle name
      kmpr_c(5,1) = 'electron'
      kmpr_l(5,1) = 8
      kmpr_c(5,2) = 'positron'
      kmpr_l(5,2) = 8
      kmpr_c(5,3) = 'nu-tau'
      kmpr_l(5,3) = 6
      kmpr_c(5,4) = 'anti-tau'
      kmpr_l(5,4) = 8
      kmpr_c(5,5) = 'tau'
      kmpr_l(5,5) = 3
      kmpr_c(5,6) = 'nu-tau-bar'
      kmpr_l(5,6) = 10
* masses of external particles
      amas_1(5,1) = 11
      amas_1(5,2) = 11
      amas_1(5,3) = 16
      amas_1(5,4) = 15
      amas_1(5,5) = 15
      amas_1(5,6) = 16

* Charge*3
      kcha_g(5,1) =  -3
      kcha_g(5,2) =   3
      kcha_g(5,3) =   0
      kcha_g(5,4) =   3
      kcha_g(5,5) =  -3
      kcha_g(5,6) =   0

* KFcode
      kfco_e(5,1) =  11
      kfco_e(5,2) = -11
      kfco_e(5,3) =  16
      kfco_e(5,4) = -15
      kfco_e(5,5) =  15
      kfco_e(5,6) = -16
* weight for the color base (llll)
      cf_mtx(5,0,0) = 1.d0

      kmcb_s(5)    = 1
      kmcb_x(5)    = 0

* particle name
      kmpr_c(6,1) = 'electron'
      kmpr_l(6,1) = 8
      kmpr_c(6,2) = 'positron'
      kmpr_l(6,2) = 8
      kmpr_c(6,3) = 'muon'
      kmpr_l(6,3) = 4
      kmpr_c(6,4) = 'nu-mu-bar'
      kmpr_l(6,4) = 9
      kmpr_c(6,5) = 'anti-tau'
      kmpr_l(6,5) = 8
      kmpr_c(6,6) = 'nu-tau'
      kmpr_l(6,6) = 6
* masses of external particles
      amas_1(6,1) = 11
      amas_1(6,2) = 11
      amas_1(6,3) = 13
      amas_1(6,4) = 14
      amas_1(6,5) = 15
      amas_1(6,6) = 16

* Charge*3
      kcha_g(6,1) =  -3
      kcha_g(6,2) =   3
      kcha_g(6,3) =  -3
      kcha_g(6,4) =   0
      kcha_g(6,5) =   3
      kcha_g(6,6) =   0

* KFcode
      kfco_e(6,1) =  11
      kfco_e(6,2) = -11
      kfco_e(6,3) =  13
      kfco_e(6,4) = -14
      kfco_e(6,5) = -15
      kfco_e(6,6) =  16
* weight for the color base (llll)
      cf_mtx(6,0,0) = 1.d0

      kmcb_s(6)    = 1
      kmcb_x(6)    = 0

* particle name
      kmpr_c(7,1) = 'electron'
      kmpr_l(7,1) = 8
      kmpr_c(7,2) = 'positron'
      kmpr_l(7,2) = 8
      kmpr_c(7,3) = 'electron'
      kmpr_l(7,3) = 8
      kmpr_c(7,4) = 'positron'
      kmpr_l(7,4) = 8
      kmpr_c(7,5) = 'electron'
      kmpr_l(7,5) = 8
      kmpr_c(7,6) = 'positron'
      kmpr_l(7,6) = 8
* masses of external particles
      amas_1(7,1) = 11
      amas_1(7,2) = 11
      amas_1(7,3) = 11
      amas_1(7,4) = 11
      amas_1(7,5) = 11
      amas_1(7,6) = 11

* Charge*3
      kcha_g(7,1) =  -3
      kcha_g(7,2) =   3
      kcha_g(7,3) =  -3
      kcha_g(7,4) =   3
      kcha_g(7,5) =  -3
      kcha_g(7,6) =   3

* KFcode
      kfco_e(7,1) =  11
      kfco_e(7,2) = -11
      kfco_e(7,3) =  11
      kfco_e(7,4) = -11
      kfco_e(7,5) =  11
      kfco_e(7,6) = -11
* weight for the color base (llll)
      cf_mtx(7,0,0) = 1.d0

      kmcb_s(7)    = 1
      kmcb_x(7)    = 0

* particle name
      kmpr_c(8,1) = 'electron'
      kmpr_l(8,1) = 8
      kmpr_c(8,2) = 'positron'
      kmpr_l(8,2) = 8
      kmpr_c(8,3) = 'electron'
      kmpr_l(8,3) = 8
      kmpr_c(8,4) = 'positron'
      kmpr_l(8,4) = 8
      kmpr_c(8,5) = 'muon'
      kmpr_l(8,5) = 4
      kmpr_c(8,6) = 'anti-muon'
      kmpr_l(8,6) = 9
* masses of external particles
      amas_1(8,1) = 11
      amas_1(8,2) = 11
      amas_1(8,3) = 11
      amas_1(8,4) = 11
      amas_1(8,5) = 13
      amas_1(8,6) = 13

* Charge*3
      kcha_g(8,1) =  -3
      kcha_g(8,2) =   3
      kcha_g(8,3) =  -3
      kcha_g(8,4) =   3
      kcha_g(8,5) =  -3
      kcha_g(8,6) =   3

* KFcode
      kfco_e(8,1) =  11
      kfco_e(8,2) = -11
      kfco_e(8,3) =  11
      kfco_e(8,4) = -11
      kfco_e(8,5) =  13
      kfco_e(8,6) = -13
* weight for the color base (llll)
      cf_mtx(8,0,0) = 1.d0

      kmcb_s(8)    = 1
      kmcb_x(8)    = 0

* particle name
      kmpr_c(9,1) = 'electron'
      kmpr_l(9,1) = 8
      kmpr_c(9,2) = 'positron'
      kmpr_l(9,2) = 8
      kmpr_c(9,3) = 'electron'
      kmpr_l(9,3) = 8
      kmpr_c(9,4) = 'positron'
      kmpr_l(9,4) = 8
      kmpr_c(9,5) = 'tau'
      kmpr_l(9,5) = 3
      kmpr_c(9,6) = 'anti-tau'
      kmpr_l(9,6) = 8
* masses of external particles
      amas_1(9,1) = 11
      amas_1(9,2) = 11
      amas_1(9,3) = 11
      amas_1(9,4) = 11
      amas_1(9,5) = 15
      amas_1(9,6) = 15

* Charge*3
      kcha_g(9,1) =  -3
      kcha_g(9,2) =   3
      kcha_g(9,3) =  -3
      kcha_g(9,4) =   3
      kcha_g(9,5) =  -3
      kcha_g(9,6) =   3

* KFcode
      kfco_e(9,1) =  11
      kfco_e(9,2) = -11
      kfco_e(9,3) =  11
      kfco_e(9,4) = -11
      kfco_e(9,5) =  15
      kfco_e(9,6) = -15
* weight for the color base (llll)
      cf_mtx(9,0,0) = 1.d0

      kmcb_s(9)    = 1
      kmcb_x(9)    = 0
      
* particle name
      kmpr_c(10,1) = 'electron'
      kmpr_l(10,1) = 8
      kmpr_c(10,2) = 'positron'
      kmpr_l(10,2) = 8
      kmpr_c(10,3) = 'muon'
      kmpr_l(10,3) = 4
      kmpr_c(10,4) = 'anti-muon'
      kmpr_l(10,4) = 9
      kmpr_c(10,5) = 'muon'
      kmpr_l(10,5) = 4
      kmpr_c(10,6) = 'anti-muon'
      kmpr_l(10,6) = 9
* masses of external particles
      amas_1(10,1) = 11
      amas_1(10,2) = 11
      amas_1(10,3) = 13
      amas_1(10,4) = 13
      amas_1(10,5) = 13
      amas_1(10,6) = 13

* Charge*3
      kcha_g(10,1) =  -3
      kcha_g(10,2) =   3
      kcha_g(10,3) =  -3
      kcha_g(10,4) =   3
      kcha_g(10,5) =  -3
      kcha_g(10,6) =   3

* KFcode
      kfco_e(10,1) =  11
      kfco_e(10,2) = -11
      kfco_e(10,3) =  13
      kfco_e(10,4) = -13
      kfco_e(10,5) =  13
      kfco_e(10,6) = -13
* weight for the color base (llll)
      cf_mtx(10,0,0) = 1.d0

      kmcb_s(10)    = 1
      kmcb_x(10)    = 0

* particle name
      kmpr_c(11,1) = 'electron'
      kmpr_l(11,1) = 8
      kmpr_c(11,2) = 'positron'
      kmpr_l(11,2) = 8
      kmpr_c(11,3) = 'tau'
      kmpr_l(11,3) = 3
      kmpr_c(11,4) = 'anti-tau'
      kmpr_l(11,4) = 8
      kmpr_c(11,5) = 'tau'
      kmpr_l(11,5) = 3
      kmpr_c(11,6) = 'anti-tau'
      kmpr_l(11,6) = 8
* masses of external particles
      amas_1(11,1) = 11
      amas_1(11,2) = 11
      amas_1(11,3) = 15
      amas_1(11,4) = 15
      amas_1(11,5) = 15
      amas_1(11,6) = 15

* Charge*3
      kcha_g(11,1) =  -3
      kcha_g(11,2) =   3
      kcha_g(11,3) =  -3
      kcha_g(11,4) =   3
      kcha_g(11,5) =  -3
      kcha_g(11,6) =   3

* KFcode
      kfco_e(11,1) =  11
      kfco_e(11,2) = -11
      kfco_e(11,3) =  15
      kfco_e(11,4) = -15
      kfco_e(11,5) =  15
      kfco_e(11,6) = -15
* weight for the color base (llll)
      cf_mtx(11,0,0) = 1.d0

      kmcb_s(11)    = 1
      kmcb_x(11)    = 0

* particle name
      kmpr_c(12,1) = 'electron'
      kmpr_l(12,1) = 8
      kmpr_c(12,2) = 'positron'
      kmpr_l(12,2) = 8
      kmpr_c(12,3) = 'muon'
      kmpr_l(12,3) = 4
      kmpr_c(12,4) = 'anti-muon'
      kmpr_l(12,4) = 9
      kmpr_c(12,5) = 'tau'
      kmpr_l(12,5) = 3
      kmpr_c(12,6) = 'anti-tau'
      kmpr_l(12,6) = 8
* masses of external particles
      amas_1(12,1) = 11
      amas_1(12,2) = 11
      amas_1(12,3) = 13
      amas_1(12,4) = 13
      amas_1(12,5) = 15
      amas_1(12,6) = 15

* Charge*3
      kcha_g(12,1) =  -3
      kcha_g(12,2) =   3
      kcha_g(12,3) =  -3
      kcha_g(12,4) =   3
      kcha_g(12,5) =  -3
      kcha_g(12,6) =   3

* KFcode
      kfco_e(12,1) =  11
      kfco_e(12,2) = -11
      kfco_e(12,3) =  13
      kfco_e(12,4) = -13
      kfco_e(12,5) =  15
      kfco_e(12,6) = -15
* weight for the color base (llll)
      cf_mtx(12,0,0) = 1.d0

      kmcb_s(12)    = 1
      kmcb_x(12)    = 0

* particle name
      kmpr_c(13,1) = 'electron'
      kmpr_l(13,1) = 8
      kmpr_c(13,2) = 'positron'
      kmpr_l(13,2) = 8
      kmpr_c(13,3) = 'electron'
      kmpr_l(13,3) = 8
      kmpr_c(13,4) = 'positron'
      kmpr_l(13,4) = 8
      kmpr_c(13,5) = 'nu-mu'
      kmpr_l(13,5) = 5
      kmpr_c(13,6) = 'nu-mu-bar'
      kmpr_l(13,6) = 9
* masses of external particles
      amas_1(13,1) = 11
      amas_1(13,2) = 11
      amas_1(13,3) = 11
      amas_1(13,4) = 11
      amas_1(13,5) = 14
      amas_1(13,6) = 14

* Charge*3
      kcha_g(13,1) =  -3
      kcha_g(13,2) =   3
      kcha_g(13,3) =  -3
      kcha_g(13,4) =   3
      kcha_g(13,5) =   0
      kcha_g(13,6) =   0

* KFcode
      kfco_e(13,1) =  11
      kfco_e(13,2) = -11
      kfco_e(13,3) =  11
      kfco_e(13,4) = -11
      kfco_e(13,5) =  14
      kfco_e(13,6) = -14
* weight for the color base (llll)
      cf_mtx(13,0,0) = 1.d0

      kmcb_s(13)    = 1
      kmcb_x(13)    = 0

* particle name
      kmpr_c(14,1) = 'electron'
      kmpr_l(14,1) = 8
      kmpr_c(14,2) = 'positron'
      kmpr_l(14,2) = 8
      kmpr_c(14,3) = 'electron'
      kmpr_l(14,3) = 8
      kmpr_c(14,4) = 'positron'
      kmpr_l(14,4) = 8
      kmpr_c(14,5) = 'nu-tau'
      kmpr_l(14,5) = 6
      kmpr_c(14,6) = 'nu-tau-bar'
      kmpr_l(14,6) = 10
* masses of external particles
      amas_1(14,1) = 11
      amas_1(14,2) = 11
      amas_1(14,3) = 11
      amas_1(14,4) = 11
      amas_1(14,5) = 16
      amas_1(14,6) = 16

* Charge*3
      kcha_g(14,1) =  -3
      kcha_g(14,2) =   3
      kcha_g(14,3) =  -3
      kcha_g(14,4) =   3
      kcha_g(14,5) =   0
      kcha_g(14,6) =   0

* KFcode
      kfco_e(14,1) =  11
      kfco_e(14,2) = -11
      kfco_e(14,3) =  11
      kfco_e(14,4) = -11
      kfco_e(14,5) =  16
      kfco_e(14,6) = -16
* weight for the color base (llll)
      cf_mtx(14,0,0) = 1.d0

      kmcb_s(14)    = 1
      kmcb_x(14)    = 0

* particle name
      kmpr_c(15,1) = 'electron'
      kmpr_l(15,1) = 8
      kmpr_c(15,2) = 'positron'
      kmpr_l(15,2) = 8
      kmpr_c(15,3) = 'nu-e'
      kmpr_l(15,3) = 4
      kmpr_c(15,4) = 'nu-e-bar'
      kmpr_l(15,4) = 8
      kmpr_c(15,5) = 'muon'
      kmpr_l(15,5) = 4
      kmpr_c(15,6) = 'anti-muon'
      kmpr_l(15,6) = 9
* masses of external particles
      amas_1(15,1) = 11
      amas_1(15,2) = 11
      amas_1(15,3) = 12
      amas_1(15,4) = 12
      amas_1(15,5) = 13
      amas_1(15,6) = 13

* Charge*3
      kcha_g(15,1) =  -3
      kcha_g(15,2) =   3
      kcha_g(15,3) =   0
      kcha_g(15,4) =   0
      kcha_g(15,5) =  -3
      kcha_g(15,6) =   3

* KFcode
      kfco_e(15,1) =  11
      kfco_e(15,2) = -11
      kfco_e(15,3) =  12
      kfco_e(15,4) = -12
      kfco_e(15,5) =  13
      kfco_e(15,6) = -13
* weight for the color base (llll)
      cf_mtx(15,0,0) = 1.d0

      kmcb_s(15)    = 1
      kmcb_x(15)    = 0

* particle name
      kmpr_c(16,1) = 'electron'
      kmpr_l(16,1) = 8
      kmpr_c(16,2) = 'positron'
      kmpr_l(16,2) = 8
      kmpr_c(16,3) = 'nu-e'
      kmpr_l(16,3) = 4
      kmpr_c(16,4) = 'nu-e-bar'
      kmpr_l(16,4) = 8
      kmpr_c(16,5) = 'tau'
      kmpr_l(16,5) = 3
      kmpr_c(16,6) = 'anti-tau'
      kmpr_l(16,6) = 8
* masses of external particles
      amas_1(16,1) = 11
      amas_1(16,2) = 11
      amas_1(16,3) = 12
      amas_1(16,4) = 12
      amas_1(16,5) = 15
      amas_1(16,6) = 15

* Charge*3
      kcha_g(16,1) =  -3
      kcha_g(16,2) =   3
      kcha_g(16,3) =   0
      kcha_g(16,4) =   0
      kcha_g(16,5) =  -3
      kcha_g(16,6) =   3

* KFcode
      kfco_e(16,1) =  11
      kfco_e(16,2) = -11
      kfco_e(16,3) =  12
      kfco_e(16,4) = -12
      kfco_e(16,5) =  15
      kfco_e(16,6) = -15
* weight for the color base (llll)
      cf_mtx(16,0,0) = 1.d0

      kmcb_s(16)    = 1
      kmcb_x(16)    = 0

* particle name
      kmpr_c(17,1) = 'electron'
      kmpr_l(17,1) = 8
      kmpr_c(17,2) = 'positron'
      kmpr_l(17,2) = 8
      kmpr_c(17,3) = 'nu-tau'
      kmpr_l(17,3) = 6
      kmpr_c(17,4) = 'nu-tau-bar'
      kmpr_l(17,4) = 10
      kmpr_c(17,5) = 'muon'
      kmpr_l(17,5) = 4
      kmpr_c(17,6) = 'anti-muon'
      kmpr_l(17,6) = 9
* masses of external particles
      amas_1(17,1) = 11
      amas_1(17,2) = 11
      amas_1(17,3) = 16
      amas_1(17,4) = 16
      amas_1(17,5) = 13
      amas_1(17,6) = 13

* Charge*3
      kcha_g(17,1) =  -3
      kcha_g(17,2) =   3
      kcha_g(17,3) =   0
      kcha_g(17,4) =   0
      kcha_g(17,5) =  -3
      kcha_g(17,6) =   3

* KFcode
      kfco_e(17,1) =  11
      kfco_e(17,2) = -11
      kfco_e(17,3) =  16
      kfco_e(17,4) = -16
      kfco_e(17,5) =  13
      kfco_e(17,6) = -13
* weight for the color base (llll)
      cf_mtx(17,0,0) = 1.d0

      kmcb_s(17)    = 1
      kmcb_x(17)    = 0

* particle name
      kmpr_c(18,1) = 'electron'
      kmpr_l(18,1) = 8
      kmpr_c(18,2) = 'positron'
      kmpr_l(18,2) = 8
      kmpr_c(18,3) = 'nu-mu'
      kmpr_l(18,3) = 5
      kmpr_c(18,4) = 'nu-mu-bar'
      kmpr_l(18,4) = 9
      kmpr_c(18,5) = 'tau'
      kmpr_l(18,5) = 3
      kmpr_c(18,6) = 'anti-tau'
      kmpr_l(18,6) = 8
* masses of external particles
      amas_1(18,1) = 11
      amas_1(18,2) = 11
      amas_1(18,3) = 14
      amas_1(18,4) = 14
      amas_1(18,5) = 15
      amas_1(18,6) = 15

* Charge*3
      kcha_g(18,1) =  -3
      kcha_g(18,2) =   3
      kcha_g(18,3) =   0
      kcha_g(18,4) =   0
      kcha_g(18,5) =  -3
      kcha_g(18,6) =   3

* KFcode
      kfco_e(18,1) =  11
      kfco_e(18,2) = -11
      kfco_e(18,3) =  14
      kfco_e(18,4) = -14
      kfco_e(18,5) =  15
      kfco_e(18,6) = -15
* weight for the color base (llll)
      cf_mtx(18,0,0) = 1.d0

      kmcb_s(18)    = 1
      kmcb_x(18)    = 0

* particle name
      kmpr_c(19,1) = 'electron'
      kmpr_l(19,1) = 8
      kmpr_c(19,2) = 'positron'
      kmpr_l(19,2) = 8
      kmpr_c(19,3) = 'nu-e'
      kmpr_l(19,3) = 4
      kmpr_c(19,4) = 'nu-e-bar'
      kmpr_l(19,4) = 8
      kmpr_c(19,5) = 'nu-e'
      kmpr_l(19,5) = 4
      kmpr_c(19,6) = 'nu-e-bar'
      kmpr_l(19,6) = 8
* masses of external particles
      amas_1(19,1) = 11
      amas_1(19,2) = 11
      amas_1(19,3) = 12
      amas_1(19,4) = 12
      amas_1(19,5) = 12
      amas_1(19,6) = 12

* Charge*3
      kcha_g(19,1) =  -3
      kcha_g(19,2) =   3
      kcha_g(19,3) =   0
      kcha_g(19,4) =   0
      kcha_g(19,5) =   0
      kcha_g(19,6) =   0

* KFcode
      kfco_e(19,1) =  11
      kfco_e(19,2) = -11
      kfco_e(19,3) =  12
      kfco_e(19,4) = -12
      kfco_e(19,5) =  12
      kfco_e(19,6) = -12
* weight for the color base (llll)
      cf_mtx(19,0,0) = 1.d0

      kmcb_s(19)    = 1
      kmcb_x(19)    = 0

* particle name
      kmpr_c(20,1) = 'electron'
      kmpr_l(20,1) = 8
      kmpr_c(20,2) = 'positron'
      kmpr_l(20,2) = 8
      kmpr_c(20,3) = 'nu-e'
      kmpr_l(20,3) = 4
      kmpr_c(20,4) = 'nu-e-bar'
      kmpr_l(20,4) = 8
      kmpr_c(20,5) = 'nu-mu'
      kmpr_l(20,5) = 5
      kmpr_c(20,6) = 'nu-mu-bar'
      kmpr_l(20,6) = 9
* masses of external particles
      amas_1(20,1) = 11
      amas_1(20,2) = 11
      amas_1(20,3) = 12
      amas_1(20,4) = 12
      amas_1(20,5) = 14
      amas_1(20,6) = 14

* Charge*3
      kcha_g(20,1) =  -3
      kcha_g(20,2) =   3
      kcha_g(20,3) =   0
      kcha_g(20,4) =   0
      kcha_g(20,5) =   0
      kcha_g(20,6) =   0

* KFcode
      kfco_e(20,1) =  11
      kfco_e(20,2) = -11
      kfco_e(20,3) =  12
      kfco_e(20,4) = -12
      kfco_e(20,5) =  14
      kfco_e(20,6) = -14
* weight for the color base (llll)
      cf_mtx(20,0,0) = 1.d0

      kmcb_s(20)    = 1
      kmcb_x(20)    = 0

* particle name
      kmpr_c(21,1) = 'electron'
      kmpr_l(21,1) = 8
      kmpr_c(21,2) = 'positron'
      kmpr_l(21,2) = 8
      kmpr_c(21,3) = 'nu-e'
      kmpr_l(21,3) = 4
      kmpr_c(21,4) = 'nu-e-bar'
      kmpr_l(21,4) = 8
      kmpr_c(21,5) = 'nu-tau'
      kmpr_l(21,5) = 6
      kmpr_c(21,6) = 'nu-tau-bar'
      kmpr_l(21,6) = 10
* masses of external particles
      amas_1(21,1) = 11
      amas_1(21,2) = 11
      amas_1(21,3) = 12
      amas_1(21,4) = 12
      amas_1(21,5) = 16
      amas_1(21,6) = 16

* Charge*3
      kcha_g(21,1) =  -3
      kcha_g(21,2) =   3
      kcha_g(21,3) =   0
      kcha_g(21,4) =   0
      kcha_g(21,5) =   0
      kcha_g(21,6) =   0

* KFcode
      kfco_e(21,1) =  11
      kfco_e(21,2) = -11
      kfco_e(21,3) =  12
      kfco_e(21,4) = -12
      kfco_e(21,5) =  16
      kfco_e(21,6) = -16
* weight for the color base (llll)
      cf_mtx(21,0,0) = 1.d0

      kmcb_s(21)    = 1
      kmcb_x(21)    = 0

* particle name
      kmpr_c(22,1) = 'electron'
      kmpr_l(22,1) = 8
      kmpr_c(22,2) = 'positron'
      kmpr_l(22,2) = 8
      kmpr_c(22,3) = 'nu-mu'
      kmpr_l(22,3) = 5
      kmpr_c(22,4) = 'nu-mu-bar'
      kmpr_l(22,4) = 9
      kmpr_c(22,5) = 'nu-mu'
      kmpr_l(22,5) = 5
      kmpr_c(22,6) = 'nu-mu-bar'
      kmpr_l(22,6) = 9
* masses of external particles
      amas_1(22,1) = 11
      amas_1(22,2) = 11
      amas_1(22,3) = 14
      amas_1(22,4) = 14
      amas_1(22,5) = 14
      amas_1(22,6) = 14

* Charge*3
      kcha_g(22,1) =  -3
      kcha_g(22,2) =   3
      kcha_g(22,3) =   0
      kcha_g(22,4) =   0
      kcha_g(22,5) =   0
      kcha_g(22,6) =   0

* KFcode
      kfco_e(22,1) =  11
      kfco_e(22,2) = -11
      kfco_e(22,3) =  14
      kfco_e(22,4) = -14
      kfco_e(22,5) =  14
      kfco_e(22,6) = -14
* weight for the color base (llll)
      cf_mtx(22,0,0) = 1.d0

      kmcb_s(22)    = 1
      kmcb_x(22)    = 0

* particle name
      kmpr_c(23,1) = 'electron'
      kmpr_l(23,1) = 8
      kmpr_c(23,2) = 'positron'
      kmpr_l(23,2) = 8
      kmpr_c(23,3) = 'nu-tau'
      kmpr_l(23,3) = 6
      kmpr_c(23,4) = 'nu-tau-bar'
      kmpr_l(23,4) = 10
      kmpr_c(23,5) = 'nu-tau'
      kmpr_l(23,5) = 6
      kmpr_c(23,6) = 'nu-tau-bar'
      kmpr_l(23,6) = 10
* masses of external particles
      amas_1(23,1) = 11
      amas_1(23,2) = 11
      amas_1(23,3) = 16
      amas_1(23,4) = 16
      amas_1(23,5) = 16
      amas_1(23,6) = 16

* Charge*3
      kcha_g(23,1) =  -3
      kcha_g(23,2) =   3
      kcha_g(23,3) =   0
      kcha_g(23,4) =   0
      kcha_g(23,5) =   0
      kcha_g(23,6) =   0

* KFcode
      kfco_e(23,1) =  11
      kfco_e(23,2) = -11
      kfco_e(23,3) =  16
      kfco_e(23,4) = -16
      kfco_e(23,5) =  16
      kfco_e(23,6) = -16
* weight for the color base (llll)
      cf_mtx(23,0,0) = 1.d0

      kmcb_s(23)    = 1
      kmcb_x(23)    = 0

* particle name
      kmpr_c(24,1) = 'electron'
      kmpr_l(24,1) = 8
      kmpr_c(24,2) = 'positron'
      kmpr_l(24,2) = 8
      kmpr_c(24,3) = 'nu-tau'
      kmpr_l(24,3) = 6
      kmpr_c(24,4) = 'nu-tau-bar'
      kmpr_l(24,4) = 10
      kmpr_c(24,5) = 'nu-mu'
      kmpr_l(24,5) = 5
      kmpr_c(24,6) = 'nu-mu-bar'
      kmpr_l(24,6) = 9
* masses of external particles
      amas_1(24,1) = 11
      amas_1(24,2) = 11
      amas_1(24,3) = 16
      amas_1(24,4) = 16
      amas_1(24,5) = 14
      amas_1(24,6) = 14

* Charge*3
      kcha_g(24,1) =  -3
      kcha_g(24,2) =   3
      kcha_g(24,3) =   0
      kcha_g(24,4) =   0
      kcha_g(24,5) =   0
      kcha_g(24,6) =   0

* KFcode
      kfco_e(24,1) =  11
      kfco_e(24,2) = -11
      kfco_e(24,3) =  16
      kfco_e(24,4) = -16
      kfco_e(24,5) =  14
      kfco_e(24,6) = -14
* weight for the color base (llll)
      cf_mtx(24,0,0) = 1.d0

      kmcb_s(24)    = 1
      kmcb_x(24)    = 0

* particle name
      kmpr_c(25,1) = 'electron'
      kmpr_l(25,1) = 8
      kmpr_c(25,2) = 'positron'
      kmpr_l(25,2) = 8
      kmpr_c(25,3) = 'electron'
      kmpr_l(25,3) = 8
      kmpr_c(25,4) = 'nu-e-bar'
      kmpr_l(25,4) = 8
      kmpr_c(25,5) = 'u'
      kmpr_l(25,5) = 1
      kmpr_c(25,6) = 'd-bar'
      kmpr_l(25,6) = 5
* masses of external particles
      amas_1(25,1) = 11
      amas_1(25,2) = 11
      amas_1(25,3) = 11
      amas_1(25,4) = 12
      amas_1(25,5) = 2
      amas_1(25,6) = 1

* Charge*3
      kcha_g(25,1) =  -3
      kcha_g(25,2) =   3
      kcha_g(25,3) =  -3
      kcha_g(25,4) =   0
      kcha_g(25,5) =   2
      kcha_g(25,6) =   1

* KFcode
      kfco_e(25,1) =  11
      kfco_e(25,2) = -11
      kfco_e(25,3) =  11
      kfco_e(25,4) = -12
      kfco_e(25,5) =   2
      kfco_e(25,6) =  -1
* weight for the color base (llqq)
      cf_mtx(25,0,0) = 3.d0

      kmcb_s(25)    = 1
      kmcb_x(25)    = 2
      kmcs_r(25,1,0) = 5
      kmcs_r(25,2,0) = 6

* particle name
      kmpr_c(26,1) = 'electron'
      kmpr_l(26,1) = 8
      kmpr_c(26,2) = 'positron'
      kmpr_l(26,2) = 8
      kmpr_c(26,3) = 'electron'
      kmpr_l(26,3) = 8
      kmpr_c(26,4) = 'nu-e-bar'
      kmpr_l(26,4) = 8
      kmpr_c(26,5) = 'c'
      kmpr_l(26,5) = 1
      kmpr_c(26,6) = 's-bar'
      kmpr_l(26,6) = 5
* masses of external particles
      amas_1(26,1) = 11
      amas_1(26,2) = 11
      amas_1(26,3) = 11
      amas_1(26,4) = 12
      amas_1(26,5) = 4
      amas_1(26,6) = 3

* Charge*3
      kcha_g(26,1) =  -3
      kcha_g(26,2) =   3
      kcha_g(26,3) =  -3
      kcha_g(26,4) =   0
      kcha_g(26,5) =   2
      kcha_g(26,6) =   1

* KFcode
      kfco_e(26,1) =  11
      kfco_e(26,2) = -11
      kfco_e(26,3) =  11
      kfco_e(26,4) = -12
      kfco_e(26,5) =   4
      kfco_e(26,6) =  -3
* weight for the color base (llqq)
      cf_mtx(26,0,0) = 3.d0

      kmcb_s(26)    = 1
      kmcb_x(26)    = 2
      kmcs_r(26,1,0) = 5
      kmcs_r(26,2,0) = 6

* particle name
      kmpr_c(27,1) = 'electron'
      kmpr_l(27,1) = 8
      kmpr_c(27,2) = 'positron'
      kmpr_l(27,2) = 8
      kmpr_c(27,3) = 'muon'
      kmpr_l(27,3) = 4
      kmpr_c(27,4) = 'nu-mu-bar'
      kmpr_l(27,4) = 9
      kmpr_c(27,5) = 'u'
      kmpr_l(27,5) = 1
      kmpr_c(27,6) = 'd-bar'
      kmpr_l(27,6) = 5
* masses of external particles
      amas_1(27,1) = 11
      amas_1(27,2) = 11
      amas_1(27,3) = 13
      amas_1(27,4) = 14
      amas_1(27,5) = 2
      amas_1(27,6) = 1

* Charge*3
      kcha_g(27,1) =  -3
      kcha_g(27,2) =   3
      kcha_g(27,3) =  -3
      kcha_g(27,4) =   0
      kcha_g(27,5) =   2
      kcha_g(27,6) =   1

* KFcode
      kfco_e(27,1) =  11
      kfco_e(27,2) = -11
      kfco_e(27,3) =  13
      kfco_e(27,4) = -14
      kfco_e(27,5) =   2
      kfco_e(27,6) =  -1
* weight for the color base (llqq)
      cf_mtx(27,0,0) = 3.d0

      kmcb_s(27)    = 1
      kmcb_x(27)    = 2
      kmcs_r(27,1,0) = 5
      kmcs_r(27,2,0) = 6

* particle name
      kmpr_c(28,1) = 'electron'
      kmpr_l(28,1) = 8
      kmpr_c(28,2) = 'positron'
      kmpr_l(28,2) = 8
      kmpr_c(28,3) = 'muon'
      kmpr_l(28,3) = 4
      kmpr_c(28,4) = 'nu-mu-bar'
      kmpr_l(28,4) = 9
      kmpr_c(28,5) = 'c'
      kmpr_l(28,5) = 1
      kmpr_c(28,6) = 's-bar'
      kmpr_l(28,6) = 5
* masses of external particles
      amas_1(28,1) = 11
      amas_1(28,2) = 11
      amas_1(28,3) = 13
      amas_1(28,4) = 14
      amas_1(28,5) = 4
      amas_1(28,6) = 3

* Charge*3
      kcha_g(28,1) =  -3
      kcha_g(28,2) =   3
      kcha_g(28,3) =  -3
      kcha_g(28,4) =   0
      kcha_g(28,5) =   2
      kcha_g(28,6) =   1

* KFcode
      kfco_e(28,1) =  11
      kfco_e(28,2) = -11
      kfco_e(28,3) =  13
      kfco_e(28,4) = -14
      kfco_e(28,5) =   4
      kfco_e(28,6) =  -3
* weight for the color base (llqq)
      cf_mtx(28,0,0) = 3.d0

      kmcb_s(28)    = 1
      kmcb_x(28)    = 2
      kmcs_r(28,1,0) = 5
      kmcs_r(28,2,0) = 6

* particle name
      kmpr_c(29,1) = 'electron'
      kmpr_l(29,1) = 8
      kmpr_c(29,2) = 'positron'
      kmpr_l(29,2) = 8
      kmpr_c(29,3) = 'tau'
      kmpr_l(29,3) = 3
      kmpr_c(29,4) = 'nu-tau-bar'
      kmpr_l(29,4) = 10
      kmpr_c(29,5) = 'u'
      kmpr_l(29,5) = 1
      kmpr_c(29,6) = 'd-bar'
      kmpr_l(29,6) = 5
* masses of external particles
      amas_1(29,1) = 11
      amas_1(29,2) = 11
      amas_1(29,3) = 15
      amas_1(29,4) = 16
      amas_1(29,5) = 2
      amas_1(29,6) = 1

* Charge*3
      kcha_g(29,1) =  -3
      kcha_g(29,2) =   3
      kcha_g(29,3) =  -3
      kcha_g(29,4) =   0
      kcha_g(29,5) =   2
      kcha_g(29,6) =   1

* KFcode
      kfco_e(29,1) =  11
      kfco_e(29,2) = -11
      kfco_e(29,3) =  15
      kfco_e(29,4) = -16
      kfco_e(29,5) =   2
      kfco_e(29,6) =  -1
* weight for the color base (llqq)
      cf_mtx(29,0,0) = 3.d0

      kmcb_s(29)    = 1
      kmcb_x(29)    = 2
      kmcs_r(29,1,0) = 5
      kmcs_r(29,2,0) = 6

* particle name
      kmpr_c(30,1) = 'electron'
      kmpr_l(30,1) = 8
      kmpr_c(30,2) = 'positron'
      kmpr_l(30,2) = 8
      kmpr_c(30,3) = 'tau'
      kmpr_l(30,3) = 3
      kmpr_c(30,4) = 'nu-tau-bar'
      kmpr_l(30,4) = 10
      kmpr_c(30,5) = 'c'
      kmpr_l(30,5) = 1
      kmpr_c(30,6) = 's-bar'
      kmpr_l(30,6) = 5
* masses of external particles
      amas_1(30,1) = 11
      amas_1(30,2) = 11
      amas_1(30,3) = 15
      amas_1(30,4) = 16
      amas_1(30,5) = 4
      amas_1(30,6) = 3

* Charge*3
      kcha_g(30,1) =  -3
      kcha_g(30,2) =   3
      kcha_g(30,3) =  -3
      kcha_g(30,4) =   0
      kcha_g(30,5) =   2
      kcha_g(30,6) =   1

* KFcode
      kfco_e(30,1) =  11
      kfco_e(30,2) = -11
      kfco_e(30,3) =  15
      kfco_e(30,4) = -16
      kfco_e(30,5) =   4
      kfco_e(30,6) =  -3
* weight for the color base (llqq)
      cf_mtx(30,0,0) = 3.d0

      kmcb_s(30)    = 1
      kmcb_x(30)    = 2
      kmcs_r(30,1,0) = 5
      kmcs_r(30,2,0) = 6

* particle name
      kmpr_c(31,1) = 'electron'
      kmpr_l(31,1) = 8
      kmpr_c(31,2) = 'positron'
      kmpr_l(31,2) = 8
      kmpr_c(31,3) = 'electron'
      kmpr_l(31,3) = 8
      kmpr_c(31,4) = 'positron'
      kmpr_l(31,4) = 8
      kmpr_c(31,5) = 'u'
      kmpr_l(31,5) = 1
      kmpr_c(31,6) = 'u-bar'
      kmpr_l(31,6) = 5
* masses of external particles
      amas_1(31,1) = 11
      amas_1(31,2) = 11
      amas_1(31,3) = 11
      amas_1(31,4) = 11
      amas_1(31,5) = 2
      amas_1(31,6) = 2

* Charge*3
      kcha_g(31,1) =  -3
      kcha_g(31,2) =   3
      kcha_g(31,3) =  -3
      kcha_g(31,4) =   3
      kcha_g(31,5) =   2
      kcha_g(31,6) =  -2

* KFcode
      kfco_e(31,1) =  11
      kfco_e(31,2) = -11
      kfco_e(31,3) =  11
      kfco_e(31,4) = -11
      kfco_e(31,5) =   2
      kfco_e(31,6) =  -2
* weight for the color base (llqq)
      cf_mtx(31,0,0) = 3.d0

      kmcb_s(31)    = 1
      kmcb_x(31)    = 2
      kmcs_r(31,1,0) = 5
      kmcs_r(31,2,0) = 6

* particle name
      kmpr_c(32,1) = 'electron'
      kmpr_l(32,1) = 8
      kmpr_c(32,2) = 'positron'
      kmpr_l(32,2) = 8
      kmpr_c(32,3) = 'electron'
      kmpr_l(32,3) = 8
      kmpr_c(32,4) = 'positron'
      kmpr_l(32,4) = 8
      kmpr_c(32,5) = 'c'
      kmpr_l(32,5) = 1
      kmpr_c(32,6) = 'c-bar'
      kmpr_l(32,6) = 5
* masses of external particles
      amas_1(32,1) = 11
      amas_1(32,2) = 11
      amas_1(32,3) = 11
      amas_1(32,4) = 11
      amas_1(32,5) = 4
      amas_1(32,6) = 4

* Charge*3
      kcha_g(32,1) =  -3
      kcha_g(32,2) =   3
      kcha_g(32,3) =  -3
      kcha_g(32,4) =   3
      kcha_g(32,5) =   2
      kcha_g(32,6) =  -2

* KFcode
      kfco_e(32,1) =  11
      kfco_e(32,2) = -11
      kfco_e(32,3) =  11
      kfco_e(32,4) = -11
      kfco_e(32,5) =   4
      kfco_e(32,6) =  -4
* weight for the color base (llqq)
      cf_mtx(32,0,0) = 3.d0

      kmcb_s(32)    = 1
      kmcb_x(32)    = 2
      kmcs_r(32,1,0) = 5
      kmcs_r(32,2,0) = 6

* particle name
      kmpr_c(33,1) = 'electron'
      kmpr_l(33,1) = 8
      kmpr_c(33,2) = 'positron'
      kmpr_l(33,2) = 8
      kmpr_c(33,3) = 'electron'
      kmpr_l(33,3) = 8
      kmpr_c(33,4) = 'positron'
      kmpr_l(33,4) = 8
      kmpr_c(33,5) = 'd'
      kmpr_l(33,5) = 1
      kmpr_c(33,6) = 'd-bar'
      kmpr_l(33,6) = 5
* masses of external particles
      amas_1(33,1) = 11
      amas_1(33,2) = 11
      amas_1(33,3) = 11
      amas_1(33,4) = 11
      amas_1(33,5) = 1
      amas_1(33,6) = 1

* Charge*3
      kcha_g(33,1) =  -3
      kcha_g(33,2) =   3
      kcha_g(33,3) =  -3
      kcha_g(33,4) =   3
      kcha_g(33,5) =  -1
      kcha_g(33,6) =   1

* KFcode
      kfco_e(33,1) =  11
      kfco_e(33,2) = -11
      kfco_e(33,3) =  11
      kfco_e(33,4) = -11
      kfco_e(33,5) =   1
      kfco_e(33,6) =  -1
* weight for the color base (llqq)
      cf_mtx(33,0,0) = 3.d0

      kmcb_s(33)    = 1
      kmcb_x(33)    = 2
      kmcs_r(33,1,0) = 5
      kmcs_r(33,2,0) = 6

* particle name
      kmpr_c(34,1) = 'electron'
      kmpr_l(34,1) = 8
      kmpr_c(34,2) = 'positron'
      kmpr_l(34,2) = 8
      kmpr_c(34,3) = 'electron'
      kmpr_l(34,3) = 8
      kmpr_c(34,4) = 'positron'
      kmpr_l(34,4) = 8
      kmpr_c(34,5) = 's'
      kmpr_l(34,5) = 1
      kmpr_c(34,6) = 's-bar'
      kmpr_l(34,6) = 5
* masses of external particles
      amas_1(34,1) = 11
      amas_1(34,2) = 11
      amas_1(34,3) = 11
      amas_1(34,4) = 11
      amas_1(34,5) = 3
      amas_1(34,6) = 3

* Charge*3
      kcha_g(34,1) =  -3
      kcha_g(34,2) =   3
      kcha_g(34,3) =  -3
      kcha_g(34,4) =   3
      kcha_g(34,5) =  -1
      kcha_g(34,6) =   1

* KFcode
      kfco_e(34,1) =  11
      kfco_e(34,2) = -11
      kfco_e(34,3) =  11
      kfco_e(34,4) = -11
      kfco_e(34,5) =   3
      kfco_e(34,6) =  -3
* weight for the color base (llqq)
      cf_mtx(34,0,0) = 3.d0

      kmcb_s(34)    = 1
      kmcb_x(34)    = 2
      kmcs_r(34,1,0) = 5
      kmcs_r(34,2,0) = 6

* particle name
      kmpr_c(35,1) = 'electron'
      kmpr_l(35,1) = 8
      kmpr_c(35,2) = 'positron'
      kmpr_l(35,2) = 8
      kmpr_c(35,3) = 'electron'
      kmpr_l(35,3) = 8
      kmpr_c(35,4) = 'positron'
      kmpr_l(35,4) = 8
      kmpr_c(35,5) = 'b'
      kmpr_l(35,5) = 1
      kmpr_c(35,6) = 'b-bar'
      kmpr_l(35,6) = 5
* masses of external particles
      amas_1(35,1) = 11
      amas_1(35,2) = 11
      amas_1(35,3) = 11
      amas_1(35,4) = 11
      amas_1(35,5) = 5
      amas_1(35,6) = 5

* Charge*3
      kcha_g(35,1) =  -3
      kcha_g(35,2) =   3
      kcha_g(35,3) =  -3
      kcha_g(35,4) =   3
      kcha_g(35,5) =  -1
      kcha_g(35,6) =   1

* KFcode
      kfco_e(35,1) =  11
      kfco_e(35,2) = -11
      kfco_e(35,3) =  11
      kfco_e(35,4) = -11
      kfco_e(35,5) =   5
      kfco_e(35,6) =  -5
* weight for the color base (llqq)
      cf_mtx(35,0,0) = 3.d0

      kmcb_s(35)    = 1
      kmcb_x(35)    = 2
      kmcs_r(35,1,0) = 5
      kmcs_r(35,2,0) = 6

* particle name
      kmpr_c(36,1) = 'electron'
      kmpr_l(36,1) = 8
      kmpr_c(36,2) = 'positron'
      kmpr_l(36,2) = 8
      kmpr_c(36,3) = 'muon'
      kmpr_l(36,3) = 4
      kmpr_c(36,4) = 'anti-muon'
      kmpr_l(36,4) = 9
      kmpr_c(36,5) = 'u'
      kmpr_l(36,5) = 1
      kmpr_c(36,6) = 'u-bar'
      kmpr_l(36,6) = 5
* masses of external particles
      amas_1(36,1) = 11
      amas_1(36,2) = 11
      amas_1(36,3) = 13
      amas_1(36,4) = 13
      amas_1(36,5) = 2
      amas_1(36,6) = 2

* Charge*3
      kcha_g(36,1) =  -3
      kcha_g(36,2) =   3
      kcha_g(36,3) =  -3
      kcha_g(36,4) =   3
      kcha_g(36,5) =   2
      kcha_g(36,6) =  -2

* KFcode
      kfco_e(36,1) =  11
      kfco_e(36,2) = -11
      kfco_e(36,3) =  13
      kfco_e(36,4) = -13
      kfco_e(36,5) =   2
      kfco_e(36,6) =  -2
* weight for the color base (llqq)
      cf_mtx(36,0,0) = 3.d0

      kmcb_s(36)    = 1
      kmcb_x(36)    = 2
      kmcs_r(36,1,0) = 5
      kmcs_r(36,2,0) = 6

* particle name
      kmpr_c(37,1) = 'electron'
      kmpr_l(37,1) = 8
      kmpr_c(37,2) = 'positron'
      kmpr_l(37,2) = 8
      kmpr_c(37,3) = 'muon'
      kmpr_l(37,3) = 4
      kmpr_c(37,4) = 'anti-muon'
      kmpr_l(37,4) = 9
      kmpr_c(37,5) = 'c'
      kmpr_l(37,5) = 1
      kmpr_c(37,6) = 'c-bar'
      kmpr_l(37,6) = 5
* masses of external particles
      amas_1(37,1) = 11
      amas_1(37,2) = 11
      amas_1(37,3) = 13
      amas_1(37,4) = 13
      amas_1(37,5) = 4
      amas_1(37,6) = 4

* Charge*3
      kcha_g(37,1) =  -3
      kcha_g(37,2) =   3
      kcha_g(37,3) =  -3
      kcha_g(37,4) =   3
      kcha_g(37,5) =   2
      kcha_g(37,6) =  -2

* KFcode
      kfco_e(37,1) =  11
      kfco_e(37,2) = -11
      kfco_e(37,3) =  13
      kfco_e(37,4) = -13
      kfco_e(37,5) =   4
      kfco_e(37,6) =  -4
* weight for the color base (llqq)
      cf_mtx(37,0,0) = 3.d0

      kmcb_s(37)    = 1
      kmcb_x(37)    = 2
      kmcs_r(37,1,0) = 5
      kmcs_r(37,2,0) = 6

* particle name
      kmpr_c(38,1) = 'electron'
      kmpr_l(38,1) = 8
      kmpr_c(38,2) = 'positron'
      kmpr_l(38,2) = 8
      kmpr_c(38,3) = 'tau'
      kmpr_l(38,3) = 3
      kmpr_c(38,4) = 'anti-tau'
      kmpr_l(38,4) = 8
      kmpr_c(38,5) = 'u'
      kmpr_l(38,5) = 1
      kmpr_c(38,6) = 'u-bar'
      kmpr_l(38,6) = 5
* masses of external particles
      amas_1(38,1) = 11
      amas_1(38,2) = 11
      amas_1(38,3) = 15
      amas_1(38,4) = 15
      amas_1(38,5) = 2
      amas_1(38,6) = 2

* Charge*3
      kcha_g(38,1) =  -3
      kcha_g(38,2) =   3
      kcha_g(38,3) =  -3
      kcha_g(38,4) =   3
      kcha_g(38,5) =   2
      kcha_g(38,6) =  -2

* KFcode
      kfco_e(38,1) =  11
      kfco_e(38,2) = -11
      kfco_e(38,3) =  15
      kfco_e(38,4) = -15
      kfco_e(38,5) =   2
      kfco_e(38,6) =  -2
* weight for the color base (llqq)
      cf_mtx(38,0,0) = 3.d0

      kmcb_s(38)    = 1
      kmcb_x(38)    = 2
      kmcs_r(38,1,0) = 5
      kmcs_r(38,2,0) = 6

* particle name
      kmpr_c(39,1) = 'electron'
      kmpr_l(39,1) = 8
      kmpr_c(39,2) = 'positron'
      kmpr_l(39,2) = 8
      kmpr_c(39,3) = 'tau'
      kmpr_l(39,3) = 3
      kmpr_c(39,4) = 'anti-tau'
      kmpr_l(39,4) = 8
      kmpr_c(39,5) = 'c'
      kmpr_l(39,5) = 1
      kmpr_c(39,6) = 'c-bar'
      kmpr_l(39,6) = 5
* masses of external particles
      amas_1(39,1) = 11
      amas_1(39,2) = 11
      amas_1(39,3) = 15
      amas_1(39,4) = 15
      amas_1(39,5) = 4
      amas_1(39,6) = 4

* Charge*3
      kcha_g(39,1) =  -3
      kcha_g(39,2) =   3
      kcha_g(39,3) =  -3
      kcha_g(39,4) =   3
      kcha_g(39,5) =   2
      kcha_g(39,6) =  -2

* KFcode
      kfco_e(39,1) =  11
      kfco_e(39,2) = -11
      kfco_e(39,3) =  15
      kfco_e(39,4) = -15
      kfco_e(39,5) =   4
      kfco_e(39,6) =  -4
* weight for the color base (llqq)
      cf_mtx(39,0,0) = 3.d0

      kmcb_s(39)    = 1
      kmcb_x(39)    = 2
      kmcs_r(39,1,0) = 5
      kmcs_r(39,2,0) = 6

* particle name
      kmpr_c(40,1) = 'electron'
      kmpr_l(40,1) = 8
      kmpr_c(40,2) = 'positron'
      kmpr_l(40,2) = 8
      kmpr_c(40,3) = 'muon'
      kmpr_l(40,3) = 4
      kmpr_c(40,4) = 'anti-muon'
      kmpr_l(40,4) = 9
      kmpr_c(40,5) = 'd'
      kmpr_l(40,5) = 1
      kmpr_c(40,6) = 'd-bar'
      kmpr_l(40,6) = 5
* masses of external particles
      amas_1(40,1) = 11
      amas_1(40,2) = 11
      amas_1(40,3) = 13
      amas_1(40,4) = 13
      amas_1(40,5) = 1
      amas_1(40,6) = 1

* Charge*3
      kcha_g(40,1) =  -3
      kcha_g(40,2) =   3
      kcha_g(40,3) =  -3
      kcha_g(40,4) =   3
      kcha_g(40,5) =  -1
      kcha_g(40,6) =   1

* KFcode
      kfco_e(40,1) =  11
      kfco_e(40,2) = -11
      kfco_e(40,3) =  13
      kfco_e(40,4) = -13
      kfco_e(40,5) =   1
      kfco_e(40,6) =  -1
* weight for the color base (llqq)
      cf_mtx(40,0,0) = 3.d0

      kmcb_s(40)    = 1
      kmcb_x(40)    = 2
      kmcs_r(40,1,0) = 5
      kmcs_r(40,2,0) = 6

* particle name
      kmpr_c(41,1) = 'electron'
      kmpr_l(41,1) = 8
      kmpr_c(41,2) = 'positron'
      kmpr_l(41,2) = 8
      kmpr_c(41,3) = 'muon'
      kmpr_l(41,3) = 4
      kmpr_c(41,4) = 'anti-muon'
      kmpr_l(41,4) = 9
      kmpr_c(41,5) = 's'
      kmpr_l(41,5) = 1
      kmpr_c(41,6) = 's-bar'
      kmpr_l(41,6) = 5
* masses of external particles
      amas_1(41,1) = 11
      amas_1(41,2) = 11
      amas_1(41,3) = 13
      amas_1(41,4) = 13
      amas_1(41,5) = 3
      amas_1(41,6) = 3

* Charge*3
      kcha_g(41,1) =  -3
      kcha_g(41,2) =   3
      kcha_g(41,3) =  -3
      kcha_g(41,4) =   3
      kcha_g(41,5) =  -1
      kcha_g(41,6) =   1

* KFcode
      kfco_e(41,1) =  11
      kfco_e(41,2) = -11
      kfco_e(41,3) =  13
      kfco_e(41,4) = -13
      kfco_e(41,5) =   3
      kfco_e(41,6) =  -3
* weight for the color base (llqq)
      cf_mtx(41,0,0) = 3.d0

      kmcb_s(41)    = 1
      kmcb_x(41)    = 2
      kmcs_r(41,1,0) = 5
      kmcs_r(41,2,0) = 6

* particle name
      kmpr_c(42,1) = 'electron'
      kmpr_l(42,1) = 8
      kmpr_c(42,2) = 'positron'
      kmpr_l(42,2) = 8
      kmpr_c(42,3) = 'muon'
      kmpr_l(42,3) = 4
      kmpr_c(42,4) = 'anti-muon'
      kmpr_l(42,4) = 9
      kmpr_c(42,5) = 'b'
      kmpr_l(42,5) = 1
      kmpr_c(42,6) = 'b-bar'
      kmpr_l(42,6) = 5
* masses of external particles
      amas_1(42,1) = 11
      amas_1(42,2) = 11
      amas_1(42,3) = 13
      amas_1(42,4) = 13
      amas_1(42,5) = 5
      amas_1(42,6) = 5

* Charge*3
      kcha_g(42,1) =  -3
      kcha_g(42,2) =   3
      kcha_g(42,3) =  -3
      kcha_g(42,4) =   3
      kcha_g(42,5) =  -1
      kcha_g(42,6) =   1

* KFcode
      kfco_e(42,1) =  11
      kfco_e(42,2) = -11
      kfco_e(42,3) =  13
      kfco_e(42,4) = -13
      kfco_e(42,5) =   5
      kfco_e(42,6) =  -5
* weight for the color base (llqq)
      cf_mtx(42,0,0) = 3.d0

      kmcb_s(42)    = 1
      kmcb_x(42)    = 2
      kmcs_r(42,1,0) = 5
      kmcs_r(42,2,0) = 6

* particle name
      kmpr_c(43,1) = 'electron'
      kmpr_l(43,1) = 8
      kmpr_c(43,2) = 'positron'
      kmpr_l(43,2) = 8
      kmpr_c(43,3) = 'tau'
      kmpr_l(43,3) = 3
      kmpr_c(43,4) = 'anti-tau'
      kmpr_l(43,4) = 8
      kmpr_c(43,5) = 'd'
      kmpr_l(43,5) = 1
      kmpr_c(43,6) = 'd-bar'
      kmpr_l(43,6) = 5
* masses of external particles
      amas_1(43,1) = 11
      amas_1(43,2) = 11
      amas_1(43,3) = 15
      amas_1(43,4) = 15
      amas_1(43,5) = 1
      amas_1(43,6) = 1

* Charge*3
      kcha_g(43,1) =  -3
      kcha_g(43,2) =   3
      kcha_g(43,3) =  -3
      kcha_g(43,4) =   3
      kcha_g(43,5) =  -1
      kcha_g(43,6) =   1

* KFcode
      kfco_e(43,1) =  11
      kfco_e(43,2) = -11
      kfco_e(43,3) =  15
      kfco_e(43,4) = -15
      kfco_e(43,5) =   1
      kfco_e(43,6) =  -1
* weight for the color base (llqq)
      cf_mtx(43,0,0) = 3.d0

      kmcb_s(43)    = 1
      kmcb_x(43)    = 2
      kmcs_r(43,1,0) = 5
      kmcs_r(43,2,0) = 6

* particle name
      kmpr_c(44,1) = 'electron'
      kmpr_l(44,1) = 8
      kmpr_c(44,2) = 'positron'
      kmpr_l(44,2) = 8
      kmpr_c(44,3) = 'tau'
      kmpr_l(44,3) = 3
      kmpr_c(44,4) = 'anti-tau'
      kmpr_l(44,4) = 8
      kmpr_c(44,5) = 's'
      kmpr_l(44,5) = 1
      kmpr_c(44,6) = 's-bar'
      kmpr_l(44,6) = 5
* masses of external particles
      amas_1(44,1) = 11
      amas_1(44,2) = 11
      amas_1(44,3) = 15
      amas_1(44,4) = 15
      amas_1(44,5) = 3
      amas_1(44,6) = 3

* Charge*3
      kcha_g(44,1) =  -3
      kcha_g(44,2) =   3
      kcha_g(44,3) =  -3
      kcha_g(44,4) =   3
      kcha_g(44,5) =  -1
      kcha_g(44,6) =   1

* KFcode
      kfco_e(44,1) =  11
      kfco_e(44,2) = -11
      kfco_e(44,3) =  15
      kfco_e(44,4) = -15
      kfco_e(44,5) =   3
      kfco_e(44,6) =  -3
* weight for the color base (llqq)
      cf_mtx(44,0,0) = 3.d0

      kmcb_s(44)    = 1
      kmcb_x(44)    = 2
      kmcs_r(44,1,0) = 5
      kmcs_r(44,2,0) = 6

* particle name
      kmpr_c(45,1) = 'electron'
      kmpr_l(45,1) = 8
      kmpr_c(45,2) = 'positron'
      kmpr_l(45,2) = 8
      kmpr_c(45,3) = 'tau'
      kmpr_l(45,3) = 3
      kmpr_c(45,4) = 'anti-tau'
      kmpr_l(45,4) = 8
      kmpr_c(45,5) = 'b'
      kmpr_l(45,5) = 1
      kmpr_c(45,6) = 'b-bar'
      kmpr_l(45,6) = 5
* masses of external particles
      amas_1(45,1) = 11
      amas_1(45,2) = 11
      amas_1(45,3) = 15
      amas_1(45,4) = 15
      amas_1(45,5) = 5
      amas_1(45,6) = 5

* Charge*3
      kcha_g(45,1) =  -3
      kcha_g(45,2) =   3
      kcha_g(45,3) =  -3
      kcha_g(45,4) =   3
      kcha_g(45,5) =  -1
      kcha_g(45,6) =   1

* KFcode
      kfco_e(45,1) =  11
      kfco_e(45,2) = -11
      kfco_e(45,3) =  15
      kfco_e(45,4) = -15
      kfco_e(45,5) =   5
      kfco_e(45,6) =  -5
* weight for the color base (llqq)
      cf_mtx(45,0,0) = 3.d0

      kmcb_s(45)    = 1
      kmcb_x(45)    = 2
      kmcs_r(45,1,0) = 5
      kmcs_r(45,2,0) = 6

* particle name
      kmpr_c(46,1) = 'electron'
      kmpr_l(46,1) = 8
      kmpr_c(46,2) = 'positron'
      kmpr_l(46,2) = 8
      kmpr_c(46,3) = 'nu-e'
      kmpr_l(46,3) = 4
      kmpr_c(46,4) = 'nu-e-bar'
      kmpr_l(46,4) = 8
      kmpr_c(46,5) = 'u'
      kmpr_l(46,5) = 1
      kmpr_c(46,6) = 'u-bar'
      kmpr_l(46,6) = 5
* masses of external particles
      amas_1(46,1) = 11
      amas_1(46,2) = 11
      amas_1(46,3) = 12
      amas_1(46,4) = 12
      amas_1(46,5) = 2
      amas_1(46,6) = 2

* Charge*3
      kcha_g(46,1) =  -3
      kcha_g(46,2) =   3
      kcha_g(46,3) =   0
      kcha_g(46,4) =   0
      kcha_g(46,5) =   2
      kcha_g(46,6) =  -2

* KFcode
      kfco_e(46,1) =  11
      kfco_e(46,2) = -11
      kfco_e(46,3) =  12
      kfco_e(46,4) = -12
      kfco_e(46,5) =   2
      kfco_e(46,6) =  -2
* weight for the color base (llqq)
      cf_mtx(46,0,0) = 3.d0

      kmcb_s(46)    = 1
      kmcb_x(46)    = 2
      kmcs_r(46,1,0) = 5
      kmcs_r(46,2,0) = 6

* particle name
      kmpr_c(47,1) = 'electron'
      kmpr_l(47,1) = 8
      kmpr_c(47,2) = 'positron'
      kmpr_l(47,2) = 8
      kmpr_c(47,3) = 'nu-e'
      kmpr_l(47,3) = 4
      kmpr_c(47,4) = 'nu-e-bar'
      kmpr_l(47,4) = 8
      kmpr_c(47,5) = 'c'
      kmpr_l(47,5) = 1
      kmpr_c(47,6) = 'c-bar'
      kmpr_l(47,6) = 5
* masses of external particles
      amas_1(47,1) = 11
      amas_1(47,2) = 11
      amas_1(47,3) = 12
      amas_1(47,4) = 12
      amas_1(47,5) = 4
      amas_1(47,6) = 4

* Charge*3
      kcha_g(47,1) =  -3
      kcha_g(47,2) =   3
      kcha_g(47,3) =   0
      kcha_g(47,4) =   0
      kcha_g(47,5) =   2
      kcha_g(47,6) =  -2

* KFcode
      kfco_e(47,1) =  11
      kfco_e(47,2) = -11
      kfco_e(47,3) =  12
      kfco_e(47,4) = -12
      kfco_e(47,5) =   4
      kfco_e(47,6) =  -4
* weight for the color base (llqq)
      cf_mtx(47,0,0) = 3.d0

      kmcb_s(47)    = 1
      kmcb_x(47)    = 2
      kmcs_r(47,1,0) = 5
      kmcs_r(47,2,0) = 6

* particle name
      kmpr_c(48,1) = 'electron'
      kmpr_l(48,1) = 8
      kmpr_c(48,2) = 'positron'
      kmpr_l(48,2) = 8
      kmpr_c(48,3) = 'nu-e'
      kmpr_l(48,3) = 4
      kmpr_c(48,4) = 'nu-e-bar'
      kmpr_l(48,4) = 8
      kmpr_c(48,5) = 'd'
      kmpr_l(48,5) = 1
      kmpr_c(48,6) = 'd-bar'
      kmpr_l(48,6) = 5
* masses of external particles
      amas_1(48,1) = 11
      amas_1(48,2) = 11
      amas_1(48,3) = 12
      amas_1(48,4) = 12
      amas_1(48,5) = 1
      amas_1(48,6) = 1

* Charge*3
      kcha_g(48,1) =  -3
      kcha_g(48,2) =   3
      kcha_g(48,3) =   0
      kcha_g(48,4) =   0
      kcha_g(48,5) =  -1
      kcha_g(48,6) =   1

* KFcode
      kfco_e(48,1) =  11
      kfco_e(48,2) = -11
      kfco_e(48,3) =  12
      kfco_e(48,4) = -12
      kfco_e(48,5) =   1
      kfco_e(48,6) =  -1
* weight for the color base (llqq)
      cf_mtx(48,0,0) = 3.d0

      kmcb_s(48)    = 1
      kmcb_x(48)    = 2
      kmcs_r(48,1,0) = 5
      kmcs_r(48,2,0) = 6

* particle name
      kmpr_c(49,1) = 'electron'
      kmpr_l(49,1) = 8
      kmpr_c(49,2) = 'positron'
      kmpr_l(49,2) = 8
      kmpr_c(49,3) = 'nu-e'
      kmpr_l(49,3) = 4
      kmpr_c(49,4) = 'nu-e-bar'
      kmpr_l(49,4) = 8
      kmpr_c(49,5) = 's'
      kmpr_l(49,5) = 1
      kmpr_c(49,6) = 's-bar'
      kmpr_l(49,6) = 5
* masses of external particles
      amas_1(49,1) = 11
      amas_1(49,2) = 11
      amas_1(49,3) = 12
      amas_1(49,4) = 12
      amas_1(49,5) = 3
      amas_1(49,6) = 3

* Charge*3
      kcha_g(49,1) =  -3
      kcha_g(49,2) =   3
      kcha_g(49,3) =   0
      kcha_g(49,4) =   0
      kcha_g(49,5) =  -1
      kcha_g(49,6) =   1

* KFcode
      kfco_e(49,1) =  11
      kfco_e(49,2) = -11
      kfco_e(49,3) =  12
      kfco_e(49,4) = -12
      kfco_e(49,5) =   3
      kfco_e(49,6) =  -3
* weight for the color base (llqq)
      cf_mtx(49,0,0) = 3.d0

      kmcb_s(49)    = 1
      kmcb_x(49)    = 2
      kmcs_r(49,1,0) = 5
      kmcs_r(49,2,0) = 6

* particle name
      kmpr_c(50,1) = 'electron'
      kmpr_l(50,1) = 8
      kmpr_c(50,2) = 'positron'
      kmpr_l(50,2) = 8
      kmpr_c(50,3) = 'nu-e'
      kmpr_l(50,3) = 4
      kmpr_c(50,4) = 'nu-e-bar'
      kmpr_l(50,4) = 8
      kmpr_c(50,5) = 'b'
      kmpr_l(50,5) = 1
      kmpr_c(50,6) = 'b-bar'
      kmpr_l(50,6) = 5
* masses of external particles
      amas_1(50,1) = 11
      amas_1(50,2) = 11
      amas_1(50,3) = 12
      amas_1(50,4) = 12
      amas_1(50,5) = 5
      amas_1(50,6) = 5

* Charge*3
      kcha_g(50,1) =  -3
      kcha_g(50,2) =   3
      kcha_g(50,3) =   0
      kcha_g(50,4) =   0
      kcha_g(50,5) =  -1
      kcha_g(50,6) =   1

* KFcode
      kfco_e(50,1) =  11
      kfco_e(50,2) = -11
      kfco_e(50,3) =  12
      kfco_e(50,4) = -12
      kfco_e(50,5) =   5
      kfco_e(50,6) =  -5
* weight for the color base (llqq)
      cf_mtx(50,0,0) = 3.d0

      kmcb_s(50)    = 1
      kmcb_x(50)    = 2
      kmcs_r(50,1,0) = 5
      kmcs_r(50,2,0) = 6

* particle name
      kmpr_c(51,1) = 'electron'
      kmpr_l(51,1) = 8
      kmpr_c(51,2) = 'positron'
      kmpr_l(51,2) = 8
      kmpr_c(51,3) = 'nu-mu'
      kmpr_l(51,3) = 5
      kmpr_c(51,4) = 'nu-mu-bar'
      kmpr_l(51,4) = 9
      kmpr_c(51,5) = 'u'
      kmpr_l(51,5) = 1
      kmpr_c(51,6) = 'u-bar'
      kmpr_l(51,6) = 5
* masses of external particles
      amas_1(51,1) = 11
      amas_1(51,2) = 11
      amas_1(51,3) = 14
      amas_1(51,4) = 14
      amas_1(51,5) = 2
      amas_1(51,6) = 2

* Charge*3
      kcha_g(51,1) =  -3
      kcha_g(51,2) =   3
      kcha_g(51,3) =   0
      kcha_g(51,4) =   0
      kcha_g(51,5) =   2
      kcha_g(51,6) =  -2

* KFcode
      kfco_e(51,1) =  11
      kfco_e(51,2) = -11
      kfco_e(51,3) =  14
      kfco_e(51,4) = -14
      kfco_e(51,5) =   2
      kfco_e(51,6) =  -2
* weight for the color base (llqq)
      cf_mtx(51,0,0) = 3.d0

      kmcb_s(51)    = 1
      kmcb_x(51)    = 2
      kmcs_r(51,1,0) = 5
      kmcs_r(51,2,0) = 6

* particle name
      kmpr_c(52,1) = 'electron'
      kmpr_l(52,1) = 8
      kmpr_c(52,2) = 'positron'
      kmpr_l(52,2) = 8
      kmpr_c(52,3) = 'nu-mu'
      kmpr_l(52,3) = 5
      kmpr_c(52,4) = 'nu-mu-bar'
      kmpr_l(52,4) = 9
      kmpr_c(52,5) = 'c'
      kmpr_l(52,5) = 1
      kmpr_c(52,6) = 'c-bar'
      kmpr_l(52,6) = 5
* masses of external particles
      amas_1(52,1) = 11
      amas_1(52,2) = 11
      amas_1(52,3) = 14
      amas_1(52,4) = 14
      amas_1(52,5) = 4
      amas_1(52,6) = 4

* Charge*3
      kcha_g(52,1) =  -3
      kcha_g(52,2) =   3
      kcha_g(52,3) =   0
      kcha_g(52,4) =   0
      kcha_g(52,5) =   2
      kcha_g(52,6) =  -2

* KFcode
      kfco_e(52,1) =  11
      kfco_e(52,2) = -11
      kfco_e(52,3) =  14
      kfco_e(52,4) = -14
      kfco_e(52,5) =   4
      kfco_e(52,6) =  -4
* weight for the color base (llqq)
      cf_mtx(52,0,0) = 3.d0

      kmcb_s(52)    = 1
      kmcb_x(52)    = 2
      kmcs_r(52,1,0) = 5
      kmcs_r(52,2,0) = 6

* particle name
      kmpr_c(53,1) = 'electron'
      kmpr_l(53,1) = 8
      kmpr_c(53,2) = 'positron'
      kmpr_l(53,2) = 8
      kmpr_c(53,3) = 'nu-tau'
      kmpr_l(53,3) = 6
      kmpr_c(53,4) = 'nu-tau-bar'
      kmpr_l(53,4) = 10
      kmpr_c(53,5) = 'u'
      kmpr_l(53,5) = 1
      kmpr_c(53,6) = 'u-bar'
      kmpr_l(53,6) = 5
* masses of external particles
      amas_1(53,1) = 11
      amas_1(53,2) = 11
      amas_1(53,3) = 16
      amas_1(53,4) = 16
      amas_1(53,5) = 2
      amas_1(53,6) = 2

* Charge*3
      kcha_g(53,1) =  -3
      kcha_g(53,2) =   3
      kcha_g(53,3) =   0
      kcha_g(53,4) =   0
      kcha_g(53,5) =   2
      kcha_g(53,6) =  -2

* KFcode
      kfco_e(53,1) =  11
      kfco_e(53,2) = -11
      kfco_e(53,3) =  16
      kfco_e(53,4) = -16
      kfco_e(53,5) =   2
      kfco_e(53,6) =  -2
* weight for the color base (llqq)
      cf_mtx(53,0,0) = 3.d0

      kmcb_s(53)    = 1
      kmcb_x(53)    = 2
      kmcs_r(53,1,0) = 5
      kmcs_r(53,2,0) = 6

* particle name
      kmpr_c(54,1) = 'electron'
      kmpr_l(54,1) = 8
      kmpr_c(54,2) = 'positron'
      kmpr_l(54,2) = 8
      kmpr_c(54,3) = 'nu-tau'
      kmpr_l(54,3) = 6
      kmpr_c(54,4) = 'nu-tau-bar'
      kmpr_l(54,4) = 10
      kmpr_c(54,5) = 'c'
      kmpr_l(54,5) = 1
      kmpr_c(54,6) = 'c-bar'
      kmpr_l(54,6) = 5
* masses of external particles
      amas_1(54,1) = 11
      amas_1(54,2) = 11
      amas_1(54,3) = 16
      amas_1(54,4) = 16
      amas_1(54,5) = 4
      amas_1(54,6) = 4

* Charge*3
      kcha_g(54,1) =  -3
      kcha_g(54,2) =   3
      kcha_g(54,3) =   0
      kcha_g(54,4) =   0
      kcha_g(54,5) =   2
      kcha_g(54,6) =  -2

* KFcode
      kfco_e(54,1) =  11
      kfco_e(54,2) = -11
      kfco_e(54,3) =  16
      kfco_e(54,4) = -16
      kfco_e(54,5) =   4
      kfco_e(54,6) =  -4
* weight for the color base (llqq)
      cf_mtx(54,0,0) = 3.d0

      kmcb_s(54)    = 1
      kmcb_x(54)    = 2
      kmcs_r(54,1,0) = 5
      kmcs_r(54,2,0) = 6

* particle name
      kmpr_c(55,1) = 'electron'
      kmpr_l(55,1) = 8
      kmpr_c(55,2) = 'positron'
      kmpr_l(55,2) = 8
      kmpr_c(55,3) = 'nu-mu'
      kmpr_l(55,3) = 5
      kmpr_c(55,4) = 'nu-mu-bar'
      kmpr_l(55,4) = 9
      kmpr_c(55,5) = 'd'
      kmpr_l(55,5) = 1
      kmpr_c(55,6) = 'd-bar'
      kmpr_l(55,6) = 5
* masses of external particles
      amas_1(55,1) = 11
      amas_1(55,2) = 11
      amas_1(55,3) = 14
      amas_1(55,4) = 14
      amas_1(55,5) = 1
      amas_1(55,6) = 1

* Charge*3
      kcha_g(55,1) =  -3
      kcha_g(55,2) =   3
      kcha_g(55,3) =   0
      kcha_g(55,4) =   0
      kcha_g(55,5) =  -1
      kcha_g(55,6) =   1

* KFcode
      kfco_e(55,1) =  11
      kfco_e(55,2) = -11
      kfco_e(55,3) =  14
      kfco_e(55,4) = -14
      kfco_e(55,5) =   1
      kfco_e(55,6) =  -1
* weight for the color base (llqq)
      cf_mtx(55,0,0) = 3.d0

      kmcb_s(55)    = 1
      kmcb_x(55)    = 2
      kmcs_r(55,1,0) = 5
      kmcs_r(55,2,0) = 6

* particle name
      kmpr_c(56,1) = 'electron'
      kmpr_l(56,1) = 8
      kmpr_c(56,2) = 'positron'
      kmpr_l(56,2) = 8
      kmpr_c(56,3) = 'nu-mu'
      kmpr_l(56,3) = 5
      kmpr_c(56,4) = 'nu-mu-bar'
      kmpr_l(56,4) = 9
      kmpr_c(56,5) = 's'
      kmpr_l(56,5) = 1
      kmpr_c(56,6) = 's-bar'
      kmpr_l(56,6) = 5
* masses of external particles
      amas_1(56,1) = 11
      amas_1(56,2) = 11
      amas_1(56,3) = 14
      amas_1(56,4) = 14
      amas_1(56,5) = 3
      amas_1(56,6) = 3

* Charge*3
      kcha_g(56,1) =  -3
      kcha_g(56,2) =   3
      kcha_g(56,3) =   0
      kcha_g(56,4) =   0
      kcha_g(56,5) =  -1
      kcha_g(56,6) =   1

* KFcode
      kfco_e(56,1) =  11
      kfco_e(56,2) = -11
      kfco_e(56,3) =  14
      kfco_e(56,4) = -14
      kfco_e(56,5) =   3
      kfco_e(56,6) =  -3
* weight for the color base (llqq)
      cf_mtx(56,0,0) = 3.d0

      kmcb_s(56)    = 1
      kmcb_x(56)    = 2
      kmcs_r(56,1,0) = 5
      kmcs_r(56,2,0) = 6

* particle name
      kmpr_c(57,1) = 'electron'
      kmpr_l(57,1) = 8
      kmpr_c(57,2) = 'positron'
      kmpr_l(57,2) = 8
      kmpr_c(57,3) = 'nu-mu'
      kmpr_l(57,3) = 5
      kmpr_c(57,4) = 'nu-mu-bar'
      kmpr_l(57,4) = 9
      kmpr_c(57,5) = 'b'
      kmpr_l(57,5) = 1
      kmpr_c(57,6) = 'b-bar'
      kmpr_l(57,6) = 5
* masses of external particles
      amas_1(57,1) = 11
      amas_1(57,2) = 11
      amas_1(57,3) = 14
      amas_1(57,4) = 14
      amas_1(57,5) = 5
      amas_1(57,6) = 5

* Charge*3
      kcha_g(57,1) =  -3
      kcha_g(57,2) =   3
      kcha_g(57,3) =   0
      kcha_g(57,4) =   0
      kcha_g(57,5) =  -1
      kcha_g(57,6) =   1

* KFcode
      kfco_e(57,1) =  11
      kfco_e(57,2) = -11
      kfco_e(57,3) =  14
      kfco_e(57,4) = -14
      kfco_e(57,5) =   5
      kfco_e(57,6) =  -5
* weight for the color base (llqq)
      cf_mtx(57,0,0) = 3.d0

      kmcb_s(57)    = 1
      kmcb_x(57)    = 2
      kmcs_r(57,1,0) = 5
      kmcs_r(57,2,0) = 6

* particle name
      kmpr_c(58,1) = 'electron'
      kmpr_l(58,1) = 8
      kmpr_c(58,2) = 'positron'
      kmpr_l(58,2) = 8
      kmpr_c(58,3) = 'nu-tau'
      kmpr_l(58,3) = 6
      kmpr_c(58,4) = 'nu-tau-bar'
      kmpr_l(58,4) = 10
      kmpr_c(58,5) = 'd'
      kmpr_l(58,5) = 1
      kmpr_c(58,6) = 'd-bar'
      kmpr_l(58,6) = 5
* masses of external particles
      amas_1(58,1) = 11
      amas_1(58,2) = 11
      amas_1(58,3) = 16
      amas_1(58,4) = 16
      amas_1(58,5) = 1
      amas_1(58,6) = 1

* Charge*3
      kcha_g(58,1) =  -3
      kcha_g(58,2) =   3
      kcha_g(58,3) =   0
      kcha_g(58,4) =   0
      kcha_g(58,5) =  -1
      kcha_g(58,6) =   1

* KFcode
      kfco_e(58,1) =  11
      kfco_e(58,2) = -11
      kfco_e(58,3) =  16
      kfco_e(58,4) = -16
      kfco_e(58,5) =   1
      kfco_e(58,6) =  -1
* weight for the color base (llqq)
      cf_mtx(58,0,0) = 3.d0

      kmcb_s(58)    = 1
      kmcb_x(58)    = 2
      kmcs_r(58,1,0) = 5
      kmcs_r(58,2,0) = 6

* particle name
      kmpr_c(59,1) = 'electron'
      kmpr_l(59,1) = 8
      kmpr_c(59,2) = 'positron'
      kmpr_l(59,2) = 8
      kmpr_c(59,3) = 'nu-tau'
      kmpr_l(59,3) = 6
      kmpr_c(59,4) = 'nu-tau-bar'
      kmpr_l(59,4) = 10
      kmpr_c(59,5) = 's'
      kmpr_l(59,5) = 1
      kmpr_c(59,6) = 's-bar'
      kmpr_l(59,6) = 5
* masses of external particles
      amas_1(59,1) = 11
      amas_1(59,2) = 11
      amas_1(59,3) = 16
      amas_1(59,4) = 16
      amas_1(59,5) = 3
      amas_1(59,6) = 3

* Charge*3
      kcha_g(59,1) =  -3
      kcha_g(59,2) =   3
      kcha_g(59,3) =   0
      kcha_g(59,4) =   0
      kcha_g(59,5) =  -1
      kcha_g(59,6) =   1

* KFcode
      kfco_e(59,1) =  11
      kfco_e(59,2) = -11
      kfco_e(59,3) =  16
      kfco_e(59,4) = -16
      kfco_e(59,5) =   3
      kfco_e(59,6) =  -3
* weight for the color base (llqq)
      cf_mtx(59,0,0) = 3.d0

      kmcb_s(59)    = 1
      kmcb_x(59)    = 2
      kmcs_r(59,1,0) = 5
      kmcs_r(59,2,0) = 6

* particle name
      kmpr_c(60,1) = 'electron'
      kmpr_l(60,1) = 8
      kmpr_c(60,2) = 'positron'
      kmpr_l(60,2) = 8
      kmpr_c(60,3) = 'nu-tau'
      kmpr_l(60,3) = 6
      kmpr_c(60,4) = 'nu-tau-bar'
      kmpr_l(60,4) = 10
      kmpr_c(60,5) = 'b'
      kmpr_l(60,5) = 1
      kmpr_c(60,6) = 'b-bar'
      kmpr_l(60,6) = 5
* masses of external particles
      amas_1(60,1) = 11
      amas_1(60,2) = 11
      amas_1(60,3) = 16
      amas_1(60,4) = 16
      amas_1(60,5) = 5
      amas_1(60,6) = 5

* Charge*3
      kcha_g(60,1) =  -3
      kcha_g(60,2) =   3
      kcha_g(60,3) =   0
      kcha_g(60,4) =   0
      kcha_g(60,5) =  -1
      kcha_g(60,6) =   1

* KFcode
      kfco_e(60,1) =  11
      kfco_e(60,2) = -11
      kfco_e(60,3) =  16
      kfco_e(60,4) = -16
      kfco_e(60,5) =   5
      kfco_e(60,6) =  -5
* weight for the color base (llqq)
      cf_mtx(60,0,0) = 3.d0

      kmcb_s(60)    = 1
      kmcb_x(60)    = 2
      kmcs_r(60,1,0) = 5
      kmcs_r(60,2,0) = 6

* particle name
      kmpr_c(61,1) = 'electron'
      kmpr_l(61,1) = 8
      kmpr_c(61,2) = 'positron'
      kmpr_l(61,2) = 8
      kmpr_c(61,3) = 'u'
      kmpr_l(61,3) = 1
      kmpr_c(61,4) = 'd-bar'
      kmpr_l(61,4) = 5
      kmpr_c(61,5) = 'd'
      kmpr_l(61,5) = 1
      kmpr_c(61,6) = 'u-bar'
      kmpr_l(61,6) = 5
* masses of external particles
      amas_1(61,1) = 11
      amas_1(61,2) = 11
      amas_1(61,3) = 2
      amas_1(61,4) = 1
      amas_1(61,5) = 1
      amas_1(61,6) = 2

* Charge*3
      kcha_g(61,1) =  -3
      kcha_g(61,2) =   3
      kcha_g(61,3) =   2
      kcha_g(61,4) =   1
      kcha_g(61,5) =  -1
      kcha_g(61,6) =  -2

* KFcode
      kfco_e(61,1) =  11
      kfco_e(61,2) = -11
      kfco_e(61,3) =   2
      kfco_e(61,4) =  -1
      kfco_e(61,5) =   1
      kfco_e(61,6) =  -2
* weight for the color base (qqqq)
      cf_mtx(61,0,0)= 9.d0
      cf_mtx(61,0,1)= 3.d0
      cf_mtx(61,1,0)= 3.d0
      cf_mtx(61,1,1)= 9.d0

      kmcb_s(61)    = 2
      kmcb_x(61)    = 4
      kmcs_r(61,1,0) = 3
      kmcs_r(61,2,0) = 4
      kmcs_r(61,3,0) = 5
      kmcs_r(61,4,0) = 6
      kmcs_r(61,1,1) = 3
      kmcs_r(61,2,1) = 6
      kmcs_r(61,3,1) = 4
      kmcs_r(61,4,1) = 5

* particle name
      kmpr_c(62,1) = 'electron'
      kmpr_l(62,1) = 8
      kmpr_c(62,2) = 'positron'
      kmpr_l(62,2) = 8
      kmpr_c(62,3) = 'c'
      kmpr_l(62,3) = 1
      kmpr_c(62,4) = 's-bar'
      kmpr_l(62,4) = 5
      kmpr_c(62,5) = 's'
      kmpr_l(62,5) = 1
      kmpr_c(62,6) = 'c-bar'
      kmpr_l(62,6) = 5
* masses of external particles
      amas_1(62,1) = 11
      amas_1(62,2) = 11
      amas_1(62,3) = 4
      amas_1(62,4) = 3
      amas_1(62,5) = 3
      amas_1(62,6) = 4

* Charge*3
      kcha_g(62,1) =  -3
      kcha_g(62,2) =   3
      kcha_g(62,3) =   2
      kcha_g(62,4) =   1
      kcha_g(62,5) =  -1
      kcha_g(62,6) =  -2

* KFcode
      kfco_e(62,1) =  11
      kfco_e(62,2) = -11
      kfco_e(62,3) =   4
      kfco_e(62,4) =  -3
      kfco_e(62,5) =   3
      kfco_e(62,6) =  -4
* weight for the color base (qqqq)
      cf_mtx(62,0,0)= 9.d0
      cf_mtx(62,0,1)= 3.d0
      cf_mtx(62,1,0)= 3.d0
      cf_mtx(62,1,1)= 9.d0

      kmcb_s(62)    = 2
      kmcb_x(62)    = 4
      kmcs_r(62,1,0) = 3
      kmcs_r(62,2,0) = 4
      kmcs_r(62,3,0) = 5
      kmcs_r(62,4,0) = 6
      kmcs_r(62,1,1) = 3
      kmcs_r(62,2,1) = 6
      kmcs_r(62,3,1) = 4
      kmcs_r(62,4,1) = 5

* particle name
      kmpr_c(63,1) = 'electron'
      kmpr_l(63,1) = 8
      kmpr_c(63,2) = 'positron'
      kmpr_l(63,2) = 8
      kmpr_c(63,3) = 'u'
      kmpr_l(63,3) = 1
      kmpr_c(63,4) = 'd-bar'
      kmpr_l(63,4) = 5
      kmpr_c(63,5) = 's'
      kmpr_l(63,5) = 1
      kmpr_c(63,6) = 'c-bar'
      kmpr_l(63,6) = 5
* masses of external particles
      amas_1(63,1) = 11
      amas_1(63,2) = 11
      amas_1(63,3) = 2
      amas_1(63,4) = 1
      amas_1(63,5) = 3
      amas_1(63,6) = 4

* Charge*3
      kcha_g(63,1) =  -3
      kcha_g(63,2) =   3
      kcha_g(63,3) =   2
      kcha_g(63,4) =   1
      kcha_g(63,5) =  -1
      kcha_g(63,6) =  -2

* KFcode
      kfco_e(63,1) =  11
      kfco_e(63,2) = -11
      kfco_e(63,3) =   2
      kfco_e(63,4) =  -1
      kfco_e(63,5) =   3
      kfco_e(63,6) =  -4
* weight for the color base (qqqq)
      cf_mtx(63,0,0)= 9.d0
      cf_mtx(63,0,1)= 3.d0
      cf_mtx(63,1,0)= 3.d0
      cf_mtx(63,1,1)= 9.d0

      kmcb_s(63)    = 2
      kmcb_x(63)    = 4
      kmcs_r(63,1,0) = 3
      kmcs_r(63,2,0) = 4
      kmcs_r(63,3,0) = 5
      kmcs_r(63,4,0) = 6
      kmcs_r(63,1,1) = 3
      kmcs_r(63,2,1) = 6
      kmcs_r(63,3,1) = 4
      kmcs_r(63,4,1) = 5

* particle name
      kmpr_c(64,1) = 'electron'
      kmpr_l(64,1) = 8
      kmpr_c(64,2) = 'positron'
      kmpr_l(64,2) = 8
      kmpr_c(64,3) = 'u'
      kmpr_l(64,3) = 1
      kmpr_c(64,4) = 'u-bar'
      kmpr_l(64,4) = 5
      kmpr_c(64,5) = 'u'
      kmpr_l(64,5) = 1
      kmpr_c(64,6) = 'u-bar'
      kmpr_l(64,6) = 5
* masses of external particles
      amas_1(64,1) = 11
      amas_1(64,2) = 11
      amas_1(64,3) = 2
      amas_1(64,4) = 2
      amas_1(64,5) = 2
      amas_1(64,6) = 2

* Charge*3
      kcha_g(64,1) =  -3
      kcha_g(64,2) =   3
      kcha_g(64,3) =   2
      kcha_g(64,4) =  -2
      kcha_g(64,5) =   2
      kcha_g(64,6) =  -2

* KFcode
      kfco_e(64,1) =  11
      kfco_e(64,2) = -11
      kfco_e(64,3) =   2
      kfco_e(64,4) =  -2
      kfco_e(64,5) =   2
      kfco_e(64,6) =  -2
* weight for the color base (qqqq)
      cf_mtx(64,0,0)= 9.d0
      cf_mtx(64,0,1)= 3.d0
      cf_mtx(64,1,0)= 3.d0
      cf_mtx(64,1,1)= 9.d0

      kmcb_s(64)    = 2
      kmcb_x(64)    = 4
      kmcs_r(64,1,0) = 3
      kmcs_r(64,2,0) = 4
      kmcs_r(64,3,0) = 5
      kmcs_r(64,4,0) = 6
      kmcs_r(64,1,1) = 3
      kmcs_r(64,2,1) = 6
      kmcs_r(64,3,1) = 4
      kmcs_r(64,4,1) = 5

* particle name
      kmpr_c(65,1) = 'electron'
      kmpr_l(65,1) = 8
      kmpr_c(65,2) = 'positron'
      kmpr_l(65,2) = 8
      kmpr_c(65,3) = 'c'
      kmpr_l(65,3) = 1
      kmpr_c(65,4) = 'c-bar'
      kmpr_l(65,4) = 5
      kmpr_c(65,5) = 'c'
      kmpr_l(65,5) = 1
      kmpr_c(65,6) = 'c-bar'
      kmpr_l(65,6) = 5
* masses of external particles
      amas_1(65,1) = 11
      amas_1(65,2) = 11
      amas_1(65,3) = 4
      amas_1(65,4) = 4
      amas_1(65,5) = 4
      amas_1(65,6) = 4

* Charge*3
      kcha_g(65,1) =  -3
      kcha_g(65,2) =   3
      kcha_g(65,3) =   2
      kcha_g(65,4) =  -2
      kcha_g(65,5) =   2
      kcha_g(65,6) =  -2

* KFcode
      kfco_e(65,1) =  11
      kfco_e(65,2) = -11
      kfco_e(65,3) =   4
      kfco_e(65,4) =  -4
      kfco_e(65,5) =   4
      kfco_e(65,6) =  -4
* weight for the color base (qqqq)
      cf_mtx(65,0,0)= 9.d0
      cf_mtx(65,0,1)= 3.d0
      cf_mtx(65,1,0)= 3.d0
      cf_mtx(65,1,1)= 9.d0

      kmcb_s(65)    = 2
      kmcb_x(65)    = 4
      kmcs_r(65,1,0) = 3
      kmcs_r(65,2,0) = 4
      kmcs_r(65,3,0) = 5
      kmcs_r(65,4,0) = 6
      kmcs_r(65,1,1) = 3
      kmcs_r(65,2,1) = 6
      kmcs_r(65,3,1) = 4
      kmcs_r(65,4,1) = 5

* particle name
      kmpr_c(66,1) = 'electron'
      kmpr_l(66,1) = 8
      kmpr_c(66,2) = 'positron'
      kmpr_l(66,2) = 8
      kmpr_c(66,3) = 'd'
      kmpr_l(66,3) = 1
      kmpr_c(66,4) = 'd-bar'
      kmpr_l(66,4) = 5
      kmpr_c(66,5) = 'd'
      kmpr_l(66,5) = 1
      kmpr_c(66,6) = 'd-bar'
      kmpr_l(66,6) = 5
* masses of external particles
      amas_1(66,1) = 11
      amas_1(66,2) = 11
      amas_1(66,3) = 1
      amas_1(66,4) = 1
      amas_1(66,5) = 1
      amas_1(66,6) = 1

* Charge*3
      kcha_g(66,1) =  -3
      kcha_g(66,2) =   3
      kcha_g(66,3) =  -1
      kcha_g(66,4) =   1
      kcha_g(66,5) =  -1
      kcha_g(66,6) =   1

* KFcode
      kfco_e(66,1) =  11
      kfco_e(66,2) = -11
      kfco_e(66,3) =   1
      kfco_e(66,4) =  -1
      kfco_e(66,5) =   1
      kfco_e(66,6) =  -1
* weight for the color base (qqqq)
      cf_mtx(66,0,0)= 9.d0
      cf_mtx(66,0,1)= 3.d0
      cf_mtx(66,1,0)= 3.d0
      cf_mtx(66,1,1)= 9.d0

      kmcb_s(66)    = 2
      kmcb_x(66)    = 4
      kmcs_r(66,1,0) = 3
      kmcs_r(66,2,0) = 4
      kmcs_r(66,3,0) = 5
      kmcs_r(66,4,0) = 6
      kmcs_r(66,1,1) = 3
      kmcs_r(66,2,1) = 6
      kmcs_r(66,3,1) = 4
      kmcs_r(66,4,1) = 5

* particle name
      kmpr_c(67,1) = 'electron'
      kmpr_l(67,1) = 8
      kmpr_c(67,2) = 'positron'
      kmpr_l(67,2) = 8
      kmpr_c(67,3) = 's'
      kmpr_l(67,3) = 1
      kmpr_c(67,4) = 's-bar'
      kmpr_l(67,4) = 5
      kmpr_c(67,5) = 's'
      kmpr_l(67,5) = 1
      kmpr_c(67,6) = 's-bar'
      kmpr_l(67,6) = 5
* masses of external particles
      amas_1(67,1) = 11
      amas_1(67,2) = 11
      amas_1(67,3) = 3
      amas_1(67,4) = 3
      amas_1(67,5) = 3
      amas_1(67,6) = 3

* Charge*3
      kcha_g(67,1) =  -3
      kcha_g(67,2) =   3
      kcha_g(67,3) =  -1
      kcha_g(67,4) =   1
      kcha_g(67,5) =  -1
      kcha_g(67,6) =   1

* KFcode
      kfco_e(67,1) =  11
      kfco_e(67,2) = -11
      kfco_e(67,3) =   3
      kfco_e(67,4) =  -3
      kfco_e(67,5) =   3
      kfco_e(67,6) =  -3
* weight for the color base (qqqq)
      cf_mtx(67,0,0)= 9.d0
      cf_mtx(67,0,1)= 3.d0
      cf_mtx(67,1,0)= 3.d0
      cf_mtx(67,1,1)= 9.d0

      kmcb_s(67)    = 2
      kmcb_x(67)    = 4
      kmcs_r(67,1,0) = 3
      kmcs_r(67,2,0) = 4
      kmcs_r(67,3,0) = 5
      kmcs_r(67,4,0) = 6
      kmcs_r(67,1,1) = 3
      kmcs_r(67,2,1) = 6
      kmcs_r(67,3,1) = 4
      kmcs_r(67,4,1) = 5

* particle name
      kmpr_c(68,1) = 'electron'
      kmpr_l(68,1) = 8
      kmpr_c(68,2) = 'positron'
      kmpr_l(68,2) = 8
      kmpr_c(68,3) = 'b'
      kmpr_l(68,3) = 1
      kmpr_c(68,4) = 'b-bar'
      kmpr_l(68,4) = 5
      kmpr_c(68,5) = 'b'
      kmpr_l(68,5) = 1
      kmpr_c(68,6) = 'b-bar'
      kmpr_l(68,6) = 5
* masses of external particles
      amas_1(68,1) = 11
      amas_1(68,2) = 11
      amas_1(68,3) = 5
      amas_1(68,4) = 5
      amas_1(68,5) = 5
      amas_1(68,6) = 5

* Charge*3
      kcha_g(68,1) =  -3
      kcha_g(68,2) =   3
      kcha_g(68,3) =  -1
      kcha_g(68,4) =   1
      kcha_g(68,5) =  -1
      kcha_g(68,6) =   1

* KFcode
      kfco_e(68,1) =  11
      kfco_e(68,2) = -11
      kfco_e(68,3) =   5
      kfco_e(68,4) =  -5
      kfco_e(68,5) =   5
      kfco_e(68,6) =  -5
* weight for the color base (qqqq)
      cf_mtx(68,0,0)= 9.d0
      cf_mtx(68,0,1)= 3.d0
      cf_mtx(68,1,0)= 3.d0
      cf_mtx(68,1,1)= 9.d0

      kmcb_s(68)    = 2
      kmcb_x(68)    = 4
      kmcs_r(68,1,0) = 3
      kmcs_r(68,2,0) = 4
      kmcs_r(68,3,0) = 5
      kmcs_r(68,4,0) = 6
      kmcs_r(68,1,1) = 3
      kmcs_r(68,2,1) = 6
      kmcs_r(68,3,1) = 4
      kmcs_r(68,4,1) = 5

* particle name
      kmpr_c(69,1) = 'electron'
      kmpr_l(69,1) = 8
      kmpr_c(69,2) = 'positron'
      kmpr_l(69,2) = 8
      kmpr_c(69,3) = 'u'
      kmpr_l(69,3) = 1
      kmpr_c(69,4) = 'u-bar'
      kmpr_l(69,4) = 5
      kmpr_c(69,5) = 'c'
      kmpr_l(69,5) = 1
      kmpr_c(69,6) = 'c-bar'
      kmpr_l(69,6) = 5
* masses of external particles
      amas_1(69,1) = 11
      amas_1(69,2) = 11
      amas_1(69,3) = 2
      amas_1(69,4) = 2
      amas_1(69,5) = 4
      amas_1(69,6) = 4

* Charge*3
      kcha_g(69,1) =  -3
      kcha_g(69,2) =   3
      kcha_g(69,3) =   2
      kcha_g(69,4) =  -2
      kcha_g(69,5) =   2
      kcha_g(69,6) =  -2

* KFcode
      kfco_e(69,1) =  11
      kfco_e(69,2) = -11
      kfco_e(69,3) =   2
      kfco_e(69,4) =  -2
      kfco_e(69,5) =   4
      kfco_e(69,6) =  -4
* weight for the color base (qqqq)
      cf_mtx(69,0,0)= 9.d0
      cf_mtx(69,0,1)= 3.d0
      cf_mtx(69,1,0)= 3.d0
      cf_mtx(69,1,1)= 9.d0

      kmcb_s(69)    = 2
      kmcb_x(69)    = 4
      kmcs_r(69,1,0) = 3
      kmcs_r(69,2,0) = 4
      kmcs_r(69,3,0) = 5
      kmcs_r(69,4,0) = 6
      kmcs_r(69,1,1) = 3
      kmcs_r(69,2,1) = 6
      kmcs_r(69,3,1) = 4
      kmcs_r(69,4,1) = 5

* particle name
      kmpr_c(70,1) = 'electron'
      kmpr_l(70,1) = 8
      kmpr_c(70,2) = 'positron'
      kmpr_l(70,2) = 8
      kmpr_c(70,3) = 'u'
      kmpr_l(70,3) = 1
      kmpr_c(70,4) = 'u-bar'
      kmpr_l(70,4) = 5
      kmpr_c(70,5) = 's'
      kmpr_l(70,5) = 1
      kmpr_c(70,6) = 's-bar'
      kmpr_l(70,6) = 5
* masses of external particles
      amas_1(70,1) = 11
      amas_1(70,2) = 11
      amas_1(70,3) = 2
      amas_1(70,4) = 2
      amas_1(70,5) = 3
      amas_1(70,6) = 3

* Charge*3
      kcha_g(70,1) =  -3
      kcha_g(70,2) =   3
      kcha_g(70,3) =   2
      kcha_g(70,4) =  -2
      kcha_g(70,5) =  -1
      kcha_g(70,6) =   1

* KFcode
      kfco_e(70,1) =  11
      kfco_e(70,2) = -11
      kfco_e(70,3) =   2
      kfco_e(70,4) =  -2
      kfco_e(70,5) =   3
      kfco_e(70,6) =  -3
* weight for the color base (qqqq)
      cf_mtx(70,0,0)= 9.d0
      cf_mtx(70,0,1)= 3.d0
      cf_mtx(70,1,0)= 3.d0
      cf_mtx(70,1,1)= 9.d0

      kmcb_s(70)    = 2
      kmcb_x(70)    = 4
      kmcs_r(70,1,0) = 3
      kmcs_r(70,2,0) = 4
      kmcs_r(70,3,0) = 5
      kmcs_r(70,4,0) = 6
      kmcs_r(70,1,1) = 3
      kmcs_r(70,2,1) = 6
      kmcs_r(70,3,1) = 4
      kmcs_r(70,4,1) = 5

* particle name
      kmpr_c(71,1) = 'electron'
      kmpr_l(71,1) = 8
      kmpr_c(71,2) = 'positron'
      kmpr_l(71,2) = 8
      kmpr_c(71,3) = 'u'
      kmpr_l(71,3) = 1
      kmpr_c(71,4) = 'u-bar'
      kmpr_l(71,4) = 5
      kmpr_c(71,5) = 'b'
      kmpr_l(71,5) = 1
      kmpr_c(71,6) = 'b-bar'
      kmpr_l(71,6) = 5
* masses of external particles
      amas_1(71,1) = 11
      amas_1(71,2) = 11
      amas_1(71,3) = 2
      amas_1(71,4) = 2
      amas_1(71,5) = 5
      amas_1(71,6) = 5

* Charge*3
      kcha_g(71,1) =  -3
      kcha_g(71,2) =   3
      kcha_g(71,3) =   2
      kcha_g(71,4) =  -2
      kcha_g(71,5) =  -1
      kcha_g(71,6) =   1

* KFcode
      kfco_e(71,1) =  11
      kfco_e(71,2) = -11
      kfco_e(71,3) =   2
      kfco_e(71,4) =  -2
      kfco_e(71,5) =   5
      kfco_e(71,6) =  -5
* weight for the color base (qqqq)
      cf_mtx(71,0,0)= 9.d0
      cf_mtx(71,0,1)= 3.d0
      cf_mtx(71,1,0)= 3.d0
      cf_mtx(71,1,1)= 9.d0

      kmcb_s(71)    = 2
      kmcb_x(71)    = 4
      kmcs_r(71,1,0) = 3
      kmcs_r(71,2,0) = 4
      kmcs_r(71,3,0) = 5
      kmcs_r(71,4,0) = 6
      kmcs_r(71,1,1) = 3
      kmcs_r(71,2,1) = 6
      kmcs_r(71,3,1) = 4
      kmcs_r(71,4,1) = 5

* particle name
      kmpr_c(72,1) = 'electron'
      kmpr_l(72,1) = 8
      kmpr_c(72,2) = 'positron'
      kmpr_l(72,2) = 8
      kmpr_c(72,3) = 'c'
      kmpr_l(72,3) = 1
      kmpr_c(72,4) = 'c-bar'
      kmpr_l(72,4) = 5
      kmpr_c(72,5) = 'd'
      kmpr_l(72,5) = 1
      kmpr_c(72,6) = 'd-bar'
      kmpr_l(72,6) = 5
* masses of external particles
      amas_1(72,1) = 11
      amas_1(72,2) = 11
      amas_1(72,3) = 4
      amas_1(72,4) = 4
      amas_1(72,5) = 1
      amas_1(72,6) = 1

* Charge*3
      kcha_g(72,1) =  -3
      kcha_g(72,2) =   3
      kcha_g(72,3) =   2
      kcha_g(72,4) =  -2
      kcha_g(72,5) =  -1
      kcha_g(72,6) =   1

* KFcode
      kfco_e(72,1) =  11
      kfco_e(72,2) = -11
      kfco_e(72,3) =   4
      kfco_e(72,4) =  -4
      kfco_e(72,5) =   1
      kfco_e(72,6) =  -1
* weight for the color base (qqqq)
      cf_mtx(72,0,0)= 9.d0
      cf_mtx(72,0,1)= 3.d0
      cf_mtx(72,1,0)= 3.d0
      cf_mtx(72,1,1)= 9.d0

      kmcb_s(72)    = 2
      kmcb_x(72)    = 4
      kmcs_r(72,1,0) = 3
      kmcs_r(72,2,0) = 4
      kmcs_r(72,3,0) = 5
      kmcs_r(72,4,0) = 6
      kmcs_r(72,1,1) = 3
      kmcs_r(72,2,1) = 6
      kmcs_r(72,3,1) = 4
      kmcs_r(72,4,1) = 5

* particle name
      kmpr_c(73,1) = 'electron'
      kmpr_l(73,1) = 8
      kmpr_c(73,2) = 'positron'
      kmpr_l(73,2) = 8
      kmpr_c(73,3) = 'c'
      kmpr_l(73,3) = 1
      kmpr_c(73,4) = 'c-bar'
      kmpr_l(73,4) = 5
      kmpr_c(73,5) = 'b'
      kmpr_l(73,5) = 1
      kmpr_c(73,6) = 'b-bar'
      kmpr_l(73,6) = 5
* masses of external particles
      amas_1(73,1) = 11
      amas_1(73,2) = 11
      amas_1(73,3) = 4
      amas_1(73,4) = 4
      amas_1(73,5) = 5
      amas_1(73,6) = 5

* Charge*3
      kcha_g(73,1) =  -3
      kcha_g(73,2) =   3
      kcha_g(73,3) =   2
      kcha_g(73,4) =  -2
      kcha_g(73,5) =  -1
      kcha_g(73,6) =   1

* KFcode
      kfco_e(73,1) =  11
      kfco_e(73,2) = -11
      kfco_e(73,3) =   4
      kfco_e(73,4) =  -4
      kfco_e(73,5) =   5
      kfco_e(73,6) =  -5
* weight for the color base (qqqq)
      cf_mtx(73,0,0)= 9.d0
      cf_mtx(73,0,1)= 3.d0
      cf_mtx(73,1,0)= 3.d0
      cf_mtx(73,1,1)= 9.d0

      kmcb_s(73)    = 2
      kmcb_x(73)    = 4
      kmcs_r(73,1,0) = 3
      kmcs_r(73,2,0) = 4
      kmcs_r(73,3,0) = 5
      kmcs_r(73,4,0) = 6
      kmcs_r(73,1,1) = 3
      kmcs_r(73,2,1) = 6
      kmcs_r(73,3,1) = 4
      kmcs_r(73,4,1) = 5

* particle name
      kmpr_c(74,1) = 'electron'
      kmpr_l(74,1) = 8
      kmpr_c(74,2) = 'positron'
      kmpr_l(74,2) = 8
      kmpr_c(74,3) = 'd'
      kmpr_l(74,3) = 1
      kmpr_c(74,4) = 'd-bar'
      kmpr_l(74,4) = 5
      kmpr_c(74,5) = 's'
      kmpr_l(74,5) = 1
      kmpr_c(74,6) = 's-bar'
      kmpr_l(74,6) = 5
* masses of external particles
      amas_1(74,1) = 11
      amas_1(74,2) = 11
      amas_1(74,3) = 1
      amas_1(74,4) = 1
      amas_1(74,5) = 3
      amas_1(74,6) = 3

* Charge*3
      kcha_g(74,1) =  -3
      kcha_g(74,2) =   3
      kcha_g(74,3) =  -1
      kcha_g(74,4) =   1
      kcha_g(74,5) =  -1
      kcha_g(74,6) =   1

* KFcode
      kfco_e(74,1) =  11
      kfco_e(74,2) = -11
      kfco_e(74,3) =   1
      kfco_e(74,4) =  -1
      kfco_e(74,5) =   3
      kfco_e(74,6) =  -3
* weight for the color base (qqqq)
      cf_mtx(74,0,0)= 9.d0
      cf_mtx(74,0,1)= 3.d0
      cf_mtx(74,1,0)= 3.d0
      cf_mtx(74,1,1)= 9.d0

      kmcb_s(74)    = 2
      kmcb_x(74)    = 4
      kmcs_r(74,1,0) = 3
      kmcs_r(74,2,0) = 4
      kmcs_r(74,3,0) = 5
      kmcs_r(74,4,0) = 6
      kmcs_r(74,1,1) = 3
      kmcs_r(74,2,1) = 6
      kmcs_r(74,3,1) = 4
      kmcs_r(74,4,1) = 5

* particle name
      kmpr_c(75,1) = 'electron'
      kmpr_l(75,1) = 8
      kmpr_c(75,2) = 'positron'
      kmpr_l(75,2) = 8
      kmpr_c(75,3) = 'd'
      kmpr_l(75,3) = 1
      kmpr_c(75,4) = 'd-bar'
      kmpr_l(75,4) = 5
      kmpr_c(75,5) = 'b'
      kmpr_l(75,5) = 1
      kmpr_c(75,6) = 'b-bar'
      kmpr_l(75,6) = 5
* masses of external particles
      amas_1(75,1) = 11
      amas_1(75,2) = 11
      amas_1(75,3) = 1
      amas_1(75,4) = 1
      amas_1(75,5) = 5
      amas_1(75,6) = 5

* Charge*3
      kcha_g(75,1) =  -3
      kcha_g(75,2) =   3
      kcha_g(75,3) =  -1
      kcha_g(75,4) =   1
      kcha_g(75,5) =  -1
      kcha_g(75,6) =   1

* KFcode
      kfco_e(75,1) =  11
      kfco_e(75,2) = -11
      kfco_e(75,3) =   1
      kfco_e(75,4) =  -1
      kfco_e(75,5) =   5
      kfco_e(75,6) =  -5
* weight for the color base (qqqq)
      cf_mtx(75,0,0)= 9.d0
      cf_mtx(75,0,1)= 3.d0
      cf_mtx(75,1,0)= 3.d0
      cf_mtx(75,1,1)= 9.d0

      kmcb_s(75)    = 2
      kmcb_x(75)    = 4
      kmcs_r(75,1,0) = 3
      kmcs_r(75,2,0) = 4
      kmcs_r(75,3,0) = 5
      kmcs_r(75,4,0) = 6
      kmcs_r(75,1,1) = 3
      kmcs_r(75,2,1) = 6
      kmcs_r(75,3,1) = 4
      kmcs_r(75,4,1) = 5

* particle name
      kmpr_c(76,1) = 'electron'
      kmpr_l(76,1) = 8
      kmpr_c(76,2) = 'positron'
      kmpr_l(76,2) = 8
      kmpr_c(76,3) = 's'
      kmpr_l(76,3) = 1
      kmpr_c(76,4) = 's-bar'
      kmpr_l(76,4) = 5
      kmpr_c(76,5) = 'b'
      kmpr_l(76,5) = 1
      kmpr_c(76,6) = 'b-bar'
      kmpr_l(76,6) = 5
* masses of external particles
      amas_1(76,1) = 11
      amas_1(76,2) = 11
      amas_1(76,3) = 3
      amas_1(76,4) = 3
      amas_1(76,5) = 5
      amas_1(76,6) = 5

* Charge*3
      kcha_g(76,1) =  -3
      kcha_g(76,2) =   3
      kcha_g(76,3) =  -1
      kcha_g(76,4) =   1
      kcha_g(76,5) =  -1
      kcha_g(76,6) =   1

* KFcode
      kfco_e(76,1) =  11
      kfco_e(76,2) = -11
      kfco_e(76,3) =   3
      kfco_e(76,4) =  -3
      kfco_e(76,5) =   5
      kfco_e(76,6) =  -5
* weight for the color base (qqqq)
      cf_mtx(76,0,0)= 9.d0
      cf_mtx(76,0,1)= 3.d0
      cf_mtx(76,1,0)= 3.d0
      cf_mtx(76,1,1)= 9.d0

      kmcb_s(76)    = 2
      kmcb_x(76)    = 4
      kmcs_r(76,1,0) = 3
      kmcs_r(76,2,0) = 4
      kmcs_r(76,3,0) = 5
      kmcs_r(76,4,0) = 6
      kmcs_r(76,1,1) = 3
      kmcs_r(76,2,1) = 6
      kmcs_r(76,3,1) = 4
      kmcs_r(76,4,1) = 5
 
* particle name
      kmpr_c(1,1) = 'electron'
      kmpr_l(1,1) = 8
      kmpr_c(1,2) = 'positron'
      kmpr_l(1,2) = 8
      kmpr_c(1,3) = 'electron'
      kmpr_l(1,3) = 8
      kmpr_c(1,4) = 'positron'
      kmpr_l(1,4) = 8
      kmpr_c(1,5) = 'nu-e'
      kmpr_l(1,5) = 4
      kmpr_c(1,6) = 'nu-e-bar'
      kmpr_l(1,6) = 8
* masses of external particles
      amas_1(1,1) = 11
      amas_1(1,2) = 11
      amas_1(1,3) = 11
      amas_1(1,4) = 11
      amas_1(1,5) = 12
      amas_1(1,6) = 12

* Charge*3
      kcha_g(1,1) =  -3
      kcha_g(1,2) =   3
      kcha_g(1,3) =  -3
      kcha_g(1,4) =   3
      kcha_g(1,5) =   0
      kcha_g(1,6) =   0

* KFcode
      kfco_e(1,1) =  11
      kfco_e(1,2) = -11
      kfco_e(1,3) =  11
      kfco_e(1,4) = -11
      kfco_e(1,5) =  12
      kfco_e(1,6) = -12
* weight for the color base (llll)
      cf_mtx(1,0,0) = 1.d0

      kmcb_s(1)    = 1
      kmcb_x(1)    = 0

*========
      do 999   ip = 1 , MPROC
      do 999 iset = 1 , 20
      do 999   ig = 1 , NGRAPH
         kdiagr(ip,iset,ig) = 0
  999 continue

C      kdiagr( p, 1, g) .... WW(CC03)
C      kdiagr( p, 2, g) .... ZZ(NC02)
C      kdiagr( p, 3, g) .... Wln without CC03
C      kdiagr( p, 4, g) .... Zll without gg/gZ/ZZ
C      kdiagr( p, 5, g) .... Znn/gnn without gg/gz/ZZ
C      kdiagr( p, 6, g) .... gg 
C      kdiagr( p, 7, g) .... s-ch
C      kdiagr( p, 8, g) .... t-ch
C      kdiagr( p, 9, g) .... Multi-periph + Bremss (gg 6 diagrams) 
C      kdiagr( p,10, g) .... Multi-periph only (gg)
C      kdiagr( p,11, g) .... All but Multi-periph (gg)
C      kdiagr( p,12, g) .... ZZ/Zg/gg (NC08)
C      kdiagr( p,13, g) ....  kdiagr(p,3,g) Wln without CC03
C                            +kdiagr(p,1,g) WW(CC03)
C      kdiagr( p,14, g) ....  kdiagr(p,4,g) + kdiagr(p,12,g)
C      kdiagr( p,15, g) ....  kdiagr(p,5,g) + kdiagr(p,12,g)
C      kdiagr( p,16, g) .... two-photon Annihilation
C      kdiagr( p,17, g) .... two-photon Conversion
C      kdiagr( p,18, g) .... two-photon Bremsstrahlung
C      kdiagr( p,19, g) .... two-photon Multi-peripheral
C -----------------------------------------------------------------
C  1  [56] electron,positron,nu-e,nu-e-bar
          kdiagr( 1, 1,13) = 1
          kdiagr( 1, 1,14) = 1
          kdiagr( 1, 1,49) = 1

          kdiagr( 1, 2,48) = 1
          kdiagr( 1, 2,51) = 1

          kdiagr( 1,12,48) = 1
c - bug Apr.8,1999 RT
c         kdiagr( 1,12,49) = 1
          kdiagr( 1,12,47) = 1
          kdiagr( 1,12,50) = 1
          kdiagr( 1,12,51) = 1

          kdiagr( 1, 3, 3) = 1
          kdiagr( 1, 3, 4) = 1
          kdiagr( 1, 3, 7) = 1
          kdiagr( 1, 3, 8) = 1
          kdiagr( 1, 3, 9) = 1
          kdiagr( 1, 3,11) = 1
          kdiagr( 1, 3,17) = 1
          kdiagr( 1, 3,18) = 1
          kdiagr( 1, 3,25) = 1
          kdiagr( 1, 3,27) = 1
          kdiagr( 1, 3,28) = 1
          kdiagr( 1, 3,29) = 1
          kdiagr( 1, 3,37) = 1
          kdiagr( 1, 3,38) = 1
          kdiagr( 1, 3,43) = 1
          kdiagr( 1, 3,44) = 1
          kdiagr( 1, 3,45) = 1
          kdiagr( 1, 3,54) = 1

*single W + WW
          kdiagr( 1,13,17) = 1
*not yet
* Zll 
          kdiagr( 1, 4, 1) = 1
          kdiagr( 1, 4, 2) = 1
          kdiagr( 1, 4, 5) = 1
          kdiagr( 1, 4, 6) = 1
          kdiagr( 1, 4,15) = 1
          kdiagr( 1, 4,16) = 1
          kdiagr( 1, 4,19) = 1
          kdiagr( 1, 4,20) = 1
          kdiagr( 1, 4,41) = 1
          kdiagr( 1, 4,42) = 1
          kdiagr( 1, 4,55) = 1
          kdiagr( 1, 4,56) = 1
* Znn
          kdiagr( 1, 5,10) = 1
          kdiagr( 1, 5,12) = 1
          kdiagr( 1, 5,30) = 1
          kdiagr( 1, 5,39) = 1
          kdiagr( 1, 5,46) = 1
          kdiagr( 1, 5,53) = 1
* Gnn
          kdiagr( 1, 5,33) = 1
          kdiagr( 1, 5,40) = 1
          kdiagr( 1, 5,52) = 1

C  2  [18] electron,nu-e-bar,anti-muon,nu-mu
C  3  [18] electron,nu-e-bar,anti-tau,nu-tau (same as #2)
      do 1002 ip = 2 , 3
         kdiagr(ip, 1, 7) = 1
         kdiagr(ip, 1, 8) = 1
         kdiagr(ip, 1,18) = 1
      do 2010 ig =  1 , 18
         kdiagr(ip, 3, ig) = 1
 2010 continue
         kdiagr(ip, 3, 7) = 0
         kdiagr(ip, 3, 8) = 0
         kdiagr(ip, 3,12) = 0
         kdiagr(ip, 3,13) = 0
         kdiagr(ip, 3,14) = 0
         kdiagr(ip, 3,18) = 0

         kdiagr(ip,13, 9) = 1
         kdiagr(ip,13,12) = 1
         kdiagr(ip,13,15) = 1

 1002 continue
C  4  [20] nu-mu,anti-muon,muon,nu-mu-bar
C  5  [20] nu-tau,anti-tau,tau,nu-tau-bar (same as #4)
      do 1004 ip = 4 , 5
         kdiagr(ip, 1,13) = 1
         kdiagr(ip, 1,14) = 1
         kdiagr(ip, 1,20) = 1
         kdiagr(ip, 2,17) = 1
         kdiagr(ip, 2,19) = 1
         kdiagr(ip,12,16) = 1
         kdiagr(ip,12,17) = 1
         kdiagr(ip,12,18) = 1
         kdiagr(ip,12,19) = 1

         kdiagr(ip, 3, 1) = 1
         kdiagr(ip, 3, 3) = 1
         kdiagr(ip, 3, 4) = 1
         kdiagr(ip, 3, 9) = 1
         kdiagr(ip, 3,10) = 1
         kdiagr(ip, 3,12) = 1

         kdiagr(ip, 4, 5) = 1
         kdiagr(ip, 4, 6) = 1
         kdiagr(ip, 4, 7) = 1
         kdiagr(ip, 4, 8) = 1
* Znn
         kdiagr(ip, 5, 2) = 1
         kdiagr(ip, 5,11) = 1
 1004 continue
C  6  [ 9] muon,nu-mu-bar,anti-tau,nu-tau
         kdiagr( 6, 1, 7) = 1
         kdiagr( 6, 1, 8) = 1
         kdiagr( 6, 1, 9) = 1
      do 3110 ig =  1 , 9
         kdiagr( 6, 3, ig) = 1
 3110 continue
         kdiagr( 6, 3, 7) = 0
         kdiagr( 6, 3, 8) = 0
         kdiagr( 6, 3, 9) = 0

C  7 [144] electron,positron,electron,positron
         kdiagr( 7, 2,116) = 1
         kdiagr( 7, 2,120) = 1
         kdiagr( 7, 2,124) = 1
         kdiagr( 7, 2,128) = 1

         kdiagr( 7, 6,113) = 1
         kdiagr( 7, 6,117) = 1
         kdiagr( 7, 6,121) = 1
         kdiagr( 7, 6,125) = 1

         kdiagr( 7, 9, 33) = 1
         kdiagr( 7, 9, 37) = 1
         kdiagr( 7, 9, 41) = 1
         kdiagr( 7, 9, 45) = 1
         kdiagr( 7, 9, 49) = 1
         kdiagr( 7, 9, 53) = 1
         kdiagr( 7, 9, 57) = 1
         kdiagr( 7, 9, 61) = 1
         kdiagr( 7, 9, 65) = 1
         kdiagr( 7, 9, 69) = 1
         kdiagr( 7, 9, 73) = 1
         kdiagr( 7, 9, 77) = 1
         kdiagr( 7, 9, 81) = 1
         kdiagr( 7, 9, 85) = 1
         kdiagr( 7, 9, 89) = 1
         kdiagr( 7, 9, 93) = 1
         kdiagr( 7, 9, 97) = 1
         kdiagr( 7, 9,101) = 1
         kdiagr( 7, 9,105) = 1
         kdiagr( 7, 9,109) = 1
         kdiagr( 7, 9,129) = 1
         kdiagr( 7, 9,133) = 1
         kdiagr( 7, 9,137) = 1
         kdiagr( 7, 9,141) = 1

         kdiagr( 7,10, 45) = 1
         kdiagr( 7,10, 49) = 1
         kdiagr( 7,10, 53) = 1
         kdiagr( 7,10, 61) = 1
         kdiagr( 7,10, 73) = 1
         kdiagr( 7,10, 77) = 1
         kdiagr( 7,10, 85) = 1
         kdiagr( 7,10, 93) = 1

      do 7001 ig = 113, 128
         kdiagr( 7,12, ig) = 1
 7001 continue

c   - Zll
      do 7002 ig = 2,44,2
         kdiagr( 7, 4, ig) = 1
 7002 continue
         kdiagr( 7, 4, 58) = 1
         kdiagr( 7, 4, 60) = 1
      do 7003 ig = 66,72,2
         kdiagr( 7, 4, ig) = 1
 7003 continue
         kdiagr( 7, 4, 82) = 1
         kdiagr( 7, 4, 84) = 1
         kdiagr( 7, 4, 90) = 1
         kdiagr( 7, 4, 92) = 1
      do 7004 ig = 98,112,2
         kdiagr( 7, 4, ig) = 1
 7004 continue
         kdiagr( 7, 4,131) = 1
         kdiagr( 7, 4,132) = 1
         kdiagr( 7, 4,135) = 1
         kdiagr( 7, 4,136) = 1
         kdiagr( 7, 4,139) = 1
         kdiagr( 7, 4,140) = 1
         kdiagr( 7, 4,143) = 1
         kdiagr( 7, 4,144) = 1

c   - s-ch
      do ig =   1, 32
         kdiagr( 7, 7, ig) = 1
      end do   
      do ig = 113,128
         kdiagr( 7, 7, ig) = 1
      end do   

c   - t-ch
      do ig =  33, 44
         kdiagr( 7, 8, ig) = 1
      end do   
      do ig =  57, 60
         kdiagr( 7, 8, ig) = 1
      end do   
      do ig =  65, 72
         kdiagr( 7, 8, ig) = 1
      end do   
      do ig =  81, 84
         kdiagr( 7, 8, ig) = 1
      end do   
      do ig =  89, 92
         kdiagr( 7, 8, ig) = 1
      end do   
      do ig =  97,112
         kdiagr( 7, 8, ig) = 1
      end do   
      do ig = 129,144
         kdiagr( 7, 8, ig) = 1
      end do   
*
      do ig =  45, 56
         kdiagr( 7, 8, ig) = 1
      end do   
      do ig =  61, 64
         kdiagr( 7, 8, ig) = 1
      end do   
      do ig =  73, 80
         kdiagr( 7, 8, ig) = 1
      end do   
      do ig =  85, 88
         kdiagr( 7, 8, ig) = 1
      end do   
      do ig =  93, 96
         kdiagr( 7, 8, ig) = 1
      end do   

c   - Annihilation
      do ig =   1, 32
         kdiagr( 7,16, ig) = 1
      end do   

c   - Conversion
      do ig =113,128
         kdiagr( 7,17, ig) = 1
      end do   

c   - Bremsstrahlung
      do ig =  33, 44
         kdiagr( 7,18, ig) = 1
      end do   
      do ig =  57, 60
         kdiagr( 7,18, ig) = 1
      end do   
      do ig =  65, 72
         kdiagr( 7,18, ig) = 1
      end do   
      do ig =  81, 84
         kdiagr( 7,18, ig) = 1
      end do   
      do ig =  89, 92
         kdiagr( 7,18, ig) = 1
      end do   
      do ig =  97,112
         kdiagr( 7,18, ig) = 1
      end do   
      do ig = 129,144
         kdiagr( 7,18, ig) = 1
      end do   

c   - Multi-peripheral
      do ig =  45, 56
         kdiagr( 7,19, ig) = 1
      end do   
      do ig =  61, 64
         kdiagr( 7,19, ig) = 1
      end do   
      do ig =  73, 80
         kdiagr( 7,19, ig) = 1
      end do   
      do ig =  85, 88
         kdiagr( 7,19, ig) = 1
      end do   
      do ig =  93, 96
         kdiagr( 7,19, ig) = 1
      end do   

c   - All, but Multi-peripheral
      do ig =   1,144
         kdiagr( 7,11, ig) = 1
      end do
      do ig =  45, 56
         kdiagr( 7,11, ig) = 0
      end do   
      do ig =  61, 64
         kdiagr( 7,11, ig) = 0
      end do   
      do ig =  73, 80
         kdiagr( 7,11, ig) = 0
      end do   
      do ig =  85, 88
         kdiagr( 7,11, ig) = 0
      end do   
      do ig =  93, 96
         kdiagr( 7,11, ig) = 0
      end do   

C  8  [50] electron,positron,muon,anti-muon
C  9  [50] electron,positron,tau,anti-tau (sam as #8)
      do 1008 ip = 8 , 9
         kdiagr(ip, 2,42) = 1
         kdiagr(ip, 2,46) = 1
         kdiagr(ip, 6,39) = 1
         kdiagr(ip, 6,43) = 1

         kdiagr(ip, 9,18) = 1
         kdiagr(ip, 9,22) = 1
         kdiagr(ip, 9,26) = 1
         kdiagr(ip, 9,30) = 1
         kdiagr(ip, 9,35) = 1
         kdiagr(ip, 9,47) = 1

         kdiagr(ip,10,26) = 1
         kdiagr(ip,10,30) = 1

      do 8001 ig = 39, 46
         kdiagr(ip,12, ig) = 1
 8001 continue

      do 8002 ig = 2,16,2
         kdiagr(ip, 4, ig) = 1
 8002 continue
         kdiagr(ip, 4, 19) = 1
         kdiagr(ip, 4, 21) = 1
         kdiagr(ip, 4, 23) = 1
         kdiagr(ip, 4, 25) = 1
         kdiagr(ip, 4, 36) = 1
         kdiagr(ip, 4, 38) = 1

c   - s-ch
         do ig =   1, 16
            kdiagr(ip, 7, ig) = 1
         end do   
         do ig =  39, 46
            kdiagr(ip, 7, ig) = 1
         end do   

c   - t-ch
         do ig =  18, 25
            kdiagr(ip, 8, ig) = 1
         end do   
         do ig =  35, 38
            kdiagr(ip, 8, ig) = 1
         end do   
         do ig =  47, 50
            kdiagr(ip, 8, ig) = 1
         end do   
*
         do ig =  26, 33
            kdiagr(ip, 8, ig) = 1
         end do   

c   - Annihilation
         do ig =   1, 16
            kdiagr(ip,16, ig) = 1
         end do   

c   - Conversion
         do ig =  39, 46
            kdiagr(ip,17, ig) = 1
         end do   

c   - Bremsstrahlung
         do ig =  18, 25
            kdiagr(ip,18, ig) = 1
         end do   
         do ig =  35, 38
            kdiagr(ip,18, ig) = 1
         end do   
         do ig =  47, 50
            kdiagr(ip,18, ig) = 1
         end do   

c   - Multi-peripheral
         do ig =  26, 33
            kdiagr(ip,19, ig) = 1
         end do   

c   - All, but Multi-peripheral
         do ig =   1, 50
            kdiagr(ip,11, ig) = 1
         end do
         do ig =  26, 33
            kdiagr(ip,11, ig) = 0
         end do   

 1008 continue

*HERE HERE
C 10  [68] muon,anti-muon,muon,anti-muon
C 11  [68] tau,anti-tau,tau,anti-tau
      do 1010 ip = 10 , 11
         kdiagr(ip, 2,56) = 1
         kdiagr(ip, 2,60) = 1
         kdiagr(ip, 2,64) = 1
         kdiagr(ip, 2,68) = 1
         kdiagr(ip, 6,53) = 1
         kdiagr(ip, 6,57) = 1
         kdiagr(ip, 6,61) = 1
         kdiagr(ip, 6,65) = 1
      do 1011 ig = 53, 68
         kdiagr(ip,12, ig) = 1
 1011 continue

      do 1111 ig = 2, 47, 3
         kdiagr(ip, 4, ig) = 1
 1111 continue

 1010 continue
C 12  [34] muon,anti-muon,tau,anti-tau
         kdiagr(12, 2,30) = 1
         kdiagr(12, 2,34) = 1
         kdiagr(12, 6,27) = 1
         kdiagr(12, 6,31) = 1
      do 1251 ig = 27, 34
         kdiagr(12,12, ig) = 1
 1251 continue
      do 1252 ig = 2, 23, 3
         kdiagr(12, 4, ig) = 1
 1252 continue

C 13  [20] electron,positron,nu-mu,nu-mu-bar
C 14  [20] electron,positron,nu-tau,nu-tau-bar
      do 1013 ip = 13 , 14
         kdiagr(ip, 2,16) = 1
         kdiagr(ip, 2,18) = 1
      do 1301 ig = 15, 18
         kdiagr(ip,12, ig) = 1
 1301 continue
      do 1302 ig = 1,4
         kdiagr(ip, 4, ig) = 1
 1302 continue
      do 1303 ig = 7,10
         kdiagr(ip, 4, ig) = 1
 1303 continue
         kdiagr(ip, 4, 13) = 1
         kdiagr(ip, 4, 14) = 1
         kdiagr(ip, 4, 19) = 1
         kdiagr(ip, 4, 20) = 1

         kdiagr(ip, 5,  5) = 1
         kdiagr(ip, 5,  6) = 1
 1013 continue
C 15  [21] nu-e,nu-e-bar,muon,anti-muon
C 16  [21] nu-e,nu-e-bar,tau,anti-tau
      do 1015 ip = 15 , 16
         kdiagr(ip, 2,17) = 1
         kdiagr(ip, 2,19) = 1
      do 1501 ig = 16, 19
         kdiagr(ip,12, ig) = 1
 1501 continue
         kdiagr(ip, 4,  3) = 1
         kdiagr(ip, 4,  4) = 1
         kdiagr(ip, 4,  5) = 1
         kdiagr(ip, 4,  6) = 1
*Znn
         kdiagr(ip, 5,  1) = 1
         kdiagr(ip, 5,  2) = 1
         kdiagr(ip, 5,  8) = 1
         kdiagr(ip, 5, 10) = 1
         kdiagr(ip, 5, 15) = 1
         kdiagr(ip, 5, 21) = 1
*Gnn
         kdiagr(ip, 5,  9) = 1
         kdiagr(ip, 5, 20) = 1
 1015 continue
C 17  [11] nu-tau,nu-tau-bar,muon,anti-muon
C 18  [11] nu-mu,nu-mu-bar,tau,anti-tau
      do 1017 ip = 17 , 18
         kdiagr(ip, 2, 9) = 1
         kdiagr(ip, 2,11) = 1
      do 1701 ig = 8, 11
         kdiagr(ip,12, ig) = 1
 1701 continue
         kdiagr(ip, 4,  3) = 1
         kdiagr(ip, 4,  4) = 1
         kdiagr(ip, 4,  5) = 1
         kdiagr(ip, 4,  6) = 1
*Znn
         kdiagr(ip, 5,  1) = 1
         kdiagr(ip, 5,  2) = 1
 1017 continue
C-----------------------------------------
C 19  [36] nu-e,nu-e-bar,nu-e,nu-e-bar
         kdiagr(19, 2,29) = 1
         kdiagr(19, 2,30) = 1
         kdiagr(19, 2,31) = 1
         kdiagr(19, 2,32) = 1
         kdiagr(19,12,29) = 1
         kdiagr(19,12,30) = 1
         kdiagr(19,12,31) = 1
         kdiagr(19,12,32) = 1
      do 1901 ig = 1, 11
         kdiagr(19, 5, ig) = 1
 1901 continue
         kdiagr(19, 5, 13) = 1
         kdiagr(19, 5, 15) = 1
         kdiagr(19, 5, 16) = 1
         kdiagr(19, 5, 17) = 1
         kdiagr(19, 5, 18) = 1
         kdiagr(19, 5, 19) = 1
         kdiagr(19, 5, 21) = 1
         kdiagr(19, 5, 23) = 1
         kdiagr(19, 5, 24) = 1
      do 1902 ig = 25, 28
         kdiagr(19, 5, ig) = 1
 1902 continue
      do 1903 ig = 33, 36
         kdiagr(19, 5, ig) = 1
 1903 continue
C-----------------------------------------
C 20  [12] nu-e,nu-e-bar,nu-mu,nu-mu-bar
C 21  [12] nu-e,nu-e-bar,nu-tau,nu-tau-bar
C 22  [12] nu-mu,nu-mu-bar,nu-mu,nu-mu-bar
C 23  [12] nu-tau,nu-tau-bar,nu-tau,nu-tau-bar
      do 1020 ip = 20 , 23
         kdiagr(ip, 2,10) = 1
         kdiagr(ip, 2,11) = 1
         kdiagr(ip,12,10) = 1
         kdiagr(ip,12,11) = 1
      do 2001 ig =  1 , 6
         kdiagr(ip, 5, ig) = 1
 2001 continue
         kdiagr(ip, 5,  8) = 1
         kdiagr(ip, 5,  9) = 1
         kdiagr(ip, 5, 12) = 1
 1020 continue
C-----------------------------------------
C 24   [6] nu-tau,nu-tau-bar,nu-mu,nu-mu-bar
      do 2401 ig =  1 , 4
         kdiagr(24, 5, ig) = 1
 2401 continue
         kdiagr(24, 2, 5) = 1
         kdiagr(24, 2, 6) = 1
         kdiagr(24,12, 5) = 1
         kdiagr(24,12, 6) = 1
C 25  [20] electron,nu-e-bar,u,d-bar
C 26  [20] electron,nu-e-bar,c,s-bar
      do 1025 ip = 25 , 26
         kdiagr(ip, 1, 8) = 1
         kdiagr(ip, 1, 9) = 1
         kdiagr(ip, 1,20) = 1
         kdiagr(ip, 3, 1) = 1
         kdiagr(ip, 3, 2) = 1
         kdiagr(ip, 3, 3) = 1
         kdiagr(ip, 3,10) = 1
         kdiagr(ip, 3,11) = 1
         kdiagr(ip, 3,12) = 1
         kdiagr(ip, 3,17) = 1
         kdiagr(ip, 3,18) = 1
         kdiagr(ip, 3,19) = 1
 1025 continue
C 27  [10] muon,nu-mu-bar,u,d-bar
C 28  [10] muon,nu-mu-bar,c,s-bar
C 29  [10] tau,nu-tau-bar,u,d-bar
C 30  [10] tau,nu-tau-bar,c,S-bar
      do 1027 ip = 27 , 30
         kdiagr(ip, 1, 8) = 1
         kdiagr(ip, 1, 9) = 1
         kdiagr(ip, 1,10) = 1
         kdiagr(ip, 3, 1) = 1
         kdiagr(ip, 3, 2) = 1
         kdiagr(ip, 3, 3) = 1
 1027 continue
C 31  [50] electron,positron,u,u-bar
C 32  [50] electron,positron,c,c-bar
C 33  [50] electron,positron,d,d-bar
C 34  [50] electron,positron,s,s-bar
C 35  [50] electron,positron,b,b-bar
      do 1031 ip = 31 , 35
         kdiagr(ip, 2,42) = 1
         kdiagr(ip, 2,46) = 1
         kdiagr(ip, 6,39) = 1
         kdiagr(ip, 6,43) = 1

         kdiagr(ip, 9,18) = 1
         kdiagr(ip, 9,22) = 1
         kdiagr(ip, 9,26) = 1
         kdiagr(ip, 9,30) = 1
         kdiagr(ip, 9,35) = 1
         kdiagr(ip, 9,47) = 1

         kdiagr(ip,10,26) = 1
         kdiagr(ip,10,30) = 1

      do 3101 ig = 39, 46
         kdiagr(ip,12, ig) = 1
 3101 continue

         kdiagr(ip, 4, 2) = 1
         kdiagr(ip, 4, 4) = 1
         kdiagr(ip, 4, 6) = 1
         kdiagr(ip, 4, 8) = 1
         kdiagr(ip, 4,19) = 1
         kdiagr(ip, 4,21) = 1
         kdiagr(ip, 4,23) = 1
         kdiagr(ip, 4,25) = 1
         kdiagr(ip, 4,36) = 1
         kdiagr(ip, 4,38) = 1
         kdiagr(ip, 4,49) = 1
         kdiagr(ip, 4,50) = 1

c   - s-ch
         do ig =   1, 16
            kdiagr(ip, 7, ig) = 1
         end do   
         do ig =  39, 46
            kdiagr(ip, 7, ig) = 1
         end do   

c   - t-ch
         do ig =  18, 25
            kdiagr(ip, 8, ig) = 1
         end do   
         do ig =  35, 38
            kdiagr(ip, 8, ig) = 1
         end do   
         do ig =  47, 50
            kdiagr(ip, 8, ig) = 1
         end do   
*
         do ig =  26, 33
            kdiagr(ip, 8, ig) = 1
         end do   

c   - Annihilation
         do ig =   1, 16
            kdiagr(ip,16, ig) = 1
         end do   

c   - Conversion
         do ig =  39, 46
            kdiagr(ip,17, ig) = 1
         end do   

c   - Bremsstrahlung
         do ig =  18, 25
            kdiagr(ip,18, ig) = 1
         end do   
         do ig =  35, 38
            kdiagr(ip,18, ig) = 1
         end do   
         do ig =  47, 50
            kdiagr(ip,18, ig) = 1
         end do   

c   - Multi-peripheral
         do ig =  26, 33
            kdiagr(ip,19, ig) = 1
         end do   

c   - All, but Multi-peripheral
         do ig =   1, 50
            kdiagr(ip,11, ig) = 1
         end do
         do ig =  26, 33
            kdiagr(ip,11, ig) = 0
         end do   

 1031 continue

C 36  [34] muon,anti-muon,u,u-bar
C 37  [34] muon,anti-muon,c,c-bar
C 38  [34] tau,anti-tau,u,u-bar
C 39  [34] tau,anti-tau,c,c-bar
C 40  [34] muon,anti-muon,d,d-bar
C 41  [34] muon,anti-muon,s,s-bar
C 42  [34] muon,anti-muon,b,b-bar
C 43  [34] tau,anti-tau,d,d-bar
C 44  [34] tau,anti-tau,s,s-bar
C 45  [34] tau,anti-tau,b,b-bar
      do 1036 ip = 36 , 45
         kdiagr(ip, 2,30) = 1
         kdiagr(ip, 2,34) = 1
         kdiagr(ip, 6,27) = 1
         kdiagr(ip, 6,31) = 1
      do 3601 ig = 27, 34
         kdiagr(ip,12, ig) = 1
 3601 continue
         kdiagr(ip, 4, 2) = 1
         kdiagr(ip, 4, 5) = 1
         kdiagr(ip, 4, 8) = 1
         kdiagr(ip, 4,11) = 1
 1036 continue
C 46  [21] nu-e,nu-e-bar,u,u-bar
C 47  [21] nu-e,nu-e-bar,c,c-bar
C 48  [21] nu-e,nu-e-bar,d,d-bar
C 49  [21] nu-e,nu-e-bar,s,s-bar
C 50  [21] nu-e,nu-e-bar,b,b-bar
      do 1046 ip = 46 , 50
         kdiagr(ip, 2,17) = 1
         kdiagr(ip, 2,19) = 1
      do 4601 ig = 16, 19
         kdiagr(ip,12, ig) = 1
 4601 continue
*Znn
         kdiagr(ip, 5,  1) = 1
         kdiagr(ip, 5,  2) = 1
         kdiagr(ip, 5,  8) = 1
         kdiagr(ip, 5, 10) = 1
         kdiagr(ip, 5, 15) = 1
         kdiagr(ip, 5, 21) = 1
*Gnn
         kdiagr(ip, 5,  9) = 1
         kdiagr(ip, 5, 20) = 1
 1046 continue
C 51  [11] nu-mu,nu-mu-bar,u,u-bar
C 52  [11] nu-mu,nu-mu-bar,c,c-bar
C 53  [11] nu-tau,nu-tau-bar,u,u-bar
C 54  [11] nu-tau,nu-tau-bar,c,c-bar
C 55  [11] nu-mu,nu-mu-bar,d,d-bar
C 56  [11] nu-mu,nu-mu-bar,s,s-bar
C 57  [11] nu-mu,nu-mu-bar,b,b-bar
C 58  [11] nu-tau,nu-tau-bar,d,d-bar
C 59  [11] nu-tau,nu-tau-bar,s,s-bar
C 60  [11] nu-tau,nu-tau-bar,b,b-bar
      do 1051 ip = 51 , 60
         kdiagr(ip, 2, 9) = 1
         kdiagr(ip, 2,11) = 1
      do 5101 ig = 8 , 11
         kdiagr(ip,12, ig) = 1
 5101 continue
         kdiagr(ip, 5,  1) = 1
         kdiagr(ip, 5,  2) = 1
 1051 continue
C 61  [53] u,d-bar,d,u-bar
C 62  [53] c,s-bar,s,c-bar
      do 1061 ip = 61 , 62
         kdiagr(ip, 1,41) = 1
         kdiagr(ip, 1,42) = 1
         kdiagr(ip, 1,53) = 1
         kdiagr(ip, 2,48) = 1
         kdiagr(ip, 2,52) = 1
         kdiagr(ip, 6,45) = 1
         kdiagr(ip, 6,49) = 1
      do 6101 ig = 45 , 52
         kdiagr(ip,12, ig) = 1
 6101 continue
 1061 continue
C 63  [11] u,d-bar,s,c-bar
      do 1063 ip = 63 , 63
         kdiagr(ip, 1, 9) = 1
         kdiagr(ip, 1,10) = 1
         kdiagr(ip, 1,11) = 1
 1063 continue
C 64  [84] u,u-bar,u,u-bar
C 65  [84] c,c-bar,c,c-bar
C 66  [84] d,d-bar,d,d-bar
C 67  [84] s,s-bar,s,s-bar
C 68  [84] b,b-bar,b,b-bar
      do 1064 ip = 64 , 68
         kdiagr(ip, 2,72) = 1
         kdiagr(ip, 2,76) = 1
         kdiagr(ip, 2,80) = 1
         kdiagr(ip, 2,84) = 1
         kdiagr(ip, 6,69) = 1
         kdiagr(ip, 6,73) = 1
         kdiagr(ip, 6,77) = 1
         kdiagr(ip, 6,81) = 1
      do 6401 ig = 69, 84
         kdiagr(ip,12, ig) = 1
 6401 continue
 1064 continue
C 69  [42] u,u-bar,c,c-bar
C 70  [42] u,u-bar,s,s-bar
C 71  [42] u,u-bar,b,b-bar
C 72  [42] c,c-bar,d,d-bar
C 73  [42] c,c-bar,b,b-bar
C 74  [42] d,d-bar,s,s-bar
C 75  [42] d,d-bar,b,b-bar
C 76  [42] s,s-bar,b,b-bar
      do 1069 ip = 69 , 76
         kdiagr(ip, 2,38) = 1
         kdiagr(ip, 2,42) = 1
         kdiagr(ip, 6,35) = 1
         kdiagr(ip, 6,39) = 1
      do 6901 ig = 35, 42
         kdiagr(ip,12, ig) = 1
 6901 continue
 1069 continue

*    def 11
* 11 : 10 -> 0    others -> 1
      do 1100 ip = 7 , 9
*        print *,'ngd',ip,ngd(ip)
         do 1100 ig = 1 , ngd(ip)
             if( kdiagr(ip,10,ig) .eq. 1 ) then
                 kdiagr(ip,11,ig) = 0
             else 
                 kdiagr(ip,11,ig) = 1
             endif
 1100 continue

      do 1110 ip = 31, 35
*        print *,'ngd',ip,ngd(ip)
         do 1110 ig = 1 , ngd(ip)
             if( kdiagr(ip,10,ig) .eq. 1 ) then
                 kdiagr(ip,11,ig) = 0
             else 
                 kdiagr(ip,11,ig) = 1
             endif
 1110 continue

C      kdiagr( p, 7, g) .... S chanel
C      kdiagr( p, 8, g) .... T chanel
      do 1200 ip = 1 , 76
      do 1200 ig = 1 , ngd(ip)
         kdiagr(ip, 7, ig) = 1
         kdiagr(ip, 8, ig) = 0
 1200 continue
      do 1201 ig = 15, 46
         kdiagr( 1, 7, ig) = 0
         kdiagr( 1, 8, ig) = 1
 1201 continue
c     do 1202 ig = 51, 56
      do 1202 ig = 52, 56
         kdiagr( 1, 7, ig) = 0
         kdiagr( 1, 8, ig) = 1
 1202 continue
      do 1203 ip = 2 , 3
      do 1203 ig = 9, 17
         kdiagr(ip, 7, ig) = 0
         kdiagr(ip, 8, ig) = 1
 1203 continue
      do 1204 ig = 33, 112
         kdiagr( 7, 7, ig) = 0
         kdiagr( 7, 8, ig) = 1
 1204 continue
      do 1205 ig = 129, 144
         kdiagr( 7, 7, ig) = 0
         kdiagr( 7, 8, ig) = 1
 1205 continue
      do 1208 ip = 8 , 9
      do 1206 ig =18, 38
         kdiagr(ip, 7, ig) = 0
         kdiagr(ip, 8, ig) = 1
 1206 continue
      do 1207 ig =47, 50
         kdiagr(ip, 7, ig) = 0
         kdiagr(ip, 8, ig) = 1
 1207 continue
 1208 continue
      do 1212 ip =13 ,14
      do 1210 ig = 7, 14
         kdiagr(ip, 7, ig) = 0
         kdiagr(ip, 8, ig) = 1
 1210 continue
      do 1211 ig =19, 20
         kdiagr(ip, 7, ig) = 0
         kdiagr(ip, 8, ig) = 1
 1211 continue
 1212 continue
      do 1215 ip =15 ,16
      do 1213 ig = 8, 15
         kdiagr(ip, 7, ig) = 0
         kdiagr(ip, 8, ig) = 1
 1213 continue
      do 1214 ig =20, 21
         kdiagr(ip, 7, ig) = 0
         kdiagr(ip, 8, ig) = 1
 1214 continue
 1215 continue
      do 1216 ig = 9, 28
         kdiagr(19, 7, ig) = 0
         kdiagr(19, 8, ig) = 1
 1216 continue
      do 1217 ig = 33,36
         kdiagr(19, 7, ig) = 0
         kdiagr(19, 8, ig) = 1
 1217 continue
      do 1220 ip =20 ,21
      do 1218 ig = 5,  9
         kdiagr(ip, 7, ig) = 0
         kdiagr(ip, 8, ig) = 1
 1218 continue
      do 1219 ig =12, 12
         kdiagr(ip, 7, ig) = 0
         kdiagr(ip, 8, ig) = 1
 1219 continue
 1220 continue

      do 1222 ip =25 ,26
      do 1221 ig =10, 19
         kdiagr(ip, 7, ig) = 0
         kdiagr(ip, 8, ig) = 1
 1221 continue
 1222 continue
      do 1225 ip =31, 35
      do 1223 ig =18, 38
         kdiagr(ip, 7, ig) = 0
         kdiagr(ip, 8, ig) = 1
 1223 continue
      do 1224 ig =47, 50
         kdiagr(ip, 7, ig) = 0
         kdiagr(ip, 8, ig) = 1
 1224 continue
 1225 continue
      do 1228 ip =46, 50
      do 1226 ig = 8, 15
         kdiagr(ip, 7, ig) = 0
         kdiagr(ip, 8, ig) = 1
 1226 continue
      do 1227 ig =20, 21
         kdiagr(ip, 7, ig) = 0
         kdiagr(ip, 8, ig) = 1
 1227 continue
 1228 continue
*****************
C      kdiagr( p,13, g) ....  kdiagr(p,3,g) + kdiagr(p,1,g)
C      kdiagr( p,14, g) ....  kdiagr(p,4,g) + kdiagr(p,12,g)
C      kdiagr( p,15, g) ....  kdiagr(p,5,g) + kdiagr(p,12,g)
*****************
      do 1399 ip = 1 , MPROC
      do 1399 ig = 1 , NGRAPH
         if( kdiagr(ip,1,ig) .eq. 1 .or. kdiagr(ip,3,ig) .eq. 1 ) then
             kdiagr(ip,13,ig) = 1
         endif
         if( kdiagr(ip,4,ig) .eq. 1 .or. kdiagr(ip,12,ig) .eq. 1 ) then
             kdiagr(ip,14,ig) = 1
         endif
         if( kdiagr(ip,5,ig) .eq. 1 .or. kdiagr(ip,12,ig) .eq. 1 ) then
             kdiagr(ip,15,ig) = 1
         endif
 1399 continue
*****************
*   count from 0
*****************
      do 8900 i = 1, MPROC
         ngd(i-1) =  ngd(i)
         do 8100 j = 1 , 6
            kmpr_c(i-1,j) = kmpr_c(i,j)
            kmpr_l(i-1,j) = kmpr_l(i,j)
            amas_1(i-1,j) = amas_1(i,j)
            kcha_g(i-1,j) = kcha_g(i,j)
            kfco_e(i-1,j) = kfco_e(i,j)
 8100    continue
         do 8200 j = 0 , 1
         do 8200 k = 0 , 1
             cf_mtx(i-1,j,k) = cf_mtx(i,j,k)
 8200    continue
         kmcb_s(i-1) =  kmcb_s(i)
         kmcb_x(i-1) =  kmcb_x(i)
         do 8300 j = 1 , 4
         do 8300 k = 0 , 1
            kmcs_r(i-1,j,k) =  kmcs_r(i,j,k)
 8300    continue
         do 8400 j = 1 , 20
         do 8400 k = 1 , NGRAPH
            kdiagr(i-1,j,k) = kdiagr(i,j,k)
 8400    continue
 8900 continue

      return
      end




