C|  move routines CHOICE,DCDMAS,LUNPIK,TRALO4,BOSTDQ to TAUOLA        |
C|  move INIPHX,INITDK,INIMAS  to Tauola                              |
C======================================================================
C======================= G L I B K  ===================================
C==================General Library of utilities========================
C===========It is imilar but not identical to HBOOK and HPLOT==========
C======================================================================
C   
C                      Version:    1.10
C              Last correction:    September 1994
C
C
C  Installation remarks: 
C  (1) printing backslash character depends on F77 compilator,
C      user may need to modify definition of BS variable in HPLCAP
C
C  Usage of the program:
C  (1) In most cases names and meanings of programs and their 
C      parameters is the same as in original CERN libraries HBOOK
C  (2) Unlike to original HBOOK and HPLOT, all floating parameters 
C      of the programs are in double precision!
C  (3) GLIBK stores histograms in double precision and always with
C      errors. REAL*8 storage is essential for 10**7 events statistics!
C  (4) Output from GLIBK is a picture recorded as regular a LaTeX file 
C      with frame and curves/histograms, it is easy to change fonts
C      add captions, merge plots, etc. by normal ediding. Finally,
C      picture may be inserted in any place into LaTeX source of the
C      article.
C
C  ********************************************************************
C  *  History of the program:                                         *
C  *  MINI-HBOOK writen by S. Jadach, Rutherford Lab. 1976            *
C  *  Rewritten December 1989 (S.J.)                                  *
C  *  Version with DOUBLE PRECISION ARGUMENTS ONLY!  and SAVE         *
C  *  Subrogram names start with G instead of H letter!               *
C  *  Entries:   Obligatory:  GLIMIT                                  *
C  *             Optional: see table below                            *
C  *  non-user subprograms in brackets                                *
C  ********************************************************************
C    SUBR/FUNC  1 PAR. 2 PAR. 3 PAR. 4 PAR. 5 PAR. 6 PAR.       
C  ====================================================================
*     (GINIT)   ----   ----    ----   ----   ----   ----        
*      GI       INT    INT     ----   ----   ----   ----        
*      GIE      INT    INT     ----   ----   ----   ----        
*      GF1      INT    DBL     DBL    ----   ----   ----        
*      GFILL    INT    DBL     DBL    DBL    ----   ----        
*      GBOOK1   INT    CHR*80  INT    DBL    DBL    ----  
*     (GOPTOU)  INT    INT     INT    INT    INT     INT
* (L.F. GEXIST) INT    -----  ------  ----   ----   ----        
*      GIDOPT   INT    CHR*4   -----  ----   ----   ----        
*      GBFUN1   INT    CHR*80   INT   DBL    DBL  DP-FUNC       
*      GIDOPT   INT    CHR*4   -----  ----   ----   ----        
*      GBOOK2   INT    CHR*80   INT   DBL    DBL     INT   DBL   DBL
*      GISTDO     ---   ----   ----   ----   ----   ----        
*      GOUTPU   INT     ----   ----   ----   ----   ----        
*      GPRINT   INT     ----   ----   ----   ----   ----        
*      GOPERA   INT    CHR*1   INT    INT    DBL    DBL         
*      GINBO1   INT    CHR*8   INT    DBL    DBL    ----        
*      GUNPAK   INT    DBL(*) CHR*(*) INT    ---    ----        
*      GPAK     INT    DBL(*)  ----   ----   ---    ----        
*      GPAKE    INT    DBL(*)  ----   ----   ---    ----       
*      GRANG1   INT    DBL     DBL    ----   ---    ----        
*      GINBO2   INT    INT     DBL    DBL    INT    DBL   DBL      
*      GMAXIM   INT    DBL     ----   ----   ---    ----        
*      GMINIM   INT    DBL     ----   ----   ---    ----        
*      GRESET   INT   CHR*(*)  ----   ----   ---    ----        
*      GDELET   INT     ----   ----   ----   ----   ----        
*      GLIMIT   INT     ----   ----   ----   ----   ----        
*     (COPCH)   CHR*80 CHR*80  ----   ----   ----   ----        
* (F. JADRES)   INT     ----   ----   ----   ----   ----        
*      GRFILE   INT   CHR*(*) CHR*(*) ----   ----   ----        
*      GROUT    INT    INT    CHR*8   ----   ----   ----        
*      GRIN     INT    INT     INT    ----   ----   ----        
*      GREND   CHR*(*) ----    ----   ----   ----   ----        
C  *******************  HPLOT entries ******************
*      GPLINT   INT    ----    ----   ----   ----   ----        
*      GPLCAP   INT    ----    ----   ----   ----   ----        
*      GPLEND   ----   ----    ----   ----   ----   ----        
*      GPLOT    INT    CHR*1   CHR*1   INT   ----   ----        
*     (LFRAM1)  INT      INT     INT  ----   ----   ----        
*     (SAXIX)   INT      DBL     DBL   INT    DBL   ----        
*     (SAXIY)   INT      DBL     DBL   INT    DBL   ----        
*     (PLHIST)  INT      INT     DBL   DBL    INT    INT        
*     (PLHIS2)  INT      INT     DBL   DBL    INT    INT        
*     (PLCIRC)  INT      INT     INT   DBL    DBL    DBL        
*     (APROF)   DBL      INT     DBL  ----   ----   ----        
*      GPLSET   INT      DBL    ----  ----   ----   ----        
*      GPLTIT   INT    CHR*80   ----  ----   ----   ----        
C  *******************  WMONIT entries ******************
*      GMONIT   INT ???
C  *******************************************************************
C                         END OF TABLE        
C  *******************************************************************
*          Map of memory for single histogram
*          ----------------------------------
*  (1-7) Header
*  ist +1   mark      9999999999999
*  ist +2   mark      9d12 + id*10 + 9
*  ist +3   iflag1    9d12 + iflag1*10 +9
*  ist +4   iflag2    9d12 + iflag2*10 +9
*  ist +5   scamin    minimum y-scale
*  ist +6   scamax    maximum y-scale
*  ist +7   jdlast    address of the next histogram 
*                     from previous history of calls (see jadres)
*          ----------------------------------
*              Binning size informations
*          ----------------------------------
*  One dimensional histogram            Two dimensional histog.
*  -------------------------            ----------------------
*  (8-11) Binning information           (8-15) Binning information
*  ist2 +1    NCHX                          ist2 +5   NCHY
*  ist2 +2      XL                          ist2 +6     YL
*  ist2 +3      XU                          ist2 +7     YU
*  ist2 +4   FACTX                          ist2 +8  FACTY
*
*          ----------------------------------
*           All kind of sums except of maxwt
*          ----------------------------------
*  (12-24) Under/over-flow average x    (16-24)
*  ist3 +1   Underflow                     All nine combinations
*  ist3 +2   Normal                        (U,N,O) x (U,N,O)
*  ist3 +3   Overerflow                    sum wt only (no errors)
*  ist3 +4   U  sum w**2
*  ist3 +5   N  sum w**2
*  ist3 +6   O  sum w**2
*  ist3 +7   Sum 1
*  ist3 +8   Sum wt*x
*  ist3 +9   Sum wt*x*x
*  ist3 +10  nevzer    (gmonit)
*  ist3 +11  nevove    (gmonit)
*  ist3 +12  nevacc    (gmonit)
*  ist3 +13  maxwt     (gmonit)
*          ----------------------------------
*           Content of bins including errors
*          ----------------------------------
*  (25 to 24+2*nchx)                     (25 to 24 +nchx*nchy)
*     sum wt and sum wt**2            sum wt only (no errors)
*  ----------------------------------------------------------------

      SUBROUTINE ginit
*     ****************
! First Initialization called from may routines
*     *************************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      SAVE   / cglib /
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /gind/
      DATA init /0/
      SAVE init
*
      IF(init .NE. 0) RETURN
      init=1
c this is version version number
      nvrs=111
c default output unit
      nout=16
      lenmax=0
      length=0
      DO i=1,idmx
         DO k=1,3
            index(i,k)=0
         ENDDO
         DO k=1,80
            titlc(i)(k:k)=' '
         ENDDO
      ENDDO
      DO k=1,50000
         b(k)=0d0
      ENDDO
      END

      SUBROUTINE gflush
*     ****************
! FLUSH memory, all histos erased!
*     *************************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      SAVE   / cglib /
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /gind/

      CALL ginit
      length=0
      DO i=1,idmx
         DO k=1,3
            index(i,k)=0
         ENDDO
         DO k=1,80
            titlc(i)(k:k)=' '
         ENDDO
      ENDDO
      DO k=1,50000
         b(k)=0d0
      ENDDO
      END

      LOGICAL FUNCTION gexist(id)
!     ***************************
! this function is true when id  exists !!!! 
!     ***************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /cglib/,/gind/
!
      lact=jadres(id)
      gexist = lact .NE. 0
c###  IF(gexist)      write(6,*) 'gexist: does   ID,lact= ',id,lact
c###  IF(.not.gexist) write(6,*) 'gexist: doesnt ID,lact= ',id,lact
      END

      function gi(id,ib)
*     ******************
C getting out bin content
C S.J. 18-Nov. 90
*     ***********************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
      save idmem,nch,lact,ist,ist2,ist3
      data idmem / -1256765/
c
      IF(id .EQ. idmem) goto 100
      idmem=id
c some checks, not repeated if id the same as previously
      lact=jadres(id)
      IF(lact .EQ. 0) then
        write(nout,*) ' gi: nonexisting histo id=',id
        write(   6,*) ' gi: nonexisting histo id=',id
        gi= 0d0
        stop
      ENDIF
      ist  = index(lact,2)
      ist2 = ist+7
      ist3 = ist+11
c checking if histo is of proper type
      iflag2   = nint(b(ist+4)-9d0-9d12)/10
      ityphi   = mod(iflag2,10)
      IF(ityphi .NE. 1) then
        write(nout,*) ' gi: 1-dim histos only !!! id=',id
        write(   6,*) ' gi: 1-dim histos only !!! id=',id
        gi= 0d0
        stop
      ENDIF
  100 continue
      nch  = nint(b(ist2+1))
      IF(ib .EQ. 0) then
c underflow
         gi=   b(ist3 +1)
      ELSEIF(ib .GE. 1.and.ib .LE. nch) then
c normal bin
         gi=   b(ist +nbuf+ib)
      ELSEIF(ib .EQ. nch+1) then
c overflow
         gi=   b(ist3 +3)
      ELSE
c abnormal exit
         write(nout,*) ' gi: wrong binning id,ib=',id,ib
         write(   6,*) ' gi: wrong binning id,ib=',id,ib
         gi=0d0
         stop
      ENDIF
      end

      function  gie(id,ib)
*     ********************
c getting out error of the bin
c s.j. 18-nov. 90
*     ***********************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
      save idmem,nch,lact,ist,ist2,ist3
      data idmem / -1256765/
c
      IF(id .EQ. idmem) goto 100
      idmem=id
c some checks, not repeated if id the same as previously
      lact=jadres(id)
      IF(lact .EQ. 0) then
        write(nout,*) ' gie: nonexisting histo id=',id
        write(   6,*) ' gie: nonexisting histo id=',id
        gie= 0d0
        stop
      ENDIF
      ist  = index(lact,2)
      ist2 = ist+7
      ist3 = ist+11
c checking if histo is of proper type
      iflag2   = nint(b(ist+4)-9d0-9d12)/10
      ityphi   = mod(iflag2,10)
      IF(ityphi .NE. 1) then
        write(nout,*) ' gie: 1-dim histos only !!! id=',id
        write(   6,*) ' gie: 1-dim histos only !!! id=',id
        gie= 0d0
        stop
      ENDIF
  100 continue
      nch  = b(ist2+1)
      IF(ib .EQ. 0) then
c underflow
         gie=   dsqrt( dabs(b(ist3 +4)))
      ELSEIF(ib .GE. 1.and.ib .LE. nch) then
c...normal bin, error content
         gie=   dsqrt( dabs(b(ist+nbuf+nch+ib)) )
      ELSEIF(ib .EQ. nch+1) then
c overflow
         gie=   dsqrt( dabs(b(ist3 +6)))
      ELSE
c abnormal exit
         write(nout,*) ' gie: wrong binning id, ib=',id,ib
         write(   6,*) ' gie: wrong binning id, ib=',id,ib
         gie=0d0
         stop
      ENDIF
      end

      subroutine gf1(id,x,wtw)
*     ************************
c recommended fast filling 1-dim. histogram
c s.j. 18 nov. 90
*     ***********************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
c
      lact=jadres(id)
c exit for non-existig histo
      IF(lact .EQ. 0)  return
      ist  = index(lact,2)
      ist2 = ist+7
      ist3 = ist+11
c one-dim. histo only
      iflag2   = nint(b(ist+4)-9d0-9d12)/10
      ityphi   = mod(iflag2,10)
      IF(ityphi .NE. 1) return
      xx= x
      wt= wtw
      index(lact,3)=index(lact,3)+1
c all entries
      b(ist3 +7)  =b(ist3 +7)   +1
c for average x
      b(ist3 +8)  =b(ist3 +8)  +wt*xx
      b(ist3 +9)  =b(ist3 +9)  +wt*xx*xx
c filling bins
      nchx  =b(ist2 +1)
      xl    =b(ist2 +2)
      factx =b(ist2 +4)
      kx = (xx-xl)*factx+1d0
      IF(kx .LT. 1) then
c underflow
         b(ist3 +1)    = b(ist3 +1)         +wt
         b(ist3 +4)    = b(ist3 +4)         +wt*wt
      ELSEIF(kx .GT. nchx) then
c overflow
         b(ist3 +3)    = b(ist3 +3)         +wt
         b(ist3 +6)    = b(ist3 +6)         +wt*wt
      ELSE
c normal bin
         b(ist3 +2)    = b(ist3 +2)         +wt
         b(ist +nbuf+kx) = b(ist+nbuf+kx)   +wt
c normal bin error 
         b(ist3 +5)    = b(ist3 +5)         +wt*wt
         b(ist +nbuf+nchx+kx) = b(ist+nbuf+nchx+kx)   +wt**2
      ENDIF
      end

      subroutine gfill(id,x,y,wtw)
*     ****************************
c this routine not finished, 1-dim only!
*     ***********************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
c
      lact=jadres(id)
      IF(lact .EQ. 0)  return
      ist  = index(lact,2)
c one-dim. histo 
      iflag2   = nint(b(ist+4)-9d0-9d12)/10
      ityphi   = mod(iflag2,10)
      IF(ityphi .EQ. 1) then
c...one-dim. histogram
        call gf1(id,x,wtw)
        return
      ENDIF
c...two-dim. scattergram, no errors!
      ist2 = ist+7
      ist3 = ist+15
      xx= x
      yy= y
      wt= wtw
      index(lact,3)=index(lact,3)+1
c x-axis
      nchx  =b(ist2 +1)
      xl    =b(ist2 +2)
      factx =b(ist2 +4)
      kx=(xx-xl)*factx+1d0
      lx=2
      IF(kx .LT. 1)     lx=1
      IF(kx .GT. nchx)  lx=3
      l     = ist+34  +lx
      b(l)  = b(l)    +wt
      k     = ist+nbuf2  +kx
      IF(lx .EQ. 2) b(k)  =b(k)  +wt
      k2    = ist+nbuf2  +nchx+kx
      IF(lx .EQ. 2) b(k2) =b(k2) +wt**2
c y-axix
      nchy  =b(ist2 +5)
      yl    =b(ist2 +6)
      facty =b(ist2 +8)
      ky=(yy-yl)*facty+1d0
      ly=2
      IF(ky .LT. 1)    ly=1
      IF(ky .GT. nchy) ly=3
c under/over-flow
      l = ist3  +lx +3*(ly-1)
      b(l) =b(l)+wt
c regular bin
      k = ist+nbuf2 +kx +nchx*(ky-1)
      IF(lx .EQ. 2.and.ly .EQ. 2) b(k)=b(k)+wt
      end

      subroutine gbook1(id,title,nnchx,xxl,xxu)
*     *****************************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
      character*80 title
      logical gexist
c
      call ginit
      IF(gexist(id)) goto 900
      ist=length
      lact=jadres(0)
c the case of no free entry in the index
      IF(lact .EQ. 0) goto 901
      index(lact,1)=id
      index(lact,2)=length
      index(lact,3)=0
*----
ccc      write(6,*) 'GBOOK1: ID= ',ID
c -------
      call copch(title,titlc(lact))
      nchx =nnchx
      xl   =xxl
      xu   =xxu
c ---------- title and bin content ----------
      lengt2 = length +2*nchx +nbuf+1
      IF(lengt2 .GE. lenmax) goto 902
      do 10 j=length+1,lengt2+1
  10  b(j) = 0d0
      length=lengt2
c... default flags
      ioplog   = 1
      iopsla   = 1
      ioperb   = 1
      iopsc1   = 1
      iopsc2   = 1
      iflag1   = 
     $ ioplog+10*iopsla+100*ioperb+1000*iopsc1+10000*iopsc2
      ityphi   = 1
      iflag2   = ityphi
C examples of decoding flags 
c      id       = nint(b(ist+2)-9d0-9d12)/10
c      iflag1   = nint(b(ist+3)-9d0-9d12)/10
c      ioplog = mod(iflag1,10)
c      iopsla = mod(iflag1,100)/10
c      ioperb = mod(iflag1,1000)/100
c      iopsc1 = mod(iflag1,10000)/1000
c      iopsc2 = mod(iflag1,100000)/10000
c      iflag2   = nint(b(ist+4)-9d0-9d12)/10
c      ityphi = mod(iflag2,10)
c--------- buffer -----------------
c header
      b(ist +1)  = 9999999999999d0
      b(ist +2)  = 9d12 +     id*10 +9d0
      b(ist +3)  = 9d12 + iflag1*10 +9d0
      b(ist +4)  = 9d12 + iflag2*10 +9d0
c dummy vertical scale
      b(ist +5)  =  -100d0
      b(ist +6)  =   100d0
c pointer used to speed up search of histogram address
      b(ist +7)  =   0d0
c information on binning
      ist2       = ist+7
      b(ist2 +1) = nchx
      b(ist2 +2) = xl
      b(ist2 +3) = xu
      ddx = xu-xl
      IF(ddx .EQ. 0d0) goto 903
      b(ist2 +4) = float(nchx)/ddx
c under/over-flow etc.
      ist3       = ist+11
      do 100  j=1,13
 100  b(ist3 +j)=0d0
c
      RETURN
 900  continue
      write(6   ,*) ' WARNING gbook1: already exists id=  ', id
      write(NOUT,*) ' WARNING gbook1: already exists id=  ', id
      RETURN      
 901  continue
      call gstop1(' gbook1: to many histos !!!!!,     id=  ',id)
 902  continue
      call gstop1(' gbook1: to litle storage!!!!,  lenmax= ',lenmax)
 903  continue
      call gstop1('  gbook1:    xl=xu,               id=   ',id)
      end

      subroutine gstop1(mesage,id)
*     *******************************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save   /gind/
      character*40 mesage

      write(nout,'(a)') 
     $          '++++++++++++++++++++++++++++++++++++++++++++++++++++++'
      write(nout,'(a,a,i10,a)')  
     $                          '+ ', mesage, id, ' +'
      write(nout,'(a)') 
     $          '++++++++++++++++++++++++++++++++++++++++++++++++++++++'
      write(6   ,'(a)') 
     $          '++++++++++++++++++++++++++++++++++++++++++++++++++++++'
      write(6   ,'(a,a,i10,a)')  
     $                          '+ ', mesage, id, ' +'
      write(6   ,'(a)') 
     $          '++++++++++++++++++++++++++++++++++++++++++++++++++++++'
      stop
      end


      subroutine goptou(id,ioplog,iopsla,ioperb,iopsc1,iopsc2)
c     ********************************************************
c decoding option flags
c     **********************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/

      lact=jadres(id)
      IF(lact .EQ. 0) return
      ist=index(lact,2)
c decoding flags 
      iflag1   = nint(b(ist+3)-9d0-9d12)/10
      ioplog = mod(iflag1,10)
      iopsla = mod(iflag1,100)/10
      ioperb = mod(iflag1,1000)/100
      iopsc1 = mod(iflag1,10000)/1000
      iopsc2 = mod(iflag1,100000)/10000
      end

      subroutine gidopt(id,ch)
c     ************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
      character*4 ch
c
      lact=jadres(id)
      IF(lact .EQ. 0) return
      ist=index(lact,2)
C decoding flags 
      call goptou(id,ioplog,iopsla,ioperb,iopsc1,iopsc2)
      IF(ch .EQ.       'LOGY'  ) then
c log scale for print
        ioplog = 2 
      ELSEIF(ch .EQ.   'ERRO'  ) then
C errors in printing/plotting
       ioperb  = 2
      ELSEIF(ch .EQ.   'SLAN'  ) then
c slanted line in plotting
       iopsla  = 2
      ELSEIF(ch .EQ.   'YMIN'  ) then
       iopsc1  = 2
      ELSEIF(ch .EQ.   'YMAX'  ) then
       iopsc2  = 2
      ENDIF
c encoding back
      iflag1   = 
     $ ioplog+10*iopsla+100*ioperb+1000*iopsc1+10000*iopsc2
      b(ist+3) = 9d12 + iflag1*10 +9d0
      end


      SUBROUTINE gbfun1(id,title,nchx,xmin,xmax,func)
c     ***********************************************
c ...fills histogram with function func(x)
c     ***********************************
      IMPLICIT double precision (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /gind/
      DIMENSION yy(200)
      EXTERNAL func
      CHARACTER*80 title
      LOGICAL gexist
c
      CALL ginit
      IF(gexist(id)) GOTO 900
 15   xl=xmin
      xu=xmax
      CALL gbook1(id,title,nchx,xl,xu)
c...slanted line in plotting
      CALL gidopt(id,'SLAN')
      IF(nchx .GT. 200) goto 901
      DO 20 ib=1,nchx
      x= xmin +(xmax-xmin)/nchx*(ib-0.5d0)
      yy(ib) = func(x)
   20 CONTINUE
      CALL gpak(id,yy)
      RETURN
 900  WRITE(nout,*) ' +++gbfun1: already exists id=',id
      WRITE(6   ,*) ' +++gbfun1: already exists id=',id      
      CALL gdelet(id)
      GO to 15
 901  WRITE(nout,*) ' +++gbfun1: to many bins'
      END

      SUBROUTINE gbfun2(id,title,nchx,xmin,xmax,func)
c     ***********************************************
c ...fills histogram with function func(x)
c.. three point fit used
c     ***********************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /gind/
      DIMENSION yy(200),yy1(0:200)
      EXTERNAL func
      CHARACTER*80 title
      LOGICAL gexist
!
      CALL ginit
      IF( gexist(id) ) GOTO 900
 15   xl=xmin
      xu=xmax
      CALL gbook1(id,title,nchx,xl,xu)

c...slanted line in plotting
      CALL gidopt(id,'SLAN')
      IF(nchx.gt.200) GOTO 901

      yy1(0) = func(xmin)
      dx=(xmax-xmin)/nchx

      DO ib=1,nchx
         x2= xmin +dx*(ib-0.5d0)
         x3= x2 +dx*0.5d0
         yy(ib)  = func(x2)
         yy1(ib) = func(x3)
c..  simpson 
         yy(ib) = ( yy1(ib-1) +4*yy (ib) +yy1(ib))/6d0
      ENDDO

      CALL gpak(id,yy)
      RETURN
 900  WRITE(nout,*) ' +++gbfun2: already exists id=',id
      WRITE(6   ,*) ' +++gbfun2: already exists id=',id      
      CALL gdelet(id)
      GO TO 15
 901  WRITE(nout,*) ' +++gbfun2: to many bins'
      END



      SUBROUTINE GBOOK2(ID,TITLE,NCHX,XL,XU,NCHY,YL,YU)
*     *************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER( IDMX=400,NBUF=24,NBUF2=24)
      COMMON / Cglib / B(50000)
      COMMON /GIND/ NVRS,NOUT,LENMAX,LENGTH,INDEX(IDMX,3),TITLC(IDMX)
      CHARACTER*80 TITLC
      save /cglib/,/gind/
      CHARACTER*80 TITLE
      LOGICAL GEXIST
c
      CALL GINIT
      IF(GEXIST(ID)) GOTO 900
      ist=length
      LACT=JADRES(0)
      IF(LACT .EQ. 0) GOTO 901
      index(LACT,1)=ID
      index(LACT,2)=length
      CALL COPCH(TITLE,TITLC(LACT))
      nnchx=NCHX
      nnchy=NCHY
      LENGT2 = LENGTH  +44+nnchx*nnchy
      IF(LENGT2 .GE. LENMAX) GOTO 902
      DO 10 J=LENGTH+1,LENGT2+1
   10 B(J) = 0D0
      LENGTH=LENGT2
      B(ist+1)=nnchx
      B(ist+2)=XL
      B(ist+3)=XU
      B(ist+4)=float(nnchx)/(b(ist+3)-b(ist+2))
      B(ist+5)=nnchy
      B(ist+6)=YL
      B(ist+7)=YU
      B(ist+8)=float(nnchy)/(b(ist+7)-b(ist+6))
      RETURN
  900 WRITE(NOUT,*) ' GBOOK2: HISTO ALREADY EXISTS!!!! ID=',ID
      RETURN
  901 WRITE(NOUT,*) ' GBOOK2: TO MANY HISTOS !!!!!',LACT
      STOP
  902 WRITE(NOUT,*) ' GBOOK2: TO LITLE STORAGE!!!!',LENMAX
      STOP
      END

      subroutine gistdo
*     *****************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /gind/
      do 10 i=1,idmx
      id=index(i,1)
      IF(id .GT. 0) call gprint(id)
   10 continue
      end

      subroutine goutpu(ilun)
*     ***********************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /gind/
      call ginit
      nout=ilun
      end


      subroutine gprint(id)
*     *********************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
CC..M.S.>>
C      character*1 line(105),lchr(22),lb,lx,li,l0
      character*1 line(0:105),lchr(22),lb,lx,li,l0
CC..M.S.<<
      data lb,lx,li,l0 /' ','X','I','0'/
      data lchr/' ','1','2','3','4','5','6','7','8','9',
     $      'A','B','C','D','E','F','G','H','I','J','K','*'/
      logical llg
      save lb,lx,li,l0,lchr

      lact=jadres(id)
      if(lact.eq.0) goto 900
      ist  = index(lact,2)
      ist2 = ist+7
      ist3 = ist+11

      call goptou(id,ioplog,iopsla,ioperb,iopsc1,iopsc2)
      ker    =  ioperb-1
cc..m.s.
      lmx = 57
      lmx = 52
cc..m.s.
      if(ker.eq.1) lmx=54
      nent=index(lact,3)
      if(nent.eq.0) goto 901
      write(nout,1000) id,titlc(lact)
 1000 FORMAT('1',/,1X,I6,10X,A)
c
c one-dim. histo 
      iflag2   = nint(b(ist+4)-9d0-9d12)/10
      ityphi   = mod(iflag2,10)
      if(ityphi.ne.1) goto 200
      nchx =   b(ist2 +1)
      xl   =   b(ist2 +2)
      dx   =  (  b(ist2 +3)-b(ist2 +2)  )/float(nchx)
c fixing vertical scale
      istr=ist+nbuf+1
      bmax = b(istr)
      bmin = b(istr)
      do 15 ibn=istr,istr+nchx-1
      bmax = max(bmax,b(ibn))
      bmin = min(bmin,b(ibn))
  15  continue
      if(bmin.eq.bmax) goto 901
      if(iopsc1.eq.2) bmin=b(ist +5)
      if(iopsc2.eq.2) bmax=b(ist +6)
c
      llg=ioplog.eq.2
      if(llg.and.bmin.le.0d0) bmin=bmax/10000.d0
c
      deltb = bmax-bmin
      if(deltb.eq.0d0) goto 902
      fact  = (lmx-1)/deltb
      kzer  = -bmin*fact+1.00001d0
      if(llg) fact=(lmx-1)/(log(bmax)-log(bmin))
      if(llg) kzer=-log(bmin)*fact+1.00001d0
c
      undf = b(ist3 +1)
      ovef = b(ist3 +3)
      avex = 0d0
      sum  = b(ist3 +8)
      if(nent.ne.0) avex = sum/nent
      write(nout,'(4a15      )')  'nent','sum','bmin','bmax'
      write(nout,'(i15,3e15.5)')   nent,  sum,  bmin,  bmax
      write(nout,'(4a15  )')      'undf','ovef','avex'
      write(nout,'(4e15.5)')       undf,  ovef,  avex
c
      if(llg) write(nout,1105)
 1105 format(35x,17hlogarithmic scale)
c
      kzer=max0(kzer,0)
      kzer=min0(kzer,lmx)
      xlow=xl
      do 100 k=1,nchx
c first fill with blanks
CC..M.S.>>
C      do  45 j=1,105
      do  45 j=0,105
CC..M.S.<<
   45 line(j)  =lb
c then fill upper and lower boundry
      line(1)  =li
      line(lmx)=li
      ind=istr+k-1
      bind=b(ind)
      bind= max(bind,bmin)
      bind= min(bind,bmax)
      kros=(bind-bmin)*fact+1.0001d0
      if(llg) kros=log(bind/bmin)*fact+1.0001d0
      k2=max0(kros,kzer)
      k2=min0(lmx,max0(1,k2))
      k1=min0(kros,kzer)
      k1=min0(lmx,max0(1,k1))
      do 50 j=k1,k2
   50 line(j)=lx
      line(kzer)=l0
      z=b(ind)
      if(ker.ne.1) then 
cc..m.s.        write(nout,'(a, f7.4,  a, d12.4,  132a1)') 
        write(nout,'(a, d12.6,  a, d12.6,  132a1)') 
     $             ' ', xlow,' ',     z,' ',(line(i),i=1,lmx)
      else
        er=dsqrt(dabs(b(ind+nchx)))
cc..m.s.        write(nout,'(a,f7.4,  a,d12.4,  a,d12.4, 132a1 )') 
        write(nout,'(a,f8.4,  a,d14.7,  a,d9.2, 132a1 )') 
     $             ' ',xlow,' ',    z,' ',   er,' ',(line(i),i=1,lmx)
      endif
c      if(ker.ne.1) then
c        write(nout,'(a, f7.4,  a, d14.6,  132a1)') 
c     $             ' ', xlow,' ',     z,' ',(line(i),i=1,lmx)
c      else
c        er=dsqrt(dabs(b(ind+nchx)))
c        write(nout,'(a,f7.4,  a,d14.6,  a,d14.6, 132a1 )') 
c     $             ' ',xlow,' ',    z,' ',   er,' ',(line(i),i=1,lmx)
c      endif
      xlow=xlow+dx
  100 continue
      return
C------------- two dimensional requires complete restoration!!!----------------
  200 continue
      nchx=B(ist+1)
      nchy=B(ist+5)
      write(nout,2000) (lx,i=1,nchy)
 2000 format(1h ,10x,2hxx,100a1)
      do 300 kx=1,nchx
      do 250 ky=1,nchy
      k=ist +NBUF2 +kx+nchx*(ky-1)
      N=B(K)+1.99999D0
      n=max0(n,1)
      n=min0(n,22)
      if(DABS(b(k)).lt.1D-20) n=1
      line(ky)=lchr(n)
  250 continue
      line(nchy+1)=lx
      i1=nchy+1
      write(nout,2100) (line(i),i=1,i1)
 2100 format(1h ,10x,1hx,100a1)
  300 continue
      write(nout,2000) (lx,i=1,nchy)
      RETURN
  900 WRITE(NOUT,*) ' +++GPRINT: NONEXISTING HISTO',ID
      WRITE(6   ,*) ' +++GPRINT: NONEXISTING HISTO',ID
      RETURN
 901  WRITE(NOUT,*) ' +++GPRINT: NO ENTRIES  HISTO',ID
      WRITE(   6,*) ' +++GPRINT: NO ENTRIES  HISTO',ID
      RETURN
 902  WRITE(NOUT,*) ' +++GPRINT: wrong plotting limits',ID
      WRITE(   6,*) ' +++GPRINT: wrong plotting limits',ID
      END


      subroutine gopera(ida,chr,idb,idc,coef1,coef2)
*     **********************************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
      character*80 title
      character*1  chr
c
      lacta=jadres(ida)
      IF(lacta .EQ. 0) return
      ista  = index(lacta,2)
      ista2 = ista+7
      ncha  = b(ista2+1)
c
      lactb =jadres(idb)
      IF(lactb .EQ. 0) return
      istb  = index(lactb,2)
      istb2 = istb+7
      nchb  = b(istb2+1)
      IF(nchb .NE. ncha) goto 900
c
      lactc=jadres(idc)
      IF(lactc .EQ. 0) then
c ...if nonexistent, histo idc is here defined
        call ginbo1(ida,title,nchx,xl,xu)
        call gbook1(idc,title,nchx,xl,xu)
        lactc = jadres(idc)
        istc  = index(lactc,2)
c...option copied from ida
        b(istc+ 3)= b(ista +3)
      ENDIF
c...one nominal entry recorded
      index(lactc,3) = 1
c
      istc  =  index(lactc,2)
      istc2 =  istc+7
      nchc  =  b(istc2+1)
c
      IF(nchc .NE. ncha) goto 900
      IF(ncha .NE. nchb.or.nchb .NE. nchc) goto 900
      do 30 k=1,ncha
      i1 = ista+nbuf+k
      i2 = istb+nbuf+k
      i3 = istc+nbuf+k
      j1 = ista+nbuf+ncha+k
      j2 = istb+nbuf+ncha+k
      j3 = istc+nbuf+ncha+k
      if    (chr .EQ. '+')   then
        b(i3) =    coef1*b(i1) +    coef2*b(i2)
        b(j3) = coef1**2*b(j1) + coef2**2*b(j2)
      ELSEIF(chr .EQ. '-')   then
        b(i3) = coef1*b(i1) - coef2*b(i2)
        b(j3) = coef1**2*b(j1) + coef2**2*b(j2)
      ELSEIF(chr .EQ. '*')   then
        b(j3) = (coef1*coef2)**2
     $          *(b(j1)*b(i2)**2 + b(j2)*b(i1)**2)
        b(i3) = coef1*b(i1) * coef2*b(i2)
      ELSEIF(chr .EQ. '/')   then
        IF(b(i2) .EQ. 0d0) then
          b(i3) = 0d0
          b(j3) = 0d0
        ELSE
          b(j3) = (coef1/coef2)**2/b(i2)**4
     $          *(b(j1)*b(i2)**2 + b(j2)*b(i1)**2)
          b(i3) = (coef1*b(i1) )/( coef2*b(i2))
        ENDIF
      ELSE
        goto 901
      ENDIF
   30 continue
      return
  900 write(nout,*) '+++++ gopera: non-equal no. bins ',ida,idb,idc
      write(   6,*) '+++++ gopera: non-equal no. bins ',ida,idb,idc
      return
  901 write(nout,*) '+++++ gopera: wrong chr=',chr
      END

      SUBROUTINE ginbo1(id,title,nchx,xl,xu)
!     **************************************
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /cglib/,/gind/
      CHARACTER*80 title
!
      lact=jadres(id)
      IF(lact .EQ. 0) THEN
         write(6,*) '+++++ STOP in ginbo1: wrong id=',id
         STOP
      ENDIF
      ist=index(lact,2)
      ist2   = ist+7
      nchx   = b(ist2 +1)
      xl     = b(ist2 +2)
      xu     = b(ist2 +3)
      title  = titlc(lact)
      END

      subroutine gunpak(id,a,chd1,idum)
*     *********************************
c getting out histogram content (and error)
c chd1= 'ERRO' is nonstandard option (unpack errors)
*     ***********************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      character*(*) chd1
      dimension a(*)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
c
      lact=jadres(id)
      IF(lact .EQ. 0) goto 900
      ist   = index(lact,2)
      ist2  = ist+7
      nch   = b(ist2 +1)
      local = ist +nbuf
      iflag2   = nint(b(ist+4)-9d0-9d12)/10
      ityphi   = mod(iflag2,10)
      IF(ityphi .EQ. 2) then
        nchy  = b(ist2+5)
        nch   = nch*nchy
        local = ist+ nbuf2
      ENDIF
      do 10 ib=1,nch
      IF(chd1 .NE. 'ERRO') then
c normal bin
        a(ib) = b(local+ib)
      ELSE
c error content
        IF(ityphi .EQ. 2) goto 901
        a(ib) = dsqrt( dabs(b(local+nch+ib) ))
      ENDIF
   10 continue
      return
 900  write(nout,*) '+++gunpak: nonexisting id=',id
      write(6   ,*) '+++gunpak: nonexisting id=',id
      return
 901  write(nout,*) '+++gunpak: no errors, two-dim, id=',id
      end

      subroutine gpak(id,a)
*     *********************
c getting in histogram content
*     ***********************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      dimension  a(*)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
c
      lact=jadres(id)
      IF(lact .EQ. 0) goto 900
      ist  = index(lact,2)
      ist2 = ist+7
      nch=b(ist2 +1)
      local = ist+nbuf
c 2-dimens histo alowed
      iflag2   = nint(b(ist+4)-9d0-9d12)/10
      ityphi   = mod(iflag2,10)
      IF(ityphi .EQ. 2) then
        nchy  = b(ist2+5)
        nch   = nch*nchy
        local = ist+nbuf2
      ENDIF
      do 10 ib=1,nch
   10 b(local +ib) = a(ib)
c one nominal entry recorded
      index(lact,3)  = 1
      return
  900 write(nout,*) '+++gpak: nonexisting id=',id
      write(6   ,*) '+++gpak: nonexisting id=',id
      end

      subroutine gpake(id,a)
*     **********************
c getting in error content
*     ***********************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      dimension  a(*)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
c
      lact=jadres(id)
      IF(lact .EQ. 0) goto 901
      ist  = index(lact,2)
      ist2 = ist+7
      nch=b(ist2+1)
c 2-dimens histo NOT alowed
      iflag2   = nint(b(ist+4)-9d0-9d12)/10
      ityphi   = mod(iflag2,10)
      IF(ityphi .EQ. 2) goto 900
      do 10 ib=1,nch
   10 b(ist+nbuf+nch+ib) = a(ib)**2
      return
  900 write(nout,*) ' +++++ gpake: only for one-dim histos'
      return
  901 write(nout,*) '+++ gpake: nonexisting id=',id
      write(6   ,*) '+++ gpake: nonexisting id=',id
      end


      subroutine grang1(id,ylr,yur)
*     *****************************
c provides y-scale for 1-dim plots
*     ***********************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
c
      lact=jadres(id)
      IF(lact .EQ. 0) return
      ist  = index(lact,2)
      ist2 = ist+7
      nch  = b(ist2 +1)
      yl   = b(ist+nbuf+1)
      yu   = b(ist+nbuf+1)
      do 10 ib=1,nch
      yl = min(yl,b(ist+nbuf+ib))
      yu = max(yu,b(ist+nbuf+ib))
   10 continue
      call goptou(id,ioplog,iopsla,ioperb,iopsc1,iopsc2)
      IF(iopsc1 .EQ. 2) yl= b( ist +5)
      IF(iopsc2 .EQ. 2) yu= b( ist +6)
      ylr = yl
      yur = yu
      end


      subroutine ginbo2(id,nchx,xl,xu,nchy,yl,yu)
*     *******************************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
c
      lact=jadres(id)
      IF(lact .EQ. 0) goto 900
      ist  = index(lact,2)
      ist2 = ist+7
      nchx = b(ist2 +1)
      xl   = b(ist2 +2)
      xu   = b(ist2 +3)
      nchy = b(ist2 +5)
      yl   = b(ist2 +6)
      yu   = b(ist2 +7)
      return
  900 write(nout,*) ' +++ginbo2: nonexisting histo id= ',id 
      write(   6,*) ' +++ginbo2: nonexisting histo id= ',id
      end


      subroutine gmaxim(id,wmax)
*     **************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
c
      IF(id .NE. 0) then
        lact=jadres(id)
        IF(lact .EQ. 0) return
        ist= index(lact,2)
        b(ist+6) =wmax
        call gidopt(id,'YMAX')
      ELSE
        do 20 k=1,idmx
        IF(index(k,1) .EQ. 0) goto 20
        ist=index(k,2)
        jd =index(k,1)
        b(ist+6) =wmax
        call gidopt(jd,'YMAX')
   20   continue
      ENDIF
      end

      subroutine gminim(id,wmin)
*     **************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
c
      IF(id .NE. 0) then
        lact=jadres(id)
        IF(lact .EQ. 0) return
        ist =index(lact,2)
        b(ist+5) =wmin
        call gidopt(id,'YMIN')
      ELSE
        do 20 k=1,idmx
        IF(index(k,1) .EQ. 0) goto 20
        ist=index(k,2)
        jd =index(k,1)
        b(ist+5) =wmin
        call gidopt(jd,'YMIN')
   20   continue
      ENDIF
      end

      subroutine greset(id,chd1)
*     **************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      character*(*) chd1
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
c
      lact=jadres(id)
      IF(lact .LE. 0) return
      ist  =index(lact,2)
      ist2 = ist+7
c 
      iflag2   = nint(b(ist+4)-9d0-9d12)/10
      ityphi   = mod(iflag2,10)
      IF(ityphi .EQ. 1) then
c one-dim.
        ist3  = ist+11
        nchx  = b(ist2 +1)
        nch   = 2*nchx
        local = ist + nbuf
      ELSEIF(ityphi .EQ. 2) then
c two-dim.
        ist3  = ist+15
        nchx  = b(ist2 +1)
        nchy  = b(ist2 +5)
        nch   = nchx*nchy
        local = ist +nbuf2
      ELSE
         write(nout,*) '+++greset: wrong type id=',id
         write(6   ,*) '+++greset: wrong type id=',id
        return
      ENDIF
c reset miscaelaneous entries and bins
      do 10 j=ist3+1,local +nch
  10  b(j)    = 0d0
c and no. of entries in index
      index(lact,3) = 0
      end

      SUBROUTINE GDELET(ID1)
*     *********************
C Now it should work (stj Nov. 91) but watch out!
C should works for 2-dim histos, please check this!
*     ***********************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /cglib/,/gind/
      logical gexist
c
      ID=ID1
      IF(id .EQ. 0) GOTO 300
      IF(.not.gexist(id)) GOTO 900
      lact = jadres(id)
      ist  = index(lact,2)
      ist2 = ist+7
*----
c[[[      WRITE(6,*) 'GDELET-ing ID= ',ID
      idec    = nint(b(ist+2)-9d0-9d12)/10
      IF(idec .NE. id) WRITE(6,*) '++++GDELET: ALARM! ID,IDEC= ',ID,IDEC
*----
      nch  = b(ist2 +1)
      iflag2   = nint(b(ist+4)-9d0-9d12)/10
      ityphi   = mod(iflag2,10)
      IF(ityphi .EQ. 1) THEN
c one-dim.
        nchx  = b(ist2 +1)
        nch   = 2*nchx
c lenght of local histo to be removed
        local = nch+nbuf+1
      ELSEIF(ityphi .EQ. 2) THEN
c two-dim.
        nchx  = b(ist2 +1)
        nchy  = b(ist2 +5)
        nch   = nchx*nchy
c lenght of local histo to be removed
        local = nch+nbuf2+1
      ELSE
         write(nout,*) '+++gdelet: wrong type id=',id
         write(6   ,*) '+++gdelet: wrong type id=',id
        return
      ENDIF
c starting position of next histo in storage b
      next = ist+1 +local
c move down all histos above this one 
      DO 15 k =next,length
      b(k-local)=b(k)
   15 CONTINUE  
c define new end of storage
      length=length-local
c clean free space at the end of storage b
      DO 20 k=length+1, length+local
   20 b(k)=0d0 
c shift adresses of all displaced histos 
      DO 25 l=lact+1,idmx
      IF(index(l,1) .NE. 0) index(l,2)=index(l,2)-local
   25 CONTINUE
c move entries in index down by one and remove id=lact entry
      DO 30 l=lact+1,idmx
      index(l-1,1)=index(l,1)
      index(l-1,2)=index(l,2)
      index(l-1,3)=index(l,3)
      titlc(l-1)=titlc(l)
   30 CONTINUE
c last entry should be always empty
      index(idmx,1)=0
      index(idmx,2)=0
      index(idmx,3)=0 
      do 50 k=1,80
   50 titlc(idmx)(k:k)=' '
      RETURN
C -----------------------------------
C Deleting all histos at once!!!
  300 length=0
      DO 400 i=1,idmx
      DO 340 k=1,3
  340 index(i,k)=0
      DO 350 k=1,80
  350 titlc(i)(k:k)=' '
 400  CONTINUE
      RETURN
C -----------------------------------
 900  CONTINUE
      WRITE(nout,*) ' +++GDELET: nonexisting histo id= ',id 
      WRITE(   6,*) ' +++GDELET: nonexisting histo id= ',id 
      END


      subroutine glimit(lenmx)
*     ************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
      save /gind/
      call ginit
      IF(lenmx .GE. lenmax) then
         lenmax=lenmx
      ELSE
         call gstop1('glimit: cant decrease storage lenmx  =',lenmx)
      ENDIF
      end

      subroutine copch(ch1,ch2)
*     *************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
* copies character*80 ch1 into ch2 up to a first $ sign
      character*80 ch1,ch2
      logical met
      met = .false.
      do 10 i=1,80
      IF( ch1(i:i) .EQ. '$' .or. met )   then
        ch2(i:i)=' '
        met=.true.
      ELSE
        ch2(i:i)=ch1(i:i)
      ENDIF
  10  continue
      end

      FUNCTION jadre2(id)
*     *********************
*------------------------------------------------
* Good old version -- but it is very very slow!!!
* In the case of 100 histograms or more.
*------------------------------------------------
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      jadre2=0
      DO 1 i=1,idmx
      IF(index(i,1) .EQ. id) goto 2
    1 CONTINUE
* Nothing found.
      RETURN
* Found: id=0 is also legitimate find!!!
    2 jadre2=i
      END

      FUNCTION jadres(id1)
*     *********************
*--------------------------------------------------------------------
* Educated guess based on past history is used to find quickly
* location of the histogram in the matrix index.
* This is based on observation that subsequent histogram calls 
* are linked into loops (so one can predict easily which histo will
* be called next time).
*--------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      SAVE /gind/
      DATA iguess,jdlast,idlast /-2141593,-3141593,-3141593/
      SAVE iguess,jdlast,idlast

      id=id1
! --- The case of ID=0 treated separately, it is used to find out
! --- last entry in the index (it is marked with zero)
      IF(id .EQ. 0) THEN
         DO i=1,idmx
            IF(index(i,1) .EQ. 0) goto 4
         ENDDO
         WRITE(6,*) '+++++jadres: STOP index to short'
         STOP
 4       CONTINUE
         jadres = i
         RETURN
      ENDIF

! --- Omit sophistications if lack of initialization
      IF(jdlast .EQ. -3141593) GOTO 10
      IF(iguess .EQ. -2141593) GOTO 10
      IF(iguess .EQ. 0) GOTO 10
      IF(jdlast .EQ. 0) GOTO 10

! --- Try first previous histo (for repeated calls)
      IF(jdlast .LT. 1 .OR. jdlast .GT. idmx) THEN
         WRITE(6,*) '+++++ jadres: jdlast=',jdlast
      ENDIF
      IF(index(jdlast,1) .EQ. id) THEN
         jadres = jdlast
c##   write(6,*) 
c##   $   'found, guess based on previous call to jadres ',jdlast
         GOTO 20
      ENDIF

! --- Try current guess based on previous call
      IF(iguess .LT. 1 .OR. iguess .GT. idmx)  THEN
         WRITE(6,*)'+++++ jadres: iguess=',iguess
      ENDIF
      IF(index(iguess,1) .EQ. id) THEN
         jadres = iguess
c##   write(6,*) 
c##   $   'found, guess on previous calls recorded in b(ist+7)',jdlast
         GOTO 20
      ENDIF

! ================================================
!    Do it HARD WAY, Search all matrix index
! ================================================
 10   CONTINUE
c##   write(6,*) 'trying HARD WAY'
      DO i=1,idmx
         jadres=i
         IF(index(i,1) .EQ. id) GOTO 20
      ENDDO
! -------------------------------------
!     Nothing found: jadres=0
! -------------------------------------
      jadres=0
      RETURN
! =====================================
!     Found: Set new guess for next call
! =====================================
 20   CONTINUE
! --- and store result as a new guess in previous histo 
! --- but only if it existed!!!!
      DO i=1,idmx
         IF(index(i,1) .EQ. 0) GOTO 40
         IF(index(i,1) .EQ. idlast) THEN
            ist=index(i,2)
            IF(ist .GT. 0 .AND. ist .LT. 50000) b(ist +7) = jadres
c##   write(6,*) 'STORED     id=',id
            GOTO 40
         ENDIF 
      ENDDO
 40   CONTINUE
c##   write(6,*)  'found, hard way searching all of index)', jdlast
      iguess = b( index(jadres,2) +7)
      jdlast = jadres
      idlast = id
      END


C--------------------------------------------------------------
C ----------- storing histograms in the disk file -------------
C--------------------------------------------------------------
      subroutine grfile(nhruni,dname,chd2)
c     ***********************************
      implicit double precision (a-h,o-z)
      character*(*) chd2, dname
      common / hruni / nhist
      save /hruni/
      nhist=nhruni
      end

      subroutine grout(idum1,idum2,chdum)
c     ***********************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common / cglib / b(50000)
      character*8 chdum
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      common / hruni / nhist
      character*80 titlc
      save /cglib/,/gind/, /hruni/
c
      call ginit
      nouth=nhist
      write(nouth,'(6i10)')   nvrs,nout,lenmax,length
      write(nouth,'(6i10)')   ((index(i,k),k=1,3),i=1,idmx)
      write(nouth,'(a80)')    titlc
      write(nouth,'(3d24.16)') (b(i),i=1,length)
      end


      SUBROUTINE GRIN(IDUM1,IDUM2,IDUM3)
!     **********************************
! New version which has a possibility to 
!            MERGE histograms
! If given ID already exists then it is modified by adding 1000000 !!!!
! Mergigng is done simply by appending new histograms at the 
! very end of the index and bin matrices.
!     ***********************************
      IMPLICIT double precision (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      COMMON / hruni / nhist
      SAVE /cglib/,/gind/, /hruni/
! Copy of the new index from the disk
      DIMENSION lndex(idmx,3),titld(idmx)
      CHARACTER*80 titld
      LOGICAL gexist

      CALL ginit 
      nouth=nhist
! Read basic params
      READ(nouth,'(6i10)')   nvrs3,nout3,lenma3,lengt3
      IF(length+lengt3 .GE. lenmax) GOTO 900
! Check version
      IF(nvrs .NE. nvrs3) WRITE(nout,*)
     $ '  +++++ warning (grin): histos produced by older version',nvrs3
      IF(nvrs .NE. nvrs3) WRITE(6,*)
     $ '  +++++ warning (grin): histos produced by older version',nvrs3
! Read new index  from the disk
      READ(nouth,'(6i10)')  ((lndex(i,k),k=1,3),i=1,idmx)
      READ(nouth,'(a80)')    titld

      lenold=length
! Append content of new histos AT ONCE  at the end of storage b
      length=length+lengt3
      READ(nouth,'(3d24.16)') (b(i),i=lenold+1,length)

! Append index and titlc with new histos one by one
      lact = jadres(0)
      DO 100 l=1,idmx
      IF(lact .EQ. 0) GOTO 901
      idn= lndex(l,1)
      IF(idn .EQ. 0) GOTO 100
! Identical id's are changed by adding big number = 1000000
 10   CONTINUE
      IF( gexist(idn) ) THEN
         idn = idn +1000000*(idn/iabs(idn))
         GOTO 10 
      ENDIF
      index(lact,1)=idn
      index(lact,2)=lndex(l,2)+lenold
      index(lact,3)=lndex(l,3)
      titlc(lact)  =titld(l)
!
! Still one small correction in the newly appended histo
      istn  = index(lact,2)
      b(istn +2)  = 9d12 +     idn*10 +9d0
!
      lact=lact+1
  100 CONTINUE

!
      RETURN

 900  CONTINUE
      CALL gstop1('++++ grin: to litle space, lenmax=  ',lenmax)
 901  CONTINUE
      CALL gstop1('++++ grin: to many histos, idmx=    ',idmx)
      END




      SUBROUTINE GRIN2(IDUM1,IDUM2,IDUM3)
!     **********************************
! New version which has a possibility to 
!            ADD histograms
! If ID is not existing already then no action is taken
!     ***********************************
      IMPLICIT double precision (a-h,o-z)
      PARAMETER( idmx=400,nbuf=24,nbuf2=24)
      COMMON / cglib / b(50000)
      COMMON /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      CHARACTER*80 titlc
      COMMON / hruni / nhist
      SAVE /cglib/,/gind/, /hruni/
! Copy of the histos from the disk
      DIMENSION bz(50000)
      DIMENSION indez(idmx,3),titlz(idmx)
      CHARACTER*80 titlz
      LOGICAL gexist

      CALL ginit 
      nouth=nhist
! Read basic params
      READ(nouth,'(6i10)')   nvrsz,noutz,lenmaz,lengtz
! Check version
      IF(nvrs .NE. nvrsz) WRITE(nout,*)
     $ '  +++++ warning (grin2): histos produced by older version',nvrsz
      IF(nvrs .NE. nvrsz) WRITE(6,*)
     $ '  +++++ warning (grin2): histos produced by older version',nvrsz
! Read new index, title and bins from the disk
      READ(nouth,'(6i10)')    ((indez(i,k),k=1,3),i=1,idmx)
      READ(nouth,'(a80)')     titlz
      READ(nouth,'(3d24.16)') (bz(i),i=1,lengtz)

! Add new histos from disk to existing ones one by one
      DO 100 lz=1,idmx
      id= indez(lz,1)
      IF(id .EQ. 0) GOTO 200
      IF(.not.gexist(id)) THEN
        write(6,*) ' Grin2: unmached histo ID=', id, '  Skipped'
        goto 100
      ENDIF
! parameters of existing histo
      lact = jadres(id)
      ist  = index(lact,2)
      ist2 = ist+7
      ist3 = ist+11
      nchx = b(ist2 +1)
! parameters of the histo from the disk
      istz   = indez(lz,2)
      ist2z  = istz+7
      ist3z  = istz+11
      nchxz  = bz(ist2z +1)
      IF(nchx .NE. nchxz) THEN
        write(6,*) ' Grin2: non-equal binning ID=', id, '  Skipped' 
        goto 100
      ENDIF
! Add/Merge all additive entries of the two histos
! No of entries in index
      index(lact,3) = index(lact,3)+indez(lact,3)
! Overflows, underflows etc.
      DO i=1,12
        b(ist3+i)=b(ist3+i) +bz(ist3z+i)
      ENDDO
! Except of this one non-additive entry 
      b(ist3+13)=max(b(ist3+13),b(ist3z+13))
! Regular bin content added now!
      DO i= 1, 2*nchx
        b(ist+nbuf+i)=b(ist+nbuf+i) +bz(istz+nbuf+i)
      ENDDO
  100 CONTINUE
  200 CONTINUE

      END




      subroutine grend(chdum)
c     ***********************
      implicit double precision (a-h,o-z)
      common / hruni / nhist
      save   /hruni/
      character*(*) chdum
      close(nhist)
c======================================================================
c======================end of gbook====================================
c======================================================================
      end

C======================================================================
C======================Mini-GPLOT======================================
C======================================================================
C... Plotting using LATeX
      SUBROUTINE GPLINT(IDUM)
C     ***********************
C ...dummy routine
      END
      SUBROUTINE GPLCAP(IFILE)
C     ***********************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON / LPLDAT / NOUH1,NOUH2,ILINE
      COMMON / LPLTIT / TITCH,KEYTIT
      CHARACTER*80 TITCH
C Note that backslash definition is varying from one 
C instalation/compiler to another, you have to figure out by yourself 
C how to fill backslash code into BS
      COMMON / BSLASH / BS
      CHARACTER*1 BS,BBS
      save /LPLDAT/, /LPLTIT/, /BSLASH/
C     DATA BBS / 1H\ /
      DATA BBS / '\\' /
      BS = BBS
cc      BS = '\\'
C---
      KEYTIT= 0
      ILINE = 1
      NOUH1=IABS(IFILE)
      NOUH2=NOUH1+1
      WRITE(NOUH1,'(A,A)') BS,'voffset =  1.0cm'
      WRITE(NOUH1,'(A,A)') BS,'hoffset = -1cm'
      WRITE(NOUH1,'(A,A)') BS,'documentstyle[12pt]{article}'
      WRITE(NOUH1,'(A,A)') BS,'textwidth  = 16cm'
      WRITE(NOUH1,'(A,A)') BS,'textheight = 18cm'
      WRITE(NOUH1,'(A,A)') BS,'begin{document}'
      WRITE(NOUH1,'(A)') '  '
      WRITE(NOUH1,'(A)') '  '
      END

      SUBROUTINE GPLEND
C     *****************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON / LPLDAT / NOUH1,NOUH2,ILINE
      COMMON / BSLASH / BS
      save /LPLDAT/, /BSLASH/
      CHARACTER*1 BS
      WRITE(NOUH1,'(2A)') BS,'end{document}'
      CLOSE(NOUH1)
      END

      SUBROUTINE GPLOT(ID,CH1,CH2,KDUM)
C     *********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION YY(200),YER(200)
      CHARACTER CH1,CH2,CHR
      CHARACTER*80 TITLE
      LOGICAL GEXIST
      COMMON / LPLDAT / NOUH1,NOUH2,ILINE
      COMMON / BSLASH / BS
      CHARACTER*1 BS
      save /LPLDAT/, /BSLASH/
      DATA CHR /' '/
C return if histo non-existing
      IF(.NOT.GEXIST(ID)) GOTO 900
C ...unpack histogram
      CALL GUNPAK(ID,YY ,'    ',IDUM)
      CALL GUNPAK(ID,YER,'ERRO',IDUM)
      CALL GINBO1(ID,TITLE,NCHX,DXL,DXU)
      XL = DXL
      XU = DXU
      CALL GRANG1(ID,YL,YU)
      KAX=1200
      KAY=1200
      IF(CH1 .EQ. 'S') THEN
C ...superimpose plot
        BACKSPACE(NOUH1)
        BACKSPACE(NOUH1)
      ELSE
C ...new frame only
        CHR=CH1
        CALL LFRAM1(ID,KAX,KAY)
      ENDIF
      WRITE(NOUH1,'(A)')    '%========== next plot (line) =========='
      WRITE(NOUH1,'(A,I6)') '%==== HISTOGRAM ID=',ID
      WRITE(NOUH1,'(A,A70 )') '% ',TITLE
C...cont. line for functions
      call goptou(id,ioplog,iopsla,ioperb,iopsc1,iopsc2)
      ker = ioperb-1
      IF (iopsla .EQ. 2)  CHR='C'
C...suppress GPLOT assignments
      IF (CH2 .EQ. 'B')   CHR=' '
      IF (CH2 .EQ. '*')   CHR='*'
      IF (CH2 .EQ. 'C')   CHR='C'
C...various types of lines
      IF     (CHR .EQ. ' ') THEN
C...contour line used for histogram
          CALL PLHIST(KAX,KAY,NCHX,YL,YU,YY,KER,YER)
      ELSE IF(CHR .EQ. '*') THEN
C...marks in the midle of the bin
          CALL PLHIS2(KAX,KAY,NCHX,YL,YU,YY,KER,YER)
      ELSE IF(CHR .EQ. 'C') THEN
C...slanted (dotted) line in plotting non-MC functions
          CALL PLCIRC(KAX,KAY,NCHX,YL,YU,YY)
      ENDIF
!------------------------------!
! Ending
!------------------------------!
      WRITE(NOUH1,'(2A)') BS,'end{picture} % close entire picture '
      WRITE(NOUH1,'(2A)') BS,'end{figure}'

      RETURN
  900 WRITE(*,*) ' ++++ GPLOT: NONEXISTIG HISTO ' ,ID
      END

      SUBROUTINE LFRAM1(ID,KAX,KAY)
C     *****************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*80 TITLE
      COMMON / LPLTIT / TITCH,KEYTIT
      CHARACTER*80 TITCH
      DIMENSION TIPSY(20),TIPSX(20)
      COMMON / LPLDAT / NOUH1,NOUH2,ILINE
      DOUBLE PRECISION DXL,DXU
      COMMON / BSLASH / BS
      CHARACTER*1 BS
      save /LPLDAT/, /LPLTIT/, /BSLASH/
      DATA ICONT/0/

      ICONT=ICONT+1
      CALL GINBO1(ID,TITLE,NCHX,DXL,DXU)
      XL = DXL
      XU = DXU
      CALL GRANG1(ID,YL,YU)

      IF(ICONT .GT. 1) WRITE(NOUH1,'(2A)') BS,'newpage'
!------------------------------!
!           Header
!------------------------------!
      WRITE(NOUH1,'(A)') ' '
      WRITE(NOUH1,'(A)') ' '
      WRITE(NOUH1,'(A)') '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     $%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
      WRITE(NOUH1,'(A)') '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     $%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
      WRITE(NOUH1,'(2A)') BS,'begin{figure}[!ht]'
      WRITE(NOUH1,'(2A)') BS,'centering'
!------------------------------!
! General Caption
!------------------------------!
      WRITE(NOUH1,'(4A)') BS,'caption{',BS,'small'
      IF(KEYTIT.EQ.0) THEN
        WRITE(NOUH1,'(A)')     TITLE
      ELSE
        WRITE(NOUH1,'(A)')     TITCH
      ENDIF
      WRITE(NOUH1,'(A)') '}'
!------------------------------!
! Frames and labels
!------------------------------!
      WRITE(NOUH1,'(A)') '% =========== big frame, title etc. ======='
      WRITE(NOUH1,'(4A)') BS,'setlength{',BS,'unitlength}{0.1mm}'
      WRITE(NOUH1,'(2A)') BS,'begin{picture}(1600,1500)'
      WRITE(NOUH1,'(4A)') BS,'put(0,0){',BS,'framebox(1600,1500){ }}'
      WRITE(NOUH1,'(A)') '% =========== small frame, labeled axis ==='
      WRITE(NOUH1,'(4A,I4,A,I4,A)')
     $    BS,'put(300,250){',BS,'begin{picture}( ',KAX,',',KAY,')'
      WRITE(NOUH1,'(4A,I4,A,I4,A)')
     $    BS,'put(0,0){',BS,'framebox( ',KAX,',',KAY,'){ }}'
      WRITE(NOUH1,'(A)') '% =========== x and y axis ================'
      CALL SAXIX(KAX,XL,XU,NTIPX,TIPSX)
      CALL SAXIY(KAY,YL,YU,NTIPY,TIPSY)
      WRITE(NOUH1,'(3A)') BS,'end{picture}}'
     $                ,'% end of plotting labeled axis'
      END

      SUBROUTINE SAXIX(KAY,YL,YU,NLT,TIPSY)
C     ***************************************
C plotting x-axis with long and short tips
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION TIPSY(20)
      COMMON / LPLDAT / NOUH1,NOUH2,ILINE
      COMMON / BSLASH / BS
      CHARACTER*1 BS
      save /LPLDAT/, /BSLASH/

      DY= ABS(YU-YL)
      LY = NINT( LOG10(DY) -0.4999999d0 )
      JY = NINT(DY/10d0**LY)
      DDYL = DY*10d0**(-LY)
      IF( JY .EQ. 1)             DDYL = 10d0**LY*0.25d0
      IF( JY .GE. 2.AND.JY .LE. 3) DDYL = 10d0**LY*0.5d0
      IF( JY .GE. 4.AND.JY .LE. 6) DDYL = 10d0**LY*1.0d0
      IF( JY .GE. 7)             DDYL = 10d0**LY*2.0d0
      WRITE(NOUH1,'(A)') '% .......SAXIX........ '
      WRITE(NOUH1,'(A,I4)') '%  JY= ',JY
C-------
      NLT = INT(DY/DDYL)
      NLT = MAX0(MIN0(NLT,20),1)+1
      YY0L = NINT(YL/DDYL+0.5d0)*DDYL
      DDYS = DDYL/10d0
      YY0S = NINT(YL/DDYS+0.4999999d0)*DDYS
      P0L = KAY*(YY0L-YL)/(YU-YL)
      PDL = KAY*DDYL/(YU-YL)
      P0S = KAY*(YY0S-YL)/(YU-YL)
      PDS = KAY*DDYS/(YU-YL)
      NLT = INT(ABS(YU-YY0L)/DDYL+0.0000001d0)+1
      NTS = INT(ABS(YU-YY0S)/DDYS+0.0000001d0)+1
      DO 41 N=1,NLT
      TIPSY(N) =YY0L+ DDYL*(N-1)
  41  CONTINUE
      WRITE(NOUH1,1000)
     $ BS,'multiput('  ,P0L,  ',0)('  ,PDL,  ',0){'  ,NLT,  '}{',
     $ BS,'line(0,1){25}}',
     $ BS,'multiput('  ,P0S,  ',0)('  ,PDS,  ',0){'  ,NTS,  '}{',
     $ BS,'line(0,1){10}}'
      WRITE(NOUH1,1001)
     $ BS,'multiput('  ,P0L,  ','  ,KAY,  ')('  ,PDL,  ',0){'  ,NLT,
     $ '}{'  ,BS,  'line(0,-1){25}}',
     $ BS,'multiput('  ,P0S,  ','  ,KAY,  ')('  ,PDS,  ',0){'  ,NTS,
     $ '}{'  ,BS,  'line(0,-1){10}}'
 1000 FORMAT(2A,F8.2,A,F8.2,A,I4,3A)
 1001 FORMAT(2A,F8.2,A,I4,A,F8.2,A,I4,3A)
C ...labeling of axis
      SCMX = DMAX1(DABS(YL),DABS(YU))
      LEX  = NINT( LOG10(SCMX) -0.50001)
      DO 45 N=1,NLT
      K = NINT(KAY*(TIPSY(N)-YL)/(YU-YL))
      IF(LEX .LT. 2.AND.LEX .GT. -1) THEN
C ...without exponent
      WRITE(NOUH1,'(2A,I4,5A,F8.3,A)')
     $ BS,'put(',K,',-25){',BS,'makebox(0,0)[t]{',BS,'large $ ',
     $ TIPSY(N), ' $}}'
      ELSE
C ...with exponent
      WRITE(NOUH1,'(2A,I4,5A,F8.3,2A,I4,A)')
     $ BS,'put('  ,K,  ',-25){',BS,'makebox(0,0)[t]{',BS,'large $ ',
     $ TIPSY(N)/(10d0**LEX),BS,'cdot 10^{',LEX,'} $}}'
      ENDIF
  45  CONTINUE
      END

      SUBROUTINE SAXIY(KAY,YL,YU,NLT,TIPSY)
C     ***************************************
C plotting y-axis with long and short tips
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION TIPSY(20)
      COMMON / LPLDAT / NOUH1,NOUH2,ILINE
      COMMON / BSLASH / BS
      CHARACTER*1 BS
      save /LPLDAT/, /BSLASH/

      DY= ABS(YU-YL)
      LY = NINT( LOG10(DY) -0.49999999d0 )
      JY = NINT(DY/10d0**LY)
      DDYL = DY*10d0**(-LY)
      IF( JY .EQ. 1)             DDYL = 10d0**LY*0.25d0
      IF( JY .GE. 2.AND.JY .LE. 3) DDYL = 10d0**LY*0.5d0
      IF( JY .GE. 4.AND.JY .LE. 6) DDYL = 10d0**LY*1.0d0
      IF( JY .GE. 7)             DDYL = 10d0**LY*2.0d0
      WRITE(NOUH1,'(A)') '% .......SAXIY........ '
      WRITE(NOUH1,'(A,I4)') '%  JY= ',JY
C-------
      NLT = INT(DY/DDYL)
      NLT = MAX0(MIN0(NLT,20),1)+1
      YY0L = NINT(YL/DDYL+0.4999999d0)*DDYL
      DDYS = DDYL/10d0
      YY0S = NINT(YL/DDYS+0.5d0)*DDYS
      P0L = KAY*(YY0L-YL)/(YU-YL)
      PDL = KAY*DDYL/(YU-YL)
      P0S = KAY*(YY0S-YL)/(YU-YL)
      PDS = KAY*DDYS/(YU-YL)
      NLT= INT(ABS(YU-YY0L)/DDYL+0.0000001d0) +1
      NTS= INT(ABS(YU-YY0S)/DDYS+0.0000001d0) +1
      DO 41 N=1,NLT
      TIPSY(N) =YY0L+ DDYL*(N-1)
  41  CONTINUE
C plotting tics on vertical axis
      WRITE(NOUH1,1000)
     $ BS,'multiput(0,'  ,P0L,  ')(0,'  ,PDL  ,'){'  ,NLT,  '}{',
     $ BS,'line(1,0){25}}',
     $ BS,'multiput(0,'  ,P0S,  ')(0,'  ,PDS,  '){'  ,NTS,  '}{',
     $ BS,'line(1,0){10}}'
      WRITE(NOUH1,1001)
     $ BS,'multiput('  ,KAY,  ','  ,P0L,  ')(0,'  ,PDL,  '){'  ,NLT,
     $ '}{',BS,'line(-1,0){25}}',
     $ BS,'multiput('  ,KAY,  ','  ,P0S,  ')(0,'  ,PDS,  '){'  ,NTS,
     $ '}{',BS,'line(-1,0){10}}'
 1000 FORMAT(2A,F8.2,A,F8.2,A,I4,3A)
 1001 FORMAT(2A,I4,A,F8.2,A,F8.2,A,I4,3A)
C ...Zero line if necessary
      Z0L = KAY*(-YL)/(YU-YL)
      IF(Z0L .GT. 0D0.AND.Z0L .LT. FLOAT(KAY))
     $      WRITE(NOUH1,'(2A,F8.2,3A,I4,A)')
     $       BS,'put(0,'  ,Z0L,  '){',BS,'line(1,0){'  ,KAY,  '}}'
C ...labeling of axis
      SCMX = DMAX1(DABS(YL),DABS(YU))
      LEX  = NINT( LOG10(SCMX) -0.50001d0)
      DO 45 N=1,NLT
      K = NINT(KAY*(TIPSY(N)-YL)/(YU-YL))
      IF(LEX .LT. 2.AND.LEX .GT. -1) THEN
C ...without exponent
      WRITE(NOUH1,'(2A,I4,5A,F8.3,A)')
     $  BS,'put(-25,'  ,K,  '){',BS,'makebox(0,0)[r]{',
     $  BS,'large $ '  ,TIPSY(N),  ' $}}'
      ELSE
C ...with exponent
      WRITE(NOUH1,'(2A,I4,5A,F8.3,2A,I4,A)')
     $ BS,'put(-25,'  ,K,  '){',BS,'makebox(0,0)[r]{',
     $ BS,'large $ '
     $ ,TIPSY(N)/(10d0**LEX),  BS,'cdot 10^{'  ,LEX,  '} $}}'
      ENDIF
  45  CONTINUE
      END
      SUBROUTINE PLHIST(KAX,KAY,NCHX,YL,YU,YY,KER,YER)
C     ************************************************
C plotting contour line for histogram
C     ***********************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION YY(*),YER(*)
      CHARACTER*80 FMT1
      COMMON / LPLDAT / NOUH1,NOUH2,ILINE
      COMMON / BSLASH / BS
      CHARACTER*1 BS
      save /LPLDAT/, /BSLASH/
      WRITE(NOUH1,'(4A,I4,A,I4,A)')
     $  BS,'put(300,250){',BS,'begin{picture}( ',KAX,',',KAY,')'
      WRITE(NOUH1,'(A)') '% ========== plotting primitives =========='
C...various types of line
      IF(ILINE .EQ. 1) THEN
         WRITE(NOUH1,'(2A)') BS,'thicklines '
      ELSE
         WRITE(NOUH1,'(2A)') BS,'thinlines '
      ENDIF
C...short macros for vertical/horizontal straight lines
      WRITE(NOUH1,'(8A)')
     $ BS,'newcommand{',BS,'x}[3]{',BS,'put(#1,#2){',
     $ BS,'line(1,0){#3}}}'
      WRITE(NOUH1,'(8A)')
     $ BS,'newcommand{',BS,'y}[3]{',BS,'put(#1,#2){',
     $ BS,'line(0,1){#3}}}'
      WRITE(NOUH1,'(8A)')
     $ BS,'newcommand{',BS,'z}[3]{',BS,'put(#1,#2){',
     $ BS,'line(0,-1){#3}}}'
C   error bars
      WRITE(NOUH1,'(8A)')
     $   BS,'newcommand{',BS,'e}[3]{',
     $   BS,'put(#1,#2){',BS,'line(0,1){#3}}}'
      IX0=0
      IY0=0
      DO 100 IB=1,NCHX
      IX1 = NINT(KAX*(IB-0.00001)/NCHX)
      IY1 = NINT(KAY*(YY(IB)-YL)/(YU-YL))
      IDY = IY1-IY0
      IDX = IX1-IX0
      FMT1 = '(2(2A,I4,A,I4,A,I4,A))'
      IF( IDY .GE. 0) THEN  
         IF(IY1 .GE. 0.AND.IY1 .LE. KAY)
     $   WRITE(NOUH1,FMT1) BS,'y{',IX0,'}{',IY0,'}{',IDY,'}',
     $                     BS,'x{',IX0,'}{',IY1,'}{',IDX,'}'
      ELSE
         IF(IY1 .GE. 0.AND.IY1 .LE. KAY)
     $   WRITE(NOUH1,FMT1) BS,'z{',IX0,'}{',IY0,'}{',-IDY,'}',
     $                     BS,'x{',IX0,'}{',IY1,'}{',IDX,'}'
      ENDIF
      IX0=IX1
      IY0=IY1
      IF(KER .EQ. 1) THEN
        IX2  = NINT(KAX*(IB-0.5000d0)/NCHX)
        IERR = NINT(KAY*((YY(IB)-YER(IB))-YL)/(YU-YL))
        IE = NINT(KAY*YER(IB)/(YU-YL))
        IF(IY1 .GE. 0.AND.IY1 .LE. KAY.and.abs(ierr) .LE. 9999
     $     .and.2*ie .LE. 9999) WRITE(NOUH1,8000) BS,IX2,IERR,IE*2
      ENDIF
 100  CONTINUE
8000  FORMAT(4(A1,2He{,I4,2H}{,I5,2H}{,I4,1H}:1X ))
      WRITE(NOUH1,'(3A)') BS,'end{picture}}',
     $       ' % end of plotting histogram'
C change line-style
      ILINE= ILINE+1
      IF(ILINE .GT. 2) ILINE=1
      END
      SUBROUTINE PLHIS2(KAX,KAY,NCHX,YL,YU,YY,KER,YER)
C     ************************************************
C marks in the midle of the bin
C     **********************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION YY(*),YER(*)
      COMMON / LPLDAT / NOUH1,NOUH2,ILINE
      COMMON / BSLASH / BS
      CHARACTER*1 BS
      save /LPLDAT/, /BSLASH/

      WRITE(NOUH1,'(4A,I4,A,I4,A)')
     $ BS,'put(300,250){',BS,'begin{picture}( ',KAX,',',KAY,')'
      WRITE(NOUH1,'(A)') '% ========== plotting primitives =========='
C...various types of mark
      IRAD1= 6
      IRAD2=10
      IF(ILINE .EQ. 1) THEN
C   small filled circle
       WRITE(NOUH1,'(8A,I3,A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'circle*{',IRAD1,'}}}'
      ELSEIF(ILINE .EQ. 2) THEN
C   small open circle
       WRITE(NOUH1,'(8A,I3,A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'circle{',IRAD1,'}}}'
      ELSEIF(ILINE .EQ. 3) THEN
C   big filled circle
       WRITE(NOUH1,'(8A,I3,A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'circle*{',IRAD2,'}}}'
      ELSEIF(ILINE .EQ. 4) THEN
C   big open circle
       WRITE(NOUH1,'(8A,I3,A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'circle{',IRAD2,'}}}'
C Other symbols
      ELSEIF(ILINE .EQ. 5) THEN
       WRITE(NOUH1,'(10A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'makebox(0,0){$',BS,'diamond$}}}'
      ELSE
       WRITE(NOUH1,'(10A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'makebox(0,0){$',BS,'star$}}}'
      ENDIF
C   error bars
      WRITE(NOUH1,'(8A)')
     $   BS,'newcommand{',BS,'E}[3]{',
     $   BS,'put(#1,#2){',BS,'line(0,1){#3}}}'
      DO 100 IB=1,NCHX
      IX1 = NINT(KAX*(IB-0.5000d0)/NCHX)
      IY1 = NINT(KAY*(YY(IB)-YL)/(YU-YL))
      IF(IY1 .GE. 0.AND.IY1 .LE. KAY) WRITE(NOUH1,7000) BS,IX1,IY1
      IF(KER .EQ. 1) THEN
        IERR = NINT(KAY*((YY(IB)-YER(IB))-YL)/(YU-YL))
        IE   = NINT(KAY*YER(IB)/(YU-YL))
        IF(IY1 .GE. 0.AND.IY1 .LE. KAY.and.abs(ierr) .LE. 9999
     $       .and.2*ie .LE. 9999) WRITE(NOUH1,8000) BS,IX1,IERR,IE*2
      ENDIF
 100  CONTINUE
7000  FORMAT(4(A1,2HR{,I4,2H}{,I4,1H}:1X ))
8000  FORMAT(4(A1,2HE{,I4,2H}{,I5,2H}{,I4,1H}:1X ))
      WRITE(NOUH1,'(3A)') BS,'end{picture}}',
     $    ' % end of plotting histogram'
C change line-style
      ILINE= ILINE+1
      IF(ILINE .GT. 6) ILINE=1
      END
      SUBROUTINE PLCIRC(KAX,KAY,NCHX,YL,YU,YY)
C     ****************************************
C plots equidistant points, four-point interpolation,
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION YY(*),IX(3000),IY(3000)
      COMMON / LPLDAT / NOUH1,NOUH2,ILINE
      COMMON / BSLASH / BS
      CHARACTER*1 BS
      save /LPLDAT/, /BSLASH/
      SAVE DS

C ...various types of line
C ...distance between points is DS, radius of a point is IRAD
      IRAD2=6
      IRAD1=3
C .............
      WRITE(NOUH1,'(4A,I4,A,I4,A)')
     $  BS,'put(300,250){',BS,'begin{picture}( ',KAX,',',KAY,')'
      WRITE(NOUH1,'(A)') '% ========== plotting primitives =========='
      IF(ILINE .EQ. 1) THEN
C   small filled circle
       DS = 10
       WRITE(NOUH1,'(8A,I3,A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'circle*{',IRAD1,'}}}'
      ELSEIF(ILINE .EQ. 2) THEN
C   small open circle
       DS = 10
       WRITE(NOUH1,'(8A,I3,A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'circle{',IRAD1,'}}}'
      ELSEIF(ILINE .EQ. 3) THEN
C   big filled circle
       DS = 20
       WRITE(NOUH1,'(8A,I3,A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'circle*{',IRAD2,'}}}'
      ELSEIF(ILINE .EQ. 4) THEN
C   big open circle
       DS = 20
       WRITE(NOUH1,'(8A,I3,A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'circle{',IRAD2,'}}}'
C Other symbols
      ELSEIF(ILINE .EQ. 5) THEN
       DS = 20
       WRITE(NOUH1,'(10A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'makebox(0,0){$',BS,'diamond$}}}'
      ELSE
       DS = 20
       WRITE(NOUH1,'(10A)')
     $   BS,'newcommand{',BS,'R}[2]{',
     $   BS,'put(#1,#2){',BS,'makebox(0,0){$',BS,'star$}}}'
      ENDIF
      FACY = KAY/(YU-YL)
C plot first point
      AI  = 0.
      AJ  = (APROF( (AI/KAX)*NCHX+0.5d0, NCHX, YY) -YL)*FACY
      IPNT =1
      IX(IPNT) = INT(AI)
      IY(IPNT) = INT(AJ)
      DX =  DS
      AI0 = AI
      AJ0 = AJ
C plot next points
      DO 100 IPOIN=2,3000
C iteration to get (approximately) equal distance among ploted points
      DO  50 ITER=1,3
      AI  = AI0+DX
      AJ  = (APROF( (AI/KAX)*NCHX+0.5d0, NCHX, YY) -YL)*FACY
      DX  = DX *DS/SQRT(DX**2 + (AJ-AJ0)**2)
  50  CONTINUE
      IF(INT(AJ) .GE. 0.AND.INT(AJ) .LE. KAY.AND.INT(AI) .LE. KAX) THEN
         IPNT = IPNT+1
         IX(IPNT) = INT(AI)
         IY(IPNT) = INT(AJ)
      ENDIF
      AI0 = AI
      AJ0 = AJ
      IF(INT(AI) .GT. KAX) GOTO 101
 100  CONTINUE
 101  CONTINUE
      WRITE(NOUH1,7000) (BS,IX(I),IY(I), I=1,IPNT)
7000  FORMAT(4(A1,2HR{,I4,2H}{,I4,1H}:1X ))
      WRITE(NOUH1,'(2A)') BS,'end{picture}} % end of plotting line'
C change line-style
      ILINE= ILINE+1
      IF(ILINE .GT. 2) ILINE=1
      END
      FUNCTION APROF(PX,NCH,YY)
C     *************************
C PX is a continuous extension of the index in array YY
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION YY(*)
      X=PX
      IF(X .LT. 0.0.OR.X .GT. FLOAT(NCH+1)) THEN
        APROF= -1E-20
        RETURN
      ENDIF
      IP=INT(X)
      IF(IP .LT. 2)     IP=2
      IF(IP .GT. NCH-2) IP=NCH-2
      P=X-IP
      APROF = -(1./6.)*P*(P-1)*(P-2)  *YY(IP-1)
     $        +(1./2.)*(P*P-1)*(P-2)  *YY(IP  )
     $        -(1./2.)*P*(P+1)*(P-2)  *YY(IP+1)
     $        +(1./6.)*P*(P*P-1)      *YY(IP+2)
      END
      SUBROUTINE GPLSET(CH,XX)
*     ************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON / LPLDAT / NOUH1,NOUH2,ILINE
      save / LPLDAT /
      CHARACTER*4 CH
      KTY=NINT(XX)
      IF(CH .EQ. 'DMOD') THEN
        ILINE=KTY
      ENDIF
      END
      SUBROUTINE GPLTIT(TITLE)
*     ************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*80 TITLE
      COMMON / LPLTIT / TITCH,KEYTIT
      CHARACTER*80 TITCH
      save / LPLTIT /
      KEYTIT=1
      DO 50 K=1,80
   50 TITCH(K:K)=' '
      CALL COPCH(TITLE,TITCH)
      END



      SUBROUTINE gpltab(Npl,idl,capt,fmt,nch1,incr,npag)
C     ******************************************************
! Tables in TeX, up to 5 columns
C     ******************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE 
!------- parameters
      DIMENSION    idl(5)
      CHARACTER*16 capt(6)
      CHARACTER*8  fmt(3)
!======= commons of glibk
      COMMON / LPLDAT / nouh1,nouh2,iline
      COMMON / LPLTIT / titch,keytit
      CHARACTER*80 titch
      COMMON / BSLASH / BS
      CHARACTER*1 BS
      SAVE /LPLDAT/, /BSLASH/
!======= other
      LOGICAL gexist
      DIMENSION yyy(200),yer(200),bi(200,5),er(200,5)
      CHARACTER*80 title
      CHARACTER*1 Cn(5)
      DATA Cn /'1','2','3','4','5'/
!----------

! return if histo non-existing or to many columns
      IF(.NOT.GEXIST(ID)) GOTO 900
      IF(Npl .GT. 5 )     GOTO 901
!
! npack histograms
      CALL ginbo1( idl(1),title,nchx,dxl,dxu)
      xl = dxl
      xu = dxu
      DO n=1,Npl
        CALL gunpak( idl(n),yyy ,'    ',idum)
        CALL gunpak( idl(n),yer ,'ERRO',idum)
        DO k=1,nchx
           bi(k,n)=yyy(k)
           er(k,n)=yer(k)
        ENDDO
      ENDDO
!------------------------------!
!           Header
!------------------------------!
      WRITE(NOUH1,'(A)') ' '
      WRITE(NOUH1,'(A)') ' '
      WRITE(NOUH1,'(A)') '% ========================================='
      WRITE(NOUH1,'(A)') '% ============= begin table ==============='
      WRITE(NOUH1,'(2A)') BS,'begin{table}[!ht]'
      WRITE(NOUH1,'(2A)') BS,'centering'
!------------------------------!
! Central Caption
!------------------------------!
      WRITE(NOUH1,'(4A)') BS,'caption{',BS,'small'
      IF(KEYTIT.EQ.0) THEN
        WRITE(NOUH1,'(A)')     TITLE
      ELSE
        WRITE(NOUH1,'(A)')     TITCH
      ENDIF
      WRITE(NOUH1,'(A)') '}'
!------------------------------!
! Tabular header
!------------------------------!
      WRITE(NOUH1,'(20A)') BS,'begin{tabular}
     $ {|',  ('|c',j=1,Npl+1),  '||}'
!
      WRITE(NOUH1,'(4A)') BS,'hline',BS,'hline'
!------------------------------!
! Captions in columns
!------------------------------!
      WRITE(NOUH1,'(2A)') capt(1),('&',capt(j+1),j=1,Npl)
!
      WRITE(NOUH1,'(2A)') BS,BS
      WRITE(NOUH1,'(2A)') BS,'hline'
!----------------------------------------!
! Table content
! Note that by default RIGHT EDGE of bin is printed, as necessary for
! cumulative distributions, this can be changed with SLAN option
!----------------------------------------!
      CALL goptou(idl(1),ioplog,iopsla,ioperb,iopsc1,iopsc2)
      DO k=nch1,nchx,incr
        xi= dxl + (dxu-dxl)*k/(1d0*nchx)
        IF(iopsla.eq.2) xi= dxl + (dxu-dxl)*(k-0.5d0)/(1d0*nchx)
        IF(ioperb.eq.2) THEN
        WRITE(NOUH1,
     $  '(A,'//fmt(1)//'
     $     ,      '//Cn(Npl)//'(A,'//fmt(2)//',A,A,'//fmt(3)//'),  A)')
     $   '$', xi, ('$ & $', bi(k,j), BS, 'pm', er(k,j), j=1,Npl), '$'
        WRITE(NOUH1,'(2A)') BS,BS
        ELSE
        WRITE(NOUH1,
     $  '(A,'//fmt(1)//'
     $     ,      '//Cn(Npl)//'(A,'//fmt(2)//'),  A)')
     $   '$', xi, ('$ & $', bi(k,j), j=1,Npl), '$'
        WRITE(NOUH1,'(2A)') BS,BS
        ENDIF
      ENDDO
!------------------------------!
! Ending
!------------------------------!
      WRITE(NOUH1,'(4A)') BS,'hline',BS,'hline'
      WRITE(NOUH1,'(2A)') BS,'end{tabular}'
      WRITE(NOUH1,'(2A)') BS,'end{table}'
      WRITE(NOUH1,'(A)') '% ============= end   table ==============='
      WRITE(NOUH1,'(A)') '% ========================================='
      IF(npag .NE. 0) WRITE(NOUH1,'(2A)') BS,'newpage'

      RETURN
  900 WRITE(*,*) ' ++++ gpltab: NONEXISTIG HISTO ' ,ID
      RETURN
 901  WRITE(*,*) ' ++++ gpltab: TO MANY COLUMNS  ' ,Nplt
      END


      subroutine gmonit(mode,id,wt,wtmax,rn)
c     **************************************
c Utility program for monitoring m.c. rejection weights.
c ---------------------------------------------------------
C It is backward compatible with WMONIT except:
c  (1) for id=-1 one  should call as follows:
c      call(-1,id,0d0,1d0,1d0) or skip initialisation completely!
c  (2) maximum absolute weight is looked for,
c  (3) gprint(-id) prints weight distribution, net profit!
c  (4) no restriction id<100 any more!
c ---------------------------------------------------------
c wt is weight, wtmax is maximum weight and rn is random number.
c IF(mode .EQ. -1) then
c          initalization if entry id, 
c        - wtmax is maximum weight used for couting overweighted
c          other arguments are ignored
c ELSEIF(mode .EQ. 0) then
c          summing up weights etc. for a given event for entry id
c        - wt is current weight.
c        - wtmax is maximum weight used for couting overweighted
c          events with wt>wtmax.
c        - rn is random number used in rejection, it is used to
c          count no. of accepted (rn < wt/wtmax) and rejected
c          (wt > wt/wtmax) events,
c          if ro rejection then put rn=0d0.
c ELSEIF(mode .EQ. 1) then
c          in this mode wmonit repports on accumulated statistics
c          and the information is stored in common /cmonit/
c        - averwt= average weight wt counting all event
c        - errela= relative error of averwt
c        - nevtot= total nimber of accounted events
c        - nevacc= no. of accepted events (rn < wt/wtmax)
c        - nevneg= no. of events with negative weight (wt < 0)
c        - nevzer= no. of events with zero weight (wt = 0d0)
c        - nevove= no. of overweghted events (wt > wtmax)
c          and if you do not want to use cmonit then the value
c          the value of averwt is assigned to wt,
c          the value of errela is assigned to wtmax and
c          the value of wtmax  is assigned to rn in this mode.
c ELSEIF(mode .EQ. 2) then
c          all information defined for entry id defined above
c          for mode=2 is just printed of unit nout
c ENDIF
c note that output repport (mode=1,2) is done dynamically just for a
c given entry id only and it may be repeated many times for one id and
c for various id's as well.
c     ************************
      implicit double precision (a-h,o-z)
      parameter( idmx=400,nbuf=24,nbuf2=24)
      common / cglib / b(50000)
      common /gind/ nvrs,nout,lenmax,length,index(idmx,3),titlc(idmx)
      character*80 titlc
c special gmonit common
      common / cmonit/ averwt,errela,nevtot,nevacc,nevneg,nevove,nevzer
      save / cglib /,/gind/, /cmonit/
c
      idg = -id
      IF(id .LE. 0) then
           write(nout,*) ' =====> Gmonit: wrong id= ',id
           write(   6,*) ' =====> Gmonit: wrong id= ',id
           stop
      ENDIF
      IF(mode .EQ. -1) then
c     *******************
           nbin = nint(dabs(rn))
           IF(nbin .GT. 100) nbin =100 
           IF(nbin .EQ. 0)   nbin =1
           xl   =  wt
           xu   =  wtmax
           IF(xu .LE. xl) then
             xl = 0d0
             xu = 1d0
           ENDIF
           lact=jadres(idg)
           IF(lact .EQ. 0) then
              call gbook1(idg,' gmonit $',nbin,xl,xu)
           ELSE
              write(nout,*) ' WARNING gmonit: exists, id= ',id
              write(   6,*) ' WARNING gmonit: exists, id= ',id
           ENDIF
      ELSEIF(mode .EQ. 0) then
c     **********************
           lact=jadres(idg)
           IF(lact .EQ. 0) then
              write(nout,*) ' *****> Gmonit: uinitialized, id= ',id
              write(   6,*) ' *****> Gmonit: uinitialized, id= ',id
              call gbook1(idg,' gmonit $',1,0d0,1d0)
           ENDIF
c     standard entries
           call gf1(idg,wt,1d0)
c     additional goodies
           ist  = index(lact,2)
           ist2 = ist+7
           ist3 = ist+11
c    maximum weight -- maximum by absolute value but keeping sign
           b(ist3+13)    = max( dabs(b(ist3+13)) ,dabs(wt))
           IF(wt .NE. 0d0) b(ist3+13)=b(ist3+13) *wt/dabs(wt)
c    nevzer,nevove,nevacc
           IF(wt .EQ. 0d0)        b(ist3+10) =b(ist3+10) +1d0
           IF(wt .GT. wtmax)      b(ist3+11) =b(ist3+11) +1d0
           IF(rn*wtmax .LE. wt)   b(ist3+12) =b(ist3+12) +1d0
      ELSEIF(mode .GE. 1.or.mode .LE. 3) then
c     ***********************************
           lact=jadres(idg)
           IF(lact .EQ. 0) then
              write(nout,*) ' +++++++++ STOP in  wmonit ++++++++++++++'
              write(   6,*) ' +++++++++ STOP in  wmonit ++++++++++++++'
              write(nout,*) ' lack of initialization, id=',id
              write(   6,*) ' lack of initialization, id=',id
              STOP
           ENDIF
           ist    = index(lact,2)
           ist2   = ist+7
           ist3   = ist+11
           ntot   = nint(b(ist3 +7))
           swt    =      b(ist3 +8)
           sswt   =      b(ist3 +9)
           IF(ntot .LE. 0 .or. swt  .EQ.  0d0 )  then
              averwt=0d0
              errela=0d0
           ELSE
              averwt=swt/float(ntot)
              errela=sqrt(abs(sswt/swt**2-1d0/float(ntot)))
           ENDIF
! output through commons
           nevtot = ntot
           nevacc = b(ist3 +12)
           nevneg = b(ist3  +1)
           nevzer = b(ist3 +10)
           nevove = b(ist3 +11)
           wwmax  = b(ist3 +13)
!  output through parameters
           wt     = averwt
           wtmax  = errela
           rn     = wwmax
c  no printout for mode > 1
c  ************************
           IF(mode .EQ. 1) return
           write(nout,1003) id, averwt, errela, wwmax
           write(nout,1004) nevtot,nevacc,nevneg,nevove,nevzer
           IF(mode .EQ. 2) return
           call gprint(idg)
      ELSE
c     ****
           write(nout,*) ' =====wmonit: wrong mode',mode
           write(   6,*) ' =====wmonit: wrong mode',mode
           stop
      ENDIF
c     *****
 1003 format(
     $  ' =======================gmonit========================'
     $/,'   id           averwt         errela            wwmax'
     $/,    i5,           e17.7,         f15.9,           e17.7)
 1004 format(
     $  ' -----------------------------------------------------------'
     $/,'      nevtot      nevacc      nevneg      nevove      nevzer'
     $/,   5i12)
      end
C  !!!! LOGBOOK of corrections since 24 Nov 91 !!!!!
C  
C * line in MARRAN to long ( in printout of ijkl)
C * CHBIN2 replaced by CHBIN1
C  !!!!!!!!!!!!!!

C Library of utilities for YFS and BHLUMI programs
C version 1.0 November 91
      SUBROUTINE CHBIN3(R,ALF,BET,X,XPRIM,DJAC)
C     *****************************************
C Written: Dec. 1991
C This routine mapps variable R into X, XPRIM=1-X.
C To be employed in the integration (either ordinary or Monte Carlo)
C of any distributions resambling the binomial distribution 
C             x**(alf-1)*(1-x)**(bet-1).
C with 1> alf,bet > 0. Variables R and X are  in (0,1) range.
C Djac is the Jacobian factor d(x)/d(r).
C Mapping is such that 1/djac is very close to
C binomial distribution x**(alf-1)*(1-x)**(bet-1).
C WARNING: 
C Mapping may fail very close to R=0 and R=1. User is recommended 
C to assure that: fleps**alf < R < 1-fleps**bet, 
C where fleps = 1.d-30.
C     ************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON / INOUT  / NINP,NOUT
      SAVE   / INOUT  /
C
      IF( ALF.LE.1D-10 .OR. ALF .GT. 3D0 ) GOTO 900
      IF( BET.LE.1D-10 .OR. BET .GT. 3D0 ) GOTO 900
      X0=(1D0-ALF)/(2D0-ALF-BET)
      X0= MIN( MAX(X0, 0.001D0), 0.999D0)
      Q1=       X0**ALF            *BET*(1D0-X0)**(BET-1D0)
      Q2=       ALF*X0**(ALF-1D0)  *((1D0-X0)**BET)
      P1= Q1/(Q1+Q2)
      IF( R.LE.P1 ) THEN
         X    =  X0*(R/P1)**(1D0/ALF)
         XPRIM=  1D0-X
         DIST =  ALF* X**(ALF-1D0)  *BET*(1D0-X0)**(BET-1D0)
ccc      write(6,*) '3A:x,x1=',x,xprim
      ELSE
         XPRIM=  (1-X0)*((1D0-R)/(1D0-P1))**(1D0/BET)
         X    =  1D0- XPRIM
         DIST =  ALF*X0**(ALF-1D0)  *BET*XPRIM**(BET-1D0)
ccc      write(6,*) '3B:x,x1=',x,xprim
      ENDIF
      DJAC    =  (Q1+Q2)/DIST
      RETURN
  900 WRITE(NOUT,*) ' ++++ STOP IN CHBIN3: wrong parameters'
      WRITE(   6,*) ' ++++ STOP IN CHBIN3: wrong parameters'
      STOP
      END

      SUBROUTINE CHBIN1(R,ALF,BET,XMAX,X,DJAC)
C     ****************************************
C     last correction Dec. 91
c this mapps variable r into x.
c to be employed in the integration (either ordinary or monte carlo)
c of distributions resambling
c the binomial distribution x**(alf-1)*(1-x)**(bet-1)
c with alf > 0 and  bet arbitrary.
c variable r is in (0,1) range and x is within (0,xmax) range.
c djac is jacobian factor d(x)/d(r).
c mapping is such that 1/djac is very close to
c binomial distribution x**(alf-1)*(1-x)**(bet-1).
c WARNING: mapping may fail very close to R=0. Practically, one is
c recommended to obey: fleps**alf < r, where fleps = 1.d-30.
c Problems may also arise for very small xmax ( below 1.d-12 ).
C     ************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON / INOUT  / NINP,NOUT
      save   / INOUT  / 
C
      IF( ALF.LE.0D0 ) GOTO 900
      X0=(ALF-1D0)/(ALF+BET-2D0)
      IF(X0.GT.XMAX) X0=XMAX
      X0= MAX(X0, 0D0)
      Q1= 1D0/ALF*X0**ALF  *(1D0-X0)**(BET-1D0)
      Q2= X0**(ALF-1D0) /BET*((1D0-X0)**BET-(1D0-XMAX)**BET)
      P1= Q1/(Q1+Q2)
      IF( R.LE.P1 ) THEN
         X=X0*(R/P1)**(1D0/ALF)
         DIST= X**(ALF-1D0)*(1D0-X0)**(BET-1D0)
      ELSE
         R1= (1D0-R)/(1D0-P1)
         X = (1D0-XMAX)**BET + ((1D0-X0)**BET-(1D0-XMAX)**BET)*R1
         X = 1D0 - X**(1D0/BET)
         DIST= X0**(ALF-1D0)*(1D0-X)**(BET-1D0)
      ENDIF
      DJAC=(Q1+Q2)/DIST
      RETURN
  900 WRITE(NOUT,*) ' ========= STOP IN CHBIN1: WRONG PARAMS'
      STOP
      END


      SUBROUTINE VESK1W(MMODE,FUNSKO,PAR1,PAR2,PAR3) 
C     **********************************************
C======================================================================
C======================================================================
C===================== V E S K 1 W ====================================
C==================S. JADACH  SEPTEMBER 1985=========================== 
C==================S. JADACH  November  1991=========================== 
C======================================================================
C ONE DIMENSIONAL MONTE CARLO  SAMPLER. 
C Vesrion with weighted events! 
C DOUBLE PRECISION  FUNCTION FUNSKO IS THE DISTRIBUTION TO BE GENERATED.
C JLIM1 IS THE NUMBER OF ENTRIES IN THE EQUIDISTANT LATICE WHICH
C IS FORMED IN THE FIRST STAGE AND JLIM2 IS THE TOTAL MAXIMUM 
C NUMBER OF ENTRIES IN THE LATICE, NOTE THAT DIMENSIONS OF
C MATRICES IN /CESK8A/ SHOULD BE AT LEAST JLIM2+1 . 
C FOR MILD FUNSKO JLIM2=128 IS ENOUGH.   
C TO CREATE AN INDEPENDENT VERSION REPLACE /ESK8A/=>/ESK8B/. 
C     ********************************** 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      save
      COMMON / CESK1W / XX(1025),YY(1025),ZINT(1025),ZSUM,JMAX 
      COMMON / INOUT  / NINP,NOUT  
      DIMENSION RNUM(1)
      DATA JLIM1,JLIM2/16,257/           
      EXTERNAL FUNSKO
      DATA IWARM/0/                      
C                                        
      MODE=MMODE                         
      IF(MODE.EQ.-1) THEN                
C     ===================                
C INITIALISATION PART, SEE VINSKO FOR MORE COMMENTS
      INIRAN=1                           
      IWARM=1                            
      WT=0.                              
      SWT=0.                             
      SSWT=0.                            
      NEVS=0                             
C INITIALISATION PART, SAMPLING DISTRIBUTION FUNSKO
C AND FILLING MATRICES XX,YY,ZINT ETC.   
      JMAX=1                             
      XX(1)=0.                           
      XX(2)=1.                           
      YY(1)=FUNSKO(XX(1))                
      YY(2)=FUNSKO(XX(2))                
      IF(YY(1).LT.0.0.OR.YY(2).LT.0.0) GO TO 999 
      ZINT(1)=.5D0*(YY(2)+YY(1))*(XX(2)-XX(1))  
C
      JDIV=1                             
      DO 200 K=1,JLIM2-1                 
      IF(JMAX.LT.JLIM1) THEN             
C...    NOTE THAT DESK1W INCREMENTS JMAX=JMAX+1 IN EVERY CALL 
        CALL DESK1W(FUNSKO,JDIV)                
        JDIV=JDIV+2                      
        IF(JDIV.GT.JMAX) JDIV=1          
      ELSE                               
        JDIV=1                           
        ZMX=ZINT(1)                      
        DO 180 J=1,JMAX                  
        IF(ZMX.LT.ZINT(J)) THEN          
          ZMX=ZINT(J)                    
          JDIV=J                         
        ENDIF                            
  180   CONTINUE                         
        CALL DESK1W(FUNSKO,JDIV)                
      ENDIF                              
  200 CONTINUE                           
C                                        
C...  FINAL ADMINISTRATION, NORMALIZING ZINT ETC. 
      ZSUM1=0.                           
      ZSUM =0.                           
      DO 220 J=1,JMAX                    
      ZSUM1=ZSUM1+ZINT(J)                
      YMAX= MAX( YY(J+1),YY(J))          
      ZINT(J)=YMAX*(XX(J+1)-XX(J))       
  220 ZSUM=ZSUM+ZINT(J)                  
      SUM=0.                             
      DO 240 J=1,JMAX                    
      SUM=SUM+ZINT(J)                    
  240 ZINT(J)=SUM/ZSUM                   
C====>>>
C Crude x-section estimate
ccc      CINTEG=ZSUM 
ccc      ERRINT=0D0 
      PAR1=  ZSUM                      
      PAR2=  ZSUM
      PAR3=  ZSUM                      
C===<<<                                 
      ELSE IF(MODE.EQ.0) THEN            
C     =======================            
C GENERATION PART                        
      IF(IWARM.EQ.0) GOTO 901            
ccc  222 CONTINUE                           
ccc      IF( (WT-1D0).GT.1D-10) THEN                  
ccc        WT=WT-1.D0                       
ccc      ELSE                               
        CALL VARRAN(RNUM,1)
        RNUMB=RNUM(1)
        DO 215 J=1,JMAX                  
        JSTOP=J                          
  215   IF(ZINT(J).GT.RNUMB) GOTO 216    
  216   CONTINUE                         
        IF(JSTOP.EQ.1) THEN              
          D=RNUMB/ZINT(1)                
        ELSE                             
          D =(RNUMB-ZINT(JSTOP-1))/(ZINT(JSTOP)-ZINT(JSTOP-1))
        ENDIF                            
        X=XX(JSTOP)*(1.D0 -D )+XX(JSTOP+1)*D   
        FN=FUNSKO(X)                     
        IF(FN.LT.0.D0) GOTO 999            
        YYMAX=MAX(YY(JSTOP+1),YY(JSTOP)) 
        WT=FN/YYMAX                      
        NEVS=NEVS+1                      
        SWT=SWT+WT                       
        SSWT=SSWT+WT*WT                  
ccc      ENDIF                              
ccc      CALL VARRAN(RNUMB,1)
ccc      IF(RNUMB.GT.WT) GOTO 222           
      PAR1=  X                           
      PAR2=  FN
      PAR3=  WT                          
C                                        
      ELSE IF(MODE.EQ.1) THEN            
C     =======================            
C FINAL STATISTICS                       
C STJ 24.OCT.89                          
      CINTEG=0D0                         
      ERRINT=0D0                         
      IF(NEVS.GT.0) CINTEG=ZSUM*SWT/FLOAT(NEVS) 
      IF(NEVS.GT.0) ERRINT=SQRT(SSWT/SWT**2-1.D0/FLOAT(NEVS)) 
      PAR1=  CINTEG                      
      PAR2=  ERRINT
      PAR3=  ZSUM                      
C--
      ELSE                               
C     ====                               
      GOTO  902                          
      ENDIF                              
C     =====                              
C                                        
      RETURN                             
 901  WRITE(NOUT,9010)                   
 9010 FORMAT(' **** STOP IN VESK8A, LACK OF INITIALISATION') 
      STOP                               
 902  WRITE(NOUT,9020)                   
 9020 FORMAT(' **** STOP IN VESK8A, WRONG MODE ') 
      STOP                               
 999  WRITE(NOUT,9990)                   
 9990 FORMAT(' **** STOP IN VESK8A, NEGATIVE VALUE OF FUNSKO ') 
      STOP                               
      END                                
      SUBROUTINE DESK1W(FUNSKO,JDIV)            
C     ******************************            
C THIS ROUTINE BELONGS TO VESK8A PACKAGE 
C IT SUDIVIDES INTO TWO EQUAL PARTS THE INTERVAL 
C (XX(JDIV),XX(JDIV+1))  IN THE LATICE   
C     ***********************            
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      save
      COMMON / CESK1W / XX(1025),YY(1025),ZINT(1025),ZSUM,JMAX 
      COMMON / INOUT  / NINP,NOUT  
      EXTERNAL FUNSKO
C                                        
      XNEW=.5D0*(XX(JDIV) +XX(JDIV+1))   
      DO 100 J=JMAX,JDIV,-1              
      XX(J+2)  =XX(J+1)                  
      YY(J+2)  =YY(J+1)                  
  100 ZINT(J+1)=ZINT(J)                  
      XX(JDIV+1)= XNEW                   
      YY(JDIV+1)= FUNSKO(XNEW)           
      IF(YY(JDIV+1).LT.0.) GOTO 999      
      ZINT(JDIV)  =.5D0*(YY(JDIV+1)+YY(JDIV)  )*(XX(JDIV+1)-XX(JDIV)  ) 
      ZINT(JDIV+1)=.5D0*(YY(JDIV+2)+YY(JDIV+1))*(XX(JDIV+2)-XX(JDIV+1)) 
      JMAX=JMAX+1                        
      RETURN                             
  999 WRITE(NOUT,9000)                   
 9000 FORMAT(' **** STOP IN DESK1W, NEGATIVE VALUE OF FUNSKO ')
      STOP                               
      END                                


      SUBROUTINE VESK2W(MODE,FUNSKO,X,Y,WT)
C     *************************************
C=======================================================================
C=======================================================================
C=======================================================================
C===============TWO DIMENSIONAL SAMPLER VESK2W==========================
C=======================================================================
C=======================================================================
C=======================================================================
C                         VESK2W                                       C
C  GENERAL PURPOSE ROUTINE TO GENERATE AN ARBITRARY TWO DIMENSIONAL    C
C  DISTRIBUTION SUPPLIED BY USER IN A FORM OF FUNCTION FUNSKO(X,Y)     C
C                 WRITTEN NOVEMBER 1985                                C
C                    BY S. JADACH                                      C
C                 LAST UPDATE:  07.NOV.1990                            C
C                 version with weighted event....                      C
C======================================================================C
C VESKO2 GENERATES TWO DIMENSIONAL DISTRIBUTION DEFINED BY ARBITRARY
C FUNCTION FUNSKO(X,Y) WHERE X,Y BELONG  TO (0,1) RANGE.
C THE METHOD CONSISTS IN DIVIDING UNIT PLAQUET INTO CELLS USING
C SORT OF 'LIFE-GAME' METHOD IN WHICH THE DIVISION OF A CELLS IS MADE
C (DURING INITIALISATION) ALWAYS FOR THIS CELL WHICH CONTAINS
C A MAXIMUM VALUE OF THE INTEGRAL OVER FUNSKO IN THE CELL.
C RESULTING CELLS CONTAIN (USUALLY UP TO FACTOR TWO) EQUAL INTERGRAL
C VALUE. THE GENERATION CONSISTS IN CHOOSING RANDOMLY  A CELL
C ACCORDING TO ITS CONTENT AND THEN IN GENERATING X,Y WITHIN THE CELL.
C REJECTION METHOD IS APPLIED AT THE END OF THE PROCEDURE IN ORDER TO
C ASSURE THAT X,Y ARE DISTRIBUTED PRECISELY ACCORDING TO FUNSKO(X,Y)
C                    PARAMETERS
C -/ MODE = -1 INITIALISATION, NO (X,Y) GENERATED, CALL VESKO2(-1,D1,D2)
C    HAS TO BE MADE PRIOR  TO GENERATING FIRST (X,Y) PAIR
C -/ MODE =  0 GENERATION OF (X,Y) PAIR BY CALL VESKO2(0,X,Y)
C -/ MODE =  1 CALL VESKO2(1,VALINT,ERRINT) MAY BE DONE AFTER LAST
C    (X,Y) WAS GENERATED IN ORDER TO OBTAIN THE VALUE OF THE INTEGRAL
C    VALINT AND ITS ERROR ERRINT, INTEGRAL IS CALCULATED USING AVERAGE
C    WEIGHTS ENCOUTERED DURING GENERATION PHASE
C -/ X,Y  IF MODE=-1 THE THEY ARE DUMMY
C         IF MODE= 0 THE RESULT OF RANDOM GENERATION ACCORDING TO
C                    FUNCTION FUNSKO, X AND Y BELONG TO (0,1)
C         IF MODE= 1 X= VALUE OF INTEGRAL AND Y=ERROR (RELATIVE)
C                    WT = crude x-section
C ------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      save
      PARAMETER( JLIM1 = 64, JLIM2 = 1000 , NOUT = 6 )
      COMMON / VESW2  / XX(JLIM2,2),DX(JLIM2,2),YY(JLIM2,2,2)
     $  ,YYMX(JLIM2),ZINT(JLIM2),ZSUM,LEV(JLIM2),JMAX
      DOUBLE PRECISION DRVEC(100)
      EXTERNAL FUNSKO
      DATA IWARM/77/

      IF(MODE) 100,200,300
C...  INITIALISATION PART, SEE VINSKO FOR MORE COMMENTS
  100 CALL VINSKW(FUNSKO)
      IWARM=0
      WT=0D0
      WTMAX = 1D0
      WTMXX = WTMAX
      NEVOV=0
      SWT=0D0
      SSWT=0D0
      NEVS=0
C(((((((((((((
C     CALL HBOOK1(1, 16H WT-VESKO2     $,75,0.0D0,1.5D0)
C     CALL HMINIM(1,0)
C     CALL HBOOK2(2,16H X-Y VESKO2    $, 64,0,1, 32,0,1,0)
C     CALL HSCALE(2)
C))))))))))))
      RETURN
C...
  200 CONTINUE
C...  GENERATION PART
      IF(IWARM.EQ.77) GO TO 980
cc    IF(WT.GT.WTMAX) THEN
cc      write(6,*) ' vesko2: ev. overweighted, dont worry, wt=',wt
cc      WT=WT-WTMAX
cc      NEVOV=NEVOV+1
cc    ELSE
        CALL VARRAN(DRVEC,3)
        R = DRVEC(1)
        DO 215 J=1,JMAX
        JSTOP=J
  215   IF(ZINT(J).GT.R) GOTO 216
  216   CONTINUE
        XR=XX(JSTOP,1)+DX(JSTOP,1)*DRVEC(2)
        YR=XX(JSTOP,2)+DX(JSTOP,2)*DRVEC(3)
        FN=FUNSKO(XR,YR)
        IF(FN.LT.0.) GOTO 999
        YYMAX=YYMX(JSTOP)
        WT=FN/YYMAX
        WTMXX = MAX(WTMXX,WT)
cc      IF(NEVS.LE.(4*JLIM2).AND.WT.GT.WTMAX) THEN
cc         WTMAX=WT*1.1D0
cc         WRITE(6,*) ' VESKO2: NEVS, new WTMAX= ',NEVS,WTMAX
cc      ENDIF
        NEVS=NEVS+1
        SWT=SWT+WT
        SSWT=SSWT+WT*WT
C((((((((((
C       CALL HFILL(1,WT,0D0,1D0)
C))))))))))
ccc   ENDIF
CCC    CALL VARRAN(DRVEC,1)
ccc    RN=DRVEC(1)
ccc   IF(WTMAX*RN.GT.WT) GOTO 200
      X=XR
      Y=YR
C((((((((((
C     CALL HFILL(2,XR,YR)
C))))))))))
      RETURN
C...
  300 CONTINUE
C THIS IS THE VALUE OF THE INTEGRAL
      CINTEG=ZSUM*SWT/NEVS
C AND ITS ERROR
      ERRINT=SQRT(SSWT/SWT**2-1D0/NEVS)
      X=CINTEG
      Y=ERRINT
      WT=ZSUM
C((((((((((
C     CALL HPRINT(1)
C     CALL HDELET(1)
C     CALL HPRINT(2)
C     CALL HDELET(2)
      PRINT 7000,NEVS,NEVOV,WTMAX,WTMXX
 7000 FORMAT(' VESK2W: NEVS,NEVOV,WTMAX,WTMXX= ',2I7,2F7.3)
C))))))))))
      RETURN
  980 WRITE(NOUT,9002)
 9002 FORMAT(' **** STOP IN VESK2W, LACK OF INITIALISATION   ')
      STOP
  999 WRITE(NOUT,9004)
 9004 FORMAT(' **** STOP IN VESK2W, NEGATIVE VALUE OF FUNSKO ')
      STOP
      END

      SUBROUTINE VINSKW(FUNSKO)
C     *************************
C THIS ROUTINE BELONGS TO VESKO2 PACKAGE
C JLIM1 IS THE NUMBER OF CELLS, DIVISION OF THE UNIT PLAQUE INTO CELLS
C IS MADE IN THE FIRST STAGE.    JLIM2 IS THE TOTAL MAXIMUM
C NUMBER OF CELLS, NOTE THAT DIMENSIONS OF
C MATRICES IN /VESKOA/ SHOULD BE AT LEAST JLIM2
C     **********************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      save
C ------------------------------------------------------------
      PARAMETER( JLIM1 = 64, JLIM2 = 1000 , NOUT = 6 )
      COMMON / VESW2  / XX(JLIM2,2),DX(JLIM2,2),YY(JLIM2,2,2)
     $  ,YYMX(JLIM2),ZINT(JLIM2),ZSUM,LEV(JLIM2),JMAX
      EXTERNAL FUNSKO

C...  INITIALISATION PART, SAMPLING DISTRIBUTION FUNSKO
C...  AND FILLING MATRICES XX,YY,ZINT ETC.
      JMAX=1
      XX(1,1)=0D0
      XX(1,2)=0D0
      DX(1,1)=1D0
      DX(1,2)=1D0
      LEV(1)=1
      SUM=0D0
      DO 150 I=1,2
      DO 150 K=1,2
C... THIS IS NOT ELEGANT BUT SIMPLE
      YY(1,I,K)=FUNSKO(XX(1,1)+(I-1.)*DX(1,1),XX(1,2)+(K-1.)*DX(1,2))
      IF(YY(1,I,K).LT.0.0) GO TO 999
  150 SUM=SUM+YY(1,I,K)
      ZINT(1)=SUM*DX(1,1)*DX(1,2)/4D0

      JDIV=1
      DO 200 KK=1,JLIM2-1
      IF(JMAX.LT.JLIM1) THEN
C...    NOTE THAT DIVSKW INCREMENTS JMAX=JMAX+1 IN EVERY CALL
        CALL DIVSKW(JDIV,FUNSKO)
C(((((((((((
c      IF(JMAX.EQ.JLIM1) THEN
c      PRINT 9900,JMAX,(LEV(I),I=1,JMAX)
c 9900 FORMAT(///,' JMAX...  LEV LEV LEV LEV LEV',I10,/(24I5))
c      PRINT 9901,((XX(JD,I),I=1,2),JD=1,JMAX)
c 9901 FORMAT('  XX XX XX XX XX XX XX  ',/(10E12.5))
c      PRINT 9902,((DX(JD,I),I=1,2),JD=1,JMAX)
c 9902 FORMAT('  DX  DX DX DX DX DX ',/(10E12.5))
c      PRINT 9903,(((YY(JD,I,K),I=1,2),K=1,2),JD=1,JMAX)
c 9903 FORMAT('  YY  YY YY YY YY YY ',/(8E15.5))
c      PRINT 9904,(ZINT(I),I=1,JMAX)
c 9904 FORMAT('   ZINT ZINT ZINT ZINT ',/(10E12.5))
c      ENDIF
C))))))))))))
        JDIV=JDIV+2
        IF(JDIV.GT.JMAX) JDIV=1
      ELSE
        JDIV=1
        ZMX=ZINT(1)
        DO 180 J=1,JMAX
        IF(ZMX.LT.ZINT(J)) THEN
          ZMX=ZINT(J)
          JDIV=J
        ENDIF
  180   CONTINUE
        CALL DIVSKW(JDIV,FUNSKO)
      ENDIF
  200 CONTINUE

C(((((((((((
c      JPRN=64
c      PRINT 9910,JMAX,(LEV(I),I=1,JMAX)
c 9910 FORMAT(/,' JMAX...  LEV LEV LEV LEV LEV',I10,/(24I5))
c      IF(JMAX.LE.JPRN) PRINT 9911,((XX(JD,I),I=1,2),JD=1,JMAX)
c 9911 FORMAT('  XX XX XX XX XX XX XX  ',/(10E12.5))
c      IF(JMAX.LE.JPRN) PRINT 9912,((DX(JD,I),I=1,2),JD=1,JMAX)
c 9912 FORMAT('  DX  DX DX DX DX DX ',/(10E12.5))
c      IF(JMAX.LE.JPRN) PRINT 9913,(((YY(JD,I,K),I=1,2),K=1,2),JD=1,JMAX)
c 9913 FORMAT('  YY  YY YY YY YY YY ',/(8E15.5))
c      IF(JMAX.LE.JPRN) PRINT 9914,(ZINT(I),I=1,JMAX)
c 9914 FORMAT('   ZINT ZINT ZINT ZINT ',/(10E12.5))
C     DO 902 J=1,JMAX
C     Z=1D0*J-.5D0
C 902 CALL HFILL(202,Z,ZINT(J))
C))))))))))))
C...  FINAL ADMINISTRATION, NORMALIZING ZINT ETC.
      ZSUM1=0D0
      ZSUM =0D0
      DO 260 J=1,JMAX
      ZSUM1=ZSUM1+ZINT(J)
      YMAX= 0D0
      DO 250 I=1,2
      DO 250 K=1,2
  250 YMAX= MAX(YMAX,YY(J,I,K))
      YYMX(J)=YMAX
      ZINT(J)=YMAX*DX(J,1)*DX(J,2)
  260 ZSUM=ZSUM+ZINT(J)
C((((((((
      ZR=ZSUM1/ZSUM
      PRINT 7000,ZR
 7000 FORMAT(' /////// ZSUM1/ZSUM= ',F20.8)
C)))))))))
      SUM=0D0
      DO 240 J=1,JMAX
      SUM=SUM+ZINT(J)
  240 ZINT(J)=SUM/ZSUM
C(((((((((((
c     JPRN=64
c     PRINT 9932,JMAX
c9932 FORMAT(/'=====JMAX ZINT ZINT ZINT  ',I10)
c     IF(JMAX.LE.JPRN) PRINT 9935,(ZINT(I),I=1,JMAX)
c9935            FORMAT(10E12.5)
C     DO 901 J=2,JMAX
C 901 CALL HFILL(201,(ZINT(J)-ZINT(J-1))*JMAX)
C     CALL HFILL(201,ZINT(1)*JMAX)
C))))))))))))
      RETURN
  999 WRITE(NOUT,9000)
 9000 FORMAT(' **** STOP IN VINSKW, NEGATIVE VALUE OF FUNSKO ')
      STOP
      END

      SUBROUTINE DIVSKW(JD,FUNSKO)
C     ****************************
C THIS ROUTINE BELONGS TO VESKO2 PACKAGE
C IT SUBDIVIDES ONE CELL (NO. JD) INTO TWO EQUAL SIZE CELLS
C     **********************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      save
C ------------------------------------------------------------
      PARAMETER( JLIM1 = 64, JLIM2 = 1000 , NOUT = 6 )
      COMMON / VESW2  / XX(JLIM2,2),DX(JLIM2,2),YY(JLIM2,2,2)
     $  ,YYMX(JLIM2),ZINT(JLIM2),ZSUM,LEV(JLIM2),JMAX
      EXTERNAL FUNSKO

C...  MOOVE TO MAKE A HOLE FOR A NEW ENTRY (ONE ADDITIONAL CELL)
      DO 100 J=JMAX,JD,-1
      ZINT(J+1)=ZINT(J)
      LEV(J+1)=LEV(J)
      DO 100 I=1,2
      XX(J+1,I)  =XX(J,I)
      DX(J+1,I)  =DX(J,I)
      DO 100 K=1,2
  100 YY(J+1,I,K)  =YY(J,I,K)
C...  CREATE TWO NEW CELLS AND STORE THEM
      LL= MOD(LEV(JD),2)+1
      DX(JD,LL)=DX(JD,LL)/2D0
      DX(JD+1,LL)=DX(JD+1,LL)/2D0
      XX(JD+1,LL)=XX(JD,LL)+DX(JD,LL)
      IF(LL.EQ.1) THEN
        DO 150 I=1,2
C... THIS IS NOT ELEGANT, PROBABLY COULD BE DONE BETTER
        YY(JD,2,I)=FUNSKO(XX(JD,1)+DX(JD,1),XX(JD,2)+(I-1.)*DX(JD,2))
  150   YY(JD+1,1,I)=YY(JD,2,I)
      ELSE
        DO 152 I=1,2
        YY(JD,I,2)=FUNSKO(XX(JD,1)+(I-1.)*DX(JD,1),XX(JD,2)+DX(JD,2))
  152   YY(JD+1,I,1)=YY(JD,I,2)
      ENDIF
C...  ESTIMATE THE INTEGRALS OVER NEW CELLS RESULTING FROM DIVISION
      DO 220 JDV=JD,JD+1
      LEV(JDV)=LEV(JDV)+1
      SUM=0D0
      DO 210 I=1,2
      DO 210 K=1,2
      IF(YY(JDV,I,K).LT.0.D0) GO TO 999
  210 SUM=SUM+YY(JDV,I,K)
  220 ZINT(JDV) =SUM*DX(JDV,1)*DX(JDV,2)/4D0
      JMAX=JMAX+1
      RETURN
  999 WRITE(NOUT,9000)
 9000 FORMAT(' **** STOP IN DIVSKW, NEGATIVE VALUE OF FUNSKO ')
      STOP
      END


      SUBROUTINE GAUSJD(FUN,AA,BB,EEPS,RESULT)
C     ****************************************
C Gauss integration by S. Jadach, Oct. 90.
C This is NON-ADAPTIVE (!!!!) UNOPTIMIZED (!!!) integration subprogram.
C     *************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION WG(12),XX(12)
      COMMON / INOUT  / NINP,NOUT
      EXTERNAL FUN
      save /inout/,wg,xx,ITERMX 
      DATA WG
     $/0.101228536290376D0, 0.222381034453374D0, 0.313706645877887D0,
     $ 0.362683783378362D0, 0.027152459411754D0, 0.062253523938648D0,
     $ 0.095158511682493D0, 0.124628971255534D0, 0.149595988816577D0,
     $ 0.169156519395003D0, 0.182603415044924D0, 0.189450610455069D0/
      DATA XX
     $/0.960289856497536D0, 0.796666477413627D0, 0.525532409916329D0,
     $ 0.183434642495650D0, 0.989400934991650D0, 0.944575023073233D0,
     $ 0.865631202387832D0, 0.755404408355003D0, 0.617876244402644D0,
     $ 0.458016777657227D0, 0.281603550779259D0, 0.095012509837637D0/
      DATA ITERMX / 15/
      EPS=ABS(EEPS)
      A=AA
      B=BB
      NDIVI=1
C iteration over subdivisions terminated by precision requirement
      DO 400 ITER=1,ITERMX
      CALK8  =0D0
      CALK16 =0D0
C sum over DELTA subintegrals
      DO 200 K = 1,NDIVI
      DELTA = (B-A)/NDIVI
      X1    =  A + (K-1)*DELTA
      X2    =  X1+ DELTA
      XMIDLE= 0.5D0*(X2+X1)
      RANGE = 0.5D0*(X2-X1)
      SUM8 =0D0
      SUM16=0D0
C 8- and 12-point   Gauss integration over single DELTA subinterval
      DO 100 I=1,12
      XPLUS= XMIDLE+RANGE*XX(I)
      XMINU= XMIDLE-RANGE*XX(I)
      FPLUS=FUN(XPLUS)
      FMINU=FUN(XMINU)
      IF(I.LE.4) THEN
          SUM8 =SUM8  +(FPLUS+FMINU)*WG(I)/2D0
      ELSE
          SUM16=SUM16 +(FPLUS+FMINU)*WG(I)/2D0
      ENDIF
  100 CONTINUE
      CALK8 = CALK8 + SUM8 *(X2-X1)
      CALK16= CALK16+ SUM16*(X2-X1)
  200 CONTINUE
      ERABS = ABS(CALK16-CALK8)
      ERELA = 0D0
      IF(CALK16.NE.0D0) ERELA= ERABS/ABS(CALK16)
c     write(6,*) 'gausjd: CALK8,CALK16=',ITER,CALK8,CALK16,ERELA
C precision check to terminate integration
      IF(EEPS.GT.0D0) THEN
        IF(ERABS.LT. EPS) GOTO 800
      ELSE
        IF(ERELA.LT. EPS) GOTO 800
      ENDIF
  400 NDIVI=NDIVI*2
      WRITE(NOUT,*) ' +++++ GAUSJD:  REQUIRED PRECISION TO HIGH!'
      WRITE(NOUT,*) ' +++++ GAUSJD:  ITER,ERELA=',ITER,ERELA
  800 RESULT= CALK16
      END


      SUBROUTINE WMONIT(MODE,ID,WT,WTMAX,RN)
C     **************************************
C last correction 19 sept. 89
C Utility program for monitoring M.C. rejection weights.
C ID is weight idendifier, maximum IDMX (defined below).
C WT IS WEIGHT, WTMAX IS MAXIMUM WEIGHT AND RN IS RANDOM NUMBER.
C IF(MODE.EQ.-1) THEN
C          INITALIZATION IF ENTRY ID, OTHER ARGUMENTS ARE IGNORED
C ELSEIF(MODE.EQ.0) THEN
C          SUMMING UP WEIGHTS ETC. FOR A GIVEN EVENT FOR ENTRY ID
C        - WT IS CURRENT WEIGHT.
C        - WTMAX IS MAXIMUM WEIGHT USED FOR COUTING OVERWEIGHTED
C          EVENTS WITH WT>WTMAX.
C        - RN IS RANDOM NUMBER USED IN REJECTION, IT IS USED TO
C          COUNT NO. OF ACCEPTED (RN<WT/WTMAX) AND REJECTED
C          (WT>WT/WTMAX) EVENTS,
C          IF RO REJECTION THEN PUT RN=0D0.
C ELSEIF(MODE.EQ.1) THEN
C          IN THIS MODE WMONIT REPPORTS ON ACCUMULATED STATISTICS
C          AND THE INFORMATION IS STORED IN COMMON /CMONIT/
C        - AVERWT= AVERAGE WEIGHT WT COUNTING ALL EVENT
C        - ERRELA= RELATIVE ERROR OF AVERWT
C        - NEVTOT= TOTAL NIMBER OF ACCOUNTED EVENTS
C        - NEVACC= NO. OF ACCEPTED EVENTS (RN<WT\WTMAX)
C        - NEVNEG= NO. OF EVENTS WITH NEGATIVE WEIGHT (WT<0)
C        - NEVZER= NO. OF EVENTS WITH ZERO WEIGHT (WT.EQ.0D0)
C        - NEVOVE= NO. OF OVERWEGHTED EVENTS (WT>WTMAX)
C          AND IF YOU DO NOT WANT TO USE CMONIT THEN THE VALUE
C          The value of AVERWT is assigned to WT,
C          the value of ERRELA is assigned to WTMAX and
C          the value of WTMAX  is assigned to RN in this mode.
C ELSEIF(MODEE.EQ.2) THEN
C          ALL INFORMATION DEFINED FOR ENTRY ID DEFINED ABOVE
C          FOR MODE=2 IS JUST PRINTED OF UNIT NOUT
C ENDIF
C NOTE THAT OUTPUT REPPORT (MODE=1,2) IS DONE DYNAMICALLY JUST FOR A
C GIVEN ENTRY ID ONLY AND IT MAY BE REPEATED MANY TIMES FOR ONE ID AND
C FOR VARIOUS ID'S AS WELL.
C     ************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      save
      PARAMETER(IDMX=100)
      COMMON / CMONIT/ AVERWT,ERRELA,NEVTOT,NEVACC,NEVNEG,NEVOVE,NEVZER
      COMMON / INOUT  / NINP,NOUT
      INTEGER NTOT(IDMX),NACC(IDMX),NNEG(IDMX),NOVE(IDMX),NZER(IDMX)
      DIMENSION SWT(IDMX),SSWT(IDMX),WWMX(IDMX)
      DATA NTOT /IDMX* -1/  SWT /IDMX*   0D0/
      DATA SSWT /IDMX*0D0/ WWMX /IDMX*-1D-20/
C
      IF(ID.LE.0.OR.ID.GT.IDMX) THEN
           WRITE(NOUT,*) ' =====WMONIT: WRONG ID',ID
           STOP
      ENDIF
      IF(MODE.EQ.-1) THEN
           NTOT(ID)=0
           NACC(ID)=0
           NNEG(ID)=0
           NZER(ID)=0
           NOVE(ID)=0
           SWT(ID)   =0D0
           SSWT(ID)  =0D0
           WWMX(ID)  = -1D-20
      ELSEIF(MODE.EQ.0) THEN
           IF(NTOT(ID).LT.0) THEN
              WRITE(NOUT,*) ' ==== WARNING FROM WMONIT: '
              WRITE(NOUT,*) ' LACK OF INITIALIZATION, ID=',ID
           ENDIF
           NTOT(ID)=NTOT(ID)+1
           SWT(ID)=SWT(ID)+WT
           SSWT(ID)=SSWT(ID)+WT**2
           WWMX(ID)= MAX(WWMX(ID),WT)
           IF(WT.EQ.0D0)   NZER(ID)=NZER(ID)+1
           IF(WT.LT.0D0)   NNEG(ID)=NNEG(ID)+1
           IF(WT.GT.WTMAX)      NOVE(ID)=NOVE(ID)+1
           IF(RN*WTMAX.LE.WT)   NACC(ID)=NACC(ID)+1
      ELSEIF(MODE.EQ.1) THEN
           IF(NTOT(ID).LT.0) THEN
              WRITE(NOUT,*) ' ==== WARNING FROM WMONIT: '
              WRITE(NOUT,*) ' LACK OF INITIALIZATION, ID=',ID
           ENDIF
           IF(NTOT(ID).LE.0.OR.SWT(ID).EQ.0D0)  THEN
              AVERWT=0D0
              ERRELA=0D0
           ELSE
              AVERWT=SWT(ID)/FLOAT(NTOT(ID))
              ERRELA=SQRT(ABS(SSWT(ID)/SWT(ID)**2-1D0/FLOAT(NTOT(ID))))
           ENDIF
           NEVTOT=NTOT(ID)
           NEVACC=NACC(ID)
           NEVNEG=NNEG(ID)
           NEVZER=NZER(ID)
           NEVOVE=NOVE(ID)
           WT=AVERWT
           WTMAX=ERRELA
           RN    =WWMX(ID)
      ELSEIF(MODE.EQ.2) THEN
           IF(NTOT(ID).LE.0.OR.SWT(ID).EQ.0D0)  THEN
              AVERWT=0D0
              ERRELA=0D0
           ELSE
              AVERWT=SWT(ID)/FLOAT(NTOT(ID))
              ERRELA=SQRT(ABS(SSWT(ID)/SWT(ID)**2-1D0/FLOAT(NTOT(ID))))
              WWMAX=WWMX(ID)
           ENDIF
           WRITE(NOUT,1003) ID, AVERWT, ERRELA, WWMAX
           WRITE(NOUT,1004) NTOT(ID),NACC(ID),NNEG(ID),NOVE(ID),NZER(ID)
           WT=AVERWT
           WTMAX=ERRELA
           RN    =WWMX(ID)
      ELSE
           WRITE(NOUT,*) ' =====WMONIT: WRONG MODE',MODE
           STOP
      ENDIF
 1003 FORMAT(
     $  ' =======================WMONIT========================'
     $/,'   ID           AVERWT         ERRELA            WWMAX'
     $/,    I5,           E17.7,         F15.9,           E17.7)
 1004 FORMAT(
     $  ' -----------------------------------------------------------'
     $/,'      NEVTOT      NEVACC      NEVNEG      NEVOVE      NEVZER'
     $/,   5I12)
      END

      SUBROUTINE WMONI2(MODE,ID,WT,WTMAX,RN)
C     **************************************
C -------------- SECOND COPY OF WMONIT ----------------
C last correction 19 sept. 89
C Utility program for monitoring M.C. rejection weights.
C ID is weight idendifier, maximum IDMX (defined below).
C WT IS WEIGHT, WTMAX IS MAXIMUM WEIGHT AND RN IS RANDOM NUMBER.
C IF(MODE.EQ.-1) THEN
C          INITALIZATION IF ENTRY ID, OTHER ARGUMENTS ARE IGNORED
C ELSEIF(MODE.EQ.0) THEN
C          SUMMING UP WEIGHTS ETC. FOR A GIVEN EVENT FOR ENTRY ID
C        - WT IS CURRENT WEIGHT.
C        - WTMAX IS MAXIMUM WEIGHT USED FOR COUTING OVERWEIGHTED
C          EVENTS WITH WT>WTMAX.
C        - RN IS RANDOM NUMBER USED IN REJECTION, IT IS USED TO
C          COUNT NO. OF ACCEPTED (RN<WT/WTMAX) AND REJECTED
C          (WT>WT/WTMAX) EVENTS,
C          IF RO REJECTION THEN PUT RN=0D0.
C ELSEIF(MODE.EQ.1) THEN
C          IN THIS MODE WMONIT REPPORTS ON ACCUMULATED STATISTICS
C          AND THE INFORMATION IS STORED IN COMMON /CMONIT/
C        - AVERWT= AVERAGE WEIGHT WT COUNTING ALL EVENT
C        - ERRELA= RELATIVE ERROR OF AVERWT
C        - NEVTOT= TOTAL NIMBER OF ACCOUNTED EVENTS
C        - NEVACC= NO. OF ACCEPTED EVENTS (RN<WT\WTMAX)
C        - NEVNEG= NO. OF EVENTS WITH NEGATIVE WEIGHT (WT<0)
C        - NEVZER= NO. OF EVENTS WITH ZERO WEIGHT (WT.EQ.0D0)
C        - NEVOVE= NO. OF OVERWEGHTED EVENTS (WT>WTMAX)
C          AND IF YOU DO NOT WANT TO USE CMONIT THEN THE VALUE
C          The value of AVERWT is assigned to WT,
C          the value of ERRELA is assigned to WTMAX and
C          the value of WTMAX  is assigned to RN in this mode.
C ELSEIF(MODEE.EQ.2) THEN
C          ALL INFORMATION DEFINED FOR ENTRY ID DEFINED ABOVE
C          FOR MODE=2 IS JUST PRINTED OF UNIT NOUT
C ENDIF
C NOTE THAT OUTPUT REPPORT (MODE=1,2) IS DONE DYNAMICALLY JUST FOR A
C GIVEN ENTRY ID ONLY AND IT MAY BE REPEATED MANY TIMES FOR ONE ID AND
C FOR VARIOUS ID'S AS WELL.
C     ************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      save
      PARAMETER(IDMX=100)
      COMMON / CMONI2/ AVERWT,ERRELA,NEVTOT,NEVACC,NEVNEG,NEVOVE,NEVZER
      COMMON / INOUT  / NINP,NOUT
      INTEGER NTOT(IDMX),NACC(IDMX),NNEG(IDMX),NOVE(IDMX),NZER(IDMX)
      DIMENSION SWT(IDMX),SSWT(IDMX),WWMX(IDMX)
      DATA NTOT /IDMX* -1/  SWT /IDMX*   0D0/
      DATA SSWT /IDMX*0D0/ WWMX /IDMX*-1D-20/
C
      IF(ID.LE.0.OR.ID.GT.IDMX) THEN
           WRITE(NOUT,*) ' =====WMONI2: WRONG ID',ID
           STOP
      ENDIF
      IF(MODE.EQ.-1) THEN
           NTOT(ID)=0
           NACC(ID)=0
           NNEG(ID)=0
           NZER(ID)=0
           NOVE(ID)=0
           SWT(ID)   =0D0
           SSWT(ID)  =0D0
           WWMX(ID)  = -1D-20
      ELSEIF(MODE.EQ.0) THEN
           IF(NTOT(ID).LT.0) THEN
              WRITE(NOUT,*) ' ==== WARNING FROM WMONIT: '
              WRITE(NOUT,*) ' LACK OF INITIALIZATION, ID=',ID
           ENDIF
           NTOT(ID)=NTOT(ID)+1
           SWT(ID)=SWT(ID)+WT
           SSWT(ID)=SSWT(ID)+WT**2
           WWMX(ID)= MAX(WWMX(ID),WT)
           IF(WT.EQ.0D0)   NZER(ID)=NZER(ID)+1
           IF(WT.LT.0D0)   NNEG(ID)=NNEG(ID)+1
           IF(WT.GT.WTMAX)      NOVE(ID)=NOVE(ID)+1
           IF(RN*WTMAX.LE.WT)   NACC(ID)=NACC(ID)+1
      ELSEIF(MODE.EQ.1) THEN
           IF(NTOT(ID).LT.0) THEN
              WRITE(NOUT,*) ' ==== WARNING FROM WMONI2: '
              WRITE(NOUT,*) ' LACK OF INITIALIZATION, ID=',ID
           ENDIF
           IF(NTOT(ID).LE.0.OR.SWT(ID).EQ.0D0)  THEN
              AVERWT=0D0
              ERRELA=0D0
           ELSE
              AVERWT=SWT(ID)/FLOAT(NTOT(ID))
              ERRELA=SQRT(ABS(SSWT(ID)/SWT(ID)**2-1D0/FLOAT(NTOT(ID))))
           ENDIF
           NEVTOT=NTOT(ID)
           NEVACC=NACC(ID)
           NEVNEG=NNEG(ID)
           NEVZER=NZER(ID)
           NEVOVE=NOVE(ID)
           WT=AVERWT
           WTMAX=ERRELA
           RN    =WWMX(ID)
      ELSEIF(MODE.EQ.2) THEN
           IF(NTOT(ID).LE.0.OR.SWT(ID).EQ.0D0)  THEN
              AVERWT=0D0
              ERRELA=0D0
           ELSE
              AVERWT=SWT(ID)/FLOAT(NTOT(ID))
              ERRELA=SQRT(ABS(SSWT(ID)/SWT(ID)**2-1D0/FLOAT(NTOT(ID))))
              WWMAX=WWMX(ID)
           ENDIF
           WRITE(NOUT,1003) ID, AVERWT, ERRELA, WWMAX
           WRITE(NOUT,1004) NTOT(ID),NACC(ID),NNEG(ID),NOVE(ID),NZER(ID)
           WT=AVERWT
           WTMAX=ERRELA
           RN    =WWMX(ID)
      ELSE
           WRITE(NOUT,*) ' =====WMONI2: WRONG MODE',MODE
           STOP
      ENDIF
 1003 FORMAT(
     $  ' =======================WMONI2========================'
     $/,'   ID           AVERWT         ERRELA            WWMAX'
     $/,    I5,           E17.7,         F15.9,           E17.7)
 1004 FORMAT(
     $  ' -----------------------------------------------------------'
     $/,'      NEVTOT      NEVACC      NEVNEG      NEVOVE      NEVZER'
     $/,   5I12)
      END

      FUNCTION GAUS(F,A,B,EEPS)  
C     *************************   
C THIS IS ITERATIVE INTEGRATION PROCEDURE                             
C ORIGINATES  PROBABLY FROM CERN LIBRARY                              
C IT SUBDIVIDES INEGRATION RANGE UNTIL REQUIRED PRECISION IS REACHED  
C PRECISION IS A DIFFERENCE FROM 8 AND 16 POINT GAUSS ITEGR. RESULT   
C EEPS POSITIVE TREATED AS ABSOLUTE PRECISION                         
C EEPS NEGATIVE TREATED AS RELATIVE PRECISION                         
C     *************************              
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
      DIMENSION W(12),X(12)        
      COMMON / INOUT  / NINP,NOUT      
      EXTERNAL F                             
      DATA CONST /1.0D-19/     
      save     / INOUT/, CONST, W, X
      DATA W                       
     1/0.10122 85362 90376d0, 0.22238 10344 53374d0, 
     1 0.31370 66458 77887d0, 
     2 0.36268 37833 78362d0, 0.02715 24594 11754d0, 
     2 0.06225 35239 38648d0, 
     3 0.09515 85116 82493d0, 0.12462 89712 55534d0, 
     3 0.14959 59888 16577d0, 
     4 0.16915 65193 95003d0, 0.18260 34150 44924d0, 
     4 0.18945 06104 55069d0/ 
      DATA X                       
     1/0.96028 98564 97536d0, 0.79666 64774 13627d0, 
     1 0.52553 24099 16329d0, 
     2 0.18343 46424 95650d0, 0.98940 09349 91650d0, 
     1 0.94457 50230 73233d0, 
     3 0.86563 12023 87832d0, 0.75540 44083 55003d0, 
     1 0.61787 62444 02644d0, 
     4 0.45801 67776 57227d0, 0.28160 35507 79259d0, 
     1 0.09501 25098 37637d0/ 
      EPS=ABS(EEPS)                
      DELTA=CONST*ABS(A-B)         
      GAUS=0D0                     
      AA=A                         
    5 Y=B-AA                       
      IF(ABS(Y) .LE. DELTA) RETURN 
    2 BB=AA+Y                      
      C1=0.5D0*(AA+BB)             
      C2=C1-AA                     
      S8=0D0                       
      S16=0D0                      
      DO 1 I=1,4                   
      U=X(I)*C2                    
    1 S8=S8+W(I)*(F(C1+U)+F(C1-U)) 
      DO 3 I=5,12                  
      U=X(I)*C2                    
    3 S16=S16+W(I)*(F(C1+U)+F(C1-U))                                  
      S8=S8*C2                     
      S16=S16*C2                   
      IF(EEPS.LT.0D0) THEN         
        IF(ABS(S16-S8) .GT. EPS*ABS(S16)) GO TO 4                     
      ELSE             
        IF(ABS(S16-S8) .GT. EPS) GO TO 4                  
      ENDIF            
      GAUS=GAUS+S16    
      AA=BB            
      GO TO 5          
    4 Y=0.5D0*Y        
      IF(ABS(Y) .GT. DELTA) GOTO 2                        
      WRITE(NOUT,7)                          
      GAUS=0D0                
      RETURN                  
    7 FORMAT(1X,36HGAUS  ... TOO HIGH ACCURACY REQUIRED)         
      END                     

C  CORRECTIONS ST. JADACH   (STJ)
C    DOUBLE PRECISION,
C    THIS PROGRAM IS NOT REALY ABLE TO FIND INTEGRAL
C    WITH RELATIVE PRECISION, EPS IS NOW ABSOLUTE ERROR (INPUT ONLY!!)
C.......................................................................
C
C   PURPOSE           - INTEGRATE A FUNCTION F(X)
C   METHOD            - ADAPTIVE GAUSSIAN
C   USAGE             - CALL GADAP(A0,B0,F,EPS,SUM)
C   PARAMETERS  A0    - LOWER LIMIT (INPUT,REAL)
C               B0    - UPPER LIMIT (INPUT,REAL)
C               F     - FUNCTION F(X) TO BE INTEGRATED. MUST BE
C                       SUPPLIED BY THE USER. (INPUT,REAL FUNCTION)
C               EPS   - DESIRED RELATIVE ACCURACY. IF SUM IS SMALL EPS
C                       WILL BE ABSOLUTE ACCURACY INSTEAD. (INPUT,REAL)
C               SUM   - CALCULATED VALUE FOR THE INTEGRAL (OUTPUT,REAL)
C   PRECISION         - DOUBLE
C   REQ'D PROG'S      - F
C   AUTHOR            - THOMAS JOHANSSON, LDC,1973
C   REFERENCE(S)      - THE AUSTRALIAN COMPUTER JOURNAL,3 P.126 AUG. -71
C
C.......................................................................
      SUBROUTINE DGADAP(A0,B0,F,EPS1,SUM)
*     **********************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON/GADAP1/ NUM,IFU
      EXTERNAL F
      DIMENSION A(300),B(300),F1(300),F2(300),F3(300),S(300),N(300)
    1 FORMAT(16H GADAP:I TOO BIG)
      DSUM(F1F,F2F,F3F,AA,BB)=5D0/18D0*(BB-AA)*(F1F+1.6D0*F2F+F3F)

      EPS=EPS1
      IF(EPS.LT.1D-15) EPS=1D-15
      RED=1.3D0
      L=1
      I=1
      SUM=0D0
      C=SQRT(15D0)/5D0
      A(1)=A0
      B(1)=B0
      F1(1)=F(0.5D0*(1D0+C)*A0+0.5D0*(1D0-C)*B0)
      F2(1)=F(0.5D0*(A0+B0))
      F3(1)=F(0.5D0*(1D0-C)*A0+0.5D0*(1D0+C)*B0)
      IFU=3
      S(1)=  DSUM(F1(1),F2(1),F3(1),A0,B0)
  100 CONTINUE
      L=L+1
      N(L)=3
      EPS=EPS*RED
      A(I+1)=A(I)+C*(B(I)-A(I))
      B(I+1)=B(I)
      A(I+2)=A(I)+B(I)-A(I+1)
      B(I+2)=A(I+1)
      A(I+3)=A(I)
      B(I+3)=A(I+2)
      W1=A(I)+(B(I)-A(I))/5D0
      U2=2D0*W1-(A(I)+A(I+2))/2D0
      F1(I+1)=F(A(I)+B(I)-W1)
      F2(I+1)=F3(I)
      F3(I+1)=F(B(I)-A(I+2)+W1)
      F1(I+2)=F(U2)
      F2(I+2)=F2(I)
      F3(I+2)=F(B(I+2)+A(I+2)-U2)
      F1(I+3)=F(A(I)+A(I+2)-W1)
      F2(I+3)=F1(I)
      F3(I+3)=F(W1)
      IFU=IFU+6
      IF(IFU.GT.5000) GOTO 130
      S(I+1)=  DSUM(F1(I+1),F2(I+1),F3(I+1),A(I+1),B(I+1))
      S(I+2)=  DSUM(F1(I+2),F2(I+2),F3(I+2),A(I+2),B(I+2))
      S(I+3)=  DSUM(F1(I+3),F2(I+3),F3(I+3),A(I+3),B(I+3))
      SS=S(I+1)+S(I+2)+S(I+3)
      I=I+3
      IF(I.GT.300)GOTO 120
      SOLD=S(I-3)
*STJ  IF(ABS(SOLD-SS).GT.EPS*(1D0+ABS(SS))/2D0) GOTO 100
      IF(ABS(SOLD-SS).GT.EPS/2D0) GOTO 100
      SUM=SUM+SS
      I=I-4
      N(L)=0
      L=L-1
  110 CONTINUE
      IF(L.EQ.1) GOTO 130
      N(L)=N(L)-1
      EPS=EPS/RED
      IF(N(L).NE.0) GOTO 100
      I=I-1
      L=L-1
      GOTO 110
  120 WRITE(6,1)
 130  CONTINUE
      END


      DOUBLE PRECISION FUNCTION DILOGX(X)
C-------------------------------------------- REMARKS ---------------
C DILOGARITHM FUNCTION: DILOG(X)=INT( -LN(1-Z)/Z ) , 0 < Z < X .
C THIS IS THE CERNLIB VERSION.
C--------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      Z=-1.644934066848226D0
      IF(X .LT.-1.D0) GO TO 1
      IF(X .LE. 0.5D0) GO TO 2
      IF(X .EQ. 1.D0) GO TO 3
      IF(X .LE. 2.D0) GO TO 4
      Z=3.289868133696453D0
    1 T=1.D0/X
      S=-0.5D0
      Z=Z-0.5D0*DLOG(DABS(X))**2
      GO TO 5
    2 T=X
      S=0.5D0
      Z=0.D0
      GO TO 5
    3 DILOGX=1.644934066848226D0
      RETURN
    4 T=1.D0-X
      S=-0.5D0
      Z=1.644934066848226D0-DLOG(X)*DLOG(DABS(T))
    5 Y=2.666666666666667D0*T+0.666666666666667D0
      B=      0.000000000000001D0
      A=Y*B  +0.000000000000004D0
      B=Y*A-B+0.000000000000011D0
      A=Y*B-A+0.000000000000037D0
      B=Y*A-B+0.000000000000121D0
      A=Y*B-A+0.000000000000398D0
      B=Y*A-B+0.000000000001312D0
      A=Y*B-A+0.000000000004342D0
      B=Y*A-B+0.000000000014437D0
      A=Y*B-A+0.000000000048274D0
      B=Y*A-B+0.000000000162421D0
      A=Y*B-A+0.000000000550291D0
      B=Y*A-B+0.000000001879117D0
      A=Y*B-A+0.000000006474338D0
      B=Y*A-B+0.000000022536705D0
      A=Y*B-A+0.000000079387055D0
      B=Y*A-B+0.000000283575385D0
      A=Y*B-A+0.000001029904264D0
      B=Y*A-B+0.000003816329463D0
      A=Y*B-A+0.000014496300557D0
      B=Y*A-B+0.000056817822718D0
      A=Y*B-A+0.000232002196094D0
      B=Y*A-B+0.001001627496164D0
      A=Y*B-A+0.004686361959447D0
      B=Y*A-B+0.024879322924228D0
      A=Y*B-A+0.166073032927855D0
      A=Y*A-B+1.935064300869969D0
      DILOGX=S*T*(A-B)+Z
      END

      DOUBLE PRECISION FUNCTION DILOGY(X)
C-------------------------------------------- REMARKS ---------------
C DILOGARITHM FUNCTION: DILOG(X)=INT( -LN(1-Z)/Z ) , 0 < Z < X .
C THIS IS THE CERNLIB VERSION.
C--------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      Z=-1.644934066848226D0
      IF(X .LT.-1.D0) GO TO 1
      IF(X .LE. 0.5D0) GO TO 2
      IF(X .EQ. 1.D0) GO TO 3
      IF(X .LE. 2.D0) GO TO 4
      Z=3.289868133696453D0
    1 T=1.D0/X
      S=-0.5D0
      Z=Z-0.5D0*DLOG(DABS(X))**2
      GO TO 5
    2 T=X
      S=0.5D0
      Z=0.D0
      GO TO 5
    3 DILOGY=1.644934066848226D0
      RETURN
    4 T=1.D0-X
      S=-0.5D0
      Z=1.644934066848226D0-DLOG(X)*DLOG(DABS(T))
    5 Y=2.666666666666667D0*T+0.666666666666667D0
      B=      0.000000000000001D0
      A=Y*B  +0.000000000000004D0
      B=Y*A-B+0.000000000000011D0
      A=Y*B-A+0.000000000000037D0
      B=Y*A-B+0.000000000000121D0
      A=Y*B-A+0.000000000000398D0
      B=Y*A-B+0.000000000001312D0
      A=Y*B-A+0.000000000004342D0
      B=Y*A-B+0.000000000014437D0
      A=Y*B-A+0.000000000048274D0
      B=Y*A-B+0.000000000162421D0
      A=Y*B-A+0.000000000550291D0
      B=Y*A-B+0.000000001879117D0
      A=Y*B-A+0.000000006474338D0
      B=Y*A-B+0.000000022536705D0
      A=Y*B-A+0.000000079387055D0
      B=Y*A-B+0.000000283575385D0
      A=Y*B-A+0.000001029904264D0
      B=Y*A-B+0.000003816329463D0
      A=Y*B-A+0.000014496300557D0
      B=Y*A-B+0.000056817822718D0
      A=Y*B-A+0.000232002196094D0
      B=Y*A-B+0.001001627496164D0
      A=Y*B-A+0.004686361959447D0
      B=Y*A-B+0.024879322924228D0
      A=Y*B-A+0.166073032927855D0
      A=Y*A-B+1.935064300869969D0
      DILOGY=S*T*(A-B)+Z
      END


      DOUBLE PRECISION FUNCTION DPGAMM(Z)
C     **********************************
C Double precision Gamma function
      DOUBLE PRECISION Z,Z1,X,X1,X2,D1,D2,S1,S2,S3,PI,C(20),CONST
      save C,PI,CONST
      DATA C( 1) / 8.3333333333333333333333333332D-02/
      DATA C( 2) /-2.7777777777777777777777777777D-03/
      DATA C( 3) / 7.9365079365079365079365079364D-04/
      DATA C( 4) /-5.9523809523809523809523809523D-04/
      DATA C( 5) / 8.4175084175084175084175084175D-04/
      DATA C( 6) /-1.9175269175269175269175269175D-03/
      DATA C( 7) / 6.4102564102564102564102564102D-03/
      DATA C( 8) /-2.9550653594771241830065359477D-02/
      DATA C( 9) / 1.7964437236883057316493849001D-01/
      DATA C(10) /-1.3924322169059011164274322169D+00/
      DATA C(11) / 1.3402864044168391994478951001D+01/
      DATA C(12) /-1.5684828462600201730636513245D+02/
      DATA C(13) / 2.1931033333333333333333333333D+03/
      DATA C(14) /-3.6108771253724989357173265219D+04/
      DATA C(15) / 6.9147226885131306710839525077D+05/
      DATA C(16) /-1.5238221539407416192283364959D+07/
      DATA C(17) / 3.8290075139141414141414141414D+08/
      DATA C(18) /-1.0882266035784391089015149165D+10/
      DATA C(19) / 3.4732028376500225225225225224D+11/
      DATA C(20) /-1.2369602142269274454251710349D+13/
      DATA PI    / 3.1415926535897932384626433832D+00/
      DATA CONST / 9.1893853320467274178032973641D-01/
      IF(Z.GT.5.75D1)                                     GOTO  6666
      NN = Z
      IF (Z  -  DBLE(FLOAT(NN)))                 3,1,3
    1 IF (Z    .LE.    0.D0)                    GOTO 6667
      DPGAMM = 1.D 0
      IF (Z    .LE.    2.D0)                    RETURN
      Z1 = Z
    2 Z1 = Z1  -  1.D 0
      DPGAMM = DPGAMM * Z1
      IF (Z1  -  2.D 0)                          61,61,2
    3 IF (DABS(Z)    .LT.    1.D-29)             GOTO 60
      IF (Z    .LT.    0.D0)                    GOTO 4
      X  = Z
      KK = 1
      GOTO 10
    4 X  = 1.D 0  -  Z
      KK = 2
   10 X1 = X
      IF (X    .GT.    19.D 0)                   GOTO 13
      D1 = X
   11 X1 = X1  +  1.D 0
      IF (X1    .GE.    19.D 0)                  GOTO 12
      D1 = D1 * X1
      GOTO 11
   12 S3 = -DLOG(D1)
      GOTO 14
   13 S3 = 0.D 0
   14 D1 = X1 * X1
      S1 = (X1  -  5.D-1) * DLOG(X1)  -  X1  +  CONST
      DO 20                  K=1,20
      S2 = S1  +  C(K)/X1
      IF (DABS(S2  -  S1)    .LT.    1.D-28)     GOTO 21
      X1 = X1 * D1
   20 S1 = S2
   21 S3 = S3  +  S2
      GOTO (50,22),    KK
   22 D2 = DABS(Z  -  NN)
      D1 = D2 * PI
      IF (D1    .LT.    1.D-15)                  GOTO 31
   30 X2 =  DLOG(PI/DSIN(D1))  -  S3
      GOTO 40
   31 X2 = -DLOG(D2)
   40 MM = DABS(Z)
      IF(X2      .GT.      1.74D2)                         GO TO 6666
      DPGAMM = DEXP(X2)
      IF (MM    .NE.    (MM/2) * 2)              RETURN
      DPGAMM = -DPGAMM
      RETURN
   50 IF(S3      .GT.      1.74D2)                         GO TO 6666
      DPGAMM = DEXP(S3)
      RETURN
 6666 PRINT *, 2000
      DPGAMM = 0.D0
      RETURN
 6667 PRINT *, 2001
      DPGAMM = 0.D0
      RETURN
   60 DPGAMM = 0.D 0
      IF(DABS(Z)   .LT.   1.D-77)   RETURN
      DPGAMM = 1.D 0/Z
   61 RETURN
 2000 FORMAT (/////, 2X, 32HDPGAMM ..... ARGUMENT TOO LARGE., /////)
 2001 FORMAT (/////, 2X, 32HDPGAMM ..... ARGUMENT IS A POLE., /////)
      END
C=======================================================================
C=======================================================================
C=======================================================================
C==Received: by dxmint.cern.ch (cernvax) (5.57/3.14)
C== id AA13405; Wed, 23 Jan 91 17:19:06 +0100
C==Message-Id: <9101231619.AA13405@dxmint.cern.ch>
C==Received: by cernapo; Wed, 23 Jan 91 17:23:40 +0100
C==Received: by apojames.cern.ch; Wed, 23 Jan 91 17:05:23 CET
C==Date: Wed, 23 Jan 91 17:05:23 CET
C==From: james@cernapo.cern.ch (Frederick James)
C==To: jadach@cernvm
C==Subject: Random generators
C==
C==      PROGRAM PSEUDORAN
C==C  CPC # ABTK                                           CPC # ABTK
C==C         Pseudorandom generator demonstration (test case)
C==      DIMENSION RVEC(1000)
C==      DIMENSION VERI(5), ISD25(25)
C==C
C==C
C==C   ................................................
C==      WRITE(6,'(20X,A)') 'DEMONSTRATION OF PSEUDORANDOM GENERATORS'
C==      WRITE(6,'(20X,A)') 'MACHINE/SYSTEM: date:'
C==      WRITE(6,'(/20X,A/)') 'INITIALIZATION AND TEST OF PORTABILITY'
C==C   ................................................
C==C
C==C                   initialization and verification  RANMAR
C==        DO 40 I9= 1, 20
C==   40   CALL RANMAR(RVEC,1000)
C==      CALL RANMAR(RVEC,5)
C==      DO 41 I= 1 ,5
C==   41 VERI(I) = (4096.*RVEC(I))*(4096.)
C==      WRITE(6,'(A,5F12.1/)') '  RANMAR 20001  ',VERI
C==C
C==C                   initialization and verification  RANECU
C==      CALL RANECU(RVEC,1000)
C==      CALL RANECU(VERI,5)
C==      DO 52 I= 1 ,5
C==   52 VERI(I) = 4096.*(4096.*VERI(I))
C==      WRITE(6,'(A,5F12.1/)') '  RANECU 1001   ',VERI
C==C
C==C                   initialization and verification  RCARRY
C==      CALL RCARRY(RVEC,1000)
C==      CALL RCARRY(VERI,5)
C==      DO 62 I= 1 ,5
C==   62 VERI(I) = 4096.*(4096.*VERI(I))
C==      WRITE(6,'(A,5F12.1/)') '  RCARRY 1001   ',VERI
C==C
C==      WRITE(6,'(//20X,A/)') 'TEST OF REPEATABILITY'
C==C  .................................................
C==C                  verify restarting      RANMAR
C==      WRITE(6,'(/A)') '   THE NEXT LINE SHOULD BE REPEATED:'
C==      CALL RMARUT(IMAR1,IMAR2,IMAR3)
C==      CALL RANMAR(RVEC,777)
C==      CALL RANMAR(VERI,5)
C==      WRITE(6,'(A,5F12.9)') '       RANMAR 1 ',VERI
C==      CALL RMARIN(IMAR1,IMAR2,IMAR3)
C==      CALL RANMAR(RVEC,777)
C==      CALL RANMAR(VERI,5)
C==      WRITE(6,'(A,5F12.9)') '       RANMAR 2 ',VERI
C==C
C==C                  verify restarting      RANECU
C==      WRITE(6,'(/A)') '   THE NEXT LINE SHOULD BE REPEATED:'
C==      CALL RECUUT(IS1,IS2)
C==      CALL RANECU(RVEC,777)
C==      CALL RANECU(VERI,5)
C==      WRITE(6,'(A,5F12.9)') '       RANECU 1 ',VERI
C==      CALL RECUIN(IS1,IS2)
C==      CALL RANECU(RVEC,777)
C==      CALL RANECU(VERI,5)
C==      WRITE(6,'(A,5F12.9)') '       RANECU 2 ',VERI
C==C
C==C                  verify restarting      RCARRY
C==      WRITE(6,'(/A)') '   THE NEXT LINE SHOULD BE REPEATED:'
C==      CALL RCARUT(ISD25)
C==      CALL RCARRY(RVEC,777)
C==      CALL RCARRY(VERI,5)
C==      WRITE(6,'(A,5F12.9)') '       RCARRY 1 ',VERI
C==      CALL RCARIN(ISD25)
C==      CALL RCARRY(RVEC,777)
C==      CALL RCARRY(VERI,5)
C==      WRITE(6,'(A,5F12.9)') '       RCARRY 2 ',VERI
C==C
C==      STOP
C==      END
C=======================================================================
C=======================================================================
C=======================================================================
      SUBROUTINE MARRAN(RVEC,LENV)
C =======================S. JADACH===================================
C == This commes from F. James, The name of RANMAR is changed to   ==
C == MARRAN in order to avoid interference with the version        ==
C == already in use and the public library version (if present).   ==
C ==      THIS IS THE ONLY MODIFICATION !!!!                       ==
C ========================S. JADACH==================================
C Universal random number generator proposed by Marsaglia and Zaman
C in report FSU-SCRI-87-50
C        modified by F. James, 1988 and 1989, to generate a vector
C        of pseudorandom numbers RVEC of length LENV, and to put in
C        the COMMON block everything needed to specify currrent state,
C        and to add input and output entry points MARINI, MAROUT.
C!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C!!!  Calling sequences for RANMAR:                                  ++
C!!!      CALL RANMAR (RVEC, LEN)   returns a vector RVEC of LEN     ++
C!!!                   32-bit random floating point numbers between  ++
C!!!                   zero and one.                                 ++
C!!!      CALL MARINI(I1,N1,N2)   initializes the generator from one ++
C!!!                   32-bit integer I1, and number counts N1,N2    ++
C!!!                  (for initializing, set N1=N2=0, but to restart ++
C!!!                    a previously generated sequence, use values  ++
C!!!                    output by MAROUT)                            ++
C!!!      CALL MAROUT(I1,N1,N2)   outputs the value of the original  ++
C!!!                  seed and the two number counts, to be used     ++
C!!!                  for restarting by initializing to I1 and       ++
C!!!                  skipping N2*100000000+N1 numbers.              ++
C!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      DIMENSION RVEC(*)
      PARAMETER (MODCNS=1000000000)
C!!!  COMMON/RASET1/U(97),C,I97,J97
      DIMENSION     U(97)
      SAVE          U    ,C,I97,J97
      SAVE CD, CM, TWOM24, NTOT, NTOT2, IJKL
      DATA NTOT,NTOT2,IJKL/-1,0,0/
C
      IF (NTOT .GE. 0)  GO TO 50
C
C        Default initialization. User has called RANMAR without MARINI.
      IJKL = 54217137
      NTOT = 0
      NTOT2 = 0
      KALLED = 0
      GO TO 1
C
      ENTRY      MARINI(IJKLIN, NTOTIN,NTOT2N)
C         Initializing routine for RANMAR, may be called before
C         generating pseudorandom numbers with RANMAR. The input
C         values should be in the ranges:  0<=IJKLIN<=900 OOO OOO
C                                          0<=NTOTIN<=999 999 999
C                                          0<=NTOT2N<<999 999 999!
C To get the standard values in Marsaglia's paper, IJKLIN=54217137
C                                            NTOTIN,NTOT2N=0
      IJKL = IJKLIN
      NTOT = MAX(NTOTIN,0)
      NTOT2= MAX(NTOT2N,0)
      KALLED = 1
C          always come here to initialize
    1 CONTINUE
      IJ = IJKL/30082
      KL = IJKL - 30082*IJ
      I = MOD(IJ/177, 177) + 2
      J = MOD(IJ, 177)     + 2
      K = MOD(KL/169, 178) + 1
      L = MOD(KL, 169)
      WRITE(6,'(A,5I10)')
     $' MARran INITIALIZED: IJ,KL,IJKL,NTOT,NTOT2=',IJ,KL,IJKL,NTOT,NTOT2
      DO 2 II= 1, 97
      S = 0.
      T = .5
      DO 3 JJ= 1, 24
         M = MOD(MOD(I*J,179)*K, 179)
         I = J
         J = K
         K = M
         L = MOD(53*L+1, 169)
         IF (MOD(L*M,64) .GE. 32)  S = S+T
    3    T = 0.5*T
    2 U(II) = S
      TWOM24 = 1.0
      DO 4 I24= 1, 24
    4 TWOM24 = 0.5*TWOM24
      C  =   362436.*TWOM24
      CD =  7654321.*TWOM24
      CM = 16777213.*TWOM24
      I97 = 97
      J97 = 33
C       Complete initialization by skipping
C            (NTOT2*MODCNS + NTOT) random numbers
      DO 45 LOOP2= 1, NTOT2+1
      NOW = MODCNS
      IF (LOOP2 .EQ. NTOT2+1)  NOW=NTOT
      IF (NOW .GT. 0)  THEN
        WRITE(6,'(A,I15)') ' MARINI SKIPPING OVER ',NOW
       DO 40 IDUM = 1, NTOT
       UNI = U(I97)-U(J97)
       IF (UNI .LT. 0.)  UNI=UNI+1.
       U(I97) = UNI
       I97 = I97-1
       IF (I97 .EQ. 0)  I97=97
       J97 = J97-1
       IF (J97 .EQ. 0)  J97=97
       C = C - CD
       IF (C .LT. 0.)  C=C+CM
   40  CONTINUE
      ENDIF
   45 CONTINUE
      IF (KALLED .EQ. 1)  RETURN
C
C          Normal entry to generate LENV random numbers
   50 CONTINUE
      DO 100 IVEC= 1, LENV
      UNI = U(I97)-U(J97)
      IF (UNI .LT. 0.)  UNI=UNI+1.
      U(I97) = UNI
      I97 = I97-1
      IF (I97 .EQ. 0)  I97=97
      J97 = J97-1
      IF (J97 .EQ. 0)  J97=97
      C = C - CD
      IF (C .LT. 0.)  C=C+CM
      UNI = UNI-C
      IF (UNI .LT. 0.) UNI=UNI+1.
      RVEC(IVEC) = UNI
C             Replace exact zeros by uniform distr. *2**-24
         IF (UNI .EQ. 0.)  THEN
         ZUNI = TWOM24*U(2)
C             An exact zero here is very unlikely, but let's be safe.
         IF (ZUNI .EQ. 0.) ZUNI= TWOM24*TWOM24
         RVEC(IVEC) = ZUNI
         ENDIF
  100 CONTINUE
      NTOT = NTOT + LENV
         IF (NTOT .GE. MODCNS)  THEN
         NTOT2 = NTOT2 + 1
         NTOT = NTOT - MODCNS
         ENDIF
      RETURN
C           Entry to output current status
      ENTRY MAROUT(IJKLUT,NTOTUT,NTOT2T)
      IJKLUT = IJKL
      NTOTUT = NTOT
      NTOT2T = NTOT2
      RETURN
      END
      SUBROUTINE CARRAN(RVEC,LENV)
C         Add-and-carry random number generator proposed by
C         Marsaglia and Zaman in SIAM J. Scientific and Statistical
C             Computing, to appear probably 1990.
C         modified with enhanced initialization by F. James, 1990
C!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C!!!  Calling sequences for CARRAN:                                  ++
C!!!      CALL CARRAN (RVEC, LEN)   returns a vector RVEC of LEN     ++
C!!!                   32-bit random floating point numbers between  ++
C!!!                   zero and one.                                 ++
C!!!      CALL CARINI(INT)     initializes the generator from one    ++
C!!!                   32-bit integer INT                            ++
C!!!      CALL CARRES(IVEC)    restarts the generator from vector    ++
C!!!                   IVEC of 25 32-bit integers (see CAROUT)       ++
C!!!      CALL CAROUT(IVEC)    outputs the current values of the 25  ++
C!!!                 32-bit integer seeds, to be used for restarting ++
C!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      DIMENSION RVEC(LENV)
      DIMENSION SEEDS(24), ISEEDS(24), ISDEXT(25)
      PARAMETER (TWOP12=4096.)
      PARAMETER (ITWO24=2**24, ICONS=2147483563)
      SAVE NOTYET, I24, J24, CARRY, SEEDS, TWOM24
      LOGICAL NOTYET
      DATA NOTYET/.TRUE./
      DATA I24,J24,CARRY/24,10,0./
C
C              Default Initialization by Multiplicative Congruential
      IF (NOTYET) THEN
         NOTYET = .FALSE.
         JSEED = 314159265
         WRITE(6,'(A,I12)') ' CARRAN DEFAULT INITIALIZATION: ',JSEED
            TWOM24 = 1.
         DO 25 I= 1, 24
            TWOM24 = TWOM24 * 0.5
         K = JSEED/53668
         JSEED = 40014*(JSEED-K*53668) -K*12211
         IF (JSEED .LT. 0)  JSEED = JSEED+ICONS
         ISEEDS(I) = MOD(JSEED,ITWO24)
   25    CONTINUE
         DO 50 I= 1,24
         SEEDS(I) = REAL(ISEEDS(I))*TWOM24
   50    CONTINUE
         I24 = 24
         J24 = 10
         CARRY = 0.
         IF (SEEDS(24) .LT. SEEDS(14)) CARRY = TWOM24
      ENDIF
C
C          The Generator proper: "Subtract-with-borrow",
C          as proposed by Marsaglia and Zaman,
C          Florida State University, March, 1989
C
      DO 100 IVEC= 1, LENV
      UNI = SEEDS(I24) - SEEDS(J24) - CARRY
      IF (UNI .LT. 0.)  THEN
         UNI = UNI + 1.0
         CARRY = TWOM24
      ELSE
         CARRY = 0.
      ENDIF
      SEEDS(I24) = UNI
      I24 = I24 - 1
      IF (I24 .EQ. 0)  I24 = 24
      J24 = J24 - 1
      IF (J24 .EQ. 0)  J24 = 24
      RVEC(IVEC) = UNI
  100 CONTINUE
      RETURN
C           Entry to input and float integer seeds from previous run
      ENTRY CARRES(ISDEXT)
         TWOM24 = 1.
         DO 195 I= 1, 24
  195    TWOM24 = TWOM24 * 0.5
      WRITE(6,'(A)') ' FULL INITIALIZATION OF CARRAN WITH 25 INTEGERS:'
      WRITE(6,'(5X,5I12)') ISDEXT
      DO 200 I= 1, 24
      SEEDS(I) = REAL(ISDEXT(I))*TWOM24
  200 CONTINUE
      CARRY = REAL(MOD(ISDEXT(25),10))*TWOM24
      ISD = ISDEXT(25)/10
      I24 = MOD(ISD,100)
      ISD = ISD/100
      J24 = ISD
      RETURN
C                    Entry to ouput seeds as integers
      ENTRY CAROUT(ISDEXT)
      DO 300 I= 1, 24
         ISDEXT(I) = INT(SEEDS(I)*TWOP12*TWOP12)
  300 CONTINUE
      ICARRY = 0
      IF (CARRY .GT. 0.)  ICARRY = 1
      ISDEXT(25) = 1000*J24 + 10*I24 + ICARRY
      RETURN
C                    Entry to initialize from one integer
      ENTRY CARINI(INSEED)
      JSEED = INSEED
      WRITE(6,'(A,I12)') ' CARRAN INITIALIZED FROM SEED ',INSEED
C      TWOM24 = 1.
         DO 325 I= 1, 24
           TWOM24 = TWOM24 * 0.5
         K = JSEED/53668
         JSEED = 40014*(JSEED-K*53668) -K*12211
         IF (JSEED .LT. 0)  JSEED = JSEED+ICONS
         ISEEDS(I) = MOD(JSEED,ITWO24)
  325    CONTINUE
         DO 350 I= 1,24
         SEEDS(I) = REAL(ISEEDS(I))*TWOM24
  350    CONTINUE
         I24 = 24
         J24 = 10
         CARRY = 0.
         IF (SEEDS(24) .LT. SEEDS(14)) CARRY = TWOM24
      RETURN
      END

      SUBROUTINE ECURAN(RVEC,LEN)
C         Random number generator given by L'Ecuyer in
C            Comm. ACM Vol 31, p.742, 1988
C            modified by F. James to return a vector of numbers
C!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C!!!  Calling sequences for ECURAN:                                  ++
C!!!      CALL ECURAN (RVEC, LEN)   returns a vector RVEC of LEN     ++
C!!!                   32-bit random floating point numbers between  ++
C!!!                   zero and one.                                 ++
C!!!      CALL ECUINI(I1,I2)    initializes the generator from two   ++
C!!!                   32-bit integers I1 and I2                     ++
C!!!      CALL ECUOUT(I1,I2)    outputs the current values of the    ++
C!!!                   two integer seeds, to be used for restarting  ++
C!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      DIMENSION RVEC(*)
      SAVE ISEED1,ISEED2
      DATA ISEED1,ISEED2 /12345,67890/
C
      DO 100 I= 1, LEN
      K = ISEED1/53668
      ISEED1 = 40014*(ISEED1 - K*53668) - K*12211
      IF (ISEED1 .LT. 0) ISEED1=ISEED1+2147483563
C
      K = ISEED2/52774
      ISEED2 = 40692*(ISEED2 - K*52774) - K* 3791
      IF (ISEED2 .LT. 0) ISEED2=ISEED2+2147483399
C
      IZ = ISEED1 - ISEED2
      IF (IZ .LT. 1)  IZ = IZ + 2147483562
C
      RVEC(I) = REAL(IZ) * 4.656613E-10
  100 CONTINUE
      RETURN
C
      ENTRY ECUINI(IS1,IS2)
      ISEED1 = IS1
      ISEED2 = IS2
      RETURN
C
      ENTRY ECUOUT(IS1,IS2)
      IS1 = ISEED1
      IS2 = ISEED2
      RETURN
      END

      SUBROUTINE VARRAN(DRVEC,LENGT)
C     ***************************
C Switchable random number generator
C Translation to double precision
C     ***************************
      COMMON / RANPAR / KEYRND
      save   / RANPAR /
      DOUBLE PRECISION DRVEC(*)
      DIMENSION RVEC(1000)
      IF(LENGT.LT.1.OR.LENGT.GT.1000) GOTO 901
   10 CONTINUE
      IF(KEYRND.EQ.1) THEN
CBB         CALL MARRAN(RVEC,LENGT)
         CALL RANMAR(RVEC,LENGT)
      ELSEIF(KEYRND.EQ.2) THEN
         CALL ECURAN(RVEC,LENGT)
      ELSEIF(KEYRND.EQ.3) THEN
         CALL CARRAN(RVEC,LENGT)
      ELSE
         GOTO 902
      ENDIF
C random numbers 0 and 1 not accepted
      DO 30 I=1,LENGT
      IF(RVEC(I).LE.0E0.OR.RVEC(I).GE.1E0) THEN
        WRITE(6,*) ' +++++ VARRAN: RVEC=',RVEC(I)
        GOTO 10
      ENDIF
      DRVEC(I)=RVEC(I)
   30 CONTINUE
      RETURN
  901 WRITE(6,*) ' +++++ STOP IN VARRAN: LENGT=',LENGT
      STOP
  902 WRITE(6,*) ' +++++ STOP IN VARRAN: WRONG KEYRND',KEYRND
      STOP
      END
C BOOST ALONG X AXIS, EXE=EXP(ETA), ETA= HIPERBOLIC VELOCITY.
      SUBROUTINE BOSTD1(EXE,PVEC,QVEC)
C     ********************************
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 PVEC(4),QVEC(4),RVEC(4)
      DO 10 I=1,4
  10  RVEC(I)=PVEC(I)
      RPL=RVEC(4)+RVEC(1)
      RMI=RVEC(4)-RVEC(1)
      QPL=RPL*EXE
      QMI=RMI/EXE
      QVEC(2)=RVEC(2)
      QVEC(3)=RVEC(3)
      QVEC(1)=(QPL-QMI)/2
      QVEC(4)=(QPL+QMI)/2
      END

C BOOST ALONG Z AXIS, EXE=EXP(ETA), ETA= HIPERBOLIC VELOCITY.
      SUBROUTINE BXSTD3(EXE,PVEC,QVEC)
C     ********************************
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 PVEC(4),QVEC(4),RVEC(4)
      DO 10 I=1,4
  10  RVEC(I)=PVEC(I)
      RPL=RVEC(4)+RVEC(3)
      RMI=RVEC(4)-RVEC(3)
      QPL=RPL*EXE
      QMI=RMI/EXE
      QVEC(1)=RVEC(1)
      QVEC(2)=RVEC(2)
      QVEC(3)=(QPL-QMI)/2
      QVEC(4)=(QPL+QMI)/2
      END

      SUBROUTINE RXTOD1(PH1,PVEC,QVEC)
C     ********************************
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 PVEC(4),QVEC(4),RVEC(4)
      PHI=PH1
      CS=COS(PHI)
      SN=SIN(PHI)
      DO 10 I=1,4
  10  RVEC(I)=PVEC(I)
      QVEC(1)=RVEC(1)
      QVEC(2)= CS*RVEC(2)-SN*RVEC(3)
      QVEC(3)= SN*RVEC(2)+CS*RVEC(3)
      QVEC(4)=RVEC(4)
      END

      SUBROUTINE RXTOD2(PH1,PVEC,QVEC)
C     ********************************
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 PVEC(4),QVEC(4),RVEC(4)
      PHI=PH1
      CS=COS(PHI)
      SN=SIN(PHI)
      DO 10 I=1,4
  10  RVEC(I)=PVEC(I)
      QVEC(1)= CS*RVEC(1)+SN*RVEC(3)
      QVEC(2)=RVEC(2)
      QVEC(3)=-SN*RVEC(1)+CS*RVEC(3)
      QVEC(4)=RVEC(4)
      END

      SUBROUTINE RXTOD3(PH1,PVEC,QVEC)
C     ********************************
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 PVEC(4),QVEC(4),RVEC(4)
      PHI=PH1
      CS=COS(PHI)
      SN=SIN(PHI)
      DO 10 I=1,4
  10  RVEC(I)=PVEC(I)
      QVEC(1)= CS*RVEC(1)-SN*RVEC(2)
      QVEC(2)= SN*RVEC(1)+CS*RVEC(2)
      QVEC(3)=RVEC(3)
      QVEC(4)=RVEC(4)
      END

      FUNCTION ANGFIX(X,Y)
C     *******************
* CALCULATES ANGLE IN (0,2*PI) RANGE OUT OF X-Y
*     ***********************
      IMPLICIT REAL*8(A-H,O-Z)
      DATA PI /3.1415926535897932D0/

      IF(ABS(Y).LT.ABS(X)) THEN
        THE=ATAN(ABS(Y/X))
        IF(X.LE.0D0) THE=PI-THE
      ELSE
        THE=ACOS(X/SQRT(X**2+Y**2))
      ENDIF
      IF(Y.LT.0D0) THE=2D0*PI-THE
      ANGFIX=THE
      END

      SUBROUTINE DUMPT(NUNIT,WORD,PP)        
C     *******************************        
      IMPLICIT REAL*8(A-H,O-Z)               
      CHARACTER*8 WORD                       
      REAL*8 PP(4)                           
      AMS=PP(4)**2-PP(3)**2-PP(2)**2-PP(1)**2  
      IF(AMS.GT.0.0) AMS=SQRT(AMS)           
      WRITE(NUNIT,'(1X,A8,5(1X,F13.8))') WORD,(PP(I),I=1,4),AMS 
C====================================================================== 
C================END OF YFSLIB========================================= 
C====================================================================== 
      END 
      SUBROUTINE TOHEP
      DOUBLE PRECISION  QEFF1(4),QEFF2(4),SPHUM(4),SPHOT(100,4)
      DOUBLE PRECISION Q1(4),Q2(4),P1(4),P2(4),P3(4),P4(4)
      INTEGER NPHOT
      COMMON / MOMSET / QEFF1,QEFF2,SPHUM,SPHOT,NPHOT
      COMMON / MOMDEC / Q1,Q2,P1,P2,P3,P4
      DOUBLE PRECISION ENE,AMAZ,GAMMZ,AMEL,AMFIN,XK0,SINW2
      INTEGER IDE,IDF
      COMMON / WEKING / ENE,AMAZ,GAMMZ,AMEL,AMFIN,XK0,SINW2,IDE,IDF
      COMMON /TAUPOS/ NP1,NP2                
      COMMON /PHOACT/ IFPHOT
      INTEGER IFLAV(4)
      REAL*4    POL1(4),POL2(4)
      DOUBLE PRECISION  AMDEC(4), BRRAT(2), BREL
      COMMON / DECAYS / IFLAV, AMDEC, BRRAT, BREL

      DATA  POL1 /0.0,0.0,-1.0,0.0/
      DATA  POL2 /0.0,0.0,-1.0,0.0/
C
      REAL*4 APH(4),XPB1(4),XPB2(4),AQF1(4),AQF2(4),XAMD(4),AM
      REAL*4 XP1(4),XP2(4),XP3(4),XP4(4)

      SAVE
C
      DO K=1,4
       AQF1(K)=Q1(K)
       AQF2(K)=Q2(K)
       XP1(K) =P1(K) 
       XP2(K) =P2(K) 
       XP3(K) =P3(K) 
       XP4(K) =P4(K) 
C
       XAMD(K)=AMDEC(K)
      ENDDO

C initial state (1,2)
      AM=AMEL
! e- (0 0 +1 1)
      XPB1(1) = 0d0
      XPB1(2) = 0d0
      XPB1(3) = dsqrt(ene**2 -am**2) !e- (00+11)
      XPB1(4) = ene
! e+ (0 0 -1 1)
      XPB2(1) = 0d0
      XPB2(2) = 0d0
      XPB2(3) =-xpb1(3) !e+ (00-11)
      XPB2(4) = ene

      CALL FILHEP(1,3, 11,0,0,0,0,XPB1,AM,.TRUE.)
      CALL FILHEP(2,3,-11,0,0,0,0,XPB2,AM,.TRUE.)
C primary final state W-W+ (3,4)
      AM=sqrt(Q1(4)**2-Q1(3)**2-Q1(2)**2-Q1(1)**2)
      CALL FILHEP(3,2,-24,1,2,0,0,AQF1,AM,.TRUE.)
      AM=sqrt(Q2(4)**2-Q2(3)**2-Q2(2)**2-Q2(1)**2)
      CALL FILHEP(4,2, 24,1,2,0,0,AQF2,AM,.TRUE.)
C radiative photons (5 ... 4+NPHOT) (PDG-code for gamma is 22)
      IF(NPHOT.NE.0) THEN
        IP=0
        DO I=1,NPHOT
          DO J=1,4
            APH(J)=SPHOT(I,J)
          END DO
          IF (APH(4).GT.0.0) THEN
            IP=IP+1
            CALL FILHEP(4+IP,1,22,1,2,0,0,APH,0.0,.TRUE.)
          END IF
       END DO
      END IF
C decay products W- (5,6)+NPHOT
      CALL FILHEP(0,1,IFLAV(1),3,3,0,0,XP1,XAMD(1),.TRUE.)
      CALL FILHEP(0,1,IFLAV(2),3,3,0,0,XP2,XAMD(2),.TRUE.)
C decay products W+ (7,8)+NPHOT
      CALL FILHEP(0,1,IFLAV(3),4,4,0,0,XP3,XAMD(3),.TRUE.)
      CALL FILHEP(0,1,IFLAV(4),4,4,0,0,XP4,XAMD(4),.TRUE.)
C

C tau decays:
      if (abs(iflav(1)).eq.15) then
          KTO=2
          NP2=5+NPHOT
          CALL DEXAY(KTO,POL2)
      endif
      if (abs(iflav(4)).eq.15) then
          KTO=1
          NP1=8+NPHOT
          CALL DEXAY(KTO,POL1)
      endif
C radiate photons for  leptonic W decays.
        IF (IFPHOT.EQ.1) THEN
          if (abs(iflav(1)).gt.10) CALL PHOTOS(3)
          if (abs(iflav(3)).gt.10) CALL PHOTOS(4)
        ENDIF
      END 




      SUBROUTINE TOHAD(IFHADM,IFHADP)
      integer ijoin(2)
      integer iflav(4)
      double precision  amdec(4), brrat(2), brel
      common / decays / iflav, amdec, brrat, brel

      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      real*4 phep,vhep



C
C lets first go to LUND and later hadronize
C
C
      call luhepc(2)
 
        DO 7 ID1=5,NHEP
          IF (IDHEP(ID1).NE.22) THEN
            goto 8
          ENDIF
  7     CONTINUE
  8     CONTINUE

        DO 17 ID2=ID1+2,NHEP
          IF (IDHEP(ID2).NE.22) THEN
            goto 18
          ENDIF
  17    CONTINUE
  18    CONTINUE

      if (ifhadm.ne.0 .or. ifhadp.ne.0) then

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
        SQRS1=PHEP(5,JMOHEP(1,ID1))
        IF (ABS(IFLAV(1)).LT.10) CALL LUSHOW(ID1,ID1+1,SQRS1)
        SQRS2=PHEP(5,JMOHEP(1,ID2))
        IF (ABS(IFLAV(3)).LT.10) CALL LUSHOW(ID2,ID2+1,SQRS2)
        call luexec

      endif

      end

      SUBROUTINE INIETC(jakk1,jakk2,itd,ifpho)
      COMMON / IDFC  / IDFF
      COMMON / TAURAD / XK0DEC,ITDKRC
      REAL*8            XK0DEC
      COMMON / JAKI   /  JAK1,JAK2,JAKP,JAKM,KTOM
      COMMON /PHOACT/ IFPHOT
      SAVE
C KTO=1 will denote tau+, thus :: IDFF=-15
          IDFF=-15
C XK0 for tau decays.
          XK0DEC=0.01
C radiative correction switch in tau --> e (mu) decays !
          ITDKRC=itd
C switches of tau+ tau- decay modes !!
          JAK1=jakk1
          JAK2=jakk2
C photos activation switch
          IFPHOT=IFPHO
      end


      SUBROUTINE koralw(mode,xpar,npar)     
!     *********************************    
! =======================================================================
! =======================================================================
! ============================KORALW=====================================
! =======================WW PAIR PRODUCTION==============================
! ==================INITIAL STATE EXPONENTIATION=========================
! =======================================================================
! =======================================================================
! =========================VERSION 1.02==================================
! =======================================================================
! ========================== July  1995==================================
! =======================================================================
!     This program is written by: 
!          M. Skrzypek    (skrzypek@hpjmiady.ifj.edu)
!     in collaboration with  
!          S. Jadach      (jadach@cernvm.cern.ch),
!          W. Placzek     (placzek@hephp02.phys.utk.edu),
!          Z. Was         (wasm@cernvm.cern.ch)
!     We acknowledge warmly very usefull help of M. Martinez in testing
!     the program.
! =======================================================================
! =======================================================================
!  
!          
! !!! SOME CONTROLL HISTOGRAMING STILL IN PROGRAM   !!!   
!          
! Generator of Yennie-Frautschi-Suura type         
! with Exponentiated Initial State Bremsstrahlung.
! ********* input    
! mode =-1/0/1/2 defines       
!       initialization/generation/give-xsection/final-report  
! cmsene = centre of mass energy (gev)  
! npar(1)= KeyRad =  1000*KeyCul+100*KeyNLL+10*KeyFSR+KeyISR
!     KeyCul Coulomb correction, INACTIVE !!!
!     KeyNLL=0 sets Next-to Leading alpha/pi terms to zero
!     KeyNLL=1 alpha/pi in YFS formfactor is kept
!     KeyFSR   Fnitial State Radiation switch, INACTIVE
!     KeyISR=0   Initial State Radiation OFF,
!     KeyISR=1  Initial State Radiation ON.
! npar(2)= KeyPhy
!     KeyPhy = 10000*KeyRed+1000*KeySpn+100*KeyZet+10*KeyMas+KeyBra
!     KeyBra= 0 Born branching ratios, no mixing
!     KeyBra= 1 Branching ratios with mixing and QCD
!     KeyMas= 0 Masless kinematics for W decay products 
!     KeyMas= 1 Massive kinematics for W decay products 
!     KeyZet= 0 Z width in Z propagator: s/M_Z *GAMM_Z
!     KeyZet= 1 Z width in Z propagator:   M_Z *GAMM_Z 
!     KeyZet= 2 Z zero width in Z propagator.
!     KeySpn= 0-off, 1-on for  spin effects in W decays 
!     KeyRed= Reduction of massive FS to massles Matr.El.
!     KeyRed= 0 fine
!     KeyRed= 1 crude, 4-mom. non conserving
! npar(3)= KeyTek= 10*KeyRnd +KeyWgt
!     KeyWgt =0, WTMOD=1 useful for apparatus Monte Carlo.
!     KeyWgt =1, WTMOD varying, option faster and safer
!     KeyRnd = ?????
! npar(4)= KeyMis= KeyMix
!     miscelaneus auxiliarym key for tests
!     actualy KeyMix=0 causes  sinw2 to be according LEP200 presription
!     while   KeyMix=1 prowides sinw2 as in 1.00 version.
! npar(5)=KeyDwm  decay channel of W- 
! npar(6)=KeyDwp  decay channel of W+ 
!                 0-all chann. according to br. ratios
!                 1-ud, 2-cd, 3-us, 4-cs, 5-ub, 6-cb, 7-e, 8-mu, 9-tau
! npar(7)=NOUT  output unit number,  
!               if npar(7) .le. 0 then NOUT=16. 
! npar(21)=jak1      TAUOLA, tau of W+ decay channel 
! npar(22)=jak2      TAUOLA, tau of W- decay channel 
! npar(23)=ITDKRC    TAUOLA, radiative corr. in leponic tau decays switch
! npar(24)=IFPHOT    PHOTOS, activation switch
! npar(25)=IFHADM    JETSET, W- hadronisation activation switch
! npar(26)=IFHADP    JETSET, W+ hadronisation activation switch
!               !!!! IFHADM and IFHADP are glued together (for now) !!!!
! xpar(1)=cmsene  =  cms energy  
! xpar(2)=gmu     =  G_Fermi
! xpar(3)=alfwin  =  1/alpha_w  
! xpar(4)=amaz    =  mass  of Z0     
! xpar(5)=gammz   =  width of Z0       
! xpar(6)=amaw    =  mass  of W    
! xpar(7)=gammw   =  width of W       
! xpar(8)=vvmin   =  minimum v-variable (dimesionless) =epsilon 
! xpar(9)=vvmax   =  maximum v-variable   
! xpar(10)=wtmax  =  maximum weight for rejection  
! 
! ********* output   
!
! fourmomenta and photon multiplicity in standard common HEPEVT:  
!
!      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
!     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
! also in KORALW internal commons MOMSET and MOMDEC:
!   COMMON / MOMSET / QEFF1(4),QEFF2(4),SPHUM(4),SPHOT(100,4),NPHOT  
!      effective parameters for Matr.El. only (e-,e+); .. ;photons ;phot. multipl.
!   COMMON / MOMDEC / Q1(4),Q2(4),P1(4),P2(4),P3(4),P4(4)
!      W- ;W+ ;f (W-);\bar f (W-);f (W+);\bar f (W+);
! npar(10)=nevtru = number of accepted UNWEIGHTED events or
!                   number of generated WEIGHTED events     
! npar(11)=nevtot = number of generated events (before rej.)   
!                   (for weighted evts nevtru=nevtot)
! xpar(20)=xsecpb = BEST cross section in picobarns 
!                   / Born for NO bremss. option, 2 ord exp for bremss. 
! xpar(21)=errpb  = error in picobarns 
! xpar(30)=xcrude = crude Born cross section in picobarns  
!       (xcrude* NEVTRU/NEVTOT*WTMAX for UNWEIGHTED evts.)       
! xpar(31)=wtmax  = max wt used for rejection
! **************************    
! For advanced users only:     
! For KEYWGT=1 weighted events are generated and the user should   
! use the weight WTMOD from the common block /WGTALL/.      
! WTMOD is the actual model weight depending on other input params.    
! The other interesting possibility is to use      
!     WT=WTCRUD*WTSET(i) where   
!     WTSET( 1) =   born          
!     WTSET( 2) =   first order           
!     WTSET( 3) =   second order           
! and the corresponding components      
!     WTSET(20) =   First order, beta0 contribution alone   
!     WTSET(21) =   First order, beta1 contribution alone   
!     WTSET(30) =   Second order, beta0 contribution alone   
!     WTSET(31) =   Second order, beta1 contribution alone   
!     WTSET(32) =   Second order, beta2 contribution alone   
! N.B. WTMOD=WTCRUD*WTSET(3)    
!      ( WTMOD=WTCRUD*WTSET(1) for No Bremsstrahlung option )
!   
!     ********************************** 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)          
      DIMENSION  XPAR( *),NPAR( *)      
      COMMON / CGLIB / BLIBK(20000)
      SAVE   / CGLIB / 
      COMMON / MATPAR / PI,CEULER     
      COMMON / PHYPAR / ALFINV,GPICOB     
      COMMON / CMONIT/ AVERWT,ERRELA,NEVTOT,NEVACC,NEVNEG,NEVOVE,NEVZER
      COMMON / MOMSET / QEFF1(4),QEFF2(4),SPHUM(4),SPHOT(100,4),NPHOT
      COMMON / MOMDEC / Q1(4),Q2(4),P1(4),P2(4),P3(4),P4(4)
      COMMON / WEKING / ENE,AMAZ,GAMMZ,AMEL,AMFIN,XK0,SINW2,IDE,IDF    
      COMMON / WEKIN2 / AMAW,GAMMW,GMU,ALPHAW   
      COMMON / WGTGEN / WTMAX,WTVES,WTYFS,WTSS,WTBWIG,WTBORN
!      
! This common can be everywhere, contains various switches
      COMMON / KeyKey/  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
! TAUOLA, PHOTOS and JETSET overall switches
      COMMON / LIBRA  / JAK1,JAK2,ITDKRC,IFPHOT,IFHADM,IFHADP
!
      COMMON / WGTALL / WTCRUD,WTMOD,WTSET(100)      
      COMMON / INOUT  / NINP,NOUT   
      COMMON / VVREC  / VVMIN,VVMAX,VV,BETI                   
      COMMON / SSTHE  / S1,S2                 
      COMMON / ANGLES / COSTHE,PHI,COSDE1,PHI1,COSDE2,PHI2
      COMMON / BXFMTS / BXOPE,BXCLO,BXTXT,BXL1I,BXL1F,BXL2F,BXL1G,BXL2G 
      COMMON / DECAYS / IFLAV(4), AMDEC(4), BR(2), BREL
!      COMMON / DECANG / CT1,CT2
      CHARACTER*80      BXOPE,BXCLO,BXTXT,BXL1I,BXL1F,BXL2F,BXL1G,BXL2G 
      SAVE   / CMONIT /,/ MOMSET /,/ WEKING /,/ VVREC /,/ SSTHE /
      SAVE   / WGTGEN /,/ WGTALL /,/ INOUT  /,/ BXFMTS /
      SAVE   / WEKIN2 /,/ DECAYS /,/ MOMDEC /,/ ANGLES /
      SAVE   / KeyKey/ 
!-- Vector of random numbers
      DIMENSION drvec(100)
!-- Single precision parameter for Tauola
      REAL POL(4)
!-- Externals
CBB      EXTERNAL RHOSKO,VVDISB,VVFUN,UNIT
      EXTERNAL RHOSKO,VVDISB
!-- Test DipSwitch KARDMP:  printout on wt over wtmax: 1-on, 0-off
      DATA KARDMP /0/
      SAVE


! ==================================================================
! =====================INITIALIZATION===============================
! ==================================================================
!     *******************
      IF(MODE.EQ.-1) THEN
!     *******************
      
      write(6,*) 'koralw  <-1>'

! Initialization of common blocks
      CALL filexp(xpar,npar)
      write(6,*) 'koralw  <-2>'

!= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
! Identificator for THIS Generator 
      idgen = 7 
! Important histo which remembers total x-section 
      CALL Gmonit(  -1, idgen,1d0,1d0,1d0)          
!= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

! This is "pointer" for internal monitoring histograms/averages     
      IDYFS = 0

      SVAR=4*ENE**2
      KeyWgt = MOD(KeyTek,10)
      KeyISR = MOD(KeyRad,10)

!!!!!!!!!!! This should go out to tests !!!!!!
* ============================================================
* Let us keep for KORALW the GLIBK ID-ent range from 2 to 1000 
* ============================================================
      IF( KeyISR .NE. 0 ) THEN
!-- betas
         CALL GMONIT(-1,IDYFS+71,0D0,1D0,1D0)  
         CALL GMONIT(-1,IDYFS+72,0D0,1D0,1D0)  
         CALL GMONIT(-1,IDYFS+73,0D0,1D0,1D0)  
         CALL GMONIT(-1,IDYFS+74,0D0,1D0,1D0)  
         CALL GMONIT(-1,IDYFS+75,0D0,1D0,1D0)  
         CALL GMONIT(-1,IDYFS+76,0D0,1D0,1D0)  
         CALL GMONIT(-1,IDYFS+77,0D0,1D0,1D0)  
         CALL GMONIT(-1,IDYFS+78,0D0,1D0,1D0)  
         CALL GMONIT(-1,IDYFS+80,0D0,1D0,1D0)  ! WTMOD over WTMAX
      ENDIF
!!!!!!!!!!!
 
      write(6,*) 'koralw  <-3>'
!-- Initialization of QED part
      CALL karlud(-1,xcrude,dum2,dum3)     

      IEVACC=0
      NEVTOT=0
      NEVTRU=0
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! Initialization TAUOLA PHOTOS etc.
!!      IF( KeyWgt .EQ. 0) THEN
         write(6,*) '>>>>>>>> Initialization TAUOLA PHOTOS etc.'
         CALL  inietc(npar(21),npar(22),npar(23),npar(24))
         CALL  inimas
         CALL  iniphx(0.01)
         CALL  initdk
         CALL  phoini
!!      ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      write(6,*) 'koralw  <-4>'

! ==================================================================
! ========================GENERATION================================
! ==================================================================
!     ***********************
      ELSEIF(MODE.EQ.0) THEN    
!     ***********************

      NEVTRU=NEVTRU+1
 200  CONTINUE
      NEVTOT=NEVTOT+1
      CALL KARLUD( 0,DUM1,WTKARL,WTBORN)     
      SPRIM=SVAR*(1-VV)

      WTCRUD   =WTKARL
!---------------------------------------------------------
!--          MODEL weight
!---------------------------------------------------------
!---------------------------------------------------------
!--{{{{{{{{{    This should go to MODEL !!!!!!!
!---------------------------------------------------------
      IF( KeyISR .NE. 0 ) THEN
        wtbe00 =0d0
        wtbe01 =0d0
        wtbe02 =0d0
        wtbe10 =0d0
        wtbe11 =0d0
        wtbe20 =0d0
        IF(WTCRUD .NE. 0d0) THEN
          CALL 
     $      betax(svar,amel,wtbe00,wtbe01,wtbe02,wtbe10,wtbe11,wtbe20)
          wtx0=wtbe00
          wtx1=wtbe01 +wtbe10
          wtx2=wtbe02 +wtbe11 + wtbe20
        ENDIF
      ENDIF

      IF( KeyISR .EQ. 0 ) THEN
! **********************************
!       Principal weight
        WTMOD    =WTCRUD*WTBorn
! **********************************
        WTSET(1) =0d0
        WTSET(2) =0D0
        WTSET(20)=0D0
        WTSET(21)=0D0
      ELSE
! **********************************
!       Principal weight
        WTMOD    =WTCRUD*WTBorn*wtx2
! **********************************
        WTSET(1) =WTBorn*wtx0
        WTSET(2) =WTBorn*wtx1
        WTSET(3) =WTBorn*wtx2
        WTSET(20)=WTBorn*wtbe01
        WTSET(21)=WTBorn*wtbe10
        WTSET(30)=WTBorn*wtbe02
        WTSET(31)=WTBorn*wtbe11
        WTSET(32)=WTBorn*wtbe20
      ENDIF       
!---------------------}}}}}}}}}

!---------------------------------------------------------
!------[[[[[[[[    This should go to Tests !!!!!!!
!---------------------------------------------------------
      WTTOT=WTKARL*WTBORN 
      IF( KeyISR .NE. 0 ) THEN
!-- betas..................
        CALL GMONIT(0,IDYFS+71,wtbe01,  1D0,1D0)        
        CALL GMONIT(0,IDYFS+72,wtbe10,  1D0,1D0)        
        CALL GMONIT(0,IDYFS+73,wtx1,    1D0,1D0)        
        CALL GMONIT(0,IDYFS+74,WTTOT*wtbe01,  1D0,1D0)        
        CALL GMONIT(0,IDYFS+75,WTTOT*wtbe10,  1D0,1D0)        
        CALL GMONIT(0,IDYFS+76,WTTOT*wtx1,    1D0,1D0)        
        CALL GMONIT(0,IDYFS+77,wtx2,    1D0,1D0)        
        CALL GMONIT(0,IDYFS+78,WTTOT*wtx2,    1D0,1D0)        
        IF(wtmod.lt.wtmax) THEN 
          WTOVR=0D0
        ELSE
          WTOVR=WTMOD
        ENDIF
        CALL gmonit(0,idyfs+80,wtovr,  1d0,1d0)        
      ENDIF

!-- printout for big weights
      IF(kardmp.eq.1.and.wtborn.gt.wtmax) THEN
        CTHE1=COS(ANGLE(QEFF1,P1))
        CTHE2=COS(ANGLE(QEFF1,P2))
        CTHE3=COS(ANGLE(QEFF1,P3))
        CTHE4=COS(ANGLE(QEFF1,P4))
        WLAMBD=SPRIM**2+S1**2+S2**2-2*SPRIM*S1-2*SPRIM*S2-2*S1*S2
        T=-(SPRIM-S1-S2-DSQRT(WLAMBD)*COSTHE)/2
        write(6,*) 'wtbor=',real(wtborn),' wttot=',real(wttot)
     @            ,' log t=',real(dlog(-t))
        write(6,*) 'costhe=',real(costhe)
     @            ,' sqrt sprim=',real(dsqrt(sprim))
        write(6,*) 'sqrt s1=',real(sqrt(s1)),' sqrt s2=',real(sqrt(s2))
     @             ,' sqrt(wlambd)/sprim=',real(dsqrt(wlambd)/sprim)
        write(6,*)
     @            'decay c1e-,c2-n,c3n,c4e+',real(cthe1),real(cthe2)
     @             ,real(cthe3),real(cthe4)
      ENDIF
!------------]]]]]]]]

!====================================================================
!--------------------------------------------------------------------
!  Optional Rejection according to principal weight  
      IF( keywgt .EQ. 0) THEN
         CALL varran(drvec,1)
         rn = drvec(1)
         CALL Gmonit(  0, idgen, xcrude*wtmax, wtmax,1d0)
!        = = = = = = = = = = = = = = = = = = = = = = = = 
         IF(wtmod .LT. rn*wtmax ) GOTO 200
         WTCRUD   =1D0
         WTMOD    =1D0
!-- Precaution measure
         DO i=1,100
            WTSET(i)=0
         ENDDO
      ELSE
!-- Weighted events  
         CALL Gmonit(  0, idgen,      xcrude, wtmax,1d0)
!        = = = = = = = = = = = = = = = = = = = = = = = = 
      ENDIF

! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IF( wtcrud .NE. 0d0) THEN
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! TOHEP sets into HEPEVT all generated particles.
!       it decays taus and generates bremsstrahlung
!       in tau and W decays.
         CALL TOHEP
! and TOHAD moves to LUND format.
! It hadronizes whatever requires.
         CALL TOHAD(IFHADM,IFHADP)
      ELSE
! Some routine to set HEPEVT to 0 should be here <<<<<============
        continue
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      ENDIF
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!--------------------------------------------------------------------
!====================================================================


!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      IF((NEVTRU.LE.10. OR. NEVTRU.EQ.2000000).AND.WTKARL.GT.0D0)THEN
c         write(   6,*) 'wtborn=',wtborn
c         write(nout,*) 'wtborn=',wtborn
c         CALL DUMPL(6,Q1,Q2,P1,P2,P3,P4,QEFF1,QEFF2,sphot,nphot)
C         CALL DUMPW(6,Q1,Q2,P1,P2,P3,P4,QEFF1,QEFF2,sphot,nphot)
         CALL DUMPW(NOUT,Q1,Q2,P1,P2,P3,P4,QEFF1,QEFF2,sphot,nphot)
      ENDIF

!      IF(wtkarl.ne.0) THEN
!        CALL EXCAL(SVAR,SPRIM,S1,S2,XMATR) !COMP. WITH EXCALIBUR
!      ENDIF
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



! ==================================================================
! ====================POSTgeneration================================
! ==================================================================
!     *************************
      ELSEIF( mode .EQ .1) THEN     
!     *************************

      CALL karlud(1,xcrude,xcvesk,dumm1)       

      IF( KeyISR .NE. 0 ) THEN
!-- betas

!-- beta0 1ord
        CALL GMONIT(1,IDYFS+71,DUMM1,DUMM2,DUMM3)   
        WTKABO = AVERWT      
        ERKABO = ERRELA 
        WRITE(NOUT,BXOPE)         
        WRITE(NOUT,BXTXT) '         KORALW  FINAL  REPORT '
        WRITE(NOUT,BXTXT) '               window B        '
        WRITE(NOUT,BXTXT) '                               '
        WRITE(NOUT,BXTXT) ' Beta0  1st Order contrib. to Total Xsect.'
        WRITE(NOUT,BXL1I) NEVNEG,  'WTbe01<0  events '    ,'NEVNEG','B0'
        WRITE(NOUT,BXL2F) WTKABO,ERKABO
     $                             ,'<WTbe01>virt rel err','WTKABO','B1'
        CALL GMONIT(1,IDYFS+74,DUMM1,DUMM2,DUMM3)   
        WTKABO = AVERWT      
        ERKABO = ERRELA 
        XSKB   = XCRUDE*WTKABO
        ERKB   = XSKB*ERKABO
        WRITE(NOUT,BXL2F) WTKABO,ERKABO
     $                           ,'<WTSET(20)*WTCRUD>virt','WTKABO','B2'
        WRITE(NOUT,BXL2F) XSKB,ERKB, 'sigma (born*bevi01)','XSKABO','B3'

!-- beta1 1ord
        CALL GMONIT(1,IDYFS+72,DUMM1,DUMM2,DUMM3)   
        WTKABO = AVERWT      
        ERKABO = ERRELA 
        WRITE(NOUT,BXTXT) '                               '
        WRITE(NOUT,BXTXT) ' Beta1  1st Order contrib. to Total Xsect.'
        WRITE(NOUT,BXL1I) NEVNEG,  'WTbe10<0  events '    ,'NEVNEG','B4'
        WRITE(NOUT,BXL2F) WTKABO,ERKABO
     $                             ,'<WTbe10>real rel err','WTKABO','B5'
        CALL GMONIT(1,IDYFS+75,DUMM1,DUMM2,DUMM3)   
        WTKABO = AVERWT      
        ERKABO = ERRELA 
        XSKB   = XCRUDE*WTKABO
        ERKB   = XSKB*ERKABO
        WRITE(NOUT,BXL2F) XSKB,ERKB, 'sigma (born*bere10)','XSKABO','B6'
        WRITE(NOUT,BXCLO)  

!-- beta0+beta1 1ord
        CALL GMONIT(1,IDYFS+73,DUMM1,DUMM2,DUMM3)   
        WTKABO = AVERWT      
        ERKABO = ERRELA 
        WRITE(NOUT,BXOPE)         
        WRITE(NOUT,BXTXT) '         KORALW  FINAL  REPORT '
        WRITE(NOUT,BXTXT) '               window C        '
        WRITE(NOUT,BXTXT) '                               '
        WRITE(NOUT,BXTXT) '        1st Order  Total Xsect.'
        WRITE(NOUT,BXL1I) NEVNEG,  'WTbe0+1<0  events '   ,'NEVNEG','C0'
        WRITE(NOUT,BXL2F) 
     $          WTKABO,ERKABO,'<WTbe0+1> 1ord rel err','WTKABO','C1'
        CALL GMONIT(1,IDYFS+76,DUMM1,DUMM2,DUMM3)   
        WTKABO = AVERWT      
        ERKABO = ERRELA 
        XSKB   = XCRUDE*WTKABO
        ERKB   = XSKB*ERKABO
        WRITE(NOUT,BXL2F)XSKB,ERKB,'sigma O(alpha)'       ,'XSKABO','C2'
!-- beta0+beta1+beta2 2ord
        CALL GMONIT(1,IDYFS+77,DUMM1,DUMM2,DUMM3)   
        WTKABO = AVERWT      
        ERKABO = ERRELA 
        WRITE(NOUT,BXTXT) '                               '
        WRITE(NOUT,BXTXT) '        2nd Order  Total Xsect.'
        WRITE(NOUT,BXL1I) NEVNEG,'WTbe0+1+2<0  events '   ,'NEVNEG','C3'
        WRITE(NOUT,BXL2F) 
     $          WTKABO,ERKABO,'<WTbe0+1+2> 2ord rel err'  ,'WTKABO','C4'
        CALL GMONIT(1,IDYFS+78,DUMM1,DUMM2,DUMM3)   
        WTKABO = AVERWT      
        ERKABO = ERRELA 
        XSKB   = XCRUDE*WTKABO
        ERKB   = XSKB*ERKABO
        WRITE(NOUT,BXL2F)XSKB,ERKB,'sigma O(alpha**2)'    ,'XSKABO','C5'

!-- beta0+beta1+beta2 2ord OVER wtmax
        CALL GMONIT(1,IDYFS+80,DUMM1,DUMM2,DUMM3)   
        WTKABO = AVERWT      
        ERKABO = ERRELA 
        XSKB   = XCRUDE*WTKABO
        ERKB   = XSKB*ERKABO
        WRITE(NOUT,BXTXT) '                               '
        WRITE(NOUT,BXTXT) ' Best Order (WTMOD) Total Xsect. OVER wtmax'
        WRITE(NOUT,BXL1I) NEVACC,  'no of ev OVER wtmax','NEVACC','C6'
        WRITE(NOUT,BXL2F)XSKB,ERKB,'sigma MODEL OVER   ','XSKABO','C7'
        WRITE(NOUT,BXCLO)         
!-- end betas
      ENDIF     

!-- best xsection, total, for output purposes only!
      IF( KeyISR .eq. 0) THEN
        CALL GMONIT(1,IDYFS+58,DUMM1,DUMM2,DUMM3)   
      ELSE
        CALL GMONIT(1,IDYFS+78,DUMM1,DUMM2,DUMM3) 
      ENDIF  
      XSBEST  = XCRUDE*AVERWT 
      ERBEST  = XSBEST*ERRELA 

      NPAR(10)=NEVTRU
      NPAR(11)=NEVTOT
      XPAR(20)=XSBEST
      XPAR(21)=ERBEST
      XPAR(30)=XCRUDE !!!!! <==== to be changed
      XPAR(31)=WTMAX 
      IF(keywgt.eq.0) 
     $ XPAR(30)=XPAR(30) *NEVTRU/NEVTOT*WTMAX !!!!! <====

C CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! Final printouts of TAUOLA
      KeyWgt = MOD(KeyTek,10)
      IF( KeyWgt .EQ. 0) THEN
         CALL DEXAY(100,POL)
      ENDIF
C CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


!     **********************
      ELSEIF(MODE.EQ.2) THEN     
!     **********************
      CALL KARLUD(MODE,DUM1,XSBORN,ERBORN)       
!     ****
      ELSE          
!     ****
      WRITE(NOUT,*) '===>KORALW: WRONG MODE'                
      STOP          
!     *****        
      ENDIF         
!     *****        
      END                 

      SUBROUTINE karlud(mode,par1,par2,par3)     
!     **************************************     
! low level  monte-carlo generator          
! administrates directly generation of v-variable             
! and indirectly of all other variables.    
!     **************************************              
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)   
      PARAMETER (NMAX= 40)   
      COMMON / MATPAR / PI,CEULER     
      COMMON / PHYPAR / ALFINV,GPICOB     
      COMMON / WEKING / ENE,AMAZ,GAMMZ,AMEL,AMFIN,XK0,SINW2,IDE,IDF 
      COMMON / WEKIN2 / AMAW,GAMMW,GMU,ALPHAW   
! This common can be everywhere, contains various switches
      COMMON / KeyKey/  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
! communicates with vesko/rhosko            
      COMMON / VVREC  / VVMIN,VVMAX,VV,BETI  
      COMMON / VVINT  / VVTRUE  
      COMMON / SSTHE / S1,S2                 
      COMMON / ANGLES / COSTHE,PHI,COSDE1,PHI1,COSDE2,PHI2
      COMMON / INOUT  / NINP,NOUT     
! communicates with gmonit                  
      COMMON / CMONIT/ AVERWT,ERRELA,NEVTOT,NEVACC,NEVNEG,NEVOVE,NEVZER 
      COMMON / BXFMTS / BXOPE,BXCLO,BXTXT,BXL1I,BXL1F,BXL2F,BXL1G,BXL2G
      COMMON / MOMSET / QEFF1(4),QEFF2(4),SPHUM(4),SPHOT(100,4),NPHOT 
      COMMON / MOMDEC / Q1(4),Q2(4),P1(4),P2(4),P3(4),P4(4)
      COMMON / WGTGEN / WTMAX,WTVES,WTYFS,WTSS,WTBWIG,WTBORN      
      COMMON / WGTALL / WTCRUD,WTMOD,WTSET(100)      
      COMMON / DECAYS / IFLAV(4), AMDEC(4), BR(2), BREL
      CHARACTER*80      BXOPE,BXCLO,BXTXT,BXL1I,BXL1F,BXL2F,BXL1G,BXL2G
      SAVE / VVINT /,/ ANGLES /,/ MOMSET /,/ MOMDEC /
      SAVE / WEKING /,/ VVREC  /,/ INOUT  /,/ DECAYS /
      SAVE / CMONIT /,/ BXFMTS /,/ WEKIN2/,/ WGTGEN /,/ SSTHE/,/WGTALL/
      SAVE SVAR,BETI2,GAMFAP,GAMFAC,GAMFA2,PREC,XDEL,XCVESK
      SAVE
CBB      EXTERNAL RHOSKO,VVDISB,VVFUN,UNIT
      EXTERNAL RHOSKO,VVDISB
      DIMENSION drvec(1)


      IF(MODE.EQ.-1) THEN                   
!     ==================================================================
!     ===================INITIALIZATION=================================
!     ==================================================================
ccc      WRITE(NOUT,BXOPE)         
ccc      WRITE(NOUT,BXTXT) 'Initialize KARLUD  start'              
      SVAR = 4D0*ENE**2
      IDYFS = 0

!-- initialize control histos for YFSGEN
      CALL GMONIT(-1,IDYFS+51,0D0,1D0,1D0)  
      CALL GMONIT(-1,IDYFS+52,0D0,1D0,1D0)  
      CALL GMONIT(-1,IDYFS+53,0D0,1D0,1D0)  
      CALL GMONIT(-1,IDYFS+54,0D0,1D0,1D0)  
      CALL GMONIT(-1,IDYFS+55,0D0,1D0,1D0)  
      CALL GMONIT(-1,IDYFS+56,0D0,1D0,1D0)  
!-- initialize control weight histos
      CALL GMONIT(-1,IDYFS+57,0D0,1D0,1D0)  ! VESKO crude
      CALL GMONIT(-1,IDYFS+58,0D0,1D0,1D0)  ! WTBORN
      CALL GMONIT(-1,IDYFS+59,0D0,1D0,1D0)  ! crude BORN (no Matr.El.)
      CALL GMONIT(-1,IDYFS+60,0D0,1D0,1D0)  ! WTBORN over WTMAX
!-- initialize decays
      CALL DECAY(-1)

      BETI = 2D0/ALFINV/PI*(DLOG(4D0*ENE**2/AMEL**2)-1D0)     
      BETI2= 2D0/ALFINV/PI* DLOG(4D0*ENE**2/AMEL**2)          
      GAMFAP =1D0-PI**2*BETI**2/12D0        
      GAMFAC =EXP(-CEULER*BETI)/DPGAMM(1D0+BETI)               
      GAMFA2 =EXP(-CEULER*BETI2)/DPGAMM(1D0+BETI2)

!-- Initialization of QED part
      KeyISR = MOD(KeyRad,10)
!-- Calculation of CRUDE normalization
      IF( KeyISR.ne.0)THEN
        CALL vesk1w(-1,rhosko,dum1,dum2,xcvesk) 
        CALL gifyfs(svar,amel,fyfs)
        xcrude = xcvesk*fyfs          
!((((((( test            
cccccc        PREC = 1D-14
cccccc        XCGAUS =BREMKF(1,PREC)   
cccccc        XDEL = XCVESK/XCGAUS-1
cccccc        WRITE(NOUT,BXTXT) '  VESKO initialisation REPORT'
cccccc        WRITE(NOUT,BXTXT) '          mode -1            '
cccccc        WRITE(NOUT,BXL1F) XCVESK,'approx xs_crude  VESKO','XCVESK','V1'
cccccc        WRITE(NOUT,BXL1F) XCGAUS,'exact  xs_crude  GAUSS','XCGAUS','V2'
cccccc        WRITE(NOUT,BXL1F) XDEL   ,'XCVESK/XCGAUS-1      ','      ','V3'
cccccc        WRITE(NOUT,BXTXT) 'Initialize KARLUD  end '              
cccccc        WRITE(NOUT,BXCLO)
!))))))) end test
      ELSE
        CALL resms2(1,svar,svar,amaw,gammw,s1dum,s2dum,xcss,sscru)
        xcrude=bornsc(svar,2)*xcss
        fyfs=1d0
        xcvesk=0d0
      ENDIF
!-- outputs
      par1=xcrude
      par2=xcvesk
      par3=xcrude

! ==================================================================
! ====================GENERATION====================================
! ==================================================================
      ELSEIF( mode .EQ. 0) THEN
!     *************************
   30 CONTINUE            
      WTVES=1D0
      WTSS=1D0
      WTYFS=1D0

! generate vv             
      IF( KeyISR.ne.0 ) THEN                  
        CALL vesk1w( 0,rhosko,dum1,dum2,wtves)           
! low-level multiphoton generator           
        CALL YFSgen(vv,vvmin,nmax,wt1,wt2,wt3)
                  
!((((((((------TESTS ON INTERNAL WEIGHTS    
!--------------DOES NOT INTERFERE WITH THE EVENT GENERATION !!! 
        REF  = VVRHO(50,SVAR,AMEL,VV,VVMIN)                   
        WTR  = REF/VVRHO(1,SVAR,AMEL,VV,VVMIN)                
        CALL varran(drvec,1)
        CALL GMONIT(0,IDYFS+56,WTR,1D0,drvec(1))          
! pseudorejection in order to introduce reference xsection
!cc        IF(KEYFIX.EQ.0 .AND. drvec(1).GT.WTR) GOTO 110                
        IF(drvec(1).GT.WTR) GOTO 110                
        WF1  = WT1*VVRHO(51,SVAR,AMEL,VV,VVMIN)/REF           
        WF2  = WT2*VVRHO(52,SVAR,AMEL,VV,VVMIN)/REF           
        WF3  = WT3          
        WF13 = WF1*WF3      
        WF123= WF1*WF2*WF3                    
        CALL GMONIT(0,IDYFS+51,WF1,  1D0,1D0)       
        CALL GMONIT(0,IDYFS+52,WF2,  1D0,1D0)       
        CALL GMONIT(0,IDYFS+53,WF3,  1D0,1D0)       
        CALL GMONIT(0,IDYFS+54,WF13, 1D0,1D0)       
        CALL GMONIT(0,IDYFS+55,WF123,1D0,1D0)       
  110   CONTINUE            
!))))))))----------

!-- photons under IR cut treated as 0
        vvtrue=vv
        IF( vv .LT. vvmin) vv=0d0
      ELSE                
        vv=0d0          
        vvtrue=0d0
        wt1=1d0
        wt2=1d0
        wt3=1d0
      ENDIF         
      wtYFS=wt1*wt2*wt3
      IF( wtYFS .EQ. 0D0) GOTO 140           
      sprim=svar*(1-vv)      
!-- generate ww masses
      CALL RESMS2(0,SVAR,SPRIM,AMAW,GAMMW,S1,S2,XCSS,SSCRU)
!-- check threshold on s1, s2
      IF(s1 .LT. 0d0 .OR. s2 .LT. 0d0 .OR .sprim .LT. 0d0) THEN
        write(6,*)'s1,s2,sprim',s1,s2,sprim
      ENDIF
      IF(sqrt(s1)+sqrt(s2).gt.sqrt(sprim)) THEN
        wtss=0d0
        GOTO 140
      ENDIF
!-- generate decay channel
      CALL DECAY(0)
!-- check thresholds on decays
      IF(amdec(1)+amdec(2).gt.sqrt(s1)) THEN
        wtss=0D0
        GOTO 140
      ENDIF
      IF(amdec(3)+amdec(4).gt.sqrt(s2)) THEN
        wtss=0D0
        GOTO 140
      ENDIF
!-- generate cos theta in ww rest fr.
      CALL cospro(0,sprim,s1,s2,costhe,phi,xccos)
!-- generate decay angles in w rest fr.
      CALL cosdec(0,sprim,cosde1,phi1,xccos1)
      CALL cosdec(0,sprim,cosde2,phi2,xccos2)
!-- generate decay products
      CALL kineww(svar,sprim,costhe,phi,cosde1,phi1,cosde2,phi2,
     $             sqrt(s1),sqrt(s2),amdec,   q1,q2,p1,p2,p3,p4)
!-- end generation
!--------------------------------------------------------
!-- running ww breit-wigners weight
      wtbwig= bwign(s1,s2)
 140  CONTINUE
      wtkarl=wtves*wtss*wtyfs*wtbwig
!-- crude born 
      xcborn=bornsc(sprim,0)*xccos *xccos1*xccos2*sscru
!--------------------------------------------------------
!-- exact born !!!
      IF(wtkarl.ne.0d0)THEN
        BORN= WWBORN(SVAR,SPRIM,S1,S2)
        WTBORN=BORN/XCBORN
      ELSE
        WTBORN=0D0
      ENDIF
!-- end born
!--------------------------------------------------------
!-- control histogramming of weights
      wttot=wtkarl*wtborn
      CALL gmonit(0,idyfs+59,wtkarl,  1d0,1d0)        
      CALL gmonit(0,idyfs+58,wttot,  1d0,1d0)        
      CALL gmonit(0,idyfs+57,wtves,  1d0,1d0)        
      IF(wttot.lt.wtmax) THEN 
        WTOVR=0D0
      ELSE
        WTOVR=WTTOT
      ENDIF
      CALL gmonit(0,idyfs+60,wtovr,  1d0,1d0)        
!-- end weights histogramming
!--------------------------------------------------------
      par1=0d0             
      par2=wtkarl
      par3=wtborn 

!     ======================
      ELSEIF(MODE.EQ.1) THEN                
!     ======================

!-- crude xsection XCVESK and YFS formfactor FYFS
      IF( KeyISR.ne.0 )THEN
        CALL vesk1w( 1,rhosko,xsve,erelve,xcvesk)
        CALL gifyfs(svar,amel,fyfs)
        xcrude=xcvesk*fyfs          
!(((((( internal checks on VESKO
        CALL GMONIT(1,IDYFS+57,DUMM1,DUMM2,DUMM3)   
        WTNOSS = AVERWT      
        ERNOSS = ERRELA 
        PREC   = 1D-16
        XSGS   = BREMKF(1,PREC)
        ERGS   = XSGS*PREC
        ERVE   = XSVE*ERELVE       
        DDV    = XSVE/XSGS-1D0
        DDR    = ERELVE + 1D-6
        XDEL   = XCVESK/XSGS-1
        WRITE(NOUT,BXOPE)         
        WRITE(NOUT,BXTXT) '            Window V           '
        WRITE(NOUT,BXTXT) '       VESKO FINAL REPORT      '
        WRITE(NOUT,BXTXT) '             mode  1           '
        WRITE(NOUT,BXL1F)XCVESK,   'approx xs_crude VESKO','XCVESK','V4'
        WRITE(NOUT,BXL2F)XSVE,ERVE,'exact  xs_crude VESKO','XSVE  ','V5'
        WRITE(NOUT,BXL2F)XSGS,ERGS,'exact  xs_crude Gauss','XSGS  ','V6'
        WRITE(NOUT,BXL1F) XDEL    ,'XCVESK_appr/XSGS-1   ','      ','V7'
        WRITE(NOUT,BXL2F)DDV,DDR,  ' XSVE_exact/XSGS-1   ','      ','V8'
        WRITE(NOUT,BXCLO)         
!)))))) end VESKO internal checks 
      ELSE
        CALL resms2(1,svar,svar,amaw,gammw,s1dum,s2dum,xcss,sscru)
        xcrude=bornsc(svar,2)*xcss
        fyfs=1d0
        xcvesk=0d0
      ENDIF
!-- outputs
      par1=xcrude
      par2=xcvesk  
      par3=xcrude

!-- crude xs. NO born
      CALL GMONIT(1,IDYFS+59,DUMM1,DUMM2,DUMM3)   
      WTKACR = AVERWT      
      ERKACR = ERRELA 
      WRITE(NOUT,BXOPE)         
      WRITE(NOUT,BXTXT) '         KARLUD  FINAL  REPORT '
      WRITE(NOUT,BXTXT) '               window A        '
      WRITE(NOUT,BXTXT) '                               '
      WRITE(NOUT,BXTXT) '     Xsect with NO Matrix Element   '
      WRITE(NOUT,BXL1I) NEVTOT,     'total no of events ','NEVTOT','A0'
      WRITE(NOUT,BXL1I) NEVNEG,'WTcrud < 0 evts         ','NEVNEG','A1'
      XSKR   = XCRUDE*WTKACR
      ERKR   = XSKR*ERKACR
      WRITE(NOUT,BXL1F) XCRUDE,' sigma_crude            ','XCRUDE','A2'
      WRITE(NOUT,BXL2F) WTKACR,ERKACR,'<WTcrud> rel err ','WTKACR','A3'
      WRITE(NOUT,BXL2F) XSKR,ERKR,'sigma, no Matrix El. ',  'XSKR','A4'
      WRITE(NOUT,BXTXT) ' '         


!-- born xsection, total
      CALL GMONIT(1,IDYFS+58,DUMM1,DUMM2,DUMM3)   
      WTKABO = AVERWT      
      ERKABO = ERRELA 
      XSKB0  = XCRUDE*WTKABO 
      ERKB0  = XSKB0*ERKABO 
      WRITE(NOUT,BXTXT) '                               '
      WRITE(NOUT,BXTXT) ' Xsect with Born Matrix El. only, NO Betas'
      WRITE(NOUT,BXL1I) NEVTOT,  'total no of events '   ,'NEVTOT','A5'
      WRITE(NOUT,BXL1I) NEVNEG,  'WTcrud*WTborn <0 evts' ,'NEVNEG','A6'
      WRITE(NOUT,BXL2F) 
     $           WTKABO,ERKABO,'<WTcrud*WTborn>, rel err','WTKABO','A7'
      WRITE(NOUT,BXL2F) XSKB0,ERKB0, 'sigma (Born M.El.)', 'XSKA0','A8'

!-- born xsection from above WTMAX
      CALL GMONIT(1,IDYFS+60,DUMM1,DUMM2,DUMM3)   
      WTKABO = AVERWT
      ERKABO = ERRELA
      XSKB   = XCRUDE*WTKABO
      ERKB   = XSKB*ERKABO
      XX=XSKB/XSKB0
      EE=ERKB/XSKB0
      WRITE(NOUT,BXTXT) '                               '
      WRITE(NOUT,BXTXT) '        Xsect OVER Wtmax       '
      WRITE(NOUT,BXTXT) ' Born Matrix El. only, no Betas'
      WRITE(NOUT,BXL1I) NEVACC,  'no of ev OVER wtmax','NEVACC','A9'
      WRITE(NOUT,BXL2F) XSKB,ERKB,  'sigma OVER wtmax','XSKABO','A10'
      WRITE(NOUT,BXL2F) XX,EE,      'relat sigma OVER','XSKABO','A11'
      WRITE(NOUT,BXCLO)         

! ============
      ELSE  
! ============

      IF( KeyISR.ne.0 ) THEN
      CALL GMONIT(1,IDYFS+51,DUMM1,DUMM2,DUMM3)   
      DEL1   = AVERWT-1D0                   
      DWT1   = ERRELA     
      CALL GMONIT(1,IDYFS+52,DUMM1,DUMM2,DUMM3)   
      AWF2   = AVERWT     
      DWT2   = ERRELA     
      CALL GMONIT(1,IDYFS+53,DUMM1,DUMM2,DUMM3)   
      AWF3   = AVERWT     
      DEL3   = AVERWT-GAMFA2                
      DWT3   = ERRELA     
      CALL GMONIT(1,IDYFS+54,DUMM1,DUMM2,DUMM3)   
      AWF4   = AVERWT     
      DEL4   = AVERWT-GAMFAC                
      DWT4   = ERRELA     
      WRITE(NOUT,BXOPE)         
      WRITE(NOUT,BXTXT) '     KARLUD  FINAL  REPORT     '
      WRITE(NOUT,BXTXT) '         window B              '
      WRITE(NOUT,BXL2F) DEL1,DWT1,  '<WF1>-1  mass wt   ','DEL1  ','B1'
      WRITE(NOUT,BXL2F) AWF2,DWT2,  '<WF2> dilat. weight','AWF2  ','B2'
      WRITE(NOUT,BXL2F) AWF3,DWT3,  '<WF3> dilat. weight','AWF3  ','B3'
      WRITE(NOUT,BXL2F) DEL3,DWT3,  '<WF3>-YGF(BETI2)   ','DEL3  ','B4'
      WRITE(NOUT,BXL2F) AWF4,DWT4,  '<WF1*WF3>          ','AWF4  ','B5'
      WRITE(NOUT,BXL2F) DEL4,DWT4,  '<WF1*WF3>-YGF(BETI)','DEL4  ','B6'
      WRITE(NOUT,BXCLO)         
!     ==================================================================
      CALL GMONIT(1,IDYFS+59,DUMM1,DUMM2,DUMM3)   
      WTKARL = AVERWT      
      ERKARL = ERRELA 
      CALL VESK1W( 1,RHOSKO,XSVE,ERELVE,XCVESK)
      XSKR=XCVESK*WTKARL
      CALL GMONIT(1,IDYFS+55,DUMM1,DUMM2,DUMM3)   
      AWF5   = AVERWT     
      DEL5   = AVERWT-GAMFAC                
      DWT5   = ERRELA     
      CALL GMONIT(1,IDYFS+56,DUMM1,DUMM2,DUMM3)   
      AWF6   = AVERWT     
      PREC = 1D-6
      XREFER = BREMKF(50,PREC)                   
      DELKAR = XREFER*AWF5/XSKR  -1D0       
      DELREF = XCVESK*AWF6/XREFER-1D0       
      WRITE(NOUT,BXOPE)         
      WRITE(NOUT,BXTXT) '     KARLUD  FINAL  REPORT CONT.   '
      WRITE(NOUT,BXTXT) '         WINDOW C                  '
      WRITE(NOUT,BXTXT) 'BETI= 2*ALFA/PI*(LOG(S/MEL**2)-1)       '
      WRITE(NOUT,BXTXT) 'GAMFAP= 1-PI**2*BETI**2/12              '
      WRITE(NOUT,BXTXT) 'GAMFAC=EXP(-CEULER*BETI)/GAMMA(1+BETI)  '
      WRITE(NOUT,BXTXT) 'GAMFA2=EXP(-CEULER*BETI2)/GAMMA(1+BETI2)'
      WRITE(NOUT,BXL1F)  BETI,        '                =','BETI  ','C1'
      WRITE(NOUT,BXL1F)  GAMFAP,      '                =','GAMFAP','C2'
      WRITE(NOUT,BXL1F)  GAMFAC,      '                =','GAMFAC','C3'
      WRITE(NOUT,BXL1F)  GAMFA2,      '                =','GAMFA2','C4'
      WRITE(NOUT,BXL2F) AWF5,DWT5, ' <WF1*WF3*WF4>      ','AWF5  ','C5'
      WRITE(NOUT,BXL2F) DEL5,DWT5, ' <WF1*WF3>-YGF(BETI)','DEL5  ','C6'
      WRITE(NOUT,BXTXT) 'DELKAR=XREFER*AVER(WF1*WF1*WF3)/XSKARL-1'
      WRITE(NOUT,BXTXT) 'DELREF=XCRUDE*AVER(WTR)/XREFER-1        '
      WRITE(NOUT,BXL1F) XREFER,    'reference x_sect.   ','XREFER','C7'
      WRITE(NOUT,BXL1F) DELKAR,    'XREFER*AWF5/XSKR  -1','DELKAR','C8'
      WRITE(NOUT,BXL1F) DELREF,    'XCVESK*AWF6/XREFER-1','DELREF','C9'
      WRITE(NOUT,BXCLO)         
      ENDIF

!-- born xsection, total, for output purposes only!
      IF( KeyISR.ne.0 ) THEN
        CALL vesk1w( 1,rhosko,xsve,erelve,xcvesk)
        CALL gifyfs(svar,amel,fyfs)          
        xcrude=xcvesk*fyfs          
      ELSE
        CALL resms2(1,svar,svar,amaw,gammw,s1dum,s2dum,xcss,sscru)
        xcrude=bornsc(sprim,2)*xcss
        fyfs=1d0
        xcvesk=0d0
      ENDIF
      CALL gmonit(1,idyfs+58,dumm1,dumm2,dumm3)   
      xsborn  = xcrude*averwt 
      erborn  = xsborn*errela 
      par1=0d0
      par2=xsborn
      par3=erborn
! ==========
      ENDIF               
! ==========
      END                 



      SUBROUTINE resms2(mode,svar,sprim,rmas,rgam,s1,s2,prnorm,xcrud)
!     ***************************************************************
! Generation of masses of two resonances produced in a collision;
! note: both resonances have the same mass distribution function
! INPUT:  mode      0 -generation
!                   1 -normalisation prnorm and crude dist. xcrud  
!                      only (no gener.)
!         svar    - max sprim
!         sprim   - actual s
!         rmas    - central value of a resonance mass distribution
!         rgam    - width of a resonance
! OUTPUT: s1, s2  - svar's of two resonances
!         prnorm  - value of the integral of crude distr. 
!         xcrud   - value of the crude distribution
!                   non-running BWigners divided out!
!
! Written by: M. Skrzypek            date: 2/16/95
! Last update:                         by: 
!
      IMPLICIT real*8 (a-h,o-z)
      COMMON / matpar / pi,ceuler     
      REAL*8 drvec(100)
      SAVE a,b,winf,wmi,wma,wsqr,wrec,prsqr,prrec
      SAVE uma,umi,uinf,usqr,urec
      SAVE
!
!        write(6,*)'resms2',rmas,rgam
      a=rmas**2
      b=rmas*rgam
! arctg
      winf = 1/b*atan((svar   -a)/b)
      wma  = 1/b*atan((sprim/4d0-a)/b)
      wmi  = 1/b*atan(        -a /b)
! logarithm
      uinf =1/2d0/a*dlog((svar   -a)**2 +b**2)
      uma  =1/2d0/a*dlog((sprim/4d0-a)**2 +b**2)
      umi  =1/2d0/a*dlog(                b**2)
! thetas
      thespr=1d0
      thesvr=1d0
      IF((sprim/4d0).lt.a) thespr=0d0
      IF( svar      .lt.a) thesvr=0d0
      ulo= thespr*uma +(1d0-thespr)*umi
! normalisations
      wsqr=wma-wmi 
      usqr=thespr*(uma-umi)
      prsqr=(wsqr+usqr)**2
      wrec=winf-wma 
      urec=thesvr*(uinf -ulo)
      prrec=(wsqr+usqr)*(wrec+urec)
      prnorm=prsqr+2*prrec
! crude distrib. value
      xcrud=1d0
      IF(s1.gt.a) xcrud=xcrud*s1/a
      IF(s2.gt.a) xcrud=xcrud*s2/a

      IF(mode.eq.1) return
!     ====================
 10   call varran(drvec,5)
      r1=drvec(1)
      r2=drvec(2)
      r3=drvec(3)
      r4=drvec(4)
      r5=drvec(5)

      IF(r3.le.prsqr/prnorm) THEN
!     ..square

!     ....s1
        IF(r4.ge.usqr/(wsqr+usqr)) THEN
!       ..arctg
          w1=r1*(wma-wmi) +wmi
          s1=b*tan(b*w1) +a
        ELSE
!       ..log
          u1=r1*(uma-umi) +umi
          s1=dsqrt(exp(2*a*u1) -b**2)+a
        ENDIF
!     ....s2
        IF(r5.ge.usqr/(wsqr+usqr)) THEN
!       ..arctg
          w2=r2*(wma-wmi) +wmi
          s2=b*tan(b*w2) +a
        ELSE
!       ..log
          u2=r2*(uma-umi) +umi
          s2=dsqrt(exp(2*a*u2) -b**2)+a
        ENDIF

      ELSEIF(r3.le.(prsqr+prrec)/prnorm) THEN
!     ..rectangle 1

!     ....s1
        IF(r4.ge.usqr/(wsqr+usqr)) THEN
!       ..arctg
          w1=r1*(wma-wmi) +wmi
          s1=b*tan(b*w1) +a
        ELSE
!       ..log
          u1=r1*(uma-umi) +umi
          s1=dsqrt(exp(2*a*u1) -b**2)+a
        ENDIF
!     ....s2
        IF(r5.ge.urec/(wrec+urec)) THEN
!       ..arctg
          w2=r2*(winf-wma) +wma     
          s2=b*tan(b*w2) +a
        ELSE
!       ..log
          u2=r2*(uinf-ulo) +ulo
          s2=dsqrt(exp(2*a*u2) -b**2)+a
        ENDIF

      ELSE
!     ..rectangle 2
!         write(6,*)'rect 1'
!     ....s1
        IF(r4.ge.urec/(wrec+urec)) THEN
!       ..arctg
          w1=r1*(winf-wma) +wma     
          s1=b*tan(b*w1) +a
        ELSE
!       ..log
          u1=r1*(uinf-ulo) +ulo
          s1=dsqrt(exp(2*a*u1) -b**2)+a
        ENDIF
!     ....s2
        IF(r5.ge.usqr/(wsqr+usqr)) THEN
!       ..arctg
          w2=r2*(wma-wmi) +wmi
          s2=b*tan(b*w2) +a
        ELSE
!       ..log
          u2=r2*(uma-umi) +umi
          s2=dsqrt(exp(2*a*u2) -b**2)+a
        ENDIF

      ENDIF

! crude distrib. value
      xcrud=1d0
      IF(s1.gt.a) xcrud=xcrud*s1/a
      IF(s2.gt.a) xcrud=xcrud*s2/a

      END

      SUBROUTINE cosdec(mode,svar,cdec,phi,xccos)
*     ***************************************
! Crude generation of decay costhe according to a simplified distribution.
!   mode: 0-generation
!         1-xccos of given cdec
!   cdec:  value of generated cosine
!   xccos: value of distribution function
      implicit real*8 (a-h,o-z)
      common / matpar / pi,ceuler     
      common / wekin2 / amaw,gammw,gmu,alphaw   
! This common can be everywhere, contains various switches
      COMMON / KeyKey/  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
      real*8 drvec(100)
      save

      KeySpn = MOD(KeyPhy,10000)/1000

      IF(keyspn.eq.1) THEN                        !1002=.78
        IF(svar.gt.500**2) THEN                    !502=.4
          delta=0.4d0+ (svar/500**2 -1)/8d0
        ELSEIF(svar.gt.4*amaw**2) THEN             !162=.4
          delta=.4d0
        ELSEIF(svar.gt.4*(amaw-5*gammw)**2) THEN   !142=.78
          delta=.4d0+ (1-svar/(4*amaw**2))*2d0
        ELSEIF(svar.gt.4*(amaw-10*gammw)**2) THEN  !122=40
          delta=.844d0+ (1-svar/(4*(amaw-5*gammw)**2))*100d0
        ELSE
          delta=40d0
        ENDIF

        IF(mode.eq.0)THEN
 11       call varran(drvec,3)
          cdec=2*drvec(1)-1
          xccos=(1+delta+cdec)/(1+delta)
          IF((2+delta)/(1+delta)*drvec(2).gt.xccos) goto 11
          phi =2*pi*drvec(3)
        ELSE
          xccos=(1+delta+cdec)/(1+delta)
        ENDIF
      ELSEIF(keyspn.eq.0) THEN
        IF(mode.eq.0)THEN
          call varran(drvec,3)
          cdec=2*drvec(1)-1
          phi =2*pi*drvec(3)
        ENDIF
        xccos=1d0
      ENDIF
      end


      SUBROUTINE cospro(mode,s,s1,s2,costhe,phipro,xccos)
*     ***************************************************
! Crude generation of costhe according to a simplified distribution.
! OUTPUT: costhe - cos(theta), theta - polar angle of W- in the CMS 
!         of the incoming beams (+z axis along e- direction)
!         xccos - value of the function
!                      (for mode=1 costhe becames input for xccos
!                                     - no generation)
c
! Written by: M. Skrzypek            date: 3/1/95
! Last update:                         by: 
c
      implicit real*8 (a-h,o-z)
      common / matpar / pi,ceuler     
      real*8 drvec(100)
      save
!
CC==>>

      wlambd=s**2+s1**2+s2**2-2*s*s1-2*s*s2-2*s1*s2
!      write(6,*)s,s1,s2,wlambd
      aa=(s-s1-s2)/2d0
      bb=-dsqrt(wlambd)/2d0
      ymi=dlog(aa-bb)/bb
      yma=dlog(aa+bb)/bb

!      z=.4d0/s*(aa-bb)*2/s
      z=0d0  ! auxilliary, supposed to be 0

      xccos=(1/(aa+bb*costhe) +z/2d0)/(yma-ymi+z) 
      IF(mode.eq.1) return

      call varran(drvec,3)

      y=drvec(1)*(yma-ymi)+ymi
      costhe=(exp(bb*y)-aa)/bb
      IF( drvec(2).gt.(yma-ymi)/(z+yma-ymi) )  costhe=2*drvec(1)-1  !
      xccos=(1/(aa+bb*costhe) +z/2d0)/(yma-ymi+z) 
c++      write(6,*)'tran cosgen',aa+bb*costhe
      phipro=2*pi*drvec(3)

      end

      SUBROUTINE excal(svar,sprim,s1,s2,xmatr)
!     ****************************************
!  plain matrix el. for comparison with excalibur
      implicit double precision (a-h,o-z)    
      common / matpar / pi,ceuler     
      common / phypar / alfinv,gpicob     
      common / wekin2 / amaw,gammw,gmu,alphaw   
      common / decays / iflav(4), amdec(4), br(2), brel
      save
        IF(iflav(1).eq.11.and.iflav(4).eq.-11) THEN
          call moms(15) 
          born= wwborn(svar,sprim,s1,s2)
          fak= (4*pi)**(-4)
!... Kinematical factors included in our Born
          x1=s1/sprim
          x2=s2/sprim
          bmain=sqrt( (1-x1-x2)**2 - 4*x1*x2 )
          xwm1=amdec(1)**2/s1
          xwm2=amdec(2)**2/s1
          bwm=sqrt( (1-xwm1-xwm2)**2 - 4*xwm1*xwm2 )
          xwp1=amdec(3)**2/s2
          xwp2=amdec(4)**2/s2
          bwp=sqrt( (1-xwp1-xwp2)**2 - 4*xwp1*xwp2 )
          fkin=fak *bmain*bwp*bwm/sprim
!          write(6,*)'s1,s2,sprim',s1,s2,sprim
          xmatr=born/fkin 
!-- breit-wigners for ww
          bwig1=(s1-amaw**2)**2 +(s1*gammw/amaw)**2
          bwig2=(s2-amaw**2)**2 +(s2*gammw/amaw)**2
!          write(6,*)'xmatr no BW',xmatr
          xmatr=xmatr/bwig1/bwig2
          IF(xmat1.eq.0d0) xmat1=xmatr
          write(6,*)'xmatr',xmatr ,xmatr/xmat1
          write(25,*)xmatr,'xmatr',xmatr/xmat1
        ENDIF
      end

      FUNCTION BWIGN(S1,S2)
!     *********************
!     WW running B-Wigners weight
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)   
      COMMON / WEKIN2 / AMAW,GAMMW,GMU,ALPHAW   
      SAVE / WEKIN2 /
      SAVE
      BWCRU1=(S1-AMAW**2)**2 +(AMAW*GAMMW)**2
      BWCRU2=(S2-AMAW**2)**2 +(AMAW*GAMMW)**2
      BWEXA1=(S1-AMAW**2)**2 +(S1/AMAW*GAMMW)**2
      BWEXA2=(S2-AMAW**2)**2 +(S2/AMAW*GAMMW)**2
      BWIGN=BWCRU1/BWEXA1*BWCRU2/BWEXA2
      END

      FUNCTION BORNSC(SVARI,MODE)            
!     ***********************************     
! THIS ROUTINE PROVIDES BORN CRUDE CROSS SECTION 
! mode = 1 : normalized to total(s)  
!        2 : normalized to differential 
!        0 : not normalized
!        difference is only in normalization !!!!                      
!     ***********************************     
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)     
      COMMON / MATPAR / PI,CEULER     
      COMMON / PHYPAR / ALFINV,GPICOB     
      COMMON / INOUT  / NINP,NOUT       
      COMMON / WEKING / ENE,AMAZ,GAMMZ,AMEL,AMFIN,XK0,SINW2,IDE,IDF 
      COMMON / WEKIN2 / AMAW,GAMMW,GMU,ALPHAW   
      SAVE / INOUT  /,/ WEKING /,/ WEKIN2 /
      SAVE
      
      BSS= PI/ALFINV**2 /2D0 /SINW2**2
      BBWIGN=AMAW**2/ALFINV/PI/SINW2
      THR=1D0/SVARI/4D0
      IF(SVARI.GT.8*AMAW**2) THEN
        BS=1D0/SVARI*(1+4*AMAW**2/SVARI)/2D0*LOG(SVARI/AMAW**2)
      ELSEIF(SVARI.GT.4*AMAW**2) THEN
        BETA=DSQRT(1-4*AMAW**2/SVARI)
        IF(BETA.LT.PI/ALFINV) BETA=PI/ALFINV ! COULOMB!
        BSR=1D0/SVARI*(1+4*AMAW**2/SVARI)/2D0*LOG(SVARI/AMAW**2)
        BS=BSR*(THR/BSR+BETA*(1-THR/BSR))
      ELSE
        BS=THR*SVARI/4/AMAW**2
      ENDIF
      BS=BS*(1+SVARI/SQRT((SVARI-AMAZ**2)**2+(SVARI*GAMMZ/AMAZ)**2))/2D0
      BNORDI= BNORL2(2)
      BNORTO=BSS*GPICOB*BBWIGN**2
!      BORNSC=BS*BNORM
!      WRITE(6,*)'NORMALIZ ', BNORDI, BNORTO,4*AMAW**2*BNORDI/BNORTO 
      IF(MODE.EQ.2) BORNSC=BS*4*AMAW**2 *2 *BNORDI
      IF(MODE.EQ.1) BORNSC=BS*4*AMAW**2 *2 *BNORTO
      IF(MODE.EQ.0) BORNSC=BS*4*AMAW**2 *2 
      END




      function bnorl2(keysch)
*     *************************************
c     normalisation from the scratch, convention ala LEP200
c
c Written by: M.Skrzypek            date: 18.03.95
c Last update:                 by: 
c
      implicit real*8 (a-h,o-z)
      common / matpar / pi,ceuler     
      common / phypar / alfinv,gpicob     
! This common can be everywhere, contains various switches
      common / KeyKey/  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp     
      common / weking / ene,amaz,gammz,amel,amfin,xk0,sinw2,ide,idf 
      common / wekin2 / amaw,gammw,gmu,alphaw   
      common / decays / iflav(4), amdec(4), br(2), brel
      save   / decays /,/ weking /,/ wekin2 /,/ keykey /
      save
c
c.. the total normalization from kinematics
c     (4*pi)**(-8)/(16d0*sprim) ds1 ds2 do1 do2 do beta1 beta2 beta |Mif|**2 

c.. |Mif| normalization
c     production:  e**4 = 16*pi**2*alphaw**2
c..   decays:      g**4 = e**4/sinw2**2=16*pi**2*alphaw**2/sinw2**2
c                         this is normalized to the value for (ev)(ev) channel 
c        for total x-sect over various channels the above is replaced by:
c        g**4* (\sum br(i))/br(el) *(\sum br(j))/br(el) = g**4/br(el)**2 
c        with  \sum br(i) = 1        

c.. the following fkin is included in WWBORN !!
c      x1=s1/sprim
c      x2=s2/sprim
c      bmain=sqrt( (1-x1-x2)**2 - 4*x1*x2 )
c      xwm1=amdec(1)**2/s1
c      xwm2=amdec(2)**2/s1
c      bwm=sqrt( (1-xwm1-xwm2)**2 - 4*xwm1*xwm2 )
c      xwp1=amdec(3)**2/s2
c      xwp2=amdec(4)**2/s2
c      bwp=sqrt( (1-xwp1-xwp2)**2 - 4*xwp1*xwp2 )
c      fkin=fak *bmain*bwp*bwm/sprim
c      wwborn=fkin*xmatr 
 
c.. inputs: alphaw, wmass, zmass, br(1) .. br(7) = br(el)
c.. other parameters
c      sinw2 = *see FILEXP*
c      g**2 = e**2/sinw2 = 4*pi*alphaw/sinw2

!... Coupling constant for WW production
      cprod=(4*pi*alphaw)**2

!... Coupling constant for single W-decay
      cdeca1=4*pi*alphaw/sinw2
      cdeca2=4*pi*alphaw/sinw2

!... Overall normalization factor for the cross section
!... remember what is already in WWBORN
      bnor = 1d0/(4*pi)**4 /16d0 *cprod*cdeca1*cdeca2
!... angular integrals
      bnor = bnor*(2*pi)**3

!... Normalization factor for the cross section depending on
!    choosen decay options
      if (keydwm.eq.0) then
        dfwm=1/brel
      else
        dfwm=br(1)/brel
      endif
      if (keydwp.eq.0) then
        dfwp=1/brel
      else
        dfwp=br(2)/brel
      endif
      defa=dfwm*dfwp
      bnor = defa*bnor

!... change units to picobarns
      bnor = bnor*gpicob
      bnorl2 = bnor
      end


!!! UNUSED
      function bornex(s)
*     *********************************
! exact, on-shell born(s)
      implicit real*8 (a-h,o-z)
      common / matpar / pi,ceuler     
      common / phypar / alfinv,gpicob     
      common / weking / ene,amaz,gammz,amel,amfin,xk0,sinw2,ide,idf 
      common / wekin2 / amaw,gammw,gmu,alphaw   
      save   / weking /,/ wekin2 /
      save
   
      IF(s.le.4*amaw**2) THEN
        bornex=0d0
        return
      ENDIF
       
      ams= amaw**2/s
      bet=sqrt(1-4*ams)
      cc1=amaz**2*(1-2*sinw2)/(s-amaz**2)
      cc2=amaz**4*(8*sinw2**2-4*sinw2+1)*bet**2/(s-amaz**2)**2/48d0

      sigma=  (1+2*ams+2*ams**2)/bet*log((1+bet)/(1-bet)) -5d0/4d0
     @       +cc1*( 2*(ams**2+2*ams)/bet*log((1+bet)/(1-bet))
     @             -1/ams/12d0 -5d0/3d0 -ams )
     @       +cc2*( 1/ams**2 +20/ams +12)

      bornex=pi/alfinv**2*bet/2d0/sinw2**2/s *gpicob
      end


      SUBROUTINE FILEXP(XPAR,NPAR)   
!     ***********************************   
! transfers and defines input params, prints input parameters 
      IMPLICIT DOUBLE PRECISION  (A-H,O-Z)  
      DIMENSION  XPAR( *),NPAR( *)          
      COMMON / WEKING / ENE,AMAZ,GAMMZ,AMEL,AMFIN,XK0,SINW2,IDE,IDF  
      COMMON / WEKIN2 / AMAW,GAMMW,GMU,ALPHAW
! This common can be everywhere, contains various switches
      COMMON / KeyKey/  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
! TAUOLA, PHOTOS and JETSET overall switches
      COMMON / LIBRA  / JAK1,JAK2,ITDKRC,IFPHOT,IFHADM,IFHADP

      COMMON / VVREC  / VVMIN,VVMAX,VV,BETI                   
      COMMON / WGTGEN / WTMAX,WTVES,WTYFS,WTSS,WTBWIG,WTBORN      
      COMMON / WGTALL / WTCRUD,WTMOD,WTSET(100)      
      COMMON / DECDAT / AMAFIN(20), BR(20), ICOD(20)
      COMMON / INOUT  / NINP,NOUT     
      COMMON / MATPAR / PI,CEULER     
      COMMON / PHYPAR / ALFINV,GPICOB     
      COMMON / BXFMTS / BXOPE,BXCLO,BXTXT,BXL1I,BXL1F,BXL2F,BXL1G,BXL2G 
      CHARACTER*80      BXOPE,BXCLO,BXTXT,BXL1I,BXL1F,BXL2F,BXL1G,BXL2G       
      COMMON / RANPAR / KEYRND
      SAVE / WEKING /,/ KEYkey /,/ VVREC  /
      SAVE / INOUT  /,/ BXFMTS /,/ RANPAR /,/ WEKIN2 /
      SAVE

!-- Data
      CHARACTER*64 Logo(30)
      DATA Logo /
     $'***************************************************************',
     $'***************************************************************',
     $'***************************************************************',
     $'*  ###   ###                                   ###       ###  *',
     $'*  ###  ###  ####    ######      ##     ##     ###       ###  *',
     $'*  ### ###  ##  ##   ##   ##    ####    ##     ###       ###  *',
     $'*  ######  ##    ##  ##   ##   ##  ##   ##     ###       ###  *',
     $'*  ######  ##    ##  #####    ##    ##  ##     ###   #   ###  *',
     $'*  ### ###  ##  ##   ##  ##   ########  ##      ### ### ###   *',
     $'*  ###  ###  ####    ##   ##  ##    ##  #######  #### ####    *',
     $'*  ###   ###            version 1.02              ##   ##     *',
     $'***************************************************************',
     $'************************** July 95 ****************************',
     $'***************************************************************',
     $'                                                               ',
     $'***************************************************************',
     $'*  Written by:                                                *',
     $'*    M. Skrzypek    (skrzypek@hpjmiady.ifj.edu)               *',
     $'*    S. Jadach      (jadach@cernvm.cern.ch),                  *',
     $'*    W. Placzek     (placzek@hephp02.phys.utk.edu),           *',
     $'*    Z. Was         (wasm@cernvm.cern.ch)                     *',
     $'*  Papers:                                                    *',
     $'*    preprint CERN-TH/95-205, July 1995                       *',
     $'*  WWW:                                                       *',
     $'*    http://hpjmiady.ifj.edu.pl/                              *',
     $'*  Acknowledgements:                                          *',
     $'*    We acknowledge warmly very useful help of M. Martinez    *',
     $'*    in testing the program.                                  *',
     $'***************************************************************',
     $' '/ 

! ...BX-formats for nice and flexible outputs                 
      BXOPE =  '(//1X,15(5H*****)    )'     
      BXTXT =  '(1X,1H*,                  A48,25X,    1H*)'   
      BXL1I =  '(1X,1H*,I17,                 16X, A20,A12,A7, 1X,1H*)'
      BXL1F =  '(1X,1H*,F17.8,               16X, A20,A12,A7, 1X,1H*)' 
      BXL2F =  '(1X,1H*,F17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H*)'
      BXL1G =  '(1X,1H*,G17.8,               16X, A20,A12,A7, 1X,1H*)'
      BXL2G =  '(1X,1H*,G17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H*)'
      BXCLO =  '(1X,15(5H*****)/   )'       

C.. math. constants
      PI=3.1415926535897932D0
      CEULER = 0.57721566D0                  
      ZET2= PI**2/6D0  
      ZET3= 1.2020569031595942854D0       
C.. phys. constants
!     ALFINV  = 137.03604D0
      alfinv  = 137.0359895D0
!     GPICOB =  389.385D6    
      gpicob =  389.37966d6 
c.. electron mass for QED 
!     AMEL   = 0.5111D-3   
      amel   = 0.51099906d-3

C  type of the RANDOM NUMBER GENERATOR RANMAR                   

!-----------------------------------------------------------------------
! Physics switches 
! KeyRad =  1000*KeyCul+100*KeyNLL+10*KeyFSR+KeyISR
      KeyRad = NPAR(1)
      KeyISR = MOD(KeyRad,10)
      KeyFSR = MOD(KeyRad,100)/10
      KeyNLL = MOD(KeyRad,1000)/100
      KeyCul = MOD(KeyRad,10000)/1000
!
! KeyPhy = 10000*KeyRed +1000*KeySpn +100*KeyZet +10*KeyMas +KeyBra
      KeyPhy = NPAR(2)
      KeyBra = MOD(KeyPhy,10)
      KeyMas = MOD(KeyPhy,100)/10
      KeyZet = MOD(KeyPhy,1000)/100
      KeySpn = MOD(KeyPhy,10000)/1000
      KeyRed = MOD(KeyPhy,100000)/10000
!-----------------------------------------------------------------------
! Technical switches
! KeyTek = 10*KeyRnd +KeyWgt
      KeyTek = NPAR(3)
      KeyWgt = MOD(KeyTek,10)
      KeyRnd = MOD(KeyTek,100)/10
!-----------------------------------------------------------------------
! Miscelaneous, for future develoment
! KeyMis = KeyMix
      KeyMis = NPAR(4)
      KeyMix = MOD(KeyMis,10)
!-----------------------------------------------------------------------
!
      KEYDWM = NPAR(5)     
      KEYDWP = NPAR(6)     

      NOUT = NPAR(7)
      IF(nout.LE.0) nout=16     

      JAK1 = NPAR(21)
      JAK2 = NPAR(22)
      ITDKRC = NPAR(23)
      IFPHOT = NPAR(24)
      IFHADM = NPAR(25)
      IFHADP = NPAR(26)


      CMSENE = XPAR(1)      
      GMU    = XPAR(2)   
      ALFWIN = XPAR(3)
      AMAZ   = XPAR(4)
      GAMMZ  = XPAR(5)
      AMAW   = XPAR(6)
      GAMMW  = XPAR(7)
      VVMIN  = XPAR(8)
      VVMAX  = XPAR(9)
      WTMAX  = XPAR(10)
      ENE    = CMSENE/2D0      
      VVMAX  = MIN(VVMAX,1D0-(AMEL/ENE)**2)                   
      XPAR(9) =VVMAX ! send it back !!!
      IDE=2               
      IDF=2               
      XK0=3.D-3         

c.. sinw2 is calculated from gmu,alfwin,amaz
      A2 = PI / ( ALFWIN*SQRT(2D0)*GMU )
      SINW2 = ( 1-SQRT( 1-(4*A2/AMAZ**2) ) )/2D0  

c.. alpha_w
      ALPHAW = 1D0/ ALFWIN
!-----------------------------------------------------------------------
!-- sinw2 electroweak mixing angle
      IF(KeyMix .EQ. 1) THEN
         A2 = PI / ( ALFWIN*SQRT(2D0)*GMU )
         SINW2 = ( 1-SQRT( 1-(4*A2/AMAZ**2) ) )/2D0 
      ELSE 
!-- new definition for LEP200 workshop
         sinw2 = pi * alphaw /( sqrt(2d0) * amaw**2 * gmu )
      ENDIF
!-----------------------------------------------------------------------
!-- W width recalculated on request
      IF ( gammw .LE. 0d0 ) THEN
         gwc  =  9d0 * Gmu * amaw**2 /( 6d0 * sqrt(2d0) * pi)
         gammw = amaw * gwc
         XPAR(7) = GAMMW  ! send it back !!!
      ENDIF
!-----------------------------------------------------------------------

c.. wtmax for rejections
!      if(wtmax.le.0d0) then
!! old!!!
!        WTMAX=6D0
!c        IF(CMSENE.GT.140) WTMAX=5.5D0
!        IF(CMSENE.GT.160) WTMAX=4.4D0
!        IF(CMSENE.GT.170) WTMAX=3.6D0
!        IF(CMSENE.GT.250) WTMAX=4.6D0
!        IF(CMSENE.GT.350) WTMAX=7.0D0
!        IF(CMSENE.GT.700) WTMAX=9.0D0
!        XPAR(10) = WTMAX   ! send it back !!!
!      endif
      if(wtmax.le.0d0) then
        WTMAX=7D0
        IF(CMSENE.GT.162) WTMAX=5.0D0
        IF(CMSENE.GT.175) WTMAX=4.0D0
        IF(CMSENE.GT.200) WTMAX=4.4D0
        IF(CMSENE.GT.250) WTMAX=4.8D0
        IF(CMSENE.GT.350) WTMAX=7.0D0
        IF(CMSENE.GT.700) WTMAX=9.0D0
        XPAR(10) = WTMAX   ! send it back !!!
      endif

!--. decays
! Branching ratios for W decay channels:
! 1-ud, 2-cd, 3-us, 4-cs, 5-ub, 6-cb, 7-e, 8-mu, 9-tau
      IF(  KeyBra .EQ. 0 )THEN
         BR(1)=1/3d0            !  <== ud
         BR(2)=0                !  <== cd
         BR(3)=0                !  <== us
         BR(4)=1/3d0            !  <== cs
         BR(5)=0                !  <== ub
         BR(6)=0                !  <== cb
         BR(7)=1/9d0            !  <== e
         BR(8)=1/9d0            !  <== mu
         BR(9)=1/9d0            !  <== tau
      ELSE
         BR(1)=0.32110D0
         BR(2)=0.01630D0
         BR(3)=0.01635D0
         BR(4)=0.32043D0
         BR(5)=0.00002D0
         BR(6)=0.00070D0
         BR(7)=0.1084D0
         BR(8)=0.1084D0
         BR(9)=0.1083D0
      ENDIF

! final fermions masses
      IF(  KeyMas .EQ. 0 ) THEN
         AMAFIN(1)= 0d0
         AMAFIN(2)= 0d0
         AMAFIN(3)= 0d0
         AMAFIN(4)= 0d0
         AMAFIN(5)= 0d0
         
         AMAFIN(11)= 0d0
         AMAFIN(12)= 0d0
         AMAFIN(13)= 0d0
         AMAFIN(14)= 0d0
         AMAFIN(15)= 0d0
         AMAFIN(16)= 0d0
      ELSEIF(  KeyMas .EQ. 1 )THEN
         AMAFIN(1)=10D-3        ! d -1/3
         AMAFIN(2)= 5D-3        ! u +2/3
         AMAFIN(3)= .2D0        ! s -1/3
         AMAFIN(4)=1.3D0        ! c +2/3
         AMAFIN(5)=4.3D0        ! b -1/3
         
         AMAFIN(11)=.51099906D-3 ! e
         AMAFIN(12)=0D0         ! ve
         AMAFIN(13)=.105658389D0 ! mu
         AMAFIN(14)=0D0         ! vmu
         AMAFIN(15)=1.7771D0    ! tau
         AMAFIN(16)=0D0         ! vtau
      ELSE
         write(6,*) ' +++++++ wrong KeyMas= ', KeyMas
         STOP
      ENDIF

!
C      WRITE(6,   '(10X,A)') Logo
      WRITE(NOUT,'(10X,A)') Logo

      WRITE(NOUT,BXOPE)         
      WRITE(NOUT,BXTXT) '           KORALW input parameters used    '
      WRITE(NOUT,BXL1F) CMSENE,     'CMS energy total   ','CMSENE','I.0'
      WRITE(NOUT,BXTXT)'***********************************************'
      WRITE(NOUT,BXL1I) KeyRad,     'QED super-switch   ','KeyRad','IQ1'
      WRITE(NOUT,BXL1I) KeyISR,     'Init. state Rad.   ','KeyISR','IQ2'
      WRITE(NOUT,BXL1I) KeyFSR,     'Final state Rad.   ','KeyFSR','IQ3'
      WRITE(NOUT,BXL1I) KeyNLL,     'Next. To Leading   ','KeyNLL','IQ4'
      WRITE(NOUT,BXL1I) KeyCul,     'Coulomb, INACTIVE  ','KeyCul','IQ5'
      WRITE(NOUT,BXTXT)'***********************************************'
      WRITE(NOUT,BXL1I) KeyPhy,     'Physics super-switc','KeyPhy','IP1'
      WRITE(NOUT,BXL1I) KeyRed,     'FS mass reduction  ','KeyRed','IP2'
      WRITE(NOUT,BXL1I) KeySpn,     'Spin in W decays   ','KeySpn','IP3'
      WRITE(NOUT,BXL1I) KeyZet,     'Z propag.          ','KeyZet','IP4'
      WRITE(NOUT,BXL1I) KeyMas,     'Mass kinematics.   ','KeyMas','IP5'
      WRITE(NOUT,BXL1I) KeyBra,     'Branching Rat.     ','KeyBra','IP6'
      WRITE(NOUT,BXTXT)'***********************************************'
      WRITE(NOUT,BXL1I) KeyTek,     'Technical super-swi','KeyTek','IT1'
      WRITE(NOUT,BXL1I) KeyRnd,     'rand Numb type     ','KeyRnd','IT2'
      WRITE(NOUT,BXL1I) KeyWgt,     'weighting  switch  ','KeyWgt','IT3'
      WRITE(NOUT,BXTXT)'***********************************************'
      WRITE(NOUT,BXL1I) KeyMis,     'Miscelaneous       ','KeyMis','IM1'
      WRITE(NOUT,BXL1I) KeyMix,     'sinW2 input type   ','KeyMix','IM2'
      WRITE(NOUT,BXTXT)'***********************************************'
      WRITE(NOUT,BXL1F) GMU*1d5,    'G_mu * 1d5         ','GMU   ','I.1'
      WRITE(NOUT,BXL1F) ALFWIN,     'inv alpha_w        ','ALFWIN','I.2'
      WRITE(NOUT,BXL1F) AMAZ,       'Z mass   [GeV]     ','AMAZ  ','I.3'
      WRITE(NOUT,BXL1F) GAMMZ,      'Z width  [GeV]     ','GAMMZ ','I.4'
      WRITE(NOUT,BXL1F) AMAW,       'W mass   [GeV]     ','AMAW  ','I.5'
      WRITE(NOUT,BXL1F) GAMMW,      'W width  [GeV]     ','GAMMW ','I.6'
      WRITE(NOUT,BXL1F) VVMIN,      'dummy infrared cut ','VVMIN ','I.7'

      WRITE(NOUT,BXL1F) VVMAX,      'v_max ( =1 )       ','VVMAX ','I.8'
      WRITE(NOUT,BXL1I) KEYDWM,     'W- decay mode      ','KEYDWM','I.9'
      WRITE(NOUT,BXL1I) KEYDWP,     'W+ decay mode      ','KEYDWP','I10'
      WRITE(NOUT,BXL1F) WTMAX,      'max wt for rejectn.','WTMAX ','I11'
      WRITE(NOUT,BXTXT)'***********************************************'
      WRITE(NOUT,BXL1F) SINW2,      'sin(theta_W)**2    ','SINW2 ','I12'
      WRITE(NOUT,BXTXT)'***********************************************'
!-----------------------------------------------------------------------
! Feynman rules and their printout, LEP200 style
!-----------------------------------------------------------------------
      QE =-1
      VEI= 1-4*SINW2
      AEI= 1
      EEW = SQRT(4D0*PI*ALPHAW)
      GAE =-EEW/sqrt(16D0*SinW2*(1d0-SinW2))
      GVE = GAE*VEI
      GWF = EEW/(2D0*sqrt(2d0)*sqrt(SinW2))
      GWWG= EEW
      GWWZ= EEW * sqrt(1d0-SinW2) /sqrt(SinW2)
!-----------------------------------------------------------------------
      WRITE(NOUT,BXL1F) GVE,        'LEP200 workshop      ','GVE ','***'
      WRITE(NOUT,BXL1F) GAE,        'LEP200 workshop      ','GAE ','***'
      WRITE(NOUT,BXL1F) GWF,        'LEP200 workshop      ','GWF ','***'
      WRITE(NOUT,BXL1F) GWWG,       'LEP200 workshop      ','GWWG','***'
      WRITE(NOUT,BXL1F) GWWZ,       'LEP200 workshop      ','GWWZ','***'
      WRITE(NOUT,BXTXT)'***********************************************'
c>    WRITE(NOUT,BXTXT) '       sin(th_W) from G_mu, alpha_w and M_Z: '
c>    WRITE(NOUT,BXTXT) '        A2 = PI / ( ALFWIN*SQRT(2D0)*GMU )   '
c>    WRITE(NOUT,BXTXT) '     SINW2 = ( 1-SQRT( 1-(4*A2/AMAZ**2) ) )/2'
c>    WRITE(NOUT,BXL1F) SINW2,      'sin(theta_W)**2    ','SINW2 ','A6'
c>    WRITE(NOUT,BXTXT)'***********************************************'
      IF(keyzet.eq.0) THEN 
        WRITE(NOUT,BXTXT) '  Z width in Z propagator: s/M_Z *GAMM_Z '
      ELSEIF(keyzet.eq.1) THEN 
        WRITE(NOUT,BXTXT) '  Z width in Z propagator:   M_Z *GAMM_Z '
      ELSEIF(keyzet.eq.2) THEN 
        WRITE(NOUT,BXTXT) '  Z width in Z propagator:   0           '
      ELSE
        WRITE(NOUT,BXTXT) '  FILEXP ==> wrong KEYZET =',keyzet
        STOP
      ENDIF
      WRITE(NOUT,BXTXT)'***********************************************'
      IF(keyspn.ne.1) THEN 
        WRITE(NOUT,BXTXT) '         WARNING!  spin in decays is OFF: '
        WRITE(NOUT,BXL1I) KEYSPN, 'spin in decays switch','KEYSPN','A13'
      WRITE(NOUT,BXTXT)'***********************************************'
      ENDIF
      WRITE(NOUT,BXTXT) '                                             '
      WRITE(NOUT,BXTXT) '                                      DECAYS:'
      WRITE(NOUT,BXTXT) '                            branching ratios:'
      WRITE(NOUT,BXL1F) BR(1),            'ud','BR(1)','IB1'
      WRITE(NOUT,BXL1F) BR(2),            'cd','BR(2)','IB2'
      WRITE(NOUT,BXL1F) BR(3),            'us','BR(3)','IB3'
      WRITE(NOUT,BXL1F) BR(4),            'cs','BR(4)','IB4'
      WRITE(NOUT,BXL1F) BR(5),            'ub','BR(5)','IB5'
      WRITE(NOUT,BXL1F) BR(6),            'cb','BR(6)','IB6'
      WRITE(NOUT,BXL1F) BR(7),            ' e','BR(7)','IB7'
      WRITE(NOUT,BXL1F) BR(8),           ' mu','BR(8)','IB8'
      WRITE(NOUT,BXL1F) BR(9),          ' tau','BR(9)','IB9'
      WRITE(NOUT,BXTXT) '                                      masses:'
      WRITE(NOUT,BXL1F) AMAFIN(1),     ' d','AMAFIN(1)','IM1'
      WRITE(NOUT,BXL1F) AMAFIN(2),     ' u','AMAFIN(2)','IM2'
      WRITE(NOUT,BXL1F) AMAFIN(3),     ' s','AMAFIN(3)','IM3'
      WRITE(NOUT,BXL1F) AMAFIN(4),     ' c','AMAFIN(4)','IM4'
      WRITE(NOUT,BXL1F) AMAFIN(5),     ' b','AMAFIN(5)','IM5'
      WRITE(NOUT,BXL1F) AMAFIN(11),    ' e','AMAFIN(11)','IM6'
      WRITE(NOUT,BXL1F) AMAFIN(12),    've','AMAFIN(12)','IM7'
      WRITE(NOUT,BXL1F) AMAFIN(13),    'mu','AMAFIN(13)','IM8'
      WRITE(NOUT,BXL1F) AMAFIN(14),   'vmu','AMAFIN(14)','IM9'
      WRITE(NOUT,BXL1F) AMAFIN(15),   'tau','AMAFIN(15)','IM10'
      WRITE(NOUT,BXL1F) AMAFIN(16),  'vtau','AMAFIN(16)','IM11'
      WRITE(NOUT,BXTXT) '                                             '
      WRITE(NOUT,BXTXT) '                              DECAY LIBRARIES'
      WRITE(NOUT,BXL1I) JAK1,         'TAUOLA for W+' ,'JAK1','IL1'
      WRITE(NOUT,BXL1I) JAK2,         'TAUOLA for W-' ,'JAK2','IL2'
      WRITE(NOUT,BXL1I) ITDKRC,   'TAUOLA Ord(alpha)' ,'ITDKRC','IL3'
      WRITE(NOUT,BXL1I) IFPHOT,              'PHOTOS' ,'IFPHOT','IL4'
      WRITE(NOUT,BXL1I) IFHADM,       'JETSET for W-' ,'IFHADM','IL5'
      WRITE(NOUT,BXL1I) IFHADP,       'JETSET for W+' ,'IFHADP','IL6'
      WRITE(NOUT,BXCLO)         


      END       


      SUBROUTINE gifyfs(svar,amel,fyfs)       
C     *********************************       
C YFS formfactor       
C     *********************************       
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      COMMON / MATPAR / PI,CEULER     
      COMMON / PHYPAR / ALFINV,GPICOB     
! This common can be everywhere, contains various switches
      COMMON / KeyKey/  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
      SAVE

      KeyNLL = MOD(KeyRad,1000)/100

      alf1  =  1d0/alfinv/pi

      bilg  =  dlog(svar/amel**2)             
      beta  =  2*alf1*(bilg-1)
      IF(KeyNLL .EQ. 0) THEN
         delb  =  beta/4d0
      ELSEIF( KeyNLL .EQ. 1) THEN
         delb  =  beta/4d0 + alf1*( -.5d0  +pi**2/3d0)
      ELSE
         WRITE(6,*) '+++++ STOP in gifyfs, wrong KeyNLL= ',KeyNLL
      ENDIF
      fyfs  =  exp(delb)                  
      END              

      FUNCTION RHOSKO(R)                    
C     ********************                  
C CALLED IN VESK1W        
C PROVIDES V OR K DISTRIBUTION TO BE GENERATED                
C     ********************                  
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)   
      PARAMETER (FLEPS = 1D-35)
      COMMON / MATPAR / PI,CEULER     
      COMMON / PHYPAR / ALFINV,GPICOB     
      COMMON / WEKING / ENE,AMAZ,GAMMZ,AMEL,AMFIN,XK0,SINW2,IDE,IDF
      COMMON / WEKIN2 / AMAW,GAMMW,GMU,ALPHAW   
      COMMON / VVREC  / VVMIN,VVMAX,VV,BETI                   
      SAVE
C MAPPING  R => VV CHANGE  TO IMPROVE ON EFFICIENCY
C Note that the replacement below makes program more precise
C and bulet-proof with respect numerical instabilities close to VV=0    
      ALF1   = 1D0/PI/ALFINV
      SVAR   = 4D0*ENE**2
      BILG   = DLOG(SVAR/AMEL**2)           
      BETI   = 2D0*ALF1*(BILG-1D0)          
      X = MAX(R,FLEPS**BETI)                
      BBT = -0.5D0 
cc      write(6,*) amaw,gammw
      CALL CHBIN1(X,BETI,BBT,VVMAX,VV,RJAC)               
C BORN XSECTION           
      SVAR1  = SVAR*(1D0-VV)                 
C S1-S2 INTEGRAL
      CALL RESMS2(1,SVAR,SVAR1,AMAW,GAMMW,S1DUM,S2DUM,SSNORM,SSCRU)
      XBORN  = BORNSC(SVAR1,2)         
cc      write(6,*)'rhosko',ssnorm,xborn
      DILAT=1D0           
      IF(VV.GT.VVMIN) DILAT=(1D0+1D0/SQRT(1D0-VV))/2D0        
      BETI2  = 2D0*ALF1*BILG                
      DAMEL=1D0           
      IF(VV.GT.VVMIN) DAMEL=BETI2/BETI*(VV/VVMIN)**(BETI2-BETI)
      DISTR= BETI*VV**(BETI-1D0)*DILAT*DAMEL       
      RHOSKO = RJAC*XBORN*SSNORM*DISTR
c      RHOSKO = RJAC*VVRHO(1,SVAR,AMEL,VV,VVMIN)
      END                 

      SUBROUTINE YFSGEN(VV,VMIN,NMAX,WT1,WT2,WT3)               
C     *******************************************               
C======================================================================
C================== Y F S G E N =======================================
C======================================================================
C*********INPUT                 
C VV    = V VARIABLE            
C VMIN  = MINIMUM V VARIABLE (INFRARED CUTOFF)  
C NMAX  = MAXIMUM PHOTON MULTIPLICITY           
C*********OUTPUT                
C WT1  = WEIGHT DUE TO NEGLECTED MASS TERMS     
C WT2  = WEIGHT DUE TO DILATATION OF PHOTON MOMENTA             
C WT3  = ANOTHER DILATATION WEIGHT              
C OTHER OUTPUT RESULTS IN /MOMSET/              
C*****************************                  
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)       
      COMMON / MATPAR / PI,CEULER     
      COMMON / PHYPAR / ALFINV,GPICOB     
      COMMON / WEKING / ENE,AMAZ,GAMMZ,AMEL,AMFIN,XK0,SINW2,IDE,IDF
      COMMON / WEKIN2 / AMAW,GAMMW,GMU,ALPHAW   
      COMMON / MOMSET / QEFF1(4),QEFF2(4),SPHUM(4),SPHOT(100,4),NPHOT 
! This common can be everywhere, contains various switches
      COMMON / KeyKey/  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp   
      SAVE / WEKING /,/ MOMSET /,/ KeyKey /,/ WEKIN2 /
      DIMENSION XPH(100),RR(100)                
      SAVE
C               
C HERE BETI2 MUST BE USED INSTEAD OF BETI (MASS TERM NEGLECTED) 
      BETI2 = 2D0/ALFINV/PI*DLOG(4D0*ENE**2/AMEL**2)            
      AM2=(AMEL/ENE)**2         
      DO 10 I=1,NMAX            
      XPH(I)=0D0                
      DO 10 J=1,4               
   10 SPHOT(I,J)=0D0            
      IF(VV.LE.VMIN) THEN       
C NO PHOTON ABOVE DETECTABILITY THRESHOLD       
         WT1=1D0                
         WT2=1D0                
         WT3=1D0                
         NPHOT=0                
      ELSE      
C ONE OR MORE PHOTONS, GENERATE PHOTON MULTIPLICITY             
C NPHOT = POISSON(WITH AVERAGE = AVERG) + 1     
         AVERG=BETI2*DLOG(VV/VMIN)              
  100    CALL POISSG(AVERG,NMAX,MULTP,RR)       
         NPHOT = MULTP+1        
C This is for tests of program at fixed multiplicity (for adv. users)
! switch off the fixed multiplicity by hand !!!!!!!!!!!!!!!
!         NPHFIX =  MOD(KEYBRM,10000)/1000       
         nphfix = 0
! switch off the fixed multiplicity by hand !!!!!!!!!!!!!!!
         IF(NPHFIX.NE.0.AND.NPHOT.NE.NPHFIX) GOTO 100           
         IF(NPHOT.EQ.1) THEN    
            XPH(1)=VV           
            CALL BREMUL(XPH,AM2,WT1)            
            DJAC0=(1D0+1D0/SQRT(1D0-VV))/2D0    
            WT2  = 1D0/DJAC0    
            WT3  = 1D0          
         ELSE                   
            XPH(1)=VV           
            DO 200 I=2,NPHOT    
  200       XPH(I)=VV*(VMIN/VV)**RR(I-1)        
            CALL BREMUL(XPH,AM2,WT1)            
            CALL RESOLH(VV,EXPY,DJAC)           
            DJAC0=(1D0+1D0/SQRT(1D0-VV))/2D0    
            WT2  = DJAC/DJAC0   
            WT3  = 1D0          
C SCALE DOWN PHOTON ENERGIES AND MOMENTA        
            DO 300 I=1,NPHOT    
            DO 300 K=1,4        
  300       SPHOT(I,K)=SPHOT(I,K)/EXPY          
C CHECK ON LOWER ENERGY CUT-OFF                 
            IF(SPHOT(NPHOT,4).LT.VMIN) WT3 =0D0                 
         ENDIF                  
      ENDIF     
C PHOTON MOMENTA IN GEV UNITS   
      DO 420 J=1,4              
  420 SPHUM(J)=0D0              
      DO 480 I=1,NPHOT          
      DO 480 J=1,4              
      SPHOT(I,J)=SPHOT(I,J)*ENE                 
  480 SPHUM(J)=SPHUM(J)+SPHOT(I,J)              

C DEFINE FERMION MOMENTA        
C..      CALL KINEKR    ! MOVED OUTSIDE YFSGEN           
      END       
      SUBROUTINE RESOLH(VV,EXPY,DJAC)           
C     *******************************           
C THIS SOLVES CONSTRAINT EQUATION ON PHOTON MOMENTA             
C ALSO CALCULATES CORRESPONDING JACOBIAN FACTOR                 
C INPUT:  VV    = COSTRAINT PARAMETER V         
C OUTPUT  EXPY  = RESCALING FACTOR - A SOLUTION OF THE EQUATION 
C         DJAC  = JACOBIAN FACTOR               
C     ************************                  
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)       
      DIMENSION PP(4),PK(4)     
      COMMON / MOMSET / QEFF1(4),QEFF2(4),SPHUM(4),SPHOT(100,4),NPHOT
      SAVE   / MOMSET /
      SAVE
C               
      DO 210 K=1,4              
      PK(K)=0D0                 
  210 PP(K)=0D0                 
      PP(4)=2D0                 
      DO 215 I=1,NPHOT          
      DO 215 K=1,4              
  215 PK(K)=PK(K)+SPHOT(I,K)    
      PPDPP=PP(4)**2-PP(3)**2-PP(2)**2-PP(1)**2                 
      PKDPK=PK(4)**2-PK(3)**2-PK(2)**2-PK(1)**2                 
      PPDPK=PP(4)*PK(4)-PP(3)*PK(3)-PP(2)*PK(2)-PP(1)*PK(1)     
      AA=PPDPP*PKDPK/(PPDPK)**2                 
      EXPY=2D0*PPDPK/PPDPP/VV   
C SOLUTION FOR CONSTRAINT ON PHOTON FOUR MOMENTA                
      EXPY=EXPY*.5D0*(1D0+SQRT(1D0-VV*AA))      
C JACOBIAN FACTOR               
      DJAC=(1D0+1D0/SQRT(1D0-VV*AA))/2D0        
      END       
      SUBROUTINE BREMUL(XPH,AM2,WT)             
C     *****************************             
C PROVIDES PHOTON FOURMOMENTA   
C INPUT  : XPH    = LIST OF PHOTON ENERGIES     
C OUTPUT : SPHOT  = LIST OF PHPTON FOUR-MOMENTA                 
C          WT     = WEIGHT DUE TO MASS TERMS    
C     ************************                  
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)       
      COMMON / MATPAR / PI,CEULER     
      COMMON / MOMSET / QEFF1(4),QEFF2(4),SPHUM(4),SPHOT(100,4),NPHOT
      SAVE   / MOMSET /
      SAVE
      DIMENSION XPH(*),rn(1),rnumb(1)          

      WT=1D0    
      DO 100 I=1,NPHOT          
      XK=XPH(I)                 
      CALL VARRAN(RN,1)
      CALL ANGBRE(RN(1),AM2,CG,SG,DIST0,DIST1)     
      WTM   =DIST1/DIST0        
      WT    =WT*WTM             
      CALL VARRAN(RNUMB,1)
      PHI=2D0*PI*RNUMB(1)          
      SPHOT(I,1)=XK*SG*COS(PHI)                 
      SPHOT(I,2)=XK*SG*SIN(PHI)                 
      SPHOT(I,3)=XK*CG          
      SPHOT(I,4)=XK             
  100 CONTINUE                  
C======================================================================
C==================END OF YFSGEN=======================================
C======================================================================
      END       

      SUBROUTINE POISSG(AVERG,NMAX,MULT,RR)
C     ************************************** 
C Last corr. Nov. 91              
C This generates photon multipl. NPHOT according to Poisson distr. 
C INPUT:  AVERG = AVERAGE MULTIPLICITY   
C         NMAX  = MAXIMUM MULTIPLICITY   
C OUTPUT: MULT = GENERATED MULTIPLICITY 
C         RR(1:100) LIST OF ORDERED UNIFORM RANDOM NUMBERS, 
C         A BYPRODUCT RESULT, TO BE EVENTUALLY USED FOR SOME FURTHER
C         PURPOSE (I.E.  GENERATION OF PHOTON ENERGIES). 
C     ************************           
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION RR(*),rn(1)                    
      COMMON / INOUT  / NINP,NOUT  
      SAVE   / INOUT  /
      SAVE
      DATA NFAIL/0/                      
   50 NN=0                               
      SUM=0D0                            
      DO 100 IT=1,NMAX                   
      CALL VARRAN(RN,1)
      Y= LOG(RN(1))                         
      SUM=SUM+Y                          
      NN=NN+1                            
      RR(NN)=SUM/(-AVERG)                
      IF(SUM.LT.-AVERG) GOTO 130         
  100 CONTINUE                           
      NFAIL=NFAIL+1                      
      IF(NFAIL.GT.100) GOTO 900          
      GOTO 50                            
  130 MULT=NN-1                         
      RETURN                             
  900 WRITE(NOUT,*) ' POISSG: TO SMALL NMAX'
      STOP                               
      END                                

      SUBROUTINE ANGBRE(RN1,AM2,COSTHG,SINTHG,DIST0,DIST1)
C     **************************************************** 
C THIS ROUTINE GENERATES PHOTON ANGULAR DISTRIBUTION 
C IN THE REST FRAME OF THE FERMION PAIR. 
C THE DISTRIBUTION IS TAKEN IN THE INFRARED LIMIT.
C GENERATES WEIGHTED EVENTS              
C INPUT:  AM2 = 4*MASSF**2/S WHERE MASSF IS FERMION MASS
C         AND S IS FERMION PAIR EFFECTIVE MASS.
C OUTPUT: COSTHG, SINTHG, COS AND SIN OF THE PHOTON 
C         ANGLE WITH RESPECT TO FERMIONS DIRECTION 
C         DIST0 = distribution  generated without m**2/(kp)**2 terms
C         DIST1 = distribution  with m**2/(kp)**2 terms 
C     *************************************** 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION RN2(1)
      SAVE
      BETA=SQRT(1.D0-AM2)                
      EPS=AM2/(1.D0+SQRT(1.D0-AM2))      
      DEL1=(2.D0-EPS)*(EPS/(2.D0-EPS))**RN1 
      DEL2=2.D0-DEL1                     
C SYMMETRIZATION                         
      CALL VARRAN(RN2,1)
      IF(RN2(1).LE.0.5D0) THEN              
        A=DEL1                           
        DEL1=DEL2                        
        DEL2=A                           
      ENDIF                              
      DIST0=1D0/DEL1/DEL2                
      DIST1=DIST0-EPS/2.D0*(1D0/DEL1**2+1D0/DEL2**2)
C CALCULATION OF SIN AND COS THETA FROM INTERNAL VARIABLES 
      COSTHG=(1.D0-DEL1)/BETA            
      SINTHG=SQRT(DEL1*DEL2-AM2)/BETA    
      END                                

      FUNCTION BREMKF(KEY,EREL)                  
C     *************************         
C NON-MONTECARLO INTEGRATION OF THE V-DISTRIBUTION            
C GAUSS METHOD, CHANGE OF VARIABLES WITH HELP OF CHBIN1       
C SEE VVDISB              
C KEY= 1,2,3,...FOR VARIOUS DISTRIBUTIONS   
C KEY= 3 FOR MC GENERATION, OTHER FOR TESTS                   
C FOR KEYFIX=1, EXEPTIONALLY, IT PROVIDES INTEGRAND AT VV=VVMAX 
C WITH BORN OMITTED       
C     ************************              
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)   
! This common can be everywhere, contains various switches
      COMMON / KeyKey/  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp  
C COMMON KEYDST COMMUNICATES ONLY WITH VVDISB - INTEGRAND FUNCTION 
      COMMON / KEYDST / KEYDIS              
      COMMON / WEKING / ENE,AMAZ,GAMMZ,AMEL,AMFIN,XK0,SINW2,IDE,IDF 
      COMMON / VVREC  / VVMIN,VVMAX,VV,BETI                   
      SAVE / KeyKey /,/ KEYDST /,/ WEKING /,/ VVREC  /
      SAVE
      EXTERNAL VVDISB     
C       
      KEYDIS=KEY
CBB define KEYFIX !!!!!!!!
      KEYFIX = 0          
      IF(KEYFIX.EQ.0) THEN                  
         XBORN  =BORNSC(4D0*ENE**2,2)          
         PREC=  XBORN*EREL                  
cc      write(6,*)xborn,prec
         XA= 0D0          
         XB= 1D0
cc         CALL GAUSJD(VVDISB,XA,XB,PREC,RESULT) ! switched to ADAPTIVE etc
         result =GAUS(VVDISB,XA,XB,PREC)
cc      call DGADAP(XA,XB,VVDISB,PREC,RESULT)
         BREMKF=RESULT          
      ELSE                
         SVAR  = 4D0*ENE**2
         BREMKF= VVRHO(KEYDIS,SVAR,AMEL,VVMAX,VVMIN)
     $          /VVRHO(     9,SVAR,AMEL,VVMAX,VVMIN)          
      ENDIF               
      END                 
      FUNCTION VVDISB(R)                    
C     ******************                    
C INTEGRAND FOR BREMKF    
C MAPPING XX => VV CHANGE  TO IMPROVE ON EFFICIENCY           
C     ************************              
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)   
      PARAMETER( FLEPS =1D-35)              
      COMMON / WEKING / ENE,AMAZ,GAMMZ,AMEL,AMFIN,XK0,SINW2,IDE,IDF
      COMMON / WEKIN2 / AMAW,GAMMW,GMU,ALPHAW   
      COMMON / VVREC  / VVMIN,VVMAX,VV,BETI                   
      COMMON / KEYDST / KEYDIS              
      SAVE / WEKING /,/ VVREC  /,/ KEYDST /
      SAVE
C       
      KEYD=KEYDIS        
      X = MAX(R,FLEPS**BETI)                
      ALF=  BETI          
      BET=  1D0           
C ...SPECIAL CASES        
C ...Monte Carlo crude distr                
      IF    (KEYD.EQ.1)  THEN               
        BET=  -0.5D0      
C ...YFS exponentiation beta0,1,2 contribs  
      ELSEIF(KEYD.EQ.310)  THEN              
        ALF=  BETI        
      ELSEIF(KEYD.EQ.311)  THEN             
        ALF=  BETI +1     
      ELSEIF(KEYD.EQ.320)  THEN              
        ALF=  BETI        
      ELSEIF(KEYD.EQ.321)  THEN             
        ALF=  BETI +1     
      ELSEIF(KEYD.EQ.322)  THEN             
        ALF=  BETI +2     
C ...Reference distr including dilatation factor DAMEL        
      ELSEIF(KEYD.EQ.12) THEN               
        BET=  -0.5        
      ENDIF               
      CALL CHBIN1(X,ALF,BET,VVMAX,VV,RJAC) 
C BORN XSECTION           
      SVAR   = 4D0*ENE**2
      SVAR1  = SVAR*(1D0-VV)                 
      XBORN  = BORNSC(SVAR1,2)  
cc      write(6,*)'vvdisb',alf,bet,svar,svar1,vv,ene,vvmax       
C.. S1 S2               
      CALL RESMS2(1,SVAR,SVAR1,AMAW,GAMMW,S1DUM,S2DUM,BS12,SSCRU)
      VVDISB = VVRHO(KEYD,SVAR,AMEL,VV,VVMIN) *RJAC*XBORN *BS12         
      END                 
      FUNCTION VVRHO(KEYDIS,SVAR,AMEL,VV,VVMIN) 
C     *****************************************
C-------------------------------------------------------------
C Convention for KEYDIS      
C     KEYDIS   =  1      crude distribution for initial state MC
C     KEYDIS   =  9      reference distr.  of YFS2 CPC paper 
C     KEYDIS   =  50-52  obsolete test distr. for YFS2 CPC paper 
C-------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON / MATPAR / PI,CEULER     
      COMMON / PHYPAR / ALFINV,GPICOB     
      COMMON / INOUT  / NINP,NOUT     
      SAVE   / INOUT  /
      SAVE
C           
      ALF1   = 1D0/PI/ALFINV
      KEYD = KEYDIS           
      BILG   = DLOG(SVAR/AMEL**2)           
      BETI   = 2D0*ALF1*(BILG-1D0)          
C===================================================================
C ---------------------- KEYD = 1 ----------------------------------
C ---- Crude distribution in YFS2 initial state Monte Carlo --------
C ------------------------------------------------------------------
c dilat is related to dilatation jacobian in yfsgen                
c damel is responsible for modification of photon ang. distribution
c see also weight wt=wt1 in   angbre                               
      IF(KEYD.GE.1.AND.KEYD.LT.100) THEN
         DILAT=1D0           
         IF(VV.GT.VVMIN) DILAT=(1D0+1D0/SQRT(1D0-VV))/2D0        
         BETI2  = 2D0*ALF1*BILG                
         DAMEL=1D0           
         IF(VV.GT.VVMIN) DAMEL=BETI2/BETI*(VV/VVMIN)**(BETI2-BETI)
C---------
         IF    (KEYD.EQ.1)  THEN               
            DISTR= BETI*VV**(BETI-1D0)*DILAT*DAMEL       
C ...Reference distribution used in YFS2 paper --------------------
         ELSEIF(KEYD.EQ. 9)  THEN   
            DISTR= BETI*VV**(BETI-1D0)*(1+(1-VV)**2)/2               
C basic reference distribution  xrefer=sigma-ref                
         ELSEIF(KEYD.EQ.50) THEN   
            DISTR= BETI*VV**(BETI-1D0)             
C XREFER TIMES DAMEL            
         ELSEIF(KEYD.EQ.51) THEN   
            DISTR= BETI*VV**(BETI-1D0)*DAMEL       
C XREFER TIMES DILATATION FACTOR DILAT          
         ELSEIF(KEYD.EQ.52) THEN   
            DISTR= BETI*VV**(BETI-1D0)*DILAT
         ENDIF       
      ELSE       
         GOTO 900             
      ENDIF      
      VVRHO = DISTR                
      RETURN    
 900  WRITE(6,*) ' ===--->  WRONG KEYDIS IN VVRHO',KEYD 
      STOP       
      END        


      SUBROUTINE decay(mode)
*     ****************************************
c generates type of final states
c mode =-1 : initiates icod, br, amafin
c        0 : generation
c OUTPUT   (in common DECAYS)
c          iflav(4) PDG numbers of decay prods (m,\bar m,p,\bar p)
c          amdec(4) masses of the above
c          brrat(2) br. ratios of the above (m,p) 
c          brel     br. ratio to (e ne) channel
c
c Written by: M.Skrzypek       date: 3/2/95
c Last update:                   by: 
c
      IMPLICIT REAL*8 (a-h,o-z)
! This common can be everywhere, contains various switches
      COMMON / KeyKey/  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp  
      COMMON / decdat / amafin(20), br(20), icod(20)
! Codes, br ratios and masses for final particles 
      COMMON / decays / iflav(4), amdec(4), brrat(2), brel
      SAVE / decays /,/ keykey /
      SAVE


      IF(mode.eq.-1) THEN
! Codes for final state flavors
        icod(1)=102
        icod(2)=104
        icod(3)=302
        icod(4)=304
        icod(5)=502
        icod(6)=504
        icod(7)=1112
        icod(8)=1314
        icod(9)=1516

        brel=br(7)
        if (keydwm.ne.0) brrat(1)=br(keydwm)
        if (keydwp.ne.0) brrat(2)=br(keydwp)

      ELSEIF(mode.eq.0) THEN
! W-decay channels if chosen by the user
        icwm=keydwm
        icwp=keydwp      
! If keydwm[p]=0 decay channel is generated according to br. ratio.
        if (keydwm.eq.0) icwm=icwdec(20,br)
        if (keydwp.eq.0) icwp=icwdec(20,br)
! Assigning codes to final particles according to PDG
! coding convention (1992)
        iflav(1)= icod(icwm)/100
        iflav(2)=-mod(icod(icwm),100)
        iflav(3)= mod(icod(icwp),100)
        iflav(4)=-icod(icwp)/100
        amdec(1) = amafin( iflav(1))
        amdec(2) = amafin(-iflav(2))
        amdec(3) = amafin( iflav(3))
        amdec(4) = amafin(-iflav(4))
        brrat(1)=br(icwm)
        brrat(2)=br(icwp)
      ENDIF
      END


      FUNCTION icwdec(icmax,br)
*     *************************
c This function provides a number of a decay channel 
c according to the appriopriate branching ratio value.
c Note: Sum over all br(ic) should be equal to 1.
c INPUT: icmax     - max. number of decay channels
c        br(icmax) - array containing branching ratio values
c                    for decay channels
c
c Written by: Wieslaw Placzek            date: 20.07.1994
c Last update: 22.07.1994                by: W.P.
c
      IMPLICIT real*8 (a-h,o-z)
      REAL*8 br(icmax),drvec(100)
      SAVE
!
      CALL varran(drvec,1)
      rr=drvec(1)
      sum=0
      DO 10 i=1,icmax
        ic=i
        sum=sum+br(i)
        if (sum.ge.rr) goto 11
 10   continue
 11   icwdec=ic
      END
 

      subroutine kineww(s,sprim,ct,fi,ct1,fi1,ct2,fi2,
     $                  amwm,amwp,amdec,  q1,q2,p1,p2,p3,p4)
*     **********************************************************
c This routine calculates kinematics for W-W+ pair production
c and decay in e+e- collision in the CMS with z-axis pointing 
c in the e- direction.
c fixes also the 'effective beams', qeff1,qeff2
c INPUT:  s    - beams energy squared (in GeV**2)
c         sprim - actual center mass energy squared (in GeV**2)
c         cthe,fi - W-  production angles 
c         cdec1,fi1 - W- decay products angles
c         cdec2,fi2 - W+ decay products angles
c         amwm, amwp - masses of W- and W+ resonances
c         amdec(4) - decay products masses
c OUTPUT:
c         qeff1(4)      -effective (massless) e- beam in /MOMSET/
c         qeff2(4)      -effective (massless) e+ beam in /MOMSET/
c         q1(4)        - four-momentum of W-  
c         q2(4)        - four-momentum of W+
c         p1(4), p2(4) - four-momenta of W- decay products
c         p3(4), p4(4) - four-momenta of W+ decay products
c
c Written by: Wieslaw Placzek            date: 22.07.1994
c Rewritten by: M. Skrzypek              date: 3/15/95
c Last update: 4/1/95                by: M.S.
c
      implicit real*8 (a-h,o-z)
      common / matpar / pi,ceuler     
      common / momset / qeff1(4),qeff2(4),sphum(4),sphot(100,4),nphot  
c.. four momenta in CMS' (effective e- beam along z+, exclusively for WWBORN)
      common / bormom / bq1(4),bq2(4),bp1(4),bp2(4),bp3(4),bp4(4)
      save   / momset /,/ bormom /
      save
      real*8 qsu(4),ef1(4),ef2(4),photd(4),sphotd(100,4)
      dimension amdec(4),  q1(4),q2(4),p1(4),p2(4),p3(4),p4(4)
!
c to activate dumps KINDMP=1
      KINDMP=0

      do 111 i=1,4
      q1(i)=0d0      
      q2(i)=0d0      
      p1(i)=0d0      
      p2(i)=0d0      
      p3(i)=0d0      
      p4(i)=0d0      
      ef1(i)=0d0      
      ef2(i)=0d0      
      qeff1(i)=0d0      
 111  qeff2(i)=0d0      

      ecm=sqrt(sprim)
      amwm2=amwm**2
      amwp2=amwp**2
      s1=amwm2
      s2=amwp2
      amp1s=amdec(1)**2
      amp2s=amdec(2)**2
      amp3s=amdec(3)**2
      amp4s=amdec(4)**2
      IF(KINDMP.EQ.1)THEN
        WRITE(6,*) AMDEC(1),AMDEC(2),AMDEC(3),AMDEC(4)
        WRITE(6,*) AMP1S,AMP2S,AMP3S,AMP4S
        WRITE(6,*)'S,SPRIM,S1,S2',S,SPRIM,S1,S2
        WRITE(6,*)'DECAY COS1,2',CT1,CT2
      ENDIF
      st =sqrt((1-ct )*(1+ct ))
c..
      st1=sqrt((1-ct1)*(1+ct1))
      st2=sqrt((1-ct2)*(1+ct2))
c..
!... Momentum q1 of the first resonance
      q1(4)=(sprim+amwm2-amwp2)/(2d0*ecm)
      qq=dsqrt( (q1(4)-amwm)*(q1(4)+amwm) )
      q1(1)=qq*st*cos(fi)
      q1(2)=qq*st*sin(fi)
      q1(3)=qq*ct
      q1(4)=dsqrt(amwm2+q1(1)**2+q1(2)**2+q1(3)**2)
!... Momentum p1 in the rest frame of the first resonance
      ppene=(s1+amp1s-amp2s)/(2d0*amwm)
      ppe=dsqrt( (ppene-amdec(1))*(ppene+amdec(1)) )
      p1(1)=ppe*st1*cos(fi1)
      p1(2)=ppe*st1*sin(fi1)
      p1(3)=ppe*ct1
c      p1(4)=ppene
      p1(4)=dsqrt(amdec(1)**2+p1(1)**2+p1(2)**2+p1(3)**2)
c...
      IF(KINDMP.EQ.1)THEN
        WRITE(6,*)'Q1,P1 '
        CALL DUMPL(6,Q1,Q2,P1,P2,P3,P4,QEFF1,QEFF2,SPHOT,nphot)
      ENDIF
!... Boost to CMS-WW frame
      call boostv(-1,q1,p1,p1)
!... Momentum p2 of the second product of first resonance decay
      do 10 k=1,4
 10   p2(k)=q1(k)-p1(k)
c.. fine tuning on masses
      p1(4)=dsqrt(amdec(1)**2+p1(1)**2+p1(2)**2+p1(3)**2)
      p2(4)=dsqrt(amdec(2)**2+p2(1)**2+p2(2)**2+p2(3)**2)
      IF(KINDMP.EQ.1)THEN
        WRITE(6,*)'Q1,P1,P2 '
        CALL DUMPL(6,Q1,Q2,P1,P2,P3,P4,QEFF1,QEFF2,SPHOT,nphot)
      ENDIF
!... Momentum q2 of the second resonance
      q2(4)=ecm-q1(4)
      do 20 k=1,3
 20   q2(k)=-q1(k)
!... Momentum p3 in the rest frame of the second resonance
      ppene=(s2+amp3s-amp4s)/(2d0*amwp)
      ppe=dsqrt( (ppene-amdec(3))*(ppene+amdec(3)) )
      p3(1)=ppe*st2*cos(fi2)
      p3(2)=ppe*st2*sin(fi2)
      p3(3)=ppe*ct2
c      p3(4)=ppene
      p3(4)=dsqrt(amdec(3)**2+p3(1)**2+p3(2)**2+p3(3)**2)
c...
      IF(KINDMP.EQ.1)THEN
        WRITE(6,*)'Q1,P1,P2,Q2,P3 '
        CALL DUMPL(6,Q1,Q2,P1,P2,P3,P4,QEFF1,QEFF2,SPHOT,nphot)
      ENDIF
!... Boost to CMS-WW frame
      call boostv(-1,q2,p3,p3)
!... Momentum p2 of the second product of first resonance decay
      do 30 k=1,4
 30   p4(k)=q2(k)-p3(k)
c.. fine tuning on masses
      p4(4)=dsqrt(amdec(4)**2+p4(1)**2+p4(2)**2+p4(3)**2)
      p3(4)=dsqrt(amdec(3)**2+p3(1)**2+p3(2)**2+p3(3)**2)
      IF(KINDMP.EQ.1)THEN
        WRITE(6,*)'Q1,P1,P2,Q2,P3,P4, WW frame '
        CALL DUMPL(6,Q1,Q2,P1,P2,P3,P4,QEFF1,QEFF2,SPHOT,nphot)
      ENDIF
      if(nphot.eq.0) then       
c     ===================
c define effective beams (massless) in CMS''
        qeff1(4)= sqrt(s)/2d0
        qeff1(3)= qeff1(4)
        qeff1(2)= 0d0
        qeff1(1)= 0d0
        qeff2(4)= sqrt(s)/2d0
        qeff2(3)=-qeff2(4)
        qeff2(2)= 0d0
        qeff2(1)= 0d0
      IF(KINDMP.EQ.1)THEN
        WRITE(6,*)'LAB NO PHOTS'
        CALL DUMPL(6,Q1,Q2,P1,P2,P3,P4,QEFF1,QEFF2,SPHOT,nphot)
      ENDIF
c.. 4momenta for born, in effective CMS, z+ along qeff1
      do 124, i=1,4
      bq1(i)=q1(i)
      bq2(i)=q2(i)
      bp1(i)=p1(i)
      bp2(i)=p2(i)
      bp3(i)=p3(i)
 124  bp4(i)=p4(i)
      IF(KINDMP.EQ.1)THEN
        write(6,*)'born dumps',nphot
        call dumpb(6)
        call dumpl(6,Q1,Q2,P1,P2,P3,P4,QEFF1,QEFF2,SPHOT,nphot)
      ENDIF
      elseif(nphot.ge.1) then       
c     =======================
c effective beams (in LAB)
        ef1(4)= sqrt(s)/2d0
        ef1(3)= ef1(4)
        ef1(2)= 0d0
        ef1(1)= 0d0
        ef2(4)= sqrt(s)/2d0
        ef2(3)=-ef2(4)
        ef2(2)= 0d0
        ef2(1)= 0d0
        do 11 ii=1,nphot
        if(sphot(ii,3).ge.0d0) then
          do 12 jj=1,4
 12       ef1(jj)= ef1(jj)-sphot(ii,jj)
        else
          do 13 jj=1,4
 13       ef2(jj)= ef2(jj)-sphot(ii,jj)
        endif
 11     continue
       
      IF(KINDMP.EQ.1)THEN
        WRITE(6,*)'LAB0, ef1,ef2'
        CALL DUMPL(6,Q1,Q2,P1,P2,P3,P4,EF1,EF2,SPHOT,nphot)
      ENDIF
c qsu is fourmomentum of ww pair in cms              
        do 110 k=1,4               
  110 qsu(k)=-sphum(k)          
        qsu(4)=qsu(4)+sqrt(s)     
c transform ef1,2 to rest frame (CMS') 
        call boostv(1,qsu,ef1,ef1) 
        call boostv(1,qsu,ef2,ef2) 
      IF(KINDMP.EQ.1)THEN
        WRITE(6,*)'CMS EF1,2, PHOTS LAB'
        CALL DUMPL(6,Q1,Q2,P1,P2,P3,P4,EF1,EF2,SPHOT,NPHOT)
        DO 70 I=1,NPHOT
        DO 71 J=1,4
 71     PHOTD(J)=SPHOT(I,J)
C.. PHOTONS TO CMS'
        CALL boostv( 1,QSU,PHOTD,PHOTD) 
C.. PHOTONS TO CMS''
        CALL rotatv(1,EF1,PHOTD,PHOTD)
        DO 72 J=1,4
 72     SPHOTD(I,J)=PHOTD(J)
 70     CONTINUE
        WRITE(6,*)'CMS EF1,2 PHOTS CMSBIS'
        CALL DUMPL(6,Q1,Q2,P1,P2,P3,P4,EF1,EF2,SPHOTD,NPHOT)
C CONTROL
        CALL rotatv(-1,EF1,EF2,EF2)
        WRITE(6,*)'CONTROL'
        CALL DUMPL(6,Q1,Q2,P1,P2,P3,P4,EF1,EF2,SPHOT,NPHOT)
        CALL rotatv( 1,EF1,EF2,EF2)
        WRITE(6,*)'CONTROL'
        CALL DUMPL(6,Q1,Q2,P1,P2,P3,P4,EF1,EF2,SPHOT,NPHOT)
      ENDIF

c define effective beams (massless) in CMS''
      qeff1(4)= sqrt(sprim)/2d0
      qeff1(3)= qeff1(4)
      qeff1(2)= 0d0
      qeff1(1)= 0d0
      qeff2(4)= sqrt(sprim)/2d0
      qeff2(3)=-qeff2(4)
      qeff2(2)= 0d0
      qeff2(1)= 0d0

      IF(KINDMP.EQ.1)THEN
        WRITE(6,*)'CMSBIS, QEFF1,QEFF2'
        CALL DUMPL(6,Q1,Q2,P1,P2,P3,P4,QEFF1,QEFF2,SPHOTD,NPHOT)
      ENDIF
c.. 4momenta for born, in effective CMS, z+ along qeff1
      do 123, i=1,4
      bq1(i)=q1(i)
      bq2(i)=q2(i)
      bp1(i)=p1(i)
      bp2(i)=p2(i)
      bp3(i)=p3(i)
 123  bp4(i)=p4(i)
      IF(KINDMP.EQ.1)THEN
        WRITE(6,*)'BORN DUMPS',NPHOT
        CALL DUMPB(6)
      ENDIF

c rotate from CMS'' (Z along ef1) to CMS' (Z along e- beam)    
        call rotatv(-1,ef1,qeff1,qeff1)
        call rotatv(-1,ef1,qeff2,qeff2)
        call rotatv(-1,ef1,q1,q1)
        call rotatv(-1,ef1,q2,q2)
        call rotatv(-1,ef1,p1,p1)
        call rotatv(-1,ef1,p2,p2)
        call rotatv(-1,ef1,p3,p3)
        call rotatv(-1,ef1,p4,p4)
      IF(KINDMP.EQ.1)THEN
        WRITE(6,*)'CMSPRIM'
        CALL DUMPL(6,Q1,Q2,P1,P2,P3,P4,QEFF1,QEFF2,SPHOT,NPHOT)
      ENDIF
C transform back to LAB    
        call boostv(-1,qsu,qeff1,qeff1)              
        call boostv(-1,qsu,qeff2,qeff2)              
        call boostv(-1,qsu,q1,q1)              
        call boostv(-1,qsu,q2,q2)              
        call boostv(-1,qsu,p1,p1)              
        call boostv(-1,qsu,p2,p2)              
        call boostv(-1,qsu,p3,p3)              
        call boostv(-1,qsu,p4,p4)              
c.. fine tuning on masses
        p1(4)=dsqrt(amdec(1)**2+p1(1)**2+p1(2)**2+p1(3)**2)
        p2(4)=dsqrt(amdec(2)**2+p2(1)**2+p2(2)**2+p2(3)**2)
        p3(4)=dsqrt(amdec(3)**2+p3(1)**2+p3(2)**2+p3(3)**2)
        p4(4)=dsqrt(amdec(4)**2+p4(1)**2+p4(2)**2+p4(3)**2)
        qeff1(4)=dsqrt(qeff1(1)**2+qeff1(2)**2+qeff1(3)**2)
        qeff2(4)=dsqrt(qeff2(1)**2+qeff2(2)**2+qeff2(3)**2)
      IF(KINDMP.EQ.1)THEN
        WRITE(6,*)'LAB, QEFF'
        CALL DUMPL(6,Q1,Q2,P1,P2,P3,P4,QEFF1,QEFF2,SPHOT,NPHOT)
C TRANSFORM EF1 BACK TO LAB 
        CALL BOOSTV(-1,QSU,EF1,EF1) 
        CALL BOOSTV(-1,QSU,EF2,EF2) 
        WRITE(6,*)'LAB ALL, EF'
        CALL DUMPL(6,Q1,Q2,P1,P2,P3,P4,EF1,EF2,SPHOT,NPHOT)
      ENDIF
      else
c     ====
       write(6,*)'kineww==> wrong no of phots: ',nphot
      endif     
c     =====
      end


      subroutine boostv(idir,vv,pp,q)
*     *******************************
c Boost along arbitrary vector v (see eg. J.D. Jacson, "Classical 
c Electrodynamics).
c Four-vector pp is boosted from an actual frame to the rest frame 
c of the four-vector v (for idir=1) or back (for idir=-1). 
c q is a resulting four-vector.
c Note: v must be time-like, pp may be arbitrary.
c
c Written by: Wieslaw Placzek            date: 22.07.1994
c Last update: 3/29/95                     by: M.S.
c 
      implicit real*8 (a-h,o-z)
      parameter (nout=6)
      real*8 v(4),p(4),q(4),pp(4),vv(4)  
      save
!
      do 1 i=1,4
      v(i)=vv(i)
 1    p(i)=pp(i)
      amv=sqrt(v(4)**2-v(1)**2-v(2)**2-v(3)**2)
      if (idir.eq.-1) then
        q(4)=( p(1)*v(1)+p(2)*v(2)+p(3)*v(3)+p(4)*v(4))/amv
        wsp =(q(4)+p(4))/(v(4)+amv)
      elseif (idir.eq.1) then
        q(4)=(-p(1)*v(1)-p(2)*v(2)-p(3)*v(3)+p(4)*v(4))/amv
        wsp =-(q(4)+p(4))/(v(4)+amv)
      else
        write(nout,*)' >>> boostv: wrong value of idir = ',idir
      endif
      q(1)=p(1)+wsp*v(1)
      q(2)=p(2)+wsp*v(2)
      q(3)=p(3)+wsp*v(3)
      end
             

      subroutine rotatv(mode,qq,pp,r)        
c     *******************************        
c rotation along arbitrary axis.
c pp rotated into r  from actual frame to frame with z-axis along qq  
c (mode = 1) or back (mode = -1).      
c Written by: M. Skrzypek           date: 04.1995
c Last update:                      by:
      implicit double precision (a-h,o-z) 
      dimension qq(4),pp(4),r(4),tt(4)   
      save
      the= asin(qq(1)/sqrt(qq(1)**2+qq(2)**2+qq(3)**2))
      phi= asin(qq(2)/sqrt(qq(2)**2+qq(3)**2))   
      if(mode.eq.-1)then
        call rxtod1(phi,pp,tt)
        call rxtod2(-the,tt,r)
      elseif(mode.eq. 1)then
        call rxtod2(the,pp,tt)
        call rxtod1(-phi,tt,r)
      else
        write(6,*)'rotatv==> wrong mode:',mode
      endif
      end
 
      FUNCTION ANGLE(P,Q)
*     ******************
*  ANGLE BETWEEN TWO 3-COMPONENTS OF FOUR-VECTORS
*     ******************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION P(4),Q(4)
      PQ=P(3)*Q(3)+P(2)*Q(2)+P(1)*Q(1)
      PP=P(3)*P(3)+P(2)*P(2)+P(1)*P(1)
      QQ=Q(3)*Q(3)+Q(2)*Q(2)+Q(1)*Q(1)
      ARG=PQ/SQRT(PP*QQ)
      IF(ARG.GT. 1D0) ARG= 1D0
      IF(ARG.LT.-1D0) ARG=-1D0
      ANGLE=ACOS(ARG)
      END

      function dmas2(p)
c     *******************
      implicit real*8 (a-h,o-z)
      real*8 p(4)
      d3m = dsqrt(p(3)**2 +p(2)**2 +p(1)**2)  
      dmas2= (p(4)-d3m)*(p(4)+d3m)
      end

      function dot(p,q)
c     *******************
      implicit real*8 (a-h,o-z)
      real*8 p(4),q(4)  
      dot= p(4)*q(4) -p(3)*q(3) -p(2)*q(2) -p(1)*q(1)
      end
      SUBROUTINE DUMPS(NOUT)                 
C     **********************                 
C THIS PRINTS OUT FOUR MOMENTA OF PHOTONS    
C ON UNIT NO. NOUT                           
C     **********************                 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
      COMMON / MOMSET / QEFF1(4),QEFF2(4),SPHUM(4),SPHOT(100,4),NPHOT  
      SAVE   / MOMSET /
      SAVE
      DIMENSION SUM(4)                       
      WRITE(NOUT,*) '=====================DUMPS===================='
      WRITE(NOUT,3100) 'QF1',(  qeff1(  K),K=1,4) 
      WRITE(NOUT,3100) 'QF2',(  qeff2(  K),K=1,4) 
      DO 100 I=1,NPHOT                       
  100 WRITE(NOUT,3100) 'PHO',(SPHOT(I,K),K=1,4) 
      DO 200 K=1,4                           
  200 SUM(K)=qeff1(K)+qeff2(K)                   
      DO 210 I=1,NPHOT                       
      DO 210 K=1,4                           
  210 SUM(K)=SUM(K)+SPHOT(I,K)               
      WRITE(NOUT,3100) 'SUM',(  SUM(  K),K=1,4) 
 3100 FORMAT(1X,A3,1X,5F18.14)               
      END                                    

      subroutine moms(nout)
c     *********************
      implicit double precision (a-h,o-z)    
      common / momset / qeff1(4),qeff2(4),sphum(4),sphot(100,4),nphot  
      common / momdec / q1(4),q2(4),p1(4),p2(4),p3(4),p4(4)
      common / decays / iflav(4), amdec(4), br(2), brel
      save   / momset /,/ momdec /,/ decays /
      save
      write(nout,*) qeff1(4),(qeff1(k),k=1,3),' 11'  
      write(nout,*) qeff2(4),(qeff2(k),k=1,3),'-11'
      write(nout,*) p1(4),(p1(k),k=1,3),iflav(1)  
      write(nout,*) p2(4),(p2(k),k=1,3),iflav(2)
      write(nout,*) p3(4),(p3(k),k=1,3),iflav(3)
      write(nout,*) p4(4),(p4(k),k=1,3),iflav(4)
      end

      subroutine dumpw(nout)     
*     **********************     
c Prints out four-momenta and flavours of inermediate and 
c final particles on output unit nout
c 
c Written by: Wieslaw Placzek        date: 26.07.1994
c Last update: 27.07.1994            by: W.P.  
c
      implicit real*8 (a-h,o-z)
      common / momset / QEFF1(4),QEFF2(4),sphum(4),sphot(100,4),nphot 
      common / momdec / q1(4),q2(4),p1(4),p2(4),p3(4),p4(4)
      common / decays / iflav(4), amdec(4), br(2), brel
      save   / momset /,/ momdec /,/ decays /
      real*8 sum(4)     
      iflwm=-24
      iflwp= 24
      iflph= 22
      write(nout,*) '=====================dumpw===================='
      write(nout,3200) ' p(1)',' p(2)',' p(3)',' p(4)',' pdg-code'
      do 100 i=1,nphot                       
  100 write(nout,3100) 'PHO',(sphot(i,k),k=1,4),iflph 
      write(nout,3100) ' W-',(q1(k),k=1,4),iflwm   
      write(nout,3100) ' W+',(q2(k),k=1,4),iflwp   
      write(nout,3100) ' p1',(p1(k),k=1,4),iflav(1)   
      write(nout,3100) ' p2',(p2(k),k=1,4),iflav(2)
      write(nout,3100) ' p3',(p3(k),k=1,4),iflav(3)   
      write(nout,3100) ' p4',(p4(k),k=1,4),iflav(4)
      do 101 k=1,4      
 101  sum(k)=p1(k)+p2(k)+p3(k)+p4(k)         
      do 210 i=1,nphot                       
      do 210 k=1,4                           
  210 sum(k)=sum(k)+sphot(i,k)               
      isfla=iflav(1)+iflav(2)+iflav(3)+iflav(4)
      write(nout,3100) 'sum',(sum(k),k=1,4),isfla 
      write(nout,*) '=============================================='  
 3100 format(1x,a3,1x,4f15.8,i7)   
c 3100 format(1x,a3,1x,5f18.14)               
 3200 format(5x,4a15,a10)
      end   
      subroutine dumpl(nout,Q1,Q2,P1,P2,P3,P4,QEFF1,QEFF2,SPHOT,nphot)     
*     ****************************************************************     
c Prints out four-momenta and flavours of inermediate and 
c final particles on output unit nout
c 
c Written by: M.Skrzypek        date: 17.03.95
c Last update:             by:  
c
      implicit real*8 (a-h,o-z)
      common / decays / iflav(4), amdec(4), br(2), brel
      save   / decays /
      real*8 sum(4),pho(4),sphot(100,4)     
      dimension q1(4),q2(4),p1(4),p2(4),p3(4),p4(4),qeff1(4),qeff2(4)

      p4mass=dmas2(p4)
      p3mass=dmas2(p3)
      p2mass=dmas2(p2)
      p1mass=dmas2(p1)
      q2mass=dmas2(q2)
      q1mass=dmas2(q1)
      e2mass=dmas2(qeff2)
      e1mass=dmas2(qeff1)

      q1mass=dsign(dsqrt(abs(q1mass)),q1mass)
      q2mass=dsign(dsqrt(abs(q2mass)),q2mass)
      p1mass=dsign(dsqrt(abs(p1mass)),p1mass)
      p2mass=dsign(dsqrt(abs(p2mass)),p2mass)
      p3mass=dsign(dsqrt(abs(p3mass)),p3mass)
      p4mass=dsign(dsqrt(abs(p4mass)),p4mass)
      e1mass=dsign(dsqrt(abs(e1mass)),e1mass)
      e2mass=dsign(dsqrt(abs(e2mass)),e2mass)


      write(nout,*) '=====================dumpl===================='
      write(nout,3200) ' p(1)',' p(2)',' p(3)',' p(4)','  sign*mass'

      do 100 i=1,nphot    
      do 110 k=1,4    
 110  pho(k)=sphot(i,k)
      amphot=dmas2(pho)
      phmass=dsign(dsqrt(abs(amphot)),amphot)
  100 write(nout,3100) 'PHO',(sphot(i,k),k=1,4),phmass
      write(nout,3100) ' W-',(q1(k),k=1,4),q1mass   
      write(nout,3100) ' W+',(q2(k),k=1,4),q2mass 
      write(nout,3100) ' p1',(p1(k),k=1,4),p1mass,iflav(1)  
      write(nout,3100) ' p2',(p2(k),k=1,4),p2mass,iflav(2)
      write(nout,3100) ' p3',(p3(k),k=1,4),p3mass,iflav(3)
      write(nout,3100) ' p4',(p4(k),k=1,4),p4mass,iflav(4)
      write(nout,3100) 'qf1',(qeff1(k),k=1,4),e1mass  
      write(nout,3100) 'qf2',(qeff2(k),k=1,4),e2mass
      do 101 k=1,4      
 101  sum(k)=p1(k)+p2(k)+p3(k)+p4(k)         
      do 210 i=1,nphot                       
      do 210 k=1,4                           
  210 sum(k)=sum(k)+sphot(i,k)               
      isfla=iflav(1)+iflav(2)+iflav(3)+iflav(4)
      e2mass=dmas2(sum)
      sumas=dsign(dsqrt(abs(e2mass)),e2mass)
      write(nout,3100) 'sum',(sum(k),k=1,4), sumas !,isfla 
      write(nout,*) '=============================================='  
 3100 format(1x,a3,5f21.15,i4)   
c 3100 format(1x,a3,1x,5f18.14)               
 3200 format(5x,4a22,a10)
      end   

      subroutine dumpb(nout)     
*     **********************     
c Prints out four-momenta and flavours of inermediate and 
c final particles on output unit nout
c 
c Written by: M.Skrzypek        date: 17.03.95
c Last update:             by:  
c
      implicit real*8 (a-h,o-z)
      common / momset / QEFF1(4),QEFF2(4),sphum(4),sphot(100,4),nphot
      common / bormom / q1(4),q2(4),p1(4),p2(4),p3(4),p4(4)
      common / decays / iflav(4), amdec(4), br(2), brel
      save   / momset /,/ decays /
      real*8 sum(4),pho(4)     

      p4mass=dmas2(p4)
      p3mass=dmas2(p3)
      p2mass=dmas2(p2)
      p1mass=dmas2(p1)
      q2mass=dmas2(q2)
      q1mass=dmas2(q1)
      e2mass=dmas2(qeff2)
      e1mass=dmas2(qeff1)

      q1mass=dsign(dsqrt(abs(q1mass)),q1mass)
      q2mass=dsign(dsqrt(abs(q2mass)),q2mass)
      p1mass=dsign(dsqrt(abs(p1mass)),p1mass)
      p2mass=dsign(dsqrt(abs(p2mass)),p2mass)
      p3mass=dsign(dsqrt(abs(p3mass)),p3mass)
      p4mass=dsign(dsqrt(abs(p4mass)),p4mass)
      e1mass=dsign(dsqrt(abs(e1mass)),e1mass)
      e2mass=dsign(dsqrt(abs(e2mass)),e2mass)


      write(nout,*) '=====================dump born================'
      write(nout,3200) ' p(1)',' p(2)',' p(3)',' p(4)','  sign*mass'

      do 100 i=1,nphot    
      do 110 k=1,4    
 110  pho(k)=sphot(i,k)
      amphot=dmas2(pho)
      phmass=dsign(dsqrt(abs(amphot)),amphot)
  100 write(nout,3100) 'PHO',(sphot(i,k),k=1,4),phmass
      write(nout,3100) 'bW-',(q1(k),k=1,4),q1mass   
      write(nout,3100) 'bW+',(q2(k),k=1,4),q2mass 
      write(nout,3100) 'bp1',(p1(k),k=1,4),p1mass  
      write(nout,3100) 'bp2',(p2(k),k=1,4),p2mass
      write(nout,3100) 'bp3',(p3(k),k=1,4),p3mass   
      write(nout,3100) 'bp4',(p4(k),k=1,4),p4mass
      write(nout,3100) 'ef1',(qeff1(k),k=1,4),e1mass  
      write(nout,3100) 'ef2',(qeff2(k),k=1,4),e2mass
      do 101 k=1,4      
 101  sum(k)=p1(k)+p2(k)+p3(k)+p4(k)         
      do 210 i=1,nphot                       
      do 210 k=1,4                           
  210 sum(k)=sum(k)+sphot(i,k)               
      isfla=iflav(1)+iflav(2)+iflav(3)+iflav(4)
      write(nout,3100) 'sum',(sum(k),k=1,4) !,isfla 
      write(nout,*) '=============================================='  
 3100 format(1x,a3,1x,5f22.15)   
c 3100 format(1x,a3,1x,5f18.14)               
 3200 format(5x,4a22,a10)
      end   
c......==================== BORN ==========================
      function wwborn(svar,sprim,s1,s2)
*     **************************************
c This function provides a value of a differential born cross section
c for W+W- pair production and decay in e+e- scattering. 
c INPUT: svar   - CMS energy squared (in GeV**2)
c        costhe - cosine of the W- polar angle in the CMS
c                 of the incoming e+e- with z-axis pointing 
c                 in the e- direction
c Written by: Wieslaw Placzek            date: 20.07.1994
c Last update: 27.07.1994                by: W.P.
c
      implicit real*8 (a-h,o-z)
      common / matpar / pi,ceuler     
      common / weking / ene,amaz,gammz,amel,amfin,xk0,sinw2,ide,idf 
      common / wekin2 / amaw,gammw,gmu,alphaw   
      common / bormom / bq1(4),bq2(4),bp1(4),bp2(4),bp3(4),bp4(4)
      common / angles / costhe,phi,cosde1,phi1,cosde2,phi2
      common / decays / iflav(4), amdec(4), br(2), brel
      save   / decays /,/ weking /,/ wekin2 /
! This common can be everywhere, contains various switches
      COMMON / KeyKey/  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
! polarization amplitudes for WW production for left and right 
! handed electrons
      complex*16 awwel(3,3),awwer(3,3)
! polarization amplitudes for W-decays
      complex*16 adwm(3),adwp(3)
! auxilliary
c      complex*16 amww(3,3)
      complex*16 amwwr, amwwl 
      dimension amdec0(4),dq1(4),dq2(4),dp1(4),dp2(4),dp3(4),dp4(4)
      save

      fak= (4*pi)**(-4)
      KeySpn = MOD(KeyPhy,10000)/1000
      KeyRed = MOD(KeyPhy,100000)/10000
!
c.. first make decay products massless
cc        write(6,*)'WWborn dumps before'
cc        call dumpb(6)
      if(KeyRed.eq.0) then
        do 11,i=1,4
 11     amdec0(i)=0d0
        call kineww(svar,sprim,costhe,phi,cosde1,phi1,cosde2,phi2,
     $          sqrt(s1),sqrt(s2),amdec0,   dq1,dq2,dp1,dp2,dp3,dp4)
      elseif(KeyRed.eq.1) then
c.. brute force, no 4 mom conserv.
        bp1mod=bp1(4)/dsqrt(bp1(1)**2+bp1(2)**2+bp1(3)**2)
        bp2mod=bp2(4)/dsqrt(bp2(1)**2+bp2(2)**2+bp2(3)**2)
        bp3mod=bp3(4)/dsqrt(bp3(1)**2+bp3(2)**2+bp3(3)**2)
        bp4mod=bp4(4)/dsqrt(bp4(1)**2+bp4(2)**2+bp4(3)**2)
        do 15,i=1,3
        bp1(i)=bp1(i)*bp1mod
        bp2(i)=bp2(i)*bp2mod
        bp3(i)=bp3(i)*bp3mod
 15     bp4(i)=bp4(i)*bp4mod
      else
        write(6,*)'wwborn==>wrong KEYMAS:',keymas
        stop
      endif
cc        write(6,*)'WWborn dumps after'
cc        call dumpb(6)
c++      write(6,*)'masses',dmas2(bp1),dmas2(bp2),dmas2(bp3),dmas2(bp4)
      wwborn=0
      wlambd=sprim**2+s1**2+s2**2-2*sprim*s1-2*sprim*s2-2*s1*s2
      tvar=-(sprim-s1-s2-dsqrt(wlambd)*costhe)/2
!... Amplitudes for WW production
      call wwprod(sprim,tvar,bq1,bq2,awwel,awwer) 
!... Amplitudes for W-decays
      call wdecay(bq1,bp1,bp2,adwm)  
      call wdecay(bq2,bp3,bp4,adwp) 

!... Polarization amplitudes for WW production and decay
      if(keyspn.eq.0)then
        xmatr=0
        do 10 l2=1,3
        do 10 l1=1,3
          amwwl=awwel(l1,l2)*adwm(l1)*adwp(l2) !ms
          amwwr=awwer(l1,l2)*adwm(l1)*adwp(l2) !ms
          xmatr=xmatr + amwwl*dconjg(amwwl) !ms
          xmatr=xmatr + amwwr*dconjg(amwwr) !ms
 10     continue
      elseif(keyspn.eq.1)then
        xmatr=0
        amwwl=(0,0)
        amwwr=(0,0)
        do 20 l2=1,3
        do 20 l1=1,3
          amwwl=amwwl+awwel(l1,l2)*adwm(l1)*adwp(l2) !ms
          amwwr=amwwr+awwer(l1,l2)*adwm(l1)*adwp(l2) !ms
 20     continue
        xmatr=xmatr + amwwl*dconjg(amwwl) !ms
        xmatr=xmatr + amwwr*dconjg(amwwr) !ms
      endif
!... Kinematical factors
      x1=s1/sprim
      x2=s2/sprim
      bmain=sqrt( (1-x1-x2)**2 - 4*x1*x2 )
      xwm1=amdec(1)**2/s1
      xwm2=amdec(2)**2/s1
      bwm=sqrt( (1-xwm1-xwm2)**2 - 4*xwm1*xwm2 )
      xwp1=amdec(3)**2/s2
      xwp2=amdec(4)**2/s2
      bwp=sqrt( (1-xwp1-xwp2)**2 - 4*xwp1*xwp2 )
      fkin=fak *bmain*bwp*bwm/sprim
      wwborn=fkin*xmatr 
      end



      subroutine wwprod(s,t,q1,q2,awwel,awwer)
*     ****************************************
c This routine calculates polarization amplitudes for the process
c e+e- --> W+W-, for on-shell W's in Born approximation. Calculation
c is done in the CMS of e+e- with z-axis pointing along e- direction. 
c It is based on the paper: 
c K. Kolodziej, M. Zralek, Phys. Rev. D43 (1991) 43;
c INPUT: s   - center mass energy squared (in GeV**2)
c        t   - transfer (in GeV**2)
c        q1(4) - four-momentum of W-  
c        q2(4) - four-momentum of W+
c OUTPUT: awwel(3,3) - complex array containing polarization amplitudes 
c                     for left-handed electron 
c                     {M_0(-,+,l1,l2) in eq. (31)}
c         awwel(3,3) - complex array containing polarization amplitudes 
c                     for right-handed electron 
c                     {M_0(+,-,l1,l2) in eq. (31)}
c
c Written by: Wieslaw Placzek            date: 01.07.1994
c Last update: 02.08.1994                by: W.P.
c
      implicit real*8 (a-h,o-z)
      real*8 q1(4),q2(4)
      complex*16 awwel(3,3),awwer(3,3)
      common / weking / ene,amaz,gammz,amel,amfin,xk0,sinw2,ide,idf 
      common / wekin2 / amaw,gammw,gmu,alphaw   
! This common can be everywhere, contains various switches
      COMMON / KeyKey/  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
! polarization vectors of W-bosons, eq. (9)
      real*8 eps1(4,3),eps2(4,3)
      real*8 e1(4),e2(4),p1(4),pmq(4)
      complex*16 f1e1,f1e2,f1q2,fkz3
      data init /0/
      save init,zetl,etal,etar
      save
!
!...initialization
!--------------------------------------------------------------------- 
      if (init.eq.0) then
        init=1 
        KeyZet = MOD(KeyPhy,1000)/100
!... electroweak coefficient factors, eq. (30)
        zetl=0.5d0/sinw2
        etal=1-zetl
        etar=1
      endif  
!... calculation
!=====================================================================
!... four-momentum of the incoming electron in CMS 
!    (+z axis along the electron beam)
      ecm=dsqrt(s)   
      p1(1)=0d0
      p1(2)=0d0
      p1(3)=ecm/2d0
      p1(4)=p1(3)
!... calculation of polarization vectors of W-bosons, eq. (9)
      call polvec(q1,eps1)
      call polvec(q2,eps2)
!... calculation of the polarization amplitudes, eq. (31)
!    {note: we use different normalization!}
      wsp=-ecm
      do 10 k=1,4
 10   pmq(k)=p1(k)-q1(k)
      do 20 l2=1,3
      do 20 l1=1,3
        do 25 k=1,4
          e1(k)=eps1(k,l1)
 25       e2(k)=eps2(k,l2)
        f1e1=dcmplx(e1(1),-e1(2))
        f1e2=dcmplx(e2(1),-e2(2))
        f1q2=dcmplx(q2(1),-q2(2))
        e1e2=prodm(e1,e2)
        e1q2=prodm(e1,q2)
        e2q1=prodm(e2,q1) 
        if(keyzet.eq.0)then
        awwel(l1,l2)=( 2*(1/s - etal/dcmplx(s-amaz**2,s/amaz*gammz))*
     &                   (e1q2*f1e2 - e1e2*f1q2 - e2q1*f1e1) + 
     &                 zetl/t*fkz3(e2,pmq,e1) ) *wsp   
        awwer(l1,l2)= -2*(1/s - etar/dcmplx(s-amaz**2,s/amaz*gammz))*
     &                   (e1q2*conjg(f1e2) - e1e2*conjg(f1q2) -
     &                    e2q1*conjg(f1e1)) *wsp
        elseif(keyzet.eq.1)then
        awwel(l1,l2)=( 2*(1/s - etal/dcmplx(s-amaz**2,amaz*gammz))*
     &                   (e1q2*f1e2 - e1e2*f1q2 - e2q1*f1e1) + 
     &                 zetl/t*fkz3(e2,pmq,e1) ) *wsp   
        awwer(l1,l2)= -2*(1/s - etar/dcmplx(s-amaz**2,amaz*gammz))*
     &                   (e1q2*conjg(f1e2) - e1e2*conjg(f1q2) -
     &                    e2q1*conjg(f1e1)) *wsp
        elseif(keyzet.eq.2)then
        awwel(l1,l2)=( 2*(1/s - etal/(s-amaz**2))*
     &                   (e1q2*f1e2 - e1e2*f1q2 - e2q1*f1e1) + 
     &                 zetl/t*fkz3(e2,pmq,e1) ) *wsp   
        awwer(l1,l2)= -2*(1/s - etar/(s-amaz**2))*
     &                   (e1q2*conjg(f1e2) - e1e2*conjg(f1q2) -
     &                    e2q1*conjg(f1e1)) *wsp
        else
          write(6,*)'wrong KEYZET:',keyzet
        endif
 20   continue
      end  

      function prodm(p,q)
*     *******************
c Scalar product of the four-vectors p and q in Minkowski space;
c note: p_0=p(4), q_0=q(4)
c 
      implicit real*8 (a-h,o-z)
      real*8 p(4),q(4)
      prodm=p(4)*q(4)-p(3)*q(3)-p(2)*q(2)-p(1)*q(1)
      end

      subroutine polvec(q,eps)
*     **************************
c Calculation of polarization vectors of a vector boson 
c in the rectangular basis, see eq. (9); see also K. Hagiwara 
c and D. Zeppenfeld, Nucl. Phys. B274 (1986) 1, eq. (3.47).
c     INPUT:  q(4)     - four-momentum of the vector boson
c     OUTPUT: eps(4,3) - three polarization four-vector
c
c Written by: Wieslaw Placzek            date: 01.07.1994
c Last update: 27.07.1994                by: W.P.
c
      implicit real*8 (a-h,o-z)
      real*8 q(4),eps(4,3)   
      save
!
      qt2=q(1)**2+q(2)**2
      qt =sqrt(qt2)
      aq2=qt2+q(3)**2
      aq =sqrt(aq2)
      am =sqrt(q(4)**2-aq2) 
      do 10 l=1,3
      do 10 k=1,4
   10 eps(k,l)=0
      if(aq.lt.1d-10) then
        eps(1,1)=1
        eps(2,2)=1
        if (am.gt.1d-10) eps(3,3)=1 
      elseif (qt2.lt.1d-10) then
        eps(1,1)=q(3)/aq
        eps(2,2)=q(3)/aq
        if (am.gt.1d-10) then
          eps(3,3)=q(4)/am/aq*q(3)
          eps(4,3)=aq/am
        endif
      else
        ws1=1/aq/qt
        eps(1,1)= ws1*q(1)*q(3)
        eps(2,1)= ws1*q(2)*q(3)
        eps(3,1)=-ws1*qt2
        eps(4,1)= 0
        ws2=1/qt 
        eps(1,2)=-ws2*q(2)
        eps(2,2)= ws2*q(1)
        eps(3,2)= 0
        eps(4,2)= 0
        if (am.gt.1d-10) then
          ws3=q(4)/am/aq 
          eps(1,3)=ws3*q(1)
          eps(2,3)=ws3*q(2)
          eps(3,3)=ws3*q(3)
          eps(4,3)=ws3*aq2/q(4)
        endif
      endif  
      end

      function fkz3(a,b,c)
*     *******************************
c Function F_3 of four-vectors contracted with Dirac matrices; 
c see eq. (19)
c
c Written by: Wieslaw Placzek            date: 01.07.1994
c
      real*8 a(4),b(4),c(4)
      complex*16 fkz3
      fkz3=(a(4) +a(3)) * ((b(4) -b(3))*dcmplx(c(1),-c(2)) -
     &                     dcmplx(b(1),-b(2))*(c(4) -c(3)))+ 
     &     dcmplx(a(1),-a(2)) * ((b(4) +b(3))*(c(4) -c(3)) -
     &                     dcmplx(b(1), b(2))*dcmplx(c(1),-c(2)))
      end        

      subroutine wdecay(q,p1,p2,adw)
*     ******************************
c This routine calculates polarization amplitudes for W decays 
c into massless fermions. It is based on the paper: 
c K. Hagiwara et al., Nucl. Phys. B282 (1987) 253; see Appendix C.
c No CKM-mixing matrix elements incuded here.
c INPUT: q(4)        - four-momentum of W  
c        p1(4),p2(4) - four-momenta of decay products
c OUTPUT: adw(3) - complex array containing W decay amplitudes 
c                   {M(lambda,sigma_1,sigma_2) in eq. (C.16)}
c
c Written by: Wieslaw Placzek            date: 20.07.1994
c Last update: 02.08.1994                by: W.P.
c
      implicit real*8 (a-h,o-z)
      real*8 q(4),p1(4),p2(4)
      complex*16 adw(3)
      real*8 eps(4,3),e(4)
      complex*16 sfunhz
!
!... calculation of polarization vectors of W in rectangular basis
      call polvec(q,eps)
!... calculation of the W decay amplitudes
      do 10 l=1,3
        do 15 k=1,4
 15       e(k)=eps(k,l)
        adw(l)= 2*sqrt(p1(4)*p2(4)) *sfunhz(p1,e,p2)
 10   continue
      end

      function sfunhz(p1,a,p2)
*     ***********************************
c Spinorial string S(pi,a,pf) for massless spinors chi(pi), chi(pf);
c a(4) - given four-vector, 
c see K. Hagiwara et al., Nucl. Phys. B282 (1987) 253; Appendix C. 
c
c Written by: Wieslaw Placzek            date: 20.07.1994
c Last update: 01.08.1994                by: W.P.
c
      implicit real*8 (a-h,o-z)
      real*8 a(4),p1(4),p2(4)
      complex*16 z1p,z2m,zam,zap
      complex*16 sfunhz
!
      x1p=p1(3)+p1(4)
      x2p=p2(3)+p2(4)
      z1p=dcmplx(p1(1), p1(2))
      z2m=dcmplx(p2(1),-p2(2))
      xam=a(4)-a(3)
      xap=a(4)+a(3)
      zam=dcmplx(a(1),-a(2))
      zap=dcmplx(a(1), a(2))
      if (x1p.gt.1d-20 .and. x2p.gt.1d-20) then
        fac=0.5/sqrt(p1(4)*p2(4)*x1p*x2p)
        sfunhz=fac *( x1p*(x2p*xam-z2m*zap) + z1p*(z2m*xap-x2p*zam) )
      elseif (x1p.gt.1d-20) then
        sfunhz= (x1p*zap - z1p*xap)/sqrt(2*p1(4)*x1p)
      elseif (x2p.gt.1d-20) then
        sfunhz= (z2m*xap - x2p*zam)/sqrt(2*p2(4)*x2p)
      else
        sfunhz=xap
      endif
      end
!================================================
!=========== beta0, beta1 llogs =================
!================================================

      SUBROUTINE betax(svar,amel,beta00,beta01,beta02,beta10,beta11,
     &                                                       beta20)    
!     *****************************************************************
! This routine defines weights for the beta0, beta1 and beta2,
! to be implemented on top of basic distribution from karlud    
!     ***************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      PARAMETER(NPMX=25) 
      PARAMETER(VLIM1= 1.D-9, VLIM2= 1.D-9)   
      COMMON / MOMSET / QEFF1(4),QEFF2(4),SPHUM(4),SPHOT(100,4),NPHOT 
      COMMON / MATPAR / PI,CEULER     
      COMMON / PHYPAR / ALFINV,GPICOB     
      SAVE 
! ------------------ Inline functions ------------------
! Elements of single bremss. distribution
      chi(x)= (1d0+(1d0-x)**2)/2d0
      xni(x)= x*(-1+x/2d0 )

      beta00=0d0
      beta01=0d0
      beta02=0d0
      beta10=0d0
      beta11=0d0
      beta20=0d0

      gami=  2d0/alfinv/pi*(dlog(svar/amel**2)-1) 
!-- beta0
      beta00 = 1d0
      beta01 = 1d0 + gami/2d0     
      beta02 = 1d0 + gami/2d0 + gami**2/8d0 

!-- beta1
!-- Contributions from beta1            
      ene=sqrt(svar/4d0)    
      DO  jph=1,nphot 
         vv=sphot(jph,4)/ene            
         b10 = xni(vv)
         b11 = xni(vv)*(1d0 +gami/2d0)
     $        -chi(vv)*(gami/4d0)*dlog(1d0-vv)
         beta10  =  beta10 +b10             
         beta11  =  beta11 +b11             
      ENDDO

!-- beta2
      DO i=1,nphot
         DO j=i+1,nphot
            v1=sphot(i,4)/ene
            v2=sphot(j,4)/ene
            v1st = v1/(1-v2)
            v2st = v2/(1-v1)
            IF ( sphot(i,3)*sphot(j,3) .LT. 0d0) THEN
!           OPPOSITE directions two photons
               dis2= chi(v1)*chi(v2)
            ELSE
!           SAME directions two photons
               dis2= 0.5d0*( chi(v1)*chi(v2st) + chi(v1st)*chi(v2) )
            ENDIF
            beta1i = xni(v1)
            beta1j = xni(v2)
            bt20  = dis2 -beta1i -beta1j -beta00
            beta20 = beta20 +bt20  
         ENDDO
      ENDDO

      END


c================= SEMIANALYTICAL FORMULAS ===========
c================= SEMIANALYTICAL FORMULAS ===========
c================= SEMIANALYTICAL FORMULAS ===========
      SUBROUTINE korwan(svar,vvmin,vvmax,keymod,keypre,xsect,errabs)
C     **************************************************************
! KORWAN is a semianalytical routine calculating total xsection 
! with and without bremsstrahlung. Based on formula by Muta et.al. 
! and LL third order exponentiated structure functions.
!---------------------------------------------------------------
!  INPUTS:
!    svar   = CMS energy squared [GeV]
!    vvmin  = minimal photon energy (in most cases should be set to 0d0)
!    vvmax  = maximal photon energy
!    keymod = type of ISR corrections requested
!      general
!          0 ....Born               
!        300 ....Zero   Order, YFS style
!        301 ....First  Order, YFS style
!        302 ....Second Order, YFS style
!        303 ....Third  Order, YFS style
!        502 ....Second Order, Gribov-Kuraev-Fadin style
!      technical tests on various components of KORALW matr. el.
!        310 ....First  Order Beta0
!        311 ....First  Order Beta1
!        320 ....Second Order Beta0
!        321 ....Second Order Beta1
!        322 ....Second Order Beta2
!    keypre = precision key
!        1,2,3   ..for Born
!        1,2,3,4 ..for bremsstrahlung
!    other inputs are sent from KORALW input routine via commons
!    see KORALW for further explanations
!      COMMON / WEKING / ENE,AMAZ,GAMMZ,AMEL,AMFIN,XK0,SINW2,IDE,IDF  
!      COMMON / WEKIN2 / AMAW,GAMMW,DELTAR,ALPHAW    
!      COMMON / DECDAT / amdumm(20), br(20), icdumm(20)
!      COMMON / KeyKey/  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
!      COMMON / MATPAR / pi,ceuler     
!      COMMON / PHYPAR / alfinv,gpicob     
!---------------------------------------------------------------
!  OUTPUT:
!    xsect  = cross-section in picobarns
!    errabs = absolute error in pbarns
!---------------------------------------------------------------

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)  
! The next 4 commons are input data for KORWAN
      COMMON / WEKING / ENE,AMAZ,GAMMZ,AMEL,AMFIN,XK0,SINW2,IDE,IDF  
      COMMON / WEKIN2 / AMAW,GAMMW,DELTAR,ALPHAW    
      COMMON / DECDAT / amdumm(20), br(20), icdumm(20)
! This coomon can be everywhere, contains various switches
      COMMON / KeyKey/  KeyRad,KeyPhy,KeyTek,KeyMis,KeyDwm,KeyDwp
! This common sends rtesults back to KORWAN from its subprograms
      COMMON / GOUP / totfac,beti,xsmut0,dels,db
! This common sends external parameters from KORWAN to its subprograms
      COMMON / KOREXT / DFWM,DFWP,KEYZET,KEYACC,KEYNLL,KEYPHO,
     $   XAMEL,XSVAR,XAMAZ,XGAMMZ,XAMAW,XGAMMW,XSINW2,XALPHAW
! This PHOTONIC common sends internal parameters from KORWAN 
! to its subprograms
      COMMON / KORINT / vmin,vmax

      SAVE
      EXTERNAL yfspho

      XAMEL=AMEL
      XSVAR=SVAR
      XAMAZ=AMAZ
      XGAMMZ=GAMMZ
      XAMAW=AMAW
      XGAMMW=GAMMW
      XSINW2=SINW2
      XALPHAW=ALPHAW

      VMIN=VVMIN
      VMAX=VVMAX

      KeyAcc = KeyPre
      KeyZet = MOD(KeyPhy,1000)/100
      KeyNLL = MOD(KeyRad,1000)/100
      KeyPho = KeyMod

      brel=br(7)
      IF ( keydwm .EQ. 0) THEN
        dfwm=1/brel
      ELSE
        dfwm = br(keydwm)/brel
      ENDIF

      IF ( keydwp .EQ. 0 ) THEN
        dfwp=1/brel
      ELSE
        dfwp = br(keydwp)/brel
      ENDIF

      IF(KEYMOD.LT.0) THEN   ! diff. distrib.
        akor = yfspho(vmin)
        IF(KEYPRE.EQ.1) THEN
          errabs= 1d-5*dfwm*dfwp     ! test    (time:   1.0)  
        ELSEIF(KEYPRE.EQ.2) THEN
          errabs= 1d-6*dfwm*dfwp     ! normal  (time: * 2.5)  
        ELSEIF(KEYPRE.EQ.3) THEN
          errabs= 1d-7*dfwm*dfwp     ! high    (time: * 5.5)   
        ENDIF
      ELSEIF(KEYMOD.EQ.0) THEN   ! born
        akor = xsmuta(svar)
        IF(KEYPRE.EQ.1) THEN
          errabs= 1d-5*dfwm*dfwp     ! test    (time:   1.0)  
        ELSEIF(KEYPRE.EQ.2) THEN
          errabs= 1d-6*dfwm*dfwp     ! normal  (time: * 2.5)  
        ELSEIF(KEYPRE.EQ.3) THEN
          errabs= 1d-7*dfwm*dfwp     ! high    (time: * 5.5)   
        ENDIF
      ELSE  
        IF(KEYPRE.EQ.1) THEN
          eeps= 1d-5*dfwm*dfwp   ! test    (time:    1.0)
          errabs= 3d-5*dfwm*dfwp    
        ELSEIF(KEYPRE.EQ.2) THEN
          eeps= 5d-6*dfwm*dfwp   ! normal  (time: *  4.5) 
          errabs= 1d-5*dfwm*dfwp   
        ELSEIF(KEYPRE.EQ.3) THEN
          eeps= 1d-6*dfwm*dfwp   ! high    (time: * 40.0)
          errabs= 1d-6*dfwm*dfwp    
        ELSEIF(KEYPRE.EQ.4) THEN
          eeps= 1d-7*dfwm*dfwp   ! super-high    (time: * 200)
          errabs= 1d-7*dfwm*dfwp 
        ENDIF

        ndiv=4
        dv=(vmax-vmin)/dble(ndiv)
        akor=0d0
        vvma=vmin

        DO 100 ncurr=1,ndiv

        vvmi=vvma
        vvma=vvmi+dv
        if(vvma.ge.vmax) vvma=vmax

        CALL gausjd(yfspho,vvmi,vvma,eeps,res)
        xcr=res +totfac/beti*(vvma**beti-vvmi**beti)*xsmut0*(db+dels)
        akor=akor+xcr

        if(vvma.ge.vmax) goto 200
 100    continue
 200    continue

      ENDIF
      xsect=akor
      END

      FUNCTION xsmuta(ssact)
!     **********************
! integrated total xsect from Muta et.al.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)  
! This common sends external parameters from KORWAN to its subprograms
      COMMON / KOREXT / DFWM,DFWP,KEYZET,KEYACC,KEYNLL,KEYPHO,
     $   AMEL,SVAR,AMAZ,GAMMZ,AMAW,GAMMW,SINW2,ALPHAW
! This common sends internal parameters from XSMUTA to its subprograms
      COMMON / XMUINT / ssvar
      EXTERNAL d1muta
      SAVE

      ssvar=ssact
      aa=0d0
      bb=ssact

! change of vars
      gm =amaw*gammw
      am2=amaw**2
      umi=1d0/gm* ( ATAN((aa-am2)/gm) )
      uma=1d0/gm* ( ATAN((bb-am2)/gm) ) 

      IF(KEYACC.EQ.1) THEN
        eeps= 1d-6*dfwm*dfwp   ! test  
      ELSEIF(KEYACC.EQ.2) THEN
        eeps= 1d-8*dfwm*dfwp   ! normal
      ELSEIF(KEYACC.EQ.3.or.KEYACC.EQ.4) THEN
        eeps= 1d-9*dfwm*dfwp   ! high
      ENDIF

      xsmuta=gaus(d1muta,umi,uma,eeps)  

      END

      FUNCTION d1muta(u)
!     *******************
! one dimensional diff. xsect from muta et.al.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)  
! This common sends internal parameters from XSMUTA to its subprograms
      COMMON / XMUINT / ssvar
! This common sends internal parameters from D1MUTA to its subprograms
      COMMON / D1MUIN / s1
! This common sends external parameters from KORWAN to its subprograms
      COMMON / KOREXT / DFWM,DFWP,KEYZET,KEYACC,KEYNLL,KEYPHO,
     $   AMEL,SVAR,AMAZ,GAMMZ,AMAW,GAMMW,SINW2,ALPHAW
      EXTERNAL D2MUTA
      SAVE


      GM =AMAW*GAMMW
      AM2=AMAW**2
      S1=GM*TAN(GM*U)+AM2
      AA=0D0
      BB=(SQRT(SSVAR)-SQRT(S1))**2
! change of vars
      UMI=1D0/GM* ( ATAN((AA-AM2)/GM) )
      UMA=1D0/GM* ( ATAN((BB-AM2)/GM) ) 
! change of vars

      IF(KEYACC.EQ.1) THEN
        eeps= 1d-9*dfwm*dfwp   ! test  
      ELSEIF(KEYACC.EQ.2) THEN
        eeps= 1d-11*dfwm*dfwp   ! normal 
      ELSEIF(KEYACC.EQ.3.or.KEYACC.EQ.4) THEN
        eeps= 1d-13*dfwm*dfwp   ! high, long run
      ENDIF

      call dgadap(umi,uma,d2muta,eeps,result)

      d1muta =result
     $          *((s1-am2)**2+gm**2)  ! JACOBIAN

      END

      FUNCTION d2muta(u)
C     *******************
C TWO DIMENSIONAL DIFF. XSECT FROM MUTA et.al.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)  
      COMMON / MATPAR / pi,ceuler     
      COMMON / PHYPAR / alfinv,gpicob     
! This common sends internal parameters from XSMUTA to its subprograms
      COMMON / XMUINT / ssvar
! This common sends internal parameters from D1MUTA to its subprograms
      COMMON / D1MUIN / s1
! This common sends external parameters from KORWAN to its subprograms
      COMMON / KOREXT / DFWM,DFWP,KEYZET,KEYACC,KEYNLL,KEYPHO,
     $   AMEL,SVAR,AMAZ,GAMMZ,AMAW,GAMMW,SINW2,ALPHAW

      SAVE
      data init / 1 /

      IF( init.eq.1.or.ssvar.ne.sold) THEN
        sold=ssvar
        init=0

        e2 = 4*pi*alphaw
        g2 = e2/sinw2
        g2e2=g2*e2/2d0
        g4=g2**2/8d0
        a=1-4*sinw2
        b=-1
        a2b2=a**2+b**2
        amb=a-b

        gmw =gammw/amaw
        gmwm=gammw*amaw
        gmwm2=gmwm**2
        amw2=amaw**2

        gennor=(g2/(4*pi)/12d0)**2*gpicob/pi**2/(64*pi)
     $            *(-2d0)/ssvar**2*dfwm*dfwp

        IF( keyzet .eq. 0) THEN
          bwz=(ssvar-amaz**2)**2 +(ssvar/amaz*gammz)**2
        ELSEIF( keyzet .EQ. 1) THEN
          bwz=(ssvar-amaz**2)**2 +(amaz*gammz)**2
        ELSEIF( keyzet .EQ. 2) THEN
          bwz=(ssvar-amaz**2)**2 
        ELSE
          write(6,*)'d2muta==> wrong keyzet:',keyzet
        ENDIF
        bwzre=(ssvar-amaz**2)

        gaa = e2**2/ssvar**2
        gzz=g4/2d0 *a2b2/bwz
        gaz=g2e2   *a*bwzre/bwz/ssvar 
        gnn=g4 
        gnz=-g4     *amb*bwzre/bwz
        gna=-g2e2   /ssvar

        cngg1=(gaa+gzz+gaz)*gennor
        cngg2=(gnn)*gennor
        cngg3=(gnz+gna)*gennor
      ENDIF

C.. change of vars
      S2=GMWM*TAN(GMWM*U)+AMW2
C.. change of vars

      S1S2=S1*S2      
      S1pS2=S1+S2       
      SmS=ssvar-S1pS2

! Ad hoc correction S.J.
      IF(s2 .LT. 0d0 ) THEN
        d2muta=0d0
c[[[[[
c        write(6,*) 'd2muta <2> u,s2=',u,s2
c        write(6,*) 'd2muta <2> GMWM,AMW2=',GMWM,AMW2
c]]]]]
        RETURN
      ENDIF


      IF( (sqrt(ssvar)-sqrt(s1)-sqrt(s2)).le.0d0) THEN
        d2muta=0d0
        RETURN
c        wlambd=0d0
c        sqrwla=0d0
c        flog=0d0
      ELSE
        WLAMBD=(SmS)**2 -4*S1S2
c        IF(WLAMBD.LT.0D0) WRITE(6,*)'WLAMBD=',WLAMBD
c        IF(WLAMBD.LT.0D0) WRITE(6,*)sqrt(s2),sqrt(ssvar)-sqrt(s1)
        SQRWLA=DSQRT(WLAMBD)
        IF(s1s2.gt.0d0) THEN
          FLOG= S1S2*DLOG ((SmS-SQRWLA) / (SmS+SQRWLA)) 
        ELSE
          FLOG= 0D0
        ENDIF
      ENDIF

      GG1= -WLAMBD*SQRWLA* ( WLAMBD/6D0 +2*(ssvar*(S1pS2)+  S1S2) )
      GG2= -SQRWLA* ( WLAMBD/6D0 +2*(ssvar*(S1pS2)-4*S1S2) )
     &     +4*(SmS)*FLOG
      GG3= -SQRWLA* ( 
     &               WLAMBD/6D0*(ssvar+11*S1pS2)
     &         +2*(S1**2 +3*S1S2 +S2**2)*ssvar -2*(S1**3 +S2**3) 
     &               )
     &    -4*(ssvar*(S1pS2)+S1S2)*FLOG


c from Manel Martinez
c      GG3MM= -SQRWLA*(ssvar-S1-S2)* ( WLAMBD/6D0
c     &              +2*(S1*S2+ssvar*S1+ssvar*S2)
c     &                           )
c     &    -4*S1*S2*(ssvar*(S1+S2)+S1*S2)*FLOG

      D2MUTA=( CNGG1*GG1 +CNGG2*GG2 +CNGG3*GG3) 
     &        /((S1-AMW2)**2+(S1*GMW)**2)  ! breit-wigner
     &        /((S2-AMW2)**2+(S2*GMW)**2)  ! breit-wigner
     &        *((S2-AMW2)**2+GMWM2)     ! JACOBIAN
      

      END


      FUNCTION xsbrem(svar)
c     ccccccccccccccccccccccc
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)  
      COMMON / PHODIS / keypho
      SAVE

      KEYMOD=KEYPHO
      KEYPRE=2
      
      VVMI=0D0
      XSTOT=0D0
      DO vvma=0.25d0,1.001d0,.25d0
         call korwan(svar,vvmi,vvma,keymod,keypre,res,errabs)
         VVMI=VVMA
         xstot=xstot+res
      ENDDO
      xsbrem=xstot
      END

      FUNCTION yfspho(vv)
!     ************************************************
! for negative KEYPHO vv*D\otimes D*xsmuta is returned
! = differential distribution d sigma/ d log v
!     ************************************************
!          0 ....Born               
!        300 ....Zero   Order, YFS style
!        301 ....First  Order, YFS style
!        302 ....Second Order, YFS style
!        303 ....Third  Order, YFS style
!        502 ....Second Order, Gribov-Kuraev-Fadin style
!      technical tests on various components of KORALW matr. el.
!        310 ....First  Order Beta0
!        311 ....First  Order Beta1
!        320 ....Second Order Beta0
!        321 ....Second Order Beta1
!        322 ....Second Order Beta2
!     ************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)  

      COMMON / GOUP / totfac,beti,xsmut0,dels,db
      COMMON / MATPAR / pi,ceuler     
      COMMON / PHYPAR / alfinv,gpicob     
! This common sends external parameters from KORWAN to its subprograms
      COMMON / KOREXT / DFWM,DFWP,KEYZET,KEYACC,KEYNLL,KEYPHO,
     $   AMEL,SVAR,AMAZ,GAMMZ,AMAW,GAMMW,SINW2,ALPHAW
! This PHOTONIC common sends internal parameters from KORWAN 
! to its subprograms
      COMMON / KORINT / vmin,vmax
      SAVE
      data init / 1 /

      IF( init.eq.1 .or. svar.ne.sold .or. vmin.ne.vmiold 
     &              .or. keyNLL.ne.knlold ) THEN
        init=0
        sold=svar
        vmiold=vmin
        knlold=keyNLL
        alf1   = 1d0/pi/alfinv
        bilg   = dlog(svar/amel**2)
        beti   = 2d0*alf1*(bilg-1d0)
        IF(    KeyNLL .EQ. 0) THEN
           delb = 1/4d0*beti  
        ELSEIF(KeyNLL .EQ. 1) THEN
           delb = 1/4d0*beti  +alf1*(-0.5d0  +pi**2/3d0)        
        ELSE
           WRITE(6,*) '++++ STOP in yfspho, KeyNLL= ',KeyNLL
           STOP
        ENDIF
        gamfac = exp(-ceuler*beti)/dpgamm(1d0+beti)          
        totfac = beti*gamfac*exp(delb)
        xsmut0 = xsmuta(svar*(1-vmin))
      ENDIF

      sprim=svar*(1-vv)
      IF( vv .GT. 1d0) THEN
        write(6,*)'WARNING YFSPHO==> 1-vv<0 ',1-vv,', vv set to 1'
        vv=1d0
        sprim=0d0
      ENDIF
      born = xsmuta(sprim)


      IF(KEYPHO.GE.0) THEN     
        keypht = keypho
        yfs  = totfac*vv**(beti-1d0) 
      ELSE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! WATCH OUT:  1/VV removed for negative KEYPHO !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        yfs  = totfac*vv**beti 
        xsmut0 = 0d0
        keypht = -keypho
      ENDIF
!---------------------------------------------------------------
! ....Zero   Order               
      IF(KEYPHT  .EQ.300)  THEN 
        db = 1d0        
        dels = 0d0           
        delh = -beti/4d0 *log(1-vv)          
!---------------------------------------------------------------
! ....First  Order               
      ELSEIF(KEYPHT  .EQ.301)  THEN           
        db = 1d0        
        dels = beti/2d0        
        delh = vv*(-1 +vv/2d0)            
     $      -beti/2d0*vv**2 - beti/4d0*(1-vv)**2*log(1-vv)           
!---------------------------------------------------------------
!-- Second COMPLETE, YFS style (from yfs3mod.f)
      ELSEIF(KEYPHT  .EQ.302)  THEN           
        db = 1d0
        dels = beti/2d0  +1/8d0*beti**2
        delh = vv*(-1 +vv/2d0) 
     $   +0.5d0*beti*(-0.25d0*(4d0-6d0*vv+3d0*vv**2)*dlog(1d0-vv) -vv)
!---------------------------------------------------------------
!-- Third COMPLETE, YFS style 
      ELSEIF(KEYPHT  .EQ.303)  THEN           
        db = 1d0
        ds1= beti/2d0 
        ds2=            1/8d0*beti**2 
        ds3=                           1/48d0*beti**3

        dh0=   vv*(-1 +vv/2d0)
        dh1= beti*( -(1d0+3d0*(1d0-vv)**2)/8d0*dlog(1d0-vv)
     @                 -.25d0*vv**2   )
        dh2= beti**2*( 0
     @                 +(3d0*vv-2d0)*vv/16*dlog(1d0-vv)
     @                 +(8d0-14d0*vv+7d0*vv**2)/96*dlog(1d0-vv)**2
     @                 +vv**2/8d0
     @                 +(2d0-vv)*vv/8*dilogy(vv)  
     @               )
        dels = ds1 +ds2 +ds3 
        delh = dh0*(1+ds1+ds2) +dh1*(1+ds1) +dh2
!---------------------------------------------------------------
!-- First  Order Beta0 
      ELSEIF(KEYPHT  .EQ.310)  THEN 
        db = 1d0        
        dels = beti/2d0        
        delh = -beti/4d0 *log(1-vv)   
!---------------------------------------------------------------
!-- First  Order Beta1
      ELSEIF(KEYPHT  .EQ.311)  THEN           
        db = 0d0        
        dels = 0d0        
        delh =    
     $        vv*(-1d0+vv/2d0/(1+beti))*(1-0.5*beti*log(1-vv))
!---------------------------------------------------------------
!-- Second Beta0 
      ELSEIF( KEYPHT .EQ. 320) THEN       
         db = 1d0        
         dels = beti/2d0  +beti**2/8d0             
         delh = -beti/4d0 *log(1-vv)   
!---------------------------------------------------------------
!-- Second  Order Beta1
      ELSEIF(KEYPHT  .EQ.321)  THEN           
        db = 0d0        
        dels = 0d0        
        delh = vv*(-1+vv/2d0)         
     $      -beti*vv/2 -beti*vv**2/4 +beti/8*(-2+6*vv-3*vv**2)*log(1-vv)
!---------------------------------------------------------------
!-- Second  Beta2 
      ELSEIF( KEYPHT .EQ. 322)  THEN     
        db = 0d0        
        dels = 0d0        
        delh =    beti*  vv**2/4d0  
!---------------------------------------------------------------
!-- Second COMPLETE, Gribov-Kuraev-Fadin style
!---This is added in artificial manner but numerically should be OK
      ELSEIF(KEYPHT  .EQ.502)  THEN
        totfac = beti
        dels   = 0d0
        db = gamfac*exp( 3/4d0 *beti)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       DELH=  -ALF1*(1D0+Z)*(BILG-1D0)    
!    $      +ALF1**2*( -(1D0+Z*Z)/VV     *DLOG(Z)                
!    $              +(1D0+Z)*(0.5D0*DLOG(Z)-2D0*DLOG(VV))        
!    $              -2.5D0-0.5D0*Z)*BILG**2    
!       DISTR=  BETI*VV**(BETI-1D0)*( 1D0+DELVS) +DELH          
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        z=1-vv
        delh= -1/2d0*(1d0+z)
     $        +1/4d0*beti*( -(1d0+z*z)/vv     *dlog(z)                
     $                +(1d0+z)*(0.5d0*dlog(z)-2d0*dlog(vv))        
     $                 -2.5d0-0.5d0*z)
        delh=delh/vv**(beti-1d0)
      ELSE
        write(6,* ) '+++++ STOP in YFSPHO, wrong KEYPHO=',KEYPHT
      ENDIF        

      yfspho= yfs*(  (db+dels+delh)*born -(db+dels)*xsmut0  )
!----------------------------------------------------------------   
      END


c================= END SEMIANALYTICAL FORMULAS ===========
c================= END SEMIANALYTICAL FORMULAS ===========
c================= END SEMIANALYTICAL FORMULAS ===========


