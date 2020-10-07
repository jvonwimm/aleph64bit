      SUBROUTINE askusi(icode)
C-----------------------------------------------------------------------
C! Initialization routine for HZHA01
C  B.Bloch March 97 add setting to store fragmentation info
C
C-----------------------------------------------------------------------
C
      PARAMETER(lpdec=48)
      PARAMETER(igcod=7020)
      INTEGER nodec(lpdec)
      INTEGER altabl,namind,alrlep
      EXTERNAL altabl,namind,alrlep
C
      DIMENSION indneu(4), indcha(2)
      DATA indneu/51,52,53,54/
      DATA indcha/55,56/
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON / BCS / IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (L1MST=200, L1PAR=200)
      PARAMETER (L2PAR=500, L2PARF=2000 )
      PARAMETER (LJNPAR=4000)
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),
     &                KFDP(L2PARF,5)
      COMMON /LUDAT4/ CHAF(L2PAR)
      CHARACTER*8 CHAF
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5)
C
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      COMMON / elweak / sw2,alpha(0:nhig),gweak2(0:nhig),
     .                  alphas(0:nhig),g_f,deltar,alpha2,sw,cw2,cw
C
      PARAMETER (maxpro= 9)
      COMMON / cropro / cross(maxpro), sthr(maxpro), reduc(maxpro)
      CHARACTER*14 chapro(maxpro)
      DATA chapro /
     .             'e+e- --> h Z',
     .             'e+e- --> H Z',
     .             'e+e- --> h A',
     .             'e+e- --> H A',
     .             'W+W- --> h  ',
     .             'W+W- --> H  ',
     .             'Z Z  --> h  ',
     .             'Z Z  --> H  ',
     .             'e+e- --> H+H-'
     .                                /
      COMMON / prosim / iproyn(maxpro),nevpro(maxpro)
      CHARACTER*14 channel
      CHARACTER*21 channeut, chanchar
      PARAMETER(nchneut=8,nchchar=5)
      COMMON / hhdeck / branch(nchan,nhig),width(nhig),
     .                  parwth(nhig),xymas(2,nchan,nhig),
     .                  xywid(2,nchan,nhig)
      COMMON / chaneu / ichn(2),
     .                  wneut(4,4,nhig), wchar(2,2,nhig),
     .                  widneut(4), brneut(nchneut,4),
     .                  widchar(2), brchar(nchchar,2)
      COMMON / chanch / channeut(nchneut,4), chanchar(nchchar,2)
      COMMON / vect4 / pvect4(5,2)
      COMMON / hinput / ichan(nchan,nhig), channel(nchan,nhig)
      COMMON / hisbr / bchpp,bchgg
      DIMENSION ph(4)
C
      DIMENSION brai(11), kcode(11), xmasmi(11)
      DATA brai/.0335,.0665,.0335,.0665,.0335,.0665,
     .          .1540,.1190,.1540,.1190,.1540/
      DATA kcode/11,12,13,14,15,16,1,2,3,4,5/
      DATA xmasmi/6*0.,0.3,0.3,1.0,4.0,11.0/
      COMMON / zzdec / braz(11), kselec(11), fracz
C
      COMMON / miscl / loutbe,ecm,idb1,idb2,xrad,empir,empirm,ism,
     &                 iklei,icar,sdvrt(3),vrtx(4),tabl(26),
     &                 nevent(11)
      INTEGER loutbe,idb1,idb2,nevent,ism,iklei,icar
      REAL*4 ecm,sdvrt,vrtx,tabl,empir,empirm,xrad
C
      INTEGER    IHCD
      REAL*8     MH, TBT, TAF
      COMMON /HCCOMM/ MH(4), TBT, TAF, IHCD(2)
      INTEGER    HCCON, NSCH, NCH, NDCH, NMXC
      PARAMETER( NCH= 49, NDCH= 19, NMXC= 9 )
      REAL*8     HCCBR1, HCCBR2, TAUHP
      COMMON /HCDEFF/ NSCH(NDCH), HCCON(NDCH),
     .                HCCBR1(0:NDCH), HCCBR2(NDCH,NMXC), TAUHP
      CHARACTER*17 HCCHAN
      COMMON /HCNAME/ HCCHAN(NDCH)
      INTEGER    UPMAX
      REAL*8     MEMAX, THRES
      COMMON /HCMAXX/ MEMAX(NCH), THRES(NCH), UPMAX(NCH)
      INTEGER    HCDSTA
      COMMON /HCSTAT/ HCDSTA(0:NCH)
C-----------------------------------------------------------------------
C!    HCCHAN           channel names
C!    HCCON            requested decay ( 1= ON, 0= OFF )
C!    HCCBR1           cumulative BR on first channel classif. (data car
C!    HCCBR2           cumulative BR on subchannel classif. (internal)
C!    NSCH             number of subchannels per channel
C!    TAUHP            lifetime in seconds
C-----------------------------------------------------------------------
      parameter ( lgene=7,lgsmo=6,lgsus=11,lpryn=9,lgzdc=11,lghcc=8)
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
C
C
      loutbe = IW(6)
C...Set generator code
      icode = igcod
C     Necessary to keep info on fragmentation
C
      MSTU(17) = 1
C
C...Create the KLIN bank and complete the PART bank
C
      CALL kxl74a(ipart,iklin)
      IF( ipart .LE . 0 .OR. iklin .LE. 0 ) THEN
        WRITE(loutbe,1000) ipart,iklin
 1000   FORMAT(1X,'+++ASKUSI+++ ERROR FILLING PART OR KLIN---STOP'
     $  ,2I5)
        STOP
      ENDIF
C
C Welcome !
C
      WRITE(loutbe,1001) icode
 1001 FORMAT(//15X,'+---------------------------------------------+'/
     .      15X,'|                                             |'/
     .      15X,'|     W E L C O M E   T O    H Z H A 0 2      |'/
     .      15X,'|     **********************************      |'/
     .      15X,'|      The (SUSY) Higgs boson generator       |'/
     .      15X,'|             in e+e- collisions              |'/
     .      15X,'|     **********************************      |'/
     .      15X,'|             Code is # ',I6,'                |'/
     .      15X,'|                                             |'/
     .      15X,'|                                             |'/
     .      15X,'|     Last date of change : 26 January  1998  |'/
     .      15X,'|                                             |'/
     .      15X,'|   . Patrick Janot  --  CERN/PPE             |'/
     .      15X,'|       for the neutral Higgs bosons          |'/
     .      15X,'|                                             |'/
     .      15X,'|   . Gerardo Ganis  --  MPI/Munich           |'/
     .      15X,'|       for the charged Higgs bosons          |'/
     .      15X,'+---------------------------------------------+'//)
C
C Read the generator parameters
C
      ngene = NAMIND('GENE')
      jgene = IW(ngene)
      IF(jgene.NE.0) THEN
        len= iw(jgene)
        if ( len.ne.lgene) then
           write (6,*) ' the GENE card has',len,' inputs','should be'
     $        ,lgene
           call exit
        endif
        iklei   = NINT(RW(jgene+1))
        iproc   = NINT(RW(jgene+2))
        xrad    =      RW(jgene+3)
        ecm     =      RW(jgene+4)
        empir   =      RW(jgene+5)
        ism     =      RW(jgene+6)
        icar    =      RW(jgene+7)
      ELSE
        iklei   = 1
        iproc   = 1
        xrad    = 1.
        ecm     = 190.
        empir   = 4.0
        ism     = 1
        icar    = 3
      ENDIF
C
C Check consistency
C
      IF( iproc.EQ.9 .AND. ism.EQ.1 ) THEN
        WRITE(*,*)
     .   'No charged Higgs in SM - STOP '
        STOP
      END IF
C
C Read the Standard Model parameters
C
      ngsmo = NAMIND('GSMO')
      jgsmo = IW(ngsmo)
      IF(jgsmo.NE.0) THEN
        len = iw(jgsmo)
        if ( len.ne.lgsmo) then
           write (6,*) ' the GSMO card has',len,' inputs','should be'
     $        ,lgsmo
           call exit
        endif
        amz     =      RW(jgsmo+1)
        gmz     =      RW(jgsmo+2)
        g_f     =      Rw(jgsmo+3)
        amt     =      RW(jgsmo+4)
        amh     =      RW(jgsmo+5)
        xlamda5 =      RW(jgsmo+6)
      ELSE
        amz     = 91.189
        gmz     = 2.497
        g_f     = 1.166392E-5
        amt     = 174.
        amh     = 60.
        xlamda5 = .208
      ENDIF
C
C Read the MSSM parameters
C
      ngsus = NAMIND('GSUS')
      jgsus = IW(ngsus)
      IF(jgsus.NE.0) THEN
        len = iw(jgsus)
        if ( len.ne.lgsus) then
           write (6,*) ' the GSUS card has',len,' inputs','should be'
     $        ,lgsus
           call exit
        endif
        amarun  =      RW(jgsus+1)
        tb      =      RW(jgsus+2)
        susM    =      RW(jgsus+3)
        susMU   =      RW(jgsus+4)
        susAt   =      RW(jgsus+5)
        susAb   =      RW(jgsus+6)
        susSMQ  =      RW(jgsus+7)
        susSMU  =      RW(jgsus+8)
        susSMD  =      RW(jgsus+9)
        susSML  =      RW(jgsus+10)
        susSME  =      RW(jgsus+11)
      ELSE
        amarun  = 100.
        tb      = 10.
        susM    = 0.
        susMU   = 0.
        susAt   = 0.
        susAb   = 0.
        susSMQ  = 1000.
        susSMU  = 1000.
        susSMD  = 1000.
        susSML  = 1000.
        susSME  = 1000.
      ENDIF
C
      CALL vzero(nevpro(1),maxpro)
      npryn = NAMIND('PRYN')
      jpryn = IW(npryn)
      IF(jpryn.NE.0) THEN
        len = iw(jpryn)
        if ( len.ne.lpryn) then
           write (6,*) ' the PRYN card has',len,' inputs','should be'
     $        ,lpryn
           call exit
        endif
        DO ipro = 1, maxpro
          iproyn(ipro) = IW(jpryn+ipro)
        ENDDO
      ELSE
        CALL vzero(iproyn(1),maxpro)
        iproyn(iproc) = 1
      ENDIF
C
      igzdc = NLINK('GZDC',0)
      jgzdc= iw(igzdc)
      if ( (jgzdc.gt.0).and.(jgzdc.ne.lgzdc)) then
           write (6,*) ' the GZDC card has',jgzdc,' inputs','should be'
     $        ,lgzdc
           call exit
      endif
      DO ifs = 1, 11
        IF ( igzdc .LE. 0 ) THEN
          kselec(ifs) = 1
        ELSE
          kselec(ifs) = IW(igzdc+ifs)
        ENDIF
      ENDDO
C
      braz(1) = brai(1) * kselec(1)
      DO ifs = 2, 11
        braz(ifs) = brai(ifs)*kselec(ifs)+braz(ifs-1)
      ENDDO
C
      fracz = braz(11)
      DO ifs = 1, 11
        braz(ifs) = braz(ifs)/braz(11)
      ENDDO
C
      pmas(23,1) = amz
      pmas(23,2) = gmz
      pmas( 6,1) = amt
C
C - Charged Higgs
      ighcc = NLINK('GHCC',0)
      IF( ighcc .GT. 0 ) THEN
        jghcc = iw(ighcc)
        if ( jghcc.ne.lghcc) then
           write (6,*) ' the GHCC card has',jghcc,' inputs','should be'
     $        ,lghcc
           call exit
        endif
        ihcd(1)= IW( ighcc+1 )
        ihcd(2)= IW( ighcc+2 )
        IF( ism.EQ.-1 ) THEN
          WRITE(*,3000)
          gmh=  RW( ighcc+3 )
          amh=  RW( ighcc+4 )
          ama=  RW( ighcc+5 )
          amhp= RW( ighcc+6 )
          tb=   RW( ighcc+7 )
          alfa= RW( ighcc+8 )
        END IF
      ELSE
        IF( iproc.EQ.9 ) THEN
          WRITE(*,*)
     .     'For H+H- production (iproc=9) GHCC card is mandatory'
          STOP
        END IF
      END IF
3000  FORMAT(/,'***************************************************',/,
     .         '*** Generic Type II two-higgs-doublet model     ***',/,
     .         '*** Take Higgs masses and angles from data      ***',/,
     .         '*** card GHCC                                   ***',/,
     .         '***************************************************',//)
C
      TABL(1)  = iklei
      TABL(2)  = iproc
      TABL(3)  = xrad
      TABL(4)  = ecm
      TABL(5)  = empir
      TABL(6)  = ism
      TABL(7)  = amz
      TABL(8)  = gmz
      TABL(9)  = g_f
      TABL(10) = amt
      IF( iproc.EQ.9 ) THEN
        IF( ism.GE.0 ) THEN
          TABL(11) = amh
          TABL(12) = xlamda5
          TABL(13) = amarun
          TABL(14) = tb
          TABL(15) = susM
          TABL(16) = susMU
        ELSE IF( ism.EQ.-1 ) THEN
          TABL(11) = gmh
          TABL(12) = amh
          TABL(13) = ama
          TABL(14) = amhp
          TABL(15) = tb
          TABL(16) = alfa
        END IF
      END IF
      TABL(17) = susAt
      TABL(18) = susAb
      TABL(19) = susSMQ
      TABL(20) = susSMU
      TABL(21) = susSMD
      TABL(22) = susSML
      TABL(23) = susSME
C
C...Initialize Higgs decay routine
C
      CALL hhinit
      IF ( amh .LT. 0. ) STOP 99
      IF ( ism .EQ. 0 .AND. amst(1) .LE. 0. ) STOP 99
      WRITE(6,*) ' Lifetimes (ps) : ',
     .            tauh(1)*1E12,tauh(2)*1E12,tauh(3)*1E12
C
C...Compute Born cross sections
C
      CALL vzero(wtot (1),4)
      CALL vzero(wtot2(1),4)
      CALL vzero(ntry (1),4)
      CALL vzero(nacc (1),4)
C
      sbeam = ecm**2
      DO 5 ipro = 1, maxpro
        cross(ipro) = crocom(ipro,sbeam)
    5 CONTINUE
C
      CALL vzero(reduc(1),maxpro)
      reduc(1) = fracz * parwth(2) / width(2)
      reduc(5) = parwth(2) / width(2)
      reduc(7) = parwth(2) / width(2)
      IF ( ism .LE. 0 ) THEN
        reduc(2) = fracz * parwth(1) / width(1)
        reduc(3) = parwth(2)*parwth(3) / (width(2)*width(3))
        reduc(4) = parwth(1)*parwth(3) / (width(1)*width(3))
        reduc(6) = parwth(1) / width(1)
        reduc(8) = parwth(1) / width(1)
        brhc1    = hccbr1(ihcd(1))
        IF( ihcd(1).GT.1 ) brhc1= hccbr1(ihcd(1))-hccbr1(ihcd(1)-1)
        brhc2    = hccbr1(ihcd(2))
        IF( ihcd(2).GT.1 ) brhc2= hccbr1(ihcd(2))-hccbr1(ihcd(2)-1)
        reduc(9) = brhc1*brhc2
      ENDIF
C
      WRITE(loutbe,2000) (chapro(ipro),
     .                    cross (ipro),
     .                    reduc (ipro),ipro=1,maxpro)
 2000 FORMAT(1x,'Born level cross section have been computed:'/
     .     9(1x,'   . ',A14,' : ',E12.4,' fb  * BR = ',F8.6/))
C...
C...End of HZHA stuff
C...
C
C...Fill back the Higgs masses and lifetimes into the PART bank
C
      napar = NAMIND('PART')
      jpart = IW(napar)
      mdcy(lucomp(25),1) = 0
      mdcy(lucomp(35),1) = 0
      mdcy(lucomp(36),1) = 0
      pmas(lucomp(25),4) = tauh(2)/3.33E-12
      pmas(lucomp(35),4) = tauh(1)/3.33E-12
      pmas(lucomp(36),4) = tauh(3)/3.33E-12
      pmas(lucomp(37),4) = SNGL(tauhp)/3.33E-12
C...for h
      ipart = kgpart(25)
      IF ( ipart .LE. 0 ) THEN
        ipart = KBPART(100,'Higgs h     ',100,pmas(25,1),0.,0.)
        jpart = IW(napar)
        IW(jpart+lmhlen+(ipart-1)*iw(jpart+1)+10) = ipart
        index = KBKLIN(ipart,25)
      ELSE
        rw(jpart+lmhlen+(ipart-1)*iw(jpart+1)+6) = pmas(25,1)
      ENDIF
      IF ( tauh(2) .GT. 1.E-15 .AND. ipart .GT. 0 )
     .  RW(jpart+lmhlen+(ipart-1)*iw(jpart+1)+8) = tauh(2)
C...for H
      ipart = kgpart(35)
      IF ( ipart .LE. 0 ) THEN
        ipart = KBPART(100,'Higgs H     ',100,pmas(35,1),0.,0.)
        jpart = IW(napar)
        IW(jpart+lmhlen+(ipart-1)*iw(jpart+1)+10) = ipart
        index = KBKLIN(ipart,35)
      ELSE
        rw(jpart+lmhlen+(ipart-1)*iw(jpart+1)+6) = pmas(35,1)
      ENDIF
      IF ( tauh(1) .GT. 1.E-15 .AND. ipart .GT. 0 )
     .  RW(jpart+lmhlen+(ipart-1)*iw(jpart+1)+8) = tauh(1)
C...for A
      ipart = kgpart(36)
      IF ( ipart .LE. 0 ) THEN
        ipart = KBPART(100,'Higgs A     ',100,pmas(36,1),0.,0.)
        jpart = IW(napar)
        IW(jpart+lmhlen+(ipart-1)*iw(jpart+1)+10) = ipart
        index = KBKLIN(ipart,36)
      ELSE
        rw(jpart+lmhlen+(ipart-1)*iw(jpart+1)+6) = pmas(36,1)
      ENDIF
      IF ( tauh(3) .GT. 1.E-15 .AND. ipart .GT. 0 )
     .  RW(jpart+lmhlen+(ipart-1)*iw(jpart+1)+8) = tauh(3)
C...for H+
      ipart = kgpart(37)
      IF ( ipart .LE. 0 ) THEN
        ipart = KBPART(100,'Higgs H+    ',100,pmas(37,1),0.,0.)
        jpart = IW(napar)
        IW(jpart+lmhlen+(ipart-1)*iw(jpart+1)+10) = ipart
        index = KBKLIN(ipart,37)
      ELSE
        rw(jpart+lmhlen+(ipart-1)*iw(jpart+1)+6) = pmas(37,1)
      ENDIF
      IF ( SNGL(tauhp) .GT. 1.E-15 .AND. ipart .GT. 0 )
     .  RW(jpart+lmhlen+(ipart-1)*iw(jpart+1)+8) = SNGL(tauhp)
C...for H-
      ipart = kgpart(-37)
      IF ( ipart .LE. 0 ) THEN
        ipart = KBPART(100,'Higgs H-    ',100,pmas(37,1),0.,0.)
        jpart = IW(napar)
        IW(jpart+lmhlen+(ipart-1)*iw(jpart+1)+10) = ipart
        index = KBKLIN(ipart,-37)
      ELSE
        rw(jpart+lmhlen+(ipart-1)*iw(jpart+1)+6) = pmas(37,1)
      ENDIF
      IF ( SNGL(tauhp) .GT. 1.E-15 .AND. ipart .GT. 0 )
     .  RW(jpart+lmhlen+(ipart-1)*iw(jpart+1)+8) = SNGL(tauhp)
C...For Z
      ipart = kgpart(23)
      rw(jpart+lmhlen+(ipart-1)*iw(jpart+1)+6) = pmas(23,1)
C...For top
      ipart = kgpart(6)
      rw(jpart+lmhlen+(ipart-1)*iw(jpart+1)+6) = pmas(6,1)
      ianti = ITABL(jpart,ipart,10)
      kapar = krow(jpart,ianti)
      rw(kapar+6)=pmas(6,1)
C
C...Add neutralinos to the particle list
C
      DO ineut = 1, 4
        jchi = indneu(ineut)
        kchi = LUCOMP(jchi)
C
        CHAF(kchi)(1:4)='Chi0'
        WRITE(CHAF(kchi)(4:4),50) ineut
   50   FORMAT(I1)
        PMAS(kchi,1) = ABS(amneut(ineut))
        IF ( ineut .NE. 1 ) MDCY(kchi,1) = 0
        KCHG(kchi,1) = 0
        KCHG(kchi,3) = 0
C
        ipart = KGPART(jchi)
        IF ( ipart .LE. 0 ) THEN
          ipart = KBPART(100,chaf(kchi)(1:4)//'_0      ',
     .                   100,pmas(kchi,1),0.,0.)
          jpart = IW(napar)
          IW(jpart+lmhlen+(ipart-1)*IW(jpart+1)+10) = ipart
          index = KBKLIN(ipart,jchi)
        ELSE
          rw(jpart+lmhlen+(ipart-1)*IW(jpart+1)+6) = pmas(kchi,1)
        ENDIF
      ENDDO
C
C...Add Charginos to the particle list
C
      DO ichar = 1, 2
        jchi = indcha(ichar)
        kchi = LUCOMP(jchi)
C
        CHAF(kchi)(1:4)='Chi0'
        WRITE(CHAF(kchi)(4:4),51) ichar
   51   FORMAT(I1)
        PMAS(kchi,1) = ABS(amchar(ichar))
        MDCY(kchi,1) = 0
        KCHG(kchi,1) = 3
        KCHG(kchi,3) = 1
C
        ipart = KGPART(jchi)
        IF ( ipart .LE. 0 ) THEN
          ipart = KBPART(100,chaf(kchi)(1:4)//'~+      ',
     .                   100,pmas(kchi,1),1.,0.)
          jpart = IW(napar)
          IW(jpart+lmhlen+(ipart-1)*IW(jpart+1)+10) = ipart
          index = KBKLIN(ipart,jchi)
        ELSE
          rw(jpart+lmhlen+(ipart-1)*IW(jpart+1)+6) = pmas(kchi,1)
        ENDIF
C
        ipart = KGPART(-jchi)
        IF ( ipart .LE. 0 ) THEN
          ipart = KBPART(100,chaf(kchi)(1:4)//'~-      ',
     .                   100,pmas(kchi,1),-1.,0.)
          jpart = IW(napar)
          IW(jpart+lmhlen+(ipart-1)*IW(jpart+1)+10) = ipart
          index = KBKLIN(ipart,-jchi)
        ELSE
          rw(jpart+lmhlen+(ipart-1)*IW(jpart+1)+6) = pmas(kchi,1)
        ENDIF
C
      ENDDO
C
C...Get list of lund particle # which should not be decayed
      mxdec = KNODEC(nodec,lpdec)
C     WRITE(6,*) 'KNODEC = ',mxdec,' LPDEC = ',lpdec
      mxdec = MIN(mxdec,lpdec)
C...Inhibit lund decays which should be done in galeph
      IF ( mxdec .GT. 0 ) THEN
        DO 10 i=1,mxdec
          IF ( nodec(i) .GT. 0 ) THEN
            jidb = NLINK('MDC1',nodec(i))
            IF ( jidb .eq. 0 ) mdcy(lucomp(nodec(i)),1) = 0
          ENDIF
   10   CONTINUE
      ENDIF
C...Vertex smearing
      sdvrt(1) = 0.035
      sdvrt(2) = 0.0012
      sdvrt(3) = 1.28
      jsvrt = NLINK('SVRT',0)
      IF ( jsvrt .NE. 0 ) THEN
        sdvrt(1) = RW(jsvrt+1)
        sdvrt(2) = RW(jsvrt+2)
        sdvrt(3) = RW(jsvrt+3)
      ENDIF
      tabl(24) = sdvrt(1)
      tabl(25) = sdvrt(2)
      tabl(26) = sdvrt(3)
C
C  Fill the KPAR bank with the generator parameters
C
       jkpar = altabl('KPAR',26,1,tabl,'2I,(F)','C')
C  Fill RLEP bank
       iebeam = NINT(ecm*500)
       jrlep = alrlep(iebeam,'    ',0,0,0)
C...Debug flags
      jdebu = IW(NAMIND('DEBU'))
      IF ( jdebu .GT. 0 ) THEN
        idb1 = IW(jdebu+1)
        idb2 = IW(jdebu+2)
      ENDIF
C
C  Initialize events counters
C
       DO 11 i = 1,11
   11  nevent(i) = 0
C
C  Print PART and KLIN banks
C
C     CALL PRPART
C
      CALL PRTABL('KPAR',0)
      CALL PRTABL('RLEP',0)
      CALL PRTABL('KHHG',0)
C
      RETURN
      END
      SUBROUTINE ASKUSE(IDPR,ISTA,NITR,NIVX,ECMS,WEIT)
C-----------------------------------------------------------------------
C! Event generation
C   B. Bloch add code to keep track of fragmentation info  and store it
C            in KZFR bank , March 97
C-----------------------------------------------------------------------
      DIMENSION qg1(4),qg2(4),qh(4),qa(4),ph(4),hh(4),qp(4),qm(4)
      DIMENSION qz(4),q1(4),q2(4),q3(4),ch(4),ijoin(2)
      DIMENSION ptrak(4,2)
      REAL*8 qin1(4),qin2(4),qout1(4),qout2(4),qout3(4)
      REAL DUMMY(4)
      DATA dummy/4*0./
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON / BCS / IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (L1MST=200, L1PAR=200)
      PARAMETER (L2PAR=500, L2PARF=2000 )
      PARAMETER (LJNPAR=4000)
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),
     &                KFDP(L2PARF,5)
      COMMON /LUDAT4/ CHAF(L2PAR)
      CHARACTER*8 CHAF
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5)
C
      COMMON / miscl / loutbe,ecm,idb1,idb2,xrad,empir,empirm,ism,
     &                 iklei,icar,sdvrt(3),vrtx(4),tabl(26),
     &                 nevent(11)
      INTEGER loutbe,idb1,idb2,nevent,ism,iklei,icar
      REAL*4 ecm,sdvrt,vrtx,tabl,empir,empirm,xrad
C
      PARAMETER (maxpro= 9)
      COMMON / cropro / cross(maxpro), sthr(maxpro), reduc(maxpro)
      CHARACTER*14 chapro(maxpro)
      DATA chapro /
     .             'e+e- --> h Z',
     .             'e+e- --> H Z',
     .             'e+e- --> h A',
     .             'e+e- --> H A',
     .             'W+W- --> h  ',
     .             'W+W- --> H  ',
     .             'Z Z  --> h  ',
     .             'Z Z  --> H  ',
     .             'e+e- --> H+H-'
     .                                /
      COMMON / prosim / iproyn(maxpro),nevpro(maxpro)
      DIMENSION brai(11), kcode(11), xmasmi(11)
      DATA brai/.0335,.0665,.0335,.0665,.0335,.0665,
     .          .1540,.1190,.1540,.1190,.1540/
      DATA kcode/11,12,13,14,15,16,1,2,3,4,5/
      DATA xmasmi/6*0.,0.3,0.3,1.0,4.0,11.0/
      COMMON / zzdec / braz(11), kselec(11), fracz
C
      EXTERNAL sigma1,sigma2,sigma3,sigma4
      EXTERNAL sigma5,sigma6,sigma7,sigma8,sigma9
      EXTERNAL sigmat
C
C
C  Initialization ASKUSE's arguments
C
      ista   = 0
      nitr   = 0
      nivx   = 0
      ecms   = ecm
      weit   = 1.
      sbeam  = ecm**2
      n7lu   = 0
      idpr   = 0
      ipro   = 0
      jchanh = 0
      jchana = 0
      ifs    = 0
C Reset fragmentation storage in common
      MSTU(90) = 0
C
C  Store beam particles also in bos banks
C
      ipart = kgpart(11)
      DO 2 itr = 1,2
         DO 9 i=1,4
 9       ptrak(i,itr) = 0.
         ipart = kgpart(11)
         ptrak(3,itr) = 0.5*ecm
         IF ( itr .EQ. 2 ) THEN
           ipart = kgpart(-11)
           ptrak(3,itr) =- 0.5*ecm
         ENDIF
         ist=kbkine(-itr,ptrak(1,itr),ipart,0)
         IF ( ist .LE. 0 ) THEN
            ista = -2
            GO TO 998
         ENDIF
  2   CONTINUE
C
C  Choice of the channel (if ipro = 0)
C
      sigtot = 0.
      DO ipro = 1 , maxpro
        sigtot = sigtot + iproyn(ipro)*cross(ipro)*reduc(ipro)
      ENDDO
C
      IF ( sigtot .EQ. 0. ) THEN
        WRITE(6,*) 'Not a single process has a non-vanishing',
     .             ' Cross section. Execution stopped.'
        STOP 99
      ENDIF
C
      rnch = RNDM(dummy)
      rint = 0.
      DO ipro = 1 , maxpro
        rint = rint + iproyn(ipro)*cross(ipro)*reduc(ipro) / sigtot
        if ( rnch .LT. rint ) GOTO 30
      ENDDO
  30  CONTINUE
C
C  Now generate the event
C
      IF ( cross(ipro)*reduc(ipro) .LE. 0. ) THEN
        WRITE(6,*) 'Warning : Process ',chapro(ipro),
     .  ' has a vanishing cross-section, Execution stopped'
        STOP 99
      ENDIF
C
      nevent(1) = nevent(1) + 1
C     WRITE(6,*) 'Event # ',nevent(1)
      nevpro(ipro) = nevpro(ipro) + 1
    1 CALL hzha(ipro,ecms,qg1,qg2,qh,qa,hh,izpol)
C
C  Radiative photon in the initial state ?
C
      IF ( xrad .GT. 0. ) THEN
        n7lu = n7lu + 1
        kfg = 22
        CALL hhlu1(n7lu,kfg,qg1(1),qg1(2),qg1(3),qg1(4),0.)
        k7lu(n7lu,1) = 1
        k7lu(n7lu,3) = 0
        n7lu = n7lu + 1
        kfg = 22
        CALL hhlu1(n7lu,kfg,qg2(1),qg2(2),qg2(3),qg2(4),0.)
        k7lu(n7lu,1) = 1
        k7lu(n7lu,3) = 0
      ENDIF
C
      IF ( ipro .GE. 5 .AND. ipro .NE. 9 ) GOTO 1000
C
C  Virtual exchange boson (gamma or Z0)
C
      n7lu = n7lu + 1
      kfz = 23
      CALL hhlu1(n7lu,kfz,-qg1(1)-qg2(1),-qg1(2)-qg2(2),
     .                    -qg1(3)-qg2(3),ecm-qg1(4)-qg2(4),-1.)
      k7lu(n7lu,1) = 11
      k7lu(n7lu,3) = 0
      k7lu(n7lu,4) = n7lu + 1
      k7lu(n7lu,5) = n7lu + 2
      ipo1 = n7lu
C
C  Fill the h (if IPRO = 1 or 3) or the H (if IPRO = 2 or 4)
C
      IF ( ipro .EQ. 1 .OR. ipro .EQ. 3 ) THEN
        kfh = 25
        jhig = 2
      ELSE IF( ipro .EQ. 9 ) THEN
        kfh = 37                       ! H+
      ELSE
        kfh = 35
        jhig = 1
      ENDIF
      n7lu = n7lu + 1
      qhe = SQRT(qh(1)**2+qh(2)**2+qh(3)**2+qh(4)**2)
      CALL hhlu1(n7lu,kfh,qh(1),qh(2),qh(3),qhe,qh(4))
      k7lu(n7lu,1) = 1
      k7lu(n7lu,3) = ipo1
      ipoh = n7lu
C
C   Fill the Z0 (if IPRO = 1 or 2) or the A (if IPRO = 3 or 4)
C
      IF ( ipro .LE. 2 ) THEN
        kfa = 23
      ELSE IF( ipro .EQ. 9 ) THEN
        kfa =-37                       ! H-
      ELSE
        kfa = 36
      ENDIF
      n7lu = n7lu + 1
      qae = SQRT(qa(1)**2+qa(2)**2+qa(3)**2+qa(4)**2)
      CALL hhlu1(n7lu,kfa,qa(1),qa(2),qa(3),qae,qa(4))
      k7lu(n7lu,1) = 1
      k7lu(n7lu,3) = ipo1
      ipoa = n7lu
C
C  Decay charged higgses if H+H- production
C
      IF( ipro .EQ. 9 ) THEN
        CALL hcdecc(ihpd,ihmd)
        CALL hcdecy(ipoh,ihpd,ist)
        IF( ist.NE.0 ) GOTO 1
        CALL hcdecy(ipoa,ihmd,ist)
        IF( ist.NE.0 ) GOTO 1
        GOTO 100
      END IF
C
C  Decay the Higgses
C
      CALL hhdecay(qh,jchanh,jhig,ipoh)
      IF ( ipro.EQ.3 .OR. ipro.EQ.4 ) CALL hhdecay(qa,jchana,3,ipoa)
      GOTO 100
C
C Now the WW/ZZ fusion
C
 1000 CONTINUE
C
C Fill the neutrinos (WW) or the e+e- pair (ZZ)
C
      IF ( ipro .EQ. 5 .OR. ipro .EQ. 6 ) THEN
        kfn = 12
      ELSEIF ( ipro .EQ. 7 .OR. ipro .EQ. 8 ) THEN
        kfn = 11
      ENDIF
      q5 = pmas(kfn,1)
C
      n7lu = n7lu + 1
      CALL hhlu1(n7lu, kfn,qh(1),qh(2),qh(3),qh(4),q5)
      k7lu(n7lu,1) = 1
      k7lu(n7lu,3) = 0
C
      n7lu = n7lu + 1
      CALL hhlu1(n7lu,-kfn,qa(1),qa(2),qa(3),qa(4),q5)
      k7lu(n7lu,1) = 1
      k7lu(n7lu,3) = 0
C
C Fill the Higgs
C
      IF ( ipro .EQ. 5 .OR. ipro .EQ. 7 ) THEN
        kfh = 25
        jhig = 2
      ELSEIF ( ipro .EQ. 6 .OR. ipro .EQ. 8 ) THEN
        kfh = 35
        jhig = 1
      ENDIF
C
      n7lu = n7lu + 1
      hhe = SQRT(hh(1)**2+hh(2)**2+hh(3)**2+hh(4)**2)
      CALL hhlu1(n7lu,kfh,hh(1),hh(2),hh(3),hhe,hh(4))
      k7lu(n7lu,1) = 1
      k7lu(n7lu,3) = 0
      ipoh = n7lu
C
C  Decay the Higgs
C
      CALL hhdecay(hh,jchanh,jhig,ipoh)
C
C Is there any Higgses in the decay products?
C
  100 nnhg = 0
      DO i7lu = 1 , n7lu
        IF ( ( k7lu(i7lu,2) .EQ. 25 .OR.
     .         k7lu(i7lu,2) .EQ. 35 .OR.
     .         k7lu(i7lu,2) .EQ. 36 ) .AND.
     .         k7lu(i7lu,1) .EQ. 1 ) THEN
          ph(1) = p7lu(i7lu,1)
          ph(2) = p7lu(i7lu,2)
          ph(3) = p7lu(i7lu,3)
          ph(4) = p7lu(i7lu,5)
          iph = i7lu
          nnhg = nnhg + 1
          IF ( k7lu(i7lu,2) .EQ. 35 ) khig = 1
          IF ( k7lu(i7lu,2) .EQ. 25 ) khig = 2
          IF ( k7lu(i7lu,2) .EQ. 36 ) khig = 3
          CALL hhdecay(ph,jchan,khig,iph)
        ENDIF
      ENDDO
      IF ( nnhg .GT. 0 ) GOTO 100
C
C Any neutralino or chargino in the decay products ?
C
  200 nnchi = 0
      DO i7lu = 1 , n7lu
        IF ( ABS(k7lu(i7lu,2)) .GE. 52 .AND.
     .       ABS(k7lu(i7lu,2)) .LE. 56 .AND.
     .         k7lu(i7lu,1) .EQ. 1 ) THEN
          ch(1) = p7lu(i7lu,1)
          ch(2) = p7lu(i7lu,2)
          ch(3) = p7lu(i7lu,3)
          ch(4) = p7lu(i7lu,4)
          ich = i7lu
          nnchi = nnchi + 1
          IF ( ABS(k7lu(i7lu,2)) .LE. 54 ) THEN
            ichi1 = k7lu(i7lu,2)-50
            ichi = 0
          ELSEIF ( k7lu(i7lu,2) .GE. 55 ) THEN
            ichi1 = k7lu(i7lu,2)-54
            ichi = 1
          ELSE
            ichi1 = -k7lu(i7lu,2)-54
            ichi = -1
          ENDIF
          CALL decchi(ch,ichi1,ichi,ich)
        ENDIF
      ENDDO
      IF ( nnchi .GT. 0 ) GOTO 200
C
C
C  Decay the Z (if any!)
C
      IF ( ipro .LT. 3 ) THEN
        IF ( iklei .EQ. 0 ) THEN
          CALL zdecay(qa,izpol,q1,q2,ifs)
        ELSE
          ifs = izpol
          DO i = 1,3
            q2(i) = hh(i)
            q1(i) = qa(i)-q2(i)
          ENDDO
        ENDIF
        kff = kcode(ifs)
        q5 = ulmass(kff)
        q1(4) = SQRT(q1(1)**2+q1(2)**2+q1(3)**2+q5**2)
        q2(4) = SQRT(q2(1)**2+q2(2)**2+q2(3)**2+q5**2)
C
C FSR for charged leptons
C
        IF ( ifs .EQ. 1 .OR. ifs .EQ. 3 .OR. ifs .EQ. 5 ) THEN
C
          DO id = 1, 4
             qin1(id) = DBLE(q1(id))
             qin2(id) = DBLE(q2(id))
          ENDDO
C
          CALL addglu(qin1,qin2,qout1,qout2,qout3,0)
C
C Put back the leptons
C
          DO ID = 1, 4
             q1(id) = REAL(qout1(id))
             q2(id) = REAL(qout2(id))
             q3(id) = REAL(qout3(id))
          ENDDO
C
C  Feed LUND with the 4-vectors
C
          k7lu(ipoa,4) = n7lu+1
          IF ( q3(4) .GT. 0. ) THEN
            n7lu = n7lu + 1
            kfg = 22
            CALL hhlu1(n7lu,kfg,q3(1),q3(2),q3(3),q3(4),0.)
            k7lu(n7lu,3) = ipoa
          ENDIF
C
        ENDIF
C
        n7lu = n7lu + 1
        ijoin(1) = n7lu
        CALL hhlu1(n7lu,kff,q1(1),q1(2),q1(3),q1(4),q5)
        k7lu(ijoin(1),3) = ipoa
C
        n7lu = n7lu + 1
        ijoin(2) = n7lu
        CALL hhlu1(n7lu,-kff,q2(1),q2(2),q2(3),q2(4),q5)
        k7lu(ijoin(2),3) = ipoa
        k7lu(ipoa,5) = n7lu
C
C  Prepare and execute parton shower for quarks
C
        xmm    = qa(4)
        IF ( kff .LT. 6 ) THEN
          k7lu(ijoin(1),1) = 2
          njoin = 2
          CALL lujoin(njoin,ijoin)
          CALL lushow(ijoin(1), ijoin(2), xmm)
        ENDIF
C
C Don/t decay the Z twice!
C
        k7lu(ipoa,1) = 11
C
      ENDIF
C
C  Other decays and fragmentation
C
      CALL luexec
C keep fragmentation info in bank KZFR
      CALL KZFRBK(IST)
      IF(IST.NE.0) ISTAT = -1
      IF (MSTU(24).GT.0) THEN
        IST   = -8
        CALL LULIST(1)
      ENDIF
C
C  Listing of the event
C
      IF ( nevent(1) .GE. idb1 .AND.
     .     nevent(1) .LE. idb2 ) CALL lulist(1)
C
C  Smear vertex position
C
      CALL rannor (rx,ry)
      CALL rannor (rz,dum)
      vrtx(1) = rx*sdvrt(1)
      vrtx(2) = ry*sdvrt(2)
      vrtx(3) = rz*sdvrt(3)
      vrtx(4) = 0.
C
C  Event header
C
      IF( ipro.LT.9 ) THEN
        idpr = MAX(ifs,jchanA) + 100*jchanh + 10000*ipro
      ELSE IF( ipro.EQ.9 ) THEN
        idpr = ihmd + 100*ihpd + 10000*ipro
      END IF
C
C      Call the specific routine KXL7AL to fill BOS banks
C
      CALL kxl7al (vrtx,ist,nivx,nitr)
      IF ( ist .NE. 0 ) GOTO 998
C
C  Event counters
C
  998 ista = ist
      IF ( ist .EQ. 0 ) nevent(2) = nevent(2) + 1
      IF ( ist .GT. 0) THEN
        nevent(3) = nevent(3) + 1
        nevent(4) = nevent(4) + 1
        WRITE(6,*) 'Evt ',nevent(1),' ist = ',ist
        CALL lulist(1)
      ELSEIF ( ist .LT. 0) THEN
        nevent(3) = nevent(3) + 1
        nevent(4-ist) = nevent(4-ist) + 1
      ENDIF
C
C Fill Histos for iproc = 9
C
      IF( ipro .EQ. 9 ) CALL hcfilh
C
      RETURN
      END
      SUBROUTINE USCJOB
C-----------------------------------------------------------------------
C! Routine for printout at the end of a run
C
C-----------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON / BCS / IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      PARAMETER (maxpro= 9)
      COMMON / cropro / cross(maxpro), sthr(maxpro), reduc(maxpro)
      CHARACTER*14 chapro(maxpro)
      DATA chapro /
     .             'e+e- --> h Z',
     .             'e+e- --> H Z',
     .             'e+e- --> h A',
     .             'e+e- --> H A',
     .             'W+W- --> h  ',
     .             'W+W- --> H  ',
     .             'Z Z  --> h  ',
     .             'Z Z  --> H  ',
     .             'e+e- --> H+H-'
     .                                /
      COMMON / prosim / iproyn(maxpro),nevpro(maxpro)
      COMMON / miscl / loutbe,ecm,idb1,idb2,xrad,empir,empirm,ism,
     &                 iklei,icar,sdvrt(3),vrtx(4),tabl(26),
     &                 nevent(11)
      INTEGER loutbe,idb1,idb2,nevent,ism,iklei,icar
      REAL*4 ecm,sdvrt,vrtx,tabl,empir,empirm,xrad
C
      REAL DUMMY(4)
C
      CALL hhfini
C
      DO ipr = 1, 4
        ipro = ipr + 4
        IF ( ntry(ipr) .GT. 0 ) THEN
          sigto = wtot(ipr)/FLOAT(ntry(ipr))
          dsig  = SQRT((wtot2(ipr)/FLOAT(ntry(ipr)) - sigto**2)
     .          / FLOAT(ntry(ipr)))
        WRITE(6,*) ' '
        WRITE(6,*) '--------------------------------------------------'
        WRITE(6,*) ' Generated cross section for ',chapro(ipro),' :'
        WRITE(6,*) '    Sigma = ',sigto,' +/- ',dsig,' fb'
        WRITE(6,*) '    # of trials          : ',ntry(ipr)
        WRITE(6,*) '    # of accepted events : ',nacc(ipr)
        WRITE(6,*) '--------------------------------------------------'
        WRITE(6,*) ' '
        ENDIF
      ENDDO
C
      WRITE(6,*) '--------------------------------------------------'
      WRITE(6,*) 'Number of events generated in each channel : '
      WRITE(6,*) '--------------------------------------------------'
      DO ipro = 1, maxpro
        WRITE(6,*) chapro(ipro),' : ',nevpro(ipro),' events.'
      ENDDO
      WRITE(6,*) '--------------------------------------------------'
C
C Maximum weight reached
C
      WRITE (LOUTBE,100) EMPIRM
  100 FORMAT (//1X,'+++++MAXIMUM WEIGHT REACHED   ++++++',F10.3)
C
C   tau decay summary if needed
      imstj = nlink('MSTJ',28)
      IF(imstj.gt.0)then
         if ( iw(imstj+1).eq.2) call taudkay(itau,ndec,100)
      endif
C
C Print event counters
C
       WRITE(LOUTBE,101)
  101  FORMAT(//20X,'EVENTS STATISTICS',
     &         /20X,'*****************')
       WRITE(LOUTBE,102) NEVENT(1),NEVENT(2),NEVENT(3)
  102  FORMAT(/5X,'# OF GENERATED EVENTS                      = ',I10,
     &        /5X,'# OF ACCEPTED  EVENTS                      = ',I10,
     &        /5X,'# OF REJECTED  EVENTS (ISTA # 0 in ASKUSE) = ',I10)
       WRITE(LOUTBE,103)
  103  FORMAT(//20X,'ERRORS STATISTICS',
     &         /20X,'*****************')
       WRITE(LOUTBE,104) (NEVENT(I),I=4,11)
  104  FORMAT(/10X,'IR= 1 LUND ERROR unknown part   # OF REJECT = ',I10,
     &        /10X,'IR= 2 KINE/VERT banks missing   # OF REJECT = ',I10,
     &        /10X,'IR= 3 no space for VERT/KINE    # OF REJECT = ',I10,
     &        /10X,'IR= 4 LUND ERROR too many tracks# OF REJECT = ',I10,
     &        /10X,'IR= 5 LUND ERROR Beam wrong pos # OF REJECT = ',I10,
     &        /10X,'IR= 6 LUND ERROR Status code >5 # OF REJECT = ',I10,
     &        /10X,'IR= 7 free for user             # OF REJECT = ',I10,
     &        /10X,'IR= 8 free for user             # OF REJECT = ',I10)
C
      RETURN
      END
      SUBROUTINE ADDGLU(QIN1,QIN2,QOUT1,QOUT2,QOUT3,IOPT)
C-----------------------------------------------------------
C
C!EVENT GENERATOR AFFIX TO GO FROM TWO TO THREE JETS IN THE
C!DECAY OF A HEAVY COLOUR-SINGLET VECTOR BOSON (Z,W,...)
C THE MOMENTA 'QIN' ARE INPUT, THE MOMENTA 'QOUT' ARE OUTPUT
C
CRK-----------------
C IOPT=0 : THE CORRECT ALGORITHM
C IOPT=1 : CHOOSE X'S WITH EQUAL PROBABILITY
C IOPT=2 : CHOOSE THE LARGEST OF THE X'S
C IOPT=3 : CHOOSE ALWAYS X1
C-----------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4 RNDM
      EXTERNAL RNDM
      DIMENSION QIN1(4),QIN2(4),QOUT1(4),QOUT2(4),QOUT3(4)
      DIMENSION Q(4),R1(4),R2(4),E1(3),E2(3)
      DATA PI/3.1415926536D0/
      DATA XLAM/0.2D0/
C
C CONSTRUCT THE OVERALL MOMENTUM FROM THE QIN, AND ITS MASS
      DO 101 K=1,4
  101 Q(K)=QIN1(K)+QIN2(K)
      W=DSQRT(Q(4)**2-Q(3)**2-Q(2)**2-Q(1)**2)
C
C CONSTRUCT QIN1,2 IN Q'S C.M. FRAME, CALL'EM R1,2
      R1(4)=.5*W
      R2(4)=W-R1(4)
      RR=(QIN1(4)+R1(4))/(Q(4)+W)
      DO 102 K=1,3
      R1(K)=QIN1(K)-RR*Q(K)
  102 R2(K)=-R1(K)
C
C COMPUTE THE VALUE OF ALFA(S), ASSUMING THREE
C QUARK GENERATRIONS AND LAMBDA(QCD)=0.2 GEV
C     ALFAS=6.*PI/27./DLOG(W/XLAM)
      ALFAS=1.D0/137.036D0
C
C COMPUTE GAMMA, ASSUMING THE SMALL-GAMMA APPROXIMATION IS VALID
C (CHECKED OUT AS HELPED BY ROEL'S ZX SPECTRUM)
      GG=2.5-PI**2/3.-3.*(ALFAS+PI)/(2.*ALFAS)
      G=DEXP(.75*(-1.-DSQRT(1.-8.*GG/9.)))
C      PRINT *,'GAMMA =',G
C      IF(1.EQ.1) STOP
C
C GENERATE THE VALUES FOR X1 AND X2 BY MAPPING AND W.R.P.
  301 X1=1.-(1.-G)/DEXP(RNDM(1)*DLOG((1.-G)/G))
      X2=1.-(1.-G)/DEXP(RNDM(2)*DLOG((1.-G)/G))
      IF((X1+X2).GT.1.D0) GOTO 302
      X1=1.-X1
      X2=1.-X2
  302 CONTINUE
      WEIGHT=.5*X1*X2*(X1*X1+X2*X2)/(X1*X2+(1.-X1)*(1.-X2))
      WMAX=(1.-G)**4/((1.-G)**2+G**2)
      IF(WMAX.LT.WEIGHT) PRINT 303,G,X1,X2,WMAX,WEIGHT
  303 FORMAT(' WEIGHT ANOMALY:',5D15.6)
      IF(RNDM(3).GT.(WEIGHT/WMAX)) GOTO 301
C
C CHOOSE BETWEEN THE TWO POSSIBLE ORIENTATIONS
C OPTIONS 1,2 AND 3 ARE WRONG: OPTION 0 GIVES THE CORRECT RESULT
CRK-------------
      IF(IOPT.NE.0) GOTO 351
      XX=X1**2/(X1**2+X2**2)
C      CALL HISTO1(4,10,0.D0,1.D0,XX,1.D0)
      IF(RNDM(4).GT.XX) GOTO 501
      GOTO 401
CRK-------------
  351 IF(IOPT.NE.1) GOTO 352
      IF(RNDM(4).GT.0.5D0) GOTO 501
      GOTO 401
CRK-------------
  352 IF(IOPT.NE.2) GOTO 353
      IF(X2.GT.X1) GOTO 501
      GOTO 401
CRK-------------
  353 IF(IOPT.NE.3) GOTO 354
      GOTO 401
CRK-------------
  354 PRINT 355,IOPT
  355 FORMAT(' WRONG OPTION: IOPT=',I10)
      STOP
CRK-------------
C
C CASE A: Q1 RETAINS ITS ORIGINAL DIRECTION
C COMPUTE THE ANGULAR PARAMETERS OF THE Q1 DIRECTION...
  401 DO 402 K=1,3
  402 E1(K)=R1(K)/R1(4)
      C=E1(3)
      S=DSQRT(1.-C**2)
      CF=E1(2)/S
      SF=E1(1)/S
C ...THE ANGLES OF Q2 W.R.T Q1...
      C12=1.-2./X1-2./X2+2./X1/X2
      S12=DSQRT(1.-C12**2)
      PSI=2.*PI*RNDM(5)
      CP=DCOS(PSI)
      SP=DSIN(PSI)
C ...AND COMPUTE THE DIRECTION OF Q2
      E2(1)=SF*(C*S12*SP+S*C12)-CF*S12*CP
      E2(2)=CF*(C*S12*SP+S*C12)+SF*S12*CP
      E2(3)=-S*S12*SP+C*C12
      GOTO 601
C
C CASE B: Q2 RETAINS ITS ORIGINAL DIRECTION
C COMPUTE THE ANGULAR PARAMETERS OF THE Q2 DIRECTION...
  501 DO 502 K=1,3
  502 E2(K)=R2(K)/R2(4)
      C=E2(3)
      S=DSQRT(1.-C**2)
      CF=E2(2)/S
      SF=E2(1)/S
C ...THE ANGLES OF Q1 W.R.T Q2...
      C12=1.-2./X1-2./X2+2./X1/X2
      S12=DSQRT(1.-C12**2)
      PSI=2.*PI*RNDM(5)
      CP=DCOS(PSI)
      SP=DSIN(PSI)
C ...AND COMPUTE THE DIRECTION OF Q1
      E1(1)=SF*(C*S12*SP+S*C12)-CF*S12*CP
      E1(2)=CF*(C*S12*SP+S*C12)+SF*S12*CP
      E1(3)=-S*S12*SP+C*C12
C
C RETURN FROM BOTH CASES: CONSTRUCT Q1 AND Q2 IN THE C.M. FRAME...
  601 R1(4)=X1*R1(4)
      R2(4)=X2*R2(4)
      DO 602 K=1,3
      R1(K)=R1(4)*E1(K)
  602 R2(K)=R2(4)*E2(K)
C ...BOOST'EM TO THE LAB FRAME...
      QOUT1(4)=(Q(4)*R1(4)+Q(3)*R1(3)+Q(2)*R1(2)+Q(1)*R1(1))/W
      QOUT2(4)=(Q(4)*R2(4)+Q(3)*R2(3)+Q(2)*R2(2)+Q(1)*R2(1))/W
      QQ1=(QOUT1(4)+R1(4))/(Q(4)+W)
      QQ2=(QOUT2(4)+R2(4))/(Q(4)+W)
      DO 603 K=1,3
      QOUT1(K)=R1(K)+Q(K)*QQ1
  603 QOUT2(K)=R2(K)+Q(K)*QQ2
C ...AND CONSTRUCT THE GLUON MOMENTUM BY MOMENTUM CONSERVATION
      DO 605 K=1,4
  605 QOUT3(K)=Q(K)-QOUT1(K)-QOUT2(K)
      RETURN
      END
      FUNCTION brelec(xx)
C-------------------------------------------------------------------
C! Compute the decay branching ratio of W* --> e nue
C  when produced in the chi' --> chi+ W* decay.
C
C  Input:     -- xx,  the mass difference chi'-chi+
C
C  Output:    -- brelec, the decay branching ratio
C
C  V. Bertin, for the CHA001 generator
C
C  Modif:  Patrick Janot (31 Aug 1995)
C          Adapt the routine for the HZHAxx generator
C-------------------------------------------------------------------
      DIMENSION seuil(4),brtot(5)
      DATA seuil/.106,.140,1.8,2.0/
      DATA brtot/1.,2.,5.,6.,9./
C
      br = brtot(1)
      DO is = 1,4
       IF ( xx .GE. seuil(is) ) br = brtot(is+1)
      ENDDO
      brelec = 1. / br
C
      RETURN
      END
      FUNCTION brnunu(xh,xl,icp)
C-------------------------------------------------------------------
C! Compute the decay branching ratio of Z* --> nu nubar
C  when produced in the chi' --> chi Z* decay.
C
C  Input:     -- xh,  the first neutralino mass
C             -- xl,  the second neutralino mass
C             -- icp, the relative mass sign
C
C  Output:    -- brnunu, the decay branching ratio
C
C  V. Bertin, for the CHA001 generator
C
C  Modif:  Patrick Janot (31 Aug 1995)
C          Adapt the routine for the HZHAxx generator
C-------------------------------------------------------------------
      REAL*8 xh,xl
      DIMENSION dlim(19),bnup(20),bnum(20)
      DATA dlim/.001,.212,.270,.988,1.5,2.,3.,4.,5.,7.,8.,10.,
     .          12.,15.,20.,25.,30.,60.,90./
      DATA bnup/1.,.856,.774,.600,.497,.356,.323,.303,.286,.271,.256,
     .          .256,.248,.234,.222,.213,.209,.2075,.205,.204/
      DATA bnum/1.,.857,.729,.465,.389,.289,.275,.278,.248,.234,.232,
     .          .232,.232,.217,.209,.203,.203,.203,.203,.204/
C
      dx = xh-xl
      IF ( icp .GE. 0 ) brneu = bnup(1)
      IF ( icp .LT. 0 ) brneu = bnum(1)
      DO idm = 1,18
       IF ( dx .LE. dlim(3) ) THEN
        IF ( dx .GT. dlim(idm) .AND. dx .LE. dlim(idm+1) ) THEN
         IF ( icp .GE. 0 ) brneu = bnup(idm+1)
         IF ( icp .LT. 0 ) brneu = bnum(idm+1)
        ENDIF
       ELSE IF ( dx .LT. dlim(19) ) THEN
        IF ( dx .GT. dlim(idm) .AND. dx .LE. dlim(idm+1) ) THEN
         IF ( icp .GE. 0 ) THEN
          brneu = (bnup(idm+1) - bnup(idm)) * (dx - dlim(idm))
     .           / (dlim(idm+1) - dlim(idm)) + bnup(idm)
         ELSE
          brneu = (bnum(idm+1) - bnum(idm)) * (dx - dlim(idm))
     .           / (dlim(idm+1) - dlim(idm)) + bnum(idm)
         ENDIF
        ENDIF
       ELSE
        IF ( icp .GE. 0 ) brneu = bnup(20)
        IF ( icp .LT. 0 ) brneu = bnum(20)
       ENDIF
      ENDDO
C
      brnunu = brneu
C
      RETURN
      END
      FUNCTION brwipj(a,b,c,d)
C ------------------------------------------------------------------
C! Perfom a 1-dim convolution Sigma(hZ) * Breit Wigner
C
C  Inputs:        a   is ecm**2
C                 b   is the Higgs mass
C                 c,  is the Higgs width
C
C  Output:        brwipj, the hZ cross section with width effects
C
C  Patrick Janot -- 01 Sep 1995
C -------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4 a,b,c
      COMMON /BHV/ s,am1,w1
      EXTERNAL fsub
      DIMENSION x(1)
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
C
      s      = a
      am1    = b
      w1     = c
      zma    = d
      xlo    = -DATAN2(am1,w1)
      xhi1   = xlo
      IF ( s .GT. zma**2 )
     .    xhi1   =  DATAN2((SQRT(s)-zma)**2-am1**2,am1*w1)
      xhi2   =  DATAN2(s-am1**2,am1*w1)
C
      brwipj = 0D0
C
      IF ( xhi1 .GT. xlo )
     .    brwipj = brwipj + DGMLT1(fsub,xlo,xhi1,1,6,x)
      IF ( xhi2 .GT. xhi1 )
     .    brwipj = brwipj + DGMLT1(fsub,xhi1,xhi2,1,6,x)
C
      brwipj = brwipj / (piby2+DATAN2(am1,w1))
C
      RETURN
      END
      FUNCTION brwisi(a,b,c,d,e)
C -------------------------------------------------------------------
C! Perfom a 2-dim convolution Sigma(hA)*BreitWigner(h)*BreitWigner(A)
C
C  Inputs          a   is ecm**2
C                  b,c are h,A on-shell masses
C                  d,e are h,A widths
C
C  Output:         brwisi, the hA cross section with width effects
C
C  Patrick Janot -- 01 Sep 1995
C -------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4 a,b,c,d,e
      COMMON /BWC/ s,am1,am2,w1,w2
      EXTERNAL fsub2
      DIMENSION x(2)
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
C
      s      = a
      am1    = b
      am2    = c
      w1     = d
      w2     = e
      x2lo   = -DATAN2(am2,w2)
      x2hi   =  DATAN2(s-am2**2,am2*w2)
      brwisi = DGMLT2(fsub2,x2lo,x2hi,1,6,x)
     .       / (piby2+DATAN2(am1,w1))
     .       / (piby2+DATAN2(am2,w2))
C
      RETURN
      END
      FUNCTION brwwzz(a,b,c,ityp)
C ------------------------------------------------------------------
C! Compute the decay width of a Higgs boson into W+W- and Z Z
C  with a 2-dim convolution phase space * Breit Wigner
C
C  Inputs :   a   is mh (Higgs mass)
C             b   is mv (Boson mass)
C             c   is gv (Boson width)
C             ityp = 1 or 2 for two different ways of copmputing
C
C Patrick Janot. 19 Sep 1995.
C -------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4 a,b,c
      COMMON /BWC/ s,am1,am2,w1,w2
      EXTERNAL fsubwz2
      DIMENSION x(2)
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
C
      s      = a**2
      am1    = b
      am2    = b
      w1     = c
      w2     = c
      x2lo   = -DATAN2(am2,w2)
      x2hi   =  DATAN2(s-am2**2,am2*w2)
      brwwzz = DGMLT2(fsubwz2,x2lo,x2hi,4,8,x)
C    .       / (x2lo-x2hi)**2
     .       / (piby2+DATAN2(am1,w1))
     .       / (piby2+DATAN2(am2,w2))
C
      RETURN
      END
      SUBROUTINE bwgene(xmin,xmax,x,g,w,dj)
C-----------------------------------------------------------------
C! Generate w with a Breit-Wigner probability, centered on x
C  and of width g. Variable dj is the corresponding Jacobian.
C
C  Patrick Janot -- 15 Oct. 1992
C-----------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4 xmin,xmax,x,g,w,dj,RNDM
      EXTERNAL RNDM
C
      argmin = (xmin**2-x**2)/(x*g)
      argmax = (xmax**2-x**2)/(x*g)
      w2 = x**2 + x * g *  DTAN (
     .                   ( DATAN(argmax) - DATAN(argmin) )
     .                   * RNDM(w2)
     .                   + DATAN(argmin) )
C
      IF ( w2 .LT. 0 ) THEN
        w = (xmin+xmax)/2.
      ELSE
        w  = DSQRT(w2)
      ENDIF
C
      dj = DATAN(argmax)-DATAN(argmin)
      dj = dj / (x*g)
C
      RETURN
      END
      SUBROUTINE chaneucp
C------------------------------------------------------------------
C!  Compute neutralino and chargino couplings to the Higgses
C
C  Input:    /PARAM/ MSSM parameters
C
C  Output:   /PARAM/ aa1,aa2,aa3, the couplings to the neutral
C                                 Higgs bosons
C
C  P. Janot -- 4 December 1994
C------------------------------------------------------------------
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
C
C Couplings F_ijh of the neutral Higgs boson "h" to a neutralino
C pair chi_i,chi_j ("h" = generic name for H,h,A)
C
      DO 1 i=1,4
        DO 2 j=1,4
          aa(1,i,j) = ( qqmat(i,j)*ca-ssmat(i,j)*sa)/2.
          aa(2,i,j) = ( qqmat(i,j)*sa+ssmat(i,j)*ca)/2.
          aa(3,i,j) = (-qqmat(i,j)*sb+ssmat(i,j)*cb)/2.
    2   CONTINUE
    1 CONTINUE
C
C Couplings F_ijh of the neutral Higgs boson "h" to a chargino
C pair chi+_i,chi-_j ("h" = generic name for H,h,A)
C
      DO 3 i=1,2
        DO 4 j=1,2
          bb(1,i,j) = ( ca*vmat(i,1)*umat(j,2)
     .                 +sa*vmat(i,2)*umat(j,1))/SQRT(2.)
          bb(2,i,j) = (-sa*vmat(i,1)*umat(j,2)
     .                 +ca*vmat(i,2)*umat(j,1))/SQRT(2.)
          bb(3,i,j) = (-sb*vmat(i,1)*umat(j,2)
     .                 -cb*vmat(i,2)*umat(j,1))/SQRT(2.)
    4   CONTINUE
    3 CONTINUE
C
      RETURN
      END
      SUBROUTINE chargi
C------------------------------------------------------------------
C!  Compute the chargino masses from the MSSM parameters
C
C  Input:    /PARAM/ MSSM parameters
C
C  Output:   /PARAM/ amchar(2) the chargino masses
C
C
C  P. Janot -- 4 December 1994
C------------------------------------------------------------------
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
C
      COMMON / elweak / sw2,alpha(0:nhig),gweak2(0:nhig),
     .                  alphas(0:nhig),g_f,deltar,alpha2,sw,cw2,cw
C
      COMMON / miscl / loutbe,ecm,idb1,idb2,xrad,empir,empirm,ism,
     &                 iklei,icar,sdvrt(3),vrtx(4),tabl(26),
     &                 nevent(11)
      INTEGER loutbe,idb1,idb2,nevent,ism,iklei,icar
      REAL*4 ecm,sdvrt,vrtx,tabl,empir,empirm,xrad
C
      DIMENSION u(2,2),v(2,2)
C
C The chargino mass matrix
C
      a11 = susM2
      a12 = SQRT(2.)*amw*sb
      a21 = SQRT(2.)*amw*cb
      a22 = susMU
C
C The angle of the two diagonalization matrices
C
      r1   = 2.*(a22*a12+a11*a21)
      t1   = a22**2-a11**2+a21**2-a12**2
      phi1 = ATAN2(r1,t1)/2.
      c1   = COS(phi1)
      s1   = SIN(phi1)
C
      r2   = 2.*(a22*a21+a11*a12)
      t2   = a22**2-a11**2+a12**2-a21**2
      phi2 = ATAN2(r2,t2)/2.
      c2   = COS(phi2)
      s2   = SIN(phi2)
C
C The eigenvalues (=the chargino masses)
C
      amchar(1) =  a11*c1*c2+a22*s1*s2-a12*c1*s2-a21*s1*c2
      amchar(2) =  a11*s1*s2+a22*c1*c2+a12*s1*c2+a21*c1*s2
C
C Check for mass positivity and mass ordering
C
      eps1 = SIGN(1.,amchar(1))
      eps2 =-SIGN(1.,amchar(2))
C
C The diagonalization matrices U and V
C
      IF ( ABS(amchar(2)) .LE. ABS(amchar(1)) ) THEN
        u(1,1) = -s1
        u(1,2) = -c1
        u(2,1) =  c1
        u(2,2) = -s1
        v(1,1) =  s2*eps2
        v(1,2) =  c2*eps2
        v(2,1) =  c2*eps1
        v(2,2) = -s2*eps1
      ELSE
        u(1,1) = -c1
        u(1,2) =  s1
        u(2,1) = -s1
        u(2,2) = -c1
        v(1,1) = -c2*eps1
        v(1,2) =  s2*eps1
        v(2,1) =  s2*eps2
        v(2,2) =  c2*eps2
      ENDIF
      CALL ucopy(u(1,1),umat(1,1),2*2)
      CALL ucopy(v(1,1),vmat(1,1),2*2)
C
C The mass after ordering
C
      amchar(1) = u(1,1)*a11*v(1,1)
     .          + u(1,1)*a12*v(1,2)
     .          + u(1,2)*a21*v(1,1)
     .          + u(1,2)*a22*v(1,2)
      amchar(2) = u(2,1)*a11*v(2,1)
     .          + u(2,1)*a12*v(2,2)
     .          + u(2,2)*a21*v(2,1)
     .          + u(2,2)*a22*v(2,2)
C
      RETURN
      END
      FUNCTION chicha(ichip,ichar,isens)
C-------------------------------------------------------------------
C! Compute the decay width of chi --> chi+ W* or chi+ --> chi W*
C  where chi' is any of the four neutralinos.
C
C  Input:     -- ichip, the neutralino index
C             -- ichar, the chargino index
C             -- isens, = 1 for chi  --> chi+ W*
C                       = 2 for chi+ --> chi  W*
C
C  Output:    -- chicha, the decay width in GeV
C
C  V. Bertin, for the CHA001 generator
C
C  Modif:  Patrick Janot (31 Aug 1995)
C          Adapt the routine for the HZHAxx generator
C-------------------------------------------------------------------
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      COMMON / elweak / sw2,alpha(0:nhig),gweak2(0:nhig),
     .                  alphas(0:nhig),g_f,deltar,alpha2,sw,cw2,cw
C
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
C
      REAL*8 a,g,xx,xip,clr2,cltr, DGMLT1, xlo,xhi,x(1)
      COMMON / chachi / a,g,xx,xip,clr2,cltr
      EXTERNAL dgmlt1, fsubcha
C
C     PRINT *,'ichii :',ichip
      chicha = 0.
      IF ( isens .EQ. 1 ) THEN
        xip = ABS(amneut(ichip))
        dif = (xip - amchar(ichar))
        xx = amchar(ichar) / amneut(ichip)
        IF ( dif .LT. .0005 ) GOTO 999
      ELSEIF ( isens .EQ. 2 ) THEN
        xip = ABS(amchar(ichar))
        dif = (xip - ABS(amneut(ichip)))
        xx = amneut(ichip) / amchar(ichar)
        IF ( dif .LT. .0005 ) GOTO 999
      ELSE
        GOTO 999
      ENDIF
C
      copl = fieldn(2,ichip)*vmat(ichar,1)
     .     - fieldn(4,ichip)*vmat(ichar,2)/SQRT(2.)
      copr = fieldn(2,ichip)*umat(ichar,1)
     .     + fieldn(3,ichip)*umat(ichar,2)/SQRT(2.)
      clr2 = copl**2 + copr**2
      cltr = copl * copr
C     PRINT *,'copl copr clr2 cltr :',copl,copr,clr2,cltr
C
      a = amw
      g = gmw
      xlo    = -DATAN2(a/xip,g/xip)
      xhi    =  DATAN2((1D0-DABS(xx))**2-(a/xip)**2,a*g/xip**2)
      partot = DGMLT1(fsubcha,xlo,xhi,1,6,x)
     .       / ( a * g / xip**2 )
C
      brel = brelec(dif)
C
      chicha = alpha(0)**2 * xip * partot / brel
     .       / (48.*pi*sw2**2)
C     PRINT *,'gamcha partot brel :',gamcha,partot,brel
      RETURN
C
 999  CONTINUE
      RETURN
      END
      SUBROUTINE chidec
C------------------------------------------------------------------
C! Compute the decay width and branching ratios of the neutralinos
C  into Z* chi, W*-/+ chi+/- and chi gamma, and of the charginos
C  yet into W* chi only ( Z* chi+ to come)
C
C  Patrick Janot -- 31 August 1995
C------------------------------------------------------------------
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      CHARACTER*14 channel
      CHARACTER*21 channeut, chanchar
      PARAMETER(nchneut=8,nchchar=5)
      COMMON / hhdeck / branch(nchan,nhig),width(nhig),
     .                  parwth(nhig),xymas(2,nchan,nhig),
     .                  xywid(2,nchan,nhig)
      COMMON / chaneu / ichn(2),
     .                  wneut(4,4,nhig), wchar(2,2,nhig),
     .                  widneut(4), brneut(nchneut,4),
     .                  widchar(2), brchar(nchchar,2)
      COMMON / chanch / channeut(nchneut,4), chanchar(nchchar,2)
      COMMON / vect4 / pvect4(5,2)
      COMMON / hinput / ichan(nchan,nhig), channel(nchan,nhig)
      COMMON / hisbr / bchpp,bchgg
      DIMENSION ph(4)
C
C
      CALL vzero(brneut(1,1),4*nchneut)
      CALL vzero(brchar(1,1),2*nchchar)
      CALL vzero(widneut(1),4)
      CALL vzero(widchar(1),2)
C
      DO ineut = 1, 4
C
C chi' --> chi gamma
C
        DO jneut = 1, ineut-1
          brneut(jneut,ineut) = chipho(ineut,jneut)
        ENDDO
C
C chi' --> chi Z*
C
        DO jneut = 1, ineut-1
          brneut(3+jneut,ineut) = chizst(ineut,jneut)
        ENDDO
C
C chi' --> chi+/- W*-/+
C chi+ --> chi'   W*-/+
C
        DO jchar = 1, 2
          IF ( ABS(amneut(ineut)) .GT. ABS(amchar(jchar)) ) THEN
            brneut(6+jchar,ineut) = chicha(ineut,jchar,1)
          ELSE
            brchar(ineut,jchar) = chicha(ineut,jchar,2)
          ENDIF
        ENDDO
C
      ENDDO
C
C chi+(2) --> chi+(1) Z*-/+  ! Not implemented yet
C
CCCC  brchar(5,2) = chazst(1.)
C
C Total width for the neutralinos
C
      DO ineut = 1, 4
        widneut(ineut) = 0.
        DO ic = 1 , nchneut
          widneut(ineut) = widneut(ineut) + brneut(ic,ineut)
        ENDDO
C
C And branching ratios
C
        IF ( widneut(ineut) .NE. 0. ) THEN
          DO ic = 1 , nchneut
            brneut(ic,ineut) = brneut(ic,ineut)/widneut(ineut)
          ENDDO
        ENDIF
C
      ENDDO
C
C Total width for the charginos
C
      DO ichar = 1, 2
        widchar(ichar) = 0.
        DO ic = 1 , nchchar
          widchar(ichar) = widchar(ichar) + brchar(ic,ichar)
        ENDDO
C
C And branching ratios
C
        IF ( widchar(ichar) .NE. 0. ) THEN
          DO ic = 1 , nchchar
            brchar(ic,ichar) = brchar(ic,ichar)/widchar(ichar)
          ENDDO
        ENDIF
C
      ENDDO
C
C Here we go !
C
      channeut(7,1) = 'chi10 --> chi1+/- W  '
      channeut(8,1) = 'chi10 --> chi2+/- W  '
      channeut(1,2) = 'chi20 --> chi10 gamma'
      channeut(4,2) = 'chi20 --> chi10   Z  '
      channeut(7,2) = 'chi20 --> chi1+/- W  '
      channeut(8,2) = 'chi20 --> chi2+/- W  '
      channeut(1,3) = 'chi30 --> chi10 gamma'
      channeut(2,3) = 'chi30 --> chi20 gamma'
      channeut(4,3) = 'chi30 --> chi10   Z  '
      channeut(5,3) = 'chi30 --> chi20   Z  '
      channeut(7,3) = 'chi30 --> chi1+/- W  '
      channeut(8,3) = 'chi30 --> chi2+/- W  '
      channeut(1,4) = 'chi40 --> chi10 gamma'
      channeut(2,4) = 'chi40 --> chi20 gamma'
      channeut(3,4) = 'chi40 --> chi30 gamma'
      channeut(4,4) = 'chi40 --> chi10   Z  '
      channeut(5,4) = 'chi40 --> chi20   Z  '
      channeut(6,4) = 'chi40 --> chi30   Z  '
      channeut(7,4) = 'chi40 --> chi1+/- W  '
      channeut(8,4) = 'chi40 --> chi2+/- W  '
C
      chanchar(1,1) = 'chi1+ --> chi10   W  '
      chanchar(2,1) = 'chi1+ --> chi20   W  '
      chanchar(3,1) = 'chi1+ --> chi30   W  '
      chanchar(4,1) = 'chi1+ --> chi40   W  '
      chanchar(1,2) = 'chi2+ --> chi10   W  '
      chanchar(2,2) = 'chi2+ --> chi20   W  '
      chanchar(3,2) = 'chi2+ --> chi30   W  '
      chanchar(4,2) = 'chi2+ --> chi40   W  '
      chanchar(5,2) = 'chi2+ --> chi1+   Z  '
C
      RETURN
      END
      SUBROUTINE chideca(p1,ichi1,ichi,ichi2,jchi,ifn)
C------------------------------------------------------------------
C!  Derive the quadrimomenta of the neutral/charg-ino decay
C   particles
C
C   Input:   -- p1(4),  the quadri-momentum  of the initial
C                       charg/neutral-ino
C            -- ichi1,  its index
C            -- ichi,   its electric charge (absolute value)
C            -- ichi2,  the index of the final charg/neutral-ino
C            -- jchi,   its electric charge
C            -- ifn,    the final state boson index
C                        = 0 for a photon
C                        = 1 for a Z
C                        = 2 for a W
C
C   Output:  -- pvect4,  the quadri-momenta of the final state
C                        particles, stored in /VECT4/
C
C  P. Janot --  31 Aug 1995
C------------------------------------------------------------------
      DIMENSION p1(4),p2(4),p3(4)
      REAL*8 betax,betay,betaz
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      CHARACTER*14 channel
      CHARACTER*21 channeut, chanchar
      PARAMETER(nchneut=8,nchchar=5)
      COMMON / hhdeck / branch(nchan,nhig),width(nhig),
     .                  parwth(nhig),xymas(2,nchan,nhig),
     .                  xywid(2,nchan,nhig)
      COMMON / chaneu / ichn(2),
     .                  wneut(4,4,nhig), wchar(2,2,nhig),
     .                  widneut(4), brneut(nchneut,4),
     .                  widchar(2), brchar(nchchar,2)
      COMMON / chanch / channeut(nchneut,4), chanchar(nchchar,2)
      COMMON / vect4 / pvect4(5,2)
      COMMON / hinput / ichan(nchan,nhig), channel(nchan,nhig)
      COMMON / hisbr / bchpp,bchgg
      DIMENSION ph(4)
C
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
C
C
      CALL vzero(pvect4(1,1),10)
C
C Masses of the two charg/neutral-inos
C
      IF ( ichi .EQ. 0 ) THEN
        chi1mas = ABS(amneut(ichi1))
      ELSE
        chi1mas = ABS(amchar(ichi1))
      ENDIF
C
      IF ( jchi .EQ. 0 ) THEN
        chi2mas = ABS(amneut(ichi2))
      ELSE
        chi2mas = ABS(amchar(ichi2))
      ENDIF
      IF ( idbg .GE. 10 ) THEN
        WRITE(6,*) ' +++ CHIDECA +++ '
        WRITE(6,*) 'am1/am2 : ',chi1mas,chi2mas
      ENDIF
C
C Generate the mass of the final boson according to a Breit-Wigner
C
    1 CONTINUE
      IF ( ifn .EQ. 0 ) THEN
        zstarm = 0.
      ELSE
        zstarmx = chi1mas-chi2mas
        zstarmn = 0.
        IF ( ifn .EQ. 1 ) THEN
          amm = amz
          gmm = gmz
        ELSE
          amm = amw
          gmm = gmw
        ENDIF
        CALL bwgene(zstarmn,zstarmx,amm,gmm,zstarm,djdum)
        IF ( zstarm .GE. zstarmx .OR. zstarm .LE. zstarmn ) GOTO 1
      ENDIF
      IF ( idbg .GE. 10 ) THEN
        WRITE(6,*) ' +++ CHIDECA +++ '
        WRITE(6,*) 'zstarm : ',zstarm
      ENDIF
C
C Compute the phase space factor
C
      phspace = SQRT( (chi1mas**2-(chi2mas+zstarm)**2)
     .               *(chi1mas**2-(chi2mas-zstarm)**2) )
     .        / (2. * chi1mas )
      phspmax = (chi1mas**2 - chi2mas**2)
     .        / (2. * chi1mas )
C
C Select according phase space
C
      IF ( phspace .LT. phspmax * RNDM(zstarm) ) GOTO 1
C
C The boson quadri-momentum (assuming a uniform decay)
C
      phistr = 2.*pi*RNDM(zstarm)
      costar = RNDM(zstarm)
      sintar = SQRT(1.-costar**2)
      p3(1) = phspace * sintar * COS(phistr)
      p3(2) = phspace * sintar * SIN(phistr)
      p3(3) = phspace * costar
      p3(4) = SQRT(phspace**2+zstarm**2)
C
C The charg/neutral-ino quadri-momentum
C
      p2(1) = -p3(1)
      p2(2) = -p3(2)
      p2(3) = -p3(3)
      p2(4) = SQRT(phspace**2+chi2mas**2)
      IF ( idbg .GE. 10 ) THEN
        WRITE(6,*) ' +++ CHIDECA +++ Avant boost '
        WRITE(6,*) ' p2    : ',p2
        WRITE(6,*) ' p3    : ',p3
        WRITE(6,*) ' p2+p3 : ',(p2(i)+p3(i),i=1,4)
      ENDIF
C
C Boost in the LAB frame
C
      betax = -p1(1)/p1(4)
      betay = -p1(2)/p1(4)
      betaz = -p1(3)/p1(4)
      CALL lorenz(betax,betay,betaz,p2)
      CALL lorenz(betax,betay,betaz,p3)
      CALL ucopy(p2(1),pvect4(1,1),4)
      CALL ucopy(p3(1),pvect4(1,2),4)
      pvect4(5,1) = chi2mas
      pvect4(5,2) = zstarm
C
      IF ( idbg .GE. 10 ) THEN
        WRITE(6,*) ' +++ CHIDECA +++ Apres boost'
        WRITE(6,*) ' p2    : ',p2
        WRITE(6,*) ' p3    : ',p3
        WRITE(6,*) ' p2+p3 : ',(p2(i)+p3(i),i=1,4)
        WRITE(6,*) ' p1    : ',p1
      ENDIF
  999 RETURN
      END
      FUNCTION chipho(ichip, ichi)
C-------------------------------------------------------------------
C! Compute the decay width of chi' --> chi gamma
C  where chi' and chi are any two of the four neutralinos.
C
C  Input:     -- ichip, the first neutralino index
C             -- ichi,  the second neutralino index
C
C  Output:    -- chipho, the decay width in GeV
C
C  V. Bertin, for the CHA001 generator
C
C  Modif:  Patrick Janot (31 Aug 1995)
C          Adapt the routine for the HZHAxx generator
C          In particular, the sfermion and Higgs masses have
C          been computed with the MSSM relations instead of
C          being entered with DATA statements
C-------------------------------------------------------------------
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      COMMON / elweak / sw2,alpha(0:nhig),gweak2(0:nhig),
     .                  alphas(0:nhig),g_f,deltar,alpha2,sw,cw2,cw
C
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
C
      REAL*8 fi,fj,fk,fi2
      EXTERNAL fi,fj,fk,fi2
      DIMENSION amsf(8),amf(8),ch(8),color(8)
      DIMENSION fielnp(4),fielnm(4)
      DATA color/3.,3.,3.,3.,3.,1.,1.,1./
C
      chigam = 0.
c     PRINT *,'ichii ichio :',ichip,ichi
C
C-- Masses & CP & charges
C
      xxip = amneut(ichip)
      xip  = ABS(xxip)
      xxi  = amneut(ichi)
      xi   = ABS(xxi)
      amhg  = amw
      DO i = 1, 6
        amsf(i) = susSMQ
      ENDDO
      amsf(7) = amsb(1)
      amsf(8) = amsb(2)
      amf(1) = amd
      amf(2) = amu
      amf(3) = ams
      amf(4) = amc
      amf(5) = amb
      amf(6) = ame
      amf(7) = ammu
      amf(8) = amtau
      ch(1)  =-1./3.
      ch(3)  =-1./3.
      ch(5)  =-1./3.
      ch(2)  = 2./3.
      ch(4)  = 2./3.
      ch(6)  =-1.
      ch(7)  =-1.
      ch(8)  =-1.
C
      IF ( xi .GE. xip ) GOTO 999
C
C-- Relevant fields combinaisons
C
      tw = sw/cw
      fielnp(ichip) = fieldn(2,ichip) + tw * fieldn(1,ichip)
      fielnm(ichip) = fieldn(2,ichip) - tw * fieldn(1,ichip)
      fielnp(ichi ) = fieldn(2,ichi)  + tw * fieldn(1,ichi)
      fielnm(ichi ) = fieldn(2,ichi)  - tw * fieldn(1,ichi)
C
C-- Initialisation
C
      copw = 0.
      copf = 0.
      copg = 0.
      coph = 0.
      chigam = 0.
C
      DO 1 icharg = 1,2
C      IF ( xip .GE. amw + amchar(icharg) ) GOTO 901
C--- W-Chargino loop
       aw = fieldn(2,ichi)*fieldn(2,ichip)*
     .      (vmat(icharg,1)**2 - umat(icharg,1)**2)
     .    + .5 * (fieldn(4,ichi)*fieldn(4,ichip)*vmat(icharg,2)**2
     .         -  fieldn(3,ichi)*fieldn(3,ichip)*umat(icharg,2)**2)
     .    - (vmat(icharg,1) * vmat(icharg,2) *
     .       (fieldn(2,ichi) * fieldn(4,ichip) +
     .        fieldn(4,ichi) * fieldn(2,ichip))
     .     + umat(icharg,1) * umat(icharg,2) *
     .       (fieldn(2,ichi) * fieldn(3,ichip) +
     .        fieldn(3,ichi) * fieldn(2,ichip))) / SQRT(2.)
       bw = .5 * umat(icharg,2) * vmat(icharg,2) *
     .            (fieldn(3,ichi) * fieldn(4,ichip) -
     .             fieldn(4,ichi) * fieldn(3,ichip))
     .     + (umat(icharg,1) * vmat(icharg,2) *
     .        (fieldn(2,ichi) * fieldn(4,ichip) -
     .         fieldn(4,ichi) * fieldn(2,ichip))
     .      + umat(icharg,2) * vmat(icharg,1) *
     .        (fieldn(2,ichi) * fieldn(3,ichip) -
     .         fieldn(3,ichi) * fieldn(2,ichip))) / SQRT(2.)
       copw = copw
     .      + aw * (xxip
     .           * (FI2(amw/amz,amchar(icharg)/amz,xip/amz,xi/amz)
     .           -  FJ( amw/amz,amchar(icharg)/amz,xip/amz,xi/amz)
     .           -  FK( amw/amz,amchar(icharg)/amz,xip/amz,xi/amz))
     .           +  xxi
     .           * (FJ( amw/amz,amchar(icharg)/amz,xip/amz,xi/amz)
     .           -  FK( amw/amz,amchar(icharg)/amz,xip/amz,xi/amz)))
     .           +  2. * bw * amchar(icharg)
     .           *  FJ( amw/amz,amchar(icharg)/amz,xip/amz,xi/amz)
C     PRINT *,'W loop : ich aw bw copw :',icharg,aw,bw,copw
C
C--- H-Chargino loop
C      IF ( xip .GE. amhp + amchar(icharg) ) GOTO 902
       ah = s2b
     .     * (umat(icharg,1) * vmat(icharg,1)
     .      * (fieldn(4,ichi) * fieldn(3,ichip)
     .       - fieldn(3,ichi) * fieldn(4,ichip))
     .     - (fielnp(ichip)  *
     .        (fieldn(3,ichi) * umat(icharg,1) * vmat(icharg,2)
     .       + fieldn(4,ichi) * umat(icharg,2) * vmat(icharg,1))
     .      - fielnp(ichi) *
     .        (fieldn(3,ichip) * umat(icharg,1) * vmat(icharg,2)
     .       + fieldn(4,ichip) * umat(icharg,2) * vmat(icharg,1)))
     .      / SQRT(2.))
       bh = 2.* cb2 * (fieldn(4,ichi) * vmat(icharg,1)
     .               + fielnp(ichi) * vmat(icharg,2) / SQRT(2.))
     .              * (fieldn(4,ichip) * vmat(icharg,1)
     .               + fielnp(ichip) * vmat(icharg,2) / SQRT(2.))
     .    - 2.* sb2 * (fieldn(3,ichi) * umat(icharg,1)
     .               - fielnp(ichi) * umat(icharg,2) / SQRT(2.))
     .              * (fieldn(3,ichip) * umat(icharg,1)
     .               - fielnp(ichip) * umat(icharg,2) / SQRT(2.))
       coph = coph +
     .        bh * ( xxip
     .           * (FI2(amhp/amz,amchar(icharg)/amz,xip/amz,xi/amz)
     .           -  FK( amhp/amz,amchar(icharg)/amz,xip/amz,xi/amz))
     .           -   xxi
     .           *  FK( amhp/amz,amchar(icharg)/amz,xip/amz,xi/amz))
     .           + amchar(icharg) * ah
     .           *  FI( amhp/amz,amchar(icharg)/amz,xip/amz,xi/amz)
C     PRINT *,'H loop : ich ah bh coph :',icharg,ah,bh,coph
C
C--- G-Chargino loop
       ag = -s2b
     .     * (umat(icharg,1) * vmat(icharg,1)
     .      * (fieldn(4,ichi) * fieldn(3,ichip)
     .       - fieldn(3,ichi) * fieldn(4,ichip))
     .     - (fielnp(ichip)  *
     .        (fieldn(3,ichi) * umat(icharg,1) * vmat(icharg,2)
     .       + fieldn(4,ichi) * umat(icharg,2) * vmat(icharg,1))
     .      - fielnp(ichi) *
     .        (fieldn(3,ichip) * umat(icharg,1) * vmat(icharg,2)
     .       + fieldn(4,ichip) * umat(icharg,2) * vmat(icharg,1)))
     .      / SQRT(2.))
       bg = 2.* sb2 * (fieldn(4,ichi) * vmat(icharg,1)
     .               + fielnp(ichi) * vmat(icharg,2) / SQRT(2.))
     .              * (fieldn(4,ichip) * vmat(icharg,1)
     .               + fielnp(ichip) * vmat(icharg,2) / SQRT(2.))
     .    - 2.* cb2 * (fieldn(3,ichi) * umat(icharg,1)
     .               - fielnp(ichi) * umat(icharg,2) / SQRT(2.))
     .              * (fieldn(3,ichip) * umat(icharg,1)
     .               - fielnp(ichip) * umat(icharg,2) / SQRT(2.))
       copg = copg +
     .        bg * ( xxip
     .           * (FI2(amhg/amz,amchar(icharg)/amz,xip/amz,xi/amz)
     .           -  FK( amhg/amz,amchar(icharg)/amz,xip/amz,xi/amz))
     .           -   xxi
     .           *  FK( amhg/amz,amchar(icharg)/amz,xip/amz,xi/amz))
     .           +  amchar(icharg) * ag
     .           *  FI( amhg/amz,amchar(icharg)/amz,xip/amz,xi/amz)
C     PRINT *,'G loop : ich ag bg copg :',icharg,ag,bg,copg
 1    CONTINUE
C
C--- Sfermion-Fermion loop
      DO 2 isfer=1,8
C      IF ( xip .GE. amsf(isfer) + amf(isfer) ) GOTO 903
       IF ( ch(isfer) .GE. 0. ) THEN
C--- Up fermions except top
        af = -2. * amf(isfer) / (amw * sb)
     .     * (fieldn(4,ichi) * (.5 * fielnm(ichip)
     .     +  ch(isfer) * tw * fieldn(1,ichip))
     .     - fieldn(4,ichip) * (.5 *fielnm(ichi)
     .     + ch(isfer) * tw * fieldn(1,ichi))
     .     - ch(isfer) * tw
     .     * (fieldn(4,ichip) * fieldn(1,ichi)
     .     -  fieldn(1,ichip) * fieldn(4,ichi)))
        bf = fielnm(ichip) * fielnm(ichi)
     .     + 2.* ch(isfer) * tw
     .     * (fieldn(1,ichi ) * fielnm(ichip)
     .     +  fieldn(1,ichip) * fielnm(ichi))
       ELSE
C--- Down fermions
        af = -2. * amf(isfer) / (amw * cb)
     .     * (fieldn(3,ichi) * (-.5 * fielnm(ichip)
     .     + ch(isfer) * tw * fieldn(1,ichip))
     .     - fieldn(3,ichip) * (-.5 *fielnm(ichi)
     .     + ch(isfer) * tw * fieldn(1,ichi))
     .     - ch(isfer) * tw
     .     * (fieldn(3,ichip) * fieldn(1,ichi)
     .     -  fieldn(1,ichip) * fieldn(3,ichi)))
        bf = fielnm(ichip) * fielnm(ichi)
     .     - 2.* ch(isfer) * tw
     .     * (fieldn(1,ichi) * fielnm(ichip)
     .     + fieldn(1,ichip) * fielnm(ichi))
       ENDIF
       copf = copf
     .      + ch(isfer) * color(isfer)
     .      * (bf*(xxip
     .           *(FI2(amsf(isfer)/amz,amf(isfer)/amz,xip/amz,xi/amz)
     .           - FK( amsf(isfer)/amz,amf(isfer)/amz,xip/amz,xi/amz))
     .           - xxi
     .           * FK( amsf(isfer)/amz,amf(isfer)/amz,xip/amz,xi/amz))
     .        +af* amf(isfer)
     .           * FI( amsf(isfer)/amz,amf(isfer)/amz,xip/amz,xi/amz))
C     PRINT *,'F loop : isf af bf copf :',isfer,af,bf,copf
 2    CONTINUE
C
      ctt = COS(topmix)
      stt = SIN(topmix)
C
C-- 1st scalar top
C
      at1= (ctt * (fielnm(ichi) + 4./3. * tw * fieldn(1,ichi))
     .    + stt * fieldn(4,ichi) * amt / (amw * sb))
     .   * (ctt * fieldn(4,ichip) * amt / (amw * sb)
     .    - 4./3. * stt * fieldn(1,ichip) * tw)
     .   - (ctt * fieldn(4,ichi) * amt / (amw * sb)
     .    - 4./3. * stt * fieldn(1,ichi) * tw)
     .   * (ctt * (fielnm(ichip) + 4./3. * tw * fieldn(1,ichip))
     .    + stt * fieldn(4,ichip) * amt / (amw * sb))
      bt1= (ctt * (fielnm(ichi) + 4./3. * tw * fieldn(1,ichi))
     .    + stt * fieldn(4,ichi) * amt / (amw * sb))
     .   * (ctt * (fielnm(ichip) + 4./3. * tw * fieldn(1,ichip))
     .    + stt * fieldn(4,ichip) * amt / (amw * sb))
     .   - (ctt * fieldn(4,ichi) * amt / (amw * sb)
     .    - 4./3. * stt * fieldn(1,ichi) * tw)
     .   * (ctt * fieldn(4,ichip) * amt / (amw * sb)
     .    - 4./3. * stt * fieldn(1,ichip) * tw)
      copf = copf
     .    + 2./3. * 3. *
     .      (bt1 * (xxip
     .           * (FI2(amst(1)/amz,amt/amz,xip/amz,xi/amz)
     .           -  FK( amst(1)/amz,amt/amz,xip/amz,xi/amz))
     .           - xxi
     .           *  FK( amst(1)/amz,amt/amz,xip/amz,xi/amz))
     .      +at1 * amt
     .           *  FI( amst(1)/amz,amt/amz,xip/amz,xi/amz))
c     PRINT *,'T1 loop : at1 bt1 copf :',at1,bt1,copf
C--- 2nd scalar top
      at2= (stt * (fielnm(ichi) + 4./3. * tw * fieldn(1,ichi))
     .    - ctt * fieldn(4,ichi) * amt / (amw * sb))
     .   * (stt * fieldn(4,ichip) * amt / (amw * sb)
     .    + 4./3. * ctt * fieldn(1,ichip) * tw)
     .   - (stt * fieldn(4,ichi) * amt / (amw * sb)
     .    + 4./3. * ctt * fieldn(1,ichi) * tw)
     .   * (stt * (fielnm(ichip) + 4./3. * tw * fieldn(1,ichip))
     .    - ctt * fieldn(4,ichip) * amt / (amw * sb))
      bt2= (stt * (fielnm(ichi) + 4./3. * tw * fieldn(1,ichi))
     .    - ctt * fieldn(4,ichi) * amt / (amw * sb))
     .   * (stt * (fielnm(ichip) + 4./3. * tw * fieldn(1,ichip))
     .    - ctt * fieldn(4,ichip) * amt / (amw * sb))
     .   - (stt * fieldn(4,ichi) * amt / (amw * sb)
     .    + 4./3. * ctt * fieldn(1,ichi) * tw)
     .   * (stt * fieldn(4,ichip) * amt / (amw * sb)
     .    + 4./3. * ctt * fieldn(1,ichip) * tw)
      copf = copf
     .     + 2./3. * 3. *
     .      (bt2 * (xxip
     .           * (FI2(amst(2)/amz,amt/amz,xip/amz,xi/amz)
     .           -  FK( amst(2)/amz,amt/amz,xip/amz,xi/amz))
     .           - xxi
     .           *  FK( amst(2)/amz,amt/amz,xip/amz,xi/amz))
     .     + at2 * amt
     .           *  FI( amst(2)/amz,amt/amz,xip/amz,xi/amz))
C     PRINT *,'T2 loop : at2 bt2 copf :',at2,bt2,copf
C
C-- Total coupling constant
C
      copt = copw - (coph + copg +copf)/ 4.
C
C-- Width chip --> chi gamma
C
      chigam = alpha(0)**3 * (copt/amz)**2
     .       * ((xip/amz)**2 - (xi/amz)**2)**3
     .       / (xip/amz)**3
     .       / (8. * pi**2 * sw2**2)
     .       * amz
C     PRINT *,'copt chigam :',copt,chigam
C
      chipho = chigam
C
C     RETURN
C
C900  CONTINUE
C     WRITE(6,*) 'Impossible decay ',ichip,' into ',ichi
C     chipho = 0.
C     RETURN
C
C901  CONTINUE
C     WRITE(6,*) 'Posssible direct decay into W-Chargino'
C     chipho = 0.
C     RETURN
C
C902  CONTINUE
C     WRITE(6,*) 'Posssible direct decay into H-Chargino'
C     chipho = 0.
C     RETURN
C
C903  CONTINUE
C     WRITE(6,*) 'Posssible direct decay into Fermion-Sfermion'
C     chipho = 0.
C     RETURN
C
  999 RETURN
      END
      FUNCTION chizst(ichip,ichi)
C-------------------------------------------------------------------
C! Compute the decay width of chi' --> chi Z*
C  where chi' and chi are any two of the four neutralinos.
C
C  Input:     -- ichip, the first neutralino index
C             -- ichi,  the second neutralino index
C
C  Output:    -- chizst, the decay width in GeV
C
C  V. Bertin, for the CHA001 generator
C
C  Modif:  Patrick Janot (31 Aug 1995)
C          Adapt the routine for the HZHAxx generator
C-------------------------------------------------------------------
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      COMMON / elweak / sw2,alpha(0:nhig),gweak2(0:nhig),
     .                  alphas(0:nhig),g_f,deltar,alpha2,sw,cw2,cw
C
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
C
      REAL*8 a,g,xx,xi,xip,clr2,cltr, DGMLT1, xlo,xhi,x(1)
      COMMON / chachi / a,g,xx,xip,clr2,cltr
      EXTERNAL dgmlt1, fsubzst
C
C     PRINT *,'ichii ichio :',ichip,ichi
      chizst = 0.
      xip = ABS(amneut(ichip))
      xi  = ABS(amneut(ichi))
      xx = amneut(ichi) / amneut(ichip)
      isgn = 1
      IF ( xx .LT. 0D0 ) isgn = -1
      IF ( xip .LE. xi ) GOTO 999
C
      a = amz
      g = gmz
      xlo    = -DATAN2(a/xip,g/xip)
      xhi    =  DATAN2((1D0-DABS(xx))**2-(a/xip)**2,a*g/xip**2)
      partot = DGMLT1(fsubzst,xlo,xhi,1,6,x)
     .       / ( a * g / xip**2 )
C
      atrix = - fieldn(3,ichip)*fieldn(3,ichi)
     +        + fieldn(4,ichip)*fieldn(4,ichi)
C
      bneutr = brnunu(xip,xi,isgn) / 3.
C
      gamchp = alpha(0)**2 * xip * atrix**2 * partot
     .       / ( 192. * pi * bneutr )
     .       / ( sw2 * cw2 )**2
C     PRINT *,'partot atrix bneutr :',partot,atrix,bneutr
C
      chizst = gamchp
C     PRINT *,'gamchp :',gamchp
C
 999  CONTINUE
      RETURN
      END
      SUBROUTINE COMBRA
C------------------------------------------------------------------
C!  Compute branching ratios
C
C  Input:    /PARAM/
C
C  Output:   /HHDECK/
C
C   P. Janot -- 24 August 1991
C------------------------------------------------------------------
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      CHARACTER*14 channel
      CHARACTER*21 channeut, chanchar
      PARAMETER(nchneut=8,nchchar=5)
      COMMON / hhdeck / branch(nchan,nhig),width(nhig),
     .                  parwth(nhig),xymas(2,nchan,nhig),
     .                  xywid(2,nchan,nhig)
      COMMON / chaneu / ichn(2),
     .                  wneut(4,4,nhig), wchar(2,2,nhig),
     .                  widneut(4), brneut(nchneut,4),
     .                  widchar(2), brchar(nchchar,2)
      COMMON / chanch / channeut(nchneut,4), chanchar(nchchar,2)
      COMMON / vect4 / pvect4(5,2)
      COMMON / hinput / ichan(nchan,nhig), channel(nchan,nhig)
      COMMON / hisbr / bchpp,bchgg
      DIMENSION ph(4)
C
      PARAMETER (L1MST=200, L1PAR=200)
      PARAMETER (L2PAR=500, L2PARF=2000 )
      PARAMETER (LJNPAR=4000)
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),
     &                KFDP(L2PARF,5)
      COMMON /LUDAT4/ CHAF(L2PAR)
      CHARACTER*8 CHAF
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5)
C
      COMMON / elweak / sw2,alpha(0:nhig),gweak2(0:nhig),
     .                  alphas(0:nhig),g_f,deltar,alpha2,sw,cw2,cw
C
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
C
      COMMON / miscl / loutbe,ecm,idb1,idb2,xrad,empir,empirm,ism,
     &                 iklei,icar,sdvrt(3),vrtx(4),tabl(26),
     &                 nevent(11)
      INTEGER loutbe,idb1,idb2,nevent,ism,iklei,icar
      REAL*4 ecm,sdvrt,vrtx,tabl,empir,empirm,xrad
C
      INTEGER    IHCD
      REAL*8     MH, TBT, TAF
      COMMON /HCCOMM/ MH(4), TBT, TAF, IHCD(2)
      COMPLEX f12, f1, f0, gzi1, gzi2, branch1, branch2, branch3
      COMPLEX gagasm, gagall, ggsm, ggall
      REAL*8 phspgz, brwwzz
      EXTERNAL f12, f1, f0, gzi1, gzi2, phspgz, brwwzz
      COMMON /gagagg/ gagasm(nhig),gagall(nhig),ggsm(nhig),ggall(nhig)
      CHARACTER*1 star, higgs(nhig)
      DATA higgs/'H','h','A'/
      DATA hbar/6.583173D-25/
C
C
      ada(am1,am2,am3) = (am1**2-am2**2-am3**2)**2-4.*am2**2*am3**2
C
      CALL vzero(branch(1,1),nhig*nchan)
      CALL vzero(width(1),nhig)
      CALL vzero(parwth(1),nhig)
      CALL vzero(wwmax(1,1),2*nhig)
      CALL vzero(jtyp(1,1),2*nhig)
C
      couplh = 1./(32.*pi*amw**2)
      brtau = couplh*amtau**2
      bre   = couplh*ame**2
      brmu  = couplh*ammu**2
      brb   = 3.*couplh*amb**2
      brc   = 3.*couplh*amc**2
      brs   = 3.*couplh*ams**2
      brt   = 3.*couplh*amt**2
      brw   = 1./(64.*pi*amw**2)
      brz   = 1./(128.*pi*amz**2*cw2)
      brga  = 1./(1024.*pi**3*amw**2)
      brgg  = 1./(512.*pi**3*amw**2)
      brgz  = 1./(512.*pi**3*amw**2)
C
C Compute partial widths
C
      DO 1 jhig = 1, nhig
C
        IF ( ism .EQ. 1 .AND. jhig .NE. 2 ) GOTO 1
C
C Define couplings
C
        IF ( jhig .EQ. 1 ) THEN
          rup  = sa/sb
          su1  = sa/sb
          su2  =-ca/sb
          rdw  = ca/cb
          sd1  = ca/cb
          sd2  =-sa/cb
          rw   = COS(beta-alfa)
          rhp  = rw - c2b*cab/(2.*cw2)
          epsh = 1.
          rsf  = cab
          kpw  = 3
          eta  = 1.
        ELSEIF ( jhig .EQ. 2 ) THEN
          IF ( ism .EQ. 0 ) THEN
            rup  = ca/sb
            su1  = ca/sb
            su2  = sa/sb
            rdw  =-sa/cb
            sd1  =-sa/cb
            sd2  =-ca/cb
            rw   = SIN(beta-alfa)
            rhp  = rw + c2b*sab/(2.*cw2)
            epsh = 1.
            rsf  = -sab
            kpw  = 3
            eta  = 1.
          ELSE
            rup  = 1.
            su1  = 1.
            su2  = 1.
            rdw  = 1.
            sd1  = 1.
            sd2  = 1.
            rw   = 1.
            rhp  = 0.
            epsh = 0.
            rsf  = 0.
            kpw  = 3
            eta  = 0.
          ENDIF
        ELSEIF ( jhig .EQ. 3 ) THEN
          rup  = 1./tb
          su1  = 0.
          su2  = 0.
          rdw  = tb
          sd1  = 0.
          sd2  = 0.
          rw   = 0.
          rhp  = 0.
          epsh = 0.
          rsf  = 0.
          kpw  = 1
          eta  = -1.
        ENDIF
C
C Running quark masses
C
        runamu = amu/100.
        runamd = amd/100.
        runams = runmas(ams,jhig,rads)
        runamc = runmas(amc,jhig,radc)
        runamb = runmas(amb,jhig,radb)
        runamt = runmas(amt,jhig,radt)
        rggamu = 1.
        rggamd = 1.
        rggams = 1.
        rggamc = 1.
        rggamb = 1.
        rggamt = 1.
C
C h,H,A --> gamma gamma
C
        branch1 =
C Leptons
     .         rdw * f12(jhig,ame)
     .       + rdw * f12(jhig,ammu)
     .       + rdw * f12(jhig,amtau)
C Down type quarks
     .       + rdw * f12(jhig,amd*SQRT(rggamd)) / 3.
     .       + rdw * f12(jhig,ams*SQRT(rggams)) / 3.
     .       + rdw * f12(jhig,amb*SQRT(rggamb)) / 3.
C Up type quarks
     .       + rup * f12(jhig,amu*SQRT(rggamu)) * 4./3.
     .       + rup * f12(jhig,amc*SQRT(rggamc)) * 4./3.
     .       + rup * f12(jhig,amt*SQRT(rggamt)) * 4./3.
C W boson
     .       + rw * f1(jhig,amw)
C
        gagasm(jhig) = branch1
C       WRITE(6,*) jhig,branch1
        IF ( ism .EQ. 0 ) branch1 = branch1
C Charged Higgs bosons
     .       + rhp * f0(jhig,amhp) * amw**2/amhp**2
C       WRITE(6,*) jhig,branch1
        IF ( ism .EQ. 0 ) branch1 = branch1
C S-leptons (Left)
     .     + ( epsh*(ame  **2/amz**2*rdw + (sw2-1./2.)*rsf)
     .       + epsh*(ammu **2/amz**2*rdw + (sw2-1./2.)*rsf)
     .       + epsh*(amtau**2/amz**2*rdw + (sw2-1./2.)*rsf) )
     .     * f0(jhig,susSML) * (amz/susSML)**2
C S-leptons (Right)
     .     + ( epsh*(ame  **2/amz**2*rdw -  sw2*rsf)
     .       + epsh*(ammu **2/amz**2*rdw -  sw2*rsf)
     .       + epsh*(amtau**2/amz**2*rdw -  sw2*rsf) )
     .     * f0(jhig,susSME) * (amz/susSME)**2
C S-Down type quarks (Left)
     .     + ( epsh*(amd**2/amz**2*rdw + (sw2/3.-1./2.)*rsf)
     .       + epsh*(ams**2/amz**2*rdw + (sw2/3.-1./2.)*rsf) )
     .     * f0(jhig,susSMQ) * (amz/susSMQ)**2 / 3.
C S-Down type quarks (Right)
     .     + ( epsh*(amd**2/amz**2*rdw -  sw2/3.*rsf)
     .       + epsh*(ams**2/amz**2*rdw -  sw2/3.*rsf) )
C    .     * f0(jhig,susSMD) * (amz/susSMD)**2 / 3.
     .     * f0(jhig,susSMQ) * (amz/susSMQ)**2 / 3.
C S-Up type quarks (Left)
     .     + ( epsh*(amu**2/amz**2*rup - (sw2*2./3.-1./2.)*rsf)
     .       + epsh*(amc**2/amz**2*rup - (sw2*2./3.-1./2.)*rsf) )
     .     * f0(jhig,susSMQ) * (amz/susSMQ)**2 * 4./3.
C S-Up type quarks (Right)
C       WRITE(6,*) jhig,branch1
        IF ( ism .EQ. 0 ) branch1 = branch1
     .     + ( epsh*(amu**2/amz**2*rup +  sw2*2./3.*rsf)
     .       + epsh*(amc**2/amz**2*rup +  sw2*2./3.*rsf) )
C    .     * f0(jhig,susSMU) * (amz/susSMU)**2 * 4./3.
     .     * f0(jhig,susSMQ) * (amz/susSMQ)**2 * 4./3.
C       WRITE(6,*) jhig,branch1
        IF ( ism .EQ. 0 ) branch1 = branch1
C S-bottom quarks
     .     + epsh
     .     * ( (amb**2/amz**2*rdw+(sw2/3.-1./2.)*rsf)
     .       * (SIN(botmix))**2
     .       + (amb**2/amz**2*rdw- sw2/3.       *rsf)
     .       * (COS(botmix))**2
     .       - (amb/amz*(sd1*susAb/amz+sd2*susMu/amz))
     .       * SIN(botmix)*COS(botmix) )
     .     * f0(jhig,amsb(2)) * (amz/amsb(2))**2 / 3.
C
     .     + epsh
     .     * ( (amb**2/amz**2*rdw+(sw2/3.-1./2.)*rsf)
     .       * (COS(botmix))**2
     .       + (amb**2/amz**2*rdw- sw2/3.       *rsf)
     .       * (SIN(botmix))**2
     .       + (amb/amz*(sd1*susAb/amz+sd2*susMu/amz))
     .       * SIN(botmix)*COS(botmix) )
     .     * f0(jhig,amsb(1)) * (amz/amsb(1))**2 / 3.
C       WRITE(6,*) jhig,branch1
        IF ( ism .EQ. 0 ) branch1 = branch1
C S-top quarks
     .     + epsh
     .     * ( (amt**2/amz**2*rup-(sw2*2./3.-1./2.)*rsf)
     .       * (SIN(topmix))**2
     .       + (amt**2/amz**2*rup+ sw2*2./3.       *rsf)
     .       * (COS(topmix))**2
     .       - (amt/amz*(su1*susAt/amz+su2*susMu/amz))
     .       * SIN(topmix)*COS(topmix) )
     .     * f0(jhig,amst(2)) * (amz/amst(2))**2 * 4./3.
C
     .     + epsh
     .     * ( (amt**2/amz**2*rup-(sw2*2./3.-1./2.)*rsf)
     .       * (COS(topmix))**2
     .       + (amt**2/amz**2*rup+ sw2*2./3.       *rsf)
     .       * (SIN(topmix))**2
     .       + (amt/amz*(su1*susAt/amz+su2*susMu/amz))
     .       * SIN(topmix)*COS(topmix) )
     .     * f0(jhig,amst(1)) * (amz/amst(1))**2 * 4./3.
C       WRITE(6,*) jhig,branch1
        IF ( ism .EQ. 0 ) branch1 = branch1
C Charginos
     .       + 2.*bb(jhig,1,1) * f12(jhig,amchar(1))* amw/amchar(1)
     .       + 2.*bb(jhig,2,2) * f12(jhig,amchar(2))* amw/amchar(2)
C That's it
C       WRITE(6,*) jhig,branch1
        gagall(jhig) = branch1
        branch(1,jhig) = brga * alpha(jhig)**2 * gweak2(jhig)
     .                        * amhig(jhig)**3
     .                        * CABS(branch1)**2
C
C h,H,A --> gluon gluon
C
        xmh = amhig(jhig)
        IF ( xmh .GT. 1. ) THEN
          fnh = 3.
          IF ( amc .LE. xmh/2. ) fnh = fnh + 1.
          IF ( amb .LE. xmh/2. ) fnh = fnh + 1.
          IF ( amt .LE. xmh/2. ) fnh = fnh + 1.
          alphah = alphas(jhig)
          gw2    = gweak2(jhig)
C
          branch2 =
C Down type quarks
     .         rdw * f12(jhig,amd*SQRT(rggamd))
     .       + rdw * f12(jhig,ams*SQRT(rggams))
     .       + rdw * f12(jhig,amb*SQRT(rggamb))
C Up type quarks
     .       + rup * f12(jhig,amu*SQRT(rggamu))
     .       + rup * f12(jhig,amc*SQRT(rggamc))
     .       + rup * f12(jhig,amt*SQRT(rggamt))
C
C         WRITE(6,*) jhig,branch2
          ggsm (jhig) = branch2
          IF ( ism .EQ. 0 ) branch2 = branch2
C S-Down type quarks (Left)
     .     + ( epsh*(amd**2/amz**2*rdw + (sw2/3.-1./2.)*rsf)
     .       + epsh*(ams**2/amz**2*rdw + (sw2/3.-1./2.)*rsf) )
     .     * f0(jhig,susSMQ) * (amz/susSMQ)**2
C S-Down type quarks (Right)
     .     + ( epsh*(amd**2/amz**2*rdw -  sw2/3.*rsf)
     .       + epsh*(ams**2/amz**2*rdw -  sw2/3.*rsf) )
C    .     * f0(jhig,susSMD) * (amz/susSMD)**2
     .     * f0(jhig,susSMQ) * (amz/susSMQ)**2
C         WRITE(6,*) jhig,branch2
          IF ( ism .EQ. 0 ) branch2 = branch2
C S-Up type quarks (Left)
     .     + ( epsh*(amu**2/amz**2*rup - (sw2*2./3.-1./2.)*rsf)
     .       + epsh*(amc**2/amz**2*rup - (sw2*2./3.-1./2.)*rsf) )
     .     * f0(jhig,susSMQ) * (amz/susSMQ)**2
C S-Up type quarks (Right)
     .     + ( epsh*(amu**2/amz**2*rup +  sw2*2./3.*rsf)
     .       + epsh*(amc**2/amz**2*rup +  sw2*2./3.*rsf) )
C    .     * f0(jhig,susSMU) * (amz/susSMU)**2
     .     * f0(jhig,susSMQ) * (amz/susSMQ)**2
C S-bottom quarks
C         WRITE(6,*) jhig,branch2
          IF ( ism .EQ. 0 ) branch2 = branch2
     .     + epsh
     .     * ( (amb**2/amz**2*rdw+(sw2/3.-1./2.)*rsf)
     .       * (SIN(botmix))**2
     .       + (amb**2/amz**2*rdw- sw2/3.       *rsf)
     .       * (COS(botmix))**2
     .       - (amb/amz*(sd1*susAb/amz+sd2*susMu/amz))
     .       * SIN(botmix)*COS(botmix) )
     .     * f0(jhig,amsb(2)) * (amz/amsb(2))**2
C
     .     + epsh
     .     * ( (amb**2/amz**2*rdw+(sw2/3.-1./2.)*rsf)
     .       * (COS(botmix))**2
     .       + (amb**2/amz**2*rdw- sw2/3.       *rsf)
     .       * (SIN(botmix))**2
     .       + (amb/amz*(sd1*susAb/amz+sd2*susMu/amz))
     .       * SIN(botmix)*COS(botmix) )
     .     * f0(jhig,amsb(1)) * (amz/amsb(1))**2
C S-top quarks
C         WRITE(6,*) jhig,branch2
          IF ( ism .EQ. 0 ) branch2 = branch2
     .     + epsh
     .     * ( (amt**2/amz**2*rup-(sw2*2./3.-1./2.)*rsf)
     .       * (SIN(topmix))**2
     .       + (amt**2/amz**2*rup+ sw2*2./3.       *rsf)
     .       * (COS(topmix))**2
     .       - (amt/amz*(su1*susAt/amz+su2*susMu/amz))
     .       * SIN(topmix)*COS(topmix) )
     .     * f0(jhig,amst(2)) * (amz/amst(2))**2
C
     .     + epsh
     .     * ( (amt**2/amz**2*rup-(sw2*2./3.-1./2.)*rsf)
     .       * (COS(topmix))**2
     .       + (amt**2/amz**2*rup+ sw2*2./3.       *rsf)
     .       * (SIN(topmix))**2
     .       + (amt/amz*(su1*susAt/amz+su2*susMu/amz))
     .       * SIN(topmix)*COS(topmix) )
     .     * f0(jhig,amst(1)) * (amz/amst(1))**2
C That's it
C
          ggall(jhig) = branch2
C         WRITE(6,*) jhig,branch2
C         WRITE(6,*) '-------------------'
C         WRITE(6,*) jhig,gagasm(jhig),' (gaga sm )'
C         WRITE(6,*) jhig,gagall(jhig),' (gaga all)'
C         WRITE(6,*) jhig,ggsm  (jhig) ,'(glgl sm )'
C         WRITE(6,*) jhig,ggall (jhig),' (glgl all)'
C         WRITE(6,*) '-------------------'
          branch(2,jhig) = brgg * alphah**2 * xmh**3 * gw2
     .                   * CABS(branch2)**2
C But account also for large QCD corrections (Kniehl et al)
     .                   * ( 1D0 + alphah/pi*(95./4.-7.*fnh/6.) )
        ELSE
          branch(2,jhig) = 0.
        ENDIF
C
C h,H,A --> gamma Z0
C
        xmh = amhig(jhig)
        IF ( xmh .GT. 20. ) THEN
          tw = sw/cw
          tauw = 4.*amw**2/amhig(jhig)**2
          branch3 =
C Leptons
     .   2.* (-1./2. + 2.*sw2) / (sw*cw)
     .     * (  rdw * (epsh*gzi1(jhig,ame  )-gzi2(jhig,ame  ))
     .        + rdw * (epsh*gzi1(jhig,ammu )-gzi2(jhig,ammu ))
     .        + rdw * (epsh*gzi1(jhig,amtau)-gzi2(jhig,amtau)) )
C Down type quarks
     . + 2.* (-1./2. + 2.*sw2/3.) / (sw*cw)
     .     * ( rdw * (epsh*gzi1(jhig,amd)-gzi2(jhig,amd))
     .       + rdw * (epsh*gzi1(jhig,ams)-gzi2(jhig,ams))
     .       + rdw * (epsh*gzi1(jhig,amb)-gzi2(jhig,amb)) )
C Up type quarks
     . - 4.* (+1./2. - 4.*sw2/3.) / (sw*cw)
     .     * ( rup * (epsh*gzi1(jhig,amu)-gzi2(jhig,amu))
     .       + rup * (epsh*gzi1(jhig,amc)-gzi2(jhig,amc))
     .       + rup * (epsh*gzi1(jhig,amt)-gzi2(jhig,amt)) )
C W boson
     . - 1./tw * rw
     .    * ( 4.*(3.-tw**2)                  * gzi2(jhig,amw)
     .    + ((1.+2./tauw)*tw**2-(5.+2./tauw))* gzi1(jhig,amw))
C
C         WRITE(6,*) jhig,branch3
          IF ( ism .EQ. 0 ) branch3 = branch3
C Charged Higgs bosons
     . + (1.-2.*sw2) / (sw*cw)
     .       * rhp * gzi1(jhig,amhp) * amw**2/amhp**2
C         WRITE(6,*) jhig,branch3
          IF ( ism .EQ. 0 ) branch3 = branch3
C S-leptons (Left)
     . + 2.* (-1./2. + sw2) / (sw*cw)
     .     * ( epsh*(ame  **2/amz**2*rdw + (sw2-1./2.)*rsf)
     .       + epsh*(ammu **2/amz**2*rdw + (sw2-1./2.)*rsf)
     .       + epsh*(amtau**2/amz**2*rdw + (sw2-1./2.)*rsf) )
     .     * gzi1(jhig,susSML) * (amz/susSML)**2
C S-leptons (Right)
     . + 2.*           sw2  / (sw*cw)
     .     * ( epsh*(ame  **2/amz**2*rdw -  sw2*rsf)
     .       + epsh*(ammu **2/amz**2*rdw -  sw2*rsf)
     .       + epsh*(amtau**2/amz**2*rdw -  sw2*rsf) )
     .     * gzi1(jhig,susSME) * (amz/susSME)**2
C S-Down type quarks (Left)
     . + 2.* (-1./2. + sw2/3.) / (sw*cw)
     .     * ( epsh*(amd**2/amz**2*rdw + (sw2/3.-1./2.)*rsf)
     .       + epsh*(ams**2/amz**2*rdw + (sw2/3.-1./2.)*rsf) )
     .     * gzi1(jhig,susSMQ) * (amz/susSMQ)**2
C S-Down type quarks (Right)
     . + 2.*           sw2/3.  / (sw*cw)
     .     * ( epsh*(amd**2/amz**2*rdw -  sw2/3.*rsf)
     .       + epsh*(ams**2/amz**2*rdw -  sw2/3.*rsf) )
C    .     * gzi1(jhig,susSMD) * (amz/susSMD)**2
     .     * gzi1(jhig,susSMQ) * (amz/susSMQ)**2
C S-Up type quarks (Left)
     . - 4.* (1./2. - 2.*sw2/3.) / (sw*cw)
     .     * ( epsh*(amu**2/amz**2*rup - (sw2*2./3.-1./2.)*rsf)
     .       + epsh*(amc**2/amz**2*rup - (sw2*2./3.-1./2.)*rsf) )
     .     * gzi1(jhig,susSMQ) * (amz/susSMQ)**2
C S-Up type quarks (Right)
C         WRITE(6,*) jhig,branch3
          IF ( ism .EQ. 0 ) branch3 = branch3
     . - 4.* (      - 2.*sw2/3.) / (sw*cw)
     .     + ( epsh*(amu**2/amz**2*rup +  sw2*2./3.*rsf)
     .       + epsh*(amc**2/amz**2*rup +  sw2*2./3.*rsf) )
C    .     * gzi1(jhig,susSMU) * (amz/susSMU)**2
     .     * gzi1(jhig,susSMQ) * (amz/susSMQ)**2
C         WRITE(6,*) jhig,branch1
          IF ( ism .EQ. 0 ) branch3 = branch3
C S-bottom quarks
     .     + epsh
     .     * ( 2.* (-1./2. +    sw2/3.) / (sw*cw)
     .       + (amb**2/amz**2*rdw+(sw2/3.-1./2.)*rsf)
     .       * (SIN(botmix))**2
     .     +   2.*              sw2/3.  / (sw*cw)
     .       * (amb**2/amz**2*rdw- sw2/3.       *rsf)
     .       * (COS(botmix))**2
     .     -   1.* (-1./2. + 2.*sw2/3.) / (sw*cw)
     .       * (amb/amz*(sd1*susAb/amz+sd2*susMu/amz))
     .       * SIN(botmix)*COS(botmix) )
     .     * gzi1(jhig,amsb(2)) * (amz/amsb(2))**2
C
     .     + epsh
     .     * ( 2.* (-1./2. +    sw2/3.) / (sw*cw)
     .       * (amb**2/amz**2*rdw+(sw2/3.-1./2.)*rsf)
     .       * (COS(botmix))**2
     .     +   2.*              sw2/3.  / (sw*cw)
     .       * (amb**2/amz**2*rdw- sw2/3.       *rsf)
     .       * (SIN(botmix))**2
     .     +   1.* (-1./2. + 2.*sw2/3.) / (sw*cw)
     .       * (amb/amz*(sd1*susAb/amz+sd2*susMu/amz))
     .       * SIN(botmix)*COS(botmix) )
     .     * gzi1(jhig,amsb(1)) * (amz/amsb(1))**2
C         WRITE(6,*) jhig,branch1
          IF ( ism .EQ. 0 ) branch3 = branch3
C S-top quarks
     .     + epsh
     .     * ( - 4.* (1./2. - 2.*sw2/3.) / (sw*cw)
     .       * (amt**2/amz**2*rup-(sw2*2./3.-1./2.)*rsf)
     .       * (SIN(topmix))**2
     .     -     4.* (      - 2.*sw2/3.) / (sw*cw)
     .       * (amt**2/amz**2*rup+ sw2*2./3.       *rsf)
     .       * (COS(topmix))**2
     .     +     2.* (1./2. - 4.*sw2/3.) / (sw*cw)
     .       * (amt/amz*(su1*susAt/amz+su2*susMu/amz))
     .       * SIN(topmix)*COS(topmix) )
     .     * gzi1(jhig,amst(2)) * (amz/amst(2))**2
C
     .     + epsh
     .     * ( - 4.* (1./2. - 2.*sw2/3.) / (sw*cw)
     .       * (amt**2/amz**2*rup-(sw2*2./3.-1./2.)*rsf)
     .       * (COS(topmix))**2
     .     -     4.* (      - 2.*sw2/3.) / (sw*cw)
     .       * (amt**2/amz**2*rup+ sw2*2./3.       *rsf)
     .       * (SIN(topmix))**2
     .     -     2.* (1./2. - 4.*sw2/3.) / (sw*cw)
     .       * (amt/amz*(su1*susAt/amz+su2*susMu/amz))
     .       * SIN(topmix)*COS(topmix) )
     .     * gzi1(jhig,amst(1)) * (amz/amst(1))**2
C         WRITE(6,*) jhig,branch3
          IF ( ism .EQ. 0 ) branch3 = branch3
C Charginos
     .     + 2.*bb(jhig,1,1) * 2. / (cw*sw)
     .       * ( - vmat(1,1)**2 - vmat(1,2)**2/2.
     .           - umat(1,1)**2 - umat(1,2)**2/2.
     .           + 2.*sw2 )
     .       * (epsh*gzi1(jhig,amchar(1))-gzi2(jhig,amchar(1)))
     .       *  amw/amchar(1)
     .     + 2.*bb(jhig,2,2) * 2. / (cw*sw)
     .       * ( - vmat(2,1)**2 - vmat(2,2)**2/2.
     .           - umat(2,1)**2 - umat(2,2)**2/2.
     .           + 2.*sw2 )
     .       * (epsh*gzi1(jhig,amchar(2))-gzi2(jhig,amchar(2)))
     .       *  amw/amchar(2)
C That's it
C         WRITE(6,*) jhig,branch3
          branch(11,jhig) = brgz * alpha(jhig)**2 * gweak2(jhig)
     .                           * amhig(jhig)**3
     .                           * CABS(branch3)**2
     .                           * phspgz(amhig(jhig)**2,amz,gmz)
        ELSE
          branch(11,jhig) = 0.
        ENDIF
C
C h,H,A --> e+e-
C
        IF ( amhig(jhig) .GT. 2.*ame) branch(12,jhig) =
     .            gweak2(jhig)*bre*rdw**2*amhig(jhig)*
     .            SQRT(1.-4*ame**2/amhig(jhig)**2)**kpw
     .            * weakcor(jhig,ame,1.,rup/rdw,rw/rdw)
C
C h,H,A --> mu+mu-
C
        IF ( amhig(jhig) .GT. 2.*ammu) branch(13,jhig) =
     .            gweak2(jhig)*brmu*rdw**2*amhig(jhig)*
     .            SQRT(1.-4*ammu**2/amhig(jhig)**2)**kpw
     .            * weakcor(jhig,ammu,1.,rup/rdw,rw/rdw)
C
C h,H,A --> tau+tau-
C
        IF ( amhig(jhig) .GT. 2.*amtau) branch(3,jhig) =
     .            gweak2(jhig)*brtau*rdw**2*amhig(jhig)*
     .            SQRT(1.-4*amtau**2/amhig(jhig)**2)**kpw
     .            * weakcor(jhig,amtau,1.,rup/rdw,rw/rdw)
C
C h,H,A --> s sbar
C
        IF ( amhig(jhig) .GT. AMAX1(1.,2.*ams) )
     .    branch(14,jhig) = runams * rads *
     .            gweak2(jhig)*brs*rdw**2*amhig(jhig)*
     .            SQRT(1.-4*ams**2*runams/amhig(jhig)**2)**kpw
     .            * weakcor(jhig,ams,1./3.,rup/rdw,rw/rdw)
C
C h,H,A --> b bbar
C
        IF ( amhig(jhig) .GT. 2.*amb)
     .    branch(5,jhig) = runamb * radb *
     .            gweak2(jhig)*brb*rdw**2*amhig(jhig)*
     .            SQRT(1.-4*amb**2*runamb/amhig(jhig)**2)**kpw
     .            * weakcor(jhig,amb,1./3.,rup/rdw,rw/rdw)
C
C h,H,A --> c cbar
C
        IF ( amhig(jhig) .GT. 2.*amc )
     .    branch(4,jhig) = runamc * radc *
     .            gweak2(jhig)*brc*rup**2*amhig(jhig)*
     .            SQRT(1.-4*amc**2*runamc/amhig(jhig)**2)**kpw
     .            * weakcor(jhig,amc,2./3.,rup/rup,rw/rup)
C
C  h,H,A --> t tbar
C
        IF ( amhig(jhig) .GT. 2.*amt)
     .    branch(6,jhig) = runamt * radt *
     .            gweak2(jhig)*brt*rup**2*amhig(jhig)*
     .            SQRT(1.-4*amt**2*runamt/amhig(jhig)**2)**kpw
C
C  h,H,A --> W W, Z Z
C
        IF ( jhig .NE. 3 .AND. amhig(jhig) .GT. 20. ) THEN
          const = 3. * SQRT(2.) * G_F / (4.*pi*amhig(jhig)**3)
          compww = const*brwwzz(amhig(jhig),amw,gmw)
          compzz = const*brwwzz(amhig(jhig),amz,gmz)/2.
          branch(7,jhig) = rw**2 * compww
          branch(8,jhig) = rw**2 * compzz
        ENDIF
C
        IF ( ism .EQ. 1 ) THEN
          CALL vzero(amneut(1),4)
          CALL vzero(amchar(1),2)
          CALL vzero(wneut(1,1,1),4*4*nhig)
          CALL vzero(wchar(1,1,1),2*2*nhig)
          wneut(1,1,jhig) = 1.
          branch(15,jhig) = branch(12,jhig)/1E6
          branch(16,jhig) = 0.
        ENDIF
C
C Now, only the MSSM is concerned.
C
        IF ( ism .EQ. 0 ) THEN
C
C h,H,A --> chi chi
C
          DO i = 1, 4
            DO j = i, 4
              IF ( i .EQ. j ) THEN
                dij = 1.
              ELSE
                dij = 0.
              ENDIF
              IF ( amhig(jhig) .GT.
     .              ABS(amneut(i))+ABS(amneut(j)) ) THEN
                wneut(i,j,jhig) =
     .              gweak2(jhig)/(8.*pi*amhig(jhig)**3*(1.+dij))
     .            * aa(jhig,i,j)**2
     .            * (amhig(jhig)**2-(amneut(i)+eta*amneut(j))**2)
     .            * SQRT((amneut(i)**2+amneut(j)**2-amhig(jhig)**2)**2
     .                -4.*amneut(i)**2*amneut(j)**2)
              ELSE
                wneut(i,j,jhig) = 0.
              ENDIF
              branch(15,jhig) = branch(15,jhig) + wneut(i,j,jhig)
            ENDDO
          ENDDO
C
          IF ( branch(15,jhig) .GT. 0. ) THEN
            DO i = 1, 4
              DO j = 1, 4
                wneut(i,j,jhig) = wneut(i,j,jhig) / branch(15,jhig)
              ENDDO
            ENDDO
          ENDIF
C
C h,H,A --> chi+chi-
C
          DO i = 1, 2
            DO j = 1, 2
              IF ( amhig(jhig) .GT.
     .              ABS(amchar(i))+ABS(amchar(j)) ) THEN
                wchar(i,j,jhig) =
     .              gweak2(jhig)/(16.*pi*amhig(jhig)**3)
     .            * ( (bb(jhig,i,j)**2+bb(jhig,j,i)**2)
     .               *(amhig(jhig)**2-amchar(i)**2-amchar(j)**2)
     .               -4.*bb(jhig,i,j)*bb(jhig,j,i)
     .               *eta*amchar(i)*amchar(j) )
     .            * SQRT((amchar(i)**2+amchar(j)**2-amhig(jhig)**2)**2
     .                -4.*amchar(i)**2*amchar(j)**2)
              ELSE
                wchar(i,j,jhig) = 0.
              ENDIF
              branch(16,jhig) = branch(16,jhig) + wchar(i,j,jhig)
            ENDDO
          ENDDO
C
          IF ( branch(16,jhig) .GT. 0. ) THEN
            DO i = 1, 2
              DO j = 1, 2
                wchar(i,j,jhig) = wchar(i,j,jhig) / branch(16,jhig)
              ENDDO
            ENDDO
          ENDIF
C
        ENDIF
C
    1 CONTINUE
C
      IF ( ism .EQ. 1 ) GOTO 2
C
C  H --> A A
C
      IF ( gmh .GT. 2.*ama ) branch(9,1) =
     .     gweak2(1)*amz**2*SQRT(1.-4.*ama**2/gmh**2)
     .              * (c2b*cab)**2
     .              / (128.*pi*gmh*cw2)
C
C  h --> A A
C
      IF ( amh .GT. 2.*ama ) branch(9,2) =
     .     gweak2(2)*amz**2*SQRT(1.-4.*ama**2/amh**2)
C    .              * (c2b*sab)**2
C Add radiative corrections (Zwirner et al, CERN-TH 6151/91)
     .              * (c2b*sab
     .              + 3.*gweak2(2)*cw2*ca*cb**2*amt**4
     .              / (8.*pi**2*sb**3*amw**4)
     .              * ALOG(1.+amsq**2/amt**2) ) **2
C
     .              / (128.*pi*amh*cw2)
C
C  A --> Z h
C
      x = ada(ama,amz,amh)
      IF ( ama .GT. amh+amz .AND. x .GT. 0. )
     .  branch(9,3) = gweak2(3)*SQRT(x)*cab2
     .              / (64.*pi*ama**3*cw2)
     .              * (amz**2-2.*(ama**2+amh**2)
     .              + (ama**2-amh**2)**2/amz**2)
C
C  H --> h h
C
      IF ( gmh .GT. 2.*amh ) branch(10,1) =
     .     gweak2(1)*amz**2*SQRT(1.-4.*amh**2/gmh**2)
     .              * (c2a*cab-2.*s2a*sab)**2
     .              / (128.*pi*gmh*cw2)
C
    2 CONTINUE
      CALL vzero(tauh(1),nhig)
      DO 3 jhig = 1, nhig
        DO jchan = 1, nchan
          width(jhig) = width(jhig) + branch(jchan,jhig)
          IF ( ichan(jchan,jhig) .EQ. 1 )
     .    parwth(jhig) = parwth(jhig) + branch(jchan,jhig)
        ENDDO
C
        IF ( width(jhig) .EQ. 0. ) THEN
          IF ( idbg .GE. 0 ) WRITE(6,2001) higgs(jhig),amhig(jhig)
          GOTO 3
        ELSE
          tauh(jhig) = hbar/width(jhig)
          IF ( idbg .GE. 0 ) WRITE(6,1001)
     .    higgs(jhig),amhig(jhig),width(jhig),parwth(jhig)
        ENDIF
C
        btot = 0.
        DO jchan = 1, nchan
          star = ' '
          if (ichan(jchan,jhig) .eq. 1) star = '*'
          IF ( width(jhig) .GT. 0. )
     .    branch(jchan,jhig) = branch(jchan,jhig)/width(jhig)*100.
          btot = btot + branch(jchan,jhig)
          IF ( idbg .GE. 0 .AND. branch(jchan,jhig) .GT. 0.)
     .    WRITE(6,1002) channel(jchan,jhig),branch(jchan,jhig),star
          IF ( ichan(jchan,jhig) .EQ. 1 .AND.
     .         branch(jchan,jhig) .GT. 0. ) THEN
            branch(jchan,jhig) = branch(jchan,jhig)*width(jhig)/
     .                         parwth(jhig)/100.
          ELSE
            branch(jchan,jhig) = 0.
          ENDIF
        ENDDO
C
        IF ( branch(15,jhig) .GT. 0. ) THEN
          IF ( idbg .GE. 0 ) WRITE(6,1005)
          DO ineut = 1, 4
            DO jneut = ineut,4
              IF ( ineut .EQ. jneut ) THEN
                br = wneut(ineut,jneut,jhig)
              ELSE
                br = wneut(ineut,jneut,jhig)+wneut(jneut,ineut,jhig)
              ENDIF
              br = br*branch(15,jhig)*parwth(jhig)/width(jhig)*100.
              IF ( br .GT. 0. .AND. idbg .GE. 0 ) THEN
                IF ( jhig.EQ. 1 ) WRITE(6,1006) ineut,jneut,br
                IF ( jhig.EQ. 2 ) WRITE(6,1016) ineut,jneut,br
                IF ( jhig.EQ. 3 ) WRITE(6,1026) ineut,jneut,br
              ENDIF
            ENDDO
          ENDDO
        ENDIF
C
        IF ( branch(16,jhig) .GT. 0. ) THEN
          IF ( idbg .GE. 0 ) WRITE(6,1007)
          DO ichar = 1, 2
            DO jchar = ichar,2
              IF ( ichar .EQ. jchar ) THEN
                br = wchar(ichar,jchar,jhig)
              ELSE
                br = wchar(ichar,jchar,jhig)+wchar(jchar,ichar,jhig)
              ENDIF
              br = br*branch(16,jhig)*parwth(jhig)/width(jhig)*100.
              IF ( br .GT. 0. .AND. idbg .GE. 0 ) THEN
                IF ( jhig .EQ. 1 ) WRITE(6,1008) ichar,jchar,br
                IF ( jhig .EQ. 2 ) WRITE(6,1018) ichar,jchar,br
                IF ( jhig .EQ. 3 ) WRITE(6,1028) ichar,jchar,br
              ENDIF
            ENDDO
          ENDDO
        ENDIF
    3 CONTINUE
      IF ( idbg .GE. 0 ) WRITE(6,1004)
C
C Fill internal width table
C
      CALL vzero(xywid(1,1,1),2*nchan*nhig)
      DO jhig = 1 , nhig
        xywid(1,1,jhig) = 0.
        xywid(1,2,jhig) = 0.
        xywid(1,3,jhig) = 0.
        xywid(1,4,jhig) = 0.
        xywid(1,5,jhig) = 0.
        xywid(1,6,jhig) = 0.
        xywid(1,7,jhig) = pmas(24,2)
        xywid(1,8,jhig) = pmas(23,2)
        xywid(1,9,jhig) = width(3)
        xywid(1,10,jhig) = width(2)
        xywid(1,11,jhig) = 0.
        xywid(1,12,jhig) = 0.
        xywid(1,13,jhig) = 0.
        xywid(1,14,jhig) = 0.
        xywid(1,15,jhig) = 0.
        DO jchan = 1 , nchan
          xywid(2,jchan,jhig) = xywid(1,jchan,jhig)
        ENDDO
        xywid(2,11,jhig) = pmas(23,2)
      ENDDO
C
      xywid(1,9,3) = width(2)
      xywid(2,9,3) = pmas(23,2)
C
      pmas(25,2) = width(2)
      pmas(35,2) = width(1)
      pmas(36,2) = width(3)
C
      pmas(25,3) = AMIN1(pmas(25,1),10.*width(2))
      pmas(35,3) = AMIN1(pmas(35,1),10.*width(1))
      pmas(36,3) = AMIN1(pmas(36,1),10.*width(3))
C
      IF( ism.EQ.0 .OR. ism.EQ.-1 ) THEN
C - The Charged Higgs now ... First define channel names and check data
C - ... then calculate widths&BR and store them in common blocks ...
        mh(1)= DBLE(gmh)           ! H mass
        mh(2)= DBLE(amh)           ! h mass
        mh(3)= DBLE(ama)           ! A mass
        mh(4)= DBLE(amhp)          ! H+ mass
        tbt=   DBLE(tb)            ! tan beta= v2/v1
        taf=   DBLE(ta)            ! tan alpha, mixing ...
        CALL hcsetj( idbg )
      END IF
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT(/' With Lambda_QCD(5) = ',F8.5,' GeV,'/
     .        '      alpha_s(mZ)   = ',F8.5,' ... '/)
 1001 FORMAT(/50('-')//
     .       1x,'The following branching ratios have been computed :'/
     .       1X,'   ',A1,' mass                 : ',F8.3,' GeV/c**2'/
     .       1X,'    Total decay width     : ',F8.3,' GeV'/
     .       1X,'    Width to be generated : ',F8.3,' GeV'//)
 2001 FORMAT(/50('-')/
     .       1X,'   ',A1,' (mass : ',F8.3,' GeV/c**2)'/
     .       1x,'    won''t be decayed by HHDECAY !'//)
 1002 FORMAT(1X,'Channel ',A14,' --- BR = ',F10.5,' % (',A1,')')
 1003 FORMAT(1x,'The branching fraction into the lightest ',
     .          'neutralino pair is ',F10.5,'%'/)
 1004 FORMAT(/1X,'   (*) = channel requested'/)
 1005 FORMAT(/8x,' Detail of the neutralino BRs : ')
 1006 FORMAT(10x,' o H --> chi',I1,'0 chi',I1,'0 ',20('.'),1x,F9.5,'%')
 1016 FORMAT(10x,' o h --> chi',I1,'0 chi',I1,'0 ',20('.'),1x,F9.5,'%')
 1026 FORMAT(10x,' o A --> chi',I1,'0 chi',I1,'0 ',20('.'),1x,F9.5,'%')
 1007 FORMAT(/8x,' Detail of the chargino BRs : ')
 1008 FORMAT(10x,' o H --> chi',I1,'+ chi',I1,'- ',20('.'),1x,F9.5,'%')
 1018 FORMAT(10x,' o h --> chi',I1,'+ chi',I1,'- ',20('.'),1x,F9.5,'%')
 1028 FORMAT(10x,' o A --> chi',I1,'+ chi',I1,'- ',20('.'),1x,F9.5,'%')
      END
      FUNCTION CROCOM(ipro, sbeam)
C----------------------------------------------------------------------
C! Cross sections for e+e- --> hZ, HZ, hA, HA, hnn, Hnn, hee, Hee
C
C  Input :        ipro,   process Id
C                 sbeam,  is ecm**2
C
C  Output:        crocom, the cross-section value
C
C  Patrick Janot -- 31 oct 1992
C----------------------------------------------------------------------
      PARAMETER (L1MST=200, L1PAR=200)
      PARAMETER (L2PAR=500, L2PARF=2000 )
      PARAMETER (LJNPAR=4000)
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),
     &                KFDP(L2PARF,5)
      COMMON /LUDAT4/ CHAF(L2PAR)
      CHARACTER*8 CHAF
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5)
C
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      COMMON / elweak / sw2,alpha(0:nhig),gweak2(0:nhig),
     .                  alphas(0:nhig),g_f,deltar,alpha2,sw,cw2,cw
C
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
C
      COMMON / miscl / loutbe,ecm,idb1,idb2,xrad,empir,empirm,ism,
     &                 iklei,icar,sdvrt(3),vrtx(4),tabl(26),
     &                 nevent(11)
      INTEGER loutbe,idb1,idb2,nevent,ism,iklei,icar
      REAL*4 ecm,sdvrt,vrtx,tabl,empir,empirm,xrad
C
      CHARACTER*14 channel
      CHARACTER*21 channeut, chanchar
      PARAMETER(nchneut=8,nchchar=5)
      COMMON / hhdeck / branch(nchan,nhig),width(nhig),
     .                  parwth(nhig),xymas(2,nchan,nhig),
     .                  xywid(2,nchan,nhig)
      COMMON / chaneu / ichn(2),
     .                  wneut(4,4,nhig), wchar(2,2,nhig),
     .                  widneut(4), brneut(nchneut,4),
     .                  widchar(2), brchar(nchchar,2)
      COMMON / chanch / channeut(nchneut,4), chanchar(nchchar,2)
      COMMON / vect4 / pvect4(5,2)
      COMMON / hinput / ichan(nchan,nhig), channel(nchan,nhig)
      COMMON / hisbr / bchpp,bchgg
      DIMENSION ph(4)
C
      REAL*4 kappa
      REAL*8 brwipj, brwisi, sighhc
      EXTERNAL brwipj, brwisi, sighhc
      LOGICAL first
      DATA first /.TRUE./
      ada(am1,am2,am3) = (am1**2-am2**2-am3**2)**2-4.*am2**2*am3**2
C
C  Calcul des sections efficaces de production (hZ,HZ,hA,HA,WWh,WWH)
C
      ebeam = SQRT(sbeam)/2.
C
      brwig = (sbeam-amz**2)**2 + amz**2*gmz**2
      nevt  = 250
      crocom = 0.
C
      IF ( ipro .EQ. 1 ) THEN
C
        IF ( iklei.EQ.0 .AND. amh+amz.GT.2.*ebeam-10.*gmz ) THEN
          DO 1 ievt = 1, nevt
            bmin = 20.
            bmax = 2.*ebeam
            CALL bwgene(bmin,bmax,amz,gmz,bmz,djdum)
            almbda = ada(2.*ebeam,amh,bmz)
            IF ( amh+bmz .GT. 2.*ebeam .OR. almbda .LE. 0. ) GOTO 1
            weight = sqrt(almbda)*(almbda+12.*sbeam*amz**2)
            sbj =
     .      pi * alpha2 * weight * (1.+(1.-4.*sw2)**2)
     .      / (192.*sbeam**2*sw2**2*cw2**2*brwig)
            crocom = crocom + sab2*sbj
    1     CONTINUE
          crocom = crocom/FLOAT(nevt)
        ELSEIF ( iklei .EQ. 0 ) THEN
          almbda = ada(2.*ebeam,amh,amz)
          weight = sqrt(almbda)*(almbda+12.*sbeam*amz**2)
          sbj =
     .    pi * alpha2 * weight * (1.+(1.-4.*sw2)**2)
     .    / (192.*sbeam**2*sw2**2*cw2**2*brwig)
          crocom = sab2*sbj
        ELSE
          crocom = alpha2 * brwipj(sbeam,amh,width(2),amz)
          IF ( ism .EQ. 0 ) THEN
            topcor = ca/sb / SIN(beta-alfa)
            crocom = crocom * sab2 * (1.-8./3.*deltar*topcor)
     .                             / (1.-8./3.*deltar       )
          ENDIF
        ENDIF
C
      ELSEIF ( ipro .EQ. 2 .AND. ism .EQ. 0 ) THEN
C
        IF ( iklei.EQ.0 .AND. gmh+amz .GT. 2.*ebeam-10.*gmz ) THEN
          DO 2 ievt = 1, nevt
            bmin = 20.
            bmax = 2.*ebeam
            CALL bwgene(bmin,bmax,amz,gmz,bmz,djdum)
            almbda = ada(2.*ebeam,gmh,bmz)
            IF ( gmh+bmz .GT. 2.*ebeam .OR. almbda .LE. 0. ) GOTO 2
            weight = sqrt(almbda)*(almbda+12.*sbeam*amz**2)
            sbj =
     .      pi * alpha2 * weight * (1.+(1.-4.*sw2)**2)
     .      / (192.*sbeam**2*sw2**2*cw2**2*brwig)
            crocom = crocom + cab2*sbj
    2     CONTINUE
          crocom = crocom/FLOAT(nevt)
        ELSEIF ( iklei .EQ. 0 ) THEN
          almbda = ada(2.*ebeam,gmh,amz)
          weight = sqrt(almbda)*(almbda+12.*sbeam*amz**2)
          sbj =
     .    pi * alpha2 * weight * (1.+(1.-4.*sw2)**2)
     .    / (192.*sbeam**2*sw2**2*cw2**2*brwig)
          crocom = cab2*sbj
        ELSE
          crocom = alpha2 * brwipj(sbeam,gmh,width(1),amz)
          IF ( ism .EQ. 0 ) THEN
            topcor = sa/sb / COS(beta-alfa)
            crocom = crocom * cab2 * (1.-8./3.*deltar*topcor)
     .                             / (1.-8./3.*deltar       )
          ENDIF
        ENDIF
C
      ELSEIF ( ipro .EQ. 3 .AND. ism .EQ. 0 ) THEN
C
        kappa = brwisi(sbeam,amh,ama,width(2),width(3)) / 8.
        shA = pi*alpha2*kappa * (8.*sw2**2-4.*sw2+1.) * sbeam
     .      / (12.*cw2**2*sw2**2*brwig)
        crocom = cab2*shA
C
      ELSEIF ( ipro .EQ. 4 .AND. ism .EQ. 0 ) THEN
C
        kappa = brwisi(sbeam,gmh,ama,width(1),width(3)) / 8.
        sHA = pi*alpha2*kappa * (8.*sw2**2-4.*sw2+1.) * sbeam
     .      / (12.*cw2**2*sw2**2*brwig)
        crocom = sab2*sHA
C
      ELSEIF ( ipro .EQ. 5 .OR. ipro .EQ. 7 ) THEN
C
        crocom = sab2 * sigmawwh(sbeam,ipro)
C
      ELSEIF ( (ipro.EQ.6.OR.ipro.EQ.8) .AND. ism .EQ. 0 ) THEN
C
        crocom = cab2 * sigmawwh(sbeam,ipro)
C
      ELSEIF ( ipro.EQ.9 .AND. ism .LT. 1 ) THEN
C
        crocom = SNGL(sighhc(DBLE(sbeam),0))*1000.
C
      ELSE
      ENDIF
C
  999 RETURN
      END
      SUBROUTINE decchi(ch,ichi1,ichi,ich)
C----------------------------------------------------------------
C! Decay a chargino or a neutralino into two bodies
C  ( chi' --> chi gamma, chi Z(*), chi+/- W(*)-/+)
C  ( chi+ --> chi W+(*))
C  Cascades are allowed by consecutive calls to DECCHI
C
C Input:   o ch(1-4) = the quadri-momentum of the chi
C          o ichi1   = neutralino/chargino index
C          o ichi    = its charge
C          o ich     = the position in the LUJET common block
C
C Output:  o p2(1-4) = the LSP quadri-momentum
C          o p3(1-4) = the virtual boson quadri-momentum
C
C Patrick Janot -- 31 Aug 1994
C----------------------------------------------------------------
      DIMENSION ch(4)
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      CHARACTER*14 channel
      CHARACTER*21 channeut, chanchar
      PARAMETER(nchneut=8,nchchar=5)
      COMMON / hhdeck / branch(nchan,nhig),width(nhig),
     .                  parwth(nhig),xymas(2,nchan,nhig),
     .                  xywid(2,nchan,nhig)
      COMMON / chaneu / ichn(2),
     .                  wneut(4,4,nhig), wchar(2,2,nhig),
     .                  widneut(4), brneut(nchneut,4),
     .                  widchar(2), brchar(nchchar,2)
      COMMON / chanch / channeut(nchneut,4), chanchar(nchchar,2)
      COMMON / vect4 / pvect4(5,2)
      COMMON / hinput / ichan(nchan,nhig), channel(nchan,nhig)
      COMMON / hisbr / bchpp,bchgg
      DIMENSION ph(4)
C
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
C
C
      IF ( ichi.EQ.0 .AND. (ichi1.LE.0.OR.ichi.GT.4) ) THEN
        WRITE(6,*) '+++ DECCHI +++ Wrong Neutralino #',ichi1
        STOP 99
      ENDIF
C
      IF ( ichi.EQ.1 .AND. (ichi1.LE.0.OR.ichi.GT.2) ) THEN
        WRITE(6,*) '+++ DECCHI +++ Wrong Chargino #',ichi1
        STOP 99
      ENDIF
C
      IF ( ichi .EQ. 0 .AND. widneut(ichi1) .EQ. 0.) THEN
        WRITE(6,*) '+++ DECCHI +++ No accesible channels for ',
     .             'neutralino # ',ichi1
        STOP 99
      ENDIF
C
      IF ( ichi .EQ. 1 .AND. widchar(ichi1) .EQ. 0.) THEN
        WRITE(6,*) '+++ DECCHI +++ No accesible channels for ',
     .             'chargino # ',ichi1
        STOP 99
      ENDIF
C
      IF ( ichi .EQ. 0 ) THEN
        nchn = nchneut
      ELSE
        nchn = nchchar
      ENDIF
C
C  Choice of the decay channel.
C
      rnch = RNDM(dummy)
      rint = 0.D0
      DO jc = 1 , nchn
        IF ( ichi .EQ. 0 ) THEN
          brch = brneut(jc,ichi1)
        ELSE
          brch = brchar(jc,ichi1)
        ENDIF
        rint = rint + brch
        if ( rnch .lt. rint ) GOTO 30
      ENDDO
  30  CONTINUE
C
C Store the indices of the final state:
C       o ichi2 = the neutral/charg-ino index
C       o jchi  = 0 for a neutralino, 1 for a chargino
C       o ifn   = 0 for a photon, 1 for a Z and 2 for a W
C
      IF ( ichi .EQ. 0 ) THEN
        ichi2 = jc-(jc-1)/3*3
        jchi  = (jc-1)/6
        ifn   = (jc-1)/3
        IF ( RNDM(jchi) .LT. 0.5 ) jchi = -jchi
      ELSE
        ichi2 = jc-(jc-1)/4*4
        jchi  = (jc-1)/4 * ichi
        ifn   = 2-jc/4
      ENDIF
C
      IF ( idbg .GE. 10 ) THEN
        WRITE(6,*) ' +++ DECCHI +++ '
        WRITE(6,*) 'ichi1,ichi : ',ichi1,ichi
        WRITE(6,*) 'ichi2,jchi : ',ichi2,jchi
        WRITE(6,*) 'ifn       : ',ifn
      ENDIF
C
C Build the relevant quadri-impulsions
C
      CALL chideca(ch,ichi1,ichi,ichi2,jchi,ifn)
C
C Fill the common LUJET accordingly
C
      CALL filujt(ich,ichi2,jchi,ifn)
C
  999 RETURN
      END
      SUBROUTINE dilokp(xreal,ximag,f,ff)
C------------------------------------------------------------------
C! Function used in the determination of chi' --> chi gamma
C
C V. Bertin for CHA001
C------------------------------------------------------------------
      REAL*8 xreal,ximag,f,b2,ff,pi
      COMPLEX*16 f1,f2,zwei
C
      pi = 3.1415926535897932
C--berechnet (f(1/z)+f(1/zquer))*.5 --->F...(z=(xreal,ximag))
      b2 = xreal*xreal+ximag*ximag
      xreal = xreal/b2
      ximag = -ximag/b2
C--z=1/z
      b2 = 1./b2
Cvb   PRINT *,'real imag b2 :',xreal,ximag,b2
      CALL fvonz(xreal,ximag,f1)
C--f1=f(1/z)
      ximag = -ximag
      CALL fvonz(xreal,ximag,f2)
C--f2=f(1/zquer)
C     WRITE(6,*)'Imaginaerteil von (f1+f2) muesste 0 sein'
      f = DREAL((f1+f2)/2.d0)
      ff = f
      zwei = DCMPLX(2.d0,0.d0)
C     WRITE(6,*)'Im((f1+f2)/2)',dimag((f1+f2)/zwei)
      RETURN
      END
      SUBROUTINE dsigklei(ipro,s,qh,qq,qp,qm,ifs)
C---------------------------------------------------------------
C! Routine for generating final states in the reaction
C
C       e+ e-  ---> mu+ mu- H
C---------------------------------------------------------------
      DIMENSION qh(4),qp(4),qm(4),qq(4),rp(4)
      REAL*8 a,b,x,xp,x1,x2,a1,a2,xap,fap,xmh,eb
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      PARAMETER (L1MST=200, L1PAR=200)
      PARAMETER (L2PAR=500, L2PARF=2000 )
      PARAMETER (LJNPAR=4000)
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),
     &                KFDP(L2PARF,5)
      COMMON /LUDAT4/ CHAF(L2PAR)
      CHARACTER*8 CHAF
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5)
C
      COMMON / elweak / sw2,alpha(0:nhig),gweak2(0:nhig),
     .                  alphas(0:nhig),g_f,deltar,alpha2,sw,cw2,cw
C
      DIMENSION brai(11), kcode(11), xmasmi(11)
      DATA brai/.0335,.0665,.0335,.0665,.0335,.0665,
     .          .1540,.1190,.1540,.1190,.1540/
      DATA kcode/11,12,13,14,15,16,1,2,3,4,5/
      DATA xmasmi/6*0.,0.3,0.3,1.0,4.0,11.0/
      COMMON / zzdec / braz(11), kselec(11), fracz
C
      COMMON / miscl / loutbe,ecm,idb1,idb2,xrad,empir,empirm,ism,
     &                 iklei,icar,sdvrt(3),vrtx(4),tabl(26),
     &                 nevent(11)
      INTEGER loutbe,idb1,idb2,nevent,ism,iklei,icar
      REAL*4 ecm,sdvrt,vrtx,tabl,empir,empirm,xrad
C
      LOGICAL first
      DATA first /.TRUE./
      DATA empir0/0./
C
C Derived constants
C
      IF ( first ) THEN
        g     = 1./SQRT(sw2*(1.-sw2))
        tpi   = 8.*ATAN(1.)
        cv2   = (g/4.*(4.*sw2-1.))**2
        ca2   = (-g/4.)**2
        c1    = (cv2+ca2)**2 + 4.*cv2*ca2
        c2    = (cv2+ca2)**2 - 4.*cv2*ca2
        cx    = AMAX1(c1,c2)
        first = .FALSE.
      ENDIF
C
    1 ee = SQRT(s)
      IF ( ipro .EQ. 1 ) THEN
        CALL bwgene(0.,ee,pmas(25,1),pmas(25,2),ymh,djdum)
        xmh = ymh
      ELSE
        CALL bwgene(0.,ee,pmas(35,1),pmas(35,2),ymh,djdum)
        xmh = ymh
      ENDIF
      eb   = SQRT(s)/2.
      a    = 4.*xmh**2/s
      b    = amz*gmz/s
      xp   = 1. + (xmh**2-amz**2)/s
      x1   = SQRT(a)
      x2   = 1.+a/4.
      a1   = ATAN((x1-xp)/b)
      a2   = ATAN((x2-xp)/b)
      xap  = DMIN1( DMAX1(x1+b,xp) , x2 )
      fap  = ((12.+2.*a)-12.*xap+xap**2)*SQRT(xap**2-a)
C
C Generation of the Z decay
C
      choix = RNDM(choix)
      DO ifs = 1,11
       IF ( choix .LT. braz(ifs) ) GOTO 2
      ENDDO
C
C Check if the centre-of-mass energy after ISR is large enough
C
    2 CONTINUE
      kff    = kcode(ifs)
      xmdec  = ULMASS(kff)
      IF ( s .LT. (xmh+2.*xmdec)**2 ) GOTO 1
C
C Generation of x value
C
  100 r  = RNDM(dum1)
      x  = xp + b * TAN ( r*a1 + (1.-r)*a2 )
      w3 = ((12.+2.*a)-12.*x+x*x) * SQRT(x*x-a) / fap
      empir0= AMAX1(empir0,w3)
      IF ( w3 .LE. empir ) GOTO 102
      WRITE(6,101) w3,empir
  101 FORMAT('DSIGKLEI: Weight W3 =',f6.3,' is larger than',f6.3,';',/,
     .       '         You have to increase the value of ''EMPIR'' .')
      STOP
  102 IF ( w3 .LT. (RNDM(dum2)*empir) ) GOTO 100
C
C Generation of higgs solid angle
C
  200 ch = -1. + 2.*RNDM(dum3)
      w4 = 1.-ch*ch*(x*x-a)/(8.*(1.-x)+x*x+a)
      IF ( w4 .LT. RNDM(dum4) ) GOTO 200
      fh = tpi*RNDM(dum5)
C
C Construct Higgs momentum
C
      IF ( eb*x .LT. xmh ) THEN
        qh(4) = xmh
        qvec = 0.
      ELSE
        qh(4) = eb*x
        qvec  = SQRT ((eb*x)**2-xmh**2)
      ENDIF
      sh    = SQRT (1.-ch**2)
      qh(1) = qvec * sh * SIN(fh)
      qh(2) = qvec * sh * COS(fh)
      qh(3) = qvec * ch
C
C Construct sum of muon momenta
C
      DO 300 i=1,4
  300 qq(i) = -qh(i)
      qq(4) = 2.*eb + qq(4)
      xmt2 = qq(4)**2-qq(3)**2-qq(2)**2-qq(1)**2
      IF ( xmt2 .LE. 0. ) THEN
        xmt2 = 1.1E-6
        qq(4) = SQRT(qq(3)**2+qq(2)**2+qq(1)**2+xmt2)
      ENDIF
C
C Generate mu+ momentum (PEP) and energy (EP) in the mu+mu- CM frame
C
      ep = SQRT (s*(1.-x)+xmh**2)/2.
      pep2= ep*ep-xmdec*xmdec
      pep = -1.
      IF ( pep2 .GT. 0. ) pep = SQRT(pep2)
C
C Try again if this configuration kinematically not o.k.
C
      IF ( pep .LE. 0. ) GOTO 100
C
C Generate the muon directions in the mu+mu- CM frame
C
  400 cm    = -1. + 2.*RNDM(dum6)
      sm    = SQRT (1.-cm*cm)
      fm    = tpi*RNDM(dum7)
      rp(1) = pep * sm * SIN(fm)
      rp(2) = pep * sm * COS(fm)
      rp(3) = pep * cm
      rp(4) = ep
C
C Boost mu+ momenta to lab frame and construct mu- momentum
C
      qp(4)=(qq(4)*rp(4)+qq(1)*rp(1)+qq(2)*rp(2)+qq(3)*rp(3))/(2.*ep)
      qr   =(rp(4)+qp(4))/(2.*ep+qq(4))
      DO 500 i=1,3
      qp(i)=rp(i)+qq(i)*qr
  500 qm(i)=qq(i)-qp(i)
      qm(4)=qq(4)-qp(4)
C
C Calculate weight of muon angular variables
C
      ppdqp = eb * ( qp(4) - qp(3) )
      pmdqp = eb * ( qp(4) + qp(3) )
      ppdqm = eb * ( qm(4) - qm(3) )
      pmdqm = eb * ( qm(4) + qm(3) )
      w5 = ( c1*ppdqm*pmdqp + c2*ppdqp*pmdqm )
     .   / ( cx*(ppdqp+ppdqm)*(pmdqp+pmdqm))
      IF ( w5 .LT. RNDM(dum8) ) GOTO 400
C
C Store the maximum weight
C
      empirm = empir0
C
C Et voila.
C
      qh(4) = xmh
      qq(4) = SQRT(xmt2)
C
      RETURN
      END
      SUBROUTINE dsigma(ipro,ss,qh,qa,hh,izpol)
C-----------------------------------------------------------------------
C!  Angular distributions for Higgs production
C
C   Patrick Janot  --  27 Aug 1991
C-----------------------------------------------------------------------
      DIMENSION qh(4), qa(4), hh(4), gg(4)
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      PARAMETER (L1MST=200, L1PAR=200)
      PARAMETER (L2PAR=500, L2PARF=2000 )
      PARAMETER (LJNPAR=4000)
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),
     &                KFDP(L2PARF,5)
      COMMON /LUDAT4/ CHAF(L2PAR)
      CHARACTER*8 CHAF
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5)
C
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
C
      EXTERNAL sigma1,sigma2,sigma3,sigma4
      EXTERNAL sigma5,sigma6,sigma7,sigma8,sigma9
      EXTERNAL sigmat
C
      COMMON / miscl / loutbe,ecm,idb1,idb2,xrad,empir,empirm,ism,
     &                 iklei,icar,sdvrt(3),vrtx(4),tabl(26),
     &                 nevent(11)
      INTEGER loutbe,idb1,idb2,nevent,ism,iklei,icar
      REAL*4 ecm,sdvrt,vrtx,tabl,empir,empirm,xrad
C
      ada(am1,am2,am3) = (am1**2-am2**2-am3**2)**2-4.*am2**2*am3**2
C
      ee = SQRT(ss)
      izpol = 0
C
      IF ( ipro .GE. 5 .AND. ipro .NE. 9 ) GOTO 1000
C
C HZ & HA
C
C Take into account the widths of the Z and of the Higgses
C
      IF ( ipro .EQ. 1 .OR. ipro .EQ. 2 ) THEN
C
        IF ( iklei .EQ. 1 ) THEN
C
          CALL dsigklei(ipro,ss,qh,qa,hh,gg,izpol)
          GOTO 999
C
        ELSE
C
          plmbda = ada(ee,0.,0.)
          wmax   = SQRT(plmbda)*(plmbda+12.*ss*amz**2)
  101     CONTINUE
          IF ( ipro .EQ. 1 ) THEN
            CALL bwgene(0.,ee    ,pmas(25,1),pmas(25,2),am1,djdum)
            CALL bwgene(0.,ee-am1,amz       ,gmz       ,am2,djdum)
          ELSE
            CALL bwgene(0.,ee    ,pmas(35,1),pmas(35,2),am1,djdum)
            CALL bwgene(0.,ee-am1,amz       ,gmz       ,am2,djdum)
          ENDIF
          almbda = ada(ee,am1,am2)
          IF ( am2 + am1 .GT. ee .OR. almbda .LT. 0. ) GOTO 101
          weight = SQRT(almbda)*(almbda+12.*ss*amz**2)
          IF ( weight/wmax .LT. RNDM(weight) ) GOTO 101
C
        ENDIF
C
      ELSEIF ( ipro .EQ. 3 .OR. ipro .EQ. 4 ) THEN
C
        wmax   = SQRT(ada(ee,0.,0.))**3
  102   CONTINUE
        IF ( ipro .EQ. 3 ) THEN
          IF ( RNDM(ipro) .GT. 0.5 ) THEN
            CALL bwgene(0.,ee    ,pmas(25,1),pmas(25,2),am1,djdum)
            CALL bwgene(0.,ee-am1,pmas(36,1),pmas(36,2),am2,djdum)
          ELSE
            CALL bwgene(0.,ee    ,pmas(36,1),pmas(36,2),am2,djdum)
            CALL bwgene(0.,ee-am2,pmas(25,1),pmas(25,2),am1,djdum)
          ENDIF
        ELSE
          IF ( RNDM(ipro) .GT. 0.5 ) THEN
            CALL bwgene(0.,ee    ,pmas(35,1),pmas(35,2),am1,djdum)
            CALL bwgene(0.,ee-am1,pmas(36,1),pmas(36,2),am2,djdum)
          ELSE
            CALL bwgene(0.,ee    ,pmas(36,1),pmas(36,2),am2,djdum)
            CALL bwgene(0.,ee-am2,pmas(35,1),pmas(35,2),am1,djdum)
          ENDIF
        ENDIF
        almbda = ada(ee,am1,am2)
        IF ( am2 + am1 .GT. ee .OR. almbda .LT. 0. ) GOTO 102
        weight = SQRT(almbda)**3
        IF ( weight/wmax .LT. RNDM(weight) ) GOTO 102
C
      ELSEIF ( ipro .EQ. 9 ) THEN
C
        wmax   = SQRT(ada(ee,0.,0.))**3
  103   CONTINUE
        IF ( RNDM(ipro) .GT. 0.5 ) THEN
          CALL bwgene(0.,ee    ,pmas(37,1),pmas(37,2),am1,djdum)
          CALL bwgene(0.,ee-am1,pmas(37,1),pmas(37,2),am2,djdum)
        ELSE
          CALL bwgene(0.,ee    ,pmas(37,1),pmas(37,2),am2,djdum)
          CALL bwgene(0.,ee-am2,pmas(37,1),pmas(37,2),am1,djdum)
        ENDIF
        almbda = ada(ee,am1,am2)
        IF ( am2 + am1 .GT. ee .OR. almbda .LT. 0. ) GOTO 103
        weight = SQRT(almbda)**3
        IF ( weight/wmax .LT. RNDM(weight) ) GOTO 103
C
      ENDIF
C
      qh(4) = (ss+am1**2-am2**2) / (2.*ee)
      qa(4) = (ss-am1**2+am2**2) / (2.*ee)
      pmom2 = (ss-(am1+am2)**2)  * (ss-(am1-am2)**2)
      pmom  = SQRT(pmom2)/(2.*ee)
C
 100  c = 2.*rndm(c) - 1.
      IF ( ipro .GE. 3 ) THEN
        weight = 1.-c**2
        wmax = 1.
        IF ( weight/wmax .LT. RNDM(dummy) ) GOTO 100
      ELSE
        weight = 1.+c**2 + qa(4)**2/am2**2 * (1.-c**2)
        wmax   = 1. + (qa(4)/am2)**2
        IF ( weight/wmax .LT. RNDM(dummy) ) GOTO 100
        IF ( (1+c**2)/weight .GT. RNDM(dummy) ) THEN
          izpol = 1
        ELSE
          izpol = 0
        ENDIF
      ENDIF
      p = 2.*pi*rndm(p)
      s = SQRT(1.-c**2)
C
      qh(4) = am1
      qh(3) = pmom * c
      qh(2) = pmom * s * SIN(p)
      qh(1) = pmom * s * COS(p)
      qa(4) = am2
      qa(3) =-qh(3)
      qa(2) =-qh(2)
      qa(1) =-qh(1)
      GOTO 999
C
C WW fusion now.
C
 1000 CONTINUE
C
      CALL dsigwwh(ipro,ee,qh,qa,hh)
C
  999 RETURN
      END
      SUBROUTINE dsigwwh(ipro,ee,p1,p2,hh)
C -------------------------------------------------------------------
C! Generate unweighted events in the WW/ZZ fusion into h channels
C
C Input:    ee,   the effective centre-of-mass energy
C           ipro, = 5 for WW --> h
C                 = 6 for WW --> H
C                 = 7 for ZZ --> h
C                 = 8 for ZZ --> H
C
C Output    p1,   the neutrino/e- momentum
C           p2,   the anti-neutrino/e+ momentum
C           hh,   the Higgs momentum
C
C Patrick Janot -- 3 Sep 1995
C -------------------------------------------------------------------
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      PARAMETER (L1MST=200, L1PAR=200)
      PARAMETER (L2PAR=500, L2PARF=2000 )
      PARAMETER (LJNPAR=4000)
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),
     &                KFDP(L2PARF,5)
      COMMON /LUDAT4/ CHAF(L2PAR)
      CHARACTER*8 CHAF
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5)
C
      PARAMETER (maxpro= 9)
      COMMON / cropro / cross(maxpro), sthr(maxpro), reduc(maxpro)
      CHARACTER*14 chapro(maxpro)
      DATA chapro /
     .             'e+e- --> h Z',
     .             'e+e- --> H Z',
     .             'e+e- --> h A',
     .             'e+e- --> H A',
     .             'W+W- --> h  ',
     .             'W+W- --> H  ',
     .             'Z Z  --> h  ',
     .             'Z Z  --> H  ',
     .             'e+e- --> H+H-'
     .                                /
      COMMON / prosim / iproyn(maxpro),nevpro(maxpro)
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
C
      COMMON / elweak / sw2,alpha(0:nhig),gweak2(0:nhig),
     .                  alphas(0:nhig),g_f,deltar,alpha2,sw,cw2,cw
C
      COMMON / miscl / loutbe,ecm,idb1,idb2,xrad,empir,empirm,ism,
     &                 iklei,icar,sdvrt(3),vrtx(4),tabl(26),
     &                 nevent(11)
      INTEGER loutbe,idb1,idb2,nevent,ism,iklei,icar
      REAL*4 ecm,sdvrt,vrtx,tabl,empir,empirm,xrad
C
      DIMENSION p1(4), p2(4), pm(4), pp(4), hh(4), ptot(4)
      REAL*8 qbt,y0,tau,betah,x1,x2,xtau,px,pmax,eb,dv,dv1,dv2
      REAL*8 densf,densit,c1,c2,s1,s2,fi1,fi2,c1df,c2df
      REAL*8 x,ratio1,ratio2,ratio3,born1,born2,bwh
      evolwz(x) = -(1.+x)*DLOG(x)-2.*(1.-x)
C
      ss   = ee**2
      eb   = ee/2.
C
C The incoming momenta
C
      pm(1) = 0.
      pm(2) = 0.
      pm(3) = eb
      pm(4) = eb
      pp(1) = 0.
      pp(2) = 0.
      pp(3) =-eb
      pp(4) = eb
C
C Compute relevant couplings
C
      e = SQRT(4.*pi*alpha(0))
C
C Case 1: WW scattering
C
      IF ( ipro .EQ. 5 .OR. ipro .EQ. 6 ) THEN
C
        rmv  = amw
        v1   = e/sw/SQRT(8.)
        v2   = v1
        a1   = v1
        a2   = v1
        gvvh = e/sw*rmv
C
C Case 2: ZZ scattering
C
      ELSEIF ( ipro .EQ. 7 .OR. ipro .EQ. 8 ) THEN
C
        rmv  = amz
        a1   =-e/4./sw/cw
        v1   = a1*(1.-4.*sw2)
        a2   = a1
        v2   = v1
        gvvh = e*rmv/sw/cw
C
      ENDIF
C
C This goes into the matrix element squared
C
      con1 = gvvh**2*((v1-a1)**2*(v2-a2)**2+(v1+a1)**2*(v2+a2)**2)
      con2 = gvvh**2*((v1-a1)**2*(v2+a2)**2+(v1+a1)**2*(v2-a2)**2)
C
C Cross section comes out in pb
C
      picob = alpha2 / alpha(0)**2
     .               / (2.*ss*twopi**5)
C
C Generate the mass of the Higgs
C
      IF     ( ipro .EQ. 5 .OR. ipro .EQ. 7 ) THEN
C
        rmh  = pmas(25,1)
        rgh  = pmas(25,2)
        cou  = sab2
C
      ELSEIF ( ipro .EQ. 6 .OR. ipro .EQ. 8 ) THEN
C
        rmh  = pmas(35,1)
        rgh  = pmas(35,2)
        cou  = cab2
C
      ENDIF
C
      rmh2 = rmh**2
      rmgh = rmh*rgh
      CALL bwgene(0.,ee,rmh,rgh,rm1,djdum)
      rm2 = rm1**2
C
C Approximate the maximal weight for a better efficiency
C
      ratio1 = rmh2/ecm**2
      ratio2 = rm2/ss
      ratio3 = ss/rmh**2
      born2  = rmh/rgh
      born1  = -born2 * (1.-ratio3)
C
      wmax = 4. * cross(ipro)
C
      IF ( 1.-ratio2 .GT. 1D-2 ) THEN
        evol = evolwz(ratio2)
      ELSE
        evol = (1.-ratio2)**2/6. + (1.-ratio2)**3/6.
      ENDIF
C
      IF ( ratio3 .GT. 1D-1 ) THEN
        evol = evol
     .       * ( DATAN(born1)+DATAN(born2) )
     .       / pi
      ELSE
        evol = evol
     .       * ( (rmh/rgh)    / (1.+(rmh/rgh)**2)    * ratio3
     .       +   (rmh/rgh)**3 / (1.+(rmh/rgh)**2)**2 * ratio3**2 )
     .       / pi
      ENDIF
      evol = evol / evolwz(ratio1)
C
      IF ( evol .LT. 1E-2 ) evol = 0.
C    .  evol = cou * sigmawwh(ss,ipro) / cross(ipro)

C
      wmax = wmax * evol
C
C Compute the delta-V angular distribution cutoff
C
      rmv2  = rmv**2
      betah = 1.-rm2/ss
      dv    = 2.*rmv2/(ss*betah)
      dv1   = 1.+dv
      dv2   = dv*(2.+dv)
C
C Constant part of the local density
C
      densf = (dv2/4./pi)**2 * 64./ss
C
C Generation of fermion scattering angles
C
    2 c1  = dv1-dv2/(2.*RNDM(1)+dv)
      c2  = dv1-dv2/(2.*RNDM(3)+dv)
      fi1 = twopi*RNDM(2)
      fi2 = twopi*RNDM(4)
C
C Construct fermion directions
C
      s1    = SQRT(1.-c1*c1)
      s2    = SQRT(1.-c2*c2)
      p1(1) = s1*SIN(fi1)
      p1(2) = s1*COS(fi1)
      p1(3) = c1
      p1(4) = 1.
      p2(1) =-s2*SIN(fi2)
      p2(2) =-s2*COS(fi2)
      p2(3) =-c2
      p2(4) = 1.
C
C Compute acollinearity parameter
C
      tau = prosca(p1,p2)/2.
C
C Generate x1 value such that x1 and x2 distributions are the same
C
      y0   = 1.-betah*tau
      pmax = betah*betah/(4.*y0)
    1 CONTINUE
      x1   = betah*RNDM(5)
      xtau = 1./(1.-tau*x1)
      x2   = (betah-x1)*xtau
      px   = x1*x2*xtau
      IF ( px .LT. RNDM(6)*pmax ) GOTO 1
C
C Construct fermion momenta
C
      DO k=1,4
        p1(k) = p1(k)*eb*x1
        p2(k) = p2(k)*eb*x2
      ENDDO
C
C The higgs momentum follows from momentum conservation
C
      DO k=1,4
        hh(k) = pm(k)+pp(k)-p1(k)-p2(k)
      ENDDO
C
C Check on algorithm by computing the Higgs mass
C
      chk = (hh(4)**2-hh(3)**2-hh(2)**2-hh(1)**2)/rm2 - 1.
      IF ( ABS(chk) .GT. 1e-3 ) WRITE(*,*) ' +++ mh chk:',chk
      hh(4) = rm1
C
C Compute the local density
C
      IF( betah*tau .GT. 1D-3 ) THEN
        qbt  = (-(1.+y0)*DLOG(y0)+2.*y0-2.)/(tau*tau*tau)
      ELSE
        qbt  = betah**3*(10.+10.*betah*tau+9.*betah**2*tau**2)/60.
      ENDIF
      c1df   = -c1+dv1
      c2df   = -c2+dv1
      densit = densf/(qbt*c1df*c1df*c2df*c2df)
C
C Compute exact matrix element squared for
C   o e+ e- ---> nu_e-bar nu_e W+ W- --> nu_e-bar nu-e h
C   o e+ e- ---> e+ e-         Z  Z  --> e+ e-         h
C
      vv1   = -2.*prosca(pm,p1)-rmv2
      vv2   = -2.*prosca(pp,p2)-rmv2
      u1    = -2.*prosca(pm,p2)
      u2    = -2.*prosca(pp,p1)
      s1    =  2.*prosca(p1,p2)
      tfi2  = ( con1*u1*u2 + con2*ss*s1 )
     .      / ( vv1*vv1 * vv2*vv2)
C
C Compute the weight
C
      weight = cou * picob * tfi2  / densit
     .       * (1.+deltar)**3 * (1.-5./3.*deltar)
C
C Apply the rejection algorithm
C
      IF ( weight .GT. 1.05*wmax ) THEN
C       WRITE(6,*) 'Warning at ECM = ',ee,' GeV'
C       WRITE(6,*) 'Weight (',weight,') > Wmax (',wmax,')'
        wmax = 3.6*weight
        GOTO 2
      ELSEIF ( weight .GT. wmax ) THEN
        wmax = weight
      ENDIF
      ipr = ipro-4
      wtot (ipr) = wtot (ipr) + weight
      wtot2(ipr) = wtot2(ipr) + weight**2
      ntry (ipr) = ntry (ipr) + 1
      IF ( weight/wmax .LT. RNDM(weight) ) GOTO 2
C
      nacc(ipr) = nacc(ipr) + 1
C
      RETURN
      END
      FUNCTION FFFF(A,Z,Y)
C--------------------------------------------------------------------
C! Analytical expression for the e+e- --> hZ cross section
C
C  From Ronald Kleiss.
C--------------------------------------------------------------------
      IMPLICIT COMPLEX*16(A-H,O-Z)
      REAL*8 A,A2,Y
C
      z2 = z*z
      z3 = z*z2
      z4 = z*z3
      a2 = a*a
C
      c0 = ( z4 - 2.*a*z2 + a2 ) /z4 *
     .     ( z4 - 24.*z3 + (48.+10.*a)*z2 - 24.*a*z + a2)
      c1 = a2*a2/(3.*z)
      c2 = - a*a2*( 24.*z - a )/(2.*z2)
      c3 = a2*( 8.*z2*(a+6.) - 24.*a*z + a2 ) /z3
      c4 = -a2*( 24.*z3 + 8.*z2*(a+6.) - 24.*a*z + a2 )/z4
      c5 = z3 - 24.*z2 + 8.*z*(a+6.) + 24.*a
      c6 = z2/2. - 12.*z + 4.*(a+6.)
      c7 = z/3. - 8.
      c8 = 1./4.
C
      FFFF = c0*CDLOG(y-z) + c4*DLOG(y) +
     . (c1+y*(c2+y*(c3+y*y*(c5+y*(c6+y*(c7+y*c8))))))/(y*y*y)
C
      RETURN
      END
      FUNCTION FI( ambos , amfer , chii , chio )
C------------------------------------------------------------------
C! Function used in the determination of chi' --> chi gamma
C
C V. Bertin for CHA001
C------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4 ambos,amfer,chii,chio
C
C     PRINT *,'FI mbos mfer chii chio :',ambos,amfer,chii,chio
      result = 0.D0
C
C     IF ( chii .GE. ambos + amfer ) THEN
C      PRINT *,'ambos amfer chii :',ambos,amfer,chii
C      GOTO 999
C     ENDIF
      IF ( chio .LT. 1.E-9 ) chio = 1.E-9
      IF ( chii .LT. 1.E-9 ) chii = 1.E-9
C
      hi = (ambos**2 - amfer**2 - chii**2) / (2.* chii**2)
      wi = hi**2 - (amfer / chii)**2
c     PRINT *,'hi wi :',hi,wi
      IF ( wi .LT. 0.D0 )  THEN
       ximag = DSQRT(-wi)
       xreal = -hi
       CALL dilokp(xreal,ximag,f1,f2)
      ELSE
       xrpi = -hi + DSQRT(wi)
       IF ( ABS(xrpi) .LE. 1.D-15) xrpi = 1.D-15
       xrpi = 1. / xrpi
       xrmi = -hi - DSQRT(wi)
       IF ( ABS(xrmi) .LE. 1.D-15) xrmi = 1.D-15
       xrmi = 1. / xrmi
       f1 = ddilog(xrpi)
       f2 = ddilog(xrmi)
      ENDIF
c     PRINT *,'f1   f2 :',f1,f2
C
      ho = (ambos**2 - amfer**2 - chio**2) / (2.* chio**2)
      wo = ho**2 - (amfer / chio)**2
c     PRINT *,'ho wo :',ho,wo
      IF ( wo .LT. 0.D0 ) THEN
       ximag = DSQRT(-wo)
       xreal = -ho
       CALL dilokp(xreal,ximag,f3,f4)
      ELSE
       xrpi = -ho + DSQRT(wo)
       IF ( ABS(xrpi) .LE. 1.D-15) xrpi = 1.D-15
       xrpi = 1. / xrpi
       xrmi = -ho - DSQRT(wo)
       IF ( ABS(xrmi) .LE. 1.D-15) xrmi = 1.D-15
       xrmi = 1. / xrmi
       f3 = ddilog(xrpi)
       f4 = ddilog(xrmi)
      ENDIF
c     PRINT *,'f3   f4 :',f3,f4
C
      result = (-f1 -f2 + f3 + f4) / (chii**2 - chio**2)
c     PRINT *,'result :',result
C
 999  CONTINUE
      fi = result
      RETURN
      END
      SUBROUTINE filujt(ich,ichi2,jchi,ifn)
C------------------------------------------------------------------
C! Fill LUJETS common block after a neutral/charg-ino decay.
C
C  Input:    ich,   the position of the neutral/charg-ino in LUJET
C            ichi2, the index of the final charg/neutral-ino
C            jchi,  its charge (absolute value)
C            ifn,   the index of the final boson
C                     = 0 for a photon
C                     = 1 for a Z
C                     = 2 for a W
C
C  Patrick Janot -- 31 Aug 1995
C------------------------------------------------------------------
      PARAMETER (L1MST=200, L1PAR=200)
      PARAMETER (L2PAR=500, L2PARF=2000 )
      PARAMETER (LJNPAR=4000)
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),
     &                KFDP(L2PARF,5)
      COMMON /LUDAT4/ CHAF(L2PAR)
      CHARACTER*8 CHAF
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5)
C
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      CHARACTER*14 channel
      CHARACTER*21 channeut, chanchar
      PARAMETER(nchneut=8,nchchar=5)
      COMMON / hhdeck / branch(nchan,nhig),width(nhig),
     .                  parwth(nhig),xymas(2,nchan,nhig),
     .                  xywid(2,nchan,nhig)
      COMMON / chaneu / ichn(2),
     .                  wneut(4,4,nhig), wchar(2,2,nhig),
     .                  widneut(4), brneut(nchneut,4),
     .                  widchar(2), brchar(nchchar,2)
      COMMON / chanch / channeut(nchneut,4), chanchar(nchchar,2)
      COMMON / vect4 / pvect4(5,2)
      COMMON / hinput / ichan(nchan,nhig), channel(nchan,nhig)
      COMMON / hisbr / bchpp,bchgg
      DIMENSION ph(4)
C
      DIMENSION ijoin(2)
      DIMENSION p1(4),p2(4),p3(4)
C
      ichi = k7lu(ich,2)/ABS(k7lu(ich,2))
C
      IF ( jchi .EQ. 0 ) THEN
        kf1 = 50+ichi2
      ELSE
        kf1 = jchi * (54+ichi2)
      ENDIF
C
      kf2 = 22 + ifn
      IF ( kf2 .EQ. 24 ) THEN
        IF ( ichi .EQ. 0 ) THEN
          kf2 = -jchi * kf2
        ELSE
          kf2 =  ichi * kf2
        ENDIF
      ENDIF
C
      n7lu = n7lu + 1
      ipochi = n7lu
      CALL hhlu1(ipochi,kf1,pvect4(1,1),pvect4(2,1),
     .                      pvect4(3,1),pvect4(4,1),pvect4(5,1))
      k7lu(ich,4) = ipochi
      k7lu(ipochi,3) = ich
C
      n7lu = n7lu + 1
      ipobos = n7lu
      CALL hhlu1(ipobos,kf2,pvect4(1,2),pvect4(2,2),
     .                      pvect4(3,2),pvect4(4,2),pvect4(5,2))
      k7lu(ich,5) = ipobos
      k7lu(ipobos,3) = ich
C
C Special path for W and Z
C
      IF ( ifn .GT. 0 ) CALL wzdecy(ipobos)
      k7lu(ich,1) = 11
C
      IF ( idbg .GE. 5 ) CALL lulist(1)
  999 RETURN
      END
      FUNCTION FINT(a,b,c)
C--------------------------------------------------------------------
C! Quick integration for the Higgs pole masses computation
C
C  Improves on the previous method of Carena/Wagner by a
C  factor > 100.
C
C  Patrick Janot -- 21 Sep 1995
C--------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /CEM/ y1,y2,p2
      EXTERNAL fintsub
      DIMENSION x(1)
      p2 = a
      y1 = b
      y2 = c
      xlo = 0D0
      xhi = 1D0
      fint  = DGMLT1(fintsub,xlo,xhi,1,6,x)
      RETURN
      END
      SUBROUTINE fintsub(m,u1,f1,x)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /CEM/ y1,y2,p2
      DIMENSION u1(*),f1(*),x(1)
      DO l = 1,m
        x(1) = u1(l)
        f1(l) = LOG(ABS(x(1)*y1+(1-x(1))*y2-x(1)*(1-x(1))*p2)
     .                /(x(1)*(y1-y2)+y2))
      ENDDO
      RETURN
      END
      FUNCTION FINTAN(p2,y1,y2)
C--------------------------------------------------------------------
C! Analytical expression for the integration done in FINT
C
C  Not used in the code
C
C  From M. Carena and C. Wagner, 21 Sep 1995
C--------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,M,O-Z)
      delta=(y1-y2)/p2
      erre=(abs((1.+delta)**2-4.*y1/p2))**.5
      fintan=-1.+.5*((y1+y2)/(y1-y2)-delta)*log(y2/y1)
     *    +.5*erre*log(abs((delta**2-(1.+erre)**2)) /
     *      abs((delta**2-(1.-erre)**2)) )
      RETURN
      END
      FUNCTION FI2( ambos , amfer , chii , chio )
C------------------------------------------------------------------
C! Function used in the determination of chi' --> chi gamma
C
C V. Bertin for CHA001
C------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4 ambos,amfer,chii,chio
      COMPLEX*16 lambi,lambo,him,hom,hip,hop,ri2
C
      IF ( chio .LT. 1.E-9 ) chio = 1.E-9
      IF ( chii .LT. 1.E-9 ) chii = 1.E-9
      IF ( amfer.LT. 1.E-9 ) amfer= 1.E-9
C
c     PRINT *,'FI2 mbos mfer chii chio :',ambos,amfer,chii,chio
      parai = amfer**2 + ambos**2 - chii**2
      parao = amfer**2 + ambos**2 - chio**2
      rambi = parai**2 - (2.*ambos*amfer)**2
      rambo = parao**2 - (2.*ambos*amfer)**2
      zero = 0.d0
c     PRINT *,'rambi rambo zero:',rambi,rambo,zero
      lambi = DCMPLX(rambi,zero)
      lambo = DCMPLX(rambo,zero)
      lambi = CDSQRT(lambi)
      lambo = CDSQRT(lambo)
c     PRINT *,'lambi lambo :',lambi,lambo
c     PRINT *,'parai parao :',parai,parao
      hip = parai + lambi
      him = parai - lambi
      hop = parao + lambo
      hom = parao - lambo
      IF (CDABS(him) .LT. 1.d-12) him = DCMPLX(1.d-12,0.d0)
      IF (CDABS(hom) .LT. 1.d-12) hom = DCMPLX(1.d-12,0.d0)
c     PRINT *,'hip him hop hom :',hip,him,hop,hom
C
c     ri2 = (CDLOG(him / hip) * lambi / (2.* chii**2)
c    .     - CDLOG(hom / hop) * lambo / (2.* chio**2))
c    .     / (chii**2 - chio**2)
c    .    + (ambos**2 - amfer**2) / (2. * chii**2 * chio**2)
c    .     * LOG(amfer**2 / ambos**2)
      faci = .5d0 / chii**2 / (chii**2 - chio**2)
      faco = .5d0 / chio**2 / (chii**2 - chio**2)
      ri2 = CDLOG(him / hip) * lambi * faci
     .    - CDLOG(hom / hop) * lambo * faco
      fi2 = DREAL(ri2)
     .    + (ambos**2 - amfer**2) / (2.d0 * chii**2 * chio**2)
     .     * LOG(amfer**2 / ambos**2)
c     PRINT *,'ri2 fi2:',ri2,fi2
      RETURN
      END
      FUNCTION FJ( ambos , amfer , chii , chio )
C------------------------------------------------------------------
C! Function used in the determination of chi' --> chi gamma
C
C V. Bertin for CHA001
C------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4 ambos,amfer,chii,chio
C
c     PRINT *,'FJ mbos mfer chii chio :',ambos,amfer,chii,chio
      fj = fi(amfer,ambos,chii,chio)
      RETURN
      END
      FUNCTION FK( ambos , amfer , chii , chio )
C------------------------------------------------------------------
C! Function used in the determination of chi' --> chi gamma
C
C V. Bertin for CHA001
C------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4 ambos,amfer,chii,chio
C
c     PRINT *,'FK mbos mfer chii chio :',ambos,amfer,chii,chio
      fk = (1.
     .    + amfer**2 * FI(ambos,amfer,chii,chio)
     .    + ambos**2 * FJ(ambos,amfer,chii,chio)
     .    - chii**2 * FI2(ambos,amfer,chii,chio))
     .   / (chio**2 - chii**2)
      RETURN
      END
      SUBROUTINE fsubcha(m,u1,f1,x)
C--------------------------------------------------------------------
C! A routine for the chi+ decays into W* chi
C
C  Patrick Janot -- 23 Sep 1995
C--------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / chachi / a,g,xx,xip,clr2,cltr
      DIMENSION u1(*),f1(*),x(1)
      DO l = 1,m
        x(1) = u1(l)
        u = (a/xip)**2+a/xip*g/xip*TAN(x(1))
        part1 = -clr2 * u**2
     .        + .5 *((1. + xx**2)* clr2 - 12.* cltr * xx)* u
     .        + .5 *(1.-xx**2)**2 * clr2
        part2 = u**2 - 2.*(1.+xx**2)*u + (1.-xx**2)**2
        f1(l) = part1 * DSQRT(part2)
      ENDDO
      RETURN
      END
      SUBROUTINE fsubgz(m,u1,f1,x)
C--------------------------------------------------------------------
C! A routine for the h --> Z gamma decay including Z width
C
C  Patrick Janot -- 23 Sep 1995
C--------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /BHV/ s,am1,w1
      DIMENSION u1(*),f1(*),x(1)
      DO l = 1,m
        x(1) = u1(l)
        u = am1**2+am1*w1*TAN(x(1))
        f1(l) = (1.-u/s)**3
      ENDDO
      RETURN
      END
      SUBROUTINE fsubwz2(m,u2,f2,x)
C--------------------------------------------------------------------
C! Two routines for the h-->WW/ZZ decays including W/Z widths
C
C  Patrick Janot -- 15 Sep 1995
C--------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /BWC/ s,am1,am2,w1,w2
      EXTERNAL fsubwz1
      DIMENSION u2(*),f2(*),x(2)
      DO l = 1,m
        x(2)  = u2(l)
        am2W2 = am2**2+am2*w2*DTAN(x(2))
        am2w  = DSQRT(DMAX1(0D0,am2W2))
        x1lo  = -DATAN2(am1,w1)
        x1hi  =  DATAN2((SQRT(s)-am2W)**2-am1**2,am1*w1)
        IF ( x1lo .LT. x1hi ) THEN
          f2(l) = DGMLT1(fsubwz1,x1lo,x1hi,4,8,x)
        ELSE
          f2(l) = 0.
        ENDIF
      ENDDO
      RETURN
      END
      SUBROUTINE fsubwz1(m,u1,f1,x)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /BWC/ s,am1,am2,w1,w2
      DIMENSION u1(*),f1(*),x(2)
      DO l = 1,m
        x(1) = u1(l)
        u = (am1**2+am1*w1*DTAN(x(1)))
        v = (am2**2+am2*w2*DTAN(x(2)))
        ss = DMAX1((s-u-v)**2-4.*u*v, 0D0)
        f1(l) =  DSQRT(ss) * (u*v+ss/12.)
      ENDDO
      RETURN
      END
      SUBROUTINE fsubzst(m,u1,f1,x)
C--------------------------------------------------------------------
C! A routine for the chi' decays into Z* chi
C
C  Patrick Janot -- 23 Sep 1995
C--------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / chachi / a,g,xx,xip,clr2,cltr
      DIMENSION u1(*),f1(*),x(1)
      DO l = 1,m
        x(1) = u1(l)
        u = (a/xip)**2+a/xip*g/xip*TAN(x(1))
        part1 = -1.*u**2
     .        + .5*(1.+6.*xx+xx**2)*u
     .        + .5*(1.-xx**2)**2
        part2 = u**2
     .        - 2.*(1.+xx**2)*u
     .        + (1.-xx**2)**2
        f1(l) = part1 * DSQRT(part2)
      ENDDO
      RETURN
      END
      SUBROUTINE fsub2(m,u2,f2,x)
C--------------------------------------------------------------------
C! Three routines for the hZ/hA cross sections including h/A widths
C
C  Patrick Janot -- 01 Sep 1995
C--------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /BWC/ s,am1,am2,w1,w2
      EXTERNAL fsub1
      DIMENSION u2(*),f2(*),x(2)
      DO l = 1,m
        x(2)  = u2(l)
        am2W2 = am2**2+am2*w2*tan(x(2))
        am2w  = DSQRT(DMAX1(0D0,am2W2))
        x1lo  = -DATAN2(am1,w1)
        x1hi  =  DATAN2((DSQRT(s)-am2W)**2-am1**2,am1*w1)
        IF ( x1lo .LT. x1hi ) THEN
          f2(l) = DGMLT1(fsub1,x1lo,x1hi,1,6,x)
        ELSE
          f2(l) = 0.
        ENDIF
      ENDDO
      RETURN
      END
      SUBROUTINE fsub1(m,u1,f1,x)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /BWC/ s,am1,am2,w1,w2
      DIMENSION u1(*),f1(*),x(2)
      DO l = 1,m
        x(1) = u1(l)
        u = (am1**2+am1*w1*DTAN(x(1)))/s
        v = (am2**2+am2*w2*DTAN(x(2)))/s
        ss = DMAX1(1.+u*u+v*v-2.*(u*v+u+v),0D0)
        f1(l) = ss*DSQRT(ss)
      ENDDO
      RETURN
      END
      SUBROUTINE fsub(m,u1,f1,x)
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4 sigklei,u,ss
      COMMON /BHV/ s,am1,w1
      DIMENSION u1(*),f1(*),x(1)
      ss = s
      DO l = 1,m
        x(1) = u1(l)
        u = DSQRT(am1**2+am1*w1*DTAN(x(1)))
        f1(l) = sigklei(u,ss)
      ENDDO
      RETURN
      END
      FUNCTION ftau(tau)
C------------------------------------------------------------------
C! Famous function appearing in H --> gamma gamma partial widths
C
C  Patrick Janot -- 03 oct 1991
C------------------------------------------------------------------
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
C
      COMPLEX ftau
C
      IF ( tau .GE. 1 ) THEN
        ftaur = (ASIN(SQRT(1./tau)))**2
        ftaui = 0.
      ELSEIF ( tau .GE. 1.E-5 ) THEN
        b0 = SQRT(1.-tau)
        ftaur = (-(ALOG((1.-b0)/(1+b0)))**2 +pi2)/4.
        ftaui = pi/2.*ALOG((1.+b0)/(1.-b0))
      ELSE
        ftaur = (-ALOG(tau/4.)+pi2)/4.
        ftaui = pi/2.*ALOG(4./tau)
      ENDIF
C
      ftau = CMPLX(ftaur,ftaui)
C
      RETURN
      END
      SUBROUTINE fvonz(xreal,ximag,f1)
C------------------------------------------------------------------
C! Function used in the determination of chi' --> chi gamma
C
C V. Bertin for CHA001
C------------------------------------------------------------------
      REAL*8 xreal,ximag,f,pi
      COMPLEX*16 f1,z,g,a,f3,f4
C
      pi = 3.1415926535897932
CVb   PRINT *,'FVONZ : xreal ximag :',xreal,ximag
C--Mitchell 7.1 und 7.2
      f4 = 1.
      f3 = 0.
      z = DCMPLX(xreal,ximag)
Cvb   PRINT *,'z cdbas(z) :',z,CDABS(z)
      IF ((CDABS(z)) .GT. 1.d0) THEN
C--Michell 4.1
       f3 = -pi*pi/6.d0-.5*CDLOG(-z)*CDLOG(-z)
       f4 = -1.
       z = 1./z
      ENDIF
Cvb   PRINT *,'f3 f4 z :',f3,f4,z
      n = 1
      g = 0.
      a = .25*z
10    g = g+a
      a = a*z*n*n/DBLE(n+2)/DBLE(n+2)
      IF((CDABS(a)) .LT. .0000001) GOTO 20
      n = n+1
      GOTO 10
20    f1 = (z*(3.+g)+2.*(1.-z)*CDLOG(1.-z))/(1.+z)
      f1 = f1*f4+f3
Cvb   PRINT *,'f1 :',f1
      RETURN
      END
      FUNCTION f0(jhig,hmass)
C--------------------------------------------------------------------
C! Utility routine to compute gamma gamma and gluon gluon widths
C
C Input:    jhig,   Higgs type
C           hmass,  spin 0 boson mass
C
C Output:   f0,    the value of the function
C
C Patrick Janot -- 03 oct 1991
C--------------------------------------------------------------------
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      COMPLEX ftau, f0
      EXTERNAL ftau
C
      tau = 4.*hmass**2/amhig(jhig)**2
      f0 = tau*(1.-tau*ftau(tau))
C
      RETURN
      END
      FUNCTION f1(jhig,bmass)
C--------------------------------------------------------------------
C! Utility routine to compute gamma gamma and gluon gluon widths
C
C Input:    jhig,   Higgs type
C           bmass,  spin 1 boson mass
C
C Output:   f1,    the value of the function
C
C Patrick Janot -- 03 oct 1991
C--------------------------------------------------------------------
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      COMPLEX ftau, f1
      EXTERNAL ftau
C
      tau = 4.*bmass**2/amhig(jhig)**2
      f1 = 2. + 3.*tau + 3.*tau*(2.-tau)*ftau(tau)
C
      RETURN
      END
      FUNCTION f12(jhig,fmass)
C--------------------------------------------------------------------
C! Utility routine to compute gamma gamma and gluon gluon widths
C
C Input:    jhig,   Higgs type
C           fmass,  fermion mass
C
C Output:   f12,    the value of the function
C
C Patrick Janot -- 03 oct 1991
C--------------------------------------------------------------------
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
C
      COMPLEX ftau, f12
      EXTERNAL ftau
C
      IF ( jhig .LE. 2 ) epsh = 1.
      IF ( jhig .EQ. 3 ) epsh = 0.
      tau = 4.*fmass**2/amhig(jhig)**2
      f12 = -2.*tau * (epsh + (1.-tau*epsh)*ftau(tau))
C
      RETURN
      END
      FUNCTION gcaren(X,Y)
C--------------------------------------------------------------------
C! gcaren(x,y) = 2. - (x+y)/(x-y)*LOG(x/y)
C
C  20 Sep 1995
C--------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,L,M,O-Z)
      gcaren = 2. - (x+y)/(x-y)*LOG(x/y)
      RETURN
      END
      FUNCTION gf1(x,y)
C--------------------------------------------------------------------
C! gf1(x,y) = log(x/y)/(x-y)
C
C  3 Dec 1994
C--------------------------------------------------------------------
      u = x/y
      e = u-1.
      IF ( ABS(e) .GT. 1.E-6 ) THEN
        gf1 = log(u)/(u-1) /y
      ELSE
        gf1 = (1.-e/2.+e**2/3.) /y
      ENDIF
      END
      FUNCTION gf2(x,y)
C--------------------------------------------------------------------
C! gf2 = (2-(x+y)/(x-y)*log(x/y))/(x-y)**2
C
C  3 Dec 1994
C--------------------------------------------------------------------
      u = x/y
      e = u-1.
      IF ( ABS(e) .GT. 1.E-4 ) THEN
        gf2 = (2.-(u+1)/(u-1)*log(u))/(u-1)**2 /y**2
      ELSE
        gf2 = -(1.-e)/6. /y**2
      ENDIF
      END
      FUNCTION gf3(x,y,z)
C--------------------------------------------------------------------
C! gf3(x,y) = [x.gf1(x,z) - y.gf1(y,z)]/(x-y)
C
C  3 Dec 1994
C--------------------------------------------------------------------
      u = x/y
      e = u-1.
      IF ( ABS(e) .GT. 1.E-6 ) THEN
       gf3 = (u*gf1(x,z)-gf1(y,z))/(u-1.)
      ELSE
        v = x/z
        f = v-1.
        IF ( ABS(f) .GT. 1.E-6 ) THEN
          gf3 = (1.-z*gf1(x,z))/(x-z)
        ELSE
          gf3 = (1./2.-v/3.)/z
        ENDIF
      ENDIF
      END
      SUBROUTINE GFUNCAR(ma,tanb,mq,mur,md,mtop,At,Ab,mu,vh,
     *                stop1,stop2)
C--------------------------------------------------------------------
C! Compute D-terms contributing to the Higgs masses
C
C  From M. Carena and C. Wagner, 21 Sep 1995
C--------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,L,M,O-Z)
      REAL*8 cma,ctb,cmq,cmur,cmdr,cmtop,cau,cad,cmu,cmchi
      REAL*8 cmh,cmhp,chm,chmp,camp,cmhch,csa,cca
      REAL*8 cstop1,cstop2,csbot1,csbot2,ctanbA
      REAL*8 rmtop,rmbot,mz,alpha_1,alpha_2,alpha_3,v,ppi,sint,stw
      COMMON / mcarena / rmtop,rmbot,mz,alpha_1,alpha_2,alpha_3,
     .                   v,ppi,sint,stw
C
      DIMENSION DIAH(2),VH(2,2),VH1(2,2),VH2(2,2),
     *          vh3t(2,2),vh3b(2,2),
     *          hmix(2,2),al(2,2),m2(2,2)

      if(dabs(mu).lt.0.000001) mu = 0.000001
      mq2 = mq**2
      mur2 = mur**2
      md2 = md**2
      tanbA = tanb
      sinbA = tanbA/(tanbA**2+1.)**.5
      cosbA = sinbA/tanbA

      sinb = tanb/(tanb**2+1.)**.5
      cosb = sinb/tanb
Cpaj  pi = 3.14159
Cpaj  g2 = (0.0336*4.*pi)**.5
Cpaj  g12 = (0.0101*4.*pi)
Cpaj  g1 = g12**.5
Cpaj  mz = 91.18
Cpaj  v = 174.1
Cpaj  mw = (g2**2*v**2/2.)**.5
Cpaj  alpha3 = 0.12/(1.+23/12./pi*0.12*log(mtop**2/mz**2))
Cpaj  mb = 3.
      pi = ppi                 ! paj
      mb = rmbot               ! paj
      alpha1 = alpha_1         ! paj
      alpha2 = alpha_2         ! paj
      alpha3 = alpha_3         ! paj
      g1 = (alpha1*4.*pi)**.5  ! paj
      g2 = (alpha2*4.*pi)**.5  ! paj
      g3 = (alpha3*4.*pi)**.5  ! paj
      g12 = g1**2
      g32 = g3**2
      if(mq.gt.mur) mst = mq
      if(mur.gt.mq.or.mur.eq.mq) mst = mur

      msusyt = (mst**2  + mtop**2)**.5

      if(mq.gt.md) msb = mq
      if(md.gt.mq.or.md.eq.mq) msb = md

      msusyb = (msb**2 + mb**2)**.5

      tt = log(msusyt**2/mtop**2)
      tb = log(msusyb**2/mtop**2)

Cpaj  rmtop = mtop/(1.+4.*alpha3/3./pi)
Cpaj  ht = rmtop/(174.*sinb)
Cpaj  htst = rmtop/174.1
Cpaj  hb = mb/174./cosb
Cpaj  g32 = alpha3*4.*pi
      ht = rmtop/( v  *sinb)   ! paj
      htst = rmtop/ v          ! paj
      hb = mb/ v  /cosb        ! paj
      bt2 = -(8.*g32 - 9.*ht**2/2. - hb**2/2.)/(4.*pi)**2
      bb2 = -(8.*g32 - 9.*hb**2/2. - ht**2/2.)/(4.*pi)**2
      al2 = 3./8./pi**2*ht**2
      bt2st = -(8.*g32 - 9.*htst**2/2.)/(4.*pi)**2
      alst = 3./8./pi**2*htst**2
      al1 = 3./8./pi**2*hb**2

      al(1,1) = al1
      al(1,2) = (al2+al1)/2.
      al(2,1) = (al2+al1)/2.
      al(2,2) = al2

      mtop4 = rmtop**4.*(1.+2.*bt2*tt- al2*tt)
      mtop2 = mtop4**.5
      mbot4 = mb**4.*(1.+2.*bb2*tb - al1*tb)
      mbot2 = mbot4**.5

Cpaj  vi = 174.1*(1. + 3./32./pi**2*htst**2*
Cpaj
      IF ( ma .LE. mtop ) THEN
        mm = mtop
      ELSE
        mm = ma
      ENDIF
Cpaj
      vi =   v  *(1. + 3./32./pi**2*htst**2*
     *log(mtop**2/mm**2))
      h1i = vi* cosbA
      h2i = vi*sinbA
      h1t = h1i*(1.+3./8./pi**2*hb**2*log(mm**2/msusyt**2))**.25
      h2t = h2i*(1.+3./8./pi**2*ht**2*log(mm**2/msusyt**2))**.25
      h1b = h1i*(1.+3./8./pi**2*hb**2*log(mm**2/msusyb**2))**.25
      h2b = h2i*(1.+3./8./pi**2*ht**2*log(mm**2/msusyb**2))**.25

      tanbst = h2t/h1t
      sinbt = tanbst/(1.+tanbst**2)**.5
      cosbt = sinbt/tanbst

      tanbsb = h2b/h1b
      sinbb = tanbsb/(1.+tanbsb**2)**.5
      cosbb = sinbb/tanbsb

      stop12 = (mq2 + mur2)*.5 + mtop2
     *  +1./8.*(g2**2+g1**2)*(h1t**2-h2t**2)
     *   +(((g2**2-5.*g1**2/3.)/4.*(h1t**2-h2t**2) +
     *   mq2 - mur2)**2*0.25 + mtop2*(At-mu/tanbst)**2)**.5
      stop22 = (mq2 + mur2)*.5 + mtop2
     *  +1./8.*(g2**2+g1**2)*(h1t**2-h2t**2)
     *   - (((g2**2-5.*g1**2/3.)/4.*(h1t**2-h2t**2) +
     *  mq2 - mur2)**2*0.25
     *  + mtop2*(At-mu/tanbst)**2)**.5
      if(stop22.lt.0.) goto 4237
      sbot12 = (mq2 + md2)*.5
     *   - 1./8.*(g2**2+g1**2)*(h1b**2-h2b**2)
     *  + (((g1**2/3.-g2**2)/4.*(h1b**2-h2b**2) +
     *  mq2 - md2)**2*0.25 + mbot2*(Ab-mu*tanbsb)**2)**.5
      sbot22 = (mq2 + md2)*.5
     *   - 1./8.*(g2**2+g1**2)*(h1b**2-h2b**2)
     *   - (((g1**2/3.-g2**2)/4.*(h1b**2-h2b**2) +
     *   mq2 - md2)**2*0.25 + mbot2*(Ab-mu*tanbsb)**2)**.5
      if(sbot22.lt.0.) goto 4237

      stop1 = stop12**.5
      stop2 = stop22**.5
      sbot1 = sbot12**.5
      sbot2 = sbot22**.5
C
      vh1(1,1) = 1./tanbst
      vh1(2,1) = -1.
      vh1(1,2) = -1.
      vh1(2,2) = tanbst
      vh2(1,1) = tanbst
      vh2(1,2) = -1.
      vh2(2,1) = -1.
      vh2(2,2) = 1./tanbst
cccccccccccccccccccccccccccccccc
ccc   D-terms
cccccccccccccccccccccccccccccccc
Cpaj  stw=.2315

      f1t=(mq2-mur2)/(stop12-stop22)*(.5-4./3.*stw)*
     *         log(stop1/stop2)
     *        +(.5-2./3.*stw)*log(stop1*stop2/(mq2+mtop2))
     *        + 2./3.*stw*log(stop1*stop2/(mur2+mtop2))

      f1b=(mq2-md2)/(sbot12-sbot22)*(-.5+2./3.*stw)*
     *        log(sbot1/sbot2)
     *        +(-.5+1./3.*stw)*log(sbot1*sbot2/(mq2+mbot2))
     *        - 1./3.*stw*log(sbot1*sbot2/(md2+mbot2))

      f2t=mtop2**.5*(at-mu/tanbst)/(stop12-stop22)*
     *         (-.5*log(stop12/stop22)
     *        +(4./3.*stw-.5)*(mq2-mur2)/(stop12-stop22)*
     *         gcaren(stop12,stop22))

      f2b=mbot2**.5*(ab-mu*tanbsb)/(sbot12-sbot22)*
     *         (.5*log(sbot12/sbot22)
     *        +(-2./3.*stw+.5)*(mq2-md2)/(sbot12-sbot22)*
     *        gcaren(sbot12,sbot22))

      vh3b(1,1) = mbot4/(cosbb**2)*(log(sbot1**2*sbot2**2/
     *  (mq2+mbot2)/(md2+mbot2))
     *  + 2.*(Ab*(Ab-mu*tanbsb)/(sbot1**2-sbot2**2))*
     *  log(sbot1**2/sbot2**2)) +
     *  Mbot4/(cosbb**2)*(Ab*(Ab-mu*tanbsb)/
     *  (sbot1**2-sbot2**2))**2*gcaren(sbot12,sbot22)

      vh3t(1,1) =
     *  mtop4/(sinbt**2)*(mu*(-At+mu/tanbst)/(stop1**2
     * -stop2**2))**2*gcaren(stop12,stop22)

      vh3b(1,1)=vh3b(1,1)+
     *    mz**2*(2*mbot2*f1b-mbot2**.5*ab*f2b)

      vh3t(1,1) = vh3t(1,1) +
     *  mz**2*(mtop2**.5*mu/tanbst*f2t)

      vh3t(2,2) = mtop4/(sinbt**2)*(log(stop1**2*stop2**2/
     *  (mq2+mtop2)/(mur2+mtop2))
     *  + 2.*(At*(At-mu/tanbst)/(stop1**2-stop2**2))*
     *  log(stop1**2/stop2**2)) +
     *  mtop4/(sinbt**2)*(At*(At-mu/tanbst)/
     *  (stop1**2-stop2**2))**2*gcaren(stop12,stop22)

      vh3b(2,2) =
     *  Mbot4/(cosbb**2)*(mu*(-Ab+mu*tanbsb)/(sbot1**2
     * -sbot2**2))**2*gcaren(sbot12,sbot22)

      vh3t(2,2)=vh3t(2,2)+
     *    mz**2*(-2*mtop2*f1t+mtop2**.5*at*f2t)

      vh3b(2,2) = vh3b(2,2) -mz**2*mbot2**.5*mu*tanbsb*f2b

      vh3t(1,2) = -
     *   mtop4/(sinbt**2)*mu*(At-mu/tanbst)/
     * (stop1**2-stop2**2)*(log(stop1**2/stop2**2) + At*
     * (At - mu/tanbst)/(stop1**2-stop2**2)*gcaren(stop12,stop22))

      vh3b(1,2) =
     * - mbot4/(cosbb**2)*mu*(At-mu*tanbsb)/
     * (sbot1**2-sbot2**2)*(log(sbot1**2/sbot2**2) + Ab*
     * (Ab - mu*tanbsb)/(sbot1**2-sbot2**2)*gcaren(sbot12,sbot22))

      vh3t(1,2)=vh3t(1,2) +
     *      mz**2*(mtop2/tanbst*f1t-mtop2**.5*(at/tanbst+mu)/2.*f2t)

      vh3b(1,2)=vh3b(1,2)
     *  +mz**2*(-mbot2*tanbsb*f1b+mbot2**.5*(ab*tanbsb+mu)/2.*f2b)

      vh3t(2,1) = vh3t(1,2)
      vh3b(2,1) = vh3b(1,2)

      tq = log((mq2 + mtop2)/mtop2)
      tu = log((mur2+mtop2)/mtop2)
      tqd = log((mq2 + mb**2)/mb**2)
      td = log((md2+mb**2)/mb**2)


      DO 8910 I = 1,2
      DO 8911 J = 1,2
        vh(i,j) =
     *  6./(8.*pi**2*(h1t**2+h2t**2))
     *  *vh3t(i,j)*0.5*(1.-al(i,j)*tt/2.) +
     *  6./(8.*pi**2*(h1b**2+h2b**2))
     *  *vh3b(i,j)*0.5*(1.-al(i,j)*tb/2.)
 8911 CONTINUE
 8910 CONTINUE

      GOTO 4236
 4237 DO 6868 I =1,2
      DO 6867 J = 1,2
        vh(i,j) = -1.d+15
 6867 CONTINUE
 6868 CONTINUE
C
 4236 RETURN
      END
      SUBROUTINE gsub3(m,u3,f3,x)
C--------------------------------------------------------------------
C! Three routines for the WW/ZZ-->h cross section including h width
C
C  Patrick Janot -- 01 Sep 1995
C--------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,K,O-Z)
      COMMON / sigww / kappah, kappav, v, a, rmh, rgh, ss
      EXTERNAL gsub2
      DIMENSION u3(*),f3(*),x(3)
C
      DO l = 1,m
        x(3)  = u3(l)
        u = DSQRT(rmh**2+rmh*rgh*DTAN(x(3)))
        kappah = u**2/ss
        x2lo  = kappah
        x2hi  = 1.
        f3(l) = DGMLT2(gsub2,x2lo,x2hi,1,6,x)
      ENDDO
C
      RETURN
      END
      SUBROUTINE gsub2(m,u2,f2,x)
      IMPLICIT REAL*8(A-H,K,O-Z)
      EXTERNAL gsub1
      DIMENSION  u2(*),f2(*),x(3)
C
      DO l = 1,m
        x(2)  = u2(l)
        x1lo  = 1.
        x1hi  = 1./x(2)**2
        f2(l) = DGMLT1(gsub1,x1lo,x1hi,1,6,x)
      ENDDO
C
      RETURN
      END
      SUBROUTINE gsub1(m,u1,f1,x)
      IMPLICIT REAL*8(A-H,K,O-Z)
      COMMON / sigww / kappah, kappav, v, a, rmh, rgh, ss
      DIMENSION u1(*),f1(*),x(3)
C
      DO l = 1,m
        x(1) = u1(l)
        y = 1./DSQRT(x(1))
        z = y * (x(2)-kappah) / (kappav * x(2))
        f = ( 2.*x(2)/y**3 - (1.+2.*x(2))/y**2
     .     + (2.+x(2))/(2.*y) - 1./2. )
     .    * ( z/(1.+z) - DLOG(1.+z) )
     .    + x(2)*z**2*(1.-y)/(y**3*(1.+z))
        g = (-x(2)/y**2 + (2.+x(2))/(2.*y) - 1./2.)
     .    * ( z/(1.+z) - DLOG(1.+z) )
        f1(l) = ( (v**2+a**2)**2 * f + 4.*v**2*a**2 * g )
     .        / ( 1. + (y-x(2))/kappav )**2
     .        * y**3 / 2.
      ENDDO
C
      RETURN
      END
      FUNCTION gtau(tau)
C------------------------------------------------------------------
C! Famous function appearing in H --> Z gamma partial width
C
C  Patrick Janot -- 23 Sep 1995
C------------------------------------------------------------------
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
C
      COMPLEX gtau
C
      IF ( tau .GE. 1 ) THEN
        gtaur = SQRT(tau-1.) * ASIN(SQRT(1./tau))
        gtaui = 0.
      ELSEIF ( tau .GE. 1E-5 ) THEN
        b0 = SQRT(1.-tau)
        gtaur = b0 * ALOG((1.+b0)/(1-b0)) / 2.
        gtaui = -pi * b0 / 2.
      ELSE
        gtaur = -ALOG(tau/4.) / 2.
        gtaui = -pi
      ENDIF
C
      gtau = CMPLX(gtaur,gtaui)
C
      RETURN
      END
      FUNCTION gzi1(jhig,am)
C--------------------------------------------------------------------
C! Utility routine to compute the Z gamma width
C
C Input:    jhig,   Higgs type
C           am  ,   the mass of the the loop particle
C
C Output:   gzi1,   the value of the function
C
C Patrick Janot -- 23 sep 1995
C--------------------------------------------------------------------
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      COMPLEX ftau, gtau, gzi1
      EXTERNAL ftau, gtau
C
      a = 4.*am**2/amhig(jhig)**2
      b = 4.*am**2/amz**2
C
      gzi1 = a*b       / 2. / (a-b)
     .     + a**2*b**2 / 2. / (a-b)**2 * (ftau(a)-ftau(b))
     .     + a**2*b         / (a-b)**2 * (gtau(a)-gtau(b))
C
      RETURN
      END
      FUNCTION gzi2(jhig,am)
C--------------------------------------------------------------------
C! Utility routine to compute the Z gamma width
C
C Input:    jhig,   Higgs type
C           am  ,   the mass of the the loop particle
C
C Output:   gzi2,   the value of the function
C
C Patrick Janot -- 23 sep 1995
C--------------------------------------------------------------------
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      COMPLEX ftau, gtau, gzi2
      EXTERNAL ftau, gtau
C
      a = 4.*am**2/amhig(jhig)**2
      b = 4.*am**2/amz**2
C
      gzi2 = -a*b       / 2. / (a-b)    * (ftau(a)-ftau(b))
C
      RETURN
      END
      SUBROUTINE hhdecay(ph,jchan,jhig,iph)
C--------------------------------------------------------------------
C! Main routine to decay Higgses of the MSSM.
C  The Higgs is decayed in its rest frame. Then, a boost is performed
C  for going back to the lab.
C
C  Input:     Passed:   --PH(4),   Higgs quadriimpulsion (in the lab)
C                                  with the format :
C                                  PH(1,2,3)    momentum (in GeV)
C                             -->  PH(4)        Mass        "
C                       --JHIG,    higgs type ( 1:H, 2:h, 3:A )
C                       --IPH,     (optional) Row number in
C                                  the LUJETS common block
C  Output:    Passed:   --JCHAN,   channel selected
C
C   P. Janot  --  26 Aug 1991
C-----------------------------------------------------------------------
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      CHARACTER*14 channel
      CHARACTER*21 channeut, chanchar
      PARAMETER(nchneut=8,nchchar=5)
      COMMON / hhdeck / branch(nchan,nhig),width(nhig),
     .                  parwth(nhig),xymas(2,nchan,nhig),
     .                  xywid(2,nchan,nhig)
      COMMON / chaneu / ichn(2),
     .                  wneut(4,4,nhig), wchar(2,2,nhig),
     .                  widneut(4), brneut(nchneut,4),
     .                  widchar(2), brchar(nchchar,2)
      COMMON / chanch / channeut(nchneut,4), chanchar(nchchar,2)
      COMMON / vect4 / pvect4(5,2)
      COMMON / hinput / ichan(nchan,nhig), channel(nchan,nhig)
      COMMON / hisbr / bchpp,bchgg
      DIMENSION ph(4)
C
      COMMON / h0evt / kevt(nchan,nhig)
C
      DIMENSION brloc(nchan,nhig)
      DIMENSION wnloc(4,4,nhig),wcloc(2,2,nhig)
C
      IF ( jhig .LE. 0 .OR. jhig .GT. nhig ) THEN
        WRITE(6,*) '+++ HHDECAY +++ Wrong Higgs number'
        STOP 99
      ENDIF
C
      IF ( parwth(jhig) .EQ. 0.) THEN
        WRITE(6,1000) jhig
        STOP 99
      ENDIF
C
C  Simply get rid of the decay channels suddenly closed due
C  to too low a value of the generated Higgs mass (due to the
C  decay width)
C
      CALL ucopy(branch(1,1),brloc(1,1),nchan*nhig)
      CALL ucopy(wneut(1,1,1),wnloc(1,1,1),4*4*nhig)
      CALL ucopy(wchar(1,1,1),wcloc(1,1,1),2*2*nhig)
C
      IF ( ph(4) .LT. 1.       ) branch( 2,jhig) = 0.
      IF ( ph(4) .LT. 2.*amtau ) branch( 3,jhig) = 0.
      IF ( ph(4) .LT. 2.*amc   ) branch( 4,jhig) = 0.
      IF ( ph(4) .LT. 2.*amb   ) branch( 5,jhig) = 0.
      IF ( ph(4) .LT. 2.*amt   ) branch( 6,jhig) = 0.
      IF ( ph(4) .LT. 20.      ) branch( 7,jhig) = 0.
      IF ( ph(4) .LT. 20.      ) branch( 8,jhig) = 0.
      IF ( ph(4) .LT. 2.*ama  .AND. jhig .LE. 2 )
     .                           branch( 9,jhig) = 0.
      IF ( ph(4) .LT. amh+amz  ) branch( 9,3   ) = 0.
      IF ( ph(4) .LT. 2.*amh   ) branch(10,jhig) = 0.
      IF ( ph(4) .LT. 20.      ) branch(11,jhig) = 0.
      IF ( ph(4) .LT. 2*ame    ) branch(12,jhig) = 0.
      IF ( ph(4) .LT. 2*ammu   ) branch(13,jhig) = 0.
      IF ( ph(4) .LT. 2*ams    ) branch(14,jhig) = 0.
C
      IF ( branch(15,jhig) .GT. 0. ) THEN
        frn = 0.
        DO i = 1, 4
          DO j = 1, 4
            IF ( ph(4).LT.amneut(i)+amneut(j) ) THEN
              wneut(i,j,jhig) = 0.
            ELSE
              frn = frn + wneut(i,j,jhig)
            ENDIF
          ENDDO
        ENDDO
        branch(15,jhig) = branch(15,jhig) * frn
      ENDIF
C
      IF ( branch(16,jhig) .GT. 0. ) THEN
        frc = 0.
        DO i = 1, 2
          DO j = 1, 2
            IF ( ph(4).LT.amchar(i)+amchar(j) ) THEN
              wchar(i,j,jhig) = 0.
            ELSE
              frc = frc + wchar(i,j,jhig)
            ENDIF
          ENDDO
        ENDDO
        branch(16,jhig) = branch(16,jhig) * frc
      ENDIF
C
      frch = 0.
      DO ich = 1, nchan
        frch = frch + branch(ich,jhig)
      ENDDO
C
C  Choice of the channel.
C
      IF ( frch .EQ. 0. ) THEN
        jchan = 1
         GOTO 30
      ENDIF
C
      rnch = RNDM(dummy)
      rint = 0.D0
      DO jchan = 1 , nchan
        rint = rint + branch(jchan,jhig)/frch
        if ( rnch .lt. rint ) GOTO 30
      ENDDO
  30  CONTINUE
      IF (idbg .GE. 10) WRITE(6,1001) rnch,rint,jchan,jhig
C
      kevt(jchan,jhig) = kevt(jchan,jhig) + 1
C
C  If a SUSY decay (chi chi or chi+ chi-) has been chosen, select
C  which one precisely
C
      rnch = RNDM(dummy)
      rint = 0.D0
C
      IF ( jchan .EQ. 15 ) THEN
C
        DO i = 1, 4
          DO j = 1, 4
            rint = rint + wneut(i,j,jhig)/frn
            IF ( rnch .LT. rint ) THEN
              xymas(1,15,jhig) = ABS(amneut(i))
              xymas(2,15,jhig) = ABS(amneut(j))
              xywid(1,15,jhig) = 0.
              xywid(2,15,jhig) = 0.
              ichi = 0
              ichn(1) = i
              ichn(2) = j
              IF ( idbg .GE. 10 ) THEN
                WRITE(6,*) 'Neutralino decay : ',i,j
                WRITE(6,*) 'Masses : ',ABS(amneut(i)),ABS(amneut(j))
              ENDIF
              GOTO 40
            ENDIF
          ENDDO
        ENDDO
C
      ELSEIF ( jchan .EQ. 16 ) THEN
C
        DO i = 1, 2
          DO j = 1, 2
            rint = rint + wchar(i,j,jhig)/frc
            IF ( rnch .LT. rint ) THEN
              xymas(1,16,jhig) = ABS(amchar(i))
              xymas(2,16,jhig) = ABS(amchar(j))
              xywid(1,16,jhig) = 0.
              xywid(2,16,jhig) = 0.
              ichi = 1
              ichn(1) = i
              ichn(2) = j
              IF ( idbg .GE. 10 ) THEN
                WRITE(6,*) 'Chargino decay : ',i,j
                WRITE(6,*) 'Masses : ',ABS(amchar(i)),ABS(amchar(j))
              ENDIF
              GOTO 40
            ENDIF
          ENDDO
        ENDDO
C
      ENDIF
C
      CALL ucopy(brloc(1,1),branch(1,1),nchan*nhig)
      CALL ucopy(wnloc(1,1,1),wneut(1,1,1),4*4*nhig)
      CALL ucopy(wcloc(1,1,1),wchar(1,1,1),2*2*nhig)
C
C  Build quadriimpulsions of the 2 decay particles
C
   40 CALL pick4(ph,jchan,jhig)
C
C  Fill LUJETS common block. Lorentz-boost back to the lab.
C
      CALL hhlujt(jchan,jhig,ph,iph)
C
  999 RETURN
C------------------------------------------------------------------
1000  FORMAT(1X,' +++ HHDECAY +++   No accessible decay channel',
     .          ' for Higgs # ',I1)
1001  FORMAT(1X,' +++ HHDECAY +++ RNDM : ',F8.6,' < ? ',F8.6,
     .       ' Channel ',I3,', Type ',I3,' Higgs # ',I1)
      END
      SUBROUTINE HHFINI
C----------------------------------------------------------------------
C! Termination for neutral Higgs decays routines (from SM or MSSM)
C
C   P. Janot -- 24 August 1991
C----------------------------------------------------------------------
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      CHARACTER*14 channel
      CHARACTER*21 channeut, chanchar
      PARAMETER(nchneut=8,nchchar=5)
      COMMON / hhdeck / branch(nchan,nhig),width(nhig),
     .                  parwth(nhig),xymas(2,nchan,nhig),
     .                  xywid(2,nchan,nhig)
      COMMON / chaneu / ichn(2),
     .                  wneut(4,4,nhig), wchar(2,2,nhig),
     .                  widneut(4), brneut(nchneut,4),
     .                  widchar(2), brchar(nchchar,2)
      COMMON / chanch / channeut(nchneut,4), chanchar(nchchar,2)
      COMMON / vect4 / pvect4(5,2)
      COMMON / hinput / ichan(nchan,nhig), channel(nchan,nhig)
      COMMON / hisbr / bchpp,bchgg
      DIMENSION ph(4)
C
      COMMON / h0evt / kevt(nchan,nhig)
C
      WRITE(6,1000)
      WRITE(6,1001) (jchan, jchan=1,nchan)
      WRITE(6,1002) (jhig,(kevt(jchan,jhig),jchan=1,nchan),jhig=1,nhig)
C----------------------------------------------------------------------
 1000 FORMAT(1x,' Total number of events in various decay channels :'/)
 1001 FORMAT(4x,16(4x,I2))
 1002 FORMAT(3(1x,I2,1x,16(I6)/))
      RETURN
      END
      SUBROUTINE HHINIT
C------------------------------------------------------------------
C!  Initialisation for neutral Higgs decays routines(from SM or MSSM)
C           . h0 lightest scalar
C           . H0 heaviest scalar    mh < 2m_mu or mh > ~ GeV/c2
C           . A  pseudoscalar
C
C  Input:     none
C
C
C  Output:   /HHDECK/   --BRANCH(ichan,ihig), branching ratios
C                       --WIDTH(ihig), total widths
C
C   P. Janot -- 24 August 1991
C------------------------------------------------------------------
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      COMMON / h0evt / kevt(nchan,nhig)
C
      COMMON / miscl / loutbe,ecm,idb1,idb2,xrad,empir,empirm,ism,
     &                 iklei,icar,sdvrt(3),vrtx(4),tabl(26),
     &                 nevent(11)
      INTEGER loutbe,idb1,idb2,nevent,ism,iklei,icar
      REAL*4 ecm,sdvrt,vrtx,tabl,empir,empirm,xrad
C
C
C  Set event numbers to zero
C
      CALL vzero(kevt(1,1),nchan*nhig)
C
C  Set defaults conditions
C
      CALL setdef
C
C  Set user's conditions if requested
C
      CALL usrdef
C
C  Determine parameters, couplings...
C
      CALL parcou
      IF ( amh .LT. 0. ) RETURN
      IF ( ism .EQ. 0 .AND. amst(1) .LT. 0. ) RETURN
C
C  Compute decay widths and branching ratios
C
      CALL combra
C
  999 RETURN
      END
      SUBROUTINE hhlujt(jchan,jhig,ph,iph)
C------------------------------------------------------------------
C! Fill LUJETS common block after a Higgs boson decay.
C
C  Patrick Janot -- 26 Aug 1991
C------------------------------------------------------------------
      PARAMETER (L1MST=200, L1PAR=200)
      PARAMETER (L2PAR=500, L2PARF=2000 )
      PARAMETER (LJNPAR=4000)
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),
     &                KFDP(L2PARF,5)
      COMMON /LUDAT4/ CHAF(L2PAR)
      CHARACTER*8 CHAF
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5)
C
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      CHARACTER*14 channel
      CHARACTER*21 channeut, chanchar
      PARAMETER(nchneut=8,nchchar=5)
      COMMON / hhdeck / branch(nchan,nhig),width(nhig),
     .                  parwth(nhig),xymas(2,nchan,nhig),
     .                  xywid(2,nchan,nhig)
      COMMON / chaneu / ichn(2),
     .                  wneut(4,4,nhig), wchar(2,2,nhig),
     .                  widneut(4), brneut(nchneut,4),
     .                  widchar(2), brchar(nchchar,2)
      COMMON / chanch / channeut(nchneut,4), chanchar(nchchar,2)
      COMMON / vect4 / pvect4(5,2)
      COMMON / hinput / ichan(nchan,nhig), channel(nchan,nhig)
      COMMON / hisbr / bchpp,bchgg
      DIMENSION ph(4)
C
      COMMON / miscl / loutbe,ecm,idb1,idb2,xrad,empir,empirm,ism,
     &                 iklei,icar,sdvrt(3),vrtx(4),tabl(26),
     &                 nevent(11)
      INTEGER loutbe,idb1,idb2,nevent,ism,iklei,icar
      REAL*4 ecm,sdvrt,vrtx,tabl,empir,empirm,xrad
C
      DIMENSION ijoin(2), jjoin(2)
      DIMENSION p1(4),p2(4),p3(4)
C
      IF ( jchan .EQ. 1 ) THEN
        kf1 = 22
        kf2 = 22
      ELSEIF ( jchan .EQ. 2 ) THEN
        kf1 = 21
        kf2 = 21
      ELSEIF ( jchan .EQ. 3 ) THEN
        kf1 = 15
        kf2 =-15
      ELSEIF ( jchan .EQ. 4 ) THEN
        kf1 = 4
        kf2 =-4
      ELSEIF ( jchan .EQ. 5 ) THEN
        kf1 = 5
        kf2 =-5
      ELSEIF ( jchan .EQ. 6 ) THEN
        kf1 = 6
        kf2 =-6
      ELSEIF ( jchan .EQ. 7 ) THEN
        kf1 = 24
        kf2 =-24
      ELSEIF ( jchan .EQ. 8 ) THEN
        kf1 = 23
        kf2 = 23
      ELSEIF ( jchan .EQ. 9 ) THEN
        IF ( jhig .NE. 3 ) THEN
          kf1 = 36
          kf2 = 36
        ELSE
          kf1 = 25
          kf2 = 23
        ENDIF
      ELSEIF ( jchan .EQ. 10 ) THEN
        kf1 = 25
        kf2 = 25
      ELSEIF ( jchan .EQ. 11 ) THEN
        kf1 = 22
        kf2 = 23
      ELSEIF ( jchan .EQ. 12 ) THEN
        kf1 = 11
        kf2 =-11
      ELSEIF ( jchan .EQ. 13 ) THEN
        kf1 = 13
        kf2 =-13
      ELSEIF ( jchan .EQ. 14 ) THEN
        kf1 = 3
        kf2 =-3
      ELSEIF ( jchan .EQ. 15 ) THEN
        IF ( ism .EQ. 1 ) THEN
          kf1 = 51
          kf2 = 51
        ELSE
          kf1 = 50+ichn(1)
          kf2 = 50+ichn(2)
        ENDIF
      ELSEIF ( jchan .EQ. 16 ) THEN
        kf1 = 54+ichn(1)
        kf2 =-54-ichn(2)
      ENDIF
C
      n7lu = n7lu + 1
      ijoin(1) = n7lu
      CALL hhlu1(ijoin(1),kf1,pvect4(1,1),pvect4(2,1),
     .                    pvect4(3,1),pvect4(4,1),pvect4(5,1))
      k7lu(iph,4) = n7lu
      k7lu(ijoin(1),3) = iph
C
      n7lu = n7lu + 1
      ijoin(2) = n7lu
      CALL hhlu1(ijoin(2),kf2,pvect4(1,2),pvect4(2,2),
     .                    pvect4(3,2),pvect4(4,2),pvect4(5,2))
      k7lu(iph,5) = n7lu
      k7lu(ijoin(2),3) = iph
C
C Parton shower preparation in case of a quark system
C
      IF ( (jchan.GE.4 .AND. jchan.LE.6) .OR.
     &                       jchan.EQ.2  .OR.
     &                       jchan.EQ.14      ) THEN
        k7lu(ijoin(1),1) = 2
        njoin = 2
        CALL lujoin(njoin,ijoin)
        xmm = p7lu(iph,5)
        CALL lushow(ijoin(1), ijoin(2), xmm)
      ENDIF
C
C Decay and parton shower if WW or ZZ
C
      IF ( jchan .EQ. 7 .OR. jchan .EQ. 8 ) THEN
        CALL wzdecy(ijoin(1))
        CALL wzdecy(ijoin(2))
      ENDIF
C
      k7lu(iph,1) = 11
C
      IF ( idbg .GE. 5 ) CALL lulist(1)
  999 RETURN
      END
      SUBROUTINE hhlu1(ipa,kf,px,py,pz,pe,pm)
C------------------------------------------------------------------
C! Add one entry to the LUND event record
C
C  Patrick Janot -- 26 Aug 1991
C------------------------------------------------------------------
      PARAMETER (L1MST=200, L1PAR=200)
      PARAMETER (L2PAR=500, L2PARF=2000 )
      PARAMETER (LJNPAR=4000)
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),
     &                KFDP(L2PARF,5)
      COMMON /LUDAT4/ CHAF(L2PAR)
      CHARACTER*8 CHAF
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5)
C
C
      qm = pm
      IF ( qm .LT. 0. ) qm = SQRT(pe**2-px**2-py**2-pz**2)
      DO 100 J=1,5
      k7lu(ipa,j)=0
      p7lu(ipa,j)=0.
  100 v7lu(ipa,j)=0.
C...Store parton/particle in K and P vectors.
      k7lu(ipa,1)=1
      k7lu(ipa,2)=kf
      p7lu(ipa,5)=qm
      p7lu(ipa,4)=pe
      p7lu(ipa,1)=px
      p7lu(ipa,2)=py
      p7lu(ipa,3)=pz
C
  999 RETURN
      END
      SUBROUTINE hzha(ipro,ecms,qg1,qg2,qh,qa,hh,izpol)
C-----------------------------------------------------------------------
C! Event generation for the various Higgs production processes
C
C  Input :       ipro,   the process Id
C                ecms,   the centre-of-mass energy
C
C  Output:       qg1[4]
C                qg2[4], the initial state photons
C                qh[4]
C                qa[4]
C                hh[4],  the final state particles (Higgs,Z,fermions)
C                izpol,  the Z polarization, when IKLEI=0, for hZ.
C
C   Patrick Janot -- 27 Aug 1991
C----------------------------------------------------------------------
      DIMENSION qg1(4),qg2(4),qh(4),qa(4),rh(4),ra(4),rg2(4)
      DIMENSION gh(4),hh(4)
      LOGICAL first
      DATA first/.TRUE./
      PARAMETER (L1MST=200, L1PAR=200)
      PARAMETER (L2PAR=500, L2PARF=2000 )
      PARAMETER (LJNPAR=4000)
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),
     &                KFDP(L2PARF,5)
      COMMON /LUDAT4/ CHAF(L2PAR)
      CHARACTER*8 CHAF
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5)
C
      PARAMETER (maxpro= 9)
      COMMON / cropro / cross(maxpro), sthr(maxpro), reduc(maxpro)
      CHARACTER*14 chapro(maxpro)
      DATA chapro /
     .             'e+e- --> h Z',
     .             'e+e- --> H Z',
     .             'e+e- --> h A',
     .             'e+e- --> H A',
     .             'W+W- --> h  ',
     .             'W+W- --> H  ',
     .             'Z Z  --> h  ',
     .             'Z Z  --> H  ',
     .             'e+e- --> H+H-'
     .                                /
      COMMON / prosim / iproyn(maxpro),nevpro(maxpro)
      COMMON / miscl / loutbe,ecm,idb1,idb2,xrad,empir,empirm,ism,
     &                 iklei,icar,sdvrt(3),vrtx(4),tabl(26),
     &                 nevent(11)
      INTEGER loutbe,idb1,idb2,nevent,ism,iklei,icar
      REAL*4 ecm,sdvrt,vrtx,tabl,empir,empirm,xrad
C
      EXTERNAL sigma1,sigma2,sigma3,sigma4
      EXTERNAL sigma5,sigma6,sigma7,sigma8,sigma9
      EXTERNAL sigmat
C
C
      s = ecms**2
      e = ecms/2.
      CALL vzero(qg1(1),4)
      CALL vzero(qg2(1),4)
      CALL vzero(rg2(1),4)
      CALL vzero(qh(1),4)
      CALL vzero(ra(1),4)
      CALL vzero(rh(1),4)
      CALL vzero(qa(1),4)
      CALL vzero(hh(1),4)
      CALL vzero(gh(1),4)
C
C  First compute the total cross section with Brems- or beams-strahlung
C  (if requested)
C
      IF ( first ) THEN
C
        CALL vzero(sthr(1),maxpro)
C
C Bremsstrahlung- Beamstrahlung
C
        IF ( xrad .GT. 0. ) THEN
C
          IF ( iklei .EQ. 0 ) THEN
            sthr(1) = (pmas(23,1)+pmas(25,1)-10.*pmas(23,2))**2
            sthr(2) = (pmas(23,1)+pmas(35,1)-10.*pmas(23,2))**2
          ELSE
            sthr(1) = 0.1
            sthr(2) = 0.1
          ENDIF
          sthr(3) = 0.1
          sthr(4) = 0.1
          sthr(5) = 0.1
          sthr(6) = 0.1
          sthr(7) = 0.1
          sthr(8) = 0.1
          sthr(9) = 0.1
C
          IF (cross(1)*iproyn(1)*reduc(1) .GT. 0.)
     .      CALL remt1(e,sigma1,sthr(1),1,cross(1),xrad)
          IF (cross(2)*iproyn(2)*reduc(2) .GT. 0.)
     .      CALL remt1(e,sigma2,sthr(2),2,cross(2),xrad)
          IF (cross(3)*iproyn(3)*reduc(3) .GT. 0.)
     .      CALL remt1(e,sigma3,sthr(3),3,cross(3),xrad)
          IF (cross(4)*iproyn(4)*reduc(4) .GT. 0.)
     .      CALL remt1(e,sigma4,sthr(4),4,cross(4),xrad)
          IF (cross(5)*iproyn(5)*reduc(5) .GT. 0.)
     .      CALL remt1(e,sigma5,sthr(5),5,cross(5),xrad)
          IF (cross(6)*iproyn(6)*reduc(6) .GT. 0.)
     .      CALL remt1(e,sigma6,sthr(6),6,cross(6),xrad)
          IF (cross(7)*iproyn(7)*reduc(7) .GT. 0.)
     .      CALL remt1(e,sigma7,sthr(7),7,cross(7),xrad)
          IF (cross(8)*iproyn(8)*reduc(8) .GT. 0.)
     .      CALL remt1(e,sigma8,sthr(8),8,cross(8),xrad)
          IF (cross(9)*iproyn(9)*reduc(9) .GT. 0.)
     .      CALL remt1(e,sigma9,sthr(9),9,cross(9),xrad)
C
          WRITE(6,1000) (chapro(i),cross(i),iproyn(i),i=1,maxpro)
 1000     FORMAT(
     .     //20x,'------------------------------------------------'/
     .       20x,'Final cross sections :'//
     .     9(20x,'     o ',A14,'  : ',E10.4,' fb  (',I1,')'/)/
     .       20x,' (0) = Channel not requested, only Born level.'/
     .       20x,' (1) = Channel requested, ISR included.'/
     .       20x,'------------------------------------------------'//)
        ENDIF
        first = .FALSE.
      ENDIF
C
C  Event generation
C
      IF ( xrad .GT. 0. ) THEN
    1   CALL remt2(qg1,rg2,ipro)
        s1 = s * (1.-qg1(4)/e)
        e1 = SQRT(s1)/2.
        s2 = s1 * (1.-rg2(4)/e1)
        IF ( s2 .LE. sthr(ipro) ) THEN
          WRITE(6,*) ' *** Warning *** Not enough energy for production'
     .               ,s2,' < ',sthr(ipro),' !'
          GOTO 1
        ENDIF
        CALL dsigma(ipro,s2,qh,qa,hh,izpol)
C
        IF ( ipro .LT. 5 .OR. ipro .EQ. 9 ) THEN
          qhm = qh(4)
          qam = qa(4)
          qh(4) = SQRT(qh(1)**2+qh(2)**2+qh(3)**2+qh(4)**2)
          qa(4) = SQRT(qa(1)**2+qa(2)**2+qa(3)**2+qa(4)**2)
        ELSE
          hhm = hh(4)
          hh(4) = SQRT(hh(1)**2+hh(2)**2+hh(3)**2+hh(4)**2)
        ENDIF
C
        CALL remt3(qh, rh, 2)
        CALL remt3(qa, ra, 2)
        CALL remt3(rh, qh, 1)
        CALL remt3(ra, qa, 1)
        IF ( ( ipro.GE.5 .AND. ipro.NE.9 ) .OR.
     .       ( ipro.LE.2.AND.iklei.EQ.1 ) ) THEN
          CALL remt3(hh, gh, 2)
          CALL remt3(gh, hh, 1)
        ENDIF
        CALL remt3(rg2,qg2,1)
C
        IF ( ipro .LT. 5 .OR. ipro .EQ. 9 ) THEN
          qh(4) = qhm
          qa(4) = qam
        ELSE
          hh(4) = hhm
        ENDIF
C
      ELSE
        call dsigma(ipro,s,qh,qa,hh,izpol)
      ENDIF
C
C  End of event generation
C
      RETURN
      END
      SUBROUTINE LORENZ(BETAX,BETAY,BETAZ,P)
C-------------------------------------------------------------------
C!  Perform a Lorentz transformation of the quadriimpulsion P
C   from frame 1 to frame 2
C
C   Input:     Passed:    --BETAX,Y,Z    2's velocity / 1
C                         --P,           quadriimpulsion in 1
C
C   Output:    Passed:    --P,           quadriimpulsion in 2
C
C   P. Janot  --  20 oct 1988
C-------------------------------------------------------------------
      IMPLICIT REAL*8 (A-Z)
      REAL*4 P(4)
      BETA2 = BETAX**2 + BETAY**2 + BETAZ**2
      IF(BETA2 .EQ. 0.) RETURN
      GAMMA = 1./SQRT(1.-BETA2)
      ONE   = BETAX*P(1) + BETAY*P(2) + BETAZ*P(3)
      TWO   = (GAMMA-1.)*ONE/BETA2- GAMMA*P(4)
      P(1)  = P(1) + BETAX*TWO
      P(2)  = P(2) + BETAY*TWO
      P(3)  = P(3) + BETAZ*TWO
      P(4)  = GAMMA*(-ONE+P(4))
      RETURN
      END
      SUBROUTINE neutra
C------------------------------------------------------------------
C!  Compute neutralino masses from the MSSM parameters
C
C  Input:    /PARAM/ MSSM parameters
C
C  Output:   /PARAM/ amneut(4),   the neutralino masses.
C
C  P. Janot -- 4 December 1994
C------------------------------------------------------------------
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
C
      COMMON / elweak / sw2,alpha(0:nhig),gweak2(0:nhig),
     .                  alphas(0:nhig),g_f,deltar,alpha2,sw,cw2,cw
C
      COMMON / miscl / loutbe,ecm,idb1,idb2,xrad,empir,empirm,ism,
     &                 iklei,icar,sdvrt(3),vrtx(4),tabl(26),
     &                 nevent(11)
      INTEGER loutbe,idb1,idb2,nevent,ism,iklei,icar
      REAL*4 ecm,sdvrt,vrtx,tabl,empir,empirm,xrad
C
      DIMENSION y(4,4), wr(4), ir(4), zr(4,4), rr(4,4),
     .          work(4)
C
      susM1 = susM * 5./(8.*(1.-sw2))
      susM2 = susM * 3./(8.*sw2)
C
c The neutralino mass matrix Y(4,4)
C
      y(1,1) =  susM1
      y(1,2) =  0.
      y(1,3) = -amz*sw*cb
      y(1,4) =  amz*sw*sb
      y(2,1) =  y(1,2)
      y(2,2) =  susM2
      y(2,3) =  amz*cw*cb
      y(2,4) = -amz*cw*sb
      y(3,1) =  y(1,3)
      y(3,2) =  y(2,3)
      y(3,3) =  0.
      y(3,4) = -susMU
      y(4,1) =  y(1,4)
      y(4,2) =  y(2,4)
      y(4,3) =  y(3,4)
      y(4,4) =  0.
C
C  Diagonalization of the neutralino mass matrix;
C  ZR corresponds to the matrix N transpose; the eigenvalues
C  are contained in WR, and might be <0 or >0 depending of
C  the CP quantum number of the neutralino
C
      CALL eisrs1 (4,4,y,wr,zr,ierr,work)
      CALL ucopy(zr(1,1),rr(1,1),16)
      DO 20 i = 1, 4
        work(i) = ABS(wr(i))
   20 CONTINUE
C
C Sort the neutralinos in increasing mass order
C
      CALL sortzv(work,ir,4,1,0,0)
      DO 10 i = 1, 4
        k         = ir(i)
        amneut(i) = wr(k)
        DO 11 j = 1, 4
          zr(j,i) = rr(j,k)
   11   CONTINUE
   10 CONTINUE
      CALL ucopy(zr(1,1),fieldn(1,1),4*4)
C
C Matrices Q''(ij) and S''(ij)
C
      DO 1 i=1,4
        DO 2 j=1,4
          tw=sw/cw
          qqmat(i,j) = ( zr(3,i)*(zr(2,j)-tw*zr(1,j))
     .               +   zr(3,j)*(zr(2,i)-tw*zr(1,i)) )
          ssmat(i,j) = ( zr(4,i)*(zr(2,j)-tw*zr(1,j))
     .               +   zr(4,j)*(zr(2,i)-tw*zr(1,i)) )
    2   CONTINUE
    1 CONTINUE
C
      RETURN
      END
      SUBROUTINE PARCOU
C------------------------------------------------------------------
C!  Compute parameters, couplings
C
C  Input:    /PARAM/ SM and MSSM parameters
C
C  Output:   /PARAM/ Relevant couplings, masses, angles...
C
C   P. Janot -- 24 August 1991
C------------------------------------------------------------------
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      CHARACTER*14 channel
      CHARACTER*21 channeut, chanchar
      PARAMETER(nchneut=8,nchchar=5)
      COMMON / hhdeck / branch(nchan,nhig),width(nhig),
     .                  parwth(nhig),xymas(2,nchan,nhig),
     .                  xywid(2,nchan,nhig)
      COMMON / chaneu / ichn(2),
     .                  wneut(4,4,nhig), wchar(2,2,nhig),
     .                  widneut(4), brneut(nchneut,4),
     .                  widchar(2), brchar(nchchar,2)
      COMMON / chanch / channeut(nchneut,4), chanchar(nchchar,2)
      COMMON / vect4 / pvect4(5,2)
      COMMON / hinput / ichan(nchan,nhig), channel(nchan,nhig)
      COMMON / hisbr / bchpp,bchgg
      DIMENSION ph(4)
C
      PARAMETER (L1MST=200, L1PAR=200)
      PARAMETER (L2PAR=500, L2PARF=2000 )
      PARAMETER (LJNPAR=4000)
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),
     &                KFDP(L2PARF,5)
      COMMON /LUDAT4/ CHAF(L2PAR)
      CHARACTER*8 CHAF
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5)
C
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
C
      COMMON / elweak / sw2,alpha(0:nhig),gweak2(0:nhig),
     .                  alphas(0:nhig),g_f,deltar,alpha2,sw,cw2,cw
C
      COMMON / miscl / loutbe,ecm,idb1,idb2,xrad,empir,empirm,ism,
     &                 iklei,icar,sdvrt(3),vrtx(4),tabl(26),
     &                 nevent(11)
      INTEGER loutbe,idb1,idb2,nevent,ism,iklei,icar
      REAL*4 ecm,sdvrt,vrtx,tabl,empir,empirm,xrad
C
C
C Quark and lepton masses
C
      amb   = pmas(5,1)
      amc   = pmas(4,1)
      amtau = pmas(15,1)
      ame   = pmas(11,1)
      ammu  = pmas(13,1)
      amu   = .005
      amd   = .010
      ams   = .150
C
C Z mass and width, W width.
C
      amz    = pmas(23,1)
      gmz    = pmas(23,2)
      gmw    = pmas(24,2)
C
C QED coupling constant at the Z
C
      alpha(0)  = ulalem(amz**2)
      alpha2    = 0.38937966E12 * alpha(0)**2
C
C QCD coupling constant at the Z
C
      fnz       = 5.
      alphas(0) = pi*runalf(amz,fnz)
C
C Sin**2 theta_E effective fine tuned from G_F, alpha_QED, mZ
C
      sw2    = 0.5*(1.-SQRT(1.-4.*pi*alpha(0)/(SQRT(2.)*G_F*amz**2)))
      sw     = SQRT(sw2)
      cw2    = 1.-sw2
      cw     = SQRT(cw2)
C
C Miscallaneous
C
      gweak2(0) = 4. * pi * alpha(0) / sw2
      deltar    = 3.*G_F*amt**2/(8*pi**2*SQRT(2.))
      paru(102) = sw2
C
C W mass from mZ, weinberg angle and delta(rho)
C
      amw    = amz*cw/SQRT(1.-deltar)
C
C Running Delta rho to compute cross sections
C
      alpha_3 = runalf(amt,5.) * pi
      factork = 16.11
     .       - 1.04*(4.-amu/amt-amd/amt-ams/amt-amc/amt-amb/amt)
      amtcor = (1. + 4./3. *alpha_3/pi + factork*(alpha_3/pi)**2 )
      deltar = deltar / amtcor**2
C
C Relevant print out
C
      IF ( idbg .GE. 0 ) THEN
        WRITE(6,1003) amz,gmz,amt,g_f,xlamda5
        WRITE(6,1004) 1./alpha(0),sw2,deltar,amw,alphas(0)
      ENDIF
C
      IF ( ism .EQ. 0 ) THEN
C
C Now ... The MSSM !
C
        beta = ATAN(tb)
        cb = COS(beta)
        sb = SIN(beta)
        amsq = susSMQ
C
C Compute the S-top and S-bottom masses. Other squark splitting is
C assumed to be negligible.
C
        CALL squarks
        IF ( amst(1) .LE. 0. .OR. amsb(1) .LE. 0. ) THEN
          IF ( idbg .GE. 0 )
     .    WRITE(6,12001) amst(1),amst(2),amsb(1),amsb(2)
          RETURN
        ENDIF
C
C Compute the neutralino masses
C
        CALL neutra
C
C Compute the chargino masses
C
        CALL chargi
C
C Compute the neutral and charged Higgs masses, together with the
C mixing angle alpha in the CP-even sector
C
        CALL shiggs
        IF ( amh .LT. 0. ) THEN
          IF ( idbg .GE. 0 ) WRITE(6,11001) amh,gmh,ama,amhp
          RETURN
        ENDIF
C
C Compute the chargino and neutralino couplings to the Higgs bosons
C
        CALL chaneucp
C
C Compute the decay branching ratios of the neutralinos in chi Z*
C in chi gamma and in chi+/- W*+/-
C
        CALL chidec
C
      ELSE IF( ism.EQ.-1 ) THEN
C
C Two Higgs doublet Type II model
C
        beta = ATAN( tb )
        ta   = TAN( alfa )
        cb   = COS(beta)
        sb   = SIN(beta)
        ca   = COS(alfa)
        sa   = SIN(alfa)
        c2b  = (1.-tb**2)/(1.+tb**2)
        s2b  = 2.*tb/(1.+tb**2)
        sab2 = SIN(beta-alfa)**2
        cab2 = COS(beta-alfa)**2
        cab  = COS(alfa+beta)
        sab  = SIN(alfa+beta)
        c2a  = COS(2.*alfa)
C
      ELSE
C
C The Standard Model !
C
        ama  = 0.
        gmh  = 0.
        beta =  pi/4.
        alfa = -pi/4.
        tb   = 1.
        ta   =-1.
        c2b  = 0.
        s2b  = 1.
        sab2 = 1.
        cab2 = 0.
        cb = COS(beta)
        sb = SIN(beta)
        ca = COS(alfa)
        sa = SIN(alfa)
        cab = 1.
        sab = 0.
        c2a = 0.
C
      ENDIF
C
      amhig(1) = gmh
      amhig(2) = amh
      amhig(3) = ama
      pmas(24,1) = amw
      pmas(25,1) = amh
      pmas(35,1) = gmh
      pmas(36,1) = ama
      chaf(25) = 'h       '
      chaf(35) = 'H       '
      chaf(36) = 'A       '
C     mdme(337,1) = 0
      mdme(174,1) = 0
      mdme(153,1) = 0
C
C The QED/QCD running coupling constants at mh, mH, mA
C
        DO jhig = 1, nhig
          fnh = 3.
          xmh          = amhig(jhig)
          IF ( xmh .LE. 1. ) xmh = 1.
          IF ( amc .LE. xmh/2. ) fnh = fnh + 1.
          IF ( amb .LE. xmh/2. ) fnh = fnh + 1.
          IF ( amt .LE. xmh/2. ) fnh = fnh + 1.
C
          alphas(jhig) = pi*runalf(xmh,fnh)
          alpha (jhig) = ulalem(xmh**2)
          gweak2(jhig) = 4. * pi * alpha(jhig) / sw2
C
        ENDDO
C
C Print out...
C
      IF ( idbg .GE. 0 ) THEN
        IF ( ism .EQ. 0 ) THEN
          WRITE(6,1000) amarun,tb,susM,susMU,susAt,susAB,susSMQ,
     .                              susSMU,susSMD,susSML,susSME
          WRITE(6,1001) amh,gmh,ama,amhp
          WRITE(6,2001) amst(1),amst(2),topmix,
     .                  amsb(1),amsb(2),botmix
          WRITE(6,2003) amneut
          WRITE(6,2004) amchar
C
          IF ( amchar(1) .LT. ABS(amneut(1)) ) WRITE(6,2008)
C
          DO ineut = 1, 4
            WRITE(6,2005) ineut,amneut(ineut)
            DO ic = 1, nchneut
              IF ( brneut(ic,ineut) .GT. 0. )
     .        WRITE(6,2007) channeut(ic,ineut),100.*brneut(ic,ineut)
            ENDDO
          ENDDO
C
          DO ichar = 1, 2
            WRITE(6,2006) ichar,amchar(ichar)
            DO ic = 1, nchchar
              IF ( brchar(ic,ichar) .GT. 0. )
     .        WRITE(6,2007) chanchar(ic,ichar),100.*brchar(ic,ichar)
            ENDDO
          ENDDO
C
          sasb = sa/sb
          cacb = ca/cb
          casb = ca/sb
          sacb = sa/cb
          WRITE(6,2002) sin(beta-alfa),cos(beta-alfa),
     .                  sasb,cacb,casb,sacb,1./tb,tb
        ELSE
        ENDIF
      ENDIF
C
C Fill internal mass table
C
      CALL vzero(xymas(1,1,1),2*nchan*nhig)
      DO jhig = 1 , nhig
        xymas(1,1,jhig) = 0.
        xymas(1,2,jhig) = 0.
        xymas(1,3,jhig) = amtau
        xymas(1,4,jhig) = amc
        xymas(1,5,jhig) = amb
        xymas(1,6,jhig) = amt
        xymas(1,7,jhig) = amw
        xymas(1,8,jhig) = amz
        xymas(1,9,jhig) = ama
        xymas(1,10,jhig) = amh
        xymas(1,11,jhig) = 0.
        xymas(1,12,jhig) = ame
        xymas(1,13,jhig) = ammu
        xymas(1,14,jhig) = ams
        xymas(1,15,jhig) = 0.
        xymas(1,16,jhig) = 0.
        DO jchan = 1 , nchan
          xymas(2,jchan,jhig) = xymas(1,jchan,jhig)
        ENDDO
        xymas(2,11,jhig) = amz
      ENDDO
      xymas(1,9,3) = amh
      xymas(2,9,3) = amz
C------------------------------------------------------------------
 1000 FORMAT(1x,50('-')//
     .       1x,'With the following input parameters of the MSSM :'/
     .       1x,'   . A mass[Run] : ',F10.3,' GeV/c**2'/
     .       1x,'   . Tan beta    : ',F10.3/
     .       1x,'   . M           : ',F10.3,' GeV/c**2'/
     .       1x,'   . mu          : ',F10.3,' GeV/c**2'/
     .       1x,'   . At          : ',F10.3/
     .       1x,'   . Ab          : ',F10.3/
     .       1x,'   . mQ          : ',F10.3,' GeV/c**2'/
     .       1x,'   . mU          : ',F10.3,' GeV/c**2'/
     .       1x,'   . mD          : ',F10.3,' GeV/c**2'/
     .       1x,'   . mL          : ',F10.3,' GeV/c**2'/
     .       1x,'   . mE          : ',F10.3,' GeV/c**2'/)
 1001 FORMAT(1x,'we found the following Higgs masses :'/
     .       1x,'   . h mass      : ',F10.3,' GeV/c**2'/
     .       1x,'   . H mass      : ',F10.3,' GeV/c**2'/
     .       1x,'   . A mass[Pole]: ',F10.3,' GeV/c**2'/
     .       1x,'   . H+/- mass   : ',F10.3,' GeV/c**2'/)
 1002 FORMAT(1x,'and sin**2(beta-alfa) = ',F8.6//
     .       1x,50('-')//)
 1003 FORMAT(1x,50('-')//
     .       1x,'With the following input parameters :'/
     .       1x,'   . Z mass      : ',F10.4,' GeV/c**2'/
     .       1x,'   . Z width     : ',F10.4,' GeV'/
     .       1x,'   . top mass    : ',F10.4,' GeV/c**2'/
     .       1x,'   . G_F         : ',E10.4,' GeV**-2'/
     .       1x,'   . Lamba_QCD(5): ',F10.4,' GeV'/)
 1004 FORMAT(1x,'we found the following values :'/
     .       1x,'   . alpha(mZ)   : 1/',F8.3/
     .       1x,'   . sin**2(Th_W): ',F10.4/
     .       1x,'   . delta(rho)  : ',E10.4/
     .       1x,'   . W mass      : ',F10.4,' GeV/c**2'/
     .       1x,'   . alphas(mZ)  : ',F10.4,/)
 2001 FORMAT(1x,' and ... the following squark masses :'/
     .       1x,'   . t~1 mass    : ',F10.3,' GeV/c**2'/
     .       1x,'   . t~2 mass    : ',F10.3,' GeV/c**2'/
     .       1x,'   . Mixing      : ',F10.4,' rd.'/
     .       1x,'   . b~1 mass    : ',F10.3,' GeV/c**2'/
     .       1x,'   . b~2 mass    : ',F10.3,' GeV/c**2'/
     .       1x,'   . Mixing      : ',F10.4,' rd.'/)
 2003 FORMAT(1x,' and ... the following neutralino masses :'/
     .       1x,'   . chi(1) mass : ',F10.3,' GeV/c**2'/
     .       1x,'   . chi(2) mass : ',F10.3,' GeV/c**2'/
     .       1x,'   . chi(3) mass : ',F10.3,' GeV/c**2'/
     .       1x,'   . chi(4) mass : ',F10.3,' GeV/c**2'/)
 2004 FORMAT(1x,' and ... the following chargino masses :'/
     .       1x,'   . chi1+- mass : ',F10.3,' GeV/c**2'/
     .       1x,'   . chi2+- mass : ',F10.3,' GeV/c**2'/)
 2005 FORMAT(/1x,'Branching ratios for the neutralino ',
     .           I1, ' (mass ',F8.3, ' GeV/c**2)')
 2006 FORMAT(/1x,'Branching ratios for the chargino ',
     .           I1, ' (mass ',F8.3, ' GeV/c**2)')
 2007 FORMAT(10x,'o ',A21,1x,20('.'),1x,F9.5,'%')
 2008 FORMAT(//'                  +++ WARNING +++'/
     .       '      The ligthest neutralino is NOT the LSP ! '/
     .       '      The program  will stop  when  trying to '/
     .       '           decay the lightest chargino'/
     .       '                  +++ WARNING +++'//)
 2002 FORMAT(/1x,' and ... the following couplings :'/
     .       1x,'   . hWW or hZZ  : ',F8.3,/
     .       1x,'   . HWW or HZZ  : ',F8.3,/
     .       1x,'   . Htt         : ',F8.3,/
     .       1x,'   . Hbb         : ',F8.3,/
     .       1x,'   . htt         : ',F8.3,/
     .       1x,'   . hbb         : ',F8.3,/
     .       1x,'   . Att         : ',F8.3,/
     .       1x,'   . Abb         : ',F8.3,/)
11001 FORMAT(1x,' +++ Execution will stop due to ',
     .                'negative Higgs mass(es) +++'/
     .       1x,'   . h mass      : ',F12.3,' GeV/c**2'/
     .       1x,'   . H mass      : ',F12.3,' GeV/c**2'/
     .       1x,'   . A mass[Pole]: ',F12.3,' GeV/c**2'/
     .       1x,'   . H+/- mass   : ',F12.3,' GeV/c**2'/)
12001 FORMAT(1x,' +++ Execution will stop due to ',
     .                'negative squark mass(es) +++'/
     .       1x,'   . t~1 mass    : ',F12.3,' GeV/c**2'/
     .       1x,'   . t~2 mass    : ',F12.3,' GeV/c**2'/
     .       1x,'   . b~1 mass    : ',F12.3,' GeV/c**2'/
     .       1x,'   . b~2 mass    : ',F12.3,' GeV/c**2'/)
  999 RETURN
      END
      FUNCTION phspgz(a,b,c)
C ------------------------------------------------------------------
C! Phase space factor for  h --> gamma Z
C
C  Inputs:        a   is the Higgs mass squared
C                 b   is the Z mass
C                 c,  is the Z width
C
C  Output:        phspgz, the hZ cross section with width effects
C
C  Patrick Janot -- 01 Sep 1995
C -------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4 a,b,c
      COMMON /BHV/ s,am1,w1
      EXTERNAL fsubgz
      DIMENSION x(1)
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
C
      s      = a
      am1    = b
      w1     = c
      xlo    = -DATAN2(am1,w1)
      xhi    =  DATAN2(s-am1**2,am1*w1)
      phspgz = DGMLT1(fsubgz,xlo,xhi,1,6,x)
     .       / (piby2+DATAN2(am1,w1))
      RETURN
      END
      SUBROUTINE pick4(ph,jchan,jhig)
C------------------------------------------------------------------
C!  Derive the quadrimomenta of the Higgs decay particles
C
C  P. Janot --  26 Aug 1991
C------------------------------------------------------------------
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      CHARACTER*14 channel
      CHARACTER*21 channeut, chanchar
      PARAMETER(nchneut=8,nchchar=5)
      COMMON / hhdeck / branch(nchan,nhig),width(nhig),
     .                  parwth(nhig),xymas(2,nchan,nhig),
     .                  xywid(2,nchan,nhig)
      COMMON / chaneu / ichn(2),
     .                  wneut(4,4,nhig), wchar(2,2,nhig),
     .                  widneut(4), brneut(nchneut,4),
     .                  widchar(2), brchar(nchchar,2)
      COMMON / chanch / channeut(nchneut,4), chanchar(nchchar,2)
      COMMON / vect4 / pvect4(5,2)
      COMMON / hinput / ichan(nchan,nhig), channel(nchan,nhig)
      COMMON / hisbr / bchpp,bchgg
      DIMENSION ph(4)
C
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
C
      DIMENSION ptot(4),pp(4),qq(4),cormas(2)
      REAL*8 betax, betay, betaz, p4
      CALL vzero(pvect4(1,1),10)
C
      amloc = ph(4)
C
C Generate mass according to the width
C
      DO idc = 1 , 2
        IF ( idbg .GE. 10 ) WRITE(6,*) idc
     .      ,xymas(idc,jchan,jhig)
     .      ,xywid(idc,jchan,jhig)
        IF ( xywid(idc,jchan,jhig) .GT. 0.05 ) THEN
          x = xymas(idc,jchan,jhig)
          g = xywid(idc,jchan,jhig)
          IF ( idc .EQ. 1 ) THEN
            xmin = 2.*ame
            xmax = amloc-xmin
          ELSE
            xmin = 2.*ame
            xmax = amloc-cormas(1)
          ENDIF
          CALL bwgene(xmin,xmax,x,g,cormas(idc),djdummy)
        ELSE
          cormas(idc) = xymas(idc,jchan,jhig)
        ENDIF
      ENDDO
C
C  Special treatment for WW, ZZ, Z gamma
C
      IF ( jchan .EQ. 7 .OR. jchan .EQ. 8 ) THEN
        xma2 = xmax**2
        xh2  = amloc**2
        xm12 = cormas(1)**2
    1   xm22 = cormas(2)**2
        weight = SQRT((xh2-xm12-xm22)**2-4.*xm12*xm22)
     .         * (xm12*xm22 + ((xh2-xm12-xm22)**2-4.*xm12*xm22)/12.)
        wmax   = (xh2-xm12) * (xm12*xma2 + (xh2-xm12)**2/12.)
        IF ( weight/wmax .LT. RNDM(weight) ) THEN
          CALL bwgene(xmin,xmax,x,g,cormas(2),djdummy)
          GOTO 1
        ENDIF
C
        IF ( RNDM(xm1) .GT. 0.5 ) THEN
          xm1 = cormas(1)
          cormas(1) = cormas(2)
          cormas(2) = xm1
        ENDIF
C
      ENDIF
C
C  Special treatment for Z gamma
C
      IF ( jchan .EQ. 11 ) THEN
    2   weight = (1.-cormas(2)**2/amloc**2)**3
        IF ( weight .LT. RNDM(weight) ) THEN
          CALL bwgene(xmin,xmax,x,g,cormas(2),djdummy)
          GOTO 2
        ENDIF
      ENDIF
C
C Compute quadri-momenta
C
      pvect4(4,1) = (amloc**2+cormas(1)**2-cormas(2)**2)
     .            / (amloc*2.)
      pvect4(4,2) = (amloc**2+cormas(2)**2-cormas(1)**2)
     .            / (amloc*2.)
      pmom2       = (amloc**2-(cormas(1)+cormas(2))**2)
     .            * (amloc**2-(cormas(1)-cormas(2))**2)
      pmom = SQRT(pmom2)/(2.*amloc)
      c = 2.*rndm(c) - 1.
      p = 2.*pi*rndm(p)
      s = SQRT(1.-c**2)
      pvect4(3,1) = pmom * c
      pvect4(2,1) = pmom * s * SIN(p)
      pvect4(1,1) = pmom * s * COS(p)
      pvect4(3,2) =-pvect4(3,1)
      pvect4(2,2) =-pvect4(2,1)
      pvect4(1,2) =-pvect4(1,1)
C
C  Boost back to the lab
C
      CALL ucopy(pvect4(1,1),pp(1),4)
      CALL ucopy(pvect4(1,2),qq(1),4)
      betax = -ph(1)
      betay = -ph(2)
      betaz = -ph(3)
      p4 = SQRT(amloc**2+betax**2+betay**2+betaz**2)
      betax = betax/p4
      betay = betay/p4
      betaz = betaz/p4
      CALL lorenz(betax,betay,betaz,pp)
      CALL lorenz(betax,betay,betaz,qq)
      CALL ucopy(pp(1),pvect4(1,1),4)
      CALL ucopy(qq(1),pvect4(1,2),4)
      pvect4(5,1) = cormas(1)
      pvect4(5,2) = cormas(2)
C
      amas1 = (pvect4(4,1)**2
     .      -  pvect4(1,1)**2
     .      -  pvect4(2,1)**2
     .      -  pvect4(3,1)**2)
      amas2 = (pvect4(4,2)**2
     .      -  pvect4(1,2)**2
     .      -  pvect4(2,2)**2
     .      -  pvect4(3,2)**2)
      pxtot = pvect4(1,1) + pvect4(1,2)
      pytot = pvect4(2,1) + pvect4(2,2)
      pztot = pvect4(3,1) + pvect4(3,2)
      entot = pvect4(4,1) + pvect4(4,2)
      amtot = entot**2-pxtot**2-pytot**2-pztot**2
C
      IF ( idbg .GE. 10 ) THEN
        WRITE(6,1000) (pvect4(i,1),i=1,4),SQRT(amas1)
        WRITE(6,1000) (pvect4(i,2),i=1,4),SQRT(amas2)
        WRITE(6,1000) pxtot,pytot,pztot,entot,SQRT(amtot)
      ENDIF
C
  999 RETURN
C------------------------------------------------------------------
1000  FORMAT(1X,5(2X,F9.3))
      END
      SUBROUTINE pole(ihiggs,mchi,ma,tanb,mq,mur,mdr,mtop,at,ab,mu,
     *                mh,mhp,hm,hmp,amp,mhch,sa,ca,
     *                stop1,stop2,sbot1,sbot2,tanbA)
C------------------------------------------------------------------
C! Computes the Higgs pole masses and mixing angles.
C
C    Inputs: ihiggs(explained below),mchi,ma,tanb,mq,mur,mdr,mtop,
C    at,ab,mu
C
C    where mchi is the largest chargino mass, ma is the running
C    CP-odd Higgs mass, tanb is the value of the ratio of vacuum
C    expectaion values at the scale mtop,mq is the third generation
C    left handed squark mass parameter, mur is the third generation
C    right handed stop mass parameter, mdr is the third generation
C    right handed sbottom mass parameter, mtop is the pole top quark
C    mass; at,ab are the soft supersymmetry breaking trilinear
C    couplings of the stop and sbottoms, respectively, and mu is the
C    supersymmetric mass parameter.
C
C
C    Output: mh and mhp which are the lightest CP-even Higgs running
C    and pole masses, respectively; hm and hmp are the heaviest CP-even
C    Higgs running and pole masses, repectively; sa and ca are the
C    sin(alpha) and cos(alpha) where alpha is the Higgs mixing angle.
C    amp is the CP-odd Higgs pole mass. stop1,stop2,sbot1 and sbot2
C    are the stop and sbottom mass eigenvalues. Finally, tanbA is
C    the value of tanb at the CP-odd Higgs mass scale.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCcC
ccccccccccccccccccccccccccccccccccccccccccc
ccccc  The parameter ihiggs=0,1,2,3 corresponds to the
ccccc  number of Higgses whose pole mass is computed
ccccc   by the subroutine vac(...). If ihiggs=0 only running
ccccc   masses are given, what makes the running of the program
ccccc   much faster and it is quite generally a good approximation
ccccc   (for a theoretical discussion see Ref. below).
ccccc    If ihiggs=1, only the pole
ccccc   mass for h is computed. If ihiggs=2, then h and H, and
ccccc   if ihiggs=3, then h,H,A polarizations are computed
ccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c       Program based on the work by M. Carena, M. Quiros
c       and C.E.M. Wagner, "Effective potential methods and
c       the Higgs mass spectrum in the MSSM", CERN-TH/95-157.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
      IMPLICIT REAL*8(A-H,M,O-Z)
      CALL vac(ihiggs,mchi,ma,tanb,mq,mur,mdr,mtop,at,ab,mu,
     *        mh,mhp,hm,hmp,amp,mhch,stop1,stop2,sbot1,sbot2,
     *        sa,ca,stop1w,stop2w,tanbA)
      sinb = tanb/(tanb**2+1.)**.5
      cosb = 1./(tanb**2+1.)**.5
      sinbma = sinb*ca - cosb*sa
      RETURN
      END
      FUNCTION prosca(p1,p2)
C-------------------------------------------------------------------
C! Compute the scalar product p1.p2
C-------------------------------------------------------------------
      DIMENSION p1(4), p2(4)
      prosca = p1(4)*p2(4) - p1(3)*p2(3) - p1(2)*p2(2) - p1(1)*p2(1)
      RETURN
      END
      SUBROUTINE radcor(xmq,jhig,fnq,r1,r2)
C-------------------------------------------------------------------
C! Compute the 1st order rad. cor. to H --> qqbar(g) width
C
C  Inputs:       --xmq,   the running quark mass
C                --jhig,  the Higgs type
C                --fnq,   the quark flavour index
C
C  Ouptuts:      --r1,    the alpha_s    correction
C                --r2,    the alpha_s**2 correction
C
C  P. Janot  --  22 nov 1989.
C------------------------------------------------------------------
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      REAL*8 betaq, abeta
      IF( jhig.LE.3 ) THEN
        xmh = amhig(jhig)
      ELSE IF( jhig.EQ.4 ) THEN
        xmh = amhp
      END IF
      IF ( xmh .LT. 2.*xmq ) THEN
C       betaq = DSQRT(1D0-4D0*xmq**2/xmh**2)
C       abeta = (1.+betaq**2)
C    .        * (4.*DDILOG( (1D0-betaq)/(1D0+betaq))
C    .        +  2.*DDILOG(-(1D0-betaq)/(1D0+betaq))
C    .        -  3.*DLOG(2D0/(1D0+betaq))*DLOG((1D0+betaq)/(1D0-betaq))
C    .        -  2.*DLOG(betaq)*DLOG((1D0+betaq)/(1D0-betaq)))
C    .        -  3.*betaq*DLOG(4D0/(1D0-betaq**2))
C    .        -  4.*betaq*DLOG(betaq)
C       IF ( jhig .LE. 2 ) THEN
C         r1 = abeta/betaq
C    .       + (3.+34.*betaq**2-13.*betaq**4)
C    .       / (16.*betaq**3)
C    .       * DLOG((1D0+betaq)/(1D0-betaq))
C    .       + 3. * (-1.+7.*betaq**2) / (8.*betaq**2)
C       ELSEIF (jhig .EQ. 3 ) THEN
C         r1 = abeta/betaq
C    .       + (19.+2.*betaq**2+3.*betaq**4)
C    .       / (16.*betaq)
C    .       * DLOG((1D0+betaq)/(1D0-betaq))
C    .       + 3. * (7.-betaq**2) / 8.
C       ELSE
C         r1 = 3.*ALOG(xmq/xmh)
C       ENDIF
C       r1 = r1 - 3.*ALOG(xmq/xmh)
C       r2 = 0.
        r1 = 0.
        r2 = 0.
      ELSE
        r1 = 17./3.-40.*(xmq/xmh)**2
        r2 = 35.9399-1.3586*fnq
      ENDIF
C
  999 RETURN
      END
      SUBROUTINE REMT1(EBEAM,CROSS,STHR,INDEX,sig1,xrad)
C-----------------------------------------------------------------------
C! The famous Kleiss initial state radiator, modified for two photons.
C
C It calculates some quantities, and performs the
C numerical integration over the photon spectrum.
C
C EBEAM=Beam energy (in gev)
C CROSS=Nonradiative cross section, to be defined
C       with one variable: cross(s),
C       where s is the invariant mass of the e+e- pair.
C STHR =The kinematical threshold, i.e. the lowest allowed
C       value of s for which the nonradiative process can
C       take place ( in gev**2 )
C XRAD =The fudge factor that increases the beta to simulate
C       the beamstrahlung
C
c 851015 A. Schwarz. have to call REMT1 always when mass
c        of Z0-decay products changes, since threshold changes
c        as well; dimension of X(1000) changed to X(1000,10)
C 921109 P. Janot. Change the effective distribution in 1/x
C        integrated between a fictive xmin and 1. to the expo-
C        nentiated distribution in 1/x**(1-beta) integrated
C        between 0. and 1. as it should be.
C 921110 P. Janot. Generation of 2 ISR photons.
C 921111 P. Janot. Implementation of effective beamstrahlung option
C-----------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / flags  / idbg
      DIMENSION X(1000,10),F(1000),A(1000),Y(1000),Z(1000),XNEW(1000)
      DIMENSION xn(10),wmax(10)
      REAL*4 QK(4),QK1(4),QK2(4),QIN(4),QOUT(4),QKA(4),QKB(4)
      REAL*4 ebeam, cross, sthr, sig1, xrad, sig0, delt
      REAL*4 rndm
      EXTERNAL cross
      DATA INIT/0/
C
      SAVE
C
C DEFINITION OF BREMSSTRAHLUNG SPECTRUM
      DATA ame/.511D-3/
C
C Definition of Bremsstrahlung spectrum
C
      xkmax(sb)  = 1.-sthr/sb
      xps(sb)    = 2.*ame**2/sb
      xpt(sb)    = (2.+xps(sb))/xps(sb)
      beta(sb)   = 2.*alf/pi*(1.-2.*xps(sb))*(xpl-1.)*xrad
      spectr(xk) = bet * dj1
     .                 * (1.+3.*bet/4.+alf/pi*(pi**2/3.-1./2.)
     .                 - xk**(1.-bet) + xk**(2.-bet)/2.
     .                 - bet**2/24.*(2.*pi**2-37./4.+xpl/3.)
     .                 + bet/8.*strange(xk,bet) )
     .                 * cross(SNGL(s*(1.-xk)))
C        Store EBEAM into local variable EBEA for later use
      EBEA = EBEAM
C
C Initialize a few quantities and constants
C
      s   = 4.*ebeam**2
      sb  = s
      pi  = 4.*DATAN(1.d0)
      tpi = 2.*pi
C     alf = ulalem(SNGL(s))
      alf = 1./137.035989
      xpl  = DLOG(xpt(s))
      x1   = 0.
      xn(index) = xkmax(s)
      wmax(index) = 0.
      bet  = beta(s)
      dj1 = xn(index)**bet/bet
      sig1 = 0.
C
C Parameters of numerical integration step
      N    = 500
      ITER = 6
C
C Initialize by choosing equidistant u values ( du = Cx**(beta-1)dx)
C with an increased sampling around mz.
C
      IF ( idbg .GE. 0 ) THEN
        WRITE(6,*) ' '
        WRITE(6,*) '----------------------------------------'
        WRITE(6,*) '    Compute radiative corrections      '
        WRITE(6,*) '        for process # ',index
        WRITE(6,*) '----------------------------------------'
      ENDIF
      IT=0
      M=N-1
      DU=1./FLOAT(M)
      X(1,INDEX)=0.
      DO 101 I=2,N
  101 X(I,INDEX)=X(I-1,INDEX)+DU
C
C Starting point for iterations
C
  100 CONTINUE
C
C Calculate function values
C
      DO 102 I=1,N
  102 F(I)=SPECTR(xn(index)*X(I,INDEX)**(1./bet))
C
C CALCULATE BIN AREAS
      DO 103 I=1,M
  103 A(I)=(X(I+1,INDEX)-X(I,INDEX))*(F(I+1)+F(I))/2.
C
C CALCULATE CUMULATIVE SPECTRUM Y VALUES
      Y(1)=0.D0
      DO 104 I=2,N
  104 Y(I)=Y(I-1)+A(I-1)
C
C PUT EQUIDISTANT POINTS ON Y SCALE
      DZ=Y(N)/FLOAT(M)
      Z(1)=0.D0
      DO 105 I=2,N
  105 Z(I)=Z(I-1)+DZ
C
C DETERMINE SPACING OF Z POINTS IN BETWEEN Y POINTS
C FROM THIS, DETERMINE NEW X VALUES AND FINALLY REPLACE OLD VALUES
      XNEW(1)=X(1,INDEX)
      XNEW(N)=X(N,INDEX)
      K=1
      DO 108 I=2,M
  106 IF( Y(K+1) .GT. Z(I) ) GOTO 107
      K=K+1
      GOTO 106
  107 R= ( Z(I) - Y(K) ) / ( Y(K+1) - Y(K) )
  108 XNEW(I) = X(K,INDEX) + ( X(K+1,INDEX)-X(K,INDEX) )*R
      DO 109 I=1,N
  109 X(I,INDEX)=XNEW(I)
C
C CHECK ON END OF ITERATIONS AND RETURN
      IT=IT+1
      IF ( it .LE. 3 ) THEN
        SIG1 = 0.
        NT = 1
      ELSE
        NT = IT-2
      ENDIF
      SIG1 = SIG1+Y(M)
C     PRINT 3,IT,SIG1/FLOAT(NT)
    3 FORMAT(' Iteration # ',i3,'  Integral =',e15.6)
      IF(IT.LT.ITER) GOTO 100
C
C PRESENT RESULTS IN FORM OF CORRECTION
      SIG0 = CROSS(SNGL(S))
      SIG1 = SIG1/FLOAT(NT)
      DELT = (SIG1/SIG0-1.)*100.
C     IF(INIT.GT.1) RETURN
C     INIT = 2
      IF ( idbg .GE. 0 ) PRINT 4,SIG0,SIG1,DELT
    4 FORMAT(/' Results of the initialization step :',/,
     .        ' Nonradiative cross section :',e15.6,/,
     .        '    Radiative cross section :',e15.6,/,
     .        '    Radiative correction    :',f10.3,' %',/)
C     WRITE(6,500) (SQRT(s*(1.-xn*x(i,index)**(1./bet))),i=1,500)
C 500 FORMAT(10(F10.5,1X))
      RETURN
      ENTRY REMT2(QK1,QK2,IDEC)
C-----------------------------------------------------------------------
C THIS PART GENERATES A BREMSSTRAHLUNG PHOTON
C AND CALCULATES WHICH BEAM AXIS TO CHOOSE FOR
C THE GENERATION OF THE 'NONRADIATIVE' CROSS SECTION.
C THE PHOTON ENERGY SPECTRUM MUST HAVE BEEN EXAMINED
C BY CALLING ENTRY 'REMT1' BEFORE THE FIRST CALL TO
C THIS ENTRY.
C-----------------------------------------------------------------------
C
C INITIALIZE FLAG FOR REMT3
C     INDX = MINDEX(IDEC)
      INDX = IDEC
      IR=0
C
C GENERATE total PHOTON ENERGY FROM CUMULATIVE SPECTRUM BINS
  200 R=M*RNDM(1.)
      I=INT(R)
      S=R-I
      UK = X(I+1,INDX) + S*( X(I+2,INDX)-X(I+1,INDX) )
      XK = xn(INDX)*uk**(1./bet)
      IF ( xk .LE. 1D-17 ) xk = 1D-17
C
C Generate the energy of the photons
C
      IF ( xk .LE. 1D-6 ) THEN
        yx = xk**2/4.
      ELSE
        yx = (1.-SQRT(1.-xk))**2
      ENDIF
      yk = (RNDM(0.)*yx**bet)**(1./bet)
      IF ( yk/xk .GT. 1D-6 .OR. yk/xk**2 .GT. 1D-6 ) THEN
        xka  = .5*( (xk+yk)-SQRT((xk+yk)**2-4.*yk) )
      ELSE
        xka = yk/xk
      ENDIF
C 1D-15 rather than 1D-20 for these poor UNIX computers
      IF ( xka .LE. 1D-15 ) xka = 1D-15
      xkb  = xk+yk-yk/xk
      IF ( xkb .LE. 1D-15 ) xkb = 1D-15
C
      IF ( RNDM(0.2) .GT. 0.5 ) THEN
        xk1 = xka
        xk2 = xkb
      ELSE
        xk2 = xka
        xk1 = xkb
      ENDIF
C
C GENERATE AZIMUTHAL SCATTERING ANGLE OF THE two PHOTONs
      FG1 = TPI*RNDM(1.0)
      FG2 = TPI*RNDM(1.5)
C
C GENERATE COSINE OF POLAR SCATTERING ANGLE OF THE two PHOTONs
      sb1 = sb
  201 V1 = XPS(sb1) * ( XPT(sb1)**RNDM(2.0) - 1. )
      W1 = XPS(sb1) + V1*(1.-.5*V1)
      W1 = RNDM(3.0)/(1.-(XK1*XK1*W1+2.*XPS(Sb1)*(1.-XK1)/W1)
     .              /(1.+(1.-XK1)**2))
      IF(W1.GT.1.) GOTO 201
      W1 = -1. + 2.*W1
      CG1=SIGN(1.-V1,W1)
      sb2 = sb1*(1.-xk1)
  202 V2 = XPS(sb2) * ( XPT(sb2)**RNDM(2.5) - 1. )
      W2 = XPS(sb2) + V2*(1.-.5*V2)
      W2 = RNDM(3.5)/(1.-(XK2*XK2*W2+2.*XPS(Sb2)*(1.-XK2)/W2)
     .              /(1.+(1.-XK2)**2))
      IF(W2.GT.1.) GOTO 202
      W2 = -1. + 2.*W2
      CG2=SIGN(1.-V2,W2)
C
C CHOOSE WHICH OF THE TWO Z AXES SHOULD BE CONSIDERED

      CH1=-1.
      CH2=-1.
      IF(ABS(W1).LT.(1./(1.+(1.-2./(1.+XK1*CG1/(2.-XK1)))**2)))
     .CH1=+1.
      IF(ABS(W2).LT.(1./(1.+(1.-2./(1.+XK2*CG2/(2.-XK2)))**2)))
     .CH2=+1.
C
C CONSTRUCT PHOTON FOUR-MOMENTA
      SG1=SQRT(V1*(2.-V1))
      QK1(4)=XK1*SQRT(sb1)/2.
      QK1(1)=QK1(4)*SG1*COS(FG1)
      QK1(2)=QK1(4)*SG1*SIN(FG1)
      QK1(3)=QK1(4)*CG1
C
      SG2=SQRT(V2*(2.-V2))
      Qk2(4)=XK2*SQRT(sb2)/2.
      Qk2(1)=Qk2(4)*SG2*COS(FG2)
      Qk2(2)=Qk2(4)*SG2*SIN(FG2)
      Qk2(3)=Qk2(4)*CG2
C - Correction for UNIX !!!
      CALL ucopy(qk1(1),qka(1),4)
      CALL ucopy(qk2(1),qkb(1),4)
C
      RETURN
C
      ENTRY REMT3(QIN,QOUT,IPHOT)
C-----------------------------------------------------------------------
C THIS PART PERFORMS THE ROTATIONS AND BOOSTS OF THE I.S.R.
C FORMALISM AFTER THE USER'S BLACK BOX HAS RUN AN EVENT.
C THE INPUT VECTOR (FROM USERS BLACK BOX) IS QIN;
C THE RESULTING VECTOR IN THE LAB FRAME IS QOUT.
C-----------------------------------------------------------------------
C
C INITIALIZATION PART: ONCE FOR EVERY GENERATED PHOTON MOMENTUM
C     IF(IR.NE.0) GOTO 301
C     IR=1
      IF ( iphot .EQ. 1 ) THEN
        CALL ucopy(qka(1),qk(1),4)
        ebea = SQRT(sb1)/2.
        ch = ch1
      ELSE
        CALL ucopy(qkb(1),qk(1),4)
        ebea = SQRT(sb2)/2.
        ch = ch2
      ENDIF
C
C CALCULATE ROTATTION PARAMETERS FOR BEAM DIRECTION IN C.M.S.
      YK=QK(4)**2-QK(1)**2-QK(2)**2-QK(3)**2
      XKP = SQRT( QK(1)**2 + QK(2)**2 )
      XKM = 2.* SQRT( EBEA*(EBEA-QK(4)) + YK/4. )
      XKD = 2.*EBEA - QK(4) + XKM
      XKA = ( CH + QK(3)/XKD )/XKM
      XKB = SQRT( (1.+XKA*QK(3))**2 + (XKA*XKP)**2 )
      S1  = XKA*XKP/XKB
      C1  = (1.+XKA*QK(3))/XKB
      S2  = QK(1)/XKP
      C2  = QK(2)/XKP
      Y1=C1**2+S1**2-1.
      Y2=C2**2+S2**2-1.
C
C ROTATE INPUT VECTOR QIN(I) TO CORRESPOND WITH CHOZEN Z-AXIS
  301 QQ =  C1*QIN(2) + S1*QIN(3)
      QZ = -S1*QIN(2) + C1*QIN(3)
      QX =  C2*QIN(1) + S2*QQ
      QY = -S2*QIN(1) + C2*QQ
C
C BOOST ROTATED VECTOR TO LAB FRAME VECTOR QOUT
      QOUT(4)=((XKD-XKM)*QIN(4)-QK(1)*QX-QK(2)*QY-QK(3)*QZ)/XKM
      QQ     =(QIN(4)+QOUT(4))/XKD
      QOUT(1)= QX - QK(1)*QQ
      QOUT(2)= QY - QK(2)*QQ
      QOUT(3)= QZ - QK(3)*QQ
C
      RETURN
      END
      SUBROUTINE rghm(mchi,ma,tanb,mq,mur,md,mtop,au,ad,mu,
     *    mhp,hmp,mhch,sa,ca,tanbA)
C---------------------------------------------------------------------
C!  Computes the Higgs running masses and mixing angles
C
C   From M. Carena and C. Wagner, 21 Sep 1995
C---------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,L,M,O-Z)
      REAL*8 cma,ctb,cmq,cmur,cmdr,cmtop,cau,cad,cmu,cmchi
      REAL*8 cmh,cmhp,chm,chmp,camp,cmhch,csa,cca
      REAL*8 cstop1,cstop2,csbot1,csbot2,ctanbA
      REAL*8 rmtop,rmbot,mz,alpha_1,alpha_2,alpha_3,v,ppi,sint,stw
      COMMON / mcarena / rmtop,rmbot,mz,alpha_1,alpha_2,alpha_3,
     .                   v,ppi,sint,stw
C
      DIMENSION VH(2,2),M2(2,2),M2P(2,2)
C
Cpaj  mz = 91.18
Cpaj  alpha1 = 0.0101
Cpaj  alpha2 = 0.0337
Cpaj  alpha3Z = 0.12
Cpaj  v = 174.1
Cpaj  pi = 3.14159
      alpha1 = alpha_1     ! paj
      alpha2 = alpha_2     ! paj
      alpha3 = alpha_3     ! paj
      pi = ppi             ! paj
      tanbA = tanb
      tanbt = tanb
C
C     mbottom(mtop) = 3. GeV
Cpaj  mb = 3.
      mb = rmbot           ! paj
Cpaj  alpha3 = alpha3Z/(1. +(11. - 10./3.)/4./pi*alpha3Z*
Cpaj *log(mtop**2/mz**2))
C
C     rmtop= running top quark mass
Cpaj  rmtop = mtop/(1.+4.*alpha3/3./pi)
      tq = log((mq**2+mtop**2)/mtop**2)
      tu = log((mur**2 + mtop**2)/mtop**2)
      td = log((md**2 + mtop**2)/mtop**2)
      sinb = tanb/((1. + tanb**2)**.5)
      cosb = sinb/tanb
      if(ma.gt.mtop)
     *tanbA = tanb*(1.-3./32./pi**2*
     *(rmtop**2/v**2/sinb**2-mb**2/v**2/cosb**2)*
     *log(ma**2/mtop**2))
      if(ma.lt.mtop.or.ma.eq.mtop) tanbt = tanbA
      sinb = tanbt/((1. + tanbt**2)**.5)
      cosb = 1./((1. + tanbt**2)**.5)
      cos2b = (tanbt**2 - 1.)/(tanbt**2 + 1.)
      g1 = (alpha1*4.*pi)**.5
      g2 = (alpha2*4.*pi)**.5
      g3 = (alpha3*4.*pi)**.5
      hu = rmtop/v/sinb
      hd =  mb/v/cosb
C
      CALL Gfuncar(ma,tanbA,mq,mur,md,mtop,Au,Ad,mu,vh,stop1,stop2)
C
      if(mq.gt.mur) tp = tq - tu
      if(mq.lt.mur.or.mq.eq.mur) tp = tu - tq
      if(mq.gt.mur) tdp = tu
      if(mq.lt.mur.or.mq.eq.mur) tdp = tq
      if(mq.gt.md) tpd = tq - td
      if(mq.lt.md.or.mq.eq.md) tpd = td - tq
      if(mq.gt.md) tdpd = td
      if(mq.lt.md.or.mq.eq.md) tdpd = tq

      if(mq.gt.md) dlambda1 = 6./96./pi**2*g1**2*hd**2*tpd
      if(mq.lt.md.or.mq.eq.md) dlambda1 = 3./32./pi**2*
     * hd**2*(g1**2/3.+g2**2)*tpd

      if(mq.gt.mur) dlambda2 =12./96./pi**2*g1**2*hu**2*tp
      if(mq.lt.mur.or.mq.eq.mur) dlambda2 = 3./32./pi**2*
     * hu**2*(-g1**2/3.+g2**2)*tp

      dlambda3 = 0.
      dlambda4 = 0.

      if(mq.gt.md) dlambda3 = -1./32./pi**2*g1**2*hd**2*tpd
      if(mq.lt.md.or.mq.eq.md) dlambda3 = 3./64./pi**2*hd**2*
     *(g2**2-g1**2/3.)*tpd

      if(mq.gt.mur) dlambda3 = dlambda3 -
     *1./16./pi**2*g1**2*hu**2*tp
      if(mq.lt.mur.or.mq.eq.mur) dlambda3 = dlambda3 +
     * 3./64./pi**2*hu**2*(g2**2+g1**2/3.)*tp

      if(mq.lt.mur) dlambda4 = -3./32./pi**2*g2**2*hu**2*tp
      if(mq.lt.md) dlambda4 = dlambda4 - 3./32./pi**2*g2**2*
     *hd**2*tpd
C
      lambda1 = ((g1**2 + g2**2)/4.)*
     * (1.-3.*hd**2*(tpd + tdpd)/8./pi**2)
     *+(3.*hd**4./16./pi**2) *tpd*(1.
     *+ (3.*hd**2/2. + hu**2/2.
     *- 8.*g3**2) * (tpd + 2.*tdpd)/16./pi**2)
     *+(3.*hd**4./8./pi**2) *tdpd*(1.  + (3.*hd**2/2. + hu**2/2.
     *- 8.*g3**2) * tdpd/16./pi**2) + dlambda1
      lambda2 = ((g1**2 + g2**2)/4.)*(1.-3.*hu**2*
     *(tp + tdp)/8./pi**2)
     *+(3.*hu**4./16./pi**2) *tp*(1.
     *+ (3.*hu**2/2. + hd**2/2.
     *- 8.*g3**2) * (tp + 2.*tdp)/16./pi**2)
     *+(3.*hu**4./8./pi**2) *tdp*(1. + (3.*hu**2/2. + hd**2/2.
     *- 8.*g3**2) * tdp/16./pi**2) + dlambda2
      lambda3 = ((g2**2 - g1**2)/4.)*(1.-3.*
     *(hu**2)*(tp + tdp)/16./pi**2 -3.*
     *(hd**2)*(tpd + tdpd)/16./pi**2) +dlambda3
      lambda4 = (- g2**2/2.)*(1.
     *-3.*(hu**2)*(tp + tdp)/16./pi**2
     *-3.*(hd**2)*(tpd + tdpd)/16./pi**2) +dlambda4
      lambda5 = 0.
      lambda6 = 0.
      lambda7 = 0.

      m2(1,1) = 2.*v**2*(lambda1*cosb**2+2.*lambda6*
     *cosb*sinb + lambda5*sinb**2) + ma**2*sinb**2

      m2(2,2) = 2.*v**2*(lambda5*cosb**2+2.*lambda7*
     *cosb*sinb + lambda2*sinb**2) + ma**2*cosb**2
      m2(1,2) = 2.*v**2*(lambda6*cosb**2+(lambda3+lambda4)*
     *cosb*sinb + lambda7*sinb**2) - ma**2*sinb*cosb

      m2(2,1) = m2(1,2)
ccccccccccccccccccccccccccccccccccccccccccccccccc
ccc  this is the contribution from light charginos/neutralinos
ccccccccccccccccccccccccccccccccccccccccccccccccc

        mssusy=(.5*(mq**2+mur**2)+mtop**2)**.5

        if(mchi.gt.mssusy)goto 3790
        if(mchi.lt.mtop) mchi=mtop

        tchar=log(mssusy**2/mchi**2)

        deltal12=(9./64./pi**2*g2**4+5./192./pi**2*g1**4)*tchar
        deltal3p4=(3./64./pi**2*g2**4+7./192./pi**2*g1**4
     *       +4./32/pi**2*g1**2*g2**2)*tchar

        deltam112=2.*deltal12*v**2*cosb**2
        deltam222=2.*deltal12*v**2*sinb**2
        deltam122=2.*deltal3p4*v**2*sinb*cosb

        m2(1,1)=m2(1,1)+deltam112
        m2(2,2)=m2(2,2)+deltam222
        m2(1,2)=m2(1,2)+deltam122
        m2(2,1)=m2(2,1)+deltam122

 3790   continue

ccccccccccccccccccccccccccccccccccccccccccc
ccc  end of charginos/neutralinos
ccccccccccccccccccccccccccccccccccccccccccc

      do 9800 i = 1,2
      do 9801 j = 1,2
      m2p(i,j) = m2(i,j) + vh(i,j)
 9801 continue
 9800 continue

      Trm2p = m2p(1,1) + m2p(2,2)
      detm2p = m2p(1,1)*m2p(2,2) - m2p(1,2)*m2p(2,1)

      mh2p = (Trm2p - (Trm2p**2 - 4.* detm2p)**.5)/2.
      HM2p = (Trm2p + (Trm2p**2 - 4.* detm2p)**.5)/2.
      HMp = Hm2p**.5
      IF ( mh2p .GT. 0. ) THEN
        mhp = SQRT(mh2p)
      ELSE
        mhp = -SQRT(-mh2p)
      ENDIF
C
      sin2alpha = 2.*m2p(1,2)/(Trm2p**2-4.*detm2p)**.5
      cos2alpha = (m2p(1,1)-m2p(2,2))/(Trm2p**2-4.*detm2p)**.5
      if(cos2alpha.gt.0.) alpha = asin(sin2alpha)/2.
      if(cos2alpha.lt.0.) alpha = -pi/2.-asin(sin2alpha)/2.
      sa = sin(alpha)
      ca = cos(alpha)
      sqbma = (sinb*ca - cosb*sa)**2
C
C Bretelle for the charged Higgses
C
      MS = ((mq**2 + mur**2)/2. + mtop**2)**.5
      t = log(MS**2/mtop**2)
      aud = (-6.*mu**2/MS**2 - ( mu**2- Ad*Au)**2/MS**4
     *+ 3.*(Au + Ad)**2/MS**2)/6.
      carlos4 = (- g2**2/2.)*(1.-3.*(hu**2 + hd**2)*t/16./pi**2)
     *-(6.*hu**2*hd**2/16./pi**2) * (t + aud/2. + (hu**2 + hd**2
     *- 8.*g3**2) * (aud*t + t**2)/16./pi**2)
     *+(3.*hu**4/96./pi**2) * (3.*mu**2/MS**2 - mu**2*Au**2/
     *MS**4)*
     *(1+ (6.*hu**2 -2.* hd**2/2.
     *-  16.*g3**2) *t/16./pi**2)
     *+(3.*hd**4/96./pi**2) * (3.*mu**2/MS**2 - mu**2*Ad**2/
     *MS**4)*
     *(1+ (6.*hd**2 -2.* hu**2/2.
     *-  16.*g3**2) *t/16./pi**2)
      carlos5 = -(3.*hu**4* mu**2*Au**2/96./pi**2/MS**4) *
     * (1- (2.*hd**2 -6.* hu**2 + 16.*g3**2) *t/16./pi**2)
     *-(3.*hd**4* mu**2*Ad**2/96./pi**2/MS**4) *
     * (1- (2.*hu**2 -6.* hd**2 + 16.*g3**2) *t/16./pi**2)
      mhch2 = mA**2 + (carlos5 - carlos4)* v**2
      IF ( mhch2 .GE. 0. ) THEN
        mhch = SQRT(mhch2)
      ELSE
        mhch = -SQRT(-mhch2)
      ENDIF
C
 2242 RETURN
      END
      FUNCTION runalf(xmu,fn)
C-------------------------------------------------------------------
C! Running strong coupling constant
C
C  Inputs:       --xmu,   Scale mass
C                --fn,    flavour number
C
C  Ouptuts:      --runalf,  The value of the alphas(xmu)/pi
C
C  P. Janot  --  22 nov 1989.
C------------------------------------------------------------------
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      COMMON / alcoef / beta1,beta2,beta3,gama1,gama2,gama3
C
C Lambda_QCD at nf # 5 (from lambda_QCD(5), given in data cards)
C
      xlamda4 = xlamda5 * (amb/xlamda5)**(2./25.)
     .        * (ALOG(amb**2/xlamda5**2))**(963./14375.)
      xlamda3 = xlamda4 * (amc/xlamda4)**(2./27.)
     .        * (ALOG(amc**2/xlamda4**2))**(107./2025.)
      xlamda6 = xlamda5 * (xlamda5/amt)**(2./21.)
     .        * (ALOG(amt**2/xlamda5**2))**(-321./3381.)
C
C Lambda_QCD at fn
C
      nf = fn
      IF ( nf .LT. 3 ) xlamda = xlamda3
      IF ( nf .EQ. 3 ) xlamda = (fn-3.)*xlamda4+(4.-fn)*xlamda3
      IF ( nf .EQ. 4 ) xlamda = (fn-4.)*xlamda5+(5.-fn)*xlamda4
      IF ( nf .EQ. 5 ) xlamda = (fn-5.)*xlamda6+(6.-fn)*xlamda5
      IF ( nf .GE. 6 ) xlamda =        xlamda6
C
C  Coefficients
C
      beta1 = -11./2. + fn/3.
      beta2 = -51./4. + 19./12.*fn
      beta3 = (-2857. + 5033.*fn/9. - 325.*fn**2/27.) / 64.
      gama1 = 2.
      gama2 = 101./12. -5./18.*fn
      gama3 = (3747. - (160.*1.2020569+2216./9.)*fn -140.*fn**2/27.)
     .            / 96.
C
C  alpha_s / pi
C
      xmu2 = xmu
      IF ( xmu2 .LT. .5 ) xmu2 = .5
      as = 1./(-beta1*ALOG(xmu2/xlamda))
      aal = ALOG(2.*ALOG(xmu2/xlamda))
      runalf = as * (1. - as*beta2/beta1*aal
     .       + as**2 * ( (beta2/beta1)**2 * ( aal**2 -aal - 1. )
     .       + beta3/beta1))
C
      RETURN
      END
      FUNCTION RUNMAS(xmq,jhig,facrad)
C-------------------------------------------------------------------
C! Compute the running mass term in H --> qqbar(g) width
C
C  Inputs:       --xmq,   quark mass
C                --jhig,  Higgs type
C
C  Ouptuts:      --runmas,  The value of the running mass
C                --facrad,  The finite QCD corrections
C
C  P. Janot  --  22 nov 1989.
C------------------------------------------------------------------
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      COMMON / elweak / sw2,alpha(0:nhig),gweak2(0:nhig),
     .                  alphas(0:nhig),g_f,deltar,alpha2,sw,cw2,cw
C
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
C
      COMMON / alcoef / beta1,beta2,beta3,gama1,gama2,gama3
C
      IF( jhig.LE.3 ) THEN
        xmh    = amhig (jhig)
      ELSE IF( jhig.EQ.4 ) THEN
        xmh    = amhp
      END IF
C
C  Flavour number (assume mH >> mu, md, ms)
C
      fnh = 3.
      IF ( amc .LE. xmh/2. ) fnh = fnh + 1.
      IF ( amb .LE. xmh/2. ) fnh = fnh + 1.
      IF ( amt .LE. xmh/2. ) fnh = fnh + 1.
C
      alphah = runalf(xmh,fnh)
C
C Compute mq(mH)
C
      cmulth =  beta2/beta1*(gama1/beta1-gama2/beta2)
      cmulth2 = .5*( (beta2/beta1)**2*(gama1/beta1-gama2/beta2)**2
     .              -(beta2/beta1)**2*(gama1/beta1-gama2/beta2)
     .              +(beta3/beta1)   *(gama1/beta1-gama3/beta3) )
      xmhp = (-beta1*alphah)**(-gama1/beta1)
     .     * (1. + cmulth*alphah + cmulth2*alphah**2 )
C
C Compute mq(Mq)
C
      IF     ( xmq .EQ. ams ) THEN
        fnq = 3.
        factork = 16.11
     .          - 1.04*(2.-amu/ams-amd/ams)
      ELSEIF ( xmq .EQ. amc ) THEN
        fnq = 4.
        factork = 16.11
     .          - 1.04*(3.-amu/amc-amd/amc-ams/amc)
      ELSEIF ( xmq .EQ. amb ) THEN
        fnq = 5.
        factork = 16.11
     .          - 1.04*(4.-amu/amb-amd/amb-ams/amb-amc/amb)
      ELSEIF ( xmq .EQ. amt ) THEN
        fnq = 6.
        factork = 16.11
     .          - 1.04*(4.-amu/amt-amd/amt-ams/amt-amc/amt-amb/amt)
      ELSE
      ENDIF
      alphaq = runalf(xmq,fnq)
C     alphaq = runalf(2.*xmq,fnq)
      cmultq =  beta2/beta1*(gama1/beta1-gama2/beta2)
      cmultq2 = .5*( (beta2/beta1)**2*(gama1/beta1-gama2/beta2)**2
     .              -(beta2/beta1)**2*(gama1/beta1-gama2/beta2)
     .              +(beta3/beta1)   *(gama1/beta1-gama3/beta3) )
      xmqp = (-beta1*alphaq)**(-gama1/beta1)
     .     * (1. + cmultq*alphaq + cmultq2*alphaq**2 )
     .     * (1. + 4./3. *alphaq + factork*alphaq**2 )
C
      runmas = (xmhp/xmqp)**2
C
C Finite radiative corrections due to gluon radiations
C
      runrun = runmas
      CALL radcor(xmq*SQRT(runrun),jhig,fnq,r1,r2)
      facrad = 1. + r1 * alphah + r2 * alphah**2
C     facqed = 1. + r1 * alpha(jhig) + r2 * alpha(jhig)**2
C
C  Here we go
C
 999  RETURN
C-------------------------------------------------------------------
      END
      SUBROUTINE SETDEF
C-------------------------------------------------------------
C!  Set default conditions to HHDECAY
C
C   Input:   / LUNCOM /  --PMAS, mass array from LUND
C
C   Output:  / H0DECK /  --ichan = 1 for requested channels
C                        --channel = channel names
C                        --idbg        debug level
C
C   P. Janot  --  26 Aug 1991
C-------------------------------------------------------------
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      CHARACTER*14 channel
      CHARACTER*21 channeut, chanchar
      PARAMETER(nchneut=8,nchchar=5)
      COMMON / hhdeck / branch(nchan,nhig),width(nhig),
     .                  parwth(nhig),xymas(2,nchan,nhig),
     .                  xywid(2,nchan,nhig)
      COMMON / chaneu / ichn(2),
     .                  wneut(4,4,nhig), wchar(2,2,nhig),
     .                  widneut(4), brneut(nchneut,4),
     .                  widchar(2), brchar(nchchar,2)
      COMMON / chanch / channeut(nchneut,4), chanchar(nchchar,2)
      COMMON / vect4 / pvect4(5,2)
      COMMON / hinput / ichan(nchan,nhig), channel(nchan,nhig)
      COMMON / hisbr / bchpp,bchgg
      DIMENSION ph(4)
C
      INTEGER    HCCON, NSCH, NCH, NDCH, NMXC
      PARAMETER( NCH= 49, NDCH= 19, NMXC= 9 )
      REAL*8     HCCBR1, HCCBR2, TAUHP
      COMMON /HCDEFF/ NSCH(NDCH), HCCON(NDCH),
     .                HCCBR1(0:NDCH), HCCBR2(NDCH,NMXC), TAUHP
      CHARACTER*17 HCCHAN
      COMMON /HCNAME/ HCCHAN(NDCH)
      INTEGER    UPMAX
      REAL*8     MEMAX, THRES
      COMMON /HCMAXX/ MEMAX(NCH), THRES(NCH), UPMAX(NCH)
      INTEGER    HCDSTA
      COMMON /HCSTAT/ HCDSTA(0:NCH)
C-----------------------------------------------------------------------
C!    HCCHAN           channel names
C!    HCCON            requested decay ( 1= ON, 0= OFF )
C!    HCCBR1           cumulative BR on first channel classif. (data car
C!    HCCBR2           cumulative BR on subchannel classif. (internal)
C!    NSCH             number of subchannels per channel
C!    TAUHP            lifetime in seconds
C-----------------------------------------------------------------------
C
      idbg = 0
C
      DO jhig  = 1, nhig
        DO jchan = 1, nchan
          ichan(jchan,jhig) = 1
        ENDDO
      ENDDO
C
      channel( 1,1) = 'H --> gam gam '
      channel( 2,1) = 'H --> glu glu '
      channel( 3,1) = 'H --> tau+tau-'
      channel( 4,1) = 'H --> c c bar '
      channel( 5,1) = 'H --> b b bar '
      channel( 6,1) = 'H --> t t bar '
      channel( 7,1) = 'H --> W+ W-   '
      channel( 8,1) = 'H --> Z0 Z0   '
      channel( 9,1) = 'H --> A A     '
      channel(10,1) = 'H --> h h     '
      channel(11,1) = 'H --> Z0 gam  '
      channel(12,1) = 'H --> e+ e-   '
      channel(13,1) = 'H --> mu+ mu- '
      channel(14,1) = 'H --> s sbar  '
      channel(15,1) = 'H --> chi chi '
      channel(16,1) = 'H --> chi+chi-'
C
      channel( 1,2) = 'h --> gam gam '
      channel( 2,2) = 'h --> glu glu '
      channel( 3,2) = 'h --> tau+tau-'
      channel( 4,2) = 'h --> c c bar '
      channel( 5,2) = 'h --> b b bar '
      channel( 6,2) = 'h --> t t bar '
      channel( 7,2) = 'h --> W+ W-   '
      channel( 8,2) = 'h --> Z0 Z0   '
      channel( 9,2) = 'h --> A A     '
      channel(10,2) = '              '
      channel(11,2) = 'h --> Z0 gam  '
      channel(12,2) = 'h --> e+ e-   '
      channel(13,2) = 'h --> mu+ mu- '
      channel(14,2) = 'h --> s sbar  '
      channel(15,2) = 'h --> chi chi '
      channel(16,2) = 'h --> chi+chi-'
C
      channel( 1,3) = 'A --> gam gam '
      channel( 2,3) = 'A --> glu glu '
      channel( 3,3) = 'A --> tau+tau-'
      channel( 4,3) = 'A --> c c bar '
      channel( 5,3) = 'A --> b b bar '
      channel( 6,3) = 'A --> t t bar '
      channel( 7,3) = 'A --> W+ W-   '
      channel( 8,3) = 'A --> Z0 Z0   '
      channel( 9,3) = 'A --> Z0 h    '
      channel(10,3) = '              '
      channel(11,3) = 'A --> Z0 gam  '
      channel(12,3) = 'A --> e+ e-   '
      channel(13,3) = 'A --> mu+ mu- '
      channel(14,3) = 'A --> s sbar  '
      channel(15,3) = 'A --> chi chi '
      channel(16,3) = 'A --> chi+chi-'
C
C
      DO i=1,ndch
        hccon(i)= 1
      ENDDO
C
      HCCHAN( 1)= 'H+ --> nu e+     '
      HCCHAN( 2)= 'H+ --> nu mu+    '
      HCCHAN( 3)= 'H+ --> nu tau+   '
      HCCHAN( 4)= 'H+ --> u d bar   '
      HCCHAN( 5)= 'H+ --> u s bar   '
      HCCHAN( 6)= 'H+ --> u b bar   '
      HCCHAN( 7)= 'H+ --> c d bar   '
      HCCHAN( 8)= 'H+ --> c s bar   '
      HCCHAN( 9)= 'H+ --> c b bar   '
      HCCHAN(10)= 'H+ --> W+ b d bar'
      HCCHAN(11)= 'H+ --> W+ b s bar'
      HCCHAN(12)= 'H+ --> W+ b b bar'
      HCCHAN(13)= 'H+ --> h nu l+   '
      HCCHAN(14)= 'H+ --> h q q1 bar'
      HCCHAN(15)= 'H+ --> A nu l+   '
      HCCHAN(16)= 'H+ --> A q q1 bar'
      HCCHAN(17)= 'H+ --> H nu l+   '
      HCCHAN(18)= 'H+ --> H q q1 bar'
      HCCHAN(19)= 'H+ --> chi0 chi+ '
C
C - Number of subchannels
C
      DO I=1,12
        NSCH(I)= 1
      END DO
      DO I=13,17,2
        NSCH(I)  = 3
        NSCH(I+1)= 9
      END DO
      NSCH(19)= 1
C
  999 RETURN
C-----------------------------------------------------------------------
      END
      SUBROUTINE shiggs
C------------------------------------------------------------------
C!  Compute SUSY Higgs masses from the MSSM parameters
C
C  Input:    /PARAM/ MSSM parameters
C
C  Output:   /PARAM/ amh, gmh, amhp, the squared Higgs masses
C                    alfa, the mixing angle in the CP-even sector
C
C  P. Janot -- 3 December 1994
C------------------------------------------------------------------
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
C
      COMMON / elweak / sw2,alpha(0:nhig),gweak2(0:nhig),
     .                  alphas(0:nhig),g_f,deltar,alpha2,sw,cw2,cw
C
      COMMON / miscl / loutbe,ecm,idb1,idb2,xrad,empir,empirm,ism,
     &                 iklei,icar,sdvrt(3),vrtx(4),tabl(26),
     &                 nevent(11)
      INTEGER loutbe,idb1,idb2,nevent,ism,iklei,icar
      REAL*4 ecm,sdvrt,vrtx,tabl,empir,empirm,xrad
C
      REAL*8 cma,ctb,cmq,cmur,cmdr,cmtop,cau,cad,cmu,cmchi
      REAL*8 cmh,cmhp,chm,chmp,camp,cmhch,csa,cca
      REAL*8 cstop1,cstop2,csbot1,csbot2,ctanbA
      REAL*8 rmtop,rmbot,mz,alpha_1,alpha_2,alpha_3,v,ppi,sint,stw
      COMMON / mcarena / rmtop,rmbot,mz,alpha_1,alpha_2,alpha_3,
     .                   v,ppi,sint,stw
C
C
C Compute running top/bottom masses and coupling constants
C at the top mass scale
C
      alpha_1 = ulalem(amt**2) / cw2
      alpha_2 = alpha_1 / sw2 * cw2
      alpha_3 = runalf(amt,5.) * pi
      factork = 16.11
     .        - 1.04*(4.-amu/amt-amd/amt-ams/amt-amc/amt-amb/amt)
      runamt = amt
     .       / (1. + 4./3. *alpha_3/pi + factork*(alpha_3/pi)**2 )
      amhig(1) = amt
      runamb = amb * SQRT(runmas(amb,1,radb))
      amhig(1) = 0.
C
      IF ( icar .LE. 0 ) THEN
C
C This is the EPA approximation
C
        ama   = amarun
C
        amst1 = amst(1)**2
        amst2 = amst(2)**2
        amsb1 = amsb(1)**2
        amsb2 = amsb(2)**2
C
        crat    = susAt - susMU/tb
        crab    = susAb - susMU*tb
C
C The Delta_ij correction terms entering the mass matrix
C
        delta11 = runamb**4/cb2
     .          * (ALOG(amsb1*amsb2/runamb**4)
     .          + 2.*susAb*crab   * gf1(amsb1,amsb2)
     .          + (susAb*crab)**2 * gf2(amsb1,amsb2) )
     .          + runamt**4/sb2
     .          * (susMU*crat)**2 * gf2(amst1,amst2)
C
        delta22 = runamt**4/sb2
     .          * (ALOG(amst1*amst2/runamt**4)
     .          + 2.*susAt*crat   * gf1(amst1,amst2)
     .          + (susAt*crat)**2 * gf2(amst1,amst2) )
     .          + runamb**4/cb2
     .          * (susMU*crab)**2 * gf2(amsb1,amsb2)
C
        delta12 =-runamt**4/sb2
     .          * susMU*crat *    ( gf1(amst1,amst2)
     .          + susAt*crat *      gf2(amst1,amst2) )
     .          - runamb**4/cb2
     .          * susMU*crab *    (  gf1(amsb1,amsb2)
     .          + susAb*crab *       gf2(amsb1,amsb2) )
C
C The scale of the correction
C
        corr    = 3.*G_F/(SQRT(2.)*pi**2)
C
        delta11 = corr*delta11/2.
        delta22 = corr*delta22/2.
        delta12 = corr*delta12/2.
C
C The eigen values of the CP-even Higgs mass matrix
C
        a11   =  amz**2*cb2 + ama**2*sb2  + delta11
        a22   =  amz**2*sb2 + ama**2*cb2  + delta22
        a12   = -(amz**2 + ama**2)*s2b/2. + delta12
        delta = SQRT( (a11-a22)**2 + 4.*a12**2 )
        amh   = (a11 + a22 - delta)/2.
        gmh   = (a11 + a22 + delta)/2.
C
C Check the MSSM consistency
C
        IF ( amh .LE. 0. .OR. gmh .LE. 0. ) THEN
          IF ( idbg .GE. 0 ) THEN
            WRITE(6,1000) ama,tb,susM,susMU,susAt,susAB,susSMQ,susSMU,
     .                           susSMD,susSML,susSME
            WRITE(6,2000) amh,gmh
          ENDIF
          RETURN
C         STOP 99
        ENDIF
C
        IF ( amh .GE. 0 ) THEN
          amh   = SQRT(amh)
        ELSE
          amh   = -SQRT(-amh)
        ENDIF
        gmh   = SQRT(gmh)
C
C The CP-even mixing angle alpha (no ambiguities with ATAN2)
C
        costa = (a11-a22) / delta
        sinta = 2.*a12    / delta
        alfa = ATAN2(sinta,costa)/2.
C
C Radiative correction to the charged Higgs mass (coded only in
C the case of a large stop mixing)
C
        deltahp = -corr * runamt**4*susMU**2/4.
     .          * gf3(amst1,amst2,susSMQ**2)
        amhp = SQRT(amw**2+ama**2+deltahp)
C
      ELSE
C
C Here is the two-loops improved renormalization group calculation.
C
        cma   = amarun
        ctb   = tb
        cmq   = susSMQ
        cmur  = susSMU
        cmdr  = susSMD
        cmtop = amt
        cAu   = susAt
        cAd   = susAb
        cmu   = susMU
        cmchi = amchar(2)
C
        rmtop = runamt
        rmbot = runamb
C
        ppi = pi
        mz  = amz
        v   = 1. / SQRT(g_f * SQRT(2.)) / SQRT(2.)
        sint = sw2
        stw  = sw2
C
        CALL pole(3,cmchi,cma,ctb,cmq,cmur,cmdr,cmtop,cAu,cAd,cmu,
     *              cmh,cmhp,chm,chmp,camp,cmhch,csa,cca,
     *              cstop1,cstop2,csbot1,csbot2,ctanbA)
C
        amh  = cmhp
        gmh  = cHMp
        ama  = camp
C
        amhp = cmhch
        costa = cca
        sinta = csa
        alfa = ATAN2(sinta,costa)
C
      ENDIF
C
C Various couplings and angles.
C
      ta   = TAN(alfa)
      sab2 = SIN(beta-alfa)**2
      cab2 = COS(beta-alfa)**2
      ca = COS(alfa)
      sa = SIN(alfa)
      c2a = COS(2.*alfa)
      cab = COS(alfa+beta)
      sab = SIN(alfa+beta)
C
      RETURN
C------------------------------------------------------------------
 1000 FORMAT(1x,50('-')//
     .       1x,'With the following input parameters of the MSSM :'/
     .       1x,'   . A mass      : ',F8.3,' GeV/c**2'/
     .       1x,'   . Tan beta    : ',F8.3/
     .       1x,'   . M           : ',F8.3,' GeV/c**2'/
     .       1x,'   . mu          : ',F8.3,' GeV/c**2'/
     .       1x,'   . At          : ',F8.3/
     .       1x,'   . Ab          : ',F8.3/
     .       1x,'   . mQ          : ',F8.3,' GeV/c**2'/
     .       1x,'   . mU          : ',F8.3,' GeV/c**2'/
     .       1x,'   . mD          : ',F8.3,' GeV/c**2'/
     .       1x,'   . mL          : ',F8.3,' GeV/c**2'/
     .       1x,'   . mE          : ',F8.3,' GeV/c**2'/)
 2000 FORMAT(' A non physical set of Higgs masses has',
     .       ' been obtained :'/
     .       1x,'   . mh**2       : ',E10.4,' (GeV/c**2)**2'/
     .       1x,'   . mH**2       : ',E10.4,' (GeV/c**2)**2'/
     .       1x,' +++++++  STOP execution here +++++++'//)
      END
      FUNCTION sigklei(xmh,s)
C--------------------------------------------------------------------
C! Total cross section for the process e+e- ---> H Z*
C
C Subroutine from R. Kleiss, slightly modified to run with
C my COMMONs
C
C Input : s = beam-beam invariant mass square (GeV**2);
C         xmh = Higgs mass
C
C Modifications :
C   Patrick Janot
C       -- 28 Jan 1992 : Implement improved Born approximation
C       -- 12 Dec 1993 : Implement eeZ Vertex correction
C--------------------------------------------------------------------
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      COMMON / elweak / sw2,alpha(0:nhig),gweak2(0:nhig),
     .                  alphas(0:nhig),g_f,deltar,alpha2,sw,cw2,cw
C
      DIMENSION brai(11), kcode(11), xmasmi(11)
      DATA brai/.0335,.0665,.0335,.0665,.0335,.0665,
     .          .1540,.1190,.1540,.1190,.1540/
      DATA kcode/11,12,13,14,15,16,1,2,3,4,5/
      DATA xmasmi/6*0.,0.3,0.3,1.0,4.0,11.0/
      COMMON / zzdec / braz(11), kselec(11), fracz
C
C
      COMPLEX*16 xq,z1,z2,ffff
      REAL*8 a,x1
C
C Derived constants
C
      g    = 1./SQRT(sw2*(1.-sw2))
      ca   = -g/4.
      cv   = g*(4.*sw2-1.)/4.
      a    = 4.*xmh**2/s
      b    = amz*gmz/s
      xp   = 1.+(xmh**2-amz**2)/s
      x1   = SQRT(a)
      x2   = 1.+ a/4.
C The eeZ and mumuZ couplings
      con1 = alpha(0) * (g*(cv**2+ca**2))**2 / (36.*s)
C The "first" Z Breit-Wigner
      con2 = (amz**2/s)/( (1.-amz**2/s)**2 + b**2 )
C The full width correction
      con3 = alpha(0)*amz*(cv**2+ca**2)/(3.*gmz)
C The Improved Born approximation
      con4 = (1.+deltar)**2
C The ZZH vertex correction with a top loop
     .     * (1.-8./3.*deltar)
C And the weak correction associated to the e+e- --> Z vertex
     .     * (1.-8.*deltar*(1.-sw2)*cv*ca/(cv**2+ca**2))
C The overall multiplicative constant
      cons = con1 * con2 * con4 / con3
C
C The integral factor using an external function
C
      xq      = CMPLX(xp,b)
      z1      = xq + CDSQRT( xq*xq - a )
      z2      = 2.*xq - z1
C
C The integration
C
      IF ( CDABS(z1) .NE. 0. ) THEN
        sigklei = 1./(8.*b) * IMAG( 1./(z1-z2)
     .          * ( FFFF(a,z1,2D0)
     .            - FFFF(a,z1,x1)
     .            - FFFF(a,z2,2D0)
     .            + FFFF(a,z2,x1) ))
      ELSE
        sigklei = 0.
      ENDIF
C
C     sigklei = sigklei * cons / brai(3)
      sigklei = sigklei * cons
C
  999 RETURN
      END
      FUNCTION sigmawwh(s,ipro)
C -------------------------------------------------------------------
C! Compute the IBA cross section of the WW/ZZ fusion into h
C
C Input:    s,    the effective centre-of-mass energy squared
C           ipro, = 5 for WW --> h
C                 = 6 for WW --> H
C                 = 7 for ZZ --> h
C                 = 8 for ZZ --> H
C
C Patrick Janot -- 2 Sep 1995
C -------------------------------------------------------------------
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      PARAMETER (L1MST=200, L1PAR=200)
      PARAMETER (L2PAR=500, L2PARF=2000 )
      PARAMETER (LJNPAR=4000)
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),
     &                KFDP(L2PARF,5)
      COMMON /LUDAT4/ CHAF(L2PAR)
      CHARACTER*8 CHAF
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5)
C
      COMMON / elweak / sw2,alpha(0:nhig),gweak2(0:nhig),
     .                  alphas(0:nhig),g_f,deltar,alpha2,sw,cw2,cw
C
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
C
      EXTERNAL gsub3, dgmlt3
      REAL*8 x3lo,x3hi,x(3),DGMLT3
C
      REAL*8 kappah, kappav, v, a, rmh, rgh, ss
      COMMON / sigww / kappah, kappav, v, a, rmh, rgh, ss
C
C Case 1: WW scattering
C
      IF ( ipro .EQ. 5 .OR. ipro .EQ. 6 ) THEN
C
        rmv  = amw
        v    = SQRT(2.)
        a    = SQRT(2.)
        gvvh = gweak2(0) * rmv**2
C
C Case 2: ZZ scattering
C
      ELSEIF ( ipro .EQ. 7 .OR. ipro .EQ. 8 ) THEN
C
        rmv  = amz
        v    = -1.+4.*sw2
        a   = -1.
        gvvh = gweak2(0) * rmv**2 / cw**2
C
      ENDIF
C
C The Higgs mass
C
      IF     ( ipro .EQ. 5 .OR. ipro .EQ. 7 ) THEN
C
        rmh  = pmas(25,1)
        rgh  = pmas(25,2)
C
      ELSEIF ( ipro .EQ. 6 .OR. ipro .EQ. 8 ) THEN
C
        rmh  = pmas(35,1)
        rgh  = pmas(35,2)
C
      ENDIF
C
      con1     = G_F**3*rmv**4 / (64.*SQRT(2.)*pi**3)
     .         * alpha2 / alpha(0)**2
C
      con2     = alpha2 * gvvh
     .         / (1024. * pi * sw2**2 * amw**4 )
      con3     = (1.+deltar)**3 * (1.-5./3.*deltar)
C
      kappav   = rmv**2/s
C
      ss     = s
      x3lo    = -DATAN2(rmh,rgh)
      x3hi    =  DATAN2(ss-rmh**2,rmh*rgh)
      sigmawwh = con2 * con3 * DGMLT3(gsub3,x3lo,x3hi,1,6,x)
     .                       / (piby2+DATAN2(rmh,rgh))
C
  999 RETURN
      END
      FUNCTION sigma1(s)
C--------------------------------------------------------------------
C! Eight functions sigma1(s) --> sigma8(s) for the cross sections
C  of the processes 1 to 8.
C
C--------------------------------------------------------------------
      sigma1 = crocom(1,s)
      RETURN
      END
C
      FUNCTION sigma2(s)
      sigma2 = crocom(2,s)
      RETURN
      END
C
      FUNCTION sigma3(s)
      sigma3 = crocom(3,s)
      RETURN
      END
C
      FUNCTION sigma4(s)
      sigma4 = crocom(4,s)
      RETURN
      END
C
      FUNCTION sigma5(s)
      sigma5 = crocom(5,s)
      RETURN
      END
C
      FUNCTION sigma6(s)
      sigma6 = crocom(6,s)
      RETURN
      END
C
      FUNCTION sigma7(s)
      sigma7 = crocom(7,s)
      RETURN
      END
C
      FUNCTION sigma8(s)
      sigma8 = crocom(8,s)
      RETURN
      END
C
      FUNCTION sigma9(s)
      sigma9 = crocom(9,s)
      RETURN
      END
C
      FUNCTION sigmat(s)
      sigmat = sigma1(s)+sigma2(s)+sigma3(s)+sigma4(s)+sigma5(s)
      RETURN
      END
      SUBROUTINE squarks
C------------------------------------------------------------------
C!  Compute squark masses from the MSSM parameters
C
C  Input:    /PARAM/ MSSM parameters
C
C  Output:   /PARAM/ amst(2), amsb(2), the stop and sbottom masses
C
C   P. Janot -- 4 December 1994
C------------------------------------------------------------------
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
C
      COMMON / elweak / sw2,alpha(0:nhig),gweak2(0:nhig),
     .                  alphas(0:nhig),g_f,deltar,alpha2,sw,cw2,cw
C
      COMMON / miscl / loutbe,ecm,idb1,idb2,xrad,empir,empirm,ism,
     &                 iklei,icar,sdvrt(3),vrtx(4),tabl(26),
     &                 nevent(11)
      INTEGER loutbe,idb1,idb2,nevent,ism,iklei,icar
      REAL*4 ecm,sdvrt,vrtx,tabl,empir,empirm,xrad
C
C
C Compute running top/bottom masses and coupling constants
C at the top mass scale
C
      alpha_3 = runalf(amt,5.) * pi
      factork = 16.11
     .        - 1.04*(4.-amu/amt-amd/amt-ams/amt-amc/amt-amb/amt)
      runamt = amt
     .       / (1. + 4./3. *alpha_3/pi + factork*(alpha_3/pi)**2 )
      amhig(1) = amt
      runamb = amb * SQRT(runmas(amb,1,radb))
      amhig(1) = 0.
C
C
      c2b = COS(2.*beta)
      s2b = SIN(2.*beta)
      cb2 = COS(beta)**2
      sb2 = SIN(beta)**2
C
C The Stop mass matrix
C
      t11 = runamt**2 + susSMQ**2 + (4.*amw**2-1.*amz**2)*c2b/6.
      t22 = runamt**2 + susSMU**2 - (4.*amw**2-4.*amz**2)*c2b/6.
C     t12 = runamt * (susAt + susMU/tb)    ! Sign mistake (Marcela)
      t12 = runamt * (susAt - susMU/tb)
C
C The Stop masses
C
      delta   = SQRT( (t11-t22)**2 + 4.*t12**2 )
      amst(1) = (t11 + t22 - delta)/2.
      amst(2) = (t11 + t22 + delta)/2.
C
C The mixing angle in the Stop sector
C
      cosmix = -(t11-t22)
      sinmix = -2.*t12
      topmix = 0.
      IF ( cosmix*sinmix .NE. 0. ) topmix = ATAN2(sinmix,cosmix)/2.
C
C The Sbottom mass matrix
C
      b11 = runamb**2 + susSMQ**2 - (2.*amw**2+1.*amz**2)*c2b/6.
      b22 = runamb**2 + susSMD**2 + (2.*amw**2-2.*amz**2)*c2b/6.
C     b12 = runamb * (susAb + susMU*tb)  ! Sign mistake (Marcela)
      b12 = runamb * (susAb - susMU*tb)
C
C The Sbottom masses
C
      delta   = SQRT( (b11-b22)**2 + 4.*b12**2 )
      amsb(1) = (b11 + b22 - delta)/2.
      amsb(2) = (b11 + b22 + delta)/2.
C
C The mixing angle in the Sbottom sector
C
      cosmix = -(b11-b22)
      sinmix = -2.*b12
      botmix = 0.
      IF ( cosmix*sinmix .NE. 0. ) botmix = ATAN2(sinmix,cosmix)/2.
C
C Check the MSSM consistency
C
      IF ( amst(1)*amst(2) .LE. 0. .OR.
     .     amsb(1)*amsb(2) .LE. 0. ) THEN
        IF ( idbg .GE. 0 ) THEN
          WRITE(6,1000) ama,tb,susM,susMU,susAt,susAB,susSMQ,susSMU,
     .                         susSMD,susSML,susSME
          WRITE(6,2000) amst(1),amst(2),amsb(1),amsb(2)
        ENDIF
        RETURN
      ENDIF
C
      amst(1) = SQRT(amst(1))
      amst(2) = SQRT(amst(2))
      amsb(1) = SQRT(amsb(1))
      amsb(2) = SQRT(amsb(2))
C
      RETURN
C------------------------------------------------------------------
 1000 FORMAT(1x,50('-')//
     .       1x,'With the following input parameters of the MSSM :'/
     .       1x,'   . A mass      : ',F8.3,' GeV/c**2'/
     .       1x,'   . Tan beta    : ',F8.3/
     .       1x,'   . M           : ',F8.3,' GeV/c**2'/
     .       1x,'   . mu          : ',F8.3,' GeV/c**2'/
     .       1x,'   . At          : ',F8.3/
     .       1x,'   . Ab          : ',F8.3/
     .       1x,'   . mQ          : ',F8.3,' GeV/c**2'/
     .       1x,'   . mU          : ',F8.3,' GeV/c**2'/
     .       1x,'   . mD          : ',F8.3,' GeV/c**2'/
     .       1x,'   . mL          : ',F8.3,' GeV/c**2'/
     .       1x,'   . mE          : ',F8.3,' GeV/c**2'/)
 2000 FORMAT(' A non physical set of stop and sbottom masses has',
     .       ' been obtained :'/
     .       1x,'   . mStop1^2    : ',E10.4,' (GeV/c**2)**2'/
     .       1x,'   . mStop2^2    : ',E10.4,' (GeV/c**2)**2'/
     .       1x,'   . mSbottom1^2 : ',E10.4,' (GeV/c**2)**2'/
     .       1x,'   . mSbottom2^2 : ',E10.4,' (GeV/c**2)**2'/
     .       1x,' +++++++  STOP execution here +++++++'//)
      END
      FUNCTION strange(x,bet)
C-------------------------------------------------------------
C! Function used for the ISR computation to alpha_QED**2
C
C-------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      IF ( x .GT. 0D0 ) THEN
        strange = x**(1.-bet)
     .          * (4.*(2.-x)*DLOG(1./x)
     .          - (1.+3.*(1.-x)**2)/x*DLOG(1.-x)
     .          -  6.+x )
      ELSEIF ( x .EQ. 0D0 ) THEN
        strange = 0.
      ENDIF
      RETURN
      END
      SUBROUTINE USRDEF
C-------------------------------------------------------------
C!  The user puts here his running conditions for HZHA
C
C   Input:  Data cards file or user program
C
C  P. Janot  --  24 aug 1991
C-------------------------------------------------------------
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      CHARACTER*14 channel
      CHARACTER*21 channeut, chanchar
      PARAMETER(nchneut=8,nchchar=5)
      COMMON / hhdeck / branch(nchan,nhig),width(nhig),
     .                  parwth(nhig),xymas(2,nchan,nhig),
     .                  xywid(2,nchan,nhig)
      COMMON / chaneu / ichn(2),
     .                  wneut(4,4,nhig), wchar(2,2,nhig),
     .                  widneut(4), brneut(nchneut,4),
     .                  widchar(2), brchar(nchchar,2)
      COMMON / chanch / channeut(nchneut,4), chanchar(nchchar,2)
      COMMON / vect4 / pvect4(5,2)
      COMMON / hinput / ichan(nchan,nhig), channel(nchan,nhig)
      COMMON / hisbr / bchpp,bchgg
      DIMENSION ph(4)
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON / BCS / IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      INTEGER    HCCON, NSCH, NCH, NDCH, NMXC
      PARAMETER( NCH= 49, NDCH= 19, NMXC= 9 )
      REAL*8     HCCBR1, HCCBR2, TAUHP
      COMMON /HCDEFF/ NSCH(NDCH), HCCON(NDCH),
     .                HCCBR1(0:NDCH), HCCBR2(NDCH,NMXC), TAUHP
      CHARACTER*17 HCCHAN
      COMMON /HCNAME/ HCCHAN(NDCH)
      INTEGER    UPMAX
      REAL*8     MEMAX, THRES
      COMMON /HCMAXX/ MEMAX(NCH), THRES(NCH), UPMAX(NCH)
      INTEGER    HCDSTA
      COMMON /HCSTAT/ HCDSTA(0:NCH)
C-----------------------------------------------------------------------
C!    HCCHAN           channel names
C!    HCCON            requested decay ( 1= ON, 0= OFF )
C!    HCCBR1           cumulative BR on first channel classif. (data car
C!    HCCBR2           cumulative BR on subchannel classif. (internal)
C!    NSCH             number of subchannels per channel
C!    TAUHP            lifetime in seconds
C-----------------------------------------------------------------------
      INTEGER ITAB(nchan*nhig)
      REAL*4 TAB(nchan*nhig)
      EQUIVALENCE (TAB(1),ITAB(1))
      parameter ( lghhg=2,lgch=16,lgchc=19)
C
C  General steering conditions and cutoffs.
C  Look for bank 'GHHG'
C
      KNGHHG = NAMIND('GHHG')
      IF (KNGHHG .LE. 0) GOTO 100
      INGHHG = IW(KNGHHG)
      IF (INGHHG .GT. 0) THEN
        LNGHHG = IW(INGHHG)
C
C  Fill run conditions. GHHG format is :
C   Type    Inte  Inte
C          ICHAN  IDBG
C          (0=all)
C          (n=channel n only)
C
        IF (LNGHHG .GT. 0) THEN
         if ( LNGHHG.ne.lghhg) then
           write (6,*) ' the GHHG card has',LNGHHG,' inputs','should be'
     $        ,lghhg
           call exit
         endif
C  --  Channel number (if only one is selected)
          KW    = 1
          KCHAN = IW(INGHHG+KW)
          IF(KCHAN .GT. nCHAN .OR. KCHAN .LT. 0) THEN
            WRITE(6,1000) KW,KCHAN,nCHAN
            KCHAN = 0
          ENDIF
          IF (KCHAN .NE. 0) THEN
            CALL VZERO(ICHAN(1,1),nCHAN*nHIG)
            ICHAN(KCHAN,1) = 1
            ICHAN(KCHAN,2) = 1
            ICHAN(KCHAN,3) = 1
          ENDIF
C  --  Debug level
          IDBG   = IW(INGHHG+2)
        ENDIF
      ENDIF
C
C  Define a set of decay channels FOR H
C  Look for bank 'GCHn' (nth word = 0 to  disable  channel n,
C                                 = 1 to  enable   channel n)
C
  100 KNGCHA = NAMIND('GCH1')
      IF (KNGCHA .LE. 0) GOTO 110
      INGCHA = IW(KNGCHA)
      IF (INGCHA .GT. 0) THEN
        LNGCHA = IW(INGCHA)
        if ( LNGCHA.ne.lgch) then
           write (6,*) ' the GCH1 card has',LNGCHA,' inputs','should be'
     $        ,lgch
           call exit
        endif
C
C  Fill ICHAN array
C
        IF(LNGCHA .EQ. NCHAN) THEN
          DO 1 KCHAN=1,NCHAN
            ICHAN(KCHAN,1) = IW(INGCHA+KCHAN)
1         CONTINUE
        ELSEIF(LNGCHA .NE. 0) THEN
          WRITE(6,1003) LNGCHA,NCHAN
        ENDIF
      ENDIF
C
C  Same for h
C
  110 KNGCHA = NAMIND('GCH2')
      IF (KNGCHA .LE. 0) GOTO 120
      INGCHA = IW(KNGCHA)
      IF (INGCHA .GT. 0) THEN
        LNGCHA = IW(INGCHA)
        if ( LNGCHA.ne.lgch) then
           write (6,*) ' the GCH2 card has',LNGCHA,' inputs','should be'
     $        ,lgch
           call exit
        endif
C
C  Fill ICHAN array
C
        IF(LNGCHA .EQ. NCHAN) THEN
          DO 2 KCHAN=1,NCHAN
            ICHAN(KCHAN,2) = IW(INGCHA+KCHAN)
2         CONTINUE
        ELSEIF(LNGCHA .NE. 0) THEN
          WRITE(6,1003) LNGCHA,NCHAN
        ENDIF
      ENDIF
C
C  Same for A
C
  120 KNGCHA = NAMIND('GCH3')
      IF (KNGCHA .LE. 0) GOTO 130
      INGCHA = IW(KNGCHA)
      IF (INGCHA .GT. 0) THEN
        LNGCHA = IW(INGCHA)
        if ( LNGCHA.ne.lgch) then
           write (6,*) ' the GCH3 card has',LNGCHA,' inputs','should be'
     $        ,lgch
           call exit
        endif
C
C  Fill ICHAN array
C
        IF(LNGCHA .EQ. NCHAN) THEN
          DO 3 KCHAN=1,NCHAN
            ICHAN(KCHAN,3) = IW(INGCHA+KCHAN)
3         CONTINUE
        ELSEIF(LNGCHA .NE. 0) THEN
          WRITE(6,1003) LNGCHA,NCHAN
        ENDIF
      ENDIF
C
C  Print out of final values chosen for running H0DECAY
C
  130 CONTINUE
      WRITE(6,1004) IDBG
      IF ( IDBG .LT. 2 )  GOTO 11
      WRITE(6,1005)
      DO 10 KHIG =1,nHIG
      DO 10 KCHAN=1,NCHAN
        IF(ICHAN(KCHAN,KHIG) .EQ. 1)
     .  WRITE(6,1006) KCHAN,CHANNEL(KCHAN,KHIG)
   10 CONTINUE
   11 CONTINUE
C
C  Create and fill bank KHIG with those values.
C
      DO 20 KCHAN=1,NCHAN
      DO 20 KHIG =1,NHIG
        ITAB(KCHAN + NCHAN*(KHIG-1)) = ICHAN(KCHAN,KHIG)
   20 CONTINUE
      CALL ALTABL('KHHG',nchan*nhig,1,TAB,'(I)','C')
C
C - Charged Higgses:
C - Enable/Disable accordingly to data cards (default ON)
C
      JGCHC= NLINK('GCHC',0)
      IF( JGCHC.GT.0 ) THEN
        len = iw(jgchc)
        if ( len.ne.lgchc) then
           write (6,*) ' the GCHC card has',len,' inputs','should be'
     $        ,lgchc
           call exit
        endif
        DO I=1,NDCH
          J= IW(JGCHC+I)
          IF( J.EQ.0 .OR. J.EQ.1 ) THEN
            HCCON(I)= J
          ELSE
            HCCON(I)= 1
          END IF
        END DO
      ELSE
C - Data card missing: enble all the channels ...
        DO I=1,NDCH
          HCCON(I)= 1
        END DO
      END IF

      RETURN
C-------------------------------------------------------------------
  999 FORMAT(1X,'    +++ USRDEF +++ Warning +++'/
     .       1X,'Not enough space for creating KHIG bank')
 1000 FORMAT(/1X,'+++ USRDEF Warning +++ '/
     .       1X,'Word #  1 in data card GHHG (',I6,
     .          ') should be between 0 and ',I2)
 1003 FORMAT(1X,'+++ USRDEF Warning +++ '/
     .       1X,' Length of data card GCHA (',I6,
     .          ') should be 0 or ',I2)
 1004 FORMAT(1X,'Initial conditions for running H0DECAY are :'/
     .       5X,'. Debug level             : ',I2//)
1005  FORMAT(1X,'Requested decay channels are :'/)
1006  FORMAT(5X,I2,'. ',A12)
1009  FORMAT(1X,'+++ USRDEF Warning +++ '/
     .       1X,'Word #',I2,' in data card GHGG (',D14.6,
     .          ') should be between .6 and 2.5')
      END
      SUBROUTINE vac(ihiggs,mchi,ma,tanb,mq,mur,mdr,
     *  mtop,at,ab,mu,mh,mhp,hm,hmp,amp,mhch,stop1,stop2,
     *  sbot1,sbot2,sa,ca,stop1w,stop2w,tanbA)
C--------------------------------------------------------------------
C! Computes vacuum polarization effects to Higgs masses
C
C  From M. Carena and C. Wagner.
C
C  Modifications :
C      1. Patrick Janot -- 20 Sep 1995
C         Improves the iteration procedure to fasten the routine
C         (--> factor 100 improvement roughly)
C--------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,M,O-Z)
      REAL*8 cma,ctb,cmq,cmur,cmdr,cmtop,cau,cad,cmu,cmchi
      REAL*8 cmh,cmhp,chm,chmp,camp,cmhch,csa,cca
      REAL*8 cstop1,cstop2,csbot1,csbot2,ctanbA
      REAL*8 rmtop,rmbot,mz,alpha_1,alpha_2,alpha_3,v,ppi,sint,stw
      COMMON / mcarena / rmtop,rmbot,mz,alpha_1,alpha_2,alpha_3,
     .                   v,ppi,sint,stw
C
      DIMENSION delta(2,2),coupt(2,2),T(2,2),sstop2(2),
     *ssbot2(2),B(2,2),coupb(2,2),
     *hcoupt(2,2),hcoupb(2,2),
     *acoupt(2,2),acoupb(2,2)
      DIMENSION polar(3),pr(3)
C
      delta(1,1) = 1.
      delta(2,2) = 1.
      delta(1,2) = 0.
      delta(2,1) = 0.
C
Cpaj  v = 174.1
Cpaj  mz=91.18
Cpaj  pi=3.14159
Cpaj  alpha3z=.12
Cpaj  alpha3=1./(1./alpha3z+23./6./pi*log(mtop/mz))
Cpaj  rmtop = mtop/(1.+4*alpha3/3./pi)
Cpaj  rmbot = 3.
      pi = ppi           ! paj
C
      ht = rmtop /v
      CALL rghm(mchi,ma,tanb,mq,mur,mdr,mtop,at,ab,
     *   mu,mh,hm,mhch,sa,ca,tanbA)
C
      mhp = mh
      hmp = hm
      amp = ma
      IF ( mh .LE. 0. ) GOTO 3333
C
      sinb = tanb/(tanb**2+1.)**.5
      cosb = 1./(tanb**2+1.)**.5
      cos2b = sinb**2 - cosb**2
      sinbpa = sinb*ca + cosb*sa
      cosbpa = cosb*ca - sinb*sa
      mq2 = mq**2
      mur2 = mur**2
      mdr2 = mdr**2
      mst11 = Rmtop**2 + mq2  - 0.35*MZ**2*cos2b
      mst22 = Rmtop**2 + mur2 - 0.15*MZ**2*cos2b
      if(mst11.lt.0.) goto 3333
      if(mst22.lt.0.) goto 3333
      msb11 = Rmbot**2 + mq2  + 0.42*MZ**2*cos2b
      msb22 = Rmbot**2 + mdr2 + 0.08*MZ**2*cos2b
      if(msb11.lt.0.) goto 3333
      if(msb22.lt.0.) goto 3333
      wmst11 = Rmtop**2 + mq2
      wmst22 = Rmtop**2 + mur2
      mst12 = Rmtop*(At - mu/tanb)
      msb12 = Rmbot*(Ab - mu*tanb)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C            Stop Eigenvalues calculation
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      Stop12 = 0.5*(mst11+mst22) +
     * 0.5*((mst11+mst22)**2 -
     * 4.*(mst11*mst22 - mst12**2))**.5
      Stop22 = 0.5*(mst11+mst22) -
     * 0.5*((mst11+mst22)**2 - 4.*(mst11*mst22 - mst12**2))**.5
      if(Stop22.lt.0.) goto 3333
      sstop2(1) = stop12
      sstop2(2) = stop22
      stop1 = Stop12**.5
      stop2 = Stop22**.5
      stop1w = stop1
      stop2w = stop2
C
      IF ( mst12 .EQ. 0. ) THEN
        xst11 = 1.
        xst12 = 0.
        xst21 = 0.
        xst22 = 1.
      ELSE
        xst11 = mst12/(mst12**2+(mst11-stop12)**2)**.5
        xst12 = - (mst11-stop12)/(mst12**2+(mst11-stop12)**2)**.5
        xst21 = mst12/(mst12**2+(mst11-stop22)**2)**.5
        xst22 = - (mst11-stop22)/(mst12**2+(mst11-stop22)**2)**.5
      ENDIF
C
      T(1,1) = xst11
      T(2,2) = xst22
      T(1,2) = xst12
      T(2,1) = xst21
C
      Sbot12 = 0.5*(msb11+msb22) +
     * 0.5*((msb11+msb22)**2 -
     * 4.*(msb11*msb22 - msb12**2))**.5
      Sbot22 = 0.5*(msb11+msb22) -
     * 0.5*((msb11+msb22)**2 - 4.*(msb11*msb22 - msb12**2))**.5
      if(Sbot22.lt.0.) goto 3333
      sbot1 = Sbot12**.5
      sbot2 = Sbot22**.5
      ssbot2(1) = sbot12
      ssbot2(2) = sbot22
C
      IF ( msb12 .EQ. 0. ) THEN
        xsb11 = 1.
        xsb12 = 0.
        xsb21 = 0.
        xsb22 = 1.
      ELSE
        xsb11 = msb12/(msb12**2+(msb11-sbot12)**2)**.5
        xsb12 = - (msb11-sbot12)/(msb12**2+(msb11-sbot12)**2)**.5
        xsb21 = msb12/(msb12**2+(msb11-sbot22)**2)**.5
        xsb22 = - (msb11-sbot22)/(msb12**2+(msb11-sbot22)**2)**.5
      ENDIF
C
      B(1,1) = xsb11
      B(2,2) = xsb22
      B(1,2) = xsb12
      B(2,1) = xsb21
C
Cpaj  sint = 0.2320
      sqr = 2.**.5
Cpaj  vp = 174.1*sqr
      vp = v*sqr
C
cccccccccccccccccccccccccccccccccccc
ccc    starting of light higgs
cccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccc
      if(ihiggs.eq.0)goto 3524
ccccccccccccccccccccccccccccccccccccccccc
      DO 4646 i = 1,2
      DO 4576 j = 1,2
        coupt(i,j) =
Cpaj * sint*mz**2*2.*sqr/174./3.*sinbpa*(delta(i,j) +
     * sint*mz**2*2.*sqr/ v  /3.*sinbpa*(delta(i,j) +
     * (3. - 8.*sint)/4./sint*T(1,i)*T(1,j))
Cpaj * -rmtop**2/174.1**2*vp/sinb*ca*delta(i,j)
     * -rmtop**2/  v  **2*vp/sinb*ca*delta(i,j)
     * -rmtop/vp/sinb*(At*ca + mu*sa)*(T(1,i)*T(2,j) +
     *T(1,j)*T(2,i))
 4576 CONTINUE
 4646 CONTINUE
C
      DO 1646 i = 1,2
      DO 1576 j = 1,2
        coupb(i,j) =
Cpaj * -sint*mz**2*2.*sqr/174./6.*sinbpa*(delta(i,j) +
     * -sint*mz**2*2.*sqr/ v  /6.*sinbpa*(delta(i,j) +
     * (3. - 4.*sint)/2./sint*B(1,i)*B(1,j))
Cpaj * +rmbot**2/174.1**2*vp/cosb*sa*delta(i,j)
     * +rmbot**2/  v  **2*vp/cosb*sa*delta(i,j)
     * +rmbot/vp/cosb*(Ab*sa + mu*ca)*(B(1,i)*B(2,j) +
     * B(1,j)*B(2,i))
 1576 CONTINUE
 1646 CONTINUE
C
      prun = mh
      eps = 1D-4*prun
      iter = 0
7007  iter = iter + 1
      DO 7980 i3 = 1,3
        pr(i3)=prun+(i3-2)*eps/2
        p2=pr(i3)**2
        polt = 0.
        DO 7979 i = 1,2
        DO 7978 j = 1,2
         polt = polt + coupt(i,j)**2*3.*
     *   fint(p2,sstop2(i),sstop2(j))/16./pi**2
 7978   CONTINUE
 7979   CONTINUE
        polb = 0.
        DO 9979 i = 1,2
        DO 9978 j = 1,2
          polb = polb + coupb(i,j)**2*3.*
     *    fint(p2,ssbot2(i),ssbot2(j))/16./pi**2
 9978   CONTINUE
 9979   CONTINUE
        rmtop2 = rmtop**2
        mtop2=mtop**2
C
        poltt =
Cpaj * 3.*rmtop**2/8./pi**2/174.1**2*
     * 3.*rmtop**2/8./pi**2/  v  **2*
     * ca**2/sinb**2 *
     *   (-2.*mtop**2+.5*p2)*
     *  fint(p2,mtop2,mtop2)
C
        pol = polt + polb + poltt
        polar(i3) = p2 - mh**2 - pol
 7980 CONTINUE
      deriv = (polar(3)-polar(1))/eps
      drun = - polar(2)/deriv
      prun = prun + drun
      p2 = prun**2
      IF ( ABS(drun) .LT. 1D-4 ) GOTO 7777
      GOTO 7007
 7777 CONTINUE
C
       mhp = p2**.5
C
cccccccccccccccccccccccccccccccccccccccc
ccc   end of light higgs
cccccccccccccccccccccccccccccccccccccccc
 3340 IF(ihiggs.EQ.1)GOTO 3524
ccccccccccccccccccccccccccccccccccccccccc
ccc starting of heavy higgs
cccccccccccccccccccccccccccccccccccccccccc
C
      DO 1446 I = 1,2
      DO 1476 J = 1,2
        hcoupt(i,j) =
Cpaj * -sint*mz**2*2.*sqr/174./3.*cosbpa*(delta(i,j) +
     * -sint*mz**2*2.*sqr/ v  /3.*cosbpa*(delta(i,j) +
     * (3. - 8.*sint)/4./sint*T(1,i)*T(1,j))
Cpaj * -rmtop**2/174.1**2*vp/sinb*sa*delta(i,j)
     * -rmtop**2/  v  **2*vp/sinb*sa*delta(i,j)
     * -rmtop/vp/sinb*(At*sa - mu*ca)*(T(1,i)*T(2,j) +
     *T(1,j)*T(2,i))
 1476 CONTINUE
 1446 CONTINUE
C
      DO 1146 I = 1,2
      DO 1176 J = 1,2
        hcoupb(i,j) =
Cpaj * sint*mz**2*2.*sqr/174./6.*cosbpa*(delta(i,j) +
     * sint*mz**2*2.*sqr/ v  /6.*cosbpa*(delta(i,j) +
     * (3. - 4.*sint)/2./sint*B(1,i)*B(1,j))
Cpaj * -rmbot**2/174.1**2*vp/cosb*ca*delta(i,j)
     * -rmbot**2/  v  **2*vp/cosb*ca*delta(i,j)
     * -rmbot/vp/cosb*(Ab*ca - mu*sa)*(B(1,i)*B(2,j) +
     * B(1,j)*B(2,i))
        hcoupb(i,j)=0.
 1176 CONTINUE
 1146 CONTINUE
C
      prun = hm
      eps = 1D-4*prun
      iter = 0
 1001 iter = iter + 1
      DO 1780 i3 = 1,3
        pr(i3)=prun+(i3-2)*eps/2
        hp2=pr(i3)**2
C
        hpolt = 0.
        do 1779 i = 1,2
        do 1778 j = 1,2
        hpolt = hpolt + hcoupt(i,j)**2*3.*
     *  fint(hp2,sstop2(i),sstop2(j))/16./pi**2
 1778 CONTINUE
 1779 CONTINUE
C
      hpolb = 0.
      DO 1979 I = 1,2
      DO 1978 J = 1,2
        hpolb = hpolb + hcoupb(i,j)**2*3.*
     *  fint(hp2,ssbot2(i),ssbot2(j))/16./pi**2
 1978 CONTINUE
 1979 CONTINUE
C
      rmtop2 = rmtop**2
      mtop2  = mtop**2
C
      hpoltt =
Cpaj * 3.*rmtop**2/8./pi**2/174.1**2*
     * 3.*rmtop**2/8./pi**2/  v  **2*
     *  sa**2/sinb**2 *
     *   (-2.*mtop**2+.5*hp2)*
     *  fint(hp2,mtop2,mtop2)
C
      hpol = hpolt + hpolb + hpoltt
      polar(i3) =hp2-hm**2-hpol
 1780 CONTINUE
      deriv = (polar(3)-polar(1))/eps
      drun = - polar(2)/deriv
      prun = prun + drun
      hp2 = prun**2
      IF ( ABS(drun) .LT. 1D-4 ) GOTO 1111
      GOTO 1001
 1111 CONTINUE
C
      hmp = hp2**.5
ccccccccccccccccccccccccccccccccccccccccccc
ccc  end of heavy higgs
cccccccccccccccccccccccccccccccccccccccccccc
      if(ihiggs.eq.2)goto 3524
cccccccccccccccccccccccccccccccccccccccccccc
ccc  beginning of pseudoscalar higgs
cccccccccccccccccccccccccccccccccccccccccccc
C
      DO 3446 i = 1,2
      DO 3476 j = 1,2
        acoupt(i,j) =
     * -rmtop/vp/sinb*(At*cosb + mu*sinb)*
     *  (T(1,i)*T(2,j) -T(1,j)*T(2,i))
 3476 CONTINUE
 3446 CONTINUE
C
      DO 3146 I = 1,2
      DO 3176 J = 1,2
        acoupb(i,j) =
     * rmbot/vp/cosb*(Ab*sinb + mu*cosb)*
     *  (B(1,i)*B(2,j) -B(1,j)*B(2,i))
 3176 CONTINUE
 3146 CONTINUE
C
      prun = ma
      eps = 1D-4*prun
      iter = 0
 6006 iter = iter + 1
      DO 3780 i3 = 1,3
        pr(i3)=prun+(i3-2)*eps/2
        ap2=pr(i3)**2
        apolt = 0.
        DO 3779 I = 1,2
        DO 3778 J = 1,2
          apolt = apolt + acoupt(i,j)**2*3.*
     *    fint(ap2,sstop2(i),sstop2(j))/16./pi**2
 3778   CONTINUE
 3779   CONTINUE
        apolb = 0.
        DO 3979 I = 1,2
        DO 3978 J = 1,2
          apolb = apolb + acoupb(i,j)**2*3.*
     *    fint(ap2,ssbot2(i),ssbot2(j))/16./pi**2
 3978   CONTINUE
 3979   CONTINUE
        rmtop2 = rmtop**2
        mtop2=mtop**2
        apoltt =
Cpaj *  3.*rmtop**2/8./pi**2/174.1**2*
     *  3.*rmtop**2/8./pi**2/  v  **2*
     *  cosb**2/sinb**2 *
     *   (-.5*ap2)*
     *  fint(ap2,mtop2,mtop2)
        apol = apolt + apolb + apoltt
        polar(i3) = ap2 - ma**2 -apol
 3780 CONTINUE
      deriv = (polar(3)-polar(1))/eps
      drun = - polar(2)/deriv
      prun = prun + drun
      ap2 = prun**2
      IF ( ABS(drun) .LT. 1D-4 ) GOTO 6666
      GOTO 6006
 6666 CONTINUE
C
      amp = ap2**.5
C
ccccccccccccccccccccccccccccccccccccccccccc
ccc end of pseudoscalar higgs
cccccccccccccccccccccccccccccccccccccccccccc
        if(ihiggs.eq.3)goto 3524
cccccccccccccccccccccccccccccccccccccccccccc
C
3524  RETURN
3333  RETURN
      END
      FUNCTION weakcor(jhig,fmas,fchrg,rt,rw)
C-------------------------------------------------------------------
C! Compute the one loop electroweak correction to h --> ffbar
C  (for f = e,mu,tau,u,d,s,c,b, but not t). Valid for mH < 2mW
C  but clearly useless for mH > 2mW !
C
C Patrick Janot -- 28 Sept 1995
C-------------------------------------------------------------------
      PARAMETER ( nchan=16, nhig=3 )
      COMMON / hmasss / amhig(nhig), amh, gmh, ama, amz, amw, gmz,
     .                  amtau, amb, amc, amt, ame, ammu, amu,
     .                  amd, ams, amhp, gmw, amst(2), amsb(2),
     .                  amsq, amneut(4),amchar(2), amarun
      COMMON / lifeti / tauh(nhig)
      COMMON / conqcd / xlamda5
      COMMON / wwzzch / wwmax(2,nhig), jtyp(2,nhig), w1, w2
      COMMON / mixing / alfa, beta, topmix, botmix,
     .                  aa(nhig,4,4),bb(nhig,2,2),
     .                  fieldn(4,4), umat(2,2), vmat(2,2),
     .                  ssmat(4,4),qqmat(4,4)
      COMMON / coupls / sa, ca, sb, cb, ta, tb, sab2, cab2,
     .                  s2a, c2a, s2b, c2b, sb2, cb2, cab, sab
      COMMON / susyms / susM, susMU, susAt, susAb, susSMQ, susSMU,
     .                  susSMD, susSML, susSME, susM1, susM2
      COMMON / flags  / idbg
      DIMENSION suspar(11)
      EQUIVALENCE(susM,suspar(1))
C
      PARAMETER(nstep=20)
      COMMON / crocro / ecs(nstep),crs(nstep),wsup(nstep)
      COMMON / poidsm / wtot(4),wtot2(4),ntry(4),nacc(4)
C
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
C
      COMMON / elweak / sw2,alpha(0:nhig),gweak2(0:nhig),
     .                  alphas(0:nhig),g_f,deltar,alpha2,sw,cw2,cw
C
      IF ( fmas .EQ. amb ) THEN
        Cf = 1.
      ELSE
        Cf = 7.-4.*alphas(jhig)
      ENDIF
C
      qed     =      alpha(jhig)/pi * 3./2. * fchrg**2
     .             * (3./2.-ALOG(amhig(jhig)**2/fmas**2))
C
      weak    =      G_F / (8.*pi**2*SQRT(2.))
     .             * ( Cf*amt**2*rt
     .               + amw**2*rw*(3.*ALOG(cw2)/sw2-5.)
     .               + amz**2*rw*(.5-3.*(1.-4.*sw2*fchrg)**2) )
C
      weakcor = 1. + qed + weak
C
      RETURN
      END
      SUBROUTINE wzdecy(iwz)
C-------------------------------------------------------------------
C! Decay a virtual W or Z, and prepare the parton shower
C  in case of a hadronic decay
C
C  Input :    iwz, the position of the virtual boson in /LUJET/
C
C  Patrick Janot -- 01 Sep 1995
C-------------------------------------------------------------------
      PARAMETER (L1MST=200, L1PAR=200)
      PARAMETER (L2PAR=500, L2PARF=2000 )
      PARAMETER (LJNPAR=4000)
      COMMON /LUDAT1/ MSTU(L1MST),PARU(L1PAR),MSTJ(L1MST),PARJ(L1PAR)
      COMMON /LUDAT2/ KCHG(L2PAR,3),PMAS(L2PAR,4),PARF(L2PARF),VCKM(4,4)
      COMMON /LUDAT3/ MDCY(L2PAR,3),MDME(L2PARF,2),BRAT(L2PARF),
     &                KFDP(L2PARF,5)
      COMMON /LUDAT4/ CHAF(L2PAR)
      CHARACTER*8 CHAF
      COMMON /LUJETS/ N7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),V7LU(LJNPAR,5)
C
      DIMENSION ijoin(2)
C
    1 CALL ludecy(iwz)
C
C In case of a hadronic decay
C
      IF ( IABS(k7lu(n7lu,2)) .LE. 8 ) THEN
C
C Reject too low masses to avoid infinite loops in LUEXEC
C
        xmm = p7lu(iwz,5)
        IF ( xmm .LE. 0.300 ) THEN
          k7lu(n7lu,1) = 1
          n7lu = n7lu - 2
          GOTO 1
        ENDIF
C
C From Torbjorn : Reset mstj(92) to avoid a duplication of the
C parton shower and a subsequent crash in LUEXEC
C
        mstj(92) = 0
C
C Prepare the parton shower
C
        ijoin(1) = k7lu(iwz,4)
        ijoin(2) = k7lu(iwz,5)
        njoin = 2
        CALL lujoin(njoin,ijoin)
        CALL lushow(ijoin(1), ijoin(2), xmm)
C
      ENDIF
C
      RETURN
      END
      FUNCTION zcaren(x)
C-----------------------------------------------------------------------
C! Function used for the Higgs masses computation
C
C  From M. Carena and C. Wagner
C-----------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,M,O-Z)
      SA = 1. - 4.*X
      if(Sa.lt.0.) sa1 = abs(sa)
      if(SA.lt.0.) Zcaren = 2.*SA1**.5*atan(1./SA1**.5)
      if(SA.gt.0.) Zcaren = SA**.5*log((1.+SA**.5)/(1.-SA**.5))
      RETURN
      END
      SUBROUTINE zdecay(qz,izpol,q1,q2,ifs)
C-----------------------------------------------------------------------
C! Decay a Z boson with a polarization IZPOL
C
C  Not used if IKLEI = 1
C
C  Patrick Janot -- 25 Nov 1991
C-----------------------------------------------------------------------
      PARAMETER (PI=3.1415926535897932364,PI2=PI*PI,PI4=PI2*PI2)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
C
      DIMENSION brai(11), kcode(11), xmasmi(11)
      DATA brai/.0335,.0665,.0335,.0665,.0335,.0665,
     .          .1540,.1190,.1540,.1190,.1540/
      DATA kcode/11,12,13,14,15,16,1,2,3,4,5/
      DATA xmasmi/6*0.,0.3,0.3,1.0,4.0,11.0/
      COMMON / zzdec / braz(11), kselec(11), fracz
C
      DIMENSION qz(4),q1(4),q2(4),p1(4),p2(4)
      REAL*8 betax, betay, betaz, a(3,3)
C
      CALL vzero(q1(1),4)
      CALL vzero(q2(1),4)
      CALL vzero(p1(1),4)
      CALL vzero(p2(1),4)
C
C Choice of the decay channel
C
      choix = RNDM(choix)
      DO ifs = 1,11
       IF ( choix .LT. braz(ifs) ) GOTO 1
      ENDDO
C
C Choice of the polar decay angle in the Z frame
C
    1 costet = 2.*rndm(costet) - 1.
      IF ( izpol .EQ. 1 ) THEN
        weight = (1.+costet**2)/2.
      ELSE
        weight = (1.-costet**2)/2.
      ENDIF
      IF ( weight .LT. RNDM(weight) ) GOTO 1
      sintet = SQRT(1.-costet**2)
C
C Choice of the azimuthal decay angle in the Z frame
C
      phi = 2.*pi*RNDM(phi)
      sinphi = SIN(phi)
      cosphi = COS(phi)
C
C 4-vectors of the decay products in the Z frame
C
      kff = kcode(ifs)
      pz    = SQRT(qz(1)**2+qz(2)**2+qz(3)**2)
      ez    = SQRT(qz(4)**2+pz**2)
      pt = SQRT(qz(1)**2+qz(2)**2)
      ene = qz(4)/2.
      pm = ulmass(kff)
      pmm = SQRT(ene**2-pm**2)
      q1(4) = ene
      q1(3) = pmm*costet
      q1(2) = pmm*sintet*sinphi
      q1(1) = pmm*sintet*cosphi
      q2(4) = q1(4)
      q2(3) =-q1(3)
      q2(2) =-q1(2)
      q2(1) =-q1(1)
C
C Boost in the lab frame
C
      betax = 0.
      betay = 0.
      betaz =-pz/ez
      CALL lorenz(betax,betay,betaz,q1)
      CALL lorenz(betax,betay,betaz,q2)
C
C Rotation to the standard reference frame
C
      a(1,3) = qz(1)/pz
      a(2,3) = qz(2)/pz
      a(3,3) = qz(3)/pz
      a(1,2) = qz(2)/pt
      a(2,2) =-qz(1)/pt
      a(3,2) = 0.
      a(1,1) = a(2,2)*a(3,3)-a(3,2)*a(2,3)
      a(2,1) = a(3,2)*a(1,3)-a(1,2)*a(3,3)
      a(3,1) = a(1,2)*a(2,3)-a(2,2)*a(1,3)
      p1(1) = a(1,1)*q1(1)+a(1,2)*q1(2)+a(1,3)*q1(3)
      p1(2) = a(2,1)*q1(1)+a(2,2)*q1(2)+a(2,3)*q1(3)
      p1(3) = a(3,1)*q1(1)+a(3,2)*q1(2)+a(3,3)*q1(3)
      p2(1) = a(1,1)*q2(1)+a(1,2)*q2(2)+a(1,3)*q2(3)
      p2(2) = a(2,1)*q2(1)+a(2,2)*q2(2)+a(2,3)*q2(3)
      p2(3) = a(3,1)*q2(1)+a(3,2)*q2(2)+a(3,3)*q2(3)
      CALL ucopy(p1(1),q1(1),3)
      CALL ucopy(p2(1),q2(1),3)
      q1(4) = SQRT(q1(1)**2+q1(2)**2+q1(3)**2+pm**2)
      q2(4) = SQRT(q2(1)**2+q2(2)**2+q2(3)**2+pm**2)
C
      RETURN
      END
      FUNCTION SIGHHC( S, TERM )
C-----------------------------------------------------------------------
C!    Calculates total cross-section for e+e- -> H+H-
C!    with width effects
C!
C!    Input:   S  (REAL*8)     center_of_mass_energy squared (GeV**2)
C!          TERM  (INTEGER)    1 =>  only photon exchange
C!                             2 =>  only Z exchange
C!                             3 =>  only Z-photon interference
C!                             0 =>  all terms ( 1+2+3 )
C!    N.B.: Charged Higgs mass and width are taken from JETSET commons
C!
C!    Output: SIGHHC (REAL*8)   total cross-section ( picobarn )
C!
C!    Gerardo Ganis              3 May 1996
C-----------------------------------------------------------------------
      IMPLICIT   NONE
      REAL*8     SIGHHC
C - Arguments
      REAL*8     S
      INTEGER    TERM
C - JETSET commons
      INTEGER     MSTU, MSTJ
      REAL*4      PARU, PARJ
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER     KCHG
      REAL*4      PMAS, PARF, VCKM
      COMMON /LUDAT2/ KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
C - Auxiliary variables
      REAL*8     XSGA, XSZ0, XSGZ
      REAL*8     MZ, GZ, SW2, ALFEM
      REAL*8     CL, CR, ST2W, PROZ, E4, XS, HHKIN, BETA
      REAL*8     MH, GH
C - Constants
      INTEGER    KHC, KFZ, MAXWDT
      PARAMETER( KHC= 37, KFZ= 23, MAXWDT= 20 )
      REAL*8     PI, TOPB
      PARAMETER( PI= 3.141592653589793238, TOPB= 0.38937966D9 )
C - External functions
      REAL*4     ULALEM
      EXTERNAL   ULALEM
      REAL*8     HCCONV
      EXTERNAL   HCCONV
C
C - Initialize
      SIGHHC= 0.D0
      XS    = 0.D0
      XSGA  = 0.D0
      XSZ0  = 0.D0
      XSGZ  = 0.D0
C - return if s <= 0
      IF( S.LE.0.D0 ) RETURN
C - Get info from JETSET commons
      MH   = DBLE(PMAS( KHC, 1))
      GH   = DBLE(PMAS( KHC, 2))
      MZ   = DBLE(PMAS( KFZ, 1))
      GZ   = DBLE(PMAS( KFZ, 2))
      SW2  = DBLE(PARU( 102 ))
      ALFEM= DBLE(ULALEM( SNGL(S) ))
C - Calculate auxiliary variables
      CL   = -0.5D0 + SW2
      CR   =          SW2
      ST2W = 4.D0*SW2*(1.D0-SW2) / (1.D0-2.D0*SW2)
      PROZ = (S-MZ**2)**2 + (GZ*MZ)**2
      E4   = (4.D0*PI*ALFEM)**2
C - Calculate cross-section terms
      IF( TERM.EQ.0 .OR. TERM.EQ.1 ) THEN
C - photon exchange
        XSGA= 1.D0 / S
      END IF
      IF( TERM.EQ.0 .OR. TERM.EQ.2 ) THEN
C - Z exchange
        XSZ0= 2.D0 * (CL**2+CR**2) / ST2W**2 * S / PROZ
      END IF
      IF( TERM.EQ.0 .OR. TERM.EQ.3 ) THEN
C - photon-Z inteference
        XSGZ=-2.D0 * (CL+CR) / ST2W * (S-MZ**2) / PROZ
      END IF
C - Kinematical factor
      IF( MH .LT. (SQRT(S)/2.D0-MAXWDT*GH) ) THEN
        BETA = SQRT(MAX(1.D0-4*MH**2/S,0.D0))
        HHKIN= BETA**3
      ELSE
        HHKIN= HCCONV( S, MH, GH )
      END IF
C - Get total cross-section ( 48 = 64*3/4 )
      XS = E4/(48.D0*PI) * ( XSGA + XSZ0 + XSGZ ) * HHKIN
C - Convert to picobarn
      SIGHHC = XS * TOPB
C - Return successfully
      RETURN
      END

      FUNCTION ALPHAS(Q,N)
C-----------------------------------------------------------------------
C!
C!    INPUT:          Q   scale (GeV)                         (REAL*8)
C!                    N   option                              (INTEGER)
C!    OUTPUT:         ALPHAS    running quark mass             (REAL*8)
C!
C!    Comments:       Cleaning of Djouadi code.
C!    Author:         A.Djouadi
C!    Cleaned by      G.Ganis         July 2, 96
C-----------------------------------------------------------------------
      IMPLICIT   NONE
      REAL*8     ALPHAS
C - Parameters
C - Arguments
      INTEGER    N
      REAL*8     Q
C - Common blocks
      REAL*8     XLB1, XLB2
      COMMON    /ALSLAM/ XLB1(6),XLB2(6)
      INTEGER    N0
      REAL*8     XLAMBDA, AMC, AMB, AMT
      COMMON    /ALS/ XLAMBDA, AMC, AMB, AMT, N0
C - Auxiliary variables
      INTEGER    I, J, NF
      REAL*8     XLB(6), X, ALS1, ALS2, B0, B1,
     .           PI
C - Macro
      B0(I)=33.D0-2.D0*I
      B1(I)=6.D0*(153.D0-19.D0*I)/B0(I)**2
      ALS1(I,X)=12.D0*PI/(B0(I)*LOG((X/XLB(I))**2))
      ALS2(I,X)=12.D0*PI/(B0(I)*LOG((X/XLB(I))**2))
     .          *(1.D0-B1(I)*LOG(LOG((X/XLB(I))**2))
     .           /LOG((X/XLB(I))**2))
C - Statements
      PI=4.D0*DATAN(1.D0)
      IF(N.EQ.1)THEN
        DO I=1,6
          XLB(I)= XLB1(I)
        END DO
      ELSE
        DO I=1,6
          XLB(I)= XLB2(I)
        END DO
      ENDIF
      IF( Q.LT.AMC ) THEN
        NF=3
      ELSEIF( Q.LE.AMB )THEN
        NF=4
      ELSEIF( Q.LE.AMT )THEN
        NF=5
      ELSE
        NF=6
      ENDIF
      IF( N.EQ.1 ) THEN
        ALPHAS= ALS1(NF,Q)
      ELSE
        ALPHAS= ALS2(NF,Q)
      ENDIF
C -
      RETURN
      END
      SUBROUTINE ALSINI(ACC)
C-----------------------------------------------------------------------
C!
C!    INPUT:          ACC   accuracy                          (REAL*8)
C!
C!    Comments:       Cleaning of Djouadi code.
C!    Author:         A.Djouadi
C!    Cleaned by      G.Ganis         July 2, 96
C-----------------------------------------------------------------------
      IMPLICIT   NONE
C - Parameters
C - Arguments
      REAL*8     ACC
C - Common blocks
      REAL*8     XLB1, XLB2
      COMMON    /ALSLAM/ XLB1(6),XLB2(6)
      INTEGER    N0
      REAL*8     XLAMBDA, AMC, AMB, AMT
      COMMON    /ALS/ XLAMBDA, AMC, AMB, AMT, N0
C - Auxiliary variables
      INTEGER    I
      REAL*8     XLB(6), PI, XITER
C - Statements
      PI=4.D0*DATAN(1.D0)
      XLB1(1)=0D0
      XLB1(2)=0D0
      XLB2(1)=0D0
      XLB2(2)=0D0
      IF(N0.EQ.3)THEN
        XLB(3)=XLAMBDA
        XLB(4)=XLB(3)*(XLB(3)/AMC)**(2.D0/25.D0)
        XLB(5)=XLB(4)*(XLB(4)/AMB)**(2.D0/23.D0)
        XLB(6)=XLB(5)*(XLB(5)/AMT)**(2.D0/21.D0)
      ELSEIF(N0.EQ.4)THEN
        XLB(4)=XLAMBDA
        XLB(5)=XLB(4)*(XLB(4)/AMB)**(2.D0/23.D0)
        XLB(3)=XLB(4)*(XLB(4)/AMC)**(-2.D0/27.D0)
        XLB(6)=XLB(5)*(XLB(5)/AMT)**(2.D0/21.D0)
      ELSEIF(N0.EQ.5)THEN
        XLB(5)=XLAMBDA
        XLB(4)=XLB(5)*(XLB(5)/AMB)**(-2.D0/25.D0)
        XLB(3)=XLB(4)*(XLB(4)/AMC)**(-2.D0/27.D0)
        XLB(6)=XLB(5)*(XLB(5)/AMT)**(2.D0/21.D0)
      ELSEIF(N0.EQ.6)THEN
        XLB(6)=XLAMBDA
        XLB(5)=XLB(6)*(XLB(6)/AMT)**(-2.D0/23.D0)
        XLB(4)=XLB(5)*(XLB(5)/AMB)**(-2.D0/25.D0)
        XLB(3)=XLB(4)*(XLB(4)/AMC)**(-2.D0/27.D0)
      ENDIF
      DO I=1,6
        XLB1(I)=XLB(I)
      END DO
      IF(N0.EQ.3)THEN
        XLB(3)=XLAMBDA
        XLB(4)=XLB(3)*(XLB(3)/AMC)**(2.D0/25.D0)
     .              *(2.D0*DLOG(AMC/XLB(3)))**(-107.D0/1875.D0)
        XLB(4)=XITER(AMC,XLB(3),3,XLB(4),4,ACC)
        XLB(5)=XLB(4)*(XLB(4)/AMB)**(2.D0/23.D0)
     .              *(2.D0*DLOG(AMB/XLB(4)))**(-963.D0/13225.D0)
        XLB(5)=XITER(AMB,XLB(4),4,XLB(5),5,ACC)
        XLB(6)=XLB(5)*(XLB(5)/AMT)**(2.D0/21.D0)
     .             *(2.D0*DLOG(AMT/XLB(5)))**(-321.D0/3381.D0)
        XLB(6)=XITER(AMT,XLB(5),5,XLB(6),6,ACC)
      ELSEIF(N0.EQ.4)THEN
        XLB(4)=XLAMBDA
        XLB(5)=XLB(4)*(XLB(4)/AMB)**(2.D0/23.D0)
     .              *(2.D0*DLOG(AMB/XLB(4)))**(-963.D0/13225.D0)
        XLB(5)=XITER(AMB,XLB(4),4,XLB(5),5,ACC)
        XLB(3)=XLB(4)*(XLB(4)/AMC)**(-2.D0/27.D0)
     .              *(2.D0*DLOG(AMC/XLB(4)))**(107.D0/2025.D0)
        XLB(3)=XITER(AMC,XLB(4),4,XLB(3),3,ACC)
        XLB(6)=XLB(5)*(XLB(5)/AMT)**(2.D0/21.D0)
     .             *(2.D0*DLOG(AMT/XLB(5)))**(-321.D0/3381.D0)
        XLB(6)=XITER(AMT,XLB(5),5,XLB(6),6,ACC)
      ELSEIF(N0.EQ.5)THEN
        XLB(5)=XLAMBDA
        XLB(4)=XLB(5)*(XLB(5)/AMB)**(-2.D0/25.D0)
     .               *(2.D0*DLOG(AMB/XLB(5)))**(963.D0/14375.D0)
        XLB(4)=XITER(AMB,XLB(5),5,XLB(4),4,ACC)
        XLB(3)=XLB(4)*(XLB(4)/AMC)**(-2.D0/27.D0)
     .               *(2.D0*DLOG(AMC/XLB(4)))**(107.D0/2025.D0)
        XLB(3)=XITER(AMC,XLB(4),4,XLB(3),3,ACC)
        XLB(6)=XLB(5)*(XLB(5)/AMT)**(2.D0/21.D0)
     .               *(2.D0*DLOG(AMT/XLB(5)))**(-321.D0/3381.D0)
        XLB(6)=XITER(AMT,XLB(5),5,XLB(6),6,ACC)
      ELSEIF(N0.EQ.6)THEN
        XLB(6)=XLAMBDA
        XLB(5)=XLB(6)*(XLB(6)/AMT)**(-2.D0/23.D0)
     .               *(2.D0*DLOG(AMT/XLB(6)))**(321.D0/3703.D0)
        XLB(5)=XITER(AMT,XLB(6),6,XLB(5),5,ACC)
        XLB(4)=XLB(5)*(XLB(5)/AMB)**(-2.D0/25.D0)
     .               *(2.D0*DLOG(AMB/XLB(5)))**(963.D0/14375.D0)
        XLB(4)=XITER(AMB,XLB(5),5,XLB(4),4,ACC)
        XLB(3)=XLB(4)*(XLB(4)/AMC)**(-2.D0/27.D0)
     .               *(2.D0*DLOG(AMC/XLB(4)))**(107.D0/2025.D0)
        XLB(3)=XITER(AMC,XLB(4),4,XLB(3),3,ACC)
      ENDIF
      DO I=1,6
       XLB2(I)=XLB(I)
      END DO
C -
      RETURN
      END
      FUNCTION HCCONV( SS, MH, GH )
C-----------------------------------------------------------------------
C!    Calculates convolution of H+H- kinematical factors with
C!    2 Breit-Wigners
C!
C!    Input:  SS  (REAL*8)     center_of_mass_energy squared (GeV**2)
C!            MH  (REAL*8)     H+ mass                       (GeV/c**2)
C!            GH  (REAL*8)     H+ width                      (GeV/c**2)
C!
C!    Output: HCCONV (REAL*8)  convolution factor
C!
C!    Gerardo Ganis              3 May 1996
C-----------------------------------------------------------------------
      IMPLICIT   NONE
      REAL*8     HCCONV
C - Arguments
      REAL*8     SS, MH, GH
C - Working common (share the hA routines)
      REAL*8 s,am1,am2,w1,w2
      COMMON /BWC/ s,am1,am2,w1,w2
C - Auxiliary variables
      REAL*8     X2MIN, X2MAX, X(2)
C - Constants
      REAL*8     PI, piby2
      PARAMETER( PI= 3.141592653589793238, piby2=pi/2D0 )
C - External subroutine
      EXTERNAL   fsub2
C - External functions
      REAL*8     DGMLT2
      EXTERNAL   DGMLT2
C
C - Fill the common block
C
      s      = ss
      am1    = mh
      am2    = mh
      w1     = gh
      w2     = gh
C
C - Calculate the integral
C
      X2MIN  =  -DATAN2(am2,w2)
      X2MAX  =   DATAN2(s-am2**2,am2*w2)
      HCCONV= DGMLT2( fsub2, x2min, x2max, 1, 6, x)
     .       / (piby2+DATAN2(am1,w1))
     .       / (piby2+DATAN2(am2,w2))
C
      RETURN
      END
      SUBROUTINE HCSETJ( DBG )
C-----------------------------------------------------------------------
C! Define decay channels and branching fractions for Charged Higgs ( KC=
C! in JETSET framework
C!
C!    Input:           none
C!    Output:          in JETSET common blocks
C!
C!    Called by ASKUSI
C!
C!    Gerardo Ganis              3 May 1996
C-----------------------------------------------------------------------
      IMPLICIT   NONE
C - Parameters & Constants
      REAL*8     HBAR
      PARAMETER( HBAR= 6.583173D-25 )
      INTEGER    IHC
      PARAMETER( IHC= 37 )
C - Arguments
      INTEGER    DBG
C - Common blocks
      INTEGER    IHCD
      REAL*8     MH, TBT, TAF
      COMMON /HCCOMM/ MH(4), TBT, TAF, IHCD(2)
      INTEGER    HCCON, NSCH, NCH, NDCH, NMXC
      PARAMETER( NCH= 49, NDCH= 19, NMXC= 9 )
      REAL*8     HCCBR1, HCCBR2, TAUHP
      COMMON /HCDEFF/ NSCH(NDCH), HCCON(NDCH),
     .                HCCBR1(0:NDCH), HCCBR2(NDCH,NMXC), TAUHP
      CHARACTER*17 HCCHAN
      COMMON /HCNAME/ HCCHAN(NDCH)
      INTEGER    UPMAX
      REAL*8     MEMAX, THRES
      COMMON /HCMAXX/ MEMAX(NCH), THRES(NCH), UPMAX(NCH)
      INTEGER    HCDSTA
      COMMON /HCSTAT/ HCDSTA(0:NCH)
C-----------------------------------------------------------------------
C!    HCCHAN           channel names
C!    HCCON            requested decay ( 1= ON, 0= OFF )
C!    HCCBR1           cumulative BR on first channel classif. (data car
C!    HCCBR2           cumulative BR on subchannel classif. (internal)
C!    NSCH             number of subchannels per channel
C!    TAUHP            lifetime in seconds
C-----------------------------------------------------------------------
C - JETSET parameter commons
      INTEGER    KCHG
      REAL*4     PMAS, PARF, VCKM
      COMMON /LUDAT2/ KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      INTEGER    MDCY, MDME, KFDP
      REAL*4     BRAT
      COMMON /LUDAT3/ MDCY(500,3),MDME(2000,2),BRAT(2000),
     .                KFDP(2000,5)
C - Auxiliary variables
      REAL*8     BR( 0:NCH ), WD( 0:NCH ), FRAC
      INTEGER    KC, ILD, ILDMAX, K, KUP, KDW, I
      CHARACTER*1 STAR
C - Data statements
C - Macros
C - Statements
C - Get Branching Fractions
      CALL HCBRAT(BR,WD)
C - Lifetime in seconds
      TAUHP= 0.D0
      IF( WD(0).GT.0.D0 ) TAUHP= HBAR / WD(0)
C - Set parameters
      PMAS(IHC,1)= SNGL(MH(4))           ! Mass ...
      PMAS(IHC,2)= SNGL(WD(0))        ! Width ...
      PMAS(IHC,3)= SNGL(MIN(MH(4),10.*WD(0))) ! Deviations from mass ...
      MDCY(IHC,1)= 1                  ! Allow for H+ decay ...
C - Get first free position in MDME, BRAT, KFDP
      ILDMAX= 0
      DO KC=1,500
        ILD= MDCY(KC,2) + MDCY(KC,3) - 1
        IF( ILD.GT.ILDMAX ) ILDMAX= ILD
      END DO
      IF( (2000-ILDMAX).LT.NCH ) THEN
        WRITE(*,*) '_HCSETJ_ - Not enough space to define decays'
        RETURN
      END IF
C - Define entry point in the decay table
      MDCY(IHC,2)= ILDMAX+1
C - Define total number of decays defined
      MDCY(IHC,3)= NCH
C _ Enable all the decays
      K= 0
      DO I=MDCY(IHC,2),MDCY(IHC,2)+MDCY(IHC,3)-1
        K=K+1
        MDME( I, 1)= 1
C - Decays into ud
        IF( K.LE.3 ) THEN
          MDME( I, 2)= 0     ! space phase ...
          KUP=   K*2 + 10
          KDW= -(K*2 +  9)
          KFDP( I, 1)= KUP
          KFDP( I, 2)= KDW
          KFDP( I, 3)= 0
          THRES(K)= DBLE(PMAS(KUP,1)+PMAS(ABS(KDW),1))
        ELSE IF( K.LE.9 ) THEN
          MDME( I, 2)= 0    ! the same, but shower allowed ...
          KUP= ((K-1)/3)*2
          KDW= K - ((K-1)/3)*3
          KDW=-(KDW*2-1)
          KFDP( I, 1)= KUP
          KFDP( I, 2)= KDW
          KFDP( I, 3)= 0
          THRES(K)= DBLE(PMAS(KUP,1)+PMAS(ABS(KDW),1))
        ELSE IF( K.LE.12 ) THEN
          MDME( I, 2)= 0    ! the same, but shower allowed ...
          KDW= K - ((K-1)/3)*3
          KDW=-(KDW*2-1)
          KFDP( I, 1)= 24                  ! W+
          KFDP( I, 2)=  5                  ! b
          KFDP( I, 3)= KDW                 ! d bar
          KFDP( I, 4)= 0
          THRES(K)= DBLE(PMAS(24,1)+PMAS(5,1)+PMAS(ABS(KDW),1))
C - Decays into hud
        ELSE IF( K.LE.15 ) THEN
          MDME( I, 2)= 0     ! space phase ...
          KUP=   (K-12)*2 + 10
          KDW= -((K-12)*2 +  9)
          KFDP( I, 1)= 25
          KFDP( I, 2)= KUP
          KFDP( I, 3)= KDW
          KFDP( I, 4)= 0
          THRES(K)= DBLE(PMAS(25,1)+PMAS(KUP,1)+PMAS(ABS(KDW),1))
        ELSE IF( K.LE.24 ) THEN
          MDME( I, 2)= 0    ! the same, but shower allowed ...
          KUP= ((K-13)/3)*2
          KDW= K - ((K-1)/3)*3
          KDW=-(KDW*2-1)
          KFDP( I, 1)= 25
          KFDP( I, 2)= KUP
          KFDP( I, 3)= KDW
          KFDP( I, 4)= 0
          THRES(K)= DBLE(PMAS(25,1)+PMAS(KUP,1)+PMAS(ABS(KDW),1))
C - Decays into Aud
        ELSE IF( K.LE.27 ) THEN
          MDME( I, 2)= 0     ! space phase ...
          KUP=   (K-24)*2 + 10
          KDW= -((K-24)*2 +  9)
          KFDP( I, 1)= 36
          KFDP( I, 2)= KUP
          KFDP( I, 3)= KDW
          KFDP( I, 4)= 0
          THRES(K)= DBLE(PMAS(36,1)+PMAS(KUP,1)+PMAS(ABS(KDW),1))
        ELSE IF( K.LE.36 ) THEN
          MDME( I, 2)= 0    ! the same, but shower allowed ...
          KFDP( I, 1)= 36
          KUP= ((K-25)/3)*2
          KDW= K - ((K-1)/3)*3
          KDW=-(KDW*2-1)
          KFDP( I, 2)=  KUP
          KFDP( I, 3)=  KDW
          KFDP( I, 4)= 0
          THRES(K)= DBLE(PMAS(36,1)+PMAS(KUP,1)+PMAS(ABS(KDW),1))
C - Decays into Hud
        ELSE IF( K.LE.39 ) THEN
          MDME( I, 2)= 0     ! space phase ...
          KUP=   (K-36)*2 + 10
          KDW= -((K-36)*2 +  9)
          KFDP( I, 1)= 35
          KFDP( I, 2)= KUP
          KFDP( I, 3)= KDW
          KFDP( I, 4)= 0
          THRES(K)= DBLE(PMAS(35,1)+PMAS(KUP,1)+PMAS(ABS(KDW),1))
        ELSE IF( K.LE.48 ) THEN
          MDME( I, 2)= 0    ! the same, but shower allowed ...
          KFDP( I, 1)= 35
          KUP= ((K-37)/3)*2
          KDW= K - ((K-1)/3)*3
          KDW=-(KDW*2-1)
          KFDP( I, 2)=  KUP
          KFDP( I, 3)=  KDW
          KFDP( I, 4)= 0
          THRES(K)= DBLE(PMAS(35,1)+PMAS(KUP,1)+PMAS(ABS(KDW),1))
        ELSE IF( K.EQ.NCH ) THEN
          MDME( I, 2)= 0    ! the same, but shower allowed ...
          KFDP( I, 1)= 51   ! chi0
          KFDP( I, 2)= 55   ! chi+
          KFDP( I, 3)= 0
          THRES(K)= DBLE(PMAS(51,1)+PMAS(55,1))
        END IF
        BRAT(I)= BR(K)
      END DO
C - Calculate cumulative BR
      CALL HCCUBR(BR)
C - Print out now
      IF( DBG.GE.0 ) THEN
        WRITE(*,90) MH(4),WD(0)
        DO I=1,NDCH
          STAR= ' '
          IF( HCCON(I).EQ.1 ) STAR= '*'
          IF( I.EQ.1 ) THEN
            FRAC= HCCBR1(I)
          ELSE
            FRAC= HCCBR1(I)-HCCBR1(I-1)
          END IF
          WRITE(*,91) HCCHAN(I), FRAC*100.D0, STAR
        END DO
        WRITE(*,92)
      END IF
 90   FORMAT(/50('-')//
     .  1X,'The following branching ratios have been computed :',/,
     .  1X,'    H+ mass               : ',F9.4,' GeV/c**2',/,
     .  1X,'    Total decay width     : ',F9.4,' GeV',//)
 91   FORMAT(1X,'Channel ',A17,' --- BR = ',F10.5,' % (',A1,')')
 92   FORMAT(//)
      RETURN
      END

      SUBROUTINE HCBRAT(BR,WD)
C-----------------------------------------------------------------------
C! Calculate H+ branching fractions and widths
C!
C!    Input:     common blocks /HCCOMM/ and /HCMAXX/
C!    Output:    BR(i)    branching fraction
C!               WD(i)    width in GeV
C!               i=0,36   WD(0)= total width, BR(0)= 1.
C!
C!    Called by HCSETJ
C!
C!    Gerardo Ganis              3 May 1996
C-----------------------------------------------------------------------
      IMPLICIT   NONE
C - Parameters & Constants
C - Common blocks
      INTEGER    IHCD
      REAL*8     MH, TBT, TAF
      COMMON /HCCOMM/ MH(4), TBT, TAF, IHCD(2)
      INTEGER    HCCON, NSCH, NCH, NDCH, NMXC
      PARAMETER( NCH= 49, NDCH= 19, NMXC= 9 )
      REAL*8     HCCBR1, HCCBR2, TAUHP
      COMMON /HCDEFF/ NSCH(NDCH), HCCON(NDCH),
     .                HCCBR1(0:NDCH), HCCBR2(NDCH,NMXC), TAUHP
      CHARACTER*17 HCCHAN
      COMMON /HCNAME/ HCCHAN(NDCH)
      INTEGER    UPMAX
      REAL*8     MEMAX, THRES
      COMMON /HCMAXX/ MEMAX(NCH), THRES(NCH), UPMAX(NCH)
      INTEGER    HCDSTA
      COMMON /HCSTAT/ HCDSTA(0:NCH)
C-----------------------------------------------------------------------
C!    HCCHAN           channel names
C!    HCCON            requested decay ( 1= ON, 0= OFF )
C!    HCCBR1           cumulative BR on first channel classif. (data car
C!    HCCBR2           cumulative BR on subchannel classif. (internal)
C!    NSCH             number of subchannels per channel
C!    TAUHP            lifetime in seconds
C-----------------------------------------------------------------------
C - Arguments
      REAL*8     BR( 0:NCH ), WD( 0:NCH )
C - Auxiliary variables
      INTEGER     QCDL, QCDQ,
     .            I, J, N
      REAL*8      MHC, MH0, SA, CA, SB, CB, SAB, CAB, GRED,
     .            HUDWDT, HHUDWD
      EXTERNAL    HUDWDT, HHUDWD
C - Data statements
      DATA        QCDL /0/, QCDQ /1/
C - Macros
C - Statements
      MHC= MH(4)
      SA= TAF  /SQRT(1.D0+TAF**2)
      CA= 1.D0/SQRT(1.D0+TAF**2)
      SB= TBT  /SQRT(1.D0+TBT**2)
      CB= 1.D0/SQRT(1.D0+TBT**2)
      SAB= SA*CB-CA*SB
      CAB= SA*SB+CA*CB
C - Initialize
      DO I=0,NCH
        BR(I)= 0.D0
        WD(I)= 0.D0
        IF( I.GT.0 ) UPMAX(I)= 0
      END DO
      N= 0
C - 1-3 nu lepton
      DO I=12,16,2
        N=N+1
        WD(N)= HUDWDT( MHC, I, I-1, TBT, QCDL )
      END DO
C - 4-9 up down
      DO I=2,4,2
        DO J=1,5,2
          N= N+1
          WD(N)= HUDWDT( MHC, I, J, TBT, QCDQ )
        END DO
      END DO
C - 10-12 W b d bar ( not yet done )
      DO I=1,5,2
        N=N+1
        WD(N)= 0.D0
      END DO
C - Decays in h f f'
      MH0= MH(2)
      GRED= CAB
      DO I=12,16,2
        N= N+1
        WD(N)= HHUDWD(MHC,MH0,GRED,I,I-1,MEMAX(N))
      END DO
      DO I=2,6,2
        DO J=1,5,2
          N= N+1
          WD(N)= HHUDWD(MHC,MH0,GRED,I,J,MEMAX(N))
        END DO
      END DO
C - Decays in A f f'
      MH0= MH(3)
      GRED= 1.D0
      DO I=12,16,2
        N= N+1
        WD(N)= HHUDWD(MHC,MH0,GRED,I,I-1,MEMAX(N))
      END DO
      DO I=2,6,2
        DO J=1,5,2
          N= N+1
          WD(N)= HHUDWD(MHC,MH0,GRED,I,J,MEMAX(N))
        END DO
      END DO
C - Decays in H f f'
      MH0= MH(1)
      GRED= SAB
      DO I=12,16,2
        N= N+1
        WD(N)= HHUDWD(MHC,MH0,GRED,I,I-1,MEMAX(N))
      END DO
      DO I=2,6,2
        DO J=1,5,2
          N= N+1
          WD(N)= HHUDWD(MHC,MH0,GRED,I,J,MEMAX(N))
        END DO
      END DO
C - 49 chi0 chi+ (not done yet)
      WD(49)= 0.D0
C - Sum in index 0
      DO I=0,NCH
        WD(0)= WD(I) + WD(0)
      END DO
      IF( WD(0).NE.0 ) THEN
        DO I=0,NCH
          BR(I)= WD(I) / WD(0)
        END DO
      END IF
C
      RETURN
      END

      FUNCTION HUDWDT( MH, IUP, IDW, TBT, OPT)
C-----------------------------------------------------------------------
C! Implementation of H+->ud decay width accordingly to
C! eq.441, pag.405, LEP2 workshop proceed.
C! Modified for finite masses ...
C!
C!    Input:      MH       Charged Higgs mass in GeV/c**2
C!                IUP      JETSET code for up-like fermion
C!                IDW      JETSET code for down-like antifermion
C!                TBT      tan_beta, ratio between vev's (v2/v1)
C!                OPT      QCD radiative correction option (quark only)
C!                         =0  no correction (tree-level)
C!                         =1  tree-level + running quark masses
C!                         ( not available at present )
C!
C!    Output:     HUDWDT   width in GeV
C!
C!    Called by HCBRAT
C!
C!    Gerardo Ganis             21 May 1996
C-----------------------------------------------------------------------
      IMPLICIT    NONE
      REAL*8      HUDWDT
C - Parameters & Constants
      REAL*8      PI
      PARAMETER(  PI= 3.141592653589793238D0 )
C - Arguments
      REAL*8      MH, TBT
      INTEGER     IUP, IDW, OPT
C - JETSET parameter commons
      INTEGER     MSTU, MSTJ
      REAL*4      PARU, PARJ
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER     KCHG
      REAL*4      PMAS, PARF, VCKM
      COMMON /LUDAT2/ KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
C - Auxiliary variables
      REAL*8      GF, MW, SW2, HU, HD, RMU, RMD, MU0, MD0,
     .            GETCKM, ALFS, PHASE2, QCD, MMU, MMD, RQMASS,
     .            ALPHAS
      REAL*4      ULALEM, ULALPS
      INTEGER     I, NC
      LOGICAL     QUARK, FIRST
      EXTERNAL    PHASE2, GETCKM
C - Data statements
      DATA        FIRST /.TRUE./
C - Macros
      QUARK(I)=   I.GE.1 .AND. I.LE.6
C - Statements
      MW = DBLE(PMAS(24,1))
      SW2= DBLE(PARU(102))
      GF = DBLE(ULALEM(SNGL(MH**2)))*PI/SQRT(2.D0)/MW**2/SW2
C      ALFS = DBLE(ULALPS(SNGL(MH**2)))
C - Define parameters
      MU0= DBLE(PMAS(IUP,1))       ! Pole mass
      MD0= DBLE(PMAS(IDW,1))       ! Pole mass
      MMU= MU0
      MMD= MD0
      NC = 1
      QCD= 0.D0
      ALFS= 0.D0
      IF( QUARK(IUP) ) THEN
C ----------------------------------------------------------------------
C First ordes QDC corrections SWITCH OFF waiting for more deep understan
C (210596)
C        RMU= DBLE(RUNMAS( SNGL(MU0), 4, FACDUM ))
C        RMD= DBLE(RUNMAS( SNGL(MD0), 4, FACDUM ))
C        MMU= MU0*SQRT(RMU)
C        MMD= MD0*SQRT(RMD)
C ----------------------------------------------------------------------
        IF( OPT.EQ.1 ) THEN
C - Initialize and use Djouadi stuff ...
          IF( FIRST ) CALL RAMINI
          MMU= RQMASS(MH,IUP)
          MMD= RQMASS(MH,IDW)
          QCD= 1.D0
          ALFS = ALPHAS(MH,2)
          FIRST= .FALSE.
        END IF
        NC = 3
      END IF
C - Check if kinematically allowed
      HUDWDT= 0.D0
      IF( MH.LT.(MU0+MD0) ) RETURN
C - Define coupling constants (with renormalized masses, if quarks)
      HU= MMU/TBT
      HD= MMD*TBT
C - Total width
      HUDWDT= (NC*GF*MH)*2.D0/SQRT(2.D0) *
     .        ( (HU**2+HD**2)*(1.D0-(MMU**2+MMD**2)/MH**2) -
     .          4.D0*(MMU*MMD/MH)**2 ) *
     .          PHASE2(MH,MMU,MMD) * GETCKM(IUP,IDW)**2 *
     .        ( 1.D0 + 17.D0/3.D0*ALFS/PI*QCD )
C
      RETURN
      END

      FUNCTION PHASE2(M,M1,M2)
C-----------------------------------------------------------------------
C! Two-body phase space ( M -> M1 + M2 )
C!
C!    Input:      M          mass of the decaying particle
C!                M1,M2      masses of decay products
C!
C!    Output      PHASE2     beta(m1,m2)/8/pi
C!
C!    Gerardo Ganis  --  12 May 1996
C-----------------------------------------------------------------------
      IMPLICIT   NONE
      REAL*8     PHASE2
C - Parameters & Constants
      REAL*8      PI
      PARAMETER(  PI= 3.141592653589793238D0 )
C - Arguments
      REAL*8     M, M1, M2
C - Common blocks
C - Auxiliary variables
      REAL*8     B2
C - Data statements
C - Macros
C - Statements
      B2= 1.D0-2.D0*(M1**2+M2**2)/M**2+((M1**2-M2**2)/M**2)**2
C - Two body phase space
      PHASE2= SQRT(MAX(0.D0,B2)) / (8.D0*PI)
C - That's it
      RETURN
      END

      FUNCTION GETCKM(I,J)
C-----------------------------------------------------------------------
C! Get VCKM matrix element for flavours I,J
C!
C!    Input:          I   JETSET code for up-like flavour
C!                    J   JETSET code for  down-like flavour
C!
C!    Output:         GETCKM, VCKM element
C!                    1. if I,J is not consistent
C!
C!    Gerardo Ganis              3 May 1996
C-----------------------------------------------------------------------
      IMPLICIT   NONE
      REAL*8     GETCKM
C - Parameters & Constants
C - Arguments
      INTEGER    I, J
C - Common blocks
C - JETSET parameter commons
      INTEGER    KCHG
      REAL*4     PMAS, PARF, VCKM
      COMMON /LUDAT2/ KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
C - Auxiliary variables
      INTEGER    IU, ID, L, K
      LOGICAL    UP, DW
C - Data statements
C - Macros
      UP(K)=     ABS(K).EQ.2 .OR. ABS(K).EQ.4 .OR. ABS(K).EQ.6
      DW(K)=     ABS(K).EQ.1 .OR. ABS(K).EQ.3 .OR. ABS(K).EQ.5
C - Statements
      GETCKM= 1.D0
C - Check consistency
      IF( .NOT.UP(I) .AND. .NOT.DW(I) ) RETURN
      IF( .NOT.UP(J) .AND. .NOT.DW(J) ) RETURN
      IF( UP(I) .AND. UP(J) ) RETURN
      IF( DW(I) .AND. DW(J) ) RETURN
C - Get matrix index
      IF( UP(I) ) THEN
        IU= I/2
        ID= (J+1)/2
      ELSE
        IU= J/2
        ID= (I+1)/2
      END IF
C - Get matrix element
      GETCKM= DBLE(VCKM(IU,ID))
C
      RETURN
      END

      SUBROUTINE HCDECY(ID,CHAN,STATUS)
C-----------------------------------------------------------------------
C! Decay H+ particle in /LUJETS/ position ID through channel CHAN
C!
C!    Input:          ID     /LUJETS/ position of decaying particle
C!                    CHAN   decay channel
C!    Output:         in /LUJETS/
C!                    STATUS     =0     successful
C!                               =1     decay not kinematically allowed
C!                               =2,3   decays not correctly defined in
C!                                      JETSET commons
C!
C!    Called by ASKUSE
C!
C!    Gerardo Ganis              3 May 1996
C-----------------------------------------------------------------------
      IMPLICIT   NONE
C - Parameters & Constants
C - Arguments
      INTEGER    ID, CHAN, STATUS
C - Common blocks
      INTEGER    HCCON, NSCH, NCH, NDCH, NMXC
      PARAMETER( NCH= 49, NDCH= 19, NMXC= 9 )
      REAL*8     HCCBR1, HCCBR2, TAUHP
      COMMON /HCDEFF/ NSCH(NDCH), HCCON(NDCH),
     .                HCCBR1(0:NDCH), HCCBR2(NDCH,NMXC), TAUHP
      CHARACTER*17 HCCHAN
      COMMON /HCNAME/ HCCHAN(NDCH)
      INTEGER    UPMAX
      REAL*8     MEMAX, THRES
      COMMON /HCMAXX/ MEMAX(NCH), THRES(NCH), UPMAX(NCH)
      INTEGER    HCDSTA
      COMMON /HCSTAT/ HCDSTA(0:NCH)
C-----------------------------------------------------------------------
C!    HCCHAN           channel names
C!    HCCON            requested decay ( 1= ON, 0= OFF )
C!    HCCBR1           cumulative BR on first channel classif. (data car
C!    HCCBR2           cumulative BR on subchannel classif. (internal)
C!    NSCH             number of subchannels per channel
C!    TAUHP            lifetime in seconds
C-----------------------------------------------------------------------
C - JETSET common for event record
      INTEGER    NJMAX, NTMAX
      PARAMETER( NJMAX= 4000, NTMAX= 5)
      INTEGER    NJET, KJET
      REAL*4     PJET, VJET
      COMMON /LUJETS/ NJET, KJET(NJMAX,NTMAX), PJET(NJMAX,NTMAX),
     .                      VJET(NJMAX,NTMAX)
C - JETSET parameter commons
      INTEGER     MSTU, MSTJ
      REAL*4      PARU, PARJ
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER     KCHG
      REAL*4      PMAS, PARF, VCKM
      COMMON /LUDAT2/ KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      INTEGER     MDCY, MDME, KFDP
      REAL*4      BRAT
      COMMON /LUDAT3/ MDCY(500,3),MDME(2000,2),BRAT(2000),
     .                KFDP(2000,5)
C - Auxiliary variables
      INTEGER     I, J, K, L, MDME1OLD(NCH), NJOIN, IJOIN(2),
     .            KDC, IFC, NDC,
     .            MDCY25, MDCY36, MDCY35, MDCY37
      REAL*8      MHC
      REAL*4      MASS, QMX
      LOGICAL     D2B, D2BQ, D3BQ
C - Data statements
C - Macros
      D2B(I)=     I.GE.1 .AND. I.LE.12
      D2BQ(I)=    I.GE.4 .AND. I.LE.12
      D3BQ(I)=   (I.GE.16 .AND. I.LE.24) .OR.
     .           (I.GE.28 .AND. I.LE.36)
      MASS(I,J)= SQRT((PJET(I,4)+PJET(J,4))**2 -
     .                (PJET(I,1)+PJET(J,1))**2 -
     .                (PJET(I,2)+PJET(J,2))**2 -
     .                (PJET(I,3)+PJET(J,3))**2 )
C - Statements
      STATUS= 0
C - Get pointers to tables ...
      KDC= ABS(KJET(ID,2))
      MHC= DBLE(PJET(ID,5))
      IFC= MDCY(KDC,2)
      NDC= MDCY(KDC,3)
C - Check whether the channel is kinematically allowed
      IF( MHC.LT.THRES(CHAN) ) THEN
        STATUS= 1
        RETURN
      END IF
C - Check whether decay channel is defined
      IF( IFC.EQ.0 .OR. CHAN.GT.NDC .OR. CHAN.LT.1 ) THEN
        STATUS= 2
        RETURN
      END IF
      IF( NDC.NE.NCH ) THEN
        STATUS= 3
        RETURN
      END IF
C - Inhibit all decays except the one wanted
      K=0
      DO I=IFC,IFC+NDC-1
        K= K+1
C - Keep track of the present status
        MDME1OLD(K)= MDME(I,1)
        IF( K.NE.CHAN ) THEN
          MDME(I,1)= -1
        ELSE
          MDME(I,1)=  1
        END IF
      END DO
C - Inhibit H, h and A decays
      MDCY25= MDCY(25,1)
      MDCY35= MDCY(35,1)
      MDCY36= MDCY(36,1)
      MDCY(25,1)= 0
      MDCY(35,1)= 0
      MDCY(36,1)= 0
C - Action now depends on the decay chosen ...
      IF( D2B(CHAN) ) THEN
C - Decay it
        CALL LUDECY(ID)
C - 2-body decay ... if quarks prepare fragmentation ...
        IF( D2BQ(CHAN) ) THEN
          NJOIN= 2
          IJOIN(1)= KJET(ID,4)
          IJOIN(2)= KJET(ID,5)
          QMX= MASS(IJOIN(1),IJOIN(2))
          CALL LUJOIN(NJOIN,IJOIN)
          CALL LUSHOW(IJOIN(1),IJOIN(2),QMX)
C - ... fragmentate ...
          MDCY37= MDCY(37,1)              ! Inhibit other H+ decay
          MDCY(37,1)= 0
          CALL LUEXEC
          MDCY(37,1)= MDCY37
        END IF
      ELSE
C - 3-body decay with neutral Higgs
        CALL GETHUD( ID, CHAN )
        IF( D3BQ(CHAN) ) THEN
          NJOIN= 2
          IJOIN(1)= KJET(ID,4)+1
          IJOIN(2)= KJET(ID,5)
          QMX= MASS(IJOIN(1),IJOIN(2))
          CALL LUJOIN(NJOIN,IJOIN)
          CALL LUSHOW(IJOIN(1),IJOIN(2),QMX)
C - ... fragmentate ...
          MDCY37= MDCY(37,1)             ! Inhibit other H+ decay
          MDCY(37,1)= 0
          CALL LUEXEC
          MDCY(37,1)= MDCY37
        END IF
      END IF
C - Restore old status
      K= 0
      DO I=IFC,IFC+NDC-1
        K= K+1
        MDME(I,1)= MDME1OLD(K)
      END DO
      MDCY(25,1)= MDCY25
      MDCY(35,1)= MDCY35
      MDCY(36,1)= MDCY36
C
      RETURN
      END

      SUBROUTINE GETHUD( ID, CHAN)
C-----------------------------------------------------------------------
C! Decay H+ in neutral higgs + up-down pair. Get phase space from JETSET
C! and choose accordingly to the matrix element with a Hit_Or_Miss metho
C!
C!    Input:            ID    /LUJETS/ position for H+
C!                      CHAN  decay channel
C!    Output:           in /LUJETS/
C!
C!    Called by HCDECY
C!
C!    Gerardo Ganis              3 May 1996
C-----------------------------------------------------------------------
      IMPLICIT   NONE
C - Parameters & Constants
C - Arguments
      INTEGER    ID, CHAN
C - Common blocks
      INTEGER    HCCON, NSCH, NCH, NDCH, NMXC
      PARAMETER( NCH= 49, NDCH= 19, NMXC= 9 )
      REAL*8     HCCBR1, HCCBR2, TAUHP
      COMMON /HCDEFF/ NSCH(NDCH), HCCON(NDCH),
     .                HCCBR1(0:NDCH), HCCBR2(NDCH,NMXC), TAUHP
      CHARACTER*17 HCCHAN
      COMMON /HCNAME/ HCCHAN(NDCH)
      INTEGER    UPMAX
      REAL*8     MEMAX, THRES
      COMMON /HCMAXX/ MEMAX(NCH), THRES(NCH), UPMAX(NCH)
      INTEGER    HCDSTA
      COMMON /HCSTAT/ HCDSTA(0:NCH)
C-----------------------------------------------------------------------
C!    HCCHAN           channel names
C!    HCCON            requested decay ( 1= ON, 0= OFF )
C!    HCCBR1           cumulative BR on first channel classif. (data car
C!    HCCBR2           cumulative BR on subchannel classif. (internal)
C!    NSCH             number of subchannels per channel
C!    TAUHP            lifetime in seconds
C-----------------------------------------------------------------------
C - JETSET common for event record
      INTEGER    NJMAX, NTMAX
      PARAMETER( NJMAX= 4000, NTMAX= 5)
      INTEGER    NJET, KJET
      REAL*4     PJET, VJET
      COMMON /LUJETS/ NJET, KJET(NJMAX,NTMAX), PJET(NJMAX,NTMAX),
     .                      VJET(NJMAX,NTMAX)
C - Auxiliary variables
      REAL*8     M(4), Q(4,4), ME, RAN(1),
     .           HHUDME
      INTEGER    I, J, IP(4), KJOLD
      LOGICAL    OK
      EXTERNAL   HHUDME
C - Data statements
C - Macros
C - Statements
      OK= .FALSE.
      KJOLD= KJET(ID,1)
      DO WHILE( .NOT.OK )
C - Decay the particle accordingly to phase space ...
        CALL LUDECY( ID )
C - fill relevant information ...
        IP(1)= KJET(ID,4)
        IP(2)= KJET(ID,4)+1
        IP(3)= KJET(ID,4)+2
        IP(4)= ID
        DO I=1,4
          M(I)= DBLE(PJET(IP(I),5))
        END DO
        DO I=1,4
          DO J=I,4
            Q(I,J)= DBLE( PJET(IP(I),4)*PJET(IP(J),4) -
     .                    PJET(IP(I),1)*PJET(IP(J),1) -
     .                    PJET(IP(I),2)*PJET(IP(J),2) -
     .                    PJET(IP(I),3)*PJET(IP(J),3) )
            IF( I.NE.J ) Q(J,I)= Q(I,J)
          END DO
        END DO
C - Get matrix element
        ME= HHUDME(M,Q)
        IF( ME.GT.MEMAX(CHAN) ) THEN
          MEMAX(CHAN)= ME
          UPMAX(CHAN)= UPMAX(CHAN)+1
        END IF
C - Hit or miss
        CALL HCRNDM(RAN,1)
        IF( RAN(1).LT. ME/MEMAX(CHAN) ) THEN
          OK= .TRUE.
        ELSE
          NJET= NJET-3
          KJET(ID,1)= KJOLD
          KJET(ID,4)= 0
          KJET(ID,5)= 0
        END IF
      END DO

      RETURN
      END

      REAL*8 FUNCTION HHUDWD(MH,MH0,GRED,IU,ID)
C-----------------------------------------------------------------------
C! Calculate the width for the decay H+ -> H0 u d
C!
C!    Input:     MH              H+ mass in GeV
C!               MH0             H0 mass in GeV
C!               GRED            reduced coupling
C!                               sin(alpha-beta)    for H0= H
C!                               cos(alpha-beta)    for H0= h
C!                               1.                 for H0= A
C!               IU              JETSET code for up-like fermion
C!               ID              JETSET code for down-like antifermion
C!    Output:    HHUDWD          Width in GeV
C!
C!    Called by HCBRAT
C!
C!    Gerardo Ganis              3 May 1996
C-----------------------------------------------------------------------
      IMPLICIT   NONE
C - Parameters & Constants
      INTEGER    NV
      PARAMETER( NV= 4 )
      REAL*8     PI
      PARAMETER( PI= 3.141592653589793238 )
C - Arguments
      INTEGER    IU, ID
      REAL*8     MH, MH0, GRED
C - Common blocks
      REAL*4     IPAR, RPAR
      COMMON /PARPS3/ IPAR(6), RPAR(6)
C - JETSET parameter commons
      INTEGER    KCHG
      REAL*4     PMAS, PARF, VCKM
      COMMON /LUDAT2/ KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
C - Auxiliary variables
      INTEGER    I, J, K, L, NG
      REAL*8     M(NV), ME, RAN(4), QF, PHASE,
     .           DE2, DE3, E2, E3, CT23,
     .           XMIN, XMAX,
     .           DE, X, Y, Z, W,
     .           GETCKM, DGMLT2
      LOGICAL    QUARK
      EXTERNAL   GETCKM, HHUDWDX2, DGMLT2
C - Data statements
C - Macros
      DE(X,Y,Z,W)= X*(1.D0+(Y**2-Z**2-W**2)/X**2)/2.-Y
      QUARK(I)=  ABS(I).GE.1 .AND. ABS(I).LE.6
C - Statements
      M(1)= MH0
      M(2)= DBLE(PMAS(IU,1))
      M(3)= DBLE(PMAS(ID,1))
      M(4)= MH
C - Check if the decay is possible
      HHUDWD= 0.D0
      IF( M(4).LT.(M(1)+M(2)+M(3)) ) RETURN
C - Energy ranges .
      DE2 = DE(M(4),M(2),M(1),M(3))
      DE3 = DE(M(4),M(3),M(1),M(2))
C - Fill /PARPS3/
      RPAR(1)= M(4)
      RPAR(2)= M(1)
      RPAR(3)= M(2)
      RPAR(4)= M(3)
      RPAR(5)= DE2
      RPAR(6)= DE3
C - Numerical integration
      XMIN= M(3)
      XMAX= M(3)+DE3
      HHUDWD= 4*PI*DGMLT2(HHUDWDX2,XMIN,XMAX,3,6,X)
C - Multiply by CKM coupling and colour ( 1 for leptons )
      QF= GETCKM(IU,ID)**2
      IF( QUARK(IU) ) QF= QF*3.D0
C - Phase space factor
      PHASE= 1.D0 /( 8.D0 * ((2.D0*PI)**4) )
      HHUDWD= HHUDWD * QF * PHASE / (2.D0*M(4))
C
      RETURN
      END
      SUBROUTINE HHUDWDX1(M,U1,F1,X)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      IMPLICIT   NONE
C - Parameters & Constants
      INTEGER    NV
      PARAMETER( NV= 4 )
C - Arguments
      INTEGER    M
      REAL*8     U1(*),F1(*),X(2)
C - Common blocks
      REAL*4     IPAR, RPAR
      COMMON /PARPS3/ IPAR(6), RPAR(6)
C - Auxiliary variables
      INTEGER    L, I, J
      REAL*8     XMIN, XMAX, XG, X1, X2, X3, Q(4,4), E2, E3,
     .           MM(4), CT23,
     .           HHUDME
      LOGICAL    HCPS3
      EXTERNAL   HCPS3, HHUDME
C - Data statements
C - Macros
C - Statements
      MM(4) = RPAR(1)
      MM(1) = RPAR(2)
      MM(2) = RPAR(3)
      MM(3) = RPAR(4)
      DO L=1,M
        X(1)= U1(L)
        E2  = X(1)
        E3  = X(2)
        F1(L)= 0.D0
        IF( HCPS3(MM(4),MM(1),MM(2),MM(3),E2,E3,CT23) ) THEN
          X2= E2/MM(4)
          X3= E3/MM(4)
          X1= 1.D0-X2-X3
C - Fill Q(i,j)= P(i)*metric*P(j)
          DO I=1,NV
            Q(I,I)= MM(I)**2
          END DO
          Q(1,2)= ((MM(4)**2)*(1.D0-2.D0*X3)+
     .              MM(3)**2-MM(1)**2-MM(2)**2)/2.D0
          Q(1,3)= ((MM(4)**2)*(1.D0-2.D0*X2)+
     .              MM(2)**2-MM(1)**2-MM(3)**2)/2.D0
          Q(2,3)= ((MM(4)**2)*(1.D0-2.D0*X1)+
     .              MM(1)**2-MM(2)**2-MM(3)**2)/2.D0
          Q(1,4)= (MM(4)**2)*X1
          Q(2,4)= (MM(4)**2)*X2
          Q(3,4)= (MM(4)**2)*X3
          DO I=1,NV
            DO J=I+1,NV
              Q(J,I)= Q(I,J)
            END DO
          END DO
C - Decay width
          F1(L)= HHUDME(MM,Q)
        END IF
      END DO
C
      RETURN
      END
      SUBROUTINE HHUDWDX2(M,U2,F2,X)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      IMPLICIT   NONE
C - Parameters & Constants
C - Arguments
      INTEGER    M
      REAL*8     U2(*),F2(*),X(2)
C - Common blocks
      REAL*4     IPAR, RPAR
      COMMON /PARPS3/ IPAR(6), RPAR(6)
C - Auxiliary variables
      INTEGER    L
      REAL*8     XMIN, XMAX, DGMLT1
      EXTERNAL   HHUDWDX1, DGMLT1
C - Data statements
C - Macros
C - Statements
      XMIN= RPAR(4)
      XMAX= RPAR(4)+RPAR(6)
      DO L=1,M
        X(2)= U2(L)
        F2(L)= DGMLT1(HHUDWDX1,XMIN,XMAX,5,6,X)
      END DO
C
      RETURN
      END
      LOGICAL FUNCTION HCPS3(MH,M1,M2,M3,E2,E3,CT23)
C----------------------------------------------------------------------
C!    Check 3-body phase space
C!
C!    Input:           MH  = decaying particle mass (GeV)
C!                     M1, M2, M3 = masses of decay products (GeV)
C!                     E2, E3 = energies of particles with mass M2, M3 (
C!    Output:          HCPS3= .TRUE. if good phase space configuration
C!                     CT23 = cosine of the angle between 2 and 3 direct
C!                     ( = -10 if the check is .FALSE. )
C!
C!    Called by HHUDWD
C!
C!    Gerardo Ganis              3 May 1996
C----------------------------------------------------------------------
      IMPLICIT    NONE
C - Arguments
      REAL*8      MH, M1, M2, M3,
     .            E2, E3, CT23
C - Auxiliary variables
      REAL*8      P2, P3, A, B
C
      HCPS3= .FALSE.
      CT23= -10.D0
C - Check low bound
      IF( E2 .LT. M2 )         RETURN
      IF( E3 .LT. M3 )         RETURN
C - Check sum
      IF( E2+E3 .GT. MH-M1 ) RETURN
C - From phase space definition
      P2= SQRT( E2**2- M2**2 )
      P3= SQRT( E3**2- M3**2 )
      A  = MH**2-M1**2+M2**2+M3**2-2*MH*(E2+E3)+2*E2*E3
      B  = 2*P2*P3
      IF( ABS(A).GE. B  )   RETURN
C
      CT23= A/B
      HCPS3= .TRUE.
C
      RETURN
      END

      FUNCTION HHUDME(M,Q)
C-----------------------------------------------------------------------
C! Matrix element for the process H+ -> H0 u d
C!
C!    Input:     M(i) masses in GeV/**2: i=1  m_H0
C!                                        =2  m_u
C!                                        =3  m_d
C!                                        =4  m_H+
C!               Q(i,j) dot products P(i)*metric*P(j)
C!                      i,j=1,4 ( H0, u, d, H+ )
C!    Output:    HHUDME
C!
C!    Called by HHUDWD, GETHUD
C!
C!    Gerardo Ganis              3 May 1996
C-----------------------------------------------------------------------
      IMPLICIT   NONE
      REAL*8     HHUDME
C - Parameters & Constants
      INTEGER    NV
      PARAMETER( NV= 4 )
      REAL*8     PI
      PARAMETER( PI= 3.141592653589793238 )
C - Arguments
      REAL*8     M(NV),Q(NV,NV)
C - Common blocks
C - JETSET parameter commons
      INTEGER     MSTU, MSTJ
      REAL*4      PARU, PARJ
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER     KCHG
      REAL*4      PMAS, PARF, VCKM
      COMMON /LUDAT2/ KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
C - Auxiliary variables
      REAL*8      MW, GW, SW2, EE,
     .            PW, KK, KAP, MER
      REAL*4      ULALEM
C - Data statements
C - Macros
C - Statements
      MW = DBLE(PMAS(24,1))
      GW = DBLE(PMAS(24,2))
      SW2= DBLE(PARU(102))
      EE = DBLE(ULALEM(SNGL(M(4)**2))) * 4.D0 * PI
C - Check if kinematically allowed
      HHUDME= 0.D0
      IF( M(4).LT.(M(1)+M(2)+M(3)) ) RETURN
C - Propagator
      KK = M(2)**2+M(3)**2+2.D0*Q(2,3)
      PW = 1.D0/((KK-MW**2)**2+(MW*GW)**2)
C - Kappa
      KAP= (M(4)+M(1))*(M(4)-M(1))/(MW**2)
C - reduced matrix element
      MER= 4.D0*(Q(1,3)+Q(3,4))*(Q(1,2)+Q(2,4)) -
     .     2.D0*(M(1)**2+M(4)**2+2.D0*Q(1,4))*Q(2,3) +
     .     2.D0*(KAP**2)*Q(2,3)*(M(2)**2+M(3)**2) +
     .     4.D0*KAP*M(2)*M(3)*(Q(1,2)+Q(2,4)-Q(1,3)-Q(3,4)) -
     .     4.D0*((KAP*M(2)*M(3))**2)
C - Matrix element
      HHUDME= (EE/SW2)**2 /8.D0 * PW * MER
C
      RETURN
      END

      SUBROUTINE HCRNDM( RANDOM, LEN )
C----------------------------------------------------------------------
C! Get LEN random numbers in [0,1]
C!
C!    Input:     LEN     = wanted number of randoms
C!    Output:    RANDOM  = array dimensioned at least LEN with randoms
C!
C!    Called by many
C!
C!    Gerardo Ganis              3 May 1996
C----------------------------------------------------------------------
      IMPLICIT    NONE
      REAL*8    RANDOM(*)
      INTEGER   LEN
      REAL*4    RNDM
      INTEGER   I, DUM
C
      DUM= 0
      DO I=1,LEN
        RANDOM(I)= DBLE(RNDM(DUM))
      END DO
C
      RETURN
      END
      SUBROUTINE HCCUBR(BR)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      IMPLICIT   NONE
C - Parameters & Constants
C - Common blocks
      INTEGER    IHCD
      REAL*8     MH, TBT, TAF
      COMMON /HCCOMM/ MH(4), TBT, TAF, IHCD(2)
      INTEGER    HCCON, NSCH, NCH, NDCH, NMXC
      PARAMETER( NCH= 49, NDCH= 19, NMXC= 9 )
      REAL*8     HCCBR1, HCCBR2, TAUHP
      COMMON /HCDEFF/ NSCH(NDCH), HCCON(NDCH),
     .                HCCBR1(0:NDCH), HCCBR2(NDCH,NMXC), TAUHP
      CHARACTER*17 HCCHAN
      COMMON /HCNAME/ HCCHAN(NDCH)
      INTEGER    UPMAX
      REAL*8     MEMAX, THRES
      COMMON /HCMAXX/ MEMAX(NCH), THRES(NCH), UPMAX(NCH)
      INTEGER    HCDSTA
      COMMON /HCSTAT/ HCDSTA(0:NCH)
C-----------------------------------------------------------------------
C!    HCCHAN           channel names
C!    HCCON            requested decay ( 1= ON, 0= OFF )
C!    HCCBR1           cumulative BR on first channel classif. (data car
C!    HCCBR2           cumulative BR on subchannel classif. (internal)
C!    NSCH             number of subchannels per channel
C!    TAUHP            lifetime in seconds
C-----------------------------------------------------------------------
C - Arguments
      REAL*8     BR(0:NCH)
C - Auxiliary variables
      INTEGER    I, J, N
C - Data statements
C - Macros
C - Statements
      DO I=1,NDCH
        HCCBR1(I)= 0.D0
        DO J=1,NMXC
          HCCBR2(I,J)= 0.D0
        END DO
      END DO
C - Cumulate ...
      N= 1
      HCCBR1(N)= BR(1)*HCCON(N)
      DO I=2,12
        N= N+1
        HCCBR1(N)= HCCBR1(N-1) + BR(I)*HCCON(N)
      END DO
      N= N+1
      HCCBR1(N)= HCCBR1(N-1)
      DO I=13,15
        HCCBR1(N)= HCCBR1(N) + BR(I)*HCCON(N)
      END DO
      N= N+1
      HCCBR1(N)= HCCBR1(N-1)
      DO I=16,24
        HCCBR1(N)= HCCBR1(N) + BR(I)*HCCON(N)
      END DO
      N= N+1
      HCCBR1(N)= HCCBR1(N-1)
      DO I=25,27
        HCCBR1(N)= HCCBR1(N) + BR(I)*HCCON(N)
      END DO
      N= N+1
      HCCBR1(N)= HCCBR1(N-1)
      DO I=28,36
        HCCBR1(N)= HCCBR1(N) + BR(I)*HCCON(N)
      END DO
      N= N+1
      HCCBR1(N)= HCCBR1(N-1)
      DO I=37,39
        HCCBR1(N)= HCCBR1(N) + BR(I)*HCCON(N)
      END DO
      N= N+1
      HCCBR1(N)= HCCBR1(N-1)
      DO I=40,48
        HCCBR1(N)= HCCBR1(N) + BR(I)*HCCON(N)
      END DO
      N= N+1
      HCCBR1(N)= HCCBR1(N-1) + BR(NCH)*HCCON(N)
      HCCBR1(0)= HCCBR1(N)
      IF( HCCBR1(0).GT.0.D0 ) THEN
        DO I=1,NDCH
          HCCBR1(I)= HCCBR1(I)/HCCBR1(0)
        END DO
      END IF
C - Subchannel cumulative BR
      HCCBR2(13,1)= BR(13)
      HCCBR2(13,2)= HCCBR2(13,1)
      DO I=14,15
        HCCBR2(13,I-12)= HCCBR2(13,I-12) + BR(I)
      END DO
      HCCBR2(14,1)= BR(16)
      HCCBR2(14,2)= HCCBR2(14,1)
      DO I=17,24
        HCCBR2(14,I-15)= HCCBR2(14,I-15) + BR(I)
      END DO
      HCCBR2(15,1)= BR(25)
      HCCBR2(15,2)= HCCBR2(15,1)
      DO I=26,27
        HCCBR2(15,I-24)= HCCBR2(15,I-24) + BR(I)
      END DO
      HCCBR2(16,1)= BR(28)
      HCCBR2(16,2)= HCCBR2(16,1)
      DO I=29,36
        HCCBR2(16,I-27)= HCCBR2(16,I-27) + BR(I)
      END DO
      HCCBR2(17,1)= BR(37)
      HCCBR2(17,2)= HCCBR2(17,1)
      DO I=38,39
        HCCBR2(17,I-36)= HCCBR2(17,I-36) + BR(I)
      END DO
      HCCBR2(18,1)= BR(40)
      HCCBR2(18,2)= HCCBR2(18,1)
      DO I=41,48
        HCCBR2(18,I-39)= HCCBR2(18,I-39) + BR(I)
      END DO
C - Normalize them to 1.
      DO I=13,18
        DO J=1,NSCH(I)
          IF( HCCBR2(I,NSCH(I)).GT.0.D0 )
     .        HCCBR2(I,J)= HCCBR2(I,J)/HCCBR2(I,NSCH(I))
        END DO
      END DO
C
      RETURN
      END

      SUBROUTINE HCDECC(IDCP,IDCM)
C-----------------------------------------------------------------------
C! Choose decay channels for Charged Higgses
C!
C!    Input:         ihcd(2) in common block /HCCOMM/
C!    Output:        IDCP   channel number for H+ decay
C!                   IDCM   channel number for H- decay
C!
C!    Gerardo Ganis  --  5 May 1996
C-----------------------------------------------------------------------
      IMPLICIT   NONE
C - Parameters & Constants
C - Common blocks
      INTEGER    IHCD
      REAL*8     MH, TBT, TAF
      COMMON /HCCOMM/ MH(4), TBT, TAF, IHCD(2)
      INTEGER    HCCON, NSCH, NCH, NDCH, NMXC
      PARAMETER( NCH= 49, NDCH= 19, NMXC= 9 )
      REAL*8     HCCBR1, HCCBR2, TAUHP
      COMMON /HCDEFF/ NSCH(NDCH), HCCON(NDCH),
     .                HCCBR1(0:NDCH), HCCBR2(NDCH,NMXC), TAUHP
      CHARACTER*17 HCCHAN
      COMMON /HCNAME/ HCCHAN(NDCH)
      INTEGER    UPMAX
      REAL*8     MEMAX, THRES
      COMMON /HCMAXX/ MEMAX(NCH), THRES(NCH), UPMAX(NCH)
      INTEGER    HCDSTA
      COMMON /HCSTAT/ HCDSTA(0:NCH)
C-----------------------------------------------------------------------
C!    HCCHAN           channel names
C!    HCCON            requested decay ( 1= ON, 0= OFF )
C!    HCCBR1           cumulative BR on first channel classif. (data car
C!    HCCBR2           cumulative BR on subchannel classif. (internal)
C!    NSCH             number of subchannels per channel
C!    TAUHP            lifetime in seconds
C-----------------------------------------------------------------------
C - Arguments
      INTEGER    IDCP, IDCM
C - Auxiliary variables
      REAL*8     RAN(1)
      INTEGER    I, J, K, CHAN(2), SUBC(2), IDC(2)
      LOGICAL    FIRST, NOTYET
C - Data statements
      DATA       FIRST /.TRUE./
C - Macros
      NOTYET(I)= (I.GE.13.AND.I.LE.15) .OR. I.EQ.49
C - Statements
      IF( FIRST ) THEN
        DO I=1,NCH
          HCDSTA(I)= 0
        END DO
      END IF
      DO I=1,2
        SUBC(I)= 1
        IF( IHCD(I).GT.0 .AND. IHCD(I).LE.NDCH ) THEN
          CHAN(I)= IHCD(I)
          IF( FIRST .AND. NOTYET( CHAN(I) ) )
     .                           WRITE(*,101) HCCHAN(CHAN(I))
        ELSE
          IF( IHCD(I).NE.0 ) THEN
            IF( FIRST ) WRITE(*,100) I, IHCD(I)
            IHCD(I)= 0
          END IF
          CALL HCRNDM(RAN,1)
          DO J=1,NDCH
            IF( RAN(1).LT.HCCBR1(J) ) GOTO 1
          END DO
 1        CONTINUE
          CHAN(I)= J
        END IF
C - Choose subchannel (if the case)
        IF( NSCH(CHAN(I)).GT.1 ) THEN
          CALL HCRNDM(RAN,1)
          DO J=1,NSCH(CHAN(I))
            IF( RAN(1).LT.HCCBR2(CHAN(I),J) ) GOTO 2
          END DO
 2        CONTINUE
          SUBC(I)= J
        END IF
      END DO
C - Get internal channel number
      DO I=1,2
        IDC(I)= 0
        DO J=1,CHAN(I)-1
          DO K=1,NSCH(J)
            IDC(I)= IDC(I)+1
          END DO
        END DO
        IDC(I)= IDC(I)+SUBC(I)
      END DO
C - Define output variables
      IDCP= IDC(1)
      IDCM= IDC(2)
      CALL HCRNDM(RAN,1)
      IF( RAN(1).GT. 0.5D0 ) THEN
        IDCP= IDC(2)
        IDCM= IDC(1)
      END IF
C - Statistics
      HCDSTA(0)= HCDSTA(0)+2
      HCDSTA(IDC(1))= HCDSTA(IDC(1))+1
      HCDSTA(IDC(2))= HCDSTA(IDC(2))+1
C
 100  FORMAT(//,'*******************************************',/,
     .   'WARNING: channel for decay ',I1,' unknown (',I6,')',/,
     .          '*** Set it to 0 for the rest of the run ***',//)
 101  FORMAT(//,
     .    '***************************************************',/,
     .    '*** The channel chosen has not been implemented ***',/,
     .    '*** properly yet; it does not contribute to the ***',/,
     .    '*** total width, and it decays the H+- through  ***',/,
     .    '*** phase-space only. BE AWARE OF THIS.         ***',/,
     .    '*** Channel :',A17,           '                 ***',/,
     .    '***************************************************',//)
C
      IF( FIRST ) FIRST= .FALSE.
C
      RETURN
      END

      SUBROUTINE HCFILH
C-----------------------------------------------------------------------
C! Fill some histo for H+H- production
C!
C!    Called by ASKUSE
C!
C!    Gerardo Ganis              3 May 1996
C-----------------------------------------------------------------------
      IMPLICIT   NONE
C - Parameters & Constants
      REAL*4     PI
      PARAMETER( PI= 3.141593 )
C - Arguments
C - Common blocks
      INTEGER    HCCON, NSCH, NCH, NDCH, NMXC
      PARAMETER( NCH= 49, NDCH= 19, NMXC= 9 )
      REAL*8     HCCBR1, HCCBR2, TAUHP
      COMMON /HCDEFF/ NSCH(NDCH), HCCON(NDCH),
     .                HCCBR1(0:NDCH), HCCBR2(NDCH,NMXC), TAUHP
      CHARACTER*17 HCCHAN
      COMMON /HCNAME/ HCCHAN(NDCH)
      INTEGER    UPMAX
      REAL*8     MEMAX, THRES
      COMMON /HCMAXX/ MEMAX(NCH), THRES(NCH), UPMAX(NCH)
      INTEGER    HCDSTA
      COMMON /HCSTAT/ HCDSTA(0:NCH)
C-----------------------------------------------------------------------
C!    HCCHAN           channel names
C!    HCCON            requested decay ( 1= ON, 0= OFF )
C!    HCCBR1           cumulative BR on first channel classif. (data car
C!    HCCBR2           cumulative BR on subchannel classif. (internal)
C!    NSCH             number of subchannels per channel
C!    TAUHP            lifetime in seconds
C-----------------------------------------------------------------------
      COMMON / miscl / loutbe,ecm,idb1,idb2,xrad,empir,empirm,ism,
     &                 iklei,icar,sdvrt(3),vrtx(4),tabl(26),
     &                 nevent(11)
      INTEGER loutbe,idb1,idb2,nevent,ism,iklei,icar
      REAL*4 ecm,sdvrt,vrtx,tabl,empir,empirm,xrad
C
C - JETSET common for event record
      INTEGER    NJMAX, NTMAX
      PARAMETER( NJMAX= 4000, NTMAX= 5)
      INTEGER    NJET, KJET
      REAL*4     PJET, VJET
      COMMON /LUJETS/ NJET, KJET(NJMAX,NTMAX), PJET(NJMAX,NTMAX),
     .                      VJET(NJMAX,NTMAX)
C - JETSET parameter commons
      INTEGER     MSTU, MSTJ
      REAL*4      PARU, PARJ
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER     KCHG
      REAL*4      PMAS, PARF, VCKM
      COMMON /LUDAT2/ KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      INTEGER     MDCY, MDME, KFDP
      REAL*4      BRAT
      COMMON /LUDAT3/ MDCY(500,3),MDME(2000,2),BRAT(2000),
     .                KFDP(2000,5)
C - Auxiliary variables
      INTEGER     I, J, K, L
      REAL*4      MASS, QMX, MH, GH, MHP, MHM, THP, THM, CTHS,
     .            PLU, EVIS, EB, ETP, ETM, EPP, EPM
      LOGICAL     FIRST, VISIB
C - Data statements
      DATA        FIRST /.TRUE./
C - Macros
      MASS(I,J)= SQRT((PJET(I,4)+PJET(J,4))**2 -
     .                (PJET(I,1)+PJET(J,1))**2 -
     .                (PJET(I,2)+PJET(J,2))**2 -
     .                (PJET(I,3)+PJET(J,3))**2 )
      VISIB(I) = ABS(I).NE.12 .AND. ABS(I).NE.14 .AND. ABS(I).NE.16
C - Statements
      IF( FIRST ) THEN
        MH= PMAS(37,1)
        GH= PMAS(37,2)
        CALL HBOOK2(10001,'generated widths',
     .            100,MH-20*GH,MH+20*GH,100,MH-20*GH,MH+20*GH,0.)
        CALL HBOOK1(10002,' cos theta * ',50,-1.,1.,0.)
        CALL HBOOK1(10003,' evis ',60,0.,200.,0.)
        CALL HBOOK1(10004,' tau- energy (Ebeam units)',50,0.,1.,0.)
        CALL HBOOK1(10005,' tau+ energy (Ebeam units)',50,0.,1.,0.)
        CALL HBOOK1(10006,' pi- + pi+ ene/eb - lab frame',50,0.,2.,0.)
        CALL HBOOK1(10007,' pi- ene/eb - lab frame',50,0.,1.,0.)
        CALL HBOOK1(10008,' pi+ ene/eb - lab frame',50,0.,1.,0.)
        FIRST= .FALSE.
      END IF
C - get HIGGS
      EB  = ECM/2.
      EVIS= 0.
      DO I=1,NJET
        IF( KJET(I,2).EQ.37 ) THEN
           THP= PLU(I,13)
           MHP= PJET(I,5)
           DO J= KJET(I,4), KJET(I,5)
             IF( KJET(J,2).EQ.-15 ) THEN
               ETP= PJET(J,4)
               DO K= KJET(J,4), KJET(J,5)
                 IF( KJET(K,2).EQ.211 ) EPP= PJET(K,4)
               END DO
             END IF
           END DO
        END IF
        IF( KJET(I,2).EQ.-37 ) THEN
           THM= PLU(I,13)
           MHM= PJET(I,5)
           DO J= KJET(I,4), KJET(I,5)
             IF( KJET(J,2).EQ. 15 ) THEN
               ETM= PJET(J,4)
               DO K= KJET(J,4), KJET(J,5)
                 IF( KJET(K,2).EQ.-211 ) EPM= PJET(K,4)
               END DO
             END IF
           END DO
        END IF
        IF( KJET(I,1).EQ.1 ) THEN
          IF( VISIB(KJET(I,2)) ) EVIS= EVIS + PJET(I,4)
        END IF
      END DO
C - Calculate cos(theta*)
      CTHS= COS(.5*(THM+PI-THP))/COS(.5*(THM-PI+THP))
C - Fill histos
      CALL HF2(10001,MHP,MHM,1.)
      CALL HF1(10002,CTHS,1.)
      CALL HF1(10003,EVIS,1.)
      CALL HF1(10004,ETP/EB,1.)
      CALL HF1(10005,ETM/EB,1.)
      CALL HF1(10006,(EPP+EPM)/EB,1.)
      CALL HF1(10007,EPP/EB,1.)
      CALL HF1(10008,EPM/EB,1.)
C
      RETURN
      END

      SUBROUTINE RAMINI
C-----------------------------------------------------------------------
C! Initialize stuff to run alpha_S and quark masses ...
C!
C! From A.Djouadi code ...
C!    G.Ganis        May 96
C-----------------------------------------------------------------------
      IMPLICIT   NONE
C - Parameters & Constants
      REAL*8     ACC
      PARAMETER( ACC= 1.0D-10 )
C - Arguments
C - Common blocks
      INTEGER    N0
      REAL*8     XLAMBDA, AMCA, AMBA, AMTA
      COMMON    /ALS/ XLAMBDA, AMCA, AMBA, AMTA, N0
      REAL*8     AMS, AMC, AMB, AMT, AMSB
      COMMON    /MASSES/ AMS, AMC, AMB, AMT, AMSB
C - JETSET parameter commons
      INTEGER     MSTU, MSTJ
      REAL*4      PARU, PARJ
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER     KCHG
      REAL*4      PMAS, PARF, VCKM
      COMMON /LUDAT2/ KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
C - Auxiliary variables
C - Data statements
C - Macros
C - Statements
      N0     = 5
C - Get relevant parameters from JETSET commons
      XLAMBDA= DBLE(PARU(112))
      AMS    = DBLE(PMAS(3,1))
      AMC    = DBLE(PMAS(4,1))
      AMB    = DBLE(PMAS(5,1))
      AMT    = DBLE(PMAS(6,1))
      AMSB   = AMS
      AMCA   = AMC
      AMBA   = AMB
      AMTA   = AMT
C - Init alpha_s stuff ...
      CALL ALSINI( ACC )

      RETURN
      END

      FUNCTION RQMASS(Q,KF)
C-----------------------------------------------------------------------
C! Get running quark mass
C!
C!    INPUT:          Q   scale (GeV)                         (REAL*8)
C!                    KF  number of flavours above threshold  (INTEGER)
C!    OUTPUT:         RQMASS  running quark mass              (REAL*8)
C!
C!    Commnets:       Adapted from function RUNP in Djouadi code.
C!    G.Ganis         May 96
C-----------------------------------------------------------------------
      IMPLICIT   NONE
      REAL*8     RQMASS
C - Parameters & Constants
C - Arguments
      INTEGER    KF
      REAL*8     Q
C - Common blocks
C - JETSET parameter commons
      INTEGER     KCHG
      REAL*4      PMAS, PARF, VCKM
      COMMON /LUDAT2/ KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
C - Auxiliary variables
      REAL*8      RUNM
C - Data statements
C - Macros
C - Statements
      IF( KF.EQ.1 .OR. KF.EQ.2 ) THEN
        RQMASS= DBLE(PMAS(KF,1))
        RETURN
      END IF
      IF( KF.GE.3 .AND. KF.LE.6 ) THEN
        RQMASS= RUNM(Q,KF,1)
      END IF
C
      RETURN
      END

      FUNCTION RUNM(Q,NF,IMSBAR)
C-----------------------------------------------------------------------
C!
C!    INPUT:          Q   scale (GeV)                         (REAL*8)
C!                    NF  number of flavours above threshold  (INTEGER)
C!    OUTPUT:         RUNM    running quark mass              (REAL*8)
C!
C!    Comments:       Cleaning of Djouadi code.
C!    Author:         A.Djouadi
C!    Cleaned by      G.Ganis         July 2, 96
C-----------------------------------------------------------------------
      IMPLICIT   NONE
      REAL*8     RUNM
C - Parameters
      INTEGER    NN
      REAL*8     ZETA3, ACC
      PARAMETER( NN= 6, ZETA3= 1.202056903159594D0, ACC= 1.D-8 )
C - Arguments
      INTEGER    NF
      REAL*8     Q
C - Common blocks
      INTEGER    N0A
      REAL*8     XLAMBDA, AMCA, AMBA, AMTA
      COMMON    /ALS/    XLAMBDA, AMCA, AMBA, AMTA, N0A
      REAL*8     AMS, AMC, AMB, AMT, AMSB
      COMMON    /MASSES/ AMS, AMC, AMB, AMT, AMSB
      REAL*8     XMSB, XMHAT, XKFAC
      COMMON    /RUN/    XMSB, XMHAT, XKFAC
C - Auxiliary variables
      INTEGER    I, J, ISTRANGE, IMSBAR, N0
      REAL*8     AM(NN), YMSB(NN),
     .           B0, B1, B2, G0, G1, G2, C0, C1, C2, CQ, TRAN,
     .           X, XK, PI, ALPHAS, DD, AMSU, AMSD, Q0
      SAVE ISTRANGE
C - Macro statements
      B0(I)=(33.D0-2.D0*I)/12D0
      B1(I) = (102D0-38D0/3D0*I)/16D0
      B2(I) = (2857D0/2D0-5033D0/18D0*I+325D0/54D0*I**2)/64D0
      G0(I) = 1D0
      G1(I) = (202D0/3D0-20D0/9D0*I)/16D0
      G2(I) = (1249D0-(2216D0/27D0+160D0/3D0*ZETA3)*I
     .       - 140D0/81D0*I**2)/64D0
      C1(I) = G1(I)/B0(I) - B1(I)*G0(I)/B0(I)**2
      C2(I) = ((G1(I)/B0(I) - B1(I)*G0(I)/B0(I)**2)**2
     .       + G2(I)/B0(I) + B1(I)**2*G0(I)/B0(I)**3
     .       - B1(I)*G1(I)/B0(I)**2 - B2(I)*G0(I)/B0(I)**2)/2D0
      TRAN(X,XK)=1D0+4D0/3D0*ALPHAS(X,2)/PI+XK*(ALPHAS(X,2)/PI)**2
      CQ(X,I)=(2D0*B0(I)*X)**(G0(I)/B0(I))
     .            *(1D0+C1(I)*X+C2(I)*X**2)
C - Data statements
      DATA ISTRANGE/0/
C - Statements
      PI=4.D0*ATAN(1.D0)
      AM(1) = 0
      AM(2) = 0
C -
      IF( IMSBAR.EQ.1 ) THEN
        IF( ISTRANGE.EQ.0 ) THEN
C - Strange pole mass from msbar-mass at 1 GeV
          AMSD = XLAMBDA
          AMSU = 1.D8
123       AMS  = (AMSU+AMSD)/2
          AM(3) = AMS
          XMSB = AMS/CQ(ALPHAS(AMS,2)/PI,3)
     .            *CQ(ALPHAS(1.D0,2)/PI,3)/TRAN(AMS,0D0)
          DD = (XMSB-AMSB)/AMSB
          IF( ABS(DD).GE.ACC ) THEN
            IF(DD.LE.0.D0)THEN
              AMSD = AM(3)
            ELSE
              AMSU = AM(3)
            ENDIF
            GOTO 123
          ENDIF
          ISTRANGE=1
        ENDIF
      ELSE
        AMS=AMSB
        AM(3) = AMS
      ENDIF
C -
      AM(4) = AMC
      AM(5) = AMB
      AM(6) = AMT
      XK = 16.11D0
      DO I=1,NF-1
        XK = XK - 1.04D0*(1.D0-AM(I)/AM(NF))
      END DO
C -
      XMSB = AM(NF)/TRAN(AM(NF),0D0)
      XMHAT = XMSB/CQ(ALPHAS(AM(NF),2)/PI,NF)
C     XKFAC = TRAN(AM(NF),0D0)/TRAN(AM(NF),XK)
      XKFAC = TRAN(AM(NF),0D0)
      IF(NF.EQ.3)THEN
        YMSB(3) = XMSB
        YMSB(4) = YMSB(3)*CQ(ALPHAS(AM(4),2)/PI,3)/
     .                    CQ(ALPHAS(AM(3),2)/PI,3)
        YMSB(5) = YMSB(4)*CQ(ALPHAS(AM(5),2)/PI,4)/
     .                    CQ(ALPHAS(AM(4),2)/PI,4)
        YMSB(6) = YMSB(5)*CQ(ALPHAS(AM(6),2)/PI,5)/
     .                    CQ(ALPHAS(AM(5),2)/PI,5)
      ELSEIF(NF.EQ.4)THEN
        YMSB(4) = XMSB
        YMSB(3) = YMSB(4)*CQ(ALPHAS(AM(3),2)/PI,3)/
     .                    CQ(ALPHAS(AM(4),2)/PI,3)
        YMSB(5) = YMSB(4)*CQ(ALPHAS(AM(5),2)/PI,4)/
     .                    CQ(ALPHAS(AM(4),2)/PI,4)
        YMSB(6) = YMSB(5)*CQ(ALPHAS(AM(6),2)/PI,5)/
     .                    CQ(ALPHAS(AM(5),2)/PI,5)
      ELSEIF(NF.EQ.5)THEN
        YMSB(5) = XMSB
        YMSB(4) = YMSB(5)*CQ(ALPHAS(AM(4),2)/PI,4)/
     .                    CQ(ALPHAS(AM(5),2)/PI,4)
        YMSB(3) = YMSB(4)*CQ(ALPHAS(AM(3),2)/PI,3)/
     .                    CQ(ALPHAS(AM(4),2)/PI,3)
        YMSB(6) = YMSB(5)*CQ(ALPHAS(AM(6),2)/PI,5)/
     .                    CQ(ALPHAS(AM(5),2)/PI,5)
      ELSEIF(NF.EQ.6)THEN
        YMSB(6) = XMSB
        YMSB(5) = YMSB(6)*CQ(ALPHAS(AM(5),2)/PI,5)/
     .                    CQ(ALPHAS(AM(6),2)/PI,5)
        YMSB(4) = YMSB(5)*CQ(ALPHAS(AM(4),2)/PI,4)/
     .                    CQ(ALPHAS(AM(5),2)/PI,4)
        YMSB(3) = YMSB(4)*CQ(ALPHAS(AM(3),2)/PI,3)/
     .                    CQ(ALPHAS(AM(4),2)/PI,3)
      ENDIF
      IF(Q.LT.AMC)THEN
        N0=3
        Q0 = AMS
      ELSEIF(Q.LE.AMB)THEN
        N0=4
        Q0 = AMC
      ELSEIF(Q.LE.AMT)THEN
        N0=5
        Q0 = AMB
      ELSE
        N0=6
        Q0 = AMT
      ENDIF
C - Output
      RUNM = YMSB(N0)*CQ(ALPHAS(Q,2)/PI,N0)/
     .                CQ(ALPHAS(Q0,2)/PI,N0)
C -
      RETURN
      END
      FUNCTION XITER(Q,XLB1,NF1,XLB,NF2,ACC)
C-----------------------------------------------------------------------
C!
C!    INPUT:          Q   scale (GeV)                         (REAL*8)
C!                    XLB1                                    (REAL*8)
C!                    NF1                                     (INTEGER)
C!                    XLB                                     (REAL*8)
C!                    NF2                                     (INTEGER)
C!                    ACC                                     (REAL*8)
C!    OUTPUT:         XITER                                   (REAL*8)
C!
C!    Comments:       Cleaning of Djouadi code.
C!    Author:         A.Djouadi
C!    Cleaned by      G.Ganis         July 2, 96
C-----------------------------------------------------------------------
      IMPLICIT   NONE
      REAL*8     XITER
C - Parameters
C - Arguments
      INTEGER    NF1, NF2
      REAL*8     Q, XLB1, XLB, ACC
C - Auxiliary variables
      INTEGER    I, II
      REAL*8     B0, B1, ALS2, AA, BB, XIT,
     .           A, B, X, Y,
     .           PI, ALP, XX, Y1, Y2, DY, XLB2
C - Macro
      B0(I)= 33.D0-2.D0*I
      B1(I)= 6.D0*(153.D0-19.D0*I)/B0(I)**2
      ALS2(I,X,Y)= 12.D0*PI/(B0(I)*LOG(X**2/Y**2))
     .                  *(1.D0-B1(I)*LOG(LOG(X**2/Y**2))
     .                  /LOG(X**2/Y**2))
      AA(I)= 12D0*PI/B0(I)
      BB(I)= B1(I)/AA(I)
      XIT(A,B,X)= A/2.D0*(1D0+SQRT(1D0-4D0*B*LOG(X)))
C - Statements
      PI  = 4.D0*ATAN(1.D0)
      XLB2= XLB
      II  = 0
1     II  = II+1
      X   = LOG(Q**2/XLB2**2)
      ALP = ALS2(NF1,Q,XLB1)
      A   = AA(NF2)/ALP
      B   = BB(NF2)*ALP
      XX  = XIT(A,B,X)
      XLB2= Q*EXP(-XX/2.D0)
      Y1  = ALS2(NF1,Q,XLB1)
      Y2  = ALS2(NF2,Q,XLB2)
      DY  = ABS(Y2-Y1)/Y1
      IF( DY.GE.ACC ) GOTO 1
C - Output
      XITER=XLB2
C -
      RETURN
      END
