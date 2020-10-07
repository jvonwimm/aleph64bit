      SUBROUTINE MDSTWR(LOUT,FNAME,ATYPE,FDEVI,ILAST)
C-----------------------------------------------------------------------
C! Write the ENFLW output in micro-DST format to avoid waste of time...
C
C  22 Aug 1990 -- Patrick Janot.
C-----------------------------------------------------------------------
#ifndef DOC
#include "btpar.h"
#include "parcut.h"
#include "parabank.h"
      COMMON / vernvw / vernfw
      COMMON / mdstag / imdswr 
      CHARACTER*(*) fname,atype,fdevi
      INTEGER altrig
      LOGICAL first,xhvok
      DIMENSION rmass(4),rchar(4),rsigm(4),riexp(4),hyp(4),ihyp(4)
      DATA rmass/.511E-3,.1396,.4975,.93827/
      DATA rchar/1.,1.,1.,1./
      DATA first/.TRUE./
      DATA krun0 /0/
C
      PARAMETER ( mgtk = 100 )
      DIMENSION bpos(3),bsiz(3),epvtx(3),pvtx(3)
      DIMENSION jgtk(mgtk)
C
#include "qcde.h"
      DIMENSION QT(KCQVEC,1), KT(KCQVEC,1), QV(KCQVRT,1), KV(KCQVEC,1)
#include "qmacro.h"
C-----------------------------------------------------------------------
C
      IF ( first ) THEN
C---Give format to the banks
        CALL bkfmt('FWIN','6I,9F')
        CALL bkfmt('FWEV','14I,4F')
        CALL bkfmt('FWPA','2I,(F)')
        CALL bkfmt('FWID','2I,(I)')
        CALL bkfmt('FWCH','2I,(I)')
        CALL bkfmt('FWD0','2I,(F)')
        CALL bkfmt('FWDX','2I,(2I)')
        CALL bkfmt('FWVX','2I,6F,I')
        CALL bkfmt('FWFR','2I,(5F,I)')
        CALL bkfmt('FWF2','2I,(2F,I)')
C---Change the default NUMJET value for QIPBTAGx
C---   Should not be called any more after ALPHA 124.06 ---
C---   due to new QIPBATG initialisation (btag_par obsolete)--- 
C---   Instead : initialise imdswr 
c        CALL btag_par
c        numjet = 10
         imdswr=1
C---Open the output unit
        CALL aopenw(lout,fname,atype,fdevi,ier)
        IF ( ier .NE. 0 ) THEN
          WRITE (IW(6),*) 'Unsuccessful open for micro-DST'
          GOTO 999
        ENDIF
C---After the first, the second and so on
        first = .FALSE.
      ENDIF
C
C  Then, create the FWIN bank and write the RUN record
C
      IF ( krun .NE. krun0 ) THEN
        krun0 = krun
        IF (krun.LE.4000.AND.NLINK('X1RG',0).LE.0) CALL x1ini(-1)
        ifwin = nbank('FWIN',0,15)
        IF ( ifwin .LE. 0 ) THEN
          WRITE (IW(6),*) 'Bank FWIN doesn''t exist'
          GOTO 999
        ENDIF
        iw(ifwin +  1) = 13
        iw(ifwin +  2) = 1
        iw(ifwin +  3) = NINT(vernfw*100.)
        iw(ifwin +  4) = ntrack
        iw(ifwin +  5) = mtrack
        iw(ifwin +  6) = n0
        rw(ifwin +  7) = d0
        rw(ifwin +  8) = z0
        rw(ifwin +  9) = echmin
        rw(ifwin + 10) = echmax
        rw(ifwin + 11) = sigec
        rw(ifwin + 12) = sighc
        rw(ifwin + 13) = epilca
        rw(ifwin + 14) = epiend
        rw(ifwin + 15) = epibar
C       CALL prtabl('FWIN',0)
C       CALL blist(IW,'C+','FWIN')
        CALL bkrhaw('ENFLW   ',IW(ifwin+3),1,5,ifail)
        CALL bwrite(iw,lout,'RUNRRUNHRHAHRLEPRALEPLSCFWIN')
        CALL bdrop(iw,'FWIN')
      ENDIF
C
C  Terminate properly for the last event processed
C
      IF ( ilast .EQ. 1 ) THEN
        CALL bwrite(iw,lout,'0')
        GOTO 999
      ENDIF
C
C  Then, write the event information and the ENFLW particles in
C  the FWEV and FWPA banks.
C
      nqual = 0
      ifwev = nbank('FWEV',0,18)
      IF ( ifwev .LE. 0 ) THEN
        WRITE (IW(6),*) 'Bank FWEV doesn''t exist'
        GOTO 999
      ENDIF
      ieaux = NLINK('EAUX',0)
      irtrg = NLINK('RTRG',0)
      IF ( ieaux .GT. 0 .AND. irtrg .LE. 0 ) THEN
        itrgw = IW(ieaux+lmhlen+1)
      ELSE
        IF ( NLINK('X1RG',0) .GT. 0 ) THEN
          CALL x1redo
        ELSE
          CALL x1trg
        ENDIF
        itrgfl = ALTRIG(itrgw,itrg2,itrg3)
      ENDIF
      iw(ifwev +  1) = 16
      iw(ifwev +  2) = 1
      iw(ifwev +  3) = itrgw
      iw(ifwev +  4) =  1000*nphco + npeco
      IF ( xhvtrg ) THEN
        iw(ifwev +  5) = 1
      ELSE
        iw(ifwev +  5) = 0
      ENDIF
      xhvok = xvlcal.AND.xvecal.and.xvhcal
      IF ( xhvok ) THEN
        iw(ifwev +  6) = 1
      ELSE
        iw(ifwev +  6) = 0
      ENDIF
      CALL getlum(krun,ifoun,nqual,nz,nb,rlumi,bk,bt)
      IF ( ifoun .GT. 0 ) THEN
        iw(ifwev +  7) = nqual
      ELSE
        iw(ifwev +  7) = -2
      ENDIF
      iw(ifwev +  8) = klstot(1)-kfrtot(1)+1
      iw(ifwev +  9) = klstot(2)-kfrtot(2)+1
      iw(ifwev + 10) = klstot(3)-kfrtot(3)+1
      iw(ifwev + 11) = klstot(4)-kfrtot(4)+1
      iw(ifwev + 12) = klstot(5)-kfrtot(5)+1
      iw(ifwev + 13) = klstot(6)-kfrtot(6)+1
      iw(ifwev + 14) = klstot(7)-kfrtot(7)+1
      rw(ifwev + 15) = qelep
      rw(ifwev + 16) = hadtot
      rw(ifwev + 17) = hadkil
      rw(ifwev + 18) = ene12
C
      nwords = 2 + 4 * (klstot(7)-kfrtot(1)+1)
      ifwpa = nbank('FWPA',0,nwords)
      IF ( ifwpa .LE. 0 ) THEN
        WRITE (IW(6),*) 'Bank FWPA doesn''t exist'
      GOTO 999
      ENDIF
      iw(ifwpa +  1) = 4
      iw(ifwpa +  2) = klstot(7) - kfrtot(1) + 1
      DO 1 iparfw = 1 , iw(ifwpa + 2)
        iefw = kfrefw+iparfw-1
        iefo = iefw
  130   iefo = ksame(iefo)
        IF ( iefo .EQ. iefw ) THEN
          WRITE (IW(6),*) '+++ Warning MDSTWR +++ No EFOL/ENFW matching'
          GOTO 131
        ENDIF
        IF ( iefo .LT. kfeft .OR. iefo .GT. kleft ) GOTO 130
  131   CONTINUE
        rw(ifwpa + 3 + (iparfw-1)*4) = qx(iefw)
        rw(ifwpa + 4 + (iparfw-1)*4) = qy(iefw)
        rw(ifwpa + 5 + (iparfw-1)*4) = qz(iefw)
C       rw(ifwpa + 6 + (iparfw-1)*4) = qe(iefw)
        IF ( iparfw .LE. klstot(1)-kfrefw+1 ) THEN
          kjul = ktn(iefw)
          rw(ifwpa + 6 + (iparfw-1)*4) =
     .    FLOAT(kjul+200*kefolj(iefo)) * qch(iefw)
        ELSEIF ( iparfw .LE. klstot(2)-kfrefw+1 ) THEN
          kjul = ktn(iefw)
          ksis = ksister(iefw)-kfrefw+1
          rw(ifwpa + 6 + (iparfw-1)*4) =
     .    FLOAT(ksis+200*kjul+40000*kefolj(iefo)) * qch(iefw)
        ELSEIF ( iparfw .LE. klstot(3)-kfrefw+1 ) THEN
          nphefw = kfrefw + iparfw - 1
          call infoga(nphefw,icluec,npcflg,ncrflg,nga)
          rw(ifwpa + 6 + (iparfw-1)*4) =
     .    FLOAT(ncrflg + 10*npcflg + 100*icluec+20000*kefolj(iefo))
        ELSEIF ( iparfw .LE. klstot(6)-kfrefw+1 ) THEN
          rw(ifwpa + 6 + (iparfw-1)*4) =
     .    FLOAT(kefolj(iefo)) + AMIN1(99.999,qe(iefw))/100.
        ELSE
        ENDIF
    1 CONTINUE
C
      nwords = 2 + 2 * (klstot(2)-kfrtot(1)+1)
      ifwid = nbank('FWID',0,nwords)
      IF ( ifwid .LE. 0 ) THEN
        WRITE (IW(6),*) 'Bank FWID doesn''t exist'
      GOTO 999
      ENDIF
      iw(ifwid +  1) = 2
      nch = 0
      DO 11 itk = kfrtot(1),klstot(2)
        ich = itk-kfrtot(1)+1
        idt = idtflg(ich)
        IF ( idt .EQ. 0 ) GOTO 11
        nch = nch+1
        iw(ifwid + 3 + (nch-1)*2 ) = ich
        iw(ifwid + 4 + (nch-1)*2 ) = idt
   11 CONTINUE
      iw(ifwid+2) = nch
      nwords = 2 + 2 * nch
      ifwid = nbank('FWID',0,nwords)
C
      nwords = 2 + 3 * (klstot(2)-kfrtot(1)+1)
      ifwch = nbank('FWCH',0,nwords)
      IF ( ifwch .LE. 0 ) THEN
        WRITE (IW(6),*) 'Bank FWCH doesn''t exist'
      GOTO 999
      ENDIF
      iw(ifwch +  1) = 3
      nch = 0
      DO 21 itk = kfrtot(1),klstot(2)
        IF ( qp(itk) .LT. 0.8 ) GOTO 21
        ich = itk - kfrtot(1) + 1
        IF ( muflg(ich) .EQ. 0  .AND. muwrd(ich) .EQ. 0  .AND.
     .       leflg(ich) .LE. 0  .AND. ehcal(ich) .EQ. 0. .AND.
     .       elrtr(ich) .EQ. 0. .AND. elrlg(ich) .EQ. 0. ) GOTO 21
        nch = nch+1
        ih = NINT ( ehcal(ich)*20. )
        i1 = AMAX1( 10.+elrtr(ich), 0.) * 100.
        i1 = MIN0 ( i1, 3999)
        i2 = AMAX1( 10.+elrlg(ich), 0.) * 100.
        i2 = MIN0 ( i2, 3999)
        iw(ifwch + 3 + (nch-1)*3 ) = muflg(ich) + 500 * leflg(ich)
     .                             + 5000 * ich
        iw(ifwch + 4 + (nch-1)*3 ) = muwrd(ich) + 50000*ih
        iw(ifwch + 5 + (nch-1)*3 ) = i1 + 4000*i2
   21 CONTINUE
      iw(ifwch+2) = nch
      nwords = 2 + 3 * nch
      ifwch = nbank('FWCH',0,nwords)
C
C dE/dx and distance of closest approach for all charged particles.
C
      nwords = 2 + (klstot(2)-kfrtot(1)+1)
      ifwd0 = nbank('FWD0',0,nwords)
      nwords = 2 + 2*(klstot(2)-kfrtot(1)+1)
      ifwdx = nbank('FWDX',0,nwords)
      IF ( ifwd0 .LE. 0 ) THEN
        WRITE (IW(6),*) 'Bank FWD0 doesn''t exist'
        GOTO 999
      ENDIF
      IF ( ifwdx .LE. 0 ) THEN
        WRITE (IW(6),*) 'Bank FWDX doesn''t exist'
        GOTO 999
      ENDIF
      iw(ifwd0 +  1) = 1
      iw(ifwd0 +  2) = klstot(2) - kfrtot(1) + 1
      iw(ifwdx +  1) = 2
      iw(ifwdx +  2) = klstot(2) - kfrtot(1) + 1
      nch = 0
      DO 31 itk = kfrtot(1),klstot(2)
        nch = nch+1
C---Distance of closest approach
        rw(ifwd0 + 3 + (nch-1)*1 ) = dzero(nch)
C---dE/dx
        CALL qdedx(itk,4,rmass,rchar,ri,ns,tl,riexp,rsigm,ier)
        DO i = 1, 4
          IF ( ier .EQ. 0 ) THEN
            hyp(i) = (ri-riexp(i))/rsigm(i)
          ELSE
            hyp(i) = 32.767
          ENDIF
          ihyp(i)= NINT(ABS(hyp(i))*1000.)
          ihyp(i) = IAND(ihyp(i),32767)
          IF ( hyp(i) .GE. 0. )
     .        ihyp(i) = ihyp(i)+32768
        ENDDO
        iw(ifwdx + 3 + (nch-1)*2 ) =
     .        ihyp(1) + ISHFT(ihyp(2),16)
        iw(ifwdx + 4 + (nch-1)*2 ) =
     .        ihyp(3) + ISHFT(ihyp(4),16)
   31 CONTINUE
C
C  Rebuild a new and more complete bank NBIP (with numjet = 10)
C
      CALL bdrop(IW,'DBTGNBIP')
      CALL mdipbt
      CALL mdwrip (iret)
      knbip =  NSWAP('DBTG', 1,'NBIP',1)
C
C  Create a spurious bank PDLT if not saved on the MINI.
C
      jpdlt = IW(napdlt)
      IF ( jpdlt .LE. 0 ) THEN
        jpdlt = NBANK('PDLT',0,2)
        IW (jpdlt + lmhcol) = lpdlta
        IW (jpdlt + lmhlen) = 0
      ENDIF
C
C     Get QVSRCH Primary vertex
C
      BPOS(1) = QVX(KFREV)
      BPOS(2) = QVY(KFREV)
      BPOS(3) = QVZ(KFREV)

      BSIZ(1) = 0.020
      BSIZ(2) = 0.002
      BSIZ(3) = 2.0
C
      TSMR=.0000
      CALL QVSPVX(BPOS,BSIZ,TSMR,PVTX,EPVTX,IERR)
C
      nwords = 9
      ifwvx = nbank('FWVX',0,nwords)
      IF ( ifwvx .LE. 0 ) THEN
        WRITE (IW(6),*) 'Bank FWVX doesn''t exist'
        GOTO 999
      ENDIF
      iw(ifwvx +  1) = 6
      iw(ifwvx +  2) = 1
      rw(ifwvx +  3) =  pvtx(1)
      rw(ifwvx +  4) =  pvtx(2)
      rw(ifwvx +  5) =  pvtx(3)
      rw(ifwvx +  6) = epvtx(1)
      rw(ifwvx +  7) = epvtx(2)
      rw(ifwvx +  8) = epvtx(3)
      iw(ifwvx +  9) = ierr
C
C Get good track list for QVSRCH
C
      CALL qvsgtl1(pvtx,mgtk,ngtk,jgtk)
C
      nwords = 2 + 6 * (klstot(1)-kfrtot(1)+1)
      ifwfr = nbank('FWFR',0,nwords)
      IF ( ifwfr .LE. 0 ) THEN
        WRITE (IW(6),*) 'Bank FWFR doesn''t exist'
        GOTO 999
      ENDIF
C
      iw(ifwfr +  1) = 6
      iw(ifwfr +  2) = klstot(1)-kfrtot(1)+1
      DO itk = kfrtot(1), klstot(1)
        ich = itk-kfrtot(1)+1
        ifr = KTN(itk)
        ial = ifr + kfcht - 1
        RW(ifwfr + (ich-1)*6 + 3 ) = qfrfd0(ial)
        RW(ifwfr + (ich-1)*6 + 4 ) = qfrfz0(ial)
        RW(ifwfr + (ich-1)*6 + 5 ) = qfrfem(ial,4,4)
        RW(ifwfr + (ich-1)*6 + 6 ) = qfrfem(ial,5,4)
        RW(ifwfr + (ich-1)*6 + 7 ) = qfrfem(ial,5,5)
        IW(ifwfr + (ich-1)*6 + 8 ) = MIN(kfrtnv(ial), 2)
     &                             + MIN(kfrtni(ial), 8)*10
     &                             + MIN(kfrtnt(ial),21)*100
        DO jtk = 1, ngtk
          IF ( ial .EQ. jgtk(jtk) ) THEN
            IW(ifwfr + (ich-1)*6 + 8 ) =
     &      IW(ifwfr + (ich-1)*6 + 8 ) + 10000
          ENDIF
        ENDDO
      ENDDO
C
      nwords = 2 + 3 * (klstot(2)-kfrtot(2)+1)
      ifwf2 = nbank('FWF2',0,nwords)
      IF ( ifwf2 .LE. 0 ) THEN
        WRITE (IW(6),*) 'Bank FWF2 doesn''t exist'
        GOTO 999
      ENDIF
C
      iw(ifwf2 +  1) = 3
      nst = 0
      DO itk = kfrtot(2), klstot(2)
        ist = ksister(itk)
        ial = kalpha(ist)
        ifr = KTN(ial)
        IF ( ist .LE. klstot(1) ) THEN
          nst = nst+1
          RW(ifwf2 + (nst-1)*3 +  3 ) = qfrftl(ial)
          RW(ifwf2 + (nst-1)*3 +  4 ) = qfrfp0(ial)
          IW(ifwf2 + (nst-1)*3 +  5 ) = ist-kfrtot(1)+1
        ENDIF
      ENDDO
C
      iw(ifwf2 +  2) = nst
      nwords = 2 + 3*nst
      ifwf2 = nbank('FWF2',0,nwords)
C
      CALL bwrite(IW,lout,
     .'EVEHKEVHPTHRNBIPEJETPDLTFWEVFWPAFWIDFWCHFWD0FWDXFWVXFWFRFWF2')
      CALL bdrop(IW,'FWEVFWPAFWIDFWCHFWD0FWDXFWVXFWFRFWF2')
      IF ( LROWS(jpdlt) .EQ. 0 ) CALL bdrop(IW,'PDLT')
C
  999 RETURN
      END
#endif
