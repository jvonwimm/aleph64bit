c
c A.Valassi 14/06/2001
c - first version copied from yfsww release (demo.koralw directory)
c A.Valassi 15/06/2001
c - set output unit to 6
c - cleanup and rewrite in implicit none
c A.Valassi 21/06/2001
c - input parameters read from FIFO file
c A.Valassi 22/06/2001
c - add CPU time and event monitoring
c

******************************************************************************
* Program to run YFSWW3 in a re-weighting mode
******************************************************************************
* Calls the following routines from the YFSWW library:
* - YFSWW_ReaDataX (read the xpar array from disk)
* - YFSWW_Initialize (initializes YFSWW)
* - YFSWW_WtNL (computes the NL weight for the event passed as input)
* - dumpd (dumps the current event)
******************************************************************************

      PROGRAM MAIN
      IMPLICIT none
c Read YFSWW parameters
      CHARACTER*80 DiskFile
      INTEGER ireset
      INTEGER npar
      PARAMETER (npar=10000)
      DOUBLE PRECISION xpar(npar) ! array for input parameters
c Read 4vectors from disk
      INTEGER i_disk
      INTEGER iflav(4), nphot
      DOUBLE PRECISION pf1(4), pf2(4), pf3(4), pf4(4), phot(100,4)
      DOUBLE PRECISION wtkarl
      INTEGER iend
c Write weights to disk
      INTEGER i_writ_wt
      INTEGER iwt
      DOUBLE PRECISION wtset(100), wt_nl
      DOUBLE PRECISION YFSWW_WtNL
c CPU and event monitors
      INTEGER nrec, nrew
      REAL*4 telapsed, tlost, tinit, trew1, trewt
C Welcome and byebye
      CHARACTER*200 chhost
      CHARACTER*200 chos
      INTEGER lnblnk
      INTEGER idate, itime
C
C Welcome
C
      WRITE (6,*)
     &     'Welcome to YWrewt: ',
     &     'the YFSWW3-based O(alpha) reweighting for KRLW03!'
      CALL getenv('HOST',chhost)
      CALL getenv('OS',chos)
      CALL datime(idate,itime)
      WRITE (6,'(a,1x,i6,1x,i4)') 
     &     'Execution on '//chhost(1:lnblnk(chhost))
     &     //' under '//chos(1:lnblnk(chos))//' starting on '
     &     //'[yymmdd] [hhmm]', idate, itime
C
C Initialise CPU and event monitors
C
      CALL timed(telapsed)
      nrec  = 0
      nrew  = 0
      tinit = 0
      tlost = 0
      trew1 = 0
      trewt = 0
C
C Read xpar array from FIFO file
C
      CALL reader_xpar(npar, xpar, iend)
      IF (iend.EQ.1) THEN       ! end of data, program stops
        WRITE(6,*)
     &       'YFSWW3: End of xpar data detected, program stops'
        GOTO 999
      ENDIF
      xpar(1082) = 3            ! Y: read 4vectors from disk (format 3)
      xpar(1086) = 1            ! Y: write weights to disk (only wtext)
      xpar(1087) = 0            ! Y: do not write 4vectors to disk
      xpar(1088) = 0            ! Y: do not read weights from disk
C
C YFSWW initialization
C
      CALL YFSWW_Initialize (xpar)
C
C Main loop for matrix element weight calculation
C
      i_disk    = nint(xpar(1082)) ! (=3) Y: read 4vectors from disk (format 3)
      i_writ_wt = nint(xpar(1086)) ! (=1) Y: write weights to disk (only wtext)
      DO iwt = 1, 100
         wtset(iwt) = 0d0
      ENDDO
c Read events from disk
      CALL timed(telapsed)
      tinit = tinit + telapsed
 100  CONTINUE
      CALL reader_4v
     &     (i_disk,iflav,pf1,pf2,pf3,pf4,phot,nphot,wtkarl,iend)
      nrec = nrec + 1
c Compute weight and write it out
      IF     (iend.EQ.1) THEN   ! end of data, program stops
        GOTO 999
      ELSEIF (iend.EQ.0) THEN   ! good event: compute weight and save it
        nrew = nrew + 1
        CALL timed(telapsed)
        tlost = tlost + telapsed
        wt_nl = YFSWW_WtNL (iflav, pf1, pf2, pf3, pf4, phot, nphot)
        CALL timed(telapsed)
        IF (nrew.EQ.1) THEN
          trew1 = trew1 + telapsed
        ELSE
          trewt = trewt + telapsed
        ENDIF
        CALL writer_wt (i_writ_wt, wtset, wt_nl)
        IF (nrew.LE.3) CALL dumpd(6)
      ELSEIF (iend.EQ.-1) THEN   ! rejected event: skip it
        wt_nl = 0d0
        CALL writer_wt (i_writ_wt, wtset, wt_nl)
      ENDIF
      IF (mod(nrec,10**INT(log(1.*nrec)/log(10.))).EQ.0) THEN
        WRITE(6,*)   'YFSWW3: Received event number ', nrec
        WRITE(6,200) 
     &       'Net time (no init) so far in reweighting [sec] = ', 
     &       trewt
      ENDIF
      GOTO 100
c Stop the program
 999  CONTINUE
      WRITE(6,*) 'YFSWW3: End of 4vector data detected'
      WRITE(6,*) 'Number of events received (including EOF): ', nrec 
      WRITE(6,*) 'Number of events reweighted:               ', nrew 
      CALL timed(telapsed)
      tlost = tlost + telapsed
      WRITE(6,200) 'Time spent in initialisation          [sec] = ', 
     &     tinit
      WRITE(6,200) 'Time spent in reweighting 1st evt     [sec] = ', 
     &     trew1
      WRITE(6,200) 'Time spent in reweighting next evts   [sec] = ', 
     &     trewt
      WRITE(6,200) 'Time spent in overhead and I/O        [sec] = ', 
     &     tlost
      WRITE(6,200) 'YFSWW3: Total time spent              [sec] = ', 
     &     tinit+trew1+trewt+tlost
      IF (nrew.GT.0) 
     &WRITE(6,200) 'YFSWW3: Total time per rewgtd evt     [sec] = ', 
     &     (tinit+trew1+trewt+tlost)/nrew
      IF (nrew.GT.0) 
     &WRITE(6,200) 'YFSWW3: Total net time per rewgtd evt [sec] = ', 
     &     trewt/nrew
 200  FORMAT (1x, a, f20.3)
      WRITE(6,*) 'YFSWW3: Program stops.. bye-bye!'
      CALL datime(idate,itime)
      WRITE (6,'(a,1x,i6,1x,i4)') 
     &     'Execution on '//chhost(1:lnblnk(chhost))
     &     //' under '//chos(1:lnblnk(chos))//' ending on '
     &     //'[yymmdd] [hhmm]', idate, itime
      STOP
      END	

******************************************************************************

      SUBROUTINE reader_4v
     &     (i_read,ipdg,dbp1,dbp2,dbp3,dbp4,sphot,nphot,wtkarl,iend)
      IMPLICIT none
      INTEGER i_read, ipdg(4), nphot, iend
      DOUBLE PRECISION dbp1(4),dbp2(4),dbp3(4),dbp4(4),sphot(100,4)
      DOUBLE PRECISION wtkarl
      DOUBLE PRECISION photo(4)
      DOUBLE PRECISION p1(4),p2(4),p3(4),p4(4)
      INTEGER ip(4)
      CHARACTER char1
      INTEGER iphot, j4
      INTEGER init
      SAVE    init
      DATA    init /1/
c initialize
      IF(i_read.EQ.0) RETURN
      IF(init.EQ.1) THEN
         OPEN(22,file='4vect.data.in')
         init = 0
      ENDIF
c read flag for next event
      iend   = 0
      wtkarl = 1d0
      IF     (i_read.EQ.3) THEN
        READ(22,101,end=12) char1
        IF     (char1.EQ.'E') THEN ! end of run requested
          GOTO 12
        ENDIF
      ELSEIF (i_read.EQ.4) THEN
        READ(22,101,end=12) char1
        IF     (char1.EQ.'E') THEN ! end of run requested
          GOTO 12
        ELSEIF (char1.EQ.'R') THEN ! rejected event
          DO j4 = 1, 4
            dbp1(j4) = 0d0
            dbp2(j4) = 0d0
            dbp3(j4) = 0d0
            dbp4(j4) = 0d0
          ENDDO
          nphot  = 0
          iend   = -1
          wtkarl = 0d0
          RETURN
        ENDIF
      ENDIF
c read 4vectors or weight for the event
      READ(22,102,end=12) ip
      READ(22,'(4e26.18)') p1
      READ(22,'(4e26.18)') p2
      READ(22,'(4e26.18)') p3
      READ(22,'(4e26.18)') p4
      READ(22,103) nphot
      DO iphot = 1, nphot
        READ(22,'(4e26.18)') photo
        DO j4 = 1, 4
          sphot(iphot,j4) = photo(j4)
        ENDDO
      ENDDO
      IF(i_read.EQ.4) THEN
         READ(22,'(4e26.18)') wtkarl
      ENDIF
      CALL swapper(ip,p1,p2,p3,p4,ipdg,dbp1,dbp2,dbp3,dbp4)
      RETURN
 12   CONTINUE
      iend = 1                  ! end of file detected
 101  FORMAT (1x,a1)
 102  FORMAT (1x,4(i5,1x))
 103  FORMAT (1x,i20)
      RETURN
      END

******************************************************************************

      SUBROUTINE swapper(ipdg,p1,p2,p3,p4,ip,dbp1,dbp2,dbp3,dbp4)
      IMPLICIT none
      INTEGER ipdg(4)
      DOUBLE PRECISION p1(4),p2(4),p3(4),p4(4)
      INTEGER ip(4)
      DOUBLE PRECISION dbp1(4),dbp2(4),dbp3(4),dbp4(4)
      INTEGER i4
c WW order: do not swap
      IF ( ipdg(1).NE.-ipdg(2) .OR.
     &     ipdg(3).NE.-ipdg(4) ) THEN ! no swap
        DO i4=1,4
          ip(i4)=ipdg(i4)
          dbp1(i4)=p1(i4)
          dbp2(i4)=p2(i4)
          dbp3(i4)=p3(i4)
          dbp4(i4)=p4(i4)
        ENDDO
        RETURN
      ENDIF
c ZZ order: swap to WW order
      IF(  (ipdg(1).EQ.2 .AND. ipdg(3).EQ.3)   .OR.
     &     (ipdg(1).EQ.2 .AND. ipdg(3).EQ.5)   .OR.
     &     (ipdg(1).EQ.4 .AND. ipdg(3).EQ.5)   .OR.
     &     (ipdg(1).EQ.4 .AND. ipdg(3).EQ.1) ) THEN
        ip(1)=ipdg(3)
        ip(2)=ipdg(2)
        ip(3)=ipdg(1)
        ip(4)=ipdg(4)
        DO i4=1,4
          dbp1(i4)=p3(i4)
          dbp2(i4)=p2(i4)
          dbp3(i4)=p1(i4)
          dbp4(i4)=p4(i4)
        ENDDO
c ZZ order: swap to WW order
      ELSEIF((ipdg(1).EQ.1 .AND. ipdg(3).EQ.4)   .OR.
     &       (ipdg(1).EQ.3 .AND. ipdg(3).EQ.2)   .OR.
     &       (ipdg(1).EQ.5 .AND. ipdg(3).EQ.2)   .OR.
     &       (ipdg(1).EQ.5 .AND. ipdg(3).EQ.4) ) THEN
        ip(1)=ipdg(1)
        ip(2)=ipdg(4)
        ip(3)=ipdg(3)
        ip(4)=ipdg(2)
        DO i4=1,4
          dbp1(i4)=p1(i4)
          dbp2(i4)=p4(i4)
          dbp3(i4)=p3(i4)
          dbp4(i4)=p2(i4)
        ENDDO
c WW order: do not swap
      ELSE
        DO i4=1,4
          ip(i4)=ipdg(i4)
          dbp1(i4)=p1(i4)
          dbp2(i4)=p2(i4)
          dbp3(i4)=p3(i4)
          dbp4(i4)=p4(i4)
        ENDDO
      ENDIF
      RETURN
      END

******************************************************************************

      SUBROUTINE writer_wt(i_writ,wtset,wtext)
      IMPLICIT none
      INTEGER i_writ
      DOUBLE PRECISION wtset(100), wtext
      INTEGER i9
      INTEGER init
      SAVE    init
      DATA    init /1/
      IF (i_writ.EQ.0) RETURN
      IF (init.EQ.1) THEN
        OPEN(23,file='wtext.data.out')
        INIT=0
      ENDIF
      IF     (i_writ.GE.-2 .AND. i_writ.LE.2) THEN
        WRITE(23,'(e26.18)') wtext
      ELSEIF (i_writ.LE.-3  .OR. i_writ.GE.3) THEN
        DO i9=1,9
          WRITE(23,'(9e26.18)') wtset(i9)
        ENDDO
      ENDIF
      CALL flush(23)
      END


******************************************************************************

      SUBROUTINE reader_xpar (npar, xpar, iend)
      IMPLICIT none
      INTEGER npar, iend
      DOUBLE PRECISION xpar(*)
      INTEGER ipar
c open file
      WRITE (6,*) '_READER_XPAR starting'
      OPEN(22,file='4vect.data.in')
c read array from file
      iend = 0
      DO ipar = 1, npar
        READ(22,100,end=12) xpar(ipar)
*       WRITE (6,*) ipar, xpar(ipar)
      END DO
 100  FORMAT (1x,e26.18)
      GOTO 13
 12   CONTINUE
      iend = 1
 13   CONTINUE
      WRITE (6,*) '_READER_XPAR exiting'
      CLOSE (22)
      RETURN
      END

******************************************************************************
