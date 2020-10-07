      subroutine alevnum(is,idum1,idum2)
C -------------------------------------------
C  B.Bloch-Devaux May 2001
C  this routine writes out on a file the run and event number,
C  as provided by args, IF card EVNM exists
C  the file name is given in CARD: EVNM 'filename | ftype'
C  ftype is optionnal (CARDS used if not given)
C       is: 1   calling program is Kingal/Kinagain
C           2   calling program is Galeph
C           3   calling program is Julia
C       idum1   Run number
C       idum2   Evt number
C ------------------------------------------------
      character*10 atype, fdevi
      character*10 dtype, ddevi
      character*80 fname
      character*4 cname
      logical first,fok
      character*1 letter
      dimension letter(3)
      save fname,fok
      data cname,dtype,ddevi/'EVNM','CARDS','    '/
      data letter/'K','G','J'/
      data first /.true./
      if(first)then
        call acdarg(cname,dtype,ddevi,fname, atype, fdevi, ier)
        first = .false.
C   file is ok if given with type (ier=0) or default type (ier=1)
        fok = (ier.eq.0) .or. ((ier.eq.1).and.(fname.ne.' '))
        if (fok) 
     &     print *,' file',fname,' type',atype,' device',fdevi
      endif
      if(fok) then
        krun = idum1
        kevnt = idum2
        OPEN(UNIT=23,STATUS='UNKNOWN',FILE=fname)
        write(23, 900) krun,letter(is), kevnt
 900    format('Run ',I10,A3,' Evt ',I10)
        CLOSE (23)
      endif
      return
      end
