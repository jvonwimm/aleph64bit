C! print fname,ftype,fdevi
#if defined(ARDEB)
      LEND = MAX (LNBLNK (FDEVI),1)
      LENF = MAX (LNBLNK (FNAME),1)
      LENT = MAX (LNBLNK (ATYPE),1)
      WRITE (IW(6),*) ' PRFNAME ', SUBR,' LUN= ',LUN,' FNAME= ',
     &                FNAME(1:LENF),' ATYPE= ',ATYPE(1:LENT),' FDEVI= ',
     &                FDEVI(1:LEND)
#endif
