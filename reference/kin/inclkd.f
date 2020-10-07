      parameter( MPROC = 76 )
      integer ars1md,ars2md,ars3md,ars4md
      common /ckindi/ 
     .  irs1d(0:MPROC),ars1md(0:MPROC),irs2d(0:MPROC),ars2md(0:MPROC),
     .  irs3d(0:MPROC),ars3md(0:MPROC),irs4d(0:MPROC),ars4md(0:MPROC),
     .  ics3d(0:MPROC),ics5d(0:MPROC),iph6d(0:MPROC),icq3d(0:MPROC),
     .  iswpd(0:MPROC),idntd(0:MPROC),jtgmd(0:MPROC),kinmt(0:MPROC),
     .  iit1(0:MPROC),iit2(0:MPROC),incal(0:MPROC),kclmb(0:MPROC),
     .  kqedps(0:MPROC)
      common /ckindr/ swpmd(0:MPROC)
      common /kgrc4f/ jkclmb,jktype
