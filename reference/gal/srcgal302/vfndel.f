      SUBROUTINE VFNDEL (IMOD,NBCLN,NVDCR,VDSLP,VDPAL,VDELC,
     $                   MXCSI,MXCNO,VELGV,VDLCO,IOFSET,NBITSH)
C----------------------------------------------------------------------
C! Get electronic parameters for one module
CKEY VDET DBASE
C!
C!
C!  Author         A. Bonissent 15-Jan-1994
C!
C!  Description
C!  ===========
C! Input : IMOD module number
C! Output : NBCLN  number of noisy strips added to a cluster
C           NVDCR  number of strips contributing to correlated noise
C           VDSLP  noise slope in electron/pf
C           VDPAL  parallel noise in electrons
C           VDELC  electronic input capacitance in pf
C           VELGV  Gev equivalent to an electron
C           VDLCO  conversion factor from electron to counts
C           NBITSH Offset added when converting digitizings into integer
C           MXCNO  number of strips contributing to correlated noise
C           MXCSI  number of strips contributing to signal
C-----------------------------------------------------------------------
C
      SAVE NAVDEP,NAVPHO,IVSTP
C
      DIMENSION NBCLN(2),NVDCR(2),VDSLP(2),VDPAL(2),VDELC(2)
      DIMENSION MXCNO(2),MXCSI(2),VELGV(2),VDLCO(2),IOFSET(2),NBITSH(2)
      PARAMETER(JVDEEM=1,JVDEHM=2,JVDEET=3,JVDEHT=4,JVDEHE=5,JVDEHH=6,
     +          JVDEAV=7,JVDEDV=8,JVDEST=9,JVDEEC=11,JVDENL=13,
     +          JVDENC=14,JVDESN=16,JVDENE=18,JVDEPN=20,JVDEIC=22,
     +          JVDEMF=24,JVDEEG=25,LVDEPA=25)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      LOGICAL FIRST
      DATA FIRST /.TRUE./
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
      IF (FIRST) THEN
        FIRST = .FALSE.
        NAVDEP = NAMIND('VDEP')
        NAVPHO = NAMIND('VPHO')
      ENDIF
      KVPHO = IW(NAVPHO)
      IOFSET(1)=1024
      IF (KVPHO.NE.0) IOFSET(1) = ITABL(KVPHO,1,1)
      IOFSET(2)=IOFSET(1)
      KVDEP=IW(NAVDEP)
C         number of noisy strips added to a cluster
      NBCLN(1) = ITABL(KVDEP,1,JVDENL)
      NBCLN(2) = ITABL(KVDEP,1,JVDENL)
C         number of strips contributing to correlated noise
      NVDCR(1) = ITABL(KVDEP,1,JVDENC)
      NVDCR(2) = ITABL(KVDEP,1,JVDENC)
C         noise slope in electron/pf
      VDSLP(1) = RTABL(KVDEP,1,JVDENE)
      VDSLP(2) = RTABL(KVDEP,1,JVDENE)
C         parallel noise in electrons
      VDPAL(1) = RTABL(KVDEP,1,JVDEPN)
      VDPAL(2) = RTABL(KVDEP,1,JVDEPN)
C         electronic input capacitance in pf
      VDELC(1) = RTABL(KVDEP,1,JVDEIC)
      VDELC(2) = RTABL(KVDEP,1,JVDEIC)
C         Gev equivalent to an electron
      VELGV(1) = RTABL(KVDEP,1,JVDEEG)
      VELGV(2) = VELGV(1)
C         conversion factor from electron to counts
      VDLCO(1) = RTABL(KVDEP,1,JVDEEC)
      VDLCO(2) = RTABL(KVDEP,1,JVDEEC+1)
C
      NBITSH(1) = ITABL(KVDEP,1,JVDEMF)
      NBITSH(2) = NBITSH(1)
C         number of strips contributing to correlated noise
      MXCNO(1) = ITABL(KVDEP,1,JVDENC)
      MXCNO(2) = ITABL(KVDEP,1,JVDENC)
C
C         number of strips contributing to signal
      MXCSI(1) = ITABL(KVDEP,1,JVDESN)
      MXCSI(2) = ITABL(KVDEP,1,JVDESN+1)
      RETURN
      END
