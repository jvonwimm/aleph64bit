      SUBROUTINE VFNDTH(IMOD,THRES)
C----------------------------------------------------------------------
C!  Get clustering thresholds for one module
CKEY VDET DBASE
C!
C!  Author         A. Bonissent 15-Jan-1994
C!
C!  ===========
C! Input : IMOD module number
C! Output : THRES thresholds in 2 views
C-----------------------------------------------------------------------
C
      SAVE VELGV,VDLCO,IVDTH
C
      DIMENSION THRES(*)
      INTEGER GTSTUP
      DIMENSION NBCLN(2),NVDCR(2),VDSLP(2),VDPAL(2),VDELC(2)
      DIMENSION MXCNO(2),MXCSI(2),VELGV(2),VDLCO(2),IOFSET(2),NBITSH(2)
      DIMENSION IVDTH(2)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JVDEEM=1,JVDEHM=2,JVDEET=3,JVDEHT=4,JVDEHE=5,JVDEHH=6,
     +          JVDEAV=7,JVDEDV=8,JVDEST=9,JVDEEC=11,JVDENL=13,
     +          JVDENC=14,JVDESN=16,JVDENE=18,JVDEPN=20,JVDEIC=22,
     +          JVDEMF=24,JVDEEG=25,LVDEPA=25)
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
      IF(FIRST)THEN
        FIRST=.FALSE.
        CALL VFNDEL
     $     (IMOD,NBCLN,NVDCR,VDSLP,VDPAL,VDELC,
     $      MXCSI,MXCNO,VELGV,VDLCO,IOFSET,NBITSH)
        LDBAS = JUNIDB(0)
        KVDEP = MDARD (IW,LDBAS,'VDEL',0)
        KVDEP=IW(NAMIND('VDEP'))
C         acquisition threshold in counts
        IVDTH(1) = ITABL(KVDEP,1,JVDEST)
        IVDTH(2) = ITABL(KVDEP,1,JVDEST+1)
      ENDIF
      THRES(1) = VELGV(1) * VDLCO(1) * IVDTH(1)
      THRES(2) = VELGV(2) * VDLCO(2) * IVDTH(2)
      RETURN
      END
