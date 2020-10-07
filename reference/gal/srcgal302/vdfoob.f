      SUBROUTINE VDFOOB(IMOD,NHMAX)
C!----------------------------------------------------------------------
CKEY VDET DIGITIZE
C!
C! Reformat output banks according to on-line conventions
C!
C!  Author         A. Bonissent 15-Jan-1994
C
C     Input : IMOD : Module number,
C             NHMAX number of hits in this module
C!
C!  Description
C!  ===========
C! Format depends on year
C!
C!
C! Input :  VCLU, VWC1, VWC2  banks
C!
C! Output : VPLH, VHLS  banks
C!
C-----------------------------------------------------------------------
C
      SAVE NAVPLH,NAVHLS,NAVCLU,NAVWCX,NAVTRS
      SAVE NPH,IR
C
      DIMENSION NAVWCX(2)
      PARAMETER (NSPH=12)
      DIMENSION NBCLN(2),NVDCR(2),VDSLP(2),VDPAL(2),VDELC(2)
      DIMENSION MXCNO(2),MXCSI(2),VELGV(2),VDLCO(2),IOFSET(2),NBITSH(2)
      PARAMETER(JVCLVI=1,JVCLFS=2,JVCLCS=3,LVCLUA=3)
      PARAMETER(JVWCCC=1,JVWCSN=2,JVWCVT=3,LVWC1A=3)
      PARAMETER(JVHLHC=1,LVHLSA=1)
      PARAMETER(JVPLPC=1,LVPLHA=1)
      PARAMETER(JVTRAD=1,JVTRCH=2,JVTRHT=3,JVTRVT=4,LVTRSA=4)
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
C
      IF(FIRST)THEN
        FIRST=.FALSE.
        NAVPLH=NAMIND('VPLH')
        NAVHLS=NAMIND('VHLS')
        NAVCLU=NAMIND('VCLU')
        NAVWCX(1)=NAMIND('VWC1')
        NAVWCX(2)=NAMIND('VWC2')
        NAVTRS=NAMIND('VTRS')
      ENDIF
      CALL VFNDEL
     $   (IMOD,NBCLN,NVDCR,VDSLP,VDPAL,VDELC,
     $    MXCSI,MXCNO,VELGV,VDLCO,IOFSET,NBITSH)
C
C If output banks do not exist, create them
C
C First, clusters bank
C
      IF(IW(NAVHLS).EQ.0)THEN
         NROWS = NHMAX
         NDATA = NROWS*LVHLSA+LMHLEN
         CALL ALBOS('VHLS',0,NDATA,KVHLS,IGARB)
         IW(KVHLS+LMHCOL)=LVHLSA
         IW(KVHLS+LMHROW)=0
      ENDIF
C
C Now, strips bank
C
      IF(IW(NAVPLH).EQ.0)THEN
         NROWS = NHMAX*NSPH
         NDATA = NROWS*LVPLHA+LMHLEN
         CALL ALBOS('VPLH',0,NDATA,KVPLH,IGARB)
         NPH = 0
         IW(KVPLH+LMHCOL)=LVPLHA
         IW(KVPLH+LMHROW)=0
      ENDIF
C
      KVHLS=IW(NAVHLS)
      KVCLU=IW(NAVCLU)
      NVCLU=LROWS(KVCLU)
C
C Check that there is enough space in clusters bank
C
      IF(LFRROW(KVHLS).LT.NVCLU)THEN
         NDATA = (LROWS(KVHLS)+NVCLU)*LVHLSA+LMHLEN
         CALL ALBOS('VHLS',0,NDATA,KVHLS,IGARB)
      ENDIF
      KVCLU=IW(NAVCLU)
      NTS = 0
      DO 30 ICLU=1,NVCLU
      NTS = NTS + ITABL(KVCLU,ICLU,JVCLCS)
   30 CONTINUE
      KVPLH=IW(NAVPLH)
C
C Check that there is enough space in strips bank
C
      IF(LFRROW(KVPLH).LT.NTS)THEN
         NDATA = ((NPH+NTS+1)/2)*LVPLHA+LMHLEN
         CALL ALBOS('VPLH',0,NDATA,KVPLH,IGARB)
      ENDIF
      KVCLU=IW(NAVCLU)
      KVHLS=IW(NAVHLS)
      KVPLH=IW(NAVPLH)
      DO 10 ICLU=1,NVCLU
      IV = ITABL(KVCLU,ICLU,JVCLVI)
      IFS = ITABL(KVCLU,ICLU,JVCLFS)
      NBS = ITABL(KVCLU,ICLU,JVCLCS)
      KVWCX = IW(NAVWCX(IV))
C
C Change strip number and get readout direction
C according to Julia convention and
C make encoded address and fill row in cluster bank
C
      CALL VAENCL(IWRD,IMOD,IV,IFS,NBS,IFSJ,IRD)
      NCLU=LROWS(KVHLS)+1
      IW(KVHLS+LMHROW)=NCLU
      IW(KROW(KVHLS,NCLU)+JVHLHC)=IWRD
C
      DO 20 IST=1,NBS
      IGS = IFSJ+(IST-1)*IRD
      PULSH = RTABL(KVWCX,IGS,JVWCCC)/(VELGV(IV)*VDLCO(IV))
      NPH = NPH+1
      JPLS = MAX (INT (NBITSH(IV) * PULSH) + IOFSET(IV), 0)
C
C In case the pulse height is too large, make
C sure that what we have makes sense
C
      JMAX=2**16-1
      IF(JPLS.GT.JMAX)JPLS=JMAX
        IF (MOD(NPH,2) .NE. 0)THEN
          IR = (NPH+1)/2
          KVP = KROW (KVPLH,IR)
C
C  Packing is done according to the IBM I*2 convention, with the first
C  data word is packed into the upper 16 bits, the second in the lower
C
          IW(KVPLH+LMHROW)=LROWS(KVPLH)+1
          IW(KVP+1) = 0
          CALL MVBITS (JPLS,0,16,IW(KVP+1),16)
        ELSE
          KVP = KROW (KVPLH,IR)
          CALL MVBITS (JPLS,0,16,IW(KVP+1),0)
        ENDIF
   20 CONTINUE
   10 CONTINUE
  999 RETURN
      END
