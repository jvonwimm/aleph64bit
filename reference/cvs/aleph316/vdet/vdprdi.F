      SUBROUTINE VDPRDI
C                            F.Forti             3/8/86
C! Print VDET digitization banks
C   Purpose: print contents of Minivertex digitization banks in a
C             readable format
C
C   Called by: VDDIGI                            from this .HLB
C      Calls : BPRNT                             from BOS77
C
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "runhjj.h"
      DATA NAVHLS /0/
#include "bmacro.h"
C ---------------------------------------------------------------
C
      IF (NAVHLS.EQ.0) THEN
        LOUT = IW(6)
        NAVHLS = NAMIND('VHLS')
        NAVPLH = NAMIND('VPLH')
        JDVPLS = 0
        NAVDTD = NAMIND('VDTD')
        NARUNH = NAMIND('RUNH')
        IRUN = IW(IW(NARUNH)+JRUNRN)
        IOFSET = 0
        IF (IRUN.LT.2000) IOFSET = 1024
      ENDIF
C
C   Check that the banks are present.
      JVHLS = IW(NAVHLS)
      IF (JVHLS.EQ.0) GOTO 10
      JVPLH = IW(NAVPLH)
      IF (JVPLH.EQ.0) GOTO 10
      CALL WBANK (IW,JDVPLS,LMHLEN+LROWS(JVPLH)*2,*980)
      IW(JDVPLS+1) = 1
      IW(JDVPLS+LMHROW) = LROWS(JVPLH)*2
      DO 1 JR =1,LROWS(JVPLH)
        KVPLS = KROW(JDVPLS,JR*2-1)
        IPLS  = ITABL(JVPLH,JR,1)
        IPLS1 = 0
        CALL MVBITS (IPLS,0,16,IPLS1,0)
        IPLS1 = IPLS1 - IOFSET
        IPLS2 = 0
        CALL MVBITS (IPLS,16,16,IPLS2,0)
        IPLS2 = IPLS2 - IOFSET
        RW(KVPLS+1)= REAL (IPLS1) /4.
        RW(KVPLS+2)= REAL (IPLS2) /4.
    1 CONTINUE
C
C   PRINT VHLS  and VPLH banks
C
      WRITE(LOUT,1000) LROWS(JVHLS),LCOLS(JVHLS),LROWS(JVPLH)
      I1PLS = 0
      KVPLS = JDVPLS+LMHLEN
      DO 100 IDI=1,LROWS(JVHLS)
        ICLU = ITABL(JVHLS,IDI,1)
        IADDR = MOD (ICLU,2**18)
        NCLU = ICLU / 2**18
        CALL VADESA(IADDR,ILAY,IZED,IPHI,IVIEW,ISTRI)
        WRITE (LOUT,1010) IDI,IADDR,ILAY,IZED,IPHI,IVIEW,ISTRI,NCLU
        WRITE (LOUT,1011) (RW(KVPLS+M),M=I1PLS+1,I1PLS+NCLU)
        I1PLS = I1PLS+NCLU
  100 CONTINUE
 1000 FORMAT(' +++VDPRDI+++  VHLS and VPLH banks printout'/
     &       '    Number of digits : ',I5,
     &       '  Number of words per digit : ',I2,
     &       '  Number of pulseheights : ',I5   //  10X,
     &'Digit       Address   Lay   Zed    Phi  View    Strip    Width')
 1010 FORMAT(10X,I5,4X,I10,4X,I2,4X,I2,4X,I3,4X,I2,4X,I5,4X,I5)
 1011 FORMAT(10X,12G10.3)
C
C   Print VDTD bank
C
   10 KTD1 = IW(NAVDTD)
      IF (KTD1.EQ.0) RETURN
      KTD2 = IW(KTD1-1)
      KTD3 = IW(KTD2-1)
      KTD4 = IW(KTD3-1)
C
      WRITE (LOUT,2000) LROWS(KTD1)
      DO 200 IT=1,LROWS(KTD1)
        IDTRK = ITABL(KTD1,IT,1)
        NDTRK = ITABL(KTD2,IT,1)
        KDTRK = KTD4 + LMHLEN + ITABL(KTD3,IT,1)
        WRITE(LOUT,2010) IDTRK, NDTRK,(IW(KDTRK+I),I=1,NDTRK)
  200 CONTINUE
 2000 FORMAT(' +++VDPRDI+++ VDTD bank printout'/
     &       '    Number of tracks contributing to digitizing : ',I5//
     &       10X,'Track   # of dig.',T30,'strip address list')
 2010 FORMAT(10X,I5,6X,I3,(T30,10(I7,2X)))
C
  980 CONTINUE
      CALL WDROP (IW,JDVPLS)
      END
#endif
