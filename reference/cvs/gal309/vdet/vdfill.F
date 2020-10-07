      SUBROUTINE VDFILL(TYPE,IARG)
C!----------------------------------------------------------------------
C! Fill vdet histograms
CKEY VDET HISTO
C!
C!  Author         A. Bonissent 15-Jan-1994
C!
C!  Description
C!  ===========
C!  Fill Vdet histograms
C!  and accumulate statistics
C!
C! Input :   TYPE : type of operations to be done. Can be :
C!                  'INIT'
C!                  'WAFER'
C!                  'MODULE'
C!                  'EVENT'
C!                  'SUMMARY'
C!
C!           IARG : Operation dependent input arguments
C!
C-----------------------------------------------------------------------
C
#ifndef DOC
      SAVE NAVTRS,NAVDHT,NAVDLH,NAVWCX
      SAVE NFIRS,NVDHE,IVDHE,NVDHW,IVDHW,NVDHM,IVDHM
      SAVE NVDSZ,IVDSZ,NVDSP,IVDSP
      SAVE VDLGV,VDLCO
C
      DIMENSION NBCLN(2),NVDCR(2),VDSLP(2),VDPAL(2),VDELC(2)
      DIMENSION MXCNO(2),MXCSI(2),VELGV(2),VDLCO(2),IOFSET(2),NBITSH(2)
      DIMENSION THRES(2),NAVWCX(2),NFIRV(2),NSTRH(2),QSTRH(2)
      DIMENSION XI(3),XJ(3),ERPH(2)
      DIMENSION IARG(*)
      CHARACTER*(*)TYPE
      LOGICAL FIRST
      DATA FIRST /.TRUE./
#include "bcs.h"
#include "vwc1jj.h"
#include "vtrsjj.h"
#include "vdhtjj.h"
#include "vdlhjj.h"
#include "jobcom.h"
#include "bmacro.h"
      IF(FIRST)THEN
        FIRST=.FALSE.
        NAVTRS = NAMIND('VTRS')
        NAVDHT = NAMIND('VDHT')
        NAVDLH = NAMIND('VDLH')
        NAVWCX(1)=NAMIND('VWC1')
        NAVWCX(2)=NAMIND('VWC2')
        CALL VFNDEL
     $   (1,NBCLN,NVDCR,VDSLP,VDPAL,VDELC,
     $    MXCSI,MXCNO,VELGV,VDLCO,IOFSET,NBITSH)
      ENDIF
      KVDHT=IW(NAVDHT)
      IF(TYPE.EQ.'INIT')THEN
         NFIRS = 0
         NVDHE=0
         IVDHE=0
         NVDHW=0
         IVDHW=0
         NVDHM=0
         IVDHM=0
         NVDSZ=0
         IVDSZ=0
         NVDSP=0
         IVDSP=0
      ELSEIF(TYPE.EQ.'WAFER')THEN
         KVDLH=IW(NAVDLH)
         IF(KVDLH.NE.0)THEN
            IMIN=IARG(2)
            IMAX=IARG(3)
            IVDHW=IVDHW+IMAX-IMIN+1
            NVDHW=NVDHW+1
            DO 30 IVDLH=IMIN,IMAX-1
            DMIN=1000.
            DO 31 IDIM=1,3
   31       XI(IDIM)=(RTABL(KVDLH,IVDLH,JVDLXI+IDIM-1)+
     >                RTABL(KVDLH,IVDLH,JVDLXO+IDIM-1))/2.
            DO 32 JVDLH=IVDLH+1,IMAX
            DO 33 IDIM=1,3
   33       XJ(IDIM)=(RTABL(KVDLH,JVDLH,JVDLXI+IDIM-1)+
     >                RTABL(KVDLH,JVDLH,JVDLXO+IDIM-1))/2.
            DIST=VDIST(XI,XJ,3)
            DMIN=AMIN1(DIST,DMIN)
   32       CONTINUE
            IF(FHISJO(1))CALL HF1(105,DMIN,1.)
   30       CONTINUE
         ENDIF
      ELSEIF(TYPE.EQ.'MODULE')THEN
         IVDHM=IVDHM+IVDHW
         NVDHM=NVDHM+1
         IF(FHISJO(1))CALL HF1(102,FLOAT(IVDHW),1.)
         IVDHW=0
         IMOD=IARG(1)
         CALL VFNDTH(IMOD,THRES)
         DO 10 IV=1,2
         KVWCX=IW(NAVWCX(IV))
         NFIRV(IV)=0
         IF(KVWCX.NE.0)THEN
            NVWCX=LROWS(KVWCX)
            DO 11 IVWCX=1,NVWCX
               PULS = RTABL(KVWCX,IVWCX,JVWCCC)
               IF(PULS.NE.0)THEN
                  NFIRS=NFIRS+1
                  NFIRV(IV)=NFIRV(IV)+1
               ENDIF
               IF(PULS.GT.THRES(IV))THEN
                  IF(FHISJO(1))CALL HF1(106,PULS,1.)
               ENDIF
   11       CONTINUE
            IF(FHISJO(1))THEN
               IF(NFIRV(1).NE.0)CALL HF1(104,FLOAT(NFIRV(1)),1.)
               IF(NFIRV(2).NE.0)CALL HF1(110,FLOAT(NFIRV(2)),1.)
            ENDIF
         ENDIF
   10    CONTINUE
      ELSEIF(TYPE.EQ.'EVENT')THEN
         KVDHT=IW(NAVDHT)
         IF(KVDHT.NE.0)THEN
            NVDHT=LROWS(KVDHT)
            DO 20 IVDHT=1,NVDHT
               PULSH=RTABL(KVDHT,IVDHT,JVDHER)
               IF(FHISJO(1))CALL HF1(107,PULSH,1.)
   20       CONTINUE
            NVDHE =NVDHE+1
            IVDHE = IVDHE+NVDHT
            IF(FHISJO(1))THEN
               CALL HF1(101,FLOAT(NVDHT),1.)
               CALL HF1(103,FLOAT(NFIRS),1.)
            ENDIF
C Reset Nb fired strips in the event
            NFIRS = 0
         ENDIF
         KVTRS = IW(NAVTRS)
         IF(KVDHT.NE.0.AND.KVTRS.NE.0)THEN
C
C Compute number of fired strips per hit in each view
C and fraction of charge collected
C
            NVDHT=LROWS(KVDHT)
            NVTRS=LROWS(KVTRS)
            DO 40 IVDHT=1,NVDHT
            EREL=(RTABL(KVDHT,IVDHT,JVDHER))
C
C Convert into pulse height
C
            DO IV=1,2
               QSTRH(IV)=0.
               NSTRH(IV)=0
               ERPH(IV) = EREL*NBITSH(IV)/(VELGV(IV)*VDLCO(IV))
            ENDDO
            DO 41 IVTRS=1,NVTRS
            IF(IVDHT.EQ.ITABL(KVTRS,IVTRS,JVTRHT))THEN
               ICODE=ITABL(KVTRS,IVTRS,JVTRAD)
               CALL VUNADD(ICODE,IBID,IL,IZ,IP,IV,ISTR)

               NSTRH(IV) = NSTRH(IV)+1
               QSTRH(IV) = QSTRH(IV)+RTABL(KVTRS,IVTRS,JVTRCH)
            ENDIF
   41       CONTINUE
            NVDSZ=NVDSZ+1
            NVDSP=NVDSP+1
            IVDSZ=IVDSZ+NSTRH(1)
            IVDSP=IVDSP+NSTRH(2)
            IF(FHISJO(1))THEN
               IF(NSTRH(1).NE.0)THEN
                  CALL HF1(108,FLOAT(NSTRH(1)),1.)
                  CALL HF1(111,QSTRH(1)/ERPH(1),1.)
               ENDIF
               IF(NSTRH(2).NE.0)THEN
                  CALL HF1(109,FLOAT(NSTRH(2)),1.)
                  CALL HF1(111,QSTRH(2)/ERPH(2),1.)
               ENDIF
            ENDIF
   40       CONTINUE
         ENDIF
         IF (FDEBJO.AND.IPRIJO(1).NE.0) CALL VDPRDI
      ELSEIF(TYPE.EQ.'SUMMARY')THEN
         IARG(1)=NVDHE
         IARG(2)=IVDHE
         IARG(3)=NVDHM
         IARG(4)=IVDHM
         IARG(5)=NVDSZ
         IARG(6)=IVDSZ
         IARG(7)=NVDSP
         IARG(8)=IVDSP
      ENDIF
      RETURN
      END
#endif
