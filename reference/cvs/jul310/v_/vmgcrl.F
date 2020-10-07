      SUBROUTINE VMGCRL(IWAFR,MMOD)
C----------------------------------------------------------------------
C  - Make global cluster relations
C!   Author   :- A. Bonissent June 1995
C!
C  Establish relations between clusters in a wafer and global cluster
C  in a module
C
C!  IVIEW = view
C!  MMOD  = signed module number -24:+24
C?
C!======================================================================
#ifndef DOC
C
      SAVE NAVWRL,NAVDGC

C
C  Global includes
C
#include "vdmrjj.h"
#include "vdgcjj.h"
#include "vwrljj.h"
#include "vdxyjj.h"
#include "vdztjj.h"
#include "vrecon.h"
      INTEGER VMODIZ, VNRDIR, VIWFFW, VFWAFF
      INTEGER VRMMOD, VDYEAR, VJMODM, VCHNST
      INTEGER JSTRP(3)
C
C
C  BOS functions
C
      INTEGER NLINK,NBANK
C
      INTEGER ILC(3),KBNK(3),NRBK(3)
      INTEGER KVDMR(3),NRR(3)
#include "bcs.h"
      LOGICAL FIRST
      DATA FIRST /.TRUE./
#include "bmacro.h"
      IF(FIRST)THEN
         NAVWRL=NAMIND('VWRL')
         NAVDGC=NAMIND('VDGC')
         FIRST=.FALSE.
      ENDIF
      KVWRL=IW(NAVWRL)
      NVWRL=LROWS(KVWRL)
      IDIR = VNRDIR(IVIEW)
C
C     Say if we want to use bonding maps
C
      IFLBE=1
      IF(MCEVNT)IFLBE=0
C
C Decode wafer nb
C
      CALL VADEWA(IWAFR,ILAY,IZ,IFAC,IVIEW)
C
C Make JMOD from MMOD
C
      JMOD = VJMODM(MMOD)
C
      IZ = VRMMOD(MMOD,1,IVIEW)
C
C Encode readout module nb
C
      CALL VAENWA(IROMD,ILAY,IZ,IFAC,IVIEW)
C
C And apply channel to strip mapping
C
C First we set to 0 all strip nb for each data channel
C
      DO IVWRL=1,NVWRL
         DO IWA=1,3
            IW(KROW(KVWRL,IVWRL)+JVWRAS+IWA-1)=0
         ENDDO
      ENDDO
C
C  Fill the relation from data channel to readout strip for each fired strip
C
      DO IVWRL=1,NVWRL
         DO IWA=1,3
            ILLC = ITABL(KVWRL,IVWRL,JVWRLC+IWA-1)
            IF(ILLC.NE.0)THEN
               IRET = VCHNST(IFLBE,JMOD,IWA,IVIEW,IVWRL,IDATC,IBID)
               IW(KROW(KVWRL,IDATC)+JVWRAS+IWA-1)=IVWRL
            ENDIF
         ENDDO
      ENDDO
C
C
C Now, we have for each el. channel the R/O strip nb, and for
C each strip the cluster nb (was filled by VDXYZT)
C We simply need to transform into a list of relations between global
C clusters and wafer clusters.
C
      NREL=0
      NGC=0
      DO 30 IVWRL=1,NVWRL
         INCLU=0
         DO IWA=1,3
C Strip
            JS=ITABL(KVWRL,IVWRL,JVWRAS+IWA-1)
            IF(JS.NE.0)THEN
C Local cluster
              ILLC=ITABL(KVWRL,JS,JVWRLC+IWA-1)
C Replace strip by local cluster
              IW(KROW(KVWRL,IVWRL)+JVWRAS+IWA-1)=ILLC
              IF(ILLC.NE.0)THEN
C El. Channel pulse
                RW(KROW(KVWRL,IVWRL)+JVWRCP)=
     >                    RTABL(KVWRL,JS,JVWRSP+IWA-1)
                INCLU=1
              ENDIF
            ENDIF
         ENDDO
         IF(INCLU.EQ.1)THEN
            NGC=NGC+1
            IW(KROW(KVWRL,IVWRL)+JVWRGC)=NGC
         ENDIF
   30 CONTINUE
C Suppress duplicate relations
      DO IREL=1,NVWRL
        IGC=ITABL(KVWRL,IREL,JVWRGC)
        IF(IGC.NE.0)THEN
          DO IWA=1,3
           ILC(IWA)=ITABL(KVWRL,IREL,JVWRAS+IWA-1)
          ENDDO
          DO IRL2=IREL+1,NVWRL
             PULSG=RTABL(KVWRL,IREL,JVWRCP)
             IGLC=ITABL(KVWRL,IRL2,JVWRGC)
             IF(IGLC.NE.0)THEN
                ISAM=1
                IDIFF=1
                DO IWA=1,3
                 ILL=ITABL(KVWRL,IRL2,JVWRAS+IWA-1)
C Do not consider zeros
                 IF(ILL.NE.0)THEN
                   IF(ILL.EQ.ILC(IWA))THEN
                     IW(KROW(KVWRL,IRL2)+JVWRAS+IWA-1)=0
                     IDIFF=0
                   ELSE
                     ISAM=0
                   ENDIF
                 ENDIF
                ENDDO
                IF(ISAM.EQ.1)THEN
C All wafer clusters are identical, no need to keep this relation
C Transport the pulseheight and suppress global cluster
                  RW(KROW(KVWRL,IREL)+JVWRCP)=
     >                      PULSG+RTABL(KVWRL,IRL2,JVWRCP)
                  IW(KROW(KVWRL,IRL2)+JVWRGC)=0
                ELSEIF(IDIFF.EQ.0)THEN
C Global clusters have at least 1 local cluster in common(but not all)
C make them equal
                  IW(KROW(KVWRL,IRL2)+JVWRGC)=IGC
                ENDIF
             ENDIF
          ENDDO
        ENDIF
      ENDDO
C
C Remove zeros, and make sequential clust. nb.
C First make the list of final cluster numbers; use first clust. nb
C
C Reset
C
      DO IREL=1,NVWRL
        IW(KROW(KVWRL,IREL)+JVWRLC)=0
      ENDDO
C
      NGC=0
      DO IREL=1,NVWRL
        IGC=ITABL(KVWRL,IREL,JVWRGC)
        IF(IGC.NE.0.AND.ITABL(KVWRL,IGC,JVWRLC).EQ.0)THEN
          NGC=NGC+1
          IW(KROW(KVWRL,IGC)+JVWRLC)=NGC
        ENDIF
      ENDDO
C Replace global cluster nb by final nb and transport pulseheight
      NRF=0
      DO IREL=1,NVWRL
        IGC=ITABL(KVWRL,IREL,JVWRGC)
        IF(IGC.NE.0)THEN
          IGC=ITABL(KVWRL,IGC,JVWRLC)
          NRF=NRF+1
          IW(KROW(KVWRL,NRF)+JVWRGC)=IGC
          RW(KROW(KVWRL,NRF)+JVWRCP)=RTABL(KVWRL,IREL,JVWRCP)
          DO IWA=1,3
            IW(KROW(KVWRL,NRF)+JVWRAS+IWA-1)=
     >          ITABL(KVWRL,IREL,JVWRAS+IWA-1)
          ENDDO
        ENDIF
      ENDDO
C
C And finally, fill the POT banks
C First compute module nb (1 or 2)
      IRET = VFWAFF(IZ,IMOD,IWAF)
      IG=0
      DO IWAF=1,3
        IRET = VIWFFW(IMOD,IWAF,IWFF)
        CALL VAENWA(NRBK(IWAF),ILAY,IWFF,IFAC,1)
        NRR(IWAF)=NRBK(IWAF)+IVIEW-1
        IF(IVIEW.EQ.1)THEN
           KBNK(IWAF)=NLINK('VDZT',NRBK(IWAF))
        ELSE
           KBNK(IWAF)=NLINK('VDXY',NRBK(IWAF))
        ENDIF
        NCLU=LROWS(KBNK(IWAF))
        NDATA=NCLU*LVDMRA+LMHLEN
        CALL AUBOS('VDMR',NRR(IWAF),NDATA,KVDMR(IWAF),IGARB)
        IF(IGARB.EQ.2)THEN
           CALL RERROR('VMGCRL',2,'Error making BOS bank')
           GOTO 999
        ENDIF
        IF(IGARB.EQ.1)IG=1
C
C Fill miniheader
C
        IW(KVDMR(IWAF)+LMHCOL)=LVDMRA
        IW(KVDMR(IWAF)+LMHROW)=NCLU
      ENDDO
C
C Create/extend vdgc bank
C
      KVDGC=IW(NAVDGC)
      IF(KVDGC.EQ.0)THEN
         NOLD=0
      ELSE
         NOLD = LROWS(KVDGC)
      ENDIF
      NVDGC = NOLD+NGC
      NDATA = LMHLEN+NVDGC*LVDGCA
      CALL AUBOS('VDGC',0,NDATA,KVDGC,IGARB)
      IF(IGARB.EQ.2)THEN
         CALL RERROR('VMGCRL',2,'Error making BOS bank')
         GOTO 999
      ENDIF
      IF(IGARB.EQ.1)IG=1
      IW(KVDGC+LMHCOL)=LVDGCA
      IW(KVDGC+LMHROW)=NOLD
C
C Reaccess in case of garbage collection
C
      IF(IG.EQ.1)THEN
        DO IWAF=1,3
          KVDMR(IWAF)=NLINK('VDMR',NRR(IWAF))
          IF(IVIEW.EQ.1)THEN
             KBNK(IWAF)=NLINK('VDZT',NRBK(IWAF))
          ELSE
             KBNK(IWAF)=NLINK('VDXY',NRBK(IWAF))
          ENDIF
        ENDDO
      ENDIF
      IGBG=LROWS(KVDGC)
      DO IGC=1,NGC
        ILAS=LROWS(KVDGC)+1
        IW(KVDGC+LMHROW)=ILAS
        KLINE=KROW(KVDGC,ILAS)
        IW(KLINE+JVDGMD)=MMOD
        IW(KLINE+JVDGVI)=IVIEW
        RW(KLINE+JVDGPH)=0.
        IW(KLINE+JVDGNA)=0
      ENDDO
C
C Make new global cluster number : we just need to
C increase the global cluster nb in module by the
C number of clusters we start with
C
      DO IREL=1,NRF
        IGC=ITABL(KVWRL,IREL,JVWRGC) + IGBG
        QG = RTABL(KVDGC,IGC,JVDGPH) + RTABL(KVWRL,IREL,JVWRCP)
        RW(KROW(KVDGC,IGC)+JVDGPH)=QG
        DO IWAF=1,3
         ILLC=ITABL(KVWRL,IREL,JVWRAS+IWAF-1)
         IF(ILLC.NE.0)THEN
           IF(IVIEW.EQ.1)THEN
             QL=RTABL(KBNK(IWAF),ILLC,JVDZPH)
           ELSE
             QL=RTABL(KBNK(IWAF),ILLC,JVDXPH)
           ENDIF
           IW(KROW(KVDMR(IWAF),ILLC)+JVDMVD)=IGC
         ENDIF
        ENDDO
      ENDDO
  999 CONTINUE
      RETURN
      END
#endif
