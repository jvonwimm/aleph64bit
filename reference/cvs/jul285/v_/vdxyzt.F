      SUBROUTINE VDXYZT
C----------------------------------------------------------------------
C!  - creates the banks VDXY,VDZT from the banks VFPH,VFHL,VFLG
C!   Author   :- D. Brown           18-SEP-1991
C!
C!  Modified 16-1-94 by Dave Brown for the VDET upgrade
C!  Modified March 1995 A. Bonissent, M. Thulasidas
C!                 reorganise and debug
C! Modified June 1995 handle Vdet95, multiplexing was not foreseen
C! Modified November 1996 A. Bonissent, keep track of strips which 
C!                                 belong to two clusters (after splitting)
C?
C!======================================================================
#ifndef DOC
C      IMPLICIT NONE
C
C  Global includes
C
#include "vwrljj.h"
#include "vdflgs.h"
#include "vdqflg.h"
#include "vcsgjj.h"
#include "vrecon.h"
#include "vprtnc.h"
C      INTEGER JVDXXC,JVDXYC,JVDXUC,JVDXSX,JVDXSY,JVDXSU
C      INTEGER JVDXPH,JVDXQF,JVDXNA,JVDXIP,JVDXIW
C      INTEGER JVDXIH,LVDXYA
#include "vdxyjj.h"
C      INTEGER JVDZZC,JVDZWC,JVDZSZ,JVDZSW,JVDZPH,JVDZQF
C      INTEGER JVDZNA,JVDZIP,JVDZIW,JVDZIH,LVDZTA
#include "vdztjj.h"
C
C These are here only because we access the banks. But the hac
c parameters
C are not used in this routine
C
C      INTEGER JVFHHA,LVFHLA
#include "vfhljj.h"
C      INTEGER JVFPPH,LVFPHA
#include "vfphjj.h"
C      INTEGER JVFLFG,LVFLGA
#include "vflgjj.h"
#include "vspljj.h"
      INTEGER VDRSPS,VDPSLC,VABCVU,VVUWXY, VSPXYZ
      INTEGER VDYEAR, VJWAFF, VMWAFJ, VJFACI
C
C  Local variables
C
      INTEGER KVCSG
      INTEGER NAVFPH,NAVCSG,NAVWRL
      INTEGER IGARB,IGARB2,KLINE,NCOL
      INTEGER KVFHL,KVFPH,KVFLG,KVWRL,KVSPL
      INTEGER VNELRM
      INTEGER KBANK,IMPPH,IMPFL,INDEX
      INTEGER NROMD,NRONX,IBNUM,NRXYZ,NNXYZ,NZERO
      INTEGER IADDR,ILAY,IWFF,IFAC,IVIEW,IVWN
      INTEGER JLAY,JWFF,JWNX,JFAC,JVIEW
      INTEGER JWAF,IWA,MMOD,MMODN,IWANX
      INTEGER NSTRP,ISTRP,JSTRP,KSTRP,FSTRP,MSTRP,NSTNX
      INTEGER ISTRT,INWRM,ILOWT
      INTEGER NVFHL,IVFHL,NHIT,NVFPH,NCLU
      INTEGER NSEP,ISEP,MXSEP,NCHMX,NSERO
      INTEGER NAMIND
      INTEGER NDATA
      INTEGER NPOS,IFLAG
      INTEGER NC,NE,HID
      INTEGER IRC,IRET,I,JS
      INTEGER BCOL(2)/LVDZTA,LVDXYA/
      REAL SSPOS(2),PPSUM(2)
      REAL PULSE,PULSO,PULSN,PULS
      REAL PSUM,SPOS
      REAL SCUT,PSCUT
      REAL VUW(3),XYZ(3),ABC(3),PSTRP
      REAL ESUM,CSUM,EFRST,CFIRST,ETA
      CHARACTER*4 BNAME(2)/'VDZT','VDXY'/
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      PARAMETER (MXSEP=50)
      INTEGER IBEG(MXSEP),NSCLU(MXSEP)
C
C  BOS functions
C
      INTEGER NLINK,NBANK
C
C  Variables for inline functions
C
      LOGICAL IHIS1,IHIS8
      INTEGER ILINE
      INTEGER IFLG
      LOGICAL LOWTH
      INTEGER IWF,IVW,LYR
#include "bcs.h"
C      INTEGER LCOLS,LROWS,ID,KNEXT,KROW,NRBOS,LFRWRD,LFRROW,ITABL,L
C      REAL RTABL
       SAVE NAVFPH, NAVCSG, NAVWRL
#include "bmacro.h"
C
C  Inline functions
C
      IHIS1(IFLG) = IAND(IFLG,1) .EQ. 1
      IHIS8(IFLG) = IAND(IFLG,8) .EQ. 8
      ILINE(IFLG) = IAND(IFLG,VBLIN2)/VBLIN2
      LOWTH(IWF,IVW,LYR) = IVW.EQ.1.AND.
     >   ((LYR.EQ.1.AND.IWF.NE.1).OR.(LYR.EQ.2.AND.IWF.EQ.3))
      IF(FIRST)THEN
        FIRST=.FALSE.
        NCHMX = MAX(VNELRM(1),VNELRM(2))
        NAVFPH=NAMIND('VFPH')
        NAVCSG=NAMIND('VCSG')
        NAVWRL=NAMIND('VWRL')
      ENDIF
C
C  Drop old banks
C
      CALL BDROP(IW,'VDXYVDZTVSPL')
C
C  get the pointer of the first VFPH
C
      KVFPH = IW(NAVFPH)
C
C  If any exist, make the bank for the temporary storage
C
      IF(KVFPH.GT.0)THEN
        NDATA = NCHMX*LVCSGA+LMHLEN
        CALL AUBOS('VCSG',0,NDATA,KVCSG,IGARB)
        IF(IGARB.EQ.2)THEN
          CALL RERROR('VDXYZT',1,'Out of BOS space; event rejected')
          GOTO 999
        END IF
        IW(KVCSG+LMHROW)=NCHMX
        IW(KVCSG+LMHCOL)=LVCSGA
C
C Create also a vspl bank 100000 which will be used as a buffer
C Size is twice the number of strips (max. : each strip split)
C
        NDATA = 2*NCHMX*LVSPLA+LMHLEN
        CALL AUBOS('VSPL',100000,NDATA,KVSPL,IGARB)
        IF(IGARB.EQ.2)THEN
          CALL RERROR('VDXYZT',1,'Out of BOS space; event rejected')
          GOTO 999
        END IF
        IW(KVSPL+LMHROW)=0
        IW(KVSPL+LMHCOL)=LVSPLA
C
C Reaccess
C
        KVCSG=IW(NAVCSG)
      END IF

C
C Say that no VDXY or VDZT bank has yet been open
C
      INWRM=1
C
C For Vdet95, we need to create the bank for relations at module level
C
      IF(VDYEAR().EQ.95)THEN
        NDATA = NCHMX*LVWRLA+LMHLEN
        CALL AUBOS('VWRL',0,NDATA,KVWRL,IGARB)
        IF(IGARB.EQ.2)THEN
          CALL RERROR('VDXYZT',1,'Out of BOS space; event rejected')
          GOTO 999
        END IF
        KVCSG=IW(NAVCSG)
        IW(KVWRL+LMHCOL)=LVWRLA
        IW(KVWRL+LMHROW)=NCHMX
      ENDIF
C
C  Loop over all VFPH banks
C
      DO WHILE(KVFPH.GT.0)
C
C  Get the VFHL,VFLG bank indices
C
        IBNUM = IW(KVFPH-2)
        KVFHL = NLINK('VFHL',IBNUM)
        KVFLG = NLINK('VFLG',IBNUM)
        IF(KVFHL .LE. 0 .OR. KVFLG .LE. 0)THEN
          CALL RERROR('VDXYZT',1,'VFHL,VFLG banks missing')
          GOTO 999
        END IF
C
C  loop over the clusters in this VFHL bank
C
        NVFHL = LROWS(KVFHL)
        NVFPH = LROWS(KVFPH)
C
C  Unpack the first cluster address
C
        IF(NVFHL.NE.0)THEN
          IADDR = ITABL(KVFHL,1,JVFHHA)
          CALL VUNADD(IADDR,NSTRP,ILAY,IWFF,IFAC,IVIEW,FSTRP)
          CALL VAENWA(NROMD,ILAY,IWFF,IFAC,IVIEW)
C
C Find the module nb and wafer in module
C
          IRET = VJFACI(ILAY,IFAC,JFAC)
          IRET = VJWAFF(JFAC,IWFF,JWAF)
          IRET = VMWAFJ(JWAF,MMOD,IWA)
C
C Special encoding for VDXY/VDZT
C
          CALL VAENWA(NRXYZ,ILAY,IWFF,IFAC,1)
        ENDIF
        ISTRT=0
        DO IVFHL = 1,NVFHL
          SCUT = SEPSIG*SNOISE(IVIEW)
          PSCUT = POSSIG*SNOISE(IVIEW)
C
C  Copy the pulseheights and flags into local bank
C
          PULSO = 0.0
          DO ISTRP=1,NSTRP
            JSTRP = ISTRT+ISTRP
            PULSE = ITABL(KVFPH,JSTRP,1)
            IFLAG = ITABL(KVFLG,JSTRP,1)
C
C  If needed, correct for the decay factor
C
            PULSN = PULSE
            IF(IVIEW.EQ.2.AND.IDCLIN.EQ.ILINE(IFLAG))THEN
              PULSN = PULSE + DECFAC*(PULSE-PULSO)
            END IF
            KLINE = KROW(KVCSG,ISTRP+FSTRP-1)
            RW(KLINE+JVCSMP) = PULSN
            IW(KLINE+JVCSMF) = IFLAG
C
C  Save uncorrected pulseheight in case we are correcting the next one
C
            PULSO = PULSE
          END DO
C
C  Test histograms for charge division
C
          IF(IHIS8(HISTLV))THEN
C
C  Check for appropriate clusters
C
            NE = 0
            ESUM = 0.0
            DO ISTRP=1,NSTRP
              JSTRP = FSTRP+ISTRP-1
              PULSE = RTABL(KVCSG,ISTRP,JVCSMP)
              IFLAG = ITABL(KVCSG,ISTRP,JVCSMF)
              IF(PULSE.GE.PSCUT)THEN
                NE = NE + 1
                IF(NE .EQ. 1)EFRST = PULSE
                ESUM = ESUM + PULSE
              END IF
            END DO
            IF(NE.EQ.2)THEN
              ETA = EFRST/ESUM
              IF(IVIEW.EQ.2)THEN
                HID = 100+10*ILINE(IFLAG)+1
                CALL HFILL(HID,ETA,0.0,1.0)
              ELSE
                CALL HFILL(130,ETA,0.0,1.0)
              END IF
            END IF
          END IF
C
C  Hit separation
C
          IF(VDYEAR().EQ.95.AND.LOWTH(IWA,IVIEW,ILAY))THEN
            CALL VSEPLT(
     >              ILAY,IWA,FSTRP,NSTRP,NSEP,SSPOS,PPSUM,IBEG,NSCLU)
            ILOWT=1
          ELSE
            CALL VSEPAR(SCUT,FSTRP,NSTRP,NSEP,IBEG,NSCLU)
            ILOWT=0
          ENDIF
C
C  IBEG is the beginning of each subcluster w/r FSTRP, the begin
C  of the original cluster.
C  We now need to translate
C
          DO ISEP=1,NSEP
            IBEG(ISEP)=IBEG(ISEP)+FSTRP-1
          ENDDO
C
C  Loop over the separated clusters
C
          DO 22 ISEP=1,NSEP
C
C  calculate the strip coordinate cluster position
C  (only if not in the low theta region)
C
C Here, we use scut, while poscut should probably be used
C
            IF(ILOWT.EQ.0)THEN
              CALL VLCPOS(IBEG(ISEP),NSCLU(ISEP),SCUT,SPOS,PSUM,NPOS)
              SPOS = SPOS+IBEG(ISEP)-1
            ELSE
              SPOS=SSPOS(ISEP)+FSTRP-1
              PSUM=PPSUM(ISEP)
            ENDIF
            IF(NPOS .LE. 0)THEN
              CALL RERROR('VDXYZT',6,'Position calculation error')
              GOTO 22
            END IF
C
C  Histogram the hit width
C
            IF(IHIS1(HISTLV))THEN
              CALL HFILL(HISOFF+30+IVIEW,FLOAT(NPOS),0.0,1.0)
              CALL HFILL(HISOFF+20+IVIEW,PSUM,0.0,1.0)
            END IF
C
C  Re-make final pulseheight cut
C
            IF(PSUM .GE. FNLSUM)THEN
C
C  Translate strip position to global position
C
              IRC = VSPXYZ
     >           (SPOS,IVIEW,ILAY,IFAC,IWFF,NROMD,VUW,XYZ)
              IF (IRC.NE.1) GOTO 22
C
C  Tag separated clusters
C
                IF(NSEP .GT. 1)THEN
                  IFLAG = ISEPBT
                ELSE
                  IFLAG = 0
                END IF
C
C  Add cluster information to VDXY/VDZT bank.
C
C  If we are processing a readout module for the first time, create
C  the bank
                IF(INWRM.EQ.1)THEN
                  IF(IVIEW.EQ.1)THEN
                    NCOL=LVDZTA
                  ELSE
                    NCOL=LVDXYA
                  ENDIF
                  NDATA=LMHLEN+NCOL*NVFPH
                  CALL AUBOS(BNAME(IVIEW),NRXYZ,NDATA,KBANK,IGARB)
                  IF(IGARB.EQ.2)THEN
                    CALL RERROR('VDXYZT',2,'Error making BOS bank')
                    GOTO 999
                  ENDIF
                  IF(IGARB.EQ.1)THEN
                    KBANK = NLINK(BNAME(IVIEW),NRXYZ)
                    KVFHL = NLINK('VFHL',IBNUM)
                    KVFLG = NLINK('VFLG',IBNUM)
                    KVFPH = NLINK('VFPH',IBNUM)
                    KVSPL = NLINK('VSPL',100000)
                    KVCSG = IW(NAVCSG)
                    KVWRL = IW(NAVWRL)
                  ENDIF
                  IW(KBANK+LMHCOL)=NCOL
                  IW(KBANK+LMHROW)=0
                  INWRM=0
                ENDIF
                KLINE = KNEXT(KBANK)
                IF(IVIEW .EQ. 1)THEN
                  RW(KLINE+JVDZZC) = XYZ(3)
                  RW(KLINE+JVDZWC) = VUW(3)
                  RW(KLINE+JVDZSZ) = ERRNOM(IVIEW)
                  RW(KLINE+JVDZSW) = ERRNOM(IVIEW)
                  RW(KLINE+JVDZPH) = PSUM
                  IW(KLINE+JVDZIH) = IVFHL
                  IW(KLINE+JVDZIW) = IBNUM
                  IW(KLINE+JVDZQF) = IFLAG
                ELSE
                  RW(KLINE+JVDXXC) = XYZ(1)
                  RW(KLINE+JVDXYC) = XYZ(2)
                  RW(KLINE+JVDXUC) = VUW(2)
                  RW(KLINE+JVDXSX) = ERRNOM(IVIEW)
                  RW(KLINE+JVDXSY) = ERRNOM(IVIEW)
                  RW(KLINE+JVDXSU) = ERRNOM(IVIEW)
                  RW(KLINE+JVDXPH) = PSUM
                  IW(KLINE+JVDXIH) = IVFHL
                  IW(KLINE+JVDXIW) = IBNUM
                  IW(KLINE+JVDXQF) = IFLAG
                END IF
                NCLU = LROWS(KBANK) + 1
                IW(KBANK+LMHROW) = NCLU
C
C Say that there are no shared strips (yet)
C
                JSH1=0
                JSH2=0
C
C Keep track of split clusters
C
                IF(NSEP.GT.1)THEN
                   IF(LFRROW(KVSPL).LT.1)THEN
                     WRITE(6,*)' Something wrong, VSPL too small !!!'
                     call exit
                   ENDIF
                   NVSPL=LROWS(KVSPL)+1
                   IW(KVSPL+LMHROW)=NVSPL
                   KADDR = KROW(KVSPL,NVSPL)
                   IW(KADDR+JVSPNH)=NCLU
                   IW(KADDR+JVSPFS)=IBEG(ISEP)
                   IW(KADDR+JVSPLS)=IBEG(ISEP)+NSCLU(ISEP)-1
C
C Is there a shared strip ?
C
                   DO J=1,NSEP
                     IF(J.NE.ISEP)THEN
                       JS1=IBEG(J)
                       JS2=IBEG(J)+NSCLU(J)-1
                       IF(JS2.EQ.IBEG(ISEP))JSH1=1
                       IF(JS1.EQ.IBEG(ISEP)+NSCLU(ISEP)-1)JSH2=1
                     ENDIF
                   ENDDO
                   IW(KADDR+JVSPSF)=JSH1
                   IW(KADDR+JVSPSL)=JSH2
                ENDIF
C
C  Keep track of relations between clusters and R/O strips in each wafer
C
                IF(VDYEAR().EQ.95)THEN
                   DO I=1,NSCLU(ISEP)
                      JS = IBEG(ISEP) + I - 1
                      IW(KROW(KVWRL,JS)+JVWRLC+IWA-1) = NCLU
C And pulseheight ...
                      PULS = RTABL(KVCSG,JS,JVCSMP)
C If the strip is shared, pulseheight should be counted twice
                      COEF=1.
                      IF((I.EQ.1.AND.JSH1.EQ.1).OR.
     >                   (I.EQ.NSCLU(ISEP).AND.JSH2.EQ.1))THEN
                           COEF=2.
                      ENDIF
                      RW(KROW(KVWRL,JS)+JVWRSP+IWA-1) = PULS*COEF
                   ENDDO
                ENDIF
            END IF
 22       CONTINUE
          IF(IVFHL.LT.NVFHL)THEN
C
C  Uncode the wafer address of the next cluster
C
            IADDR = ITABL(KVFHL,IVFHL+1,JVFHHA)
            CALL VUNADD(IADDR,NSTNX,ILAY,IWFF,IFAC,IVWN,FSTRP)
            CALL VAENWA(NRONX,ILAY,IWFF,IFAC,IVWN)
C
C Make the special encoding for VDXY/VDZT banks : view=1 !
C
            CALL VAENWA(NNXYZ,ILAY,IWFF,IFAC,1)
C
C Find the module nb and wafer in module
C
            IRET = VJFACI(ILAY,IFAC,JFAC)
            IRET = VJWAFF(JFAC,IWFF,JWAF)
            IRET = VMWAFJ(JWAF,MMODN,IWANX)
          ENDIF
C
C  If next readout module is different, or end of bank,
C  one readout module has been finished. Close the bank
C  But only if it exists
C
          IF(NRONX.NE.NROMD.OR.IVFHL.EQ.NVFHL)THEN
            IF(INWRM.EQ.0)THEN
              NHIT = LROWS(KBANK)
              NDATA=LMHLEN+NCOL*NHIT
              CALL AUBOS(BNAME(IVIEW),NRXYZ,NDATA,KBANK,IGARB)
C
C Copy VSPL 100000 into the VSPL with right NR
C Only if there some rows in it.
C
              NVSPL = LROWS(KVSPL)
              IF(NVSPL.NE.0)THEN
                NDATA = NVSPL*LVSPLA+LMHLEN
                NRVS = NRXYZ + IVIEW-1
                CALL AUBOS('VSPL',NRVS,NDATA,KVSP2,IGARB)
                IF(IGARB.EQ.2)THEN
                  CALL RERROR('VDXYZT',2,'Error making BOS bank')
                  GOTO 999
                ENDIF
                IF(IGARB.EQ.1)THEN
                  KBANK = NLINK(BNAME(IVIEW),NRXYZ)
                  KVFHL = NLINK('VFHL',IBNUM)
                  KVFLG = NLINK('VFLG',IBNUM)
                  KVFPH = NLINK('VFPH',IBNUM)
                  KVSPL = NLINK('VSPL',100000)
                  KVCSG = IW(NAVCSG)
                  KVWRL = IW(NAVWRL)
                ENDIF
                CALL UCOPY(IW(KVSPL+1),IW(KVSP2+1),NDATA)
              ENDIF
C
C and reset VSPL
C
              IW(KVSPL+LMHROW)=0
            ENDIF
C
C  If next module (or view) is different, or end of bank,
C  one module has been finished, make global cluster relations
C  in this module
C
          IF(MMODN.NE.MMOD.OR.
     >       IVWN.NE.IVIEW.OR.
     >       IVFHL.EQ.NVFHL)THEN
             IF(VDYEAR().EQ.95)THEN
               CALL VMGCRL(NROMD,MMOD)
               NZERO=IW(KVWRL)-LMHLEN
               CALL VZERO(IW(KVWRL+LMHLEN+1),NZERO)
             ENDIF
             MMOD=MMODN
          ENDIF
          IWA=IWANX
C
C Say that no VDXY or VDZT bank is anymore opened
C
            INWRM=1
C
C Reset VCSG temporary bank
C
            CALL VZERO(IW(KVCSG+LMHLEN+1),IW(KVCSG)-LMHLEN)
C
            NRXYZ=NNXYZ
            NROMD=NRONX
            IVIEW=IVWN
          ENDIF
C
C  Advance pointer in VFPH
C
          ISTRT = ISTRT + NSTRP
          NSTRP = NSTNX
        END DO
C
C  Move to next VFPH index
C
        KVFPH = IW(KVFPH-1)
      END DO
C
C  Drop work bank
C
      CALL BDROP(IW,'VCSGVWRL')
 999  RETURN
      END
#endif
