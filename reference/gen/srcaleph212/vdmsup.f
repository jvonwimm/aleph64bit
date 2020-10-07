      SUBROUTINE VDMSUP(ITRK,KVTMA)
C----------------------------------------------------------------------
C!  Update face extrapolation bank VDMS for true VDET hit position
CKEY VDET TRACK
C!
C!   Author   :David Brown 25-9-92
C!      Modified A. Bonissent March 1995 :
C!                Use geometry package, so that Vdet 91 or 95 can be use
C!   Inputs:
C!     ITRK - FRFT track number
C!     KVTMA - Row number in VTMA; if 0, the VDCO  information is used
C!   Outputs: VDMS bank for the requested track is copied
C!            to bank# 0, and updated
C!            for VDET hit information, coming either from VTMA or VDCO
C!
C!======================================================================
C      IMPLICIT NONE
      SAVE IVDCO,IVTMA,ZCENT
C
C  Inputs
C
      INTEGER ITRK,KVTMA
C
C  Global variables
C
      INTEGER JVDCWI,JVDCR0,JVDCPH,JVDCZ0,JVDCSR,JVDCSZ,
     +          JVDCQF,JVDCTN,LVDCOA
      INTEGER JVTMNL,JVTMNU,JVTMNW,JVTMC2,JVTMIT,JVTMFR,
     +          JVTMUW,JVTMWW,JVTMIU,JVTMIW,JVTMWI,
     +          JVTMR0,JVTMPH,JVTMZ0,JVTMUC,JVTMWC,
     +          JVTMSU,JVTMSW,JVTMCO,LVTMAA
      INTEGER LVDMSA,JVDMWI,JVDMFL,JVDMRA,JVDMUC,JVDMWC
      INTEGER JVDMPV,JVDMPU,JVDMPW,JVDMCU,JVDMSG
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JVDCWI=1,JVDCR0=2,JVDCPH=3,JVDCZ0=4,JVDCSR=5,JVDCSZ=6,
     +          JVDCQF=7,JVDCTN=8,LVDCOA=8)
      PARAMETER(JVTMNL=1,JVTMNU=2,JVTMNW=3,JVTMC2=4,JVTMIT=5,JVTMFR=6,
     +          JVTMUW=7,JVTMWW=11,JVTMIU=15,JVTMIW=19,JVTMWI=23,
     +          JVTMR0=27,JVTMPH=31,JVTMZ0=35,JVTMUC=39,JVTMWC=43,
     +          JVTMSU=47,JVTMSW=51,JVTMCO=55,LVTMAA=58)
      PARAMETER(JVDMWI=1,JVDMFL=2,JVDMRA=3,JVDMUC=4,JVDMWC=5,JVDMPV=6,
     +          JVDMPU=7,JVDMPW=8,JVDMCU=9,JVDMSG=10,LVDMSA=10)
C
C  Local variables
C
      INTEGER JVDMS0, MXVDMS
      PARAMETER(MXVDMS=20)
      REAL VDMSRL
      INTEGER IFLAG,IWAFID
      REAL UC,WC
      INTEGER KVDMS0,I,IPNTN,IPNTO,IND(10)
      INTEGER IVDCO,JVDCO,IVDMS,JVDMS,KVDMS,IVTMA,JVTMA
      INTEGER IMOD(4),JMOD
      INTEGER IHIT,NHIT,IGARB
      INTEGER ILAY,IWAF,IPHI,IVIEW
      INTEGER JLAY,JWAF,JPHI,JVIEW
      INTEGER NVDCO,NVDMS,IROW,JROW,JTRK,ITYPE
      REAL RADIUS,PHI,XYZ(3),VUW(3),FPOS(2,4),RAD(10)
      REAL ZCENT(6),XYZAB(3),ABC(3)
      INTEGER   NWMOD,NWFAC,VNRWAF,VNRMOD
      INTEGER IWFF,JWAB,IRET
C
C  Functions
C
      INTEGER NAMIND,NLINK,VABCXY
C
      INTEGER LCOLS, LROWS, KROW, KNEXT, ITABL,LFRWRD, LFRROW
      REAL RTABL
      INTEGER ID, NRBOS, L
C  Data statements
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C  Statement functions
C
C!    set of intrinsic functions to handle BOS banks
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
C  First time, get the name indicies
C
      IF(FIRST)THEN
        FIRST=.FALSE.
        IVDCO=NAMIND('VDCO')
        IVTMA=NAMIND('VTMA')
C
         NWFAC = VNRWAF()*VNRMOD()
         ABC(1)=0.
         ABC(2)=0.
         ABC(3)=0.
         NWMOD = NWFAC/2
         DO IWFF = 1,NWFAC
           IF(IWFF.LE.NWMOD)THEN
             JWAB = NWMOD-IWFF+1
           ELSE
             JWAB = IWFF
           ENDIF
           IRET = VABCXY(ABC,JWAB,XYZAB)
           ZCENT(IWFF)=XYZAB(3)
         ENDDO
      END IF
C
C  reset the previous VDMS,0 bank
C
      JVDMS0 = NLINK('VDMS',0)
      IF(JVDMS0.GT.0) IW(JVDMS0+2)=0
C
C  Find the VDMS bank corresponding to this track
C
      JVDMS = NLINK('VDMS',ITRK)
      IF(JVDMS.GT.0)THEN
        NVDMS=LROWS(JVDMS)
      ELSE
        NVDMS=0
      END IF
C
C  Find the VDCO bank
C
      JVDCO = IW(IVDCO)
C
C  Create bank 10000
C
      KVDMS = NLINK('VDMS',10000)
      IF(KVDMS.LE.0)THEN
       CALL AUBOS('VDMS',10000,MXVDMS*LVDMSA+LMHLEN,KVDMS,IGARB)
       IF(KVDMS.LE.0)THEN
         CALL ALTELL('Cant make VDMS bank',1,'RETURN')
         RETURN
       END IF
       IF(IGARB.NE.0)THEN
        JVDMS = NLINK('VDMS',ITRK)
        JVDMS0= NLINK('VDMS',0)
        JVDCO = IW(IVDCO)
       END IF
      ENDIF
C
C  Copy the existing bank to number 10000
C
      IF(JVDMS.GT.0)THEN
        CALL UCOPY(IW(JVDMS+1),IW(KVDMS+1),
     &            MIN(NVDMS,MXVDMS-4)*LVDMSA+LMHLEN)
      ELSE
C
C  If we're making a new bank, preset the mini-header
C
        IW(KVDMS+1)=LVDMSA
        IW(KVDMS+2)=0
      END IF
C
C  Parse on which hit bank to use; 0 means VDCO, >0 means VTMA
C
      IF(KVTMA.GT.0)THEN
C
C  Find VTMA bank
C
        JVTMA = IW(IVTMA)
        IF(JVTMA.LE.0)THEN
          CALL ALTELL('VTMA bank missing',2,'RETURN')
          RETURN
        END IF
C
C  Loop over the hits for this row
C
        NHIT = ITABL(JVTMA,KVTMA,JVTMNL)
        DO IHIT=1,NHIT
C
C  Get wafer number and decode; take the Z wafer
C
          IMOD(IHIT) = ITABL(JVTMA,KVTMA,JVTMWW+IHIT-1)
          CALL VADEWA(IMOD(IHIT),ILAY,IWAF,IPHI,IVIEW)
C
C  Get local coordinates; transform the W into 'face' coordinates
C
          FPOS(1,IHIT) = RTABL(JVTMA,KVTMA,JVTMUC+IHIT-1)
          FPOS(2,IHIT) = RTABL(JVTMA,KVTMA,JVTMWC+IHIT-1)+
     &                   ZCENT(IWAF)
        END DO
      ELSE IF(JVDCO.GT.0)THEN
        NVDCO = LROWS(JVDCO)
C
C Loop over VDCO entries
C
        NHIT=0
        DO IROW=1,NVDCO
C
C  Check for track number match; if so, store the hit position
C
          JTRK = ITABL(JVDCO,IROW,JVDCTN)
          IF(ITRK .EQ. JTRK)THEN
             NHIT=NHIT+1
C
C  Get wafer number; decode
C
            IMOD(NHIT) = ITABL(JVDCO,IROW,JVDCWI)
            CALL VADEWA(IMOD(NHIT),ILAY,IWAF,IPHI,IVIEW)
C
C  Get global position
C
            RADIUS = RTABL(JVDCO,IROW,JVDCR0)
            PHI    = RTABL(JVDCO,IROW,JVDCPH)
            XYZ(3) = RTABL(JVDCO,IROW,JVDCZ0)
            XYZ(1) = RADIUS*COS(PHI)
            XYZ(2) = RADIUS*SIN(PHI)
C
C  Convert to local coordinates
C
            CALL VGWFVU(IMOD(NHIT),XYZ,VUW)
C
C  Convert U AND W to 'FACE' coordinates
C
            FPOS(1,NHIT) = VUW(2)
            FPOS(2,NHIT) = VUW(3)+ZCENT(IWAF)
          END IF
        END DO
      ELSE
C
C  No VDET hits from any source
C
        NHIT=0
      END IF
C
C  Loop over the hits
C
      DO IHIT=1,NHIT
C
C  Decode the module address of this hit
C
        CALL VADEWA(IMOD(IHIT),ILAY,IWAF,IPHI,IVIEW)
C
C  Loop over the VDMS rows to find the matching face
C
        DO JROW=1,NVDMS
C
C  Decod this extrapolation address; check for a layer, phi match
C
          JMOD = ITABL(KVDMS,JROW,JVDMWI)
          CALL VADEWA(JMOD,JLAY,JWAF,JPHI,JVIEW)
          IF(JLAY .EQ. ILAY .AND. JPHI .EQ. IPHI)THEN
C
C  Match; overwrite the U and W coordinates, and the wafer ID
C  Set the flag bit for this extrapolation
C
            RW(KROW(KVDMS,JROW)+JVDMUC)=FPOS(1,IHIT)
            RW(KROW(KVDMS,JROW)+JVDMWC)=FPOS(2,IHIT)
            IW(KROW(KVDMS,JROW)+JVDMWI)=IMOD(IHIT)
            IW(KROW(KVDMS,JROW)+JVDMFL)=2
C
C  Escape from the loop
C
            GOTO 300
          END IF
        END DO
C
C  Error if we got to here; hit doesn't match any extrapolation
C
        CALL ALTELL('No VDMS match',3,'RETURN')
 300    CONTINUE
      END DO
C
C now add rows in VDMS 10000 for support rings and carbon fibre tube
C
      CALL VDMSEC(ITRK)
C
C copy VDMS 10000 to VDMS 0 and order the rows
C
      KVDMS0 = NLINK('VDMS',0)
      IF(KVDMS0.LE.0)THEN
       CALL AUBOS('VDMS',0,MXVDMS*LVDMSA+LMHLEN,KVDMS0,IGARB)
       IF(KVDMS0.LE.0) RETURN
       IF(IGARB.NE.0) KVDMS = NLINK('VDMS',10000)
      ENDIF
      IW(KVDMS0+1)=IW(KVDMS+1)
      IW(KVDMS0+2)=IW(KVDMS+2)


      DO 10 I=1,LROWS(KVDMS)
       RAD(I)=RTABL(KVDMS,I,JVDMRA)
   10 CONTINUE
      IF(LROWS(KVDMS).GT.0) CALL SORTZV(RAD,IND,LROWS(KVDMS),1,1,0)
      DO 20 I=1,LROWS(KVDMS)
       IPNTO = KROW(KVDMS,IND(I))+1
       IPNTN = KROW(KVDMS0,I)+1
       CALL UCOPY(IW(IPNTO),IW(IPNTN),LVDMSA)
   20 CONTINUE
      IW(KVDMS+2)=0
C now put the correct amount of multiple scattering into VDMS 0
      DO 30 I=1,LROWS(KVDMS0)
        IFLAG=ITABL(KVDMS0,I,JVDMFL)
        UC=RTABL(KVDMS0,I,JVDMUC)
        WC=RTABL(KVDMS0,I,JVDMWC)
        IWAFID=ITABL(KVDMS0,I,JVDMWI)
        IF(IWAFID.NE.-1) RW(KVDMS0+LMHLEN+(I-1)*IW(KVDMS0+1)+JVDMSG)
     $                    = VDMSRL(UC,WC,IWAFID)
   30 CONTINUE
      RETURN
      END
