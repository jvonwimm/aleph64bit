      SUBROUTINE VDHERR(IWAFA,PV,PU,PW,SIGNU,SIGNW,UERR,WERR)
C
C! Compute the VDET hit error given wafer information and track directio
CKEY VDET JULIA
C
C  This uses a simple theoretical model to compute the hit errors
C  given the signal/noise, and other parameters (see VDET ALIGNMENT
C  ALEPHNOTE of 1991).  Dave Brown, 7-2-91
C
C  INPUT;  IWAFA =  Decimal wafer address
C          PV,PU,PW = track momentum vector at wafer in wafer frame
C          SIGNU,SIGNW = Signal/noise on U,W side
C OUTPUT;  UERR,WERR = hit errors in U,W direction
C
      SAVE
C! read-out geometry constants for VDET.
      INTEGER NSLOM,NWAFN,NWAFM,NGEOM,LVDCC,LVDNC,LVDL,NSLOI
C
      INTEGER NDIVZV,NDIVPV,NCERVZ
      INTEGER NSTPVD,NSTZVD,NVDPPI,NVDZPI,IOPZVD
      INTEGER NSLOCO,NSLOGM,NSLOME,NSLOWA,NSLOEL,NGEOWA
      INTEGER NVDLAY,NVDSLG,NVDMEC,NVDWAF,IPSIGN,IZSIGN
      INTEGER NSLOAD
C
      REAL VDWLEN,VDPPIT,VDZPIT,VDDIPP,VDDIPZ,VDDIZP,VDDIZZ
      REAL VDCRHO,ACTPVD,ACTZVD,VDTHCK,VDPSDM,VDBXTH,VDZOFF
      REAL VDPOFF,VDCPHI,VDTILT,ZWAFVD,VDAPPL,VDDEPL,VDLESP,VDLESZ
      REAL VDSTPH,VDSTZE,VDCEZO,VDCCCP,VDCCCZ,VDNCCP,VDNCCZ
C
      PARAMETER (LVDL=2, LVDCC=7, LVDNC=3)
      PARAMETER (NSLOM=15, NSLOI=12, NWAFN=4,NWAFM=4, NGEOM=4)
      COMMON /VDGEOS/ NDIVZV(NGEOM) , NDIVPV(LVDL) , NCERVZ(NGEOM),
     &                VDWLEN(2,NWAFM), VDPPIT(NWAFM) , VDZPIT(NWAFM),
     &                VDDIPP(NWAFM) , VDDIPZ(NWAFM) , VDDIZP(NWAFM),
     &                VDDIZZ(NWAFM), VDCRHO(NSLOM,LVDL), NSTPVD(NWAFM),
     &                NSTZVD(NWAFM) , ACTPVD(NWAFM) , ACTZVD(NWAFM) ,
     &                VDTHCK(NWAFM) , VDPSDM(NGEOM) ,  VDBXTH(NGEOM) ,
     &                VDZOFF(NWAFN,NGEOM) , VDPOFF(NSLOM,LVDL) ,
     &                VDCPHI(NSLOM,LVDL) , VDTILT(NSLOM,LVDL) ,
     &                ZWAFVD(NWAFN,NSLOM,LVDL), VDAPPL(NWAFM),
     &                VDDEPL(NWAFM) , VDLESP(NWAFM) , VDLESZ (NWAFM),
     &                VDSTPH(NWAFM) , VDSTZE(NWAFM) , NVDPPI(NWAFM),
     &                NVDZPI(NWAFM), VDCEZO(NWAFM,NGEOM), IOPZVD(LVDL),
     &                VDCCCP(NWAFM,0:LVDCC), VDCCCZ(NWAFM,0:LVDCC),
     &                VDNCCP(NWAFM,0:LVDNC), VDNCCZ(NWAFM,0:LVDNC),
     &                NSLOCO(NSLOM,LVDL) , NSLOGM(NSLOM,LVDL) ,
     &                NSLOME(NSLOM,LVDL) , NSLOWA(NSLOM,LVDL) ,
     &                NSLOEL(NSLOM,LVDL) , NGEOWA(NGEOM) ,
     &                NVDLAY, NVDSLG, NVDMEC, NVDWAF ,
     &                IPSIGN(4,2) , IZSIGN(4,2) , NSLOAD(NSLOM,LVDL)
C
      INTEGER IWAFA,ILAY,IWAF,IPHI,IVIEW,ITYPE
      REAL RPITU,FPITU,RPITW,FPITW,PV,PU,PW
      REAL SIGNU,SIGNW,UERR,WERR
      REAL COSW, SINW, COSU, SINU
      REAL UERRP,UERRN,WERRP,WERRN
C
C  Shower spread factor- this should be computed from the pulseheight,
C  but for now we take a nominal value.  We also need sqrt(12).
C
      REAL SPRED,ROT12
      DATA SPRED/1.5/
      DATA ROT12/3.46410/
C
C  Unpack the wafer address, and find the wafer type
C
      CALL VADEWA(IWAFA,ILAY,IWAF,IPHI,IVIEW)
      ITYPE = NSLOGM(IPHI,ILAY)
C
C  Get the readout and floating strip pitches, U and W direction
C
      RPITU = VDPPIT(ITYPE)
      RPITW = VDZPIT(ITYPE)
      FPITU = VDSTPH(ITYPE)
      FPITW = VDSTZE(ITYPE)
C
C  Compute the cosine/sines
C
      COSW = SQRT( MAX(1./(1.+ (PW/PV)**2),0.0) )
      SINW = SQRT( MAX(1. - COSW**2,0.0) )
      COSU = SQRT( MAX(1./(1.+ (PU/PV)**2),0.0) )
      SINU = SQRT( MAX(1. - COSU**2,0.0) )
C
C  Resolution due to strip pitch
C
      UERRP = FPITU*COSU/ROT12
      WERRP = FPITW*COSW/ROT12
C
C  Resolution due to noise
C
      UERRN = (RPITU/SIGNU)*
     &  ( (1.0 + SPRED*SINU)/SQRT(1.+SINU**2 + SINW**2) )
      WERRN = (RPITW/SIGNW)*
     &  ( (1.0 + SPRED*SINW)/SQRT(1.+SINU**2 + SINW**2) )
C
C  Resolution due to alignment (NOT YET IMPLEMENTED)
C
C
C  Add in quadrature
C
      UERR = SQRT( UERRP**2 + UERRN**2)
      WERR = SQRT( WERRP**2 + WERRN**2)
C
      RETURN
      END
