C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                                       
C
C  The following COMMON-blocks are needed by routine DGIVDT in file DALBG.FOR
C  for communication with the ALEPHLIB-routine VRDDAF.
C  Should there be any changes, these common-declarations have to be updated
C
C
C*CD VDGEOS  
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
