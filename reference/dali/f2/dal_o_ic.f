*DK DODCYL
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DODCYL
CH
      SUBROUTINE DODCYL(RA,ZL,ZR,LICOL)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!: draw a rubber band cylinder around Z-axis with radius R from Z1 to Z2
C   for ro projection as wire frame.
C ---------------------------------------------------------------------
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION H(2),V(2),HS(2,2),VS(2,2)
      DATA NDA/6/
      IF(PDCODD(4,LICOL).LE.0.) RETURN
      CALL DQLEVL(LICOL)
      DLINDD=PDCODD(2,LIDTDD)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE,J_RAL'
      CALL DPARAM(10
     &  ,J_PFI,J_PTE,J_RAL)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      SF=SIND(    PARADA(2,J_PFI))
      CF=COSD(    PARADA(2,J_PFI))
      ST=SIND(90.-PARADA(2,J_PTE))
      CT=COSD(90.-PARADA(2,J_PTE))
      SG=SIND(    PARADA(2,J_RAL))
      CG=COSD(    PARADA(2,J_RAL))
      Z1=ZL
      DO I=1,2
        R2MAX=0.
        HMID=ST*Z1
        VMID=SG*CT*Z1
        DO NA=0,360,NDA
          A=NA
          X1=RA*COSD(A)
          Y1=RA*SIND(A)
          X2  = CF*X1+SF*Y1
          Y2  =-SF*X1+CF*Y1
          H(2)= CT*X2+ST*Z1
          Z3  =-ST*X2+CT*Z1
          V(2)= CG*Y2+SG*Z3
          IF(NA.GT.0) THEN
            CALL DQLIE(H,V)
            R2=(H(2)-HMID)*(H(2)-HMID)+(V(2)-VMID)*(V(2)-VMID)
            IF(R2.GT.R2MAX.AND.NA.LT.180) THEN
              HS(I,1)=H(2)
              VS(I,1)=V(2)
              R2MAX=R2
            END IF
          END IF
          H(1)=H(2)
          V(1)=V(2)
        END DO
        HS(I,2)=2.*HMID-HS(I,1)
        VS(I,2)=2.*VMID-VS(I,1)
        Z1=ZR
      END DO
      CALL DQLIE(HS(1,1),VS(1,1))
      CALL DQLIE(HS(1,2),VS(1,2))
      DLINDD=PDCODD(2,LITRDD)
      END
*DK DODBPI
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DODBPI
CH
      SUBROUTINE DODBPI(TPROJ)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:  DRAW BEAM PIPE
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DE
C     ++++++++++++++++++++++++++++++ used in ATLANTIS , NRECDE not used here
      COMMON /DEVTIC/NFILDE,IRUNDE(2),IEVTDE(2),LNINDE(2),LCLSDE,NRECDE
      COMMON /DEVTIT/TFINDE(2)
      CHARACTER *80 TFINDE
C------------------------------------------------------------------- DG
      COMMON / DGDBLU / LDBSDG,IRUNDG
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DT
      COMMON /DTVCCC/ DUMYDT(4),
     1         HUMIDT,HUMADT,VUMIDT,VUMADT,
     1         NSQUDT,NOBJDT,NOCLDT,HPOSDT,VPOSDT,
     1         FPOSDT,TPOSDT,AROTDT,RPOSDT,NTVIDT,
     1         FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,
     1         FX11DT
      LOGICAL  FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,FX11DT
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C------------------------------------------------------------------- DQ
      COMMON /DQQQ1C/ AHSCDQ,BHSCDQ,CHSCDQ,AVSCDQ,BVSCDQ,CVSCDQ
      COMMON /DQQQ2C/ DMAXDQ
      COMMON /DQQQ3C/ FPRSDQ,FPSQDQ
      LOGICAL FPRSDQ,FPSQDQ
      COMMON /DQQQ4C/ PRSHDQ,PRSVDQ,PRS1DQ,PRS2DQ,PRV1DQ
      COMMON /DQQQ5C/ HUSRDQ(4,0:12),VUSRDQ(4,0:12)
C------------------------------------------------------------------- DO
C     +++++++++++++++++++++++++ IWUSDO,IWARDO not used in ATLANTIS
      COMMON /DOPR1C/IMAXDO,IWUSDO,IWARDO,IAREDO,IPICDO,IZOMDO,IDU2DO(2)
      COMMON /DOPR2C/ PICNDO
C------------------------------------------------------------------- DP
      COMMON /DPCOLR/ ICOLDP
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      INCLUDE 'A_BCS.INC'
C      INCLUDE 'FKALJJ.INC'

      CHARACTER *3 TPROJ
      DIMENSION X(0:12),Y(0:12)
      DATA RBP/7.8/
      DATA HUFS/-250./,DDFS/4./
      LOGICAL FSTRT,FIN
      DATA FSTRT/.TRUE./

      INTEGER ALGTDB
      INTEGER GTSTUP

      INCLUDE 'A_BMACRO.INC'

      CALL DPARGV(82,'RBP',4,RBP4)
      IF(RBP4.LE.0.) THEN
C
C  read beam pipe radius for the given run number from database
C
        IRUN = IRUNDE(1)

        IF (IRUN.NE.0) THEN

C  now use bank BPG! instead of FKAL! this works always!
          IBP = GTSTUP('BP',IRUN)
          IFLAG = ALGTDB(LDBSDG,'BPG1',IBP)
          IF (IFLAG.EQ.0) THEN
            CALL DWRT(' ERROR: unable to read beam pipe radius !')
            CALL DWRT('        source of error: ALGTDB.')
            RETURN
          END IF
          KBPG1 = IW(NAMIND('BPG1'))
          IF (KBPG1.NE.0) THEN
            RBP = RTABL(KBPG1,1,9)
          END IF
C  this gives maximum radius(word 9) of central region of beampipe (row 1)

        END IF
C
C
      ELSE
        CALL DPARGV(82,'RBP',2,RBP)
      END IF
      IF(FPIKDP.OR.NOCLDT.EQ.1)  RETURN
      IF(PDCODD(4,ICBPDD).NE.1.OR.PDCODD(2,ISTYDD).LT.2.) THEN
        IF(TPROJ.EQ.'SRZ') THEN
          CALL DQLEVL(KCBPDD)
          CALL DQL2E(-1000.,0.,1000.,0.)
        END IF
        RETURN
      END IF
      CALL DOFISH('BP',TPROJ(2:3))
      NC1=PDCODD(2,ICBPDD)
      CALL DQPO0('AREA',NC1,0,'NSKP')
C     CALL DQAR0(1.,1.)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PDS'
      CALL DPARAM(11
     &  ,J_PDS)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(TPROJ(2:3).EQ.'RZ') THEN
        IF(TPROJ(1:1).EQ.'S') THEN
C         CALL DQLEVL(ICBPDD)
          IF(PDCODD(2,LIDTDD).EQ.2.) THEN
C           .......... BECAUSE TWO LINES ARE DRAWN, THE PICTURE GETS ASSYMETRIC.
            Y(3)=RBP-1./BVSCDQ
          ELSE
            Y(3)=RBP
          END IF
          Y(1)=-Y(3)
        ELSE
          Y(1)=0.
          Y(3)=RBP
        END IF
        X(1)=-1000.
        X(3)= 1000.
        X(2)=X(3)
        X(4)=X(1)
        X(0)=X(4)
        Y(2)=Y(1)
        Y(4)=Y(3)
        Y(0)=Y(4)
        CALL DQPOL(5,X,Y)
C       CALL DQAR(-1000.,-RBD,1000.,RBP)
C     ELSE IF(TPROJ.EQ.'PRZ') THEN
C       CALL DQLEVL(ICBPDD)
C       CALL DQAR(-1000.,0.,1000.,RBP)
        IF(IZOMDO.EQ.0.AND.FPRSDQ) THEN
          CALL DPAR_SET_CO(95,'CFI')
          IF(ICOLDP.GE.0) THEN
            CALL DQPOC(HUFS,0.,HDFS,VDFS,FIN)
            IF(FIN) THEN
              CALL DQLEVL(ICTXDD)
              CALL DGTEXT(HDFS,VDFS-DDFS,'FISH-EYE VIEW',13)
            END IF
          END IF
        END IF
      ELSE IF(TPROJ.EQ.' YX') THEN
C        IF(FSTRT) THEN
          IF(PARADA(4,J_PDS).NE.1.) THEN
            A=0.
            DO 700 N=0,11
              X(N)=SIND(A)*RBP
              Y(N)=COSD(A)*RBP
              A=A+30.
  700       CONTINUE
            X(12)=X(0)
            Y(12)=Y(0)
            FSTRT=.FALSE.
          ELSE
C            R2 = PARADO(2,IPTODO)
C            R1 = 0.5*R2
C            P  = PARADO(2,IYDPDO)

C            R1P= R2 - R1/P
C            E = (R1P - R1)/(R1*(R2-R1P))


            A=0.
            DO 800 N=0,11
              X(N) = SIND(A)*RBP
              Y(N) = COSD(A)*RBP
              A=A+30.
  800       CONTINUE
            X(12)=X(0)
            Y(12)=Y(0)
C            FSTRT=.FALSE.
          END IF
C        END IF
        CALL DQPOL(13,X,Y)
      END IF
      CALL DOFIS2
      END
*DK DODECA
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DODECA
CH
      SUBROUTINE DODECA(TPR,TUNIT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C    Modified by R.Vogl                        31.10.1989
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DT
      COMMON /DTVCCC/ DUMYDT(4),
     1         HUMIDT,HUMADT,VUMIDT,VUMADT,
     1         NSQUDT,NOBJDT,NOCLDT,HPOSDT,VPOSDT,
     1         FPOSDT,TPOSDT,AROTDT,RPOSDT,NTVIDT,
     1         FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,
     1         FX11DT
      LOGICAL  FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,FX11DT
C------------------------------------------------------------------- DR
      COMMON /DRNG1C/
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      LOGICAL
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      COMMON /DRNG2C/DET1DR(6),DET2DR(6)
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *3 TPR
      CHARACTER *3 TPROJ
      CHARACTER *6 TDRMD(0:3),TUNIT
      DIMENSION RR(3)
      DIMENSION ROT(6)
      DATA TDRMD/'SIMPLE','DETAIL','SIMPLE','DETAIL'/
      CHARACTER *4 TSTYL(0:3),TST
      DATA  TSTYL /'LINB','LINC','AR+L','AREA' /
      DATA PERSC/8./
      TPROJ = TPR
      IF(FPIKDP.OR.NOCLDT.EQ.1) RETURN
      IF(.NOT.FECADR) RETURN
      IF(PDCODD(4,ICECDD).NE.1.) RETURN
      CALL DOFISH('EC',TPROJ(2:3))
      NC1 = PDCODD(2,ICECDD)
      NC2 = PDCODD(2,KCECDD)
      TST = TSTYL(IFIX(PDCODD(2,ISTYDD)))
      IF(TST.EQ.'AR+L'.AND.PDCODD(4,KCECDD).NE.1.) TST='AREA'
      DLINDD=PDCODD(2,LIDTDD)

      PRN = PDCODD(2,NUPODD)

C  RARELY USED 3D FEATURE FOR ECAL

C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE,J_PTO,J_PDS,J_RAL'
      CALL DPARAM(10
     &  ,J_PFI,J_PTE,J_PTO,J_PDS,J_RAL)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(TPROJ.EQ.'3-D') THEN
        ROT(1) = SIND(PARADA(2,J_PFI))
        ROT(2) = COSD(PARADA(2,J_PFI))
        THETA  = PARADA(2,J_PTE)
        IF ((THETA.GT. 89.9999).AND.(THETA.LT.90.0001)) THEN
          THETA = 90.0001
        END IF
        ROT(3) = SIND(90. - THETA)
        ROT(4) = COSD(90. - THETA)
        ROT(5) = SIND(PARADA(2,J_RAL))
        ROT(6) = COSD(PARADA(2,J_RAL))

        IF(TST.EQ.'LINB') THEN
          CALL DQPO0('LINB',0,NC2,'NSKP')
        ELSE
          CALL DQPO0('LINC',0,NC1,'NSKP')
        END IF

        CALL DODE3D(TUNIT,ROT)

      END IF
C
C  SETUP FOR PERSPECTIVE PARAMETERS
C
      IF ((TPROJ(2:3).EQ.'YX').AND.(PARADA(4,J_PDS).EQ.1.)) THEN
        TPROJ(2:3) = 'PP'
        PRN = PERSC
        RR(2) = PARADA(2,J_PTO)
        RR(1) = 0.5*RR(2)
        P = PARADA(2,J_PDS)
        RR(3) = RR(2) - RR(1)/P
      END IF
C
C  PERSPECTIVE DISPLAY IS SWITCHED ON VIA PARADO(4,IYDPDO)

      IF(TST.EQ.'LINB') THEN
        CALL DQPO0( TST,0,NC2,'NSKP')
        CALL DGDECA(TPROJ,TDRMD(IFIX(PDCODD(2,MODEDD))),
     &    TUNIT,PRN,RR)
        GO TO 99
      END IF
      IF(TST.EQ.'LINC') THEN
        CALL DQPO0( TST,0,NC1,'NSKP')
        CALL DGDECA(TPROJ,TDRMD(IFIX(PDCODD(2,MODEDD))),
     &    TUNIT,PRN,RR)
        GO TO 99
      END IF
      IF(TPROJ(2:3).NE.'FT') THEN
        CALL DQPO0(TST,NC1,NC2,'NSKP')
        CALL DGDECA(TPROJ,TDRMD(IFIX(PDCODD(2,MODEDD))),
     &    TUNIT,PRN,RR)
      ELSE
        CALL DQPO0('AREA',NC1,0,'NSKP')
        CALL DGDECA(TPROJ,TDRMD(IFIX(PDCODD(2,MODEDD))),
     &    TUNIT,PRN,RR)
        IF(TST.EQ.'AREA') GO TO 99
        CALL DQPO0('LINE',0,NC2,'NSKP')
        CALL DGDECA(TPROJ,TDRMD(IFIX(PDCODD(2,MODEDD))),
     &    TUNIT,PRN,RR)
      END IF
99    DLINDD=PDCODD(2,LITRDD)
      CALL DOFIS2
      END
*DK DODE3D
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DODE3D
CH
      SUBROUTINE DODE3D(TUNIT,ROT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Modified by R.Vogl                         11.03.1992
C!:    draws 3 d view of ECAL (NO hidden line algorithm!!!!)
C!
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DE
      PARAMETER (NEBAM=12,NEECM=12)
      COMMON /DECGEO/ CORADE(3,12,NEECM),CORBDE(3,8,NEBAM),
     &                CORCDE(3,12,NEECM),ETILDE ,
     &                POLADE(2,12,NEECM),POLBDE(2,8,NEBAM),
     &                POLCDE(2,12,NEECM),
     &                RMNBDE ,RMXBDE ,RICBDE ,ROCBDE,
     &                RMNEDE ,RMXEDE ,RICEDE ,RMCEDE ,ROCEDE
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION ROT(6)
      CHARACTER *6 TUNIT
      DIMENSION ROTCO(2,12)
      DIMENSION H(10),V(10)
      DIMENSION IBP(4),IEP(6)
      DATA IBP / 1, 3, 7, 5 /
      DATA IEP / 1, 3, 7, 11, 9, 5 /

C
C  STATEMENT FUNCTIONS :
C
      HCOR(X,Y,Z) = (ROT(2)*X+ROT(1)*Y)*ROT(4) + ROT(3)*Z
      VCOR(X,Y,Z) = ROT(6)*( ROT(2)*Y - ROT(1)*X ) +
     &              ROT(5)*( ROT(4)*Z - ROT(3)*(ROT(2)*X + ROT(1)*Y) )
      ZCOR(X,Y,Z) = ROT(5)*( ROT(1)*X - ROT(2)*Y ) +
     &              ROT(6)*( ROT(4)*Z - ROT(3)*(ROT(2)*X + ROT(1)*Y) )

C  TUNIT can be 'BARREL', 'ENDCAP' or 'BAR+EC'

      IF (TUNIT.EQ.'BARREL'.OR.TUNIT.EQ.'BAR+EC') THEN
        DO MD=1,NEBAM
          DO ICO=1,8
            ROTCO(1,ICO) =
     &        HCOR(CORBDE(1,ICO,MD),CORBDE(2,ICO,MD),CORBDE(3,ICO,MD))
            ROTCO(2,ICO) =
     &        VCOR(CORBDE(1,ICO,MD),CORBDE(2,ICO,MD),CORBDE(3,ICO,MD))
          END DO

          DO I=1,4
            H(I) = ROTCO(1,IBP(I))
            V(I) = ROTCO(2,IBP(I))
          END DO
          H(5)=H(1)
          V(5)=V(1)
          CALL DQPOL(5,H,V)

          DO I=1,4
            H(I) = ROTCO(1,IBP(I)+1)
            V(I) = ROTCO(2,IBP(I)+1)
          END DO
          H(5)=H(1)
          V(5)=V(1)
          CALL DQPOL(5,H,V)

          DO I=1,7,2
            H(1)= ROTCO(1,I)
            V(1)= ROTCO(2,I)
            H(2)= ROTCO(1,I+1)
            V(2)= ROTCO(2,I+1)
            CALL DQPOL(2,H,V)
          END DO
        END DO
      END IF

      IF (TUNIT.EQ.'ENDCAP'.OR.TUNIT.EQ.'BAR+EC') THEN
        DO MD=1,NEECM
          DO ICO=1,12
            ROTCO(1,ICO) =
     &        HCOR(CORADE(1,ICO,MD),CORADE(2,ICO,MD),CORADE(3,ICO,MD))
            ROTCO(2,ICO) =
     &        VCOR(CORADE(1,ICO,MD),CORADE(2,ICO,MD),CORADE(3,ICO,MD))
          END DO

          DO I=1,6
            H(I) = ROTCO(1,IEP(I))
            V(I) = ROTCO(2,IEP(I))
          END DO
          H(7)=H(1)
          V(7)=V(1)
          CALL DQPOL(7,H,V)

          DO I=1,6
            H(I) = ROTCO(1,IEP(I)+1)
            V(I) = ROTCO(2,IEP(I)+1)
          END DO
          H(7)=H(1)
          V(7)=V(1)
          CALL DQPOL(7,H,V)

          DO I=1,11,2
            H(1)= ROTCO(1,I)
            V(1)= ROTCO(2,I)
            H(2)= ROTCO(1,I+1)
            V(2)= ROTCO(2,I+1)
            CALL DQPOL(2,H,V)
          END DO
        END DO
        DO MD=1,NEECM
          DO ICO=1,12
            ROTCO(1,ICO) =
     &        HCOR(CORCDE(1,ICO,MD),CORCDE(2,ICO,MD),CORCDE(3,ICO,MD))
            ROTCO(2,ICO) =
     &        VCOR(CORCDE(1,ICO,MD),CORCDE(2,ICO,MD),CORCDE(3,ICO,MD))
          END DO

          DO I=1,6
            H(I) = ROTCO(1,IEP(I))
            V(I) = ROTCO(2,IEP(I))
          END DO
          H(7)=H(1)
          V(7)=V(1)
          CALL DQPOL(7,H,V)

          DO I=1,6
            H(I) = ROTCO(1,IEP(I)+1)
            V(I) = ROTCO(2,IEP(I)+1)
          END DO
          H(7)=H(1)
          V(7)=V(1)
          CALL DQPOL(7,H,V)

          DO I=1,11,2
            H(1)= ROTCO(1,I)
            V(1)= ROTCO(2,I)
            H(2)= ROTCO(1,I+1)
            V(2)= ROTCO(2,I+1)
            CALL DQPOL(2,H,V)
          END DO
        END DO
      END IF
      END
*DK DODHCA
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DODHCA
CH
      SUBROUTINE DODHCA(TPR,TUNIT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C    Modified by R.Vogl                         31.10.1989
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DT
      COMMON /DTVCCC/ DUMYDT(4),
     1         HUMIDT,HUMADT,VUMIDT,VUMADT,
     1         NSQUDT,NOBJDT,NOCLDT,HPOSDT,VPOSDT,
     1         FPOSDT,TPOSDT,AROTDT,RPOSDT,NTVIDT,
     1         FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,
     1         FX11DT
      LOGICAL  FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,FX11DT
C------------------------------------------------------------------- DR
      COMMON /DRNG1C/
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      LOGICAL
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      COMMON /DRNG2C/DET1DR(6),DET2DR(6)
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *3 TPR
      CHARACTER *3 TPROJ
      CHARACTER *6 TDRMD(0:3),TUNIT
      DIMENSION RR(3), ROT(6)
      DATA TDRMD/'SIMPLE','DETAIL','SIMPLE','DETAIL'/
      CHARACTER *4 TSTYL(0:3),TST
      DATA  TSTYL /'LINB','LINC','AR+L','AREA' /
      DATA PERSC/8./
      TPROJ = TPR
      IF(FPIKDP.OR.NOCLDT.EQ.1) RETURN
      IF(.NOT.FHCADR) RETURN
      IF(PDCODD(4,ICHCDD).NE.1.) RETURN
      CALL DOFISH('HC',TPROJ(2:3))
      NC1 = PDCODD(2,ICHCDD)
      NC2 = PDCODD(2,KCHCDD)
      TST = TSTYL(IFIX(PDCODD(2,ISTYDD)))
      IF(TST.EQ.'AR+L'.AND.PDCODD(4,KCHCDD).NE.1.) TST='AREA'
      DLINDD=PDCODD(2,LIDTDD)

      PRN = PDCODD(2,NUPODD)
C RARELY USED 3D FEATURE FOR HCAL

C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFI,J_PTE,J_PTO,J_PDS,J_RAL'
      CALL DPARAM(10
     &  ,J_PFI,J_PTE,J_PTO,J_PDS,J_RAL)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(TPROJ.EQ.'3-D') THEN
        ROT(1) = SIND(PARADA(2,J_PFI))
        ROT(2) = COSD(PARADA(2,J_PFI))
        THETA  = PARADA(2,J_PTE)
        IF ((THETA.GT. 89.9999).AND.(THETA.LT.90.0001)) THEN
          THETA = 90.0001
        END IF
        ROT(3) = SIND(90. - THETA)
        ROT(4) = COSD(90. - THETA)
        ROT(5) = SIND(PARADA(2,J_RAL))
        ROT(6) = COSD(PARADA(2,J_RAL))

        IF(TST.EQ.'LINB') THEN
          CALL DQPO0('LINB',0,NC2,'NSKP')
        ELSE
          CALL DQPO0('LINC',0,NC1,'NSKP')
        END IF

        CALL DODH3D(TUNIT,ROT)

      END IF
      IF(TPROJ(2:3).EQ.'YZ') THEN
        CALL DPARGI_24(91,'CHZ',LHZ,LHZ4)
        IF(LHZ4.GT.0) THEN
C         ..................... draw barrel frame
          CALL DQPO0('LINE',0,LHZ,' ')
          CALL DGDHCA('BYZ',' ',' ',0,0)
        END IF
      END IF

C
C  SETUP FOR PERSPECTIVE PARAMETERS
C
      IF ((TPROJ(2:3).EQ.'YX').AND.(PARADA(4,J_PDS).EQ.1.)) THEN
        TPROJ(2:3) = 'PP'
        PRN = PERSC
        RR(2) = PARADA(2,J_PTO)
        RR(1) = 0.5*RR(2)
        P = PARADA(2,J_PDS)
        RR(3) = RR(2) - RR(1)/P
      END IF
C
C  PERSPECTIVE DISPLAY IS SWITCHED ON VIA PARADO(4,IYDPDO)

      IF(TST.EQ.'LINB') THEN
        CALL DQPO0( TST,0,NC2,'NSKP')
        CALL DGDHCA(TPROJ,TDRMD(IFIX(PDCODD(2,MODEDD))),
     &    TUNIT,PRN,RR)
        GO TO 99
      END IF
      IF(TST.EQ.'LINC') THEN
        CALL DQPO0( TST,0,NC1,'NSKP')
        CALL DGDHCA(TPROJ,TDRMD(IFIX(PDCODD(2,MODEDD))),
     &    TUNIT,PRN,RR)
        GO TO 99
      END IF
      IF(TPROJ(2:3).NE.'FT') THEN
        CALL DQPO0(TST,NC1,NC2,'NSKP')
        CALL DGDHCA(TPROJ,TDRMD(IFIX(PDCODD(2,MODEDD))),
     &    TUNIT,PRN,RR)
      ELSE
        CALL DQPO0('AREA',NC1,0,'NSKP')
        CALL DGDHCA(TPROJ,TDRMD(IFIX(PDCODD(2,MODEDD))),
     &    TUNIT,PRN,RR)
        IF(TST.EQ.'AREA') GO TO 99
        CALL DQPO0('LINE',0,NC2,'NSKP')
        CALL DGDHCA(TPROJ,TDRMD(IFIX(PDCODD(2,MODEDD))),
     &    TUNIT,PRN,RR)
      END IF
99    DLINDD=PDCODD(2,LITRDD)
      CALL DOFIS2
      END
*DK DODH3D
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DODH3D
CH
      SUBROUTINE DODH3D(TUNIT,ROT)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Modified by R.Vogl                         11.03.1992
C!:    draws 3 d view of HCAL (NO hidden line algorithm!!!!)
C!
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DH
      PARAMETER (NHBAM=12,NHECM=6)
      COMMON /DHCGEO/ CORADH(3,16,NHECM),CORBDH(3,8,NHBAM),
     &                CORCDH(3,16,NHECM),
     &                POLADH(2,16,NHECM),POLBDH(2,8,NHBAM),
     &                POLCDH(2,16,NHECM),
     &                RMNBDH,RMXBDH,RICBDH,ROCBDH,
     &                RMNEDH,RMXEDH,RMXIDH,RICEDH,RMCEDH,ROCEDH
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION ROT(6)
      CHARACTER *6 TUNIT
      DIMENSION ROTCO(2,16)
      DIMENSION H(10),V(10)
      DIMENSION IBP(4),IEP1(6),IEP2(6),IEP3(4)
      DATA IBP / 1, 3, 7, 5 /
      DATA IEP1 / 1, 3, 7, 11, 9, 5 /
      DATA IEP2 / 13, 15, 8, 12, 10, 6 /
      DATA IEP3 / 2, 4, 16, 14 /

C
C  STATEMENT FUNCTIONS :
C
      HCOR(X,Y,Z) = (ROT(2)*X+ROT(1)*Y)*ROT(4) + ROT(3)*Z
      VCOR(X,Y,Z) = ROT(6)*( ROT(2)*Y - ROT(1)*X ) +
     &              ROT(5)*( ROT(4)*Z - ROT(3)*(ROT(2)*X + ROT(1)*Y) )
      ZCOR(X,Y,Z) = ROT(5)*( ROT(1)*X - ROT(2)*Y ) +
     &              ROT(6)*( ROT(4)*Z - ROT(3)*(ROT(2)*X + ROT(1)*Y) )

C  TUNIT can be 'BARREL', 'ENDCAP' or 'BAR+EC'

      IF (TUNIT.EQ.'BARREL'.OR.TUNIT.EQ.'BAR+EC') THEN
        DO MD=1,NHBAM
          DO ICO=1,8
            ROTCO(1,ICO) =
     &        HCOR(CORBDH(1,ICO,MD),CORBDH(2,ICO,MD),CORBDH(3,ICO,MD))
            ROTCO(2,ICO) =
     &        VCOR(CORBDH(1,ICO,MD),CORBDH(2,ICO,MD),CORBDH(3,ICO,MD))
          END DO

          DO I=1,4
            H(I) = ROTCO(1,IBP(I))
            V(I) = ROTCO(2,IBP(I))
          END DO
          H(5)=H(1)
          V(5)=V(1)
          CALL DQPOL(5,H,V)

          DO I=1,4
            H(I) = ROTCO(1,IBP(I)+1)
            V(I) = ROTCO(2,IBP(I)+1)
          END DO
          H(5)=H(1)
          V(5)=V(1)
          CALL DQPOL(5,H,V)

          DO I=1,7,2
            H(1)= ROTCO(1,I)
            V(1)= ROTCO(2,I)
            H(2)= ROTCO(1,I+1)
            V(2)= ROTCO(2,I+1)
            CALL DQPOL(2,H,V)
          END DO
        END DO
      END IF

      IF (TUNIT.EQ.'ENDCAP'.OR.TUNIT.EQ.'BAR+EC') THEN
        DO MD=1,NHECM
          DO ICO=1,16
            ROTCO(1,ICO) =
     &        HCOR(CORADH(1,ICO,MD),CORADH(2,ICO,MD),CORADH(3,ICO,MD))
            ROTCO(2,ICO) =
     &        VCOR(CORADH(1,ICO,MD),CORADH(2,ICO,MD),CORADH(3,ICO,MD))
          END DO

          DO I=1,6
            H(I) = ROTCO(1,IEP1(I))
            V(I) = ROTCO(2,IEP1(I))
          END DO
          H(7)=H(1)
          V(7)=V(1)
          CALL DQPOL(7,H,V)

          DO I=1,6
            H(I) = ROTCO(1,IEP2(I))
            V(I) = ROTCO(2,IEP2(I))
          END DO
          H(7)=H(1)
          V(7)=V(1)
          CALL DQPOL(7,H,V)

          DO I=1,4
            H(I) = ROTCO(1,IEP3(I))
            V(I) = ROTCO(2,IEP3(I))
          END DO
          H(5)=H(1)
          V(5)=V(1)
          CALL DQPOL(5,H,V)

          DO I=1,15,2
            H(1)= ROTCO(1,I)
            V(1)= ROTCO(2,I)
            H(2)= ROTCO(1,I+1)
            V(2)= ROTCO(2,I+1)
            CALL DQPOL(2,H,V)
          END DO
        END DO
        DO MD=1,NHECM
          DO ICO=1,16
            ROTCO(1,ICO) =
     &        HCOR(CORCDH(1,ICO,MD),CORCDH(2,ICO,MD),CORCDH(3,ICO,MD))
            ROTCO(2,ICO) =
     &        VCOR(CORCDH(1,ICO,MD),CORCDH(2,ICO,MD),CORCDH(3,ICO,MD))
          END DO

          DO I=1,6
            H(I) = ROTCO(1,IEP1(I))
            V(I) = ROTCO(2,IEP1(I))
          END DO
          H(7)=H(1)
          V(7)=V(1)
          CALL DQPOL(7,H,V)

          DO I=1,6
            H(I) = ROTCO(1,IEP2(I))
            V(I) = ROTCO(2,IEP2(I))
          END DO
          H(7)=H(1)
          V(7)=V(1)
          CALL DQPOL(7,H,V)

          DO I=1,4
            H(I) = ROTCO(1,IEP3(I))
            V(I) = ROTCO(2,IEP3(I))
          END DO
          H(5)=H(1)
          V(5)=V(1)
          CALL DQPOL(5,H,V)

          DO I=1,15,2
            H(1)= ROTCO(1,I)
            V(1)= ROTCO(2,I)
            H(2)= ROTCO(1,I+1)
            V(2)= ROTCO(2,I+1)
            CALL DQPOL(2,H,V)
          END DO
        END DO
      END IF
      END
*DK DODITC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DODITC
CH
      SUBROUTINE DODITC(TPR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C    Modified by R.Vogl                         31.10.1989
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DT
      COMMON /DTVCCC/ DUMYDT(4),
     1         HUMIDT,HUMADT,VUMIDT,VUMADT,
     1         NSQUDT,NOBJDT,NOCLDT,HPOSDT,VPOSDT,
     1         FPOSDT,TPOSDT,AROTDT,RPOSDT,NTVIDT,
     1         FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,
     1         FX11DT
      LOGICAL  FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,FX11DT
C------------------------------------------------------------------- DR
      COMMON /DRNG1C/
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      LOGICAL
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      COMMON /DRNG2C/DET1DR(6),DET2DR(6)
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *3 TPR
      CHARACTER *3 TPROJ
      CHARACTER *4 TSTYL(0:3),TST,TDR
      DIMENSION RR(3)
      DATA  TSTYL /'LINB','LINC','AR+L','AREA' /
      TPROJ = TPR
      IF(FPIKDP.OR.NOCLDT.EQ.1) RETURN
      IF(.NOT.FITCDR) RETURN
      IF(PDCODD(4,ICITDD).NE.1.) RETURN
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PTO,J_PDS'
      CALL DPARAM(11
     &  ,J_PTO,J_PDS)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(PDCODD(4,MXITDD).EQ.1.AND.
     &   PDCODD(2,MXITDD).LT.PARADA(2,J_PTO)) RETURN
      CALL DOFISH('HC',TPROJ(2:3))
      NC1 = PDCODD(2,ICITDD)
      NC2 = PDCODD(2,KCITDD)
      TST = TSTYL(IFIX(PDCODD(2,ISTYDD)))
      IF(TST.EQ.'AR+L'.AND.PDCODD(4,KCITDD).NE.1.) TST='AREA'
      DLINDD=PDCODD(2,LIDTDD)
      IF(TPROJ(2:3).EQ.'YX') THEN
        TDR='SKIP'
      ELSE
        TDR='NSKP'
      END IF
C
C  SETUP FOR PERSPECTIVE PARAMETERS
C
      IF ((TPROJ(2:3).EQ.'YX').AND.(PARADA(4,J_PDS).EQ.1.)) THEN
        RR(2) = PARADA(2,J_PTO)
        RR(1) = 0.5*RR(2)
        P = PARADA(2,J_PDS)
        RR(3) = RR(2) - RR(1)/P
        TPROJ(2:3) = 'PP'
      END IF
C
C  PERSPECTIVE DISPLAY IS SWITCHED ON VIA PARADO(4,IYDPDO)
C
      IF(TST.EQ.'LINB') THEN
        CALL DQPO0( TST,0,NC2,TDR)
        CALL DGDITC(TPROJ,'BARREL',RR)
      ELSE IF(TST.EQ.'LINC') THEN
        CALL DQPO0( TST,0,NC1,TDR)
        CALL DGDITC(TPROJ,'BARREL',RR)
      ELSE
        CALL DQPO0(TST,NC1,NC2,TDR)
        CALL DGDITC(TPROJ,'BARREL',RR)
      END IF
   99 DLINDD=PDCODD(2,LITRDD)
      CALL DOFIS2
      END
*DK DODLCA
CH
CH
CH
CH
CH
      SUBROUTINE DODLCA(TPROJ)
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- Karsten Bormann & Fred Bird & Chris Grab
C!            20-JUL-1989
C!
C!   Modified :- R.Vogl                27-OCT-1989
C!   Modified :- R.Vogl                30-OCT-1989
C!
C!
C!   Description
C!   ===========
C!   This file plots the aleph luminosity calorimeter without
C!   plotting the 'data points'. In analogy to ECAL.
C!
C!
C!======================================================================
C
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DT
      COMMON /DTVCCC/ DUMYDT(4),
     1         HUMIDT,HUMADT,VUMIDT,VUMADT,
     1         NSQUDT,NOBJDT,NOCLDT,HPOSDT,VPOSDT,
     1         FPOSDT,TPOSDT,AROTDT,RPOSDT,NTVIDT,
     1         FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,
     1         FX11DT
      LOGICAL  FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,FX11DT
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *3 TPROJ
      CHARACTER *4 TSTYL(0:3),TST
      DATA  TSTYL /'LINB','LINC','AR+L','AREA' /
      IF(FPIKDP.OR.NOCLDT.EQ.1) RETURN
C      IF(.NOT.FECADR) RETURN
      IF(PDCODD(4,ICLUDD).NE.1.) RETURN
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PTO'
      CALL DPARAM(11
     &  ,J_PTO)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(PDCODD(4,MXLUDD).EQ.1.AND.
     &   PDCODD(2,MXLUDD).LT.PARADA(2,J_PTO)) RETURN
      CALL DOFISH('LC',TPROJ(2:3))
      NC1 = PDCODD(2,ICLUDD)
      NC2 = PDCODD(2,KCLUDD)
      TST = TSTYL(IFIX(PDCODD(2,ISTYDD)))
      IF(TST.EQ.'AR+L'.AND.PDCODD(4,KCLUDD).NE.1.) TST='AREA'
      DLINDD=PDCODD(2,LIDTDD)
      IF(TST.EQ.'LINB') THEN
        CALL DQPO0( TST,0,NC2,'NSKP')
        CALL DGDLCA(TPROJ,'ENDCAP')
      ELSE IF(TST.EQ.'LINC') THEN
        CALL DQPO0( TST,0,NC1,'NSKP')
        CALL DGDLCA(TPROJ,'ENDCAP')
      ELSE
        CALL DQPO0(TST,NC1,NC2,'NSKP')
        CALL DGDLCA(TPROJ,'ENDCAP')
      END IF
99    DLINDD=PDCODD(2,LITRDD)
      CALL DOFIS2
      END
C*DK DODSAT
CCH
CCH
CCH
CCH
CCH
C      SUBROUTINE DODSAT(TPROJ)
CC----------------------------------------------------------------------
CC!  -
CC!
CC!   Modified :- R.Vogl                27-OCT-1989
CC!   Modified :- R.Vogl                30-OCT-1989
CC!
CC!
CC!   Description
CC!   ===========
CC!
CC!
CC!======================================================================
CC
C      INCLUDE 'DALI_CF.INC'
C      CHARACTER *3 TPROJ
C      CHARACTER *4 TSTYL(0:3),TST
C      DATA  TSTYL /'LINB','LINC','AR+L','AREA' /
C      DATA LARUN /-1/
C      IF(FPIKDP.OR.NOCLDT.EQ.1) RETURN
C      IF(IRUNDE(1).GT.LARUN) THEN
CC  15000 is an arbitrary number --- it has to changed, when we know from which
CC  run starting the SATR is replaced by the SICAL. Calling the SICAL display
CC  routines will still be interfaced via the SATR routine!
C        CALL DODSIC(TPROJ)
C        RETURN
C      ENDIF
CC      IF(.NOT.FECADR) RETURN
C      IF(PDCODD(4,ICSADD).NE.1.) RETURN
CC     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
C      TPARDA=
C     &  'J_PTO'
C      CALL DPARAM(11
C     &  ,J_PTO)
CC     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
C      IF(PDCODD(4,MXSADD).EQ.1.AND.
C     &   PDCODD(2,MXSADD).LT.PARADA(2,J_PTO)) RETURN
C      NC1 = PDCODD(2,ICSADD)
C      NC2 = PDCODD(2,KCSADD)
C      TST = TSTYL(IFIX(PDCODD(2,ISTYDD)))
C      IF(TST.EQ.'AR+L'.AND.PDCODD(4,KCSADD).NE.1.) TST='AREA'
C      DLINDD=PDCODD(2,LIDTDD)
C      IF(TST.EQ.'LINB') THEN
C        CALL DQPO0( TST,0,NC2,'NSKP')
C        CALL DGDSAT(TPROJ,'ENDCAP')
C      ELSE IF(TST.EQ.'LINC') THEN
C        CALL DQPO0( TST,0,NC1,'NSKP')
C        CALL DGDSAT(TPROJ,'ENDCAP')
C      ELSE
C        CALL DQPO0(TST,NC1,NC2,'NSKP')
C        CALL DGDSAT(TPROJ,'ENDCAP')
C      END IF
C99    DLINDD=PDCODD(2,LITRDD)
C      END
*DK DODSIC
CH
CH
CH
CH
CH
      SUBROUTINE DODSIC(TPROJ)
C----------------------------------------------------------------------
C!  -
C!
C!   AUTHOR :- R.Vogl                26-NOV-1991
C!
C!
C!   Description
C!   ===========
C!
C!
C!======================================================================
C
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DT
      COMMON /DTVCCC/ DUMYDT(4),
     1         HUMIDT,HUMADT,VUMIDT,VUMADT,
     1         NSQUDT,NOBJDT,NOCLDT,HPOSDT,VPOSDT,
     1         FPOSDT,TPOSDT,AROTDT,RPOSDT,NTVIDT,
     1         FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,
     1         FX11DT
      LOGICAL  FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,FX11DT
C------------------------------------------------------------------- DE
C     ++++++++++++++++++++++++++++++ used in ATLANTIS , NRECDE not used here
      COMMON /DEVTIC/NFILDE,IRUNDE(2),IEVTDE(2),LNINDE(2),LCLSDE,NRECDE
      COMMON /DEVTIT/TFINDE(2)
      CHARACTER *80 TFINDE
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *3 TPROJ
      CHARACTER *4 TSTYL(0:3),TST
      DATA  TSTYL /'LINB','LINC','AR+L','AREA' /
      DATA LARUN /16480/
      IF(FPIKDP.OR.NOCLDT.EQ.1) RETURN
      IF(IRUNDE(1).LE.LARUN) RETURN
C      IF(.NOT.FECADR) RETURN
      IF(PDCODD(4,ICSADD).NE.1.) RETURN
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PTO'
      CALL DPARAM(11
     &  ,J_PTO)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(PDCODD(4,MXSADD).EQ.1.AND.
     &   PDCODD(2,MXSADD).LT.PARADA(2,J_PTO)) RETURN
      CALL DOFISH('SI',TPROJ(2:3))
      NC1 = PDCODD(2,ICSADD)
      NC2 = PDCODD(2,KCSADD)
      TST = TSTYL(IFIX(PDCODD(2,ISTYDD)))
      IF(TST.EQ.'AR+L'.AND.PDCODD(4,KCSADD).NE.1.) TST='AREA'
      DLINDD=PDCODD(2,LIDTDD)
      IF(TST.EQ.'LINB') THEN
        CALL DQPO0( TST,0,NC2,'NSKP')
        CALL DGDSIC(TPROJ,'ENDCAP')
      ELSE IF(TST.EQ.'LINC') THEN
        CALL DQPO0( TST,0,NC1,'NSKP')
        CALL DGDSIC(TPROJ,'ENDCAP')
      ELSE
        CALL DQPO0(TST,NC1,NC2,'NSKP')
        CALL DGDSIC(TPROJ,'ENDCAP')
      END IF
99    DLINDD=PDCODD(2,LITRDD)
      CALL DOFIS2
      END
*DK DODVDT
CH
CH
CH
CH
CH
      SUBROUTINE DODVDT(TPR)
C----------------------------------------------------------------------
C!  -
C!   Modified :- R.Vogl                27-OCT-1989
C!   Modified :- R.Vogl                30-OCT-1989
C!
C!
C!   Description
C!   ===========
C!
C!======================================================================
C
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DT
      COMMON /DTVCCC/ DUMYDT(4),
     1         HUMIDT,HUMADT,VUMIDT,VUMADT,
     1         NSQUDT,NOBJDT,NOCLDT,HPOSDT,VPOSDT,
     1         FPOSDT,TPOSDT,AROTDT,RPOSDT,NTVIDT,
     1         FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,
     1         FX11DT
      LOGICAL  FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,FX11DT
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *3 TPR
      CHARACTER *3 TPROJ
C      CHARACTER *6 TDRMD(0:3),TDR
      CHARACTER *6 TDR
      DIMENSION RR(3)
C      DATA TDRMD/'SIMPLE','DETAIL','SIMPLE','DETAIL'/
      CHARACTER * 4 TSKP(0:3),TSS
      DATA TSKP/'SKIP','NSKP','SKIP','NSKP'/
      CHARACTER *4 TSTYL(0:3),TST
      DATA  TSTYL /'LINB','LINC','AR+L','AREA' /
      TPROJ = TPR
      IF(FPIKDP.OR.NOCLDT.EQ.1) RETURN
      IF(PDCODD(4,ICVDDD).NE.1.) RETURN
C      IF(.NOT.FECADR) RETURN
      CALL DOFISH('VD',TPROJ(2:3))
      NC1 = PDCODD(2,ICVDDD)
      NC2 = PDCODD(2,KCVDDD)
      TST = TSTYL(IFIX(PDCODD(2,ISTYDD)))
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PTO'
      CALL DPARAM(11
     &  ,J_PTO)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
C      IF(TST.EQ.'AR+L'.AND.PDCODD(2,MXVDDD).GE.PARADA(2,J_PTO))
C     &  TST='AREA'
      IF(TST.EQ.'AR+L'.AND.PDCODD(4,KCVDDD).NE.1.) TST='AREA'
      TSS = TSKP(IFIX(PDCODD(2,MODEDD)))
      DLINDD=PDCODD(2,LIDTDD)
      IF(TPROJ(2:3).NE.'YX') THEN
        TSS = 'NSKP'
      END IF
      IF(PDCODD(4,MXVDDD).EQ.1.AND.
     &   PDCODD(2,MXVDDD).LT.PARADA(2,J_PTO)) THEN
        TDR='SIMPLE'
      ELSE
C       TDR=TDRMD(IFIX(PDCODD(2,MODEDD)))
        TDR='DETAIL'
      END IF
      IF(TPROJ(2:3).EQ.'FR'.AND.TDR.EQ.'DETAIL') TST='LINB'

C
C  PERSPECTIVE DISPLAY IS SWITCHED ON VIA PARADO(4,IYDPDO)


      IF(TST.EQ.'LINB') THEN
        CALL DQPO0( TST,0,NC2,TSS)
        CALL DGDVDT(TPROJ,TDR,'BARREL',RR)
      ELSE IF(TST.EQ.'LINC') THEN
        CALL DQPO0( TST,0,NC1,TSS)
        CALL DGDVDT(TPROJ,TDR,'BARREL',RR)
      ELSE
        CALL DQPO0(TST,NC1,NC2,TSS)
        CALL DGDVDT(TPROJ,TDR,'BARREL',RR)
      END IF
99    DLINDD=PDCODD(2,LITRDD)
      CALL DOFIS2
      END
*DK DODMAG
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DODMAG
CH
      SUBROUTINE DODMAG(TPROJ)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:  Modified by R.Vogl                        2.11.1989
C
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DT
      COMMON /DTVCCC/ DUMYDT(4),
     1         HUMIDT,HUMADT,VUMIDT,VUMADT,
     1         NSQUDT,NOBJDT,NOCLDT,HPOSDT,VPOSDT,
     1         FPOSDT,TPOSDT,AROTDT,RPOSDT,NTVIDT,
     1         FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,
     1         FX11DT
      LOGICAL  FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,FX11DT
C------------------------------------------------------------------- DR
      COMMON /DRNG1C/
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      LOGICAL
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      COMMON /DRNG2C/DET1DR(6),DET2DR(6)
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *3 TPROJ
      DATA RIR,ROR/248.,292./,NF/36./,ZM/350./,LDEB/0/
      DIMENSION HC(5),VC(5)
      CHARACTER *4 TSTYL(0:3),TST
      DATA  TSTYL /'LINB','LINC','AR+L','AREA' /

C
C  statement-functions
C
C      HPER(X,Y,RS) = X*(1+E*R2)/(1+E*RS)
C      VPER(X,Y,RS) = Y*(1+E*R2)/(1+E*RS)
C

      IF(FPIKDP.OR.NOCLDT.EQ.1) RETURN
      IF(.NOT.FHCADR) RETURN
      IF(PDCODD(4,ICMADD).LT.0.) RETURN
      TST=TSTYL(IFIX(PDCODD(2,ISTYDD)))
      IF(LDEB.EQ.0.AND.TST(1:3).EQ.'LIN') RETURN
      CALL DOFISH('MA',TPROJ(2:3))
      NC1 = IFIX(PDCODD(2,ICMADD))
      NC2 = IFIX(PDCODD(2,KCMADD))
      Q=RIR/ROR
      CALL DQPO0(TST,NC1,NC2,'SKIP')
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PDS'
      CALL DPARAM(11
     &  ,J_PDS)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(TPROJ.EQ.' YX') THEN
        IF(PARADA(4,J_PDS).NE.1.) THEN
          R=ROR
          FI=0.
          HC(1)=R*COSD(FI)
          VC(1)=R*SIND(FI)
          HC(2)=HC(1)*Q
          VC(2)=VC(1)*Q
          DF=360./NF
          DO   600  K=1,NF
            FI=FI+DF
            HC(4)=R*COSD(FI)
            VC(4)=R*SIND(FI)
            HC(3)=HC(4)*Q
            VC(3)=VC(4)*Q
            HC(5) = HC(1)
            VC(5) = VC(1)
            CALL DQPOL(5,HC,VC)
            HC(1)=HC(4)
            VC(1)=VC(4)
            HC(2)=HC(3)
            VC(2)=VC(3)
  600     CONTINUE
        ELSE

C          R2 = PARADO(2,IPTODO)
C          R1 = 0.5*R2
C          P  = PARADO(2,IYDPDO)
C          R1P= R2 - R1/P
C          E = (R1P - R1)/(R1*(R2-R1P))

          R=ROR
          FI=0.

          HC(1)=R*COSD(FI)
          VC(1)=R*SIND(FI)
          HC(2)=HC(1)*Q
          VC(2)=VC(1)*Q

          DF=360./NF
          DO   800  K=1,NF
            FI=FI+DF
            HH1=R*COSD(FI)
            VV1=R*SIND(FI)
            HH2=HH1*Q
            VV2=VV1*Q
            HC(4) = HH1
            VC(4) = VV1
            HC(3) = HH2
            VC(3) = VV2
            HC(5) = HC(1)
            VC(5) = VC(1)
            CALL DQPOL(5,HC,VC)
            HC(1)=HC(4)
            VC(1)=VC(4)
            HC(2)=HC(3)
            VC(2)=VC(3)
  800     CONTINUE
        END IF
      ELSE IF(TPROJ.EQ.'SRZ') THEN
        HC(1)=-ZM
        HC(2)= ZM
        HC(3)= ZM
        HC(4)=-ZM
        DO 700 S=-1.,1.,2.
          VC(1)=S*RIR
          VC(2)=VC(1)
          VC(3)=S*ROR
          VC(4)=VC(3)
          CALL DQPOL(4,HC,VC)
  700   CONTINUE
      ELSE IF(TPROJ.EQ.' FR') THEN
        HC(1)=RIR
        HC(2)=ROR
        HC(3)=ROR
        HC(4)=RIR
        VC(1)=-180.
        VC(2)=VC(1)
        VC(3)= 540.
        VC(4)=VC(3)
        CALL DQPOL(4,HC,VC)
      END IF
      CALL DOFIS2
      END
*DK DODMUD
C######################################################################
C
      SUBROUTINE DODMUD(TPR,TUNIT,LAYFT)
C
C-----------------------------------------
C
C   Author   :- R.Vogl                21-SEP-1989
C
C   Modified :- R.Vogl                27-OCT-1989
C
C=========================================
C
C   Purpose   : draw the muon-dedector in various projections
C   Inputs    : TPROJ : projection to be drawn
C               TUNIT : name of unit to be drawn ( BARREL, MIDANG,ENDCAP )
C               LAYFT : currently not used!!!!!!!!!!!!!!!!
C
C   Outputs   : none
C
C   Called by : DGLD
C
C=========================================
C +
C Declarations.
C -

C
C  FSLOT( I , J ) is true if slot had a hit
C
C       J   subcomponent number : 1 .. endcap
C                                 2 .. middleangle
C                                 3 .. barrel
C
C       I   slot number         : from 1 thru 38 ( max ! )
C
C
C  Specifications for TUNIT :
C
C               'BARREL','MIDANG','ENDCAP',
C               'BAR+MA','B+MA+E','BAR+EC'
C
C
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DT
      COMMON /DTVCCC/ DUMYDT(4),
     1         HUMIDT,HUMADT,VUMIDT,VUMADT,
     1         NSQUDT,NOBJDT,NOCLDT,HPOSDT,VPOSDT,
     1         FPOSDT,TPOSDT,AROTDT,RPOSDT,NTVIDT,
     1         FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,
     1         FX11DT
      LOGICAL  FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,FX11DT
C------------------------------------------------------------------- DR
      COMMON /DRNG1C/
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      LOGICAL
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      COMMON /DRNG2C/DET1DR(6),DET2DR(6)
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C------------------------------------------------------------------- DV
      COMMON /DVARIC/
     1      IVXXDV,IVYYDV,IVZZDV,IVRODV,IVFIDV,IVTEDV,IVDIDV,IVRBDV,
     1      IVLVDV,IVDFDV,IVDTDV,IVENDV,IVIIDV,IVJJDV,IVKKDV,IVNNDV,
     1      IVLLDV,IVNTDV,IVU1DV,IVU2DV
C------------------------------------------------------------------- DF
      PARAMETER (MCOLDF=10)
      COMMON /DFTCLC/ COLIDF(4,MCOLDF),COLMDF(4,3)
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *3 TPR
      CHARACTER *3 TP
      CHARACTER *6 TUNIT,TUN
      CHARACTER *4 TSTYL(0:3),TST
      DIMENSION RR(3)
      DATA  TSTYL /'LINB','LINC','AR+L','AREA' /,DLIN/1./
      LOGICAL FOUT,FSEL,FSLOT(38,3),FSL(114)
      EQUIVALENCE (FSLOT,FSL)
      TP = TPR
      IF(FPIKDP.OR.NOCLDT.EQ.1) RETURN
      IF(.NOT.FMDEDR) RETURN
      CALL DOFISH('MD',TP(2:3))
      NC1=PDCODD(2,ICM1DD)
      NC2=PDCODD(2,ICM2DD)
      NC3=PDCODD(2,KCM1DD)
      TST=TSTYL(IFIX(PDCODD(2,ISTYDD)))
      IF(TST.EQ.'AR+L'.AND.PDCODD(4,KCM1DD).NE.1.) TST='AREA'
      DLINDD=DLIN
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PTO,J_PDS,J_PLM'
      CALL DPARAM(10
     &  ,J_PTO,J_PDS,J_PLM)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(  (TP(2:3).EQ.'YX'.AND.TUNIT.NE.'ENDCAP')
     &  .OR.TP(2:3).EQ.'FR') THEN
        MO=PARADA(2,J_PLM)
C
C  SETUP FOR PERSPECTIVE PARAMETERS
C
        IF ((TP(2:3).EQ.'YX').AND.(PARADA(4,J_PDS).EQ.1.)) THEN
          TST='AREA'
          TP(2:3) = 'PP'
          RR(2) = PARADA(2,J_PTO)
          RR(1) = 0.5*RR(2)
          P = PARADA(2,J_PDS)
          RR(3) = RR(2) - RR(1)/P
        END IF
C
C  PERSPECTIVE DISPLAY IS SWITCHED ON VIA PARADO(4,IYDPDO)

C                               ONLY SLOTS

        IF(MO.LE.1) THEN
          CALL DV0(MHITDB,NUM1,NUM2,FOUT)
          IF(FOUT) GO TO 99
          DO N=1,114
            FSL(N)=.FALSE.
          END DO
          DO N=1,NUM2
            NSUBC=DVMD(16,N)
            NSLOT=DVMD(13,N)
            YLOC =DVMD(IVLVDV,N)
C
C  it is still questionable if this is desirable !!
C
            IF (YLOC.NE.0.) THEN
              FSLOT(NSLOT,NSUBC)=.TRUE.
            ELSE
              IF (NSUBC.EQ.3) THEN
                FSLOT(NSLOT,NSUBC)=.TRUE.
              END IF
            END IF
C
C
          END DO
          TUN='BAR+MA'
          FSEL=.TRUE.
          GO TO 10
        ELSE IF(MO.EQ.2) THEN
C                               BARREL
          TUN='BARREL'
          FSEL=.FALSE.
          GO TO 10
        ELSE IF(MO.EQ.3) THEN
C                               MIDDLE ANGLE
          TUN='MIDANG'
          FSEL=.FALSE.
          GO TO 10
        ELSE
C                               BOTH
          TUN='BAR+MA'
          FSEL=.FALSE.
          GO TO 10
        END IF
      ELSE IF(TP(2:3).EQ.'FT') THEN
        NCFT=COLIDF(2,6)
        IF(NCFT.EQ.0) GO TO 99
C        IF(NCFT.EQ.1) NC2=-1
C        IF(NCFT.EQ.2) NC1=-1
        TUN='B+MA+E'
        FSEL=.FALSE.
        GO TO 10
      ELSE
        IF (PDCODD(4,ICM1DD).GT.0.) THEN
          IF(TST.EQ.'LINB') THEN
            CALL DQPO0( TST,0,NC3,'NSKP')
          ELSE IF(TST.EQ.'LINC') THEN
            CALL DQPO0( TST,0,NC1,'NSKP')
          ELSE
            CALL DQPO0(TST,NC1,NC3,'NSKP')
          END IF
          CALL DGDMUD(TP,TUNIT,PDCODD(2,NUPODD),1,.FALSE.,FSLOT,RR)
        END IF
        IF (PDCODD(4,ICM2DD).GT.0.) THEN
          IF(TST.EQ.'LINB') THEN
            CALL DQPO0( TST,0,NC3,'NSKP')
          ELSE IF(TST.EQ.'LINC') THEN
            CALL DQPO0( TST,0,NC2,'NSKP')
          ELSE
            CALL DQPO0(TST,NC2,NC3,'NSKP')
          END IF
          CALL DGDMUD(TP,TUNIT,PDCODD(2,NUPODD),2,.FALSE.,FSLOT,RR)
        END IF
      END IF
      GO TO 99
10    IF(TST.EQ.'LINB') THEN
        IF(PDCODD(4,ICM1DD).GT.0.) THEN
          CALL DQPO0('LINE',0,NC3,'NSKP')
          CALL DGDMUD(TP,TUN,PDCODD(2,NUPODD),1,FSEL,FSLOT,RR)
        END IF
        IF(PDCODD(4,ICM2DD).GT.0.) THEN
          CALL DQPO0('LINE',0,NC3,'NSKP')
          CALL DGDMUD(TP,TUN,PDCODD(2,NUPODD),2,FSEL,FSLOT,RR)
        END IF
        GO TO 99
      END IF
      IF(TST.EQ.'LINC') THEN
        IF (PDCODD(4,ICM1DD).GT.0.) THEN
          CALL DQPO0('LINE',0,NC1,'NSKP')
          CALL DGDMUD(TP,TUN,PDCODD(2,NUPODD),1,FSEL,FSLOT,RR)
        END IF
        IF (PDCODD(4,ICM2DD).GT.0.) THEN
          CALL DQPO0('LINE',0,NC2,'NSKP')
          CALL DGDMUD(TP,TUN,PDCODD(2,NUPODD),2,FSEL,FSLOT,RR)
        END IF
        GO TO 99
      END IF
      IF(PDCODD(4,ICM1DD).GT.0.) THEN
        CALL DQPO0('AREA',NC1,0,'NSKP')
        CALL DGDMUD(TP,TUN,PDCODD(2,NUPODD),1,FSEL,FSLOT,RR)
      END IF
      IF(PDCODD(4,ICM2DD).GT.0.) THEN
        CALL DQPO0('AREA',NC2,0,'NSKP')
        CALL DGDMUD(TP,TUN,PDCODD(2,NUPODD),2,FSEL,FSLOT,RR)
      END IF
      IF(TST.EQ.'AREA') GO TO 99
      IF(PDCODD(4,ICM1DD).GT.0.) THEN
        CALL DQPO0('LINE',0,NC3,'NSKP')
        CALL DGDMUD(TP,TUN,PDCODD(2,NUPODD),1,FSEL,FSLOT,RR)
      END IF
      IF(PDCODD(4,ICM2DD).GT.0.) THEN
        CALL DQPO0('LINE',0,NC3,'NSKP')
        CALL DGDMUD(TP,TUN,PDCODD(2,NUPODD),2,FSEL,FSLOT,RR)
      END IF
99    DLINDD=PDCODD(2,LITRDD)
      CALL DOFIS2
      END
*DK DODTPC
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DODTPC
CH
      SUBROUTINE DODTPC(TPR)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C    Modified by R.Vogl                         31.10.1989
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DP
      COMMON /DPICKC/ FPIKDP,FPIMDP,DPIKDP,HPIKDP,VPIKDP,
     1                              HHPKDP,VVPKDP,EVARDP(20,3),
     1 KPIKDP,NPIKDP,MDLRDP,MDLPDP,FKTRDP,FNTRDP,KSUPDP,NSUPDP
C     +++++++++++++++++++ FTRKDP,IOTRDP used in ATLANTIS
      COMMON /DPICK1/ IPTNDP,FTRKDP,IOTRDP
      LOGICAL FPIKDP,FPIMDP,FTRKDP
C------------------------------------------------------------------- DT
      COMMON /DTVCCC/ DUMYDT(4),
     1         HUMIDT,HUMADT,VUMIDT,VUMADT,
     1         NSQUDT,NOBJDT,NOCLDT,HPOSDT,VPOSDT,
     1         FPOSDT,TPOSDT,AROTDT,RPOSDT,NTVIDT,
     1         FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,
     1         FX11DT
      LOGICAL  FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,FX11DT
C------------------------------------------------------------------- DR
      COMMON /DRNG1C/
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      LOGICAL
     &  FNOPDR,FVDEDR,FITCDR,FTPCDR,FECADR,FHCADR,FMDEDR
      COMMON /DRNG2C/DET1DR(6),DET2DR(6)
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C------------------------------------------------------------------- DQ
      COMMON /DQQQ1C/ AHSCDQ,BHSCDQ,CHSCDQ,AVSCDQ,BVSCDQ,CVSCDQ
      COMMON /DQQQ2C/ DMAXDQ
      COMMON /DQQQ3C/ FPRSDQ,FPSQDQ
      LOGICAL FPRSDQ,FPSQDQ
      COMMON /DQQQ4C/ PRSHDQ,PRSVDQ,PRS1DQ,PRS2DQ,PRV1DQ
      COMMON /DQQQ5C/ HUSRDQ(4,0:12),VUSRDQ(4,0:12)
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      CHARACTER *3 TPR
      CHARACTER *3 TPROJ
      CHARACTER *6 TDR,TDRMD(0:3)
      DIMENSION RR(3)
      DATA TDRMD/'SIMPLE','DETAIL','NOGAPS','NOGAPS'/
      CHARACTER *4 TSTYL(0:3),TST
      DATA  TSTYL /'LINB','LINC','AR+L','AREA' /
      DIMENSION ROT(6)
      DATA CMPIX/32./
      DATA VR/31.8/,ZZ/220./
      DATA IDEB/0/,MDEB/0/
      DATA F111/0./
      TPROJ = TPR
      IF(FPIKDP.OR.NOCLDT.EQ.1) RETURN
      IF(.NOT.FTPCDR) RETURN
      IF(PDCODD(4,ICTPDD).NE.1.) RETURN
      CALL DOFISH('TP',TPROJ(2:3))
      NC1 = PDCODD(2,ICTPDD)
      NC2 = PDCODD(2,KCTPDD)
      TST = TSTYL(IFIX(PDCODD(2,ISTYDD)))
      IF(TST.EQ.'AR+L'.AND.PDCODD(4,KCTPDD).NE.1.) TST='AREA'
      DLINDD=PDCODD(2,LIDTDD)
      NN = PDCODD(2,MODEDD)
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PFY,J_PFI,J_PTE,J_PTO,J_PDS,J_PGA,J_RAL,J_RES,J_REY'
      CALL DPARAM(10
     &  ,J_PFY,J_PFI,J_PTE,J_PTO,J_PDS,J_PGA,J_RAL,J_RES,J_REY)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(PDCODD(4,MXTPDD).EQ.1.AND.
     &   PDCODD(2,MXTPDD).LT.PARADA(2,J_PTO)) NN=0
      IF(TPROJ(2:3).EQ.'YX'.OR.(MDEB.EQ.1.AND.TPROJ(2:3).EQ.'FR')) THEN
        TDR = TDRMD(NN)
C
C  SETUP FOR PERSPECTIVE PARAMETERS
C
        IF ((TPROJ(2:3).EQ.'YX').AND.(PARADA(4,J_PDS).EQ.1.)) THEN
          TPROJ(2:3) = 'PP'
          RR(2) = PARADA(2,J_PTO)
          RR(1) = 0.5*RR(2)
          P = PARADA(2,J_PDS)
          RR(3) = RR(2) - RR(1)/P
        END IF
C
C  PERSPECTIVE DISPLAY IS SWITCHED ON VIA PARADO(4,IYDPDO)

        IF(TST.EQ.'LINB') THEN
          CALL DQPO0( TST,0,NC2,'NSKP')
          CALL DGDTPC(TPROJ,TDR,'BARREL',ROT,RR)
        ELSE IF(TST.EQ.'LINC') THEN
          CALL DQPO0( TST,0,NC1,'NSKP')
          CALL DGDTPC(TPROJ,TDR,'BARREL',ROT,RR)
        ELSE
          IF(TDR.EQ.'SIMPLE') THEN
            CALL DQPO0('AREA',NC1,NC2,'NSKP')
            CALL DGDTPC(TPROJ,'NOGAPS','BARREL',ROT,RR)
            IF(TST.EQ.'AR+L') THEN
              CALL DQPO0('LINE',0,NC2,'NSKP')
              CALL DGDTPC(TPROJ,TDR,'BARREL',ROT,RR)
            END IF
          ELSE
            CALL DQPO0(TST,NC1,NC2,'NSKP')
            CALL DGDTPC(TPROJ,TDR,'BARREL',ROT,RR)
          END IF
        END IF
      ELSE IF(TPROJ.EQ.'3-D') THEN
        ROT(1) = SIND(PARADA(2,J_PFI))
        ROT(2) = COSD(PARADA(2,J_PFI))
        THETA  = PARADA(2,J_PTE)
        IF ((THETA.GT. 89.9999).AND.(THETA.LT.90.0001)) THEN
          THETA = 90.0001
        END IF
        ROT(3) = SIND(90. - THETA)
        ROT(4) = COSD(90. - THETA)
        ROT(5) = SIND(PARADA(2,J_RAL))
        ROT(6) = COSD(PARADA(2,J_RAL))

        IF (PARADA(4,J_RES).EQ.1.) THEN
          A=PARADA(2,J_RES)*CMPIX/AHSCDQ
          E=PARADA(2,J_REY)*CMPIX/AHSCDQ
          CALL DODT3P(TST,ROT,E,A)
        ELSE
          IF (IDEB.EQ.0) THEN
            CALL DODT3D(TST,ROT)
          ELSE
            CALL DQPO0('AREA',NC1,NC2,'NSKP')
            CALL DGDTPC(TPROJ,'SIMPLE','BARREL',ROT,RR)
            CALL DQPO0('LINB',0,NC2,'NSKP')
            CALL DGDTPC(TPROJ,'SIMPLE','BARREL',ROT,RR)
          END IF
        END IF

      ELSE
        IF (NN.GT.1) NN=NN-2
        TDR = TDRMD(NN)
        IF(TST.EQ.'LINB') THEN
          CALL DQPO0(TST,0,NC2,'NSKP')
          CALL DGDTPC(TPROJ,TDR,'BARREL',ROT,RR)
        ELSE IF(TST.EQ.'LINC') THEN
          CALL DQPO0(TST,0,NC1,'NSKP')
          CALL DGDTPC(TPROJ,TDR,'BARREL',ROT,RR)
        ELSE
          CALL DQPO0(TST,NC1,NC2,'NSKP')
          CALL DGDTPC(TPROJ,TDR,'BARREL',ROT,RR)
        END IF
      END IF
      IF(TPR.EQ.' YZ'.AND.PARADA(4,J_PGA).EQ.1..AND.
     &    ZZ.GT.0.   .AND.PARADA(2,J_PGA).GT.0.) THEN
        FIMID=PARADA(2,J_PFI)
        FMIN=FIMID-90.
        FMAX=FIMID+90.
        SF=SIND(PARADA(2,J_PFY))
        CF=COSD(PARADA(2,J_PFY))
        VY=-SF*VR*COSD(FMIN+F111)+CF*VR*SIND(FMIN+F111)
        SG=SIND(PARADA(2,J_PGA))
        CG=COSD(PARADA(2,J_PGA))
        QG=1./(SG+CG)
        V=QG*(CG*VY+SG*VR)
        IF(V.GT.0.) THEN
          CALL DQLEVL(ICBGDD)
          CALL DQAR0(1.,1.)
          CALL DQAR(-ZZ,-V,ZZ,V)
          CALL DQLEVL(KCTPDD)
          CALL DQL2E(-ZZ,-V,ZZ,-V)
          CALL DQL2E(-ZZ, V,ZZ, V)
        END IF
      END IF
      DLINDD=PDCODD(2,LITRDD)
      CALL DOFIS2
      END
C
C
C
*DK DODT3D
      SUBROUTINE DODT3D(TST,ROT)
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- R.Vogl                26-AUG-1990
C!
C!   Inputs:
C!        -
C!
C!   Outputs:
C!        -
C!
C!   Libraries required:
C!
C!   Description
C!   ===========
C!
C?
C!======================================================================
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DT
      COMMON /DTPGEO/ FISTDT(3),
     &       CORNDT (2,10,6,3), POLCDT ( 2,10,3 ),
     &       CRNGDT (2,10,6,3),
     &       NTYPDT,NCORDT(3),NSLTDT,
     &       RMAXDT , RMINDT , ZMAXDT
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION ROT(6)
      DIMENSION HRO(25,2),VRO(25,2),ZRO(25,2)
      DIMENSION HMAR(4,2),VMAR(4,2)
      DIMENSION HSID(25),VSID(25)
      CHARACTER *4 TST
      DATA DL2 / 1. /
      DATA IREDP/1/
      DATA RLEN0 / 25. /
C      DATA IDEB  / 1 /
      DATA LSYM  / 12 /
      DATA NSYM  / 8 /
      DATA DSYM  / 6. /
C
C  STATEMENT FUNCTIONS :
C
      HCOR(X,Y,Z) = (ROT(2)*X+ROT(1)*Y)*ROT(4) + ROT(3)*Z
      VCOR(X,Y,Z) = ROT(6)*( ROT(2)*Y - ROT(1)*X ) +
     &              ROT(5)*( ROT(4)*Z - ROT(3)*(ROT(2)*X + ROT(1)*Y) )
      ZCOR(X,Y,Z) = ROT(5)*( ROT(1)*X - ROT(2)*Y ) +
     &              ROT(6)*( ROT(4)*Z - ROT(3)*(ROT(2)*X + ROT(1)*Y) )

      NC1 = PDCODD(2,ICTPDD)
      NC2 = PDCODD(2,KCTPDD)

          J1 = 1
          J2 = 1
          DO J = 1 , NSLTDT
C
C     right side ( z>0 ) outer corners

            HRO(J2  ,1)=HCOR(CRNGDT(1,6,J,2),CRNGDT(2,6,J,2), ZMAXDT)
            VRO(J2  ,1)=VCOR(CRNGDT(1,6,J,2),CRNGDT(2,6,J,2), ZMAXDT)
            ZRO(J2  ,1)=ZCOR(CRNGDT(1,6,J,2),CRNGDT(2,6,J,2), ZMAXDT)
            HRO(J2+1,1)=HCOR(CRNGDT(1,5,J,2),CRNGDT(2,5,J,2), ZMAXDT)
            VRO(J2+1,1)=VCOR(CRNGDT(1,5,J,2),CRNGDT(2,5,J,2), ZMAXDT)
            ZRO(J2+1,1)=ZCOR(CRNGDT(1,5,J,2),CRNGDT(2,5,J,2), ZMAXDT)

            HRO(J2+2,1)=HCOR(CRNGDT(1,6,J,3),CRNGDT(2,6,J,3), ZMAXDT)
            VRO(J2+2,1)=VCOR(CRNGDT(1,6,J,3),CRNGDT(2,6,J,3), ZMAXDT)
            ZRO(J2+2,1)=ZCOR(CRNGDT(1,6,J,3),CRNGDT(2,6,J,3), ZMAXDT)
            HRO(J2+3,1)=HCOR(CRNGDT(1,5,J,3),CRNGDT(2,5,J,3), ZMAXDT)
            VRO(J2+3,1)=VCOR(CRNGDT(1,5,J,3),CRNGDT(2,5,J,3), ZMAXDT)
            ZRO(J2+3,1)=ZCOR(CRNGDT(1,5,J,3),CRNGDT(2,5,J,3), ZMAXDT)

C   left side ( z<0 ) outer corners

            HRO(J2  ,2)=HCOR(CRNGDT(1,6,J,2),CRNGDT(2,6,J,2),-ZMAXDT)
            VRO(J2  ,2)=VCOR(CRNGDT(1,6,J,2),CRNGDT(2,6,J,2),-ZMAXDT)
            ZRO(J2  ,2)=ZCOR(CRNGDT(1,6,J,2),CRNGDT(2,6,J,2),-ZMAXDT)
            HRO(J2+1,2)=HCOR(CRNGDT(1,5,J,2),CRNGDT(2,5,J,2),-ZMAXDT)
            VRO(J2+1,2)=VCOR(CRNGDT(1,5,J,2),CRNGDT(2,5,J,2),-ZMAXDT)
            ZRO(J2+1,2)=ZCOR(CRNGDT(1,5,J,2),CRNGDT(2,5,J,2),-ZMAXDT)

            HRO(J2+2,2)=HCOR(CRNGDT(1,6,J,3),CRNGDT(2,6,J,3),-ZMAXDT)
            VRO(J2+2,2)=VCOR(CRNGDT(1,6,J,3),CRNGDT(2,6,J,3),-ZMAXDT)
            ZRO(J2+2,2)=ZCOR(CRNGDT(1,6,J,3),CRNGDT(2,6,J,3),-ZMAXDT)
            HRO(J2+3,2)=HCOR(CRNGDT(1,5,J,3),CRNGDT(2,5,J,3),-ZMAXDT)
            VRO(J2+3,2)=VCOR(CRNGDT(1,5,J,3),CRNGDT(2,5,J,3),-ZMAXDT)
            ZRO(J2+3,2)=ZCOR(CRNGDT(1,5,J,3),CRNGDT(2,5,J,3),-ZMAXDT)

            J2 = J2 + 4

          END DO

          HRO(25,1) = HRO(1,1)
          VRO(25,1) = VRO(1,1)
          ZRO(25,1) = ZRO(1,1)
          HRO(25,2) = HRO(1,2)
          VRO(25,2) = VRO(1,2)
          ZRO(25,2) = ZRO(1,2)

          DO II=1,2
            HMAR(1,II) = 0.5*(HRO(6 ,II)+HRO(5 ,II))
            VMAR(1,II) = 0.5*(VRO(6 ,II)+VRO(5 ,II))

            HMAR(2,II) = 0.5*(HRO(12,II)+HRO(11,II))
            VMAR(2,II) = 0.5*(VRO(12,II)+VRO(11,II))

            HMAR(3,II) = 0.5*(HRO(18,II)+HRO(17,II))
            VMAR(3,II) = 0.5*(VRO(18,II)+VRO(17,II))

            HMAR(4,II) = 0.5*(HRO(24,II)+HRO(23,II))
            VMAR(4,II) = 0.5*(VRO(24,II)+VRO(23,II))
          END DO

          ZMEL = 0.5 * (ZRO(1,2)+ZRO(13,2))
          ZMER = 0.5 * (ZRO(1,1)+ZRO(13,1))
          IF (ZMEL.GT.ZMER) THEN
            JSS = 2
            JSU = 1
          ELSE
            JSS = 1
            JSU = 2
          END IF

          DMAX=0.
          DO II = 1 , 24
            A =  ( VRO(II,1) - VRO(II,2)) / ( HRO(II,1) - HRO(II,2) )
            B =  VRO(II,2) - A*HRO(II,2)
            D = ABS( B / SQRT(1. + A*A) )

            IF (D.GT.DMAX) THEN
              JMAX = II
              DMAX = D
            END IF
          END DO

          IF (JMAX.LE.12) THEN
            JJ1 = JMAX
            JJ2 = JMAX + 12
          ELSE
            JJ1 = JMAX - 12
            JJ2 = JMAX
          END IF

          CALL DQPO0('AREA',NC1,NC2,'NSKP')

          IF ((TST.EQ.'AREA').OR.(TST.EQ.'AR+L')) THEN
            DO I = 1 , 25
              HSID(I) = HRO(I,1)
              VSID(I) = VRO(I,1)
            END DO
            CALL DQPOL(25,HSID,VSID)

            DO I = 1 , 25
              HSID(I) = HRO(I,2)
              VSID(I) = VRO(I,2)
            END DO
            CALL DQPOL(25,HSID,VSID)

            HSID(1) = HRO(JJ1,2)
            HSID(2) = HRO(JJ2,2)
            HSID(3) = HRO(JJ2,1)
            HSID(4) = HRO(JJ1,1)
            HSID(5) = HSID(1)
            VSID(1) = VRO(JJ1,2)
            VSID(2) = VRO(JJ2,2)
            VSID(3) = VRO(JJ2,1)
            VSID(4) = VRO(JJ1,1)
            VSID(5) = VSID(1)
            CALL DQPOL(5,HSID,VSID)
          END IF
C
C
          CALL DQPO0('LINB',0,NC2,'NSKP')
C
C  VISIBLE SIDES OF DETECTOR ARE DRAWN WITH THICK LINES
C
          DLIN1=PDCODD(2,LIDTDD)
C          IF(ROECDU.NE.0..OR.ROHCDU.NE.0..OR.ROVDDU.NE.0.) THEN
C            DLIN3=DLIN1
C            INORP=0
C          ELSE
C            DLIN3=MIN(5.,DLIN1+DL2)
C            INORP=IREDP
C          END IF
C          DLINDD = DLIN3
          INORP=IREDP
          DLINDD = DLIN1
          DO I = 1 , 25
            HSID(I) = HRO(I,JSS)
            VSID(I) = VRO(I,JSS)
          END DO
          CALL DQPOL(25,HSID,VSID)

          HSID(1) = HRO(JJ1,2)
          HSID(2) = HRO(JJ1,1)
          VSID(1) = VRO(JJ1,2)
          VSID(2) = VRO(JJ1,1)
          CALL DQPOL(2,HSID,VSID)

          HSID(1) = HRO(JJ2,2)
          HSID(2) = HRO(JJ2,1)
          VSID(1) = VRO(JJ2,2)
          VSID(2) = VRO(JJ2,1)
          CALL DQPOL(2,HSID,VSID)

          ZMP6 = .5 * ( ZRO(JJ1+6,2) + ZRO(JJ1+6,1))
          JMP6 = JJ1 + 6
          IF (JJ1.GT.6) THEN
            ZMM6 = .5 * ( ZRO(JJ1-6,2) + ZRO(JJ1-6,1))
            JMM6 = JJ1 - 6
          ELSE
            ZMM6 = .5 * ( ZRO(JJ2+6,2) + ZRO(JJ2+6,1))
            JMM6 = JJ2 + 6
          END IF

          IF (ZMP6.GT.ZMM6) THEN
            JJS = JMP6
            JJU = JMM6
          ELSE
            JJS = JMM6
            JJU = JMP6
          END IF

          IF (JJS.LE.5) THEN
            JMS1 = 1
            JMS2 = 4
            JMU1 = 2
            JMU2 = 3
          ELSE IF (JJS.LE.11) THEN
            JMS1 = 2
            JMS2 = 1
            JMU1 = 3
            JMU2 = 4
          ELSE IF (JJS.LE.17) THEN
            JMS1 = 3
            JMS2 = 2
            JMU1 = 4
            JMU2 = 1
          ELSE IF (JJS.LE.23) THEN
            JMS1 = 4
            JMS2 = 3
            JMU1 = 1
            JMU2 = 2
          ELSE
            JMS1 = 1
            JMS2 = 4
            JMU1 = 2
            JMU2 = 3
          END IF
C
C   draw lines perpendicular to furthest seen lines
C
C          IF (IDEB.EQ.0) THEN
C            HSID(1) = HRO(JJS,2)
C            HSID(2) = HRO(JJS,1)
C            VSID(1) = VRO(JJS,2)
C            VSID(2) = VRO(JJS,1)
C            CALL DQPOL(2,HSID,VSID)
C          ELSE
C            DH = HRO(JJS,2) - HRO(JJS,1)
C            DV = VRO(JJS,2) - VRO(JJS,1)
C            RN = SQRT( DH*DH + DV*DV )
C
C            HSID(1) = HRO(JJS,2) - RLEN / RN * DH
C            HSID(2) = HRO(JJS,2)
C            VSID(1) = VRO(JJS,2) - RLEN / RN * DV
C            VSID(2) = VRO(JJS,2)
C            CALL DQPOL(2,HSID,VSID)
C
C            HSID(1) = HRO(JJS,1) + RLEN / RN * DH
C            HSID(2) = HRO(JJS,1)
C            VSID(1) = VRO(JJS,1) + RLEN / RN * DV
C            VSID(2) = VRO(JJS,1)
C            CALL DQPOL(2,HSID,VSID)
C          END IF

C
C  draw markers for y=0 and x=0 lines (edges)!
C

C
C  to indicate orientation of TPC in projection, the markers for edge
C  with phi=0, z>0 is drawn 2.5-times as long as the others
C
C  to switch of THE RED POINT, set IREDP=0 in the debugger!

          IF (INORP.NE.0) THEN
            DH = HMAR(JMS1,2) - HMAR(JMS1,1)
            DV = VMAR(JMS1,2) - VMAR(JMS1,1)
            RN = SQRT( DH*DH + DV*DV )
            RLEN = RLEN0 * ( RN / (2*ZMAXDT))
            IF (JMS1.NE.1) THEN
              RL = RLEN
            ELSE
              RL = 2.5*RLEN
            END IF

            HSID(1) = HMAR(JMS1,2) - RLEN / RN * DH
            HSID(2) = HMAR(JMS1,2)
            VSID(1) = VMAR(JMS1,2) - RLEN / RN * DV
            VSID(2) = VMAR(JMS1,2)
            CALL DQPOL(2,HSID,VSID)
            HSID(1) = HMAR(JMS1,1) + RL / RN * DH
            HSID(2) = HMAR(JMS1,1)
            VSID(1) = VMAR(JMS1,1) + RL / RN * DV
            VSID(2) = VMAR(JMS1,1)
            CALL DQPOL(2,HSID,VSID)

            DH = HMAR(JMS2,2) - HMAR(JMS2,1)
            DV = VMAR(JMS2,2) - VMAR(JMS2,1)
            RN = SQRT( DH*DH + DV*DV )
            IF (JMS2.NE.1) THEN
              RL = RLEN
            ELSE
              RL = 2.5*RLEN
            END IF

            HSID(1) = HMAR(JMS2,2) - RLEN / RN * DH
            HSID(2) = HMAR(JMS2,2)
            VSID(1) = VMAR(JMS2,2) - RLEN / RN * DV
            VSID(2) = VMAR(JMS2,2)
            CALL DQPOL(2,HSID,VSID)
            HSID(1) = HMAR(JMS2,1) + RL / RN * DH
            HSID(2) = HMAR(JMS2,1)
            VSID(1) = VMAR(JMS2,1) + RL / RN * DV
            VSID(2) = VMAR(JMS2,1)
            CALL DQPOL(2,HSID,VSID)
          END IF


          IF ((JJS.GT.JJ2).OR.(JJS.LT.JJ1)) THEN
            L = 1
            DO K=JJ2,24
              HSID(L) = HRO(K,JSU)
              VSID(L) = VRO(K,JSU)
              L = L + 1
            END DO
            DO K=1,JJ1
              HSID(L) = HRO(K,JSU)
              VSID(L) = VRO(K,JSU)
              L = L + 1
            END DO
          ELSE
            L = 1
            DO K=JJ1,JJ2
              HSID(L) = HRO(K,JSU)
              VSID(L) = VRO(K,JSU)
              L = L + 1
            END DO
          END IF
          CALL DQPOL(13,HSID,VSID)

C
C   CHANGE HERE TO THIN LINES !!!!!!!!!!!!!!
C
          DLINDD = DLIN1

C          IF (IDEB.EQ.0) THEN
C            HSID(1) = HRO(JJU,2)
C            HSID(2) = HRO(JJU,1)
C            VSID(1) = VRO(JJU,2)
C            VSID(2) = VRO(JJU,1)
C            CALL DQPOL(2,HSID,VSID)
C          ELSE
C            DH = HRO(JJU,2) - HRO(JJU,1)
C            DV = VRO(JJU,2) - VRO(JJU,1)
C            RN = SQRT( DH*DH + DV*DV )
C
C            HSID(1) = HRO(JJU,2) - RL / RN * DH
C            HSID(2) = HRO(JJU,2)
C            VSID(1) = VRO(JJU,2) - RL / RN * DV
C            VSID(2) = VRO(JJU,2)
C            CALL DQPOL(2,HSID,VSID)
C
C            HSID(1) = HRO(JJU,1) + RL / RN * DH
C            HSID(2) = HRO(JJU,1)
C            VSID(1) = VRO(JJU,1) + RL / RN * DV
C            VSID(2) = VRO(JJU,1)
C            CALL DQPOL(2,HSID,VSID)
C          END IF

C
C  draw markers for y=0 lines!
C

          IF (INORP.NE.0) THEN
            DH = HMAR(JMU1,2) - HMAR(JMU1,1)
            DV = VMAR(JMU1,2) - VMAR(JMU1,1)
            RN = SQRT( DH*DH + DV*DV )
            IF (JMU1.NE.1) THEN
              RL = RLEN
            ELSE
              RL = 2.5*RLEN
            END IF

            HSID(1) = HMAR(JMU1,2) - RLEN / RN * DH
            HSID(2) = HMAR(JMU1,2)
            VSID(1) = VMAR(JMU1,2) - RLEN / RN * DV
            VSID(2) = VMAR(JMU1,2)
            CALL DQPOL(2,HSID,VSID)
            HSID(1) = HMAR(JMU1,1) + RL / RN * DH
            HSID(2) = HMAR(JMU1,1)
            VSID(1) = VMAR(JMU1,1) + RL / RN * DV
            VSID(2) = VMAR(JMU1,1)
            CALL DQPOL(2,HSID,VSID)

            DH = HMAR(JMU2,2) - HMAR(JMU2,1)
            DV = VMAR(JMU2,2) - VMAR(JMU2,1)
            RN = SQRT( DH*DH + DV*DV )
            IF (JMU2.NE.1) THEN
              RL = RLEN
            ELSE
              RL = 2.5*RLEN
            END IF

            HSID(1) = HMAR(JMU2,2) - RLEN / RN * DH
            HSID(2) = HMAR(JMU2,2)
            VSID(1) = VMAR(JMU2,2) - RLEN / RN * DV
            VSID(2) = VMAR(JMU2,2)
            CALL DQPOL(2,HSID,VSID)
            HSID(1) = HMAR(JMU2,1) + RL / RN * DH
            HSID(2) = HMAR(JMU2,1)
            VSID(1) = VMAR(JMU2,1) + RL / RN * DV
            VSID(2) = VMAR(JMU2,1)
            CALL DQPOL(2,HSID,VSID)
          END IF


          IF ((JJS.GT.JJ2).OR.(JJS.LT.JJ1)) THEN
            L = 1
            DO K=JJ1,JJ2
              HSID(L) = HRO(K,JSU)
              VSID(L) = VRO(K,JSU)
              L = L + 1
            END DO
          ELSE
            L = 1
            DO K=JJ2,24
              HSID(L) = HRO(K,JSU)
              VSID(L) = VRO(K,JSU)
              L = L + 1
            END DO
            DO K=1,JJ1
              HSID(L) = HRO(K,JSU)
              VSID(L) = VRO(K,JSU)
              L = L + 1
            END DO
          END IF
          CALL DQPOL(13,HSID,VSID)

          DLINDD = PDCODD(2,LITRDD)

C
C  draw filled square to position phi=0, z=+z_max_TPC
C
          IF (INORP.NE.0) THEN
            CALL DGLEVL(LSYM)
            CALL DQPD0(NSYM,DSYM,0.)
            CALL DQPD (HMAR(1,1),VMAR(1,1))
          END IF

      END
C
C  end of DODT3D
C

*DK DODT3P
      SUBROUTINE DODT3P(TST,ROT,E,ASEY)
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- R.Vogl                26-AUG-1990
C!
C!   Inputs:
C!        -
C!
C!   Outputs:
C!        -
C!
C!   Libraries required:
C!
C!   Description
C!   ===========
C!
C?
C!======================================================================
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DD
      PARAMETER (MPNPDD=154)
      COMMON /DDECO1/
     &  MXSADD,MXLUDD,MXVDDD,MXITDD,MXTPDD,ICLVDD,ICBPDD,
     &  ICSADD,ICLUDD,ICVDDD,ICITDD,ICTPDD,ICECDD,ICMADD,
     &  ICHCDD,ICM1DD,ICM2DD,ICBWDD,ICBGDD,ICBADD,ICCNDD,
     &  MODEDD,NUPODD,ISTYDD,LIDTDD,LITRDD,LIGLDD,ISTHDD,
     &  ICFRDD,ICTXDD,ICALDD,ICHDDD,LCBWDD,ICRZDD,ICLFDD,
     &  ICVXDD,LCDUDD,LCVDDD,LCV1DD,LCITDD,LCIUDD,LCNBDD,
     &  LCNCDD,LCNIDD,LCSHDD,LCSLDD,LCSADD,LCSRDD,LCSSDD,
     &  LCSDDD,LCTPDD,LCTBDD,LCTCDD,LCTFDU,LCTUDD,LCTRDD,
     &  LCTWDD,LCLGDD,LCLCDD,LCLHDD,LCLKDD(1:3)         ,
     &  LCEODD,LCEGDD,LCECDD,LCELDD,LCEKDD(1:3)         ,
     &  LCESDD,LCWEDD(3)           ,LCEFDD,LCMUDD(2)    ,
     &  LCHGDD,LCHCDD,LCHLDD,LCHTDD(4)                  ,
     &  NCOLDD(8)                  ,LDUMDD(5)    ,MOLTDD,
     &  JSTRDD,JSSADD,JSVDDD,JSITDD,JSTPDD,JSLCDD,JSECDD,
     &  JSEODD,JSHCDD,JSD1DD(5)                         ,
     &  JSD2DD(4)                  ,KCBPDD,KCSADD,KCLUDD,
     &  KCVDDD,KCITDD,KCTPDD,KCECDD,KCMADD,KCHCDD,KCM1DD,
     &  KCM2DD,KCLGDD,K129DD,KSDBDD,KSAIDD,KSAODD,KSLBDD,
     &  K134DD(6),                                K140DD,
     &  NFVHDD,NFVCDD,NFVEDD,NFITDD,NFTPDD,NFECDD,NFHTDD,
     &  N148DD,NFTRDD,N150DD,N151DD,N152DD,N153DD,N154DD
      COMMON /DDECO2/ PDCODD(4,MPNPDD)
      COMMON /DDECO3/ DLINDD
      COMMON /DDECOT/ TDCODD(MPNPDD)
      CHARACTER *2 TDCODD
C------------------------------------------------------------------- DT
      COMMON /DTPGEO/ FISTDT(3),
     &       CORNDT (2,10,6,3), POLCDT ( 2,10,3 ),
     &       CRNGDT (2,10,6,3),
     &       NTYPDT,NCORDT(3),NSLTDT,
     &       RMAXDT , RMINDT , ZMAXDT
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION ROT(6)
      DIMENSION HRO(25,2),VRO(25,2),ZRO(25,2)
      DIMENSION HMAR(4,2),VMAR(4,2),ZMAR(4,2)
      DIMENSION HSID(25),VSID(25)
      CHARACTER *4 TST
C      DATA DLIN1 / 1. /
C      DATA DLIN3 / 3. /
C      DATA RLEN0 / 25. /
C      DATA IDEB  / 1 /
C      DATA NORP / 1 /
      DATA LSYM  / 12 /
      DATA NSYM  / 8 /
      DATA DSYM  / 6. /
      DATA INORP/0/
C
C  STATEMENT FUNCTIONS :
C
      HCOR(X,Y,Z) = (ROT(2)*X+ROT(1)*Y)*ROT(4) + ROT(3)*Z
      VCOR(X,Y,Z) = ROT(6)*( ROT(2)*Y - ROT(1)*X ) +
     &              ROT(5)*( ROT(4)*Z - ROT(3)*(ROT(2)*X + ROT(1)*Y) )
      ZCOR(X,Y,Z) = ROT(5)*( ROT(1)*X - ROT(2)*Y ) +
     &              ROT(6)*( ROT(4)*Z - ROT(3)*(ROT(2)*X + ROT(1)*Y) )

      NC1 = PDCODD(2,ICTPDD)
      NC2 = PDCODD(2,KCTPDD)

      A = -ASEY

      J1 = 1
      J2 = 1
      DO J = 1 , NSLTDT
C
C     right side ( z>0 ) outer corners

        HRO(J2  ,1)=HCOR(CRNGDT(1,6,J,2),CRNGDT(2,6,J,2), ZMAXDT)
        VRO(J2  ,1)=VCOR(CRNGDT(1,6,J,2),CRNGDT(2,6,J,2), ZMAXDT)
        ZRO(J2  ,1)=ZCOR(CRNGDT(1,6,J,2),CRNGDT(2,6,J,2), ZMAXDT)
        HRO(J2+1,1)=HCOR(CRNGDT(1,5,J,2),CRNGDT(2,5,J,2), ZMAXDT)
        VRO(J2+1,1)=VCOR(CRNGDT(1,5,J,2),CRNGDT(2,5,J,2), ZMAXDT)
        ZRO(J2+1,1)=ZCOR(CRNGDT(1,5,J,2),CRNGDT(2,5,J,2), ZMAXDT)

        HRO(J2+2,1)=HCOR(CRNGDT(1,6,J,3),CRNGDT(2,6,J,3), ZMAXDT)
        VRO(J2+2,1)=VCOR(CRNGDT(1,6,J,3),CRNGDT(2,6,J,3), ZMAXDT)
        ZRO(J2+2,1)=ZCOR(CRNGDT(1,6,J,3),CRNGDT(2,6,J,3), ZMAXDT)
        HRO(J2+3,1)=HCOR(CRNGDT(1,5,J,3),CRNGDT(2,5,J,3), ZMAXDT)
        VRO(J2+3,1)=VCOR(CRNGDT(1,5,J,3),CRNGDT(2,5,J,3), ZMAXDT)
        ZRO(J2+3,1)=ZCOR(CRNGDT(1,5,J,3),CRNGDT(2,5,J,3), ZMAXDT)

C   left side ( z<0 ) outer corners

        HRO(J2  ,2)=HCOR(CRNGDT(1,6,J,2),CRNGDT(2,6,J,2),-ZMAXDT)
        VRO(J2  ,2)=VCOR(CRNGDT(1,6,J,2),CRNGDT(2,6,J,2),-ZMAXDT)
        ZRO(J2  ,2)=ZCOR(CRNGDT(1,6,J,2),CRNGDT(2,6,J,2),-ZMAXDT)
        HRO(J2+1,2)=HCOR(CRNGDT(1,5,J,2),CRNGDT(2,5,J,2),-ZMAXDT)
        VRO(J2+1,2)=VCOR(CRNGDT(1,5,J,2),CRNGDT(2,5,J,2),-ZMAXDT)
        ZRO(J2+1,2)=ZCOR(CRNGDT(1,5,J,2),CRNGDT(2,5,J,2),-ZMAXDT)

        HRO(J2+2,2)=HCOR(CRNGDT(1,6,J,3),CRNGDT(2,6,J,3),-ZMAXDT)
        VRO(J2+2,2)=VCOR(CRNGDT(1,6,J,3),CRNGDT(2,6,J,3),-ZMAXDT)
        ZRO(J2+2,2)=ZCOR(CRNGDT(1,6,J,3),CRNGDT(2,6,J,3),-ZMAXDT)
        HRO(J2+3,2)=HCOR(CRNGDT(1,5,J,3),CRNGDT(2,5,J,3),-ZMAXDT)
        VRO(J2+3,2)=VCOR(CRNGDT(1,5,J,3),CRNGDT(2,5,J,3),-ZMAXDT)
        ZRO(J2+3,2)=ZCOR(CRNGDT(1,5,J,3),CRNGDT(2,5,J,3),-ZMAXDT)

        J2 = J2 + 4

      END DO

      HRO(25,1) = HRO(1,1)
      VRO(25,1) = VRO(1,1)
      ZRO(25,1) = ZRO(1,1)
      HRO(25,2) = HRO(1,2)
      VRO(25,2) = VRO(1,2)
      ZRO(25,2) = ZRO(1,2)

      DO II=1,2
        HMAR(1,II) = 0.5*(HRO(6 ,II)+HRO(5 ,II))
        VMAR(1,II) = 0.5*(VRO(6 ,II)+VRO(5 ,II))
        ZMAR(1,II) = 0.5*(ZRO(6 ,II)+ZRO(5 ,II))

        HMAR(2,II) = 0.5*(HRO(12,II)+HRO(11,II))
        VMAR(2,II) = 0.5*(VRO(12,II)+VRO(11,II))
        ZMAR(2,II) = 0.5*(ZRO(12,II)+ZRO(11,II))

        HMAR(3,II) = 0.5*(HRO(18,II)+HRO(17,II))
        VMAR(3,II) = 0.5*(VRO(18,II)+VRO(17,II))
        ZMAR(3,II) = 0.5*(ZRO(18,II)+ZRO(17,II))

        HMAR(4,II) = 0.5*(HRO(24,II)+HRO(23,II))
        VMAR(4,II) = 0.5*(VRO(24,II)+VRO(23,II))
        ZMAR(4,II) = 0.5*(ZRO(24,II)+ZRO(23,II))
      END DO

      ZMEL = 0.5 * (ZRO(1,2)+ZRO(13,2))
      ZMER = 0.5 * (ZRO(1,1)+ZRO(13,1))
      IF (ZMEL.GT.ZMER) THEN
        JSS = 2
        JSU = 1
      ELSE
        JSS = 1
        JSU = 2
      END IF

      DMAX=0.
      DO II = 1 , 24
        AA =  ( VRO(II,1) - VRO(II,2)) / ( HRO(II,1) - HRO(II,2) )
        B  =  VRO(II,2) - AA*HRO(II,2)
        D  =  ABS( B / SQRT(1. + AA*AA) )

        IF (D.GT.DMAX) THEN
          JMAX = II
          DMAX = D
        END IF
      END DO

      IF (JMAX.LE.12) THEN
        JJ1 = JMAX
        JJ2 = JMAX + 12
      ELSE
        JJ1 = JMAX - 12
        JJ2 = JMAX
      END IF

      CALL DQPO0('AREA',NC1,NC2,'NSKP')

      IF ((TST.EQ.'AREA').OR.(TST.EQ.'AR+L')) THEN
        DO I = 1 , 25
          HSID(I) = HRO(I,1)+ZRO(I,1)*(E-HRO(I,1))/(A+ZRO(I,1))
          VSID(I) = VRO(I,1)*A/(A+ZRO(I,1))
        END DO
        CALL DQPOL(25,HSID,VSID)

        DO I = 1 , 25
          HSID(I) = HRO(I,2)+ZRO(I,2)*(E-HRO(I,2))/(A+ZRO(I,2))
          VSID(I) = VRO(I,2)*A/(A+ZRO(I,2))
        END DO
        CALL DQPOL(25,HSID,VSID)

        HSID(1) = HRO(JJ1,2)+ZRO(JJ1,2)*(E-HRO(JJ1,2))/(A+ZRO(JJ1,2))
        HSID(2) = HRO(JJ2,2)+ZRO(JJ2,2)*(E-HRO(JJ2,2))/(A+ZRO(JJ2,2))
        HSID(3) = HRO(JJ2,1)+ZRO(JJ2,1)*(E-HRO(JJ2,1))/(A+ZRO(JJ2,1))
        HSID(4) = HRO(JJ1,1)+ZRO(JJ1,1)*(E-HRO(JJ1,1))/(A+ZRO(JJ1,1))
        HSID(5) = HSID(1)
        VSID(1) = VRO(JJ1,2)*A/(A+ZRO(JJ1,2))
        VSID(2) = VRO(JJ2,2)*A/(A+ZRO(JJ2,2))
        VSID(3) = VRO(JJ2,1)*A/(A+ZRO(JJ2,1))
        VSID(4) = VRO(JJ1,1)*A/(A+ZRO(JJ1,1))
        VSID(5) = VSID(1)
        CALL DQPOL(5,HSID,VSID)
      END IF
C
C
      CALL DQPO0('LINB',0,NC2,'NSKP')
C
C  VISIBLE SIDES OF DETECTOR ARE DRAWN WITH THICK LINES
C

      DLINDD = PDCODD(2,LIDTDD)

      DO I = 1 , 25
        HSID(I) = HRO(I,JSS)+ZRO(I,JSS)*(E-HRO(I,JSS))/(A+ZRO(I,JSS))
        VSID(I) = VRO(I,JSS)*A/(A+ZRO(I,JSS))
      END DO
      CALL DQPOL(25,HSID,VSID)

      DO I = 1 , 25
        HSID(I) = HRO(I,JSU)+ZRO(I,JSU)*(E-HRO(I,JSU))/(A+ZRO(I,JSU))
        VSID(I) = VRO(I,JSU)*A/(A+ZRO(I,JSU))
      END DO
      CALL DQPOL(25,HSID,VSID)

      HSID(1) = HRO(JJ1,2)+ZRO(JJ1,2)*(E-HRO(JJ1,2))/(A+ZRO(JJ1,2))
      HSID(2) = HRO(JJ1,1)+ZRO(JJ1,1)*(E-HRO(JJ1,1))/(A+ZRO(JJ1,1))
      VSID(1) = VRO(JJ1,2)*A/(A+ZRO(JJ1,2))
      VSID(2) = VRO(JJ1,1)*A/(A+ZRO(JJ1,1))
      CALL DQPOL(2,HSID,VSID)

      HSID(1) = HRO(JJ2,2)+ZRO(JJ2,2)*(E-HRO(JJ2,2))/(A+ZRO(JJ2,2))
      HSID(2) = HRO(JJ2,1)+ZRO(JJ2,1)*(E-HRO(JJ2,1))/(A+ZRO(JJ2,1))
      VSID(1) = VRO(JJ2,2)*A/(A+ZRO(JJ2,2))
      VSID(2) = VRO(JJ2,1)*A/(A+ZRO(JJ2,1))
      CALL DQPOL(2,HSID,VSID)

C      ZMP6 = .5 * ( ZRO(JJ1+6,2) + ZRO(JJ1+6,1))
C      JMP6 = JJ1 + 6
C      IF (JJ1.GT.6) THEN
C        ZMM6 = .5 * ( ZRO(JJ1-6,2) + ZRO(JJ1-6,1))
C        JMM6 = JJ1 - 6
C      ELSE
C        ZMM6 = .5 * ( ZRO(JJ2+6,2) + ZRO(JJ2+6,1))
C        JMM6 = JJ2 + 6
C      END IF
C
C      IF (ZMP6.GT.ZMM6) THEN
C        JJS = JMP6
C        JJU = JMM6
C      ELSE
C        JJS = JMM6
C        JJU = JMP6
C      END IF
C
C      IF (JJS.LE.5) THEN
C        JMS1 = 1
C        JMS2 = 4
C        JMU1 = 2
C        JMU2 = 3
C      ELSE IF (JJS.LE.11) THEN
C        JMS1 = 2
C        JMS2 = 1
C        JMU1 = 3
C        JMU2 = 4
C      ELSE IF (JJS.LE.17) THEN
C        JMS1 = 3
C        JMS2 = 2
C        JMU1 = 4
C        JMU2 = 1
C      ELSE IF (JJS.LE.23) THEN
C        JMS1 = 4
C        JMS2 = 3
C        JMU1 = 1
C        JMU2 = 2
C      ELSE
C        JMS1 = 1
C        JMS2 = 4
C        JMU1 = 2
C        JMU2 = 3
C      END IF
C
CC
CC  draw markers for y=0 and x=0 lines (edges)!
CC
C
CC
CC  to indicate orientation of TPC in projection, the markers for edge
CC  with phi=0, z>0 is drawn 2.5-times as long as the others
CC
C      DH = HMAR(JMS1,2) - HMAR(JMS1,1)
C      DV = VMAR(JMS1,2) - VMAR(JMS1,1)
C      RN = SQRT( DH*DH + DV*DV )
C      RLEN = RLEN0 * ( RN / (2*ZMAXDT))
C      IF (JMS1.NE.1) THEN
C        RL = RLEN
C      ELSE
C        RL = 2.5*RLEN
C      END IF
C
C      HSID(1) = HMAR(JMS1,2) - RLEN / RN * DH
C      HSID(2) = HMAR(JMS1,2)
C      VSID(1) = VMAR(JMS1,2) - RLEN / RN * DV
C      VSID(2) = VMAR(JMS1,2)
CC      CALL DQPOL(2,HSID,VSID)
C      HSID(1) = HMAR(JMS1,1) + RL / RN * DH
C      HSID(2) = HMAR(JMS1,1)
C      VSID(1) = VMAR(JMS1,1) + RL / RN * DV
C      VSID(2) = VMAR(JMS1,1)
CC      CALL DQPOL(2,HSID,VSID)
C
C      DH = HMAR(JMS2,2) - HMAR(JMS2,1)
C      DV = VMAR(JMS2,2) - VMAR(JMS2,1)
C      RN = SQRT( DH*DH + DV*DV )
C      IF (JMS2.NE.1) THEN
C        RL = RLEN
C      ELSE
C        RL = 2.5*RLEN
C      END IF
C
C      HSID(1) = HMAR(JMS2,2) - RLEN / RN * DH
C      HSID(2) = HMAR(JMS2,2)
C      VSID(1) = VMAR(JMS2,2) - RLEN / RN * DV
C      VSID(2) = VMAR(JMS2,2)
CC      CALL DQPOL(2,HSID,VSID)
C      HSID(1) = HMAR(JMS2,1) + RL / RN * DH
C      HSID(2) = HMAR(JMS2,1)
C      VSID(1) = VMAR(JMS2,1) + RL / RN * DV
C      VSID(2) = VMAR(JMS2,1)
CC      CALL DQPOL(2,HSID,VSID)
C
C
C      IF ((JJS.GT.JJ2).OR.(JJS.LT.JJ1)) THEN
C        L = 1
C        DO K=JJ2,24
C          HSID(L) = HRO(K,JSU)+ZRO(K,JSU)*(E-HRO(K,JSU))/(A+ZRO(K,JSU))
C          VSID(L) = VRO(K,JSU)*A/(A+ZRO(K,JSU))
C          L = L + 1
C        END DO
C        DO K=1,JJ1
C          HSID(L) = HRO(K,JSU)+ZRO(K,JSU)*(E-HRO(K,JSU))/(A+ZRO(K,JSU))
C          VSID(L) = VRO(K,JSU)*A/(A+ZRO(K,JSU))
C          L = L + 1
C        END DO
C      ELSE
C        L = 1
C        DO K=JJ1,JJ2
C          HSID(L) = HRO(K,JSU)+ZRO(K,JSU)*(E-HRO(K,JSU))/(A+ZRO(K,JSU))
C          VSID(L) = VRO(K,JSU)*A/(A+ZRO(K,JSU))
C          L = L + 1
C        END DO
C      END IF
C      CALL DQPOL(13,HSID,VSID)
C
CC
CC   CHANGE HERE TO THIN LINES !!!!!!!!!!!!!!
CC
C      DLINDD = DLIN1
C
CC
CC  draw markers for y=0 lines!
CC
C
C      DH = HMAR(JMU1,2) - HMAR(JMU1,1)
C      DV = VMAR(JMU1,2) - VMAR(JMU1,1)
C      RN = SQRT( DH*DH + DV*DV )
C      IF (JMU1.NE.1) THEN
C        RL = RLEN
C      ELSE
C        RL = 2.5*RLEN
C      END IF
C
C      HSID(1) = HMAR(JMU1,2) - RLEN / RN * DH
C      HSID(2) = HMAR(JMU1,2)
C      VSID(1) = VMAR(JMU1,2) - RLEN / RN * DV
C      VSID(2) = VMAR(JMU1,2)
CC      CALL DQPOL(2,HSID,VSID)
C      HSID(1) = HMAR(JMU1,1) + RL / RN * DH
C      HSID(2) = HMAR(JMU1,1)
C      VSID(1) = VMAR(JMU1,1) + RL / RN * DV
C      VSID(2) = VMAR(JMU1,1)
CC      CALL DQPOL(2,HSID,VSID)
C
C      DH = HMAR(JMU2,2) - HMAR(JMU2,1)
C      DV = VMAR(JMU2,2) - VMAR(JMU2,1)
C      RN = SQRT( DH*DH + DV*DV )
C      IF (JMU2.NE.1) THEN
C        RL = RLEN
C      ELSE
C        RL = 2.5*RLEN
C      END IF
C
C      HSID(1) = HMAR(JMU2,2) - RLEN / RN * DH
C      HSID(2) = HMAR(JMU2,2)
C      VSID(1) = VMAR(JMU2,2) - RLEN / RN * DV
C      VSID(2) = VMAR(JMU2,2)
CC      CALL DQPOL(2,HSID,VSID)
C      HSID(1) = HMAR(JMU2,1) + RL / RN * DH
C      HSID(2) = HMAR(JMU2,1)
C      VSID(1) = VMAR(JMU2,1) + RL / RN * DV
C      VSID(2) = VMAR(JMU2,1)
CC      CALL DQPOL(2,HSID,VSID)
C
C
C      IF ((JJS.GT.JJ2).OR.(JJS.LT.JJ1)) THEN
C        L = 1
C        DO K=JJ1,JJ2
C          HSID(L) = HRO(K,JSU)+ZRO(K,JSU)*(E-HRO(K,JSU))/(A+ZRO(K,JSU))
C          VSID(L) = VRO(K,JSU)*A/(A+ZRO(K,JSU))
C          L = L + 1
C        END DO
C      ELSE
C        L = 1
C        DO K=JJ2,24
C          HSID(L) = HRO(K,JSU)+ZRO(K,JSU)*(E-HRO(K,JSU))/(A+ZRO(K,JSU))
C          VSID(L) = VRO(K,JSU)*A/(A+ZRO(K,JSU))
C          L = L + 1
C        END DO
C        DO K=1,JJ1
C          HSID(L) = HRO(K,JSU)+ZRO(K,JSU)*(E-HRO(K,JSU))/(A+ZRO(K,JSU))
C          VSID(L) = VRO(K,JSU)*A/(A+ZRO(K,JSU))
C          L = L + 1
C        END DO
C      END IF
C      CALL DQPOL(13,HSID,VSID)

      DLINDD = PDCODD(2,LITRDD)

C
C  draw filled square to position phi=0, z=+z_max_TPC
C
      IF (INORP.NE.0) THEN
        HH = HMAR(1,1)+ZMAR(1,1)*(E-HMAR(1,1))/(A+ZMAR(1,1))
        VV = VMAR(1,1)*A/(A+ZMAR(1,1))
        CALL DGLEVL(LSYM)
        CALL DQPD0(NSYM,DSYM,0.)
        CALL DQPD (HH,VV)
      END IF

      END
C
C  end of DODT3P
C
*DK DOFISH
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DOFISH
CH
      SUBROUTINE DOFISH(TU,TP)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:  ORDER
C    Inputs    : TU
C    Outputs   : NORD() ORDERED NUMBERS
C
C ---------------------------------------------------------------------
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DT
      COMMON /DTVCCC/ DUMYDT(4),
     1         HUMIDT,HUMADT,VUMIDT,VUMADT,
     1         NSQUDT,NOBJDT,NOCLDT,HPOSDT,VPOSDT,
     1         FPOSDT,TPOSDT,AROTDT,RPOSDT,NTVIDT,
     1         FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,
     1         FX11DT
      LOGICAL  FPRIDT,FRETDT,FBLWDT,FMONDT,F256DT,FX11DT
C------------------------------------------------------------------- DP
      COMMON /DPERSC/ FRPRDP,FBUGDP,FFDRDP
      LOGICAL FRPRDP,FBUGDP,FFDRDP
C------------------------------------------------------------------- DA
C     PARAMETER (MPARDA=499)
      PARAMETER (MPARDA=800)
      COMMON /DAPAR1/PARADA(4,MPARDA)
      COMMON /DAPAR2/PLSTDA(2,MPARDA)
      CHARACTER  *2  TPOPDA
      COMMON /DAPAR3/TPOPDA(MPARDA)
      COMMON /DAPAR4/N_1_DA,N_2_DA
      CHARACTER *59  TPARDA
      COMMON /DAPAR5/TPARDA
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      PARAMETER (MPR=9,MUN=10)
      CHARACTER *2 TPR(MPR),TUN(MUN),TP,TW,TU
      DATA TPR/'YX','XF','YZ','RZ','FT','FR','FZ','-D','HZ'/
      DATA TUN/'VD','IT','TP','EC','HC','MD','SI','LC','MA','BP'/
      DIMENSION NF(MPR,MUN)
C             1 2 3 4 5 6 7 8 9
C             Y X Y R F F F R ?
C             X F Z Z T R Z O ?
C             | | | | | | | | |
      DATA NF/2,2,2,2,2,2,2,2,2,  ! VD 1
     &        2,2,2,2,2,2,2,2,2,  ! IT 2
     &        0,0,2,2,2,0,2,2,2,  ! TP 3
     &        2,0,2,2,2,0,2,2,0,  ! EC 4
     &        2,0,2,0,2,0,2,2,0,  ! HC 5
     &        2,0,2,2,2,2,2,2,2,  ! MD 6
     &        2,2,2,2,2,2,2,2,2,  ! SI 7
     &        2,2,2,2,2,2,2,2,2,  ! LC 8
     &        2,2,2,2,2,2,2,2,2,  ! MA 9
     &        2,2,2,2,2,2,2,2,2/  ! BP 10
      DATA ND/2/
      DATA IDEB/0/
      DATA LDEB/0/
      DATA NFSH/0/
C
      IF(.NOT.FX11DT) RETURN
      IF(FFDRDP) THEN
        CALL DGFISH(NFSH)
        RETURN
      END IF
      TW=TP
C     :::::::::::::::::::::::::::::::::::::::::::::::::::::::
      TPARDA=
     &  'J_PDS'
      CALL DPARAM(11
     &  ,J_PDS)
C     ::.::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF(TP.EQ.TPR(1).AND.PARADA(4,J_PDS).EQ.1.) TW='XF'
      DO K=1,MUN
        IF(TU.EQ.TUN(K)) THEN
          DO N=1,MPR
            IF(TW.EQ.TPR(N)) THEN
              ND=NF(N,K)
              IF(ND.EQ.0) THEN
                CALL DGFISH(ND)
                IF(IDEB.EQ.1) CALL DWRT(' DGFISH -> 0'//TW//' '//TU)
              END IF
              RETURN
            END IF
          END DO
        END IF
      END DO
      CALL DWRT(' Report to H.Drevermann: FISH='//TW//'  '//TU)
      RETURN
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DOFIS0
CH
      ENTRY DOFIS0
CH
CH --------------------------------------------------------------------
CH
      IF(FX11DT.AND.ND.NE.0) THEN
        ND=0
        CALL DGFISH(ND)
        IF(LDEB.EQ.1) CALL DWRT(' DGFISH -> 0')
      END IF
CH..............---
CH
CH
CH
CH
CH
CH
CH --------------------------------------------------------------------  DOFIS2
CH
      ENTRY DOFIS2
CH
CH --------------------------------------------------------------------
CH
      IF(FX11DT.AND.ND.NE.2) THEN
        ND=2
        CALL DGFISH(ND)
        IF(LDEB.EQ.1) CALL DWRT(' DGFISH -> 2')
      END IF
      END
*DK DORDTR
CH..............+++
CH
CH
CH
CH
CH
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  DORDTR
CH
      SUBROUTINE DORDTR
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:
C    Inputs    :
C    Outputs   :
C
C    Called by :
C ---------------------------------------------------------------------
*CA DALLCO
C     INCLUDE 'DALI_CF.INC'
C------------------------------------------------------------------- DT
      COMMON /DTRAKC/ FORDDT,NORDDT(999),NCOLDT(999)
      COMMON /DTRAKT/ ULABDT(999),COLRDT(999)
      COMMON /DTRAKF/ FHTRDT,FCOLDT
      LOGICAL FORDDT,FHTRDT,FCOLDT
C------------------------------------------------------------------- DB
      PARAMETER (MBNCDB=27)
      COMMON /DBOS1C/ BNUMDB(4,MBNCDB)
      COMMON /DBOS2C/ NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      INTEGER         NUMBDB,FVERDB,FKINDB,ITCODB,TPADDB,TBCODB,
     &                       TPCODB,ESDADB,HSDADB,HTUBDB,MHITDB,
     &                       FRFTDB,FRTLDB,FTCLDB,FICLDB,PYERDB,
     &                       FRIDDB,PLSDDB,PECODB,PEWDDB,RECODB,
     &                       VDZTDB,VDXYDB,CRFTDB,NRFTDB,VDCODB,
     &                       VCPLDB,YKNVDB
      COMMON /DBOS3C/ IFULDB
C------------------------------------------------------------------- DW
      COMMON /DWORKC/ WORKDW(3000)
      DIMENSION NWRKDW(3000)
      EQUIVALENCE (WORKDW,NWRKDW)
C
C-------------------End of DALI_CF commons----------------
      EXTERNAL SIND,COSD,ACOSD,TAND,ATAND,ATAN2D! Not implemented in g77
      DIMENSION FI(999)
      EQUIVALENCE (FI,NCOLDT)
      LOGICAL FTPC
      DIMENSION ICOL(0:5)
      DATA ICOL/13,10,12,15,11,14/
C      IF(FORDDT) RETURN
C      FORDDT=.TRUE.
      N=BNUMDB(2,FRFTDB)
      IF(N.LE.0.OR.BNUMDB(4,FRFTDB).LE.0.) RETURN
      CALL=DVCHT0(NUM)
      CALL=DVCHI0(NUM)
      IF(N.EQ.1) THEN
         NORDDT(1)=1
         CALL DVTRLP(1,NLAST,FTPC)
         IF(FTPC) THEN
           NCOLDT(1)=12
         ELSE
           NCOLDT(1)=9
         END IF
         RETURN
      END IF
      N=MIN(N,999)
      DO   700  K=1,N
         CALL DVTRFI(K,FI(K))
  700 CONTINUE
      CALL VZERO(NWRKDW,N)
      DO   710  K1=1,N-1
         DO   720  K2=K1+1,N
            IF(FI(K1).GT.FI(K2)) THEN
               NWRKDW(K1)=NWRKDW(K1)+1
            ELSE
               NWRKDW(K2)=NWRKDW(K2)+1
            END IF
  720    CONTINUE
  710 CONTINUE
      DO 730 I=1,N
        NORDDT(NWRKDW(I)+1)=I
  730 CONTINUE
      DO 740 I=1,N
        IF(COLRDT(I).GT.0.) THEN
          NCOLDT(I)=COLRDT(I)
        ELSE
          CALL DVTRLP(I,NLAST,FTPC)
          IF(FTPC) THEN
            NCOLDT(I)=ICOL(MOD(NWRKDW(I),6))
          ELSE
            NCOLDT(I)=9
          END IF
        END IF
  740 CONTINUE
      END
*DK D_ORDER
CH..............+++
CH
CH
CH
CH
CH
CH
CH +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  D_ORDER
CH
      SUBROUTINE D_ORDER(IDIR,IABS,IFILL,NUM,E,NPOS,NORD)
CH
CH ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CH
C ---------------------------------------------------------------------
C
C    Created by H.Drevermann                   28-JUL-1988
C
C!:  ORDER
C              : IDIR: 1=LOW TO HIGH  or 2=HIGH TO LOW
C              : TABS: 0 or 1 if ABS(E) is used
C              : IFILL: 0 or 1 to fill NORD
C    Inputs    : NUM=LENGTH OF ARRAY, E()=ARRAY FOR ORDERING
C    Outputs   : NORD() ORDERED NUMBERS
C              : NPOS =  position #, parrallel to E
C    Example   : CALL D_ORDER(2,1,1,NUM,E,NPOS,NORD)
C    Input  #       :   1     2     3     4     5     6     7     8     9
C           E       :   4     1     9     3     3     7     8   -11    99
C        NPOS       :   6     9     3     7     8     5     4     2     1
C        NORD       :   9     8     3     7     6     1     4     5     2
C ordering sequence :  99   -11     9     8     7     4     3     3     1
C ------------------------------------------------------------------------
      DIMENSION E(*),NORD(*),NPOS(*)
      DO K=1,NUM
        NPOS(K)=1
      END DO
      IF(IDIR.EQ.2) THEN
        IF(IABS.EQ.1) THEN
          DO K1=1,NUM-1
            E1=ABS(E(K1))
            DO K2=K1+1,NUM
              E2=ABS(E(K2))
              IF(E1.LT.E2) THEN
C                  ****
                NPOS(K1)=NPOS(K1)+1
              ELSE
                NPOS(K2)=NPOS(K2)+1
              END IF
            END DO
          END DO
        ELSE
          DO K1=1,NUM-1
            E1=E(K1)
            DO K2=K1+1,NUM
              E2=E(K2)
              IF(E1.LT.E2) THEN
C                  ****
                NPOS(K1)=NPOS(K1)+1
              ELSE
                NPOS(K2)=NPOS(K2)+1
              END IF
            END DO
          END DO
        END IF
      ELSE
        IF(IABS.EQ.1) THEN
          DO K1=1,NUM-1
            E1=ABS(E(K1))
            DO K2=K1+1,NUM
              E2=ABS(E(K2))
              IF(E1.GE.E2) THEN
C                  ****
                NPOS(K1)=NPOS(K1)+1
              ELSE
                NPOS(K2)=NPOS(K2)+1
              END IF
            END DO
          END DO
        ELSE
          DO K1=1,NUM-1
            E1=E(K1)
            DO K2=K1+1,NUM
              E2=E(K2)
              IF(E1.GE.E2) THEN
C                  ****
                NPOS(K1)=NPOS(K1)+1
              ELSE
                NPOS(K2)=NPOS(K2)+1
              END IF
            END DO
          END DO
        END IF
      END IF
      IF(IFILL.EQ.1) THEN
        DO K=1,NUM
          NORD(NPOS(K))=K
        END DO
      END IF
      END
