        SUBROUTINE CATINO
C.----------------------------------------------------------------
C J.Boucrot   March 87    F.Ranjard - 870815
C                                     880316
C! Create Geantino Part type LTYPEL or next (48-52)
C  Called by ECHIT, HCHIT, CALHIT                from this .HLB
C.----------------------------------------------------------------
      SAVE
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      PARAMETER(LTYPEL=48, LTYPME=49, LTYPBA=50, LTYPAN=51, LTYPGA=52)
      COMMON / CAPANO / NATGER,METHCO,TOTNRJ,TINOX0,TINOL0,EMGNRJ,
     + EMALFA,EMBETA,EMALM1,EMAEXP,EMFACT,EMUDP0,EMUDP2, HADNRJ,HALPHA,
     + HABETA,HADRAY,HAPUIS,HGAMMA,SMAXLR,SMAXLA
       COMMON / CANAME / EMNAME
       CHARACTER*4 EMNAME
C
      PARAMETER (LETRK=27, LITRK=11, LNTRK=10)
      COMMON /TRKCOM/   ITRKEL(LITRK), TRKELE(LETRK), TRKNXT(LNTRK)
     &                , FTRHAD
      LOGICAL FTRHAD
      COMMON /TRKKAR/   TRKVOL
      CHARACTER*4 TRKVOL
C
     INTEGER HPANTP
      PARAMETER (LGKINE=100)
      COMMON/GCKING/KGCASE,NGKINE,GKIN(5,LGKINE),GTOFD(LGKINE)
     &             ,IGFLGK(LGKINE)
C
      COMMON/GCKIN3/GPOS(3,LGKINE)
      REAL GPOS
C
C ---------------------------------------------------------
C Build the "shower particle" which will have :
C        - GEANT particle code = LTYPEL/LTYPME/LTYPBA/LTYPAN/LTYPGA
C        - GEANT tracking type =  6 ( no interactions )
C
C     select geantino type  electron / photon / hadron
      IF (ITRKEL(11).EQ.2) THEN
         JTYP = LTYPEL
      ELSE IF (ITRKEL(11).EQ.1) THEN
         JTYP = LTYPGA
      ELSE IF (ITRKEL(11).EQ.4) THEN
         IBAR = HPANTP (ITRKEL(4))
         IF (IBAR .EQ. -1) THEN
            JTYP = LTYPBA
         ELSE IF (IBAR .EQ. 0) THEN
            JTYP = LTYPME
         ELSE IF (IBAR .EQ. 1) THEN
            JTYP = LTYPAN
         ENDIF
      ELSE
         WRITE (LOUTIO,'(/1X,''+++CAPANO+++ this tracking type is '',
     &    ''not considered - RETURN'',12I5)') ITRKEL
         RETURN
      ENDIF
C
C set shower interaction mechanism
      KGCASE = JHOCHA ('SHOW')
      NGKINE = 1
C
      DO I=1,3
         GKIN(I,1) = TRKELE(7)*TRKNXT(I+3)
         GPOS(I,1) = TRKNXT(I)
      ENDDO
      GKIN(4,1) = TRKELE(8)
      GKIN(5,1) = REAL(JTYP)
      GTOFD(1) = 0.
C
   10 RETURN
      END
