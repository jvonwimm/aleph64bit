      SUBROUTINE ASWRTP (LIST)
C ----------------------------------------------------------------------
C - F.Ranjard - 850903
C! Write output file
C - Input   : LIST   / A  = 'C' or 'E'  list
C - Called by    ASCEVE                                  from this .HLB
C
C ----------------------------------------------------------
      SAVE
      CHARACTER*1 LIST
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON /NAMCOM/   NARUNH, NAPART, NAEVEH, NAVERT, NAKINE, NAKRUN
     &                 ,NAKEVH, NAIMPA, NAASEV, NARUNE, NAKLIN, NARUNR
     &                 ,NAKVOL, NAVOLU
      EQUIVALENCE (NAPAR,NAPART)
C
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      PARAMETER (LOFFMC = 1000)
      PARAMETER (LHIS=20, LPRI=20, LTIM=6, LPRO=6, LRND=3)
      PARAMETER (LBIN=20, LST1=LBIN+3, LST2=3)
      PARAMETER (LSET=15, LTCUT=5, LKINP=20)
      PARAMETER (LDET=9,  LGEO=LDET+4, LBGE=LGEO+5)
      PARAMETER (LCVD=10, LCIT=10, LCTP=10, LCEC=15, LCHC=10, LCMU=10)
      PARAMETER (LCLC=10, LCSA=10, LCSI=10)
      COMMON /JOBCOM/   JDATJO,JTIMJO,VERSJO
     &                 ,NEVTJO,NRNDJO(LRND),FDEBJO,FDISJO
     &                 ,FBEGJO(LDET),TIMEJO(LTIM),NSTAJO(LST1,LST2)
     &                 ,IDB1JO,IDB2JO,IDB3JO,IDS1JO,IDS2JO
     &                 ,MBINJO(LST2),MHISJO,FHISJO(LHIS)
     &                 ,IRNDJO(LRND,LPRO)
     &                 ,IPRIJO(LPRI),MSETJO,IRUNJO,IEXPJO,AVERJO
     3                 ,MPROJO,IPROJO(LPRO),MGETJO,MSAVJO,TIMLJO,IDATJO
     5                 ,TCUTJO(LTCUT),IBREJO,NKINJO,BKINJO(LKINP),IPACJO
     6                 ,IDETJO(LDET),IGEOJO(LGEO),LVELJO(LGEO)
     7                 ,ICVDJO(LCVD),ICITJO(LCIT),ICTPJO(LCTP)
     8                 ,ICECJO(LCEC),ICHCJO(LCHC),ICLCJO(LCLC)
     9                 ,ICSAJO(LCSA),ICMUJO(LCMU),ICSIJO(LCSI)
     &                 ,FGALJO,FPARJO,FXXXJO,FWRDJO,FXTKJO,FXSHJO,CUTFJO
     &                 ,IDAFJO,IDCHJO,TVERJO
      LOGICAL FDEBJO,FDISJO,FHISJO,FBEGJO,FGALJO,FPARJO,FXXXJO,FWRDJO
     &       ,FXTKJO,FXSHJO
      COMMON /JOBKAR/   TITLJO,TSETJO(LSET),TPROJO(LPRO)
     1                 ,TKINJO,TGEOJO(LBGE),TRUNJO
      CHARACTER TRUNJO*60
      CHARACTER*4 TKINJO,TPROJO,TSETJO,TITLJO*40
      CHARACTER*2 TGEOJO
C
      PARAMETER (LERR=20)
      COMMON /JOBERR/   ITELJO,KERRJO,NERRJO(LERR)
      COMMON /JOBCAR/   TACTJO
      CHARACTER*6 TACTJO
C
      INTEGER ALRUNR, ALFIND
      CHARACTER*4 NLISTF, NAME
C ----------------------------------------------------------------------
C
      IF (LIST.EQ.'E') THEN
C - Event record
         NAME = 'EVEH'
C
      ELSE
C
C - parameters/ calibration/ Geant3 geometry ....
         NAME = 'RUNR'
C
C - build the RUNR bank if it does not exist
         IF (IW(NARUNR) .EQ. 0) THEN
            JRUNR = ALRUNR (IEXPJO,IRUNJO)
            IF (JRUNR .EQ. 0)
     &      CALL ALTELL ('ASWRTP: not enough space for RUNR ',1,'STOP')
         ENDIF
      ENDIF
C
C   check that the bank RUNR is 1st in the C-list , or
C   EVEH on E-list. If not put it first.
C
      IF (NLISTF(IW,1,LIST) .NE. NAME) THEN
         CALL BDROP (IW,'T')
         CALL BLIST (IW,'T=',LIST)
         CALL BLIST (IW,LIST//'=',NAME)
         CALL BLIST (IW,LIST//'+','T')
         CALL BLIST (IW,'T=','0')
      ENDIF
C
C - suppress empty banks from the list
C
      I=0
 10   I=I+1
      NAME = NLISTF (IW,I,LIST)
      IF (NAME .NE. '    ') THEN
         JNAME = IW(NAMIND(NAME))
         IF (JNAME.EQ.0) CALL BLIST (IW,LIST//'-',NAME)
         GOTO 10
      ENDIF
C
C - write the record
C
      IF (MGETJO.EQ.0) THEN
C     no input tape so cannot use ABRSEL package
         CALL BWRITE (IW,LSAVIO,LIST)
      ELSE
         CALL ABWSEL (LIST)
      ENDIF
C
      RETURN
      END
