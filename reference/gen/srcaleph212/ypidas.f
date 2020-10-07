      SUBROUTINE YPIDAS(IER)
C
C----------------------------------------------------------*
C!    Assign particle identity from data or Monte Carlo generation
CKEY YTOP PARTICLE
C!    Author :     G. Lutz   jan 5 1989
C!    modified:    J.Lauber  May 2 1991
C!    modified:    M.Bosman  jul 15 1991
C!
C!
C!    Description
C!    ===========
C!    This routine assigns particle identification possibilities of
C!    charged tracks either from data or from Monte Carlo generation.
C!    Results are stored in markers KPIDF0
C!    Assignment for real data introduced by J.Lauber
C!
C!
C!---------------------------------------------------------*
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JFRFIR=1,JFRFTL=2,JFRFP0=3,JFRFD0=4,JFRFZ0=5,JFRFAL=6,
     +          JFRFEM=7,JFRFC2=28,JFRFDF=29,JFRFNO=30,LFRFTA=30)
      PARAMETER(JFRIBP=1,JFRIDZ=2,JFRIBC=3,JFRIDC=4,JFRIPE=5,JFRIPM=6,
     +          JFRIPI=7,JFRIPK=8,JFRIPP=9,JFRINK=10,JFRIQF=11,
     +          LFRIDA=11)
      PARAMETER(JFRTIV=1,JFRTNV=2,JFRTII=3,JFRTNI=4,JFRTNE=5,JFRTIT=6,
     +          JFRTNT=7,JFRTNR=8,LFRTLA=8)
      PARAMETER(JFKIPX=1,JFKIPY=2,JFKIPZ=3,JFKIMA=4,JFKIPA=5,JFKIOV=6,
     +          JFKIEV=7,JFKIHC=8,LFKINA=8)
C! YTOP array dimensions
      PARAMETER(NMSIZZ=31,MKDIMM=6,MAXTRK=186,MAXHLX=124,MAXNTR=62,
     &  MXVTOP=10,MXMULS=10,MAXVXP=50,MAXEXC=60,MXVTYP=2)
C! YTOP particle masses
      PARAMETER(    JPAFEP=1,JPAFEM=2,JPAFMP=3,JPAFMM=4,
     &              JPAFPP=5,JPAFPM=6,JPAFKP=7,JPAFKM=8,
     &              JPAFPR=9,JPAFPB=10,JPAFPH=11,JPAFPZ=12,
     &              JPAFKZ=13,JPAFLA=14,JPAFLB=15   )
      COMMON/YPMASS/ YPMASS(20)
C! YTOP parameters
      COMMON/YPARTO/DHXLIM,CHISEL,PMINSE,PIDACP,BFIELD,
     &       MNTHPV,MXTSPV, PMINRQ,PMINRA,
     &       CHVXCO,CHPTCO,RVACCO,AMCTCO,DZMXCO,NAMXCO,EPLOCO,EPHICO,
     &       CHVXV0,CHPTV0,CHVSV0,CHMLV0,DZMXV0,NAMXV0,
     &       PIPKV0,PRPLV0,PIPLV0,
     &       LBCRFD,LRYOLD,LRFRF2,
     &       LRPVTX,LRSVTX,LVBCR0,LRMVPV,LRLPVX,LCONVS,LVZERS,LRUSER
      LOGICAL LBCRFD,LRYOLD,LRFRF2
      LOGICAL LRPVTX,LRSVTX,LVBCR0,LRMVPV,LRLPVX,LCONVS,LVZERS,LRUSER
C! YTOP track parameters
      COMMON/YTRKTO/NGTRTO,IPTRTO(MAXTRK),
     &              PTRECT(MAXTRK),PRECTO(MAXTRK),
     &              KPIDF0(MAXTRK),KPORF0(MAXTRK)
C
C
C!---------------------------------------------------------*
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
C!---------------------------------------------------------*
C
C-- Define the logical unit for printout
C
      LOUT = IW(6)
      IER = 0
C
C----- for the charged tracks the probabilities of being
C----- e,mu,kaon,pion,proton are taken out of bank FRID
      KFRID=IW(NAMIND('FRID'))
      CALL YDEFRF(KFRFT0,KFRFT,KFRTL,IFAIL)
      IF(KFRID.LE.0 .OR. KFRFT.LE.0) THEN
        IER = 1
        RETURN
      ENDIF
C-----process all entries of frid
C----- cut on acceptance of particle identification probability pcut
C----- change to supplyabler parameter(s)
C----- default         pcut =  0.01
C----- to be changed:
      PCUT=PIDACP
      DO 200 I=1,LROWS(KFRID)
        CALL YMKZER(1,NMSIZZ,IPIDM)
C
C-- electron JFRIPE muon JFRIPM ,pion JFRIPI, kaon JFRIPK , proton JFRIP
C
        IF(RTABL(KFRID,I,JFRIPE) .GE. PCUT )
     &         CALL YMKSET(1,NMSIZZ,IPIDM,JPAFEP)
        IF(RTABL(KFRID,I,JFRIPM) .GE. PCUT )
     &         CALL YMKSET(1,NMSIZZ,IPIDM,JPAFMP)
        IF(RTABL(KFRID,I,JFRIPI) .GE. PCUT )
     &         CALL YMKSET(1,NMSIZZ,IPIDM,JPAFPP)
        IF(RTABL(KFRID,I,JFRIPK) .GE. PCUT )
     &         CALL YMKSET(1,NMSIZZ,IPIDM,JPAFKP)
        IF(RTABL(KFRID,I,JFRIPP) .GE. PCUT )
     &         CALL YMKSET(1,NMSIZZ,IPIDM,JPAFPR)
C-----get sign of charge as product of curvature and magnetic field
        IF ( (BFIELD * RTABL(KFRFT,I,JFRFIR)) .LE. 0.0 ) THEN
          KPIDF0(I)=IPIDM
          ICHRG=1
        ELSE
C----- negative charged particles
          KPIDF0(I)=ISHFT(IPIDM,1)
          ICHRG=-1
        ENDIF
C-----
C-----
  200 CONTINUE
      RETURN
      END
