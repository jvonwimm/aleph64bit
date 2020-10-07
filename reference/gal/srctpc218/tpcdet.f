      FUNCTION TPCDET(ITYPE,KCHAN,JCHN)
C---------------------------------------------------------------------
C! Return the detector capacitance for channel type KCHAN
C  and number JCHN
C
C  Called from:  TSSHAM
C  Calls      :  None
C
C  Inputs:  PASSED:       --ITYPE, the sector type
C                         --KCHAN, the channel type
C                         --JCHN, the channel number
C
C            /TPGEOP/     --TDPHGT, the height of a long pad
C            /TPGEOT/     --TPTRBG, TPTRST, TPTPHW: trigger pad
C                           geometric factors to get trigger-pad lengths
C
C  Outputs:  Func. return --TPCDET, the capacitance in farads
C
C  D. DeMille
C-----------------------------------------------------------------------
      PARAMETER (LTPDRO=21,LTTROW=19,LTSROW=12,LTWIRE=200,LTSTYP=3,
     +           LTSLOT=12,LTCORN=6,LTSECT=LTSLOT*LTSTYP,LTTPAD=4,
     +           LMXPDR=150,LTTSRW=11)
C
      COMMON /TPGEOM/RTPCMN,RTPCMX,ZTPCMX,DRTPMN,DRTPMX,DZTPMX,
     &               TPFRDZ,TPFRDW,TPAVDZ,TPFOF1,TPFOF2,TPFOF3,
     &               TPPROW(LTPDRO),TPTROW(LTTROW),NTSECT,NTPROW,
     &               NTPCRN(LTSTYP),TPCORN(2,LTCORN,LTSTYP),
     &               TPPHI0(LTSECT),TPCPH0(LTSECT),TPSPH0(LTSECT),
     &               ITPTYP(LTSECT),ITPSEC(LTSECT),IENDTP(LTSECT)
C
C
      COMMON /TPGEOP/ NTPDRW(LTSTYP),NTPDPR(LTSROW,LTSTYP),
     &                TPDRBG(LTSTYP),TPDRST(LTSTYP),TPDHGT(LTSTYP),
     &                TPDSEP(LTSTYP),TPDWID(LTSTYP),TPDHWD(LTSTYP),
     &                TPDPHF(LTSROW,LTSTYP),TPDPHW(LTSROW,LTSTYP),
     &                TPDPHS(LTSROW,LTSTYP)
C
C
      COMMON /TPGEOT/ NTPTRW(LTSTYP),NTPTPR(LTTSRW,LTSTYP),
     &                TPTRBG(LTSTYP),TPTRST(LTSTYP),TPTRHG(LTSTYP),
     &                TPTPHC(LTTPAD,LTTSRW,LTSTYP),
     &                TPTPHW(LTTPAD,LTTSRW,LTSTYP),
     &                ITPADG(LTTPAD,LTTSRW,LTSTYP)
C
C
      DATA CPRCM/2.8E-12/
C
      IF ( KCHAN .EQ. 1 ) THEN
         CDET = 5.E-12
C
      ELSEIF ( KCHAN .EQ. 2 ) THEN
         CDET = CPRCM * TPDHGT(ITYPE)
C
      ELSEIF ( KCHAN .EQ. 3 ) THEN
C
         NROW = ((JCHN-1)/4) + 1
         NTPAD = JCHN - 4*(NROW-1)
C
         RAD = TPTRBG(ITYPE) + (NROW-1)*TPTRST(ITYPE)
         PHWD = 2.*TPTPHW(NTPAD,NROW,ITYPE)
         PADLN = RAD * PHWD
C
         CDET = CPRCM * PADLN
C
      ENDIF
C
      TPCDET = CDET
C
      RETURN
      END
