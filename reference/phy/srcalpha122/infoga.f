      SUBROUTINE INFOGA(IPH,ICLUS,NCHFLG,NCRFLG,NGA)
C-----------------------------------------------------------------
C! Return information about a photon IPH
C
C  Input:   -- IPH,    the photon index (ENFLW object)
C  Output:  -- ICLUS,  the original index of the ECALobject
C           -- NCHFLG, 1 if the photon has been extracted from
C                        a charged cluster, 0 otherwise
C           -- NCRFLG, 1 if the photon is in a crack region
C                      0 otherwise.
C           -- NG,     number of extracted photons in cluster ICLUS
C
C  Patrick Janot - 02 July 1991
C-----------------------------------------------------------------
      COMMON / paramfw / idbg,n0,z0,d0,ntrack,mtrack,echmin,echmax,
     .                   rpsf,nngama,sigec,sighc,epilca,epiend,epibar,
     .                   nclea
      COMMON / phyout / gdmult, v0mult, chatot, chapo,chane, enechq(5),
     .                  hadtot, splash, ene12, hadkil, enelon, enetran,
     .                  ierph30, ediff(36),ewire(36),epad(36), difl(4),
     .                  padl(4), wirl(4), sicapi(2)
      COMMON / trknew / lch, lv0, lmg, lmh, lum, lkg, lkh, ltot,
     .                  inew, isave, kfrefw, klsefw, kfrtot(7),
     .                  klstot(7),kan(200),kav(200),kag(200),kam(200),
     .                  nphoch(200),nphocr(200)
      COMMON / flgidt / idtflg(200),muflg(200),muwrd(200),ehcal(200),
     .                  leflg(200),elrtr(200),elrlg(200),dzero(200)
      COMMON / infomu / yxmult(200),yrapp (200),yang (200),
     .                  ysudnt(200),jishad(200),jimcf(200)
      COMMON / infefo / newefol(200),indefol(200)
      COMMON / infohn / propec(200)
      COMMON / infodx / dedxhy(4,200)
      DIMENSION vec(4), lps(7)
      EQUIVALENCE (lps(1), lch)
      CHARACTER*6 prnam(7)
      DATA prnam/'CHARGE','VZERO ','PHOTON','HADRON','LUMI  ',
     .           'KILPHO','KILHAD'/
C
      iclus = 0
      nchflg = 0
      ncrflg = 0
      nga = 0
      iiph = ichefw(iph)
      IF ( iiph .LT. kfrtot(3) .OR. iiph .GT. klstot(3)) GOTO 999
      iclus = kalpha(iiph)
      nchflg = nphoch(iiph-kfrtot(3)+1)
      ncrflg = nphocr(iiph-kfrtot(3)+1)
      DO 1 jph = kfrtot(3), klstot(3)
    1 IF ( kalpha(jph) .EQ. iclus ) nga = nga+1
C
  999 RETURN
      END
