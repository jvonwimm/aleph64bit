      FUNCTION KSISTER(ICH)
C-----------------------------------------------------------------
C! Return the ALPHA index of the sister of a V0 track
C
C  Warning: It works only with V0 tracks!!!
C
C  Patrick Janot - 18 Nov. 1991
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
      jch = ichefw(ich)
      IF     ( jch .LT. kfrtot(2) ) THEN
        ksister = 0
      ELSEIF ( ich .LE. klstot(2) ) THEN
        ksister = kam(jch-kfrtot(2)+1)
      ELSE
        ksister = 0
      ENDIF
C
      RETURN
      END
