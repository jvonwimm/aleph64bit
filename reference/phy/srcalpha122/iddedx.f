      SUBROUTINE iddedx(itk,hypele,hyppio,hypkao,hyppro)
C----------------------------------------------------------------------
C! Return the dE/dx for four mass hypotheses in the Micro DSTs
C
C  Input  : itk, charged particle ALPHA index in the ENFW section
C                between kfrtot(1) and klstot(2)
C
C  Output : (I_meas-I_exp)/sigma_exp
C              o Hypele : electron mass hypothesis
C              o Hyppio : pion     mass hypothesis
C              o Hypkao : kaon     mass hypothesis
C              o Hyppro : proton   mass hypothesis
C
C  Patrick Janot -- 12 Jan 1995
C----------------------------------------------------------------------
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
      IF ( itk .LT. kfrtot(1) .OR. itk .GT. klstot(2) ) THEN
        hypele = 999.
        hyppio = 999.
        hypkao = 999.
        hyppro = 999.
      ELSE
        hypele = dedxhy(1,itk-kfrtot(1)+1)
        hyppio = dedxhy(2,itk-kfrtot(1)+1)
        hypkao = dedxhy(3,itk-kfrtot(1)+1)
        hyppro = dedxhy(4,itk-kfrtot(1)+1)
        IF ( hypele .GT. 32.766 .AND.
     .       hyppio .GT. 32.766 .AND.
     .       hypkao .GT. 32.766 .AND.
     .       hyppro .GT. 32.766       ) THEN
          hypele = 999.
          hyppio = 999.
          hypkao = 999.
          hyppro = 999.
        ENDIF
      ENDIF
C
  999 RETURN
      END
