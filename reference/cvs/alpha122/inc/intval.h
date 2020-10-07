      PARAMETER (mxtrk = 200)
      COMMON / muonidt / muonch(mxtrk), muonhc(mxtrk), enmuon(mxtrk),
     .                   nbmuon
      COMMON / elecidt / nlecch(mxtrk), enelec(mxtrk),pcelec(mxtrk),
     .                   npelec(mxtrk),nelect
      COMMON / relink / ncht(mxtrk),delt(mxtrk),pexhca(mxtrk,3),kchatt
      COMMON / idtint / idflag(mxtrk), itkflg(mxtrk), nchflg
      COMMON / storid / muflag(mxtrk), muword(mxtrk), umhcal(mxtrk),
     .                  leflag(mxtrk), elrt  (mxtrk), elrl  (mxtrk),
     .                  itkmue(mxtrk), nstore
      COMMON / muinfo / xxmult(mxtrk),xrapp (mxtrk), xang(mxtrk),
     .                  xsudnt(mxtrk),iishad(mxtrk),iimcf(mxtrk)
      COMMON / rescal / itkres(mxtrk), resfac(mxtrk), nresca
C
