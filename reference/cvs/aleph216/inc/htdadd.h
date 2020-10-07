C! decode the tube address
      NFLROR = IADR
      ITUB   = MOD (NFLROR,1000)
      NFLROR = NFLROR/1000
      ILAY   = MOD (NFLROR,100)
      NFLROR = NFLROR/100
      IMOD   = MOD (NFLROR,100)
      IPOR   = NFLROR/100
