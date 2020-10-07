      PARAMETER (LHCTR1=LHCTR+1)
      COMMON /HCSEVA/ NTHCFI(LHCRE),HCAPDE(LPHCT),HCFITW(LHCRE)
     &               ,HCBLSP(LHCNL,LHCSP),NHCTU1(LHCNL),HCTHUL(LHCTR1)
     &               ,PHCTOR(LHCTR),IHCREG(LHCTR)
     &               ,HCLARA(LHCNL),HCLAWI(LHCNL)
     &               ,YBAST1,YBARMX,ZENST1,ZENDMX
     &               ,XBARR0,XENDC0
C
#if defined(DOC)
C!  Hadron Calorimeter geometrical constants
        NTHCFI(LHCRE) number of columns in each tower region
        HCAPDE(LPHCT) upper edges of iron spacers in end-cap
              sextant reference frame
        HCFITW(LHCRE) phi size of tower in region I
        HCBLSP(LHCNL,LHCSP) spacer position in layer
        NHCTU1(LHCNL) = # of tubes in module layer
        HCLARA(LHCNL) layer radius
        HCLAWI(LHCNL) layer width
        HCTHUL(LHCTR1) theta upper limit for towers in row (2-LHCTR1)
                       (the 1st element is the lower limit for tower geo.)
        PHCTOR(LHCTR) phi width in a tower row
        IHCREG(LHCTR) region number for tower in row
        YBAST1 = barrel radius at stack1/stack2 boundary
        YBARMX = max barrel radius at end of stack2
        ZENST1 = endcap zeta at stack1/stack2 boundary
        ZENDMX = endcap zeta at end of stack2
        XBARR0 = local coordinate of wire 0 in barrel layer
        XENDC0 = local coordinate of wire 0 in endcap layer
#endif
