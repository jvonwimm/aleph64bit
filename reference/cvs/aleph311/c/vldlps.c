#if defined(ALEPH_C)

#include "vgbran.h"

lprec *vldlps(int ntrk, const int nhits[NHITDIM], 
	      const int nUseHit[][NHITDIM],
	      int npat, const struct VGBPAT *pat )
{
   lprec *Mlp;
   int l, i, ipat;
   REAL *colArr;
   int hit, offset, h1, h2;
   
   Mlp = make_lp(0, 0);

   /* objective function row; nothing to do */

   /* track constraints */
   for (i=0 ; i<ntrk; i++)
   {
      str_add_constraint(Mlp, "", EQ, 1.0);
   }
   
   /* hit constraints */
   for (l=0; l<NHITDIM; l++)
   {
      for (i = 0; i<nhits[l]; i++)
      {
	 if (nUseHit[i][l] == 1) 
	 {
	    /* hit used only once */
	    str_add_constraint(Mlp, "", EQ, 1.0);
	 }
	 else
	 {
	    /* hit used 1 or 2 times */
	    str_add_constraint(Mlp, "", GE, 1.0);
	    Mlp->orig_upbo[Mlp->rows] = 1.0;
	 }
      }
   }
   
   /* rows are done. now, columns (ie. patterns) */
   
   /* lp_solve has rows numbered 0..Mpl->rows */
   colArr = (REAL *) malloc((Mlp->rows+1)*sizeof(REAL));
   if (colArr == NULL) 
   {
      free(Mlp);
      return NULL;
   }
   
   /* generate 1 row per hit, allowing the hit to be assigned to trk 0 with */
   /* no penalty */
   for (i=0; i<=Mlp->rows; i++) colArr[i] = 0.0;
   offset = ntrk+1;
   for (l=0; l<NHITDIM; l++)
   {
      for (i=0; i<nhits[l]; i++)
      {
	 colArr[offset] = 1.0;
	 /* store the column */
	 add_column(Mlp, colArr);
	 
	 /* set bounds */
	 set_upbo(Mlp, Mlp->columns, 1.0);
	 set_int(Mlp, Mlp->columns, 1);
	 
	 colArr[offset] = 0.0;
	 offset++;
      }
   }
   
   for (ipat=0; ipat<npat; ipat++)
   {
      /* objective */
      colArr[0] = pat[ipat].obj;

      /* clear out track and hit number columns */
      for (i=1; i<=Mlp->rows; i++) colArr[i] = 0.0;
	 
      /* track number */
      if (pat[ipat].trk > 0) colArr[pat[ipat].trk] = 1.0;
	 
      /* hits */
      offset = ntrk;
      for (l=0; l<NHITDIM; l++)
      {
	 hit = pat[ipat].hit[l];
	 if (hit > 0) 
	 {
	    if (hit <= nhits[l])
	       colArr[offset + hit] = 1.0;
	    else 
	    {
	       /* this is a overlap ("greek") hit. the real hit numbers
		  are packed into one integer like h1*GRKSCF + h2 */
	       h1 = hit / GRKSCF;
	       h2 = hit - h1 * GRKSCF;
	       colArr[offset + h1] = 1.0;
	       colArr[offset + h2] = 1.0;
	    }
	 }
	 offset += nhits[l];
      }
	 
      /* store the column */
      add_column(Mlp, colArr);

      /* set bounds */
      set_upbo(Mlp, Mlp->columns, 1.0);
      set_int(Mlp, Mlp->columns, 1);
   }

   free(colArr);
   return Mlp;
}

#endif
