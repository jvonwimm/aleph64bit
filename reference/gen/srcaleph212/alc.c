extzv(pos,size,base,dest)                                                       
                                                                                
int pos;                                                                        
int size;                                                                       
int base;                                                                       
int *dest;                                                                      
                                                                                
{                                                                               
                                                                                
   *dest = (base >> pos) & ((1 << size) - 1);                                   
   if ((32 - pos) < size) {                                                     
     *dest = *dest & ((1 << (32 - pos)) - 1);                                   
   }                                                                            
                                                                                
   return;                                                                      
                                                                                
}                                                                               
                                                                                
insv(src,pos,size,base)                                                         
                                                                                
int src;                                                                        
int pos;                                                                        
int size;                                                                       
int *base;                                                                      
                                                                                
{                                                                               
                                                                                
   if (size == 0) return;                                                       
                                                                                
   *base = (~(((1 << size) - 1) << pos) & *base) |                              
                          ((((1 << size) - 1) & src) << pos);                   
                                                                                
   return;                                                                      
                                                                                
}                                                                               
                                                                                
/* CMPBU5(A,B,NROW,NBITS,NCOL) */                                               
                                                                                
#define WORDLENGTH 32                                                           
                                                                                
cmpbu5_(a,b,nr,l,nc)                                                            
                                                                                
int a[];                                                                        
int b[];                                                                        
int *nr;                                                                        
int *l;                                                                         
int *nc;                                                                        
                                                                                
{                                                                               
                                                                                
   int nrow;   /* r0 */                                                         
   int ib;     /* r2 */                                                         
   int i;      /* r5 - bit position [0..31] */                                  
   int temp1;  /* r6 */                                                         
   int temp2;  /* r7 */                                                         
   int m;      /* r9 - extracted integer */                                     
   int ilow;   /* r10 */                                                        
   int fact;   /* r11 */                                                        
   int bindex;                                                                  
                                                                                
         ib = 0;                                                                
         bindex = 0;                                                            
         nrow = *nr;                                                            
                                                                                
         i = WORDLENGTH;                                                        
bloop:   i = i - *l;                                                            
         if (0 > i) goto bl101;                                                 
         insv(a[ib],i,*l,&temp2);                                               
         ib = ib + *nc;                                                         
         nrow--;                                                                
         if (nrow > 0) goto bloop;                                              
         b[bindex++] = temp2;                                                   
         return;                                                                
                                                                                
bl101:   i = i & (WORDLENGTH - 1);                                              
         insv(a[ib]>>(WORDLENGTH-i),0,*l-(WORDLENGTH-i),&temp2);                
         insv(a[ib],i,*l,&temp1);                                               
         ib = ib + *nc;                                                         
         b[bindex++] = temp2;                                                   
         temp2 = temp1;                                                         
         nrow--;                                                                
         if (nrow > 0) goto bloop;                                              
         b[bindex] = temp2;                                                     
         return;                                                                
                                                                                
}                                                                               
                                                                                
/* CMPBU4(A,B,NROW,NBITS,NCOL,ILOW) */                                          
                                                                                
#define WORDLENGTH 32                                                           
                                                                                
cmpbu4_(a,b,nr,l,nc,ilow)                                                       
                                                                                
int a[];                                                                        
int b[];                                                                        
int *nr;                                                                        
int *l;                                                                         
int *nc;                                                                        
int *ilow;                                                                      
                                                                                
{                                                                               
                                                                                
   int nrow;   /* r0 */                                                         
   int ib;     /* r2 */                                                         
   int i;      /* r5 - bit position [0..31] */                                  
   int temp1;  /* r6 */                                                         
   int temp2;  /* r7 */                                                         
   int m;      /* r9 - extracted integer */                                     
   int fact;   /* r11 */                                                        
   int bindex;                                                                  
                                                                                
         ib = 0;                                                                
         bindex = 0;                                                            
         nrow = *nr;                                                            
                                                                                
         i = WORDLENGTH;                                                        
bloop2:  fact = a[ib] - *ilow;                                                  
         ib = ib + *nc;                                                         
         i = i - *l;                                                            
         if (0 > i) goto bl1012;                                                
         insv(fact,i,*l,&temp2);                                                
         nrow--;                                                                
         if (nrow > 0) goto bloop2;                                             
         b[bindex] = temp2;                                                     
         return;                                                                
                                                                                
bl1012:  i = i & (WORDLENGTH - 1);                                              
         insv(fact>>(WORDLENGTH-i),0,*l-(WORDLENGTH-i),&temp2);                 
         insv(fact,i,*l,&temp1);                                                
         b[bindex++] = temp2;                                                   
         temp2 = temp1;                                                         
         nrow--;                                                                
         if (nrow > 0) goto bloop2;                                             
         b[bindex] = temp2;                                                     
         return;                                                                
                                                                                
}                                                                               
                                                                                
/* CMPBU3(A,B,NROW,NBITS,NCOL,ILOW,FACT) */                                     
                                                                                
#define WORDLENGTH 32                                                           
                                                                                
cmpbu3_(a,b,nr,l,nc,ilow,fact)                                                  
                                                                                
float *a[];                                                                     
int *b[];                                                                       
int *nr;                                                                        
int *l;                                                                         
int *nc;                                                                        
int *ilow;                                                                      
float *fact;                                                                    
                                                                                
{                                                                               
                                                                                
   int nrow;   /* r0 */                                                         
   int ib;     /* r2 */                                                         
   int i;      /* r5 - bit position [0..31] */                                  
   int temp1;  /* r6 */                                                         
   int temp2;  /* r7 */                                                         
   int m;      /* r9 - extracted integer */                                     
   int r12;    /* r12 */                                                        
   int bindex;                                                                  
                                                                                
         ib = 0;                                                                
         bindex = 0;                                                            
         nrow = *nr;                                                            
                                                                                
bloop3:  r12 = *fact * *a[ib];                                                  
         r12 - r12 - *ilow;                                                     
         ib = ib + *nc;                                                         
         i = WORDLENGTH - *l;                                                   
         if (*l > i) goto bl1013;                                               
         insv(r12,i,*l,&temp2);                                                 
         nrow--;                                                                
         if (nrow > 0) goto bloop3;                                             
         *b[bindex] = temp2;                                                    
         return;                                                                
                                                                                
bl1013:  i = i & (WORDLENGTH - 1);                                              
         insv(r12,i,*l,&temp1);                                                 
         *b[bindex++] = temp2;                                                  
         temp2 = temp1;                                                         
         nrow--;                                                                
         if (nrow > 0) goto bloop3;                                             
         *b[bindex] = temp2;                                                    
         return;                                                                
                                                                                
}                                                                               
                                                                                
/* DMPBL5(A,B,NROW,NBITS,NCOL) */                                               
                                                                                
#define WORDLENGTH 32                                                           
                                                                                
dmpbl5_(a,b,nr,l,nc)                                                            
                                                                                
int a[];                                                                        
int b[];                                                                        
int *nr;                                                                        
int *l;                                                                         
int *nc;                                                                        
                                                                                
{                                                                               
                                                                                
   int nrow;   /* r0 */                                                         
   int ib;     /* r2 */                                                         
   int i;      /* r5 - bit position [0..31] */                                  
   int r6;     /* r6 */                                                         
   int r7;     /* r7 */                                                         
   int m;      /* r9 - extracted integer */                                     
   int ilow;   /* r10 */                                                        
   int fact;   /* r11 */                                                        
   int aindex;                                                                  
   int j;                                                                       
                                                                                
       ib = 0;                                                                  
       aindex = 0;                                                              
       nrow = *nr;                                                              
                                                                                
       if (*l == 16) goto s16;                                                  
       if (*l == 8) goto s8;                                                    
       if (*l == 4) goto s4;                                                    
                                                                                
       r7 = a[aindex++];                                                        
       r6 = a[aindex++];                                                        
       i = WORDLENGTH;                                                          
                                                                                
loop:  j = i;                                                                   
       i = i - *l;                                                              
       if (i < 0) goto l101;                                                    
       extzv(i,*l,r7,&b[ib]);                                                   
       ib = ib + *nc;                                                           
       nrow--;                                                                  
       if (nrow > 0) goto loop;                                                 
       return;                                                                  
                                                                                
l101:  i = i & (WORDLENGTH - 1);                                                
       extzv(i,*l,r6,&b[ib]);                                                   
       b[ib] = b[ib] | ((r7 & ((1 << j) - 1)) << (WORDLENGTH - i));             
       r7 = r6;                                                                 
       r6 = a[aindex++];                                                        
       ib = ib + *nc;                                                           
       nrow--;                                                                  
       if (nrow > 0) goto loop;                                                 
       return;                                                                  
                                                                                
s16:   i = nrow >> 1;                                                           
       if (i == 0) goto ll16;                                                   
l16:   r7 = a[aindex++];                                                        
       extzv(16,*l,r7,&b[ib]);                                                  
       ib = ib + *nc;                                                           
       b[ib] = r7 & 65535;                                                      
       ib = ib + *nc;                                                           
       i--;                                                                     
       if (i > 0) goto l16;                                                     
ll16:  nrow = nrow & 1;                                                         
       if (nrow == 0) return;                                                   
       r7 = a[aindex++];                                                        
       extzv(16,*l,r7,&b[ib]);                                                  
       return;                                                                  
                                                                                
s8:    i = nrow >> 2;                                                           
       if (i == 0) goto ll8;                                                    
l8:    r7 = a[aindex++];                                                        
       extzv(24,*l,r7,&b[ib]);                                                  
       ib = ib + *nc;                                                           
       extzv(16,*l,r7,&b[ib]);                                                  
       ib = ib + *nc;                                                           
       extzv(8,*l,r7,&b[ib]);                                                   
       ib = ib + *nc;                                                           
       b[ib] = r7 & 255;                                                        
       ib = ib + *nc;                                                           
       i--;                                                                     
       if (i > 0) goto l8;                                                      
ll8:   nrow = nrow & 3;                                                         
       if (nrow == 0) return;                                                   
       r7 = a[aindex++];                                                        
       extzv(24,*l,r7,&b[ib]);                                                  
       ib = ib + *nc;                                                           
       nrow--;                                                                  
       if (nrow == 0) return;                                                   
       extzv(16,*l,r7,&b[ib]);                                                  
       ib = ib + *nc;                                                           
       nrow--;                                                                  
       if (nrow == 0) return;                                                   
       extzv(8,*l,r7,&b[ib]);                                                   
       return;                                                                  
                                                                                
s4:    i = nrow >> 3;                                                           
       if (nrow == 0) goto ll4;                                                 
l4:    r7 = a[aindex++];                                                        
       extzv(28,*l,r7,&b[ib]);                                                  
       ib = ib + *nc;                                                           
       extzv(24,*l,r7,&b[ib]);                                                  
       ib = ib + *nc;                                                           
       extzv(20,*l,r7,&b[ib]);                                                  
       ib = ib + *nc;                                                           
       extzv(16,*l,r7,&b[ib]);                                                  
       ib = ib + *nc;                                                           
       extzv(12,*l,r7,&b[ib]);                                                  
       ib = ib + *nc;                                                           
       extzv(8,*l,r7,&b[ib]);                                                   
       ib = ib + *nc;                                                           
       extzv(4,*l,r7,&b[ib]);                                                   
       ib = ib + *nc;                                                           
       extzv(0,*l,r7,&b[ib]);                                                   
       ib = ib + *nc;                                                           
       i--;                                                                     
       if (i > 0) goto l4;                                                      
ll4:   nrow = nrow & 7;                                                         
       if (nrow == 0) return;                                                   
       r7 = a[aindex++];                                                        
       extzv(28,*l,r7,&b[ib]);                                                  
       ib = ib + *nc;                                                           
       nrow--;                                                                  
       if (nrow == 0) return;                                                   
       extzv(24,*l,r7,&b[ib]);                                                  
       ib = ib + *nc;                                                           
       nrow--;                                                                  
       if (nrow == 0) return;                                                   
       extzv(20,*l,r7,&b[ib]);                                                  
       ib = ib + *nc;                                                           
       nrow--;                                                                  
       if (nrow == 0) return;                                                   
       extzv(16,*l,r7,&b[ib]);                                                  
       ib = ib + *nc;                                                           
       nrow--;                                                                  
       if (nrow == 0) return;                                                   
       extzv(12,*l,r7,&b[ib]);                                                  
       ib = ib + *nc;                                                           
       nrow--;                                                                  
       if (nrow == 0) return;                                                   
       extzv(8,*l,r7,&b[ib]);                                                   
       ib = ib + *nc;                                                           
       nrow--;                                                                  
       if (nrow == 0) return;                                                   
       extzv(4,*l,r7,&b[ib]);                                                   
       return;                                                                  
                                                                                
}                                                                               
                                                                                
/* DMPBL4(A,B,NROW,NBITS,NCOL,ILOW) */                                          
                                                                                
#define WORDLENGTH 32                                                           
                                                                                
dmpbl4_(a,b,nr,l,nc,ilow)                                                       
                                                                                
int a[];                                                                        
int b[];                                                                        
int *nr;                                                                        
int *l;                                                                         
int *nc;                                                                        
int *ilow;                                                                      
                                                                                
{                                                                               
                                                                                
   int nrow;   /* r0 */                                                         
   int ib;     /* r2 */                                                         
   int i;      /* r5 - bit position [0..31] */                                  
   int r6;     /* r6 */                                                         
   int r7;     /* r7 */                                                         
   int m;      /* r9 - extracted integer */                                     
   int fact;   /* r11 */                                                        
   int aindex;                                                                  
   int j;                                                                       
                                                                                
       ib = 0;                                                                  
       aindex = 0;                                                              
       nrow = *nr;                                                              
                                                                                
       if (*l == 16) goto s16b;                                                 
       if (*l == 8) goto s8b;                                                   
       if (*l == 4) goto s4b;                                                   
                                                                                
       r7 = a[aindex++];                                                        
       r6 = a[aindex++];                                                        
       i = WORDLENGTH;                                                          
                                                                                
loop2: j = i;                                                                   
       i = i - *l;                                                              
       if (0 > i) goto l1012;                                                   
       extzv(i,*l,r7,&m);                                                       
       b[ib] = m + *ilow;                                                       
       ib = ib + *nc;                                                           
       nrow--;                                                                  
       if (nrow > 0) goto loop2;                                                
       return;                                                                  
                                                                                
l1012: i = i & (WORDLENGTH - 1);                                                
       extzv(i,*l,r6,&m);                                                       
       m = m | ((r7 & ((1 << j) - 1)) << (WORDLENGTH - i));                     
       r7 = r6;                                                                 
       r6 = a[aindex++];                                                        
       b[ib] = m + *ilow;                                                       
       ib = ib + *nc;                                                           
       nrow--;                                                                  
       if (nrow > 0) goto loop2;                                                
       return;                                                                  
                                                                                
s16b:  i = nrow >> 1;                                                           
       if (i == 0) goto ll16b;                                                  
l16b:  r7 = a[aindex++];                                                        
       extzv(16,*l,r7,&m);                                                      
       b[ib] = m + *ilow;                                                       
       ib = ib + *nc;                                                           
       m = r7 & 65535;                                                          
       b[ib] = m + *ilow;                                                       
       ib = ib + *nc;                                                           
       i--;                                                                     
       if (i > 0) goto l16b;                                                    
ll16b: nrow = nrow & 1;                                                         
       if (nrow == 0) return;                                                   
       r7 = a[aindex++];                                                        
       extzv(16,*l,r7,&m);                                                      
       b[ib] = m + *ilow;                                                       
       return;                                                                  
                                                                                
s8b:   i = nrow >> 2;                                                           
       if (i == 0) goto ll8b;                                                   
l8b:   r7 = a[aindex++];                                                        
       extzv(24,*l,r7,&m);                                                      
       b[ib] = m + *ilow;                                                       
       ib = ib + *nc;                                                           
       extzv(16,*l,r7,&m);                                                      
       b[ib] = m + *ilow;                                                       
       ib = ib + *nc;                                                           
       extzv(8,*l,r7,&m);                                                       
       b[ib] = m + *ilow;                                                       
       ib = ib + *nc;                                                           
       m = r7 & 255;                                                            
       b[ib] = m + *ilow;                                                       
       ib = ib + *nc;                                                           
       i--;                                                                     
       if (i > 0) goto l8b;                                                     
ll8b:  nrow = nrow & 3;                                                         
       if (nrow == 0) return;                                                   
       r7 = a[aindex++];                                                        
       extzv(24,*l,r7,&m);                                                      
       b[ib] = m + *ilow;                                                       
       ib = ib + *nc;                                                           
       nrow--;                                                                  
       if (nrow == 0) return;                                                   
       extzv(16,*l,r7,&m);                                                      
       b[ib] = m + *ilow;                                                       
       ib = ib + *nc;                                                           
       nrow--;                                                                  
       if (nrow == 0) return;                                                   
       extzv(8,*l,r7,&m);                                                       
       b[ib] = m + *ilow;                                                       
       return;                                                                  
                                                                                
s4b:   i = nrow >> 3;                                                           
       if (nrow == 0) goto ll4b;                                                
l4b:   r7 = a[aindex++];                                                        
       extzv(28,*l,r7,&m);                                                      
       b[ib] = m + *ilow;                                                       
       ib = ib + *nc;                                                           
       extzv(24,*l,r7,&m);                                                      
       b[ib] = m + *ilow;                                                       
       ib = ib + *nc;                                                           
       extzv(20,*l,r7,&m);                                                      
       b[ib] = m + *ilow;                                                       
       ib = ib + *nc;                                                           
       extzv(16,*l,r7,&m);                                                      
       b[ib] = m + *ilow;                                                       
       ib = ib + *nc;                                                           
       extzv(12,*l,r7,&m);                                                      
       b[ib] = m + *ilow;                                                       
       ib = ib + *nc;                                                           
       extzv(8,*l,r7,&m);                                                       
       b[ib] = m + *ilow;                                                       
       ib = ib + *nc;                                                           
       extzv(4,*l,r7,&m);                                                       
       b[ib] = m + *ilow;                                                       
       ib = ib + *nc;                                                           
       extzv(0,*l,r7,&m);                                                       
       b[ib] = m + *ilow;                                                       
       ib = ib + *nc;                                                           
       i--;                                                                     
       if (i > 0) goto l4b;                                                     
ll4b:  nrow = nrow & 7;                                                         
       if (nrow == 0) return;                                                   
       r7 = a[aindex++];                                                        
       extzv(28,*l,r7,&m);                                                      
       b[ib] = m + *ilow;                                                       
       ib = ib + *nc;                                                           
       nrow--;                                                                  
       if (nrow == 0) return;                                                   
       extzv(24,*l,r7,&m);                                                      
       b[ib] = m + *ilow;                                                       
       ib = ib + *nc;                                                           
       nrow--;                                                                  
       if (nrow == 0) return;                                                   
       extzv(20,*l,r7,&m);                                                      
       b[ib] = m + *ilow;                                                       
       ib = ib + *nc;                                                           
       nrow--;                                                                  
       if (nrow == 0) return;                                                   
       extzv(16,*l,r7,&m);                                                      
       b[ib] = m + *ilow;                                                       
       ib = ib + *nc;                                                           
       nrow--;                                                                  
       if (nrow == 0) return;                                                   
       extzv(12,*l,r7,&m);                                                      
       b[ib] = m + *ilow;                                                       
       ib = ib + *nc;                                                           
       nrow--;                                                                  
       if (nrow == 0) return;                                                   
       extzv(8,*l,r7,&m);                                                       
       b[ib] = m + *ilow;                                                       
       ib = ib + *nc;                                                           
       nrow--;                                                                  
       if (nrow == 0) return;                                                   
       extzv(4,*l,r7,&m);                                                       
       b[ib] = m + *ilow;                                                       
       return;                                                                  
                                                                                
}                                                                               
#include <stdio.h>                                                              
                                                                                
dmpbl3_() {                                                                     
  fprintf(stderr,"FATAL ERROR: unsupported routine dmpbl3 called.\n");          
  exit(1);                                                                      
}                                                                               
                                                                                
/* DMPCOP(A,B,NCOL,NROW) */                                                     
                                                                                
#define WORDLENGTH 32                                                           
                                                                                
dmpcop_(a,b,nc,nr)                                                              
                                                                                
int a[];                                                                        
int b[];                                                                        
int *nr;                                                                        
int *nc;                                                                        
                                                                                
{                                                                               
                                                                                
   int ib;                                                                      
   int ia;                                                                      
   int nrow;                                                                    
                                                                                
         ib = 0;                                                                
         ia = 0;                                                                
         nrow = *nr;                                                            
loopc:   b[ib] = a[ia++];                                                       
         ib = ib + *nc;                                                         
         nrow--;                                                                
         if (nrow == 0) return;                                                 
         goto loopc;                                                            
                                                                                
}                                                                               
