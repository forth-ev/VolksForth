
/* ufscr2file.c, (c) Philip Zembrod, 1995, 2007 */

/* Wandelt eine C64-ultraFORTH-Screen-Diskette in ein PC- bzw. Unix-ASCII-File
   um. Die ultraFORTH-Screen-Diskette muss im .d64-Format vorliegen. */

#include <stdio.h>
#include <stdlib.h>

int nextchar(FILE *in)
{
  static int tr=1,se=0,by=0;
  int c,flag;

  do
    { c=fgetc(in);
      if(c==EOF) break;
      flag = (tr==18) && (se<3); /* bugfix 26.10.95: Spur 18 Sektor 2 muss auch
				 uebersprungen werden. */
      by++;
      if(by%256==0)
    	{   se++;
            if(se>20)
	        { se=0; tr++; }
    	}
    } while(flag);
  return(c);
}

int convchar(int c)
{
  if(c>=0x41 && c<=0x5a) return(c+0x20);
  if(c>=0xc1 && c<=0xda) return(c-0x80);
  if(c>=0xdb && c<=0xdf) return(c-0x60);
  if(c<0x20) return(0x20);
  return(c);
}

void bputc(char c, FILE *out)
{
  static char line[50], *p=line;
  if(p-line>45)
    { fprintf(stderr,"!!! overlong line !!!\n");
      exit(1);
    }
  if(c=='\n')
    { while(p>line && *(p-1)==' ') p--;
      *p='\0';
      fprintf(out,"%s\n",line);
      p=line;
      return;
    }
  *p++=c;
}

int printblock(FILE *in, FILE *out, char *name, int n)
{
  int line,col,c;
//  fprintf(out,"\\ Disk %s, Block No. %d\n",name,n);
  fprintf(out,"\\ Block No. %d\n",n);
  for(line=0;line<25;line++)
    { for(col=0;col<(line==24?40:41);col++)
	{ if((c=nextchar(in))==EOF) 
	    { fprintf(out,"\n### premature end of input file ###\n");
	      return(1); }
	  bputc((char)convchar(c),out);
	}
      bputc('\n',out);
    }
  return(0);
}
	  
int main(int argc, char *argv[])
{
  FILE *in, *out;
  int block;
  if(argc<2 || argc>3)
    { fprintf(stderr,"usage: %s file.d64 [outfile]\n",argv[0]);
      return(argc!=1); }
  if((in=fopen(argv[1],"r"))==NULL)
    { fprintf(stderr,"%s: can't open %s\n",argv[0],argv[1]);
      return(1); }
  if(argc==3)
    { if((out=fopen(argv[2],"w"))==NULL)
	{ fprintf(stderr,"%s: can't open %s\n",argv[0],argv[2]);
	  return(1); }
    }
  else
    {
      out=stdout;
    }
  for(block=0;block<170;block++)
    if(printblock(in,out,argv[1],block))
      return(1);
  return(0);
}

