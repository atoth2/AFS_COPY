/*
 * File: lab2.c
 * Author: Alexander Toth
 * Created on 2-2-17
 */
#include <stdio.h>
#include <stdlib.h>
#include <xc.h>
#include <proc/p32mx460f512l.h>

#include "configbits32.h" //path relative to main C file
#include "NDlib.h" //path relative to main C file

#define send U1STAbits.UTXBF
#define receive U1STAbits.URXDA

void tserial_init(void)
{
U1MODEbits.ON = 0;    
U1MODEbits.BRGH = 1;
U1BRG = 42;
U1STA = 0x1400;
U1MODEbits.ON = 1;
}


void tputc(char dat)
{
while(send);
U1TXREG = dat;
}

void _mon_putc(char c){
    tputc(c);
}

char tgetc(void)
{
    char dat;
while(!receive);
dat = U1RXREG;
tputc(dat);
return dat;
}

unsigned short getdec(void)
{
unsigned short x;
int n;
unsigned short val;

val=0;
x=0;
for (n=1; n <=5; n++)
    {
val = tgetc();
if(val == '\r') 
{
    break;
}
else
{
    val = val-'0';
x=x*10;
x=x+val;
		}
    }
return x;
}


unsigned short gethex(void)
{
unsigned short x;
int n=0;
unsigned short val;

val=0;
x=0;
for (n=1; n <=4; n++)
{
val = tgetc();
if(val == '\r') 
{
    break;
}
else
{
if (val > '9')
{
val = val - 'A' + 10;
}
else
{
val = val - '0';
}
x = 16*x;
x=x+val;
		}
    	}
return x;
}

/*
 *
 */
void main(void)
{
 
unsigned short x;
unsigned short var;
unsigned short num;
 /* initialization code*/
 

tserial_init();
        
 while (1) // forever loop
{
printf("\r\nEnter format of number (H or D):"); 
x = tgetc();
if (x == 'H')
{
printf("\r\nType hex number: "); 

num=gethex();

printf("\r\nThe value in decimal is: %i",num); 

}
else if(x == 'D')
{
printf("\r\nType decimal number: "); 
num=getdec();
printf("\r\nThe value in hex is: %X",num); 

}

} // end of forever loop

} // end of main

