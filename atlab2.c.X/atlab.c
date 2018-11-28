/*
 * File: lab2.c
 * Author: Alexander Toth
 * Created on 2-2-17
 */
#include <xc.h>
#include <stdio.h>
#include "configbits32.h" //path relative to main C file
#include "NDlib.h" //path relative to main C file

unsigned short getdec(void);
{
x=0
	for (n=1, n <=5, n++)
    {
val = getkey(0);
x=x+val;
x=x*10;
return x;
    }
}


unsigned short gethex(void);
{
x=0
	for (n=1, n <=4, n++)
    {
val = getkey(0);
x=x+val;
x=x*16;
return val;
    }
}

/*
 *
 */
void main(void)
{
 
int short x;
int var;

 /* initialization code*/
 LCD_init(); //initialize LCD display
 while (1) // forever loop
 {
 LCD_clear();
x=getdec(void)
 
LCD_setpos(0,0); //set display to first position
 printf("Enter dec:"); 
 LCD_dec(x)
LCD_setpos(1,0); //set display to first position
printf("B:"); 
LCD_bin(x);
LCD_setpos(2,0); //set display to first position
printf("H:"); 
LCD_hex(x);
LCD_setpos(3,0); //set display to first position
printf("I:"); 
LCD_int(x);
printf(" C: "); 
LCD_char(x);

getkey(0)

x=gethex(void)
 
LCD_setpos(0,0); //set display to first position
 printf("Enter hex:"); 
 LCD_hex(x)
LCD_setpos(1,0); //set display to first position
printf("B:"); 
LCD_bin(x);
LCD_setpos(2,0); //set display to first position
printf("D:"); 
LCD_dec(x);
LCD_setpos(3,0); //set display to first position
printf("I:"); 
LCD_int(x);
printf(" C: "); 
LCD_char(x);

getkey(0)


} // end of forever loop

} // end of main
