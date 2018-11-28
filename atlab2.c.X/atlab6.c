/*
 * File: lab6.c
 * Author: Alexander Toth
 * Created on 2-22-17
 * PIC32MX460F512L
 */
#include <stdio.h>
#include <stdlib.h>
#include <xc.h>
#include <proc/p32mx460f512l.h>
#include <sys/attribs.h>

#include "configbits32.h" //path relative to main C file
#include "NDlib.h" //path relative to main C file

#define SAMP AD1CON1bits.SAMP
#define DONE AD1CON1bits.DONE

void init_AD(void);
{
AD1PCFG = 0xFFFF; // everything digital 
AD1CHSbits.CH0NA = 0; //negative input *** now or later
AD1PCFGbits.PCFG0 = 0; // set B0 and B1 to analog
AD1PCFGbits.PCFG1 = 0;
AD1CON1bits.FORM = 0;
AD1CON1bits.SSRC = 0;
AD1CON2bits.VCFG = 0;
ADC1CON2bits.BUFM = 0; //single group of 16 buffers
AD1CON2bits.ALTS = 0; //always use MUX A
AD1CON3bits.ADCS = 7;
AD1CON1bits.ON = 1;
}

init_AD(); // initiate ***

unsigned short convert(unsigned short channel);
{
    unsigned short channel;
    AD1CHSbits.CHOSA = channel;  // ***
    SAMP = 1;
    delay_us(1); //delay 200 msecs
    SAMP = 0;
    while(!DONE);
    return ADC1BUF0
}

convert(0); // now or later
unsigned short midx = ADC1BUF0; // inside function? ***
convert(1);
unsigned short midy = ADC1BUF0; // inside function? ***

signed short scale(unsigned short adval, unsigned short mid);
{
unsigned short mid, adval;
signed short ans;
ans = (float) -(mid-adval)/mid*100; 
return ans;
}

/*
 /
 /
 */
void main(void)
{
unsigned short x = midx;
unsigned short y = midy;
signed short xpct = 0;
signed short ypct = 0;

 /* initialization code*/
LCD_init(); //initialize LCD display
TRISE = 0; // Port E pins are configured as outputs ***
LCD_clear();
init_AD();

while (1) // forever loop
{

convert(0); 
x = ADC1BUF0;
scale(x, midx);
xpct = ans; // correct?
convert(1);
y = ADC1BUF0; // inside function?
scale(y, midy);
ypct = ans; // correct?
    
LCD_setpos(0,0); //set display to first position *** print correct?
printf("x: %05d",xval);
LCD_setpos(1,0); //set display to second position
printf("y: %05d",yval);
LCD_setpos(2,0); //set display to third position
printf("xpct: %04d",xpct);
LCD_setpos(3,0); //set display to fourth position
printf("ypct: %04d",ypct);   
   
} // end of forever loop

} // end of main