/*
 * File: lab6.c
 * Author: Alexander Toth
 * Created on 2-22-17
 * PIC32MX460F512L
 */

#include <xc.h>
#include "configbits32.h"
#include "NDlib.h"
#include <sys/attribs.h>

#define SAMP AD1CON1bits.SAMP
#define DONE AD1CON1bits.DONE

void initialize_AD(void) 
{
    AD1PCFG = 0xFFFF; // everything digital
    AD1PCFGbits.PCFG0 = 0; // set port pin B0 to analog
    AD1PCFGbits.PCFG1 = 0; //set port bin B1 to analog
    AD1CHSbits.CH0NA = 0;
    AD1CON1bits.FORM = 000; // specify output as unsigned integer
    AD1CON1bits.SSRC = 000; // clearing SAMP bits ends sampling and starts conversion
    AD1CON2bits.VCFG = 000; // set voltage reference
    AD1CON2bits.SMPI = 0000; // use only ADC1BUF0
    AD1CON2bits.ALTS = 0; // always use MUX A multiplexer settings
    AD1CON3bits.ADRC = 0; // PBCLK
    AD1CON3bits.ADCS = 7; // 
    AD1CON1bits.ON = 1;
}

void init_(void)
{
T3CONbits.ON = 0;
OC2CONbits.ON = 0;
T3CONbits.TCKPS = 0b010 // prescale = 4
PR3  = 49999;
OC2R = 3750;
OC2CONbits.OCM = 0b110;
OC2CONbits.OCTSEL = 1;
T3CONbits.ON = 0;
OC2CONbits.ON = 0;
}


unsigned short convert(unsigned short channel)
{
    AD1CHSbits.CH0SA = channel;
    SAMP = 1;
    delay_us(1);
    SAMP = 0;
    while (!DONE);
    return ADC1BUF0;
}

signed short scale(unsigned short adval, unsigned short mid) 
{
    signed short val;
    if (adval <= mid) {
        val = (float) (-100)*(mid - adval) / mid;
    }
    if (adval > mid) {
        val = (float) (100)*(adval - mid) / (1023 - mid);
    }
    return val;
}

unsigned short countfun(signed short pct)
{
    unsigned short count;
    count = (float) (pct*12.5)+3750;
    return count;
}

void main(void) 
{
    LCD_init();
    LCD_clear();
    initialize_AD();
    init();
    
    unsigned short midx = convert(0);
    unsigned short xval;
    signed short xpct;
    unsigned short xcount;
    while (1) 
    {
        xval = convert(0);
        yval = convert(1);
        xpct = scale(xval, midx);
        xcount = countfun(xpct);
        OC2RS = xcount;
        
        LCD_setpos(0, 0);
        printf("x: %05d ", xval);
        LCD_setpos(1, 0);
        printf("xpct: %04d ", xpct);
        LCD_setpos(2, 0);
        printf("OC2RS: %04d ", xcount);
        LCD_setpos(3, 0);
        printf("OC3RS: %04d ", xcount);
        printf("RPM: %03d ", xcount);
    }
}


