/*
 * File: lab8.c
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

// initialize timer
void init_timeroc(void)
{
    PR3 = 5999;
    T3CONbits.TCKPS = 1;

    OC2CONbits.ON = 0; //turn off
    OC2CONbits.OC32 = 0; //16 bit timer
    OC2CONbits.OCTSEL = 1; //timer 3 for OCM
    OC2CONbits.OCM = 110; //PWM mode on OCx; Fault pin disabled
    OC2R = 1; //upper 16 bits ocm
    OC2RS = PR3; // initial rate 1.5 kHz
    OC2CONbits.ON = 1; //turn on
    
    OC3CONbits.ON = 0; //turn off
    OC3CONbits.OC32 = 0; //16 bit timer
    OC3CONbits.OCTSEL = 1; //timer 3 for OCM
    OC3CONbits.OCM = 110; //PWM mode on OCx; Fault pin disabled
    OC3R = 1; //upper 16 bits ocm
    OC3RS = PR3; // initial rate 1.5 kHz
    OC3CONbits.ON = 1; //turn on
}

// stopwatch 
unsigned int hunds = 0;
char tic = 0xFF;// Set semaphore

void init_timer4(void)
{
    PR4 = 50000;
    T4CONbits.TCKPS = 1;
    
    // interrupt vectors 16
    IFS0bits.T4IF = 0;
    IEC0bits.T4IE = 0;
    IPC4bits.T4IP = 0;

}

void init_timer2(void)
{
    T2CONbits.ON = 0;
    T2CONbits.T32 = 0;
    T2CONbits.TCS = 1;
    T2CONbits.TC2KI = RC1;
    T2CONbits.ON = 1;
}

void __ISR(8, IPL6AUTO) function_name(void)
{
IFS0bits.T2IF = 0; // Clear timer interrupt status flag 
hunds++;
if (hunds >= 100)
{
    hunds =  0;
    tic = 1;
}
    toggle0;// toggle bit on logic analyzer 
return;
} 

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


unsigned short convert(unsigned short channel)
{
    AD1CHSbits.CH0SA = channel;  // ***
    SAMP = 1;
    delay_us(1); //delay 200 msecs
    SAMP = 0;
    while(!DONE);
    return ADC1BUF0;
}

unsigned short pulsewidth1;
unsigned short pulsewidth2;
signed short scale(unsigned short adval, unsigned short mid)
{
signed short ans;

ans = ((float) 50 -(mid-adval))/mid*50;

if (ans == 0)
{
    pulsewidth1 = PR3 + 1;
    pulsewidth1 = 0x0000;
}
else if (ans == 50)
{
    pulsewidth1 = PR3;
    pulsewidth2 = PR3;
}
else if (ans == 100)
{
    pulsewidth1 = 0x0000;
    pulsewidth1 = PR3 + 1;
}
return ans;
}

/*
 /
 /
 */
void main(void)
{
unsigned short midx;
unsigned short x = midx;
signed short xpct;

unsigned short rpm = 0;

 /* initialization code*/
TMR4 = 0x0; // Clear timer register
IEC0SET = 0x00000100; // Enable timer interrupts
__builtin_enable_interrupts();
INTCONbits.MVEC = 1;

LCD_init(); //initialize LCD display
TRISE = 0; // Port E pins are configured as outputs ***
LCD_clear();
init_AD();
init_timeroc();

midx = convert(0); 
xpct = scale(x, midx);

while (1) // forever loop
{
x = convert(0); 
xpct = scale(x, midx);
OC2RS = pulsewidth1;
OC3RS = pulsewidth2;
    

if(tic)
   {
   tic = 0;
   rpm = TMR2/32*60;
   TMR2 = 0x0;
   }


LCD_setpos(0,0); //set display to first position *** print correct?
printf("x: %05d",x);

LCD_setpos(1,0); //set display to third position
printf("xpct: %04d",xpct);

LCD_setpos(2,0); //set display to third position
printf("OC2RS: %04d",OC2RS);
  
LCD_setpos(3, 0);
printf("OC3RS: %04d ", OC3RS);
printf(" RPM: %03d ", rpm);
        
} // end of forever loop

} // end of main
