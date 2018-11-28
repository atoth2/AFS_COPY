/*
 * File: lab5.c
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


#define Timer2 TCON

#define toggle0 LATEbits.LATE0 = 1; Nop(); LATEbits.LATE0 = 0;
#define toggle1 LATEbits.LATE1 = 1; Nop(); LATEbits.LATE1 = 0;
#define toggle2 LATEbits.LATE2 = 1; Nop(); LATEbits.LATE2 = 0;

#define START PORTEbits.RE8
#define STOP PORTDbits.RD0
#define LAP PORTDbits.RD8
#define CLEAR PORTDbits.RD13

unsigned int hunds = 0;
char tic = 0xFF;// Set semaphore

void init_timer(void)
{
    PR2 = 50000;
    T2CONBITS.TCKPS = 1;
}


void __ISR(trate, IPL6AUTO) function_name(void)
{
IFS0CLR = 0x00000100; // Clear timer interrupt status flag 
++hunds
if (hunds >= 100)
{
    hunds =  0;
    tic = 0xFF;
}
    toggle0;// toggle bit on logic analyzer 
return;
} 


/*
 /
 /
 */
void main(void)
{
unsigned int secs;
unsigned int mins;
unsigned int hours;
unsigned short var;
unsigned int trate = 50000;

 /* initialization code*/
LCD_init(); //initialize LCD display
init_timer();
TMR2 = 0x0; // Clear timer register
TRISE = 0; // Port E pins are configured as outputs
IEC0SET = 0x00000100; // Enable timer interrupts
LCD_clear();

while (1) // forever loop
{

 LCD_setpos(0,0); //set display to first position
printf("T,%02i,%:02i,%:02i",mins,secs,hunds);
     toggle1;// toggle bit on logic analyzer 

if(tic)
   {
   tic = 0;
   secs++
   }
 if (secs >= 60)
 {
     mins++;
     secs = 0;
 }
  if (mins >= 60)
 {
     hours++;
     mins = 0;
 }
 
 if (START == 0) T3CONSET = 0x8000; // Start timer
 if (STOP == 0) T3CON = 0x0; // Stop timer and clear control register
 if (LAP == 0) 
 {
     PR3 = 0x3FFF; // Load period register    
         toggle2;// toggle bit on logic analyzer 
 }
 if (CLEAR == 0) TMR3 = 0x0; // Clear timer register
         
 }
 
} // end of forever loop

} // end of main