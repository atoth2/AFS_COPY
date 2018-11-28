/* 
 * File:   lab3at.c
 * Author: Alex
 *
 * Created on February 8, 2017, 8:49 PM
 */

#include <stdio.h>
#include <stdlib.h>
#include <xc.h>
#include "configbits32.h" //path relative to main C file
#include "NDlib.h" //path relative to main C file


#define RELEASED 0
#define PRESSED 1
#define BTN1 PORTCbits.RA6
#define BTN2 PORTCbits.RA7
#define LED1 PORTCbits.RB10
#define LED2 PORTCbits.RB13
       
/*
 * 
 */
void main(void) {

unsigned char count;
unsigned int state1;
unsigned int state2;
    
while (1)
    {
    AD1PCFGbits.RB12 = 1;
    AD1PCFGbits = 1;
    TRISBbits.TRISE = 0;
    TRISBbits.TRISB10 = 0;
    TRISBbits.TRISB13 = 0;  
        if(state1 == PRESSED)
        {
            if(BTN1 = 1)
            {
             ++count;
             LED1 = 1;
            }
            if(BTN1 = 0)
            {
             LED1 = 0;
            }
        }
        if(state1 == RELEASED)
        {
            if(BTN1 = 1)
            {
             ++count;
             LED1 = 1;
            }
            if(BTN1 = 0)
            {
             LED1 = 0;
            }
        }
        if(state2 == PRESSED)
        {
            if(BTN2 = 1)
            {
             --count;
             LED2 = 1;
            }
            if(BTN2 = 0)
            {
             LED2 = 0;
            }
        }
        if(state2 == RELEASED)
        {
            if(BTN2 = 1)
            {
             --count;
             LED2 = 1;
            }
            if(BTN2 = 0)
            {
             LED2 = 0;
            }
        }
    LATE = count;
    }
}
}

