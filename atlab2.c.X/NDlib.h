#ifndef _NDLIB_H_
#define _NDLIB_H_


/************************************************************
 * Library for EE 20222 Chipkit board
 * Delay routines
 * serial (spi) LCD
 * keypad routines
 * 
 * Note: Serial function not in this version. Are in the 
 * serial version
 */


// LCD Function prototypes

/* specific to spi display */
// LCD Function prototypes

/* specific to spi display */

void LCD_init_rate(unsigned long rate);
void LCD_init(void);
void LCD_char(char val);
void LCD_display_on(void);
void LCD_display_off(void);
void LCD_clear(void);
void LCD_backlight(char val);
void LCD_contrast(char val);
void LCD_setpos(char row, char col);


char spi_send(char val);
void LCD_printf( const char* text ); //sends ascii string to display
void LCD_hex(unsigned short val);
void LCD_bin(unsigned short val);
void LCD_dec(unsigned short val);
void LCD_int(unsigned short val);


 void set_output_device(unsigned char device);

 /* delay routines*/
 void set_sys_clock(unsigned long val);
 unsigned long get_sys_clock(void);
 unsigned long get_pb_clock(void);
 void delay_ms(unsigned short val);
 void delay_us(unsigned short val);

 /* keypad routine*/
 unsigned char getkey(unsigned char mode);



#endif //NDLIB_H
