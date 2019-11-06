typedef long int ptrdiff_t;
typedef long unsigned int size_t;
typedef int wchar_t;
typedef struct {
  long long __max_align_ll __attribute__((__aligned__(__alignof__(long long))));
  long double __max_align_ld __attribute__((__aligned__(__alignof__(long double))));
  __float128 __max_align_f128 __attribute__((__aligned__(__alignof(__float128))));
} max_align_t;
typedef signed char int8_t;
typedef short int int16_t;
typedef long int int32_t;
typedef long long int int64_t;
typedef unsigned char uint8_t;
typedef short unsigned int uint16_t;
typedef long unsigned int uint32_t;
typedef long long unsigned int uint64_t;
typedef signed char int_least8_t;
typedef short int int_least16_t;
typedef long int int_least32_t;
typedef long long int int_least64_t;
typedef unsigned char uint_least8_t;
typedef short unsigned int uint_least16_t;
typedef long unsigned int uint_least32_t;
typedef long long unsigned int uint_least64_t;
typedef int int_fast8_t;
typedef int int_fast16_t;
typedef int int_fast32_t;
typedef long long int int_fast64_t;
typedef unsigned int uint_fast8_t;
typedef unsigned int uint_fast16_t;
typedef unsigned int uint_fast32_t;
typedef long long unsigned int uint_fast64_t;
typedef long int intptr_t;
typedef long unsigned int uintptr_t;
typedef long long int intmax_t;
typedef long long unsigned int uintmax_t;
enum vga_color {
 VGA_COLOR_BLACK = 0,
 VGA_COLOR_BLUE = 1,
 VGA_COLOR_GREEN = 2,
 VGA_COLOR_CYAN = 3,
 VGA_COLOR_RED = 4,
 VGA_COLOR_MAGENTA = 5,
 VGA_COLOR_BROWN = 6,
 VGA_COLOR_LIGHT_GREY = 7,
 VGA_COLOR_DARK_GREY = 8,
 VGA_COLOR_LIGHT_BLUE = 9,
 VGA_COLOR_LIGHT_GREEN = 10,
 VGA_COLOR_LIGHT_CYAN = 11,
 VGA_COLOR_LIGHT_RED = 12,
 VGA_COLOR_LIGHT_MAGENTA = 13,
 VGA_COLOR_LIGHT_BROWN = 14,
 VGA_COLOR_WHITE = 15,
};
static inline uint8_t vga_entry_color(enum vga_color fg, enum vga_color bg)
{
 return fg | bg << 4;
}
static inline uint16_t vga_entry(unsigned char uc, uint8_t color)
{
 return (uint16_t) uc | (uint16_t) color << 8;
}
size_t strlen(const char* str)
{
 size_t len = 0;
 while (str[len])
  len++;
 return len;
}
static const size_t VGA_WIDTH = 80;
static const size_t VGA_HEIGHT = 25;
size_t terminal_row;
size_t terminal_column;
uint8_t terminal_color;
uint16_t* terminal_buffer;
void terminal_initialize(void)
{
 terminal_row = 0;
 terminal_column = 0;
 terminal_color = vga_entry_color(VGA_COLOR_LIGHT_GREY, VGA_COLOR_BLACK);
 terminal_buffer = (uint16_t*) 0xB8000;
 for (size_t y = 0; y < VGA_HEIGHT; y++) {
  for (size_t x = 0; x < VGA_WIDTH; x++) {
   const size_t index = y * VGA_WIDTH + x;
   terminal_buffer[index] = vga_entry(' ', terminal_color);
  }
 }
}
void terminal_setcolor(uint8_t color)
{
 terminal_color = color;
}
void terminal_putentryat(char c, uint8_t color, size_t x, size_t y)
{
 const size_t index = y * VGA_WIDTH + x;
 terminal_buffer[index] = vga_entry(c, color);
}
void terminal_putchar(char c)
{
 terminal_putentryat(c, terminal_color, terminal_column, terminal_row);
 if (++terminal_column == VGA_WIDTH) {
  terminal_column = 0;
  if (++terminal_row == VGA_HEIGHT)
   terminal_row = 0;
 }
}
void terminal_write(const char* data, size_t size)
{
 for (size_t i = 0; i < size; i++)
  terminal_putchar(data[i]);
}
void terminal_writestring(const char* data)
{
 terminal_write(data, strlen(data));
}
void kernel_main(void)
{
 terminal_initialize();
 terminal_writestring("Hello, kernel World!\n");
}
