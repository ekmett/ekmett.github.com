
void bfps_reverse(unsigned char *dest, unsigned char *from, unsigned long  len);
void bfps_intersperse(unsigned char *dest, unsigned char *from, unsigned long  len, unsigned char c);
unsigned char bfps_maximum(unsigned char *p, unsigned long  len);
unsigned char bfps_minimum(unsigned char *p, unsigned long  len);
unsigned long bfps_count(unsigned char *p, unsigned long  len, unsigned char w);
unsigned long bfps_count_extras(unsigned char *p, unsigned long  len, unsigned char w);
