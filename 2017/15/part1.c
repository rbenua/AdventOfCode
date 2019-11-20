#include <stdio.h>
#include <stdlib.h>

int main()
{
    int seen = 0;
    uint64_t aval = 783, bval = 325;
    for(int i = 0; i < 40000000; i++)
    {
        aval = (aval * 16807) % 2147483647;
        bval = (bval * 48271) % 2147483647;
        if((aval & 0xFFFF) == (bval & 0xFFFF))
        {
            seen++;
        }
    }
    printf("seen %d matches\n", seen);
}
