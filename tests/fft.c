#define FFT_WRITE_LANE_0 0x2000
#define FFT_WRITE_LANE_1 0x2008
#define FFT_WRITE_LANE_2 0x2010
#define FFT_WRITE_LANE_3 0x2018

#define FFT_READ_LANE_0 0x2100
#define FFT_READ_LANE_1 0x2108
#define FFT_READ_LANE_2 0x2110
#define FFT_READ_LANE_3 0x2118

#include <stdio.h>

#include "mmio.h"

int main(void)
{
  reg_write32(FFT_WRITE_LANE_0, 524416); // 00001.0000000 00001.0000000
  reg_write32(FFT_WRITE_LANE_1, 524416);
  reg_write32(FFT_WRITE_LANE_2, 524416);
  reg_write32(FFT_WRITE_LANE_3, 524416);
  return 0;
}
