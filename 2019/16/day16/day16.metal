//
//  day16.metal
//  day16
//
//  Created by Rick Benua on 12/16/19.
//  Copyright © 2019 Rick Benua. All rights reserved.
//

#include <metal_stdlib>
using namespace metal;

constant int seq[] = {0, 1, 0, -1};
int sequence(uint write_idx, uint read_idx)
{
    return seq[((read_idx + 1) / (write_idx + 1)) % 4];
}
struct bounds{
    uint32_t len;
    uint32_t start;
};

kernel void compute(device uint8_t *input [[buffer(0)]],
                    device uint8_t *output [[buffer(1)]],
                    uint32_t write_idx [[thread_position_in_grid]],
                    constant struct bounds *bounds [[buffer(2)]])
{
    int result = 0;
    write_idx += bounds->start;
    for(uint32_t read_idx = write_idx; read_idx < bounds->len; read_idx++)
    {
        result += (int)input[read_idx] * sequence(write_idx, read_idx);
    }
    
    uint8_t res8 = abs(result) % 10;
    output[write_idx] = res8;
}
