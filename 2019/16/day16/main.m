//
//  main.m
//  day16
//
//  Created by Rick Benua on 12/16/19.
//  Copyright © 2019 Rick Benua. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <Metal/Metal.h>

char *real_input = "59758034323742284979562302567188059299994912382665665642838883745982029056376663436508823581366924333715600017551568562558429576180672045533950505975691099771937719816036746551442321193912312169741318691856211013074397344457854784758130321667776862471401531789634126843370279186945621597012426944937230330233464053506510141241904155782847336539673866875764558260690223994721394144728780319578298145328345914839568238002359693873874318334948461885586664697152894541318898569630928429305464745641599948619110150923544454316910363268172732923554361048379061622935009089396894630658539536284162963303290768551107950942989042863293547237058600513191659935";
char *test_input1 = "80871224585914546619083218645595"; // expected part1 output: 24176176
char *test_input2 = "03036732577212944063491565474664"; // expected part2 output: 84462026
char *short_input = "12345678";

struct bounds{
    uint32_t len;
    uint32_t start;
};

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        char *input = real_input;
        NSError *error = NULL;
        
        // set up metal device and compute pipeline
        id<MTLDevice> device = MTLCreateSystemDefaultDevice();
        id<MTLCommandQueue> queue = [device newCommandQueue];
        id<MTLLibrary> lib = [device newDefaultLibrary];
        id<MTLFunction> compute_func = [lib newFunctionWithName:@"compute"];
        id<MTLComputePipelineState> state = [device newComputePipelineStateWithFunction:compute_func error:&error];
        if(!state)
        {
            NSLog(@"Error creating state %@", error);
            return -1;
        }
        
        // set up input + output buffers
        NSUInteger len = strlen(input);
        id<MTLBuffer> in_buf = [device newBufferWithLength:len * 10000 options:MTLResourceStorageModeManaged];
        id<MTLBuffer> out_buf = [device newBufferWithLength:len * 10000 options:MTLResourceStorageModeManaged];
        id<MTLBuffer> bounds_buf = [device newBufferWithLength:sizeof(struct bounds) options:MTLResourceStorageModeShared];
        struct bounds *bounds = bounds_buf.contents;
        bounds->len = (uint32_t)len * 10000;
        bounds->start = 0;
        uint8_t *inp = in_buf.contents;
        for(int i = 0; i < len; i++)
        {
            inp[i] = input[i] - '0';
        }
        for(int i = 1; i < 10000; i++)
        {
            memcpy(inp + (len * i), inp, len);
        }
        // copy input to GPU
        [in_buf didModifyRange:NSMakeRange(0, len * 10000)];
        
        // find result offset
        int offset = 0;
        for(int i = 0; i < 7; i++)
        {
            offset = offset * 10 + inp[i];
        }
        NSLog(@"Offset is %d", offset);
        bounds->start = offset;
        // encode 100 compute commands, flip buffers each time, final result will actually be back in in_buf
        id<MTLCommandBuffer> cmdbuffer;
        MTLSize size = MTLSizeMake(len * 10000 - offset, 1, 1);
        MTLSize groupSize = MTLSizeMake(256, 1, 1);
        for(int i = 0; i < 50; i++)
        {
            cmdbuffer = [queue commandBuffer];
            id<MTLComputeCommandEncoder> enc = [cmdbuffer computeCommandEncoder];
            [enc setComputePipelineState:state];
            [enc setBuffer:in_buf offset:0 atIndex:0];
            [enc setBuffer:out_buf offset:0 atIndex:1];
            [enc setBuffer:bounds_buf offset:0 atIndex:2];
            [enc dispatchThreads:size threadsPerThreadgroup:groupSize];
            [enc endEncoding];
            // this could be prettier.
            enc = [cmdbuffer computeCommandEncoder];
            [enc setComputePipelineState:state];
            [enc setBuffer:in_buf offset:0 atIndex:1];
            [enc setBuffer:out_buf offset:0 atIndex:0];
            [enc setBuffer:bounds_buf offset:0 atIndex:2];
            [enc dispatchThreads:size threadsPerThreadgroup:groupSize];
            [enc endEncoding];
            [cmdbuffer addCompletedHandler:^(id<MTLCommandBuffer> cb){
                NSLog(@"Finished %d", i);
            }];
            [cmdbuffer commit];
            [cmdbuffer waitUntilCompleted]; // give AMD's scheduler a chance to do UI work :|
        }
        // Synchronize results to CPU buffer
        cmdbuffer = [queue commandBuffer];
        id<MTLBlitCommandEncoder> enc = [cmdbuffer blitCommandEncoder];
        [enc synchronizeResource:in_buf];
        [enc synchronizeResource:out_buf];
        [enc endEncoding];
        
        [cmdbuffer commit];
        [cmdbuffer waitUntilCompleted];
        
        inp = in_buf.contents;
        char result[9] = {0};
        for(int i = 0; i < 8; i++)
        {
            result[i] = '0' + inp[offset + i];
        }
        NSLog(@"Part 2 result: %s", result);
    }
    return 0;
}
