// Copyright (c) Huawei Technologies Co., Ltd. 2025. All rights reserved.
// This source file is part of the Cangjie project, licensed under Apache-2.0
// with Runtime Library Exception.
//
// See https://cangjie-lang.cn/pages/LICENSE for license information.
#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct TestStruct
{
    int64_t a;
    int64_t b;
};

struct TestStruct* createStruct(int64_t i, int64_t j)
{
    struct TestStruct* s = malloc(sizeof(struct TestStruct));
	
    s->a = i;
    s->b = j;

    return s;
}

int64_t* createPrim(int64_t i)
{
  int64_t* ptr = malloc(sizeof(int64_t) * 2);
  ptr[0] = i;
  ptr[1] = i + i;
  return ptr;
}	
