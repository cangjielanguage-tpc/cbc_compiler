// Copyright (c) Huawei Technologies Co., Ltd. 2025. All rights reserved.
// This source file is part of the Cangjie project, licensed under Apache-2.0
// with Runtime Library Exception.
//
// See https://cangjie-lang.cn/pages/LICENSE for license information.

#include <stdint.h>
#include <stdlib.h>

struct TestStruct {
    int64_t a;
    int64_t b;
};

struct TestStruct* createStruct(int64_t a, int64_t b)
{
    struct TestStruct* value = malloc(sizeof(struct TestStruct));
    value->a = a;
    value->b = b;
    return value;
}

int64_t* createPrim(int64_t value)
{
    int64_t* values = malloc(sizeof(int64_t) * 2);
    values[0] = value;
    values[1] = value * 2;
    return values;
}
