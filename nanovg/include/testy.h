#ifndef TEST_H
#define TEST_H

int testy_fun();

struct TestStruct
{
	int a;
};
typedef struct TestStruct TestStruct;
TestStruct* init_test_struct();

#endif