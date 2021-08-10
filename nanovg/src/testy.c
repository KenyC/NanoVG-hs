#include <stdlib.h>
#include "testy.h"




int testy_fun() {
	int a = 148;
	return a;
}

TestStruct* init_test_struct() {
	return malloc(sizeof(TestStruct));
}