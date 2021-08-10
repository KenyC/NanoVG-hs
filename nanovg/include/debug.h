#ifndef DEBUG_H
#define DEBUG_H


#ifndef RELEASE
#   ifndef SHUNT_LOG
#      define DEBUG_LOG
#   endif
#   define DEBUG_FUNCTIONS
#endif

#ifdef DEBUG_LOG
#   include <stdio.h>
#   define LOG(...) printf(__VA_ARGS__);
#else
#   define LOG(...);
#endif

#ifdef DEBUG_FUNCTIONS
#   include <assert.h>
#   include <stdlib.h>
#   define DEBUG(INSTRUCTION) INSTRUCTION 
#   define EXIT exit(0)
#else
#   define assert(...)
#   define DEBUG(INSTRUCTION) 
#   define EXIT
#endif


/**************************************
UTITITIES
***************************************/

#define LOG_BREAK LOG("\n");


#endif
