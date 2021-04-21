/* Useful macros */

#define ASSERTED(x) (x##_assertions > 0)
#define RETRACTED(x) (x##_assertions == 0)
#define ASSERT(x) x##_assertions = x##_assertions + 1
#define RETRACT(x) x##_assertions = x##_assertions - 1
#define SEND(x) x##_messages = x##_messages + 1

/* Rest of Program */

