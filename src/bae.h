#pragma once

#include "types.h"

class Position;

namespace Eval {

#ifdef EVAL_TUNING
float update_gradient(const Position& pos, Value targetValue, float learning_rate);
void writeBaeParams();
#endif

Value evaluate(const Position& pos);

}
