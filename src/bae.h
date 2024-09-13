#pragma once

#include "bitboard.h"
#include "types.h"

class Position;

namespace Eval {

Value evaluate_bae(const Position& pos);

}
