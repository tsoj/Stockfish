
#include <array>
#include <cassert>

#include "bae.h"
#include "bitboard.h"
#include "position.h"

namespace {
enum class Phase : size_t {
    opening = 0,
    endgame = 1
};

class EvalState {
    std::array<Value, 2> value = {VALUE_ZERO, VALUE_ZERO};

   public:
    Value& operator[](const Phase phase) { return value[static_cast<size_t>(phase)]; }
};

void absolute_evaluate(const Position& pos, EvalState* evalState) {}

Value absolute_evaluate(const Position& pos) {
    EvalState evalState{};
    absolute_evaluate(pos, &evalState);
    const int   phase = popcount(pos.pieces());
    const Value result =
      (evalState[Phase::opening] * phase + evalState[Phase::endgame] * (32 - phase)) / 32;
    assert(abs(result) < VALUE_KNOWN_WIN);
    return result;
}

}  // namespace

Value Eval::evaluate_bae(const Position& pos) {
    Value result = absolute_evaluate(pos);
    if (pos.side_to_move() == BLACK)
    {
        result = -result;
    }
    return result;
}
