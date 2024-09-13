
#include <array>
#include <cassert>
#include <vector>

#include "bae.h"
#include "bitboard.h"
#include "position.h"

namespace {

enum class Phase : size_t {
    opening = 0,
    endgame = 1
};

enum Relativity : size_t {
    relativeToUs    = 0,
    relativeToEnemy = 1
};
struct BaeParamsSinglePhase {
    // clang-format off
    std::array<std::array<std::array<std::array<std::array<std::array<std::array<Value, 64>, 6>, 64>, 6> , 2> , 4>, 2> pieceRelativePst;
    std::array<std::array<Value, 19683>, 64> pawnStructureBonus;
    std::array<Value, 59049> pieceComboBonus;//*: array[3*3*3*3*3 * 3*3*3*3*3, ValueType]
    // clang-format on
};
class BaeParams {
    std::vector<BaeParamsSinglePhase> params = std::vector<BaeParamsSinglePhase>(2);

   public:
    BaeParamsSinglePhase& operator[](const Phase phase) {
        return params[static_cast<size_t>(phase)];
    }
    const BaeParamsSinglePhase& operator[](const Phase phase) const {
        return params[static_cast<size_t>(phase)];
    }
};

const BaeParams baeParams = {};  // TODO(tsoj) initialize

class EvalState {
    std::array<Value, 2> value = {VALUE_ZERO, VALUE_ZERO};

   public:
    Value& operator[](const Phase phase) { return value[static_cast<size_t>(phase)]; }
};

#define ADD_VALUE(evalState, goodFor, param) \
    for (Phase phase : {Phase::opening, Phase::endgame}) \
    { \
        Value value = baeParams[phase].param; \
        if constexpr ((goodFor) == BLACK) \
        { \
            value = -value; \
        } \
        (*(evalState))[phase] += value; \
    }


void evaluatePieceTypeFromWhitesPerspective(const Position& pos, EvalState* evalState) {}

void evaluate3x3PawnStructureFromWhitesPerspective(const Position& pos, EvalState* evalState) {}

size_t pieceComboIndex(const Position& pos) {
    size_t result  = 0;
    size_t counter = 1;
    for (const Color color : {WHITE, BLACK})
    {
        for (const PieceType piece : {PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING})
        {
            const size_t pieceCount = std::min(2, popcount(pos.pieces(color, piece)));
            result += pieceCount * counter;
            counter *= 3;
        }
    }
    return result;
}

void pieceComboBonusWhitePerspective(const Position& pos, EvalState* evalState) {
    if (std::max(popcount(pos.pieces(WHITE, PAWN)), popcount(pos.pieces(BLACK, PAWN))) <= 2)
    {
        const size_t index = 0;  //TODO(tsoj)
        ADD_VALUE(evalState, WHITE, pieceComboBonus[index]);
    }
}

void absolute_evaluate(const Position& pos, EvalState* evalState) {
    evaluatePieceTypeFromWhitesPerspective(pos, evalState);
    evaluate3x3PawnStructureFromWhitesPerspective(pos, evalState);
    pieceComboBonusWhitePerspective(pos, evalState);
}

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
