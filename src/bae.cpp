
#include <array>
#include <cassert>
#include <initializer_list>
#include <type_traits>
#include <vector>
#include <functional>
#include <iostream>

#include "bae.h"
#include "bitboard.h"
#include "position.h"
#include "types.h"
#include "bae_params.h"

namespace {

enum class Phase : size_t {
    opening = 0,
    endgame = 1
};

template<typename ValueType>
struct BaeParamsSinglePhase {
    // clang-format off
    std::array<std::array<std::array<std::array<std::array<std::array<std::array<ValueType, 64>, 6>, 64>, 6> , 2> , 4>, 2> pieceRelativePst;
    std::array<std::array<ValueType, 19683>, 30> pawnStructureBonus;
    std::array<ValueType, 59049> pieceComboBonus;
    // clang-format on
};

template<typename ValueType>
class BaeParams {
    std::vector<BaeParamsSinglePhase<ValueType>> params =
      std::vector<BaeParamsSinglePhase<ValueType>>(2);

   public:
    BaeParamsSinglePhase<ValueType>& operator[](const Phase phase) {
        return params[static_cast<size_t>(phase)];
    }
    const BaeParamsSinglePhase<ValueType>& operator[](const Phase phase) const {
        return params[static_cast<size_t>(phase)];
    }


    void doForAll(const std::function<void(ValueType&)>& op) {
        for (auto& singlePhase : params)
        {
            // clang-format off
            for(auto& a : singlePhase.pieceRelativePst)
            for(auto& b : a)
            for(auto& c : b)
            for(auto& d : c)
            for(auto& e : d)
            for(auto& f : e)
            for(auto& g : f)
            {
                op(g);
            }

            for(auto& a : singlePhase.pawnStructureBonus)
            for(auto& b : a)
            {
                op(b);
            }

            for(auto& a : singlePhase.pieceComboBonus)
            {
                op(a);
            }
            // clang-format on
        }
    }
};


#ifdef EVAL_TUNING
BaeParams<float> baeParams = []() {
    BaeParams<F> baeParams{};
    std::memset(&baeParams[Phase::opening], 0, sizeof(baeParams[Phase::opening]));
    std::memset(&baeParams[Phase::endgame], 0, sizeof(baeParams[Phase::endgame]));
    return baeParams;
}();
#else
const BaeParams<Value> baeParams = []() {
    BaeParams<Value> baeParams{};

    size_t n = 0;

    baeParams.doForAll([&](Value& value) {
        constexpr size_t charWidth = 8;

        int16_t bits = 0;

        for (size_t i = 0; i < sizeof(int16_t); ++i)
        {
            const size_t shift = charWidth * i;
            assert(sizeof(Eval::rawBaeContent) > n + 1);
            const char hexString[] = {Eval::rawBaeContent[n], Eval::rawBaeContent[n + 1], '\0'};
            uint16_t   tmp         = std::strtol(hexString, nullptr, 16);
            tmp <<= shift;
            bits |= *reinterpret_cast<int16_t*>(&tmp);
            n += 2;
        }

        value = static_cast<Value>(bits);
    });

    return baeParams;
}();
#endif

class EvalValue {
    std::array<Value, 2> value = {VALUE_ZERO, VALUE_ZERO};

   public:
    Value& operator[](const Phase phase) { return value[static_cast<size_t>(phase)]; }
};


struct EvalGradient {
    float g;
    float gamePhaseFactor;
};

template<typename T>
concept EvalState = std::same_as<T, EvalValue> || std::same_as<T, EvalGradient>;

#define ADD_VALUE(evalState, goodFor, param) \
    if constexpr (std::is_same_v<EvalGradient, std::remove_cvref_t<decltype(*evalState)>>) \
    { \
        const float f = ((goodFor) == BLACK ? -1.0F : 1.0F) * evalState->g; \
        baeParams[Phase::opening].param += f * evalState->gamePhaseFactor; \
        baeParams[Phase::endgame].param += f * (1.0F - evalState->gamePhaseFactor); \
        if (abs(baeParams[Phase::opening].param) >= 0.1) \
        { \
            std::cout << "baeParams[Phase::opening].param: " << #param << "; " \
                      << baeParams[Phase::opening].param << std::endl; \
        } \
    } \
    else \
    { \
        for (Phase phase : {Phase::opening, Phase::endgame}) \
        { \
            Value value = static_cast<Value>(baeParams[phase].param); \
            if constexpr ((goodFor) == BLACK) \
            { \
                value = -value; \
            } \
            (*(evalState))[phase] += value; \
        } \
    }

Square color_conditional_mirror_vertically(const Square square, const Color color) {
    if (color == BLACK)
    {
        return flip_rank(square);
    }
    return square;
}

#define FOR_PIECE_RANGE(body) \
    if constexpr (ourPiece == PAWN || ourPiece == KING) \
    { \
        { \
            constexpr PieceType otherPiece = PAWN; \
            body \
        } \
        { \
            constexpr PieceType otherPiece = KNIGHT; \
            body \
        } \
        { \
            constexpr PieceType otherPiece = BISHOP; \
            body \
        } \
        { \
            constexpr PieceType otherPiece = ROOK; \
            body \
        } \
        { \
            constexpr PieceType otherPiece = QUEEN; \
            body \
        } \
        { \
            constexpr PieceType otherPiece = KING; \
            body \
        } \
    } \
    if constexpr (ourPiece == QUEEN) \
    { \
        { \
            constexpr PieceType otherPiece = PAWN; \
            body \
        } \
        { \
            constexpr PieceType otherPiece = KNIGHT; \
            body \
        } \
        { \
            constexpr PieceType otherPiece = BISHOP; \
            body \
        } \
        { \
            constexpr PieceType otherPiece = ROOK; \
            body \
        } \
        { \
            constexpr PieceType otherPiece = QUEEN; \
            body \
        } \
    } \
    if constexpr (ourPiece == KNIGHT) \
    { \
        { \
            constexpr PieceType otherPiece = PAWN; \
            body \
        } \
        { \
            constexpr PieceType otherPiece = KNIGHT; \
            body \
        } \
        { \
            constexpr PieceType otherPiece = BISHOP; \
            body \
        } \
        { \
            constexpr PieceType otherPiece = ROOK; \
            body \
        } \
    } \
    if constexpr (ourPiece == BISHOP) \
    { \
        { \
            constexpr PieceType otherPiece = PAWN; \
            body \
        } \
        { \
            constexpr PieceType otherPiece = BISHOP; \
            body \
        } \
        { \
            constexpr PieceType otherPiece = ROOK; \
            body \
        } \
    } \
    if constexpr (ourPiece == ROOK) \
    { \
        { \
            constexpr PieceType otherPiece = PAWN; \
            body \
        } \
        { \
            constexpr PieceType otherPiece = ROOK; \
            body \
        } \
    }

template<PieceType ourPiece, Color us, EvalState EvalState>
void piece_relative_pst(const Position& pos, EvalState* const evalState, const Square ourSquareIn) {

    const Square ourSquare = color_conditional_mirror_vertically(ourSquareIn, us);
    const Square enemyKingSquare =
      color_conditional_mirror_vertically(lsb(pos.pieces(~us, KING)), us);
    const size_t roughEnemyKingFile = (static_cast<size_t>(enemyKingSquare) % 8) / 2;
    const size_t roughEnemyKingRank = (static_cast<size_t>(enemyKingSquare) / 8) / 4;


    FOR_PIECE_RANGE({
        for (const size_t relativity : {0, 1})
        {
            const Square* otherSquares = pos.squares<otherPiece>(relativity == 0 ? us : ~us);
            for (Square otherSquareIn = *otherSquares; otherSquareIn != SQ_NONE;
                 otherSquareIn        = *++otherSquares)
            {
                const Square otherSquare = color_conditional_mirror_vertically(otherSquareIn, us);

                ADD_VALUE(
                  evalState, us,
                  pieceRelativePst[roughEnemyKingRank][roughEnemyKingFile][relativity]
                                  [ourPiece - PAWN][ourSquare][otherPiece - PAWN][otherSquare])
            }
        }
    })
}

#undef FOR_PIECE_RANGE

template<PieceType piece, Color color, EvalState EvalState>
void evaluate_piece(const Position& pos, EvalState* const evalState, const Square square) {
    if constexpr (piece == PAWN)
    {
        if (pos.pawn_passed(color, square))
        {
            piece_relative_pst<PAWN, color>(pos, evalState, square);
        }
    }
    else
    {
        piece_relative_pst<piece, color>(pos, evalState, square);
    }
}

template<PieceType piece, Color color, EvalState EvalState>
void evaluate_piece_type_from_whites_perspective(const Position& pos, EvalState* const evalState) {

    const Square* squares = pos.squares<piece>(color);
    for (Square square = *squares; square != SQ_NONE; square = *++squares)
    {
        evaluate_piece<piece, color>(pos, evalState, square);
    }
}

template<PieceType piece, EvalState EvalState>
void evaluate_piece_type_from_whites_perspective(const Position& pos, EvalState* const evalState) {

    evaluate_piece_type_from_whites_perspective<piece, WHITE>(pos, evalState);
    evaluate_piece_type_from_whites_perspective<piece, BLACK>(pos, evalState);
}

template<EvalState EvalState>
void evaluate_piece_type_from_whites_perspective(const Position& pos, EvalState* const evalState) {

    evaluate_piece_type_from_whites_perspective<PAWN>(pos, evalState);
    evaluate_piece_type_from_whites_perspective<KNIGHT>(pos, evalState);
    evaluate_piece_type_from_whites_perspective<BISHOP>(pos, evalState);
    evaluate_piece_type_from_whites_perspective<ROOK>(pos, evalState);
    evaluate_piece_type_from_whites_perspective<QUEEN>(pos, evalState);
    evaluate_piece_type_from_whites_perspective<KING>(pos, evalState);
}

size_t pawn_mask_index(const Position& pos, const Square square) {
    const Bitboard whitePawns = pos.pieces(WHITE, PAWN) >> (square - SQ_B2);
    const Bitboard blackPawns = pos.pieces(BLACK, PAWN) >> (square - SQ_B2);

    size_t result  = 0;
    size_t counter = 1;

    for (const Bitboard bit : {
           square_bb(SQ_A3),
           square_bb(SQ_B3),
           square_bb(SQ_C3),
           square_bb(SQ_A2),
           square_bb(SQ_B2),
           square_bb(SQ_C2),
           square_bb(SQ_A1),
           square_bb(SQ_B1),
           square_bb(SQ_C1),
         })
    {
        if ((whitePawns & bit) != 0)
        {
            result += counter * 2;
        }
        else if ((blackPawns & bit) != 0)
        {
            result += counter * 1;
        }

        counter *= 3;
    }

    return result;
}

template<EvalState EvalState>
void evaluate_3x3_pawn_structure_from_whites_perspective(const Position&  pos,
                                                         EvalState* const evalState) {
    for (const Square square : {
           SQ_B3, SQ_C3, SQ_D3, SQ_E3, SQ_F3, SQ_G3, SQ_B4, SQ_C4, SQ_D4, SQ_E4, SQ_F4, SQ_G4,
           SQ_B5, SQ_C5, SQ_D5, SQ_E5, SQ_F5, SQ_G5, SQ_B6, SQ_C6, SQ_D6, SQ_E6, SQ_F6, SQ_G6,
         })
    {
        const Bitboard mask3x3 = attacks_bb<KING>(square) | square_bb(square);

        if (popcount(mask3x3 & pos.pieces(PAWN)) >= 2)
        {
            const size_t index = pawn_mask_index(pos, square);
            assert(index < 19683);
            ADD_VALUE(evalState, WHITE, pawnStructureBonus[square - SQ_B3][index]);
        }
    }
}

size_t piece_combo_index(const Position& pos) {
    size_t result  = 0;
    size_t counter = 1;
    for (const Color color : {WHITE, BLACK})
    {
        for (const PieceType piece : {PAWN, KNIGHT, BISHOP, ROOK, QUEEN})
        {
            const size_t pieceCount = std::min(2, popcount(pos.pieces(color, piece)));
            result += pieceCount * counter;
            // std::cout << "counter: " << counter <<", pieceCount: " << pieceCount << ", result: " << result << std::endl;
            counter *= 3;
        }
    }
    return result;
}

template<EvalState EvalState>
void piece_combo_bonus_white_perspective(const Position& pos, EvalState* const evalState) {
    if (std::max(popcount(pos.pieces(WHITE, PAWN)), popcount(pos.pieces(BLACK, PAWN))) <= 2)
    {
        const size_t index = piece_combo_index(pos);
        assert(index < 59049);
        ADD_VALUE(evalState, WHITE, pieceComboBonus[index]);
    }
}

#undef ADD_VALUE

template<EvalState EvalState>
void absolute_evaluate(const Position& pos, EvalState* const evalState) {
    evaluate_piece_type_from_whites_perspective(pos, evalState);
    evaluate_3x3_pawn_structure_from_whites_perspective(pos, evalState);
    piece_combo_bonus_white_perspective(pos, evalState);
}

Value absolute_evaluate(const Position& pos) {
    EvalValue evalState{};
    absolute_evaluate(pos, &evalState);
    const int phase = popcount(pos.pieces());
    Value     result =
      (evalState[Phase::opening] * phase + evalState[Phase::endgame] * (32 - phase)) / 32;
    result *= 25;  // to make the scaling be closer to what the classical eval does
    result /= 10;
    // std::cout << "result: " << result << std::endl;
    assert(abs(result) < VALUE_KNOWN_WIN);
    return result;
}

#ifdef EVAL_TUNING
float error(const float outcome, const float estimate) {
    return std::pow(outcome - estimate, 2.0F);
}

float errorDerivative(const float outcome, const float estimate) {
    return 2.0F * (outcome - estimate);
}

constexpr float k = 400.0;

float winningProbability(const Value value) {
    return 1.0F / (1.0F + std::pow(10.0F, -((static_cast<float>(value)) / k)));
}

float winningProbabilityDerivative(const Value value) {
    return (std::log(10.0F) * std::pow(2.0F, -2.0F - ((static_cast<float>(value)) / k))
            * std::pow(5.0F, -((static_cast<float>(value)) / k)))
         / std::pow(1.0F + std::pow(10.0F, -((static_cast<float>(value)) / k)), 2.0F);
}
#endif


}  // namespace


#ifdef EVAL_TUNING
float Eval::update_gradient(const Position& pos,
                            const Value     targetValue,
                            const float     learning_rate) {
    const int   phase              = popcount(pos.pieces());
    const Value currentValue       = absolute_evaluate(pos);
    const float targetProbability  = winningProbability(targetValue);
    const float currentProbability = winningProbability(currentValue);
    const float currentError       = error(targetProbability, currentProbability);

    EvalGradient evalState{.g               = static_cast<float>(phase) / 32.0F,
                           .gamePhaseFactor = learning_rate
                                            * errorDerivative(targetProbability, currentProbability)
                                            * winningProbabilityDerivative(currentValue)};

    std::cout << evalState.g << ", " << evalState.gamePhaseFactor << std::endl;

    absolute_evaluate(pos, &evalState);

    return currentError;
}
#endif

Value Eval::evaluate(const Position& pos) {
    Value result = absolute_evaluate(pos);
    if (pos.side_to_move() == BLACK)
    {
        result = -result;
    }
    return result;
}
