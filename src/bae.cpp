
#include <array>
#include <cassert>
#include <initializer_list>
#include <vector>
#include <iostream>
#include <map>


#include "bae.h"
#include "bitboard.h"
#include "position.h"
#include "types.h"
#include "bae_params.h"

namespace {


const std::map<PieceType, std::string> pieceToString = {{PAWN, "pawn"},     {KNIGHT, "knight"},
                                                        {BISHOP, "bishop"}, {ROOK, "rook"},
                                                        {QUEEN, "queen"},   {KING, "king"}};

const std::map<Color, std::string> colorToString = {{WHITE, "white"}, {BLACK, "black"}};

const std::map<Square, std::string> squareToString = {
  {SQ_A1, "a1"}, {SQ_B1, "b1"}, {SQ_C1, "c1"}, {SQ_D1, "d1"}, {SQ_E1, "e1"}, {SQ_F1, "f1"},
  {SQ_G1, "g1"}, {SQ_H1, "h1"}, {SQ_A2, "a2"}, {SQ_B2, "b2"}, {SQ_C2, "c2"}, {SQ_D2, "d2"},
  {SQ_E2, "e2"}, {SQ_F2, "f2"}, {SQ_G2, "g2"}, {SQ_H2, "h2"}, {SQ_A3, "a3"}, {SQ_B3, "b3"},
  {SQ_C3, "c3"}, {SQ_D3, "d3"}, {SQ_E3, "e3"}, {SQ_F3, "f3"}, {SQ_G3, "g3"}, {SQ_H3, "h3"},
  {SQ_A4, "a4"}, {SQ_B4, "b4"}, {SQ_C4, "c4"}, {SQ_D4, "d4"}, {SQ_E4, "e4"}, {SQ_F4, "f4"},
  {SQ_G4, "g4"}, {SQ_H4, "h4"}, {SQ_A5, "a5"}, {SQ_B5, "b5"}, {SQ_C5, "c5"}, {SQ_D5, "d5"},
  {SQ_E5, "e5"}, {SQ_F5, "f5"}, {SQ_G5, "g5"}, {SQ_H5, "h5"}, {SQ_A6, "a6"}, {SQ_B6, "b6"},
  {SQ_C6, "c6"}, {SQ_D6, "d6"}, {SQ_E6, "e6"}, {SQ_F6, "f6"}, {SQ_G6, "g6"}, {SQ_H6, "h6"},
  {SQ_A7, "a7"}, {SQ_B7, "b7"}, {SQ_C7, "c7"}, {SQ_D7, "d7"}, {SQ_E7, "e7"}, {SQ_F7, "f7"},
  {SQ_G7, "g7"}, {SQ_H7, "h7"}, {SQ_A8, "a8"}, {SQ_B8, "b8"}, {SQ_C8, "c8"}, {SQ_D8, "d8"},
  {SQ_E8, "e8"}, {SQ_F8, "f8"}, {SQ_G8, "g8"}, {SQ_H8, "h8"}};

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
    std::array<std::array<Value, 19683>, 30> pawnStructureBonus;
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

const BaeParams baeParams = []() {
    BaeParams baeParams{};
    size_t    n = 0;

    const auto nextValue = [&n]() {
        constexpr size_t charWidth = 8;

        int16_t bits = 0;

        for (size_t i = 0; i < sizeof(int16_t); ++i)
        {
            const size_t shift = charWidth * i;
            assert(sizeof(Eval::rawBaeContent) > n + 1);
            const char hexString[] = {Eval::rawBaeContent[n], Eval::rawBaeContent[n + 1], '\0'};
            // const char   hexString[] = {'0', 'F', '\0'};
            uint16_t tmp = std::strtol(hexString, nullptr, 16);
            // std::cout << "a: " << tmp << std::endl;
            tmp <<= shift;
            bits |= *reinterpret_cast<int16_t*>(&tmp);
            n += 2;
        }

        const Value result = static_cast<Value>(bits);
        // std::cout << result << std::endl;
        // if (n >= 6162*2)
        // {
        //     exit(1);
        // }
        return result;
    };
    for (Phase phase : {Phase::opening, Phase::endgame})
    {
        // clang-format off
        // for (size_t a = 0; a < 2; ++a)
        // for (size_t b = 0; b < 4; ++b)
        // for (size_t c = 0; c < 2; ++c)
        // for (size_t d = 0; d < 6; ++d)
        // for (size_t e = 0; e < 64; ++e)
        // for (size_t f = 0; f < 6; ++f)
        // for (size_t g = 0; g < 64; ++g)
        // {
        //     baeParams[phase].pieceRelativePst.at(a).at(b).at(c).at(d).at(e).at(f).at(g) = nextValue();
        // }
        // clang-format on

        // clang-format off
        for(auto& a : baeParams[phase].pieceRelativePst)
        for(auto& b : a)
        for(auto& c : b)
        for(auto& d : c)
        for(auto& e : d)
        for(auto& f : e)
        for(auto& g : f)
        {
            g = nextValue();
        }

        for(auto& a : baeParams[phase].pawnStructureBonus)
        for(auto& b : a)
        {
            b = nextValue();
        }

        for(auto& a : baeParams[phase].pieceComboBonus)
        {
            a = nextValue();
        }
        // clang-format on
    }

    return baeParams;
}();  // TODO(tsoj) initialize

class EvalState {
    std::array<Value, 2> value = {VALUE_ZERO, VALUE_ZERO};

   public:
    Value& operator[](const Phase phase) { return value[static_cast<size_t>(phase)]; }
};


// std::string s = #param; \
// s.erase(std::remove(s.begin(), s.end(), '\n'), s.cend()); \
// std::cout << s << ": " << value << std::endl; \

#define ADD_VALUE(evalState, goodFor, param) \
    for (Phase phase : {Phase::opening, Phase::endgame}) \
    { \
        Value value = baeParams[phase].param; \
        if ((goodFor) == BLACK) \
        { \
            value = -value; \
        } \
        (*(evalState))[phase] += value; \
    }

Square colorConditionalMirrorVertically(const Square square, const Color color) {
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

template<PieceType ourPiece>
void pieceRelativePst(const Position&  pos,
                      EvalState* const evalState,
                      const Square     ourSquareIn,
                      const Color      us) {

    const Square ourSquare       = colorConditionalMirrorVertically(ourSquareIn, us);
    const Square enemyKingSquare = colorConditionalMirrorVertically(lsb(pos.pieces(~us, KING)), us);
    const size_t roughEnemyKingFile = (static_cast<size_t>(enemyKingSquare) % 8) / 2;
    const size_t roughEnemyKingRank = (static_cast<size_t>(enemyKingSquare) / 8) / 4;


    FOR_PIECE_RANGE({
        for (const Relativity relativity : {relativeToUs, relativeToEnemy})
        {
            const Square* otherSquares =
              pos.squares<otherPiece>(relativity == relativeToUs ? us : ~us);
            for (Square otherSquareIn = *otherSquares; otherSquareIn != SQ_NONE;
                 otherSquareIn        = *++otherSquares)
            {
                const Square otherSquare = colorConditionalMirrorVertically(otherSquareIn, us);

                ADD_VALUE(
                  evalState, us,
                  pieceRelativePst[roughEnemyKingRank][roughEnemyKingFile][relativity]
                                  [ourPiece - PAWN][ourSquare][otherPiece - PAWN][otherSquare])
            }
        }
    })
}

#undef FOR_PIECE_RANGE

template<PieceType piece>
void evaluatePiece(const Position&  pos,
                   EvalState* const evalState,
                   const Square     square,
                   const Color      color) {
    if constexpr (piece == PAWN)
    {
        if (pos.pawn_passed(color, square))
        {
            pieceRelativePst<PAWN>(pos, evalState, square, color);
        }
    }
    else
    {
        pieceRelativePst<piece>(pos, evalState, square, color);
    }
}

template<PieceType piece>
void evaluatePieceTypeFromWhitesPerspective(const Position& pos, EvalState* const evalState) {

    for (const Color color : {WHITE, BLACK})
    {
        const Square* squares = pos.squares<piece>(color);
        for (Square square = *squares; square != SQ_NONE; square = *++squares)
        {
            // std::cout << "Evaluating piece " << pieceToString.at(piece) << " of color "
            //           << colorToString.at(color) << " at " << squareToString.at(square)
            //           << std::endl;
            evaluatePiece<piece>(pos, evalState, square, color);
        }
    }
}

void evaluatePieceTypeFromWhitesPerspective(const Position& pos, EvalState* const evalState) {

    evaluatePieceTypeFromWhitesPerspective<PAWN>(pos, evalState);
    evaluatePieceTypeFromWhitesPerspective<KNIGHT>(pos, evalState);
    evaluatePieceTypeFromWhitesPerspective<BISHOP>(pos, evalState);
    evaluatePieceTypeFromWhitesPerspective<ROOK>(pos, evalState);
    evaluatePieceTypeFromWhitesPerspective<QUEEN>(pos, evalState);
    evaluatePieceTypeFromWhitesPerspective<KING>(pos, evalState);
}

size_t pawnMaskIndex(const Position& pos, const Square square) {
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

void evaluate3x3PawnStructureFromWhitesPerspective(const Position&  pos,
                                                   EvalState* const evalState) {
    for (const Square square : {
           SQ_B3, SQ_C3, SQ_D3, SQ_E3, SQ_F3, SQ_G3, SQ_B4, SQ_C4, SQ_D4, SQ_E4, SQ_F4, SQ_G4,
           SQ_B5, SQ_C5, SQ_D5, SQ_E5, SQ_F5, SQ_G5, SQ_B6, SQ_C6, SQ_D6, SQ_E6, SQ_F6, SQ_G6,
         })
    {
        const Bitboard mask3x3 = attacks_bb<KING>(square) | square_bb(square);

        if (popcount(mask3x3 & pos.pieces(PAWN)) >= 2)
        {
            const size_t index = pawnMaskIndex(pos, square);
            assert(index < 19683);
            ADD_VALUE(evalState, WHITE, pawnStructureBonus[square - SQ_B3][index]);
        }
    }
}

size_t pieceComboIndex(const Position& pos) {
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

void pieceComboBonusWhitePerspective(const Position& pos, EvalState* const evalState) {
    if (std::max(popcount(pos.pieces(WHITE, PAWN)), popcount(pos.pieces(BLACK, PAWN))) <= 2)
    {
        const size_t index = pieceComboIndex(pos);

        // if(index >= 59049)
        // {
        //     std::cout << pos.fen() << std::endl;
        // }
        assert(index < 59049);
        ADD_VALUE(evalState, WHITE, pieceComboBonus[index]);
    }
}

#undef ADD_VALUE

void absolute_evaluate(const Position& pos, EvalState* const evalState) {
    evaluatePieceTypeFromWhitesPerspective(pos, evalState);
    evaluate3x3PawnStructureFromWhitesPerspective(pos, evalState);
    pieceComboBonusWhitePerspective(pos, evalState);
}

Value absolute_evaluate(const Position& pos) {
    EvalState evalState{};
    absolute_evaluate(pos, &evalState);
    const int phase = popcount(pos.pieces());
    Value     result =
      (evalState[Phase::opening] * phase + evalState[Phase::endgame] * (32 - phase)) / 32;
    // result /= 2;
    // std::cout << result << std::endl;
    result *= 12;
    result /= 10;
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
