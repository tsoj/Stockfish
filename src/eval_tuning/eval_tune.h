#pragma once

#include <cmath>
#include <random>

#include "binpack_format.h"
#include "../bae.h"


inline void eval_tune() {

    constexpr size_t  positionBufferSize = 1'000'000;
    constexpr float   errorDecay         = 1.0F / 1000000.0F;
    constexpr int64_t steps              = 20'000'000;  //10'000'000'000;
    constexpr float   startLr            = 10.0F;
    constexpr float   finalLr            = 0.05F;
    const double      lrDecay            = std::pow<double>(finalLr / startLr, 1.0 / steps);

    // Starting learning rate must be strictly bigger than the final learning rate
    static_assert(startLr > finalLr);
    // lrDecay should be smaller than one if the learning rate should decrease
    assert(finalLr == startLr || lrDecay < 1.0F);

    // TODO(tsoj) use alwaysAssert everywhere were it makes sense

    binpack::CompressedTrainingDataEntryReader reader("test77-dec2021-16tb7p.no-db.min.binpack");
    std::vector<std::pair<Eval::EvalPosition, Value>> positionBuffer(positionBufferSize);
    for (auto& entry : positionBuffer)
    {
        assert(reader.hasNext());
        const auto data = reader.next();
        entry.first     = Eval::toEvalPosition(data.pos);
        entry.second    = static_cast<Value>(data.score);
    }

    std::default_random_engine            e1{};
    std::uniform_int_distribution<size_t> uniformDist(0, positionBuffer.size() - 1);


    int64_t i     = 0;
    float   error = 1.0;
    double  lr    = startLr;

    std::cout << std::endl;
    while (reader.hasNext())
    {
        i += 1;
        const size_t index = uniformDist(e1);

        // std::cout << "-------------------------" << std::endl;
        // std::cout << positionBuffer.at(index).pos.fen() << std::endl;
        // std::cout << positionBuffer.at(index).score << std::endl;

        const float currentError = Eval::update_gradient(
          positionBuffer.at(index).first, static_cast<Value>(positionBuffer.at(index).second),
          static_cast<float>(lr));

        error = errorDecay * currentError + (1.0F - errorDecay) * error;
        lr *= lrDecay;

        if ((i % 1'000'000) == 0)
            std::cout << "\r" << i / 1'000'000 << "M/" << steps / 1'000'000 << "M steps, lr: " << lr
                      << ", error: " << error << "                     " << std::flush;

        if (i >= steps)
            break;

        const auto data                 = reader.next();
        positionBuffer.at(index).first  = Eval::toEvalPosition(data.pos);
        positionBuffer.at(index).second = static_cast<Value>(data.score);
    }
    std::cout << std::endl;
    Eval::writeBaeParams();

    std::cout << "Finished :D" << std::endl;
    // TODO(tsoj) write bae params to file
}
