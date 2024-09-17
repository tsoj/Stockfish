#pragma once

#include <cmath>
#include <random>
#include <chrono>
#include <optional>

#include "dataloader.h"
#include "../bae.h"

// struct BufferEntry {
//     Eval::EvalPosition pos;
//     Value              score;
// };

inline void eval_tune() {

    constexpr int64_t reportFrequency    = 1'000'000;
    constexpr size_t  positionBufferSize = 1'000'000;
    constexpr int64_t maxSteps           = 20'000'000;
    constexpr float   startLr            = 10.0F;
    constexpr float   finalLr            = 0.001F;
    const double      lrDecay            = std::pow<double>(finalLr / startLr, 1.0 / maxSteps);

    // Starting learning rate must be strictly bigger than the final learning rate
    static_assert(startLr > finalLr);
    // lrDecay should be smaller than one if the learning rate should decrease
    assert(finalLr == startLr || lrDecay < 1.0F);

    BinpackDataloader dataloader("test77-dec2021-16tb7p.no-db.min.binpack", positionBufferSize);

    const auto startTime = std::chrono::steady_clock::now();
    double     lr        = startLr;
    double     errorSum  = 0.0;


    std::cout << std::endl;
    for (int64_t currentStep = 1; currentStep <= maxSteps; ++currentStep)
    {

        const auto [pos, score] = dataloader.next();

        errorSum += Eval::update_gradient(pos, score, static_cast<float>(lr));

        lr *= lrDecay;

        if ((currentStep % reportFrequency) == 0)
        {
            const auto currentTime = std::chrono::steady_clock::now();
            const auto passedSeconds =
              std::chrono::duration_cast<std::chrono::seconds>(currentTime - startTime).count();
            const auto estimatedRemainingSeconds =
              (passedSeconds * maxSteps) / currentStep - passedSeconds;

            std::cout << "\r" << currentStep / 1'000'000 << "M/" << maxSteps / 1'000'000
                      << "M steps, lr: " << lr << ", error: " << errorSum / reportFrequency
                      << ", estimated remaining time: " << estimatedRemainingSeconds / 60
                      << " min                     " << std::flush;

            errorSum = 0.0;
        }
    }
    std::cout << std::endl;
    Eval::writeBaeParams();
    std::cout << "Finished everything :D" << std::endl;
}
