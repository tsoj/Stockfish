#pragma once

#include <cmath>
#include <random>
#include <chrono>
#include <optional>

#include "binpack_format.h"
#include "../bae.h"

struct BufferEntry {
    Eval::EvalPosition pos;
    Value              score;
};

inline void eval_tune() {

    constexpr size_t  positionBufferSize = 1'000'000;
    constexpr float   errorDecay         = 1.0F / 10'000'000.0F;
    constexpr int64_t maxSteps           = 20'000'000'000;
    constexpr float   startLr            = 1.0F;
    constexpr float   finalLr            = 0.001F;
    const double      lrDecay            = std::pow<double>(finalLr / startLr, 1.0 / maxSteps);

    // Starting learning rate must be strictly bigger than the final learning rate
    static_assert(startLr > finalLr);
    // lrDecay should be smaller than one if the learning rate should decrease
    assert(finalLr == startLr || lrDecay < 1.0F);

    int64_t currentStep = 0;

    for (int epoch = 0; currentStep < maxSteps; ++epoch)
    {

        binpack::CompressedTrainingDataEntryReader reader(
          "test77-dec2021-16tb7p.no-db.min.binpack");

        const auto getNextEntry = [&reader]() -> std::optional<BufferEntry> {
            while (reader.hasNext())
            {
                const auto e = reader.next();
                if (e.isInCheck() || e.isCapturingMove() || std::abs(e.score) > 10'000)
                {
                    continue;
                }

                return BufferEntry{Eval::toEvalPosition(e.pos), static_cast<Value>(e.score)};
            }
            return std::nullopt;
        };

        std::vector<BufferEntry> positionBuffer(positionBufferSize);
        for (auto& entry : positionBuffer)
        {
            assert(reader.hasNext());
            entry = getNextEntry().value();
        }

        std::default_random_engine            e1{};
        std::uniform_int_distribution<size_t> uniformDist(0, positionBuffer.size() - 1);


        const auto startTime = std::chrono::steady_clock::now();
        float      error     = 1.0;
        double     lr        = startLr;

        std::cout << std::endl;
        while (true)
        {
            currentStep += 1;
            const size_t index = uniformDist(e1);

            const float currentError = Eval::update_gradient(
              positionBuffer.at(index).pos, positionBuffer.at(index).score, static_cast<float>(lr));

            error = errorDecay * currentError + (1.0F - errorDecay) * error;
            lr *= lrDecay;

            if ((currentStep % 1'000'000) == 0)
            {
                const auto currentTime = std::chrono::steady_clock::now();
                const auto passedSeconds =
                  std::chrono::duration_cast<std::chrono::seconds>(currentTime - startTime).count();
                const auto estimatedRemainingSeconds =
                  (passedSeconds * maxSteps) / currentStep - passedSeconds;

                std::cout << "\r" << currentStep / 1'000'000 << "M/" << maxSteps / 1'000'000
                          << "M steps, lr: " << lr << ", error: " << error
                          << ", estimated remaining time: " << estimatedRemainingSeconds / 60
                          << " min                     " << std::flush;


                if (currentStep >= maxSteps)
                {
                    break;
                }
            }
            auto nextEntry = getNextEntry();
            if (!nextEntry.has_value())
            {
                break;
            }
            positionBuffer.at(index) = nextEntry.value();
        }
        std::cout << std::endl;
        Eval::writeBaeParams();

        std::cout << "Finished epoch " << epoch << std::endl;
    }
    std::cout << "Finished everything :D" << std::endl;
    // TODO(tsoj) write bae params to file
}
