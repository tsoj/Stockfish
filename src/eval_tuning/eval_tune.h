#pragma once

#include <cmath>
#include <random>
#include <chrono>

#include "binpack_format.h"
#include "../bae.h"


inline void eval_tune() {

    constexpr size_t  positionBufferSize = 1'000'000;
    constexpr float   errorDecay         = 1.0F / 1000000.0F;
    constexpr int64_t maxSteps           = 300'000'000'000;
    constexpr float   startLr            = 10.0F;
    constexpr float   finalLr            = 0.05F;
    const double      lrDecay            = std::pow<double>(finalLr / startLr, 1.0 / maxSteps);

    // Starting learning rate must be strictly bigger than the final learning rate
    static_assert(startLr > finalLr);
    // lrDecay should be smaller than one if the learning rate should decrease
    assert(finalLr == startLr || lrDecay < 1.0F);

    int64_t currentStep = 0;

    while (currentStep < maxSteps)
    {

        binpack::CompressedTrainingDataEntryReader reader(
          "test77-dec2021-16tb7p.no-db.min.binpack");
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


        const auto startTime = std::chrono::steady_clock::now();
        float      error     = 1.0;
        double     lr        = startLr;

        std::cout << std::endl;
        while (reader.hasNext())
        {
            currentStep += 1;
            const size_t index = uniformDist(e1);

            const float currentError = Eval::update_gradient(
              positionBuffer.at(index).first, static_cast<Value>(positionBuffer.at(index).second),
              static_cast<float>(lr));

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
                    break;
            }

            const auto data                 = reader.next();
            positionBuffer.at(index).first  = Eval::toEvalPosition(data.pos);
            positionBuffer.at(index).second = static_cast<Value>(data.score);
        }
        std::cout << std::endl;
        Eval::writeBaeParams();

        std::cout << "Finished Epoch :D" << std::endl;
    }
    std::cout << "Finished everything :D" << std::endl;
    // TODO(tsoj) write bae params to file
}
