#pragma once

#include "binpack_format.h"
#include "../bae.h"


inline void eval_tune() {
    binpack::CompressedTrainingDataEntryReader reader("test77-dec2021-16tb7p.no-db.min.binpack");

    int64_t     i     = 0;
    float       error = 1.0;
    const float decay = 1.0 / 1000000.0;

    std::cout << std::endl;
    while (reader.hasNext())
    {
        i += 1;
        auto e = reader.next();
        // std::cout << e.pos.fen() << std::endl;

        const float currentError =
          Eval::update_gradient(e.pos, static_cast<Value>(e.score), 0.0001F);

        error = decay * currentError + (1.0 - decay) * error;

        if ((i % 1'000'000) == 0)
            std::cout << "\r" << i / 1'000'000 << "M: " << error << "                     "
                      << std::flush;

        if (i >= 100)
            break;
    }
    std::cout << std::endl;
}
