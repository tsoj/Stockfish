#pragma once

#include "binpack_format.h"


inline void eval_tune() {
    binpack::CompressedTrainingDataEntryReader reader("test77-dec2021-16tb7p.no-db.min.binpack");
    int64_t                                    i = 0;
    while (reader.hasNext())
    {
        i += 1;
        auto e = reader.next();
        // std::cout << e.pos.fen() << std::endl;
        if ((i % 10'000'000) == 0)
            std::cout << i / 1'000'000 << "M" << std::endl;
    }
}
