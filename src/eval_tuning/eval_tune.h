#pragma once

#include "binpack_format.h"


inline void eval_tune() {
    binpack::CompressedTrainingDataEntryReader reader("test77-dec2021-16tb7p.no-db.min.binpack");
    while (reader.hasNext())
    {
        auto e = reader.next();
        std::cout << e.pos.fen() << std::endl;
    }
}
