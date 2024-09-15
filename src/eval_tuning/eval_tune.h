#pragma once

#include "binpack_format.h"


inline void eval_tune() {
    binpack::CompressedTrainingDataEntryReader reader("hello");
    while (reader.hasNext())
    {
        auto e = reader.next();
        std::cout << e.pos.fen() << std::endl;
    }
}
