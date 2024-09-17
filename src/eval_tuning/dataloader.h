#pragma once

#include <filesystem>
#include <cmath>
#include <random>
#include <chrono>
#include <optional>

#include "../bae.h"
#include "binpack_format.h"

struct BufferEntry {
    Eval::EvalPosition pos;
    Value              score;
};

// class Dataloader{}

template<typename T>
concept Datareader = requires(T t, std::filesystem::path path) {
    { T(path) };
    { t.next() } -> std::same_as<std::optional<BufferEntry>>;
};

class BinpackDatareader {
   public:
    explicit BinpackDatareader(const std::filesystem::path& path) :
        m_reader(path.string()) {}

    std::optional<BufferEntry> next() {
        while (m_reader.hasNext())
        {
            const auto e = m_reader.next();
            if (e.isInCheck() || e.isCapturingMove() || std::abs(e.score) > 10'000)
            {
                continue;
            }

            return BufferEntry{Eval::toEvalPosition(e.pos), static_cast<Value>(e.score)};
        }
        return std::nullopt;
    }

   private:
    binpack::CompressedTrainingDataEntryReader m_reader;
};


template<Datareader Datareader>
class Dataloader {
   public:
    explicit Dataloader(const std::filesystem::path& path, size_t bufferSize = 1'000'000) :
        m_path(path),
        m_buffer(bufferSize),
        m_uniformDist(0, bufferSize - 1) {}

    BufferEntry next() {
        if (!m_reader.has_value())
        {
            m_reader = Datareader(m_path.string());
            assert(!m_buffer.empty());
            for (auto& entry : m_buffer)
            {
                entry = m_reader->next().value();
            }
        }

        const size_t index  = m_uniformDist(m_e1);
        BufferEntry  result = m_buffer.at(index);

        const auto newEntry = m_reader->next();
        if (newEntry.has_value())
        {
            m_buffer.at(index) = newEntry.value();
        }
        else
        {
            std::cout << "Finished epoch " << m_epoch << std::endl;
            m_reader = std::nullopt;
            m_epoch += 1;
        }
        return result;
    }

   private:
    std::filesystem::path                 m_path;
    std::vector<BufferEntry>              m_buffer;
    std::optional<Datareader>             m_reader{std::nullopt};
    std::default_random_engine            m_e1;
    std::uniform_int_distribution<size_t> m_uniformDist;
    size_t                                m_epoch = 0;
};

using BinpackDataloader = Dataloader<BinpackDatareader>;
