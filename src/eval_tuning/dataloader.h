#pragma once

#include <filesystem>
#include <cmath>
#include <memory>
#include <random>
#include <optional>
#include <iostream>

#include "../bae.h"

struct BufferEntry {
    Eval::EvalPosition pos;
    float              targetProbability;
};

template<typename T>
concept Datareader = requires(T t, std::filesystem::path path) {
    { T(path) };
    { t.next() } -> std::same_as<std::optional<BufferEntry>>;
};

class AbstractDataloader {
   public:
    virtual BufferEntry next() = 0;
};

template<Datareader Datareader>
class Dataloader: public AbstractDataloader {
   public:
    explicit Dataloader(const std::filesystem::path& path, size_t bufferSize = 1'000'000) :
        m_path(path),
        m_buffer(bufferSize),
        m_uniformDist(0, bufferSize - 1) {}

    inline BufferEntry next() final {
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
            std::cout << "\nFinished epoch " << m_epoch << std::endl;
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


class AggregatedDataloader: public AbstractDataloader {
   public:
    explicit AggregatedDataloader(
      std::vector<std::pair<std::shared_ptr<AbstractDataloader>, float>>&& loaders) :
        m_loaders(std::move(loaders)) {
        float totalWeight = 0.0;
        for (auto& [loader, value] : m_loaders)
        {
            const float weight = value;
            value += totalWeight;
            totalWeight += weight;
        }
        m_uniformDist = std::uniform_real_distribution<float>(0.0, totalWeight);
    }

    inline BufferEntry next() final {
        assert(!m_loaders.empty());

        const float index = m_uniformDist(m_e1);

        for (const auto& [loader, value] : m_loaders)
        {
            if (index <= value)
            {
                return loader->next();
            }
        }

        std::cerr << "WARNING: index outside of biggest loader. Index: " << index
                  << ", loader value: " << m_loaders.back().second;

        return m_loaders.back().first->next();
    }


   private:
    std::vector<std::pair<std::shared_ptr<AbstractDataloader>, float>> m_loaders;
    std::default_random_engine                                         m_e1;
    std::uniform_real_distribution<float>                              m_uniformDist;
};
