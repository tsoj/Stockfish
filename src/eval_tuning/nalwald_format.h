
#pragma once

#include <fstream>
#include <stdexcept>

#include "dataloader.h"


template<typename T>
[[nodiscard]] inline T read(std::istream& stream) {
    T value;
    stream.read(reinterpret_cast<char*>(&value), sizeof(T));
    return value;
}

[[nodiscard]] inline BufferEntry readNalwaldPosition(std::istream& stream) {

    const Eval::BB pawns   = read<uint64_t>(stream);
    const Eval::BB knights = read<uint64_t>(stream);
    const Eval::BB bishops = read<uint64_t>(stream);
    const Eval::BB rooks   = read<uint64_t>(stream);
    const Eval::BB queens  = read<uint64_t>(stream);
    const Eval::BB kings   = read<uint64_t>(stream);
    const Eval::BB white   = read<uint64_t>(stream);
    const Eval::BB black   = read<uint64_t>(stream);

    [[maybe_unused]] const auto enPassantTarget = read<uint64_t>(stream);
    [[maybe_unused]] const auto castlingInfo1   = read<uint8_t>(stream);
    [[maybe_unused]] const auto castlingInfo2   = read<uint8_t>(stream);
    [[maybe_unused]] const auto castlingInfo3   = read<uint8_t>(stream);
    [[maybe_unused]] const auto castlingInfo4   = read<uint8_t>(stream);

    [[maybe_unused]] const auto zobristKey      = read<uint64_t>(stream);
    [[maybe_unused]] const auto us              = read<uint8_t>(stream);
    [[maybe_unused]] const auto halfmovesPlayed = read<int16_t>(stream);
    [[maybe_unused]] const auto halfmoveClock   = read<int16_t>(stream);

    const double outcome = read<double>(stream);

    return BufferEntry{
      .pos = Eval::EvalPosition(white, black, pawns, knights, bishops, rooks, queens, kings),
      .targetProbability = static_cast<float>(outcome)};
}

class NalwaldReader {
   public:
    explicit NalwaldReader(const std::filesystem::path& path) {

        for (const auto& entry : std::filesystem::directory_iterator(path))
        {
            if (entry.path().extension() == ".bin")
            {
                m_fileStreams.emplace_back(entry.path(), std::ios::binary);
                if (!m_fileStreams.back().is_open())
                {
                    throw std::runtime_error("Couldn't open file: " + entry.path().string());
                }
                std::cout << "Opened " << entry.path() << std::endl;
            }
        }

        if (m_fileStreams.empty())
        {
            throw std::runtime_error("Didn't find any files valid for file streaming in "
                                     + path.string());
        }
    }

    [[nodiscard]] inline std::optional<BufferEntry> next() {
        if (m_fileStreams.empty())
        {
            return std::nullopt;
        }


        if (m_fileStreams.at(m_index).peek() == EOF)
        {
            m_fileStreams.erase(m_fileStreams.begin() + static_cast<int64_t>(m_index));
            m_index = 0;
            return next();
        }

        auto result = readNalwaldPosition(m_fileStreams.at(m_index));

        m_index = (m_index + 1) % m_fileStreams.size();

        return result;
    }

   private:
    // binpack::CompressedTrainingDataEntryReader m_reader;
    std::vector<std::ifstream> m_fileStreams;
    size_t                     m_index = 0;
};

using NalwaldDataloader = Dataloader<NalwaldReader>;
