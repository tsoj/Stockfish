
#pragma once

#include <fstream>
#include <stdexcept>
#include <iostream>

#include "dataloader.h"

[[nodiscard]] inline BufferEntry readEPDPosition(const std::string& line) {


    std::istringstream iss(line);
    std::string        fenPieces;
    std::string        fenSide;
    std::string        fenCastling;
    std::string        fenEp;
    std::string        fenHClock;
    std::string        fenFClock;
    float              outcome = 0.5;

    if (!(iss >> fenPieces >> fenSide >> fenCastling >> fenEp >> fenHClock >> fenFClock >> outcome))
    {
        throw std::runtime_error("Error reading line: " + line);
    }

    const std::string fullFen = fenPieces + " " + fenSide + " " + fenCastling + " " + fenEp + " "
                              + fenHClock + " " + fenFClock;

    Position pos{};
    pos.set(fullFen, true, nullptr, nullptr);

    // std::cout << pos.fen() << ", " << outcome << std::endl;

    return BufferEntry{.pos               = Eval::toEvalPosition(pos),
                       .targetProbability = static_cast<float>(outcome)};
}

class EpdReader {
   public:
    explicit EpdReader(const std::filesystem::path& path) {

        for (const auto& entry : std::filesystem::directory_iterator(path))
        {
            if (entry.path().extension() == ".epd")
            {
                m_fileStreams.emplace_back(entry.path());
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


        std::string line;
        if (!std::getline(m_fileStreams.at(m_index), line))
        {
            m_fileStreams.erase(m_fileStreams.begin() + static_cast<int64_t>(m_index));
            m_index = 0;
            return next();
        }
        m_index = (m_index + 1) % m_fileStreams.size();


        return readEPDPosition(line);
    }

   private:
    // binpack::CompressedTrainingDataEntryReader m_reader;
    std::vector<std::ifstream> m_fileStreams;
    size_t                     m_index = 0;
};

using EpdDataloader = Dataloader<EpdReader>;
