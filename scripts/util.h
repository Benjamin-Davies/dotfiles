#pragma once

#include <cassert>
#include <filesystem>
#include <string>
#include <vector>

namespace fs = std::filesystem;

std::vector<std::string> collect_args(int argc, char **argv);

std::vector<std::string> read_lines(fs::path);

void append_lines_unique(fs::path, std::vector<std::string> &);

void remove_lines_unique(fs::path, std::vector<std::string> &);

bool confirm(std::string);
