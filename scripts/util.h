#pragma once

#ifdef USE_FS_POLYFILL
#include "fs.h"
#else
#include <filesystem>
namespace fs = std::filesystem;
#endif

#include <cassert>
#include <string>
#include <vector>

std::vector<std::string> collect_args(int argc, char **argv);

std::vector<std::string> read_lines(fs::path);

void append_lines_unique(fs::path, std::vector<std::string> &);

void remove_lines_unique(fs::path, std::vector<std::string> &);

bool confirm(std::string);
