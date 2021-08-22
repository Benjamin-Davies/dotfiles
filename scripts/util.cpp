#include <algorithm>
#include <fstream>
#include <iostream>
#include <unordered_set>

#include "util.h"

std::vector<std::string> collect_args(int argc, char **argv)
{
  argv++;
  argc--;

  std::vector<std::string> args;
  while (argc > 0)
  {
    args.push_back(*argv);

    argv++;
    argc--;
  }

  return args;
}

std::vector<std::string> read_lines(const fs::path &path)
{
  if (!fs::exists(path))
    return {};

  std::ifstream is(path);

  std::vector<std::string> lines;
  std::string tmp;
  while (std::getline(is, tmp))
    lines.push_back(tmp);

  return lines;
}

std::vector<std::vector<std::string>> read_tsv(const fs::path &path)
{
  auto lines = read_lines(path);

  std::vector<std::vector<std::string>> rows;
  rows.reserve(lines.size());

  std::transform(lines.begin(), lines.end(),
                 std::back_inserter(rows),
                 [](const std::string &line)
                 {
                   std::vector<std::string> cols;
                   auto start = line.begin();
                   auto pos = line.begin();
                   for (; pos < line.end(); pos++)
                   {
                     if (*pos == '\t')
                     {
                       cols.push_back(std::string(start, pos));
                       start = pos + 1;
                     }
                   }
                   cols.push_back(std::string(start, pos));
                   return cols;
                 });

  return rows;
}

void append_lines_unique(const fs::path &path, std::vector<std::string> &lines)
{
  auto existing = read_lines(path);
  std::unordered_set set(existing.begin(), existing.end());

  std::ofstream os(path, std::ios::app);

  for (auto &line : lines)
  {
    if (set.find(line) == set.end())
    {
      os << line << std::endl;
      set.insert(line);
    }
  }
}

void remove_lines_unique(const fs::path &path, std::vector<std::string> &lines)
{
  auto existing = read_lines(path);
  std::unordered_set set(lines.begin(), lines.end());

  std::ofstream os(path, std::ios::out);

  for (auto &line : existing)
  {
    if (set.find(line) == set.end())
    {
      os << line << std::endl;
      set.insert(line);
    }
  }
}

bool confirm(std::string prompt)
{
  std::string input;
  while (true)
  {
    std::cerr << prompt << " [y,n] ";
    std::cin >> input;

    if (input == "y")
      return true;
    if (input == "n" || input == "")
      return false;
  }
}
