#include <algorithm>
#include <cstdlib>
#include <iostream>

#include "util.h"

void unlink_directory(fs::path src, fs::path dest, std::vector<std::string> &linked, bool top_level = true);

int main(int argc, char **argv)
{
  // Extract env and args
  assert(getenv("DOTFILES"));
  fs::path dotfiles = std::getenv("DOTFILES");
  assert(getenv("HOME"));
  fs::path home = std::getenv("HOME");

  auto to_unlink = collect_args(argc, argv);

  // If nothing specified then unlink all
  if (to_unlink.size() == 0)
  {
    to_unlink = {"all"};
  }

  // If all specified, link all
  if (to_unlink[0] == "all")
  {
    to_unlink = read_lines(dotfiles / "categories.txt");
  }

  // Check that categories exist
  auto cats = read_lines(dotfiles / "categories.txt");

  std::sort(cats.begin(), cats.end());
  std::sort(to_unlink.begin(), to_unlink.end());
  assert(std::includes(cats.begin(), cats.end(), to_unlink.begin(), to_unlink.end()));

  // Unlink
  std::error_code err;
  for (auto &cat : to_unlink)
  {
    // Files
    {
      auto path = dotfiles / (cat + ".linked.txt");
      auto linked = read_lines(path);

      for (auto &file : linked)
      {
        fs::remove(file, err);
        if (err)
        {
          std::cerr << "Did not remove " << file << ": " << err.message() << std::endl;
        }
      }

      fs::remove(path, err);
    }

    // Repos
    {
      auto path = dotfiles / (cat + ".linked-repos.txt");
      auto linked = read_lines(path);

      for (auto &file : linked)
      {
        fs::remove_all(file);
      }

      fs::remove(path, err);
    }
  }

  // Record that they have been unlinked
  remove_lines_unique(dotfiles / "categories.linked.txt", to_unlink);
}
