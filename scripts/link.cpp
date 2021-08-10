#include <cstdlib>
#include <iostream>

#include "util.h"

void link_directory(fs::path src, fs::path dest, std::vector<std::string> &linked, bool top_level = true);

int main(int argc, char **argv)
{
  // Extract env and args
  assert(getenv("DOTFILES"));
  fs::path dotfiles = std::getenv("DOTFILES");
  assert(getenv("HOME"));
  fs::path home = std::getenv("HOME");

  auto to_link = collect_args(argc, argv);

  // If nothing specified then relink
  if (to_link.size() == 0)
  {
    to_link = read_lines(dotfiles / "categories.linked.txt");
  }

  // If still nothing then link default (dependent on platform)
  if (to_link.size() == 0)
  {
    to_link = {"shell"};
#ifdef __APPLE__
    to_link.push_back("macos");
#endif
#ifdef __linux__
    to_link.push_back("linux");
#endif
  }

  // If all specified, link all
  if (to_link[0] == "all")
  {
    to_link = read_lines(dotfiles / "categories.txt");
  }

  // Check that categories exist
  auto cats = read_lines(dotfiles / "categories.txt");

  std::sort(cats.begin(), cats.end());
  std::sort(to_link.begin(), to_link.end());
  assert(std::includes(cats.begin(), cats.end(), to_link.begin(), to_link.end()));

  // Link
  for (auto &cat : to_link)
  {
    std::vector<std::string> linked;
    link_directory(dotfiles / cat, home, linked);
    append_lines_unique(dotfiles / (cat + ".linked.txt"), linked);
  }

  // Record that they have been linked
  append_lines_unique(dotfiles / "categories.linked.txt", to_link);
}

void link_file(fs::path src, fs::path dest, std::vector<std::string> &linked)
{
  if (fs::exists(dest))
  {
    if (fs::is_symlink(dest) && fs::read_symlink(dest) == src)
    {
      return;
    }

    if (!confirm(std::string(dest) + " already exists! Would you like to overwrite it?"))
    {
      return;
    }

    fs::remove_all(dest);
  }

  fs::create_symlink(src, dest);
  linked.push_back(dest);
}

void link_directory(fs::path src, fs::path dest, std::vector<std::string> &linked, bool top_level)
{
  fs::create_directories(dest);

  for (auto &entry : fs::directory_iterator(src))
  {
    std::string child = entry.path().filename();

    if (child == "repos.json")
    {
      std::cerr << "TODO: link repos" << std::endl;
      continue;
    }

    if (top_level)
    {
      child.insert(child.begin(), '.');
    }

    if (!entry.is_symlink() && entry.is_directory())
    {
      link_directory(entry.path(), dest / child, linked, false);
    }
    else
    {
      link_file(entry.path(), dest / child, linked);
    }
  }
}
