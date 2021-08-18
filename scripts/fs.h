#pragma once
// Incomplete polyfill for <filesystem>

#include <iomanip>
#include <optional>
#include <string>

#include <dirent.h>
#include <stdio.h>
#include <unistd.h>

namespace fs
{

  class path
  {
  public:
    path(char *str) : m_str(str) {}
    path(const std::string &str) : m_str(str) {}

    std::string filename();
    std::string native() const { return m_str; }
    const char *c_str() const { return m_str.c_str(); }

    operator std::string() const;
    path operator/(path) const;
    path operator/(std::string) const;
    path operator/(const char *) const;
    bool operator==(path) const;

  private:
    std::string m_str;
  };

  template <class OS>
  OS &operator<<(OS &os, const path &p)
  {
    return os << std::quoted(p.native());
  }

  class directory_entry
  {
  public:
    directory_entry(const path &path, dirent *entry) : m_path(path), m_entry(entry) {}

    directory_entry(const directory_entry &other)
        : directory_entry(other.m_path, other.m_entry) {}
    directory_entry(directory_entry &&other)
        : directory_entry(other.m_path, other.m_entry) {}
    directory_entry &operator=(const directory_entry &other)
    {
      (fs::path &)m_path = other.m_path;
      m_entry = other.m_entry;
      return *this;
    }
    directory_entry &operator=(directory_entry &&other)
    {
      (fs::path &)m_path = other.m_path;
      m_entry = other.m_entry;
      return *this;
    }

    fs::path path();

    bool is_directory();
    bool is_symlink();

    bool operator!=(directory_entry &other);

  private:
    const fs::path &m_path;
    dirent *m_entry;
  };

  class directory_iterator
  {
  public:
    class iterator
    {
    public:
      iterator(const path &path, DIR *dir, std::optional<directory_entry> current) : m_path(path), m_dir(dir), m_current(current) {}

      bool operator!=(iterator &);
      iterator &operator++();
      directory_entry &operator*();

    private:
      const path &m_path;
      DIR *m_dir;
      std::optional<directory_entry> m_current;
    };

    directory_iterator(const path &);
    ~directory_iterator();

    iterator begin() const;
    iterator end() const;

  private:
    const path &m_path;
    DIR *m_dir;
  };

  void create_directories(const path &);
  void create_symlink(const path &, const path &);
  bool exists(const path &);
  bool is_symlink(const path &);
  bool is_directory(const path &);
  path read_symlink(const path &);
  void remove(const path &, std::error_code &);
  void remove_all(const path &);

}
