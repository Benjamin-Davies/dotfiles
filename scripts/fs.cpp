// #ifdef USE_FS_POLYFILL

#include <cassert>
#include <iostream>

#include <sys/types.h>
#include <sys/stat.h>

#include "fs.h"

namespace fs
{

  bool ignore_filename(std::string name)
  {
    return name == "." || name == "..";
  }

  dirent *_readdir(DIR *dir)
  {
    dirent *e;
    do
    {
      e = readdir(dir);
    } while (e && ignore_filename(e->d_name));
    return e;
  }

  std::string path::filename()
  {
    auto start = m_str.rfind('/');
    return std::string(m_str.begin() + 1 + start, m_str.end());
  }

  path::operator std::string() const { return m_str; }

  path path::operator/(path other) const
  {
    if (*m_str.end() == '/')
      return path(m_str + other.m_str);
    else
      return path(m_str + "/" + other.m_str);
  }

  path path::operator/(std::string other) const
  {
    if (*m_str.end() == '/')
      return path(m_str + other);
    else
      return path(m_str + "/" + other);
  }

  path path::operator/(const char *other) const
  {
    if (*m_str.end() == '/')
      return path(m_str + other);
    else
      return path(m_str + "/" + other);
  }

  bool path::operator==(path other) const
  {
    return m_str == other.m_str;
  }

  directory_iterator::directory_iterator(const path &path) : m_path(path)
  {
    m_dir = opendir(path.c_str());
    assert(m_dir);
  }

  directory_iterator::~directory_iterator()
  {
    closedir(m_dir);
  }

  directory_iterator::iterator directory_iterator::begin() const
  {
    // Consume . and ..
    auto entry = _readdir(m_dir);
    return iterator(m_path, m_dir, entry ? std::optional(directory_entry(m_path, entry)) : std::nullopt);
  }

  directory_iterator::iterator directory_iterator::end() const
  {
    return iterator(m_path, m_dir, std::nullopt);
  }

  bool directory_iterator::iterator::operator!=(iterator &other)
  {
    return m_dir != other.m_dir || m_current.has_value() != other.m_current.has_value();
  }

  directory_iterator::iterator &directory_iterator::iterator::operator++()
  {
    auto entry = _readdir(m_dir);
    if (entry)
      m_current = directory_entry(m_path, entry);
    else
      m_current = std::nullopt;
    return *this;
  }

  directory_entry &directory_iterator::iterator::operator*()
  {
    return m_current.value();
  }

  path directory_entry::path()
  {
    std::string name(m_entry->d_name);
    return m_path / name;
  }

  bool directory_entry::is_directory()
  {
    return m_entry->d_type == DT_DIR;
  }

  bool directory_entry::is_symlink()
  {
    return m_entry->d_type == DT_LNK;
  }

  void create_directories(const path &path)
  {
    // https://stackoverflow.com/a/2336245/10530876
    const auto &str = path.native();
    for (auto p = str.begin() + 1; p != str.end(); ++p)
    {
      if (*p == '/')
      {
        std::string parent(str.begin(), p);
        mkdir(parent.c_str(), 0766);
      }
    }
    int res = mkdir(path.c_str(), 0766);
  }

  void create_symlink(const path &a, const path &b)
  {
    int res = symlink(a.c_str(), b.c_str());
    assert(!res);
  }

  bool exists(const path &path)
  {
    int res = access(path.c_str(), F_OK);
    return res == 0;
  }

  bool is_symlink(const path &path)
  {
    struct stat stat;
    int res = lstat(path.c_str(), &stat);
    return S_ISLNK(stat.st_mode);
  }

  bool is_directory(const path &path)
  {
    struct stat stat;
    int res = lstat(path.c_str(), &stat);
    return S_ISDIR(stat.st_mode);
  }

  path read_symlink(const path &path)
  {
    // TODO: support long paths
    char buffer[256];
    ssize_t nchars = readlink(path.c_str(), buffer, sizeof(buffer));
    assert(nchars < sizeof(buffer) && nchars > 0);
    return std::string(buffer, buffer + nchars);
  }

  void remove(const path &path, std::error_code &)
  {
    if (is_directory(path))
    {
      int res = rmdir(path.c_str());
      assert(!res);
    }
    else
    {
      int res = unlink(path.c_str());
      assert(!res);
    }
  }

  void remove_all(const path &path)
  {
    if (!exists(path))
      return;

    if (is_directory(path))
    {
      auto children = directory_iterator(path);
      for (auto &child : children)
      {
        remove_all(child.path());
      }

      int res = rmdir(path.c_str());
      assert(!res);
    }
    else
    {
      int res = unlink(path.c_str());
      assert(!res);
    }
  }

}

// #endif
