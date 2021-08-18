#include <filesystem>

namespace fs = std::filesystem;

int main() {
  fs::exists("foo.txt");
}
