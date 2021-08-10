# Borrowed from neovim root makefile
BUILD_TYPE ?= $(shell (type ninja > /dev/null 2>&1 && echo "Ninja") || \
    echo "Unix Makefiles")

all: build/CMakeCache.txt
	cd build && cmake --build .

build/CMakeCache.txt: scripts/CMakeLists.txt
	mkdir -p build
	cd build && cmake -G $(BUILD_TYPE) ../scripts
	touch build/CMakeLists.txt

clean:
	rm -rf build

.PHONY: all clean
