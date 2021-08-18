# Borrowed from neovim root makefile
CMAKE_GENERATOR ?= $(shell (type ninja > /dev/null 2>&1 && echo "Ninja") || \
    echo "Unix Makefiles")

CMAKE_BUILD_TYPE ?= Release

CMAKE_FLAGS = -G "$(CMAKE_GENERATOR)" \
		-DCMAKE_BUILD_TYPE="$(CMAKE_BUILD_TYPE)" \
		-DUSE_FS_POLYFILL="$(USE_FS_POLYFILL)"

all: build/CMakeCache.txt
	cd build && cmake --build .

build/CMakeCache.txt: scripts/CMakeLists.txt
	mkdir -p build
	cd build && cmake $(CMAKE_FLAGS) ../scripts
	touch build/CMakeLists.txt

clean:
	rm -rf build

.PHONY: all clean
