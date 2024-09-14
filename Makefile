ifndef NAME
	NAME = Stockfish-BAE
endif

ifndef COMP
	COMP = clang++
endif

FLAGS = -std=c++20
FLAGS += -Wall -Wextra -Wpedantic -Wshadow -Wdouble-promotion -Wundef -fno-common -Wconversion
FLAGS += -Wno-deprecated-enum-enum-conversion -Wno-sign-conversion -Wno-float-conversion \
		 -Wno-implicit-int-float-conversion -Wno-deprecated-enum-float-conversion -Wno-double-promotion \
		 -Wno-shorten-64-to-32 -Wno-implicit-int-conversion

LFLAGS = -static

ifdef ARCH
	FLAGS += -march=$(ARCH)
endif

ifeq ($(RELEASE),yes)
	FLAGS += -O3 -flto -DNDEBUG
	LFLAGS = -flto
else
	FLAGS += -Og -fno-omit-frame-pointer -g
endif

ifeq ($(SANITIZE),yes)
	FLAGS += -fsanitize=undefined -fsanitize=address -fsanitize=leak
endif

ifeq ($(SANITIZE_THREAD),yes)
	FLAGS += -fsanitize=thread
endif

ifndef SRC_DIR
	SRC_DIR = ./
endif

RELEASE_BUILD_DIR = ./release_build/
DEBUG_BUILD_DIR = ./debug_build/

ifeq ($(RELEASE),yes)
	BUILD_DIR = $(RELEASE_BUILD_DIR)
else
	BUILD_DIR = $(DEBUG_BUILD_DIR)
endif

CPP = $(shell find $(SRC_DIR) -name '*.cpp')

OBJ = $(CPP:$(SRC_DIR)%.cpp=$(BUILD_DIR)%.o)

build: $(BUILD_DIR)$(NAME)

src/bae_params.h: embed_eval_binary.py bae_params.bin
	python ./embed_eval_binary.py

$(BUILD_DIR)$(NAME): src/bae_params.h $(OBJ)
	$(COMP) -o $(BUILD_DIR)$(NAME) $(OBJ) $(FLAGS) $(LFLAGS)

-include $(OBJ:%.o=%.d)

$(BUILD_DIR)%.o: $(SRC_DIR)%.cpp Makefile
	mkdir -p $(dir $(BUILD_DIR)$*)
	$(COMP) $(FLAGS) -c $(SRC_DIR)$*.cpp -o $(BUILD_DIR)$*.o
	$(COMP) $(FLAGS) -MM -MT "$(BUILD_DIR)$*.o" $(SRC_DIR)$*.cpp >> $(BUILD_DIR)$*.d

.PHONY : build
.PHONY : clean

clean:
	$(if $(RELEASE_BUILD_DIR),,$(error RELEASE_BUILD_DIR is not set))
	$(if $(DEBUG_BUILD_DIR),,$(error DEBUG_BUILD_DIR is not set))
	-rm -rf ./$(RELEASE_BUILD_DIR)
	-rm -rf ./$(DEBUG_BUILD_DIR)
	-rm src/bae_params.h
