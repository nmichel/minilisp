.PHONY: all build clean

all: build

build:
	elixirc -o $(TARGETDIR) $(SOURCES)

clean:
	rm -f $(TARGETDIR)/*.beam

SOURCESDIR=./src
SOURCES=$(wildcard $(SOURCESDIR)/*.ex)
TARGETDIR=./ebin
