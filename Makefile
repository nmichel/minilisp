.PHONY: all build clean

all: build

build:
	elixirc -o $(TARGETDIR) $(SOURCES)

clean:
	rm -f $(TARGETDIR)/*.beam

SOURCES=$(wildcard *.ex)
TARGETDIR=./ebin
