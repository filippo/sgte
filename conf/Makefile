MK_INCLUDE=include.mk

all: config.status $(MK_INCLUDE)

config.status: configure
	./configure

configure: configure.ac
	autoconf

clean:
	-rm -rf config.cache config.log config.status configure $(MK_INCLUDE) autom4te.cache


