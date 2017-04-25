main: main.cpp
		g++ -o main main.cpp -g -Wall -Wextra -pedantic -std=c++14 -O2 -Wshadow -Wformat=2 -Wfloat-equal -Wconversion -Wlogical-op -Wcast-qual -Wcast-align -Wparentheses -D_GLIBCXX_DEBUG -D_GLIBCXX_DEBUG_PEDANTIC -fsanitize=address -fsanitize=undefined -fstack-protector -lmcheck -D_FORTIFY_SOURCE=2 

test:
	bash test.sh

clean:
	$(RM) *.svg main
