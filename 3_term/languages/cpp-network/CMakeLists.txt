cmake_minimum_required(VERSION 3.5)
project(cpp_network)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14 -pthread")

set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -DCPP_NETWORK_DEBUG\
        -Wall -Wextra -pedantic -Wformat=2 -Wfloat-equal -Wconversion -Wlogical-op -Wcast-qual -Wcast-align\
         -D_GLIBCXX_DEBUG -D_GLIBCXX_DEBUG_PEDANTIC -fsanitize=address -fstack-protector -lmcheck -D_FORTIFY_SOURCE=2")
#-Wshadow

set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3")

include_directories(.)

set(LIB_FILES network/file_descriptor.h network/file_descriptor.cpp
        network/socket.cpp network/socket.h
        network/network_exception.cpp network/network_exception.h
        network/epoll.cpp network/epoll.h
        network/utils/string_buffer.cpp network/utils/string_buffer.h
        network/utils/string_view.cpp network/utils/string_view.h)

set(RSHD_FILES apps/rshd/rshd.c
        apps/rshd/ptr_set.h apps/rshd/ptr_set.cpp
        apps/rshd/string_buffer.h apps/rshd/string_buffer.cpp)
add_executable(rshd ${RSHD_FILES})

add_executable(echo_single apps/echo/echo_single.cpp ${LIB_FILES})
add_executable(echo_epoll apps/echo/echo_epoll.cpp ${LIB_FILES})