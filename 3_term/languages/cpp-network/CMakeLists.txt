cmake_minimum_required(VERSION 3.5)
project(cpp_network)

set(CMAKE_CXX_STANDARD 14)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pthread")

set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -DCPP_NETWORK_SOCKET_DEBUG -Wshadow\
        -Wall -Wextra -pedantic -Wformat=2 -Wfloat-equal -Wconversion -Wlogical-op -Wcast-qual -Wcast-align\
        -D_GLIBCXX_DEBUG -D_GLIBCXX_DEBUG_PEDANTIC -fsanitize=address -fsanitize=undefined -fstack-protector -lmcheck -D_FORTIFY_SOURCE=2")
#-DCPP_NETWORK_SOCKET_DEBUG
#-Wshadow

set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3")

include_directories(.)

# Libraries
add_library(utils STATIC
        utils/string_buffer.cpp utils/string_buffer.h
        utils/string_view.cpp utils/string_view.h
        utils/executor.cpp utils/executor.h
        utils/log.cpp utils/log.h)

add_library(network STATIC
        network/file_descriptor.cpp network/file_descriptor.h
        network/socket.cpp network/socket.h
        network/network_exception.cpp network/network_exception.h
        network/epoll.cpp network/epoll.h
        network/epoll_utils.cpp network/epoll_utils.h
        network/event_descriptor.cpp network/event_descriptor.h
        network/timer_descriptor.cpp network/timer_descriptor.h
        network/network.h)
target_link_libraries(network utils)

add_library(network_http STATIC
        network/http/request.cpp network/http/request.h
        network/http/request_parser.cpp network/http/request_parser.h
        network/http/resolver.cpp network/http/resolver.h
        network/http/http.h)
target_link_libraries(network_http network)

# Executables
add_executable(echo_single apps/echo/echo_single.cpp)
target_link_libraries(echo_single utils network network_http)
add_executable(echo_epoll apps/echo/echo_epoll.cpp)
target_link_libraries(echo_epoll utils network network_http)

add_executable(request_printer apps/misc/request_printer.cpp)
target_link_libraries(request_printer utils network network_http)

add_executable(proxy apps/proxy.cpp)
target_link_libraries(proxy utils network network_http)
