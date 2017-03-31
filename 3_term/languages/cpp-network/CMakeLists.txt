cmake_minimum_required(VERSION 3.5)
project(cpp_network)

# Flags
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14 -pthread")

set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -DCPP_NETWORK_SOCKET_DEBUG\
        -Wall -Wextra -pedantic -Wformat=2 -Wfloat-equal -Wconversion -Wlogical-op -Wcast-qual -Wcast-align\
        -D_GLIBCXX_DEBUG -D_GLIBCXX_DEBUG_PEDANTIC -fsanitize=address -fstack-protector -lmcheck -D_FORTIFY_SOURCE=2")
#-DCPP_NETWORK_SOCKET_DEBUG
#-Wshadow

set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3")

# Internal and external libs
include_directories(.)

find_package(Boost REQUIRED)
include_directories(${Boost_INCLUDE_DIR})

# Targets
set(UTILS_FILES utils/string_buffer.cpp utils/string_buffer.h
        utils/string_view.cpp utils/string_view.h
        utils/executor.cpp utils/executor.h
        utils/log.cpp utils/log.h)

set(NETWORK_FILES network/file_descriptor.cpp network/file_descriptor.h
        network/socket.cpp network/socket.h
        network/network_exception.cpp network/network_exception.h
        network/epoll.cpp network/epoll.h
        network/epoll_utils.cpp network/epoll_utils.h
        network/event_descriptor.cpp network/event_descriptor.h
        network/network.h network/timer_descriptor.cpp network/timer_descriptor.h)

set(NETWORK_HTTP_FILES network/http/request.cpp network/http/request.h
        network/http/request_parser.cpp network/http/request_parser.h
        network/http/resolver.cpp network/http/resolver.h
        network/http/http.h)

set(LIB_FILES ${UTILS_FILES} ${NETWORK_FILES} ${NETWORK_HTTP_FILES})

# Executables
add_executable(echo_single apps/echo/echo_single.cpp ${LIB_FILES})
add_executable(echo_epoll apps/echo/echo_epoll.cpp ${LIB_FILES})

add_executable(request_printer apps/misc/request_printer.cpp ${LIB_FILES})

add_executable(proxy apps/proxy/proxy.cpp ${LIB_FILES})
