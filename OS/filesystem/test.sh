#!/bin/bash

echo "Testing ..."

./init /home/doxer/fs
./format /home/doxer/fs

./import /home/doxer/fs /home/doxer/small /small
./import /home/doxer/fs /home/doxer/medium /medium
./import /home/doxer/fs /home/doxer/large /large

./ls /home/doxer/fs /

./mkdir /home/doxer/fs /dir

./ls /home/doxer/fs /
./ls /home/doxer/fs /dir

./copy /home/doxer/fs /medium /dir/medium

echo "medium copied"
./ls /home/doxer/fs /dir

./mkdir /home/doxer/fs /dir/dir
./ls /home/doxer/fs /dir

./copy /home/doxer/fs /small /dir/dir/small
./ls /home/doxer/fs /dir/dir

echo "copy"
./ls /home/doxer/fs /dir
./mkdir /home/doxer/fs /copy
./copy /home/doxer/fs /dir /copy
./ls /home/doxer/fs /
./ls /home/doxer/fs /copy
./ls /home/doxer/fs /copy/dir
./ls /home/doxer/fs /copy/dir/dir

echo "OK"
