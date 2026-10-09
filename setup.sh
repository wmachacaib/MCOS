export FC=mpif90
cmake -S . -B build
cmake --build build -j 4
cmake --install build
# run from examples/Channel
# mpirun -n 4 ../../build/opt/bin/xcompact3d input.i3d
# run from . 
# mpirun -n 4 ./build/opt/bin/xcompact3d input.i3d
