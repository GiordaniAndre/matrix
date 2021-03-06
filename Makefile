.SUFFIXES: .f90 .f .o .mod .c .cpp

#------------------------------
# Define the compiler to use
#------------------------------
CC = gcc
CX = g++
FC = gfortran

#------------------------------
# Define any compile-time flags
#------------------------------
CC_FLAGS =  #-g #-Wall
CX_FLAGS =  #-g #-Wall
FC_FLAGS =  -g -cpp -fbounds-check #-fbounds-check -ffree-form -Wall -cpp -dM -Wno-unused

TARGET = test_matrix

default: $(OBJ)
	$(FC) $(FC_FLAGS) $(INCLUDES) -o $(TARGET) $(OBJ) $(LIB_FLAGS) $(LIBS)

complex: $(OBJ)
	$(FC) $(FC_FLAGS) -DUSE_COMPLEX $(INCLUDES) -o $(TARGET) $(OBJ) $(LIB_FLAGS) $(LIBS)

#------------------------------
# Define the suffixes in use
#------------------------------
SRC_DIR=
OBJ_DIR=
BIN_DIR=

#-----------------------------------------------------------------------
# Define any directories containing header files other than /usr/include
#-----------------------------------------------------------------------
INCLUDES = -I/usr/local/include #-I./src

#-----------------------------------------------------------------------
# Define library paths in addition to /usr/lib
#   if I wanted to include libraries not in /usr/lib I'd specify
#   their path using -Lpath, something like:
#-----------------------------------------------------------------------
LIB_FLAGS = -L./lib -L${LD_LIBRARY_PATH}/lib

#-----------------------------------------------------------------------
# define any libraries to link into executable:
#   if I want to link in libraries (libx.so or libx.a) I use the -llibname 
#   option, something like (this will link in libmylib.so and libm.so:
#-----------------------------------------------------------------------
LIBS = -lm -llapack

#---------------------------------------------------------------------#
# Define all the source files to compile here
#---------------------------------------------------------------------#
SRC  :=	matrix.f90 \
	dense_matrix.f90 dense_matrix_test.f90 \
	sparse_matrix.f90 \
	sparse_dok_matrix.f90 \
	main.f90

#-----------------------------------------------------------------------
# define the C,C++, Fortran object files 
#------------------------------------------------------------------------

OBJ = $(patsubst /%.f90,/%.o,$(SRC))

#------------------------------
# Executable
#------------------------------

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.f90
	$(FC) $(FC_FLAGS) -c  $< -o $@

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(CC_FLAGS) -c  $< -o $@

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.cpp
	$(CX) $(CX_FLAGS) -c  $< -o $@

%.o : %.mod

clean:
	$(RM) *~ *.o $(TARGET) *.mod
