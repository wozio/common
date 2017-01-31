OUTDIR = ..\..\lib
PUBLIC_HEADERS_DIR = ..\..\include\yami4-cpp
YAMI_LIBS = $(OUTDIR)\yamicpp.lib $(OUTDIR)\yamicore.lib
PYTHON_HOME = C:\Python32
PYTHON_LIB = $(PYTHON_HOME)\libs\python32.lib
PYTHON_INCLUDE = $(PYTHON_HOME)\include
PYTHON_MODULE_DEF_FILE = yami4py.def
PYTHON_MODULE = $(OUTDIR)\yami4py.dll

CPP = cl.exe
CPP_OPTIONS = /nologo /EHsc /O2 /MD /D _CRT_SECURE_NO_WARNINGS \
	/I ..\..\include /I $(PYTHON_INCLUDE) /D "WIN32" /c

LINK32 = link.exe
LINK32_PYTHON_FLAGS = /nologo /dll $(YAMI_LIBS) Ws2_32.lib \
	$(PYTHON_LIB) /def:$(PYTHON_MODULE_DEF_FILE) \
	/out:$(PYTHON_MODULE)

MODULE_OBJS = yami4py.obj

all : $(PYTHON_MODULE)

$(PYTHON_MODULE) : $(MODULE_OBJS)
	$(LINK32) $(LINK32_PYTHON_FLAGS) $(MODULE_OBJS)
	copy yami.py $(OUTDIR)

.cpp.obj::
	$(CPP) $(CPP_OPTIONS) $< 

clean :
	del /Q *.obj
	del /Q $(PYTHON_MODULE) ..\..\lib\yami.py
