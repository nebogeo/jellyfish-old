LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)
#include ${ANDROID_NDK_ROOT}\sources\cxx-stl\stlport\stlport

LOCAL_MODULE := starwisp-core

LOCAL_CFLAGS := -DANDROID_NDK -DUSE_MATH -DDISABLE_IMPORTGL -O3 --fast-math -Wno-write-strings

LOCAL_SRC_FILES := \
	fluxa/Sample.cpp \
	fluxa/Allocator.cpp \
	fluxa/Graph.cpp \
	fluxa/GraphNode.cpp \
	fluxa/Modules.cpp \
	fluxa/ModuleNodes.cpp \
	core/list.cpp \
	core/fixed.cpp \
	core/geometry.cpp \
	core/idmap.cpp \
	core/noise.cpp \
	engine/primitive.cpp \
	engine/text_primitive.cpp \
	engine/scenenode.cpp \
	engine/scenegraph.cpp \
	engine/texture.cpp \
    engine/importgl.c \
	engine/obj_reader.cpp \
    engine/nomadic.cpp \
	engine/engine.cpp \
	scheme/scheme.cpp \
	jellyfish/jellyfish.cpp \
	jellyfish/jellyfish_primitive.cpp \
	audio.cpp \
    app-android.c

LOCAL_LDLIBS := -lGLESv1_CM -lOpenSLES -ldl -llog

include $(BUILD_SHARED_LIBRARY)
