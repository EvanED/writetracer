# -*- python -*-

env = Environment()

env.AppendUnique(CCFLAGS=["-Wall", "-Wextra"])

env.AppendUnique(CPPPATH=["/opt/dyninst/include"])
env.AppendUnique(LIBPATH=["/opt/dyninst/lib"])
env.AppendUnique(RPATH=["/opt/dyninst/lib"])
env.AppendUnique(LIBS=["pcontrol", "common", "pthread", "dl", "symLite", "dynElf"])

sources = Split("""
    src/main.cc
""")

env.Program("iotrace", sources)
