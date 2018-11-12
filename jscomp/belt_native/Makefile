# To compile to ios you need to
# 1) cross compile stubs.c to ios: `xcrun -sdk iphoneos clang -arch armv7 -c -I /Users/bsansouci/Desktop/reprocessing-example-cross-platform/node_modules/ocaml-cross-mobile/ios-arm64/lib/ocaml/ stubs.c`
# 2) Use the right compiler: 
#    COMPILER=/Users/bsansouci/Desktop/reprocessing-example-cross-platform/node_modules/ocaml-cross-mobile/ios-arm64/bin/ocamlrun /Users/bsansouci/Desktop/reprocessing-example-cross-platform/node_modules/ocaml-cross-mobile/ios-arm64/bin/ocamlopt
# 3) add an include because that compiler has no idea where its stdlib is
#   INCLUDES=-I /Users/bsansouci/Desktop/reprocessing-example-cross-platform/node_modules/ocaml-cross-mobile/ios-arm64/lib/ocaml

include ../Makefile.shared

ifdef SYSTEMROOT
    PATHSEP2=\\
else
    PATHSEP2=/
endif
PATHSEP=$(strip $(PATHSEP2))

COMPILER=/Users/bsansouci/Desktop/reprocessing-example-cross-platform/node_modules/ocaml-cross-mobile/ios-arm64/bin/ocamlrun /Users/bsansouci/Desktop/reprocessing-example-cross-platform/node_modules/ocaml-cross-mobile/ios-arm64/bin/ocamlopt
BSPP=..$(PATHSEP)..$(PATHSEP)lib$(PATHSEP)bspp.exe
BELT_BSPPX=..$(PATHSEP)..$(PATHSEP)lib$(PATHSEP)belt_bsppx.exe

LOCAL_SOURCE_LIST= \
	caml_hash \
	js

GENERATED_SOURCE_LIST= \
	belt_HashSetString \
	belt_HashSetInt \
	belt_HashMapString \
	belt_HashMapInt \
	belt_MapString \
	belt_MapInt \
	belt_MutableMapString \
	belt_MutableMapInt \
	belt_internalMapInt \
	belt_internalMapString \
	belt_internalSetInt \
	belt_internalSetString \
	belt_SetInt \
	belt_SetString \
	belt_MutableSetInt \
	belt_MutableSetString \
	belt_SortArrayInt \
	belt_SortArrayString

SOURCE_LIST= \
	js_math \
	belt_Option \
	belt_Range \
	belt_Id \
	belt_Array \
	belt_SortArrayInt \
	belt_SortArrayString \
	belt_internalBucketsType \
	belt_internalSetBuckets \
	belt_internalBuckets \
	belt_HashSetString \
	belt_HashSetInt \
	belt_MutableStack \
	belt_MutableQueue \
	belt_HashMapInt \
	belt_HashMapString \
	belt_HashSet \
	belt_HashMap \
	belt_SortArray \
	belt_List \
	belt_internalAVLset \
	belt_internalAVLtree \
	belt_internalSetString \
	belt_internalSetInt \
	belt_MutableSetString \
	belt_MutableSetInt \
	belt_MutableSet \
	belt_internalMapString \
	belt_internalMapInt \
	belt_SetDict \
	belt_MapDict \
	belt_MapString \
	belt_MapInt \
	belt_Result \
	belt_MutableMapInt \
	belt_MutableMapString \
	belt_SetInt \
	belt_SetString \
	belt_Set \
	belt_MutableMap \
	belt_Map \
	belt

# belt_internalSet\
# belt_internalMutableAVL\
# belt_Bag\
# belt_BagM\
# belt_dyn belt_dyn_lib\

OBJECT_FILES := $(addsuffix .cmx, $(LOCAL_SOURCE_LIST)) $(addsuffix .cmx, $(SOURCE_LIST))
RUNTIME := $(addsuffix .cmi, $(SOURCE_LIST)) $(addsuffix .cmi, $(LOCAL_SOURCE_LIST)) $(OBJECT_FILES)

INCLUDES=-I /Users/bsansouci/Desktop/reprocessing-example-cross-platform/node_modules/ocaml-cross-mobile/ios-arm64/lib/ocaml

BS_FLAGS=  $(BS_COMMON_FLAGS)
COMPFLAGS += $(BS_FLAGS) -g -w +3-40-49 -warn-error A -bin-annot -ppx $(BELT_BSPPX)

lib.cmxa: $(RUNTIME)
	$(COMPILER) -a -o lib.cmxa $(OBJECT_FILES)

js.ml js.mli $(addsuffix .ml, $(SOURCE_LIST)) $(addsuffix .mli, $(filter-out js_math belt belt_SortArrayString belt_internalSetString belt_internalSetInt belt_internalMapString belt_internalMapInt, $(SOURCE_LIST))): ../belt_byte/js.ml ../belt_byte/js.mli $(addprefix ../belt_byte/, $(addsuffix .ml, $(SOURCE_LIST))) $(addprefix ../belt_byte/, $(addsuffix .mli, $(filter-out js_math belt belt_SortArrayString belt_internalSetString belt_internalSetInt belt_internalMapString belt_internalMapInt, $(SOURCE_LIST))))
	cp $^ ./

stubs.o: stubs.c
	$(COMPILER) -c $^

$(RUNTIME): ../../lib/belt_bsppx.exe stubs.o

OCAML_SRC_UTILS=../../vendor/ocaml/utils
OCAML_SRC_PARSING=../../vendor/ocaml/parsing
OCAML_SRC_TYPING=../../vendor/ocaml/typing
OCAML_SRC_BYTECOMP=../../vendor/ocaml/bytecomp
OCAML_SRC_DRIVER=../../vendor/ocaml/driver
OCAML_SRC_TOOLS=../../vendor/ocaml/tools

../../lib/belt_bsppx.exe:
	make -C belt_bsppx force-snapshotml
	make -C ../../lib belt_bsppx.exe

test: all
	$(COMPILER) $(INCLUDES) -ppx $(BELT_BSPPX) stubs.o -g -w +3-40-49 -warn-error A -bin-annot lib.cmxa -o test.byte test.ml
	./test.byte

clean::
	rm -f *.cm* *.o *.mlast* *.mliast* belt*.ml* js_math.ml *.cppo.ml* *.a
	rm -f *~
	rm -f  *.annot
	rm -f *.rawlambda *.lam *.lambda *.map
	rm -rf js.ml js.mli belt_HashSetString.ml belt_HashSetString.mli belt_HashSetInt.ml  belt_HashSetInt.mli \
	belt_HashMapInt.ml belt_HashMapInt.mli  belt_HashMapString.ml belt_HashMapString.mli \
	belt_MapInt.ml belt_MapInt.mli  belt_MapString.ml belt_MapString.mli \
	belt_internalSetInt.ml belt_internalSetString.ml \
	belt_SetInt.ml belt_SetInt.mli belt_SetString.ml  belt_SetString.mli \
	belt_MutableSetInt.ml belt_MutableSetInt.mli belt_MutableSetString.ml belt_MutableSetString.mli\
	belt_SortArrayInt.ml belt_SortArrayInt.mli belt_SortArrayString.ml belt_SortArrayString.mli\
	belt_internalMapInt.ml belt_internalMapInt.mli \
	belt_internalMapIntM.ml belt_internalMapIntM.mli \
	belt_internalMapString.ml belt_internalMapString.mli\
	belt_internalMapStringM.ml belt_internalMapStringM.mli\
	belt_MutableMapString.mli belt_MutableMapString.ml\
	belt_MutableMapInt.mli belt_MutableMapInt.ml

.mli.cmi:
	BS_NATIVE=true $(COMPILER) $(INCLUDES) -pp $(BSPP) $(COMPFLAGS) -o $@ -c $<
.ml.cmx:
	BS_NATIVE=true $(COMPILER) $(INCLUDES) -pp $(BSPP) $(COMPFLAGS) -o $@ -c $<



-include .depend

ML_SOURCES=$(addsuffix .ml, $(SOURCE_LIST)) $(addsuffix .ml, $(LOCAL_SOURCE_LIST)) 
MLI_SOURCES=$(addsuffix .mli, $(SOURCE_LIST)) $(addsuffix .mli, $(LOCAL_SOURCE_LIST)) 

depend:
	BS_NATIVE=true $(CAMLDEP) -pp $(BSPP) -ppx $(BELT_BSPPX) -native $(INCLUDES) $(ML_SOURCES) $(MLI_SOURCES) > .depend
