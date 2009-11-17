#!/bin/zsh -fex

# Q: what to do of this old broken test suite?
# ${CL_LAUNCH} ${CL_LAUNCH_FLAGS} --system xcvb-test --restart xcvb::quit

LISPS=(sbcl ccl clisp)

abort () {
  : PATH=$PATH
  if [ -n "$*" ] ; then print -r "$*" >&2 ; fi
  exit 42
}
doenv () {
  while [ "$#" -gt 0 ] ; do
    case "$1" in
      (*=*) declare "$1" ; shift ;;
      (*) $=1 ; shift ;;
    esac
  done
}

#### Initializing variables ####
reset_variables () {
  unset RELEASE_DIR asdf_path CL_LAUNCH_ASDF_PATH XCVB_PATH XCVB_DIR
}
initialize_variables () {
  : ${TMP:=/tmp}
  : ${LISP:=sbcl}
}
finalize_variables () {
  obj="${BUILD_DIR}/obj"
  INSTALL_BIN="$BUILD_DIR/bin"
  INSTALL_LISP="$BUILD_DIR/common-lisp"
  INSTALL_IMAGE="$BUILD_DIR/common-lisp/images"
  INSTALL_SOURCE="$BUILD_DIR/common-lisp/source"
  INSTALL_SYSTEMS="$BUILD_DIR/common-lisp/systems"

  ENV=(INSTALL_BIN=$INSTALL_BIN
       INSTALL_IMAGE=$INSTALL_IMAGE
       INSTALL_LISP=$INSTALL_LISP
       INSTALL_SOURCE=$INSTALL_SOURCE
       INSTALL_SYSTEMS=$INSTALL_SYSTEMS
       XCVB_PATH=$XCVB_PATH
       XCVB_OBJECT_DIRECTORY=$obj
       LISP=$LISP)

  export PATH=$INSTALL_BIN:$PATH
  export XCVB_PATH
  export LISP_FASL_CACHE="$BUILD_DIR/_cache"
  export LISP
  TEST_CL_LAUNCH_FLAGS=(
    --lisp "$LISP"
    #${(s: :)asdf_path+"--path ${(j: --path :)asdf_path}"} ## fails when path contains spaces!
  )
  for i in $asdf_path ; do TEST_CL_LAUNCH_FLAGS=(${TEST_CL_LAUNCH_FLAGS} --path $i) ; done
}
compute_release_dir_variables () {
  initialize_variables
  BUILD_DIR="$RELEASE_DIR/build"
  XCVB_DIR="$RELEASE_DIR/xcvb"
  asdf_path=($XCVB_DIR "$RELEASE_DIR"/dependencies/*)
  XCVB_PATH="${BUILD_DIR}:${RELEASE_DIR}"
  finalize_variables
  ENV=($ENV CL_LAUNCH_FLAGS="$TEST_CL_LAUNCH_FLAGS")
}
compute_xcvb_dir_variables () {
  initialize_variables
  [ -n "$XCVB_PATH" ] || abort "You need to define an XCVB_PATH to test xcvb"
  BUILD_DIR="$XCVB_DIR/build"
  finalize_variables
}

#### Checking the preconditions for the test suite itself
check_release_dir () {
  [ -d "$RELEASE_DIR" -a -f "$RELEASE_DIR/INSTALL" ] ||
  abort "Invalid release directory $RELEASE_DIR"
  check_xcvb_dir
}
check_xcvb_dir () {
  [ -f $XCVB_DIR/xcvb.asd -a -f $XCVB_DIR/driver.lisp ] ||
  abort "Invalid xcvb directory $XCVB_DIR"
  [ -f $XCVB_DIR/configure.mk ] ||
  abort "Please configure your configure.mk (and don't forget to properly setup ASDF)"
}
check_asdf_setup () {
  CONFIGURED_CL_LAUNCH_FLAGS="$(make show-config $ENV | grep "^CL_LAUNCH_FLAGS=" | cut -d= -f2-)"
  for i in asdf-dependency-grovel cl-launch closer-mop command-line-arguments poiu ; do
    cl-launch $=CONFIGURED_CL_LAUNCH_FLAGS -i \
    "(cl-launch::quit (if (asdf:find-system :$i nil) 0 1))" ||
    abort "Couldn't locate ASDF system $i. Make sure your configure.mk has it."
  done
}

#### And now, the Tests! ####

validate_xcvb_version () {
  xcvb version

  xcvb version | grep '^XCVB version ' ||
  abort "XCVB version failed"

  case "$LISP" in
   (sbcl) x=SBCL ;;
   (ccl) x="Clozure Common Lisp" ;;
   (clisp) x="CLISP" ;;
  esac

  xcvb version | grep -i "^(compiled with $x" ||
  abort "XCVB version using wrong Lisp"
}

validate_xcvb_ssp () {
  # preconditions: env, xcvb built, PWD=.../xcvb/
  # postconditions: xcvb ssp working
  local out=${BUILD_DIR}/xcvb-ssp.out

  xcvb ssp --xcvb-path $XCVB_DIR | tee $out

  grep -q "(:BUILD \"/xcvb\") in \".*/build.xcvb\"" $out ||
  abort "Can't find build for xcvb"

  fgrep -q "(:ASDF \"xcvb\") superseded by (:BUILD \"/xcvb\")" $out ||
  abort "can't find superseded asdf for xcvb"

  grep -q "CONFLICT for \"/xcvb/test/conflict/b\" between (\".*/test/conflict/b2\\?/build.xcvb\" \".*/test/conflict/b2\\?/build.xcvb\")" $out ||
  abort "can't find conflict for /xcvb/test/conflict/b"
}

validate_xcvb () {
  # preconditions: env, xcvb built, PWD=.../xcvb/
  validate_xcvb_version # is built xcvb what we think it is?
  validate_xcvb_ssp # can it search its search path?
  #validate_sa_backend # can it build hello with the standalone backend?
  validate_mk_backend # can it build hello with the Makefile backend?
  validate_nemk_backend # can it build hello with the non-enforcing Makefile backend?
  validate_x2a # can it convert hello back to asdf?
  validate_rmx # can it remove the xcvb annotations from a2x-test?
  validate_a2x # can it migrate a2x-test from xcvb?
  validate_master # does xcvb-master work?
}

validate_hello () {
  [ "hello, world" = "$($INSTALL_BIN/hello -t)" ] || abort "hello not working"
}

validate_hello_build () {
  mkdir -p $INSTALL_BIN $INSTALL_IMAGE
  cd $XCVB_DIR/hello
  rm -f $INSTALL_BIN/hello setup.lisp ; :
  $@
  rehash
  validate_hello
  rm -f $INSTALL_BIN/hello setup.lisp ; :
}

validate_mk_backend () {
  validate_hello_build make hello $ENV
}

validate_nemk_backend () {
  validate_hello_build make hello-using-nemk $ENV
}

validate_x2a () {
  validate_hello_build make hello-using-asdf $ENV
}

validate_rmx () {
  mkdir -p $BUILD_DIR/a2x_rmx/
  rsync -a $XCVB_DIR/test/a2x/ $BUILD_DIR/a2x_rmx/
  cd $BUILD_DIR/a2x_rmx
  XCVB_PATH=$BUILD_DIR/a2x_rmx xcvb ssp
  XCVB_PATH=$BUILD_DIR/a2x_rmx xcvb rmx --build /xcvb/test/a2x
  [ ! -f $BUILD_DIR/a2x_rmx/build.xcvb ] ||
  abort "xcvb rmx failed to remove build.xcvb"
  grep '(module' $BUILD_DIR/a2x_rmx/*.lisp &&
  abort "xcvb rmx failed to delete module forms"
}

validate_a2x () {
  mkdir -p $BUILD_DIR/a2x_a2x/
  rsync -a $XCVB_DIR/test/a2x/ $BUILD_DIR/a2x_a2x/
  cd $BUILD_DIR/a2x_a2x
  xcvb a2x --system a2x-test --name /xcvb/test/a2x
  [ -f $BUILD_DIR/a2x_a2x/build.xcvb ] ||
  abort "xcvb a2x failed to create build.xcvb"
  for i in $BUILD_DIR/a2x_a2x/*.lisp ; do
    grep '(module' $i ||
    abort "xcvb rmx failed to create module form for $i"
  done
}

validate_master () {
  which xcvb
  xcvb version
  xcvb fm -n xcvb/master -s
#  cl-launch -l $LISP -f $(xcvb fm -n xcvb/master -s) \
#	-i "(xcvb-master:bnl \"xcvb/hello\" :setup \"xcvb/no-asdf\" \
#		:output-path\"$BUILD_DIR/\" :object-directory\"$obj\" :verbosity 9)" \
#        -i "(let ((*print-base* 30)) (xcvb-hello::hello :name 716822547 :traditional t))" \
#	> $BUILD_DIR/out
  xcvb eval "'(#.(xcvb-master:bnl \"xcvb/hello\" :setup \"xcvb/no-asdf\" \
		:output-path \"$BUILD_DIR/\" :object-directory\"$obj\" :verbosity 9) \
	       #.(let ((*print-base* 30)) (xcvb-hello::hello :name 716822547 :traditional t)))" \
	> $BUILD_DIR/out
  fgrep 'hello, tester' < $BUILD_DIR/out ||
  abort "Failed to use hello through the XCVB master"
}

validate_slave () {
  xcvb slave-builder --build /xcvb/hello \
	--lisp-implementation $LISP --object-directory $obj --output-path $BUILD_DIR/ |
  fgrep 'Your desires are my orders' ||
  abort "Failed to drive a slave $LISP to build hello"
}

do_asdf_build () {
  # preconditions: env, PWD=.../xcvb/
  # postconditions: xcvb built
  check_asdf_setup
  make xcvb-using-asdf $ENV # bootstrap
  rehash
}
do_self_mk_build () {
  # pre-requisites (besides env): PWD=.../xcvb/
  make xcvb $ENV
  rehash
}
do_self_nemk_build () {
  # pre-requisites (besides env): PWD=.../xcvb/
  rm -f setup.lisp ; :
  make setup.lisp $ENV
  make xcvb-using-nemk $ENV
  rm setup.lisp ; :
  rehash
}

validate_asdf_xcvb () {
  cd $XCVB_DIR
  do_asdf_build ; validate_xcvb
}
validate_mk_xcvb () {
  cd $XCVB_DIR
  do_self_mk_build ; validate_xcvb
}
validate_nemk_xcvb () {
  cd $XCVB_DIR
  do_self_nemk_build ; validate_xcvb
}
validate_bootstrapped_xcvb () {
  cd ${RELEASE_DIR:-/dev/null}
  do_bootstrapped_build ; validate_xcvb
}

do_bootstrapped_build () {
  # pre-requisites (besides env): PWD=.../xcvb-release/
  rm -rf "$XCVB_DIR/obj"
  make install $ENV
  rehash
}

clean_xcvb_dir () {
  cd $XCVB_DIR ;
  make clean
  rm -rf "${XCVB_DIR}/build/"
}

with_xcvb_build_dir () {
  mkdir -p $obj $INSTALL_BIN $INSTALL_IMAGE
  $@
  clean_xcvb_dir
}

validate_xcvb_dir () {
  compute_xcvb_dir_variables
  check_xcvb_dir
  cd $XCVB_DIR
  clean_xcvb_dir
  mkdir -p $obj $INSTALL_BIN
  with_xcvb_build_dir validate_asdf_xcvb
  with_xcvb_build_dir validate_mk_xcvb
  with_xcvb_build_dir validate_nemk_xcvb
  clean_xcvb_dir
}

validate_xcvb_dir_all_lisps () {
  compute_xcvb_dir_variables
  for LISP in $LISPS ; do
    validate_xcvb_dir
  done
}

clean_release_dir () {
  cd $XCVB_DIR ;
  make clean
  rm -rf "${RELEASE_DIR}/build/"
}

with_release_build_dir () {
  clean_release_dir
  mkdir -p $obj $INSTALL_BIN $INSTALL_IMAGE
  $@
  clean_release_dir
}
validate_release_dir () {
  compute_release_dir_variables
  check_release_dir
  with_release_build_dir validate_bootstrapped_xcvb
  with_release_build_dir validate_asdf_xcvb
  with_release_build_dir validate_mk_xcvb
  with_release_build_dir validate_nemk_xcvb
}

validate_release_dir_all_lisps () {
  compute_release_dir_variables
  for LISP in $LISPS ; do
    validate_release_dir
  done
}

#### And now, let's do whatever the user asks us to! ####
#reset_variables # don't do it.

# NB: the same-line exit ensures no funny surprise if the script is edited while running.
doenv $@ ; exit
