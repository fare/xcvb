#!/bin/zsh -fex

# Q: what to do of this old broken test suite?
# ${CL_LAUNCH} ${CL_LAUNCH_FLAGS} --system xcvb-test --restart xcvb::quit

abort () {
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
  LISPS=(clisp sbcl ccl)
}
initialize_variables () {
  : ${TMP:=/tmp}
  : ${LISP:=sbcl}
}
finalize_variables () {
  obj="$BUILD_DIR/obj"
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
       LISP=$LISP)

  export PATH=$INSTALL_BIN:$PATH
  export XCVB_PATH
  export LISP_FASL_CACHE="$BUILD_DIR/cache"
  TEST_CL_LAUNCH_FLAGS=(
    --lisp "$LISP"
    $TEST_CL_LAUNCH_FLAGS
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
}
check_xcvb_dir () {
  [ -f $XCVB_DIR/xcvb.asd -a -f $XCVB_DIR/driver.lisp ] ||
  abort "Invalid xcvb directory $XCVB_DIR"
  [ -f $XCVB/configure.mk ] ||
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

validate_xcvb_ssp () {
  # preconditions: env, xcvb built, PWD=.../xcvb/
  # postconditions: xcvb ssp working

  xcvb ssp --xcvb-path $XCVB_DIR > $TMP/xcvb-ssp.out

  fgrep -q "(:BUILD \"/xcvb\") in \"$XCVB_DIR/build.xcvb\"" $TMP/xcvb-ssp.out ||
  abort "Can't find build for xcvb"

  fgrep -q "(:ASDF \"xcvb\") superseded by (:BUILD \"/xcvb\")" $TMP/xcvb-ssp.out ||
  abort "can't find superseded asdf for xcvb"

  grep -q "CONFLICT for \"/xcvb/test/conflict/b\" between (\"$XCVB_DIR/test/conflict/b2\\?/build.xcvb\" \"$XCVB_DIR/test/conflict/b2\\?/build.xcvb\")" $TMP/xcvb-ssp.out ||
  abort "can't find conflict for /xcvb/test/conflict/b"
}

validate_xcvb () {
  # preconditions: env, xcvb built, PWD=.../xcvb/
  validate_xcvb_ssp # can the built xcvb search its search path?
  #validate_sa_build # can it build hello with the standalone backend?
  validate_mk_build # can it build hello with the Makefile backend?
  validate_nemk_build # can it build hello with the non-enforcing Makefile backend?
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
  rm -f $INSTALL_BIN/hello || :
  $@
  validate_hello
  rm $INSTALL_BIN/hello
}

validate_mk_build () {
  validate_hello_build make hello $ENV
}

validate_nemk_build () {
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
  diff -urN $XCVB_DIR/test/a2x $BUILD_DIR/a2x_rmx |
  grep -v '^diff\|^---\|^+++' |
  diff - $XCVB_DIR/test/rmx.diff
}

validate_a2x () {
  mkdir -p $BUILD_DIR/a2x_a2x/
  rsync -a $XCVB_DIR/test/a2x/ $BUILD_DIR/a2x_a2x/
  cd $BUILD_DIR/a2x_a2x
  xcvb a2x --system a2x-test --name /xcvb/test/a2x
  diff -urN $XCVB_DIR/test/a2x $BUILD_DIR/a2x_a2x |
  grep -v '^diff\|^---\|^+++' |
  diff - $XCVB_DIR/test/a2x.diff
}

validate_master () {
  xcvb eval "(progn(xcvb-master:bnl\"xcvb/hello\":output-path\"$BUILD_DIR/\":object-directory\"$obj\")(let((*print-base* 30))(xcvbd:call :xcvb-hello :hello :name 716822547 :traditional t)))" |
  fgrep 'hello, tester' ||
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
}
do_self_mk_build () {
  # pre-requisites (besides env): PWD=.../xcvb/
  make xcvb $ENV
}
do_self_nemk_build () {
  # pre-requisites (besides env): PWD=.../xcvb/
  rm setup.lisp
  make setup.lisp $ENV
  make xcvb-using-nemk $ENV
  rm setup.lisp
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
  make install $ENV
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
  mkdir -p $obj $INSTALL_BIN
  with_xcvb_build_dir validate_asdf_xcvb
  with_xcvb_build_dir validate_mk_xcvb
  with_xcvb_build_dir validate_nemk_xcvb
  clean_xcvb_dir
}

validate_xcvb_dir_all_lisps () {
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
  for LISP in $LISPS ; do
    validate_release_dir
  done
}

#### And now, let's do whatever the user asks us to! ####
#reset_variables # don't do it.

# NB: the same-line exit ensures no funny surprise if the script is edited while running.
doenv $@ ; exit
