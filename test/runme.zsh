#!/bin/zsh -fex

## Plan:
## 1- Get this called by make with all the correct options as extracted from configure.mk
##
## 2- be able to run this on the actual release directory
##
## 3- properly cleanup after ourselves in the end.
##

abort () {
  if [ -n "$*" ] ; then print -r "$*" >&2 ; fi
  exit 42
}
doenv () {
  while [ "$#" -gt 0 ] ; do
    case "$1" in
      (*=*) declare "$1" ; shift ;;
      (*) $@ ; return ;;
    esac
  done
}

#### Initializing variables ####
reset_variables () {
  unset RELEASE_DIR asdf_path CL_LAUNCH_ASDF_PATH XCVB_PATH XCVB_DIR
  LISPS=(clisp sbcl ccl)
}
initialize_variables () {
  : ${LISP:=sbcl}
}
finalize_variables () {
  obj="$BUILD_DIR/obj"
  INSTALL_BIN="$BUILD_DIR/bin"
  INSTALL_LISP="$BUILD_DIR/common-lisp"
  INSTALL_IMAGE="$BUILD_DIR/lib/common-lisp/images"

  ENV=(INSTALL_BIN=$INSTALL_BIN INSTALL_IMAGE=$INSTALL_IMAGE)

  export PATH=$INSTALL_BIN:$PATH
  export XCVB_PATH
  CL_LAUNCH_FLAGS=(
    --lisp "$LISP"
    #${(s: :)asdf_path+"--path ${(j: --path :)asdf_path}"} ## fails when path contains spaces!
  )
  for i in $asdf_path ; do CL_LAUNCH_FLAGS=(${CL_LAUNCH_FLAGS} --path $i) ; done
}
compute_release_dir_variables () {
  initialize_variables
  BUILD_DIR="$RELEASE_DIR/build"
  XCVB_DIR="$RELEASE_DIR/xcvb"
  asdf_path=($XCVB_DIR "$RELEASE_DIR"/dependencies/*)
  XCVB_PATH="$RELEASE_DIR"
  finalize_variables
  ENV=($ENV CL_LAUNCH_FLAGS="$CL_LAUNCH_FLAGS")
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
}

#### And now, the Tests! ####

validate_xcvb_ssp () {
  # preconditions: env, xcvb built, PWD=.../xcvb/
  # postconditions: xcvb ssp working

  xcvb ssp --xcvb-path $XCVB_DIR |
  fgrep -q "(:BUILD \"/xcvb\") in \"$XCVB_DIR/build.xcvb\"" ||
  abort "Can't find build for xcvb"

  xcvb ssp --xcvb-path $XCVB_DIR |
  fgrep -q "(:ASDF \"/xcvb\") superseded by (:BUILD \"xcvb\")" ||
  abort "can't find superseded asdf for xcvb"

  xcvb ssp --xcvb-path $XCVB_DIR |
  fgrep -q "CONFLICT for \"/xcvb/test/conflict/b\" between (\"$XCVB_DIR/test/conflict/b/build.xcvb\" \"$XCVB_DIR/test/conflict/b2/build.xcvb\")" || abort "can't find conflict for /xcvb/test/conflict/b"
}

validate_xcvb () {
  # preconditions: env, xcvb built, PWD=.../xcvb/
  validate_xcvb_ssp # can the built xcvb search its search path?
  #validate_sa_build # can it build itself with the standalone backend?
  validate_mk_build # can it build itself with the Makefile backend?
  validate_nemk_build # can it build itself with the non-enforcing Makefile backend?
  validate_x2a # can it convert a simple build back to asdf?
  validate_rmx_a2x # can it remove the xcvb annotations and add them back?
  validate_master # does xcvb-master work?
}

validate_hello () {
  [ "hello, world" = "$(hello -t)" ] || abort "hello not working"
}

validate_hello_build () {
  cd $XCVB_DIR/test/hello
  rm $INSTALL_BIN/hello
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

validate_rmx_a2x () {
  cd $XCVB_DIR/test/a2x
  xcvb rmx a2x-test
  git diff $XCVB/test/a2x | cmp - rmx.diff
  xcvb a2x a2x-test
  git diff $XCVB/test/a2x | cmp - /dev/null
}

validate_master () {
  echo "master test NIY"
}

do_asdf_build () {
  # preconditions: env, PWD=.../xcvb/
  # postconditions: xcvb built
  make xcvb-using-asdf $ENV # bootstrap
}
do_self_mk_build () {
  # pre-requisites (besides env): PWD=.../xcvb/
  make xcvb INSTALL_BIN=$INSTALL_BIN INSTALL_LISP=$INSTALL_LISP
}
do_self_nemk_build () {
  # pre-requisites (besides env): PWD=.../xcvb/
  make xcvb-using-nemk INSTALL_BIN=$INSTALL_BIN INSTALL_LISP=$INSTALL_LISP
}

validate_asdf_build () {
  do_asdf_build ; validate_xcvb
}
validate_self_build () {
  do_self_build ; validate_xcvb
}

do_bootstrapped_build () {
  # pre-requisites (besides env): PWD=.../xcvb-release/
  make install INSTALL_BIN=$INSTALL_BIN
  validate-xcvb
}

clean_xcvb_dir () {
  cd $XCVB_DIR ;
  make clean
  rm -rf "${XCVB_DIR}/build/"
}

clean_release_dir () {
  cd $XCVB_DIR ;
  make clean
  rm -rf "${RELEASE_DIR}/build/"
}

validate_xcvb_dir () {
  compute_xcvb_dir_variables
  check_xcvb_dir
  cd $XCVB_DIR
  mkdir -p $obj $INSTALL_BIN
  validate_asdf_build
  validate_self_build
}

validate_release_dir () {
  compute_release_dir_variables
  check_release_dir
  cd $RELEASE_DIR
  mkdir -p $obj $INSTALL_BIN
  for LISP in $LISPS ; do
    compute_release_variables
    do_bootstrapped_build
    validate_xcvb
    clean_release_dir
  done
}

#### And now, let's do whatever the user asks us to! ####
#reset_variables # don't do it.

# NB: the same-line exit ensures no funny surprise if the script is edited while running.
doenv $@ ; exit
