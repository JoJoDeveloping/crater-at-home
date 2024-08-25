set -u
exec 2>&1

# export TERM=xterm

# Extract the cache if it exists
# Ideally it's just an error to not have a cache, but this script is executed to build the cache.
if [ -e /cache.tar.gz ]
then
    tar xf /cache.tar.gz
fi

TOOLCHAIN=miri

export CARGO_INCREMENTAL=0
export RUST_BACKTRACE=1
export RUSTFLAGS="--cap-lints=warn -Copt-level=0 -Zvalidate-mir -Aunexpected-cfgs"
if [[ $TARGET == "x86_64-unknown-linux-gnu" ]]; then
    export RUSTFLAGS="$RUSTFLAGS -Ctarget-cpu=x86-64-v2"
elif [[ $TARGET == "aarch64-unknown-linux-gnu" ]]; then
    export RUSTFLAGS="$RUSTFLAGS -Ctarget-cpu=apple-a14"
fi

export RUSTFLAGS="$RUSTFLAGS -Zrandomize-layout -Cdebuginfo=1 -Zlayout-seed=3405691582"
export MIRIFLAGS="-Zmiri-disable-isolation -Zmiri-ignore-leaks -Zmiri-num-cpus=4 -Zmiri-strict-provenance"
export RUSTDOCFLAGS=$RUSTFLAGS
export NEXTEST_EXPERIMENTAL_LIBTEST_JSON=1

function timed {
    timeout -v --kill-after=10s 1h inapty cargo +$TOOLCHAIN "$@" --target=$TARGET
}

function not_timed {
    inapty cargo +$TOOLCHAIN "$@" --target=$TARGET
}

function run_check {
    timed check $ARGS
}

function run_miri {
    echo "Running miri, kind $KIND, target $TARGET, profile $PROFILE"
    mkdir -p $OUTPUTDIR/$KIND
    MIRIFLAGS="$MIRIFLAGS $EXTRAMIRIFLAGS" not_timed miri nextest run --hide-progress-bar --no-fail-fast -j8 --config-file=/root/.cargo/nextest.toml --profile $PROFILE $ARGS
    mv target/nextest/$PROFILE/junit.xml $OUTPUTDIR/$KIND/junit.xml
}

function run_miri_thrice {
    MIRIFLAGS="$MIRIFLAGS" timed miri test --no-run $ARGS &> /dev/null
    # print miri version (hopefully?)
    MIRIFLAGS="-Zmiri-version" timed miri run
    KIND=noborrows EXTRAMIRIFLAGS="-Zmiri-disable-stacked-borrows" PROFILE="default-miri" run_miri
    KIND=stackedborrows EXTRAMIRIFLAGS="" PROFILE="slow-miri" run_miri
    KIND=treeborrows EXTRAMIRIFLAGS="-Zmiri-tree-borrows" PROFILE="slow-miri" run_miri
}

timed miri setup &> /dev/null

while read crate;
do
    cd /build
    export OUTPUTDIR=/output/$crate
    # Delete everything in our writable mount points
    find /build /tmp /root/.cargo/registry -mindepth 1 -delete
    if cargo download $crate /build; then
        ARGS=$(get-args $crate)
        cargo update &> /dev/null
        run_miri_thrice
    fi
    chown -R 1000:1000 $OUTPUTDIR
    echo "-${TEST_END_DELIMITER}-"
    # Delete everything in our writable mount points
    find /build /tmp /root/.cargo/registry -mindepth 1 -delete
done < /dev/stdin
