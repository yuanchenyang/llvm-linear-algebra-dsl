CS164 Final Project
===================

Proposal: https://docs.google.com/document/d/1gH8FUI_r3geOlxcH8uTkqoJPcMoihPSUoW2LKaROUmI
PA 6 Report: https://docs.google.com/document/d/1YQ0ljScwbgqIqp_zTqe_28akUQ1iJcR3szPaDjQ-E8c
Design Document: https://docs.google.com/document/d/1jxXGokp4xRlaZ8CKpWOpg-vOyd2hZooCkJ8mj0_cFmw
Presentation Slides: https://docs.google.com/presentation/d/1R167b4CC90N_FAhNEubsBVET6piK1DK2_JPPSHtrwh0
Poster: https://docs.google.com/presentation/d/1nBllOTOpP18xrpa_EW7FfPJBJyOPZkI7NQFag5mze90

# LMS


## Dependencies

* [SBT](http://www.scala-sbt.org/):
  * Create or edit the file `~/.sbtconfig` to contain the following options for the JVM:

    `SBT_OPTS="-Xms3G -Xmx3G -Xss1M -XX:MaxPermSize=192M -XX:+UseParallelGC"`

* [LMS](https://github.com/TiarkRompf/virtualization-lms-core):
  * `git clone https://github.com/TiarkRompf/virtualization-lms-core.git`
  * `sbt publish-local`

## To run

    $ cd lms
    $ sbt run


# racket-llvm (OSX)

    $ cd cs164fa14-final-project
    $ git submodule update --init --recursive
    $ brew install llvm35 --with-clang
    $ raco pkg install srfi  # answer a to have it grab all dependencies automatically
    $ raco link racket-llvm
    $ DYLD_LIBRARY_PATH=/usr/local/Cellar/llvm35/3.5.0/lib/llvm-3.5/lib/ LLVM_CONFIG=llvm-config-3.5 raco setup racket-llvm
    $ cd racket-llvm
    $ DYLD_LIBRARY_PATH=/usr/local/Cellar/llvm35/3.5.0/lib/llvm-3.5/lib/ LLVM_CONFIG=llvm-config-3.5 racket build.rkt
    $ cd ..
    $ DYLD_LIBRARY_PATH=/usr/local/Cellar/llvm35/3.5.0/lib/llvm-3.5/lib/ LLVM_CONFIG=llvm-config-3.5 racket racket-llvm/examples/hello_world.rkt
    ; ModuleID = 'gcd-module'

    define i32 @gcd(i32 %x, i32 %y) {
    test-zero:
      %cond = icmp eq i32 %x, 0
      br i1 %cond, label %zero, label %test-less

    test-less:                                        ; preds = %test-zero
      %cond1 = icmp ult i32 %x, %y
      br i1 %cond1, label %less, label %greater

    zero:                                             ; preds = %test-zero
      ret i32 %y

    less:                                             ; preds = %test-less
      %z = sub i32 %y, %x
      %ans = call i32 @gcd(i32 %x, i32 %z)
      ret i32 %ans

    greater:                                          ; preds = %test-less
      %z2 = sub i32 %x, %y
      %ans3 = call i32 @gcd(i32 %y, i32 %z2)
      ret i32 %ans3
    }

    define i32 @mul-add(i32 %x, i32 %y, i32 %z) {
    entry:
      %a = mul i32 %x, %y
      %b = add i32 %z, %a
      ret i32 %b
    }
    4
