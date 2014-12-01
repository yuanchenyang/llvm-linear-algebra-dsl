CS164 Final Project
===================

Proposal: https://docs.google.com/document/d/1gH8FUI_r3geOlxcH8uTkqoJPcMoihPSUoW2LKaROUmI  
PA 6 Report: https://docs.google.com/document/d/1YQ0ljScwbgqIqp_zTqe_28akUQ1iJcR3szPaDjQ-E8c/edit?usp=sharing  
Design Document: https://docs.google.com/document/d/1jxXGokp4xRlaZ8CKpWOpg-vOyd2hZooCkJ8mj0_cFmw/edit?usp=sharing  

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