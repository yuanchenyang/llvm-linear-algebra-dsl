CS164 Final Project
===================

Proposal: https://docs.google.com/document/d/1gH8FUI_r3geOlxcH8uTkqoJPcMoihPSUoW2LKaROUmI

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
