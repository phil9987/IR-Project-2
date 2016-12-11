#!/bin/bash


for i in 1 2 3 4 5
do
    sbt "run time ptB"
    sbt "run time ii"
    sbt "run time iiLDB"
done

sbt "run time pt"