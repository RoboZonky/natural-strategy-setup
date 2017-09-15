#!/usr/bin/env bash
DEPLOYMENT_DIR=../../jhrcek/jhrcek.github.io/natural-strategy-setup
rm --recursive --force $DEPLOYMENT_DIR
cp --recursive dist $DEPLOYMENT_DIR
