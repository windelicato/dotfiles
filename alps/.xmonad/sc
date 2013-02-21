#!/bin/bash

if [ $(pgrep sleep | wc -w) -gt 0 ]; then
	kill $(pgrep sleep | tail -n1);
else
	$1
fi
