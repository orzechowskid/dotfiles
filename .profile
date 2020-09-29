#!/bin/bash

# node is managed by /etc/alternatives, and we want to add its location to $PATH to get npm/npx
[[ "$(command -v node)" ]] && {
  export PATH=$PATH:$(dirname $(realpath $(command -v node)))
}
