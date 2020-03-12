#!/bin/bash

[[ "$(command -v node)" ]] && {
  export PATH=$PATH:$(npm get prefix)/bin
}
