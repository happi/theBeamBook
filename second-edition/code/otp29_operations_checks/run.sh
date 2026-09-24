#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
check_tmp=$(mktemp -d)
trap 'rm -rf "$check_tmp"' EXIT
ssh-keygen -q -t ed25519 -N '' -f "$check_tmp/ssh_host_ed25519_key"
escript code/otp29_operations_checks/check.escript "$check_tmp"
