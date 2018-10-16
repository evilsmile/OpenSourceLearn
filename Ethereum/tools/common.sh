CHAINDATA_VIEWER='utils/ethereum_chaindata_leveldb_viewer --dbpath chaindata/'
RLPTOOL='utils/rlp_tool'

extractValue() {
    echo "$1" |  sed 's/.*Value\[\(.*\)\]/\1/g'
}
