CHAINDATA_VIEWER='utils/ethereum_chaindata_leveldb_viewer --dbpath chaindata/'

extractValue() {
    echo "$1" |  sed 's/.*Value\[\(.*\)\]/\1/g'
}
