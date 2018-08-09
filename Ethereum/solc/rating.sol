pragma solidity ^0.4.24;

contract Rating {

    function setRating (bytes32 _key, uint256 _value) public {

        /* 为特定编号的商品打分 */

        ratings[_key] = _value;

    }

    /* 显示特定商品的分数 */

    mapping (bytes32 => uint256) public ratings;

}
