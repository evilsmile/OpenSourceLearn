pragma solidity ^0.4.16;

contract SimpleToken {

    mapping(address => uint) public balanceOf;

    address public owner;

    function() payable public {

    }

    constructor() public payable {
        owner = msg.sender;
        balanceOf[owner] = 1000000;
    }


    function _transfer(address _from, address _to, uint256 _value) internal {
        require(_from != address(0), "From shouldn't be 0!");
        require(_to != address(0), "To shouldn't be 0!");
        require(balanceOf[_from] >= _value, "Balance not enough!");
        require(balanceOf[_to] + _value >= balanceOf[_to], "Transfer overflow!");

        balanceOf[_from] -= _value;
        balanceOf[_to] += _value;
    }

    function transferTo(address _to, uint256 _value) public returns (bool success) {    
        _transfer(msg.sender, _to, _value);
        return true;
    }

    function getBalance(address addr) public view returns (uint ){
        return balanceOf[addr];
    }

}
