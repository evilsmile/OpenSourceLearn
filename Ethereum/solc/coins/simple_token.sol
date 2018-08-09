pragma solidity ^0.4.20;

contract owned {
    address public owner;

    function owned() {
        owner = msg.sender;
    }

    modifier onlyOwner {
        require(msg.sender == owner);
        _;
    }

    function transferOwnerShip(address newOwner) onlyOwner {
        owner = newOwner;
    }
}

contract MyToken is owned {
    /* This creates an array with all balances */
    mapping (address => uint256) public balanceOf;
    mapping (address => bool) public frozenAccounts;

    string name;
    string symbol;

    uint8 decimal;

    uint256 public totalSupply;
    uint256 public sellPrice;
    uint256 public buyPrice;

    uint public minBalanceForAccounts;

    event FrozenFunds(address target, bool frozen);

    /* Initializes contract with initial supply tokens to the creator of the contract */
    constructor ( uint256 initialSupply, string tokenName, uint8 decimalUnit, string tokenSymbol, address centralMinter) public {
        balanceOf[msg.sender] = initialSupply;
        name = tokenName;
        decimalUnit = decimal;
        symbol = tokenSymbol;
        owner = centralMinter;
        totalSupply = initialSupply;
    }

    function _transfer(address _from, address _to, uint _value) internal {
        require (_to != 0x0);
        require (balanceOf[_from] >= _value);
        require (balanceOf[_to] + _value >= balanceOf[_to]);
        require(!frozenAccounts[_from]);
        require(!frozenAccounts[_to]);
        balanceOf[_from] -= _value;
        balanceOf[_to] += _value;
        emit Transfer(_from, _to, _value);
    }

    /* Send coins */
    function transfer(address _to, uint256 _value) public returns (bool success) {
        require(!frozenAccounts[msg.sender]);
        require(balanceOf[msg.sender] >= _value);           // Check if the sender has enough
        require(balanceOf[_to] + _value >= balanceOf[_to]);   // Check for overflows
        balanceOf[msg.sender] -= _value;                     // Subtract from the sender
        balanceOf[_to] += _value;                            // Add the same to the recipient

        /* ensure that no account receiving the token has less than the necessary Ether to pay the fee. */
        if (msg.sender.balance < minBalanceForAccounts) 
            sell((minBalanceForAccounts - msg.sender.balance) / sellPrice);

        if (_to.balance < minBalanceForAccounts) 
            _to.send(sell((minBalanceForAccounts - _to.balance) / sellPrice));

        return true;
    }

    function mintToken(address target, uint256 mintedAmount) onlyOwner {
        balanceOf[target] += mintedAmount;
        totalSupply += mintedAmount;
        emit Transfer(0, owner, mintedAmount);
        emit Transfer(owner, target, mintedAmount);
    }

    function freezeAccount(address target, bool freeze) onlyOwner{
        frozenAccounts[target] = freeze;
        emit FrozenFunds(target, forzen);
    }

    /* price */
    function setPrice(uint256 newSellPrice, uint256 newBuyPrice) onlyOwner {
        sellPrice = newSellPrice;
        buyPrice = newBuyPrice;
    }

    function buy() payable returns (uint amount) {
        amount = msg.value / buyPrice;
        _transfer(this, msg.sender, amount);
        return amount;
    }

    function sell(uint amount) returns (uint revenue) {
        require(balanceOf[msg.sender] >= amount);
        balanceOf[this] += amount;
        balanceOf[msg.sender] -= amount;
        revenue = amount * sellPrice;
        msg.sender.transfer(revenue);
        Transfer(msg.sender, this, amount);
        return revenue;
    }

    function setMinBalance(uint minimumBalanceInFinney) onlyOwner {
        minBalanceForAccounts = minimumBalanceInFinney * 1 finney;
    }

}
