pragma solidity ^0.4.16;

contract owned {
    address public owner;

    function owner() public {
        owner = msg.sender;
    }

    modifier onlyOwner() {
        require(msg.sender == owner);
        _;
    }

    function transferOwnerShip(address newOwner) onlyOwner public {
        require (newOwner != address(0), "Empty address!");
        owner = newOwner;
    }
}

interface tokenRecipient { 
    function receiveApproval(address _from, uint256 _value, address _token, bytes _extraData) external;
}

contract TokenERC20 {
    // Public variables 
    string public name;
    string public symbol;
    // 18 decimals is the strongly suggested default, avoid changing it
    uint8 public decimals = 2;
    uint256 public totalSupply;

    // arrays
    mapping (address => uint256) public balanceOf;
    mapping (address => mapping (address => uint256)) public allowance;

    // notify clients on the blockchain
    event Transfer(address indexed from, address indexed to, uint256 value);

    event Approval(address indexed owner, address indexed _spender, uint256 _value);

    event Burn(address indexed from, uint256 value);

    constructor(uint256 initialSupply, string tokenName, string tokenSymbol) public payable{
        totalSupply = initialSupply * 10 ** uint256(decimals);
        balanceOf[msg.sender] = totalSupply;
        name = tokenName;
        symbol = tokenSymbol;
    }

    function _transfer(address _from, address _to, uint _value) internal {
       require (_to != address(0));
       require (balanceOf[_from] >= _value);
       require (balanceOf[_to] + _value > balanceOf[_to]);

        uint previousBalances = balanceOf[_from] + balanceOf[_to];
        balanceOf[_from] -= _value;
        balanceOf[_to] += _value;

        emit Transfer(_from, _to, _value);
        // Asserts are used to use static analysis to find bugs in your code. They should never fail
        assert(balanceOf[_from] + balanceOf[_to] == previousBalances);
    }

    function transfer(address _to, uint256 _value) public returns (bool success) {
        _transfer(msg.sender, _to, _value);
        return true;
    }

    function transferFrom(address _from, address _to, uint256 _value) public returns (bool success) {
        require (_value <= allowance[_from][msg.sender]);
        _transfer(_from, _to, _value);
        return true;
    }

    function approve(address _spender, uint256 _value) public returns (bool success) {
        allowance[msg.sender][_spender] = _value;
        emit Approval(msg.sender, _spender, _value);
        return true;
    }

    function getApprove(address _spender) public view returns (uint bal){
        return allowance[msg.sender][_spender];
    }

    function approveAndCall(address _spender, uint256 _value, bytes _extraData) public returns (bool success) {
        tokenRecipient spender = tokenRecipient(_spender);
        if (approve(_spender, _value)) {
            spender.receiveApproval(msg.sender, _value, this, _extraData);
            return true;
        }
    }

    function burn(uint256 _value) public returns (bool success) {
        require(balanceOf[msg.sender] >= _value);
        balanceOf[msg.sender] -= _value;
        totalSupply -= _value;
        emit Burn(msg.sender, _value);
        return true;
    }

    function burnFrom(address _from, uint256 _value) public returns (bool success) {
        require (balanceOf[_from] >= _value);
        require (_value <= allowance[_from][msg.sender]);
        balanceOf[_from] -= _value;
        allowance[_from][msg.sender] -= _value;
        totalSupply -= _value;
        emit Burn(_from, _value);
        return true;
    }

    function getBalance(address addr) public view returns(uint) {
        return balanceOf[addr];
    }

    function getSymbol() public view returns (string) {
        return symbol;
    }
}

contract MyAdvancedToken is owned, TokenERC20 {
    uint256 public sellPrice;
    uint256 public buyPrice;

    mapping (address => bool) public frozenAccounts;

    event FrozenFunds(address target, bool frozen);

    uint public timeOfLastProof;

    constructor (uint256 initialSupply, string tokenName, string tokenSymbol) TokenERC20(initialSupply, tokenName, tokenSymbol) public payable{
        timeOfLastProof = now; 
    }

    // Empty fallback function, just used to accept Ether
    function() public payable {
    }

    function _transfer(address _from, address _to, uint _value) internal {
        require (_to != 0x0, "Nil address 'To'");
        require (balanceOf[_from] >= _value, "From-balance not enough!");
        require (balanceOf[_to] + _value >= balanceOf[_to], "Overflow!");
        require (!frozenAccounts[_from], "From-Account freezed!");
        require (!frozenAccounts[_to], "To-Account freezed!");
        balanceOf[_from] -= _value;
        balanceOf[_to] += _value;
        emit Transfer(_from, _to, _value);
    }

    // @notice Create `mintedAmount` tokens and send it to the target
    function mintToken(address target, uint256 mintedAmount) onlyOwner public {
        balanceOf[target] += mintedAmount;
        totalSupply += mintedAmount;
        emit Transfer(0, owner, mintedAmount);
        emit Transfer(this, target, mintedAmount);
    }

    function freezeAccount(address target, bool freeze) onlyOwner public {
        frozenAccounts[target] = freeze;
        emit FrozenFunds(target, freeze);
    }

    function setPrices(uint256 newSellPrice, uint256 newBuyPrice) onlyOwner public {
        sellPrice = newSellPrice;
        buyPrice = newBuyPrice;
    }

    // Here has no real-ether transfer
    function buy() payable public {
        uint amount = msg.value / buyPrice;
        _transfer(this, msg.sender, amount);
    }

    function sell(uint256 amount) public {
        address myAddress = this;
        require (myAddress.balance >= amount * sellPrice);
        _transfer(msg.sender, this, amount);
        msg.sender.transfer(amount * sellPrice);
    }

    /*  ---------- NOT USED ----------------- */
    /* ABOUT PROOF OF WORK */
    function giveBlockReward() public {
        balanceOf[block.coinbase] += 1;
    }

    /* anyone who can do math win a reward */
    /*
    uint public currentCubChallenge = 1;
    function rewardMathGeniuses(uint answerToCurrentReward, uint nextChallenge) public {
        require(answerToCurrentReward ** 3 == currentCubChallenge);
        balanceOf[msg.sender] += 1;
        currentCubChallenge = nextChallenge;
    }

    bytes32 public currentChallenge;
    uint public difficulty = 10 ** 32;
    function proofOfWork(uint nonce) public {
        bytes8 n = bytes8(keccak256(nonce, currentChallenge));
        require(n > bytes8(difficulty));
        uint timeSinceLastProof = (now - timeOfLastProof);
        require(timeSinceLastProof >= 5 seconds);
        balanceOf[msg.sender] += timeSinceLastProof / 60 seconds;

        difficulty = difficulty * 10 minutes / timeSinceLastProof + 1;

        timeOfLastProof = now;
        currentChallenge = keccak256(nonce, currentChallenge, blockhash(block.number - 1));
    }
    */
}
