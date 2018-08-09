contract MyToken {
    /* Public variables of the token */
    string public name;
    string public symbol;
    uint8 public decimals;

    /* Thies creates an array with all balances */
    mapping (address => uint256) public balanceOf;
    mapping (address => mapping (address => uint)) public allowance;
    mapping (address => mapping (address => uint)) public spentAllowance;

    /* This generates a public event on the blockchain that will notify clients */
    event Transfer(address indexed from, address indexed to, uint256 value);
    event ReceiveApproval(address _from, uint256 _value, address _token, bytes _extraData);

    /* Initializes contract with initial supply tokens to the creator of the contract */
    function MyToken (uint256 initialSupply, string tokenName, uint8 decimalUnits, string tokenSymbol) {
        // Give the creator all initial tokens
        balanceOf[msg.sender] = initialSupply;   
        // Set the name for display purposes
        name = tokenName;  
        // Set the symbol for display purposes
        symbol = tokenSymbol;
        // Amount of decimals for display purposes
        decimals = decimalUnits;
    }

    /* Send coins */
    function transfer(address _to, uint256 _value) {
        // Check if the sender has enough
        if (balanceOf[msg.sender] < _value) throw;
        // Check for overflows
        if (balanceOf[_to] + _value < balanceOf[_to]) throw;
        // Subtract from the sender
        balanceOf[msg.sender] -= _value;
        balanceOf[_to] += _value;
        // Notify anyone listening result of this transfer 
        Transfer(msg.sender, _to, _value);
    }

    /* Allow another contract to spend some tokens on your behalf */
    function approveAndCall(address _spender, uint256 _value, bytes _extraData) returns (bool success) {
        allowance[msg.sender][_spender] = _value;
        ReceiveApproval(msg.sender, _value, this, _extraData);
    }

    /* A contract attempts to get the coin */
    function transferFrom(address _from, address _to, uint256 _value) returns (bool success) {
        if (balanceOf[_from] < _value) throw;
        if (balanceOf[_to] + _value < balanceOf[_to]) throw;
        if (spentAllowance[_from][msg.sender] + _value > allowance[_from][msg.sender]) throw;
        balanceOf[_from] -= _value;
        balanceOf[_to] += _value;
        spentAllowance[_from][msg.sender] += _value;
        Transfer(msg.sender, _to, _value);
    }

    /* this unamed function is called whenever someone tries to send ether to it */
    function () {
        // Prevent accidental sending of ether
        throw;
    }
}
