pragma solidity ^0.4.18;

interface token {
    function transfer(address receiver, uint amount) external;
    function getSymbol() external view returns (string);
}

contract Crowdsale {
    address public beneficiary;
    uint public fundingGoal;
    uint public amountRaised;
    uint public deadline;
    uint public price;
    address public rewardTokenAddr;
    token public tokenReward;

    mapping(address => uint256) public balanceOf;
    mapping(address => uint256) public tokenBalanceOf;
    bool public fundingGoalReached = false;
    bool public crowdsaleClosed = false;

    event GoalReached(address recipient, uint totalAmountRaised);
    event FundTransfer(address backer, uint amount, bool isContribution);

    constructor(address ifSuccessfulSendTo, uint fundingGoalInEthers, uint durationInSeconds, uint etherCostOfEachToken, address addressOfTokenUsedAsReward) public payable {
        beneficiary = ifSuccessfulSendTo;
        fundingGoal = fundingGoalInEthers * 1 ether;
        deadline = now + durationInSeconds * 1 seconds;
        price = etherCostOfEachToken * 1 wei;
        rewardTokenAddr = addressOfTokenUsedAsReward;
        tokenReward = token(addressOfTokenUsedAsReward);
    }

    /**
     * Fallback function
     * The function without name is the default function that is called whenever anyone funds to a contract
     */
   function () public payable {
       require(!crowdsaleClosed);
       uint amount = msg.value;
       balanceOf[msg.sender] += amount;
       tokenBalanceOf[msg.sender] += amount/price;
       amountRaised += amount;
//       tokenReward.transfer(msg.sender, amount / price);
       emit FundTransfer(msg.sender, amount, true);
   }

   modifier afterDeadline() { if (now >= deadline) _; }

   function checkGlobalReached() public afterDeadline {
       if (amountRaised >= fundingGoal) {
           fundingGoalReached = true;
           emit GoalReached(beneficiary, amountRaised);
       }
       crowdsaleClosed = true;
   }

   /**
    * Withdraw the funds
    */
   function safeWithdrawal() public payable afterDeadline {
       if (!fundingGoalReached) {
           uint amount = balanceOf[msg.sender];
           balanceOf[msg.sender] = 0;
           if (amount > 0) {
               if (msg.sender.send(amount)) {
                   emit FundTransfer(msg.sender, amount, false);
               } else {
                   balanceOf[msg.sender] = amount;
               }
           }
       }

       if (fundingGoalReached && beneficiary == msg.sender) {
           if (beneficiary.send(amountRaised)) {
               emit FundTransfer(beneficiary, amountRaised, false);
           } else {
               fundingGoalReached = false;
           }
       }
   }

   function getTokenSymbol() public view returns(string) {
        return tokenReward.getSymbol();
   }
}
