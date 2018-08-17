var Crowdsale = artifacts.require("./Crowdsale.sol");
var MyAdvancedToken = artifacts.require("./MyAdvancedToken.sol");

contract("Crowdsale", function(accounts) {
    const ETHER = 1e18
    const expFundingGoal = 10 * ETHER
    const expInitAmountRaised = 0
    const expEtherCostOfEachToken = 1 ;
    const expDurationInSeconds = 1;

    const BENEFICIARY_ADDR = accounts[5];
    const SALER_ONE = accounts[1];
    const SALER_TWO = accounts[2];

    var beneficiaryBalance;

    it("check fundingGoal value", function() {
        return Crowdsale.deployed().then(function(instance) {
            return instance.fundingGoal.call();
        }).then(function(fundingGoal) {
            assert.equal(fundingGoal.valueOf(), expFundingGoal, "fundingGoal is not 10 Ether!");
        });
    });

    it("check beneficiary value", function() {
        return Crowdsale.deployed().then(function(instance) {
            return instance.beneficiary.call();
        }).then(function(beneficiary) {
            assert.equal(beneficiary.valueOf(), BENEFICIARY_ADDR, "beneficiary account should be set correctly");
            return web3.eth.getBalance(BENEFICIARY_ADDR);
        }).then(function(bal) {
            beneficiaryBalance = bal.toNumber();
            console.log("beneficiary has balance of ", beneficiaryBalance); });
    });

    it("check amountRaised value", function() {
        return Crowdsale.deployed().then(function(instance) {
            return instance.amountRaised.call();
        }).then(function(amountRaised) {
            assert.equal(amountRaised.valueOf(), expInitAmountRaised, "init raisedAmount should be 0!");
        });
    });

    it("check dealine value", function() {
        return Crowdsale.deployed().then(function(instance) {
            return instance.deadline.call();
        }).then(function(deadline) {
            assert.equal(deadline.valueOf(), parseInt(new Date()/1000) + expDurationInSeconds, "deadline should be set in minutes duration!");
        });
    });

    // In order to support saler's pay, contract itself should have some tokens.
    it("check token charge", function() {
        var meta;
        var InitCrowdsaleTokenBalance = 100000;

        return Crowdsale.deployed().then(function(instance) {

            meta = instance;
            return web3.eth.getBalance(Crowdsale.address);

        }).then(function(bal) {

            console.log("Get Crowdsale's balance is ", bal.toNumber());
            return meta.rewardTokenAddr.call();

        }).then(function(addr){
            var tokenInstance;
            var tokenOwnerAddr = accounts[0];
            // TokenTransfer: tokenOwner(accounts[0]) => Crowdsale.address
            return MyAdvancedToken.deployed().then(function(instance) {
                tokenInstance = instance;

                return tokenInstance.approve(addr, InitCrowdsaleTokenBalance, {from: tokenOwnerAddr});
            }).then(function() {

                return tokenInstance.freeTransferFrom(tokenOwnerAddr, addr, InitCrowdsaleTokenBalance, {from: tokenOwnerAddr});
            }).then(function() {

                console.log("Init Crowdsale token success.");
                return tokenInstance.getBalance.call(addr);

            }).then(function(crowdsaleTokenBal) {

                console.log("Crowdsale token balance: ", crowdsaleTokenBal.toNumber());
                return tokenInstance.getBalance.call(tokenOwnerAddr);

            }).then(function(ownerTokenBal) {
                console.log("Owner token balance: ", ownerTokenBal.toNumber());
            });
        });
    });

    it("test successful crowdsale", function() {
        var meta;

        var tokenAddr;
        var amountRaised;
        return Crowdsale.deployed().then(function(instance) {

            meta = instance;

            return meta.price.call();
        }).then(function(p) {
            console.log("price is ", p.toNumber());
            return meta.rewardTokenAddr.call();

        }).then(function(addr) {
            tokenAddr = addr.valueOf();
            var tokenInstance;
            var tokenOwnerAddr = accounts[0];

            return MyAdvancedToken.deployed().then(function(instance) {
                tokenInstance = instance;

                return tokenInstance.balanceOf.call(addr);

            }).then(function(crowdsaleTokenBal) {
                console.log("--> Crowdsale token balance: ", crowdsaleTokenBal.toNumber());
                return tokenInstance.balanceOf.call(tokenOwnerAddr);

            }).then(function(ownerTokenBal) {
                console.log("--> Owner token balance: ", ownerTokenBal.toNumber());
            });

        }).then(function() {

            return web3.eth.sendTransaction({
                from: SALER_ONE,
                to: Crowdsale.address,
                value: 4 * ETHER
            });

        }).then(function() {

            return meta.beneficiary.call();

        }).then(function(addr) {
            console.log("transfer address ", addr);
            console.log("contract address ", Crowdsale.address);

            console.log("sale1 charged.");
            return web3.eth.sendTransaction({
                from: SALER_TWO,
                to: Crowdsale.address,
                value: 6 *ETHER
            });

        }).then(function() {

            return meta.amountRaised.call();

        }).then(function(_amountRaised) {

            amountRaised = _amountRaised.toNumber();
            assert.equal(10 * ETHER, amountRaised, "amount raised should match 10 ETHER now.");
            return meta.fundingGoal.call();

        }).then(function(goal) {

            assert.equal(amountRaised, goal.toNumber(), "After two salers' crowdsale the amount should match wanting");
            return meta.checkGlobalReached({from: SALER_ONE});   

        }).then(function() {

            return meta.crowdsaleClosed.call();

        }).then(function(closed) {

            assert.equal(1, !!closed, "Crowdsale success. Value should be closed!");
            return meta.fundingGoalReached.call();

        }).then(function(reached) {

            assert.equal(1, !!reached, "Crowdsale success. Value should be reached!");
            return meta.safeWithdrawal({from: BENEFICIARY_ADDR});

        }).then(function() {

            return web3.eth.getBalance(BENEFICIARY_ADDR);

        }).then(function(bal) {

            console.log("After crowdsale beneficiary has balance of ", bal.toNumber());
            assert.equal(parseInt((expFundingGoal+beneficiaryBalance)/ETHER), parseInt(bal.toNumber()/ETHER), "After crowdsale beneficiary should get all ethers for it.");

        });
    });
});
